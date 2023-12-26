% Copyright (C) 2022-2024 Olivier Boudeville
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
% Creation date: Sunday, November 20, 2022.


% @doc Actual module in charge of the Oceanic <b>telegram sending</b>.
%
% Better here than in `send-telegram.escript' in order to benefit from a more
% user-friendly debugging.
%
-module(telegram_sending).


-define( exec_name, "send-telegram.escript" ).


-export([ run/0, main/1 ]).


-export([ send_telegram/1 ]).


% Shorthands:

-type ustring() :: text_utils:ustring().



% @doc Typically for testing.
-spec run() -> void().
run() ->
	ArgTable = shell_utils:get_argument_table(),
	main( ArgTable ).




% @doc Returns the usage information of the corresponding application.
-spec get_usage() -> void().
get_usage() ->
	text_utils:format( "Usage: ~ts TELEGRAM_STRING [-h|--help]~n"
		"  Sends the telegram specified as an hexadecimal string.~n"
		"Example: ~ts \"55000707017af630002ef1963001ffffffff4400fe\"",
		[ ?exec_name, ?exec_name ] ).



% @doc Sole entry point for this sending service, either triggered by `run/0'
% or by the associated escript.
%
-spec main( shell_utils:argument_table() ) -> void().
main( ArgTable ) ->

	%trace_utils:debug_fmt( "Original script-specific arguments: ~ts",
	%   [ shell_utils:argument_table_to_string( ArgTable ) ] ),

	HelpRefKey = '-help',

	% Standardises command-line options:
	MergedTable = list_table:merge_in_keys( [
		{ HelpRefKey, [ 'h' ] } ], ArgTable ),

	%trace_utils:debug_fmt( "Canonicalized script-specific arguments: ~ts",
	%   [ shell_utils:argument_table_to_string( MergedTable ) ] ),

	list_table:has_entry( HelpRefKey, MergedTable ) andalso display_usage(),

	{ MaybeArgs, ShrunkArgTable } =
		shell_utils:extract_optionless_command_arguments( MergedTable ),

	case list_table:keys( ShrunkArgTable ) of

		[] ->
			ok;

		_UnexpectedOpts ->
			shell_utils:error_fmt( 10, "unexpected user input, ~ts~n~ts",
				[ shell_utils:argument_table_to_string( ShrunkArgTable ),
				  get_usage() ] )
			%throw( { unexpected_command_line_options, UnexpectedOpts } )

	end,

	% trace_utils:debug_fmt( "MaybeArgs=~p, ShrunkArgTable=~p",
	%                        [ MaybeArgs, ShrunkArgTable ] ),

	case MaybeArgs of

		undefined ->
			shell_utils:error_fmt( 15, "no argument specified.~n~ts",
								   [ get_usage() ] );
			%exit( no_argument_specified );

		_SingleArg=[ TelegramStr ] ->
			send_telegram( TelegramStr ),
			basic_utils:stop( _ErrorCode=0 );

		Args ->
			shell_utils:error_fmt( 20,
				"extra argument(s) specified, got ~ts.~n~ts",
				[ text_utils:strings_to_listed_string( Args ), get_usage() ] )

	end.



% @doc Displays the usage of this service, and stops (with no error).
display_usage() ->
	io:format( get_usage(), [] ),
	basic_utils:stop( _ErrorCode=0 ).


-spec send_telegram( ustring() ) -> void().
send_telegram( TelegramHexStr ) ->

	Telegram = oceanic:hexastring_to_telegram( TelegramHexStr ),

	% Quite similar to oceanic_just_send_to_device_test:

	TtyPath = oceanic:get_default_tty_path(),

	case oceanic:has_tty( TtyPath ) of

		true ->

			SerialPid = oceanic:secure_tty( TtyPath ),

			io:format( "Sending ~ts...~n",
					   [ oceanic:telegram_to_string( Telegram ) ] ),

			SerialPid ! { send, Telegram },

			SerialPid ! stop,

			io:format( "Telegram sent.~n", [] ),

			% For any late printout:
			timer:sleep( 500 );

		{ false, Reason } ->
			shell_utils:error_fmt( 25, "no suitable TTY environment found "
				"(cause: ~p; searched for device '~ts'), no sending done.",
				[ Reason, TtyPath ] )
			%exit( { no_tty_available, TtyPath } )

	end.
