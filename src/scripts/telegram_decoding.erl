% Copyright (C) 2022-2023 Olivier Boudeville
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
% Creation date: Sunday, November 20, 2022.


% @doc Actual module in charge of the Oceanic <b>telegram decoding</b>.
%
% Better here than in `decode-telegram.escript' in order to benefit from a more
% user-friendly debugging.
%
-module(telegram_decoding).


-define( exec_name, "decode-telegram.escript" ).


-export([ run/0, main/1 ]).


-export([ decode_telegram/1 ]).


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
		"  Decodes the telegram specified as an hexadecimal string.~n"
		"Example: ~ts \"55000707017af630002ef1963001ffffffff4400fe\"",
		[ ?exec_name, ?exec_name ] ).



% @doc Sole entry point for this decoding service, either triggered by `run/0'
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
			decode_telegram( TelegramStr ),
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


-spec decode_telegram( ustring() ) -> void().
decode_telegram( TelegramHexStr ) ->

	Telegram = oceanic:hexastring_to_telegram( TelegramHexStr ),

	% Quite similar to oceanic_decode_recorded_test:

	TtyPath = oceanic:get_default_tty_path(),

	case oceanic:has_tty( TtyPath ) of

		true ->

			_SerialPid = oceanic:secure_tty( TtyPath ),

			io:format( "Decoding ~ts...~n",
					   [ oceanic:telegram_to_string( Telegram ) ] ),

			BogusState = oceanic:get_test_state(),

			case oceanic:try_integrate_chunk( _ToSkipLen=0,
						_MaybeAccChunk=undefined, Telegram, BogusState ) of

				{ decoded, Event, _MaybeDiscoverOrigin,
				  _MaybeNextChunk=undefined, _NewState } ->
					io:format( "Decoded as ~ts.",
						[ oceanic:device_event_to_string( Event ) ] ),
					basic_utils:stop( _ErrorCode=0 );

				{ decoded, Event, _MaybeDiscoverOrigin, NextChunk,
				  _NewState } ->
					io:format( "Decoded as ~ts, with following extra chunk "
						"of ~B bytes: ~ts.",
						[ oceanic:device_event_to_string( Event ),
						  size( NextChunk ),
						  oceanic:telegram_to_string( NextChunk ) ] ),
					basic_utils:stop( _ErrorCode=0 );

				{ Unsuccessful, _NewToSkipLen, _NewAccChunk, _NewState } ->
					shell_utils:error_fmt( 10, "Unable to decode telegram: "
						"~ts.", [ Unsuccessful ] )

			end,

			% For any late printout:
			timer:sleep( 500 );

		{ false, Reason } ->
			shell_utils:error_fmt( 25, "no suitable TTY environment found "
				"(cause: ~p; searched for device '~ts'), no decoding done.",
				[ Reason, TtyPath ] )
			%exit( { no_tty_available, TtyPath } )

	end.
