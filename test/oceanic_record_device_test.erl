% Copyright (C) 2022-2022 Olivier Boudeville
%
% This file is part of the Ceylan-OCEANIC library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Wednesday, September 7, 2022.


% @doc Testing of the Ceylan-Oceanic <b>reading from actual devices and
% recording the corresponding telegrams in file</b>.
%
% The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
% prerequisites shall be already available.
%
% See oceanic_decode_recorded_test.erl for the decoding of the file recorded by
% this test.
%
-module(oceanic_record_device_test).


% For oceanic_decode_recorded_test:
-export([ run/0, get_record_file_path/0 ]).



% Shorthand:

-type device_path() :: file_utils:device_path().
-type file_path() :: file_utils:file_path().


% Triggered if a suitable environment is believed to be available.
-spec actual_test( device_path(), maybe( file_path() ) ) -> void().
actual_test( TtyPath, MaybeRecordFilePath ) ->

	test_facilities:display( "Starting the Enocean test based on the "
							 "gateway TTY '~ts'.", [ TtyPath ] ),

	SerialPid = oceanic:start( TtyPath ),

	MaybeFile = case MaybeRecordFilePath of

		undefined ->
			undefined;

		RecordFilePath ->
			test_facilities:display( "Received telegrams will be written "
									 "to '~ts'.", [ RecordFilePath ] ),

			file_utils:open( RecordFilePath, _Opts=[ append ] )

	end,

	listen( MaybeFile ),

	% Record file never closed.

	oceanic:stop( SerialPid ).



% @doc Listens endlessly.
listen( MaybeFile ) ->

	test_facilities:display( "Test waiting for next telegram "
		"(hit CTRL-C to stop)..." ),

	T = oceanic:read_next_telegram(),

	test_facilities:display( "Test received telegram: ~p.", [ T ] ),

	MaybeFile =:= undefined orelse
		file_utils:write_ustring( MaybeFile,
			"{ \"~ts\", ~w }.~n", [ time_utils:get_textual_timestamp(), T ] ),

	%oceanic:decode_telegram( T ),

	listen( MaybeFile ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Actually this is the default one:
	TtyPath = "/dev/ttyUSBEnOcean",

	case oceanic:has_tty( TtyPath ) of

		true ->
			case executable_utils:is_batch() of

				true ->
					test_facilities:display( "(not running the device "
						"listening test, being in batch mode)" );

				false ->
					actual_test( TtyPath, get_record_file_path()  )

			end;

		% For example in continuous integration:
		{ false, Reason } ->
			test_facilities:display( "Warning: no suitable TTY environment "
				"found (cause: ~p), no test done.", [ Reason ] )

	end,

	test_facilities:stop().



% @doc Returns the path to the file used for recording.
-spec get_record_file_path() -> file_path().
get_record_file_path() ->
	"enocean-test-recording.etf".
