% Copyright (C) 2022-2026 Olivier Boudeville
%
% This file is part of the Ceylan-Oceanic library.
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

-module(oceanic_just_record_device_test).

-moduledoc """
Testing of the Ceylan-Oceanic **reading from actual devices and recording the
corresponding telegrams in file**.

The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
prerequisites shall be already available.

This test mostly does not depend on Oceanic, it just focuses on raw recording,
and performs no actual decoding.

When running this test, ensure that at least a few Enocean telegrams are
received, so that they can be stored; see the oceanic_decode_recorded_test
module for the decoding of the file recorded by the current test.
""".


% For oceanic_decode_recorded_test:
-export([ run/0, get_record_file_path/0 ]).



% Type shorthands:

-type device_path() :: file_utils:device_path().
-type file_path() :: file_utils:file_path().



-doc "Triggered iff a suitable environment is believed to be available.".
-spec actual_test( device_path(), option( file_path() ) ) -> void().
actual_test( TtyPath, MaybeRecordFilePath ) ->

    test_facilities:display( "Starting the Enocean recording test based on the "
                             "gateway TTY '~ts'.", [ TtyPath ] ),

    % We hijack the Oceanic logic by interacting directly from this test process
    % with the serial server:
    %
    _SerialPid = oceanic:secure_tty( TtyPath ),

    MaybeFile = case MaybeRecordFilePath of

        undefined ->
            undefined;

        RecordFilePath ->
            test_facilities:display( "Received telegrams will be written "
                                     "to '~ts'.", [ RecordFilePath ] ),

            file_utils:open( RecordFilePath, _Opts=[ append ] )

    end,

    % Never returns:
    listen( MaybeFile ).

    % Record file never closed.

    % No specific call to oceanic:stop/*.



-doc "Listens endlessly.".
listen( MaybeFile ) ->

    Hint = " (hit CTRL-C to stop)...",

    test_facilities:display( "Test waiting for the next telegram to be received"
                             ++ Hint ),

    receive

        % Receives data from the serial port:
        { data, NewChunk } ->
            test_facilities:display( "~nTest received as chunk ~ts.~n" ++ Hint,
                [ oceanic:telegram_to_string( NewChunk ) ] ),

            MaybeFile =:= undefined orelse
                file_utils:write_ustring( MaybeFile,
                    "{ \"~ts\", ~w, \"~ts\" }.~n",
                    [ time_utils:get_textual_timestamp(), NewChunk,
                      text_utils:binary_to_hexastring( NewChunk ) ] ),

            listen( MaybeFile )

    end.



-doc "Returns the path to the file used for recording.".
-spec get_record_file_path() -> file_path().
get_record_file_path() ->
    "enocean-test-recording.etf".



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    TtyPath = oceanic:get_default_tty_path(),

    case oceanic:has_tty( TtyPath ) of

        true ->
            case executable_utils:is_batch() of

                true ->
                    test_facilities:display( "(not running the device "
                        "recording test, being in batch mode)" );

                false ->
                    actual_test( TtyPath, get_record_file_path() )

            end;

        % For example in continuous integration:
        { false, Reason } ->
            test_facilities:display( "Warning: no suitable TTY environment "
                "found (cause: ~p; searched for device '~ts'), no test done.",
                [ Reason, TtyPath ] )

    end,

    test_facilities:stop().
