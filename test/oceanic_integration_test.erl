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
% Creation date: Sunday, October 9, 2022.

-module(oceanic_integration_test).

-moduledoc """
**Integration test** of Ceylan-Oceanic, representative of its actual use.

The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
prerequisites shall be already available.
""".


-export([ run/0, wait_for_test_events/2 ]).


% Type shorthands:

-type count() :: basic_utils:count().
-type device_path() :: file_utils:device_path().



-doc "Triggered iff a suitable environment is believed to be available.".
-spec actual_test( device_path() ) -> void().
actual_test( TtyPath ) ->

    OcSrvPid = oceanic:start_link( TtyPath ),

    %WaitCount = 5,

    % Infinite listening:
    WaitCount = -1,

    wait_for_test_events( WaitCount, OcSrvPid ),

    oceanic:stop( OcSrvPid ).



-doc """
Waits for actual test telegrams to be received.

The PID of the Oceanic server is just for test purpose.
""".
-spec wait_for_test_events( count(), oceanic:oceanic_server_pid() ) -> void().
wait_for_test_events( _Count=0, _OcSrvPid ) ->
    test_facilities:display( "(all intended events received)" );

wait_for_test_events( Count, OcSrvPid ) ->

    case Count > 0 of

        true ->
            test_facilities:display(
                "~n  (test still waiting for ~B Enocean events)", [ Count ] );

        false ->
            test_facilities:display( "~n  (test waiting indefinitely for "
                "Enocean events; hit CTRL-C to stop)", [] )

    end,

    receive

        { onEnoceanConfiguredDeviceFirstSeen,
                        [ Event, _BackOnlineInfo, OcSrvPid ] } ->

            test_facilities:display( "Test received at ~ts the following "
                "first-seen device event: ~ts.",
                [ time_utils:get_textual_timestamp(),
                  oceanic:device_event_to_string( Event ) ] ),

            wait_for_test_events( Count-1, OcSrvPid );


        { onEnoceanDeviceEvent, [ Event, _BackOnlineInfo, OcSrvPid ] } ->

            test_facilities:display( "Test received at ~ts the following "
                "device event: ~ts.",
                [ time_utils:get_textual_timestamp(),
                  oceanic:device_event_to_string( Event ) ] ),

            wait_for_test_events( Count-1, OcSrvPid );


        { onEnoceanJamming, [ AlertTrafficLevel, OcSrvPid ] } ->

            test_facilities:display( "Received at ~ts a possible jamming "
                "detection from ~w (at ~B bytes per second).",
                [ time_utils:get_textual_timestamp(), OcSrvPid,
                  AlertTrafficLevel ] ),

            wait_for_test_events( Count, OcSrvPid );


        Other ->

            test_facilities:display( "Received following unexpected (ignored) "
                                     "message:~n ~p", [ Other ] ),

            wait_for_test_events( Count, OcSrvPid )

    end.



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    test_facilities:display( "The version of this currently tested Oceanic "
        "library is ~ts (i.e. ~w).", [ oceanic:get_oceanic_version_string(),
                                       oceanic:get_oceanic_version() ] ),

    TtyPath = oceanic:get_default_tty_path(),

    case oceanic:has_tty( TtyPath ) of

        true ->
            case executable_utils:is_batch() of

                true ->
                    test_facilities:display( "(not running the integration "
                        "test, being in batch mode)" );

                false ->
                    actual_test( TtyPath )

            end;

        % For example in continuous integration:
        { false, Reason } ->
            test_facilities:display( "Warning: no suitable TTY environment "
                "found (cause: ~p; searched for device '~ts'), no test done.",
                [ Reason, TtyPath ] )

    end,

    test_facilities:stop().
