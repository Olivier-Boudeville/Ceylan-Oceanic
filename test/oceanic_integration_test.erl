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
% Creation date: Sunday, October 9, 2022.


% @doc <b>Integration test</b> of Ceylan-Oceanic, representative of its actual
% use.
%
% The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
% prerequisites shall be already available.
%
-module(oceanic_integration_test).


-export([ run/0 ]).


% Shorthands:

%-type count() :: basic_utils:count().
-type device_path() :: file_utils:device_path().



% Triggered iff a suitable environment is believed to be available.
-spec actual_test( device_path() ) -> void().
actual_test( TtyPath ) ->

	OcSrvPid = oceanic:start_link( TtyPath ),

	%wait_for_test_events( _Count=5, OcSrvPid ),

	% Infinite listening:
	wait_for_test_events( _Count=-1, OcSrvPid ),

	oceanic:stop( OcSrvPid ).



% @doc Waits for actual test telegrams to be received.
%
% The PID of the Oceanic server is just for test purpose.
%
wait_for_test_events( _Count=0, _OcSrvPid ) ->
	test_facilities:display( "(all intended events received)" );

wait_for_test_events( Count, OcSrvPid ) ->

	test_facilities:display( "~n(test still waiting for ~B Enocean events)",
							 [ Count ] ),

	receive

		{ onEnoceanEvent, [ Event, OcSrvPid ] } ->
			test_facilities:display( "Received at ~ts event ~ts.",
				[ time_utils:get_textual_timestamp(),
				  oceanic:device_event_to_string( Event ) ] ),

			wait_for_test_events( Count-1, OcSrvPid )

	end.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

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
