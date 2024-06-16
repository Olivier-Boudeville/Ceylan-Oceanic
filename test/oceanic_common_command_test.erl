% Copyright (C) 2022-2024 Olivier Boudeville
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
% Creation date: Friday, November 11, 2022.

-module(oceanic_common_command_test).

-moduledoc """
Testing of the Ceylan-Oceanic management of Enocean **Common Commands**, to
interact directly with the local USB (Enocean) gateway.
""".



-export([ run/0 ]).

% Silencing:
-export([ receive_event/0 ]).

% For the enocean_device record:
-include("oceanic.hrl").


% Type shorthand:

-type device_path() :: file_utils:device_path().



-doc "Receives a (single) event.".
receive_event() ->

	test_facilities:display( "(waiting for any incoming event)" ),

	receive

		{ onEnoceanDeviceEvent, [ Event, _BackOnlineInfo, _OcSrvPid ] } ->

			test_facilities:display( "Received at ~ts event ~ts.",
				[ time_utils:get_textual_timestamp(),
				  oceanic:device_event_to_string( Event ) ] );


		Other ->
			test_facilities:display( "Received following message: ~p.",
									 [ Other ] )

	end.



-doc "Triggered iff a suitable environment is believed to be available.".
-spec actual_test( device_path() ) -> void().
actual_test( TtyPath ) ->

	test_facilities:display( "Testing the management of Common Commands." ),

	OcSrvPid = oceanic:start_link( TtyPath ),

	ReadResp = oceanic:read_version( OcSrvPid ),
	test_facilities:display( "Read version: ~ts.",
							 [ oceanic:device_event_to_string( ReadResp ) ] ),

	ReadLogs = oceanic:read_logs( OcSrvPid ),
	test_facilities:display( "Read logs: ~ts.",
							 [ oceanic:device_event_to_string( ReadLogs ) ] ),

	oceanic:stop( OcSrvPid ),

	% So that any final trace sent when stopping can be transmitted and seen (as
	% stopping is an asynchronous operation):
	%
	timer:sleep( 1000 ),

	test_facilities:display( "Stopped." ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	TtyPath = oceanic:get_default_tty_path(),

	case oceanic:has_tty( TtyPath ) of

		true ->
			case executable_utils:is_batch() of

				true ->
					test_facilities:display( "(not running the sending "
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
