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
% Creation date: Thursday, October 20, 2022.


% @doc Testing of the Ceylan-Oceanic <b>sending to actual devices telegrams</b>.
%
% The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
% prerequisites shall be already available.
%
% This test mostly does not depend on Oceanic, it just focuses on raw sending,
% and performs no actual encoding.
%
-module(oceanic_just_send_to_device_test).


% For oceanic_decode_recorded_test:
-export([ run/0 ]).

% Silencing:
-export([ replay_telegrams/1 ]).


% For the enocean_device record:
-include("oceanic.hrl").


% Shorthands:

-type device_path() :: file_utils:device_path().


% These telegrams were captured verbatim by oceanic_just_record_device_test.
%
% They will not be accepted by the target device:
replay_telegrams( SerialPid ) ->

	% Double-rocker device (whose EURID is 002ee196) has its top A button
	% pressed, based on with a single subtelegram, targeted to the address for
	% broadcast transmission, best RSSI value being -68 dBm; security level:
	% telegram not processed; its EEP is double_rocker_switch (F6-02-01):
	%
	PressTelegram1 =
		%<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,68,0,254>>,
		<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>,
		%<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,58,0,60>>,

	test_facilities:display( "Sending first press event: ~ts.",
							 [ oceanic:telegram_to_string( PressTelegram1 ) ] ),

	SerialPid ! { send, PressTelegram1 },


	test_facilities:display( "Sending first release event." ),

	% Double-rocker device (whose EURID is 002ee196) has no button released
	% simultaneously, based on with a single subtelegram, targeted to the
	% address for broadcast transmission, best RSSI value being -65 dBm;
	% security level: telegram not processed; its EEP is double_rocker_switch
	% (F6-02-01):
	%
	ReleaseTelegram1 =
		<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,65,0,9>>,
	SerialPid ! { send, ReleaseTelegram1 },


	test_facilities:display( "Sending first press event." ),

	PressTelegram2 =
		<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>,

	SerialPid ! { send, PressTelegram2 },


	test_facilities:display( "Sending second release event." ),

	ReleaseTelegram2 =
		<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,73,0,161>>,

	SerialPid ! { send, ReleaseTelegram2 }.



% Triggered iff a suitable environment is believed to be available.
-spec actual_test( device_path() ) -> void().
actual_test( TtyPath ) ->

	% No specific call to oceanic:start/*.

	test_facilities:display( "Starting the Enocean sending test based on the "
							 "gateway TTY '~ts'.", [ TtyPath ] ),


	% We hijack the Oceanic logic by interacting directly from this test process
	% with the serial server:
	%
	%TestTtyPath = "/dev/pts/10",

	SerialPid = oceanic:secure_tty( TtyPath ),
	%{ ok, FD } = serctl:open( TtyPath ),

	replay_telegrams( SerialPid ),

	SourceEuridStr = "002ee196",
	SourceEurid = oceanic:string_to_eurid( SourceEuridStr ),

	% We create a device for the source, so that we can decode by ourselves the
	% telegram that we will forge (in order to check its generation):

	EepId = oceanic_generated:get_first_for_eep_strings( <<"F6-02-01">> ),

	SourceDeviceRec = #enocean_device{ eurid=SourceEurid,
									   name= <<"Test Source Device">>,
									   eep=EepId },

	_InitialDeviceTable = table:new( [ { SourceEurid, SourceDeviceRec } ] ),

	TargetEurid = oceanic:get_broadcast_eurid(),
	TargetEuridStr = "all (broadcast)",

	% Double-rocker device has its top A button
	% pressed, based on with a single subtelegram, targeted to the address for
	% broadcast transmission, best RSSI value being -68 dBm; security level:
	% telegram not processed; its EEP is double_rocker_switch (F6-02-01):
	%
	%PressTelegram = oceanic:encode_double_rocker_switch_telegram( SourceEurid,
	%	TargetEurid, button_ao, pressed ),

	% Alternate form:
	basic_utils:ignore_unused( [ TargetEurid, TargetEuridStr ] ),
	PressTelegram = oceanic:hexastring_to_telegram(
		"55000707017af630002ee1963001ffffffff" ),

	%InitialTestState = oceanic:get_test_state( InitialDeviceTable ),

	% Trying to decode the telegram we just forged:
	%{ decoded, Event, _AnyNextChunk, _NewState } =
	%   oceanic:try_integrate_chunk( _ToSkipLen=0, _AccChunk= <<>>,
	%                                PressTelegram, InitialTestState ),

	%test_facilities:display( "Forged telegram corresponding to: ~ts.",
	%                         [ oceanic:device_event_to_string( Event ) ] ),

	test_facilities:display( "Sending as ~ts, to ~ts, for double-rocker press "
		"following ~ts.",
		[ SourceEuridStr, TargetEuridStr,
		  oceanic:telegram_to_string( PressTelegram ) ] ),

	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },
	SerialPid ! { send, PressTelegram },


	timer:sleep( 500 ),

	SerialPid ! stop.

	% No specific call to oceanic:stop/*.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	TtyPath = oceanic:get_default_tty_path(),

	case oceanic:has_tty( TtyPath ) of

		true ->
			case executable_utils:is_batch() of

				true ->
					test_facilities:display( "(not running the device "
						"sending test, being in batch mode)" );

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
