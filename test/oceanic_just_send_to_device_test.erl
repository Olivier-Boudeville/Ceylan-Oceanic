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

% Silencing if unused:
-export([ replay_telegrams/1, replay_telegrams_for_green_switch/1,
		  replay_telegrams_for_white_switch/1,

		  emit_forged_telegrams/1 ]).


% For the enocean_device record:
-include("oceanic.hrl").


% Shorthands:

-type device_path() :: file_utils:device_path().




% @doc These telegrams were captured verbatim by
% oceanic_just_record_device_test.
%
-spec replay_telegrams( pid() ) -> void().
replay_telegrams( SerialPid ) ->
	replay_telegrams_for_green_switch( SerialPid ).
	%replay_telegrams_for_white_switch( SerialPid ).



% This Eltako smart plug can learn a new device by pressing a relatively long
% time (a bit more than 1 second) its left button (fixed LED lights up), then
% pressing (shortly) its right (LED blinks), until a telegram is received (then
% LED turns off).


% @doc Replays telegrams for the green switch
-spec replay_telegrams_for_green_switch( pid() ) -> void().
replay_telegrams_for_green_switch( SerialPid ) ->

	test_facilities:display( "Testing the behaviour of the green switch." ),

	% Defining first the telegrams to emit of interest:
	%
	% (for each telegram, an extra variation is also available for additional
	% testing; it only differs in terms of dbM and thus final CRC)

	% Short name: telegram "A"
	TopButtonPressedTelegram = oceanic:hexastring_to_telegram(
		%"55000707017af630002ee1963001ffffffff4600d4" ),
		 "55000707017af630002ee1963001ffffffff4a0028" ),

	% Short name: telegram "B"
	TopButtonReleasedTelegram = oceanic:hexastring_to_telegram(
		%"55000707017af600002ee1962001ffffffff460062" ),
		 "55000707017af600002ee1962001ffffffff4c00e0" ),

	% Short name: telegram "C"
	BottomButtonPressedTelegram = oceanic:hexastring_to_telegram(
		%"55000707017af610002ee1963001ffffffff43006b" ),
		 "55000707017af610002ee1963001ffffffff47003f" ),

	% Short name: telegram "D"
	BottomButtonReleasedTelegram = oceanic:hexastring_to_telegram(
		%"55000707017af600002ee1962001ffffffff440048" ),
		 "55000707017af600002ee1962001ffffffff460062" ),

	basic_utils:ignore_unused( [ TopButtonPressedTelegram,
		TopButtonReleasedTelegram, BottomButtonPressedTelegram,
		BottomButtonReleasedTelegram ] ),


	% What we found out, once this (unique) rocker has been learnt by the Eltako
	% smart plug (by pressing once its top button when in learn mode):
	%
	% - from an initially switched off plug:
	%   * just sending A (any number of times) will not trigger anything
	%   * same for B and D
	%   * the first sending of C will switch on the lamp, but the next ones will
	%   have no effect
	%
	% - from an initially switched on plug:
	%   * just sending A (any number of times) will not trigger anything
	%   * same for B and D
	%   * the first sending of C will switch off the lamp, but the next ones
	%   will have no effect

	% This looks illogical, as A, B and D would be useless, whereas C would have
	% inconsistent effects. The reason is that the interpretation of a telegram
	% depends on the one of the previous telegram(s).
	%
	% If we emulate more closely the behaviour of the rocker, the user presses
	% and releases first its top button (light turns on), then the user presses
	% and releases its bottom button (light becomes off). This corresponds to
	% sending A then B are sent, then C then D.
	%
	% Nevertheless if we play this scenario, we see a different outcome than
	% with the actuial switch: each run of it will toggle (on/off) the lamp once
	% (not twice, as expected); we also notice that the actual switch trigger
	% happens when C is processed.
	%
	% If trying to simplify this scenario (to obtain the same effect with fewer
	% telegrams):
	%
	% - just removing the sending of D: scenario does nothing (C does not
	% operate) -> D necessary to "unblock" C, hence D kept
	% - just removing the sending of A: scenario OK
	% - just removing the sending of B: scenario OK
	% - removing the sending of A and B: scenario OK

	% In conclusion:
	%  - the switch action happens iff, and when, C is processed
	%  - D must be sent after C to "unblock it"
	%  - A and B are useless here
	%
	% So we *can* operate this plug (one stateful command, sending C then D for
	% each toggle), yet this is not the same behaviour as when using the switch
	% (two stateless commands, to force on or off), which is rather confusing.

	%test_facilities:display( "Emulating the top button of the green switch being pressed." ), SerialPid ! { send, TopButtonPressedTelegram },

	% Sheer paranoia:
	timer:sleep( 200 ),

	%test_facilities:display( "Emulating the top button of the green switch being released." ), SerialPid ! { send, TopButtonReleasedTelegram },


	test_facilities:display( "Pausing." ), timer:sleep( 2000 ),

	test_facilities:display( "Emulating the bottom button of the green switch being pressed." ), SerialPid ! { send, BottomButtonPressedTelegram },

	% Sheer paranoia:
	timer:sleep( 200 ),

	test_facilities:display( "Emulating the bottom button of the green switch being released." ), SerialPid ! { send, BottomButtonReleasedTelegram },


	test_facilities:display( "End of green switch test." ).




% @doc Replays telegrams for the white switch, once its left rocker has
% been learnt by the Eltako smart plug by pressing (once) its top button.
%
-spec replay_telegrams_for_white_switch( pid() ) -> void().
replay_telegrams_for_white_switch( SerialPid ) ->

	% Double-rocker device (whose EURID is 002ee196) has its top A button
	% pressed, based on with a single subtelegram, targeted to the address for
	% broadcast transmission, best RSSI value being -68 dBm; security level:
	% telegram not processed; its EEP is double_rocker_switch (F6-02-01):
	%
	PressTelegram1 =
		%<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,68,0,254>>,
		%<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>,
		%<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,58,0,60>>,
		<<85,0,7,7,1,122,246,48,0,47,80,214,48,1,255,255,255,255,61,0,177>>,

	test_facilities:display( "Sending first press event: ~ts.",
							 [ oceanic:telegram_to_string( PressTelegram1 ) ] ),

	SerialPid ! { send, PressTelegram1 },


	test_facilities:display( "Sending first release event." ),

	timer:sleep( 1000 ),

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



-spec emit_forged_telegrams( pid() ) -> void().
emit_forged_telegrams( SerialPid ) ->

	%SourceEuridStr = "002ee196",
	SourceEuridStr = "ffa2df00",
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
	%Telegram = oceanic:encode_double_rocker_switch_telegram( SourceEurid,
	%	TargetEurid, button_ao, pressed ),

	% Alternate form:
	basic_utils:ignore_unused( [ TargetEurid, TargetEuridStr ] ),
	%Telegram = oceanic:hexastring_to_telegram(
	%   "55000707017af630002ee1963001ffffffff" ),
	%   "5500010005700838" ),

	% Another alternate form:
	Telegram = oceanic:encode_esp3_packet( _PacketType=radio_erp1_type,
		%_Data=text_utils:hexastring_to_binary("F6000109D97020") ),
		 _Data=text_utils:hexastring_to_binary("550007000111f6100109d970300c" ) ),
		%_Data=text_utils:hexastring_to_binary("550007000111f6000109d97020e2" ) ),

	test_facilities:display( "Sending as ~ts, to ~ts, for double-rocker press "
		"following ~ts.",
		[ SourceEuridStr, TargetEuridStr,
		  oceanic:telegram_to_string( Telegram ) ] ),

	SerialPid ! { send, Telegram }.

	%InitialTestState = oceanic:get_test_state( InitialDeviceTable ),

	% Trying to decode the telegram we just forged:
	%{ decoded, Event, _AnyNextChunk, _NewState } =
	%   oceanic:try_integrate_chunk( _ToSkipLen=0, _MaybeAccChunk=undefined,
	%                                Telegram, InitialTestState ),

	%test_facilities:display( "Forged telegram corresponding to: ~ts.",
	%                         [ oceanic:device_event_to_string( Event ) ] ),




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

	replay_telegrams( SerialPid ),

	%emit_forged_telegrams( SerialPid ),

	SerialPid ! stop,

	% For any late printout:
	timer:sleep( 500 ).

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
