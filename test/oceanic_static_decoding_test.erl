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
% Creation date: Monday, September 19, 2022.


% @doc Testing of the Ceylan-Oceanic <b>decoding of static (pre-recorded)
% telegrams/b>.
%
-module(oceanic_static_decoding_test).


-export([ run/0 ]).


% Shorthands:

-type count() :: basic_utils:count().

-type telegram() :: oceanic:telegram().

-type device_table() :: oceanic:device_table().



% @doc Attempts to decode the specified telegrams in turn.
-spec decode_telegrams( [ telegram() ], device_table(), count() ) -> count().
decode_telegrams( _Telegrams=[], _DeviceTable, Count ) ->
	Count;

decode_telegrams( _Telegrams=[ Tele | T ], DeviceTable, Count ) ->

	test_facilities:display( "~nWhereas this current test device table "
		"references ~ts decoding test ~ts",
		[ oceanic:device_table_to_string( DeviceTable ),
		  oceanic:telegram_to_string( Tele ) ] ),

	% To have different timestamps, otherwise all pseudo-devices will be
	% considered to be seen only once:
	%
	timer:sleep( _Ms=1000 ),

	case oceanic:test_decode( Tele, DeviceTable ) of

		{ decoded, Event, _NextChunk= <<>>, NewState } ->

			test_facilities:display( "Decoded event: ~ts.",
				[ oceanic:device_event_to_string( Event ) ] ),

			decode_telegrams( T, oceanic:get_device_table( NewState ),
							  Count+1 );


		{ FailedOutcome, SkipLen, NextChunk, NewState } ->

			test_facilities:display( "Decoding failed: ~ts "
				"(while skip length: ~B and next chunk: ~w).",
				[ FailedOutcome, SkipLen, NextChunk ] ),

			decode_telegrams( T, oceanic:get_device_table( NewState ), Count )

	end.



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Not useful here: OceanicServerPid = oceanic:start(),

	% Yet for the decoding of at least some types of packets, we need the EEP to
	% be configured from the corresponding emitting devices, so:
	%
	InitialOcState = oceanic:load_configuration(),

	InitialDeviceTable = oceanic:get_device_table( InitialOcState ),

	% Samples of a few hardcoded telegrams:
	% (here we gathered only full telegrams, not truncated ones)

	% R-ORG 4BS thermo_hygro_low device (whose EURID is 01a96926) reports a
	% relative humidity of 58% and a temperature of 20°C (sensitivity range:
	% low), with no device learning activated, based on with a single
	% subtelegram, targeted to the address for broadcast transmission, best RSSI
	% value being -58dBm; security level: telegram not processed; its EEP is
	% thermo_hygro_low (A5-04-01):
	%
	TA5 = <<85,0,10,7,1,235,165,0,146,124,10,1,169,105,38,0,1,255,255,255,255,58,0,213>>,

	% R-ORG 1BS single-contact device (whose EURID is 050533ec) has been closed,
	% with no device learning activated, based on with a single subtelegram,
	% targeted to the address for broadcast transmission, best RSSI value being
	% -46dBm; security level: telegram not processed; its EEP is
	% single_input_contact (D5-00-01):
	%
	TD5 = <<85,0,7,7,1,122,213,9,5,5,51,236,0,1,255,255,255,255,46,0,146>>,

	% Double-rocker device (whose EURID is 002ee196) has its top A button
	% pressed, based on with a single subtelegram, targeted to the address for
	% broadcast transmission, best RSSI value being -83dBm; security level:
	% telegram not processed; its EEP is double_rocker_switch (F6-02-01):
	%
	TF6A = <<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,83,0,194>>,

	% Double-rocker device (whose EURID is 002ee196) has its top A button
	% pressed, based on with a single subtelegram, targeted to the address for
	% broadcast transmission, best RSSI value being -57dBm; security level:
	% telegram not processed; its EEP is double_rocker_switch (F6-02-01):
	%
	% (different RSSI)
	%
	TF6B = <<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>,

	% Not even a start byte here:
	TInvalid = <<0,7,7,1,122,213,9,1,149,159,98,0,1,255,255,255,255,70,0,67>>,

	% Just a command response (status code: OK) - whereas no command sent:
	TResp = oceanic:hexastring_to_telegram( "5500010002650000" ),

	% Another F6-02-01 double-rocker, for a bottom secondary button press:
	TDRWhitePress = oceanic:hexastring_to_telegram(
						"55000707017af650002f50d63001ffffffff3d00b4" ),

	TDRWhiteRelease = oceanic:hexastring_to_telegram(
						"55000707017af600002f50d62001ffffffff3c0012" ),

	% Placeholder for easy decoding of test telegrams:
	TTested = oceanic:hexastring_to_telegram(
		%"55000707017af630002f50d63001ffffffff490047" ),
		%"55000707017af600002f50d62001ffffffff4900f1" ),
		"55000707017af632deadbeef3103ffffffffff000c" ),

	AllTelegrams = [ TA5, TD5, TF6A, TF6B, TInvalid, TResp,
					 TDRWhitePress, TDRWhiteRelease, TTested ],

	% To select any subset of them next:
	basic_utils:ignore_unused( AllTelegrams ),

	% Useful for the debugging of the support of new telegram types:
	%Telegrams = [ TA5 ],
	%Telegrams = [ TD5 ],
	%Telegrams = [ TF6A ],
	%Telegrams = [ TF6B ],
	%Telegrams = [ TInvalid ],
	%Telegrams = [ TResp ],
	%Telegrams = [ TDRWhitePress ],
	%Telegrams = [ TDRWhiteRelease ],
	Telegrams = [ TTested ],

	%Telegrams = AllTelegrams,

	test_facilities:display(
		"Starting the Enocean test based on ~B static, pre-recorded telegrams.",
		[ length( Telegrams ) ] ),

	Count = decode_telegrams( Telegrams, InitialDeviceTable, _Count=0 ),

	test_facilities:display( "Successfully decoded ~B telegram(s).",
							 [ Count ] ),

	% Not relevant here: oceanic:stop( OceanicServerPid ),

	% So no final state is displayed here.

	test_facilities:stop().
