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
		"references ~ts decoding test telegram '~w':",
		[ oceanic:device_table_to_string( DeviceTable ), Tele ] ),

	% To have different timestamps, otherwise all pseudo-devices will be
	% considered to be seen only once:
	%
	timer:sleep( _Ms=1000 ),

	case oceanic:test_decode( Tele, DeviceTable ) of

		{ decoded, Event, _NextChunk= <<>>, NewState } ->

			test_facilities:display( "Decoded event: ~ts",
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
	InitialDeviceTable = oceanic:load_configuration(),

	% Samples of a few hardcoded telegrams:
	% (here we gathered only full telegrams, not truncated ones)

	% ERP1 radio packets of R-ORG a5, hence rorg_4bs (i.e. '4BS (4-byte
	% Communication)'):
	%
	TA5 = <<85,0,10,7,1,235,165,0,146,124,10,1,169,105,38,0,1,255,255,255,255,58,0,213>>,

	% ERP1 radio packet of R-ORG d5, hence rorg_1bs (i.e. '1BS (1-byte
	% Communication)'):
	%
	TD5 = <<85,0,7,7,1,122,213,9,5,5,51,236,0,1,255,255,255,255,46,0,146>>,

	% R-ORG f6, hence rorg_rps, i.e. 'RPS (Repeated Switch Communication)':
	TF6 = <<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,83,0,194>>,

	T1 = <<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>,
	T2 = <<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>,
	T3 = <<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>,

	% Not even a start byte here:
	TInvalid = <<0,7,7,1,122,213,9,1,149,159,98,0,1,255,255,255,255,70,0,67>>,

	basic_utils:ignore_unused( [ TA5, TD5, TF6, T1, T2, T3, TInvalid ] ),

	% Useful for the debugging of the support of new telegram types:
	Telegrams = [ TA5 ],
	%Telegrams = [ TA5, TD5, TF6, T1, T2, T3 ],

	test_facilities:display(
		"Starting the Enocean test based on ~B static, pre-recorded telegrams.",
		[ length( Telegrams ) ] ),

	Count = decode_telegrams( Telegrams, InitialDeviceTable, _Count=0 ),

	test_facilities:display( "Successfully decoded ~B telegram(s).",
							 [ Count ] ),

	% Not useful here: oceanic:stop( OceanicServerPid ),

	test_facilities:stop().