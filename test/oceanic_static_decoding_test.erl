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


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Not useful here: OceanicServerPid = oceanic:start(),

	Telegrams = [
		<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>,
		<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>,
		<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>> ],

	test_facilities:display(
		"Starting the Enocean test based on ~B static, pre-recorded telegrams.",
		[ length( Telegrams ) ] ),

	[ begin

		test_facilities:display( "~nDecoding test telegram '~w'...", [ T ] ),

		{ decoded, Event, _NextChunk= <<>> } = oceanic:test_decode( T ),

		test_facilities:display( "Decoded: ~ts",
								 [ oceanic:device_event_to_string( Event ) ] )

	  end || T <- Telegrams ],

	% Not useful here: oceanic:start( OceanicServerPid ),

	test_facilities:stop().
