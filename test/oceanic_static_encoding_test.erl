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
% Creation date: Friday, November 18, 2022.

-module(oceanic_static_encoding_test).

-moduledoc """
Testing of the Ceylan-Oceanic <b>encoding of static telegrams/b>.
""".


-export([ run/0 ]).


-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Not useful here: OceanicServerPid = oceanic:start(),

	% Yet for the decoding of at least some types of packets, we need the EEP to
	% be configured from the corresponding emitting devices, so:
	%
	%InitialOcState = oceanic:get_test_state(),

	SourceEurid = oceanic:string_to_eurid( "0109d970" ),

	Tele = oceanic:encode_double_rocker_switch_telegram( SourceEurid,
		_MaybeTargetEurid=undefined, _ButtonDesignator=button_ai,
		_ButtonTransition=released ),

	test_facilities:display( "Generated following telegram for double-rocker: "
							 "~ts.", [ oceanic:telegram_to_string( Tele ) ] ),

	% Not relevant here: oceanic:stop( OceanicServerPid ),

	% So no final state is displayed here.

	test_facilities:stop().
