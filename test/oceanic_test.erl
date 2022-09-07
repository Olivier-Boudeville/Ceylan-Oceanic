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
% Creation date: Wednesday, September 7, 2022


% @doc Testing of Ceylan-Oceanic.
%
% The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
% prerequisites shall be already available.
%
-module(oceanic_test).


-export([ run/0 ]).


% Triggered if a suitable environment is believed to be available.
-spec actual_test( file_utils:file_path() ) -> void().
actual_test( TtyPath ) ->

	test_facilities:display( "Starting the Enocean test based on the "
							 "gateway TTY '~ts'.", [ TtyPath ] ),

	SerialPid = oceanic:start( TtyPath ),

	oceanic:stop( SerialPid ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	% Actually this is the default one:
	TtyPath = "/dev/ttyUSBEnOcean",

	case oceanic:has_tty( TtyPath ) of

		true ->
			actual_test( TtyPath );

		% For example in continuous integration:
		{ false, Reason } ->
			test_facilities:display( "Warning: no suitable TTY environment "
				"found (cause: ~p), no test done.", [ Reason ] )

	end,

	test_facilities:stop().
