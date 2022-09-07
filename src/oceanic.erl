% Copyright (C) 2022-2022 Olivier Boudeville
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
% Creation date: Wednesday, September 7, 2022.


% @doc Main module of Ceylan-Oceanic, in order to drive Enocean communications.
-module(oceanic).


-export([ get_default_tty_path/0, has_tty/0, has_tty/1,
		  start/0, start/1, stop/1 ]).


-type serial_server_pid() :: pid().
% The PID of a process in charge of a serial connection.


-export_type([ serial_server_pid/0 ]).


% Depends on Ceylan-Myriad and Serial; refer to http://oceanic.esperide.org)

% Shorthands:

-type file_path() :: file_utils:file_path().
-type entry_type() :: file_utils:entry_type().



% @doc Returns the path to the default allocated TTY to the USB Enocean gateway.
-spec get_default_tty_path() -> file_path().
get_default_tty_path() ->
	"/dev/ttyUSBEnOcean".


% @doc Tells whether the default TTY exists and is a device.
%
% Useful at least for testing.
%
-spec has_tty() -> 'true'
			  | { 'false', 'non_existing' | { 'not_device', entry_type() } }.
has_tty() ->
	has_tty( get_default_tty_path() ).



% @doc Tells whether the specified TTY exists and is a device, together with any
% failure reason.
%
% Useful at least for testing.
%
-spec has_tty( file_path() ) ->
		  'true' | { 'false', 'non_existing' | { 'not_device', entry_type() } }.
has_tty( TtyPath ) ->

	case file_utils:exists( TtyPath ) of

		true ->
			case file_utils:resolve_type_of( TtyPath ) of

				device ->
					true;

				OtherType ->
					{ false, { not_device, OtherType } }

			end;

		false ->
			{ false, non_existing }

	end.



% @doc Starts the Enocean support, based on the default conventions regarding
% the TTY allocated to the USB Enocean gateway.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start() -> serial_server_pid().
start() ->
	start( get_default_tty_path() ).



% @doc Starts the Enocean support, based on the specified path to the TTY
% allocated to the USB Enocean gateway.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start( file_path() ) -> serial_server_pid().
start( TtyPath ) ->

	case has_tty( TtyPath ) of

		true ->
			ok;

		{ false, non_existing } ->
			trace_bridge:error_fmt( "The specified TTY, '~ts', "
				"does not exist. Is the device for the Enocean gateway "
				"plugged in and named accordingly?", [ TtyPath ] ),
			throw( { non_existing_tty, TtyPath } );


		{ false, { not_device, OtherType } } ->

			trace_bridge:error_fmt( "The specified TTY for the "
				"Enocean gateway, '~ts', is not a device but a ~ts.",
				[ TtyPath, OtherType ] ),

			throw( { not_a_device, TtyPath, OtherType } )

	end,

	% Symmetrical speed (in bits per second):
	SerialPid = serial:start( [ { open, TtyPath }, { speed, 57600 }]),

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Using TTY '~ts' to connect to Enocean gateway,"
			" corresponding to serial server ~w.", [ TtyPath, SerialPid ] ) ),

	SerialPid.



% @doc Stops the Enocean support managed by the specified serial server.
-spec stop( serial_server_pid() ) -> void().
stop( SerialPid ) ->

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Stopping serial server ~w.", [ SerialPid ] ) ),

	SerialPid ! stop.
