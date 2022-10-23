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
% Creation date: Saturday, October 22, 2022.


% @doc Testing of the Ceylan-Oceanic <b>sending and encoding of statically
% defined telegrams/b>.
%
-module(oceanic_static_sending_test).


-export([ run/0 ]).


% For the enocean_device record:
-include("oceanic.hrl").


% Shorthands:

%-type telegram() :: oceanic:telegram().

%-type device_table() :: oceanic:device_table().
-type device_path() :: oceanic:device_path().



% @doc Receives a (single) message.
receive_message() ->

	test_facilities:display( "(waiting for any incoming message)" ),

	receive

		{ onEnoceanEvent, [ Event, _OcSrvPid ] } ->

			test_facilities:display( "Received at ~ts event ~ts.",
				[ time_utils:get_textual_timestamp(),
				  oceanic:device_event_to_string( Event ) ] );


		Other ->
			test_facilities:display( "Received following message: ~p.",
									 [ Other ] )

	end.



% Triggered iff a suitable environment is believed to be available.
-spec actual_test( device_path() ) -> void().
actual_test( TtyPath ) ->

	OcSrvPid = oceanic:start_link( TtyPath ),

	%oceanic:register_device( _SourceEurid= "002ee196",
	%   _Name="Test Source Device", _EEP="F6-02-01", OcSrvPid ),

	SourceEuridStr = "002ee196",
	SourceEurid = oceanic:string_to_eurid( SourceEuridStr ),

	% We create a device for the source, so that we can decode by ourselves the
	% telegram that we will forge (in order to check its generation):

	%EepId = oceanic_generated:get_first_for_eep_strings( <<"F6-02-01">> ),

	%SourceDeviceRec = #enocean_device{ eurid=SourceEurid,
	%								   name= <<"Test Source Device">>,
	%								   eep=EepId },

	%InitialDeviceTable = table:new( [ { SourceEurid, SourceDeviceRec } ] ),

	TargetEurid = oceanic:get_broadcast_eurid(),
	%TargetEuridStr = "all (broadcast)",

	% Double-rocker device has its top A button pressed, based on with a single
	% subtelegram, targeted to the address for broadcast transmission; its EEP
	% is double_rocker_switch (F6-02-01):
	%
	PressTelegram = oceanic:encode_double_rocker_switch_telegram( SourceEurid,
		TargetEurid, button_ao, pressed ),

	DecodeStr = case oceanic:decode_telegram( PressTelegram, OcSrvPid ) of

		DecodingError when is_atom( DecodingError ) ->
			text_utils:format( "a decoding error (~ts)", [ DecodingError ] );

		DecodedEvent ->
			text_utils:format( "following event: ~ts",
				[ oceanic:device_event_to_string( DecodedEvent ) ] )

	end,

	test_facilities:display( "The generated telegram to be sent next is:~n ~w"
		"~nDecoding it before results in ~ts", [ PressTelegram, DecodeStr ] ),

	oceanic:send( PressTelegram, OcSrvPid ),

	receive_message(),

	timer:sleep( 1000 ),

	oceanic:stop( OcSrvPid ).



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
