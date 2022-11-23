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
% defined telegrams</b>.
%
-module(oceanic_static_sending_test).


-export([ run/0 ]).

% Silencing:
-export([ receive_event/0 ]).

% For the enocean_device record:
-include("oceanic.hrl").


% Shorthands:

-type device_path() :: oceanic:device_path().



% @doc Receives a (single) event, if any is received at short term.
receive_event() ->

	test_facilities:display( "(waiting for any incoming event)" ),

	receive

		{ onEnoceanEvent, [ Event, _OcSrvPid ] } ->

			test_facilities:display( "Received at ~ts event ~ts.",
				[ time_utils:get_textual_timestamp(),
				  oceanic:device_event_to_string( Event ) ] );


		Other ->
			test_facilities:display( "Received following message: ~p.",
									 [ Other ] )

	after 1000 ->

		test_facilities:display( "(no specific event received afterwards)" )

	end.



% Triggered iff a suitable environment is believed to be available.
-spec actual_test( device_path() ) -> void().
actual_test( TtyPath ) ->

	OcSrvPid = oceanic:start_link( TtyPath ),

	test_facilities:display( "Starting test; note that direct telegram "
		"sendings are made here, thus Oceanic will detect responses "
		"that do not match with any past request that it sent." ),

	%oceanic:register_device( _SourceEurid= "002ef196",
	%   _Name="Test Source Device", _EEP="F6-02-01", OcSrvPid ),

	% If attempting (and failing) to spoof a well-known device:
	%SourceEuridStr = "002ef196",

	% Not to clash (hopefully) with any actual device:
	% (deactivated as we want to use the base EURID of the USB gateway)
	%
	%SourceEuridStr = ?default_emitter_eurid,

	%SourceEurid = oceanic:string_to_eurid( SourceEuridStr ),

	%SourceEurid = oceanic:get_base_eurid( OcSrvPid ),
	SourceEurid = oceanic:string_to_eurid( "0109d970" ),


	% We create a device for the source, so that we can decode by ourselves the
	% telegram that we will forge (in order to check its generation):

	%EepId = oceanic_generated:get_first_for_eep_strings( <<"F6-02-01">> ),

	%SourceDeviceRec = #enocean_device{ eurid=SourceEurid,
	%                                   name= <<"Test Source Device">>,
	%                                   eep=EepId },

	%InitialDeviceTable = table:new( [ { SourceEurid, SourceDeviceRec } ] ),

	% Both forms, with a target (hence with optional data) or not, will work:
	MaybeTargetEurid = oceanic:get_broadcast_eurid(),
	%MaybeTargetEurid = undefined,

	% Alternate form:
	%basic_utils:ignore_unused( [ SourceEurid, MaybeTargetEurid ] ),


	% These two buttons are actually considered independently, and for our test
	% the "on" one has no special interest, only the "off" one is taken account
	% by the target device, a (double-rocker) switch:
	%
	SwitchOnButton = button_ao,
	SwitchOffButton = button_ai,

	% We first encode all telegrams of interest, so that we can send them
	% afterwards as wanted and in any order:


	% Apparently, after a telegram notifying a press or a release has been sent,
	% all telegrams of the same type will be ignored. So a proper transition has
	% to send them both (and in the correct order: 'pressed' then 'released').


	% Double-rocker device has its top A rocker pressed, based on with a single
	% subtelegram, not targeting any specified address (not even the address for
	% broadcast transmission); its EEP is double_rocker_switch (F6-02-01):
	%
	PressOnButtonTelegram = oceanic:encode_double_rocker_switch_telegram(
		SourceEurid, MaybeTargetEurid, SwitchOnButton, pressed ),

	ReleaseOnButtonTelegram = oceanic:encode_double_rocker_switch_telegram(
		SourceEurid, MaybeTargetEurid, SwitchOnButton, released ),


	PressOffButtonTelegram = oceanic:encode_double_rocker_switch_telegram(
		SourceEurid, MaybeTargetEurid, SwitchOffButton, pressed ),

	ReleaseOffButtonTelegram = oceanic:encode_double_rocker_switch_telegram(
		SourceEurid, MaybeTargetEurid, SwitchOffButton, released ),

	SwitchTelegrams =  [ PressOnButtonTelegram, ReleaseOnButtonTelegram,
						 PressOffButtonTelegram, ReleaseOffButtonTelegram ],

	basic_utils:ignore_unused( SwitchTelegrams ),

	DecodeStr = case oceanic:decode_telegram( PressOffButtonTelegram,
											  OcSrvPid ) of

		DecodingError when is_atom( DecodingError ) ->
			text_utils:format( "a decoding error (~ts)", [ DecodingError ] );

		DecodedEvent ->
			text_utils:format( "following event: ~ts",
				[ oceanic:device_event_to_string( DecodedEvent ) ] )

	end,

	test_facilities:display( "The generated telegrams are: ~ts"
		"Decoding the 'pressed' one for the 'off' button results in ~ts",
		[ text_utils:strings_to_string( [
			oceanic:telegram_to_string( T ) || T <- SwitchTelegrams ] ),
		  DecodeStr ] ),

	test_facilities:display( "All telegrams of interest encoded." ),


	% In our test setting, the "on" button is not specifically learnt and has
	% not impact:
	%
	%oceanic:send( PressOnButtonTelegram, OcSrvPid ),
	%oceanic:send( ReleaseOnButtonTelegram, OcSrvPid ),


	% In our test setting, the "off" button has been learnt; a press *and* a
	% release are needed to trigger any action; and this action is to toggle
	% on/off the switch.
	%
	% So, supposing that for this test the switch is initially off and we want
	% to switch it one temporarily, we have to:

	test_facilities:display( "First we press (and then also release) the "
		"'switch off' button, '~ts' (which must have already been learnt), "
		"typically in order to switch on a lamp.", [ SwitchOnButton ] ),

	% The actual switching on depends on both telegrams:
	oceanic:send( PressOffButtonTelegram, OcSrvPid ),
	% No waiting needed:
	oceanic:send( ReleaseOffButtonTelegram, OcSrvPid ),


	test_facilities:display( "Then, after a short waiting, we press "
		"(and then release) this 'switch off' button again, '~ts', typically "
		"to switch off the lamp.", [ SwitchOffButton ] ),

	timer:sleep( 1000 ),

	oceanic:send( PressOffButtonTelegram, OcSrvPid ),
	oceanic:send( ReleaseOffButtonTelegram, OcSrvPid ),

	% May be useful if the sending corresponds to a request:
	receive_event(),

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
