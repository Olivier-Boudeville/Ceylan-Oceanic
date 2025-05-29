% Copyright (C) 2025-2025 Olivier Boudeville
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
% Creation date: Wednesday, May 21, 2025.

-module(oceanic_encode).

-moduledoc """
Module centralising **all encoding** made by Ceylan-Oceanic.
""".


% For the records and defines:
-include("oceanic.hrl").
-include("oceanic_internal.hrl").


-export_type([  ]).


-export([ encode_esp3_packet/2,

          encode_double_rocker_telegram/3,
          encode_double_rocker_switch_telegram/5,
          encode_double_rocker_multipress_telegram/4,

          encode_teach_in_response/5,

          encode_switch_dimmer_set_output/3,

          get_optional_data_for_sending/1,
          get_designated_button_enum/2, get_button_transition_enum/1 ]).





% Implementation notes.

% Smart plugs are described by Enocean as "Electronic switches and dimmers with
% Energy Measurement and Local Control", i.e. EEP D2-01-* (see [EEP] p.131),
% hence based on VLD telegrams.
%
% We support mainly the D2-01 type 0B, in the objective of simply having a
% gateway be able to switch on/off a given smart plug.
%
% Such a smart plug is to be controlled as a home automation gateway, rather
% than for example by emulating a double rocker, which would imply only
% broadcast operations and thus no possibility of controlling actuators
% separately. This requires going through first a pass of teach-in.
%
% We are mostly interested here in the following commands:
% - "CMD 0x1 - Actuator Set Output" (see [EEP] p.132)


% Type shorthands:

-type eurid() :: oceanic:eurid().

-type packet_type() :: oceanic:packet_type().

-type telegram() :: oceanic:telegram().
-type telegram_chunk() :: oceanic:telegram_chunk().

-type telegram_data() :: oceanic:telegram_data().
-type telegram_opt_data() :: oceanic:telegram_opt_data().
-type enum() :: oceanic:enum().
-type application_style() :: oceanic:application_style() .
-type button_locator() :: oceanic:button_locator().
-type button_designator() :: oceanic: button_designator().
-type button_transition() :: oceanic:button_transition().
-type button_counting() :: oceanic:button_counting().
-type canon_outgoing_trigger_spec() :: oceanic:canon_outgoing_trigger_spec().
-type teach_outcome() :: oceanic:teach_outcome().
-type communication_direction() :: oceanic:communication_direction().
-type device_status() :: oceanic:device_status() .



% Section for encoding, in ESP3 packets.


-doc """
Encodes an ESP3 packet from its packet type and base data.
""".
-spec encode_esp3_packet( packet_type(), telegram_data() ) -> telegram().
encode_esp3_packet( PacketType, Data ) ->
	encode_esp3_packet( PacketType, Data, _OptData= <<>> ).



-doc """
Encodes an ESP3 packet from its packet type, base and optional data.
""".
-spec encode_esp3_packet( packet_type(), telegram_data(),
						  option( telegram_opt_data() ) ) -> telegram().
encode_esp3_packet( PacketType, Data, MaybeOptData ) ->

	DataLen = size( Data ),

	{ FullData, OptDataLen } = case MaybeOptData of

		undefined ->
			{ Data, 0 };

		OptData ->
			{ <<Data/binary, OptData/binary>>, size( OptData ) }

	end,

	% For example PacketType is radio_erp1_type:
	PacketTypeNum = oceanic_generated:get_second_for_packet_type( PacketType ),

	ESP3Header = <<DataLen:16, OptDataLen:8, PacketTypeNum:8>>,

	ESP3HeaderCRC = oceanic:compute_crc( ESP3Header ),

	FullDataCRC = oceanic:compute_crc( FullData ),

	% This is an ESP3 packet:
	<<?sync_byte, ESP3Header/binary, ESP3HeaderCRC:8, FullData/binary,
	  FullDataCRC:8>>.


% Subsection for the encoding of the packets of the ERP1 radio type.


% Finer section for the RPS telegrams, which include only the F6-* EEPs.



-doc """
Encodes a double-rocker telegram, from the specified EURID to the targeted one,
based on the specified outgoing trigger specification.
""".
-spec encode_double_rocker_telegram( eurid(), canon_outgoing_trigger_spec(),
									 eurid() ) -> telegram().
encode_double_rocker_telegram( SourceEurid,
		% With a check:
		_COTS={ _DevType=double_rocker,
				_VirtEmitInfo=AppStyle,
				_DevStChgSpec={ Channel, ButPos, ButTrans } },
		TargetEurid ) ->

	ButtonLocator = { Channel, ButPos },

	encode_double_rocker_switch_telegram( SourceEurid, AppStyle, ButtonLocator,
                                          ButTrans, TargetEurid ).



-doc """
Encodes a double-rocker switch telegram, from the specified device (of the
specified application style) to the specified one (if any), regarding a
transition of the single specified button (located based on channel and
position).

As this encoding is done exclusively on the caller side (the Oceanic server not
being involved), it is up to the caller to specify the source EURID (and
application style). We recommend that the caller fetches from the Oceanic server
its EURID (see `get_oceanic_eurid/1`) once for all, and uses it afterwards
(otherwise, no emitted telegram will be taken into account by receivers).

Event sent in the context of EEP F6-02-01 or EEP F6-02-02 (`"Light and Blind
Control - Application Style 1 or 2"`), for `T21=1`. It results thus in a RPS
telegram, an ERP1 radio packet encapsulated into an ESP3 one.

See `[EEP-spec]` p.15 and its decode_rps_double_rocker_packet/7 counterpart.

Depending on how Oceanic was learnt by the target actuator, it will be seen
either as a rocker (recommended) or as push-button(s): refer to
[http://oceanic.esperide.org/#buttons-vs-rocker-transition-vs-state] for more
information.

In the future, the encoding of two buttons transitions (second action) could be
added (not that useful though).
""".
-spec encode_double_rocker_switch_telegram( eurid(), application_style(),
		button_locator(), button_transition(), option( eurid() ) ) ->
											telegram().
encode_double_rocker_switch_telegram( SourceEurid, SourceAppStyle,
		ButtonLocator, ButtonTransition, MaybeTargetEurid ) ->

	% No EEP to be determined from double_rocker_switch (implicit in packet).

	% Best understood backwards, from the end of this function.

	RadioPacketType = radio_erp1_type,

	% Must be defined to F6 here:
	RorgNum = oceanic_generated:get_maybe_second_for_rorg( _Rorg=rorg_rps ),
	%RorgNum = 16#f6,

	R1Enum = get_located_button_enum( ButtonLocator, SourceAppStyle ),

	EB = get_button_transition_enum( ButtonTransition ),

	IsSecondActionValid = false,

	% Semantics of R2Enum/SA unclear; we thought R2 was meaningless if SA
	% (second action) was invalid, but visibly it matters:

	%R2Enum = R1Enum,

	% Test, about a button that should be ignored:

	% This results in enum = 2, and the telegram had no effect:
	%R2Enum = get_designated_button_enum( button_bi, SourceAppStyle ),

	% This results in enum = 1, and the telegram had no effect:
	%R2Enum = R1Enum,

	% enum = 0 works on our test for some reason:
	R2Enum = 0,

	SA = case IsSecondActionValid of

		true ->
			1;

		false ->
			0

	end,

	%trace_bridge:debug_fmt( "encode_double_rocker_switch_telegram: "
	%   "R1Enum = ~B, R2Enum = ~B.", [ R1Enum, R2Enum ] ),

	% No LRN (Learn) bit for RPS, which can only send data and has no special
	% telegram modification to teach-in the device. Therefore, the teach-in
	% procedure takes place manually on the actuator/controller through a normal
	% data telegram. The EEP profile must be manually supplied to the controller
	% per sender ID.
	%
	DB_0 = <<R1Enum:3, EB:1, R2Enum:3, SA:1>>,

	T21 = 1,
	NU = 1,

	% Apparently Repeater Count (see `[EEP-gen]` p.14); non-zero deemed safer,
	% yet zero already works, so:

	%RC = 1,
	RC = 0,

	_A=1,
	_B=0,

	Status = <<0:2, T21:1, NU:1, RC:4>>,
	%Status = <<A:1, B:1, T21:1, NU:1, RC:4>>,
	%Status = <<16#20>>,

	Data = <<RorgNum:8, DB_0:1/binary, SourceEurid:32, Status:1/binary>>,

	MaybeOptData = get_optional_data_for_sending( MaybeTargetEurid ),

	cond_utils:if_defined( oceanic_debug_decoding,
		begin
			<<DB_0AsInt>> = DB_0,
			<<StatusAsInt>> = Status,
			PadWidth = 8,
			trace_bridge:debug_fmt(
				"Generated packet: type=~ts RORG=~ts, DB_0=~ts, "
				"data size=~B, optional data size=~B, status=~ts.",
				[ RadioPacketType, text_utils:integer_to_hexastring( RorgNum ),
				  text_utils:integer_to_bits( DB_0AsInt, PadWidth ),
				  size( Data ), size( MaybeOptData ),
				  text_utils:integer_to_bits( StatusAsInt, PadWidth ) ] )
		end ),

	encode_esp3_packet( RadioPacketType, Data, MaybeOptData ).



-doc """
Encodes a double-rocker multipress telegram, from the specified device to the
specified one (if any), reporting the specified transition for the specified
button.

Event sent in the context of EEP F6-02-01 or F6-02-02 (`"Light and Blind Control
- Application Style 1 or 2"`), for `T21=1`. It results thus in a RPS telegram,
an ERP1 radio packet encapsulated into an ESP3 one.

See `[EEP-spec]` p.15 and its decode_rps_double_rocker_packet/7 counterpart.
""".
-spec encode_double_rocker_multipress_telegram( eurid(), option( eurid() ),
		button_counting(), button_transition() ) -> telegram().
encode_double_rocker_multipress_telegram( SourceEurid, ButtonCounting,
		ButtonTransition, MaybeTargetEurid ) ->

	% No EEP to be determined from double_rocker_multipress (implicit in
	% packet).

	% Best understood backwards, from the end of this function.

	RadioPacketType = radio_erp1_type,

	% Must be F6 here:
	RorgNum = oceanic_generated:get_second_for_rorg( _Rorg=rorg_rps ),
	%RorgNum = 16#f6,

	CountEnum = case ButtonCounting of

		none ->
			0;

		three_or_four ->
			3

	end,


	EB = get_button_transition_enum( ButtonTransition ),

	% No LRN (Learn) bit for RPS, which can only send data and has no special
	% telegram modification to teach-in the device. Therefore, the teach-in
	% procedure takes place manually on the actuator/controller through a normal
	% data telegram. The EEP profile must be manually supplied to the controller
	% per sender ID.
	%
	DB_0 = <<CountEnum:3, EB:1, 0:4>>,

	T21 = 1,
	NU = 0,

	% Apparently Repeater Count (see [EEP-gen] p.14); non-zero deemed safer:
	%RC = 1,
	RC = 0,

	_A=1,
	_B=0,

	Status = <<0:2, T21:1, NU:1, RC:4>>,
	%Status = <<A:1, B:1, T21:1, NU:1, RC:4>>,

	Data = <<RorgNum:8, DB_0:1/binary, SourceEurid:32, Status:1/binary>>,

	MaybeOptData = get_optional_data_for_sending( MaybeTargetEurid ),

	cond_utils:if_defined( oceanic_debug_decoding,
		begin
			<<DB_0AsInt>> = DB_0,
			<<StatusAsInt>> = Status,
			PadWidth = 8,
			trace_bridge:debug_fmt(
				"Generated packet: type=~ts RORG=~ts, DB_0=~ts, "
				"data size=~B, optional data size=~B, status=~ts.",
				[ RadioPacketType, text_utils:integer_to_hexastring( RorgNum ),
				  text_utils:integer_to_bits( DB_0AsInt, PadWidth ),
				  size( Data ), size( MaybeOptData ),
				  text_utils:integer_to_bits( StatusAsInt, PadWidth ) ] )
		end ),

	% Radio, hence 1 here:
	encode_esp3_packet( RadioPacketType, Data, MaybeOptData ).



-doc """
Encodes a successful Teach-In Response - UTE Message telegram (Addressed / CMD:
0x1).

This message is the reply to an EEP Teach-In Query message that was sent by an
initiator device (e.g. a smart plug, to register any new controller of it).

The content to echo is the 6 last bytes of the teach-in query, i.e. `DB_{5..0}`.

The returned telegram allows the sender (the home automation gateway),
supposedly in learn mode, to be registered by the learning device as a
controller thereof.

Refer to `[EEP-spec]` p.255 for more information.
""".
-spec encode_teach_in_response( teach_outcome(), eurid(), eurid(),
    communication_direction(), telegram_chunk() ) -> telegram().
encode_teach_in_response( TeachOutcome, InitiatorEurid, EmitterEurid,
                          CommDirection, EchoContent ) ->

    cond_utils:if_defined( oceanic_debug_teaching, trace_bridge:debug_fmt(
        "Encoding teach-in response: outcome is ~p, initiator EURID is ~ts, "
        "communication  direction is ~p, echoed content is ~w.",
        [ TeachOutcome, oceanic_text:eurid_to_string( InitiatorEurid ),
          CommDirection, EchoContent ] ) ),

	RorgNum = oceanic_generated:get_maybe_second_for_rorg( _Rorg=rorg_ute ),

    % Supposedly not bidirectional, generally:
	CommDir = case CommDirection of

		unidirectional ->
			0;

		bidirectional ->
			1

	end,

    Unused = 0,

	ReqOutcome = case TeachOutcome of

		teach_refused ->
			0;

		teach_in_accepted ->
			1;

        % No teach_out_accepted here:
		%teach_out_accepted ->
		%   2;

		teach_eep_unsupported ->
			3

	end,

    % Command identifier (CMD) / 0x1: EEP Teach-In:
    CmdId = 1,

    DB6 = <<CommDir:1, Unused:1, ReqOutcome:2, CmdId:4>>,

    Status = 0,

	% DB5.7 to DB0.0 has the same structure as Teach-in-Query, and contents are
	% echoed back (the two final fields had to be retro-engineered from
	% https://github.com/kipe/enocean/blob/master/enocean/protocol/packet.py#L410
	% as we were not able to find them aywhere in the specs):
	%
	Data = <<RorgNum:8, DB6/binary, EchoContent/binary, EmitterEurid:32,
             Status:8>>,

    % 1+1+6+4+1=13:
    cond_utils:assert( oceanic_check_teaching, size( Data ) =:= 13),

    % The target device is the teach-initiating one:
	OptData = get_optional_data_for_sending( _TargetEurid=InitiatorEurid ),

	encode_esp3_packet( _RadioPacketType=radio_erp1_type, Data, OptData ).



-doc """
Encodes a telegram to set the output of a switch/dimmer.

It is based on the `"CMD 0x1 - Actuator Set Output"` command (see `[EEP]`
p.132).

If this telegram is to be addressed (to a particular device, as opposed to being
broadcast), the target device (e.g. a smart plug) must have already registered
(through teach-in) the emitter (typically this gateway).
""".
-spec encode_switch_dimmer_set_output( eurid(), device_status(),
                                       option( eurid() ) ) -> telegram().
encode_switch_dimmer_set_output( SourceEurid, TargetStatus,
                                 MaybeTargetEurid ) ->

    cond_utils:if_defined( oceanic_debug_encoding,
        trace_bridge:debug_fmt( "Encoding a switch/dimmer set output telegram, "
            "to be sent by ~ts to ~ts, for a target status ~ts.",
            [ oceanic_text:eurid_to_string( SourceEurid ),
              oceanic_text:eurid_to_string( MaybeTargetEurid ),
              TargetStatus ] ) ),

	RorgNum = oceanic_generated:get_maybe_second_for_rorg( _Rorg=rorg_vld ),


    CmdId = 1,

    DB_2 = <<0:4, CmdId:4 >>,


    DimValue = 0, % 0x00:Switch to new output value

    IOChannels = 16#1e, % All output channels supported by the device

    DB_1 = <<DimValue:3, IOChannels:5>>,


    OutputValue = case TargetStatus of

        on ->
            16#64; % 100% / on

        off ->
            0

    end,

    DB_0 = <<0:1, OutputValue:7>>,

    Status = 0,

	Data = <<RorgNum:8, DB_2:1/binary, DB_1:1/binary, DB_0:1/binary,
             SourceEurid:32, Status:8>>,

	MaybeOptData = get_optional_data_for_sending( MaybeTargetEurid ),

	encode_esp3_packet( _RadioPacketType=radio_erp1_type, Data, MaybeOptData ).



-doc """
Returns the optional data suitable for sending to the specified device (if any).

This is ADT (Adressed Communication, as opposed to broadcast).
""".
-spec get_optional_data_for_sending( option( eurid() ) ) -> telegram_opt_data().
get_optional_data_for_sending( _MaybeTargetEurid=undefined ) ->
	<<>>;

get_optional_data_for_sending( TargetEurid ) ->

	% We are sending here:
	%SubTelNum = 1,
	SubTelNum = 3,

	DBm = 16#ff,
	SecurityLevel = 0,

	_OptData= <<SubTelNum:8, TargetEurid:32, DBm:8, SecurityLevel:8>>.



% Section for encoding helpers.




-doc "Returns the enumeration of the designated button.".
-spec get_designated_button_enum( button_designator(), application_style() ) ->
										enum().
get_designated_button_enum( _Des=button_ai, _AppStyle=1 ) ->
	0; % Button A, bottom

get_designated_button_enum( _Des=button_ao, _AppStyle=1 ) ->
	1; % Button A, top

get_designated_button_enum( _Des=button_bi, _AppStyle=1 ) ->
	2; % Button B, bottom

get_designated_button_enum( _Des=button_bo, _AppStyle=1 ) ->
	3; % Button B, top

get_designated_button_enum( _Des=button_ao, _AppStyle=2 ) ->
	0; % Button A, bottom

get_designated_button_enum( _Des=button_ai, _AppStyle=2 ) ->
	1; % Button A, top

get_designated_button_enum( _Des=button_bo, _AppStyle=2 ) ->
	2; % Button B, bottom

get_designated_button_enum( _Des=button_bi, _AppStyle=2 ) ->
	3. % Button B, top



-doc "Returns the enumeration corresponding to the specified button.".
-spec get_located_button_enum( button_locator(), application_style() ) ->
													enum().
% Application Style 1: O is top.
get_located_button_enum( _Loc={ _Channel=1, _Pos=top }, _AppStyle=1 ) ->
	1;

get_located_button_enum( _Loc={ _Channel=1, _Pos=bottom }, _AppStyle=1 ) ->
	0;

get_located_button_enum( _Loc={ _Channel=2, _Pos=top },  _AppStyle=1 ) ->
	3;

get_located_button_enum( _Loc={ _Channel=2, _Pos=bottom }, _AppStyle=1 ) ->
	2;

% Application Style 2: I is bottom.
%
% Updated after the actual readings from a Nodon CRC-2-6-04:
get_located_button_enum( _Loc={ _Channel=1, _Pos=top }, _AppStyle=2 ) ->
	0;

get_located_button_enum( _Loc={ _Channel=1, _Pos=bottom }, _AppStyle=2 ) ->
	1;

get_located_button_enum( _Loc={ _Channel=2, _Pos=top }, _AppStyle=2 ) ->
	2;

get_located_button_enum( _Loc={ _Channel=2, _Pos=bottom }, _AppStyle=2 ) ->
	3.


-doc "Returns the enumeration of the specified button transition.".
-spec get_button_transition_enum( button_transition() ) -> enum().
get_button_transition_enum( released ) ->
	_EnergyBow=0;

get_button_transition_enum( pressed ) ->
	_EnergyBow=1.
