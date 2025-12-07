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

-module(oceanic_decode).

-moduledoc """
Module centralising **all decoding** made by Ceylan-Oceanic.
""".


% For the records and defines:
-include("oceanic.hrl").
-include("oceanic_internal.hrl").


-doc "The various kinds of errors that may happen when decoding.".
% 'unresolved*' now managed separately, as an event:
-type decoding_error() ::
    'not_reached'   % Beginning of next packet still ahead
  | 'incomplete'    % Truncated packet (end of packet still ahead)
  | 'invalid'       % Corrupted packet
  | 'unsupported'.  % Type of packet (currently) unsupported by Oceanic



-doc """
The outcome of a decoding.

The outcome of an attempt of integrating / decoding a telegram chunk.

A maybe-event was returned, as for example a common command response that is
received whereas no request was sent shall be discarded.

A maybe-discovery origin is also returned, so that an already discovered device
is not reported as being discovered more than once (`undefined` is returned once
already discovered).

Note that it is expected (see the use of `type_utils:get_last_tuple_element/1`)
that the last element of the returned tuples is the Oceanic state.
""".
-type decoding_outcome() ::

    { 'decoded', device_event() | 'command_processed',
      MaybeDiscoverOrigin :: option( discovery_origin() ),
      IsBackOnline :: boolean(), MaybeDevice :: option( enocean_device() ),
      oceanic_state() }

    % Device not configured and EEP cannot be determined, when first seen:
  | { 'unresolved_first_seen', unresolved_device_event(), enocean_device(),
      ToSkipLen :: count(), NextMaybeTelTail :: option( telegram_tail() ),
      oceanic_state() }

    % Device not configured and EEP cannot be determined, once already seen:
    % (even less interest in returning the corresponding device)
  | { 'unresolved', unresolved_device_event(), ToSkipLen :: count(),
      NextMaybeTelTail :: option( telegram_tail() ), oceanic_state() }

  | { decoding_error(), ToSkipLen :: count(),
      NextMaybeTelTail :: option( telegram_tail() ), oceanic_state() }.



-doc "The result of a decoding request.".
-type decoding_result() :: decoding_error() | device_event().


-export_type([ decoding_error/0, decoding_outcome/0, decoding_result/0 ]).


-export([ decode_packet/5 ]).


% Exported only for testing:
-export([ decode_telegram/2 ]).



% Module-internal defines.


% For DB_0 for example, 8 bits:
%  * bit name:   B7 - B6 - B5 - B4 - B3 - B2 - B1 - B0
%  * bit offset:  0 -  1 -  2 -  3 -  4 -  5 -  6 -  7
%
% So the following bit masks:

-define( b0, 2#00000001 ).
-define( b1, 2#00000010 ).
-define( b2, 2#00000100 ).
-define( b3, 2#00001000 ).
-define( b4, 2#00010000 ).
-define( b5, 2#00100000 ).
-define( b6, 2#01000000 ).
-define( b7, 2#10000000 ).



% Type shorthands:

-type count() :: basic_utils:count().
-type uint8() :: type_utils:uint8().

-type ustring() ::text_utils:ustring().

-type eurid() :: oceanic:eurid().
-type device_event() :: oceanic:device_event().
-type unresolved_device_event() :: oceanic:unresolved_device_event().
-type discovery_origin() :: oceanic:discovery_origin().
-type enocean_device() :: oceanic:enocean_device().
-type telegram() :: oceanic:telegram().
-type telegram_chunk() :: oceanic:telegram_chunk().
-type telegram_tail() :: oceanic:telegram_tail().
-type telegram_data() :: oceanic:telegram_data().
-type telegram_data_tail() :: oceanic:telegram_data_tail().
-type telegram_opt_data() :: oceanic:telegram_opt_data().
-type decoded_optional_data() :: oceanic:decoded_optional_data().
-type oceanic_state() :: oceanic:oceanic_state().
-type oceanic_server_pid() :: oceanic:oceanic_server_pid().
-type packet_type() :: oceanic:packet_type().
-type vld_payload() :: oceanic:vld_payload().
-type dbm():: oceanic:dbm().
-type security_level() :: oceanic:security_level().
-type subtelegram_count() :: oceanic:subtelegram_count().
-type enum() :: oceanic:enum().
-type application_style() :: oceanic:application_style() .
-type button_locator() :: oceanic:button_locator().
-type button_designator() :: oceanic:button_designator().
-type button_transition() :: oceanic:button_transition().
-type ptm_switch_module_type() :: oceanic:ptm_switch_module_type().
-type nu_message_type() :: oceanic:nu_message_type() .
-type repetition_count() :: oceanic:repetition_count().
-type power_report() :: oceanic:power_report().



-doc """
Returns, if possible, the specified telegram once decoded as an event, using the
specified Oceanic server for that.

Mostly useful for testing purpose.
""".
-spec decode_telegram( telegram(), oceanic_server_pid() ) -> decoding_result().
decode_telegram( Telegram, OcSrvPid ) ->
    OcSrvPid ! { decodeOceanic, Telegram, self() },
    receive

        { decoding_result, R } ->
            R

    end.


-doc """
Decodes the specified packet, based on the specified data elements.

Data corresponds to the actual packet payload of the specified type.
""".
-spec decode_packet( packet_type(), telegram_data(), telegram_opt_data(),
            option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
% Clause only for ERP1 packets (e.g. not covering responses):
decode_packet( _PacketType=radio_erp1_type,
               _Data= <<RorgNum:8, DataTail/binary>>, OptData, NextMaybeTelTail,
               State ) ->

    MaybeRorg = oceanic_generated:get_maybe_first_for_rorg( RorgNum ),

    cond_utils:if_defined( oceanic_debug_decoding,
        trace_bridge:debug_fmt( "Decoding an ERP1 radio packet of R-ORG ~ts, "
            "hence ~ts, i.e. '~ts'...",
            [ text_utils:integer_to_hexastring( RorgNum ), MaybeRorg,
              oceanic_generated:get_maybe_second_for_rorg_description(
                MaybeRorg ) ] ) ),

    case MaybeRorg of

        rorg_rps ->
            decode_rps_packet( DataTail, OptData, NextMaybeTelTail, State );

        rorg_1bs ->
            decode_1bs_packet( DataTail, OptData, NextMaybeTelTail, State );

        rorg_4bs ->
            decode_4bs_packet( DataTail, OptData, NextMaybeTelTail, State );

        rorg_ute ->
            decode_ute_packet( DataTail, OptData, NextMaybeTelTail, State );

        rorg_vld ->
            decode_vld_packet( DataTail, OptData, NextMaybeTelTail, State );

        undefined ->
            trace_bridge:warning_fmt( "The RORG ~ts is not known, "
                "the corresponding packet is thus dropped.",
                [ text_utils:integer_to_hexastring( RorgNum ) ] ),

            % Not even an EURID to track.

            { unsupported, _ToSkipLen=0, NextMaybeTelTail, State };

        Rorg ->
            trace_bridge:warning_fmt( "The decoding of ERP1 radio packets "
                "of R-ORG ~ts, hence ~ts (i.e. '~ts') is not implemented; "
                "the corresponding packet is thus dropped.",
                [ text_utils:integer_to_hexastring( RorgNum ), Rorg,
                  oceanic_generated:get_maybe_second_for_rorg_description(
                    Rorg ) ] ),

            % Not even an EURID to track.

            { unsupported, _ToSkipLen=0, NextMaybeTelTail, State }

    end;


% Here a response is received whereas no command was issued:
decode_packet( _PacketType=response_type, Data, OptData, NextMaybeTelTail,
               State=#oceanic_state{ waited_command_info=undefined,
                                     discarded_count=DiscCount } ) ->

    trace_bridge:warning_fmt( "Received a command response "
        "(data: ~w, optional data: ~w) whereas there is no pending request, "
        "dropping it.", [ Data, OptData ] ),

    % (timer already cancelled by caller)

    { decoded, _MaybeDeviceEvent=command_processed,
      _MaybeDiscoverOrigin=undefined, _IsBackOnline=false,
      _MaybeDevice=undefined, NextMaybeTelTail,
      State#oceanic_state{ discarded_count=DiscCount+1 } };


% Response received, presumably for this pending (possibly internal) command:
decode_packet( _PacketType=response_type,
               _Data= <<ReturnCode:8, DataTail/binary>>, OptData,
               NextMaybeTelTail,
               State=#oceanic_state{
                    waited_command_info={ WaitedCmdTrk, MaybeTimerRef } } ) ->

    cond_utils:if_defined( oceanic_debug_decoding,
        trace_bridge:debug_fmt( "Decoding a command response, whereas "
            "awaiting ~ts.",
            [ oceanic_text:command_tracking_to_string( WaitedCmdTrk ) ] ) ),

    % In all cases the pending request is over:
    oceanic:stop_any_timer( MaybeTimerRef ),

    RespState = State#oceanic_state{ waited_command_info=undefined },

    DecodingOutcome = case oceanic_generated:get_maybe_first_for_return_code(
            ReturnCode ) of

        undefined ->
            trace_bridge:warning_fmt( "Unable to decode response whose return "
                "code is invalid (~B), dropping packet and pending "
                "command (#~B).",
                [ ReturnCode, State#oceanic_state.command_count ] ),
            { invalid, _ToSkipLen=0, NextMaybeTelTail, RespState };

        ok_return ->
            oceanic_common_command:decode_response_tail( WaitedCmdTrk,
                DataTail, OptData, NextMaybeTelTail, RespState );

        % Not a decoding failure, but more a protocol-level one that shall be
        % notified to the requester.
        %
        % Expected in [error_return, unsupported_return,
        %              wrong_parameter_return, operation_denied]:
        %
        % (a bit like notify_requester/4)
        %
        FailureReturn ->
            oceanic_common_command:manage_failure_return( FailureReturn,
                WaitedCmdTrk, DataTail, OptData, NextMaybeTelTail, RespState )

    end,

    % Maybe then a new command can be launched:

    DecodedState = type_utils:get_last_tuple_element( DecodingOutcome ),

    NextState = oceanic:handle_next_command(
        DecodedState#oceanic_state.command_queue, DecodedState ),

    % Recreate a proper decoding outcome:
    type_utils:set_last_tuple_element( DecodingOutcome, NextState );




decode_packet( PacketType, _Data, _OptData, NextMaybeTelTail, State ) ->

    trace_bridge:warning_fmt( "Unsupported packet type '~ts' (hence ignored).",
                              [ PacketType ] ),

    % Not even an EURID to track, no real need to go further.

    { unsupported, _ToSkipLen=0, NextMaybeTelTail, State }.



-doc """
Decodes a rorg_rps (F6, "Repeated Switch Communication") packet.

If the RORG value (here "F6", RPS) is specified in the packet, FUNC and TYPES
are not, hence the full, precise EEP of the packet cannot be determined from the
telegram; extra device information must thus be available (typically specified
out of band, in a configuration file) is order to decode it.

Supported:

- F6-01 corresponds to simple "Switch Buttons" (with no rocker, hence with
punctual press/release events), described here as "push buttons"

- F6-02: Rocker Switch, 2 Rocker: each rocker (A or B) has a top and a bottom
button; pressing one sends a `double_rocker_switch_event/0` telling that a given
button (possibly both) is/are being pressed, and releasing it/them sends a
`double_rocker_multipress_event/0` telling "no button released simultaneously"

Support to be added:
- F6-03: Rocker Switch, 4 Rocker
- F6-04: Position Switch, Home and Office Application
- F6-05: Detectors
- F6-10: Mechanical Handle

Discussed a bit in `[ESP3]` "2.1 Packet Type 1: RADIO_ERP1", p. 18, and in
`[EEP-spec]` p. 11.

See `decode_1bs_packet/3` for more information.
""".
-spec decode_rps_packet( telegram_data_tail(), telegram_opt_data(),
        option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
decode_rps_packet( _DataTail= <<DB_0:1/binary, SenderEurid:32,
                   Status:1/binary>>, OptData, NextMaybeTelTail,
                   State=#oceanic_state{ device_table=DeviceTable } ) ->

    % We have to know the specific (main) EEP of this device in order to decode
    % this telegram:
    %
    case table:lookup_entry( SenderEurid, DeviceTable ) of

        % Device first time seen:
        key_not_found ->

            % Not trying to decode optional data then; a priori unable to infer
            % any actual EEP (multiple ones correspond):
            %
            { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
              _ListeningDiscoverOrigin, _IsBackOnline, UndefinedDeviceName,
              UndefinedDeviceShortName, _MaybeEepId } =
                  oceanic:record_device_failure( SenderEurid, DeviceTable,
                      _MaybeInferredEepId=undefined ),

            trace_bridge:warning_fmt( "Unable to decode a RPS (F6) packet ~ts",
                [ get_unresolved_device_message( SenderEurid ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            MaybeDecodedOptData = decode_optional_data( OptData ),

            { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
                resolve_maybe_decoded_data( MaybeDecodedOptData ),

            UnresolvedEvent = #unresolved_device_event{
                source_eurid=SenderEurid,
                name=UndefinedDeviceName,
                short_name=UndefinedDeviceShortName,
                timestamp=Now,
                last_seen=MaybeLastSeen,
                subtelegram_count=MaybeTelCount,
                destination_eurid=MaybeDestEurid,
                dbm=MaybeDBm,
                security_level=MaybeSecLvl,
                type_hint=rorg_rps },

            { unresolved_first_seen, UnresolvedEvent, NewDevice, _ToSkipLen=0,
              NextMaybeTelTail, NewState };


        % Knowing the actual EEP is needed in order to decode; here also unable
        % to infer it:
        %
        { value, _Device=#enocean_device{ eep=undefined } } ->

            % Not trying to decode optional data then.

            { NewDeviceTable, _NewDevice, Now, MaybeLastSeen,
              _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
              MaybeDeviceShortName, _UndefinedEepId } =
                  oceanic:record_device_failure( SenderEurid, DeviceTable,
                      _MaybeInferredEepId=undefined ),

            % Device probably already seen, so just a debug trace:
            trace_bridge:debug_fmt( "Unable to decode a RPS packet "
                "for ~ts: no EEP known for it (hence ignored).",
                [ oceanic_text:get_best_naming( MaybeDeviceName,
                      MaybeDeviceShortName, SenderEurid ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            MaybeDecodedOptData = decode_optional_data( OptData ),

            { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
                resolve_maybe_decoded_data( MaybeDecodedOptData ),

            UnresolvedEvent = #unresolved_device_event{
                source_eurid=SenderEurid,
                name=MaybeDeviceName,
                short_name=MaybeDeviceShortName,
                timestamp=Now,
                last_seen=MaybeLastSeen,
                subtelegram_count=MaybeTelCount,
                destination_eurid=MaybeDestEurid,
                dbm=MaybeDBm,
                security_level=MaybeSecLvl,
                type_hint=rorg_rps },

            { unresolved, UnresolvedEvent, _ToSkipLen=0, NextMaybeTelTail,
              NewState };


        { value, Device=#enocean_device{ eep=double_rocker_switch_style_1 } } ->
            decode_rps_double_rocker_packet( DB_0, SenderEurid, Status,
                OptData, NextMaybeTelTail, Device, State );

        % Same as previous:
        { value, Device=#enocean_device{ eep=double_rocker_switch_style_2 } } ->
            decode_rps_double_rocker_packet( DB_0, SenderEurid, Status,
                OptData, NextMaybeTelTail, Device, State );

        { value, Device=#enocean_device{ eep=push_button } } ->
            decode_rps_push_button_packet( DB_0, SenderEurid, Status,
                OptData, NextMaybeTelTail, Device, State );

        % Some smart plugs may send such packets, with EEP D2-01-0A:
        % <<85,0,7,7,1,122,246,80,255,136,14,128,48,1,255,255,255,255,57,0,218>>
        { value, Device=#enocean_device{ eep=smart_plug } } ->
            %decode_rps_push_button_packet( DB_0, SenderEurid, Status,
            decode_rps_double_rocker_packet( DB_0, SenderEurid, Status,
                OptData, NextMaybeTelTail, Device, State );


        { value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

            % Not trying to decode optional data then; EEP already known:
            { NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
              _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
              MaybeDeviceShortName, _UnsupportedEepId } =
                oceanic:record_device_failure( SenderEurid, DeviceTable,
                      _MaybeInferredEepId=undefined ),

            % Device probably already seen:
            trace_bridge:warning_fmt( "Unable to decode a RPS (F6) packet "
                "for ~ts: EEP ~ts (~ts) not supported.",
                [ oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ),
                  UnsupportedEepId,
                  oceanic_generated:get_maybe_second_for_eep_strings(
                    UnsupportedEepId ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            { unsupported, _ToSkipLen=0, NextMaybeTelTail, NewState }

    end.



-doc """
Decodes a rorg_rps single_input_contact (F6-01, simple "Switch Buttons") packet;
in practice, only "F6-01-01" ("Push Button") exists.

This corresponds to simple "Switch Buttons" (with no rocker, hence with punctual
press/release events).

Discussed a bit in `[ESP3]` "2.1 Packet Type 1: RADIO_ERP1", p. 18, and in
`[EEP-spec]` p. 15.
""".
-spec decode_rps_push_button_packet( telegram_chunk(), eurid(),
    telegram_chunk(), telegram_opt_data(), option( telegram_tail() ),
    enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_rps_push_button_packet( DB_0= <<DB_0AsInt:8>>, SenderEurid,
        Status, OptData, NextMaybeTelTail, Device,
        State=#oceanic_state{ device_table=DeviceTable } ) ->

    ButtonTransition = case DB_0AsInt band ?b4 =:= 0 of

        true ->
            just_released;

        false ->
            just_pressed

    end,

    % (no learn bit in DB_0)

    % All other bits than b4 shall be 0:
    %cond_utils:assert( oceanic_check_decoding,
    %                  % Superfluous parentheses:
    %                  DB_0AsInt band ( bnot ?b4 ) =:= 0 ),

    % Apparently we can have DB_0 equal to 0b101-0000, whereas the first 1 shall
    % be 0 according to [EEP-spec] p. 15.
    %
    cond_utils:if_defined( oceanic_debug_decoding,
        begin
            OtherBits = DB_0AsInt band ( bnot ?b4 ),
            OtherBits =:= 0 orelse
                trace_bridge:warning_fmt( "Unexpected bits when decoding "
                    "push-button packet based on DB_0=~ts: ~ts.",
                    [ text_utils:integer_to_bits( DB_0AsInt ),
                      text_utils:integer_to_bits( OtherBits ) ] )
        end ),

    { PTMSwitchModuleType, NuType, RepCount } = get_rps_status_info( Status ),

    % EEP was known, hence device was already known as well:
    { NewDeviceTable, NewDevice, Now, MaybeLastSeen, UndefinedDiscoverOrigin,
      IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
                                             _InferredEepId=push_button ),

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        trace_bridge:debug_fmt( "Decoding a R-ORG RPS packet, "
            "with DB_0=~w; sender is ~ts, PTM switch module is ~ts, "
            "NU message type is ~ts, ~ts~ts.",
            [ DB_0,
              oceanic_text:get_best_naming( MaybeDeviceName,
                MaybeDeviceShortName, SenderEurid ),
              oceanic_text:ptm_module_to_string( PTMSwitchModuleType ),
              oceanic_text:nu_message_type_to_string( NuType ),
              oceanic_text:repeater_count_to_string( RepCount ),
              oceanic_text:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                          OptData ) ] ),
        basic_utils:ignore_unused(
            [ DB_0, PTMSwitchModuleType, NuType, RepCount ] ) ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    Event = #push_button_switch_event{ source_eurid=SenderEurid,
                                       name=MaybeDeviceName,
                                       short_name=MaybeDeviceShortName,
                                       eep=MaybeEepId,
                                       timestamp=Now,
                                       last_seen=MaybeLastSeen,
                                       subtelegram_count=MaybeTelCount,
                                       destination_eurid=MaybeDestEurid,
                                       dbm=MaybeDBm,
                                       security_level=MaybeSecLvl,
                                       transition= ButtonTransition },

    { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, NewState }.



-doc """
Decodes a rorg_rps F6-02-0{1,2}, "Light and Blind Control - Application Style 1
or 2" packet (switch or multipress).

It may contain 2 actions.

Discussed a bit in `[ESP3]` "2.1 Packet Type 1: RADIO_ERP1", p. 18, and in
`[EEP-spec]` p. 15.

Apparently, when pressing a button (e.g. AO in application style 1, i.e. the top
left one) of a double rocker (EEP F6-02-01, be it an Avidsen, an O2Line, a
TRIO2SYS one):

- first, when it is pressed, a packet corresponding to
  `{button_ao,just_pressed}` (just below, for T21=1 and NU=1) is sent, resulting
  in a double_rocker_switch_event

- then when it is released, a packet corresponding to `{none,just_released}`
  (T21=1 and NU=0, still below) is sent, resulting in a
  double_rocker_multipress_event

If instead two buttons (typically AO and BO) were simultaneously pressed then
released, then `{button_ao,just_pressed},{button_bo,just_pressed}}` would be
sent, and then `{none,just_released}`.
""".
-spec decode_rps_double_rocker_packet( telegram_chunk(), eurid(),
        telegram_chunk(), telegram_opt_data(), option( telegram_tail() ),
        enocean_device(), oceanic_state() ) -> decoding_outcome().

% No EEP a priori known here, thus unable to know the application style and thus
% to decode that telegram:
%
decode_rps_double_rocker_packet( _DB_0, SenderEurid,
       _Status, OptData, NextMaybeTelTail,
       _Device=#enocean_device{ eep=undefined },
       State=#oceanic_state{ device_table=DeviceTable } ) ->

    { NewDeviceTable, _NewDevice, Now, MaybeLastSeen, _MaybeDiscoverOrigin,
      _IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, _UndefinedEepId } =
        oceanic:record_device_failure( SenderEurid, DeviceTable,
                                       _MaybeInferredEepId=undefined ),

    trace_bridge:warning_fmt( "Received a RPS telegram from device of "
        "EURID ~ts, but it could not be decoded short of knowing the EEP "
        "that it implements.",
        [ oceanic_text:eurid_to_string( SenderEurid ) ] ),

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    UnresolvedEvent = #unresolved_device_event{
        source_eurid=SenderEurid,
        name=MaybeDeviceName,
        short_name=MaybeDeviceShortName,
        timestamp=Now,
        last_seen=MaybeLastSeen,
        subtelegram_count=MaybeTelCount,
        destination_eurid=MaybeDestEurid,
        dbm=MaybeDBm,
        security_level=MaybeSecLvl,
        type_hint=rorg_rps_double_rocker },

    { unresolved, UnresolvedEvent, _ToSkipLen=0, NextMaybeTelTail, NewState };


decode_rps_double_rocker_packet( DB_0= <<_DB_0AsInt:8>>, SenderEurid,
       % (T21 is at offset 2, thus b5; NU at offset 3, thus b4)
       _Status= <<_:2, T21:1, NU:1, _:4>>, OptData,
       NextMaybeTelTail, Device=#enocean_device{ eep=EepId },
       State=#oceanic_state{ device_table=DeviceTable } ) ->

   AppStyle = oceanic:get_app_style_from_eep( EepId ),

   cond_utils:if_defined( oceanic_debug_decoding, trace_bridge:debug_fmt(
         "Decoding a RPS F6-02-01/02 double-rocker switch from ~ts; "
         "app style: ~w, T21: ~w, NU: ~w.",
         [ oceanic_text:eurid_to_string( SenderEurid ), AppStyle, T21,
           NU ] ) ),

   case { T21, NU } of

       { _T21=1, _NU=1 } ->

           <<R1Enum:3, EB:1, R2Enum:3, SA:1>> = DB_0,

           FirstButtonLocator = get_button_locator( R1Enum, AppStyle ),

           ButtonTransition = get_button_transition( EB ),

           SecondButtonLocator = get_button_locator( R2Enum, AppStyle ),

           % Spec unclear at least about the second action: supposing it is
           % valid, does it describe a state or a transition/action? Is the
           % second rocker pressed or released?

           IsSecondActionValid = case SA of

               0 ->
                   false;

               1 ->
                   true

           end,

           % EEP was known, hence device already was known as well:
           { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
             UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName,
             MaybeDeviceShortName, MaybeEepId } =
                 oceanic:record_known_device_success( Device, DeviceTable,
                    _AlreadyKnownEepId=undefined ),

           RecState = State#oceanic_state{ device_table=NewDeviceTable },

           MaybeDecodedOptData = decode_optional_data( OptData ),

           { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
               resolve_maybe_decoded_data( MaybeDecodedOptData ),

             % Examining whether this telegram may be received in answer to a
             % past (smart-plug related, switching for Eltako) request, which
             % can thus be acknowledged and unmonitored:
             %
             ReqState = handle_possible_eltako_switching_request_answer(
                 NewDevice, FirstButtonLocator, ButtonTransition, SenderEurid,
                 RecState ),

           Event = #double_rocker_switch_event{
               source_eurid=SenderEurid,
               name=MaybeDeviceName,
               short_name=MaybeDeviceShortName,
               eep=MaybeEepId,
               timestamp=Now,
               last_seen=MaybeLastSeen,
               subtelegram_count=MaybeTelCount,
               destination_eurid=MaybeDestEurid,
               dbm=MaybeDBm,
               security_level=MaybeSecLvl,
               first_action_button=FirstButtonLocator,
               first_designator=get_button_designator( R1Enum ),
               energy_bow=ButtonTransition,
               second_action_button=SecondButtonLocator,
               second_designator=get_button_designator( R2Enum ),
               second_action_valid=IsSecondActionValid },

           { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
             NextMaybeTelTail, ReqState };


       { _T21=1, _NU=0 } ->

           % The 4 last bits shall be 0:
           %cond_utils:assert( oceanic_check_decoding,
           %                   B_0AsInt band 2#00001111 =:= 0 ),

           <<R1:3, EB:1, _:4>> = DB_0,

           MaybeButtonCounting = case R1 of

               0 ->
                   none;

               3 ->
                   three_or_four;

               % Abnormal:
               _ ->
                   undefined

           end,

           ButtonTransition = get_button_transition( EB ),

           % EEP was known, hence device already was known as well:
           { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
             UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName,
             MaybeDeviceShortName, MaybeEepId } =
                 oceanic:record_known_device_success( Device, DeviceTable,
                    _AlreadyKnownEepId=undefined ),

           NewState = State#oceanic_state{ device_table=NewDeviceTable },

           MaybeDecodedOptData = decode_optional_data( OptData ),

           { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
               resolve_maybe_decoded_data( MaybeDecodedOptData ),

             % Not considering that is a potential smart plug feedback.

           Event = #double_rocker_multipress_event{
               source_eurid=SenderEurid,
               name=MaybeDeviceName,
               short_name=MaybeDeviceShortName,
               eep=MaybeEepId,
               timestamp=Now,
               last_seen=MaybeLastSeen,
               subtelegram_count=MaybeTelCount,
               destination_eurid=MaybeDestEurid,
               dbm=MaybeDBm,
               security_level=MaybeSecLvl,
               button_counting=MaybeButtonCounting,
               energy_bow=ButtonTransition },

           { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
             NextMaybeTelTail, NewState };


       _Other ->
           { NewDeviceTable, _NewDevice, _Now,  _PrevLastSeen, _DiscoverOrigin,
             _IsBackOnline, _MaybeDeviceName, _MaybeDeviceShortName,
               _MaybeEepId } =
                    oceanic:record_device_failure( SenderEurid, DeviceTable,
                        _AlreadyKnownEepId=undefined ),

           trace_bridge:warning_fmt( "Unable to decode a RPS packet "
               "from device whose EURID is ~ts and EEP ~ts (~ts), "
               "as T21=~B and NU=~B.",
               [ oceanic_text:eurid_to_string( SenderEurid ), EepId,
                 oceanic_generated:get_maybe_second_for_eep_strings( EepId ),
                 T21, NU ] ),

           NewState = State#oceanic_state{ device_table=NewDeviceTable },

           { unsupported, _ToSkipLen=0, NextMaybeTelTail, NewState }

   end.


% The newer interpretation is very different, quite simpler and logical, but
% does not correspond to the specs we found.
%
% With it:
% - transitions/actions/events are reported (not states)
% - "energy bow" is a detail that does not really matter here (not in the data)
% - bits are actually these ones:
%   * DB0.{0..3} tell which rockers are pressed:
%     - DB0.0: B0 pressed (true) or not (false)
%     - DB0.1: B1 pressed or not
%     - DB0.2: A0 pressed or not
%     - DB0.3: A1 pressed or not
%   * DB0.{4..7} tell which rockers are released:
%     - DB0.4: B0 released or not
%     - DB0.5: B1 released or not
%     - DB0.6: A0 released or not
%     - DB0.7: A1 released or not
%
% No inconsistency is expected (e.g. if DB0.(x) is true, DB0.(x+4) must be
% false, as a rocker cannot be pressed and released at the same time).
%
% - T21: Teach-in bit, tells whether this telegram is a teach-in one
% - NU: Not Used bit (no meaning in this profile)
%
% Nevertheless this look as an invention of a lost AI bot.
% decode_rps_double_rocker_packet(
%         DB_0= <<A1r:1, A0r:1, B1r:1, B0r:1, A1p:1, A0p:1, B1p:1, B0p:1>>,
%         SenderEurid,
%         % (T21 is at offset 2, thus b5; NU at offset 3, thus b4)
%         _Status= <<_:2, T21:1, NU:1, _:4>>, OptData,
%         NextMaybeTelTail, Device=#enocean_device{ eep=EepId },
%         State=#oceanic_state{ device_table=DeviceTable } ) ->

%     cond_utils:if_defined( oceanic_debug_decoding, trace_bridge:debug_fmt(
%         "Decoding a RPS F6-02-01 double-rocker switch from ~ts, "
%         "based on DB_0=~ts.",
%         [ oceanic_text:eurid_to_string( SenderEurid ),
%           text_utils:integer_to_bits( DB_0 ) ],
%         basic_utils:ignore_unused( DB_0 ) ) ),

    % TO-DO: determine all impossible cases: case A1r =:= A1p...

    % EEP was known, hence device already was known as well:
    % { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
    %   UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName,
    %   MaybeDeviceShortName, MaybeEepId } =
    %     oceanic:record_known_device_success( Device, DeviceTable ),

    % RecState = State#oceanic_state{ device_table=NewDeviceTable },

    % MaybeDecodedOptData = decode_optional_data( OptData ),

    % { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
    %     resolve_maybe_decoded_data( MaybeDecodedOptData ),

    % % Examining whether this telegram may be received in answer to a
    % % past (smart-plug related, switching for Eltako) request, which can
    % % thus be acknowledged and unmonitored:
    % %
    % %ReqState = handle_possible_eltako_switching_request_answer(
    % %    NewDevice, FirstButtonLocator, ButtonTransition, SenderEurid,
    % %    RecState ),

    % ReqState = RecState,

    % Event = #double_rocker_switch_event{
    %     source_eurid=SenderEurid,
    %     name=MaybeDeviceName,
    %     short_name=MaybeDeviceShortName,
    %     eep=MaybeEepId,
    %     timestamp=Now,
    %     last_seen=MaybeLastSeen,
    %     subtelegram_count=MaybeTelCount,
    %     destination_eurid=MaybeDestEurid,
    %     dbm=MaybeDBm,
    %     security_level=MaybeSecLvl,

    %     % In the spirit of application style 1.
    %     % Supposedly:
    %     rocker_1_top_state=f(A0p,A0r),
    %     rocker_1_bottom_state=A1p,
    %     rocker_2_top_state=...,
    %     rocker_2_bottom_state=... },

    % { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
    %   NextMaybeTelTail, ReqState }.



-doc """
Handles the corresponding received telegram as a possible answer to a request
that is still on the air for any Eltako smart plug.

A problem is that a state feedback is sent by these plugs iff they actually
switched; so for example if requesting such a switched-on plug to switch on, it
will send back no telegram, and the request will be considered as failed.

So initially a failed (first) request may be wrongly reported.
""".
-spec handle_possible_eltako_switching_request_answer( enocean_device(),
        button_locator(), button_transition(), eurid(), oceanic_state() ) ->
                                            oceanic_state().
% No request waited:
handle_possible_eltako_switching_request_answer(
        Device=#enocean_device{ waited_request_info=undefined },
        _ButtonLocator, _ButtonTransition, _SenderEurid, State ) ->

    cond_utils:if_defined( oceanic_debug_requests,
        trace_bridge:debug_fmt( "(no request was waited for ~ts)",
            [ oceanic_text:get_device_description( Device ) ] ),
        basic_utils:ignore_unused( Device ) ),

    State;

% switch_on request waited:
handle_possible_eltako_switching_request_answer(
        Device=#enocean_device{ waited_request_info={
            #request_tracking{ target_eurid=SenderEurid,
                               operation=switch_on }, TimerRef } },
        _ButtonLocator={ _Channel=1, _ButPos=top },
        _ButtonTransition=just_pressed, SenderEurid, State )  ->

    { ok, cancel } = timer:cancel( TimerRef ),

    cond_utils:if_defined( oceanic_debug_requests,
        trace_bridge:debug_fmt( "Switch on request acknowledged for ~ts.",
            [ oceanic_text:get_device_description( Device ) ] ) ),

    oceanic:handle_next_request( _MaybeWaitRqInfo=undefined,
        Device#enocean_device.request_queue, Device, State );

% switch_off request waited:
handle_possible_eltako_switching_request_answer(
        Device=#enocean_device{ waited_request_info={
            #request_tracking{ target_eurid=SenderEurid,
                               operation=switch_off }, TimerRef } },
        _ButtonLocator={ _Channel=1, _ButPos=bottom },
        _ButtonTransition=just_pressed, SenderEurid, State )  ->

    { ok, cancel } = timer:cancel( TimerRef ),

    cond_utils:if_defined( oceanic_debug_requests,
        trace_bridge:debug_fmt( "Switch off request acknowledged for ~ts.",
            [ oceanic_text:get_device_description( Device ) ] ) ),

    oceanic:handle_next_request( _MaybeWaitRqInfo=undefined,
        Device#enocean_device.request_queue, Device, State );


handle_possible_eltako_switching_request_answer(
        Device=#enocean_device{ waited_request_info={
            #request_tracking{ target_eurid=SenderEurid,
                               operation=Operation }, _TimerRef } },
        ButtonLocator, ButtonTransition, SenderEurid, State ) ->

    %{ ok, cancel } = timer:cancel( TimerRef ),

    trace_bridge:warning_fmt( "Unmatching request conditions for the ~ts "
        "of ~ts: button locator is ~w, transition is ~w; nothing done.",
        [ Operation, oceanic_text:get_device_description( Device ),
          ButtonLocator, ButtonTransition ] ),

    %oceanic:handle_next_request( _MaybeWaitRqInfo=undefined,
    %    Device#enocean_device.request_queue, Device, State ).

    State.



-doc """
Decodes a rorg_1bs (D5) packet, that is a R-ORG telegram on one byte.

In practice these are only D5-00-01 contact and switch devices, i.e. opening
detectors.

Discussed in `[EEP-spec]` p. 27.
""".
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
-spec decode_1bs_packet( telegram_data_tail(), telegram_opt_data(),
            option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
decode_1bs_packet( DataTail= <<DB_0:8, SenderEurid:32, Status:8>>, OptData,
        NextMaybeTelTail, State=#oceanic_state{ device_table=DeviceTable } ) ->

    % 0xd5 is 213.

    % Apparently, at least in EEP 2.1, RPS/1BS packets are made of:
    %  1. RORG (1 byte)
    %  2. Data: DB0 (1 byte)
    %  3. Sender ID (4 bytes)
    %  4. Status (1 byte)
    %
    % DataTail is to contain fields 2, 3 and 4, and thus should be 6 bytes,
    % e.g. <<9,5,5,51,236,0>>.
    %
    % As for OptData, an example could be <<1,255,255,255,255,45,0>>.

    % For DB_0 for example, 8 bits:
    %  * bit name:   B7 - B6 - B5 - B4 - B3 - B2 - B1 - B0
    %  * bit offset:  0 -  1 -  2 -  3 -  4 -  5 -  6 -  7

    % DB_0.3 i.e. B3:
    LearnActivated = DB_0 band ?b3 =:= 0,

    ContactStatus = case DB_0 band ?b0 =:= 0 of

        true ->
            open;

        false ->
            closed

    end,

    % D5 implies single_input_contact:
    { NewDeviceTable, NewDevice, Now, MaybePrevLastSeen, MaybeDiscoverOrigin,
      IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_device_success( SenderEurid, DeviceTable,
                                       _InferredEepId=single_input_contact ),

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        trace_bridge:debug_fmt( "Decoding a R-ORG 1BS packet, "
            "with a payload of ~B bytes (with DB_0=~w~ts; "
            "contact is ~ts), sender is ~ts, status is ~w~ts.",
            [ size( DataTail ), DB_0,
              oceanic_text:learn_to_string( LearnActivated ),
              ContactStatus,
              oceanic_text:get_best_naming( MaybeDeviceName,
                MaybeDeviceShortName, SenderEurid ),
              Status,
              oceanic_text:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                          OptData ) ] ),
        basic_utils:ignore_unused( [ DataTail, Status ] ) ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    Event = #single_input_contact_event{
        source_eurid=SenderEurid,
        name=MaybeDeviceName,
        short_name=MaybeDeviceShortName,
        eep=MaybeEepId,
        timestamp=Now,
        last_seen=MaybePrevLastSeen,
        subtelegram_count=MaybeTelCount,
        destination_eurid=MaybeDestEurid,
        dbm=MaybeDBm,
        security_level=MaybeSecLvl,
        learn_activated=LearnActivated,
        contact=ContactStatus },

    { decoded, Event, MaybeDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, NewState }.



-doc """
Decodes a rorg_4bs (A5) packet (for example a thermometer report), that is a
R-ORG telegram on four bytes.

Discussed in `[EEP-spec]` p. 12.
""".
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
-spec decode_4bs_packet( telegram_data_tail(), telegram_opt_data(),
            option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
decode_4bs_packet( DataTail= <<DB_3:8, DB_2:8, DB_1:8, DB_0:8,
        SenderEurid:32, _StatusFirstHalf:4, RC:4>>, OptData,
        NextMaybeTelTail, State=#oceanic_state{ device_table=DeviceTable } ) ->

    % 0xa5 is 165.

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        trace_bridge:debug_fmt( "Decoding a R-ORG 4BS packet, "
            "with a payload of ~B bytes "
            "(with DB_3=~w, DB_2=~w, DB_1=~w, DB_0=~w), "
            "sending device has for EURID ~ts, ~ts~ts",
            [ size( DataTail ), DB_3, DB_2, DB_1, DB_0,
              oceanic_text:eurid_to_string( SenderEurid ),
              oceanic_text:repeater_count_to_string( RC ),
              oceanic:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                     OptData )
            ] ),
        basic_utils:ignore_unused( [ DataTail, RC, MaybeDecodedOptData ] ) ),

    % We have to know the specific EEP of this device in order to decode this
    % telegram:
    %
    case table:lookup_entry( SenderEurid, DeviceTable ) of

        key_not_found ->

            { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
              _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
              MaybeDeviceShortName, _MaybeEepId } =
                oceanic:record_device_failure( SenderEurid, DeviceTable,
                    _MaybeInferredEepId=undefined ), % Multiple EEPs correspond

            % Device first time seen:
            trace_bridge:warning_fmt( "Unable to decode a 4BS (A5) packet ~ts",
                [ get_unresolved_device_message( SenderEurid ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            MaybeDecodedOptData = decode_optional_data( OptData ),

            { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
                resolve_maybe_decoded_data( MaybeDecodedOptData ),

            UnresolvedEvent = #unresolved_device_event{
                source_eurid=SenderEurid,
                name=MaybeDeviceName,
                short_name=MaybeDeviceShortName,
                timestamp=Now,
                last_seen=MaybeLastSeen,
                subtelegram_count=MaybeTelCount,
                destination_eurid=MaybeDestEurid,
                dbm=MaybeDBm,
                security_level=MaybeSecLvl,
                type_hint=rorg_4bs },

            { unresolved_first_seen, UnresolvedEvent, NewDevice, _ToSkipLen=0,
              NextMaybeTelTail, NewState };


        % An a-priori knowledge of the actual EEP is needed in order to decode:
        { value, _Device=#enocean_device{ eep=undefined } } ->

            { NewDeviceTable, _NewDevice, Now, MaybeLastSeen,
              _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
              MaybeDeviceShortName, _MaybeEepId } =
                oceanic:record_device_failure( SenderEurid, DeviceTable,
                      _MaybeInferredEepId=undefined ), % Cannot do better

            % Device probably already seen:
            trace_bridge:debug_fmt( "Unable to decode a 4BS (A5) packet "
                "for ~ts: no EEP known for it (hence ignored).",
                [ oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            MaybeDecodedOptData = decode_optional_data( OptData ),

            { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
                resolve_maybe_decoded_data( MaybeDecodedOptData ),

            UnresolvedEvent = #unresolved_device_event{
                source_eurid=SenderEurid,
                name=MaybeDeviceName,
                short_name=MaybeDeviceShortName,
                timestamp=Now,
                last_seen=MaybeLastSeen,
                subtelegram_count=MaybeTelCount,
                destination_eurid=MaybeDestEurid,
                dbm=MaybeDBm,
                security_level=MaybeSecLvl,
                type_hint=rorg_4bs },

            { unresolved, UnresolvedEvent, _ToSkipLen=0, NextMaybeTelTail,
              NewState };


        { value, Device=#enocean_device{ eep=thermo_hygro_low } } ->
            decode_4bs_thermo_hygro_low_packet( DB_3, DB_2, DB_1, DB_0,
                SenderEurid, OptData, NextMaybeTelTail, Device, State );

        % Maybe later:
        % { value, Device=#enocean_device{ eep=thermo_hygro_mid } } ->
        %     decode_4bs_thermo_hygro_mid_packet( DB_3, DB_2, DB_1, DB_0,
        %         SenderEurid, OptData, NextMaybeTelTail, Device, State );

        % { value, Device=#enocean_device{ eep=thermo_hygro_high } } ->
        %     decode_4bs_thermo_hygro_high_packet( DB_3, DB_2, DB_1, DB_0,
        %         SenderEurid, OptData, NextMaybeTelTail, Device, State );

        % Expected to be less common than the previous one:
        { value, Device=#enocean_device{ eep=thermometer } } ->
            decode_4bs_thermometer_packet( DB_3, DB_2, DB_1, DB_0,
                SenderEurid, OptData, NextMaybeTelTail, Device, State );


        { value, Device=#enocean_device{ eep=motion_detector } } ->
            decode_4bs_motion_detector_packet( DB_3, DB_2, DB_1, DB_0,
                SenderEurid, OptData, NextMaybeTelTail, Device, State );

        % Maybe later:
        % { value, Device=#enocean_device{ eep=occupancy_detector } } ->
        %     decode_4bs__packet( DB_3, DB_2, DB_1, DB_0,
        %         SenderEurid, OptData, NextMaybeTelTail, Device, State );

        { value, Device=#enocean_device{
                eep=motion_detector_with_illumination } } ->
            % To reassemble a bit, as illumination is not byte-aligned (over
            % DB_1 and DB_2):
            %
            SubDataTail = <<DB_2:8, DB_1:8, DB_0:8>>,
            decode_4bs_motion_detector_with_illumination_packet( DB_3,
                SubDataTail, SenderEurid, OptData, NextMaybeTelTail,
                Device, State );

        { value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

            { NewDeviceTable, _NewDevice, Now, MaybeLastSeen,
              _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
              MaybeDeviceShortName, _UnsupportedEepId } =
                oceanic:record_device_failure( SenderEurid, DeviceTable,
                    _MaybeInferredEepId=undefined ),

            % Device probably already seen:
            trace_bridge:debug_fmt( "Unable to decode a 4BS (A5-F6) packet "
                "for ~ts: EEP ~ts (~ts) not supported.",
                [ oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ), UnsupportedEepId,
                  oceanic_generated:get_maybe_second_for_eep_strings(
                    UnsupportedEepId ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            MaybeDecodedOptData = decode_optional_data( OptData ),

            { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
                resolve_maybe_decoded_data( MaybeDecodedOptData ),

            UnresolvedEvent = #unresolved_device_event{
                source_eurid=SenderEurid,
                name=MaybeDeviceName,
                short_name=MaybeDeviceShortName,
                timestamp=Now,
                last_seen=MaybeLastSeen,
                subtelegram_count=MaybeTelCount,
                destination_eurid=MaybeDestEurid,
                dbm=MaybeDBm,
                security_level=MaybeSecLvl,
                type_hint=rorg_4bs },

            { unresolved, UnresolvedEvent, _ToSkipLen=0, NextMaybeTelTail,
              NewState }

   end.



-doc """
Decodes a rorg_4bs (A5) packet for the thermometer EEP "A5-02-*", precisely here
"A5-02-05": "Temperature Sensors" (05), range 0°C to +40°C.

Refer to `[EEP-spec]` p. 29.
""".
-spec decode_4bs_thermometer_packet( uint8(), uint8(), uint8(), uint8(),
        eurid(), telegram_opt_data(), option( telegram_tail() ),
        enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_4bs_thermometer_packet( _DB_3=0, _DB_2=0,
        DB_1=ScaledTemperature, DB_0, SenderEurid, OptData, NextMaybeTelTail,
        Device, State ) ->

    % Always 0 and 12 with our sensor, so never reporting anything of interest:
    %trace_bridge:debug_fmt( "For temperature: DB_1=~w, DB_0=~w.",
    %                        [ ScaledTemperature, DB_0 ] ),

    case DB_0 band 2#11110111 =:= 0 of

        true ->
            decode_4bs_thermometer_packet_helper( ScaledTemperature, DB_0,
                SenderEurid, OptData, NextMaybeTelTail, Device, State );

        % Despite CRC:
        false ->
            trace_bridge:warning_fmt( "Received from device ~ts a faulty "
                "content (DB_0=~B; with DB_1=~w), dropping telegram.",
                [ oceanic_text:eurid_to_string( SenderEurid ), DB_0, DB_1 ] ),

            { invalid, _ToSkipLen=0, NextMaybeTelTail, State }

    end;

decode_4bs_thermometer_packet( DB_3, DB_2, DB_1, DB_0, _SenderEurid, OptData,
                               NextMaybeTelTail, Device, State ) ->

    trace_bridge:warning_fmt( "Ignoring invalid rorg_4bs (A5) packet "
        "corresponding to the thermometer EEP, sent by ~ts; "
        "DB_3=~w, DB_2=~w, DB_1=~w, DB_0=~w "
        "(optional data: ~w, next tail: ~w).",
        [ oceanic_text:device_to_string( Device ), DB_3, DB_2, DB_1, DB_0,
          OptData, NextMaybeTelTail ] ),

    { invalid, _ToSkipLen=0, NextMaybeTelTail, State }.



% (helper)
decode_4bs_thermometer_packet_helper( ScaledTemperature, DB_0, SenderEurid,
        OptData, NextMaybeTelTail, Device,
        State=#oceanic_state{ device_table=DeviceTable } ) ->

    % Not using round/1:
    %Temperature = ScaledTemperature / 255.0 * 40,
    Temperature = (1 - ScaledTemperature / 255.0) * 40,

    LearnActivated = DB_0 band ?b3 =:= 0,

    { NewDeviceTable, NewDevice, Now, MaybeLastSeen, UndefinedDiscoverOrigin,
      IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
            _InferredEepId=thermometer ), % Already known if branching here

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        trace_bridge:debug_fmt( "Decoding a R-ORG 4BS thermometer "
            "packet, reporting a ~ts~ts; sender is ~ts~ts.",
            [ oceanic_text:temperature_to_string( Temperature ),
              oceanic_text:learn_to_string( LearnActivated ),
              oceanic_text:get_best_naming( MaybeDeviceName,
                 MaybeDeviceShortName, SenderEurid ),
              oceanic_text:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                          OptData ) ] ) ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    Event = #thermometer_event{ source_eurid=SenderEurid,
                                name=MaybeDeviceName,
                                short_name=MaybeDeviceShortName,
                                eep=MaybeEepId,
                                timestamp=Now,
                                last_seen=MaybeLastSeen,
                                subtelegram_count=MaybeTelCount,
                                destination_eurid=MaybeDestEurid,
                                dbm=MaybeDBm,
                                security_level=MaybeSecLvl,
                                temperature=Temperature,

                                % As "A5-04-01":
                                temperature_range=low,

                                learn_activated=LearnActivated },

    { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, NewState }.




-doc """
Decodes a rorg_4bs (A5) packet for the thermo_hygro_low EEP ("A5-04-01"):
"Temperature and Humidity Sensor" (04), range 0°C to +40°C and 0% to 100% (01).

Refer to `[EEP-spec]` p. 35.
""".
-spec decode_4bs_thermo_hygro_low_packet( uint8(), uint8(), uint8(), uint8(),
        eurid(), telegram_opt_data(), option( telegram_tail() ),
        enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_4bs_thermo_hygro_low_packet( _DB_3=0, _DB_2=ScaledHumidity,
        _DB_1=ScaledTemperature, DB_0, SenderEurid, OptData, NextMaybeTelTail,
        Device, State=#oceanic_state{ device_table=DeviceTable } ) ->

    cond_utils:assert( oceanic_check_decoding, DB_0 band 2#11110101 =:= 0 ),

    MaybeTemperature = case DB_0 band ?b1 =:= 0 of

        true ->
            undefined;

        false ->
            ScaledTemperature / 250.0 * 40

    end,

    RelativeHumidity = ScaledHumidity / 250.0 * 100,

    LearnActivated = DB_0 band ?b3 =:= 0,

    { NewDeviceTable, NewDevice, Now, MaybeLastSeen, UndefinedDiscoverOrigin,
      IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
            _InferredEepId=thermo_hygro_low ), % Already known if branching here

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        begin

            TempStr = case MaybeTemperature of

                undefined ->
                    "(no temperature available)";

                Temp ->
                    text_utils:format( "and a ~ts",
                        [ oceanic_text:temperature_to_string( Temp ) ] )

            end,

            trace_bridge:debug_fmt( "Decoding a R-ORG 4BS thermo_hygro_low "
                "packet, reporting a ~ts ~ts~ts; sender is ~ts~ts.",
                [ oceanic_text:relative_humidity_to_string( RelativeHumidity ),
                  TempStr,
                  oceanic_text:learn_to_string( LearnActivated ),
                  oceanic_text:get_best_naming( MaybeDeviceName,
                      MaybeDeviceShortName, SenderEurid ),
                  oceanic:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                         OptData ) ] )
        end ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    Event = #thermo_hygro_event{ source_eurid=SenderEurid,
                                 name=MaybeDeviceName,
                                 short_name=MaybeDeviceShortName,
                                 eep=MaybeEepId,
                                 timestamp=Now,
                                 last_seen=MaybeLastSeen,
                                 subtelegram_count=MaybeTelCount,
                                 destination_eurid=MaybeDestEurid,
                                 dbm=MaybeDBm,
                                 security_level=MaybeSecLvl,
                                 temperature=MaybeTemperature,
                                 relative_humidity=RelativeHumidity,

                                 % As "A5-04-01":
                                 temperature_range=low,

                                 learn_activated=LearnActivated },

    { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, NewState }.



-doc """
Decodes a rorg_4bs (A5) packet for the motion detector ("Occupancy Sensor with
Supply voltage monitor"), i.e. EEP "A5-07-01".

Refer to `[EEP-spec]` p. 39.
""".
-spec decode_4bs_motion_detector_packet( uint8(), uint8(), uint8(), uint8(),
        eurid(), telegram_opt_data(), option( telegram_tail() ),
        enocean_device(), oceanic_state() ) -> decoding_outcome().
% DB_2 supposed to be 0, yet found equal to 124 (i.e. 0b111-1100) with O2 LINE
% Comfort:
%
decode_4bs_motion_detector_packet( DB_3, _DB_2, DB_1, DB_0, SenderEurid,
        OptData, NextMaybeTelTail, Device,
        State=#oceanic_state{ device_table=DeviceTable } ) ->

    % In Volts:
    MaybeSetSupplyVoltage = case DB_3 of

        V when V =< 250 ->
            V / 250 * 5;

       ErrorCode ->
            trace_bridge:error_fmt( "Error code obtained for motion detector "
                "~ts: ~B.",
                [ oceanic_text:eurid_to_string( SenderEurid ), ErrorCode ] ),
            undefined

    end,

    % Passive Infrared Sensor:
    PIRTriggered = DB_1 >= 128,

    % Otherwise is a mere data telegram:
    IsTeachIn = DB_0 band ?b3 =:= 0,

    SupplyVoltageAvailable = DB_0 band ?b1 =:= 1,

    MaybeActualSupplyVoltage = case SupplyVoltageAvailable of

        true ->
            MaybeSetSupplyVoltage;

        false ->
            undefined

    end,

    { NewDeviceTable, NewDevice, Now, MaybeLastSeen, UndefinedDiscoverOrigin,
      IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
            _InferredEepId=motion_detector ), % Already known if branching here

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        begin
             VoltageStr = case MaybeActualSupplyVoltage of

                undefined ->
                    "no supply voltage";

                Voltage ->
                    text_utils:format( "a supply voltage of ~ts",
                        [ unit_utils:voltage_to_string( Voltage ) ] )

            end,

            trace_bridge:debug_fmt( "Decoding a R-ORG 4BS A5-07-01 packet, "
                "reporting ~ts and ~ts; teach-in: ~ts "
                "(DB_3=~w, DB_1=~w, DB_0=~w); "
                "sender is ~ts~ts.",
                [ oceanic_text:motion_detection_to_string( PIRTriggered ),
                  VoltageStr, IsTeachIn, DB_3, DB_1, DB_0,
                  oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ),
                  oceanic_text:maybe_optional_data_to_string(
                    MaybeDecodedOptData, OptData ) ] )
        end ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    Event = #motion_detector_event{ source_eurid=SenderEurid,
                                    name=MaybeDeviceName,
                                    short_name=MaybeDeviceShortName,
                                    eep=MaybeEepId,
                                    timestamp=Now,
                                    last_seen=MaybeLastSeen,
                                    subtelegram_count=MaybeTelCount,
                                    destination_eurid=MaybeDestEurid,
                                    dbm=MaybeDBm,
                                    security_level=MaybeSecLvl,
                                    motion_detected=PIRTriggered,
                                    supply_voltage=MaybeActualSupplyVoltage,
                                    teach_in=IsTeachIn },

    { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, NewState }.




-doc """
Decodes a rorg_4bs (A5) packet for the motion detector ("Occupancy Sensor with
Supply voltage monitor and 10-bit illumination measurement"), i.e. EEP
"A5-07-03".

Refer to `[EEP-spec]` p. 40.
""".
-spec decode_4bs_motion_detector_with_illumination_packet( uint8(), binary(),
        eurid(), telegram_opt_data(), option( telegram_tail() ),
        enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_4bs_motion_detector_with_illumination_packet( DB_3,
        SubDataTail= <<Illum:10, _NotUsed:6, PIRStatus:1, _Other:3,
            LearnBit:1, _Last:3>>, SenderEurid, OptData, NextMaybeTelTail,
        Device, State=#oceanic_state{ device_table=DeviceTable } ) ->

    % In Volts:
    MaybeSupplyVoltage = case DB_3 of

        V when V =< 250 ->
            V / 250 * 5;

       ErrorCode ->
            trace_bridge:error_fmt( "Error code obtained for motion detector "
                "~ts: ~B.",
                [ oceanic_text:eurid_to_string( SenderEurid ), ErrorCode ] ),
            undefined

    end,


    % In lx (2^10=1024):
    MaybeLux = case Illum of

        Lx when Lx =< 1000 ->
            Lx;

        1001 ->
            trace_bridge:warning_fmt( "Illumination over range reported "
                "for motion detector ~ts.",
                [ oceanic_text:eurid_to_string( SenderEurid ) ] ),
            undefined;


       ResCode ->
            trace_bridge:error_fmt( "Reserved code obtained for motion detector"
                " ~ts: ~B.",
                [ oceanic_text:eurid_to_string( SenderEurid ), ResCode ] ),
            undefined

    end,

    % Otherwise "uncertain of occupancy status":
    MotionDetected = PIRStatus =:= 1,

    % Otherwise is a mere data telegram:
    IsTeachIn = LearnBit =:= 0,

    { NewDeviceTable, NewDevice, Now, MaybeLastSeen, UndefinedDiscoverOrigin,
      IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
            % Already known if branching here:
            _InferredEepId=motion_detector_with_illumination ),

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        begin
            LuxStr = case MaybeLux of

                undefined ->
                   "no valid illumination";

                Lux ->
                    text_utils:format( "an illumination of ~ts",
                        [ unit_utils:illuminance_to_string( Lux ) ] )

            end,

            VoltageStr = case MaybeSupplyVoltage of

                undefined ->
                    "no supply voltage";

                Voltage ->
                    text_utils:format( "a supply voltage of ~ts",
                        [ unit_utils:voltage_to_string( Voltage ) ] )

            end,

            trace_bridge:debug_fmt( "Decoding a R-ORG 4BS A5-07-03 packet, "
                "reporting ~ts, ~ts, and ~ts; teach-in: ~ts "
                "(data tail: ~ts); sender is ~ts~ts.",
                [ oceanic_text:motion_detection_to_string( MotionDetected ),
                  LuxStr, VoltageStr, IsTeachIn,
                  text_utils:binary_to_bits( SubDataTail ),
                  oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ),
                  oceanic_text:maybe_optional_data_to_string(
                    MaybeDecodedOptData, OptData ) ] )
        end,
        basic_utils:ignore_unused( SubDataTail ) ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    Event = #motion_detector_event_with_illumination{
        source_eurid=SenderEurid,
        name=MaybeDeviceName,
        short_name=MaybeDeviceShortName,
        eep=MaybeEepId,
        timestamp=Now,
        last_seen=MaybeLastSeen,
        subtelegram_count=MaybeTelCount,
        destination_eurid=MaybeDestEurid,
        dbm=MaybeDBm,
        security_level=MaybeSecLvl,
        motion_detected=MotionDetected,
        illuminance=MaybeLux,
        supply_voltage=MaybeSupplyVoltage,
        teach_in=IsTeachIn },

    { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, NewState }.



-doc """
Decodes a rorg_ute (D4) packet, that is a R-ORG telegram for Universal
Teach-in/out, EEP based (UTE), one way of pairing devices.

Discussed in `[EEP-gen]` p. 17; p. 25 for the query and p. 26 for the response.
""".
-spec decode_ute_packet( telegram_data_tail(), telegram_opt_data(),
            option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
% This is a Teach-In/Out query UTE request (Broadcast / CMD: 0x0, p. 25),
% broadcasting (typically after one of its relevant buttons has been pressed in
% the case of a smart plug) a request that devices (e.g. a home automation
% gateway) declare to this initiator device.
%
% Proceeding byte per byte is probably clearer:
decode_ute_packet(
        % Hence 7+4+1=12 bytes:
        DataTail= << CommDir:1, ResExpected:1, ReqType:2, Cmd:4, % = DB_6
                     ChanCount:8,                                % = DB_5
                     ManufIdLSB:8,                               % = DB_4
                     _:5, ManufIdMSB:3,                          % = DB_3
                     Type:8,                                     % = DB_2
                     Func:8,                                     % = DB_1
                     RORG:8,                                     % = DB_0
                     InitiatorEurid:32,
                     Status:1/binary>>,
        OptData, NextMaybeTelTail,
        State=#oceanic_state{ device_table=DeviceTable } ) ->

    CommDirection= case CommDir of

        0 ->
            unidirectional;

        % Usually:
        1 ->
            bidirectional

    end,

    { ResponseExpected, ResponseExpectedStr } = case ResExpected of

        % Usually:
        0 ->
            { true, "a response is expected" } ;

        1 ->
            { false, "no response is expected" }

    end,

    MaybeRequestType = case ReqType of

        0 ->
            teach_in;

        1 ->
            teach_out;

        % Unspecified (usual); it is then supposed to be some kind of teach-in,
        % (not teach-out):
        %
        2 ->
            %undefined;
            teach_in;

        % Not used:
        3 ->
            undefined

    end,

    % Check:
    Cmd = 0,

    ChannelTaught = case ChanCount of

        255 ->
            all;

        % Often 1:
        ChanCount ->
            ChanCount

    end,

    <<ManufId>> = <<ManufIdMSB:3, ManufIdLSB:5>>,

    InitiatorEep = { RORG, Func, Type },

    { NewDeviceTable, Device=#enocean_device{ name=MaybeDevName,
                                              short_name=MaybeDevShortName,
                                              eep=MaybeEepId }, Now } =
        oceanic:declare_device_from_teach_in( InitiatorEurid, InitiatorEep,
                                              DeviceTable ),

    { PTMSwitchModuleType, NuType, RepCount } = get_rps_status_info( Status ),

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        begin

            ChanStr = case ChannelTaught of

                all ->
                    "all";

                ChanCount ->
                    text_utils:format( "~B", [ ChanCount ] )

            end,

            trace_bridge:debug_fmt( "Decoding a Teach query UTE packet "
                "from ~ts, whereas ~ts, for a ~ts communication, ~ts, "
                "request type: ~ts involving ~ts channel(s), "
                "manufacturer ID: ~ts, "
                "PTM switch module is ~ts, message type is ~ts, ~ts~ts.",
                [ oceanic_text:eurid_to_bin_string( InitiatorEurid, State ),
                  oceanic_text:get_eep_description( MaybeEepId ), CommDirection,
                  ResponseExpectedStr, MaybeRequestType, ChanStr,
                  text_utils:integer_to_hexastring( ManufId ),
                  oceanic_text:ptm_module_to_string( PTMSwitchModuleType ),
                  oceanic_text:nu_message_type_to_string( NuType ),
                  oceanic_text:repeater_count_to_string( RepCount ),
                  oceanic:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                         OptData )
                ] )

        end,
        basic_utils:ignore_unused(
            [ ResponseExpectedStr, PTMSwitchModuleType, NuType, RepCount ] ) ),

    % Just needing to echo DB5.7...DB0.0, hence 6 bytes:
    <<_DB_6:8, ToEcho:6/binary, _/binary>> = DataTail,

    % Probably that destination is broadcast:
    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    TeachReqEvent = #teach_request_event{ source_eurid=InitiatorEurid,
                                          name=MaybeDevName,
                                          short_name=MaybeDevShortName,
                                          eep=MaybeEepId,
                                          timestamp=Now,
                                          last_seen=Now,
                                          subtelegram_count=MaybeTelCount,
                                          destination_eurid=MaybeDestEurid,
                                          dbm=MaybeDBm,
                                          security_level=MaybeSecLvl,
                                          comm_direction=CommDirection,
                                          response_expected=ResponseExpected,
                                          request_type=MaybeRequestType,
                                          channel_taught=ChannelTaught,
                                          manufacturer_id=ManufId,
                                          echo_content=ToEcho },

    TeachState = case State#oceanic_state.auto_ack_teach_queries of

        true ->
            trace_bridge:debug( "Auto-acknowledgemet of teach query." ),
            case MaybeRequestType of

                teach_in ->
                    oceanic:acknowledge_teach_request( TeachReqEvent,
                        _TeachOutcome=teach_in_accepted, State );

                teach_out ->
                    oceanic:acknowledge_teach_request( TeachReqEvent,
                        _TeachOutcome=teach_out_accepted, State );

                undefined ->
                    trace_bridge:debug( "Type of teach request unknown, hence "
                        "not acknowledged." ),
                    State

            end;

        false ->
            State

    end,

    FinalState = TeachState#oceanic_state{ device_table=NewDeviceTable },

    trace_bridge:debug( "Teach query decoded." ),

    { decoded, TeachReqEvent, _DiscoverOrigin=teaching, _IsBackOnline=false,
      Device, NextMaybeTelTail, FinalState }.



-doc """
Decodes a rorg_vld (D2) packet (for example used by smart plugs), that is a
R-ORG telegram containing Variable Length Data.

VLD telegrams carry a variable payload between 1 and 14 bytes, depending on
their design.

Discussed in `[EEP-gen]` p. 12.

Various packet types exist, in both directions (from/to sensor/actuator), and
depend on the actual EEP (hence on its FUNC and TYPE) implemented by the emitter
device.

Yet only the RORG (namely D2) is specified on such telegrams, therefore their
interpretation depends on the extra FUNC and TYPE information supposed to be
known a priori by the receiver.
""".
-spec decode_vld_packet( telegram_data_tail(), telegram_opt_data(),
            option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
decode_vld_packet( DataTail, OptData, NextMaybeTelTail,
                   State=#oceanic_state{ device_table=DeviceTable } ) ->

    % We do not know yet the size of the payload, but we know that DataTail ends
    % with SenderEurid:32 then Status:1/binary:
    %
    PayloadSize = size( DataTail ) - (4+1),

    <<Payload:PayloadSize/binary, SenderEurid:32, Status:1/binary>> = DataTail,

    cond_utils:if_defined( oceanic_debug_decoding,
        trace_bridge:debug_fmt( "Found a payload of ~B bytes: ~w.",
                                [ PayloadSize, Payload ] ) ),

    % We have to know the specific EEP of this device in order to decode this
    % telegram:
    %
    case table:lookup_entry( SenderEurid, DeviceTable ) of

        % Device first time seen:
        key_not_found ->

            % Not trying to decode optional data then; multiple smart plug EEPs
            % could match:
            %
            { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
              _ListeningDiscoverOrigin, _IsBackOnline, UndefinedDeviceName,
              UndefinedDeviceShortName, _MaybeEepId } =
                oceanic:record_device_failure( SenderEurid, DeviceTable,
                    _MaybeInferredEepId=undefined ),

            trace_bridge:warning_fmt( "Unable to decode a VLD (D2) packet ~ts",
                [ get_unresolved_device_message( SenderEurid ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            MaybeDecodedOptData = decode_optional_data( OptData ),

            { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
                resolve_maybe_decoded_data( MaybeDecodedOptData ),

            UnresolvedEvent = #unresolved_device_event{
                source_eurid=SenderEurid,
                name=UndefinedDeviceName,
                short_name=UndefinedDeviceShortName,
                timestamp=Now,
                last_seen=MaybeLastSeen,
                subtelegram_count=MaybeTelCount,
                destination_eurid=MaybeDestEurid,
                dbm=MaybeDBm,
                security_level=MaybeSecLvl,
                type_hint=rorg_vld },

            { unresolved_first_seen, UnresolvedEvent, NewDevice, _ToSkipLen=0,
              NextMaybeTelTail, NewState };


        % Knowing the actual EEP is needed in order to decode:
        { value, _Device=#enocean_device{ eep=undefined } } ->

            % Not trying to decode optional data then.

            { NewDeviceTable, _NewDevice, Now, MaybeLastSeen,
              _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
              MaybeDeviceShortName, _UndefinedEepId } =
                oceanic:record_device_failure( SenderEurid, DeviceTable,
                    _MaybeInferredEepId=undefined ),

            % Device probably already seen, so just a debug trace:
            trace_bridge:debug_fmt( "Unable to decode a VLD packet "
                "for ~ts: no EEP known for it (hence ignored).",
                [ oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            MaybeDecodedOptData = decode_optional_data( OptData ),

            { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
                resolve_maybe_decoded_data( MaybeDecodedOptData ),

            UnresolvedEvent = #unresolved_device_event{
                source_eurid=SenderEurid,
                name=MaybeDeviceName,
                short_name=MaybeDeviceShortName,
                timestamp=Now,
                last_seen=MaybeLastSeen,
                subtelegram_count=MaybeTelCount,
                destination_eurid=MaybeDestEurid,
                dbm=MaybeDBm,
                security_level=MaybeSecLvl,
                type_hint=rorg_vld },

            { unresolved, UnresolvedEvent, _ToSkipLen=0, NextMaybeTelTail,
              NewState };


        { value, Device=#enocean_device{ eep=smart_plug } } ->
            decode_vld_smart_plug_packet( Payload, SenderEurid, Status,
                OptData, NextMaybeTelTail, Device, State );


        { value, Device=#enocean_device{ eep=smart_plug_with_metering } } ->
            decode_vld_smart_plug_with_metering_packet( Payload,
                SenderEurid, Status, OptData, NextMaybeTelTail, Device, State );


        { value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

            % Not trying to decode optional data then.

            { NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
              _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
              MaybeDeviceShortName, _UnsupportedEepId } =
                oceanic:record_device_failure( SenderEurid, DeviceTable,
                    _MaybeInferredEepId=undefined ), % Already known

            % Device probably already seen:
            trace_bridge:debug_fmt( "Unable to decode a VLD (D2) packet "
                "for ~ts: EEP ~ts (~ts) not supported.",
                [ oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ),
                  UnsupportedEepId,
                  oceanic_generated:get_maybe_second_for_eep_strings(
                    UnsupportedEepId ) ] ),

            NewState = State#oceanic_state{ device_table=NewDeviceTable },

            { unsupported, _ToSkipLen=0, NextMaybeTelTail, NewState }

    end.



-doc """
Decodes a packet emitted by a rorg_vld smart_plug (D2-01-0A, an `Electronic
switches and dimmers with Energy Measurement and Local Control` device of type
0A).

This corresponds to basic smart, non-metering plugs bidirectional actuators that
may control (switch on/off) most electrical loads (e.g. appliances); they do not
perform metering.

Discussed in `[EEP-spec]` p. 132.

Notably, if the command is equal to 0x4 (hence packet of type `"Actuator Status
Response"`, i.e. with `CmdAsInt=16#4` (`"CMD 0x4"`, 'actuator_status_response';
see `[EEP-spec]` p. 135), it is an information sent (as a broadcast) by the
smart plug about its status (either after a status request or after a state
change request - whether or not it triggered an actual state change), typically
to acknowledge that a requested switching was indeed triggered.
""".
-spec decode_vld_smart_plug_packet( vld_payload(), eurid(), telegram_chunk(),
        telegram_opt_data(), option( telegram_tail() ), enocean_device(),
        oceanic_state() ) -> decoding_outcome().
decode_vld_smart_plug_packet(
        % 3 bytes (no OutputValue available):
        _Payload= <<PowerFailureEnabled:1, PowerFailureDetected:1, _:2,
                    CmdAsInt:4, OverCurrentSwitchOff:1, ErrorLevel:2,
                    _IOChannel:5, LocalControl:1, OutputValue:7>>,
        SenderEurid, _Status, OptData, NextMaybeTelTail, Device,
        State=#oceanic_state{ device_table=DeviceTable } )
                        when CmdAsInt =:= 16#4 ->

    % Mostly as decode_vld_smart_plug_with_metering_packet/7, except no
    % measurement.

    { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
      UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName,
        MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
            % Already known if branching here:
            _InferredEepId=smart_plug ),

    RecState = State#oceanic_state{ device_table=NewDeviceTable },

    { IsPowerFailureEnabled, IsPowerFailureDetected } =
            case PowerFailureEnabled of

        0 ->
            { false, false };

        1 ->
            Detect = case PowerFailureDetected of

                0 ->
                    false;

                1 ->
                    true

            end,
            { true, Detect }

    end,

    IsOverCurrentSwitchOffTrigger = case OverCurrentSwitchOff of

        0 ->
            false;

        1 ->
            true

    end,

    HardwareStatus = case ErrorLevel of

        0 ->
            nominal;

        1 ->
            warning;

        2 ->
            failure;

        3 ->
            unsupported

    end,

    % IOChannel not decoded yet (purpose unclear).

    IsLocalControlEnabled = case LocalControl of

        0 ->
            false;

        1 ->
            true

    end,

    % This is not a metering plug yet it *may* return some appropriate
    % information here to determine whether on or off:

    MaxLevel = 16#64,

    % For a non-metering plug, knowing whether it is on or off does not seem
    % very clear; supposing this information is not in I/O channel but in:
    %
    OutputPower = case OutputValue of

        0 ->
            off;

        V when V =< MaxLevel ->
            round( V / MaxLevel * 100.0 );

        % 16#7e: not used.

        16#7f ->
            not_available

    end,

    trace_bridge:debug_fmt(
        "(non-metering plug ~ts reports an output power of ~w)",
        [ oceanic_text:get_best_naming( MaybeDeviceName, MaybeDeviceShortName,
                                        SenderEurid ),
          OutputPower ] ),

    MaybeCmd = oceanic_generated:get_second_for_vld_d2_00_cmd( CmdAsInt ),

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,

        begin
            PFStr = oceanic_text:interpret_power_failure( IsPowerFailureEnabled,
                IsPowerFailureDetected ),

            OCStr = oceanic_text:interpret_overcurrent_trigger(
                IsOverCurrentSwitchOffTrigger ),

            HardStr = oceanic_text:interpret_hardware_status( HardwareStatus ),

            LocCtrlStr = oceanic_text:interpret_local_control(
                IsLocalControlEnabled ),

            trace_bridge:debug_fmt( "Decoding a VLD smart plug packet "
                "for command '~ts' (~B); sender is ~ts; ~ts, ~ts, ~ts, ~ts~ts.",
                [ MaybeCmd, CmdAsInt,
                  oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ),
                  PFStr, OCStr, HardStr, LocCtrlStr,
                  oceanic:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                         OptData )
                ] )

        end,

        basic_utils:ignore_unused( [ SenderEurid, MaybeCmd, MaybeDeviceName,
            MaybeDecodedOptData, IsPowerFailureEnabled,
            IsPowerFailureDetected ] ) ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
       resolve_maybe_decoded_data( MaybeDecodedOptData ),


    % Examining whether this telegram may be received in answer to a past
    % (smart-plug related, standard switching) request, which can thus be
    % acknowledged and unmonitored:
    %
    ReqState = handle_possible_power_request_answer( NewDevice, OutputPower,
        SenderEurid, RecState ),

    Event = #smart_plug_status_report_event{
        source_eurid=SenderEurid,
        name=MaybeDeviceName,
        eep=MaybeEepId,
        timestamp=Now,
        last_seen=MaybeLastSeen,
        subtelegram_count=MaybeTelCount,
        destination_eurid=MaybeDestEurid,
        dbm=MaybeDBm,
        security_level=MaybeSecLvl,
        power_failure_detected=IsPowerFailureDetected,
        overcurrent_triggered=IsOverCurrentSwitchOffTrigger,
        hardware_status=HardwareStatus,
        local_control_enabled=IsLocalControlEnabled,
        output_power=OutputPower },

    { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, ReqState };


% For other, not supported yet, smart plug command packets:
decode_vld_smart_plug_packet(
        % 3 bytes:
        _Payload= <<_:4, CmdAsInt:5, _Rest/binary>>,
        SenderEurid, _Status, OptData, NextMaybeTelTail, Device,
        State=#oceanic_state{ device_table=DeviceTable } ) ->

    MaybeCmd = oceanic_generated:get_maybe_second_for_vld_d2_00_cmd( CmdAsInt ),

    % Actually is currently not managed:
    { NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
      _UndefinedDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
      MaybeDeviceShortName, _MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
            _MaybeInferredEepId=undefined ),

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    %{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
    %   resolve_maybe_decoded_data( MaybeDecodedOptData ),

    cond_utils:if_defined( oceanic_debug_decoding,

        begin
            trace_bridge:debug_fmt(
                "Partial decoding a VLD smart plug packet "
                "for command '~ts' (~B); sender is ~ts~ts.",
                [ MaybeCmd, CmdAsInt,
                  oceanic_text:get_best_naming( MaybeDeviceName,
                    MaybeDeviceShortName, SenderEurid ),
                  oceanic:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                         OptData )
                ] )
        end,

        basic_utils:ignore_unused( [ SenderEurid, MaybeCmd, MaybeDeviceName,
            MaybeDeviceShortName, MaybeDecodedOptData ] ) ),

    { unsupported, _SkipLen=0, NextMaybeTelTail, NewState }.




-doc """
Decodes a packet emitted by a rorg_vld smart_plug_with_metering (D2-01-0B, an
`Electronic switches and dimmers with Energy Measurement and Local Control`
device of type 0B).

This corresponds to smart, metering plugs bidirectional actuators that may
control (switch on/off) most electrical loads (e.g. appliances) and may report
metering information.

See `decode_vld_smart_plug_packet/7` for extra details.

Discussed in `[EEP-spec]` p. 143.
""".
-spec decode_vld_smart_plug_with_metering_packet( vld_payload(), eurid(),
        telegram_chunk(), telegram_opt_data(), option( telegram_tail() ),
        enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_vld_smart_plug_with_metering_packet(
        % 3 bytes:
        _Payload= <<PowerFailureEnabled:1, PowerFailureDetected:1, _:2,
                    CmdAsInt:4, OverCurrentSwitchOff:1, ErrorLevel:2,
                    _IOChannel:5, LocalControl:1, OutputValue:7>>,
        SenderEurid, _Status, OptData, NextMaybeTelTail, Device,
        State=#oceanic_state{ device_table=DeviceTable } )
                    when CmdAsInt =:= 16#4 ->

    { NewDeviceTable, NewDevice, Now, MaybeLastSeen,
      UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName,
        MaybeDeviceShortName, MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
             % Already known if branching here:
            _InferredEepId=smart_plug_with_metering ),

    RecState = State#oceanic_state{ device_table=NewDeviceTable },

    { IsPowerFailureEnabled, IsPowerFailureDetected } =
            case PowerFailureEnabled of

        0 ->
            { false, false };

        1 ->
            Detect = case PowerFailureDetected of

                0 ->
                    false;

                1 ->
                    true

            end,
            { true, Detect }

    end,

    IsOverCurrentSwitchOffTrigger = case OverCurrentSwitchOff of

        0 ->
            false;

        1 ->
            true

    end,

    HardwareStatus = case ErrorLevel of

        0 ->
            nominal;

        1 ->
            warning;

        2 ->
            failure;

        3 ->
            unsupported

    end,

    % IOChannel not decoded yet (purpose unclear).

    IsLocalControlEnabled = case LocalControl of

        0 ->
            false;

        1 ->
            true

    end,

    MaxLevel = 16#64,

    OutputPower = case OutputValue of

        0 ->
            off;

        V when V =< MaxLevel ->
            round( V / MaxLevel * 100.0 );

        % 16#7e: not used.

        16#7f ->
            not_available

    end,

    MaybeCmd = oceanic_generated:get_maybe_second_for_vld_d2_00_cmd( CmdAsInt ),

    MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,

        begin
            PFStr = oceanic_text:interpret_power_failure( IsPowerFailureEnabled,
                IsPowerFailureDetected ),

            OCStr = oceanic_text:interpret_overcurrent_trigger(
                IsOverCurrentSwitchOffTrigger ),

            HardStr = oceanic_text:interpret_hardware_status( HardwareStatus ),

            LocCtrlStr =
                oceanic_text:interpret_local_control( IsLocalControlEnabled ),

            PowerStr = oceanic_text:interpret_power_report( OutputPower ),

            trace_bridge:debug_fmt(
                "Decoding a VLD smart plug with metering packet "
                "for command '~ts' (~B); sender is ~ts; ~ts, ~ts, ~ts, "
                "~ts; this plug is ~ts~ts.",
                [ MaybeCmd, CmdAsInt,
                  oceanic_text:get_best_naming( MaybeDeviceName,
                      MaybeDeviceShortName, SenderEurid ),
                  PFStr, OCStr, HardStr, LocCtrlStr, PowerStr,
                  oceanic:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                         OptData ) ] )

        end,

        basic_utils:ignore_unused( [ MaybeCmd, IsPowerFailureEnabled ] ) ),

    { MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
        resolve_maybe_decoded_data( MaybeDecodedOptData ),

    % Examining whether this telegram may be received in answer to a past
    % request, which can thus be acknowledged and unmonitored:
    %
    ReqState = handle_possible_power_request_answer( NewDevice, OutputPower,
        SenderEurid, RecState ),

    Event = #smart_plug_status_report_event{
        source_eurid=SenderEurid,
        name=MaybeDeviceName,
        short_name=MaybeDeviceShortName,
        eep=MaybeEepId,
        timestamp=Now,
        last_seen=MaybeLastSeen,
        subtelegram_count=MaybeTelCount,
        destination_eurid=MaybeDestEurid,
        dbm=MaybeDBm,
        security_level=MaybeSecLvl,
        power_failure_detected=IsPowerFailureDetected,
        overcurrent_triggered=IsOverCurrentSwitchOffTrigger,
        hardware_status=HardwareStatus,
        local_control_enabled=IsLocalControlEnabled,
        output_power=OutputPower },

    { decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
      NextMaybeTelTail, ReqState };


% For other, not supported yet, smart plug with metering command packets:
decode_vld_smart_plug_with_metering_packet(
        % 3 bytes:
        _Payload= <<_:4, CmdAsInt:5, _Rest/binary>>,
        SenderEurid, _Status, OptData, NextMaybeTelTail, Device,
        State=#oceanic_state{ device_table=DeviceTable } ) ->

    MaybeCmd = oceanic_generated:get_maybe_second_for_vld_d2_00_cmd( CmdAsInt ),

    % Actually is currently not managed:
    { NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
      _UndefinedDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
      MaybeDeviceShortName, _MaybeEepId } =
        oceanic:record_known_device_success( Device, DeviceTable,
            _MaybeInferredEepId=undefined ),

    NewState = State#oceanic_state{ device_table=NewDeviceTable },

    MaybeDecodedOptData = decode_optional_data( OptData ),

    %{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
    %   resolve_maybe_decoded_data( MaybeDecodedOptData ),

    cond_utils:if_defined( oceanic_debug_decoding,

        begin
            trace_bridge:debug_fmt(
                "Partial decoding of a VLD smart plug with metering packet "
                "for command '~ts' (~B); sender is ~ts~ts.",
                [ MaybeCmd, CmdAsInt,
                  oceanic_text:get_best_naming( MaybeDeviceName,
                      MaybeDeviceShortName, SenderEurid ),
                  oceanic:maybe_optional_data_to_string( MaybeDecodedOptData,
                                                         OptData )
                ] )
        end,

        basic_utils:ignore_unused( [ SenderEurid, MaybeCmd, MaybeDeviceName,
            MaybeDeviceShortName, MaybeDecodedOptData ] ) ),

    { unsupported, _SkipLen=0, NextMaybeTelTail, NewState }.



-doc """
Handles the corresponding received telegram as a possible answer to a request
that is still on the air for any standard smart plug.
""".
-spec handle_possible_power_request_answer( enocean_device(), power_report(),
    eurid(), oceanic_state() ) -> oceanic_state().
% No request waited:
handle_possible_power_request_answer(
        Device=#enocean_device{ waited_request_info=undefined },
        _PowerReport, _SenderEurid, State ) ->

    cond_utils:if_defined( oceanic_debug_requests,
        trace_bridge:debug_fmt( "(no request was waited for ~ts)",
            [ oceanic_text:get_device_description( Device ) ] ),
        basic_utils:ignore_unused( Device ) ),

    State;

% switch_on request waited:
handle_possible_power_request_answer(
        Device=#enocean_device{ waited_request_info={
            #request_tracking{ target_eurid=SenderEurid,
                               operation=switch_on }, TimerRef } },
        _PowerReport=Level, SenderEurid, State ) when is_integer( Level ) ->

    { ok, cancel } = timer:cancel( TimerRef ),

    cond_utils:if_defined( oceanic_debug_requests,
        trace_bridge:debug_fmt( "Switch on request acknowledged for ~ts.",
            [ oceanic_text:get_device_description( Device ) ] ) ),

    oceanic:handle_next_request( _MaybeWaitRqInfo=undefined,
        Device#enocean_device.request_queue, Device, State );


% switch_off request waited:
handle_possible_power_request_answer(
        Device=#enocean_device{ waited_request_info={
            #request_tracking{ target_eurid=SenderEurid,
                               operation=switch_off }, TimerRef } },
        _PowerReport=off, SenderEurid, State ) ->

    { ok, cancel } = timer:cancel( TimerRef ),

    cond_utils:if_defined( oceanic_debug_requests,
        trace_bridge:debug_fmt( "Switch off request acknowledged for ~ts.",
            [ oceanic_text:get_device_description( Device ) ] ) ),

    oceanic:handle_next_request( _MaybeWaitRqInfo=undefined,
        Device#enocean_device.request_queue, Device, State );


handle_possible_power_request_answer(
        Device=#enocean_device{ waited_request_info={
            #request_tracking{ target_eurid=SenderEurid,
                               operation=Operation }, TimerRef } },
        _PowerReport=not_available, SenderEurid, State ) ->

    { ok, cancel } = timer:cancel( TimerRef ),

    trace_bridge:warning_fmt( "No power report available, supposing the "
        "waited ~ts request to be acknowledged for ~ts.",
        [ Operation, oceanic_text:get_device_description( Device ) ] ),

    oceanic:handle_next_request( _MaybeWaitRqInfo=undefined,
        Device#enocean_device.request_queue, Device, State ).





% Section for decoding helpers.


-doc """
Decodes the specified optional data, if any.

Refer to `[ESP3]` p. 18 for its description.

The CRC for the overall full data (base+optional) is expected to have been
checked beforehand.
""".
-spec decode_optional_data( telegram_opt_data() ) ->
                                        option( decoded_optional_data() ).
decode_optional_data( _OptData= <<SubTelNum:8, DestinationEurid:32, DBm:8,
                                  SecurityLevel:8>> ) ->
    { SubTelNum, DestinationEurid, decode_maybe_dbm( DBm ),
      decode_maybe_security_level( SecurityLevel ) };

decode_optional_data( _NoOptData= <<>> ) ->
    undefined;

decode_optional_data( Other ) ->
    trace_bridge:warning_fmt( "Unable to decode following optional data "
        "(of size ~B bytes): ~w.", [ size( Other ), Other ] ),
    undefined.



-doc "Decodes the specified byte as a dBm measurement.".
-spec decode_maybe_dbm( uint8() ) -> option( dbm() ).
decode_maybe_dbm( 16#ff ) ->
    % Should be a sending:
    undefined;

decode_maybe_dbm( V ) ->
    -V.



-doc "Decodes the specified byte as a security level.".
-spec decode_maybe_security_level( uint8() ) -> option( security_level() ).
decode_maybe_security_level( 0 ) ->
    not_processed;

decode_maybe_security_level( 1 ) ->
    obsolete;

decode_maybe_security_level( 2 ) ->
    decrypted;

decode_maybe_security_level( 3 ) ->
    authenticated;

decode_maybe_security_level( 4 ) ->
    decrypted_and_authenticated;

decode_maybe_security_level( Other ) ->
    trace_bridge:warning_fmt( "Invalid telegram security level: ~B",
                              [ Other ] ),
    undefined.



-doc "Helper to resolve correctly elements (if any) of optional data.".
-spec resolve_maybe_decoded_data( option( decoded_optional_data() ) ) ->
        { option( subtelegram_count() ), option( eurid() ), option( dbm() ),
          option( security_level() ) }.
resolve_maybe_decoded_data( _MaybeDecodedOptData=undefined ) ->
    { undefined, undefined, undefined, undefined };

resolve_maybe_decoded_data( DecodedOptData ) ->
    DecodedOptData.




% Could be a bijective topic as well:

-doc """
Returns the button designated by the specified enumeration (application style
does not matter here).
""".
-spec get_button_designator( enum() ) -> button_designator().
get_button_designator( _Enum=0 ) ->
    button_ai;

get_button_designator( _Enum=1 ) ->
    button_ao;

get_button_designator( _Enum=2 ) ->
    button_bi;

get_button_designator( _Enum=3 ) ->
    button_bo.



-doc """
Returns the button designated by the specified enumeration, in the context of
the specified application style.
""".
-spec get_button_locator( enum(), application_style() ) -> button_locator().
% Application style 1:
get_button_locator( _Enum=0, _AppStyle=1 ) ->
    % button_ai / button A, bottom
    { _Channel=1, _Pos=bottom };

get_button_locator( _Enum=1, _AppStyle=1 ) ->
    % button_ao / button A, top
    { _Channel=1, _Pos=top };

get_button_locator( _Enum=2, _AppStyle=1 ) ->
    % button_bi / button B, bottom
    { _Channel=2, _Pos=bottom };

get_button_locator( _Enum=3, _AppStyle=1 ) ->
    % button_bo / button B, top
    { _Channel=2, _Pos=top };


% Application style 2; checked from the actual readings from a Nodon CRC-2-6-04
% (EEP F6-02-02):
%
get_button_locator( _Enum=0, _AppStyle=2 ) ->
    % button_ai / button A, top
    { _Channel=1, _Pos=bottom };

get_button_locator( _Enum=1, _AppStyle=2 ) ->
    % button_ao / button A, bottom
    { _Channel=1, _Pos=top };

get_button_locator( _Enum=2, _AppStyle=2 ) ->
    % button_bi / button B, top
    { _Channel=2, _Pos=bottom };

get_button_locator( _Enum=3, _AppStyle=2 ) ->
    % button_bo / button B, bottom
    { _Channel=2, _Pos=top }.



% Could be a bijective topic as well:


-doc "Returns the button transition corresponding to the specified energy bow.".
-spec get_button_transition( enum() ) -> button_transition().
get_button_transition( _EnergyBow=0 ) ->
    just_released;

get_button_transition( _EnergyBow=1 ) ->
    just_pressed.




-doc """
Decodes the RPS status byte, common to many RPS telegrams.

Refer to `[EEP-spec]` p. 11 for further details.
""".
-spec get_rps_status_info( telegram_chunk() ) ->
        { ptm_switch_module_type(), nu_message_type(), repetition_count() }.
get_rps_status_info( _Status= <<T21:2, Nu:2, RC:4>> ) ->

    PTMSwitchModuleType = case T21 + 1 of

        1 ->
            ptm1xx;

        2 ->
            ptm2xx

    end,

    % Nu expected to be 0 (Normal-message) or 1 (Unassigned-message), yet found
    % to be 2 or 3:
    %
    NuType = case Nu of

        0 ->
            normal;

        1 ->
            unassigned;

        2 ->
            unknown_type_2;

        3 ->
            unknown_type_3

    end,

    { PTMSwitchModuleType, NuType, RC }.



-doc """
Returns the message corresponding to an unresolved - thus unusable - device.
""".
-spec get_unresolved_device_message( eurid() ) -> ustring().
get_unresolved_device_message( Eurid ) ->
   text_utils:format( "from device whose EURID is ~ts: unresolved device "
        "(not configured, and with no EEP known for it); "
        "this device will be ignored from now.",
        [ oceanic_text:eurid_to_string( Eurid ) ] ).
