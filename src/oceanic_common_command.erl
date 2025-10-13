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

-module(oceanic_common_command).

-moduledoc """
Module centralising the management of the **common commands**.

Includes their encoding and decoding.
""".


% About Common commands.

% These exchanges take place locally, directly between the host (the computer at
% hand) and its connected Enocean (typically USB) module, based on a TCM 310
% chip.
%
% This corresponds to packet type 5; a host sends a ESP3 common command request
% to an EnOcean module, answered with a response message.
%
% See the `common_command` topic spec in the oceanic_constants/generated module,
% and [TCM] p.9.
%
% Apparently the TCM 310 supports only the following commands (see [TCM] p.9):
%  - co_wr_sleep to enter energy saving mode (deep sleep mode)
%  - co_wr_reset to reset the device
%  - co_rd_version to read sw/hw versions, chip id etc.
%  - co_rd_sys_log to read system log from device data base
%  - co_wr_sys_log to reset system log from device data base
%  - co_wr_bist to perform flash bist operation
%  - co_wr_idbase to write id range base number
%  - co_rd_idbase to read id range base number
%  - co_wr_repeater to configure repeater functionality
%  - co_rd_repeater to read repeater state
%  - co_wr_filter_add to add filter to filter list or to selective repeating
%  (up to 30 filters are supported)
%  - co_wr_filter_del to delete filter from filter list or from selective
%  repeating
%  - co_wr_filter_del_all to delete all filter
%  - co_wr_filter_enable to enable/disable supplied filters
%  - co_rd_filter to read supplied filters
%  - co_wr_wait_maturity to wait maturity time before returning radio telegrams
%  - co_wr_mem for writing into memory
%  - co_rd_mem for reading memory
%  - co_rd_mem_address to get addresses of special areas
%  - co_rd_dutycycle_limit to read information about current duty cycle
%    limitations



% For the common command related records:
-include("oceanic.hrl").


-doc """
Designates an ESP3 common command, like `co_wr_sleep` or `co_rd_repeater`, that
is addressed to the Enocean module.

Refer to `oceanic_{constants,generated}:get_maybe_common_command_topic_spec/0`
for further information.
""".
-type common_command_type() ::
    'co_rd_version'
  | 'co_rd_sys_log'
  | 'co_rd_idbase'
  | atom().



-doc """
Generic causes of failure for a common command request.

See also `oceanic_generated:get_return_code_topic_spec/0`.
""".
-type common_command_failure() :: 'error_return'
                                | 'unsupported_return'
                                | 'wrong_parameter_return'
                                | 'operation_denied'
                                | 'time_out'.

-doc "The status of a command.".
-type common_command_status() :: 'success' | common_command_failure().


-doc "Response to a successful `read version` common command request.".
-type read_version_response() :: #read_version_response{}.


-doc "Response to a successful `read logs` common command request.".
-type read_logs_response() :: #read_logs_response{}.


-doc """
Response to a successful `read base ID information` (`CO_RD_IDBASE`) common
command request.
""".
-type read_base_id_info_response() :: #read_base_id_info_response{}.



-doc "Designates a response to a common command request".
-type common_command_response() :: read_version_response()
                                 | read_logs_response()
                                 | read_base_id_info_response()
                                 | common_command_failure().



-doc "The response to a command, as sent back to the user.".
-type command_response() :: 'command_processed' | common_command_response().


-export_type([ common_command_type/0, common_command_failure/0,
               common_command_status/0, common_command_response/0,
               command_response/0,

               read_version_response/0, read_logs_response/0,
               read_base_id_info_response/0 ]).



-export([ read_version/1, read_logs/1, read_base_id_info/1,
          notify_requester/4,

          encode_common_command_tracking/1, encode_common_command/2,
          decode_response_tail/5, manage_failure_return/6 ]).



% Type shorthands:

-type requester() :: oceanic:requester().
-type telegram() :: oceanic:telegram().
-type telegram_tail() :: oceanic:telegram_tail().
-type telegram_data() :: oceanic:telegram_data().
-type telegram_data_tail() :: oceanic:telegram_data_tail().
-type telegram_opt_data() :: oceanic:telegram_opt_data().
-type oceanic_state() :: oceanic:oceanic_state().
-type oceanic_server_pid() :: oceanic:oceanic_server_pid().
-type command_tracking() :: oceanic:command_tracking().

-type decoding_outcome() :: oceanic_decode:decoding_outcome().



-doc """
Notifies the specified requester of the success response regarding the current
common command.
""".
-spec notify_requester( command_response(), requester(),
    option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
notify_requester( Response, _Requester=internal, NextMaybeTelTail, State ) ->

    cond_utils:if_defined( oceanic_debug_commands,
        trace_bridge:debug_fmt(
            "Returning the following internal response: ~ts.",
            [ oceanic_text:device_event_to_string( Response ) ] ) ),

    % We return directly the response event in that case:
    { decoded, Response, _MaybeDiscoverOrigin=undefined,
      _IsBackOnline=false, _MaybeDevice=undefined, NextMaybeTelTail, State };


notify_requester( Response, RequesterPid, NextMaybeTelTail, State ) ->

    cond_utils:if_defined( oceanic_debug_commands,
        trace_bridge:debug_fmt( "Sending back to requester ~w "
        "the following response: ~ts.",
        [ RequesterPid, oceanic_text:device_event_to_string( Response ) ] ) ),

    RequesterPid ! { oceanic_command_outcome, Response },

    { decoded, command_processed, _MaybeDiscoverOrigin=undefined,
      _IsBackOnline=false, _MaybeDevice=undefined, NextMaybeTelTail, State }.





% Section for the encoding of packets of the Common Command type.


-doc """
Returns the version information held by the Enocean module, thanks to a (local)
common command.
""".
-spec read_version( oceanic_server_pid() ) ->
                        read_version_response() | common_command_failure().
read_version( OcSrvPid ) ->
    send_common_command( _Cmd=co_rd_version, OcSrvPid ).



-doc """
Returns the log information held by the Enocean module, thanks to a (local)
common command.
""".
-spec read_logs( oceanic_server_pid() ) ->
                        read_logs_response() | common_command_failure().
read_logs( OcSrvPid ) ->
    send_common_command( _Cmd=co_rd_sys_log, OcSrvPid ).



-doc """
Returns the information held by the Enocean module about its base ID, thanks to
a (local) common command.
""".
-spec read_base_id_info( oceanic_server_pid() ) ->
                        read_base_id_info_response() | common_command_failure().
read_base_id_info( OcSrvPid ) ->
    send_common_command( _Cmd=co_rd_idbase, OcSrvPid ).



-doc "Sends the specified common command and returns its outcome.".
-spec send_common_command( common_command_type(), oceanic_server_pid() ) ->
            common_command_response() | common_command_failure().
send_common_command( CommonCmdType, OcSrvPid ) ->

    OcSrvPid ! { executeCommonCommand, CommonCmdType, self() },
    receive

        { oceanic_command_outcome, Outcome } ->
            Outcome

        % To debug:
        %Other ->
        %   trace_bridge:warning_fmt( "Received for common command '~ts': ~p",
        %                            [ CommonCmdType, Other ] )

    end.



-doc """
Encodes the specified common command, to be executed by the Enocean module.
""".
-spec encode_common_command_tracking( common_command_type() ) -> telegram().
% Future commands may have to be special-cased (e.g. if having parameters):
encode_common_command_tracking( _CmdType=co_rd_version ) ->
    encode_read_version_request();

encode_common_command_tracking( _CmdType=co_rd_sys_log ) ->
    encode_read_logs_request();

encode_common_command_tracking( _CmdType=co_rd_idbase ) ->
    encode_base_id_info_request();

encode_common_command_tracking( CmdType ) ->
    throw( { unknown_common_command_type, CmdType } ).



-doc """
Encodes a common command request of type `CO_RD_VERSION`, to read version
information from the Enocean module.

See its actual specification in `[ESP3]`, p.36, and the `decode_response_tail/5`
for `WaitedCmd=co_rd_version`.
""".
-spec encode_read_version_request() -> telegram().
encode_read_version_request() ->
    CmdNum = oceanic_generated:get_first_for_common_command( co_rd_version ),
    Data = <<CmdNum:8>>,
    encode_common_command( Data ).



-doc """
Encodes a common command request of type `CO_RD_SYS_LOG`, to read logs from the
Enocean module.

See its actual specification in `[ESP3]`, p.37, and the `decode_response_tail/5`
for `WaitedCmd=co_rd_sys_log`.
""".
-spec encode_read_logs_request() -> telegram().
encode_read_logs_request() ->
    CmdNum = oceanic_generated:get_first_for_common_command( co_rd_sys_log ),
    Data = <<CmdNum:8>>,
    encode_common_command( Data ).



-doc """
Encodes a common command request of type `CO_RD_IDBASE`, to read base ID
information from the Enocean module.

See its actual specification in `[ESP3]`, p.40, and the `decode_response_tail/5`
for WaitedCmd=co_rd_idbase.
""".
-spec encode_base_id_info_request() -> telegram().
encode_base_id_info_request() ->
    CmdNum = oceanic_generated:get_first_for_common_command( co_rd_idbase ),
    Data = <<CmdNum:8>>,
    encode_common_command( Data ).



-doc """
Encodes a common command request, based on the specified data (and with no
optional data defined).

The actual specification of common commands starts at p.32 of `[ESP3]`.
""".
-spec encode_common_command( telegram_data() ) -> telegram().
encode_common_command( Data ) ->
    oceanic_encode:encode_esp3_packet( _PacketType=common_command_type, Data ).



-doc """
Encodes a common command, based on the specified data and optional data.

The actual specification of common commands starts at p.32 of `[ESP3]`.
""".
-spec encode_common_command( telegram_data(), telegram_opt_data() ) ->
                                                telegram().
encode_common_command( Data, OptData ) ->
    oceanic_encode:encode_esp3_packet( _PacketType=common_command, Data,
                                       OptData ).



% Section for the decoding of packets of the Common Command type.
%
% Waited information expected to be already cleared, and timer cancelled.


-doc """
Actual decoding of responses to pending common commands.

Note that the return code has already been extracted, and corresponds to a
success.

The actual waiting information is expected to have been already cleared by the
caller.
""".
-spec decode_response_tail( command_tracking(), telegram_data_tail(),
                            telegram_opt_data(), option( telegram_tail() ),
                            oceanic_state() ) -> decoding_outcome().
% For (our) telegram_sending:
decode_response_tail(
        #command_tracking{ command_type=telegram_sending,
                          command_telegram=CmdTelegram,
                          requester=Requester },
        _RemainingDataTail= <<>>,
        _OptData= <<>>, NextMaybeTelTail,
        State ) ->

    % Returned code already checked to be ok_return, so:
    cond_utils:if_defined( oceanic_debug_commands,
        trace_bridge:debug_fmt( "Received a successful command acknowledgement "
            "regarding the sending of ~ts (on behalf of requester ~w).",
            [ oceanic_text:telegram_to_string( CmdTelegram ), Requester ] ),
        basic_utils:ignore_unused( [ CmdTelegram, Requester ] ) ),

    case Requester of

        internal ->
            ok;

        RequesterPid ->
            RequesterPid ! { onOceanicSendingOutcome, success }

    end,

    % Ignoring here any actual requester (e.g. not sending
    % {oceanic_command_outcome, Response }), as the convention is that it is
    % specifically notified (by a call to its onEnoceanDeviceEvent/4 method)
    % only in case of (final) failure:
    %
    %notify_requester( Response, _SetRequester=internal, NextMaybeTelTail,
    %                  State );

    { decoded, _Event=command_processed, _MaybeDiscoverOrigin=undefined,
      _IsBackOnline=undefined, _MaybeDevice=undefined, NextMaybeTelTail,
      State };


% For co_rd_version:
decode_response_tail(
        #command_tracking{ command_type=co_rd_version,
                          requester=Requester },
        _DataTail= <<AppVerMain:8, AppVerBeta:8, AppVerAlpha:8, AppVerBuild:8,
                     ApiVerMain:8, ApiVerBeta:8, ApiVerAlpha:8, ApiVerBuild:8,
                     % 16 bytes:
                     ChipId:32, ChipVer:32, AppDesc:16/binary>>,
        _OptData= <<>>, NextMaybeTelTail, State ) ->

    Response = #read_version_response{
        app_version={ AppVerMain, AppVerBeta, AppVerAlpha, AppVerBuild },
        api_version={ ApiVerMain, ApiVerBeta, ApiVerAlpha, ApiVerBuild },
        chip_id=ChipId,
        chip_version=ChipVer,
        app_description=text_utils:buffer_to_binstring( AppDesc ) },

    notify_requester( Response, Requester, NextMaybeTelTail, State );


% Non-matched data tail:
decode_response_tail( #command_tracking{ command_type=co_rd_version }, DataTail,
                      _OptData= <<>>, NextMaybeTelTail, State ) ->

    trace_bridge:error_fmt( "Received a response to a pending co_rd_version "
        "common command with an invalid data tail (~ts).",
        [ oceanic_text:telegram_to_string( DataTail ) ] ),

    { invalid, _ToSkipLen=0, NextMaybeTelTail, State };


% For co_rd_sys_log:
decode_response_tail( #command_tracking{ command_type=co_rd_sys_log,
                                        requester=Requester },
                      DataTail, OptData, NextMaybeTelTail, State ) ->

    % Nope, we have a series of size(OptData) APP log counters (e.g. 6 of them,
    % each starting initially at 255):
    %
    %MaybeDecodedOptData = decode_optional_data( OptData ),

    cond_utils:if_defined( oceanic_debug_decoding,
        % For example 38 API counters, each starting initially at 255:
        trace_bridge:debug_fmt( "DataTail (~B API log counters): ~w, "
            "OptData (~B APP log counters): ~w",
            [ size( DataTail ), DataTail, size( OptData ), OptData ] ) ),

    % Yes, this dispatching is the correct one:
    Response = #read_logs_response{ app_counters=binary_to_list( OptData ),
                                    api_counters=binary_to_list( DataTail ) },

    notify_requester( Response, Requester, NextMaybeTelTail, State );


% (apparently no restriction applies to DataTail, no non-matching clause to add)


% For co_rd_idbase:
decode_response_tail( #command_tracking{ command_type=co_rd_idbase,
                                        requester=Requester },
        _DataTail= <<BaseEurid:32>>,
        _OptData= <<RemainWrtCyclesNum:8>>, NextMaybeTelTail, State ) ->

    RemainWrtCycles = case RemainWrtCyclesNum of

        16#ff ->
            unlimited;

        Num ->
            Num

    end,

    Response = #read_base_id_info_response{
        base_eurid=BaseEurid,
        remaining_write_cycles=RemainWrtCycles },

    notify_requester( Response, Requester, NextMaybeTelTail, State );


decode_response_tail( #command_tracking{ command_type=co_rd_idbase }, DataTail,
                      OptData, NextMaybeTelTail, State ) ->

    trace_bridge:error_fmt( "Received a response to a pending co_rd_idbase "
        "common command with an invalid data tail (~ts) "
        "and/or optional data (~ts).",
        [ oceanic_text:telegram_to_string( DataTail ),
          oceanic_text:telegram_to_string( OptData ) ] ),

    { invalid, _ToSkipLen=0, NextMaybeTelTail, State };


% Other common commands:
decode_response_tail( OtherCmdReq, DataTail, OptData, NextMaybeTelTail,
                      State ) ->

    trace_bridge:error_fmt( "Responses to ~ts are currently "
        "unsupported (dropping response and waited request).~n"
        "Extra information: DataTail=~ts, OptData=~ts.",
        [ oceanic_text:command_tracking_to_string( OtherCmdReq ),
          oceanic_text:telegram_to_string( DataTail ),
          oceanic_text:telegram_to_string( OptData ) ] ),

    { unsupported, _ToSkipLen=0, NextMaybeTelTail, State }.



-doc "Manages a response packet reporting an error return code.".
-spec manage_failure_return( common_command_failure(),
        command_tracking(), telegram_data_tail(), telegram_opt_data(),
        option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
manage_failure_return( FailureReturn,
                       WaitedCmdTrk=#command_tracking{ command_type=CmdType,
                                                      requester=Requester },
                       DataTail, OptData, NextMaybeTelTail, State ) ->

    trace_bridge:error_fmt( "Received a failure response (~ts), presumably "
        "to the pending ~ts (data tail: ~w, optional data: ~w).",
        [ FailureReturn,
          oceanic_text:command_tracking_to_string( WaitedCmdTrk ),
          DataTail, OptData ] ),

    case CmdType of

        % Our special case:
        telegram_sending ->

            case Requester of

                internal ->
                    ok;

                RequesterPid ->
                    RequesterPid ! { onOceanicSendingOutcome, FailureReturn }

            end;

        % Other, base cases:
        _ ->
            case Requester of

                internal ->
                    ok;

                RequesterPid ->
                    RequesterPid ! { oceanic_command_outcome,
                                     _Outcome=FailureReturn }

            end

    end,

    % Waiting information already cleared:
    { decoded, _Event=command_processed, _MaybeDiscoverOrigin=undefined,
      _IsBackOnline=false, _MaybeDevice=undefined, NextMaybeTelTail, State }.
