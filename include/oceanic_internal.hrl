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


% Oceanic private, internal defines and records.



% Each telegram must start with:
-define( sync_byte, 85 ). % i.e. 0x55


% Most number of retries in terms of request sending:
-define( max_request_retries, 15 ).


% Definition of the overall state of an Oceanic server, including configuration
% (typically loaded from an ETF file).
%
-record( oceanic_state, {

    % The PID of the process in charge of the serial connection to the Enocean
    % gateway (USB dongle):
    %
    serial_server_pid :: oceanic:serial_server_pid(),


    % The (binary) path to the Enocean gateway (USB dongle), kept so that the
    % serial link can be reset if needed:
    %
    device_path :: oceanic:bin_device_path(),


    % To identify the pseudo-device emitter of any telegram to be sent by
    % Oceanic; by default this will be the actual base ID advertised by the
    % local USB gateway, as obtained thanks to the co_rd_idbase common command
    % (otherwise telegrams are likely not to be processed by the sender or
    % ignored by the receiver).
    %
    emitter_eurid = oceanic_text:string_to_eurid( ?default_emitter_eurid )
                                    :: oceanic:eurid(),


    % A table recording all information regarding the known Enocean devices:
    device_table :: oceanic:device_table(),


    % Tells whether, when receiving a teach-in or teach-out query (e.g. from a
    % smart plug put in learning mode), a teach-in/out acknowledgement response
    % shall be automatically sent back, so that this gateway becomes
    % registered/unregistered as a controller of the device that sent the
    % teach-in query.
    %
    % Teach-in could be accepted iff if the initiator device is defined in the
    % configuration.
    %
    % Does not do anything besides sending that acknowledgement response to the
    % taught device, and recording it is taught.
    %
    auto_ack_teach_queries = 'true' :: boolean(),


    % We enqueue (low-level) commands (not to be mixed up with higher-level
    % requests) sent to the Enocean module; they shall result in an
    % acknowledgement from the module (most of them; possibly all of them).
    %
    % Such acks, at least generally, just contain a corresponding return code -
    % nothing else that could be associated to a sender, a request, etc. - so we
    % ensure that at any time up to one of such commands is in the air (anyway
    % the module has a single thread of operation), and store in this queue the
    % next ones for a later sending thereof in turn, when the module is ready.
    %
    % We could see for example that sending an ERP1 packet that orresponds to
    % the F6-02-01 EEP results in the receiving by this server of a response
    % packet from the Enocean module.
    %
    % Indeed most operations, starting from telegram sending, result in the
    % Enocean module to send the host (this server) a command response of packet
    % type 2 which holds as payload only a single byte, the return code; in case
    % of success (by far the most general case) its value is RET_OK (i.e. 0;
    % resulting on the full 16-byte telegram being
    % <<85,0,1,0,2,101,0,0,85,0,1,0,2,101,0,0>>, hence data: <<0>> and optional
    % data: <<>>).
    %
    % So this acknowledgement does not say anything for example about any device
    % having received a telegram (the gateway will receive it even if no other
    % device exists), but it is useful for error management and possibly flow
    % control (not to overwhelm the module, if ever it was possible).
    %
    % Refer to [ESP3] p.17 for further information.
    %
    % This queue therefore contains any pending, not-yet-sent ESP3 commands (be
    % them requests for ERP1 commands, common commands, etc.); this is not a
    % queue for higher-level, applicative requests (which are separately
    % managed, each having a counterpart common command here, insofar as sending
    % a telegram implies a command to be issued).
    %
    command_queue :: oceanic:command_queue(),


    % Information about any currently waited (low-level) command that shall
    % result in an acknowledgement; so corresponds to any pending, sent but not
    % yet acknowledged ESP3 command whose response telegram is still waited for.
    %
    % Note that some devices apparently may be configured to not ack incoming
    % commands (however [ESP3] p.17 tells that "it is mandatory to wait for the
    % RESPONSE message"); in this case this information should be registered in
    % their enocean_device() record (by default acknowledgements are expected).
    %
    waited_command_info = 'undefined'
                                :: option( oceanic:waited_command_info() ),


    % The maximum waiting duration for a pending command, sent yet not
    % acknowledged:
    %
    command_wait_timeout = ?default_max_command_response_waiting_duration
                                    :: time_utils:time_out(),


    % The total number of commands issued:
    command_count = 0 :: basic_utils:count(),


    % The number of telegrams sent:
    sent_count = 0 :: basic_utils:count(),

    % The number of telegrams discarded, typically because they were out of
    % context (e.g. a response being received whereas no request is pending):
    %
    discarded_count = 0 :: basic_utils:count(),


    % The current level of recent sliding traffic, roughly monitored for
    % jamming:
    %
    traffic_level = 0 :: system_utils:bytes_per_second(),

    % The timestamp corresponding the last time incoming traffic was detected,
    % to determine jamming level:
    %
    last_traffic_seen :: time_utils:timestamp(),


    % The threshold above which an onEnoceanJamming event is triggered:
    jamming_threshold = ?default_jamming_threshold
                                    :: system_utils:bytes_per_second(),

    % A list of the PID of any processes listening for Enocean events:
    event_listeners = [] :: [ oceanic:event_listener_pid() ] } ).
