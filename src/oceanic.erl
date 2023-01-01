% Copyright (C) 2022-2023 Olivier Boudeville
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


% @doc Main module of Ceylan-Oceanic, in order to <b>drive Enocean
% communications through an Oceanic server</b>.
%
-module(oceanic).


% Base API:
-export([

	get_default_tty_path/0, has_tty/0, has_tty/1,

	is_available/1,

	start/0, start/1, start/2,
	start_link/0, start_link/1, start_link/2,

	get_server_registration_name/0, get_server_pid/0,

	load_configuration/1, load_configuration/2,

	send/2,

	acknowledge_teach_request/2, acknowledge_teach_request/3,

	% For lower-level operation/testing:
	encode_esp3_packet/2, encode_esp3_packet/3,

	encode_double_rocker_switch_telegram/4,
	encode_double_rocker_multipress_telegram/4,

	% For any kind of (already-encoded) command:
	execute_command/2,

	% Useful to properly encode telegrams:
	get_oceanic_eurid/1,

	% For common commands:
	read_version/1, read_logs/1, read_base_id_info/1,


	decode_telegram/2,

	stop/0, stop/1, synchronous_stop/1,

	eurid_to_string/1, eurid_to_bin_string/1, eurid_to_bin_string/2,
	string_to_eurid/1, get_broadcast_eurid/0,

	get_best_naming/2,

	get_device_table/1,

	string_to_eep/1 ] ).


% For execution as (e)script:
-export([ secure_serial/1 ]).


% API for module generation:
-export([ generate_support_modules/0 ]).


% Exported only for testing:
-export([ get_test_state/0, get_test_state/1,
		  test_decode/1, secure_tty/1, try_integrate_chunk/4,

		  telegram_to_string/1,
		  telegram_to_hexastring/1, hexastring_to_telegram/1,

		  device_event_to_string/1, device_table_to_string/1,
		  state_to_string/1 ]).


% Silencing (depending on the tokens defined):
-export([ encode_common_command/2, ptm_module_to_string/1,
		  nu_message_type_to_string/1, optional_data_to_string/1,
		  maybe_optional_data_to_string/2, repeater_count_to_string/1 ]).


-type availability_outcome() ::
		{ 'true', SerialRootDir :: directory_path() }
	  | { 'false', Reason :: ustring(), basic_utils:error_term() }.
% The outcome of an availability check for Oceanic.


-type oceanic_server_pid() :: pid().
% The PID of an Oceanic server.


-type serial_server_pid() :: pid().
% The PID of a process in charge of a serial connection to the Enocean gateway
% (USB dongle).

-type event_listener_pid() :: pid().
% The PID of any process registered to an Oceanic server as a listener of
% Enocean events.


-type requester() :: pid() | 'internal'.
% The PID of the requester of a common command, or the 'internal' atom to tell
% that this is a request emitted by Oceanic for its own economy.


-type device_name() :: bin_string().
% A user-defined device name, a lot more convenient than a EURID.

-type device_plain_name() :: ustring().
% A user-defined device name, a lot more convenient than a EURID.

-type device_any_name() :: any_string().
% A user-defined device name, a lot more convenient than a EURID.


-type device_designator() :: eurid() | device_any_name().
% An element designating a device, either thanks to an EURID (as an integer), or
% thanks to a user-defined name (as any kind of string).


% For the device-related records:
-include("oceanic.hrl").


-type enocean_device() :: #enocean_device{}.
% Information regarding an Enocean device, as known by the Oceanic server.


-type device_table() :: table( eurid(), enocean_device() ).
% A table recording information regarding Enocean devices.


-type command_queue() :: queue:queue( command_request() ).
% A (FIFO) queue of command requests, to be sent in turn next.


-type device_config() ::

	{ UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring() }

  | { UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring(), Comment :: ustring() }.
% An entry in the Oceanic configuration (see its 'oceanic_devices' key) to
% describe a given device.


-type tty_detection_outcome() ::
		'true' | { 'false', 'non_existing' | { 'not_device', entry_type() } }.
% The outcome of an attempt of TTY detection.


-type serial_protocol() :: 'esp2' | 'esp3'.
% Defines the serial, bidirectional communication between a host and EnOcean
% modules.
%
% There are two Enocean serial protocols: ESP2 and ESP3.
% Oceanic focuses primarily on newer, richer, ESP3.
%
% The physical interface between a host and an EnOcean RF module (UART) is a
% 3-wire connection (Rx, Tx, GND / software handshake / full-duplex), modelled
% on RS-232 serial interfaces.


-type telegram() :: binary().
% A telegram is a raw (non-decoded) series of bytes that have been received or
% are to be sent. Ideally a telegram would correspond to a full, unitary radio
% ESP3 packet typically received from an Enocean gateway.
%
% E.g. `<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>',
% `<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>' or
% `<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>'.


-type telegram_chunk() :: binary().
% A telegram chunk is a partial telegram, possibly a full ESP3 packet.


-type telegram_data() :: telegram_chunk().
% The part of a telegram with the base, normalised, stable data (corresponding
% to the actual payload of an ESP3 packet, which can be for example an ERP1
% radio packet), possibly to be complemented with optional data.


-type telegram_data_tail() :: telegram_chunk().
% A base, normalised, stable data of a telegram once its initial byte (typically
% R-ORG or Return Code) has already been chopped.


-type telegram_opt_data() :: telegram_chunk().
% The (encoded) part of a telegram with the optional data that may
% complement/extend the base data.
%
% See also decoded_optional_data/0.


-type decoded_optional_data() :: { subtelegram_count(), eurid(), maybe( dbm() ),
								   maybe( security_level() ) }.
% The decoded data for the optional part of the Packet Type 1 (RADIO_ERP1)
% telegrams.
%
% See also telegram_opt_data/0.


-type subtelegram_count() :: count().
% A number of subtelegrams.


-type dbm() :: integer().
% The best RSSI value, expressed in decibels (dB) with reference to one
% milliwatt (mW), of all received subtelegrams.
%
% Here a negative value, like -6 dBm.
%
% Of course only applies when receiving telegrams (not sending).


-type security_level() :: 'not_processed'
						| 'obsolete' % A deprecated security concept
						| 'decrypted'
						| 'authenticated'
						| 'decrypted_and_authenticated'.
% The level of security of a received telegram.
%
% Only applies when receiving telegrams (when sending, security is selected by
% link table entries).


-type communication_direction() :: 'unidirectional' | 'bidirectional'.

-type teach_request_type() :: 'teach_in' | 'teach_out'.

-type teach_outcome() :: 'teach_refused'          % "General reason"
					   | 'teach_in_accepted'      % Addition success
					   | 'teach_out_accepted'     % Deletion success
					   | 'teach_eep_unsupported'. % EEP not supported


-type channel_taught() :: uint8() | 'all'.
% Tells which channel(s) should be taught.


-type manufacturer_id () :: uint8().
% Identifier of device manufacturer.


% Since EEP 3.0:

-type rorg() :: uint8().
% Radio ORG (organization number / Radio-telegram types grouped
% ORGanizationally); describes the ERP radio telegram type, as an
% identifier. Equivalent of "Choice".

-type func() :: uint8().
% Describes the basic functionality of the data content.

-type type() :: uint8().
% Describes the type of device in its individual characteristics.


-type eep() :: { rorg(), func(), type() }.
% An EEP defines the coding of the data to be exchanged, so that two devices
% complying to the same EEP can be interchanged.


-type eep_id() :: 'thermo_hygro_low'
				| 'thermo_hygro_mid'
				| 'thermo_hygro_high'
				| 'push_button'

				  % They include the simple rocker ones:
				| 'double_rocker_switch'
				| 'double_rocker_multipress'

				| 'single_input_contact'
				| 'single_channel_module'
				| 'double_channel_module'
				| atom().
% The (atom) identifier of an EnOcean Equipment Profile, corresponding to
% (R-ORG)-(FUNC)-(TYPE) triplet.
%
% For example the 'single_input_contact' EEP identifier corresponds to EEP
% D5-00-01.
%
% Refer to get_eep_topic_specs/0 for further details.


-type eep_string() :: ustring().
% An EEP defined as a string (e.g. "D5-00-01").


-type discovery_origin() ::
	'configuration'  % Loaded from Oceanic's user-defined configuration
  | 'listening'      % Passively listened from trafic
  | 'teaching'.      % Through a teach-in mechanism
% Tells how a device was discovered by Oceanic.


-type enum() :: integer().
% An enumeration specified in the protocol.


-type button_designator() ::
		'button_ai'  % Switch light on / Dim light down / Move blind closed
	  | 'button_ao'  % Switch light off / Dim light up / Move blind open
	  | 'button_bi'  % Switch light on / Dim light down / Move blind closed
	  | 'button_bo'. % Switch light off / Dim light up / Move blind open
% Designates a button corresponding to a A or B channel.
%
% A given ocker behaves as two buttons (e.g. AI/AO).
%
% In application style 1, the O position is the top/up one, while the I position
% is the bottom/down one.


-type button_transition() :: 'pressed' | 'released'.
% Tells whether a button has been pressed (and held) or released.


-type button_counting() :: 'none' | 'three_or_four'.
% A "number" of buttons, typically involved in a multipress event.


-type contact_status() :: 'open' | 'closed'.
% Tells whether a contact is open or closed.


-type ptm_switch_module_type() :: 'ptm1xx' % synonymous for module PTM1xx
								| 'ptm2xx'. % synonymous for module PTM2xx
% The types of PTM switch modules (radio emitter), as defined in RPS packets.


-type nu_message_type() :: 'normal' | 'unassigned'
						 | 'unknown_type_2' | 'unknown_type_3'.
% "Nu" Message type, as defined in RPS packets.

-type repetition_count() :: count().
% A number of repetitions, typically in a RPS packet.


-type temperature_range() :: 'low'   % 0°C to +40°C (A5-04-01)
						   | 'high'. % -20°C to +60°C (A5-04-02)
% The range of a temperature sensor.


-type vld_rcp_message_type() ::
	'a'  % ID 01
  | 'b'  % ID 02
  | 'c'  % ID 03
  | 'd'  % ID 04
  | 'e'  % ID 05
  | 'f'  % ID 06 (non-existing)
  | 'g'  % ID 07 (non-existing)
  | 'h'. % ID 08 (non-existing)
% The type of a VLD message, in the context of the D2-00 EEPs: "Room Control
% Panel (RCP)".
%
% It is also designated by the MI field of these VLD telegrams, the 3 last bits
% of the first byte of the payload (hence 8 possible values).
%
% Described in [EEP-spec] p.127.


-type vld_d2_00_cmd() :: 'actuator_set_output'
					   | 'actuator_set_local'
					   | 'actuator_status_query'
					   | 'actuator_status_response'
					   | 'actuator_set_measurement'
					   | 'actuator_measurement_query'
					   | 'actuator_measurement_response'
					   | 'actuator_set_pilot_wire_mode'
					   | 'actuator_pilot_wire_mode_query'
					   | 'actuator_pilot_wire_mode_response'
					   | 'actuator_set_external_interface_settings'
					   | 'actuator_external_interface_settings_query'
					   | 'actuator_external_interface_settings_response'.
% The type of a VLD message, in the context of the D2-01 EEPs: "Electronic
% switches and dimmers with Energy Measurement and Local Control".
%
% Refer to the 'vld_d2_00_cmd' topic.
%
% It is also designated by the CMD field of these VLD telegrams, the 4 last bits
% of the first byte of the payload (hence 16 possible values).
%
% Described in [EEP-spec] p.131.



-type decoding_error() ::
	  'not_reached'   % Beginning of next packet still ahead
	| 'incomplete'    % Truncated packet (end of packet still ahead)
	| 'invalid'       % Corrupted packet
	| 'unsupported'   % Type of packet (currently) unsupported by Oceanic
	| 'unconfigured'. % Device not configure (typically EEP not known)
% The various kinds of errors that may happen when decoding.


-type decoding_outcome() ::

	{ decoding_error(), ToSkipLen :: count(), NextChunk :: telegram_chunk(),
	  oceanic_state() }

  | { 'decoded', maybe( device_event() | 'command_processed' ),
	  NextChunk :: telegram_chunk(), oceanic_state() }.
% The outcome of an attempt of integrating / decoding a telegram chunk.
%
% A maybe-event is returned, as for example a common command response received
% whereas no request was sent shall be discarded.



-type decoding_result() :: decoding_error() | device_event().
% The result of a decoding request.


-type eurid() :: type_utils:uint32(). % Previously `<<_:32>>'.
% EURID (EnOcean Unique Radio Identifier) is a unique and non-changeable
% identification number (as a 32-bit value) assigned to every EnOcean
% transmitter during its production process.
%
% The EURID corresponds to the hexadecimal identifier typically labelled at the
% back of devices (e.g. "ID: B50533EC").
%
% Our EURIDs are defined and stored in uppercase, as they are generally written
% on devices.
%
% A specific EURID is 0xff-ff-ff-ff (see the eurid_broadcast define), which
% denotes a broadcast transmission (as opposed to an Addressed Transmission,
% ADT).


-type eurid_string() :: ustring().
% An EURID, expressed as a string.
%
% For example: "B50533EC".


-type packet() :: binary().
% An ESP-level data unit.


-type crc() :: byte().
% The CRC (Cyclic Redundancy Check) or polynomial code checksum of a
% sub-telegram/packet can be computed.
%
% Remainder on 8 bits of the modulo 2 division of the G(x) = x^8 + x^2 + x^1 +
% x^0 polynom.


-type esp3_packet() :: binary().
% An ESP3 data unit, consisting of Header, Data and Optional Data.
%
% The semantics of the optional data is defined by the packet type, it can be
% used to extend an existing ESP3 packet.


-type packet_type() :: 'reserved' | 'radio_erp1' | 'response'
					 | 'radio_sub_tel' | 'event' | 'common_command'
					 | 'smart_ack_command' | 'remote_man_command'
					 | 'radio_message' | 'radio_erp2' | 'radio_802_15_4'
					 | 'command_2_4'.
% The type of a (typically ESP3) packet.
%
% Refer to [ESP3] p.12.
%
% After a radio_erp1, radio_sub_tel or remote_man_command packet, a response
% packet is expected.
%
% See also the 'packet_type' topic in the oceanic_generated module.


-type payload() :: binary().
% The payload of a (typically ESP3) packet, a sequence of bytes sometimes
% designated as 'DataTail', that is all bytes in the "data" chunk (as opposed to
% the "optional data" one) found after the R-ORG one.
%
% Such a payload corresponds to a packet of a given type (e.g. an ERP1 radio
% packet, encapsulated in an ESP3 packet).


-type vld_payload() :: binary().
% The actual payload of a VLD (D2) packet.
%
% VLD telegrams carry a variable payload between 1 and 14 bytes, depending on
% their design.



%-type decode_result() :: 'ok' | 'incomplete' | 'crc_mismatch'.


%-type read_outcome() ::
%    { ReadMessage :: device_event(), AnyNextChunk :: telegram_chunk() }.
% Outcome of a (blocking) request of telegram reading.
%
% Exactly one event will be read (any remainding chunk returned), possibly
% waiting for it indefinitely.
%
% No content can remain to be skipped here, as by design we ended when having
% read a new event, returning any next chunk as it is.


-type enocean_version() :: { Main :: version_number(), Beta :: version_number(),
					Alpha :: version_number(), Build :: version_number() }.
% The version of an EnOcean application or API.
%
% This is a basic_utils:four_digit_version().


-type log_counter() :: type_utils:uint8().
% A log counter, starting from 255 downward.


-type log_counters() :: [ log_counter() ].
% A series of log entries of an USB gateway.


-type command_type() :: 'device_command' | common_command().
% Type information regarding a command.

-type command_request() :: #command_request{}.
% Allows to keep track of an ongoing command request.

-type command_outcome() :: command_response() | 'time_out'.
% The (synchronous) outcome of a sent command.


-type waited_command_info() :: { command_request(), maybe( timer_ref() ) }.
% Tracking information regarding a currently pending command.
%
% A timer is used for most commands, except typically internally-triggered
% common commands.


% Event types ordered here as well by increasing EEP:


-type thermo_hygro_event() :: #thermo_hygro_event{}.
% Event sent by EEP A5-04-01: "Temperature and Humidity Sensor" (with any
% range).
%
% Refer to [EEP-spec] p.35 for further details.



-type single_input_contact_event() :: #single_input_contact_event{}.
% Event sent by EEP D5-00-01: Single Input Contact.
%
% D5-00 corresponds to Contacts and Switches.
%
% Refer to [EEP-spec] p.27 for further details.
%
% Note that, at least by default, most if not all opening detectors not only
% report state transitions (between closed and opened), they also notify
% regularly (e.g. every 5-30 minutes, on average often 15 minutes) and
% spontaneously their current state (even if no specific transition happened),
% presumably to help overcoming any message loss.
%
% So any listener of these events shall store their current state, to be able to
% detect the actual transitions (even if they are late).


-type push_button_event() :: #push_button_event{}.
% Event sent in the context of EEP F6-01 ("Switch Buttons (with no rockers)").
%
% Refer to [EEP-spec] p.15 for further details.


%-type rocker_switch_event() :: #rocker_switch_event{}.


-type double_rocker_switch_event() :: #double_rocker_switch_event{}.
% Event sent in the context of EEP F6-02-01 ("Light and Blind Control -
% Application Style 1"), for T21=1.
%
% Refer to [EEP-spec] p.16 for further details.


-type double_rocker_multipress_event() :: #double_rocker_multipress_event{}.
% Event sent in the context of EEP F6-02-01 ("Light and Blind Control -
% Application Style 1"), for T21=1 and NU=0.
%
% Refer to [EEP-spec] p.16 for further details.


%-type position_switch_event() :: #position_switch_event{}.


-type teach_request() :: #teach_request{}.
% Message (hence not an event per se) corresponding to the receiving a R-ORG
% telegram for an universal Teach-in request, EEP based (UTE), one way of
% pairing devices.
%
% Refer to [EEP-gen] p.17 for further details.



-type read_version_response() :: #read_version_response{}.
% Response to a successful 'read version' common command request.

-type read_logs_response() :: #read_logs_response{}.
% Response to a successful 'read logs' common command request.

-type read_base_id_info_response() :: #read_base_id_info_response{}.
% Response to a successful 'read base ID information' (CO_RD_IDBASE) common
% command request.


-type common_command_response() :: read_version_response()
								 | read_logs_response()
								 | read_base_id_info_response()
								 | common_command_failure().
% Designates a response to a common command request.


-type command_response() :: 'command_processed' | common_command_response().
% The response to a command, as sent back to the user.


-type device_event() ::
		% Device events:
		thermo_hygro_event()
	  | single_input_contact_event()
	  | push_button_event()
	  | double_rocker_switch_event()
	  | double_rocker_multipress_event()

		% Other events:
	  | teach_request()
	  | command_response().
% Any event notified by an EnOcean device.


-type common_command() :: 'co_rd_version' | 'co_rd_sys_log'
						| 'co_rd_idbase' | atom().
% Designates an ESP3 command, like co_wr_sleep or co_rd_repeater.
%
% Refer to oceanic_generated:get_common_command_topic_spec/0 for further
% information.


-type common_command_failure() :: 'error_return'
								| 'not_supported_return'
								| 'wrong_parameter_return'
								| 'operation_denied'
								| 'time_out'.
% Generic causes of failure for a common command request.
%
% See also oceanic_generated:get_return_code_topic_spec/0.




-export_type([ availability_outcome/0,

			   oceanic_server_pid/0, serial_server_pid/0, event_listener_pid/0,
			   requester/0,

			   device_name/0, device_plain_name/0, device_any_name/0,
			   device_designator/0,

			   enocean_device/0, device_table/0, device_config/0,
			   tty_detection_outcome/0, serial_protocol/0,

			   telegram/0, telegram_chunk/0,
			   telegram_data/0, telegram_data_tail/0,
			   telegram_opt_data/0, decoded_optional_data/0,

			   subtelegram_count/0, dbm/0, security_level/0,
			   communication_direction/0, teach_request_type/0, teach_outcome/0,
			   channel_taught/0, manufacturer_id/0,

			   rorg/0, func/0, type/0, eep/0, eep_id/0, eep_string/0,

			   button_designator/0, button_transition/0, button_counting/0,

			   contact_status/0,
			   ptm_switch_module_type/0, nu_message_type/0, repetition_count/0,
			   temperature_range/0,
			   vld_rcp_message_type/0, vld_d2_00_cmd/0,
			   decoding_outcome/0,

			   eurid/0, eurid_string/0,
			   packet/0, crc/0, esp3_packet/0, packet_type/0, payload/0,
			   enocean_version/0, log_counter/0, log_counters/0,
			   command_type/0, command_request/0, command_outcome/0,

			   thermo_hygro_event/0, single_input_contact_event/0,
			   push_button_event/0,
			   double_rocker_switch_event/0, double_rocker_multipress_event/0,

			   device_event/0,

			   common_command/0, common_command_failure/0,

			   read_version_response/0, read_logs_response/0,
			   read_base_id_info_response/0,
			   common_command_response/0,

			   command_response/0 ]).




% Implementation notes:
%
% Oceanic relies on the 'oceanic_generated' module, which is automatically
% generated (as a result, no oceanic_generated.erl file ever exists) from the
% generate_support_modules/0 function at the bottom of the current file (see
% also the local 'all' make target).
%
% Regarding packet parsing, consider reading first
% https://www.erlang.org/doc/programming_examples/bit_syntax.html#segments and
% https://www.erlang.org/doc/reference_manual/expressions.html#bit_syntax to be
% sufficiently familiar with segments and the Value:Size/TypeSpecifierList
% notation, where:
%
%  - if no suffix is specified, the default type is (unsigned big-endian)
%  integer, and a single one (unit: 1, hence bit-addressed)
%
%  - for a binary (hence using the /binary suffix), the default addressed
%  element is a byte (unit: 8, type: bit, hence byte-addressed)

% Depends on Ceylan-Myriad and Serial; refer to http://oceanic.esperide.org.
%
% Telegrams may arrive corrupted or truncated. For example, <<85,0,7,7>> may be
% received, then <<1,122,213,9,5,5,51,236,0,1,255,255,255,255,45,0,173>>,
% whereas the actual telegram is actually the concatenation of the two.  We do
% not want to lose/ignore, as if some are transient (e.g. sensor readings),
% others are one-off and may matter quite a lot (e.g. opening detection).

% An Oceanic server could have been a WOOPER instance.


% Local types:

-type esp3_header() :: <<_:32>>.
% 32-bit; could have been ``type_utils:uint32()''.




% Internal defines.


% Each telegram must start with:
-define( sync_byte, 85 ).


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


% The default maximum waiting duration, in milliseconds, for a pending command,
% sent yet not acknowledged:
%
-define( default_max_response_waiting_duration, 1000 ).


% The default threshold, in bytes per second (hence roughly a dozen legit
% telegrams per second) above which an onEnoceanJamming event is triggered:
%
-define( default_jamming_threshold, 250 ).

% To test detection:
%-define( default_jamming_threshold, 25 ).



% Protocol notes:

% ERP means *EnOcean Radio Protocol*; discusses telegrams.
% The EnOcean Radio Protocol (ERP) covers the first three layers of the OSI
% model: Physical, Data Link and Network.
%
% By default, 8 bits of data, no parity, 1 stop bit.
% Optional repeater mode.
% Some modules support multiple channels.

% ESP means *EnOcean Serial Protocol*; discusses packets. 1 start bit (always at
% 0), 8 bits of data (from LSB to MSB), no parity bit, 1 stop bit (always at
% 1). Taking into account the esp3_speed define, this corresponds in terms of
% configuration serial settings to "57600 8N1".

% EEP means *EnOcean Equipment Profiles*; discusses devices.


% Below, regarding documentation:
%
% - "[EEP-gen]" refers to the "EnOcean Equipment Profiles" (see
% EnOcean-Equipment-Profiles-3-1-1.pdf, available from
% https://www.enocean-alliance.org/eep/); 36 pages
%
% - "[EEP-spec]" refers to the "EEP Specification" (see
% EnOcean_Equipment_Profiles_EEP_v2.6.7_public.pdf, available from
% https://www.enocean-alliance.org/wp-content/uploads/2017/05/); 270 pages
%
% - "[ESP3]" refers to the "Enocean Serial Protocol (ESP3) Specification" (see
% EnOceanSerialProtocol3.pdf, available from https://www.enocean.com/esp,
% typically resolving in
% https://www.enocean.com/wp-content/uploads/Knowledge-Base/EnOceanSerialProtocol3.pdf);
% 116 pages
%
% - "[TCM]" refers to the "TCM 310 / TCM 310U Transceiver Gateway Module User
% Manual" (https://www.enocean.com/en/product/tcm-310/?frequency=868); 29 pages
%
% Regarding code: [PY-EN] designates Python EnOcean
% (https://github.com/kipe/enocean).


% How to determine the EEP (i.e. RORG-FUNC-TYPE) of a given device?
%
% First, RORG is transmitted for at least most packet types. FUNC and TYPES
% remain to be determined.
%
% This depends on the type of packets that it emits:
%
% - for RPS: the controller must have been configured out of the band (typically
% here in an Oceanic ETF configuration file), to know a priori, for each
% device/Sender ID of interest what is its corresponding EEP (as the full EEP
% cannot be deduced solely from the packet)
%
% - for 1BS: a bit tells whether the telegram at hand is a teach-in one
%
% - for 4BS: 3 variations exist, some allowing to exchange EEPs
%
% See [EEP-gen] starting from p.18 for more details.


% Common command section.

% These exchanges take place locally, directly between the host (the computer at
% hand) and its connected USB Enocean gateway, based on a TCM 310 chip.
%
% This corresponds to packet type 5; a host sends a ESP3 common command request
% to an EnOcean module, answered with a response message.
%
% See the 'common_command' topic spec in the oceanic_constants/generated module,
% and [TCM] p.9.
%
% Apparently the TCM 310 supports only the following commands (see [TCM] p.9):%
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




% Learning/teach-in section.

% Frequently, a learn button on the receiver triggers the teach-in process; then
% the Sender-ID of an arriving telegram is interpreted as an authorized
% information source. To prevent unwanted devices from being learned, the input
% sensitivity of the receiver is often reduced, and the device to be learned
% should be placed close by the receiver. Some transmitters can also be
% switched into the learn-mode via a remote management command. To avoid
% inadvertent learning the RPS telegrams have to be triggered 3 times
% within 2 seconds.


% Definition of the overall state of an Oceanic server.
-record( oceanic_state, {

	% The PID of the process in charge of the serial connection to the Enocean
	% gateway (USB dongle):
	%
	serial_server_pid :: serial_server_pid(),

	% To identify the pseudo-device emitter of any telegram to be sent by
	% Oceanic; by default this will be the actual base ID advertised by the
	% local USB gateway, as obtained thanks to the co_rd_idbase common command.
	%
	emitter_eurid = string_to_eurid( ?default_emitter_eurid ) :: eurid(),

	% A table recording all information regarding the known Enocean devices:
	device_table = table:new() :: device_table(),


	% We enqueue command requests that shall result in an acknowledgement (most
	% of them; possibly all of them), as such acks, at least generally, just
	% contain a corresponding return code - nothing else that could be
	% associated to a sender, a request, etc.; so we ensure that at any time up
	% to one of such commands is in the air, and store in this queue the next
	% ones for a later sending thereof in turn.
	%
	% We could see for example that sending an ERP1 packet for the F6-02-01 EEP
	% does result in the receiving of a response packet (with a success return
	% code).
	%
	% So this queue contains any pending, not-yet-sent ESP3 commands (be
	% them requests for ERP1 commands, common commands, etc.):
	%
	command_queue = queue:new() :: command_queue(),


	% Information about any currently waited command request that shall result
	% in an acknowledgement; so corresponds to any pending, sent but not yet
	% acknowledged ESP3 command whose response telegram is still waited for.
	% Note that some devices apparently may be configured to not ack incoming
	% commands (however [ESP3] p.17 tells that "it is mandatory to wait for the
	% RESPONSE message"); in this case this information should be registered in
	% their enocean_device() record.
	%
	waited_command_info :: maybe( waited_command_info() ),


	% The maximum waiting duration for a pending command, sent yet not
	% acknowledged:
	%
	wait_timeout = ?default_max_response_waiting_duration
									:: time_utils:time_out(),


	% The total number of commands issued:
	command_count = 0 :: count(),


	% The number of telegrams sent:
	sent_count = 0 :: count(),

	% The number of telegrams discarded, typically because they were out of
	% context (e.g. a response being received whereas no request is pending):
	%
	discarded_count = 0 :: count(),


	% The current level of recent sliding traffic, roughly monitored for
	% jamming:
	%
	traffic_level = 0 :: bytes_per_second(),

	% The timestamp corresponding the last time incoming traffic was detected:
	last_traffic_seen = time_utils:get_timestamp() :: timestamp(),

	% The threshold above which an onEnoceanJamming event is triggered:
	jamming_threshold = ?default_jamming_threshold :: bytes_per_second(),


	% The PID of any process listening for Enocean events:
	event_listener_pid :: maybe( event_listener_pid() ) } ).


-type oceanic_state() :: #oceanic_state{}.
% An Oceanic state, including configuration typically loaded from an ETF file.


-type timer_ref() :: timer:tref().


% Shorthands:

-type count() :: basic_utils:count().
-type version_number() :: basic_utils:version_number().

-type any_file_path() :: file_utils:any_file_path().
-type directory_path() :: file_utils:directory_path().
-type device_path() :: file_utils:device_path().
-type entry_type() :: file_utils:entry_type().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type byte_size() :: system_utils:byte_size().
-type bytes_per_second() :: system_utils:bytes_per_second().

-type uint8() :: type_utils:uint8().

-type timestamp() :: time_utils:timestamp().

-type percent() :: math_utils:percent().

-type registration_name() :: naming_utils:registration_name().

-type celsius() :: unit_utils:celsius().



% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% @doc Tells whether Oceanic should be available, that is if all its
% prerequisites seem to be met.
%
% Useful to de-risk a future launch thereof and factor code.
%
-spec is_available( device_path() ) -> availability_outcome().
is_available( TtyPath ) ->

	case has_tty( TtyPath ) of

		true ->
			% Not necessarily in the ~/Software tree:
			case code_utils:is_beam_in_path( serial ) of

				not_found ->
					ReasonStr = text_utils:format(
						"The 'serial' module is not found, whereas the ~ts~n"
						"Has our fork of 'serial' been already installed? "
						"Please refer to Oceanic's documentation.",
						[ code_utils:get_code_path_as_string() ] ),

					{ false, ReasonStr, _ErrorTerm=serial_library_not_found };


				[ SinglePath ] ->

					% We have typically
					% ~/Software/erlang-serial/ebin/serial.beam, so:
					%
					SerialRootDir = file_utils:get_base_path(
						file_utils:get_base_path( SinglePath ) ),

					{ true, SerialRootDir };


				MultiplePaths ->

					ReasonStr = text_utils:format(
						"The 'serial' module has been found in "
						"multiple locations (which is abnormal): ~ts.",
						[ text_utils:strings_to_listed_string(
							MultiplePaths ) ] ),

					{ false, ReasonStr,
					  _ErrorTerm=multiple_serial_libraries_found }

			end;


		{ false, non_existing } ->

			ReasonStr = text_utils:format( "The specified TTY, '~ts', "
				"does not exist. Is the device for the Enocean gateway "
				"plugged in, and named accordingly?", [ TtyPath ] ),

			ErrorTerm = { non_existing_tty, TtyPath },

			{ false, ReasonStr, ErrorTerm };


		{ false, { not_device, OtherType } } ->

			ReasonStr = text_utils:format( "The specified TTY for the "
				"Enocean gateway, '~ts', is not a device but a ~ts.",
				[ TtyPath, OtherType ] ),

			ErrorTerm = { not_a_device, TtyPath, OtherType },

			{ false, ReasonStr, ErrorTerm }

	end.



% Start subsection, with no link.


% @doc Starts the Enocean support, based on our default conventions regarding
% the TTY allocated to the USB Enocean gateway; returns the PID of the launched
% Oceanic server, which registered the calling process as its Enocean event
% listener (but did not link to it).
%
% Throws an exception if no relevant TTY can be used.
%
-spec start() -> oceanic_server_pid().
start() ->
	start( get_default_tty_path() ).



% @doc Starts the Enocean support, based on the specified device path to the TTY
% allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
% server, which registered the calling process as its Enocean event listener
% (but did not link to it).
%
% Throws an exception if no relevant TTY can be used.
%
-spec start( device_path() ) -> oceanic_server_pid().
start( TtyPath ) ->
	start( TtyPath, _EventListenerPid=self() ).



% @doc Starts the Enocean support, based on the specified path to the TTY
% allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
% server (which is not linked to the calling process), registering the specified
% listener process for Enocean events.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start( device_path(), event_listener_pid() ) -> oceanic_server_pid().
start( TtyPath, EventListenerPid ) ->
	?myriad_spawn( fun() -> oceanic_start( TtyPath, EventListenerPid ) end ).




% Start subsection, with link.


% @doc Starts the Enocean support, based on our default conventions regarding
% the TTY allocated to the USB Enocean gateway; returns the PID of the launched
% Oceanic server, which registered the calling process as its Enocean event
% listener and linked to it.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start_link() -> oceanic_server_pid().
start_link() ->
	start_link( get_default_tty_path() ).


% @doc Starts the Enocean support, based on the specified device path to the TTY
% allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
% server, which registered the calling process as its Enocean event listener and
% linked to it.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start_link( device_path() ) -> oceanic_server_pid().
start_link( TtyPath ) ->
	start_link( TtyPath, _EventListenerPid=self() ).



% @doc Starts the Enocean support, based on the specified path to the TTY
% allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
% server, which is linked to the calling process and registers the specified
% listener process for Enocean events.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start_link( device_path(), event_listener_pid() ) -> oceanic_server_pid().
start_link( TtyPath, EventListenerPid ) ->
	?myriad_spawn_link( fun() ->
							oceanic_start( TtyPath, EventListenerPid )
						end ).



% TTY subsection.


% @doc Returns the path to the default TTY allocated to the USB Enocean gateway,
% according to our conventions.
%
-spec get_default_tty_path() -> device_path().
get_default_tty_path() ->

	% Better than, say:
	%"/dev/ttyUSB0".

	"/dev/ttyUSBEnOcean".


% @doc Tells whether the default TTY exists and is a device.
%
% Useful at least for testing.
%
-spec has_tty() -> tty_detection_outcome().
has_tty() ->
	has_tty( get_default_tty_path() ).



% @doc Tells whether the specified TTY exists and is a device, together with any
% failure reason.
%
% Useful at least for testing.
%
-spec has_tty( device_path() ) -> tty_detection_outcome().
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



% @doc Secures the TTY connection to the Enocean gateway.
-spec secure_tty( device_path() ) -> serial_server_pid().
secure_tty( TtyPath ) ->

	SerialRootDir = case is_available( TtyPath ) of

		{ true, SerialDir } ->
			SerialDir;

		{ false, ReasonStr, ErrorTerm } ->
			trace_bridge:error( ReasonStr ),
			throw( ErrorTerm )

	end,

	% Determined explicitly, as in an escript context the 'priv' directory of
	% erlang-serial may resolve into the one of Oceanic:
	%
	SerialPrivDir = file_utils:join( SerialRootDir, "priv" ),

	% Symmetrical speed here (in bits per second):
	Speed = ?esp3_speed,

	% No parity (no {parity_even} / {parity_odd}).

	% Parameters correspond to "57600 8N1".

	% Linked process:
	SerialPid = serial:start( [ { open, TtyPath },
								{ speed, Speed } ],
								%{ speed, _In=Speed, _Out=Speed } ]
							   SerialPrivDir ),

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Using TTY '~ts' to connect to Enocean gateway,"
			" corresponding to serial server ~w (speed: ~B bits per second).",
			[ TtyPath, SerialPid, Speed ] ) ),

	SerialPid.



% Oceanic server process subsection.

-spec oceanic_start( device_path(), maybe( event_listener_pid() )  ) ->
											no_return().
oceanic_start( TtyPath, MaybeEventListenerPid ) ->

	SerialPid = secure_tty( TtyPath ),

	BaseState = get_base_state( SerialPid ),

	LoadedState = load_configuration( BaseState ),

	naming_utils:register_as( ?oceanic_server_reg_name, _RegScope=local_only ),

	InitialState = LoadedState#oceanic_state{
		event_listener_pid=MaybeEventListenerPid },

	oceanic_loop( _SkipLen=0, _MaybeAccChunk=undefined, InitialState ).



% @doc Returns a base, blank yet initialised, Oceanic state.
-spec get_base_state( serial_server_pid() ) -> oceanic_state().
get_base_state( SerialServerPid ) ->

	% We discover from start our base EURID; a direct, ad hoc logic cannot
	% really be used, as request tracking would be in the way:

	cond_utils:if_defined( oceanic_debug_tty,
		trace_utils:debug( "Discovering our base EURID." ) ),

	CommonCmd = co_rd_idbase,

	CmdTelegram = encode_common_command_request( CommonCmd ),

	% For decoding re-use (try_integrate_chunk/4), we have to have a state
	% anyway:

	InitCmdReq = #command_request{ command_type=CommonCmd,
								   command_telegram=CmdTelegram,
								   requester=internal },

	InitialState = #oceanic_state{
		serial_server_pid=SerialServerPid,
		waited_command_info={ InitCmdReq, _MaybeTimerRef=undefined } },

	SentState = send_raw_telegram( CmdTelegram, InitialState ),

	wait_initial_base_request( _ToSkipLen=0, _MaybeAccChunk=undefined,
							   SentState ).



% @doc Returns an initialised, Oceanic state, once the initial base ID request
% has been properly answered.
%
-spec wait_initial_base_request( count(), telegram_chunk(),
								 oceanic_state() ) -> oceanic_state().
wait_initial_base_request( ToSkipLen, AccChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_tty,
		trace_utils:debug_fmt( "Waiting initial base request "
			"(ToSkipLen=~B, AccChunk=~w).", [ ToSkipLen, AccChunk ] ) ),

	receive

		% Received data from the serial port:
		{ data, NewChunk } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_utils:debug_fmt( "Read ~ts.",
					[ telegram_to_string( NewChunk ) ] ) ),

			case try_integrate_chunk( ToSkipLen, AccChunk, NewChunk, State ) of

				{ decoded, Event=#read_base_id_info_response{
						% (not interested here in remaining_write_cycles)
						base_eurid=BaseEurid }, MaybeAnyNextChunk,
						ReadState } ->

					% Clearer that way:
					case MaybeAnyNextChunk of

						undefined ->
							ok;

						<<>> ->
							ok;

						DroppedChunk ->
							trace_utils:warning_fmt( "Dropping initially "
								"chunk ~ts.",
								[ telegram_to_string( DroppedChunk ) ] )

					end,

					cond_utils:if_defined( oceanic_debug_tty,
						trace_utils:debug_fmt( "Successfully ~ts.",
							[ device_event_to_string( Event ) ] ),
						basic_utils:ignore_unused( Event ) ),

					ReadState#oceanic_state{ emitter_eurid=BaseEurid };


				{ Unsuccessful, NewToSkipLen, NewAccChunk, NewState } ->

					cond_utils:if_defined( oceanic_debug_tty,
						trace_utils:debug_fmt( "Unsuccessful decoding, '~w' "
							"(whereas NewToSkipLen=~B, NewAccChunk=~w).",
							[ Unsuccessful, NewToSkipLen, NewAccChunk ] ),
						basic_utils:ignore_unused( Unsuccessful ) ),

					wait_initial_base_request( NewToSkipLen, NewAccChunk,
											   NewState )

			end

	end.




% @doc Loads Oceanic configuration information from the default Ceylan
% preferences file, if any, otherwise returns a state with an empty device
% table.
%
% See the 'preferences' module.
%
-spec load_configuration( oceanic_state() ) -> oceanic_state().
load_configuration( State ) ->
	case preferences:is_preferences_default_file_available() of

		{ true, PrefPath } ->

			LoadedState = load_configuration( PrefPath, State ),

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Initial state: ~ts",
					[ state_to_string( LoadedState ) ] ) ),

			LoadedState;

		{ false, PrefPath } ->

			trace_bridge:info_fmt( "No preferences file ('~ts') found.",
								   [ PrefPath ] ),

			State

	end.



% @doc Loads Oceanic configuration information in the specified state from the
% specified ETF file, and returns a corresponding updated state.
%
% The configuration information is expected to contain up to one entry for the
% following keys (atoms):
%
% - oceanic_emitter: to specify the pseudo-device emitting any telegram to be
% sent by Oceanic (note that USB gateways have already their own base EURID that
% shall be preferred; refer to the co_rd_idbase common command)
%
% - oceanic_config: to declare the known devices
%
-spec load_configuration( any_file_path(), oceanic_state() ) -> oceanic_state().
load_configuration( ConfFilePath,
					State=#oceanic_state{ emitter_eurid=BaseEurid,
										  device_table=DeviceTable,
										  jamming_threshold=JamThreshold } ) ->

	file_utils:is_existing_file_or_link( ConfFilePath )
		orelse throw( { oceanic_config_file_not_found, ConfFilePath } ),

	Pairs = file_utils:read_etf_file( ConfFilePath ),

	EmitterEurid = case list_table:lookup_entry( _K=oceanic_emitter,
												 _Table=Pairs ) of

		key_not_found ->
			BaseEurid;

		{ value, EmitterEuridStr } ->
			text_utils:hexastring_to_integer( EmitterEuridStr )

	end,

	DeviceEntries = list_table:get_value_with_default( oceanic_devices,
													   _DefDevs=[], Pairs ),

	% Device table indexed by device eurid(), which is duplicated in the record
	% values for convenience:
	%
	LoadedDeviceTable = declare_devices( DeviceEntries, DeviceTable ),

	CfgJamThreshold = case list_table:lookup_entry( oceanic_jamming_threshold,
													Pairs ) of

		key_not_found ->
			JamThreshold;

		{ value, UserJamThreshold } ->
			UserJamThreshold

	end,

	State#oceanic_state{ emitter_eurid=EmitterEurid,
						 device_table=LoadedDeviceTable,
						 jamming_threshold=CfgJamThreshold }.



% @doc Adds the specified devices in the specified device table.
-spec declare_devices( [ device_config() ], device_table() ) -> device_table().
declare_devices( _DeviceCfgs=[], DeviceTable ) ->
	DeviceTable;

declare_devices( _DeviceCfgs=[ { NameStr, EuridStr, EepStr } | T ],
				 DeviceTable ) ->

	Eurid = try text_utils:hexastring_to_integer(
			text_utils:ensure_string( EuridStr ), _ExpectPrefix=false ) of

				Int ->
					Int

		% Typically error:badarg:
		catch _:E ->
			trace_bridge:error_fmt( "Invalid EURID ('~ts') "
				"for device named '~ts'.", [ EuridStr, NameStr ] ),
			throw( { invalid_eurid, EuridStr, E, NameStr } )

	end,

	EepBinStr = text_utils:ensure_binary( EepStr ),

	EepId = case oceanic_generated:get_maybe_first_for_eep_strings(
						EepBinStr ) of

		undefined ->
			trace_bridge:warning_fmt( "The EEP of the configured "
				"device '~ts' (EURID: ~ts), i.e. '~ts', is not known "
				"of Oceanic and will be ignored.",
				[ NameStr, EuridStr, EepBinStr ] ),
			undefined;

		KnownEepId ->
			KnownEepId

	end,

	DeviceRec = #enocean_device{ eurid=Eurid,
								 name=text_utils:ensure_binary( NameStr ),
								 eep=EepId,
								 discovered_through=configuration },

	NewDeviceTable = table:add_new_entry( Eurid, DeviceRec, DeviceTable ),

	declare_devices( T, NewDeviceTable );

% Dropping comment (useful only for the user configuration):
declare_devices( _DeviceCfgs=[ { NameStr, EuridStr, EepStr, CommentStr } | T ],
				 DeviceTable ) when is_list( CommentStr ) ->
	declare_devices( [ { NameStr, EuridStr, EepStr } | T ], DeviceTable );


declare_devices( _DeviceCfgs=[ Other | _T ], _DeviceTable ) ->
	throw( { invalid_device_config, Other } ).



% @doc Declares the specifed taught-in device.
-spec declare_device_from_teach_in( eurid(), eep(), device_table() ) ->
												{ device_table(), timestamp() }.
declare_device_from_teach_in( Eurid, Eep, DeviceTable ) ->

	MaybeEepId = resolve_eep( Eep ),

	Now = time_utils:get_timestamp(),

	NewDevice = case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device ~ts through "
				"teach-in.", [ eurid_to_string( Eurid ) ] ),

			#enocean_device{ eurid=Eurid,
							 name=undefined,
							 eep=MaybeEepId,
							 discovered_through=teaching,
							 first_seen=Now,
							 last_seen=Now,
							 telegram_count=1,
							 error_count=0 };

		% From then, already discovered:
		{ value, Device=#enocean_device{ eep=undefined,
										 telegram_count=TelCount } } ->
			Device#enocean_device{ eep=MaybeEepId,
								   last_seen=Now,
								   telegram_count=TelCount+1 };

		{ value, Device=#enocean_device{ eep=KnownEepId,
										 telegram_count=TelCount } } ->
			NewEepId = case KnownEepId of

				MaybeEepId ->
					MaybeEepId;

				_OtherEepId ->
					trace_bridge:error_fmt( "For ~ts, received a teach-in "
						"declaring that its EEP is ~ts, whereas it was known "
						"as ~ts; ignoring the new EEP.",
						[ device_to_string( Device ), MaybeEepId,
						  KnownEepId ] ),
					KnownEepId

			end,

			Device#enocean_device{ eep=NewEepId,
								   last_seen=Now,
								   telegram_count=TelCount+1 }

	end,

	{ table:add_entry( Eurid, NewDevice, DeviceTable ), NewDevice, Now }.



% @doc Sends the specified telegram, through the specified Oceanic server.
-spec send( telegram(), oceanic_server_pid() ) -> void().
send( Telegram, OcSrvPid ) ->
	OcSrvPid ! { sendOceanic, Telegram }.



% @doc Acknowledges (accepts) the specified teach request, by sending a
% (successful) teach response.
%
% See EEP Teach-(In/Out) Response - UTE Message (Broadcast / CMD: 0x1) [EEP-gen]
% p.26.
%
-spec acknowledge_teach_request( teach_request(), oceanic_server_pid() ) ->
													void().
acknowledge_teach_request( TeachReq=#teach_request{ request_type=teach_in },
						   OcSrvPid ) ->
	acknowledge_teach_request( TeachReq, _TeachOutcome=teach_in_accepted,
							   OcSrvPid );

acknowledge_teach_request( TeachReq=#teach_request{ request_type=teach_out },
						   OcSrvPid ) ->
	acknowledge_teach_request( TeachReq, _TeachOutcome=teach_out_accepted,
							   OcSrvPid ).



% @doc Acknowledges the specified teach-in request, by sending the specified
% teach-in response.
%
% See EEP Teach-In Response - UTE Message (Broadcast / CMD: 0x1) [EEP-gen] p.26.
%
-spec acknowledge_teach_request( teach_request(), teach_outcome(),
								 oceanic_server_pid() ) -> void().
acknowledge_teach_request( #teach_request{ source_eurid=RequesterEurid,
										   comm_direction=CommDirection,
										   echo_content=EchoContent },
						   TeachOutcome, OcSrvPid ) ->

	CommDir = case CommDirection of

		unidirectional ->
			0;

		bidirectional ->
			1

	end,

	ReqType = case TeachOutcome of

		teach_refused ->
			0;

		teach_in_accepted ->
			1;

		teach_out_accepted ->
			2;

		teach_eep_unsupported ->
			3

	end,

	CmdId = 1,

	FirstByte = <<CommDir:1, 0:1, ReqType:2, CmdId:4>>,

	% DB5.7 to DB0.0 has same structure as Teach-in-Query, and contents are
	% echoed back:
	%
	Data = <<FirstByte/binary, EchoContent/binary>>,

	OptData = get_optional_data_for_sending( RequesterEurid ),

	Telegram = encode_esp3_packet( _RadioPacketType=rorg_vld, Data, OptData ),

	send( Telegram, OcSrvPid ).



% Section for encoding, in ESP3 packets.


% Subsection for the encoding of the packets of the ERP1 radio type.


% Finer section for the RPS telegrams, which include only the F6-* EEPs.


% @doc Encodes a double-rocker switch telegram, from the specified device to the
% specified one (if any), reporting the specified transition for the specified
% button.
%
% As this encoding is done exclusively on the caller side (the Oceanic server
% not being involved), it is up to the caller to specify the source EURID. We
% recommend at the caller fetches from the Oceanic server its EURID (see
% get_oceanic_eurid/1) once for all, and use it afterwards.
%
% Event sent in the context of EEP F6-02-01 ("Light and Blind Control -
% Application Style 1"), for T21=1. It results thus in a RPS telegram, an ERP1
% radio packet encapsulated into an ESP3 one.
%
% See [EEP-spec] p.15 and its decode_rps_double_rocker_packet/7 counterpart.
%
% Depending on how Oceanic was learnt by the target actuator, it will be seen
% either as a rocker (recommended) or as push-button(s).
%
-spec encode_double_rocker_switch_telegram( eurid(), maybe( eurid() ),
		button_designator(), button_transition() ) -> telegram().
encode_double_rocker_switch_telegram( SourceEurid, MaybeTargetEurid,
									  ButtonDesignator, ButtonTransition ) ->

	% No EEP to be determined from double_rocker_switch (implicit in packet).

	% Best understood backwards, from the end of this function.

	RadioPacketType = radio_erp1_type,

	% F6 here:
	RorgNum = oceanic_generated:get_second_for_rorg( _Rorg=rorg_rps ),
	RorgNum = 16#f6,

	R1Enum = get_designated_button_enum( ButtonDesignator ),

	EB = get_button_transition_enum( ButtonTransition ),

	IsSecondActionValid = false,

	% Semantics R2Enum/SA unclear; we thought R2 was meaningless if second
	% action was invalid, but visibly it matters:

	%R2Enum = R1Enum,

	% Test:
	R2Enum = get_designated_button_enum( button_ai ),

	SA = case IsSecondActionValid of

		true ->
			1;

		false ->
			0

	end,

	% No LRN (Learn) bit for RPS, which can only send data and has no special
	% telegram modification to teach-in the device. Therefore, the teach-in
	% procedure takes place manually on the actuator/controller through a normal
	% data telegram. The EEP profile must be manually supplied to the controller
	% per sender ID.
	%
	DB_0 = <<R1Enum:3, EB:1, R2Enum:3, SA:1>>,

	T21 = 1,
	NU = 1,

	% Apparently Repeater Count (see [EEP-gen] p.14); non-zero deemed safer, yet
	% zero already works, so:

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

	% Radio, hence 1 here:
	encode_esp3_packet( RadioPacketType, Data, MaybeOptData ).



% @doc Encodes a double-rocker multipress telegram, from the specified device to
% the specified one (if any), reporting the specified transition for the
% specified button.
%
% Event sent in the context of EEP F6-02-01 ("Light and Blind Control -
% Application Style 1"), for T21=1. It results thus in a RPS telegram, an ERP1
% radio packet encapsulated into an ESP3 one.
%
% See [EEP-spec] p.15 and its decode_rps_double_rocker_packet/7 counterpart.
%
-spec encode_double_rocker_multipress_telegram( eurid(), maybe( eurid() ),
		button_counting(), button_transition() ) -> telegram().
encode_double_rocker_multipress_telegram( SourceEurid, MaybeTargetEurid,
										  ButtonCounting, ButtonTransition ) ->

	% No EEP to be determined from double_rocker_multipress (implicit in
	% packet).

	% Best understood backwards, from the end of this function.

	RadioPacketType = radio_erp1_type,

	% F6 here:
	RorgNum = oceanic_generated:get_second_for_rorg( _Rorg=rorg_rps ),
	RorgNum = 16#f6,

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




% @doc Returns the optional data suitable for sending to the specified device
% (if any).
%
-spec get_optional_data_for_sending( maybe( eurid() ) ) -> telegram_opt_data().
get_optional_data_for_sending( _MaybeTargetEurid=undefined ) ->
	<<>>;

get_optional_data_for_sending( TargetEurid ) ->

	% We are sending here:
	%SubTelNum = 3,
	SubTelNum = 1,
	DBm = 16#ff,
	SecurityLevel = 0,

	_OptData= <<SubTelNum:8, TargetEurid:32, DBm:8, SecurityLevel:8>>.




% @doc Executes synchronously the specified command, specified as an opaque,
% already-encoded telegram, that is supposed to be acknowledged next by the
% recipient device, with a response telegram.
%
-spec execute_command( telegram(), oceanic_server_pid() ) -> command_outcome().
execute_command( CmdTelegram, OcSrvPid ) ->

	RequesterPid = self(),

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Sending a command to execute, as ~ts, "
			"on behalf of requester ~w.",
			[ telegram_to_string( CmdTelegram ), RequesterPid ] ) ),

	OcSrvPid ! { executeCommand, CmdTelegram, RequesterPid },

	receive

		{ oceanic_command_outcome, Outcome } ->
			Outcome

		% To debug:
		%Other ->
		%   trace_utils:warning_fmt( "Received for command: ~p", [ Other ] )

	end.



% @doc Returns the current (emitter) EURID used by Oceanic for the local USB
% gateway, notably as default source base identifier when generating telegrams.
%
% Useful when encoding telegrams.
%
-spec get_oceanic_eurid( oceanic_server_pid() ) -> eurid().
get_oceanic_eurid( OcSrvPid ) ->
	OcSrvPid ! { getOceanicEurid, self() },

	receive

		{ oceanic_eurid, BaseEurid } ->
			BaseEurid

	end.


% Section for common commands.


% @doc Returns the version information held by the USB gateway, thanks to a
% (local) common command.
%
-spec read_version( oceanic_server_pid() ) ->
						read_version_response() | common_command_failure().
read_version( OcSrvPid ) ->
	send_common_command( _Cmd=co_rd_version, OcSrvPid ).



% @doc Returns the log information held by the USB gateway, thanks to a
% (local) common command.
%
-spec read_logs( oceanic_server_pid() ) ->
						read_logs_response() | common_command_failure().
read_logs( OcSrvPid ) ->
	send_common_command( _Cmd=co_rd_sys_log, OcSrvPid ).


% @doc Returns the information held by the USB gateway about its base ID, thanks
% to a (local) common command.
%
-spec read_base_id_info( oceanic_server_pid() ) ->
						read_base_id_info_response() | common_command_failure().
read_base_id_info( OcSrvPid ) ->
	send_common_command( _Cmd=co_rd_idbase, OcSrvPid ).



% @doc Sends the specified common command and returns its outcome.
-spec send_common_command( common_command(), oceanic_server_pid() ) ->
			common_command_response() | common_command_failure().
send_common_command( CommonCmd, OcSrvPid ) ->

	OcSrvPid ! { executeCommonCommand, CommonCmd, self() },
	receive

		{ oceanic_command_outcome, Outcome } ->
			Outcome

		% To debug:
		%Other ->
		%   trace_utils:warning_fmt( "Received for common command '~ts': ~p",
		%                            [ CommonCmd, Other ] )

	end.



% @doc Encodes the specified common command request, to be executed by the USB
% gateway.
%
-spec encode_common_command_request( common_command() ) -> telegram().
% Future commands may have to be special-cased (e.g. if having parameters):
encode_common_command_request( _Cmd=co_rd_version ) ->
	encode_read_version_request();

encode_common_command_request( _Cmd=co_rd_sys_log ) ->
	encode_read_logs_request();

encode_common_command_request( _Cmd=co_rd_idbase ) ->
	encode_base_id_info_request().



% @doc Encodes a common command request of type 'CO_RD_VERSION', to read version
% information from the USB gateway.
%
% See its actual specification in [ESP3], p.36, and the decode_response_tail/5
% for WaitedCmd=co_rd_version.
%
-spec encode_read_version_request() -> telegram().
encode_read_version_request() ->
	CmdNum = oceanic_generated:get_first_for_common_command( co_rd_version ),
	Data = <<CmdNum:8>>,
	encode_common_command( Data ).


% @doc Encodes a common command request of type 'CO_RD_SYS_LOG', to read logs
% from the USB gateway.
%
% See its actual specification in [ESP3], p.37, and the decode_response_tail/5
% for WaitedCmd=co_rd_sys_log.
%
-spec encode_read_logs_request() -> telegram().
encode_read_logs_request() ->
	CmdNum = oceanic_generated:get_first_for_common_command( co_rd_sys_log ),
	Data = <<CmdNum:8>>,
	encode_common_command( Data ).



% @doc Encodes a common command request of type 'CO_RD_IDBASE', to read base ID
% information from the USB gateway.
%
% See its actual specification in [ESP3], p.40, and the decode_response_tail/5
% for WaitedCmd=co_rd_idbase.
%
-spec encode_base_id_info_request() -> telegram().
encode_base_id_info_request() ->
	CmdNum = oceanic_generated:get_first_for_common_command( co_rd_idbase ),
	Data = <<CmdNum:8>>,
	encode_common_command( Data ).




% @doc Encodes a common command request, based on the specified data (and with
% no optional data defined).
%
% The actual specification of common commands starts at p.32 of [ESP3].
%
-spec encode_common_command( telegram_data() ) -> telegram().
encode_common_command( Data ) ->
	encode_esp3_packet( _PacketType=common_command_type, Data ).


% @doc Encodes a common command, based on the specified data and optional data.
%
% The actual specification of common commands starts at p.32 of [ESP3].
%
-spec encode_common_command( telegram_data(), telegram_opt_data() ) ->
												telegram().
encode_common_command( Data, OptData ) ->
	encode_esp3_packet( _PacketType=common_command, Data, OptData ).



% @doc Encodes an ESP3 packet from its packet type and base data.
-spec encode_esp3_packet( packet_type(), telegram_data() ) -> telegram().
encode_esp3_packet( PacketType, Data ) ->
	encode_esp3_packet( PacketType, Data, _OptData= <<>> ).



% @doc Encodes an ESP3 packet from its packet type, base and optional data.
-spec encode_esp3_packet( packet_type(), telegram_data(),
						  telegram_opt_data() ) -> telegram().
encode_esp3_packet( PacketType, Data, OptData ) ->

	DataLen = size( Data ),

	OptDataLen = size( OptData ),

	% For example PacketType is radio_erp1_type:
	PacketTypeNum = oceanic_generated:get_second_for_packet_type( PacketType ),

	ESP3Header = <<DataLen:16, OptDataLen:8, PacketTypeNum:8>>,

	ESP3HeaderCRC = compute_crc( ESP3Header ),

	FullData = <<Data/binary, OptData/binary>>,
	FullDataCRC = compute_crc( FullData ),

	% This is an ESP3 packet:
	<<?sync_byte, ESP3Header/binary, ESP3HeaderCRC:8, FullData/binary,
	  FullDataCRC:8>>.



% Subsection for the encoding of packets of the Common Command type.


% Finer section for the RPS telegrams, which include only the F6-* EEPs.




% @doc Returns, if possible, the specified telegram once decoded as an event,
% using the specified Oceanic server for that.
%
% Mostly useful for testing purpose.
%
-spec decode_telegram( telegram(), oceanic_server_pid() ) -> decoding_result().
decode_telegram( Telegram, OcSrvPid ) ->
	OcSrvPid ! { decodeOceanic, Telegram, self() },
	receive

		{ decoding_result, R } ->
			R

	end.



% Main loop of the Oceanic server.
%
% There may be:
% - remaining content from past, unsupported packet types that is still to be
% skipped (hence ToSkipLen; finer than only searching for start bytes)
% - any already-received beginning of the current telegram to be taken into
% account (hence MaybeAccChunk - which never includes the starting byte); we
% have to discriminate the value of MaybeAccChunk between "nothing already read"
% (then it is equal to undefined) and "only the start byte was read (and
% chopped)" (then it is equal to <<>>)
%
-spec oceanic_loop( count(), maybe( telegram_chunk() ), oceanic_state() ) ->
											no_return().
oceanic_loop( ToSkipLen, MaybeAccChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		begin
			SkipStr = case ToSkipLen of

				0 ->
					"no byte";

				1 ->
					"a single byte";

				_ ->
					text_utils:format( "~B bytes", [ ToSkipLen ] )

			end,

			ChunkStr = case MaybeAccChunk of

				undefined ->
					"no chunk";

				<<>> ->
					"just the start byte";

				AccChunk ->
					 text_utils:format( "a chunk of ~B bytes (~p) after "
						"the start byte", [ size( AccChunk ), AccChunk ] )

			end,

			trace_bridge:debug_fmt( "Waiting for any message "
				"including a telegram chunk, whereas having ~ts to skip, "
				"and having accumulated ~ts.", [ SkipStr, ChunkStr ] )

		end ),


	receive

		% Received data from the serial port:
		{ data, NewChunk } ->

			NewChunkSize = size( NewChunk ),

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Received a telegram chunk "
					"of ~B bytes: ~w, corresponding to hexadecimal ~ts "
					"(whereas there are ~B bytes to skip).",
					[ NewChunkSize, NewChunk,
					  telegram_to_hexastring( NewChunk ), ToSkipLen ] ) ),

			JamState = monitor_jamming( NewChunkSize, State ),

			case try_integrate_chunk( ToSkipLen, MaybeAccChunk, NewChunk,
									  JamState ) of

				% Commands have been already answered directly, no response
				% echoed to any listener:
				%
				{ decoded, _Event=command_processed, AnyNextMaybeChunk,
				  NewState } ->
					oceanic_loop( _SkipLen=0, AnyNextMaybeChunk, NewState );

				% Then just an event, possibly listened to:
				{ decoded, Event, AnyNextMaybeChunk, NewState } ->

					cond_utils:if_defined( oceanic_debug_decoding,
						trace_bridge:debug_fmt( "Decoded following event: ~ts.",
							[ device_event_to_string( Event ) ] ) ),

					case NewState#oceanic_state.event_listener_pid of

						undefined ->
							ok;

						ListenerPid ->
							ListenerPid ! { onEnoceanEvent, [ Event, self() ] }

					end,

					oceanic_loop( _SkipLen=0, AnyNextMaybeChunk, NewState );


				{ _Unsuccessful, NewToSkipLen, NewMaybeAccChunk, NewState } ->
					% when Unsuccessful     =:= not_reached
					%   orelse Unsuccessful =:= incomplete
					%   orelse Unsuccessful =:= invalid
					%   orelse Unsuccessful =:= unsupported ->
					%   orelse Unsuccessful =:= unconfigured ->
					oceanic_loop( NewToSkipLen, NewMaybeAccChunk, NewState )

			end;


		{ executeCommand, CmdTelegram, RequesterPid } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Requested to execute command as ~ts, "
					"on behalf of requester ~w.",
					[ telegram_to_string( CmdTelegram ), RequesterPid ] ) ),

			ExecState = execute_command_helper( _CmdType=device_command,
				CmdTelegram, RequesterPid, State ),

			oceanic_loop( ToSkipLen, MaybeAccChunk, ExecState );


		{ executeCommonCommand, CommonCommand, RequesterPid } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt(
					"Requested to execute common command '~ts', "
					"on behalf of requester ~w.",
					[ CommonCommand, RequesterPid ] ) ),

			CmdTelegram = encode_common_command_request( CommonCommand ),

			% Response will be automatically sent back to the requester when
			% decoding it (refer to decode_response_tail/5).

			ExecState = execute_command_helper( _CmdType=CommonCommand,
				CmdTelegram, RequesterPid, State ),

			oceanic_loop( ToSkipLen, MaybeAccChunk, ExecState );


		{ getOceanicEurid, RequesterPid } ->
			RequesterPid !
				{ oceanic_eurid, State#oceanic_state.emitter_eurid },

			oceanic_loop( ToSkipLen, MaybeAccChunk, State );


		% Sent by the timer associated to the currently pending command, a timer
		% that just expired:
		%
		{ considerCommandTimeout, CmdCount } ->

			TimeState = case State#oceanic_state.waited_command_info of

				% Surprising:
				undefined ->
					trace_bridge:warning_fmt( "Received a command time-out "
						"(for command count: ~B), whereas no command is "
						"awaited and the current count is ~B; ignoring it.",
						[ CmdCount, State#oceanic_state.command_count ] ),
					State;


				_CmdInfo={ CmdReq=#command_request{
									requester=RequesterPid },
						   _ThisTimerRef } ->

					cond_utils:if_defined( oceanic_check_commands,
						CmdCount = State#oceanic_state.command_count ),

					cond_utils:if_defined( oceanic_debug_tty,
						trace_bridge:debug_fmt( "Sending to requester "
							"a time-out regarding command ~ts.",
							[ command_request_to_string( CmdReq ) ] ),
						basic_utils:ignore_unused( CmdReq ) ),

					RequesterPid !
						{ oceanic_command_outcome, _Outcome=time_out },

					State#oceanic_state{ waited_command_info=undefined }

			end,

			% May unblock a queued command:
			NewState = handle_next_command(
				TimeState#oceanic_state.command_queue, TimeState ),

			oceanic_loop( ToSkipLen, MaybeAccChunk, NewState );


		% Mostly useful for testing purpose:
		{ sendOceanic, Telegram } ->
			NewState = send_raw_telegram( Telegram, State ),
			oceanic_loop( ToSkipLen, MaybeAccChunk, NewState );


		% Mostly useful for testing purpose:
		{ decodeOceanic, Telegram, SenderPid } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Requested to decode telegram ~ts.",
					[ telegram_to_string( Telegram ) ] ) ),

			% Not interfering with received bits (current state used for that,
			% but will not be affected):
			%
			Res = case try_integrate_chunk( _ToSkipLen=0,
								_MaybeAccChunk=undefined, Telegram, State ) of

				{ decoded, DeviceEvent, _NewNextChunk, _NewState } ->
					DeviceEvent;

				{ DecError, _NewToSkipLen, _NewNextChunk, _NewState } ->
					DecError

			end,

			SenderPid ! { decoding_result, Res },

			% Strictly unaffected:
			oceanic_loop( ToSkipLen, MaybeAccChunk, State );


		% Asynchronous:
		terminate ->

			% (any pending chunk discarded)

			SerialPid = State#oceanic_state.serial_server_pid,

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Stopping serial server ~w, "
					"while in following state: ~ts.",
					[ SerialPid, state_to_string( State ) ] ) ),

			SerialPid ! stop,

			trace_bridge:debug_fmt( "Oceanic server ~w terminated.",
									[ self() ] );


		{ terminateSynchronously, SenderPid } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Requested by ~w to "
					"terminate synchronously.", [ SenderPid ] ) ),

			% (any pending chunk discarded)

			SerialPid = State#oceanic_state.serial_server_pid,

			SerialPid ! { stop, self() },

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Stopping serial server ~w, "
					"while in following state: ~ts.",
					[ SerialPid, state_to_string( State ) ] ) ),

			receive

				serial_stopped ->
					ok

			end,

			SenderPid ! oceanic_terminated,

			trace_bridge:debug_fmt(
				"Oceanic server ~w terminated synchronously.", [ self() ] );


		UnexpectedMsg ->
			trace_bridge:debug_fmt( "Oceanic server ~w received an unexpected "
				"message: '~w', ignoring it.", [ self(), UnexpectedMsg ] ),

			oceanic_loop( ToSkipLen, MaybeAccChunk, State )

	end.



% @doc Detects and notifies any suspected jamming attempt.
-spec monitor_jamming( byte_size(), oceanic_state() ) -> oceanic_state().
monitor_jamming( ChunkSize,
				 State=#oceanic_state{ traffic_level=TrafficLvl,
									   last_traffic_seen=LastTimestamp,
									   jamming_threshold=JamThreshold,
									   event_listener_pid=MaybeListnPid } ) ->

	Now = time_utils:get_timestamp(),

	Dur = time_utils:get_duration( LastTimestamp, Now ),

	% Relatively exponential backoff, as same second: not reduced; halved if
	% previous second, etc.:
	%
	AggTrafficLvl = round( TrafficLvl / (Dur+1) ) + ChunkSize,

	NewTrafficLvl = case AggTrafficLvl > JamThreshold of

		true ->
			% We notify (if possible) and reset the monitored level:
			trace_bridge:alert_fmt( "The jamming detection threshold "
				"(~B bytes per second) has been reached (with ~B bytes per "
				"second); an attempt to saturate actuators may be "
				"in progress.",
				[ JamThreshold, AggTrafficLvl ] ),

			MaybeListnPid =:= undefined orelse
				% PID sent mostly to discriminate between multiple Oceanic
				% servers:
				%
				MaybeListnPid ! { onEnoceanJamming, [ AggTrafficLvl, self() ] },

			% Single notification per detection:
			0;

		false ->
			AggTrafficLvl

	end,

	State#oceanic_state{ traffic_level=NewTrafficLvl,
						 last_traffic_seen=Now }.



% @doc Requests to execute the specified telegram-based command, by queueing it.
-spec execute_command_helper( command_type(), telegram(), requester(),
							  oceanic_state() ) -> oceanic_state().
execute_command_helper( CmdType, CmdTelegram, RequesterPid,
						State=#oceanic_state{ command_queue=CmdQueue } ) ->

	CmdReq = #command_request{ command_type=CmdType,
							   command_telegram=CmdTelegram,
							   requester=RequesterPid },

	ExpandedQueue = queue:in( CmdReq, CmdQueue ),

	handle_next_command( ExpandedQueue, State ).



% @doc Handles, if appropriate, the sending of the next command, using the
% specified queue for that.
%
-spec handle_next_command( command_queue(), oceanic_state() ) ->
										oceanic_state().

% Here no command is waited, so we can send one directly - provided there is
% any.
%
handle_next_command( CurrentQueue, State=#oceanic_state{
										waited_command_info=undefined } ) ->

	case queue:out( CurrentQueue ) of

		{ { value, OldestCmdReq=#command_request{
									command_telegram=CmdTelegram } },
				ShrunkQueue } ->

			% Not a record mistake:
			SentState = #oceanic_state{ wait_timeout=MaxWaitMs,
										command_count=CmdCount } =
				send_raw_telegram( CmdTelegram, State ),

			NewCmdCount = CmdCount+1,

			% {error, Reason} hardly manageable here; we cannot specify a timer
			% reference in its own message (as of course the reference does not
			% exist yet); we could have created and stored a reference instead,
			% but a counter has its own interest:
			%
			{ ok, TimerRef } = timer:send_after(
				MaxWaitMs,
				%_Msg={ considerCommandTimeout, TimerRef } ),
				_Msg={ considerCommandTimeout, NewCmdCount } ),

			CmdInfo = { OldestCmdReq, TimerRef },

			SentState#oceanic_state{ command_queue=ShrunkQueue,
									 waited_command_info=CmdInfo,
									 command_count=NewCmdCount };

		% Nothing to send here:
		{ empty, SameCurrentQueue } ->
			State#oceanic_state{ command_queue=SameCurrentQueue }

	end;


% Here there is already a waited command, we just update the queue:
handle_next_command( CurrentQueue, State ) ->
	State#oceanic_state{ command_queue=CurrentQueue }.



% @doc Sends from the Oceanic server the specified telegram.
-spec send_raw_telegram( telegram(), oceanic_state() ) -> oceanic_state().
send_raw_telegram( Telegram, State=#oceanic_state{ serial_server_pid=SerialPid,
												   sent_count=SentCount } ) ->

	% Not useful: ActualSending = binary_to_list( Telegram ),
	ActualSending = Telegram,

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt(
			"Sending to serial server ~w actual telegram ~w "
			"(hexadecimal form: '~ts').",
			[ SerialPid, ActualSending,
			  telegram_to_hexastring( Telegram ) ] ) ),

	SerialPid ! { send, ActualSending },

	State#oceanic_state{ sent_count=SentCount+1 }.



% @doc Helper introduced only to make the decoding logic available for tests.
-spec test_decode( telegram_chunk() ) -> decoding_outcome().
test_decode( Chunk ) ->
	try_integrate_chunk( _ToSkipLen=0, _MaybeAccChunk=undefined, Chunk,
						 get_test_state() ).



% @doc Returns a pseudo-state, loaded from default configuration; only useful
% for some tests.
%
% Note that, in order that an encoding or decoding (non-sending, non-receiving)
% test can work without any actual device, we create here a type-correct yet
% incorrect state (no real base ID, no relevant serial server).
%
% So thise resulting state, which does not need an actual USB gateway to be
% available, is mostly bogus.
%
-spec get_test_state() -> oceanic_state().
get_test_state() ->
	TestState = #oceanic_state{ serial_server_pid=self() },
	load_configuration( TestState ).



% @doc Returns a pseudo-state, based on the specified device table; only useful
% for some tests.
%
-spec get_test_state( device_table() ) -> oceanic_state().
get_test_state( DeviceTable ) ->

	% Normally there is a real serial server:
	BaseState = get_test_state(),

	BaseState#oceanic_state{ device_table=DeviceTable }.



% @doc Returns the device state in the specified state; only useful for some
% tests.
%
-spec get_device_table( oceanic_state() ) -> device_table().
get_device_table( #oceanic_state{ device_table=DeviceTable } ) ->
	DeviceTable.



% @doc Tries to integrate a new telegram chunk, that is to decode an ESP3 packet
% from the specified chunk.
%
-spec try_integrate_chunk( count(), maybe( telegram_chunk() ), telegram_chunk(),
						   oceanic_state() ) -> decoding_outcome().

try_integrate_chunk( ToSkipLen, _MaybeAccChunk=undefined, NewChunk, State ) ->

	ChunkSize = size( NewChunk ),

	case ToSkipLen - ChunkSize of

		% Not having reached a new packet yet:
		StillToSkip when StillToSkip >= 0 ->
			{ not_reached, StillToSkip, _NoAccChunk=undefined, State };

		% ChunkSize > ToSkipLen, so next packet already started in this
		% new chunk.
		%
		% This will start by scanning for any start byte:
		_ ->
			<<_Skipped:ToSkipLen/binary, TargetChunk/binary>> = NewChunk,
			try_decode_chunk( TargetChunk, State )

	end;

% Special-casing "no skip" is clearer; guard needed to ensure we indeed already
% chopped a start byte:
%
% May happen (at least initially):
try_integrate_chunk( _ToSkipLen=0, _MaybeAccChunk=undefined, NewChunk,
					 State ) ->
	scan_past_start( NewChunk, State );


try_integrate_chunk( _ToSkipLen=0, AccChunk, NewChunk, State ) ->
	% Start byte was already chopped from AccChunk:
	scan_past_start( <<AccChunk/binary, NewChunk/binary>>, State ).





% @doc Tries to decode the specified telegram chunk (any needed skipping having
% already taken place), and returns the outcome.
%
% Incomplete chunks may be completed later, by next receivings (hence are kept,
% from their first start byte included), whereas invalid ones are dropped (until
% any start byte found).
%
-spec try_decode_chunk( telegram_chunk(), oceanic_state() ) ->
								decoding_outcome().
try_decode_chunk( TelegramChunk, State ) ->

	% (an additional source of inspiration can be [PY-EN], in
	% enocean/protocol/packet.py, the parse_msg/1 method)

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Trying to decode '~w' (of size ~B bytes)",
								[ TelegramChunk, size( TelegramChunk ) ] ) ),

	% First 6 bytes correspond to the serial synchronisation:
	% - byte #1: Packet start (?sync_byte, 0x55)
	% - bytes #2-5 (32 bits): Header, containing:
	%   * byte #2-3 (16 bits): byte count of DATA to interpret
	%   * byte #4: (8 bits) byte count of OPTIONAL_DATA to interpret
	%   * byte #5: (8 bits) packet type
	% - byte #6: CRC of header

	case scan_for_packet_start( TelegramChunk ) of

		{ no_content, DroppedCount } ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "(no start byte found in whole chunk, "
					"so dropping its ~B bytes)", [ DroppedCount ] ),
				basic_utils:ignore_unused( DroppedCount ) ),

			{ invalid, _StillToSkipLen=0, _NoAccChunk=undefined, State };


		{ NewTelegramChunk, DroppedCount } ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Start byte found, retaining now "
					"following chunk (of size ~B bytes; "
					"after dropping ~B byte(s)):~n  ~p.",
					[ size( NewTelegramChunk ), DroppedCount,
					  NewTelegramChunk ] ),
				basic_utils:ignore_unused( DroppedCount ) ),

			scan_past_start( NewTelegramChunk, State )

	end.



% @doc Scans the specified chunk, knowing that it used to begin with a start
% byte (which has already been chopped).
%
scan_past_start( NewTelegramChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Examining now following chunk of ~B bytes: "
			"~p.", [ size( NewTelegramChunk ), NewTelegramChunk  ] ) ),

	% This new chunk corresponds to a telegram that is invalid, or unsupported,
	% or (currently) truncated, or valid (hence decoded):
	%
	case NewTelegramChunk of

		% First 32 bits:
		<<Header:4/binary, HeaderCRC, Rest/binary>> ->
			examine_header( Header, HeaderCRC, Rest, NewTelegramChunk, State );

		% So less than 5 bytes (yet), cannot be complete, but is kept:
		_ ->
			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug( "(no complete header to decode)" ) ),

			% Waiting to concatenate any additional receiving:
			{ incomplete, _ToSkipLen=0, NewTelegramChunk, State }

	end.




% @doc Extracts the content from the specified telegram chunk, returning a chunk
% that is beginning just after any start byte (which is 0x55, i.e. 85), if any.
%
% Generally there are no leading bytes to be dropped.
%
% Refer to [ESP3] "1.6 UART synchronization (start of packet detection)".
%
-spec scan_for_packet_start( telegram_chunk() ) ->
				{ telegram_chunk() | 'no_content', DropCount :: count() }.
scan_for_packet_start( TelegramChunk ) ->
	scan_for_packet_start( TelegramChunk, _DropCount=0 ).


% (helper)
scan_for_packet_start( _Chunk= <<>>, DropCount ) ->
	{ no_content, DropCount };

scan_for_packet_start( _Chunk= <<?sync_byte, T/binary>>, DropCount ) ->
	% No need to keep/include the start byte: repeated decoding attempts may
	% have to be made, yet any acc'ed chunk is a post-start telegram chunk:
	%
	{ T, DropCount };

% Skip all bytes before first start byte:
scan_for_packet_start( _Chunk= <<_OtherByte, T/binary>>, DropCount ) ->
	scan_for_packet_start( T, DropCount+1 ).



% @doc Checks the telegram header and decodes it.
-spec examine_header( esp3_header(), crc(), telegram_chunk(), telegram_chunk(),
					  oceanic_state() ) -> decoding_outcome().
examine_header( Header= <<DataLen:16, OptDataLen:8, PacketTypeNum:8>>,
				HeaderCRC, Rest, FullTelegramChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Packet type ~B; expecting ~B bytes of data, "
			"then ~B of optional data; checking first header CRC.",
			[ PacketTypeNum, DataLen, OptDataLen ] ) ),

	case compute_crc( Header ) of

		HeaderCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Header CRC validated (~B).",
										[ HeaderCRC ] ) ),

			% +1 for its CRC size:
			ExpectedRestSize = DataLen + OptDataLen + 1,

			case get_packet_type( PacketTypeNum ) of

				undefined ->

					SkipLen = ExpectedRestSize,

					trace_bridge:warning_fmt( "Unknown packet type (~B), "
						"dropping corresponding content "
						"(hence ~B bytes to be skipped).",
						[ PacketTypeNum, SkipLen ] ),

					% Not wanting to drop also the content of any next telegram:
					case Rest of

						<<_PacketContent:SkipLen/binary, NextChunk/binary>> ->
							% We already skipped what was needed:
							{ unsupported, _SkipLen=0, NextChunk, State };

						% Rest too short here; so we have to skip more than this
						% chunk:
						%
						_ ->
							StillToSkip = SkipLen - size( Rest ),
							{ not_reached, StillToSkip, _NoAccChunk=undefined,
							  State }

					end;


				PacketType ->

					cond_utils:if_defined( oceanic_debug_decoding,
						trace_bridge:debug_fmt( "Detected packet type: ~ts.",
												[ PacketType ] ) ),

					ActualRestSize = size( Rest ),

					case ActualRestSize < ExpectedRestSize of

						% Not having received enough yet (will be parsed again
						% once at least partially completed next):
						%
						true ->
							{ incomplete, _SkipLen=0, FullTelegramChunk,
							  State };

						% We have at least enough:
						false ->

							<<Data:DataLen/binary, OptData:OptDataLen/binary,
							  FullDataCRC:8, NextBin/binary>> = Rest,

							MaybeNextChunk = case NextBin of

								<<>> ->
									undefined;

								% We cannot include NextBin as a chunk, this
								% would mean it was starting with a start byte;
								% so:
								%
								NonEmptyBin ->
									case scan_for_packet_start( NonEmptyBin ) of

										{ no_content, _DropCount } ->
											undefined;

										{ ChoppedChunk, _DropCount } ->
											ChoppedChunk

									end

							end,

							% This CRC corresponds to the whole FullData, we
							% extract it (again) rather than concatenating
							% <<Data/binary, OptData/binary>>:

							FullLen = DataLen + OptDataLen,

							% May happen; e.g. if two telegrams overlap:
							case Rest of

								% Post-telegram not lost:
								<<FullData:FullLen/binary, _>> ->
									examine_full_data( FullData, FullDataCRC,
										Data, OptData, PacketType,
										FullTelegramChunk, MaybeNextChunk,
										State );

								TooShortChunk ->

									cond_utils:if_defined(
										oceanic_debug_decoding,
										trace_bridge:debug_fmt(
											"Chunk ~p too short.",
											[ TooShortChunk ] ),
										basic_utils:ignore_unused(
											TooShortChunk ) ),

									% By design start byte already chopped:
									{ incomplete, _ToSkipLen=0,
									  FullTelegramChunk, State }


							end

					end

			end;

		OtherHeaderCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Obtained other header CRC (~B), "
					"dropping this telegram candidate.", [ OtherHeaderCRC ] ),
				basic_utils:ignore_unused( OtherHeaderCRC ) ),

			% Rather than discarding this chunk as a whole, tries to scavage
			% (very conservatively) any trailing element by reintroducing a
			% potentially still valid new chunk - knowing that the past start
			% byte has already been chopped, so we go for any next one:
			%
			try_decode_chunk( FullTelegramChunk, State )

	end.



% @doc Further checks and decodes a telegram now that its type is known.
-spec examine_full_data( telegram_chunk(), crc(), telegram_data(),
	telegram_opt_data(), packet_type(), telegram_chunk(), telegram_chunk(),
	oceanic_state() ) -> decoding_outcome().
examine_full_data( FullData, ExpectedFullDataCRC, Data, OptData, PacketType,
				   FullTelegramChunk, AnyNextChunk, State ) ->

	case compute_crc( FullData ) of

		ExpectedFullDataCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Full-data CRC validated (~B).",
										[ ExpectedFullDataCRC ] ) ),

			decode_packet( PacketType, Data, OptData, AnyNextChunk, State );


		OtherCRC ->
			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Obtained unexpected full-data CRC "
					"(~B, instead of ~B), dropping candidate telegram.",
					[ OtherCRC, ExpectedFullDataCRC ] ),
				basic_utils:ignore_unused( OtherCRC ) ),

			% Not expecting being fooled by data accidentally looking like a
			% legit CRC'ed header, so supposing this is just a valid telegram
			% that ended up to be corrupted, yet for extra safety we will
			% restart the decoding at the very first step (we are nevertheless
			% still progressing - not wanting to recurse infinitely on a chunk):
			%
			scan_for_packet_start( FullTelegramChunk )

	end.



% @doc Decodes the specified packet, based on the specified data elements.
%
% Data corresponds to the actual packet payload of the specified type.
%
-spec decode_packet( packet_type(), telegram_data(), telegram_opt_data(),
					 telegram_chunk(), oceanic_state() ) -> decoding_outcome().
% Clause only for ERP1 packets (e.g. not covering responses):
decode_packet( _PacketType=radio_erp1_type,
			   _Data= <<RorgNum:8, DataTail/binary>>, OptData, AnyNextChunk,
			   State ) ->

	Rorg = oceanic_generated:get_first_for_rorg( RorgNum ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding an ERP1 radio packet of R-ORG ~ts, "
			"hence ~ts, i.e. '~ts'...",
			[ text_utils:integer_to_hexastring( RorgNum ), Rorg,
			  oceanic_generated:get_second_for_rorg_description( Rorg ) ] ) ),

	case Rorg of

		rorg_rps ->
			decode_rps_packet( DataTail, OptData, AnyNextChunk, State );

		rorg_1bs ->
			decode_1bs_packet( DataTail, OptData, AnyNextChunk, State );

		rorg_4bs ->
			decode_4bs_packet( DataTail, OptData, AnyNextChunk, State );

		rorg_ute ->
			decode_ute_packet( DataTail, OptData, AnyNextChunk, State );

		rorg_vld ->
			decode_vld_packet( DataTail, OptData, AnyNextChunk, State );

		_ ->
			trace_bridge:warning_fmt( "The decoding of ERP1 radio packets "
				"of R-ORG ~ts, hence ~ts (i.e. '~ts') is not implemented; "
				"the corresponding packet is thus dropped.",
				[ text_utils:integer_to_hexastring( RorgNum ), Rorg,
				  oceanic_generated:get_second_for_rorg_description( Rorg ) ] ),

			% Not even an EURID to track.

			{ unsupported, _ToSkipLen=0, AnyNextChunk, State }

	end;



% Here a response is received whereas no request was sent:
decode_packet( _PacketType=response_type, Data, OptData, AnyNextChunk,
			   State=#oceanic_state{ waited_command_info=undefined,
									 discarded_count=DiscCount } ) ->

	trace_bridge:error_fmt( "Received a command response "
		"(data: ~w, optional data: ~w) whereas there is no pending request, "
		"dropping it.", [ Data, OptData ] ),

	{ decoded, _MaybeDeviceEvent=command_processed, AnyNextChunk,
	  State#oceanic_state{ discarded_count=DiscCount+1 } };


% Response received, presumably for this pending (possibly internal) command:
decode_packet( _PacketType=response_type,
			   _Data= <<ReturnCode:8, DataTail/binary>>, OptData, AnyNextChunk,
			   State=#oceanic_state{
					waited_command_info={ WaitedCmdReq, MaybeTimerRef },
					command_count=CmdCount } ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a command response, whereas "
			"awaiting ~ts.", [ command_request_to_string( WaitedCmdReq ) ] ) ),

	% In all cases the pending request is over:

	case MaybeTimerRef of

		undefined ->
			ok;

		TimerRef ->
			{ ok, cancel } = timer:cancel( TimerRef )

	end,

	RespState = State#oceanic_state{ waited_command_info=undefined },

	case oceanic_generated:get_first_for_return_code( ReturnCode ) of

		undefined ->
			trace_bridge:warning_fmt( "Unable to decode response whose return "
				"code is invalid (~B), dropping packet and pending "
				"command (#~B).", [ ReturnCode, CmdCount ] ),
			{ invalid, _ToSkipLen=0, AnyNextChunk, RespState };


		ok_return ->
			decode_response_tail( WaitedCmdReq, DataTail, OptData,
								  AnyNextChunk, RespState );

		% Not a decoding failure, but more a protocol-level one that shall be
		% notified to the requester.
		%
		% Expected in [error_return, not_supported_return,
		%              wrong_parameter_return, operation_denied]:
		%
		FailureReturn ->

			trace_bridge:error_fmt( "Received a failure response (~ts), "
				"presumably to the pending ~ts.",
				[ FailureReturn, command_request_to_string( WaitedCmdReq ) ] ),

			Requester = WaitedCmdReq#command_request.requester,

			case Requester of

				internal ->
					{ invalid, _ToSkipLen=0, AnyNextChunk, RespState };

				RequesterPid ->

					RequesterPid !
						{ oceanic_command_outcome, _Outcome=FailureReturn },

					% Waiting information already cleared:
					{ decoded, command_processed, AnyNextChunk, RespState }

			end

	end;


decode_packet( PacketType, _Data, _OptData, AnyNextChunk, State ) ->

	trace_bridge:warning_fmt( "Unsupported packet type '~ts' (hence ignored).",
							  [ PacketType ] ),

	% Not even an EURID to track, no real need to go further.

	{ unsupported, _ToSkipLen=0, AnyNextChunk, State }.



% @doc Decodes a rorg_rps (F6, "Repeated Switch Communication") packet.
%
% If the RORG value (here "F6", RPS) is specified in the packet, FUNC and TYPES
% are not, hence the full, precise EEP of the packet cannot be determined from
% the telegram; extra device information must thus be available (typically
% specified out of band, in a configuration file) is order to decode it.
%
% Supported:
% - F6-01 corresponds to simple "Switch Buttons" (with no rocker, hence with
% punctual press/release events), described here as "push buttons"
%
% - F6-02: Rocker Switch, 2 Rocker: each rocker (A or B) has a top and a bottom
% button; pressing one sends a double_rocker_switch_event() telling that a given
% button (possibly both) is/are being pressed, and releasing it/them sends a
% double_rocker_multipress_event() telling "no button released simultaneously"
%
% Support to be added:
% - F6-03: Rocker Switch, 4 Rocker
% - F6-04: Position Switch, Home and Office Application
% - F6-05: Detectors
% - F6-10: Mechanical Handle
%
% Discussed a bit in [ESP3] "2.1 Packet Type 1: RADIO_ERP1", p.18, and in
% [EEP-spec] p.11.
%
% See decode_1bs_packet/3 for more information.
%
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
-spec decode_rps_packet( telegram_data_tail(), telegram_opt_data(),
		telegram_chunk(), oceanic_state() ) -> decoding_outcome().
decode_rps_packet( _DataTail= <<DB_0:1/binary, SenderEurid:32,
				   Status:1/binary>>, OptData, AnyNextChunk,
				   State=#oceanic_state{ device_table=DeviceTable } ) ->

	% We have to know the specific EEP of this device in order to decode this
	% telegram:
	%
	case table:lookup_entry( SenderEurid, DeviceTable ) of

		% Device first time seen:
		key_not_found ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _Now, _MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			trace_bridge:warning_fmt( "Unable to decode a RPS (F6) packet "
				"for ~ts: device not configured, no EEP known for it.",
				[ eurid_to_string( SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, AnyNextChunk, NewState };


		% Knowing the actual EEP is needed in order to decode:
		{ value, _Device=#enocean_device{ eep=undefined } } ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a RPS packet "
				"for ~ts: no EEP known for it.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, AnyNextChunk, NewState };


		{ value, Device=#enocean_device{ eep=single_input_contact } } ->
			decode_rps_single_input_contact_packet( DB_0, SenderEurid, Status,
				OptData, AnyNextChunk, Device, State );

		{ value, Device=#enocean_device{ eep=double_rocker_switch } } ->
			decode_rps_double_rocker_packet( DB_0, SenderEurid, Status,
				OptData, AnyNextChunk, Device, State );

		{ value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a RPS (F6) packet "
				"for ~ts: EEP ~ts (~ts) not supported.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ),
				  UnsupportedEepId,
				  oceanic_generated:get_second_for_eep_strings(
					UnsupportedEepId ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unsupported, _ToSkipLen=0, AnyNextChunk, NewState }

	end.



% @doc Decodes a rorg_rps single_input_contact (F6-01, simple "Switch Buttons")
% packet; in practice, only "F6-01-01" ("Push Button") exists.
%
% This corresponds to simple "Switch Buttons" (with no rocker, hence with
% punctual press/release events).
%
% Discussed a bit in [ESP3] "2.1 Packet Type 1: RADIO_ERP1", p.18, and in
% [EEP-spec] p.15.
%
-spec decode_rps_single_input_contact_packet( telegram_chunk(), eurid(),
		telegram_chunk(), telegram_opt_data(), telegram_chunk(),
		enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_rps_single_input_contact_packet( DB_0= <<DB_0AsInt:8>>, SenderEurid,
		Status, OptData, AnyNextChunk, Device,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	ButtonTransition = case DB_0AsInt band ?b4 =:= 0 of

		true ->
			pressed;

		false ->
			released

	end,

	% (no learn bit in DB_0)

	% All other bits than b4 shall be 0:
	cond_utils:assert( oceanic_check_decoding,
					   % Superfluous parentheses:
					   DB_0AsInt band ( bnot ?b4 ) =:= 0 ),

	{ PTMSwitchModuleType, NuType, RepCount } = get_rps_status_info( Status ),

	{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a R-ORG RPS packet, "
			"with DB_0=~w; sender is ~ts, PTM switch module is ~ts, "
			"NU message type is ~ts, ~ts~ts.",
			[ DB_0, get_best_naming( MaybeDeviceName, SenderEurid ),
			  ptm_module_to_string( PTMSwitchModuleType ),
			  nu_message_type_to_string( NuType ),
			  repeater_count_to_string( RepCount ),
			  maybe_optional_data_to_string( MaybeDecodedOptData, OptData ) ] ),
		basic_utils:ignore_unused(
		  [ DB_0, PTMSwitchModuleType, NuType, RepCount ] ) ),

	{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
		resolve_maybe_decoded_data( MaybeDecodedOptData ),

	Event = #push_button_event{ source_eurid=SenderEurid,
								name=MaybeDeviceName,
								eep=MaybeEepId,
								timestamp=Now,
								subtelegram_count=MaybeTelCount,
								destination_eurid=MaybeDestEurid,
								dbm=MaybeDBm,
								security_level=MaybeSecLvl,
								transition=ButtonTransition },

	{ decoded, Event, AnyNextChunk, NewState }.



% @doc Decodes a rorg_rps F6-02-01, "Light and Blind Control -
% Application Style 1" packet (switch or multipress).
%
% It contains 2 actions.
%
% Discussed a bit in [ESP3] "2.1 Packet Type 1: RADIO_ERP1", p.18, and in
% [EEP-spec] p.15.
%
-spec decode_rps_double_rocker_packet( telegram_chunk(), eurid(),
		telegram_chunk(), telegram_opt_data(), telegram_chunk(),
		enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_rps_double_rocker_packet( DB_0= <<_DB_0AsInt:8>>, SenderEurid,
		% (T21 is at offset 2, thus b5; NU at offset 3, thus b4)
		_Status= <<_:2, T21:1, NU:1, _:4>>, OptData,
		AnyNextChunk, Device,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	case { T21, NU } of

		{ _T21=1, _NU=1 } ->

			<<R1Enum:3, EB:1, R2Enum:3, SA:1>> = DB_0,

			FirstButtonDesignator = get_button_designator( R1Enum ),

			ButtonTransition = get_button_transition( EB ),

			SecondButtonDesignator = get_button_designator( R2Enum ),

			IsSecondActionValid = case SA of

				0 ->
					false;

				1 ->
					true

			end,

			{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId } =
				record_known_device_success( Device, DeviceTable ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			MaybeDecodedOptData = decode_optional_data( OptData ),

			{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
				resolve_maybe_decoded_data( MaybeDecodedOptData ),

			Event = #double_rocker_switch_event{
				source_eurid=SenderEurid,
				name=MaybeDeviceName,
				eep=MaybeEepId,
				timestamp=Now,
				subtelegram_count=MaybeTelCount,
				destination_eurid=MaybeDestEurid,
				dbm=MaybeDBm,
				security_level=MaybeSecLvl,
				first_action_button=FirstButtonDesignator,
				energy_bow=ButtonTransition,
				second_action_button=SecondButtonDesignator,
				second_action_valid=IsSecondActionValid },

			{ decoded, Event, AnyNextChunk, NewState };


		{ _T21=1, _NU=0 } ->

			% The 4 last bits shall be 0:
			%cond_utils:assert( oceanic_check_decoding,
			%				   DB_0AsInt band 2#00001111 =:= 0 ),

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

			{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId } =
				record_known_device_success( Device, DeviceTable ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			MaybeDecodedOptData = decode_optional_data( OptData ),

			{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
				resolve_maybe_decoded_data( MaybeDecodedOptData ),

			Event = #double_rocker_multipress_event{
				source_eurid=SenderEurid,
				name=MaybeDeviceName,
				eep=MaybeEepId,
				timestamp=Now,
				subtelegram_count=MaybeTelCount,
				destination_eurid=MaybeDestEurid,
				dbm=MaybeDBm,
				security_level=MaybeSecLvl,
				button_counting=MaybeButtonCounting,
				energy_bow=ButtonTransition },

			{ decoded, Event, AnyNextChunk, NewState };


		_Other ->
			{ NewDeviceTable, _Now, _MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			trace_bridge:warning_fmt( "Unable to decode a RPS packet "
				"for ~ts and EEP F6-02-01, as T21=~B and NU=~B.",
				[ eurid_to_string( SenderEurid ), T21, NU ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unsupported, _ToSkipLen=0, AnyNextChunk, NewState }

	end.



% @doc Actual decoding of responses to pending common commands.
%
% Note that the return code has already been extracted, and corresponds to a
% success.
%
% The actual waiting information is expected to have been already cleared by the
% caller.
%
-spec decode_response_tail( command_request(), telegram_data_tail(),
							telegram_opt_data(), telegram_chunk(),
							oceanic_state() ) -> decoding_outcome().
% For co_rd_version:
decode_response_tail( #command_request{ command_type=co_rd_version,
										requester=Requester },
		_DataTail= <<AppVerMain:8, AppVerBeta:8, AppVerAlpha:8, AppVerBuild:8,
					 ApiVerMain:8, ApiVerBeta:8, ApiVerAlpha:8, ApiVerBuild:8,
					 % 16 bytes:
					 ChipId:32, ChipVer:32, AppDesc:16/binary>>,
		_OptData= <<>>, AnyNextChunk, State ) ->

	Response = #read_version_response{
		app_version={ AppVerMain, AppVerBeta, AppVerAlpha, AppVerBuild },
		api_version={ ApiVerMain, ApiVerBeta, ApiVerAlpha, ApiVerBuild },
		chip_id=ChipId,
		chip_version=ChipVer,
		app_description=text_utils:buffer_to_binstring( AppDesc ) },

	notify_requester( Response, Requester, AnyNextChunk, State );


decode_response_tail( #command_request{ command_type=co_rd_version }, DataTail,
					  _OptData= <<>>, AnyNextChunk, State ) ->

	trace_bridge:error_fmt( "Received a response to a pending co_rd_version "
		"common command with an invalid data tail (~ts).",
		[ DataTail, telegram_to_string( DataTail ) ] ),

	{ invalid, _ToSkipLen=0, AnyNextChunk, State };


% For co_rd_sys_log:
decode_response_tail( #command_request{ command_type=co_rd_sys_log,
										requester=Requester },
					  DataTail, OptData, AnyNextChunk, State ) ->

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

	notify_requester( Response, Requester, AnyNextChunk, State );


% (apparently no restriction applies to DataTail, no non-matching clause to add)


% For co_rd_idbase:
decode_response_tail( #command_request{ command_type=co_rd_idbase,
										requester=Requester },
		_DataTail= <<BaseEurid:32>>,
		_OptData= <<RemainWrtCyclesNum:8>>, AnyNextChunk, State ) ->

	RemainWrtCycles = case RemainWrtCyclesNum of

		16#ff ->
			unlimited;

		Num ->
			Num

	end,

	Response = #read_base_id_info_response{
		base_eurid=BaseEurid,
		remaining_write_cycles=RemainWrtCycles },

	notify_requester( Response, Requester, AnyNextChunk, State );


decode_response_tail( #command_request{ command_type=co_rd_idbase }, DataTail,
					  OptData, AnyNextChunk, State ) ->

	trace_bridge:error_fmt( "Received a response to a pending co_rd_idbase "
		"common command with an invalid data tail (~ts) "
		"and/or optional data (~ts).",
		[ DataTail, telegram_to_string( DataTail ),
		  telegram_to_string( OptData ) ] ),

	{ invalid, _ToSkipLen=0, AnyNextChunk, State };


% Other common commands:
decode_response_tail( OtherCmdReq, DataTail, OptData, AnyNextChunk, State ) ->

	trace_bridge:error_fmt( "Responses to ~ts are currently "
		"unsupported (dropping response and waited request).",
		[ command_request_to_string( OtherCmdReq ) ] ),

	trace_bridge:debug_fmt( "Extra information: DataTail=~ts, OptData=~ts.",
		[ telegram_to_string( DataTail ), telegram_to_string( OptData ) ] ),

	{ unsupported, _ToSkipLen=0, AnyNextChunk, State }.



% @doc Notifies the specified requester of the success response regarding the
% current common command.
%
-spec notify_requester( command_response(), requester(), telegram_chunk(),
						oceanic_state() ) -> decoding_outcome().
notify_requester( Response, _Requester=internal, AnyNextChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_commands,
		trace_bridge:debug_fmt(
			"Returning the following internal response: ~ts.",
			[ device_event_to_string( Response ) ] ) ),

	% We return directly the response event in that case:
	{ decoded, Response, AnyNextChunk, State };

notify_requester( Response, RequesterPid, AnyNextChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_commands,
		trace_bridge:debug_fmt( "Sending back to requester ~w "
		"the following response: ~ts.",
		[ RequesterPid, device_event_to_string( Response ) ] ) ),

	RequesterPid ! { oceanic_command_outcome, Response },

	{ decoded, command_processed, AnyNextChunk, State }.




% Section for decoding helpers.

% Could be a bijective topic as well:

% @doc Returns the button designated by the specified enumeration.
-spec get_button_designator( enum() ) -> button_designator().
get_button_designator( _Enum=0 ) ->
	button_ai; % Button A, bottom

get_button_designator( _Enum=1 ) ->
	button_ao; % Button A, top

get_button_designator( _Enum=2 ) ->
	button_bi; % Button B, bottom

get_button_designator( _Enum=3 ) ->
	button_bo. % Button B, top


% @doc Returns the enumeration of the designated button.
-spec get_designated_button_enum( button_designator() ) -> enum().
get_designated_button_enum( _Des=button_ai ) ->
	0; % Button A, bottom

get_designated_button_enum( _Des=button_ao ) ->
	1; % Button A, top

get_designated_button_enum( _Des=button_bi ) ->
	2; % Button B, bottom

get_designated_button_enum( _Des=button_bo ) ->
	3. % Button B, top



% @doc Returns a textual description of the designated button.
-spec button_designator_to_string( button_designator() ) -> ustring().
button_designator_to_string( button_ai ) ->
	"bottom A button";

button_designator_to_string( button_ao ) ->
	"top A button";

button_designator_to_string( button_bi ) ->
	"bottom B button";

button_designator_to_string( button_bo ) ->
	"top B button".



% Could be a bijective topic as well:


% @doc Returns the button transition corresponding to the specified energy bow.
-spec get_button_transition( enum() ) -> button_transition().
get_button_transition( _EnergyBow=0 ) ->
	released;

get_button_transition( _EnergyBow=1 ) ->
	pressed.


% @doc Returns the enumeration of the specified button transition.
-spec get_button_transition_enum( button_transition() ) -> enum().
get_button_transition_enum( released ) ->
	_EnergyBow=0;

get_button_transition_enum( pressed ) ->
	_EnergyBow=1.



% @doc Decodes the RPS status byte, common to many RPS telegrams.
%
% Refer to [EEP-spec] p.11 for further details.
%
-spec get_rps_status_info( telegram_chunk() ) ->
		{ ptm_switch_module_type(), nu_message_type(), repetition_count() }.
get_rps_status_info( _Status= <<T21:2, Nu:2, RC:4>> ) ->

	PTMSwitchModuleType = case T21 + 1 of

		1 ->
			ptm1xx;

		2 ->
			ptm2xx

	end,

	% Nu expected to be 0 (Normal-message) of 1 (Unassigned-message), yet found
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



% @doc Returns a textual description of the specified PTM switch module.
-spec ptm_module_to_string( ptm_switch_module_type() ) -> ustring().
ptm_module_to_string( _ModType=ptm1xx ) ->
	% E.g. "PTM210 DB":
	"PTM1xx";

ptm_module_to_string( _ModType=ptm2xx ) ->
	"PTM2xx".



% @doc Returns a textual description of the specified "Nu" Message type, as
% defined in RPS packets.
%
-spec nu_message_type_to_string( nu_message_type() ) ->  ustring().
nu_message_type_to_string( _Nu=normal ) ->
	"normal-message";

nu_message_type_to_string( _Nu=unassigned ) ->
	"unassigned-message";

nu_message_type_to_string( _Nu=unknown_type_2 ) ->
	"unknown message type (NU=2)";

nu_message_type_to_string( _Nu=unknown_type_3 ) ->
	"unknown message type (NU=3)".



% @doc Decodes a rorg_1bs (D5) packet, that is a R-ORG telegram on one byte.
%
% Discussed in [EEP-spec] p.27.
%
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
-spec decode_1bs_packet( telegram_data_tail(), telegram_opt_data(),
			telegram_chunk(), oceanic_state() ) -> decoding_outcome().
decode_1bs_packet( DataTail= <<DB_0:8, SenderEurid:32, Status:8>>, OptData,
		AnyNextChunk, State=#oceanic_state{ device_table=DeviceTable } ) ->

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

	{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId } =
		record_device_success( SenderEurid, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a R-ORG 1BS packet, "
			"with a payload of ~B bytes (with DB_0=~w~ts; "
			"contact is ~ts), sender is ~ts, status is ~w ~ts.",
			[ size( DataTail ), DB_0, learn_to_string( LearnActivated ),
			  ContactStatus, get_best_naming( MaybeDeviceName, SenderEurid ),
			  Status,
			  maybe_optional_data_to_string( MaybeDecodedOptData,
											 OptData ) ] ),
		basic_utils:ignore_unused( [ DataTail, Status ] ) ),

	{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
		resolve_maybe_decoded_data( MaybeDecodedOptData ),

	Event = #single_input_contact_event{
		source_eurid=SenderEurid,
		name=MaybeDeviceName,
		eep=MaybeEepId,
		timestamp=Now,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		learn_activated=LearnActivated,
		contact=ContactStatus },

	{ decoded, Event, AnyNextChunk, NewState }.



% @doc Decodes a rorg_4bs (A5) packet, that is a R-ORG telegram on four bytes.
%
% Discussed in [EEP-spec] p.12.
%
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
-spec decode_4bs_packet( telegram_data_tail(), telegram_opt_data(),
			telegram_chunk(), oceanic_state() ) -> decoding_outcome().
decode_4bs_packet( DataTail= <<DB_3:8, DB_2:8, DB_1:8, DB_0:8,
		SenderEurid:32, _StatusFirstHalf:4, RC:4>>, OptData,
		AnyNextChunk, State=#oceanic_state{ device_table=DeviceTable } ) ->

	% 0xa5 is 165.

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a R-ORG 4BS packet, "
			"with a payload of ~B bytes "
			"(with DB_3=~w, DB_2=~w, DB_1=~w, DB_0=~w), "
			"sender is ~ts, ~ts~ts",
			[ size( DataTail ), DB_3, DB_2, DB_1, DB_0,
			  eurid_to_string( SenderEurid ), repeater_count_to_string( RC ),
			  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
			] ),
		basic_utils:ignore_unused( [ DataTail, RC, MaybeDecodedOptData ] ) ),

	% We have to know the specific EEP of this device in order to decode this
	% telegram:
	%
	case table:lookup_entry( SenderEurid, DeviceTable ) of

		key_not_found ->

			{ NewDeviceTable, _Now, _MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device first time seen:
			trace_bridge:warning_fmt( "Unable to decode a 4BS (A5) packet "
				"for ~ts: device not configured, no EEP known for it.",
				[ eurid_to_string( SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, AnyNextChunk, NewState };


		% Knowing the actual EEP is needed in order to decode:
		{ value, _Device=#enocean_device{ eep=undefined } } ->

			{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a 4BS (A5) packet "
				"for ~ts: no EEP known for it.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, AnyNextChunk, NewState };


		{ value, Device=#enocean_device{ eep=thermo_hygro_low } } ->
			decode_4bs_thermo_hygro_low_packet( DB_3, DB_2, DB_1, DB_0,
				SenderEurid, OptData, AnyNextChunk, Device, State );


		{ value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

			{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a 4BS (A5F6) packet "
				"for ~ts: EEP ~ts (~ts) not supported.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ),
				  UnsupportedEepId,
				  oceanic_generated:get_second_for_eep_strings(
					UnsupportedEepId ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unsupported, _ToSkipLen=0, AnyNextChunk, NewState }

   end.



% @doc Decodes a rorg_4bs (A5) packet for the thermo_hygro_low EEP ("A5-04-01"):
% "Temperature and Humidity Sensor" (04), range 0°C to +40°C and 0% to 100%
% (01).
%
% Refer to [EEP-spec] p.35.
%
-spec decode_4bs_thermo_hygro_low_packet( uint8(), uint8(), uint8(), uint8(),
		eurid(), telegram_opt_data(), telegram_chunk(), enocean_device(),
		oceanic_state() ) -> decoding_outcome().
decode_4bs_thermo_hygro_low_packet( _DB_3=0, _DB_2=ScaledHumidity,
		_DB_1=ScaledTemperature, DB_0, SenderEurid, OptData, AnyNextChunk,
		Device, State=#oceanic_state{ device_table=DeviceTable } ) ->

	cond_utils:assert( oceanic_check_decoding, DB_0 band 2#11110101 =:= 0 ),

	RelativeHumidity = round( ScaledHumidity / 250.0 * 100 ),

	MaybeTemperature = case DB_0 band ?b1 =:= 0 of

		true ->
			undefined;

		false ->
			round( ScaledTemperature / 250.0 * 40 )

	end,

	LearnActivated = DB_0 band ?b3 =:= 0,

	{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,
		begin

			TempStr = case MaybeTemperature of

				undefined ->
					"(no temperature available)";

				Temp ->
					text_utils:format( "and a ~ts",
									   [ temperature_to_string( Temp ) ] )

			end,

			trace_bridge:debug_fmt( "Decoding a R-ORG 4BS thermo_hygro_low "
				"packet, reporting a ~ts ~ts~ts; sender is ~ts, "
				"~ts.",
				[ relative_humidity_to_string( RelativeHumidity ), TempStr,
				  learn_to_string( LearnActivated ),
				  get_best_naming( MaybeDeviceName, SenderEurid ),
				  maybe_optional_data_to_string( MaybeDecodedOptData,
												 OptData ) ] )
		end ),

	{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
		resolve_maybe_decoded_data( MaybeDecodedOptData ),

	Event = #thermo_hygro_event{ source_eurid=SenderEurid,
								 name=MaybeDeviceName,
								 eep=MaybeEepId,
								 timestamp=Now,
								 subtelegram_count=MaybeTelCount,
								 destination_eurid=MaybeDestEurid,
								 dbm=MaybeDBm,
								 security_level=MaybeSecLvl,
								 relative_humidity=RelativeHumidity,
								 temperature=MaybeTemperature,

								 % As "A5-04-01":
								 temperature_range=low,

								 learn_activated=LearnActivated },

	{ decoded, Event, AnyNextChunk, NewState }.



% @doc Decodes a rorg_ute (D4) packet, that is a R-ORG telegram for Universal
% Teach-in/out, EEP based (UTE), one way of pairing devices.
%
% Discussed in [EEP-gen] p.17; p.25 for the query and p.26 for the response.
%
-spec decode_ute_packet( telegram_data_tail(), telegram_opt_data(),
			telegram_chunk(), oceanic_state() ) -> decoding_outcome().
% This is a Teach-In/Out query UTE request (Broadcast / CMD: 0x0, p.25),
% broadcasting (typically after one of its relevant buttons has been pressed) a
% request that devices declare to this requester device.
%
% Proceeding byte per byte is probably clearer:
decode_ute_packet(
		DataTail= << CommDir:1, ResExpected:1, ReqType:2, Cmd:4, % = DB_6
					 ChanCount:8,                                % = DB_5
					 ManufIdLSB:8,                               % = DB_4
					 _:5, ManufIdMSB:3,                          % = DB_3
					 Type:8,                                     % = DB_2
					 Func:8,                                     % = DB_1
					 RORG:8,                                     % = DB_0
					 InitiatorEurid:32,
					 Status:8>>,
		OptData, AnyNextChunk,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	CommDirection= case CommDir of

		0 ->
			unidirectional;

		% Usually:
		1 ->
			bidirectional

	end,

	ResponseExpected = case ResExpected of

		% Usually:
		0 ->
			true;

		1 ->
			false

	end,

	MaybeRequestType = case ReqType of

		0 ->
			teach_in;

		1 ->
			teach_out;

		% Unspecified (usual):
		2 ->
			undefined;

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

	{ NewDeviceTable, #enocean_device{ name=MaybeName,
									   eep=MaybeEepId }, Now } =
		declare_device_from_teach_in( InitiatorEurid, InitiatorEep,
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

			trace_bridge:debug_fmt( "Decoding a Teach-In query UTE packet "
				"from ~ts, whereas ~ts, for a ~ts communication, "
				"response expected: ~ts, request type: ~ts "
				"involving ~ts channels, manufacturer ID: ~ts, "
				"PTM switch module is ~ts, message type is ~ts, ~ts~ts.",
				[ eurid_to_bin_string( InitiatorEurid, State ),
				  get_eep_description( InitiatorEep ), CommDirection,
				  ResponseExpected, MaybeRequestType, ChanStr,
				  text_utils:integer_to_hexastring( ManufId ),
				  ptm_module_to_string( PTMSwitchModuleType ),
				  nu_message_type_to_string( NuType ),
				  repeater_count_to_string( RepCount ),
				  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
				] )

		end,
		basic_utils:ignore_unused( [ PTMSwitchModuleType, NuType, RepCount ] )
						),

	<<_DB_6:8,ToEcho/binary>> = DataTail,

	% Probably that destination is broadcast:
	{ _MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
		resolve_maybe_decoded_data( MaybeDecodedOptData ),

	Event = #teach_request{ source_eurid=InitiatorEurid,
							name=MaybeName,
							eep=MaybeEepId,
							timestamp=Now,
							destination_eurid=MaybeDestEurid,
							dbm=MaybeDBm,
							security_level=MaybeSecLvl,
							comm_direction=CommDirection,
							response_expected=ResponseExpected,
							request_type=MaybeRequestType,
							channel_taught=ChannelTaught,
							manufacturer_id=ManufId,
							echo_content=ToEcho },

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	{ decoded, Event, AnyNextChunk, NewState }.




% @doc Decodes a rorg_vld (D2) packet, that is a R-ORG telegram containing
% Variable Length Data.
%
% VLD telegrams carry a variable payload between 1 and 14 bytes, depending on
% their design.
%
% Discussed in [EEP-gen] p.12.
%
% Various packet types exist, in both directions (from/to sensor/actuator), and
% depend on the actual EEP (hence on its FUNC and TYPE) implemented by the
% emitter device.
%
% Yet only the RORG (namely D2) is specified on such telegrams, therefore their
% interpretation depends on the extra FUNC and TYPE information supposed to be
% known a priori by the receiver.
%
-spec decode_vld_packet( telegram_data_tail(), telegram_opt_data(),
			telegram_chunk(), oceanic_state() ) -> decoding_outcome().
decode_vld_packet( DataTail, OptData, AnyNextChunk,
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

			% Not trying to decode optional data then.

			{ NewDeviceTable, _Now, _MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			trace_bridge:warning_fmt( "Unable to decode a VLD (D2) packet "
				"for ~ts: device not configured, no EEP known for it.",
				[ eurid_to_string( SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, AnyNextChunk, NewState };


		% Knowing the actual EEP is needed in order to decode:
		{ value, _Device=#enocean_device{ eep=undefined } } ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a VLD packet "
				"for ~ts: no EEP known for it.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, AnyNextChunk, NewState };


		{ value, Device=#enocean_device{ eep=smart_plug } } ->
			decode_vld_smart_plug_packet( Payload, SenderEurid, Status,
				OptData, AnyNextChunk, Device, State );


		{ value, Device=#enocean_device{ eep=smart_plug_with_metering } } ->
			decode_vld_smart_plug_with_metering_packet( Payload,
				SenderEurid, Status, OptData, AnyNextChunk, Device, State );


		{ value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a VLD (D2) packet "
				"for ~ts: EEP ~ts (~ts) not supported.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ),
				  UnsupportedEepId,
				  oceanic_generated:get_second_for_eep_strings(
					UnsupportedEepId ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unsupported, _ToSkipLen=0, AnyNextChunk, NewState }

	end.



% @doc Decodes a rorg_vld smart_plug (D2-01-0A, an "Electronic switches and
% dimmers with Energy Measurement and Local Control" of type 0A) packet.
%
% This corresponds to basic smart, non-metering plugs bidirectional actuators
% that control (switch on/off) most electrical load (e.g. appliances).
%
% Discussed in [EEP-spec] p.143.
%
-spec decode_vld_smart_plug_packet( vld_payload(), eurid(), telegram_chunk(),
		telegram_opt_data(), telegram_chunk(), enocean_device(),
		oceanic_state() ) -> decoding_outcome().
decode_vld_smart_plug_packet( _Payload= <<_:4, CmdAsInt:4, _Rest/binary>>,
		SenderEurid, _Status, OptData, AnyNextChunk, Device,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	Cmd = oceanic_generated:get_second_for_vld_d2_00_cmd( CmdAsInt ),

	{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a VLD smart plug packet "
			"for command '~ts' (~B); sender is ~ts, ~ts.",
			[ Cmd, CmdAsInt, get_best_naming( MaybeDeviceName, SenderEurid ),
			  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
			] ),
		basic_utils:ignore_unused( [ SenderEurid, Cmd, MaybeDeviceName,
									 MaybeDecodedOptData ] ) ),

	%{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
	%   resolve_maybe_decoded_data( MaybeDecodedOptData ),

	%Event = to_do,

	%{ decoded, Event, AnyNextChunk, NewState }.

	{ unsupported, _ToSkipLen=0, AnyNextChunk, NewState }.



% @doc Decodes a rorg_vld smart_plug_with_metering (D2-01-0B, an "Electronic
% switches and dimmers with Energy Measurement and Local Control" of type 0B)
% packet.
%
% This corresponds to smart, metering plugs bidirectional actuators that control
% (switch on/off) most electrical load (e.g. appliances) and may report it.
%
% Discussed in [EEP-spec] p.143.
%
decode_vld_smart_plug_with_metering_packet(
		_Payload= <<_:4, CmdAsInt:4, _Rest/binary>>,
		SenderEurid, _Status, OptData, AnyNextChunk, Device,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	Cmd = oceanic_generated:get_second_for_vld_d2_00_cmd( CmdAsInt ),

	{ NewDeviceTable, _Now, MaybeDeviceName, _MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt(
			"Decoding a VLD smart plug with metering packet "
			"for command '~ts' (~B); sender is ~ts, ~ts.",
			[ Cmd, CmdAsInt, get_best_naming( MaybeDeviceName, SenderEurid ),
			  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
			] ),
		basic_utils:ignore_unused( [ SenderEurid, Cmd, MaybeDeviceName,
									 MaybeDecodedOptData ] ) ),

	%{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
	%   resolve_maybe_decoded_data( MaybeDecodedOptData ),

	%Event = to_do,

	%{ decoded, Event, AnyNextChunk, NewState }.

	{ unsupported, _ToSkipLen=0, AnyNextChunk, NewState }.




% @doc Returns a textual description of the specified temperature.
-spec temperature_to_string( celsius() ) -> ustring().
temperature_to_string( Temp ) ->
	text_utils:format( "temperature of ~B°C", [ Temp ] ).


% @doc Returns a textual description of the specified relative humidity.
-spec relative_humidity_to_string( percent() ) -> ustring().
relative_humidity_to_string( HPerCent ) ->
	text_utils:format( "relative humidity of ~B%", [ HPerCent ] ).



% @doc Returns a textual description of the specified learning status.
-spec learn_to_string( boolean() ) -> ustring().
learn_to_string( _LearnActivated=true ) ->
	" whereas device learning is activated";

learn_to_string( _LearnActivated=false ) ->
	", with no device learning activated".



% @doc Decodes the specified optional data, if any.
%
% Refer to [ESP3] p.18 for its description.
%
% The CRC for the overall full data (base+optional) is expected to have been
% checked beforehand.
%
-spec decode_optional_data( telegram_opt_data() ) ->
										maybe( decoded_optional_data() ).
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



% @doc Decodes specified byte as a dBm measurement.
-spec decode_maybe_dbm( uint8() ) -> maybe( dbm() ).
decode_maybe_dbm( 16#ff ) ->
	% Should be a sending:
	undefined;

decode_maybe_dbm( V ) ->
	-V.


% @doc Decodes specified byte as a security level.
-spec decode_maybe_security_level( uint8() ) -> maybe( security_level() ).
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



% @doc Helper to resolve correctly elements (if any) of optional data.
-spec resolve_maybe_decoded_data( maybe( decoded_optional_data() ) ) ->
		{ maybe( subtelegram_count() ), maybe( eurid() ), maybe( dbm() ),
		  maybe( security_level() ) }.
resolve_maybe_decoded_data( _MaybeDecodedOptData=undefined ) ->
	{ undefined, undefined, undefined, undefined };

resolve_maybe_decoded_data( DecodedOptData ) ->
	DecodedOptData.



% @doc Records that a telegram could be successfully decoded for the specified
% device, registering it if it was not already.
%
-spec record_device_success( eurid(), device_table() ) ->
	{ device_table(), timestamp(), maybe( device_name() ), maybe( eep_id() ) }.
record_device_success( Eurid, DeviceTable ) ->

	case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device ~ts through "
				"listening.", [ eurid_to_string( Eurid ) ] ),

			Now = time_utils:get_timestamp(),

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 eep=undefined,
										 discovered_through=listening,
										 first_seen=Now,
										 last_seen=Now,
										 telegram_count=1,
										 error_count=0 },

			% Necessarily new:
			NewDeviceTable = table:add_entry( Eurid, NewDevice, DeviceTable ),

			{ NewDeviceTable, Now, undefined, undefined };


		{ value, Device } ->
			record_known_device_success( Device, DeviceTable )

	end.



% @doc Records that a telegram could be successfully decoded for the specified
% already-known device.
%
-spec record_known_device_success( enocean_device(), device_table() ) ->
	{ device_table(), timestamp(), maybe( device_name() ), maybe( eep_id() ) }.
record_known_device_success( Device=#enocean_device{
		eurid=Eurid,
		name=MaybeDeviceName,
		eep=MaybeEepId,
		first_seen=MaybeFirstSeen,
		telegram_count=TeleCount }, DeviceTable ) ->

	Now = time_utils:get_timestamp(),

	NewFirstSeen = case MaybeFirstSeen of

		undefined ->
			Now;

		FirstSeen ->
			FirstSeen

	end,

	UpdatedDevice = Device#enocean_device{ first_seen=NewFirstSeen,
										   last_seen=Now,
										   telegram_count=TeleCount+1 },

	NewDeviceTable = table:add_entry( Eurid, UpdatedDevice, DeviceTable ),

	{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId }.



% @doc Records that a telegram could not be successfully decoded for the
% specified device, registering it if it was not already.
%
% Note that many failures do not even allow identifying the emitting device.
%
-spec record_device_failure( eurid(), device_table() ) ->
	{ device_table(), timestamp(), maybe( device_name() ), maybe( eep_id() ) }.
record_device_failure( Eurid, DeviceTable ) ->

	Now = time_utils:get_timestamp(),

	case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device ~ts "
				"through failure.", [ eurid_to_string( Eurid ) ] ),

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 eep=undefined,
										 discovered_through=listening,
										 first_seen=Now,
										 last_seen=Now,
										 telegram_count=0,
										 error_count=1 },

			% Necessarily new:
			NewDeviceTable = table:add_entry( Eurid, NewDevice, DeviceTable ),

			{ NewDeviceTable, Now, undefined, undefined };


		{ value, Device } ->
			record_known_device_failure( Device, DeviceTable )

	end.



% @doc Records that a telegram could not be successfully decoded for the
% specified already-known device.
%
-spec record_known_device_failure( enocean_device(), device_table() ) ->
	{ device_table(), timestamp(), maybe( device_name() ), maybe( eep_id() ) }.
record_known_device_failure( Device=#enocean_device{
		eurid=Eurid,
		name=MaybeDeviceName,
		eep=MaybeEepId,
		first_seen=MaybeFirstSeen,
		error_count=ErrCount }, DeviceTable ) ->

	Now = time_utils:get_timestamp(),

	NewFirstSeen = case MaybeFirstSeen of

		undefined ->
			Now;

		FirstSeen ->
			FirstSeen

	end,

	UpdatedDevice = Device#enocean_device{ first_seen=NewFirstSeen,
										   last_seen=Now,
										   error_count=ErrCount+1 },

	NewDeviceTable = table:add_entry( Eurid, UpdatedDevice, DeviceTable ),

	{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId }.



% @doc Stops and terminates (asynchronously) the supposedly-existing Oceanic
% server.
-spec stop() -> void().
stop() ->
	stop( get_server_pid() ).


% @doc Stops and terminates (asynchronously) the specified Oceanic server.
-spec stop( oceanic_server_pid() ) -> void().
stop( SrvPid ) ->
	trace_bridge:debug_fmt( "Stopping the Oceanic server ~w.", [ SrvPid ] ),
	SrvPid ! terminate.



% @doc Stops and terminates synchronously the specified Oceanic server.
-spec synchronous_stop( oceanic_server_pid() ) -> void().
synchronous_stop( SrvPid ) ->
	SrvPid ! { terminateSynchronously, self() },

	trace_bridge:debug_fmt( "Stopping synchronously the Oceanic server ~w.",
							[ SrvPid ] ),

	receive

		oceanic_terminated ->
			ok

	end.



% @doc Returns the Oceanic identifier corresponding to the specified packet
% type.
%
-spec get_packet_type( enum() ) -> maybe( packet_type() ).
get_packet_type( PacketTypeNum ) ->
	% Topic defined by the module that Oceanic generates:
	oceanic_generated:get_first_for_packet_type( PacketTypeNum ).



% @doc Returns the registration name of the Oceanic server.
-spec get_server_registration_name() -> registration_name().
get_server_registration_name() ->
	?oceanic_server_reg_name.


% @doc Returns the PID of the (supposedly-existing) Oceanic server.
-spec get_server_pid() -> oceanic_server_pid().
get_server_pid() ->
	naming_utils:get_locally_registered_pid_for( ?oceanic_server_reg_name ).



% @doc Returns a textual description of the specified telegram.
-spec telegram_to_string( telegram() ) -> ustring().
telegram_to_string( Telegram ) ->
	text_utils:format( "telegram ~w of size ~B bytes "
		"(corresponding to hexadecimal '~ts')",
		[ Telegram, size( Telegram ), telegram_to_hexastring( Telegram ) ] ).


% @doc Returns an hexadecimal string corresponding to the specified telegram.
%
% Useful for testing with serial clients like cutecom.
%
-spec telegram_to_hexastring( telegram() ) -> ustring().
telegram_to_hexastring( Telegram ) ->
	text_utils:binary_to_hexastring( Telegram ).



% @doc Returns an hexadecimal string corresponding to the specified telegram.
%
% Useful for testing with serial clients like cutecom.
%
-spec hexastring_to_telegram( ustring() ) -> telegram().
hexastring_to_telegram( HexaStr ) ->
	text_utils:hexastring_to_binary( HexaStr ).




% @doc Resolves the specified EEP triplet into a proper EEP identifier (atom),
% if possible.
%
-spec resolve_eep( eep() ) -> maybe( eep_id() ).
resolve_eep( EepTriplet ) ->
	case oceanic_generated:get_maybe_first_for_eep_triplets( EepTriplet ) of

		undefined ->
			trace_bridge:warning_fmt( "The EEP specified as ~w is not known "
				"of Oceanic and will be ignored.", [ EepTriplet ] ),
			undefined;

		KnownEepId ->
			KnownEepId

	end.



% @doc Returns a raw, (plain) textual description of the specified EURID.
-spec eurid_to_string( eurid() ) -> ustring().
eurid_to_string( _Eurid=?eurid_broadcast ) ->
	"the address for broadcast transmission";

eurid_to_string( Eurid ) ->

	% We want to return for example a correct "002ef196", not an
	% ambiguously-shortened "2ef196":
	%
	HexaStr = text_utils:integer_to_hexastring( Eurid ),

	% 32-bit:
	PaddedStr = text_utils:pad_string_right( HexaStr, _Width=8, $0 ),

	text_utils:flatten( PaddedStr ).



% @doc Returns the actual EURID corresponding to the specified (plain) EURID
% string.
%
% Ex: 3076502 = oceanic:string_to_eurid("002ef196")
%
-spec string_to_eurid( ustring() ) -> eurid().
string_to_eurid( EuridStr ) ->
	text_utils:hexastring_to_integer( EuridStr ).


% @doc Returns the broadcast EURID, suitable to target all devices in range.
-spec get_broadcast_eurid() -> eurid().
get_broadcast_eurid() ->
	?eurid_broadcast.


% @doc Returns a raw, direct (binary) textual description of the specified
% EURID.
%
-spec eurid_to_bin_string( eurid() ) -> bin_string().
eurid_to_bin_string( Eurid ) ->
	text_utils:string_to_binary( eurid_to_string( Eurid ) ).


% @doc Returns a (binary) textual description of the specified EURID, possibly
% translated to a user-friendly device name if any is known for that device.
%
-spec eurid_to_bin_string( eurid(), oceanic_state() ) -> bin_string().
eurid_to_bin_string( Eurid=?eurid_broadcast, _OceanicState ) ->
	eurid_to_bin_string( Eurid );

eurid_to_bin_string( Eurid, #oceanic_state{ device_table=DeviceTable } ) ->

	case table:get_value_with_default( Eurid, _Def=undefined, DeviceTable ) of

		undefined ->
			eurid_to_bin_string( Eurid );

		#enocean_device{ name=MaybeName } ->
			get_best_bin_naming( MaybeName, Eurid )

	end.



% @doc Returns the best naming for a device, as any kind of string, depending on
% the available information.
%
-spec get_best_naming( maybe( device_name() ), eurid() ) -> any_string().
get_best_naming( _MaybeDevName=undefined, Eurid ) ->
	eurid_to_string( Eurid );

get_best_naming( BinDevName, _Eurid ) ->
	text_utils:bin_format( "'~ts'", [ BinDevName ] ).


% @doc Returns the best naming for a device, as a binary, depending on the
% available information.
%
-spec get_best_bin_naming( maybe( device_name() ), eurid() ) -> bin_string().
get_best_bin_naming( _MaybeDevName=undefined, Eurid ) ->
	eurid_to_bin_string( Eurid );

get_best_bin_naming( BinDevName, _Eurid ) ->
	BinDevName.



% @doc Converts an EEP described as a string into its internal form.
%
% Input example: "D5-00-01".
%
-spec string_to_eep( ustring() ) -> eep().
string_to_eep( Str ) ->

	% No dash wanted:
	case text_utils:filter( $-, Str ) of

		[ R1, R2, F1, F2, T1, T2 ] ->

			[ Rorg, Func, Type ] = [
				text_utils:hexastring_to_integer( HS, _ExpectPrefix=false )
					|| HS <- [ [ R1, R2 ], [ F1, F2 ], [ T1, T2 ] ] ],
			_Eep={ Rorg, Func, Type };

		_ ->
			throw( { invalid_eep_string, Str } )

	end.


-spec optional_data_to_string( decoded_optional_data() ) -> ustring().
optional_data_to_string( _OptData={ SubTelNum, DestinationEurid, MaybeDBm,
									MaybeSecurityLevel } ) ->
	optional_data_to_string( SubTelNum, DestinationEurid, MaybeDBm,
							 MaybeSecurityLevel ).


% @doc Returns a textual description of the specified decoded optional data.
-spec optional_data_to_string( subtelegram_count(), eurid(), maybe( dbm() ),
							   maybe( security_level() ) ) -> ustring().
optional_data_to_string( _SubTelNum=undefined, _DestinationEurid=undefined,
						 _MaybeDBm=undefined, _MaybeSecurityLevel=undefined ) ->
	" (with no optional data)";

optional_data_to_string( SubTelNum, DestinationEurid, MaybeDBm,
						 MaybeSecurityLevel ) ->

	DBmstr = case MaybeDBm of

		undefined ->
			"";

		DBm ->
			text_utils:format( ", best RSSI value being ~B dBm", [ DBm ] )

	end,

	SecStr = case MaybeSecurityLevel of

		undefined ->
			"";

		SecLevel ->
			text_utils:format( "; security level: ~ts",
							   [ security_level_to_string( SecLevel ) ] )

	end,

	% "Send: 3 / receive: 0", yet often seen as 1:
	SubTelStr = case SubTelNum of

		0 ->
			"no subtelegram";

		1 ->
			"a single subtelegram";

		_ ->
			text_utils:format( "~B subtelegrams", [ SubTelNum ] )

	end,

	text_utils:format( " with ~ts, targeted to ~ts~ts~ts",
		[ SubTelStr, eurid_to_string( DestinationEurid ), DBmstr, SecStr ] ).



% @doc Returns a textual description of the specified decoded maybe-optional
% data, otherwise from the corresponding raw data.
%
-spec maybe_optional_data_to_string( maybe( decoded_optional_data() ),
									 telegram_opt_data() ) -> ustring().
maybe_optional_data_to_string( _MaybeDecodedOptData=undefined, OptData ) ->
	text_utils:format( ", with optional data of ~B bytes that could not "
		"be decoded: ~w", [ size( OptData ), OptData ] );

maybe_optional_data_to_string( DecodedOptData, _OptData ) ->
	optional_data_to_string( DecodedOptData ).



% @doc Returns a textual description of the specified security level.
-spec security_level_to_string( security_level() ) -> ustring().
security_level_to_string( not_processed ) ->
	"telegram not processed";
security_level_to_string( obsolete ) ->
	"obsolete";

security_level_to_string( decrypted ) ->
	"decrypted";

security_level_to_string( authenticated ) ->
	"authenticated";

security_level_to_string( decrypted_and_authenticated ) ->
	"decrypted and authenticated".



% @doc Returns a textual description of the specified repeater count.
-spec repeater_count_to_string( count() ) -> ustring().
repeater_count_to_string( _RC=0 ) ->
	"with no repeating done";

repeater_count_to_string( _RC=1 ) ->
	"with a single repeating done";

repeater_count_to_string( RC ) ->
	text_utils:format( "with ~B repeatings done", [ RC ] ).



% @doc Returns a textual description of the specified device event.
-spec device_event_to_string( device_event() ) -> ustring().
device_event_to_string( #thermo_hygro_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		relative_humidity=RelativeHumidity,
		temperature=MaybeTemperature,
		temperature_range=TempRange,
		learn_activated=LearnActivated } ) ->

	TempStr = case MaybeTemperature of

		undefined ->
			"(no temperature available)";

		Temp ->
			text_utils:format( "and a ~ts (sensitivity range: ~ts)",
				[ temperature_to_string( Temp ), TempRange ] )

	end,

	text_utils:format( "thermo-hygro sensor device ~ts reports at ~ts "
		"a ~ts ~ts~ts; this is declared~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  time_utils:timestamp_to_string( Timestamp ),
		  relative_humidity_to_string( RelativeHumidity ), TempStr,

		  learn_to_string( LearnActivated ),

		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),

		  % Multiple A5-04-01-like candidates:
		  get_eep_description( MaybeEepId ) ] );


device_event_to_string( #single_input_contact_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		learn_activated=LearnActivated,
		contact=ContactStatus } ) ->

	% Apparently either state transitions or just periodic state reports:
	text_utils:format( "single-contact device ~ts is in ~ts state at ~ts~ts; "
		"this is declared~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  get_contact_status_description( ContactStatus ),
		  time_utils:timestamp_to_string( Timestamp ),
		  learn_to_string( LearnActivated ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  get_eep_description( MaybeEepId, _DefaultDesc="D5-00-01" ) ] );


device_event_to_string( #push_button_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		transition=ButtonTransition } ) ->
	text_utils:format(
		"push-button device ~ts has been ~ts at ~ts~ts; ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  get_button_transition_description( ButtonTransition ),
		  time_utils:timestamp_to_string( Timestamp ),
		  get_eep_description( MaybeEepId, _DefaultDesc="F6-01-01" ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ) ] );


device_event_to_string( #double_rocker_switch_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		first_action_button=FirstButtonDesignator,
		energy_bow=ButtonTransition,
		second_action_button=SecondButtonDesignator,
		second_action_valid=IsValid } ) ->

	%% SecondStr = case IsValid of

	%%	true ->
	%%		text_utils:format( " and its ~ts",
	%%			[ button_designator_to_string( SecondButtonDesignator ) ] );

	%%	false ->
	%%		""

	%% end,

	% Less ambiguous:
	SecondStr = text_utils:format( "~ts is "
		++ case IsValid of

			true ->
				"";

			false ->
				"not "

		   end ++ "valid",
				[ button_designator_to_string( SecondButtonDesignator ) ] ),

	text_utils:format( "double-rocker device ~ts has its ~ts ~ts, "
		"whereas its second action ~ts, at ~ts; this is declared~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  button_designator_to_string( FirstButtonDesignator ),
		  get_button_transition_description( ButtonTransition ),
		  SecondStr,
		  time_utils:timestamp_to_string( Timestamp ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  get_eep_description( MaybeEepId, _DefaultDesc="F6-02-01" ) ] );


device_event_to_string( #double_rocker_multipress_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		button_counting=MaybeButtonCounting,
		energy_bow=ButtonTransition } ) ->

	TransStr = case MaybeButtonCounting of

		undefined ->
			"an unknown number of buttons (abnormal)";

		none ->
			"no button";

		three_or_four ->
			"3 or 4 buttons"

	end ++ " " ++ get_button_transition_description( ButtonTransition ),

	text_utils:format( "double-rocker device ~ts has ~ts simultaneously "
		"at ~ts; this is declared~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ), TransStr,
		  time_utils:timestamp_to_string( Timestamp ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  get_eep_description( MaybeEepId, _DefaultDesc="F6-02-01" ) ] );


device_event_to_string( #read_version_response{
		app_version=AppVersion,
		api_version=ApiVersion,
		chip_id=ChipId,
		chip_version=ChipVersion,
		app_description=BinAppDesc } ) ->

	text_utils:format( "read application version ~ts, API version ~ts, "
		"chip ID ~ts, chip version ~B and application description '~ts'",
		[ text_utils:version_to_string( AppVersion ),
		  text_utils:version_to_string( ApiVersion ),
		  text_utils:integer_to_hexastring( ChipId ), ChipVersion,
		  BinAppDesc ] );


device_event_to_string( #read_logs_response{ app_counters=AppCounters,
											 api_counters=ApiCounters } ) ->


	text_utils:format( "read counters: ~B for application: ~w, "
		"and ~B for API: ~w", [ length( AppCounters ), AppCounters,
								length( ApiCounters), ApiCounters ] );


device_event_to_string( #read_base_id_info_response{
		base_eurid=BaseEurid,
		remaining_write_cycles=RemainWrtCycles } ) ->

	text_utils:format( "read gateway base ID ~ts, "
		% Possibly 'unlimited':
		"for ~p remaining write cycles",
		[ eurid_to_string( BaseEurid ), RemainWrtCycles ] );


device_event_to_string( command_processed ) ->
	"the current command has been successfully processed";

device_event_to_string( error_return ) ->
	"the current command was reported by the target device as having failed";

device_event_to_string( not_supported_return ) ->
	"the current command was reported by the target device as "
		"not being supported ";

device_event_to_string( wrong_parameter_return ) ->
	"the current command was reported by the target device as "
		"having failed due to incorrect supplied parameters";

device_event_to_string( operation_denied ) ->
	"the current command was reported by the target device as "
		"having failed due to being a denied operation";

device_event_to_string( time_out ) ->
	"the current command failed to be acknowledged on time";

device_event_to_string( OtherEvent ) ->
	text_utils:format( "unknown event: ~p", [ OtherEvent ] ).



% @doc Returns a textual description of the specified command request.
-spec command_request_to_string( command_request() ) -> ustring().
% Requester is either PID or 'internal':
command_request_to_string( #command_request{ command_type=undefined,
											 command_telegram=CmdTelegram,
											 requester=Requester } ) ->
	text_utils:format( "command based on ~ts, on behalf of "
		"requester ~w", [ telegram_to_string( CmdTelegram ), Requester ] );

command_request_to_string( #command_request{ command_type=CmdType,
											 command_telegram=CmdTelegram,
											 requester=Requester } ) ->
	text_utils:format( "command of type ~p, based on ~ts, on behalf of "
		"requester ~w",
		[ CmdType, telegram_to_string( CmdTelegram ), Requester ] ).



% @doc Returns a textual description of the specified device name.
-spec get_name_description( maybe( device_name() ), eurid() ) -> ustring().
get_name_description( _MaybeName=undefined, Eurid ) ->
	text_utils:format( "of EURID ~ts", [ eurid_to_string( Eurid ) ] );

get_name_description( Name, Eurid ) ->
	text_utils:format( "'~ts' (whose EURID is ~ts)",
					   [ Name, eurid_to_string( Eurid ) ] ).



% @doc Returns a textual description of the specified button transition.
-spec get_button_transition_description( button_transition() ) -> ustring().
get_button_transition_description( _Button=pressed ) ->
	"pressed";

get_button_transition_description( _ContactStatus=released ) ->
	"released".



% @doc Returns a textual description of the specified contact status.
-spec get_contact_status_description( boolean() ) -> ustring().
get_contact_status_description( _ContactStatus=open ) ->
	"opened";

get_contact_status_description( _ContactStatus=closed ) ->
	"closed".


% @doc Returns a textual description of the specified EEP (if any).
-spec get_eep_description( maybe( eep_id() ) ) -> ustring().
get_eep_description( _MaybeEepId=undefined ) ->
	"its EEP is not known";

get_eep_description( EepId ) ->
	text_utils:format( "its EEP is ~ts (~ts)", [ EepId,
		oceanic_generated:get_second_for_eep_strings( EepId ) ] ).


% @doc Returns a textual description of the specified EEP (if any), with a
% default.
%
-spec get_eep_description( maybe( eep_id() ), ustring() ) -> ustring().
get_eep_description( _MaybeEepId=undefined, DefaultDesc ) ->
	text_utils:format( "its EEP is not known (supposing ~ts)",
					   [ DefaultDesc ] );

get_eep_description( EepId, _DefaultDesc ) ->
	get_eep_description( EepId ).



% @doc Returns a textual description of the specified state of the Oceanic
% server.
%
-spec state_to_string( oceanic_state() ) -> ustring().
state_to_string( #oceanic_state{
		serial_server_pid=SerialServerPid,
		emitter_eurid=EmitterEurid,
		device_table=DeviceTable,
		command_queue=CmdQueue,
		waited_command_info=MaybeWaitedCommandInfo,
		wait_timeout=WaitTimeout,
		command_count=CmdCount,
		sent_count=SentCount,
		discarded_count=DiscardedCount,
		traffic_level=TrafficLvl,
		jamming_threshold=JamThreshold,
		event_listener_pid=MaybeListenerPid } ) ->

	WaitStr = case MaybeWaitedCommandInfo of

		undefined ->
			"not having any command pending";

		{ WaitedCmdReq, TimerRef } ->
			text_utils:format( "waiting for a pending ~ts, "
				"associated to timer ~w",
				[ command_request_to_string( WaitedCmdReq ), TimerRef ] )

	end,

	QStr = text_utils:format( "based on a ~ts, with ~ts queued whereas "
		"~ts been issued",
		[ time_utils:time_out_to_string( WaitTimeout ),
		  case queue:len( CmdQueue ) of

			  0 ->
				  "no command";

			  1 ->
				  "a single command";

			  QCount ->
				  text_utils:format( "~B commands", [ QCount ] )

		  end,

		  case CmdCount of

			  0 ->
				  "none has";

			  1 ->
				  "a single one has";

			  _ ->
				  text_utils:format( "a total of ~B of them have",
									 [ CmdCount ] )

		  end ] ),

	ListenStr = case MaybeListenerPid of

		undefined ->
			"not having a listener of Enocean events registered";

		ListenerPid ->
			text_utils:format( "having ~w registered as listener "
							   "of Enocean events", [ ListenerPid ] )

	end,

	SentStr = case SentCount of

		0 ->
			"not having sent any telegram";

		1 ->
			"having sent a single telegram";

		_ ->
			text_utils:format( "having sent ~B telegrams", [ SentCount ] )

	end,

	DiscStr = case DiscardedCount of

		0 ->
			"not having discarded any telegram";

		1 ->
			"having discarded a single telegram";

		_ ->
			text_utils:format( "having discarded ~B telegrams",
							   [ DiscardedCount ] )

	end,

	JamStr = text_utils:format( "currently monitoring a sliding traffic "
		"of roughly ~B bytes per second (for a jamming threshold set at "
		"~B bytes per second)", [ TrafficLvl, JamThreshold ] ),

	text_utils:format( "Oceanic server using serial server ~w, "
		"using emitter EURID ~ts, ~ts, ~ts; ~ts, ~ts, ~ts, ~ts, "
		"and knowing ~ts",
		[ SerialServerPid, eurid_to_string( EmitterEurid ), WaitStr, QStr,
		  ListenStr, SentStr, DiscStr, JamStr,
		  device_table_to_string( DeviceTable ) ] ).



% @doc Returns a textual description of the specified device table.
-spec device_table_to_string( device_table() ) -> ustring().
device_table_to_string( DeviceTable ) ->

	case table:values( DeviceTable ) of

		[] ->
			"no Enocean device";

		[ SingleDevice ] ->
			text_utils:format( "a single Enocean ~ts",
							   [ device_to_string( SingleDevice ) ] );

		Devices ->
			text_utils:format( "~B Enocean devices: ~ts", [ length( Devices ),
				text_utils:strings_to_sorted_string( [ device_to_string( D )
					|| D <- Devices ] ) ] )

	end.



% @doc Returns a textual description of the specified Enocean device.
-spec device_to_string( enocean_device() ) -> ustring().
device_to_string( #enocean_device{ eurid=Eurid,
								   name=MaybeName,
								   eep=MaybeEepId,
								   discovered_through=DiscOrigin,
								   first_seen=MaybeFirstTimestamp,
								   last_seen=MaybeLastTimestamp,
								   telegram_count=TeleCount,
								   error_count=ErrCount } ) ->

	NameStr = case MaybeName of

		undefined ->
			text_utils:format( "unnamed device of EURID ~ts",
							   [ eurid_to_bin_string( Eurid ) ] );

		Name ->
			text_utils:format( "device '~ts' (EURID: ~ts)",
							   [ Name, eurid_to_bin_string( Eurid ) ] )

	end,

	EepDescStr = case MaybeEepId of

		undefined ->
			"an undefined EEP";

		EepId ->
			case oceanic_generated:get_maybe_second_for_eep_strings( EepId ) of

				undefined ->
					"an EEP not known of Oceanic";

				EepStr ->
					text_utils:format( "EEP ~ts", [ EepStr ] )

			end

	end,

	{ SeenStr, DiscStr, TeleStr, ErrStr } = case MaybeFirstTimestamp of

		undefined ->
			{ "never seen by this server", "", "", "" };

		FirstTimestamp ->
			SeenCountStr = case MaybeLastTimestamp of

				% Note that multiple telegrams may be received during the same
				% initial second:
				%
				FirstTimestamp ->
					text_utils:format( "seen only once by this server, "
						"on ~ts; ",
						[ time_utils:timestamp_to_string( FirstTimestamp ) ] );

				LastTimestamp ->
					text_utils:format( "seen firstly by this server on ~ts, "
						"and lastly on ~ts; ",
						[ time_utils:timestamp_to_string( FirstTimestamp ),
						  time_utils:timestamp_to_string( LastTimestamp ) ] )

			end,

			DiscovStr = "it was discovered through " ++ case DiscOrigin of

				configuration ->
					"user-defined configuration";

				listening ->
					"passive listening";

				teaching ->
					"teach-in"

			end,

			TeleCountStr = "; " ++ case TeleCount of

				0 ->
					"none of its telegrams could be decoded";

				1 ->
					"a single of its telegrams could be decoded";

				_ ->
					text_utils:format( "~B of its telegrams could be decoded",
									   [ TeleCount ] )

			end,

			ErrCountStr = case ErrCount of

				0 ->
					% At least usually, no decoding attempted, no possible
					% decoding error:
					%
					case TeleCount of

						0 ->
							"";

						_ ->
							", and no telegram decoding failed"
					end;

				1 ->
					", and a single telegram decoding failed";

				_ ->
					text_utils:format( ", and ~B decodings failed",
									   [ ErrCount ] )

			end,

			{ SeenCountStr, DiscovStr, TeleCountStr, ErrCountStr }

		end,

		text_utils:format( "~ts applying ~ts; it has been ~ts~ts~ts~ts",
			[ NameStr, EepDescStr, SeenStr, DiscStr, TeleStr, ErrStr ] ).



% CRC subsection.

% @doc Returns the CRC code corresponding to the specified binary.
-spec compute_crc( binary() ) -> crc().
compute_crc( Bin ) ->
	compute_crc( Bin, get_crc_array(), _Checksum=0 ).


% (helper)
compute_crc( _Bin= <<>>, _CRCArray, Checksum ) ->
	Checksum;

compute_crc( _Bin= <<HByte, T/binary>>, CRCArray, Checksum ) ->
	% Superfluous parentheses:
	Index = ( Checksum band 16#ff ) bxor ( HByte band 16#ff ),
	NewChecksum = element( Index + 1, CRCArray ),
	compute_crc( T, CRCArray, NewChecksum ).



% @doc Returns the array used to code/decode CRC.
-spec get_crc_array() -> type_utils:tuple( type_utils:uint8() ).
get_crc_array() ->

	% From https://gist.github.com/hypebeast/3833758:
	%
	% (9*28 + 4 = 256 values)
	%
	{ 16#00, 16#07, 16#0e, 16#09, 16#1c, 16#1b, 16#12, 16#15, 16#38,
	  16#3f, 16#36, 16#31, 16#24, 16#23, 16#2a, 16#2d, 16#70, 16#77,
	  16#7e, 16#79, 16#6c, 16#6b, 16#62, 16#65, 16#48, 16#4f, 16#46,
	  16#41, 16#54, 16#53, 16#5a, 16#5d, 16#e0, 16#e7, 16#ee, 16#e9,
	  16#fc, 16#fb, 16#f2, 16#f5, 16#d8, 16#df, 16#d6, 16#d1, 16#c4,
	  16#c3, 16#ca, 16#cd, 16#90, 16#97, 16#9e, 16#99, 16#8c, 16#8b,
	  16#82, 16#85, 16#a8, 16#af, 16#a6, 16#a1, 16#b4, 16#b3, 16#ba,
	  16#bd, 16#c7, 16#c0, 16#c9, 16#ce, 16#db, 16#dc, 16#d5, 16#d2,
	  16#ff, 16#f8, 16#f1, 16#f6, 16#e3, 16#e4, 16#ed, 16#ea, 16#b7,
	  16#b0, 16#b9, 16#be, 16#ab, 16#ac, 16#a5, 16#a2, 16#8f, 16#88,
	  16#81, 16#86, 16#93, 16#94, 16#9d, 16#9a, 16#27, 16#20, 16#29,
	  16#2e, 16#3b, 16#3c, 16#35, 16#32, 16#1f, 16#18, 16#11, 16#16,
	  16#03, 16#04, 16#0d, 16#0a, 16#57, 16#50, 16#59, 16#5e, 16#4b,
	  16#4c, 16#45, 16#42, 16#6f, 16#68, 16#61, 16#66, 16#73, 16#74,
	  16#7d, 16#7a, 16#89, 16#8e, 16#87, 16#80, 16#95, 16#92, 16#9b,
	  16#9c, 16#b1, 16#b6, 16#bf, 16#b8, 16#ad, 16#aa, 16#a3, 16#a4,
	  16#f9, 16#fe, 16#f7, 16#f0, 16#e5, 16#e2, 16#eb, 16#ec, 16#c1,
	  16#c6, 16#cf, 16#c8, 16#dd, 16#da, 16#d3, 16#d4, 16#69, 16#6e,
	  16#67, 16#60, 16#75, 16#72, 16#7b, 16#7c, 16#51, 16#56, 16#5f,
	  16#58, 16#4d, 16#4a, 16#43, 16#44, 16#19, 16#1e, 16#17, 16#10,
	  16#05, 16#02, 16#0b, 16#0c, 16#21, 16#26, 16#2f, 16#28, 16#3d,
	  16#3a, 16#33, 16#34, 16#4e, 16#49, 16#40, 16#47, 16#52, 16#55,
	  16#5c, 16#5b, 16#76, 16#71, 16#78, 16#7f, 16#6a, 16#6d, 16#64,
	  16#63, 16#3e, 16#39, 16#30, 16#37, 16#22, 16#25, 16#2c, 16#2b,
	  16#06, 16#01, 16#08, 16#0f, 16#1a, 16#1d, 16#14, 16#13, 16#ae,
	  16#a9, 16#a0, 16#a7, 16#b2, 16#b5, 16#bc, 16#bb, 16#96, 16#91,
	  16#98, 16#9f, 16#8a, 16#8d, 16#84, 16#83, 16#de, 16#d9, 16#d0,
	  16#d7, 16#c2, 16#c5, 16#cc, 16#cb, 16#e6, 16#e1, 16#e8, 16#ef,
	  16#fa, 16#fd, 16#f4, 16#f3 }.



% Section for execution as (e)script.
%
% See also oceanic_script_include.hrl .


% @doc Secures the usability of (our fork of) erlang-serial, typically from an
% (e)script.
%
-spec secure_serial( directory_path() ) -> void().
secure_serial( _OceanicRootDir ) ->

	SerialRootDir = file_utils:join( system_utils:get_software_base_directory(),
									 "erlang-serial" ),

	case file_utils:is_existing_directory_or_link( SerialRootDir ) of

		true ->
			SerialEbinDir = file_utils:join( SerialRootDir, "ebin" ),

			% Supposing it built then:
			code_utils:declare_beam_directory( SerialEbinDir );

		false ->
			% oceanic:secure_tty/1 will look-up the BEAM later:
			trace_utils:warning_fmt( "No user 'erlang-serial' installation "
				"found (searched for '~ts').", [ SerialRootDir ] )

	end.



% Section for the build-time generation of support modules.


% @doc To be called by the 'oceanic_generated.beam' automatic make target in
% order to generate, here, a (single) module to share the Oceanic constants.
%
-spec generate_support_modules() -> no_return().
generate_support_modules() ->

	TargetModName = oceanic_generated,

	%trace_bridge:info_fmt( "Generating module '~ts'...", [ TargetModName ] ),

	AllSpecNames = [ get_packet_type_topic_spec, get_return_code_topic_spec,
		get_event_code_topic_spec, get_rorg_topic_spec,
		get_rorg_description_topic_spec, get_common_command_topic_spec,
		get_vld_d2_00_cmd_topic_spec ],

	TopicSpecs = [ oceanic_constants:F() || F <- AllSpecNames ]
		++ oceanic_constants:get_eep_topic_specs(),

	_ModFilename =
		const_bijective_topics:generate_in_file( TargetModName, TopicSpecs ),

	%trace_bridge:info_fmt( "File '~ts' generated.", [ ModFilename ] ),

	erlang:halt().
