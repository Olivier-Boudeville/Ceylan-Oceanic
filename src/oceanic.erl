% Copyright (C) 2022-2025 Olivier Boudeville
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

-module(oceanic).

-moduledoc """
Main module of Ceylan-Oceanic, in order to **drive Enocean communications
through an Oceanic server**.
""".


%
% Export section.
%


% Base API:
-export([

	% Version-related functions.
	get_oceanic_version/0, get_oceanic_version_string/0,

	get_default_tty_path/0, has_tty/0, has_tty/1,

	is_available/1,

	start/0, start/1, start/2,
	start_link/0, start_link/1, start_link/2,

	get_server_registration_name/0, get_server_pid/0,

	load_configuration/1, load_configuration/2,
	add_configuration_settings/2,

	send/2,

	acknowledge_teach_request/2, acknowledge_teach_request/3,

	trigger_actuator/3, trigger_actuators/3,

	% For lower-level operation/testing:
	encode_esp3_packet/2, encode_esp3_packet/3,

	encode_double_rocker_switch_telegram/5,
	encode_double_rocker_multipress_telegram/4,

	% For any kind of (already-encoded) command:
	execute_command/2,

	% Useful to properly encode telegrams:
	get_oceanic_eurid/1,

	% General-purpose:
	get_device_description/2,
	get_button_ref_description/2, get_button_ref_descriptions/2,

	% For common commands:
	read_version/1, read_logs/1, read_base_id_info/1,

	decode_telegram/2,

	is_serial_available/1, restart_serial_interface/1,

	stop/0, stop/1, synchronous_stop/1,

	get_app_style_from_eep/1,

	interpret_button_ref_spec/1, interpret_button_ref_specs/1,

	button_designator_to_string/1, button_locator_to_string/1,

	eurid_to_string/1, eurid_to_short_string/1,
	eurid_to_bin_string/1, eurid_to_bin_string/2,
	string_to_eurid/1, get_broadcast_eurid/0,

	button_ref_to_string/1, button_refs_to_string/1, get_best_naming/2,
	get_device_table/1, string_to_eep/1 ] ).



% Event-related API, to achieve some kind of polymorphism on the corresponding
% records:
%
-export([ get_event_type/1, get_source_eurid/1, get_channel/1,
		  get_button_reference/1,

		  get_maybe_device_name/1, get_best_device_name_from/1,

		  get_maybe_eep/1,
		  get_timestamp/1, get_last_seen_info/1,
		  get_subtelegram_count/1, get_maybe_destination_eurid/1,
		  get_maybe_dbm/1, get_maybe_security_level/1, device_triggered/1 ]).


% Helper API for Oceanic user code:
-export([ canonicalise_listened_event_specs/1,
		  canon_listened_event_spec_to_string/1,
		  canon_listened_event_spec_to_string/2,
		  canon_listened_event_specs_to_string/1,
		  canon_listened_event_specs_to_string/2,

		  cits_to_string/1, device_state_change_spec_to_string/2,

		  canonicalise_emitted_event_specs/1,
		  canon_emitted_event_spec_to_string/1,
		  canon_emitted_event_spec_to_string/2,
		  canon_emitted_event_specs_to_string/1,
		  canon_emitted_event_specs_to_string/2,

		  get_reciprocal_state_change_spec/2,
		  event_matches_trigger/2,

		  actuator_info_to_string/1, actuator_info_to_string/2,

		  get_all_device_types/0, is_valid_device_type/1,

		  is_valid_application_style/1, is_valid_channel/1,
		  is_valid_button_position/1, is_valid_button_transition/1 ]).


% String-related functions:
-export([ telegram_to_string/1,
		  telegram_to_hexastring/1, hexastring_to_telegram/1 ]).



% For execution as (e)script:
-export([ secure_serial/1 ]).


% API for module generation:
-export([ generate_support_modules/0 ]).


% Exported only for testing:
-export([ get_test_state/0, get_test_state/1,
		  test_decode/1, secure_tty/1, try_integrate_next_telegram/4,

		  device_event_to_string/1, device_event_to_short_string/1,
		  state_to_string/1 ]).


% Silencing (depending on the tokens defined):
-export([ get_located_button_enum/2, get_designated_button_enum/2,
		  encode_common_command/2, interpret_power_failure/2,
		  ptm_module_to_string/1,
		  nu_message_type_to_string/1,
		  optional_data_to_string/1, optional_data_to_short_string/2,
		  maybe_optional_data_to_string/2, repeater_count_to_string/1 ]).




%
% Type section.
%


-doc """
The outcome of an availability check for Oceanic itself, as a whole (at least a
suitable gateway is needed).
""".
-type oceanic_availability_outcome() ::
	{ 'true', SerialRootDir :: directory_path() }
  | { 'false', Reason :: ustring(), basic_utils:error_term() }.



-doc "The PID of an Oceanic server.".
-type oceanic_server_pid() :: pid().



-doc """
The PID of a process in charge of a serial connection to the Enocean gateway
(USB dongle).
""".
-type serial_server_pid() :: pid().



-doc """
The PID of any process registered to an Oceanic server as a listener of Enocean
events.
""".
-type event_listener_pid() :: pid().



-doc """
The PID of the requester of a common command, or the 'internal' atom to tell
that this is a request emitted by Oceanic for its own economy.
""".
-type requester() :: pid() | 'internal'.



-doc "A user-defined device name, a lot more convenient than a EURID.".
-type device_name() :: bin_string().



-doc "A user-defined device name, a lot more convenient than a EURID.".
-type device_plain_name() :: ustring().



-doc "A user-defined device name, a lot more convenient than a EURID.".
-type device_any_name() :: any_string().



-doc """
An element designating a device, either thanks to an EURID (as an integer), or
thanks to a user-defined name (as any kind of string).
""".
-type device_designator() :: eurid() | device_any_name().



-doc """
A user-declared activity periodicity regarding a device (expected rhythm of
telegram sending).
""".
-type declared_device_activity_periodicity() ::

	dhms_duration()

  | 'none'    % Typically if no spontaneous state feedback is expected to be
			  % sent (e.g. most buttons/rockers).

  | 'default' % Supposes that periodical reports shall be expected and applies
			  % a reasonable default duration.

  | 'auto'.   % To be automatically determined by Oceanic; based on any known
			  % EEP, either no periodical state report will be expected, or one
			  % will be learnt through experience.



-doc "An expected periodicity of events.".
-type expected_periodicity() ::
	milliseconds() % Typically an upper bound
  | 'none'         % Typically if no state feedback or no monitoring wanted
  | 'auto'.        % To be managed by Oceanic



-doc """
Tells whether a given device is considered by Oceanic to be online or lost.
""".
-type availability_status() :: 'online' | 'lost'.


-doc """
Higher-level description of a status of a device (typically an actuator).

Useful to track trigger acknowledgements, or to specify an expected or wanted
device status.
""".
-type device_status() :: 'on' | 'off' % For example for a smart plug when driven
									  % by a rocker

					   | 'inverted' % For example for a smart plugs when driven
									% by a push button
					   | term().


-doc """
Tracking information supplied with a trigger request sent to an actuator.

For example ``{smart_plug_status_report_event, power_off}``.
""".
-type trigger_track_spec() :: { device_event_type(),
								option( reported_event_info() ) }.


-doc """
Server-internal information about a pending device-level request meant to be
acknowledged by the actuator of the specified EURID (hence being waited for,
with or without success), specifying the type of event to wait for
(e.g. smart_plug_status_report_event) together with any specific event
information expected (e.g. 'power_on'); the specified number of new attempts to
be performed if the current one times-out is also specified.

Allows to detect issues such an emitted yet not (correctly) received telegrams,
for example if wanting to switch on a smart plug and be sure that it succeeded.
""".
%
% A configurable time-out could be added as well.
-type trigger_track_info() :: { From :: eurid(),
								device_event_type(),
								option( reported_event_info() ),
								NextRetries :: count() }.


-doc """
Extra information to validate a type of device event for an acknowledgement.
""".
-type reported_event_info() :: 'power_on' | 'power_off'.



-doc """
Specification of Oceanic settings, as key/values pairs, as read from a
configuration file or transmitted by other services.
""".
-type oceanic_settings() :: list_table().


% For the device-related records:
-include("oceanic.hrl").


-doc "Information regarding an Enocean device, as known by the Oceanic server.".
-type enocean_device() :: #enocean_device{}.



-doc "A table recording information regarding Enocean devices.".
-type device_table() :: table( eurid(), enocean_device() ).



-doc "A (FIFO) queue of command requests, to be sent in turn next.".
-type command_queue() :: queue:queue( command_request() ).



-doc """
An entry in the Oceanic configuration (see its 'oceanic_devices' key) to
describe a given device of interest.
""".
-type device_config() ::

	% Then the activity periodicity will be learnt, if appropriate (e.g. contact
	% switches send transition events but generally no periodical events):
	%
	{ UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring() }

  | { UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring(), declared_device_activity_periodicity() }

  | { UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring(), declared_device_activity_periodicity(),
	  Comment :: ustring() }.



-doc "The outcome of an attempt of TTY detection.".
-type tty_detection_outcome() ::
	'true'
| { 'false', 'non_existing' | { 'not_device', entry_type() } }.



-doc """
Defines the serial, bidirectional communication between a host and EnOcean
modules.

There are two Enocean serial protocols: ESP2 and ESP3.

Oceanic focuses primarily on newer, richer, ESP3.

The physical interface between a host and an EnOcean RF module (UART) is a
3-wire connection (Rx, Tx, GND / software handshake / full-duplex), modelled on
RS-232 serial interfaces.
""".
-type serial_protocol() :: 'esp2' | 'esp3'.



-doc """
A telegram is a raw (non-decoded) series of bytes that have been received or are
to be sent. Ideally a telegram would correspond to a full, unitary radio ESP3
packet typically received from an Enocean gateway.

E.g. '<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>',
'<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>' or
'<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>'.
""".
-type telegram() :: binary().



-doc """
A telegram chunk is mere data taken from a telegram stream, with no assumption.

So it may contain nothing, or a part of a telegram, or even full ESP3 packets.

So mere data, with no assumption.
""".
-type telegram_chunk() :: binary().



-doc """
The beginning of a telegram once its start byte has already been chopped,
possibly followed by extra bytes (i.e. the start of any next telegrams).
""".
-type telegram_tail() :: telegram_chunk().


-doc """
A base, normalised, stable data of a telegram once its initial byte (typically
R-ORG or Return Code) has already been chopped, possibly followed by extra bytes
(i.e. the start of any next telegrams).
""".
-type telegram_data_tail() :: telegram_chunk().



-doc """
The part of a telegram corresponding to the base, normalised, stable data
(corresponding to the actual payload of an ESP3 packet, which can be for example
an ERP1 radio packet), a prefix possibly complemented with optional data.
""".
-type telegram_data() :: telegram_chunk().




-doc """
The (encoded) part of a telegram with the optional data that may
complement/extend the base data (see telegram_data()).

See also decoded_optional_data/0.
""".
-type telegram_opt_data() :: telegram_chunk().



-doc """
The decoded data for the optional part of the Packet Type 1 (RADIO_ERP1)
telegrams.

See also telegram_opt_data/0.
""".
-type decoded_optional_data() ::
	{ subtelegram_count(), eurid(),	option( dbm() ),
	  option( security_level() ) }.



-doc "A number of subtelegrams.".
-type subtelegram_count() :: count().



-doc """
The best RSSI value, expressed in decibels (dB) with reference to one milliwatt
(mW), of all received subtelegrams.

Here a negative value, like -6 dBm.

Of course only applies when receiving telegrams (not sending).
""".
-type dbm() :: integer().



-doc """
The level of security of a received telegram.

Only applies when receiving telegrams (when sending, security is selected by
link table entries).
""".
-type security_level() :: 'not_processed'
						| 'obsolete' % A deprecated security concept
						| 'decrypted'
						| 'authenticated'
						| 'decrypted_and_authenticated'.



-doc "A direction of communication.".
-type communication_direction() :: 'unidirectional' | 'bidirectional'.



-doc "A type of teach request.".
-type teach_request_type() :: 'teach_in' | 'teach_out'.



-doc "Outcome of a teach requests.".
-type teach_outcome() ::
	'teach_refused'          % "General reason"
  | 'teach_in_accepted'      % Addition success
  | 'teach_out_accepted'     % Deletion success
  | 'teach_eep_unsupported'. % EEP not supported



-doc """
Describes a channel (e.g. for a double rocker having channel A as first row, and
channel B as second row).

Instead of a letter, channels are designated here with an integer, so 1
corresponds to channel A, 2 to channel B, etc.

A single rocker uses only a channel, A.
""".
-type channel() :: pos_integer(). % rather than uint8().


-doc "Tells which channel(s) should be taught.".
-type channel_taught() :: channel() | 'all'.



-doc """
Specification of the identifier of a button, from the EURID of its device
(e.g. a double rocker) as a string, and its own channel (e.g. 2 to designate any
"button B").
""".
-type button_ref_spec() :: { eurid_string(), channel() }.


-doc """
Identifier of a button, from the EURID of its device (e.g. a double rocker) and
its own channel (e.g. 2 to designate any "button B").
""".
-type button_ref() :: { eurid(), channel() }.



-doc "Identifier of device manufacturer.".
-type manufacturer_id () :: uint8().



% Since EEP 3.0:


-doc """
Radio ORG (organization number / Radio-telegram types grouped ORGanizationally);
describes the ERP radio telegram type, as an identifier. Equivalent of "Choice".
""".
-type rorg() :: uint8().



-doc "Describes the basic functionality of the data content.".
-type func() :: uint8().



-doc "Describes the type of device in its individual characteristics.".
-type type() :: uint8().



-doc """
An EEP defines the coding of the data to be exchanged, so that two devices
complying to the same EEP can be interchanged.
""".
-type eep() :: { rorg(), func(), type() }.



-doc """
The (atom) identifier of an EnOcean Equipment Profile, corresponding to
(R-ORG)-(FUNC)-(TYPE) triplet.

For example the 'single_input_contact' EEP identifier corresponds to EEP
D5-00-01.

Refer to get_eep_topic_specs/0 for further details.
""".
-type eep_id() ::
	'thermo_hygro_low'
  | 'thermo_hygro_mid'
  | 'thermo_hygro_high'
  | 'push_button'

  % They include the simple rocker ones:
  | 'double_rocker_switch_style_1'
  | 'double_rocker_switch_style_2'
  | 'double_rocker_multipress' % Apparently can be an alternative to declare
							   % that a specific button was released.

  | 'single_input_contact'   % Typically opening detectors
  | 'single_channel_module'
  | 'double_channel_module'
  | atom().



-doc """
An EEP defined as a string (e.g. "D5-00-01").
""".
-type eep_string() :: ustring().



-doc """
Tells how a device was discovered (first seen) by Oceanic.

Discovery shall be understood here as (first-time) detection (even if the device
was already known through configuration).
""".
-type discovery_origin() ::
	'configuration'  % Loaded from Oceanic's user-defined configuration
  | 'listening'      % Passively listened from trafic
  | 'teaching'.      % Through a teach-in mechanism



-doc "An enumeration specified in the protocol.".
-type enum() :: integer().


-doc """
Designates an application style (generally 1 or 2, for variations A or B).
""".
-type application_style() :: count().



-doc """
Designates a button corresponding to a A or B channel.

A given rocker behaves as two buttons (e.g. AI/AO).

In application style 1, the O position is the top/up one, while the I position
is the bottom/down one, whereas the opposite holds for application style 2.

We prefer designating button based on their channel and position.
""".
-type button_designator() ::
				 % Comments apply to application style 1:
	'button_ai'  % Switch light on  / Dim light down / Move blind closed
  | 'button_ao'  % Switch light off / Dim light up   / Move blind open
  | 'button_bi'  % Switch light on  / Dim light down / Move blind closed
  | 'button_bo'. % Switch light off / Dim light up   / Move blind open


-doc """
Instead of relying on the relatively unclear AI/AO/BI/BO naming that depends on
EEP/application style, one may just reason on the channel of this button (e.g. A
or B, translated as 1 or 2) and, here, on its actual position on the device (top
or bottom).

Then, depending on the application style/EEP of the device, this may be
translated in terms of AI, AO, etc.
""".
-type button_position() :: 'top' | 'bottom'.


-doc """
Our best way of identifying a button.

For example: {2, bottom}.
""".
-type button_locator() :: { channel(), button_position() }.


-doc "Tells whether a button has been pressed (and held) or released.".
-type button_transition() :: 'pressed' | 'released'.



-doc """
A "number" of buttons, typically involved in a multipress event.
""".
-type button_counting() :: 'none' | 'three_or_four'.



-doc "Tells whether a contact is open or closed.".
-type contact_status() :: 'open' | 'closed'.



-doc """
The types of PTM switch modules (radio emitter), as defined in RPS packets.
""".
-type ptm_switch_module_type() :: 'ptm1xx'  % synonymous for module PTM1xx
								| 'ptm2xx'. % synonymous for module PTM2xx



-doc """
"Nu" Message type, as defined in RPS packets.
""".
-type nu_message_type() :: 'normal' | 'unassigned'
						 | 'unknown_type_2' | 'unknown_type_3'.



-doc "A number of repetitions, typically in a RPS packet.".
-type repetition_count() :: count().



-doc "The range of a temperature sensor.".
-type temperature_range() :: 'low'   % 0°C to +40°C (A5-04-01)
						   | 'high'. % -20°C to +60°C (A5-04-02)


-doc "A reported hardware status for a device (e.g. smart plug).".
-type hardware_status() :: 'nominal'
						 | 'warning'
						 | 'failure'
						 | 'not_supported'. % Hence unknown


-doc "The power currently output by a device (e.g. a smart plug).".
-type power_report() :: 'off'
					  | pos_integer() % Percentage of max power
					  | 'not_available'. % Hence unknown


-doc "The powering status of a device (e.g. a smart plug).".
-type power_status() :: 'power_on' | 'power_off'.



-doc """
The type of a VLD message, in the context of the D2-00 EEPs: "Room Control Panel
(RCP)".

It is also designated by the MI field of these VLD telegrams, the 3 last bits of
the first byte of the payload (hence 8 possible values).

Described in [EEP-spec] p.127.
""".
-type vld_rcp_message_type() ::
	'a'  % ID 01
  | 'b'  % ID 02
  | 'c'  % ID 03
  | 'd'  % ID 04
  | 'e'  % ID 05
  | 'f'  % ID 06 (non-existing)
  | 'g'  % ID 07 (non-existing)
  | 'h'. % ID 08 (non-existing)



-doc """
The type of a VLD message, in the context of the D2-01 EEPs: "Electronic
switches and dimmers with Energy Measurement and Local Control".

Refer to the 'vld_d2_00_cmd' topic.

It is also designated by the CMD field of these VLD telegrams, the 4 last bits
of the first byte of the payload (hence 16 possible values).

Described in [EEP-spec] p.131.
""".
-type vld_d2_00_cmd() ::
	'actuator_set_output'
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



-doc "The various kinds of errors that may happen when decoding.".
-type decoding_error() ::
	'not_reached'   % Beginning of next packet still ahead
  | 'incomplete'    % Truncated packet (end of packet still ahead)
  | 'invalid'       % Corrupted packet
  | 'unsupported'   % Type of packet (currently) unsupported by Oceanic
  | 'unconfigured'. % Device not configure (typically EEP not known)



-doc "The outcome of a decoding.".
-type decoding_outcome() ::

	{ 'decoded', option( device_event() | 'command_processed' ),
	  MaybeDiscoverOrigin :: option( discovery_origin() ),
	  IsBackOnline :: boolean(), MaybeDevice :: option( enocean_device() ),
	  NextMaybeTelTail :: option( telegram_tail() ), oceanic_state() }

  | { decoding_error(), ToSkipLen :: count(),
	  NextMaybeTelTail :: option( telegram_tail() ), oceanic_state() }.



% The outcome of an attempt of integrating / decoding a telegram chunk.
%
% A maybe-event is returned, as for example a common command response that is
% received whereas no request was sent shall be discarded.
%
% A maybe-discovery origin is also returned, so that an already discovered
% device is not reported as being discovered more than once ('undefined' is
% returned once already discovered).



-doc "The result of a decoding request.".
-type decoding_result() :: decoding_error() | device_event().



-doc """
EURID (EnOcean Unique Radio Identifier) is a unique and non-changeable
identification number (as a 32-bit value) assigned to every EnOcean transmitter
during its production process.

The EURID corresponds to the hexadecimal identifier typically labelled at the
back of devices (e.g. "ID: B50533EC").

Our EURIDs are defined and stored in uppercase, as they are generally written on
devices.

A specific EURID is 0xff-ff-ff-ff (see the eurid_broadcast define), which
denotes a broadcast transmission (as opposed to an Addressed Transmission, ADT).
""".
-type eurid() :: type_utils:uint32(). % Previously was a 32-bit binary.



-doc """
An EURID, expressed as a string.

For example: "B50533EC".
""".
-type eurid_string() :: ustring().



-doc """
An EURID, expressed as a binary string.

For example: `<<"B50533EC">>`.
""".
-type eurid_bin_string() :: bin_string().



-doc "An ESP-level data unit.".
-type packet() :: binary().



-doc """
The CRC (Cyclic Redundancy Check) or polynomial code checksum of a
sub-telegram/packet can be computed.

Remainder on 8 bits of the modulo 2 division of the G(x) = x^8 + x^2 + x^1 + x^0
polynom.
""".
-type crc() :: byte().



-doc """
An ESP3 data unit, consisting of Header, Data and Optional Data.

The semantics of the optional data is defined by the packet type, it can be used
to extend an existing ESP3 packet.
""".
-type esp3_packet() :: binary().



-doc """
The type of a (typically ESP3) packet.

Refer to [ESP3] p.12.

After a radio_erp1, radio_sub_tel or remote_man_command packet, a response
packet is expected.

See also the 'packet_type' topic in the oceanic_generated module.
""".
-type packet_type() ::
	  'reserved' | 'radio_erp1' | 'response'
	| 'radio_sub_tel' | 'event' | 'common_command'
	| 'smart_ack_command' | 'remote_man_command'
	| 'radio_message' | 'radio_erp2' | 'radio_802_15_4'
	| 'command_2_4'.



-doc """
The payload of a (typically ESP3) packet, a sequence of bytes sometimes
designated as 'DataTail', that is all bytes in the "data" chunk (as opposed to
the "optional data" one) found after the R-ORG one.

Such a payload corresponds to a packet of a given type (e.g. an ERP1 radio
packet, encapsulated in an ESP3 packet).
""".
-type payload() :: binary().



-doc """
The actual payload of a VLD (D2) packet.

VLD telegrams carry a variable payload between 1 and 14 bytes, depending on
their design.
""".
-type vld_payload() :: binary().




-doc """
The version of an EnOcean application or API.

This is a basic_utils:four_digit_version().
""".
-type enocean_version() :: { Main :: version_number(), Beta :: version_number(),
	Alpha :: version_number(), Build :: version_number() }.



-doc "A log counter, starting from 255 downward.".
-type log_counter() :: type_utils:uint8().



-doc "A series of log entries of an USB gateway.".
-type log_counters() :: [ log_counter() ].



-doc "Type information regarding a command.".
-type command_type() :: 'device_command' | common_command().



-doc "Allows to keep track of an ongoing command request.".
-type command_request() :: #command_request{}.



-doc "The (synchronous) outcome of a sent command.".
-type command_outcome() :: command_response() | 'time_out'.



-doc """
Tracking information regarding a currently pending command.

A timer is used for most commands, except typically internally-triggered common
commands.
""".
-type waited_command_info() :: { command_request(), option( timer_ref() ) }.



-doc "The description of a device, as known and returned by Oceanic.".
-type device_description() :: bin_string().


-doc """
The description of at least one button reference, as known and returned by
Oceanic.
""".
-type button_ref_description() :: bin_string().


-doc """
Information sent to notify (if not set to 'undefined') that a lost device is
back online.
""".
-type back_online_info() :: option( device_description() ).





% Event types ordered here as well by increasing EEP:


-doc """
Event sent by EEP A5-04-01: "Temperature and Humidity Sensor" (with any range).

Refer to [EEP-spec] p.35 for further details.
""".
-type thermo_hygro_event() :: #thermo_hygro_event{}.



-doc """
Event sent by EEP D5-00-01: Single Input Contact.

D5-00 corresponds to Contacts and Switches.

Refer to [EEP-spec] p.27 for further details.

Note that, at least by default, most if not all opening detectors not only
report state transitions (between closed and opened), they also notify regularly
(e.g. every 5-30 minutes, on average often 15 minutes) and spontaneously their
current state (even if no specific transition happened), presumably to help
overcoming any message loss.

So any listener of these events shall store their current state, to be able to
detect the actual transitions (even if they are late).
""".
-type single_input_contact_event() :: #single_input_contact_event{}.



-doc """
Event sent in the context of EEP F6-01 ("Switch Buttons (with no rockers)").

Refer to [EEP-spec] p.15 for further details.
""".
-type push_button_switch_event() :: #push_button_switch_event{}.



-doc """
Event sent in the context of EEPs D2-01-* (e.g 0A), corresponding to an Actuator
Status Response (command 0x4), so that a smart plug reports its current state.

Refer to [EEP-spec] p.135 for further details.
""".
-type smart_plug_status_report_event() :: #smart_plug_status_report_event{}.



%-doc "Event regarding rocker switches.".
%-type rocker_switch_event() :: #rocker_switch_event{}.



-doc """
Event sent in the context of EEP F6-02-01 and F6-02-02 ("Light and Blind Control
- Application Style 1 or 2"), for T21=1.

Refer to [EEP-spec] p.16 for further details.
""".
-type double_rocker_switch_event() :: #double_rocker_switch_event{}.



-doc """
Event sent in the context of EEP F6-02-01 and F6-02-02 ("Light and Blind Control
- Application Style 1 or 2"), for T21=1 and NU=0.

Refer to [EEP-spec] p.16 for further details.
""".
-type double_rocker_multipress_event() :: #double_rocker_multipress_event{}.



%-doc "Event regarding position switches.".
%-type position_switch_event() :: #position_switch_event{}.



-doc """
Message (hence not an event per se) corresponding to the receiving a R-ORG
telegram for an universal Teach-in request, EEP based (UTE), one way of pairing
devices.

Refer to [EEP-gen] p.17 for further details.
""".
-type teach_request() :: #teach_request{}.



-doc "Response to a successful 'read version' common command request.".
-type read_version_response() :: #read_version_response{}.



-doc "Response to a successful 'read logs' common command request.".
-type read_logs_response() :: #read_logs_response{}.



-doc """
Response to a successful 'read base ID information' (CO_RD_IDBASE) common
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



-doc "Any event notified by an EnOcean device.".
-type device_event() ::
	% Device events:
	thermo_hygro_event()
  | single_input_contact_event()
  | push_button_switch_event()
  | smart_plug_status_report_event()
  | double_rocker_switch_event()
  | double_rocker_multipress_event()

	% Other events:
  | teach_request()
  | command_response().


-doc "Lists the known types of device events.".
% Corresponds to the tags of the corresponding records.
-type device_event_type() ::
	'thermo_hygro_event'
  | 'single_input_contact_event'
  | 'push_button_switch_event'
  | 'smart_plug_status_report_event'
  | 'double_rocker_switch_event'
  | 'double_rocker_multipress_event'

	% Other events:
  | 'teach_request'
  | 'command_response'.


-doc """
Lists the known types of devices.

Each type of device is to send at least one type of events.
""".
-type device_type() ::
	'thermo_hygro_sensor'
  | 'single_contact' % Typically opening detectors
  | 'push_button'
  | 'smart_plug'
  | 'double_rocker'.



-doc """
Any kind of information relative to a device.

For example the application style of a rocker.
""".
-type device_info() :: application_style() | term().


-doc """
Information regarding a device (e.g. a double rocker) that is emulated in order
to forget telegrams that it could send, typically in order to trigger actuators.
""".
-type virtual_emitter_info() :: option( tuploid( device_info() ) ).



-doc """
Describes an elementary information about the state change of a device.

For example may correspond to Channel(), for a double-rocker change information.
""".
-type state_change_info() :: term().




-doc "Describes a stage change of a double-rocker.".
-type double_rocker_state_change_spec() ::
	canon_double_rocker_state_change_spec()
  | { channel(), button_position() } % then transition is 'pressed'
  | channel(). % then position is 'top' and transition is 'pressed'


-doc """
Canonical version of double_rocker_state_change_spec().

For example, ``{2, bottom, released}``.

Both a user-level and an internal type.
""".
-type canon_double_rocker_state_change_spec() ::
	{ channel(), button_position(), button_transition() }.


-doc "Describes a stage change of a push-button.".
-type push_button_state_change_spec() :: canon_push_button_state_change_spec().


-doc """
Canonical version of push_button_state_change_spec().
""".
-type canon_push_button_state_change_spec() :: button_transition().



-doc """
Describes a stage change of a device.

Used both for incoming and outgoing telegrams.

Abbreviated as SCS.
""".
-type device_state_change_spec() ::
	double_rocker_state_change_spec()
  |	push_button_state_change_spec()
  | tuploid( state_change_info() ).


-doc """
Canonical, internal version of device_state_change_spec().

Abbreviated as CSCS.
""".
-type canon_device_state_change_spec() ::
	canon_double_rocker_state_change_spec()
  |	canon_push_button_state_change_spec()
  | tuploid( state_change_info() ).


% For incoming, listened events:

-doc """
Describes how the server can be triggered by a device, by specifying its type
and the event that it may have sent - which will lead the trigger to be
validated.

Notably used to decode incoming trigger events.

For example, ``{double_rocker, {2, bottom, released}}.``.
""".
-type incoming_trigger_spec() ::

	{ 'double_rocker', double_rocker_state_change_spec() }

  |	{ 'push_button', push_button_state_change_spec() }

	% General form:
  | { device_type(), device_state_change_spec() }.



-doc """
A canonical incoming trigger specification.

Canonicalised, internal version of incoming_trigger_spec().

Abbreviated as CITS.
""".
-type canon_incoming_trigger_spec() ::
	{ device_type(), canon_device_state_change_spec() }.


-doc """
Describes events that may be listened to by this server, either emitted by a
device specified by its EURID, or broadcast.

Typically useful to react on the triggering events (e.g. of a presence switch,
and alarm, etc.) of interest.

For example ``{"002b6b24", {double_rocker, 1}}``, which describes that any
incoming telegram corresponding to a state change of the channel A (i.e. 1) of a
double-rocker of EURID 002b6b24 shall be interpreted as a trigger (typically if
the rocker is pressed).

User-level type.
""".
-type listened_event_spec() ::
	{ EmitterDevice :: option( eurid_string() ), incoming_trigger_spec() }.



-doc """
Canonicalised, internal version of listened_event_spec().

For example: ``{"25af97a0", {double_rocker, {2, bottom, released}}}``.

Abbreviated as CLES.
""".
-type canon_listened_event_spec() ::
	{ EmitterDevice :: eurid(), canon_incoming_trigger_spec() }.


-doc """
The outcome of the match of a trigger event, possibly telling which is the
triggering device and the new device status that shall be set.
""".
-type event_match_trigger_outcome() ::
	'false'
  | { 'true', SourceDevice :: eurid(), device_status() }.



% For outgoing, emitted events:


-doc """
Describes how the server can trigger an actuator device: describes a
corresponding virtual emitting device, notably in order to encode outgoing
trigger events.

For example: ``{double_rocker, 1, {2, top, pressed}}`` means that the virtual
emitting device will be a double-rocker of application style 1 whose channel B
(2; the second rocker) has its top button pressed.

Both a user-level and an internal type.
""".
% A difference with the incoming ones is that information regarding the emulated
% source device (e.g. the application style of such a rocker) may be specified:
%
-type outgoing_trigger_spec() ::

	{ 'double_rocker', application_style(), double_rocker_state_change_spec() }
  | { 'double_rocker', double_rocker_state_change_spec() }


	% General forms:
  | { device_type(), virtual_emitter_info(), device_state_change_spec() }

  | { device_type(), device_state_change_spec() }.


-doc """
Canonical, internal version of outgoing_trigger_spec().

Abbreviated as COTS.
""".
-type canon_outgoing_trigger_spec() ::
	{ device_type(), virtual_emitter_info(), canon_device_state_change_spec() }.



-doc """
User-level description of an actuator, typically when emitting an event
targeting it.

Specifies the EURID (possibly a broadcast address) to be used in order to reach
this actuator, and possibly the device type it will be addressed at, i.e. as
which type of device (e.g. a smart plug) the actuator is expected to react.

For example: ``{"25af97a0", smart_plug}``.

This is typically needed if, once an actuator trigger telegram has been emitted,
the sender is to register a corresponding waited acknowledgement from that
actuator: thanks to these information, incoming telegrams can be matched and
translated into such acknowledgements.

Indeed the target actuator may not be configured (or possibly may implement
multiple EEPs), whereas detecting acknowledgements requires at least an expected
EEP and/or a device type (as the incoming telegrams shall be filtered to match
them with any pending, yet-to-acknowledge, triggered actuator).

For example, if a smart plug is triggered, in order to be sure that the
operation completed (e.g. that the plug has been switched on), a corresponding
VLD status response telegram shall be waited for. So incoming telegrams shall be
inspected in order to detect such a corresponding acknowledgement telegram -
based on the corresponding actuator information.
""".
-type user_actuator_info() :: canon_user_actuator_info()
						  % No device type specified here:
						| TargetDevice :: option( eurid_string() ).


-doc "Canonical, yet still user-level.".
-type canon_user_actuator_info() ::
		{ TargetDevice :: option( eurid_string() ),
		  TargetAddressedDeviceType :: option( device_type() ) }.


-doc """
Describes an actuator, typically when emitting an event targeting it.

EURID may be a broadcast one.

Internal type.
""".
-type actuator_info() :: { eurid(), option( device_type() ) }.



-doc """
User-level description of events that may be emitted by this server.

Stores information about a "virtual" emitting device (e.g., if emulating a
rocker, its application style) to be impersonated by this server and about an
intended use thereof (e.g. which button is operated), and about the intended
target action.

For example designates which rocker/channel of which kind of double-rocker would
be used in order to generate and send a telegram, generally aimed at an actuator
(e.g. a smart plug) to control.

No (source) EURID is specified, as anyway the one of the base gateway is the one
that shall be used.

For example ``{{double_rocker,1}, {"05936ef8", smart_plug}}``, which designates
a telegram to be sent as a double-rocker (of application style 1, and with other
defaults) in order to trigger an actuator whose EURID is 05936ef8, and which is
expected to act as a smart plug (and thus is to send back a corresponding
acknowledgement telegram).
""".
-type emitted_event_spec() :: { outgoing_trigger_spec(), user_actuator_info() }.


-doc """
Canonicalised, internal version of emitted_event_spec().
""".
-type canon_emitted_event_spec() ::
	% Internal actuator info, not user one:
	{ canon_outgoing_trigger_spec(), actuator_info() }.




-doc """
Designates an ESP3 command, like co_wr_sleep or co_rd_repeater.

Refer to oceanic_generated:get_common_command_topic_spec/0 for further
information.
""".
-type common_command() :: 'co_rd_version' | 'co_rd_sys_log'
						| 'co_rd_idbase' | atom().



-doc """
Generic causes of failure for a common command request.

See also oceanic_generated:get_return_code_topic_spec/0.
""".
-type common_command_failure() :: 'error_return'
								| 'not_supported_return'
								| 'wrong_parameter_return'
								| 'operation_denied'
								| 'time_out'.


-doc "A timer reference.".
-type timer_ref() :: timer:tref().


-export_type([ oceanic_availability_outcome/0,

			   oceanic_server_pid/0, serial_server_pid/0, event_listener_pid/0,
			   requester/0,

			   device_name/0, device_plain_name/0, device_any_name/0,
			   device_designator/0, declared_device_activity_periodicity/0,
			   expected_periodicity/0, availability_status/0, device_status/0,

			   trigger_track_spec/0, trigger_track_info/0,
			   reported_event_info/0,

			   oceanic_settings/0,
			   enocean_device/0, device_table/0,
			   device_config/0, tty_detection_outcome/0, serial_protocol/0,

			   telegram/0, telegram_chunk/0,
			   telegram_data/0, telegram_tail/0, telegram_data_tail/0,
			   telegram_opt_data/0, decoded_optional_data/0,

			   subtelegram_count/0, dbm/0, security_level/0,
			   communication_direction/0, teach_request_type/0, teach_outcome/0,
			   channel/0, channel_taught/0,
			   button_ref_spec/0, button_ref/0,
			   manufacturer_id/0,

			   rorg/0, func/0, type/0, eep/0, eep_id/0, eep_string/0,
			   discovery_origin/0, enum/0,

			   application_style/0, button_designator/0,
			   button_position/0, button_locator/0,
			   button_transition/0, button_counting/0,

			   contact_status/0,
			   ptm_switch_module_type/0, nu_message_type/0, repetition_count/0,
			   temperature_range/0,

			   hardware_status/0, power_report/0, power_status/0,

			   vld_rcp_message_type/0, vld_d2_00_cmd/0,
			   decoding_outcome/0,

			   eurid/0, eurid_string/0, eurid_bin_string/0,
			   packet/0, crc/0, esp3_packet/0, packet_type/0,
			   payload/0, vld_payload/0,
			   enocean_version/0, log_counter/0, log_counters/0,
			   command_type/0, command_request/0, command_outcome/0,

			   thermo_hygro_event/0, single_input_contact_event/0,
			   push_button_switch_event/0,
			   double_rocker_switch_event/0, double_rocker_multipress_event/0,

			   device_description/0, back_online_info/0, device_event/0,
			   device_event_type/0,

			   device_type/0, device_info/0, virtual_emitter_info/0,

			   state_change_info/0,
			   device_state_change_spec/0, canon_device_state_change_spec/0,

			   push_button_state_change_spec/0,
			   canon_push_button_state_change_spec/0,

			   double_rocker_state_change_spec/0,
			   canon_double_rocker_state_change_spec/0,

			   incoming_trigger_spec/0, canon_incoming_trigger_spec/0,
			   listened_event_spec/0, canon_listened_event_spec/0,
			   outgoing_trigger_spec/0, canon_outgoing_trigger_spec/0,
			   emitted_event_spec/0, event_match_trigger_outcome/0,

			   common_command/0, common_command_failure/0,
			   timer_ref/0,

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


% Section about Command/request acknowledgements.
%
% A previous system had been start, based on command_queue(), and another one
% was also started in a user, higher-level library (namely US-Main). We have yet
% to determine whether tracking acknowledgements is of use, depending on the
% level of telegram losses.
%
% If such a system is needed, most probably that the command_queue() shall be
% replaced with some (global, or per-actuator) tracking information (see
% trigger_track_{spec,info,...}) where request time-outs would be scheduled, and
% incoming telegrams (e.g. VLD ones, for smart plugs) would be decoded and would
% unschedule the corresponding time-out. Having a time-out be triggered would
% then result in the re-emission of the corresponding telegram, provided that
% the maximum number of retries is not reached.




% Local types:

-doc "32-bit; could have been `type_utils:uint32()`.".
-type esp3_header() :: <<_:32>>.



-doc "Information about a recording of a device event.".
-type recording_info() ::
	{ device_table(), NewDevice :: enocean_device(), Now :: timestamp(),
	  MaybeLastSeen :: option( timestamp() ), option( discovery_origin() ),
	  IsBackOnline :: boolean(), option( device_name() ), option( eep_id() ) }.




% Internal defines.


% Each telegram must start with:
-define( sync_byte, 85 ). % i.e. 0x55


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


% The minimum timeout (in milliseconds) regarding the monitoring of device
% activity, as a security to avoid too frequent checking:
%
-define( min_activity_timeout, 5000 ).

% The default threshold, in bytes per second (hence, considering an usual
% telegram size of 21 bytes, roughly a dozen legit telegrams per second) above
% which an onEnoceanJamming event should be triggered:
%
-define( default_jamming_threshold, 250 ).

% To test detection:
%-define( default_jamming_threshold, 10 ).


% The default number of retries until the triggering of an actuator is
% acknowledged:
%
-define( default_trigger_retry_count, 4 ).


% The default DHMS expected activity periodicity for a device (hence a telegram
% is expected here to be received on average every 25 minutes):
%
-define( default_dhms_periodicity, { 0, 0, 25, 0 } ).


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

	% The (binary) path to the Enocean gateway (USB dongle), kept so that the
	% serial link can be reset if needed:
	%
	device_path :: bin_device_path(),

	% To identify the pseudo-device emitter of any telegram to be sent by
	% Oceanic; by default this will be the actual base ID advertised by the
	% local USB gateway, as obtained thanks to the co_rd_idbase common command
	% (otherwise telegrams are likely not to be processed by the sender or
	% ignored by the receiver).
	%
	emitter_eurid = string_to_eurid( ?default_emitter_eurid ) :: eurid(),

	% A table recording all information regarding the known Enocean devices:
	device_table :: device_table(),


	% We enqueue command requests that shall result in an acknowledgement (most
	% of them; possibly all of them), as such acks, at least generally, just
	% contain a corresponding return code - nothing else that could be
	% associated to a sender, a request, etc.; we used to ensure that at any
	% time up to one of such commands was in the air, and stored in this queue
	% the next ones for a later sending thereof in turn.
	%
	% We could see for example that sending an ERP1 packet for the F6-02-01 EEP
	% does result in the receiving of a response packet (with a success return
	% code).
	%
	% So this queue contains any pending, not-yet-sent ESP3 commands (be
	% them requests for ERP1 commands, common commands, etc.):
	%
	command_queue :: command_queue(),


	% Information about any currently waited command request that shall result
	% in an acknowledgement; so corresponds to any pending, sent but not yet
	% acknowledged ESP3 command whose response telegram is still waited for.
	% Note that some devices apparently may be configured to not ack incoming
	% commands (however [ESP3] p.17 tells that "it is mandatory to wait for the
	% RESPONSE message"); in this case this information should be registered in
	% their enocean_device() record.
	%
	waited_command_info :: option( waited_command_info() ),


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

	% The timestamp corresponding the last time incoming traffic was detected,
	% to determine jamming level:
	%
	last_traffic_seen :: timestamp(),


	% The threshold above which an onEnoceanJamming event is triggered:
	jamming_threshold = ?default_jamming_threshold :: bytes_per_second(),


	% The number of retries until the triggering of an actuator is acknowledged:
	trigger_retry_count = ?default_trigger_retry_count :: count(),

	% A list of the PID of any processes listening for Enocean events:
	event_listeners = [] :: [ event_listener_pid() ] } ).


-doc """
An Oceanic state, including configuration, typically loaded from an ETF file.
""".
-type oceanic_state() :: #oceanic_state{}.



% Type shorthands:

-type count() :: basic_utils:count().
-type version_number() :: basic_utils:version_number().
-type three_digit_version() :: basic_utils:three_digit_version().

-type any_file_path() :: file_utils:any_file_path().
-type directory_path() :: file_utils:directory_path().
-type any_directory_path() :: file_utils:any_directory_path().
-type device_path() :: file_utils:device_path().
-type bin_device_path() :: file_utils:bin_device_path().
-type any_device_path() :: file_utils:any_device_path().
-type entry_type() :: file_utils:entry_type().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type byte_size() :: system_utils:byte_size().
-type bytes_per_second() :: system_utils:bytes_per_second().

-type uint8() :: type_utils:uint8().
-type tuploid(T) :: type_utils:tuploid(T).

-type timestamp() :: time_utils:timestamp().
-type dhms_duration() :: time_utils:dhms_duration().
%-type seconds() :: time_utils:seconds().


-type percent() :: math_utils:percent().

-type registration_name() :: naming_utils:registration_name().

-type milliseconds() :: unit_utils:milliseconds().
-type celsius() :: unit_utils:celsius().

-type list_table() :: list_table:list_table().


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



% Version-related functions.


-doc "Returns the version of the Oceanic library being used.".
-spec get_oceanic_version() -> three_digit_version().
get_oceanic_version() ->
	basic_utils:parse_version( get_oceanic_version_string() ).



-doc "Returns the version of the Oceanic library being used, as a string.".
-spec get_oceanic_version_string() -> ustring().
get_oceanic_version_string() ->
	% As defined (uniquely) in GNUmakevars.inc:
	?oceanic_version.



-doc """
Tells whether Oceanic should be available, that is if all its prerequisites seem
to be met.

Useful to de-risk a future launch thereof and factor code.
""".
-spec is_available( device_path() ) -> oceanic_availability_outcome().
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


-doc """
Starts the Enocean support, based on our default conventions regarding the TTY
allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
server, which registered the calling process as one of its Enocean event
listeners (but did not link to it).

Throws an exception if no relevant TTY can be used.
""".
-spec start() -> oceanic_server_pid().
start() ->
	start( get_default_tty_path() ).



-doc """
Starts the Enocean support, based on the specified device path to the TTY
allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
server, which registered the calling process as one of its Enocean event
listeners (but did not link to it).

Throws an exception if no relevant TTY can be used.
""".
-spec start( device_path() ) -> oceanic_server_pid().
start( TtyPath ) ->
	start( TtyPath, [ _EventListenerPid=self() ] ).



-doc """
Starts the Enocean support, based on the specified path to the TTY allocated to
the USB Enocean gateway; returns the PID of the launched Oceanic server (which
is not linked to the calling process), registering the specified listener
processes for Enocean events.

Throws an exception if no relevant TTY can be used.
""".
-spec start( device_path(), [ event_listener_pid() ] ) -> oceanic_server_pid().
start( TtyPath, EventListeners ) ->
	?myriad_spawn( fun() -> oceanic_start( TtyPath, EventListeners ) end ).




% Start subsection, with link.


-doc """
Starts the Enocean support, based on our default conventions regarding the TTY
allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
server, which registered the calling process as one of its Enocean event
listeners, and linked to it.

Throws an exception if no relevant TTY can be used.
""".
-spec start_link() -> oceanic_server_pid().
start_link() ->
	start_link( get_default_tty_path() ).



-doc """
Starts the Enocean support, based on the specified device path to the TTY
allocated to the USB Enocean gateway; returns the PID of the launched Oceanic
server, which registered the calling process as one of its Enocean event
listeners, and linked to it.

Throws an exception if no relevant TTY can be used.
""".
-spec start_link( device_path() ) -> oceanic_server_pid().
start_link( TtyPath ) ->
	start_link( TtyPath, [ _EventListenerPid=self() ] ).



-doc """
Starts the Enocean support, based on the specified path to the TTY allocated to
the USB Enocean gateway; returns the PID of the launched Oceanic server, which
registered the calling process as one of its Enocean event listeners, and linked
to it.

Throws an exception if no relevant TTY can be used.
""".
-spec start_link( device_path(), [ event_listener_pid() ] ) ->
											oceanic_server_pid().
start_link( TtyPath, EventListeners ) ->
	?myriad_spawn_link( fun() ->
							oceanic_start( TtyPath, EventListeners )
						end ).



% TTY subsection.


-doc """
Returns the path to the default TTY allocated to the USB Enocean gateway,
according to our conventions.
""".
-spec get_default_tty_path() -> device_path().
get_default_tty_path() ->

	% Better than, say:
	%"/dev/ttyUSB0".

	"/dev/ttyUSBEnOcean".



-doc """
Tells whether the default TTY exists and is a device.

Useful at least for testing.
""".
-spec has_tty() -> tty_detection_outcome().
has_tty() ->
	has_tty( get_default_tty_path() ).



-doc """
Tells whether the specified TTY exists and is a device, together with any
failure reason.

Useful at least for testing.
""".
-spec has_tty( any_device_path() ) -> tty_detection_outcome().
has_tty( AnyTtyPath ) ->

	case file_utils:exists( AnyTtyPath ) of

		true ->
			case file_utils:resolve_type_of( AnyTtyPath ) of

				device ->
					true;

				OtherType ->
					{ false, { not_device, OtherType } }

			end;

		false ->
			{ false, non_existing }

	end.



-doc "Secures the TTY connection to the Enocean gateway.".
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


-doc "Executes the start procedure of the spawned Oceanic server process.".
-spec oceanic_start( device_path(), [ event_listener_pid() ] ) -> no_return().
oceanic_start( TtyPath, EventListeners ) ->

	SerialPid = secure_tty( TtyPath ),

	BaseState = get_base_state( SerialPid ),

	LoadedState = load_configuration( BaseState ),

	naming_utils:register_as( ?oceanic_server_reg_name, _RegScope=local_only ),

	InitialState = LoadedState#oceanic_state{
		device_path=text_utils:string_to_binary( TtyPath ),
		event_listeners=type_utils:check_pids( EventListeners ) },

	oceanic_loop( _SkipLen=0, _MaybeTelTail=undefined, InitialState ).



-doc "Returns a base, blank yet initialised, Oceanic state.".
-spec get_base_state( serial_server_pid() ) -> oceanic_state().
get_base_state( SerialServerPid ) ->

	% We discover from start our base EURID; a direct, ad hoc logic cannot
	% really be used, as request tracking would be in the way:
	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug( "Discovering our base EURID." ) ),

	CommonCmd = co_rd_idbase,

	CmdTelegram = encode_common_command_request( CommonCmd ),

	% For decoding re-use (try_integrate_next_telegram/4), we have to have a state
	% anyway:

	InitCmdReq = #command_request{ command_type=CommonCmd,
								   command_telegram=CmdTelegram,
								   requester=internal },

	InitialState = #oceanic_state{
		serial_server_pid=SerialServerPid,
		emitter_eurid=string_to_eurid( ?default_emitter_eurid ),
		device_table=table:new(),
		command_queue=queue:new(),
		waited_command_info={ InitCmdReq, _MaybeTimerRef=undefined },
		last_traffic_seen=time_utils:get_timestamp() },

	SentState = send_raw_telegram( CmdTelegram, InitialState ),

	% Blank start:
	wait_initial_base_request( _ToSkipLen=0, _MaybeAccChunk=undefined,
							   SentState ).



-doc """
Returns an initialised, Oceanic state, once the initial base ID request has been
properly answered.
""".
-spec wait_initial_base_request( count(), option( telegram_tail() ),
								 oceanic_state() ) -> oceanic_state().
wait_initial_base_request( ToSkipLen, MaybeNextTelTail, State ) ->

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Waiting initial base request "
			"(ToSkipLen=~B, MaybeNextTelTail=~w).",
			[ ToSkipLen, MaybeNextTelTail ] ) ),

	receive

		% Received data from the serial port:
		{ data, NewChunk } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Read ~ts.",
					[ telegram_to_string( NewChunk ) ] ) ),

			case try_integrate_next_telegram( ToSkipLen, MaybeNextTelTail,
											  NewChunk, State ) of

				{ decoded, Event=#read_base_id_info_response{
							% (not interested here in remaining_write_cycles)
							base_eurid=BaseEurid }, _MaybeDiscoverOrigin,
						_IsBackOnline, _MaybeDevice, NewMaybeNextTelTail,
						ReadState } ->

					% Clearer that way:
					case NewMaybeNextTelTail of

						undefined ->
							ok;

						DroppedChunk ->
							trace_bridge:warning_fmt( "Dropping initially "
								"chunk ~ts.",
								[ telegram_to_string( DroppedChunk ) ] )

					end,

					% Checks that this is a response to the request made in
					% get_base_state/1, before clearing it:
					%
					case State#oceanic_state.waited_command_info of

						{ _InitCmdReq=#command_request{
									command_type=co_rd_idbase,
									% Not to be checked: command_telegram
									requester=internal },
						   _MaybeTimerRef=undefined } ->
							ok;

						OtherWaitedCmdInfo ->
							trace_bridge:error_fmt( "Unexpected "
								"read_base_id_info_response, whereas waited "
								"command information is ~p.",
								[ OtherWaitedCmdInfo ] )

					end,

					cond_utils:if_defined( oceanic_debug_tty,
						trace_bridge:debug_fmt( "Successfully ~ts.",
							[ device_event_to_string( Event ) ] ),
						basic_utils:ignore_unused( Event ) ),

					trace_bridge:info_fmt( "Detected the EURID of the base "
						"emitter: ~ts.", [ oceanic:eurid_to_string( BaseEurid ) ] ),

					ReadState#oceanic_state{ emitter_eurid=BaseEurid,
											 waited_command_info=undefined };


				{ Unsuccessful, NewToSkipLen, NewMaybeNextTelTail, NewState } ->

					cond_utils:if_defined( oceanic_debug_tty,
						trace_bridge:debug_fmt( "Unsuccessful decoding, '~w' "
							"(whereas NewToSkipLen=~B, NewMaybeNextTelTail=~w).",
							[ Unsuccessful, NewToSkipLen, NewMaybeNextTelTail ] ),
						basic_utils:ignore_unused(
							[ Unsuccessful, NewMaybeNextTelTail ] ) ),

					wait_initial_base_request( NewToSkipLen, MaybeNextTelTail,
											   NewState )

			end;

		{ onSerialMessage, Msg } ->
			trace_bridge:warning( Msg ),
			wait_initial_base_request( ToSkipLen, MaybeNextTelTail, State )

	end.



-doc """
Loads Oceanic configuration information from the default Ceylan preferences
file, if any, otherwise returns a state with an empty device table.

Refer to load_configuration/2 for key information.

See also the 'preferences' Myriad module.
""".
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

			trace_bridge:info_fmt( "No preferences file ('~ts') found, "
				"no device information obtained from it.", [ PrefPath ] ),

			State

	end.



-doc """
Loads Oceanic configuration information in the specified state from the
specified ETF file, and returns a corresponding updated state.

The configuration information is expected to contain up to one entry for the
following keys (atoms):

- oceanic_emitter: to specify the pseudo-device emitting any telegram to be sent
by Oceanic (note that USB gateways have already their own base EURID that shall
be preferred; refer to the co_rd_idbase common command)

- oceanic_jamming_threshold: to set a non-default threshold

- oceanic_devices: to declare the known devices; a given device shall never be
declared more than once
""".
-spec load_configuration( any_file_path(), oceanic_state() ) -> oceanic_state().
load_configuration( ConfFilePath, State ) ->

	file_utils:is_existing_file_or_link( ConfFilePath )
		orelse throw( { oceanic_config_file_not_found, ConfFilePath } ),

	trace_bridge:info_fmt( "Reading preferences file '~ts'.",
						   [ ConfFilePath ] ),

	Pairs = file_utils:read_etf_file( ConfFilePath ),

	% Not all pairs may be about Oceanic, so extra ones may exist:
	{ _RemainingPairs, NewState } = extract_settings( Pairs, _Acc=[], State ),

	NewState.



-doc """
Registers the specified additional Oceanic configuration in the specified
Oceanic server.

Refer to load_configuration/2 for key information.
""".
-spec add_configuration_settings( oceanic_settings(), oceanic_server_pid() ) ->
		void().
add_configuration_settings( OcSettings, OcSrvPid ) ->
	OcSrvPid ! { addConfigurationSettings, OcSettings }.



-doc """
Registers internally the specified Oceanic configuration, overriding any prior
settings, and returns a corresponding updated state.
""".
-spec apply_conf_settings( oceanic_settings(), oceanic_state() ) ->
								oceanic_state().
apply_conf_settings( OcSettings, State ) ->
	case extract_settings( OcSettings, _Acc=[], State ) of

		{ _RemainingPairs=[], NewState } ->
			NewState;

		{ RemainingPairs, NewState } ->
			trace_bridge:error_fmt( "Received invalid Oceanic settings, which "
				"have been ignored: ~p.", [ RemainingPairs ] ),
			NewState

	end.



% Extracts the relevant Oceanic settings, and returns any remaining element.
extract_settings( _Pairs=[], Acc, State ) ->
	{ Acc, State };


extract_settings( _Pairs=[ { oceanic_emitter, EmitterEuridStr } | T ], Acc,
				  State ) ->
	EmitterEurid = text_utils:hexastring_to_integer( EmitterEuridStr ),
	NewState = State#oceanic_state{ emitter_eurid=EmitterEurid },
	extract_settings( T, Acc, NewState );


extract_settings( _Pairs=[ { oceanic_devices, DeviceEntries } | T ], Acc,
				  State=#oceanic_state{ device_table=DeviceTable } ) ->

	NewDeviceTable = declare_devices( DeviceEntries, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	extract_settings( T, Acc, NewState );


extract_settings(
		_Pairs=[ { oceanic_jamming_threshold, UserJamThreshold } | T ],
		Acc, State ) when is_integer( UserJamThreshold )
						  andalso UserJamThreshold > 0 ->

	NewState = State#oceanic_state{ jamming_threshold=UserJamThreshold },

	extract_settings( T, Acc, NewState );


extract_settings( _Pairs=[ { oceanic_jamming_threshold, Other } | T ], Acc,
				  State )  ->

	trace_bridge:error_fmt( "Ignoring the following incorrect jamming "
		"threshold: ~p.", [ Other ] ),

	extract_settings( T, Acc, State );


% Typically an entry unrelated to Oceanic:
extract_settings( _Pairs=[ _E={ _K, _V } | T ], Acc, State )  ->
	%trace_bridge:debug_fmt( "Ignoring the following entry: ~p.", [ E ] ),
	extract_settings( T, Acc, State );


extract_settings( Other, Acc, State ) ->
	trace_bridge:error_fmt( "Ignoring the following incorrect (non-list) "
		"Oceanic settings: ~p.", [ Other ] ),
	{ Acc, State }.



-doc "Adds the specified devices in the specified device table.".
-spec declare_devices( [ device_config() ], device_table() ) -> device_table().
declare_devices( _DeviceCfgs=[], DeviceTable ) ->
	DeviceTable;

declare_devices( _DeviceCfgs=[ { NameStr, EuridStr, EepStr } | T ],
				 DeviceTable ) ->
	declare_devices( [ { NameStr, EuridStr, EepStr,
						 _MaybeActPeriodicity=auto } | T ],
					 DeviceTable );

% Main clause; any config comment (as last element) dropped by the next clause:
declare_devices( _DeviceCfgs=[
					DC={ NameStr, EuridStr, EepStr, MaybeActPeriodicity } | T ],
				 DeviceTable ) ->

	text_utils:is_string( NameStr ) orelse
		begin
			trace_bridge:error_fmt( "Invalid device name ('~p') "
				"in configuration ~p.", [ NameStr, DC ] ),
			throw( { invalid_device_configured_name, NameStr, DC } )
		end,

	Eurid = try text_utils:hexastring_to_integer(
		text_utils:ensure_string( EuridStr ), _ExpectPrefix=false ) of

			Int ->
				Int

		% Typically error:badarg:
		catch _:E ->
			trace_bridge:error_fmt( "Invalid EURID ('~ts') "
				"for device named '~ts'.", [ EuridStr, NameStr ] ),
			throw( { invalid_device_configured_eurid, EuridStr, E, NameStr } )

	end,

	text_utils:is_string( EepStr ) orelse
		begin
			trace_bridge:error_fmt( "Invalid device EEP ('~p') "
				"in configuration ~p.", [ EepStr, DC ] ),
			throw( { invalid_device_configured_eep, EepStr, DC } )
		end,

	EepBinStr = text_utils:string_to_binary( EepStr ),

	MaybeEepId = case oceanic_generated:get_maybe_first_for_eep_strings(
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

	ActPeriod = case MaybeActPeriodicity of

		none ->
			none;

		default ->
			1000 * time_utils:dhms_to_seconds( ?default_dhms_periodicity );

		auto ->
			decide_auto_periodicity( MaybeEepId );

		DHMS ->
			case time_utils:is_dhms_duration( DHMS ) of

				true ->
					1000 * time_utils:dhms_to_seconds( DHMS );

				false ->
					trace_bridge:error_fmt( "Invalid DHMS activity periodicity "
						"('~p') for device named '~ts'.",
						[ DHMS, NameStr ] ),
					throw( { invalid_dhms_activity_periodicity, DHMS,
							 NameStr } )

			end

	end,

	% Device table indexed by device eurid(), which is duplicated in the record
	% values for convenience:
	%
	DeviceRec = #enocean_device{ eurid=Eurid,
								 name=text_utils:ensure_binary( NameStr ),
								 eep=MaybeEepId,
								 discovered_through=configuration,
								 expected_periodicity=ActPeriod },

	table:has_entry( Eurid, DeviceTable ) andalso
		trace_bridge:warning_fmt( "Overriding entry for device "
			"whose EURID is ~ts (with: ~ts)",
			[ eurid_to_string( Eurid ), device_to_string( DeviceRec ) ] ),

	% Overriding allowed:
	NewDeviceTable = table:add_entry( Eurid, DeviceRec, DeviceTable ),

	declare_devices( T, NewDeviceTable );

% Dropping comment (useful only for the user configuration):
declare_devices( _DeviceCfgs=[ { NameStr, EuridStr, EepStr,
								 MaybeActPeriodicity, CommentStr } | T ],
				 DeviceTable ) when is_list( CommentStr ) ->
	declare_devices( [ { NameStr, EuridStr, EepStr, MaybeActPeriodicity } | T ],
					 DeviceTable );


declare_devices( _DeviceCfgs=[ Other | _T ], _DeviceTable ) ->
	throw( { invalid_device_config, Other } ).



-doc """
Decides whether an 'auto' activity periodicity mode can be retained, based on
the specified EEP (if any).

We consider that devices implementing some EEPs do not send periodical state
updates.

For example contact switches shall not be left on 'auto', otherwise they are
likely to be considered lost after some time.

If the choice made here is not relevant for a given device, declare for it an
explicit (non-auto) periodicity.
""".
-spec decide_auto_periodicity( option( eep_id() ) ) -> expected_periodicity().
decide_auto_periodicity( _MaybeEEPId=undefined ) ->
	% Not known, supposed not talkative:
	none;

decide_auto_periodicity( _EEPId=thermo_hygro_low ) ->
	auto;

decide_auto_periodicity( _EEPId=thermo_hygro_mid ) ->
	auto;

decide_auto_periodicity( _EEPId=thermo_hygro_high ) ->
	auto;

decide_auto_periodicity( _EEPId=push_button ) ->
	none;

decide_auto_periodicity( _EEPId=double_rocker_switch_style_1 ) ->
	none;

decide_auto_periodicity( _EEPId=double_rocker_switch_style_2 ) ->
	none;

decide_auto_periodicity( _EEPId=double_rocker_multipress ) ->
	none;

% Typically opening detectors:
decide_auto_periodicity( _EEPId=single_input_contact ) ->
	auto;

decide_auto_periodicity( _EEPId=single_channel_module ) ->
	none;

decide_auto_periodicity( _EEPId=double_channel_module ) ->
	none;

decide_auto_periodicity( _OtherEEPId ) ->
	none.



% Helpers introduced for re-use by the code using Oceanic.


-doc "Returns a list of all known device types.".
-spec get_all_device_types() -> [ device_type() ].
get_all_device_types() ->
	[ thermo_hygro_sensor, single_contact, push_button, smart_plug,
	  double_rocker ].



-doc "Tells whether the specified term is a valid device type.".
-spec is_valid_device_type( term() ) -> boolean().
is_valid_device_type( DT ) ->
	lists:member( DT, get_all_device_types() ).


% General helpers, for incoming/outgoing specifications.

-doc "Tells whether the specified term is a valid application style.".
-spec is_valid_application_style( term() ) -> boolean().
is_valid_application_style( AppStyle ) when is_integer( AppStyle )
											andalso AppStyle > 0 ->
	true;

is_valid_application_style( _Other ) ->
	false.


-doc "Tells whether the specified term is a valid channel.".
-spec is_valid_channel( term() ) -> boolean().
is_valid_channel( Channel ) when is_integer( Channel ) andalso Channel > 0 ->
	true;

is_valid_channel( _Other ) ->
	false.


-doc "Tells whether the specified term is a valid button position.".
-spec is_valid_button_position( term() ) -> boolean().
is_valid_button_position( _ButPos=top ) ->
	true;

is_valid_button_position( _ButPos=bottom ) ->
	true;

is_valid_button_position( _Other ) ->
	false.



-doc "Tells whether the specified term is a valid button transition.".
-spec is_valid_button_transition( term() ) -> boolean().
is_valid_button_transition( _ButPos=pressed ) ->
	true;

is_valid_button_transition( _ButPos=released ) ->
	true;

is_valid_button_transition( _Other ) ->
	false.



-doc """
Returns a textual description of the specified CITS (canonical incoming trigger
specification).
""".
-spec cits_to_string( canon_incoming_trigger_spec() ) -> ustring().
cits_to_string( _CITS={ DevType, CanonDRSCSpec } ) ->
	text_utils:format( "incoming trigger specification regarding ~ts",
		[ device_state_change_spec_to_string( DevType, CanonDRSCSpec ) ] ).


-doc """
Returns a textual description of the specified canonical device state change
specification.
""".
-spec device_state_change_spec_to_string( device_type(),
								device_state_change_spec() )-> ustring().
device_state_change_spec_to_string( _DevType=double_rocker,
		_CanonDRSCSpec={ Channel, ButPos, ButTrans } ) ->
	text_utils:format( "double-rocker channel #~B, ~ts button position "
					   "and a ~ts transition", [ Channel, ButPos, ButTrans ] );

device_state_change_spec_to_string( _DevType=push_button,
									_CanonPBSCSpec=ButTrans ) ->
	text_utils:format( "a toggle on a ~ts transition", [ ButTrans ] );

device_state_change_spec_to_string( DevType, _CanonSCSpec ) ->
	text_utils:format( "an unknown state change for a device of type '~ts'",
					   [ DevType ] ).



-doc """
Canonicalises the specified user-level double-rocker change specification.
""".
-spec canonicalise_double_rocker_change_spec(
		double_rocker_state_change_spec() ) ->
			canon_double_rocker_state_change_spec().
% Full version:
canonicalise_double_rocker_change_spec( DRCS={ Channel, ButPos, ButTrans } ) ->

	is_valid_channel( Channel ) orelse
		throw( { invalid_double_rocker_channel_in_change_spec, Channel } ),

	is_valid_button_position( ButPos ) orelse
		throw( { invalid_double_rocker_button_position_in_change_spec,
				 ButPos } ),

	is_valid_button_transition( ButTrans ) orelse
		throw( { invalid_double_rocker_button_transition_in_change_spec,
				 ButTrans } ),

	DRCS;

canonicalise_double_rocker_change_spec( { Channel, ButPos } ) ->
	canonicalise_double_rocker_change_spec(
	  { Channel, ButPos, _ButTrans=pressed } );

canonicalise_double_rocker_change_spec( _DRChangeSpec=Channel ) ->
	canonicalise_double_rocker_change_spec( { Channel, _ButPos=top } ).


-doc """
Returns the specification of the reverse of the specified canonical device state
change, that is its reciprocal one (thus able to undo it).

Typically used to determine how to untrigger an actuator (e.g. switch a smart
plug off).
""".
-spec get_reciprocal_state_change_spec( device_type(),
		canon_device_state_change_spec() ) -> canon_device_state_change_spec().
get_reciprocal_state_change_spec( _DevType=double_rocker,
								  _CSCS={ Channel, _ButPos=top, ButTrans } ) ->
	% By convention, same transition, other button of the rocker:
	{ Channel, bottom, ButTrans };

get_reciprocal_state_change_spec( _DevType=double_rocker,
		_CSCS={ Channel, _ButPos=bottom, ButTrans } ) ->
	{ Channel, top, ButTrans };

% Just alternating:
get_reciprocal_state_change_spec( _DevType=push_button,	_CSCS=undefined ) ->
	undefined.




-doc """
Tells whether the specified device trigger event matches at least one if the
CLES and, if yes, for which new status reported by this event (typically the on
or off button of a rocker being transitioned).
""".
-spec event_matches_trigger( device_event(),
		[ canon_listened_event_spec() ] ) -> event_match_trigger_outcome().
event_matches_trigger( DevEvent, CLESs ) ->

	DevEurid = get_source_eurid( DevEvent ),

	% Tries to find a CITS corresponding to this emitter:
	case get_maybe_matching_cits( DevEurid, CLESs ) of

		undefined ->
			false;

		CITS ->
			interpret_cits_matching( CITS, DevEurid, DevEvent )

	end.



-doc """
Returns (any, first) CITS that matches the specified emitter EURID, among the
specified CLES.
""".
-spec get_maybe_matching_cits( eurid(), [ canon_listened_event_spec() ] ) ->
									option( canon_incoming_trigger_spec() ).
get_maybe_matching_cits( _EmitterEurid, _CLESs=[] ) ->
	undefined;

% Matching:
get_maybe_matching_cits( EmitterEurid,
						 _CLESs=[ { EmitterEurid, CITS } | _T ] ) ->
	CITS;

% Non-matching EmitterEurid:
get_maybe_matching_cits( EmitterEurid, _CLESs=[ _ | T ] ) ->
	get_maybe_matching_cits( EmitterEurid, T ).



-doc """
Reports any trigger matching and corresponding device new logical status, based
on the specified CITS and device event.
""".
-spec interpret_cits_matching( canon_incoming_trigger_spec(), eurid(),
				device_event() ) -> event_match_trigger_outcome().
% Here a double-rocker change spec matches (therefore in terms of device type,
% channel, button position and transition):
%
interpret_cits_matching(
		_CITS={ _DevType=double_rocker, _CSCS={ Channel, ButPos, ButTrans } },
		DevEurid,
		_DevEvent=#double_rocker_switch_event{
						first_action_button={ Channel, ButPos },
						energy_bow=ButTrans } ) ->

	{ true, DevEurid, _NewStatus=on };


% Here, same as previous clause, a double-rocker change spec matches (therefore
% in terms of device type, channel and transition), except for the different -
% hence opposite - button position:
%
interpret_cits_matching(
		_CITS={ _DevType=double_rocker, _CSCS={ Channel, _ButPos, ButTrans } },
		DevEurid,
		_DevEvent=#double_rocker_switch_event{
						first_action_button={ Channel, _OppositeButPos },
						energy_bow=ButTrans } ) ->

	{ true, DevEurid, _NewStatus=off };


% Here a push-button change spec matches, i.e. the specified button transition
% does; acting as a rocker (alternating between two states, on and off) based on
% a single button transition (either pressed or released, not both, otherwise a
% user action would trigger press then release, and this would lead to a double
% transition)
%
interpret_cits_matching( _CITS={ _DevType=push_button, _CSCS=ButtonTransition },
		DevEurid,
		_DevEvent=#push_button_switch_event{ transition=ButtonTransition } ) ->

	{ true, DevEurid, _NewStatus=inverted };


% Non-listened button transition for this push-button:
interpret_cits_matching( _CITS={ _DevType=push_button, _CSCS },
		_DevEurid,
		_DevEvent=#push_button_switch_event{} ) ->

	false;


% Add here clauses if wanting to match other CITS/events.

% Non-matching case:
interpret_cits_matching( CITS, _DevEurid, DevEvent ) ->


	cond_utils:if_defined( us_main_debug_home_automation,
		trace_bridge:debug_fmt(
			"(this event ~ts does not match the presence switching ~ts)",
			[ oceanic:device_event_to_string( DevEvent ),
			  oceanic:cits_to_string( CITS ) ] ),
		basic_utils:ignore_unused( [ CITS, DevEvent ] ) ),

	false.





% Helpers for incoming specifications.


-doc "Canonicalises the specified user-level listening event specifications.".
-spec canonicalise_listened_event_specs( [ listened_event_spec() ] ) ->
											[ canon_listened_event_spec() ].
% Better than "bad generator" with a list comprehension:
canonicalise_listened_event_specs( LESs ) when is_list( LESs ) ->
	canonicalise_listened_event_specs( LESs, _Acc=[] );

canonicalise_listened_event_specs( Other ) ->
	throw( { invalid_listened_event_specs, non_list, Other } ).



% (sub-helper)
canonicalise_listened_event_specs( _LESs=[], Acc ) ->
	% Preferring preserving order:
	lists:reverse( Acc );

canonicalise_listened_event_specs( _LESs=[ { MaybeEuridStr, ITS } | T ],
								   Acc ) ->

	Eurid = maybe_string_to_eurid( MaybeEuridStr ),

	CITS = case ITS of

		{ double_rocker, DRChangeSpec } ->
			CanDRChangeSpec =
				canonicalise_double_rocker_change_spec( DRChangeSpec ),

			{ double_rocker, CanDRChangeSpec };

		push_button ->
			{ push_button, pressed };

		P={ push_button, ButTrans } ->
			is_valid_button_transition( ButTrans ) orelse
				throw( { invalid_button_transition, ButTrans } ),
			P;

		{ OtherDeviceType, ChangeSpec } when is_atom( OtherDeviceType ) ->
			case is_valid_device_type( OtherDeviceType ) of

				% Valid yet not supported (yet):
				true ->
					throw( { unsupported_listened_device_type, OtherDeviceType,
							 ChangeSpec } );

				false ->
					throw( { invalid_listened_device_type, OtherDeviceType,
							 ChangeSpec } )

			end;


		Other ->
			throw( { invalid_listened_event_spec, Other, MaybeEuridStr } )

	end,

	CLES = { Eurid, CITS },

	canonicalise_listened_event_specs( T, [ CLES | Acc ] );

canonicalise_listened_event_specs( _LESs=[ Other | _T ], _Acc ) ->
	throw( { invalid_listened_event_spec, non_pair, Other } ).



-doc """
Returns a textual description of the specified canonical listening event
specification (CLES).

No Oceanic server specified, hence EURIDs cannot be resolved in actual device
descriptions.
""".
-spec canon_listened_event_spec_to_string( canon_listened_event_spec() ) ->
												ustring().
canon_listened_event_spec_to_string( { EmitterDeviceEurid,
		_CanonITS={ DevType, CanonDevSCS } } ) ->
	text_utils:format( "listening to device whose EURID is ~ts "
		"of type ~ts, for ~ts",
		[ eurid_to_string( EmitterDeviceEurid ), DevType,
		  device_state_change_spec_to_string( DevType, CanonDevSCS ) ] ).



-doc """
Returns a textual description of the specified canonical listening event
specification, enriched thanks to the specified Oceanic server.
""".
-spec canon_listened_event_spec_to_string( canon_listened_event_spec(),
										   oceanic_server_pid() ) -> ustring().
canon_listened_event_spec_to_string( { EmitterDeviceEurid,
		_CanonITS={ DevType, CanonDevSCS } }, OcSrvPid ) ->
	text_utils:format( "listening to ~ts of type ~ts, for ~ts",
		[ get_device_description( EmitterDeviceEurid, OcSrvPid ), DevType,
		  device_state_change_spec_to_string( DevType, CanonDevSCS ) ] ).




-doc """
Returns a textual description of the specified canonical listening event
specifications.

No Oceanic server specified, hence EURIDs cannot be resolved in actual device
descriptions.
""".
-spec canon_listened_event_specs_to_string( [ canon_listened_event_spec() ] ) ->
												ustring().
canon_listened_event_specs_to_string( CLESs ) ->
	text_utils:strings_to_string( [ canon_listened_event_spec_to_string( CLES )
										|| CLES <- CLESs ] ).


-doc """
Returns a textual description of the specified canonical listening event
specifications, enriched thanks to the specified Oceanic server.
""".
-spec canon_listened_event_specs_to_string( [ canon_listened_event_spec() ],
											oceanic_server_pid() ) -> ustring().
canon_listened_event_specs_to_string( CLESs, OcSrvPid ) ->
	text_utils:strings_to_string( [ canon_listened_event_spec_to_string( CLES,
										OcSrvPid ) || CLES <- CLESs ] ).




% Helpers for outgoing specifications.


-doc "Canonicalises the specified user-level emitting event specifications.".
-spec canonicalise_emitted_event_specs( [ emitted_event_spec() ] ) ->
											[ canon_emitted_event_spec() ].
% Better than "bad generator" with a list comprehension:
canonicalise_emitted_event_specs( EESs ) when is_list( EESs ) ->
	canonicalise_emitted_event_specs( EESs, _Acc=[] );

canonicalise_emitted_event_specs( Other ) ->
	throw( { invalid_emitted_event_specs, non_list, Other } ).



% (sub-helper)
canonicalise_emitted_event_specs( _EESs=[], Acc ) ->
	% Preferring preserving order:
	lists:reverse( Acc );

canonicalise_emitted_event_specs( _EESs=[
		{ OTS, ActInfo={ MaybeEuridStr, MaybeTargetDeviceType } } | T ], Acc ) ->

	COTS = case OTS of

		{ double_rocker, DRChangeSpec } ->
			DefaultAppStyle = 1,

			CanDRChangeSpec =
				canonicalise_double_rocker_change_spec( DRChangeSpec ),

			{ double_rocker, _VirtEmitInfo=DefaultAppStyle, CanDRChangeSpec };


		{ double_rocker, AppStyle, DRChangeSpec } ->

			is_valid_application_style( AppStyle ) orelse
				throw( { invalid_double_rocker_app_style_in_emitted_spec,
						 AppStyle } ),

			CanDRChangeSpec =
				canonicalise_double_rocker_change_spec( DRChangeSpec ),

			{ double_rocker, _VirtEmitInfo=AppStyle, CanDRChangeSpec };


		Other ->
			throw( { unsupported_outgoing_trigger_spec, Other, ActInfo } )

	end,

	Eurid = maybe_string_to_eurid( MaybeEuridStr ),

	MaybeTargetDeviceType =:= undefined
		orelse is_valid_device_type( MaybeTargetDeviceType )
		orelse throw( { invalid_device_type, MaybeTargetDeviceType } ),

	CanActInfo = { Eurid, MaybeTargetDeviceType },

	CEES = { COTS, CanActInfo },

	canonicalise_emitted_event_specs( T, [ CEES | Acc ] );


canonicalise_emitted_event_specs( _EESs=[ { OTS, _ActInfo=MaybeEuridStr } | T ], Acc ) ->
	CanonActInfo = { _TargetDevice=MaybeEuridStr, _TargetDeviceType=undefined },
	canonicalise_emitted_event_specs( [ { OTS, CanonActInfo } | T ], Acc );


canonicalise_emitted_event_specs( _EESs=[ Other | _T ], _Acc ) ->
	throw( { invalid_emitted_event_spec, non_pair, Other } ).



-doc """
Returns a textual description of the specified information regarding a virtual
emitting device.
""".
-spec virtual_emitter_info_to_string( virtual_emitter_info() ) -> ustring().
virtual_emitter_info_to_string( _VirtualEmitterInfo=undefined ) ->
	"";

virtual_emitter_info_to_string( VirtualEmitterInfo ) ->
	text_utils:format( " whose associated virtual-emitter information is ~p",
					   [ VirtualEmitterInfo ] ).


-doc """
Returns a textual description of the specified canonical outgoing trigger
specification (COTS).
""".
-spec canon_outgoing_trigger_spec_to_string( canon_outgoing_trigger_spec() ) ->
												ustring().
canon_outgoing_trigger_spec_to_string(
		_CanonOTS={ EmittingDeviceType, VirtualEmitterInfo, CanonDevStChS } ) ->
	text_utils:format( "emitting as a device of type ~ts~ts, "
		"for a state change of ~ts", [ EmittingDeviceType,
		  virtual_emitter_info_to_string( VirtualEmitterInfo ),
		  device_state_change_spec_to_string( EmittingDeviceType,
											  CanonDevStChS ) ] ).



-doc """
Returns a textual description of the specified canonical emitting event
specification.

No Oceanic server specified, hence EURIDs cannot be resolved in actual device
descriptions.
""".
-spec canon_emitted_event_spec_to_string( canon_emitted_event_spec() ) ->
												ustring().
canon_emitted_event_spec_to_string( _CEES={ CanonOTS, ActInfo } ) ->
	text_utils:format( "~ts, targeting ~ts",
		[ canon_outgoing_trigger_spec_to_string( CanonOTS ),
		  actuator_info_to_string( ActInfo ) ] ).



-doc """
Returns a textual description of the specified canonical emitting event
specification, enriched thanks to the specified Oceanic server.
""".
-spec canon_emitted_event_spec_to_string( canon_emitted_event_spec(),
										  oceanic_server_pid() ) -> ustring().
canon_emitted_event_spec_to_string( _CEES={ CanonOTS, ActInfo }, OcSrvPid ) ->
	text_utils:format( "~ts, targeting ~ts",
		[ canon_outgoing_trigger_spec_to_string( CanonOTS ),
		  actuator_info_to_string( ActInfo, OcSrvPid ) ] ).



-doc """
Returns a basic textual description of the specified actuator information.
""".
-spec actuator_info_to_string( actuator_info() ) -> ustring().
actuator_info_to_string( { Eurid, _MaybeDeviceType=undefined } ) ->
	text_utils:format(
		"actuator whose EURID is ~ts (not addressed as a specific type)",
		[ eurid_to_string( Eurid ) ] );

actuator_info_to_string( { Eurid, AddrDeviceType } ) ->
	text_utils:format( "actuator whose EURID is ~ts, addressed as a ~ts type",
					   [ eurid_to_string( Eurid ), AddrDeviceType ] ).


-doc """
Returns a textual description of the specified actuator information, enhanced
thanks to the Oceanic server.
""".
-spec actuator_info_to_string( actuator_info(), oceanic_server_pid() ) -> ustring().
actuator_info_to_string( { Eurid, _MaybeDeviceType=undefined }, OcSrvPid ) ->
	text_utils:format( "actuator ~ts (not addressed as a specific type)",
		[ get_device_description( Eurid, OcSrvPid ) ] );

actuator_info_to_string( { Eurid, AddrDeviceType }, OcSrvPid ) ->
	text_utils:format( "actuator ~ts, addressed as a ~ts type",
		[ get_device_description( Eurid, OcSrvPid ), AddrDeviceType ] ).



-doc """
Returns a textual description of the specified canonical emitting event
specifications.

No Oceanic server specified, hence EURIDs cannot be resolved in actual device
descriptions.
""".
-spec canon_emitted_event_specs_to_string( [ canon_emitted_event_spec() ] ) ->
												ustring().
canon_emitted_event_specs_to_string( CEESs ) ->
	text_utils:strings_to_string( [ canon_emitted_event_spec_to_string( CEES )
									  || CEES <- CEESs ] ).


-doc """
Returns a textual description of the specified canonical emitting event
specifications, enriched thanks to the specified Oceanic server.
""".
-spec canon_emitted_event_specs_to_string( [ canon_emitted_event_spec() ],
									oceanic_server_pid() ) -> ustring().
canon_emitted_event_specs_to_string( CEESs, OcSrvPid ) ->
	text_utils:strings_to_string( [ canon_emitted_event_spec_to_string( CEES,
										OcSrvPid ) || CEES <- CEESs ] ).




-doc "Declares the specified taught-in device.".
-spec declare_device_from_teach_in( eurid(), eep(), device_table() ) ->
						{ device_table(), enocean_device(), timestamp() }.
declare_device_from_teach_in( Eurid, Eep, DeviceTable ) ->

	MaybeEepId = resolve_eep( Eep ),

	Now = time_utils:get_timestamp(),

	NewDevice = case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device whose EURID "
				"is ~ts~ts through teach-in.", [ eurid_to_string( Eurid ) ] ),

			#enocean_device{ eurid=Eurid,
							 name=undefined,
							 eep=MaybeEepId,
							 discovered_through=teaching,
							 first_seen=Now,
							 last_seen=Now,
							 telegram_count=1,
							 error_count=0,
							 expected_periodicity=none };

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



-doc "Sends the specified telegram, through the specified Oceanic server.".
-spec send( telegram(), oceanic_server_pid() ) -> void().
send( Telegram, OcSrvPid ) ->
	OcSrvPid ! { sendOceanic, Telegram }.



-doc """
Acknowledges (accepts) the specified teach request, by sending a (successful)
teach response.

See EEP Teach-(In/Out) Response - UTE Message (Broadcast / CMD: 0x1) [EEP-gen]
p.26.
""".
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



-doc """
Acknowledges the specified teach-in request, by sending the specified teach-in
response.

See EEP Teach-In Response - UTE Message (Broadcast / CMD: 0x1) [EEP-gen] p.26.
""".
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
its EURID (see get_oceanic_eurid/1) once for all, and uses it afterwards
(otherwise, no emitted telegram will be taken into account by receivers).

Event sent in the context of EEP F6-02-01 or EEP F6-02-02 ("Light and Blind
Control - Application Style 1 or 2"), for T21=1. It results thus in a RPS
telegram, an ERP1 radio packet encapsulated into an ESP3 one.

See [EEP-spec] p.15 and its decode_rps_double_rocker_packet/7 counterpart.

Depending on how Oceanic was learnt by the target actuator, it will be seen
either as a rocker (recommended) or as push-button(s): refer to
http://oceanic.esperide.org/#buttons-vs-rocker-transition-vs-state for more
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

	%trace_bridge:debug_fmt( "encode_double_rocker_switch_telegram: R1Enum = ~B,"
	%                       " R2Enum = ~B.", [ R1Enum, R2Enum ] ),

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



-doc """
Encodes a double-rocker multipress telegram, from the specified device to the
specified one (if any), reporting the specified transition for the specified
button.

Event sent in the context of EEP F6-02-01 or F6-02-02 ("Light and Blind Control
- Application Style 1 or 2"), for T21=1. It results thus in a RPS telegram, an
ERP1 radio packet encapsulated into an ESP3 one.

See [EEP-spec] p.15 and its decode_rps_double_rocker_packet/7 counterpart.
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
Returns the optional data suitable for sending to the specified device (if any).
""".
-spec get_optional_data_for_sending( option( eurid() ) ) -> telegram_opt_data().
get_optional_data_for_sending( _MaybeTargetEurid=undefined ) ->
	<<>>;

get_optional_data_for_sending( TargetEurid ) ->

	% We are sending here:
	%SubTelNum = 3,
	SubTelNum = 1,
	DBm = 16#ff,
	SecurityLevel = 0,

	_OptData= <<SubTelNum:8, TargetEurid:32, DBm:8, SecurityLevel:8>>.



-doc """
Executes synchronously the specified command, specified as an opaque,
already-encoded telegram, that is supposed to be acknowledged next by the
recipient device, with a response telegram.
""".
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
		%   trace_bridge:warning_fmt( "Received for command: ~p", [ Other ] )

	end.



-doc """
Returns the current (emitter) EURID used by Oceanic for the local USB gateway,
notably as default source base identifier when generating telegrams.

Useful when encoding telegrams.
""".
-spec get_oceanic_eurid( oceanic_server_pid() ) -> eurid().
get_oceanic_eurid( OcSrvPid ) ->
	OcSrvPid ! { getOceanicEurid, self() },

	receive

		{ oceanic_eurid, BaseEurid } ->
			BaseEurid

	end.



-doc """
Returns the best textual description found for the device specified from its
EURID.
""".
-spec get_device_description( eurid(), oceanic_server_pid() ) ->
												device_description().
get_device_description( Eurid, OcSrvPid ) ->
	OcSrvPid ! { getDeviceDescription, Eurid, self() },

	receive

		{ oceanic_device_description, BinDesc } ->
			BinDesc

	end.



-doc """
Returns the best textual description found for the specified button reference,
as seen from the specified Oceanic server.
""".
-spec get_button_ref_description( button_ref(), oceanic_server_pid() ) ->
											button_ref_description().
get_button_ref_description( _ButRef={ Eurid, Channel }, OcSrvPid ) ->
	text_utils:bin_format( "button ~B of ~ts",
		[ Channel, get_device_description( Eurid, OcSrvPid ) ] ).


-doc """
Returns the best textual descriptions found for the specified button references,
as seen from the specified Oceanic server.
""".
-spec get_button_ref_descriptions( [ button_ref() ], oceanic_server_pid() ) ->
											button_ref_description().
get_button_ref_descriptions( _ButRefs=[], _OcSrvPid ) ->
	"no button reference set";

get_button_ref_descriptions( _ButRefs=[ ButRef ], OcSrvPid ) ->
	text_utils:format( "a single button reference set: ~ts",
					   [ get_button_ref_description( ButRef, OcSrvPid ) ] );

get_button_ref_descriptions( ButRefs, OcSrvPid ) ->
	Strs = [ get_button_ref_description( BR, OcSrvPid ) || BR <- ButRefs ],
	text_utils:format( "~B button references set: ~ts",
		[ length( Strs ), text_utils:strings_to_listed_string( Strs ) ] ).



% Section for common commands.


-doc """
Returns the version information held by the USB gateway, thanks to a (local)
common command.
""".
-spec read_version( oceanic_server_pid() ) ->
						read_version_response() | common_command_failure().
read_version( OcSrvPid ) ->
	send_common_command( _Cmd=co_rd_version, OcSrvPid ).



-doc """
Returns the log information held by the USB gateway, thanks to a (local) common
command.
""".
-spec read_logs( oceanic_server_pid() ) ->
						read_logs_response() | common_command_failure().
read_logs( OcSrvPid ) ->
	send_common_command( _Cmd=co_rd_sys_log, OcSrvPid ).



-doc """
Returns the information held by the USB gateway about its base ID, thanks to a
(local) common command.
""".
-spec read_base_id_info( oceanic_server_pid() ) ->
						read_base_id_info_response() | common_command_failure().
read_base_id_info( OcSrvPid ) ->
	send_common_command( _Cmd=co_rd_idbase, OcSrvPid ).



-doc "Sends the specified common command and returns its outcome.".
-spec send_common_command( common_command(), oceanic_server_pid() ) ->
			common_command_response() | common_command_failure().
send_common_command( CommonCmd, OcSrvPid ) ->

	OcSrvPid ! { executeCommonCommand, CommonCmd, self() },
	receive

		{ oceanic_command_outcome, Outcome } ->
			Outcome

		% To debug:
		%Other ->
		%   trace_bridge:warning_fmt( "Received for common command '~ts': ~p",
		%                            [ CommonCmd, Other ] )

	end.



-doc """
Encodes the specified common command request, to be executed by the USB gateway.
""".
-spec encode_common_command_request( common_command() ) -> telegram().
% Future commands may have to be special-cased (e.g. if having parameters):
encode_common_command_request( _Cmd=co_rd_version ) ->
	encode_read_version_request();

encode_common_command_request( _Cmd=co_rd_sys_log ) ->
	encode_read_logs_request();

encode_common_command_request( _Cmd=co_rd_idbase ) ->
	encode_base_id_info_request();

encode_common_command_request( Cmd ) ->
	throw( { unknown_common_command, Cmd } ).



-doc """
Encodes a common command request of type 'CO_RD_VERSION', to read version
information from the USB gateway.

See its actual specification in [ESP3], p.36, and the decode_response_tail/5 for
WaitedCmd=co_rd_version.
""".
-spec encode_read_version_request() -> telegram().
encode_read_version_request() ->
	CmdNum = oceanic_generated:get_first_for_common_command( co_rd_version ),
	Data = <<CmdNum:8>>,
	encode_common_command( Data ).



-doc """
Encodes a common command request of type 'CO_RD_SYS_LOG', to read logs from the
USB gateway.

See its actual specification in [ESP3], p.37, and the decode_response_tail/5 for
WaitedCmd=co_rd_sys_log.
""".
-spec encode_read_logs_request() -> telegram().
encode_read_logs_request() ->
	CmdNum = oceanic_generated:get_first_for_common_command( co_rd_sys_log ),
	Data = <<CmdNum:8>>,
	encode_common_command( Data ).



-doc """
Encodes a common command request of type 'CO_RD_IDBASE', to read base ID
information from the USB gateway.

See its actual specification in [ESP3], p.40, and the decode_response_tail/5 for
WaitedCmd=co_rd_idbase.
""".
-spec encode_base_id_info_request() -> telegram().
encode_base_id_info_request() ->
	CmdNum = oceanic_generated:get_first_for_common_command( co_rd_idbase ),
	Data = <<CmdNum:8>>,
	encode_common_command( Data ).



-doc """
Encodes a common command request, based on the specified data (and with no
optional data defined).

The actual specification of common commands starts at p.32 of [ESP3].
""".
-spec encode_common_command( telegram_data() ) -> telegram().
encode_common_command( Data ) ->
	encode_esp3_packet( _PacketType=common_command_type, Data ).



-doc """
Encodes a common command, based on the specified data and optional data.

The actual specification of common commands starts at p.32 of [ESP3].
""".
-spec encode_common_command( telegram_data(), telegram_opt_data() ) ->
												telegram().
encode_common_command( Data, OptData ) ->
	encode_esp3_packet( _PacketType=common_command, Data, OptData ).



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

	ESP3HeaderCRC = compute_crc( ESP3Header ),

	FullDataCRC = compute_crc( FullData ),

	% This is an ESP3 packet:
	<<?sync_byte, ESP3Header/binary, ESP3HeaderCRC:8, FullData/binary,
	  FullDataCRC:8>>.



-doc """
Triggers the actuators specified by their event specs, by requesting the Oceanic
server to emit the corresponding telegrams and possibly track their
acknowledgements.

The expected event information expected to be sent back can be specified (the
same for all actuators); this will allow waiting for any acknowledgement from
them, and detect failed triggers.
""".
-spec trigger_actuators( [ canon_emitted_event_spec() ],
	option( reported_event_info() ), oceanic_server_pid() ) -> void().

trigger_actuators( ActEvSpecs, MaybeExpectedReportedEventInfo, OcSrvPid ) ->
	[ trigger_actuator( AES, MaybeExpectedReportedEventInfo, OcSrvPid )
		|| AES <- ActEvSpecs ].



-doc """
Triggers the actuator specified by its event spec, by requesting the Oceanic
server to emit the corresponding telegram and possibly track its
acknowledgement.
""".
-spec trigger_actuator( canon_emitted_event_spec(),
	option( reported_event_info() ), oceanic_server_pid() ) -> void().
% If the actuator is to be addressed as smart plug:
trigger_actuator( _CEES={ COTS,
						  _ActInfo={ ActEurid, _MaybeActDevType=smart_plug } },
				  MaybeExpectedReportedEventInfo,
				  OcSrvPid ) ->

	TrackSpec = { _WaitedEventType=smart_plug_status_report_event,
				  MaybeExpectedReportedEventInfo },

	% Oceanic server to keep track of a corresponding TriggerTrackInfo:
	OcSrvPid ! { sendDoubleRockerTelegram, [ ActEurid, COTS, TrackSpec ] };

trigger_actuator( _CEES={ _COTS,
						  _ActInfo={ _ActEurid, MaybeActDevType } },
				  _MaybeExpectedReportedEventInfo,
				  _OcSrvPid ) ->
	% If MaybeActDevType is set, could be guessed
	throw( { unsupported_actuator_device_type, MaybeActDevType } ).





% Subsection for the encoding of packets of the Common Command type.


% Finer section for the RPS telegrams, which include only the F6-* EEPs.



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
Tells whether the serial port is deemed available.

Might allow to detect freezes/crashes.
""".
-spec is_serial_available( oceanic_server_pid() ) -> boolean().
is_serial_available( OcSrvPid ) ->

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug( "Testing whether the serial port is available." ) ),

	OcSrvPid ! { testSerialAvailability, [], self() },
	receive

		onSerialAvailableReport ->
			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug( "Serial interface reported as available." ),
				ok ),
			true;

		onSerialNotAvailableReport ->
			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:error(
					"Serial interface reported as not available." ),
				ok ),

			false

	end.



-doc """
Restarts the serial USB interface used internally.

Such a reset might be useful to avoid an USB freeze that may happen after a few
weeks. However then a second phenomenon may happen (e.g. after a duration of 10
days): despite these resets, serial does not seem then to receive telegrams
anymore, whereas the TTY seems to propagate them properly (based on `od -x <
/dev/ttyUSBEnOcean`), whereas our server and Oceanic do remain fully responsive.
""".
-spec restart_serial_interface( oceanic_server_pid() ) -> void().
restart_serial_interface( OcSrvPid ) ->

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug( "Triggering a restart of the serial interface." ) ),

	OcSrvPid ! { restartSerialInterface, [], self() },
	receive

		serial_restarted ->
			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug( "Serial interface restarted." ),
				ok )

	end.



-doc """
Main loop of the Oceanic server.

There may be:

- remaining content from past, unsupported packet types that is still to be
skipped (hence ToSkipLen; finer than only searching for start bytes)

- any already-received beginning of the current telegram to be taken into
account (hence MaybeTelTail - which never includes the starting byte); we have
to discriminate the value of MaybeTelTail between:

 - "nothing already read" (no start byte already found, no tail) - then it is
   equal to undefined

 - "only the start byte was read (and chopped)" (empty tail) - then it is equal
   to <<>>
""".
-spec oceanic_loop( count(), option( telegram_tail() ), oceanic_state() ) ->
											no_return().
oceanic_loop( ToSkipLen, MaybeTelTail, State ) ->

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

			TelTailStr = case MaybeTelTail of

				undefined ->
					"no chunk";

				<<>> ->
					"just the start byte";

				TelTail ->
					text_utils:format( "a tail of ~B bytes (~p) after "
						"the start byte", [ size( TelTail ), TelTail ] )

			end,

			% To denote a limit between processings (explicit timestamp added,
			% to be available also in erlang.log.* files):
			%
			trace_bridge:info_fmt( "### Waiting at ~ts for any message "
				"including a telegram chunk, whereas having ~ts to skip, "
				"and having accumulated ~ts.",
				[ time_utils:get_textual_timestamp(), SkipStr,
				  text_utils:ellipse( TelTailStr, _MaxLen=120 ) ] )

		end ),


	% Useful to detect any ever-increasing accumulated tail, which would be the
	% sign that the decoding logic got stuck for good:
	%
	TelTailSizeThreshold = 50,

	MaybeTelTail =:= undefined orelse
		begin
			Size = size( MaybeTelTail ),
			Size > TelTailSizeThreshold andalso
				trace_bridge:error_fmt( "Abnormally-long accumulated telegram "
					"tail (~B bytes); decoding logic stuck? Tail: ~n~p",
					[ Size, MaybeTelTail ] )
		end,

	receive

		% Received data from the serial port:
		{ data, NewChunk } ->

			NewChunkSize = size( NewChunk ),

			% Monitoring of all receivings may be done to investigate any loss
			% of communication after a long time:
			%
			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:notice_fmt( "Received a telegram chunk "
					"of ~B bytes: ~w, corresponding to hexadecimal ~ts "
					"(whereas there are ~B bytes to skip).",
					[ NewChunkSize, NewChunk,
					  telegram_to_hexastring( NewChunk ), ToSkipLen ] ) ),

			JamState = monitor_jamming( NewChunkSize, State ),

			% Note that more than one telegram may be stored in a received data
			% chunk; to avoid any accumulation, each current chunk should be
			% decoded as much as possible (not just the first telegram):
			%
			{ IntegToSkipLen, IntegMaybeTelTail, IntegState } =
				integrate_all_telegrams( ToSkipLen, MaybeTelTail, NewChunk,
										 JamState ),

			oceanic_loop( IntegToSkipLen, IntegMaybeTelTail, IntegState );


		{ sendDoubleRockerTelegram, [ ActEurid, COTS,
				TrackSpec={ DevEvType, MaybeExpectedReportedEvInfo } ] } ->

			trace_bridge:debug_fmt( "Server to send a double-rocker telegram "
				"to ~ts, ~ts, with track specification ~w.",
				[ describe_device( ActEurid, State ),
				  canon_outgoing_trigger_spec_to_string( COTS ), TrackSpec ] ),

			BaseEurid = State#oceanic_state.emitter_eurid,

			% Note that this results in the target button of the target rocker
			% to undergo a single transition (generally 'pressed'), not a double
			% one (e.g. 'pressed' then 'released'), as it showed sufficient to
			% trigger all tested actuators:
			%
			Telegram = encode_double_rocker_telegram( BaseEurid, COTS,
													  ActEurid ),

			NewState = send_raw_telegram( Telegram, State ),

			% TODO: register and monitor TrackInfo
			_TrackInfo = { _From=ActEurid, DevEvType, MaybeExpectedReportedEvInfo,
						   State#oceanic_state.trigger_retry_count },

			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		{ onSerialMessage, Msg } ->
			trace_bridge:warning( text_utils:ensure_string( Msg ) ),
			oceanic_loop( ToSkipLen, MaybeTelTail, State );


		% To track lost devices:
		{ onActivityTimeout, LostEurid, PeriodicityMs } ->

			DeviceTable = State#oceanic_state.device_table,

			NewDeviceTable =
					case table:lookup_entry( LostEurid, DeviceTable ) of

				key_not_found ->
					% Really abnormal:
					trace_bridge:error_fmt( "A sensor whose EURID is '~ts' was "
						"reported as lost whereas it is not known.",
						[ eurid_to_string( LostEurid ) ] ),
					% This EURID is not specifically registered.
					DeviceTable;

				{ value, LostDevice } ->

					cond_utils:if_defined( oceanic_debug_activity,
						trace_bridge:debug_fmt(
							"Activity time-out (after ~ts) for device ~ts.",
							[ time_utils:duration_to_string( PeriodicityMs ),
							  device_to_string( LostDevice ) ] ) ),

					IsNewLoss =
						LostDevice#enocean_device.availability =:= online,

					LostMsg = { onEnoceanDeviceLost, [ LostEurid,
						LostDevice#enocean_device.name,
						get_device_description( LostDevice ),
						IsNewLoss, LostDevice#enocean_device.last_seen,
						PeriodicityMs, self() ] },

					[ LPid ! LostMsg
						|| LPid <- State#oceanic_state.event_listeners ],

					MaybeNewTimerRef = reset_timer(
						LostDevice#enocean_device.activity_timer, LostEurid,
						LostDevice#enocean_device.expected_periodicity,
						LostDevice#enocean_device.first_seen,
						LostDevice#enocean_device.telegram_count,
						LostDevice#enocean_device.error_count,
						_Now=time_utils:get_timestamp() ),

					% Periodicity not updated:
					NewLostDevice = LostDevice#enocean_device{
						availability=lost,
						activity_timer=MaybeNewTimerRef },

					table:add_entry( LostEurid, NewLostDevice, DeviceTable )

			end,

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		{ executeCommand, CmdTelegram, RequesterPid } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Requested to execute command as ~ts, "
					"on behalf of requester ~w.",
					[ telegram_to_string( CmdTelegram ), RequesterPid ] ) ),

			ExecState = execute_command_helper( _CmdType=device_command,
				CmdTelegram, RequesterPid, State ),

			oceanic_loop( ToSkipLen, MaybeTelTail, ExecState );


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

			oceanic_loop( ToSkipLen, MaybeTelTail, ExecState );


		{ getOceanicEurid, RequesterPid } ->
			RequesterPid !
				{ oceanic_eurid, State#oceanic_state.emitter_eurid },

			oceanic_loop( ToSkipLen, MaybeTelTail, State );



		{ getDeviceDescription, Eurid, RequesterPid } ->

			BinDesc = describe_device( Eurid, State ),

			RequesterPid ! { oceanic_device_description, BinDesc },

			oceanic_loop( ToSkipLen, MaybeTelTail, State );


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

			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		{ addEventListener, ListenerPid } ->

			trace_bridge:info_fmt( "Adding Enocean event listener ~w.",
								   [ ListenerPid ] ),

			NewListeners = [ ListenerPid | #oceanic_state.event_listeners ],

			NewState = State#oceanic_state{ event_listeners=NewListeners },

			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		{ removeEventListener, ListenerPid } ->

			trace_bridge:info_fmt( "Removing Enocean event listener ~w.",
								   [ ListenerPid ] ),

			PastListeners = State#oceanic_state.event_listeners,

			NewListeners = case list_utils:delete_if_existing( ListenerPid,
												PastListeners ) of

				not_found ->
					trace_bridge:error_fmt( "Requested to remove event "
						"listener ~w whereas not registered (hence ignored).",
						[ ListenerPid ] ),
					PastListeners;

				ShrunkListeners ->
					ShrunkListeners

			end,

			NewState = State#oceanic_state{ event_listeners=NewListeners },

			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		{ addConfigurationSettings, OcSettings } ->
			NewState = apply_conf_settings( OcSettings, State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		% So that the traces emitted by Oceanic thanks to trace_bridge:* are
		% integrated in a more advanced trace system (typically Ceylan-Traces):
		%
		{ registerTraceBridge, BridgeSpec } ->
			trace_bridge:register( BridgeSpec ),
			trace_bridge:info_fmt( "Just registered the trace bridge "
				"specification ~p.", [ BridgeSpec ] ),
			oceanic_loop( ToSkipLen, MaybeTelTail, State );


		% Mostly useful for testing purpose:
		{ sendOceanic, Telegram } ->
			NewState = send_raw_telegram( Telegram, State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		{ testSerialAvailability, [], SenderPid } ->

			State#oceanic_state.serial_server_pid ! report,

			RespMsg = receive

				{ onSerialMessage, _Msg= <<"Serial is functional.">> } ->
					onSerialAvailableReport

				  after 2000 ->
					onSerialNotAvailableReport

			end,

			SenderPid ! RespMsg,

			oceanic_loop( ToSkipLen, MaybeTelTail, State );


		{ restartSerialInterface, [], SenderPid } ->

			BinSerialPath = State#oceanic_state.device_path,
			SerialPid = State#oceanic_state.serial_server_pid,

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:info_fmt( "Restarting serial interface '~ts' "
					"(was ~w).", [ BinSerialPath, SerialPid ] ) ),

			SerialPid ! { stop, self() },

			% Warning, might freeze the Oceanic server:
			receive

				serial_stopped ->
					ok

			end,

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:info_fmt( "Past serial interface ~w successfully "
					"stopped, starting a new one after a short delay.",
					[ SerialPid ] ) ),

			% If ever that helped:
			timer:sleep( 1000 ),

			NewSerialPid = secure_tty( BinSerialPath ),

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:info_fmt( "Started again, now using serial "
					"interface ~w. ", [ NewSerialPid ] ) ),

			NewState = State#oceanic_state{ serial_server_pid=NewSerialPid },

			SenderPid ! serial_restarted,

			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		% Mostly useful for testing purpose:
		{ decodeOceanic, Telegram, SenderPid } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Requested to decode telegram ~ts.",
					[ telegram_to_string( Telegram ) ] ) ),

			% Not interfering with received bits (current state used for that,
			% but will not be affected):
			%
			Res = case try_integrate_next_telegram( _ToSkipLen=0,
					_MaybeTelTail=undefined, Telegram, State ) of

				{ decoded, DeviceEvent, _MaybeDiscoverOrigin, _IsBackOnline,
				  _MaybeDevice, _NewMaybeTelTail, _NewState } ->
					DeviceEvent;

				{ DecError, _NewToSkipLen, _NewMaybeTelTail, _NewState } ->
					DecError

			end,

			SenderPid ! { decoding_result, Res },

			% Strictly unaffected:
			oceanic_loop( ToSkipLen, MaybeTelTail, State );


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

			oceanic_loop( ToSkipLen, MaybeTelTail, State )

	end.



-doc "Detects and notifies any suspected jamming attempt.".
-spec monitor_jamming( byte_size(), oceanic_state() ) -> oceanic_state().
monitor_jamming( ChunkSize,
				 State=#oceanic_state{ traffic_level=TrafficLvl,
									   last_traffic_seen=LastTimestamp,
									   jamming_threshold=JamThreshold,
									   event_listeners=EventListeners } ) ->

	Now = time_utils:get_timestamp(),

	Dur = time_utils:get_duration( LastTimestamp, Now ),

	% Relatively exponential backoff, as:
	%  - same second: not reduced
	%  - halved if previous second, etc.
	%
	AggTrafficLvl = round( TrafficLvl / (Dur+1) ) + ChunkSize,

	NewTrafficLvl = case AggTrafficLvl > JamThreshold of

		true ->
			% We notify (if possible) and reset the monitored level:
			trace_bridge:alert_fmt( "The jamming detection threshold "
				"(~B bytes per second) has been reached (with ~B bytes per "
				"second); an attempt to saturate actuators may be "
				"in progress.", [ JamThreshold, AggTrafficLvl ] ),

			% PID sent mostly to discriminate between multiple Oceanic servers:
			JamMsg = { onEnoceanJamming, [ AggTrafficLvl, self() ] },

			[ LPid ! JamMsg || LPid <- EventListeners ],

			% Single notification per detection:
			0;

		false ->
			AggTrafficLvl

	end,

	State#oceanic_state{ traffic_level=NewTrafficLvl,
						 last_traffic_seen=Now }.




-doc """
Decodes and processes all the telegrams found, based on any current telegram
tail and on the specified new chunk.
""".
-spec integrate_all_telegrams( count(), option( telegram_tail() ),
			telegram_chunk(), oceanic_state() ) -> oceanic_state().

integrate_all_telegrams( ToSkipLen, MaybeTelTail=undefined, _Chunk= <<>>,
						 State ) ->
	{ ToSkipLen, MaybeTelTail, State };

integrate_all_telegrams( ToSkipLen, MaybeTelTail, Chunk, State ) ->

	case try_integrate_next_telegram( ToSkipLen, MaybeTelTail, Chunk, State ) of

		% Commands have been already answered directly, no response echoed to
		% any listener:
		%
		{ decoded, _Event=command_processed, _MaybeDiscoverOrigin,
		  _IsBackOnline, _MaybeDevice, NextMaybeTelTail, NewState } ->

			cond_utils:if_defined( oceanic_debug_decoding, trace_bridge:debug(
				"Decoded command_processed event." ) ),

			% Just recurse on this tail (no chunk to append):
			integrate_all_telegrams( _SkipLen=0, NextMaybeTelTail, _Chunk= <<>>,
									 NewState );

		% Then just an event, possibly listened to:
		{ decoded, Event, MaybeDiscoverOrigin, IsBackOnline, MaybeDevice,
		  NextMaybeTelTail, NewState } ->

			cond_utils:if_defined( oceanic_debug_decoding,
				begin
					DiscStr = case MaybeDiscoverOrigin of

						undefined ->
									"";

						Origin ->
							text_utils:format( " (discover origin: ~ts)",
											   [ Origin ] )

					end,
					trace_bridge:debug_fmt( "Decoded following event~ts: ~ts.",
						[ DiscStr, device_event_to_string( Event ) ] )

				end ),

			EventSrcEurid = get_source_eurid( Event ),

			% Reducing noise, by not relaying the telegrams that it received
			% (possibly through repeating) yet that were presumably sent by this
			% gateway itself:
			%
			case EventSrcEurid =:= State#oceanic_state.emitter_eurid of

				true ->
					%trace_bridge:debug_fmt( "Skipping following self-emitted "
					%   "event: ~ts", [ device_event_to_string( Event ) ] ),

					integrate_all_telegrams( _SkipLen=0, NextMaybeTelTail,
											 _Chunk= <<>>,  NewState );

				false ->

					DeviceMsg = case MaybeDiscoverOrigin of

						% Most common case (already detected, hence not set):
						undefined ->
							BackOnlineInfo = case IsBackOnline of

								true ->
									case MaybeDevice of

										undefined ->
											throw( { unexpected_undefined_device,
													 Event } );

										Device ->
											get_device_description( Device )

									end;

								false ->
									undefined

							end,

						{ onEnoceanDeviceEvent,
							[ Event, BackOnlineInfo, self() ] };


					% Set, hence just detected:
					DiscoverOrigin ->

						BinDesc = case MaybeDevice of

							undefined ->
								throw( { unexpected_undefined_device, Event } );

							Device ->
								get_device_description( Device )

						end,

						DevMsgType = case DiscoverOrigin of

							configuration ->
								onEnoceanConfiguredDeviceFirstSeen;

							% Detected, yet not in configuration:
							listening ->
								onEnoceanDeviceDiscovery;

							teaching ->
								onEnoceanDeviceTeachIn

						end,

						{ DevMsgType, [ Event, BinDesc, self() ] }

				end,

				[ LPid ! DeviceMsg
					|| LPid <- NewState#oceanic_state.event_listeners ],

				integrate_all_telegrams( _SkipLen=0, NextMaybeTelTail,
										 _Chunk= <<>>,  NewState )

			end;


		% Now the unlucky cases; first the ones that may result in extra tail to
		% decode:
		%
		{ DecodingError, NewToSkipLen, NextMaybeTelTail, NewState }
			when DecodingError =:= unsupported
				 orelse DecodingError =:= unconfigured
				 orelse DecodingError =:= invalid ->
			integrate_all_telegrams( NewToSkipLen, NextMaybeTelTail,
									 _Chunk= <<>>, NewState );


		% Then the others, which are the only unlucky possible stops of the
		% ongoing recursion:
		%
		{ DecodingError, NewToSkipLen, NextMaybeTelTail, NewState }
				when DecodingError =:= not_reached
					 orelse DecodingError =:= incomplete  ->
			{ NewToSkipLen, NextMaybeTelTail, NewState }

	end.




-doc """
Requests to execute the specified telegram-based command, by queueing it.
""".
-spec execute_command_helper( command_type(), telegram(), requester(),
							  oceanic_state() ) -> oceanic_state().
execute_command_helper( CmdType, CmdTelegram, RequesterPid,
						State=#oceanic_state{ command_queue=CmdQueue } ) ->

	CmdReq = #command_request{ command_type=CmdType,
							   command_telegram=CmdTelegram,
							   requester=RequesterPid },

	ExpandedQueue = queue:in( CmdReq, CmdQueue ),

	handle_next_command( ExpandedQueue, State ).



-doc """
Handles, if appropriate, the sending of the next command, using the specified
queue for that.
""".
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



-doc "Sends from the Oceanic server the specified telegram.".
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



-doc "Helper introduced only to make the decoding logic available for tests.".
-spec test_decode( telegram_chunk() ) -> decoding_outcome().
test_decode( Chunk ) ->
	try_integrate_next_telegram( _ToSkipLen=0, _MaybeTelTail=undefined, Chunk,
								 get_test_state() ).



-doc """
Returns a pseudo-state, loaded from default configuration; only useful for some
tests.

Note that, in order that an encoding or decoding (non-sending, non-receiving)
test can work without any actual device, we create here a type-correct yet
incorrect state (no real base ID, no relevant serial server).

So thise resulting state, which does not need an actual USB gateway to be
available, is mostly bogus.
""".
-spec get_test_state() -> oceanic_state().
get_test_state() ->

	TestState = #oceanic_state{
		serial_server_pid=self() ,
		emitter_eurid=string_to_eurid( ?default_emitter_eurid ),
		device_table=table:new(),
		command_queue=queue:new(),
		last_traffic_seen=time_utils:get_timestamp() },

	load_configuration( TestState ).



-doc """
Returns a pseudo-state, based on the specified device table; only useful for
some tests.
""".
-spec get_test_state( device_table() ) -> oceanic_state().
get_test_state( DeviceTable ) ->

	% Normally there is a real serial server:
	BaseState = get_test_state(),

	BaseState#oceanic_state{ device_table=DeviceTable }.



-doc """
Returns the device state in the specified state; only useful for some tests.
""".
-spec get_device_table( oceanic_state() ) -> device_table().
get_device_table( #oceanic_state{ device_table=DeviceTable } ) ->
	DeviceTable.



-doc """
Tries to integrate a new telegram chunk, that is to decode an ESP3 packet from
the specified chunk.
""".
-spec try_integrate_next_telegram( count(), option( telegram_chunk() ),
			telegram_chunk(), oceanic_state() ) -> decoding_outcome().

% Special-casing "nothing left to skip " is clearer; no start byte was already
% chopped.
%
% May happen (at least initially).
%
try_integrate_next_telegram( _ToSkipLen=0, _MaybeTelTail=undefined, NewChunk,
							 State ) ->
	try_decode_chunk( NewChunk, State );

% Still bytes to skip, and therefore still before any start byte is detected and
% chopped:
%
try_integrate_next_telegram( ToSkipLen, _MaybeTelTail=undefined, NewChunk, State ) ->

	ChunkSize = size( NewChunk ),

	case ToSkipLen - ChunkSize of

		% Not having reached a new relevant packet yet:
		StillToSkip when StillToSkip >= 0 ->
			{ not_reached, StillToSkip, _NoNextTelTail=undefined, State };

		% ChunkSize > ToSkipLen, so the next packet already started in this new
		% chunk.
		%
		% This will start by scanning for any start byte:
		_ ->
			<<_Skipped:ToSkipLen/binary, TargetChunk/binary>> = NewChunk,
			try_decode_chunk( TargetChunk, State )

	end;

% Here there is already an accumulated chunk as a tail, hence its start byte was
% already detected and chopped:
%
try_integrate_next_telegram( _ToSkipLen=0, TelTail, NewChunk, State ) ->
	% Start byte was already chopped from TelTail:
	MoreCompleteTelTail = <<TelTail/binary, NewChunk/binary>>,
	decode_after_start_byte( MoreCompleteTelTail, State ).

% Not expecting ToSkipLen>0 and MaybeTelTail =/= undefined (a function clause
% would be triggered anyway).



-doc """
Tries to decode the specified telegram chunk (any prior byte-skipping needed
having been already done; no start byte having been already chopped), and
returns the outcome.

Incomplete chunks may be completed later by next receivings (hence are kept,
from their first start byte included, whereas invalid ones are dropped (until
any start byte is found).
""".
-spec try_decode_chunk( telegram_chunk(), oceanic_state() ) ->
								decoding_outcome().
try_decode_chunk( TelegramChunk, State ) ->

	% (an additional source of inspiration can be [PY-EN], in
	% enocean/protocol/packet.py, the parse_msg/1 method)

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Trying to decode ~w (of size ~B bytes)",
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

			{ invalid, _StillToSkipLen=0, _NoTelTail=undefined, State };


		{ NewTelTail, DroppedCount } ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Start byte found, retaining now "
					"following telegram tail (of size ~B bytes; "
					"after dropping ~B byte(s)):~n  ~p.",
					[ size( NewTelTail ), DroppedCount, NewTelTail ] ),
				basic_utils:ignore_unused( DroppedCount ) ),

			decode_after_start_byte( NewTelTail, State )

	end.



-doc """
Scans the specified telegram tail (knowing that the corresponding chunk used to
begin with a start byte, which has already been chopped).
""".
decode_after_start_byte( NewTelTail, State ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Examining now following tail of ~B bytes:~n "
			"~p.", [ size( NewTelTail ),  NewTelTail ] ) ),

	% This tail corresponds to a telegram that is invalid, or unsupported, or
	% (currently) truncated, or valid (hence decoded):
	%
	case NewTelTail of

		% First 32 bits available (header) and first CRC as well:
		<<Header:4/binary, HeaderCRC, Rest/binary>> ->
			examine_header( Header, HeaderCRC, Rest, NewTelTail, State );

		% So less than 5 bytes (yet), cannot be complete, but is kept:
		_ ->
			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug( "(no complete header to decode)" ) ),

			% Waiting to concatenate any additional receiving:
			{ incomplete, _ToSkipLen=0, NewTelTail, State }

	end.



-doc """
Extracts the content ib the specified telegram chunk, returning any telegram
tail beginning just after any start byte (which is 0x55, i.e. 85).

Often there are no leading bytes to be dropped.

Refer to [ESP3] "1.6 UART synchronization (start of packet detection)".
""".
-spec scan_for_packet_start( telegram_chunk() ) ->
				{ telegram_tail() | 'no_content', DropCount :: count() }.
scan_for_packet_start( TelegramChunk ) ->
	scan_for_packet_start( TelegramChunk, _DropCount=0 ).


% (helper)
scan_for_packet_start( _Chunk= <<>>, DropCount ) ->
	{ no_content, DropCount };

scan_for_packet_start( _Chunk= <<?sync_byte, RemainingChunk/binary>>,
					   DropCount ) ->
	% No need to keep/include the start byte: repeated decoding attempts may
	% have to be made, yet any acc'ed chunk is a post-start telegram chunk:
	%
	{ _NewTelTail=RemainingChunk, DropCount };

% Skip all bytes before first start byte:
scan_for_packet_start( _Chunk= <<_OtherNonSyncByte, T/binary>>, DropCount ) ->
	scan_for_packet_start( T, DropCount+1 ).



-doc "Checks the telegram header and decodes all next elements accordingly.".
-spec examine_header( esp3_header(), crc(), telegram_chunk(), telegram_tail(),
					  oceanic_state() ) -> decoding_outcome().
examine_header( Header= <<DataLen:16, OptDataLen:8, PacketTypeNum:8>>,
				HeaderCRC, Rest, FullTelTail, State ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Packet type ~B; expecting ~B bytes of data, "
			"then ~B of optional data; checking first header CRC.",
			[ PacketTypeNum, DataLen, OptDataLen ] ) ),

	case compute_crc( Header ) of

		HeaderCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Header CRC validated (~B).",
										[ HeaderCRC ] ) ),

			% +1 for the FullDataCRC size, after both data:
			ExpectedRestSize = DataLen + OptDataLen + 1,

			case get_packet_type( PacketTypeNum ) of

				undefined ->
					% Not able to decode, no need to try to decode its content
					% as a new telegram:

					SkipLen = ExpectedRestSize,

					trace_bridge:warning_fmt( "Unknown packet type (~B), "
						"dropping corresponding content "
						"(hence ~B bytes to be skipped).",
						[ PacketTypeNum, SkipLen ] ),

					% Not wanting to drop also the content of any next telegram:
					case Rest of

						<<_PacketContent:SkipLen/binary, NextChunk/binary>> ->
							% We already skipped what was needed; we have the
							% next chunk, but we need any corresponding tail, so:
							%
							NextMaybeTelTail = get_maybe_next_tail( NextChunk ),

							{ unsupported, _SkipLen=0, NextMaybeTelTail, State };

						% Rest too short here; so we have to skip more than this
						% chunk:
						%
						_ ->
							StillToSkip = SkipLen - size( Rest ),
							{ not_reached, StillToSkip, _NoTelTail=undefined,
							  State }

					end;


				PacketType ->

					cond_utils:if_defined( oceanic_debug_decoding,
						trace_bridge:debug_fmt( "Detected packet type: ~ts.",
												[ PacketType ] ) ),

					ActualRestSize = size( Rest ),

					case ActualRestSize < ExpectedRestSize of

						% Not having received enough yet (will be decoded again,
						% once at least partially completed next):
						%
						true ->
							{ incomplete, _SkipLen=0, FullTelTail, State };

						% We have at least enough, let's extract these elements
						% and separate them from any next bytes:
						%
						false ->

							<<Data:DataLen/binary, OptData:OptDataLen/binary,
							  FullDataCRC:8, NextChunk/binary>> = Rest,

							% Let's decode first the current packet.

							% This CRC corresponds to the whole FullData, we
							% extract it (again) rather than concatenating
							% <<Data/binary, OptData/binary>>:

							FullLen = DataLen + OptDataLen,

							% May happen; e.g. if two telegrams overlap:
							case Rest of

								% Post-telegram not to be lost (the second
								% /binary is crucial to match any size):
								%
								<<FullData:FullLen/binary, _NextChunk/binary>> ->
									examine_full_data( FullData, FullDataCRC,
										Data, OptData, PacketType,
										FullTelTail, NextChunk, State );

								% Not expected to *ever* happen:
								TooShortChunk ->
									cond_utils:if_defined(
										oceanic_debug_decoding,
										trace_bridge:debug_fmt(
											"Chunk ~p too short.",
											[ TooShortChunk ] ),
										basic_utils:ignore_unused(
											TooShortChunk ) ),

									% By design start byte already chopped:
									{ incomplete, _ToSkipLen=0, FullTelTail,
									  State }


							end

					end

			end;

		OtherHeaderCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Obtained other header CRC (~B), "
					"dropping this telegram candidate.", [ OtherHeaderCRC ] ),
				basic_utils:ignore_unused( OtherHeaderCRC ) ),

			% Rather than discarding this telegram tail as a whole, tries to
			% scavage (very conservatively) any trailing element by extracting
			% from it any potentially valid new telegram tail - knowing that the
			% prior start byte has already been chopped; so we go here for any
			% next one (and thus at least one of byte has been consumed in the
			% process - no infinite decoding loop):
			%
			NextMaybeTelTail = get_maybe_next_tail( _NextChunk=FullTelTail ),

			{ invalid, _ToSkipLen=0, NextMaybeTelTail, State }

	end.



-doc "Further checks and decodes a telegram now that its type is known.".
-spec examine_full_data( telegram_chunk(), crc(), telegram_data(),
	telegram_opt_data(), packet_type(), telegram_tail(), telegram_chunk(),
	oceanic_state() ) -> decoding_outcome().
examine_full_data( FullData, ExpectedFullDataCRC, Data, OptData, PacketType,
				   FullTelTail, NextChunk, State ) ->

	case compute_crc( FullData ) of

		ExpectedFullDataCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Full-data CRC validated (~B).",
										[ ExpectedFullDataCRC ] ) ),

			% First point where it can be done once for all:
			NextMaybeTelTail = get_maybe_next_tail( NextChunk ),

			decode_packet( PacketType, Data, OptData, NextMaybeTelTail, State );


		OtherCRC ->
			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Obtained unexpected full-data CRC "
					"(~B, instead of ~B), dropping candidate telegram.",
					[ OtherCRC, ExpectedFullDataCRC ] ),
				basic_utils:ignore_unused( OtherCRC ) ),

			% Not expecting being fooled by data accidentally looking like a
			% legit CRC'ed header, so supposing this is just a valid telegram
			% that ended up being corrupted; yet for extra safety we will
			% restart the decoding from the very first possible byte, the one
			% after the last (chopped) start byte, i.e. from the beginning of
			% this tail (so we are nevertheless still progressing - not wanting
			% to recurse infinitely on a chunk):
			%
			% (NextChunk already in that tail)
			scan_for_packet_start( _Chunk=FullTelTail )

	end.



-doc """
Returns the telegram tail (if any) that can be obtained from the specified
chunk.
""".
-spec get_maybe_next_tail( telegram_chunk() ) -> option( telegram_tail() ).
get_maybe_next_tail( Chunk ) ->
	case scan_for_packet_start( Chunk ) of

		{ no_content, _DropCount } ->
			undefined;

		{ TelTail, _DropCount } ->
			TelTail

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


% Here a response is received whereas no request was sent:
%
% (note that some smart plugs, like at least the ELTAKO FSSAF-230V, emit, once
% triggered, a <<85,0,1,0,2,101,0,0>> telegram, carrying in terms of information
% only that it is a response_type packet with a payload of <<0>>; hence no
% emitter EURID available, for example - instead of sending a
% smart_plug_status_report_event)
%
decode_packet( _PacketType=response_type, Data, OptData, NextMaybeTelTail,
			   State=#oceanic_state{ waited_command_info=undefined,
									 discarded_count=DiscCount } ) ->

	trace_bridge:warning_fmt( "Received a command response "
		"(data: ~w, optional data: ~w) whereas there is no pending request, "
		"dropping it.", [ Data, OptData ] ),

	{ decoded, _MaybeDeviceEvent=command_processed,
	  _MaybeDiscoverOrigin=undefined, _IsBackOnline=false,
	  _MaybeDevice=undefined, NextMaybeTelTail,
	  State#oceanic_state{ discarded_count=DiscCount+1 } };


% Response received, presumably for this pending (possibly internal) command:
decode_packet( _PacketType=response_type,
			   _Data= <<ReturnCode:8, DataTail/binary>>, OptData, NextMaybeTelTail,
			   State=#oceanic_state{
					waited_command_info={ WaitedCmdReq, MaybeTimerRef },
					command_count=CmdCount } ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a command response, whereas "
			"awaiting ~ts.", [ command_request_to_string( WaitedCmdReq ) ] ) ),

	% In all cases the pending request is over:
	stop_any_timer( MaybeTimerRef ),

	RespState = State#oceanic_state{ waited_command_info=undefined },

	case oceanic_generated:get_maybe_first_for_return_code( ReturnCode ) of

		undefined ->
			trace_bridge:warning_fmt( "Unable to decode response whose return "
				"code is invalid (~B), dropping packet and pending "
				"command (#~B).", [ ReturnCode, CmdCount ] ),
			{ invalid, _ToSkipLen=0, NextMaybeTelTail, RespState };


		ok_return ->
			decode_response_tail( WaitedCmdReq, DataTail, OptData,
								  NextMaybeTelTail, RespState );

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
					{ invalid, _ToSkipLen=0, NextMaybeTelTail, RespState };

				RequesterPid ->

					RequesterPid !
						{ oceanic_command_outcome, _Outcome=FailureReturn },

					% Waiting information already cleared:
					{ decoded, command_processed,
					  _MaybeDiscoverOrigin=undefined, _IsBackOnline=false,
					  _MaybeDevice=undefined, NextMaybeTelTail, RespState }

			end

	end;


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
button; pressing one sends a double_rocker_switch_event() telling that a given
button (possibly both) is/are being pressed, and releasing it/them sends a
double_rocker_multipress_event() telling "no button released simultaneously"

Support to be added:
- F6-03: Rocker Switch, 4 Rocker
- F6-04: Position Switch, Home and Office Application
- F6-05: Detectors
- F6-10: Mechanical Handle

Discussed a bit in [ESP3] "2.1 Packet Type 1: RADIO_ERP1", p.18, and in
[EEP-spec] p.11.

See decode_1bs_packet/3 for more information.

DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
""".
-spec decode_rps_packet( telegram_data_tail(), telegram_opt_data(),
		option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
decode_rps_packet( _DataTail= <<DB_0:1/binary, SenderEurid:32,
				   Status:1/binary>>, OptData, NextMaybeTelTail,
				   State=#oceanic_state{ device_table=DeviceTable } ) ->

	% We have to know the specific EEP of this device in order to decode this
	% telegram:
	%
	case table:lookup_entry( SenderEurid, DeviceTable ) of

		% Device first time seen:
		key_not_found ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
			  _ListeningDiscoverOrigin, _IsBackOnline, _UndefinedDeviceName,
			  _MaybeEepId } = record_device_failure( SenderEurid, DeviceTable ),

			trace_bridge:warning_fmt( "Unable to decode a RPS (F6) packet "
				"from device whose EURID is ~ts: device not configured, "
				"no EEP known for it.",
				[ eurid_to_string( SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, NextMaybeTelTail, NewState };


		% Knowing the actual EEP is needed in order to decode:
		{ value, _Device=#enocean_device{ eep=undefined } } ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
			  _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
			  _UndefinedEepId } =
					record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a RPS packet "
				"for ~ts: no EEP known for it.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, NextMaybeTelTail, NewState };


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

		{ value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
			  _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
			  _UnsupportedEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:warning_fmt( "Unable to decode a RPS (F6) packet "
				"for ~ts: EEP ~ts (~ts) not supported.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ),
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

Discussed a bit in [ESP3] "2.1 Packet Type 1: RADIO_ERP1", p.18, and in
[EEP-spec] p.15.
""".
-spec decode_rps_push_button_packet( telegram_chunk(), eurid(),
		telegram_chunk(), telegram_opt_data(), option( telegram_tail() ),
		enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_rps_push_button_packet( DB_0= <<DB_0AsInt:8>>, SenderEurid,
		Status, OptData, NextMaybeTelTail, Device,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	ButtonTransition = case DB_0AsInt band ?b4 =:= 0 of

		true ->
			released;

		false ->
			pressed

	end,

	% (no learn bit in DB_0)

	% All other bits than b4 shall be 0:
	cond_utils:assert( oceanic_check_decoding,
					   % Superfluous parentheses:
					   DB_0AsInt band ( bnot ?b4 ) =:= 0 ),

	{ PTMSwitchModuleType, NuType, RepCount } = get_rps_status_info( Status ),

	% EEP was known, hence device was already known as well:
	{ NewDeviceTable, NewDevice, Now, MaybeLastSeen, UndefinedDiscoverOrigin,
	  IsBackOnline, MaybeDeviceName, MaybeEepId } =
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

	Event = #push_button_switch_event{ source_eurid=SenderEurid,
									   name=MaybeDeviceName,
									   eep=MaybeEepId,
									   timestamp=Now,
									   last_seen=MaybeLastSeen,
									   subtelegram_count=MaybeTelCount,
									   destination_eurid=MaybeDestEurid,
									   dbm=MaybeDBm,
									   security_level=MaybeSecLvl,
									   transition=ButtonTransition },

	{ decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
	  NextMaybeTelTail, NewState }.



-doc """
Decodes a rorg_rps F6-02-01, "Light and Blind Control - Application Style 1 or
2" packet (switch or multipress).

It may contain 2 actions.

Discussed a bit in [ESP3] "2.1 Packet Type 1: RADIO_ERP1", p.18, and in
[EEP-spec] p.15.
""".
-spec decode_rps_double_rocker_packet( telegram_chunk(), eurid(),
		telegram_chunk(), telegram_opt_data(), option( telegram_tail() ),
		enocean_device(), oceanic_state() ) -> decoding_outcome().
decode_rps_double_rocker_packet( DB_0= <<_DB_0AsInt:8>>, SenderEurid,
		% (T21 is at offset 2, thus b5; NU at offset 3, thus b4)
		_Status= <<_:2, T21:1, NU:1, _:4>>, OptData,
		NextMaybeTelTail, Device=#enocean_device{ eep=EepId },
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	AppStyle = get_app_style_from_eep( EepId ),

	case { T21, NU } of

		{ _T21=1, _NU=1 } ->

			<<R1Enum:3, EB:1, R2Enum:3, SA:1>> = DB_0,

			FirstButtonLocator = get_button_locator( R1Enum, AppStyle ),

			ButtonTransition = get_button_transition( EB ),

			SecondButtonLocator = get_button_locator( R2Enum, AppStyle ),

			IsSecondActionValid = case SA of

				0 ->
					false;

				1 ->
					true

			end,

			% EEP was known, hence device already was known as well:
			{ NewDeviceTable, NewDevice, Now, MaybeLastSeen,
			  UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName,
			  MaybeEepId } = record_known_device_success( Device, DeviceTable ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			MaybeDecodedOptData = decode_optional_data( OptData ),

			{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
				resolve_maybe_decoded_data( MaybeDecodedOptData ),

			Event = #double_rocker_switch_event{
				source_eurid=SenderEurid,
				name=MaybeDeviceName,
				eep=MaybeEepId,
				timestamp=Now,
				last_seen=MaybeLastSeen,
				subtelegram_count=MaybeTelCount,
				destination_eurid=MaybeDestEurid,
				dbm=MaybeDBm,
				security_level=MaybeSecLvl,
				first_action_button=FirstButtonLocator,
				energy_bow=ButtonTransition,
				second_action_button=SecondButtonLocator,
				second_action_valid=IsSecondActionValid },

			{ decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
			  NextMaybeTelTail, NewState };


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
			  MaybeEepId } = record_known_device_success( Device, DeviceTable ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			MaybeDecodedOptData = decode_optional_data( OptData ),

			{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
				resolve_maybe_decoded_data( MaybeDecodedOptData ),

			Event = #double_rocker_multipress_event{
				source_eurid=SenderEurid,
				name=MaybeDeviceName,
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
			  _IsBackOnline, _MaybeDeviceName, _MaybeEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			trace_bridge:warning_fmt( "Unable to decode a RPS packet "
				"from device whose EURID is ~ts and EEP ~ts (~ts), "
				"as T21=~B and NU=~B.",
				[ eurid_to_string( SenderEurid ), EepId,
				  oceanic_generated:get_maybe_second_for_eep_strings( EepId ),
				  T21, NU ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unsupported, _ToSkipLen=0, NextMaybeTelTail, NewState }

	end.



-doc """
Actual decoding of responses to pending common commands.

Note that the return code has already been extracted, and corresponds to a
success.

The actual waiting information is expected to have been already cleared by the
caller.
""".
-spec decode_response_tail( command_request(), telegram_data_tail(),
							telegram_opt_data(), option( telegram_tail() ),
							oceanic_state() ) -> decoding_outcome().
% For co_rd_version:
decode_response_tail(
		#command_request{ command_type=co_rd_version,
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


decode_response_tail( #command_request{ command_type=co_rd_version }, DataTail,
					  _OptData= <<>>, NextMaybeTelTail, State ) ->

	trace_bridge:error_fmt( "Received a response to a pending co_rd_version "
		"common command with an invalid data tail (~ts).",
		[ DataTail, telegram_to_string( DataTail ) ] ),

	{ invalid, _ToSkipLen=0, NextMaybeTelTail, State };


% For co_rd_sys_log:
decode_response_tail( #command_request{ command_type=co_rd_sys_log,
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
decode_response_tail( #command_request{ command_type=co_rd_idbase,
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


decode_response_tail( #command_request{ command_type=co_rd_idbase }, DataTail,
					  OptData, NextMaybeTelTail, State ) ->

	trace_bridge:error_fmt( "Received a response to a pending co_rd_idbase "
		"common command with an invalid data tail (~ts) "
		"and/or optional data (~ts).",
		[ DataTail, telegram_to_string( DataTail ),
		  telegram_to_string( OptData ) ] ),

	{ invalid, _ToSkipLen=0, NextMaybeTelTail, State };


% Other common commands:
decode_response_tail( OtherCmdReq, DataTail, OptData, NextMaybeTelTail, State ) ->

	trace_bridge:error_fmt( "Responses to ~ts are currently "
		"unsupported (dropping response and waited request).",
		[ command_request_to_string( OtherCmdReq ) ] ),

	trace_bridge:debug_fmt( "Extra information: DataTail=~ts, OptData=~ts.",
		[ telegram_to_string( DataTail ), telegram_to_string( OptData ) ] ),

	{ unsupported, _ToSkipLen=0, NextMaybeTelTail, State }.



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
			[ device_event_to_string( Response ) ] ) ),

	% We return directly the response event in that case:
	{ decoded, Response, _MaybeDiscoverOrigin=undefined,
	  _IsBackOnline=false, _MaybeDevice=undefined, NextMaybeTelTail, State };


notify_requester( Response, RequesterPid, NextMaybeTelTail, State ) ->

	cond_utils:if_defined( oceanic_debug_commands,
		trace_bridge:debug_fmt( "Sending back to requester ~w "
		"the following response: ~ts.",
		[ RequesterPid, device_event_to_string( Response ) ] ) ),

	RequesterPid ! { oceanic_command_outcome, Response },

	{ decoded, command_processed, _MaybeDiscoverOrigin=undefined,
	  _IsBackOnline=false, _MaybeDevice=undefined, NextMaybeTelTail, State }.




% Section for decoding helpers.


% Could be a bijective topic as well:


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
	%{ _Channel=1, _Pos=top };
	{ _Channel=2, _Pos=bottom };

get_button_locator( _Enum=1, _AppStyle=2 ) ->
	% button_ao / button A, bottom
	%{ _Channel=1, _Pos=bottom };
	{ _Channel=2, _Pos=top };

get_button_locator( _Enum=2, _AppStyle=2 ) ->
	% button_bi / button B, top
	%{ _Channel=2, _Pos=top };
	{ _Channel=1, _Pos=bottom };

get_button_locator( _Enum=3, _AppStyle=2 ) ->
	% button_bo / button B, bottom
	%{ _Channel=2, _Pos=bottom }.
	{ _Channel=1, _Pos=top }.



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




-doc "Returns a textual description of the designated button.".
-spec button_designator_to_string( button_designator() ) -> ustring().
button_designator_to_string( button_ai ) ->
	"bottom A button";

button_designator_to_string( button_ao ) ->
	"top A button";

button_designator_to_string( button_bi ) ->
	"bottom B button";

button_designator_to_string( button_bo ) ->
	"top B button".



-doc "Returns a textual description of the located button.".
-spec button_locator_to_string( button_locator() ) -> ustring().
button_locator_to_string( { _Channel=1, _Pos=bottom } ) ->
	"bottom A button";

button_locator_to_string( { _Channel=1, _Pos=top } ) ->
	"top A button";

button_locator_to_string( { _Channel=2, _Pos=bottom } ) ->
	"bottom B button";

button_locator_to_string( { _Channel=2, _Pos=top } ) ->
	"top B button".



% Could be a bijective topic as well:


-doc "Returns the button transition corresponding to the specified energy bow.".
-spec get_button_transition( enum() ) -> button_transition().
get_button_transition( _EnergyBow=0 ) ->
	released;

get_button_transition( _EnergyBow=1 ) ->
	pressed.



-doc "Returns the enumeration of the specified button transition.".
-spec get_button_transition_enum( button_transition() ) -> enum().
get_button_transition_enum( released ) ->
	_EnergyBow=0;

get_button_transition_enum( pressed ) ->
	_EnergyBow=1.



-doc """
Decodes the RPS status byte, common to many RPS telegrams.

Refer to [EEP-spec] p.11 for further details.
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



-doc "Returns a textual description of the specified PTM switch module.".
-spec ptm_module_to_string( ptm_switch_module_type() ) -> ustring().
ptm_module_to_string( _ModType=ptm1xx ) ->
	% E.g. "PTM210 DB":
	"PTM1xx";

ptm_module_to_string( _ModType=ptm2xx ) ->
	"PTM2xx".



-doc """
Returns a textual description of the specified "Nu" Message type, as defined in
RPS packets.
""".
-spec nu_message_type_to_string( nu_message_type() ) ->  ustring().
nu_message_type_to_string( _Nu=normal ) ->
	"normal-message";

nu_message_type_to_string( _Nu=unassigned ) ->
	"unassigned-message";

nu_message_type_to_string( _Nu=unknown_type_2 ) ->
	"unknown message type (NU=2)";

nu_message_type_to_string( _Nu=unknown_type_3 ) ->
	"unknown message type (NU=3)".



-doc """
Decodes a rorg_1bs (D5) packet, that is a R-ORG telegram on one byte.

Discussed in [EEP-spec] p.27.

DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
""".
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

	{ NewDeviceTable, NewDevice, Now, MaybePrevLastSeen, MaybeDiscoverOrigin,
	  IsBackOnline, MaybeDeviceName, MaybeEepId } =
		record_device_success( SenderEurid, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a R-ORG 1BS packet, "
			"with a payload of ~B bytes (with DB_0=~w~ts; "
			"contact is ~ts), sender is ~ts, status is ~w~ts.",
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
Decodes a rorg_4bs (A5) packet, that is a R-ORG telegram on four bytes.

Discussed in [EEP-spec] p.12.

DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
""".
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
			  eurid_to_string( SenderEurid ), repeater_count_to_string( RC ),
			  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
			] ),
		basic_utils:ignore_unused( [ DataTail, RC, MaybeDecodedOptData ] ) ),

	% We have to know the specific EEP of this device in order to decode this
	% telegram:
	%
	case table:lookup_entry( SenderEurid, DeviceTable ) of

		key_not_found ->

			{ NewDeviceTable, _NewDevice, _Now, _MaybePrevLastSeen,
			  _MaybeDiscoverOrigin, _IsBackOnline, _MaybeDeviceName,
			  _MaybeEepId } = record_device_failure( SenderEurid, DeviceTable ),

			% Device first time seen:
			trace_bridge:warning_fmt( "Unable to decode a 4BS (A5) packet "
				"from device whose EURID is ~ts: device not configured, "
				"no EEP known for it.",	[ eurid_to_string( SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, NextMaybeTelTail, NewState };


		% Knowing the actual EEP is needed in order to decode:
		{ value, _Device=#enocean_device{ eep=undefined } } ->

			{ NewDeviceTable, _NewDevice, _Now, _MaybePrevLastSeen,
			  _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
			  _MaybeEepId } = record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a 4BS (A5) packet "
				"for ~ts: no EEP known for it.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, NextMaybeTelTail, NewState };


		{ value, Device=#enocean_device{ eep=thermo_hygro_low } } ->
			decode_4bs_thermo_hygro_low_packet( DB_3, DB_2, DB_1, DB_0,
				SenderEurid, OptData, NextMaybeTelTail, Device, State );


		{ value, _Device=#enocean_device{ eep=UnsupportedEepId } } ->

			{ NewDeviceTable, _NewDevice, _Now, _MaybePrevLastSeen,
			  _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
			  _UnsupportedEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a 4BS (A5F6) packet "
				"for ~ts: EEP ~ts (~ts) not supported.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ),
				  UnsupportedEepId,
				  oceanic_generated:get_maybe_second_for_eep_strings(
					UnsupportedEepId ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unsupported, _ToSkipLen=0, NextMaybeTelTail, NewState }

   end.



-doc """
Decodes a rorg_4bs (A5) packet for the thermo_hygro_low EEP ("A5-04-01"):
"Temperature and Humidity Sensor" (04), range 0°C to +40°C and 0% to 100% (01).

Refer to [EEP-spec] p.35.
""".
-spec decode_4bs_thermo_hygro_low_packet( uint8(), uint8(), uint8(), uint8(),
		eurid(), telegram_opt_data(), option( telegram_tail() ), enocean_device(),
		oceanic_state() ) -> decoding_outcome().
decode_4bs_thermo_hygro_low_packet( _DB_3=0, _DB_2=ScaledHumidity,
		_DB_1=ScaledTemperature, DB_0, SenderEurid, OptData, NextMaybeTelTail,
		Device, State=#oceanic_state{ device_table=DeviceTable } ) ->

	cond_utils:assert( oceanic_check_decoding, DB_0 band 2#11110101 =:= 0 ),

	%RelativeHumidity = round( ScaledHumidity / 250.0 * 100 ),
	RelativeHumidity = ScaledHumidity / 250.0 * 100,

	MaybeTemperature = case DB_0 band ?b1 =:= 0 of

		true ->
			undefined;

		false ->
			%round( ScaledTemperature / 250.0 * 40 )
			ScaledTemperature / 250.0 * 40

	end,

	LearnActivated = DB_0 band ?b3 =:= 0,

	{ NewDeviceTable, NewDevice, Now, MaybeLastSeen, UndefinedDiscoverOrigin,
	  IsBackOnline, MaybeDeviceName, MaybeEepId } =
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
				"packet, reporting a ~ts ~ts~ts; sender is ~ts~ts.",
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
								 last_seen=MaybeLastSeen,
								 subtelegram_count=MaybeTelCount,
								 destination_eurid=MaybeDestEurid,
								 dbm=MaybeDBm,
								 security_level=MaybeSecLvl,
								 relative_humidity=RelativeHumidity,
								 temperature=MaybeTemperature,

								 % As "A5-04-01":
								 temperature_range=low,

								 learn_activated=LearnActivated },

	{ decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
	  NextMaybeTelTail, NewState }.



-doc """
Decodes a rorg_ute (D4) packet, that is a R-ORG telegram for Universal
Teach-in/out, EEP based (UTE), one way of pairing devices.

Discussed in [EEP-gen] p.17; p.25 for the query and p.26 for the response.
""".
-spec decode_ute_packet( telegram_data_tail(), telegram_opt_data(),
			option( telegram_tail() ), oceanic_state() ) -> decoding_outcome().
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
		OptData, NextMaybeTelTail,
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

	{ NewDeviceTable, Device=#enocean_device{ name=MaybeName,
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
		basic_utils:ignore_unused(
		  [ PTMSwitchModuleType, NuType, RepCount ] ) ),

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

	{ decoded, Event, _DiscoverOrigin=teaching, _IsBackOnline=false, Device,
	  NextMaybeTelTail, NewState }.



-doc """
Decodes a rorg_vld (D2) packet, that is a R-ORG telegram containing Variable
Length Data.

VLD telegrams carry a variable payload between 1 and 14 bytes, depending on
their design.

Discussed in [EEP-gen] p.12.

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

			% Not trying to decode optional data then.

			{ NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
			  _ListeningDiscoverOrigin, _IsBackOnline, _UndefinedDeviceName,
			  _MaybeEepId } = record_device_failure( SenderEurid, DeviceTable ),

			trace_bridge:warning_fmt( "Unable to decode a VLD (D2) packet "
				"from device whose EURID is ~ts: device not configured, "
				"no EEP known for it.",	[ eurid_to_string( SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, NextMaybeTelTail, NewState };


		% Knowing the actual EEP is needed in order to decode:
		{ value, _Device=#enocean_device{ eep=undefined } } ->

			% Not trying to decode optional data then.

			{ NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
			  _MaybeDiscoverOrigin, _IsBackOnline, MaybeDeviceName,
			  _UndefinedEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a VLD packet "
				"for ~ts: no EEP known for it.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unconfigured, _ToSkipLen=0, NextMaybeTelTail, NewState };


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
			  _UnsupportedEepId } =
				record_device_failure( SenderEurid, DeviceTable ),

			% Device probably already seen:
			trace_bridge:debug_fmt( "Unable to decode a VLD (D2) packet "
				"for ~ts: EEP ~ts (~ts) not supported.",
				[ get_best_naming( MaybeDeviceName, SenderEurid ),
				  UnsupportedEepId,
				  oceanic_generated:get_maybe_second_for_eep_strings(
					UnsupportedEepId ) ] ),

			NewState = State#oceanic_state{ device_table=NewDeviceTable },

			{ unsupported, _ToSkipLen=0, NextMaybeTelTail, NewState }

	end.



-doc """
Decodes a packet emitted by a rorg_vld smart_plug (D2-01-0A, an "Electronic
switches and dimmers with Energy Measurement and Local Control" device of type
0A).

This corresponds to basic smart, non-metering plugs bidirectional actuators that
may control (switch on/off) most electrical loads (e.g. appliances); they do not
perform metering.

Discussed in [EEP-spec] p. 132.

Notably, if the command is equal to 0x4 (hence packet of type "Actuator Status Response", i.e. with CmdAsInt=16#4 ("CMD 0x4", 'actuator_status_response'; see
[EEP-spec] p. 135), it is an information sent (as a broadcast) by the smart plug
about its status (either after a status request or after a state change request
- whether or not it triggered an actual state change), typically to acknowledge
that a requested switching was indeed triggered.
""".
-spec decode_vld_smart_plug_packet( vld_payload(), eurid(), telegram_chunk(),
		telegram_opt_data(), option( telegram_tail() ), enocean_device(),
		oceanic_state() ) -> decoding_outcome().
decode_vld_smart_plug_packet(
		% 3 bytes (no OutputValue available):
		_Payload= <<PowerFailureEnabled:1, PowerFailureDetected:1, _:2,
					CmdAsInt:4, OverCurrentSwitchOff:1, ErrorLevel:2,
					_IOChannel:5, LocalControl:1, _OutputValue:7>>,
		SenderEurid, _Status, OptData, NextMaybeTelTail, Device,
		State=#oceanic_state{ device_table=DeviceTable } )
						when CmdAsInt =:= 16#4 ->

	% Mostly as decode_vld_smart_plug_with_metering_packet/7, except no
	% measurement.

	{ NewDeviceTable, NewDevice, Now, MaybeLastSeen,
	  UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName, MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },


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
			not_supported

	end,

	% IOChannel not decoded yet.

	IsLocalControlEnabled = case LocalControl of

		0 ->
			false;

		1 ->
			true

	end,

	MaybeCmd = oceanic_generated:get_second_for_vld_d2_00_cmd( CmdAsInt ),

	MaybeDecodedOptData = decode_optional_data( OptData ),

	cond_utils:if_defined( oceanic_debug_decoding,

		begin
			PFStr = interpret_power_failure( IsPowerFailureEnabled,
											 IsPowerFailureDetected ),

			OCStr = interpret_overcurrent_trigger(
				IsOverCurrentSwitchOffTrigger ),

			HardStr = interpret_hardware_status( HardwareStatus ),

			LocCtrlStr = interpret_local_control( IsLocalControlEnabled ),

			trace_bridge:debug_fmt( "Decoding a VLD smart plug packet "
				"for command '~ts' (~B); sender is ~ts; ~ts, ~ts, ~ts, ~ts~ts.",
				[ MaybeCmd, CmdAsInt,
				  get_best_naming( MaybeDeviceName, SenderEurid ), PFStr, OCStr,
				  HardStr, LocCtrlStr,
				  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
				] )

		end,

		basic_utils:ignore_unused( [ SenderEurid, MaybeCmd, MaybeDeviceName,
			 MaybeDecodedOptData, IsPowerFailureEnabled,
			 IsPowerFailureDetected ] ) ),

	{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
	   resolve_maybe_decoded_data( MaybeDecodedOptData ),

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
		output_power=undefined },

	{ decoded, Event, UndefinedDiscoverOrigin, IsBackOnline, NewDevice,
	  NextMaybeTelTail, NewState };


% For other, not supported yet, smart plug command packets:
decode_vld_smart_plug_packet(
		% 3 bytes:
		_Payload= <<_:4, CmdAsInt:5, _Rest/binary>>,
		SenderEurid, _Status, OptData, NextMaybeTelTail, Device,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	MaybeCmd = oceanic_generated:get_maybe_second_for_vld_d2_00_cmd( CmdAsInt ),

	% Actually is currently not managed:
	{ NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
	  _UndefinedDiscoverOrigin, _IsBackOnline, MaybeDeviceName, _MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	%{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
	%	resolve_maybe_decoded_data( MaybeDecodedOptData ),

	cond_utils:if_defined( oceanic_debug_decoding,

		begin
			trace_bridge:debug_fmt(
				"Partial decoding a VLD smart plug packet "
				"for command '~ts' (~B); sender is ~ts~ts.",
				[ MaybeCmd, CmdAsInt,
				  get_best_naming( MaybeDeviceName, SenderEurid ),
				  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
				] )
		end,

		basic_utils:ignore_unused( [ SenderEurid, MaybeCmd, MaybeDeviceName,
									 MaybeDecodedOptData ] ) ),

	{ unsupported, _SkipLen=0, NextMaybeTelTail, NewState }.




-doc """
Decodes a packet emitted by a rorg_vld smart_plug_with_metering (D2-01-0B, an
"Electronic switches and dimmers with Energy Measurement and Local Control"
device of type 0B).

This corresponds to smart, metering plugs bidirectional actuators that may
control (switch on/off) most electrical loads (e.g. appliances) and may report
metering information.

See decode_vld_smart_plug_packet/7 for extra details.

Discussed in [EEP-spec] p.143.
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
	  UndefinedDiscoverOrigin, IsBackOnline, MaybeDeviceName, MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

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
			not_supported

	end,

	% IOChannel not decoded yet.

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
			PFStr = interpret_power_failure( IsPowerFailureEnabled,
											 IsPowerFailureDetected ),

			OCStr = interpret_overcurrent_trigger(
				IsOverCurrentSwitchOffTrigger ),

			HardStr = interpret_hardware_status( HardwareStatus ),

			LocCtrlStr = interpret_local_control( IsLocalControlEnabled ),

			PowerStr = interpret_power_report( OutputPower ),

			trace_bridge:debug_fmt(
				"Decoding a VLD smart plug with metering packet "
				"for command '~ts' (~B); sender is ~ts; ~ts, ~ts, ~ts, "
				"~ts; this plug is ~ts~ts.",
				[ MaybeCmd, CmdAsInt,
				  get_best_naming( MaybeDeviceName, SenderEurid ), PFStr, OCStr,
				  HardStr, LocCtrlStr, PowerStr,
				  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
				] )

		end,

		basic_utils:ignore_unused( [ SenderEurid, MaybeCmd, MaybeDeviceName,
			 MaybeDecodedOptData, IsPowerFailureEnabled,
			 IsPowerFailureDetected ] ) ),

	{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
		resolve_maybe_decoded_data( MaybeDecodedOptData ),

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
	  NextMaybeTelTail, NewState };


% For other, not supported yet, smart plug with metering command packets:
decode_vld_smart_plug_with_metering_packet(
		% 3 bytes:
		_Payload= <<_:4, CmdAsInt:5, _Rest/binary>>,
		SenderEurid, _Status, OptData, NextMaybeTelTail, Device,
		State=#oceanic_state{ device_table=DeviceTable } ) ->

	MaybeCmd = oceanic_generated:get_maybe_second_for_vld_d2_00_cmd( CmdAsInt ),

	% Actually is currently not managed:
	{ NewDeviceTable, _NewDevice, _Now, _MaybeLastSeen,
	  _UndefinedDiscoverOrigin, _IsBackOnline, MaybeDeviceName, _MaybeEepId } =
		record_known_device_success( Device, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	MaybeDecodedOptData = decode_optional_data( OptData ),

	%{ MaybeTelCount, MaybeDestEurid, MaybeDBm, MaybeSecLvl } =
	%	resolve_maybe_decoded_data( MaybeDecodedOptData ),

	cond_utils:if_defined( oceanic_debug_decoding,

		begin
			trace_bridge:debug_fmt(
				"Partial decoding a VLD smart plug with metering packet "
				"for command '~ts' (~B); sender is ~ts~ts.",
				[ MaybeCmd, CmdAsInt,
				  get_best_naming( MaybeDeviceName, SenderEurid ),
				  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
				] )
		end,

		basic_utils:ignore_unused( [ SenderEurid, MaybeCmd, MaybeDeviceName,
									 MaybeDecodedOptData ] ) ),


	{ unsupported, _SkipLen=0, NextMaybeTelTail, NewState }.



-doc "Interprets the specified power failure information.".
-spec interpret_power_failure( boolean(), boolean() ) -> ustring().
interpret_power_failure( _IsPowerFailureEnabled=true,
						 IsPowerFailureDetected ) ->
	interpret_power_failure( IsPowerFailureDetected );

interpret_power_failure( _IsPowerFailureEnabled=false,
						 _IsPowerFailureDetected ) ->
	"no power failure detection enabled".



-doc "Interprets the specified power failure information.".
-spec interpret_power_failure( boolean() ) -> ustring().
interpret_power_failure( _IsPowerFailureDetected=true ) ->
	"a power failure was detected";

interpret_power_failure( _IsPowerFailureDetected=false ) ->
	"no power failure was detected".



-doc "Interprets the specified over-current switch off information.".
-spec interpret_overcurrent_trigger( boolean() ) -> ustring().
interpret_overcurrent_trigger( _IsOverCurrentSwitchOffTrigger=true ) ->
	"over-current switch was triggered";

interpret_overcurrent_trigger( _IsOverCurrentSwitchOffTrigger=false ) ->
	"no over-current has been detected".



-doc "Interprets the specified hardware status information.".
-spec interpret_hardware_status( hardware_status() ) -> ustring().
interpret_hardware_status( _HStatus=nominal ) ->
	"hardware status is nominal";

interpret_hardware_status( _HStatus=warning ) ->
	"hardware status reports a warning";

interpret_hardware_status( _HStatus=failure ) ->
	"hardware status is failed";

interpret_hardware_status( _HStatus=not_supported ) ->
	"hardware status is unknown".



-doc "Interprets the specified local control information.".
-spec interpret_local_control( boolean() ) -> ustring().
interpret_local_control( _LocCtrl=true ) ->
	"local control enabled";

interpret_local_control( _LocCtrl=false ) ->
	"no local control".


-doc "Interprets the specified power report information.".
-spec interpret_power_report( power_report() ) -> ustring().
interpret_power_report( _PwReport=off ) ->
	"is not powering (off)";

interpret_power_report( _PwReport=PInt ) when is_integer( PInt ) ->
	text_utils:format( "is powering at ~B%", [ PInt ] );

interpret_power_report( _PwReport ) ->
	"has an unknown powering status".



-doc "Returns a textual description of the specified temperature.".
-spec temperature_to_string( celsius() ) -> ustring().
temperature_to_string( Temp ) ->
	text_utils:format( "temperature of ~.1f°C", [ Temp ] ).



-doc "Returns a textual description of the specified relative humidity.".
-spec relative_humidity_to_string( percent() ) -> ustring().
relative_humidity_to_string( HPerCent ) ->
	text_utils:format( "relative humidity of ~.1f%", [ HPerCent ] ).



-doc "Returns a textual description of the specified learning status.".
-spec learn_to_string( boolean() ) -> ustring().
learn_to_string( _LearnActivated=true ) ->
	" whereas device learning is activated";

learn_to_string( _LearnActivated=false ) ->
	", with no device learning activated".



-doc """
Decodes the specified optional data, if any.

Refer to [ESP3] p.18 for its description.

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



-doc "Decodes specified byte as a dBm measurement.".
-spec decode_maybe_dbm( uint8() ) -> option( dbm() ).
decode_maybe_dbm( 16#ff ) ->
	% Should be a sending:
	undefined;

decode_maybe_dbm( V ) ->
	-V.



-doc "Decodes specified byte as a security level.".
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



-doc """
Records that a telegram could be successfully decoded for the specified device,
registering it if it was not already.
""".
-spec record_device_success( eurid(), device_table() ) -> recording_info().
record_device_success( Eurid, DeviceTable ) ->

	case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device whose EURID "
				"is ~ts through listening.", [ eurid_to_string( Eurid ) ] ),

			Now = time_utils:get_timestamp(),

			DiscoverOrigin = listening,

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 eep=undefined,
										 discovered_through=DiscoverOrigin,
										 first_seen=Now,
										 last_seen=Now,
										 availability=online,
										 telegram_count=1,
										 error_count=0,
										 expected_periodicity=none },

			% Necessarily new:
			NewDeviceTable = table:add_entry( Eurid, NewDevice, DeviceTable ),

			{ NewDeviceTable, NewDevice, Now, _MaybePrevLastSeen=undefined,
			  DiscoverOrigin, _IsBackOnline=false, _MaybeDeviceName=undefined,
			  _MaybeEEPId=undefined };


		{ value, Device } ->
			record_known_device_success( Device, DeviceTable )

	end.



-doc """
Records that a telegram could be successfully decoded for the specified
already-known device.
""".
-spec record_known_device_success( enocean_device(), device_table() ) ->
											recording_info().
record_known_device_success( Device=#enocean_device{
		eurid=Eurid,
		name=MaybeDeviceName,
		eep=MaybeEepId,
		first_seen=MaybeFirstSeen,
		last_seen=MaybeLastSeen,
		availability=MaybePrevAvail,
		telegram_count=TeleCount,
		expected_periodicity=Periodicity,
		activity_timer=MaybeActTimer }, DeviceTable ) ->

	Now = time_utils:get_timestamp(),

	% Report discovery only once, initially:
	{ NewFirstSeen, ReportedDiscoverOrigin, IsBackOnline } =
			case MaybeFirstSeen of

		% Was never seen:
		undefined ->
			{ Now, Device#enocean_device.discovered_through, _IsBack=false };

		% Has already been seen:
		FirstSeen ->
			IsBack = MaybePrevAvail =:= lost,
			{ FirstSeen, _MaybeDiscoverOrigin=undefined, IsBack }

	end,

	ResetTimer = reset_timer( MaybeActTimer, Eurid, Periodicity, NewFirstSeen,
		TeleCount, Device#enocean_device.error_count, Now ),

	UpdatedDevice = Device#enocean_device{ first_seen=NewFirstSeen,
										   last_seen=Now,
										   availability=online,
										   telegram_count=TeleCount+1,
										   activity_timer=ResetTimer },

	NewDeviceTable = table:add_entry( Eurid, UpdatedDevice, DeviceTable ),

	% The discovery origin must have already been reported (should a second
	% message be sent at the same second, hence at the same timestamp, to avoid
	% multiple "on detection messages" for a given device:
	%
	% (MaybeLastSeen here, not NewFirstSeen, to be able to select a proper
	% listener onEnocean* message afterwards; we also return always an undefined
	% discover origin to avoid that the more generic caller has to reassemble
	% this tuple)
	%
	{ NewDeviceTable, UpdatedDevice, Now, MaybeLastSeen, ReportedDiscoverOrigin,
	  IsBackOnline, MaybeDeviceName, MaybeEepId }.



-doc """
Records that a telegram could not be successfully decoded for the specified
device, registering it if it was not already.

Note that many failures do not even allow identifying the emitting device.
""".
-spec record_device_failure( eurid(), device_table() ) -> recording_info().
record_device_failure( Eurid, DeviceTable ) ->

	Now = time_utils:get_timestamp(),

	case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device whose EURID "
				"is ~ts through failure.", [ eurid_to_string( Eurid ) ] ),

			DiscoverOrigin = listening,

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 eep=undefined,
										 discovered_through=DiscoverOrigin,
										 first_seen=Now,
										 last_seen=Now,
										 availability=online,
										 telegram_count=0,
										 error_count=1,
										 expected_periodicity=none,
										 activity_timer=undefined },

			% Necessarily new:
			NewDeviceTable = table:add_entry( Eurid, NewDevice, DeviceTable ),

			{ NewDeviceTable, NewDevice, Now, _MaybePrevLastSeen=undefined,
			  DiscoverOrigin, _IsBackOnline=false, _MaybeDeviceName=undefined,
			  _MaybeEEPId=undefined };


		{ value, Device } ->
			record_known_device_failure( Device, DeviceTable )

	end.



-doc """
Records that a telegram could not be successfully decoded for the specified
already-known device.
""".
-spec record_known_device_failure( enocean_device(), device_table() ) ->
											recording_info().
record_known_device_failure( Device=#enocean_device{
		eurid=Eurid,
		name=MaybeDeviceName,
		eep=MaybeEepId,
		first_seen=MaybeFirstSeen,
		last_seen=MaybeLastSeen,
		availability=MaybePrevAvail,
		error_count=ErrCount,
		expected_periodicity=Periodicity,
		activity_timer=MaybeActTimer }, DeviceTable ) ->

	Now = time_utils:get_timestamp(),

	% Report discovery only once, initially:
	{ NewFirstSeen, ReportedDiscoverOrigin, IsBackOnline } =
			case MaybeFirstSeen of

		% Was never seen:
		undefined ->
			{ Now, Device#enocean_device.discovered_through, _IsBack=false };

		% Has already been seen:
		FirstSeen ->
			IsBack = MaybePrevAvail =:= lost,
			{ FirstSeen, _MaybeDiscoverOrigin=undefined, IsBack }

	end,

	ResetTimer = reset_timer( MaybeActTimer, Eurid, Periodicity, NewFirstSeen,
		Device#enocean_device.telegram_count, ErrCount, Now ),

	UpdatedDevice = Device#enocean_device{ first_seen=NewFirstSeen,
										   last_seen=Now,
										   availability=online,
										   error_count=ErrCount+1,
										   activity_timer=ResetTimer },

	NewDeviceTable = table:add_entry( Eurid, UpdatedDevice, DeviceTable ),

	% The discovery origin must have already been reported (should a second
	% message be sent at the same second, hence at the same timestamp, to avoid
	% multiple "on detection messages" for a given device):
	%
	% (MaybeLastSeen here, not NewFirstSeen, to be able to select a proper
	% listener onEnocean* message afterwards; we also return always an undefined
	% discover origin to avoid that the more generic caller has to reassemble
	% this tuple)
	%
	{ NewDeviceTable, UpdatedDevice, Now, MaybeLastSeen, ReportedDiscoverOrigin,
	  IsBackOnline, MaybeDeviceName, MaybeEepId }.



-doc "Resets any needed activity timer.".
-spec reset_timer( option( timer_ref() ), eurid(), expected_periodicity(),
		timestamp(), count(), count(), timestamp() ) -> option( timer_ref() ).
% No periodicity:
reset_timer( MaybeActTimer, _Eurid, _Periodicity=none, _FirstSeen,
			 _TeleCount, _ErrCount, _Now ) ->
	stop_any_timer( MaybeActTimer ),
	undefined;


% Auto periodicity:
reset_timer( MaybeActTimer, Eurid, _Periodicity=auto, FirstSeen,
			 TeleCount, ErrCount, Now ) ->

	stop_any_timer( MaybeActTimer ),

	NextDelayMs = compute_next_timeout( FirstSeen, TeleCount, ErrCount, Now ),

	TimedMsg = { onActivityTimeout, Eurid, NextDelayMs },

	cond_utils:if_defined( oceanic_debug_activity,
		trace_bridge:debug_fmt( "Setting an automatic timer for device "
			"whose EURID is ~ts, for a duration of ~ts.",
			[ eurid_to_string( Eurid ),
			  time_utils:duration_to_string( NextDelayMs ) ] ) ),

	{ ok, TimerRef } = timer:send_after( NextDelayMs, TimedMsg ),
	TimerRef;


% Fixed periodicity:
reset_timer( MaybeActTimer, Eurid, PeriodicityMs, _FirstSeen,
			 _TeleCount, _ErrCount, _Now ) ->

	stop_any_timer( MaybeActTimer ),

	TimedMsg = { onActivityTimeout, Eurid, PeriodicityMs },

	cond_utils:if_defined( oceanic_debug_activity,
		trace_bridge:debug_fmt( "Setting a fixed timer for device "
			"whose EURID is ~ts, for a duration of ~ts.",
			[ eurid_to_string( Eurid ),
			  time_utils:duration_to_string( PeriodicityMs ) ] ) ),

	case timer:send_after( PeriodicityMs, TimedMsg ) of

		{ ok, TimerRef } ->
			TimerRef;

		{ error, Reason } ->
			trace_bridge:error_fmt( "Failed to register a timer for sending "
				"message '~p' after ~w milliseconds; reason: ~p.",
				[ TimedMsg, PeriodicityMs, Reason ] ),
			undefined

	end.



-doc "Determines the next auto time-out for the specified parameters.".
-spec compute_next_timeout( timestamp(), count(), count(), timestamp() ) ->
											milliseconds().
compute_next_timeout( FirstSeen, TeleCount, ErrCount, Now ) ->

	% Adding a fixed and a 120% margin to hopefully avoid most of the false
	% alarms (many devices are *very* irregular, possibly related to their state
	% of charge):
	%
	SeenDurationMs = 2200 * time_utils:get_duration( FirstSeen, Now ),

	SeenCount = TeleCount + ErrCount,

	% A fixed (10 minute) margin can only help:
	FixedMarginMs = 10 * 60 * 1000,

	FixedMarginMs + case SeenCount of

		% No possible evaluation yet, starting with a larger default duration:
		0 ->
			2 * 1000 * time_utils:dhms_to_seconds( ?default_dhms_periodicity );

		_ ->
			erlang:max( ?min_activity_timeout, SeenDurationMs div SeenCount )

	end.



% (helper)
stop_any_timer( _MaybeTimer=undefined ) ->
	ok;

stop_any_timer( TimerRef ) ->
	{ ok, cancel } = timer:cancel( TimerRef ).



-doc """
Stops and terminates (asynchronously) the supposedly-existing Oceanic server.
""".
-spec stop() -> void().
stop() ->
	stop( get_server_pid() ).



-doc "Stops and terminates (asynchronously) the specified Oceanic server.".
-spec stop( oceanic_server_pid() ) -> void().
stop( SrvPid ) ->
	trace_bridge:debug_fmt( "Stopping the Oceanic server ~w.", [ SrvPid ] ),
	SrvPid ! terminate.



-doc "Stops and terminates synchronously the specified Oceanic server.".
-spec synchronous_stop( oceanic_server_pid() ) -> void().
synchronous_stop( SrvPid ) ->
	SrvPid ! { terminateSynchronously, self() },

	trace_bridge:debug_fmt( "Stopping synchronously the Oceanic server ~w.",
							[ SrvPid ] ),

	receive

		oceanic_terminated ->
			ok

	end.



-doc """
Returns the Oceanic identifier (if any) corresponding to the specified packet
type.
""".
-spec get_packet_type( enum() ) -> option( packet_type() ).
get_packet_type( PacketTypeNum ) ->
	% Topic defined by the module that Oceanic generates from the
	% oceanic_constants one:
	%
	oceanic_generated:get_maybe_first_for_packet_type( PacketTypeNum ).



-doc "Returns the registration name of the Oceanic server.".
-spec get_server_registration_name() -> registration_name().
get_server_registration_name() ->
	?oceanic_server_reg_name.



-doc "Returns the PID of the (supposedly-existing) Oceanic server.".
-spec get_server_pid() -> oceanic_server_pid().
get_server_pid() ->
	% Local otherwise global scope:
	naming_utils:get_registered_pid_for( ?oceanic_server_reg_name ).



-doc "Returns a textual description of the specified telegram.".
-spec telegram_to_string( telegram() ) -> ustring().
telegram_to_string( Telegram ) ->
	text_utils:format( "telegram ~w of size ~B bytes "
		"(corresponding to hexadecimal '~ts')",
		[ Telegram, size( Telegram ), telegram_to_hexastring( Telegram ) ] ).



-doc """
Returns an hexadecimal string corresponding to the specified telegram.

Useful for testing with serial clients like cutecom.
""".
-spec telegram_to_hexastring( telegram() ) -> ustring().
telegram_to_hexastring( Telegram ) ->
	text_utils:binary_to_hexastring( Telegram ).



-doc """
Returns an hexadecimal string corresponding to the specified telegram.

Useful for testing with serial clients like cutecom.
""".
-spec hexastring_to_telegram( ustring() ) -> telegram().
hexastring_to_telegram( HexaStr ) ->
	text_utils:hexastring_to_binary( HexaStr ).



-doc """
Resolves the specified EEP triplet into a proper EEP identifier (atom), if
possible.
""".
-spec resolve_eep( eep() ) -> option( eep_id() ).
resolve_eep( EepTriplet ) ->
	case oceanic_generated:get_maybe_first_for_eep_triplets( EepTriplet ) of

		undefined ->
			trace_bridge:warning_fmt( "The EEP specified as ~w is not known "
				"of Oceanic and will be ignored.", [ EepTriplet ] ),
			undefined;

		KnownEepId ->
			KnownEepId

	end.


-doc "Returns the application style corresponding to the specified EEP.".
-spec get_app_style_from_eep( eep_id() ) -> application_style().
get_app_style_from_eep( _EEPId=double_rocker_switch_style_1 ) ->
	1;

get_app_style_from_eep( _EEPId=double_rocker_switch_style_2 ) ->
	2.



-doc """
Checks that the specified term is the specification of a button reference.

If yes, returns the corresponding button reference.
If no, throws an exception.
""".
-spec interpret_button_ref_spec( term() ) -> button_ref().
interpret_button_ref_spec( { EuridStr, Channel } )
		when is_list( EuridStr )
			 andalso is_integer( Channel ) andalso Channel > 0 ->
	{ string_to_eurid( EuridStr ), Channel };

interpret_button_ref_spec( Other ) ->
	throw( { invalid_button_ref_spec, Other } ).



-doc """
Checks that the specified term is a list of specifications of button references.

If yes, returns the corresponding button references (same order).
If no, throws an exception.
""".
-spec interpret_button_ref_specs( term() ) -> [ button_ref() ].
interpret_button_ref_specs( ButRefSpecs ) ->
	[ interpret_button_ref_spec( BRS ) || BRS <- ButRefSpecs ].




-doc "Returns a raw, (plain) textual description of the specified EURID.".
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



-doc """
Returns a short, raw, (plain) textual description of the specified EURID.
""".
-spec eurid_to_short_string( eurid() ) -> ustring().
eurid_to_short_string( _Eurid=?eurid_broadcast ) ->
	"broadcast";

eurid_to_short_string( Eurid ) ->

	% We want to return for example a correct "002ef196", not an
	% ambiguously-shortened "2ef196":
	%
	HexaStr = text_utils:integer_to_hexastring( Eurid ),

	% 32-bit:
	PaddedStr = text_utils:pad_string_right( HexaStr, _Width=8, $0 ),

	text_utils:flatten( PaddedStr ).



-doc """
Returns the actual EURID corresponding to the specified (plain) EURID string.

For example `3076502 = oceanic:string_to_eurid("002ef196")`.

""".
-spec string_to_eurid( ustring() ) -> eurid().
string_to_eurid( EuridStr ) ->
	text_utils:hexastring_to_integer( EuridStr ).


-doc """
Returns the actual EURID corresponding to any specified (plain) EURID string,
otherwise the broadcast EURID.
""".
-spec maybe_string_to_eurid( option( ustring() ) ) -> eurid().
maybe_string_to_eurid( _MaybeEuridStr=undefined ) ->
	?eurid_broadcast;

maybe_string_to_eurid( EuridStr ) ->
	text_utils:hexastring_to_integer( EuridStr ).


-doc "Returns the broadcast EURID, suitable to target all devices in range.".
-spec get_broadcast_eurid() -> eurid().
get_broadcast_eurid() ->
	?eurid_broadcast.



-doc """
Returns a raw, direct (binary) textual description of the specified EURID.
""".
-spec eurid_to_bin_string( eurid() ) -> bin_string().
eurid_to_bin_string( Eurid ) ->
	text_utils:string_to_binary( eurid_to_string( Eurid ) ).



-doc """
Returns a (binary) textual description of the specified EURID, possibly
translated to a user-friendly device name if any is known for that device.
""".
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



-doc "Returns a textual description of the specified button reference.".
-spec button_ref_to_string( button_ref() ) -> ustring().
button_ref_to_string( _ButRef={ Eurid, Channel } ) ->
	text_utils:format( "button #~B of device whose EURID is ~ts",
					   [ Channel, eurid_to_string( Eurid ) ] ).



-doc "Returns a textual description of the specified button references.".
-spec button_refs_to_string( [ button_ref() ] ) -> ustring().
button_refs_to_string( _ButRefs=[] ) ->
	"no button reference set";

button_refs_to_string( _ButRefs=[ ButRef ] ) ->
	text_utils:format( "a single button reference set: ~ts",
					   [ button_ref_to_string( ButRef ) ] );

button_refs_to_string( ButRefs ) ->
	Strs = [ button_ref_to_string( BR ) || BR <- ButRefs ],
	text_utils:format( "~B button references set: ~ts",
		[ length( Strs ), text_utils:strings_to_listed_string( Strs ) ] ).




-doc """
Returns the best naming for a device, as any kind of string, depending on the
available information.
""".
-spec get_best_naming( option( device_name() ), eurid() ) -> any_string().
get_best_naming( _MaybeDevName=undefined, Eurid ) ->
	text_utils:format( "device whose EURID is ~ts",
					   [ eurid_to_string( Eurid ) ] );

get_best_naming( BinDevName, _Eurid ) ->
	text_utils:bin_format( "'~ts'", [ BinDevName ] ).



-doc """
Returns the best naming for a device, as a binary, depending on the available
information.
""".
-spec get_best_bin_naming( option( device_name() ), eurid() ) -> bin_string().
get_best_bin_naming( _MaybeDevName=undefined, Eurid ) ->
	eurid_to_bin_string( Eurid );

get_best_bin_naming( BinDevName, _Eurid ) ->
	BinDevName.



-doc """
Returns the best short description for the device of specified EURID, based on
server-internal information.
""".
-spec describe_device( eurid(), wooper:state() ) -> bin_string().
describe_device( Eurid, State ) ->

	DeviceTable = State#oceanic_state.device_table,

	case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			text_utils:bin_format( "unknown device whose EURID is ~ts",
								   [ eurid_to_string( Eurid ) ] );

		{ value, #enocean_device{ name=undefined } } ->
			text_utils:bin_format( "unnamed device whose EURID is ~ts",
								   [ eurid_to_string( Eurid ) ] );

		{ value, #enocean_device{ name=BinName } } ->
			text_utils:bin_format( "device '~ts' whose EURID is ~ts",
								   [ BinName, eurid_to_string( Eurid ) ] )

	end.



-doc """
Converts an EEP described as a string into its internal form.

Input example: "D5-00-01".
""".
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



% Device-related events API, to achieve some kind of polymorphism on the
% corresponding records.
%
% These functions are simple as, conventionally (by design), the first fields
% are uniform.



-doc """
Returns the type of the specified device event.
""".
-spec get_event_type( device_event() ) -> device_event_type().
get_event_type( DevEventTuple ) ->
	% The record tag:
	erlang:element( _PosIdx=1, DevEventTuple ).


-doc """
Returns the EURID of the emitting device stored in the specified device event.
""".
-spec get_source_eurid( device_event() ) -> eurid().
get_source_eurid( DevEventTuple ) ->
	erlang:element( _PosIdx=2, DevEventTuple ).



-doc """
Returns any channel referenced by the emitting device stored in the specified
device event.

For events from devices not having multiple channels
(e.g. push_button_switch_event, single_input_contact_event, thermo_hygro_event),
returns channel 1.
""".
-spec get_channel( device_event() ) -> channel().
% For multiple-rockers, only the first button reported is considered:
get_channel( #double_rocker_switch_event{
				first_action_button={ Channel, _Pos } } ) ->
	Channel;

get_channel( _AnyOtherEvent ) ->
	_Channel=1.



-doc """
Returns the button reference of the emitting device stored in the specified
device event, i.e. its EURID and the channel it used.
""".
-spec get_button_reference( device_event() ) -> channel().
get_button_reference( DevEventTuple ) ->
	{ get_source_eurid( DevEventTuple ), get_channel( DevEventTuple ) }.



-doc """
Returns the emitting device name (if any) stored in the specified device event.
""".
-spec get_maybe_device_name( device_event() ) -> option( device_name() ).
get_maybe_device_name( DevEventTuple ) ->
	erlang:element( _PosIdx=3, DevEventTuple ).



-doc """
Returns the best name found for the emitting device stored in the specified
device event.
""".
-spec get_best_device_name_from( device_event() ) -> device_name().
get_best_device_name_from( DevEventTuple ) ->
	case get_maybe_device_name( DevEventTuple ) of

		undefined ->
			SrcEurid = get_source_eurid( DevEventTuple ),
			eurid_to_bin_string( SrcEurid );

		BinDeviceName ->
			BinDeviceName

	end.



-doc """
Returns the EEP (if any is defined and registered) stored in the specified
device event.
""".
-spec get_maybe_eep( device_event() ) -> option( eep_id() ).
get_maybe_eep( DevEventTuple ) ->
	erlang:element( _PosIdx=4, DevEventTuple ).



-doc "Returns the timestamp stored in the specified device event.".
-spec get_timestamp( device_event() ) -> timestamp().
get_timestamp( DevEventTuple ) ->
	erlang:element( _PosIdx=5, DevEventTuple ).


-doc """
Returns the timestamp corresponding to any previously seen telegram from that
device.

Also useful to determine whether an event corresponds to a device discovery.
""".
-spec get_last_seen_info( device_event() ) -> option( timestamp() ).
get_last_seen_info( DevEventTuple ) ->
	erlang:element( _PosIdx=6, DevEventTuple ).



-doc """
Returns the number (if any) of subtelegrams stored in the specified device
event.
""".
-spec get_subtelegram_count( device_event() ) -> option( subtelegram_count() ).
get_subtelegram_count( DevEventTuple ) ->
	erlang:element( _PosIdx=6, DevEventTuple ).



-doc """
Returns the EURID of the target of this transmission (addressed or broadcast),
if any, stored in the specified device event.
""".
-spec get_maybe_destination_eurid( device_event() ) -> option( eurid() ).
get_maybe_destination_eurid( DevEventTuple ) ->
	erlang:element( _PosIdx=7, DevEventTuple ).



-doc """
Returns the best RSSI value (if any) stored in the specified device event.
""".
-spec get_maybe_dbm( device_event() ) -> option( dbm() ).
get_maybe_dbm( DevEventTuple ) ->
	erlang:element( _PosIdx=8, DevEventTuple ).



-doc "Returns the stored in the specified device event.".
-spec get_maybe_security_level( device_event() ) -> option( security_level() ).
get_maybe_security_level( DevEventTuple ) ->
	erlang:element( _PosIdx=9, DevEventTuple ).



-doc """
Tells whether the specified device event indicates that this device can be
interpreted as being triggered by the user.
""".
-spec device_triggered( device_event() ) -> boolean().
device_triggered( #push_button_switch_event{ transition=pressed } ) ->
	true;

device_triggered( #double_rocker_switch_event{ energy_bow=pressed } ) ->
	true;

device_triggered( #double_rocker_switch_event{ second_action_valid=true } ) ->
	true;

device_triggered( _DevEventTuple ) ->
	false.




% Other string-related conversions:

-doc "Returns a textual description of the specified optional data.".
-spec optional_data_to_string( decoded_optional_data() ) -> ustring().
optional_data_to_string( _OptData={ SubTelNum, DestinationEurid, MaybeDBm,
									MaybeSecurityLevel } ) ->
	optional_data_to_string( SubTelNum, DestinationEurid, MaybeDBm,
							 MaybeSecurityLevel ).



-doc "Returns a textual description of the specified decoded optional data.".
-spec optional_data_to_string( subtelegram_count(), eurid(), option( dbm() ),
							   option( security_level() ) ) -> ustring().
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

	text_utils:format( " with ~ts, targeted to device whose EURID is ~ts~ts~ts",
		[ SubTelStr, eurid_to_string( DestinationEurid ), DBmstr, SecStr ] ).



-doc """
Returns a short textual description of the specified decoded optional data,
designed for user-friendly reporting.
""".
-spec optional_data_to_short_string( eurid(), option( dbm() ) ) -> ustring().
optional_data_to_short_string( _DestinationEurid=undefined,
							   _MaybeDBm=undefined ) ->
	"";

optional_data_to_short_string( DestinationEurid, MaybeDBm ) ->

	DBmstr = case MaybeDBm of

		undefined ->
			"";

		DBm ->
			text_utils:format( " (RSSI: ~B dBm)", [ DBm ] )

	end,

	text_utils:format( "target: ~ts~ts",
		[ eurid_to_short_string( DestinationEurid ), DBmstr ] ).



-doc """
Returns a textual description of the specified decoded maybe-optional data,
otherwise from the corresponding raw data.
""".
-spec maybe_optional_data_to_string( option( decoded_optional_data() ),
									 telegram_opt_data() ) -> ustring().
maybe_optional_data_to_string( _MaybeDecodedOptData=undefined, OptData ) ->
	text_utils:format( ", with optional data of ~B bytes that could not "
		"be decoded: ~w", [ size( OptData ), OptData ] );

maybe_optional_data_to_string( DecodedOptData, _OptData ) ->
	optional_data_to_string( DecodedOptData ).



-doc "Returns a textual description of the specified security level.".
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



-doc "Returns a textual description of the specified repeater count.".
-spec repeater_count_to_string( count() ) -> ustring().
repeater_count_to_string( _RC=0 ) ->
	"with no repeating done";

repeater_count_to_string( _RC=1 ) ->
	"with a single repeating done";

repeater_count_to_string( RC ) ->
	text_utils:format( "with ~B repeatings done", [ RC ] ).



-doc """
Returns a (rather complete) textual description of the specified device event.
""".
-spec device_event_to_string( device_event() ) -> ustring().
device_event_to_string( #thermo_hygro_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
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

	text_utils:format( "thermo-hygro sensor device ~ts which reports at ~ts "
		"a ~ts ~ts~ts; this is declared~ts; ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  time_utils:timestamp_to_string( Timestamp ),
		  relative_humidity_to_string( RelativeHumidity ), TempStr,

		  learn_to_string( LearnActivated ),

		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),

		  last_seen_to_string( MaybeLastSeen ),

		  % Multiple A5-04-01-like candidates:
		  get_eep_description( MaybeEepId ) ] );


device_event_to_string( #single_input_contact_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		learn_activated=LearnActivated,
		contact=ContactStatus } ) ->

	% Apparently either state transitions or just periodic state reports:
	text_utils:format( "single-contact device ~ts is in ~ts state at ~ts~ts; "
		"this is declared~ts; ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  get_contact_status_description( ContactStatus ),
		  time_utils:timestamp_to_string( Timestamp ),
		  learn_to_string( LearnActivated ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  last_seen_to_string( MaybeLastSeen ),
		  get_eep_description( MaybeEepId, _DefaultDesc="D5-00-01" ) ] );


device_event_to_string( #push_button_switch_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
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
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  last_seen_to_string( MaybeLastSeen ),
		  get_eep_description( MaybeEepId, _DefaultDesc="F6-01-01" ) ] );

device_event_to_string( #smart_plug_status_report_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		power_failure_detected=IsPowerFailureDetected,
		overcurrent_triggered=IsOverCurrentSwitchOffTrigger,
		hardware_status=HardwareStatus,
		local_control_enabled=IsLocalControlEnabled,
		output_power=OutputPower } ) ->

	PFStr = interpret_power_failure( IsPowerFailureDetected ),
	OCStr = interpret_overcurrent_trigger( IsOverCurrentSwitchOffTrigger ),
	HardStr = interpret_hardware_status( HardwareStatus ),
	LocCtrlStr = interpret_local_control( IsLocalControlEnabled ),
	PowerStr = interpret_power_report( OutputPower ),

	text_utils:format( "smart plug ~ts reports at ~ts that ~ts, ~ts, ~ts, ~ts; "
		"this plug ~ts; notified~ts; ~ts, ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  time_utils:timestamp_to_string( Timestamp ),
		  PFStr, OCStr, HardStr, LocCtrlStr, PowerStr,
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  last_seen_to_string( MaybeLastSeen ),
		  get_eep_description( MaybeEepId, _DefaultDesc=undefined ) ] );

device_event_to_string( #double_rocker_switch_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		first_action_button=FirstButtonLocator,
		energy_bow=ButtonTransition,
		second_action_button=SecondButtonLocator,
		second_action_valid=IsValid } ) ->

	%% SecondStr = case IsValid of

	%%	true ->
	%%		text_utils:format( " and its ~ts",
	%%			[ button_locator_to_string( SecondButtonLocator ) ] );

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
				[ button_locator_to_string( SecondButtonLocator ) ] ),

	text_utils:format( "double-rocker device ~ts has its ~ts ~ts, "
		"whereas its second action ~ts, at ~ts; this is declared~ts; ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  button_locator_to_string( FirstButtonLocator ),
		  get_button_transition_description( ButtonTransition ),
		  SecondStr,
		  time_utils:timestamp_to_string( Timestamp ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  last_seen_to_string( MaybeLastSeen ),
		  get_eep_description( MaybeEepId, _DefaultDesc="F6-02-01" ) ] );


device_event_to_string( #double_rocker_multipress_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
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
			% According to the spec: "no button"; yet in practice, clearer
			% (notably when they are released):
			%
			"all buttons";

		three_or_four ->
			"3 or 4 buttons"

	end ++ " " ++ get_button_transition_description( ButtonTransition ),

	text_utils:format( "double-rocker device ~ts has ~ts simultaneously "
		"at ~ts; this is declared~ts; ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ), TransStr,
		  time_utils:timestamp_to_string( Timestamp ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  last_seen_to_string( MaybeLastSeen ),
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
								length( ApiCounters ), ApiCounters ] );


device_event_to_string( #read_base_id_info_response{
		base_eurid=BaseEurid,
		remaining_write_cycles=RemainWrtCycles } ) ->

	text_utils:format( "read gateway base ID whose EURID is ~ts, "
		% Possibly 'unlimited':
		"for ~p remaining write cycles",
		[ eurid_to_string( BaseEurid ), RemainWrtCycles ] );


device_event_to_string( command_processed ) ->
	"the current command has been successfully processed";

device_event_to_string( error_return ) ->
	"the current command was reported by the target device as having failed";

device_event_to_string( not_supported_return ) ->
	"the current command was reported by the target device as "
	"not being supported";

device_event_to_string( wrong_parameter_return ) ->
	"the current command was reported by the target device as "
	"having failed due to incorrect supplied parameters";

device_event_to_string( operation_denied ) ->
	"the current command was reported by the target device as "
	"having failed due to being a denied operation";

device_event_to_string( time_out ) ->
	"the current command failed to be acknowledged on time";

device_event_to_string( OtherEvent ) ->
	text_utils:format( "unknown event to detail: ~p", [ OtherEvent ] ).



-doc """
Returns a textual description of the specified last_seen field of a device
event.
""".
-spec last_seen_to_string( option( timestamp() ) ) -> ustring().
last_seen_to_string( _MaybeLastSeenTimestamp=undefined ) ->
	"this device has just been discovered";

last_seen_to_string( LastSeenTimestamp ) ->
	text_utils:format( "this device has already been discovered, "
		"its last telegram being detected on ~ts",
		[ time_utils:timestamp_to_string( LastSeenTimestamp ) ] ).



-doc """
Returns a short textual description of the specified device event, designed for
user-friendly reporting.
""".
-spec device_event_to_short_string( device_event() ) -> ustring().
device_event_to_short_string( #thermo_hygro_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		relative_humidity=RelativeHumidity,
		temperature=MaybeTemperature,
		temperature_range=TempRange } ) ->

	TempStr = case MaybeTemperature of

		undefined ->
			"(no temperature available)";

		Temp ->
			text_utils:format( "and a ~ts (sensitivity range: ~ts)",
				[ temperature_to_string( Temp ), TempRange ] )

	end,

	% Timestamp already available:
	text_utils:format( "The thermo-hygro sensor device ~ts reports "
		"a ~ts ~ts; ~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ),
		  relative_humidity_to_string( RelativeHumidity ), TempStr,
		  optional_data_to_short_string( MaybeDestEurid, MaybeDBm ),

		  % Multiple A5-04-01-like candidates:
		  get_eep_short_description( MaybeEepId ) ] );


device_event_to_short_string( #single_input_contact_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		contact=ContactStatus } ) ->

	% Apparently either state transitions or just periodic state reports:
	text_utils:format( "The single-contact device ~ts is in ~ts state; "
		"~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ),
		  get_contact_status_description( ContactStatus ),
		  optional_data_to_short_string( MaybeDestEurid, MaybeDBm ),
		  get_eep_short_description( MaybeEepId, _DefaultDesc="D5-00-01" ) ] );


device_event_to_short_string( #push_button_switch_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		transition=ButtonTransition } ) ->
	text_utils:format(
		"The push-button device ~ts has been ~ts; ~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ),
		  get_button_transition_description( ButtonTransition ),
		  optional_data_to_short_string( MaybeDestEurid, MaybeDBm ),
		  get_eep_short_description( MaybeEepId, _DefaultDesc="F6-01-01" ) ] );


device_event_to_short_string( #smart_plug_status_report_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		power_failure_detected=PFDetected,
		overcurrent_triggered=OCTriggered,
		hardware_status=HardwareStatus,
		local_control_enabled=IsLocalControlEnabled,
		output_power=OutputPower } ) ->
	text_utils:format( "The smart-plug device ~ts reports that it ~ts "
		"(~ts, ~ts, ~ts, ~ts); ~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ),
		  interpret_power_report( OutputPower ),
		  interpret_power_failure( PFDetected ),
		  interpret_overcurrent_trigger( OCTriggered ),
		  interpret_hardware_status( HardwareStatus ),
		  interpret_local_control( IsLocalControlEnabled ),
		  optional_data_to_short_string( MaybeDestEurid, MaybeDBm ),

		  % Possibly "D2-01-0B":
		  get_eep_short_description( MaybeEepId, _DefaultDesc="D2-01-0A" ) ] );


device_event_to_short_string( #double_rocker_switch_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		first_action_button=FirstButtonLocator,
		energy_bow=ButtonTransition,
		second_action_button=SecondButtonLocator,
		second_action_valid=IsValid } ) ->

	%% SecondStr = case IsValid of

	%%	true ->
	%%		text_utils:format( " and its ~ts",
	%%			[ button_locator_to_string( SecondButtonLocator ) ] );

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
				[ button_locator_to_string( SecondButtonLocator ) ] ),

	text_utils:format( "The double-rocker device ~ts has its ~ts ~ts, "
		"whereas its second action ~ts; ~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ),
		  button_locator_to_string( FirstButtonLocator ),
		  get_button_transition_description( ButtonTransition ),
		  SecondStr,
		  optional_data_to_short_string( MaybeDestEurid, MaybeDBm ),
		  get_eep_short_description( MaybeEepId, _DefaultDesc="F6-02-01" ) ] );


device_event_to_short_string( #double_rocker_multipress_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
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

	text_utils:format( "The double-rocker device ~ts has ~ts simultaneously; "
		"~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ), TransStr,
		  optional_data_to_short_string(  MaybeDestEurid, MaybeDBm ),
		  get_eep_short_description( MaybeEepId, _DefaultDesc="F6-02-01" ) ] );


device_event_to_short_string( #read_version_response{
		app_version=AppVersion,
		api_version=ApiVersion,
		chip_id=ChipId,
		chip_version=ChipVersion,
		app_description=BinAppDesc } ) ->

	text_utils:format( "Read application version ~ts, API version ~ts, "
		"chip ID ~ts, chip version ~B and application description '~ts'.",
		[ text_utils:version_to_string( AppVersion ),
		  text_utils:version_to_string( ApiVersion ),
		  text_utils:integer_to_hexastring( ChipId ), ChipVersion,
		  BinAppDesc ] );


device_event_to_short_string( #read_logs_response{ app_counters=AppCounters,
											api_counters=ApiCounters } ) ->

	text_utils:format( "Read counters: ~B for application: ~w, "
		"and ~B for API: ~w.", [ length( AppCounters ), AppCounters,
								 length( ApiCounters), ApiCounters ] );


device_event_to_short_string( #read_base_id_info_response{
		base_eurid=BaseEurid,
		remaining_write_cycles=RemainWrtCycles } ) ->

	text_utils:format( "Read gateway base ID of EURID ~ts, "
		% Possibly 'unlimited':
		"for ~p remaining write cycles.",
		[ eurid_to_string( BaseEurid ), RemainWrtCycles ] );


device_event_to_short_string( command_processed ) ->
	"The current command has been successfully processed.";

device_event_to_short_string( error_return ) ->
	"The current command was reported by the target device as having failed.";

device_event_to_short_string( not_supported_return ) ->
	"The current command was reported by the target device as "
	"not being supported.";

device_event_to_short_string( wrong_parameter_return ) ->
	"The current command was reported by the target device as "
	"having failed due to incorrect supplied parameters.";

device_event_to_short_string( operation_denied ) ->
	"The current command was reported by the target device as "
	"having failed due to being a denied operation.";

device_event_to_short_string( time_out ) ->
	"The current command failed to be acknowledged on time.";

device_event_to_short_string( OtherEvent ) ->
	text_utils:format( "Unknown event to summarise: ~p.", [ OtherEvent ] ).



-doc "Returns a textual description of the specified command request.".
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



-doc "Returns a textual description of the specified device name.".
-spec get_name_description( option( device_name() ), eurid() ) -> ustring().
get_name_description( _MaybeName=undefined, Eurid ) ->
	text_utils:format( "whose EURID is ~ts", [ eurid_to_string( Eurid ) ] );

get_name_description( Name, Eurid ) ->
	text_utils:format( "'~ts' (whose EURID is ~ts)",
					   [ Name, eurid_to_string( Eurid ) ] ).



-doc "Returns a textual description of the specified button transition.".
-spec get_button_transition_description( button_transition() ) -> ustring().
get_button_transition_description( _Button=pressed ) ->
	"pressed";

get_button_transition_description( _ContactStatus=released ) ->
	"released".



-doc "Returns a textual description of the specified contact status.".
-spec get_contact_status_description( contact_status() ) -> ustring().
get_contact_status_description( _ContactStatus=open ) ->
	"opened";

get_contact_status_description( _ContactStatus=closed ) ->
	"closed".



-doc "Returns a textual description of the specified EEP (if any).".
-spec get_eep_description( option( eep_id() ) ) -> ustring().
get_eep_description( _MaybeEepId=undefined ) ->
	"its EEP is not known";

get_eep_description( EepId ) ->
	text_utils:format( "its EEP is ~ts (~ts)", [ EepId,
		oceanic_generated:get_maybe_second_for_eep_strings( EepId ) ] ).



-doc """
Returns a textual description of the specified EEP (if any), with a default.
""".
-spec get_eep_description( option( eep_id() ), ustring() ) -> ustring().
get_eep_description( _MaybeEepId=undefined, DefaultDesc ) ->
	text_utils:format( "its EEP is not known (supposing ~ts)",
					   [ DefaultDesc ] );

get_eep_description( EepId, _DefaultDesc ) ->
	get_eep_description( EepId ).



-doc "Returns a short textual description of the specified EEP (if any).".
-spec get_eep_short_description( option( eep_id() ) ) -> ustring().
get_eep_short_description( _MaybeEepId=undefined ) ->
	"unknown";

get_eep_short_description( EepId ) ->
	text_utils:format( "~ts (~ts)", [ EepId,
		oceanic_generated:get_maybe_second_for_eep_strings( EepId ) ] ).



-doc """
Returns a textual description of the specified EEP (if any), with a default.
""".
-spec get_eep_short_description( option( eep_id() ), ustring() ) -> ustring().
get_eep_short_description( _MaybeEepId=undefined, DefaultDesc ) ->
	text_utils:format( "unknown (supposing ~ts)",
					   [ DefaultDesc ] );

get_eep_short_description( EepId, _DefaultDesc ) ->
	get_eep_short_description( EepId ).



-doc """
Returns a textual description of the specified state of the Oceanic server.
""".
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
		event_listeners=EventListeners } ) ->

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

	ListenStr = case EventListeners of

		[] ->
			"not having any listener of Enocean events registered";

		[ ListenerPid ] ->
			text_utils:format( "having ~w registered as listener "
							   "of Enocean events", [ ListenerPid ] );

		_ ->
			text_utils:format( "having ~B listeners of Enocean events (~w)",
							   [ length( EventListeners ), EventListeners ] )

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
		"whose emitter EURID is ~ts, ~ts, ~ts; ~ts, ~ts, ~ts, ~ts, "
		"and knowing ~ts",
		[ SerialServerPid, eurid_to_string( EmitterEurid ), WaitStr, QStr,
		  ListenStr, SentStr, DiscStr, JamStr,
		  device_table_to_string( DeviceTable ) ] ).



-doc "Returns a textual description of the specified device table.".
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



-doc "Returns a textual description of the specified Enocean device.".
-spec device_to_string( enocean_device() ) -> ustring().
device_to_string( #enocean_device{ eurid=Eurid,
								   name=MaybeName,
								   eep=MaybeEepId,
								   discovered_through=DiscOrigin,
								   first_seen=MaybeFirstTimestamp,
								   last_seen=MaybeLastTimestamp,
								   availability=MaybeAvailStatus,
								   telegram_count=TeleCount,
								   error_count=ErrCount,
								   expected_periodicity=ActPeriod,
								   activity_timer=MaybeActTimer } ) ->

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

	{ SeenStr, DiscStr, TeleStr, ErrStr, AvailStr } =
			case MaybeFirstTimestamp of

		undefined ->
			{ "never been seen by this server", "", "", "", "" };

		FirstTimestamp ->
			SeenCountStr = case MaybeLastTimestamp of

				% Note that multiple telegrams may be received during the same
				% initial second:
				%
				FirstTimestamp ->
					text_utils:format( "been seen only once by this server, "
						"on ~ts; ",
						[ time_utils:timestamp_to_string( FirstTimestamp ) ] );

				LastTimestamp ->
					text_utils:format( "been seen firstly by this server "
						"on ~ts, and lastly on ~ts; ",
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

			AvailabilityStr = case MaybeAvailStatus of

				undefined ->
					"the current availability of this device is unknown";

				online ->
					"this device is considered online";

				lost ->
					"this device is considered lost"

			end,

			{ SeenCountStr, DiscovStr, TeleCountStr, ErrCountStr,
			  AvailabilityStr }

	end,

	PeriodStr = case ActPeriod of

		none ->
			"no activity monitoring of this device";

		auto ->
			ExpectPeriodStr = case MaybeFirstTimestamp of

				undefined ->
					"its expected periodicity cannot be determined yet "
					"(device never seen)";

				FirstSeen ->

					NextDelayMs = compute_next_timeout( FirstSeen, TeleCount,
						ErrCount, _Now=time_utils:get_timestamp() ),

					% Initially may be misleading as the first duration is
					% determined differently (whereas min_activity_timeout can
					% be displayed here):
					%
					text_utils:format( "its currently expected periodicity "
						"would be ~ts",
						[ time_utils:duration_to_string( NextDelayMs ) ] )

			end,

			"the activity of this device is monitored ("
				++ case MaybeActTimer of

						undefined ->
							"no activity timer set";

						_ActTimer ->
							% Not useful enough:
							%text_utils:format( "activity timer ~w set",
							%                   [ ActTimer ] )
							"activity timer set"

				   end ++ ") and " ++ ExpectPeriodStr;

		Milliseconds ->
			text_utils:format( "its activity is monitored, and expected "
				"to happen about every ~ts",
				[ time_utils:duration_to_string( Milliseconds ) ] )

	end,

	text_utils:format( "~ts applying ~ts; it has ~ts~ts~ts~ts; ~ts; ~ts",
		[ NameStr, EepDescStr, SeenStr, DiscStr, TeleStr, ErrStr,
		  AvailStr, PeriodStr ] ).



-doc "Returns a description of the specified device, as seen from Oceanic.".
-spec get_device_description( enocean_device() ) -> device_description().
get_device_description( Device ) ->
	text_utils:string_to_binary( device_to_string( Device ) ).




% CRC subsection.


-doc "Returns the CRC code corresponding to the specified binary.".
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



-doc "Returns the array used to code/decode CRC.".
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


-doc """
Secures the usability of (our fork of) erlang-serial, typically from an
(e)script.
""".
-spec secure_serial( any_directory_path() ) -> void().
secure_serial( _AnyOceanicRootDir ) ->

	SerialRootDir = file_utils:join( system_utils:get_software_base_directory(),
									 "erlang-serial" ),

	case file_utils:is_existing_directory_or_link( SerialRootDir ) of

		true ->
			SerialEbinDir = file_utils:join( SerialRootDir, "ebin" ),

			% Supposing it built then:
			code_utils:declare_beam_directory( SerialEbinDir );

		false ->
			% oceanic:secure_tty/1 will look-up the BEAM later:
			trace_bridge:warning_fmt( "No user 'erlang-serial' installation "
				"found (searched for '~ts').", [ SerialRootDir ] )

	end.



% Section for the build-time generation of support modules.


-doc """
To be called by the 'oceanic_generated.beam' automatic make target in order to
generate, here, a (single) module to share the Oceanic constants.
""".
-spec generate_support_modules() -> no_return().
generate_support_modules() ->

	TargetModName = oceanic_generated,

	%trace_bridge:info_fmt( "Generating module '~ts'...", [ TargetModName ] ),

	AllSpecNames = [ get_maybe_packet_type_topic_spec,
		get_maybe_return_code_topic_spec,
		get_maybe_event_code_topic_spec, get_maybe_rorg_topic_spec,
		get_maybe_rorg_description_topic_spec,
		get_maybe_common_command_topic_spec,
		get_maybe_vld_d2_00_cmd_topic_spec ],

	TopicSpecs = [ oceanic_constants:F() || F <- AllSpecNames ]
		++ oceanic_constants:get_maybe_eep_topic_specs(),

	_ModFilename =
		const_bijective_topics:generate_in_file( TargetModName, TopicSpecs ),

	%trace_bridge:info_fmt( "File '~ts' generated.", [ ModFilename ] ),

	erlang:halt().
