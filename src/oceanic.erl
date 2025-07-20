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



% Design notes:
%
% EEP-spec refers to EnOcean_Equipment_Profiles_EEP_v2.6.7_public.pdf (refer to
% Oceanic web documentation).


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

	send/2, stop_any_timer/1,
	acknowledge_teach_request/2, acknowledge_teach_request/3,

	trigger_actuator/2, trigger_actuators/2,
	trigger_actuator_reciprocal/2, trigger_actuators_reciprocal/2,



	% For any kind of (already-encoded) command:
	execute_command/2,

	% Useful to properly encode telegrams:
	get_oceanic_eurid/1,

	% General-purpose:
	get_device_description/2,
	get_button_ref_description/2, get_button_ref_descriptions/2,

	is_serial_available/1, restart_serial_interface/1,

	stop/0, stop/1, synchronous_stop/1,

	get_app_style_from_eep/1,

	interpret_button_ref_spec/1, interpret_button_ref_specs/1,

	get_device_table/1 ] ).



% Event-related API, to achieve some kind of polymorphism on the corresponding
% records:
%
-export([ get_event_type/1, get_source_eurid/1,
          get_channel/1,
		  get_button_reference/1,

		  get_maybe_device_name/1, get_maybe_device_short_name/1,
          get_best_device_name_from/1,

		  get_maybe_eep/1, resolve_eep/1, get_broadcast_eurid/0,
		  get_timestamp/1, get_last_seen_info/1,
		  get_subtelegram_count/1, get_maybe_destination_eurid/1,
		  get_maybe_dbm/1, get_maybe_security_level/1, device_triggered/1 ]).


% Helper API for Oceanic user code:
-export([ telegram_to_string/1, maybe_optional_data_to_string/2,
          device_event_to_string/1, hexastring_to_telegram/1,
          string_to_eurid/1,

          get_reciprocal_state_change_spec/2,
		  event_matches_trigger/2,

		  get_all_device_types/0, is_valid_device_type/1,

		  is_valid_application_style/1, is_valid_channel/1,
		  is_valid_button_position/1, is_valid_button_transition/1 ]).


% API for internal use:

-export([ compute_crc/1, compute_next_timeout/4,
          record_device_success/2, record_known_device_success/2,
          record_device_failure/2, record_known_device_failure/2,

          get_designated_eurid/2, get_designated_device/2,

          get_device_convention/2,
          handle_next_command/2, handle_next_request/4,

          canonicalise_listened_event_specs/1,
          canonicalise_listened_event_specs/2,
          canonicalise_emitted_event_specs/1,
          canonicalise_emitted_event_specs/2,
          declare_device_from_teach_in/3 ]).


% For execution as (e)script:
-export([ secure_serial/1 ]).


% API for module generation:
-export([ generate_support_modules/0 ]).


% Exported only for testing:
-export([ get_test_state/0, get_test_state/1,
		  test_decode/1, test_describe/1,
          secure_tty/1, try_integrate_next_telegram/4 ]).


% For silencing unused warnings:
-export([ send_raw_telegram/2 ]).


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
The PID of the requester of a common command (then notified as a message), or
the 'internal' atom to tell that this is a request emitted by Oceanic for its
own economy (then notified as a term).

A requester process uses this server for command sending on its behalf, and is
to be notified of the outcome of it (see
oceanic_common_command:send_common_command/2).
""".
-type requester() :: pid() | 'internal'.



-doc """
A user-defined (full) device name, descriptive and a lot more convenient than a
EURID.
""".
-type device_name() :: bin_string().


-doc """
A user-defined short device name, useful to designate a device in the context of
an action, so generally defined more in functional terms (its role rather than
its type, like `tv_controller`).

This provides a user-friendly identifier, alternatively to an EURID.

Note that `undefined` is not a valid short name.
""".
-type device_short_name() :: atom().


-doc "A user-defined device name, typically in configuration settings.".
-type device_plain_name() :: ustring().


-doc """
The naming of a device, typically as specified in the configuration settings so
that a short name can be translated.
""".
-type device_naming() :: device_plain_name()
                       | { device_plain_name(), device_short_name() }.


-doc "A user-defined device name, a lot more convenient than a EURID.".
-type device_any_name() :: any_string().



-doc """
A user-level element designating a device, thanks to its name (as a string) and
possibly a short name (as an atom).
""".
-type device_designator_spec() :: device_plain_name()
                                | { device_plain_name(), device_short_name() }.


-doc """
A user-level device designator, for example in listened event specifications.
""".
-type user_device_designator() :: eurid_string() | device_short_name().


-doc """
An (internal) element designating a device, either thanks to an EURID (as an
integer), or thanks to a short name (as an atom).
""".
-type device_designator() :: eurid() | device_short_name().


-doc "A pair of device designators.".
-type designator_pair() :: { eurid(), option( device_short_name() ) }.


-doc "A user-level device action.".
-type user_device_action() :: { device_operation(), user_device_designator() }.

-doc "An (internal) device action.".
-type device_action() :: { device_operation(), device_designator() }.


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

For example `{smart_plug_status_report_event, power_off}`.
""".
-type trigger_track_spec() ::
    { device_event_type(), option( reported_event_info() ) }.


-doc """
Server-internal information about a pending device-level request meant to be
acknowledged by the actuator of the specified EURID (hence being waited for,
with or without success), specifying the type of event to wait for
(e.g. `smart_plug_status_report_event`) together with any specific event
information expected (e.g. `power_on`); the specified number of new attempts to
be performed if the current one times-out is also specified.

Allows detecting issues such as emitted yet not (correctly) received telegrams,
for example if wanting to switch on a smart plug and be sure that it succeeded.
""".
% A configurable time-out could be added as well.
-type trigger_track_info() :: { ActEurid :: eurid(),
								device_event_type(),
								option( reported_event_info() ),
                                option( timer_ref() ),
								NextRetries :: count() }.


-doc """
Extra information to validate a type of device event for an acknowledgement.
""".
-type reported_event_info() ::

    % For actuators that are smart plugs:
    'power_on' | 'power_off'.




-doc """
Specification of Oceanic settings, as key/values pairs, as read from a
configuration file or transmitted by other services.
""".
-type oceanic_settings() :: list_table().


% For the records and defines:
-include("oceanic.hrl").
-include("oceanic_internal.hrl").


-doc "Allows to keep track of an ongoing request.".
-type request_tracking() :: #request_tracking{}.


-doc """
Tracking information regarding a currently pending request (typically an
actuation).

This corresponds to a higher-level request sent to a device expected to send
back an applicative answer (e.g. a status report sent back by a smart plug after
it was switched on).

A timer is used to trigger a time-out, should no acknowledgement be received on
time.
""".
-type waited_request_info() :: { request_tracking(), timer_ref() }.


-doc "Information regarding an Enocean device, as known by the Oceanic server.".
-type enocean_device() :: #enocean_device{}.



-doc "A table recording information regarding Enocean devices.".
-type device_table() :: table( eurid(), enocean_device() ).



-doc "A (FIFO) queue of commands to be sent in turn next.".
-type command_queue() :: queue:queue( command_tracking() ).

-doc "A (FIFO) queue of requests to be sent in turn next.".
-type request_queue() :: queue:queue( request_tracking() ).


-doc """
An entry in the Oceanic configuration (see its `oceanic_devices` key) to
describe a given device of interest.
""".
-type device_config() ::

	% Then the activity periodicity will be learnt, if appropriate (e.g. contact
	% switches send transition events but generally no periodical events):
	%
	{ UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring() }

  | { UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring(), device_info_user_spec() }

  | { UserDefinedName :: ustring(), EURIDStr :: eurid_string(),
	  EEP :: ustring(), device_info_user_spec(), Comment :: ustring() }.



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

For example:
`<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>`,
`<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>` or
`<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>`.
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
complement/extend the base data (see `telegram_data/0`).

Refer to `[ESP3]`. p.18.

See also `decoded_optional_data/0`.
""".
-type telegram_opt_data() :: telegram_chunk().



-doc """
The decoded data for the optional part of the Packet Type 1 (RADIO_ERP1)
telegrams.

Refer to `[ESP3]`. p.18.

See also `telegram_opt_data/0`.
""".
-type decoded_optional_data() ::
	{ subtelegram_count(), % Number of subtelegrams (send: 3 / receive: 0)
      DestID :: eurid(), % Either broadcast or the EURID of the target device
      option( dbm() ),
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
(e.g. a double rocker) as a string or from its short name, and its own channel
(e.g. 2 to designate any "button B").
""".
-type button_ref_spec() :: { user_device_designator(), channel() }.


-doc """
Identifier of a button, from the EURID of its device (e.g. a double rocker) or
from its short name, and its own channel (e.g. 2 to designate any "button B").
""".
-type button_ref() :: { device_designator(), channel() }.



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
The (atom) identifier of an EnOcean Equipment Profile, corresponding to a
`(R-ORG)-(FUNC)-(TYPE)` triplet.

For example the `single_input_contact` EEP identifier corresponds to EEP
`D5-00-01`.

Refer to `get_eep_topic_specs/0` for further details.
""".
-type eep_id() ::
    'thermometer'
  | 'thermo_hygro_low'
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
An EEP defined as a string (e.g. `D5-00-01`).
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

For example: `{2, bottom}`.
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
						 | 'unsupported'. % Hence unknown


-doc "The power currently output by a device (e.g. a smart plug).".
-type power_report() :: 'off'
					  | pos_integer() % Percentage of max power
					  | 'not_available'. % Hence unknown


-doc "The powering status of a device (e.g. a smart plug).".
-type power_status() :: 'power_on' | 'power_off'.



-doc """
The type of a VLD message, in the context of the D2-00 EEPs: `Room Control Panel
(RCP)`.

It is also designated by the MI field of these VLD telegrams, the 3 last bits of
the first byte of the payload (hence 8 possible values).

Described in `[EEP-spec]` p.127.
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
The type of a VLD message, in the context of the D2-01 EEPs: `"Electronic
switches and dimmers with Energy Measurement and Local Control"`.

Refer to the `vld_d2_00_cmd` topic.

It is also designated by the CMD field of these VLD telegrams, the 4 last bits
of the first byte of the payload (hence 16 possible values).

Described in `[EEP-spec]` p.131.
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



-doc """
EURID (EnOcean Unique Radio Identifier) is a unique and non-changeable
identification number (as a 32-bit value) assigned to every EnOcean transmitter
during its production process.

The EURID corresponds to the hexadecimal identifier typically labelled at the
back of devices (e.g. `"ID: B50533EC"`).

Our EURIDs are defined and stored in uppercase, as they are generally written on
devices.

A specific EURID is `0xff-ff-ff-ff` (see the `eurid_broadcast define`), which
denotes a broadcast transmission (as opposed to an Addressed Transmission, ADT).
""".
-type eurid() :: type_utils:uint32(). % Previously was a 32-bit binary.



-doc """
An EURID, expressed as a string.

For example: `"B50533EC"`.
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

Remainder on 8 bits of the modulo 2 division of the `G(x) = x^8 + x^2 + x^1 +
x^0` polynom.
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

Refer to `[ESP3]` p.12.

After a `radio_erp1`, `radio_sub_tel` or `remote_man_command` packet, a response
packet is expected.

See also the `packet_type` topic in the oceanic_generated module.
""".
-type packet_type() ::
	  'reserved' | 'radio_erp1' | 'response'
	| 'radio_sub_tel' | 'event' | 'common_command'
	| 'smart_ack_command' | 'remote_man_command'
	| 'radio_message' | 'radio_erp2' | 'radio_802_15_4'
	| 'command_2_4'.



-doc """
The payload of a (typically ESP3) packet, a sequence of bytes sometimes
designated as 'DataTail', that is all bytes in the `data` chunk (as opposed to
the `optional data` one) found after the R-ORG one.

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



-doc """
Type information regarding a command.

This allows discriminating pure telegram sendings from the activation of (local)
common commands (e.g. to learn the base ID from the Enocean module).
""".
-type command_type() ::
    'telegram_sending'
  | common_command_type(). % A command sent to the Enocean module itself.



-doc "Allows to keep track of an ongoing command.".
-type command_tracking() :: #command_tracking{}.



-doc "The (synchronous) outcome of a sent command.".
-type command_outcome() :: command_response() | 'time_out'.



-doc """
Tracking information regarding a currently pending command.

A timer is used for most commands - except typically internally-triggered common
commands - should no acknowledgement be received on time.
""".
-type waited_command_info() :: { command_tracking(), option( timer_ref() ) }.



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

Refer to `[EEP-spec]` p.35 for further details.
""".
-type thermo_hygro_event() :: #thermo_hygro_event{}.



-doc """
Event sent by EEP D5-00-01: Single Input Contact.

D5-00 corresponds to Contacts and Switches.

Refer to `[EEP-spec]` p.27 for further details.

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

Refer to `[EEP-spec]` p.15 for further details.
""".
-type push_button_switch_event() :: #push_button_switch_event{}.



-doc """
Event sent in the context of EEPs `D2-01-*` (e.g `0A`), corresponding to an
Actuator Status Response (command `0x4`), so that a smart plug reports its
current state.

Refer to `[EEP-spec]` p.135 for further details.
""".
-type smart_plug_status_report_event() :: #smart_plug_status_report_event{}.



%-doc "Event regarding rocker switches.".
%-type rocker_switch_event() :: #rocker_switch_event{}.



-doc """
Event sent in the context of EEP `F6-02-01` and `F6-02-02` ("Light and Blind
Control - Application Style 1 or 2"), for `T21=1`.

Refer to `[EEP-spec]` p.16 for further details.
""".
-type double_rocker_switch_event() :: #double_rocker_switch_event{}.



-doc """
Event sent in the context of EEP F6-02-01 and F6-02-02 ("Light and Blind Control
- Application Style 1 or 2"), for T21=1 and NU=0.

Refer to `[EEP-spec]` p.16 for further details.
""".
-type double_rocker_multipress_event() :: #double_rocker_multipress_event{}.



%-doc "Event regarding position switches.".
%-type position_switch_event() :: #position_switch_event{}.



-doc """
Message (hence not an event per se) corresponding to the receiving a R-ORG
telegram for an universal Teach-in request, EEP based (UTE), one way of pairing
devices.

Refer to `[EEP-gen]` p.17 for further details.
""".
-type teach_request_event() :: #teach_request_event{}.





-doc """
Any event notified by an EnOcean device.

See also their corresponding tags, defined in device_event_type/0.
""".
-type device_event() ::
	% Device events:
	thermo_hygro_event()
  | single_input_contact_event()
  | push_button_switch_event()
  | smart_plug_status_report_event()
  | double_rocker_switch_event()
  | double_rocker_multipress_event()

	% Other events:
  | teach_request_event()
  | command_response().


-doc """
Lists the known types of device events.

Note that they correspond to the tags of the corresponding records.
""".
-type device_event_type() ::
	'thermo_hygro_event'
  | 'single_input_contact_event'
  | 'push_button_switch_event'
  | 'smart_plug_status_report_event'
  | 'double_rocker_switch_event'
  | 'double_rocker_multipress_event'

	% Other events:
  | 'teach_request_event'
  | 'command_response'.



-doc """
Lists the known types of devices.

Each type of device corresponds to an EEP or a set thereof, and is to send at
least one type of events.
""".
-type device_type() ::
	'thermo_hygro_sensor'
  | 'single_contact' % Typically opening detectors
  | 'push_button'
  | 'smart_plug'
  | 'double_rocker'.


-doc """
A convention to interact with a type of device, beyond the EEP(s) that it
implements.
""".
-type device_convention() :: 'standard' % If specified, just tells that the
                                     % the standard behaviour applies.
                           | 'eltako'.  % Eltako-specific behaviour
                                        % (e.g. for smart plugs).


-doc """
A key designating extra information relative to a device.

For example the application style of a rocker.
""".
% In comments, the type of the values associated to each key:
-type device_info_key() ::
    'application_style'     % Associated value of type: application_style()
  | 'convention'               % device_convention()
  | 'expected_periodicity'. % declared_device_activity_periodicity()



-doc """
Any kind of information relative to a device.

For example the application style of a rocker.
""".
% In comments, the key associated to this value, in a device info table:
-type device_info_value() ::
    application_style() % For 'application_style'
  | device_convention() % For 'convention'
  | declared_device_activity_periodicity(). % For 'expected_periodicity'


-doc """
A user-defined table specifying device-specific information.
""".
-type device_info_user_spec() ::
    list_table( device_info_key(), device_info_value() ).


-doc """
An internal device-specific information.
""".
-type device_info_spec() :: table( device_info_key(), device_info_value() ).


-doc """
Information regarding a device (e.g. a double rocker) that is emulated in order
to forge telegrams that it could send, typically in order to trigger actuators.
""".
-type virtual_emitter_info() :: option( tuploid( device_info_value() ) ).


-doc """
Describes an operation that a device is requested to perform.

For example a smart plug required to switch on.

See also `check_device_operation/1`.
""".
-type device_operation() :: 'switch_on' | 'switch_off'. % for smart plugs



-doc """
Describes an elementary information about the state change of a device.

For example may correspond to `channel/0`, for a double-rocker change
information.
""".
-type state_change_info() :: term().




-doc "Describes a stage change of a double-rocker.".
-type double_rocker_state_change_spec() ::
	canon_double_rocker_state_change_spec()
  | { channel(), button_position() } % then transition is 'pressed'
  | channel(). % then position is 'top' and transition is 'pressed'


-doc """
Canonical version of `double_rocker_state_change_spec/0`.

For example, `{2, bottom, released}`.

Both a user-level and an internal type.
""".
-type canon_double_rocker_state_change_spec() ::
	{ channel(), button_position(), button_transition() }.


-doc "Describes a stage change of a push-button.".
-type push_button_state_change_spec() :: canon_push_button_state_change_spec().


-doc """
Canonical version of `push_button_state_change_spec/0`.
""".
-type canon_push_button_state_change_spec() :: button_transition().



-doc """
Describes a stage change of a device.

Used both for incoming and outgoing telegrams.

Abbreviated as SCS.
""".
-type device_state_change_spec() ::
	double_rocker_state_change_spec()
  | push_button_state_change_spec()
  | tuploid( state_change_info() ).


-doc """
Canonical, internal version of `device_state_change_spec/0`.

Abbreviated as CSCS.
""".
-type canon_device_state_change_spec() ::
	canon_double_rocker_state_change_spec()
  | canon_push_button_state_change_spec()
  | tuploid( state_change_info() ).


% For incoming, listened events:

-doc """
Describes how the server can be triggered by a device, by specifying its type
and the event that it may have sent - which will lead the trigger to be
validated.

Notably used to decode incoming trigger events.

For example, `{double_rocker, {2, bottom, released}}.`.
""".
-type incoming_trigger_spec() ::

	{ 'double_rocker', double_rocker_state_change_spec() }

  | { 'push_button', push_button_state_change_spec() }

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

For example `{"002b6b24", {double_rocker, 1}}`, which describes that any
incoming telegram corresponding to a state change of the channel A (i.e. 1) of a
double-rocker of EURID 002b6b24 shall be interpreted as a trigger (typically if
the rocker is pressed).

User-level type.
""".
-type listened_event_spec() ::
	{ EmitterDevice :: option( user_device_designator() ),
      incoming_trigger_spec() }.



-doc """
Canonicalised, internal version of listened_event_spec().

For example: `{"25af97a0", {double_rocker, {2, bottom, released}}}`.

Abbreviated as CLES.
""".
-type canon_listened_event_spec() ::
	{ EmitterDevice :: device_designator(), canon_incoming_trigger_spec() }.


-doc """
The outcome of the match of a trigger event, possibly telling which is the
triggering device and the new device status that shall be set.
""".
-type event_match_trigger_outcome() ::
	'false'
  | { 'true', SourceDevice :: eurid(), device_status() }.



% For outgoing, emitted events:



-doc """
User-level description of an event that may be emitted by this server to send a
command to an actuator.

This event applies to a single target device (it is addressed, not broadcast),
which must have been taught in beforehand to this server.

So for example this event targets a smart plug to control (based on its EURID,
and with addressed telegrams), and tells that it should switch on.

Examples of values: `"05936ef8"`, or its longer `{"05936ef8", switch_on}` form.

No (source) EURID is specified, as anyway the one of the base gateway is the one
that shall be used.

Often abbreviated as EES.
""".
-type emitted_event_spec() ::
    user_device_designator() % Then the default operation for its EEP will
                             % apply.
 | { user_device_designator(), device_operation() }.


-doc """
Canonicalised, internal version of `emitted_event_spec/0`.

The device may or may not be already resolved at this point (requires
device-level information).

Often abbreviated as CEES.
""".
-type canon_emitted_event_spec() ::
    { device_designator(), option( device_operation() ) }.





-doc """
A timer reference.

For example: `{send_local, #Ref<0.2988593563.3655860231.2515>}`.
""".
-type timer_ref() :: timer:tref().


-export_type([ oceanic_availability_outcome/0,

			   oceanic_server_pid/0, serial_server_pid/0, event_listener_pid/0,
			   requester/0,

			   device_name/0, device_plain_name/0, device_naming/0,
               device_any_name/0,
               device_designator_spec/0,
               user_device_designator/0, device_designator/0, designator_pair/0,
               user_device_action/0, device_action/0,

               declared_device_activity_periodicity/0,
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

			   eurid/0, eurid_string/0, eurid_bin_string/0,
			   packet/0, crc/0, esp3_packet/0, packet_type/0,
			   payload/0, vld_payload/0,
			   enocean_version/0, log_counter/0, log_counters/0,
			   command_tracking/0, command_outcome/0,

			   thermo_hygro_event/0, single_input_contact_event/0,
			   push_button_switch_event/0,
			   double_rocker_switch_event/0, double_rocker_multipress_event/0,

			   device_description/0, back_online_info/0, device_event/0,
			   device_event_type/0,

			   device_type/0, device_convention/0, virtual_emitter_info/0,
               device_operation/0,

			   state_change_info/0,
			   device_state_change_spec/0, canon_device_state_change_spec/0,

			   push_button_state_change_spec/0,
			   canon_push_button_state_change_spec/0,

			   double_rocker_state_change_spec/0,
			   canon_double_rocker_state_change_spec/0,

			   incoming_trigger_spec/0, canon_incoming_trigger_spec/0,
			   listened_event_spec/0, canon_listened_event_spec/0,
			   emitted_event_spec/0, event_match_trigger_outcome/0,

			   timer_ref/0 ]).




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
% A previous system to organise sendings had been started, based on
% command_queue(), and another one was also started in a user, higher-level
% library (namely US-Main), yet tracking acknowledgements (which is almost
% necessary, due to the level of telegram losses) is best done here.
%
% So the command_queue() has been replaced with per-actuator tracking
% information (see trigger_track_{spec,info,...}) where request time-outs are
% scheduled (as timers), and incoming telegrams (e.g. VLD ones, for smart plugs)
% are tracked to unschedule the corresponding time-out. Having a time-out be
% triggered would then result in the re-emission of the corresponding telegram,
% provided that the maximum number of retries is not reached.


% Section about how actuators shall be controlled.

% Initially, we were controlling actuators (e.g. a smart plug) only by spoofing
% standard emitters that they would have previously learned, based on telegrams
% that had to be broadcast (as said actuators ignores telegrams specifically
% addressed to them - the basic emitters not being supposed to know them / to be
% able to target them specifically).
%
% This would be for example a double-rocker that would have been learnt by a
% target smart plug, and that would notify that its pseudo-button 2 on channel A
% was pressed, so that the plug switches off.
%
% Yet, because of such a forced broadcast, the spoofing prevented the gateway
% from controlling actuators independently, which was not acceptable (otherwise
% the gateway would be able to send for example only an undiscriminate, global
% switch on/off order).
%
% So we resorted to control actuators in a better way, directly, as any home
% automation gateway, by sending to each of them specific addressed telegrams,
% which can take place only after a more complex teach-in procedure.
%
% We then maintained these two kinds of control (spoofed and broadcast vs direct
% and addressed), yet finally dropped the spoofing approach, as it had little
% interest in itself and, additionally, as it prevented the implementation of
% reliable communications (to overcome telegram loss; crucial for example to
% start/stop a siren): we see that in practice spoofing means broadcast, which
% means that any number of target devices may be reached and thus would be
% expected to respond; short of being able to anticipate at least the number of
% acknowledgement telegrams, a system for the automatic resending of lost
% telegrams cannot exist without additional measures.
%
% So now all telegrams are expected to be sent as a real gateway (not spoofed),
% and be addressed (not broadcast) - even if of course we kept the old spoofing
% / emulating forgery logic (which has just been deactivated).



% Local types:

-doc "32-bit; could have been `type_utils:uint32()`.".
-type esp3_header() :: <<_:32>>.



-doc "Information about a recording of a device event.".
-type recording_info() ::
	{ device_table(), NewDevice :: enocean_device(), Now :: timestamp(),
	  MaybeLastSeen :: option( timestamp() ), option( discovery_origin() ),
	  IsBackOnline :: boolean(), option( device_name() ),
      option( device_short_name() ), option( eep_id() ) }.




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
% See `[EEP-gen]` starting from p.18 for more details.





% Learning/teach-in section.

% Frequently, a learn button on the receiver triggers the teach-in process; then
% the Sender-ID of an arriving telegram is interpreted as an authorized
% information source. To prevent unwanted devices from being learned, the input
% sensitivity of the receiver is often reduced, and the device to be learned
% should be placed close by the receiver. Some transmitters can also be switched
% into the learn-mode via a remote management command. To avoid inadvertent
% learning, the RPS telegrams have to be triggered 3 times within 2 seconds.


-doc """
Definition of the overall state of an Oceanic server, including configuration
(typically loaded from an ETF file).
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

-type uint8() :: type_utils:uint8().
-type tuploid(T) :: type_utils:tuploid(T).

-type timestamp() :: time_utils:timestamp().
-type dhms_duration() :: time_utils:dhms_duration().


-type registration_name() :: naming_utils:registration_name().

-type milliseconds() :: unit_utils:milliseconds().

-type list_table() :: list_table:list_table().
-type list_table( K, V ) :: list_table:list_table( K, V ).

-type command_response() :: oceanic_common_command:command_response().
-type common_command_type() :: oceanic_common_command:common_command_type().
-type decoding_outcome() :: oceanic_decode:decoding_outcome().



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

    % Was unconditional, yet may be emitted too early to be in the advanced
    % traces, better done in registerTraceBridge:
    %
    %trace_bridge:info_fmt( "The initial state of this Oceanic server is: ~ts",
    %                       [ oceanic_text:state_to_string( InitialState ) ] ),

	oceanic_loop( _SkipLen=0, _MaybeTelTail=undefined, InitialState ).



-doc "Returns a base, blank yet initialised, Oceanic state.".
-spec get_base_state( serial_server_pid() ) -> oceanic_state().
get_base_state( SerialServerPid ) ->

	% We discover from start our base EURID; a direct, ad hoc logic cannot
	% really be used, as request tracking would be in the way:
	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug( "Discovering our base EURID." ) ),

	CommonCmd = co_rd_idbase,

	CmdTelegram =
        oceanic_common_command:encode_common_command_tracking( CommonCmd ),

	% For decoding re-use (try_integrate_next_telegram/4), we have to have a
	% state anyway.

    EmptyTable = table:new(),

	InitialState = #oceanic_state{
		serial_server_pid=SerialServerPid,
        % Starting with a bogus value, not wanting an option/1 type:
		emitter_eurid=oceanic_text:string_to_eurid( ?default_emitter_eurid ),
		device_table=EmptyTable,
		command_queue=queue:new(),
        waited_command_info=undefined,
		last_traffic_seen=time_utils:get_timestamp() },

    ExecState = execute_command_impl( _CmdType=CommonCmd, CmdTelegram,
                                      _Requester=internal, InitialState ),

	% Blank start:
	wait_initial_base_command( _ToSkipLen=0, _MaybeAccChunk=undefined,
							   ExecState ).



-doc """
Returns an initialised, Oceanic state, once the initial base ID request has been
properly answered.
""".
-spec wait_initial_base_command( count(), option( telegram_tail() ),
								 oceanic_state() ) -> oceanic_state().
wait_initial_base_command( ToSkipLen, MaybeNextTelTail, State ) ->

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Waiting initial base request "
			"(ToSkipLen=~B, MaybeNextTelTail=~w).",
			[ ToSkipLen, MaybeNextTelTail ] ) ),

	receive

		% Received data from the serial port:
		{ data, NewChunk } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Read ~ts.",
					[ oceanic_text:telegram_to_string( NewChunk ) ] ) ),

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
								[ oceanic_text:telegram_to_string(
                                    DroppedChunk ) ] )

					end,

					cond_utils:if_defined( oceanic_debug_tty,
						trace_bridge:debug_fmt( "Successfully ~ts.",
							[ oceanic_text:device_event_to_string( Event ) ] ),
						basic_utils:ignore_unused( Event ) ),

					trace_bridge:info_fmt( "Detected the EURID of the base "
						"emitter: ~ts.",
                        [ oceanic_text:eurid_to_string( BaseEurid ) ] ),

					ReadState#oceanic_state{ emitter_eurid=BaseEurid,
											 waited_command_info=undefined };


                % Dropping any other unrelated event caught by accident:
				{ decoded, OtherEvent, _MaybeDiscoverOrigin,
						_IsBackOnline, _MaybeDevice, NewMaybeNextTelTail,
						ReadState } ->

                    trace_bridge:debug_fmt( "Dropping unrelated event ~ts read "
                        "while waiting for the initial base ID request.",
                        [ oceanic_text:device_event_to_string( OtherEvent ) ] ),

					wait_initial_base_command( _NewToSkipLen=0,
                                               NewMaybeNextTelTail, ReadState );


				{ Unsuccessful, NewToSkipLen, NewMaybeNextTelTail, NewState } ->

					%cond_utils:if_defined( oceanic_debug_tty,
						trace_bridge:debug_fmt( "Unsuccessful decoding of the "
                            "initial base ID request: '~w' "
							"(whereas NewToSkipLen=~B, "
                            "NewMaybeNextTelTail=~w).",
							[ Unsuccessful, NewToSkipLen,
                              NewMaybeNextTelTail ] ),
					%   basic_utils:ignore_unused(
					%       [ Unsuccessful, NewMaybeNextTelTail ] ) ),

					wait_initial_base_command( NewToSkipLen,
                                               NewMaybeNextTelTail, NewState );

                { considerCommandTimeout, CmdCount } ->
                    throw( { timeout_initial_base_request, CmdCount } )

			end;

		{ onSerialMessage, Msg } ->
			trace_bridge:warning_fmt( "Unexpected serial message while waiting "
                "base command: ~p; ignoring it.", [ Msg ] ),

			wait_initial_base_command( ToSkipLen, MaybeNextTelTail, State )

        % Commented-out, as not wanting to intercept messages to be processed
        % next (like {getOceanicEurid, CallerPid}):
        %
        %Other ->
		%   trace_bridge:warning_fmt( "Unexpected message while waiting "
        %        "base command: ~p; ignoring it.", [ Other ] ),
        %
		%   wait_initial_base_command( ToSkipLen, MaybeNextTelTail, State )

    % Extra time-out for additional safety:
    after 2000 ->

        trace_bridge:error( "Unable to determine the base EURID of "
                            "this gateway." ),

       throw( no_base_eurid_obtained )

	end.



-doc """
Loads Oceanic configuration information from the default Ceylan preferences
file, if any, otherwise returns a state with an empty device table.

Refer to `load_configuration/2` for key information.

See also the `preferences` Myriad module.
""".
-spec load_configuration( oceanic_state() ) -> oceanic_state().
load_configuration( State ) ->
	case preferences:is_preferences_default_file_available() of

		{ true, PrefPath } ->
			LoadedState = load_configuration( PrefPath, State ),

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Initial state: ~ts",
					[ oceanic_text:state_to_string( LoadedState ) ] ) ),

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

- `oceanic_emitter`: to specify the pseudo-device emitting any telegram to be
sent by Oceanic (note that USB gateways have already their own base EURID that
shall be preferred; refer to the `co_rd_idbase` common command)

- `oceanic_jamming_threshold`: to set a non-default threshold

- `oceanic_devices`: to declare the known devices; a given device shall never be
declared more than once
""".
-spec load_configuration( any_file_path(), oceanic_state() ) -> oceanic_state().
load_configuration( ConfFilePath, State ) ->

	file_utils:is_existing_file_or_link( ConfFilePath ) orelse
        throw( { oceanic_config_file_not_found, ConfFilePath } ),

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

	NewDeviceTable = declare_devices( DeviceEntries, _ShortNames=[],
                                      DeviceTable ),

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
-spec declare_devices( [ device_config() ], [ device_short_name() ],
                       device_table() ) -> device_table().
declare_devices( _DeviceCfgs=[], ShortNames, DeviceTable ) ->

    case list_utils:get_duplicates( ShortNames ) of

        [] ->
            DeviceTable;

        Dups ->
            trace_bridge:critical_fmt( "Duplicates found among the short "
                "names of the declared devices: ~ts.",
                [ list_utils:duplicate_info_to_string( Dups ) ] ),

            throw( { duplicated_short_names, Dups } )

    end;

% Adding ExtraDevInfo if needed:
declare_devices( _DeviceCfgs=[ { DevDesigSpec, EuridStr, EepStr } | T ],
                 DevShortNames, DeviceTable ) ->
	declare_devices( [ { DevDesigSpec, EuridStr, EepStr, _ExtraDevInfo=[] }
                                            | T ], DevShortNames, DeviceTable );

% Main clause; any short name set, and any config comment (as last element)
% dropped by the next clause:
%
declare_devices( _DeviceCfgs=[ DC={ _DevDesigSpec={ NameStr, ShortNameAtom },
                                    EuridStr, EepStr, ExtraDevInfo } | T ],
				 DevShortNames, DeviceTable ) ->

	text_utils:is_string( NameStr ) orelse
		begin
			trace_bridge:error_fmt( "Invalid device (long) name ('~p') "
				"in configuration entry ~p (not a string).", [ NameStr, DC ] ),
			throw( { invalid_device_configured_name, NameStr, DC } )
		end,

	is_atom( ShortNameAtom ) orelse
		begin
			trace_bridge:error_fmt( "Invalid device short name ('~p') "
				"in configuration entry ~p (not an atom).",
                [ ShortNameAtom, DC ] ),
			throw( { invalid_device_configured_short_name, ShortNameAtom, DC } )
		end,

    % As multiple short names may not defined:
    NewDevShortNames = case ShortNameAtom of

        undefined ->
           DevShortNames;

        _ ->
            [ ShortNameAtom | DevShortNames ]

    end,

	Eurid = try text_utils:hexastring_to_integer(
		text_utils:ensure_string( EuridStr ), _ExpectPrefix=false ) of

			Int ->
				Int

		% Typically error:badarg:
		catch _:E ->
			trace_bridge:error_fmt( "Invalid EURID ('~ts') "
				"for device named '~ts' in configuration entry ~p.",
                [ EuridStr, NameStr, DC ] ),
			throw( { invalid_device_configured_eurid, EuridStr, E, NameStr } )

	end,

	text_utils:is_string( EepStr ) orelse
		begin
			trace_bridge:error_fmt( "Invalid device EEP ('~p') "
				"in configuration entry ~p.", [ EepStr, DC ] ),
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

    % Checks its structure first:
    DevInfoTable = case list_table:check_proper( ExtraDevInfo ) of

        ok ->
            table:new( ExtraDevInfo );

        DupTable ->
            trace_bridge:error_fmt( "Invalid device information, "
                "duplicates found: ~ts", [ table:to_string( DupTable ) ] ),
            throw( { duplicate_in_device_information, table:keys( DupTable ),
                     NameStr } )

    end,

    { UserActPeriod, ShrunkDevInfoTable } =
        table:extract_entry_with_default( _K=expected_periodicity,
                                          _Def=auto, DevInfoTable ),

    ActPeriod = case UserActPeriod of

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
						"('~p') for device named '~ts' in configuration "
                        "entry ~p.", [ DHMS, NameStr, DC ] ),
					throw( { invalid_dhms_activity_periodicity, DHMS,
							 NameStr } )

			end

	end,

	% Device table indexed by device eurid(), which is duplicated in the record
	% values for convenience:
	%
	DeviceRec = #enocean_device{ eurid=Eurid,
								 name=text_utils:ensure_binary( NameStr ),
                                 short_name=ShortNameAtom,
								 eep=MaybeEepId,
								 discovered_through=configuration,
								 expected_periodicity=ActPeriod,
                                 request_queue=queue:new(),
                                 extra_info=ShrunkDevInfoTable },

	table:has_entry( Eurid, DeviceTable ) andalso
		trace_bridge:warning_fmt( "Overriding entry for device "
			"whose EURID is ~ts (with: ~ts)",
			[ oceanic_text:eurid_to_string( Eurid ),
              oceanic_text:device_to_string( DeviceRec ) ] ),

	% Overriding allowed:
	NewDeviceTable = table:add_entry( Eurid, DeviceRec, DeviceTable ),

	declare_devices( T, NewDevShortNames, NewDeviceTable );


% Adding ShortNameAtom if needed:
declare_devices( _DeviceCfgs=[ { NameStr, EuridStr, EepStr, ExtraDevInfo }
                                 | T ], DevShortNames, DeviceTable ) ->
	declare_devices( [ { { NameStr, _ShortNameAtom=undefined }, EuridStr,
        EepStr, ExtraDevInfo } | T ], DevShortNames, DeviceTable );


% Dropping comment (useful only for the user configuration):
declare_devices( _DeviceCfgs=[ { DevDesigSpec, EuridStr, EepStr,
								 ExtraDevInfo, CommentStr } | T ],
				 DevShortNames, DeviceTable ) when is_list( CommentStr ) ->
	declare_devices( [ { DevDesigSpec, EuridStr, EepStr, ExtraDevInfo } | T ],
					 DevShortNames, DeviceTable );


declare_devices( _DeviceCfgs=[ Other | _T ], _DevShortNames, _DeviceTable ) ->
	throw( { invalid_device_config, Other } ).



-doc """
Decides whether an `auto` activity periodicity mode can be retained, based on
the specified EEP (if any).

We consider that devices implementing some EEPs do not send periodical state
updates.

For example contact switches shall not be left on `auto`, otherwise they are
likely to be considered lost after some time.

If the choice made here is not relevant for a given device, declare for it an
explicit (non-auto) periodicity.
""".
-spec decide_auto_periodicity( option( eep_id() ) ) -> expected_periodicity().
decide_auto_periodicity( _MaybeEEPId=undefined ) ->
	% Not known, supposed not talkative:
	none;

decide_auto_periodicity( _EEPId=thermometer ) ->
	auto;

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



% User-facing utilities.

-doc "Returns a textual description of the specified telegram.".
-spec telegram_to_string( telegram() ) -> ustring().
telegram_to_string( Telegram ) ->
    oceanic_text:telegram_to_string( Telegram ).


-doc """
Returns a textual description of the specified decoded maybe-optional data,
otherwise from the corresponding raw data.
""".
-spec maybe_optional_data_to_string( option( decoded_optional_data() ),
									 telegram_opt_data() ) -> ustring().
maybe_optional_data_to_string( MaybeDecodedOptData, OptData ) ->
    oceanic_text:maybe_optional_data_to_string( MaybeDecodedOptData, OptData ).


-doc """
Returns a (rather complete) textual description of the specified device event.
""".
-spec device_event_to_string( device_event() ) -> ustring().
device_event_to_string( DevEvent ) ->
    oceanic_text:device_event_to_string( DevEvent ).


-doc """
Returns a telegram corresponding to the specified hexadecimal string.

Useful for testing with serial clients like cutecom.
""".
-spec hexastring_to_telegram( ustring() ) -> telegram().
hexastring_to_telegram( HexaStr ) ->
    oceanic_text:hexastring_to_telegram( HexaStr ).



-doc """
Returns the actual EURID corresponding to the specified (plain) EURID string.

For example `3076502 = oceanic:string_to_eurid("002ef196")`.
""".
-spec string_to_eurid( ustring() ) -> eurid().
string_to_eurid( EuridStr ) ->
    oceanic_text:string_to_eurid( EuridStr ).



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

	{ DevEurid, MaybeDevShortName } = get_designator_pair( DevEvent ),

	% Tries to find a CITS corresponding to this emitter:
	case get_maybe_matching_cits( DevEurid, MaybeDevShortName, CLESs ) of

		undefined ->
			false;

		CITS ->
			interpret_cits_matching( CITS, DevEurid, DevEvent )

	end.



-doc """
Returns (any, first) CITS that matches the specified emitter EURID, among the
specified CLES.
""".
-spec get_maybe_matching_cits( eurid(), option( device_short_name() ),
                               [ canon_listened_event_spec() ] ) ->
                                    option( canon_incoming_trigger_spec() ).
get_maybe_matching_cits( _EmitterEurid, _MaybeDevShortName, _CLESs=[] ) ->
	undefined;

% Matching EURID:
get_maybe_matching_cits( EmitterEurid, _MaybeDevShortName,
						 _CLESs=[ { EmitterEurid, CITS } | _T ] ) ->
	CITS;

% Matching short name ('undefined' not allowed in CLES):
get_maybe_matching_cits( _EmitterEurid, DevShortName,
						 _CLESs=[ { DevShortName, CITS } | _T ] ) ->
	CITS;

% Non-matching EmitterEurid:
get_maybe_matching_cits( EmitterEurid, DevShortName, _CLESs=[ _ | T ] ) ->
	get_maybe_matching_cits( EmitterEurid, DevShortName, T ).



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
			[ oceanic_text:device_event_to_string( DevEvent ),
			  oceanic:cits_to_string( CITS ) ] ),
		basic_utils:ignore_unused( [ CITS, DevEvent ] ) ),

	false.





% Helpers for incoming specifications.


-doc """
Canonicalises the specified user-level listening event specifications.

Checks everything but the targeted devices: device short names may still be
specified instead of actual EURIDs: the actual enocean_device records will be
needed for the final conversions to EURIDs.
""".
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

canonicalise_listened_event_specs( _LESs=[ { MaybeUserDevDesig, ITS } | T ],
								   Acc ) ->

    DevDesig = get_internal_device_designator( MaybeUserDevDesig ),

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
			throw( { invalid_listened_event_spec, Other, MaybeUserDevDesig } )

	end,

	CLES = { DevDesig, CITS },

	canonicalise_listened_event_specs( T, [ CLES | Acc ] );

canonicalise_listened_event_specs( _LESs=[ Other | _T ], _Acc ) ->
	throw( { invalid_listened_event_spec, non_pair, Other } ).




% Helpers for outgoing specifications.


-doc """
Canonicalises the specified user-level emitting event specifications.

Is not yet able to resolve default device operation at this point.
""".
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

canonicalise_emitted_event_specs( _EESs=[ { MaybeUserDevDesig, DevOp } | T ],
                                  Acc )  ->
    DevDesig = get_internal_device_designator( MaybeUserDevDesig ),
    check_device_operation( DevOp ),
	canonicalise_emitted_event_specs( T, [ _CEES={ DevDesig, DevOp } | Acc ] );

canonicalise_emitted_event_specs( _EESs=[ MaybeUserDevDesig | T ], Acc ) ->
    DevDesig = get_internal_device_designator( MaybeUserDevDesig ),
    % Default operations not resolvable yet:
	canonicalise_emitted_event_specs( T,
        [ _CEES={ DevDesig, undefined} | Acc ] ).


-doc "Returns the corresponding internal device designator.".
-spec get_internal_device_designator( option( user_device_designator() ) ) ->
                                                device_designator().
get_internal_device_designator( _MaybeUserDevDesig=undefined ) ->
    ?eurid_broadcast;

get_internal_device_designator( DevShortName ) when is_atom( DevShortName ) ->
    DevShortName;

get_internal_device_designator( EuridStr ) when is_list( EuridStr ) ->
    oceanic_text:string_to_eurid( EuridStr );

get_internal_device_designator( Other ) ->
    throw( { invalid_device_designator, Other } ).




-doc "Declares the specified taught-in device.".
-spec declare_device_from_teach_in( eurid(), eep(), device_table() ) ->
						{ device_table(), enocean_device(), timestamp() }.
declare_device_from_teach_in( Eurid, Eep, DeviceTable ) ->

	MaybeEepId = resolve_eep( Eep ),

	Now = time_utils:get_timestamp(),

	NewDevice = case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device whose EURID "
				"is ~ts through teach-in.",
                [ oceanic_text:eurid_to_string( Eurid ) ] ),

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
						[ oceanic_text:device_to_string( Device ), MaybeEepId,
						  KnownEepId ] ),
					KnownEepId

			end,

			Device#enocean_device{ eep=NewEepId,
								   last_seen=Now,
								   telegram_count=TelCount+1 }

	end,

	{ table:add_entry( Eurid, NewDevice, DeviceTable ), NewDevice, Now }.





-doc """
Acknowledges (accepts) the specified teach-in our teach-out request, by sending
a (successful) teach-in response.

See EEP Teach-(In/Out) Response - UTE Message (Broadcast / CMD: `0x1`)
`[EEP-gen]` p.26.
""".
-spec acknowledge_teach_request( teach_request_event(),
                                 oceanic_server_pid() ) -> void().
acknowledge_teach_request(
        TeachReqEv=#teach_request_event{ request_type=teach_in },
        OcSrvPid ) ->
    % Answering unconditionally that teach-in is accepted:
    OcSrvPid ! { acknowledgeTeachRequest,
                    [ TeachReqEv, _TeachOutcome=teach_in_accepted ] };

acknowledge_teach_request(
        TeachReqEv=#teach_request_event{ request_type=teach_out },
		OcSrvPid ) ->
    % Answering unconditionally that teach-out is accepted:
    OcSrvPid ! { acknowledgeTeachRequest,
                    [ TeachReqEv, _TeachOutcome=teach_out_accepted ] }.




-doc """
Acknowledges the specified teach-in request, by sending the specified teach-in
response.

No tracking of any answer is done.

See EEP Teach-In Response - UTE Message (Broadcast / CMD: `0x1`) `[EEP-gen]`
p.26.
""".
-spec acknowledge_teach_request( teach_request_event(), teach_outcome(),
								 oceanic_state() ) -> oceanic_state().
acknowledge_teach_request( #teach_request_event{ source_eurid=InitiatorEurid,
                                                 response_expected=true,
                                                 request_type=teach_in,
                                                 echo_content=EchoContent },
						   TeachOutcome=teach_in_accepted, State ) ->

    % Responding in all cases, even if already taught.

    DevTable = State#oceanic_state.device_table,

    RegState = case table:get_value_with_default( _K=InitiatorEurid,
            _DefV=undefined, DevTable ) of

        undefined ->
            trace_bridge:error_fmt(
                "Taught initiator ~ts not registered (abnormal).",
                [ oceanic_text:eurid_to_string( InitiatorEurid ) ] ),
            State;

        #enocean_device{ taught=true } ->
            trace_bridge:warning_fmt(
                "Initiator ~ts not found (abnormal).",
                [ oceanic_text:eurid_to_string( InitiatorEurid ) ] ),
            State;

        % Hence not already taught:
        EnDevice ->
            NewEnDevice = EnDevice#enocean_device{ taught=true },
            NewDevTable = table:add_entry( InitiatorEurid, NewEnDevice,
                                           DevTable ),
            State#oceanic_state{ device_table=NewDevTable }

    end,

    % We transmit the initiator EURID as the target (thus not ours with
    % State#oceanic_state.emitter_eurid), and bidirectional (not unidirectional)
    % is the relevant setting:
    %
    TeachInRespTel = oceanic_encode:encode_teach_in_response( TeachOutcome,
        InitiatorEurid, _EmitterEurid=State#oceanic_state.emitter_eurid,
        _CommDir=bidirectional, EchoContent ),

	cond_utils:if_defined( oceanic_debug_teaching, trace_bridge:debug_fmt(
       "Acknowledging teach-in request as '~ts', with telegram ~w.",
       [ TeachOutcome, TeachInRespTel ] ) ),
    send_tracked_telegram( TeachInRespTel, _Requester=internal, RegState ).




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
			[ oceanic_text:telegram_to_string( CmdTelegram ),
              RequesterPid ] ) ),

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
-spec get_device_description( device_designator(), oceanic_server_pid() ) ->
												device_description().
get_device_description( DevDesig, OcSrvPid ) ->
	OcSrvPid ! { getDeviceDescription, DevDesig, self() },

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



-doc "Sends the specified telegram, through the specified Oceanic server.".
-spec send( telegram(), oceanic_server_pid() ) -> void().
send( Telegram, OcSrvPid ) ->
	OcSrvPid ! { sendOceanic, Telegram }.




-doc """
Triggers the actuator specified by its emitted event spec, by requesting the
Oceanic server to emit the corresponding telegram and possibly track its
acknowledgement.
""".
-spec trigger_actuator( canon_emitted_event_spec(), oceanic_server_pid() ) ->
                                                        void().
trigger_actuator( CEES, OcSrvPid ) ->
    OcSrvPid ! { triggerActuator, CEES }.



-doc """
Triggers the actuators specified by their event specs, by requesting the Oceanic
server to emit the corresponding telegrams and possibly track their
acknowledgements.
""".
-spec trigger_actuators( [ canon_emitted_event_spec() ],
                                        oceanic_server_pid() ) -> void().
trigger_actuators( CEESs, OcSrvPid ) ->
   OcSrvPid ! { triggerActuators, CEESs }.


-doc """
Triggers the specified actuator in the inverse way of the one specified by its
emitted event spec, by requesting the Oceanic server to emit the corresponding
telegram and possibly track its acknowledgement.

For example, if the actuator is a smart plug meant by the specified CEES to be
switched on, here it will be switched off instead.
""".
-spec trigger_actuator_reciprocal( canon_emitted_event_spec(),
                                        oceanic_server_pid() ) -> void().
trigger_actuator_reciprocal( CEES, OcSrvPid ) ->
    OcSrvPid ! { triggerActuatorReciprocal, CEES }.


-doc """
Triggers the specified actuators in the inverse way of the one specified by
their emitted event spec, by requesting the Oceanic server to emit the
corresponding telegram and possibly track its acknowledgement.

For example, if one of the actuators is a smart plug meant by the specified CEES
to be switched on, here it will be switched off instead.
""".
-spec trigger_actuators_reciprocal( [ canon_emitted_event_spec() ],
                                        oceanic_server_pid() ) -> void().
trigger_actuators_reciprocal( CEESs, OcSrvPid ) ->
   OcSrvPid ! { triggerActuatorsReciprocal, CEESs }.



-doc "Performs the specified device action.".
-spec perform_action( device_action(), oceanic_state() ) -> oceanic_state().
perform_action( _DeviceAction={ DeviceOperation, DeviceDesignator }, State ) ->

    case get_designated_device( DeviceDesignator, State ) of

        undefined ->

            trace_bridge:error_fmt( "Unknown device designated by '~w'; "
                "operation '~w' not performed.",
                [ DeviceDesignator, DeviceOperation ] ),

            State;

        DevEurid ->
            perform_action_on( DevEurid, DeviceOperation, State )

    end;

perform_action( DeviceAction, State ) ->
    trace_bridge:error_fmt( "Unsupported device action '~w', nothing done.",
                            [ DeviceAction ] ),
    State.



-doc """
Returns any EURID found for the specified corresponding device designator.
""".
-spec get_designated_eurid( device_designator(), oceanic_state() ) ->
                                            option( eurid() ).
get_designated_eurid( DevShortName,
                      #oceanic_state{ device_table=DevTable } )
                                      when is_atom( DevShortName ) ->
    DevRecord = get_designated_device_from_short_name( DevShortName, DevTable ),
    DevRecord#enocean_device.eurid;

get_designated_eurid( DeviceEurid, _State ) when is_integer( DeviceEurid ) ->
    DeviceEurid;

get_designated_eurid( _DeviceDesignator, _State ) ->
    %trace_bridge:error_fmt( "Invalid device designator: '~w'.",
    %                        [ DeviceDesignator ] ),
    undefined.



-doc "Returns any device matching the specified designator.".
-spec get_designated_device( device_designator(), oceanic_state() ) ->
                                            option( enocean_device() ).
get_designated_device( DevShortName,
                       #oceanic_state{ device_table=DevTable } )
                                      when is_atom( DevShortName ) ->
    get_designated_device_from_short_name( DevShortName,
                                           table:values( DevTable ) );

get_designated_device( DeviceEurid, _State ) when is_integer( DeviceEurid ) ->
    DeviceEurid;

get_designated_device( _DevDesig, _State ) ->
    %trace_bridge:error_fmt( "Invalid device designator: '~w'.",
    %                        [ DevDesig ] ),
    undefined.



% (helper)
-spec get_designated_device_from_short_name( device_short_name(),
        [ enocean_device() ] ) -> option( enocean_device() ).
get_designated_device_from_short_name( _DevShortName, _Devs=[] ) ->
    undefined;

get_designated_device_from_short_name( DevShortName,
        _Devs=[ Dev=#enocean_device{ short_name=DevShortName } | _T ] ) ->
    Dev;

get_designated_device_from_short_name( DevShortName, _Devs=[ _Dev | T ] ) ->
    get_designated_device_from_short_name( DevShortName, T ).



-doc "Performs the specified action on the specified device.".
-spec perform_action_on( eurid(), device_operation(), oceanic_state() ) ->
                                                oceanic_state().
perform_action_on( TargetEurid, DeviceOperation, State )
        when DeviceOperation =:= switch_on; DeviceOperation =:= switch_off ->

    trace_bridge:debug_fmt( "Performing operation ~ts on device ~ts.",
        [ DeviceOperation,
          oceanic_text:describe_device( TargetEurid, State ) ] ),

    trigger_actuator_impl( TargetEurid, DeviceOperation, State );

perform_action_on( TargetEurid, DeviceOperation, State ) ->

    trace_bridge:error_fmt( "Unsupported operation ~w (requested to be "
        "performed on device ~ts); nothing done.",
        [ DeviceOperation,
          oceanic_text:describe_device( TargetEurid, State ) ] ),

    State.






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

 - `"nothing already read"` (no start byte already found, no tail) - then it is
   equal to undefined

 - `"only the start byte was read (and chopped)"` (empty tail) - then it is
   equal to `<<>>`
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
				"and having accumulated ~ts (waited command info: ~w).",
				[ time_utils:get_textual_timestamp(), SkipStr,
				  text_utils:ellipse( TelTailStr, _MaxLen=120 ),
                  State#oceanic_state.waited_command_info ] )

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

    % Ensure that, if all debug flags are set, at least one trace is emitted per
    % finest clause.
    %
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
					  oceanic_text:telegram_to_hexastring( NewChunk ),
                      ToSkipLen ] ) ),

			JamState = monitor_jamming( NewChunkSize, State ),

			% Note that more than one telegram may be stored in a received data
			% chunk; to avoid any accumulation, each current chunk should be
			% decoded as much as possible (not just the first telegram):
			%
			{ IntegToSkipLen, IntegMaybeTelTail, IntegState } =
				integrate_all_telegrams( ToSkipLen, MaybeTelTail, NewChunk,
										 JamState ),

			oceanic_loop( IntegToSkipLen, IntegMaybeTelTail, IntegState );


        { triggerActuator, CEES={ ActDesig, MaybeDevOp } } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Triggering actuator as ~w.",
                                        [ CEES ] ),
                basic_utils:ignore_unused( CEES ) ),

            TrigState = trigger_actuator_impl( ActDesig, MaybeDevOp, State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, TrigState );


        { triggerActuators, CEESs } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Triggering actuators as ~w.",
                                        [ CEESs ] ) ),

            TrigState = trigger_actuators_impl( CEESs, State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, TrigState );


        { triggerActuatorReciprocal, CEES={ ActDesig, MaybeDevOp } } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Triggering reciprocally "
                    "actuator based on ~w.", [ CEES ] ),
                basic_utils:ignore_unused( CEES ) ),

            TrigState = trigger_actuator_reciprocal_impl( ActDesig,
                MaybeDevOp, State ),

			oceanic_loop( ToSkipLen, MaybeTelTail, TrigState );


        { triggerActuatorsReciprocal, CEESs } ->

             cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Triggering reciprocally "
                    "actuators based on ~w.", [ CEESs ] ) ),

           TrigState = trigger_actuators_reciprocal_impl( CEESs, State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, TrigState );


        { performAction, DeviceAction } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Requested to perform action ~w.",
                                        [ DeviceAction ] ) ),

            ActState = perform_action( DeviceAction, State ),

            oceanic_loop( ToSkipLen, MaybeTelTail, ActState );


        % ActDesig could be supported, rather than just ActEurid:
		{ sendDoubleRockerTelegram, [ ActEurid, COTS,
				_TrackSpec={ _DevEvType, _MaybeExpectedReportedEvInfo } ] } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Server to send a double-rocker "
                    "telegram to ~ts, ~ts.",
                    [ oceanic_text:describe_device( ActEurid, State ),
                      oceanic_text:canon_outgoing_trigger_spec_to_string(
                        COTS ) ] ) ),

			BaseEurid = State#oceanic_state.emitter_eurid,

			% Note that this results in the target button of the target rocker
			% to undergo a single transition (generally 'pressed'), not a double
			% one (e.g. 'pressed' then 'released'), as it showed sufficient to
			% trigger all tested actuators (they are then not blocked waiting
			% for a 'released' message):
			%
			RockerTelegram = oceanic_encode:encode_double_rocker_telegram(
                BaseEurid, COTS, ActEurid ),

            DeviceTable = State#oceanic_state.device_table,

            NewState = case table:get_value_with_default( _K=ActEurid,
                    _Def=undefined, DeviceTable ) of

                undefined ->
                    trace_bridge:warning_fmt( "Actuator ~ts not known, not "
                        "tracking its requests.",
                        [ oceanic_text:eurid_to_string( ActEurid ) ] ),
                     State;

                DevRecord ->
                    ReqTrk = #request_tracking{ request_telegram=RockerTelegram,
                                                target_eurid=ActEurid,
                                                operation=fixme,
                                                sent_count=1,
                                                % No requester PID:
                                                requester=internal },
                    ReqQueue = DevRecord#enocean_device.request_queue,
                    ExpandedReqQueue = queue:in( ReqTrk, ReqQueue ),

                    % MaybeReqInfo and ExpandedReqQueue to be set in DevRecord
                    % next:
                    MaybeReqInfo = DevRecord#enocean_device.waited_request_info,

                    handle_next_request( MaybeReqInfo, ExpandedReqQueue,
                                         DevRecord, State )

            end,
			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


        { acknowledgeTeachRequest, [ TeachReqEv, TeachOutcome ] } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Acknowledging teach request ~w "
                    "(outcome: ~w).", [ TeachReqEv, TeachOutcome ] ) ),

            AckState = acknowledge_teach_request( TeachReqEv, TeachOutcome,
                                                  State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, AckState );


		{ onSerialMessage, Msg } ->
			trace_bridge:warning( text_utils:ensure_string( Msg ) ),
			oceanic_loop( ToSkipLen, MaybeTelTail, State );


		% To track lost devices:
		{ onActivityTimeout, LostEurid, PeriodicityMs } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug_fmt( "Activity timeout for EURID ~ts "
                    "(periodicity: ~B)",
                    [ oceanic_text:eurid_to_string( LostEurid ),
                      PeriodicityMs ] ) ),

			DeviceTable = State#oceanic_state.device_table,

			NewDeviceTable =
					case table:lookup_entry( LostEurid, DeviceTable ) of

				key_not_found ->
					% Really abnormal:
					trace_bridge:error_fmt( "A sensor whose EURID is '~ts' was "
						"reported as lost whereas it is not known.",
						[ oceanic_text:eurid_to_string( LostEurid ) ] ),
					% This EURID is not specifically registered.
					DeviceTable;

				{ value, LostDevice } ->

					cond_utils:if_defined( oceanic_debug_activity,
						trace_bridge:debug_fmt(
							"Activity time-out (after ~ts) for device ~ts.",
							[ time_utils:duration_to_string( PeriodicityMs ),
							  oceanic_text:device_to_string( LostDevice ) ] ) ),

					IsNewLoss =
						LostDevice#enocean_device.availability =:= online,

					LostMsg = { onEnoceanDeviceLost, [ LostEurid,
						LostDevice#enocean_device.name,
						oceanic_text:get_device_description( LostDevice ),
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


		{ executeCommonCommand, CommonCommand, RequesterPid } ->

			cond_utils:if_defined( oceanic_debug_commands,
				trace_bridge:debug_fmt(
					"Requested to execute common command '~ts', "
					"on behalf of requester ~w.",
					[ CommonCommand, RequesterPid ] ) ),

			CmdTelegram = oceanic_common_command:encode_common_command_tracking(
                CommonCommand ),

			% Response will be automatically sent back to the requester when
			% decoding it (refer to
			% oceanic_common_command:decode_response_tail/5).

			ExecState = execute_command_impl( _CmdType=CommonCommand,
                CmdTelegram, RequesterPid, State ),

			oceanic_loop( ToSkipLen, MaybeTelTail, ExecState );


		{ getOceanicEurid, RequesterPid } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug( "(getting Oceanic EURID)" ) ),

			RequesterPid !
				{ oceanic_eurid, State#oceanic_state.emitter_eurid },

			oceanic_loop( ToSkipLen, MaybeTelTail, State );



		{ getDeviceDescription, DevDesig, RequesterPid } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug( "(getting device description)" ) ),

			BinDesc = oceanic_text:describe_device( DevDesig, State ),

			RequesterPid ! { oceanic_device_description, BinDesc },

			oceanic_loop( ToSkipLen, MaybeTelTail, State );



		% Sent by the timer associated to a currently request for a device, a
		% timer that here just expired:
		%
		{ considerRequestTimeout, ActEurid } ->

            DeviceTable = State#oceanic_state.device_table,

            NewState = case table:get_value_with_default( _K=ActEurid,
                    _Def=undefined, DeviceTable ) of

                % Device not known:
                undefined ->
					trace_bridge:error_fmt( "Received a request time-out for "
                        "unknown actuator ~ts; ignoring it.",
						[ oceanic_text:describe_device( ActEurid, State ) ] ),
					State;


                #enocean_device{ waited_request_info=undefined } ->
					trace_bridge:error_fmt( "Received a request time-out for "
                        "actuator ~ts, whereas it had not request on the air; "
                        "ignoring it.",
						[ oceanic_text:describe_device( ActEurid, State ) ] ),
					State;


                % Legit time-out received, traced and then ignored:
                DevRecord=#enocean_device{
                        waited_request_info={ ReqTrk, _TimerRef },
                        request_queue=ReqQueue } ->

                    SentCount = ReqTrk#request_tracking.sent_count,
                    MaxCount = ReqTrk#request_tracking.max_send_count,

                    case SentCount < MaxCount of

                        true ->
                            cond_utils:if_defined( oceanic_debug_requests,
                                trace_bridge:warning_fmt( "Request time-out "
                                    "received for actuator ~ts, re-sending it, "
                                    "as it was already sent ~B times, while "
                                    "maximum send count is ~B.",
                                    [ oceanic_text:describe_device( ActEurid,
                                                                    State ),
                                      SentCount, MaxCount ] ) ),

                            NewReqTrk = ReqTrk#request_tracking{
                                sent_count=SentCount+1 },

                            ReqQueue = DevRecord#enocean_device.request_queue,

                            ExpandedReqQueue = queue:in( NewReqTrk, ReqQueue ),

                            % ReqInfo cleared, and ExpandedReqQueue to be set in
                            % DevRecord next:
                            %
                            handle_next_request( _MaybeReqInfo=undefined,
                                ExpandedReqQueue, DevRecord, State );

                       false ->

                            basic_utils:assert_equal( SentCount, MaxCount ),

                            manage_request_failure( ActEurid, DevRecord,
                                                    MaxCount, State )

                    end

            end,

			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		% Sent by the timer associated to the currently pending command, a timer
		% that here just expired:
		%
		{ considerCommandTimeout, CmdCount } ->

			TimeState = case State#oceanic_state.waited_command_info of

				% Surprising:
				undefined ->
					trace_bridge:error_fmt( "Received a command time-out "
						"(for command count #~B), whereas no command is "
						"awaited and the current count is #~B; ignoring it.",
						[ CmdCount, State#oceanic_state.command_count ] ),
					State;

                % Legit time-out received, traced and then ignored:
				_CmdInfo={ CmdReq=#command_tracking{ requester=internal },
						   ThisTimerRef } ->

                    trace_bridge:error_fmt( "Time-out received for "
                            "internal command ~ts (timer reference: ~w), "
                            "and ignored.",
							[ oceanic_text:command_tracking_to_string(
                                CmdReq ), ThisTimerRef ] ),

					State#oceanic_state{ waited_command_info=undefined };


				_CmdInfo={ CmdReq=#command_tracking{ requester=RequesterPid },
						   ThisTimerRef } ->

					cond_utils:if_defined( oceanic_check_commands,
						CmdCount = State#oceanic_state.command_count ),

					cond_utils:if_defined( oceanic_debug_commands,
						trace_bridge:debug_fmt( "Sending to requester "
							"a time-out (reference: ~w) regarding command ~ts.",
							[ ThisTimerRef,
                              oceanic_text:command_tracking_to_string(
                                CmdReq ) ] ),
						basic_utils:ignore_unused( [ CmdReq, ThisTimerRef ] ) ),

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

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug( "(adding configuration settings)" ) ),

			NewState = apply_conf_settings( OcSettings, State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, NewState );


		% So that the traces emitted by Oceanic thanks to trace_bridge:* are
		% integrated in a more advanced trace system (typically Ceylan-Traces):
		%
		{ registerTraceBridge, BridgeSpec } ->
			trace_bridge:register( BridgeSpec ),

			trace_bridge:info_fmt( "Just registered the trace bridge "
				"specification ~p; current state is ~ts.",
                [ BridgeSpec, oceanic_text:state_to_string( State ) ] ),

			oceanic_loop( ToSkipLen, MaybeTelTail, State );


		% Mostly useful for testing purpose:
		{ sendOceanic, Telegram } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug( "(sending telegram)" ) ),

			SentState = send_tracked_telegram( Telegram, _Requester=internal,
                                               State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, SentState );

		% Mostly useful for testing purpose:
        %
        % (requester to receive back a {onOceanicSendingOutcome,
        %  oceanic_common_command:common_command_status()} message)
        %
		{ sendOceanic, Telegram, RequesterPid } ->

            cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug( "(sending telegram for requester)" ) ),

			SentState = send_tracked_telegram( Telegram, RequesterPid, State ),
			oceanic_loop( ToSkipLen, MaybeTelTail, SentState );


		{ testSerialAvailability, [], SenderPid } ->

           cond_utils:if_defined( oceanic_debug_activity,
                trace_bridge:debug( "(testing serial availability)" ) ),

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
					[ oceanic_text:telegram_to_string( Telegram ) ] ) ),

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
					[ SerialPid, oceanic_text:state_to_string( State ) ] ) ),

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
					[ SerialPid, oceanic_text:state_to_string( State ) ] ) ),

			receive

				serial_stopped ->
					ok

			end,

			SenderPid ! oceanic_terminated,

			trace_bridge:debug_fmt(
				"Oceanic server ~w terminated synchronously.", [ self() ] );

		UnexpectedMsg ->
			trace_bridge:warning_fmt( "Oceanic server ~w received an "
				"unexpected message: '~w', ignoring it.",
                [ self(), UnexpectedMsg ] ),

			oceanic_loop( ToSkipLen, MaybeTelTail, State )

	end.



-doc """
Manages the failure to acknowledge all attempts of a request sent to the
specified actuator.
""".
-spec manage_request_failure( eurid(), enocean_device(), count(),
                              oceanic_state() ) -> oceanic_state().
manage_request_failure( ActEurid, DevRecord, FailedCount, State ) ->

    % Special-casing Eltako smart plugs for their request failure, as if they
    % are already in the target state (e.g. off, for example initially, when all
    % lights are switched off unconditionally), they will never send back a
    % telegram when requested to switch, and thus the request will appear
    % failed.
    %
    % As a result, as soon as a switch ack sent by such plugs is lost, the
    % corresponding request will be reported failed.

    DevInfoTable = DevRecord#enocean_device.extra_info,

    % For example standard, eltako, etc.:
    Convention = table:get_value_with_default( convention, standard,
                                               DevInfoTable ),

    Msg = text_utils:format( "Request time-out received for actuator ~ts, "
        "giving up re-sending it, as it reached its maximum send count of ~B.",
         [ oceanic_text:describe_device( ActEurid, State ), FailedCount ] ),

    case Convention of

        eltako ->
            trace_bridge:warning( Msg ++ " Either the single acknowledgement "
                "it sent has been lost, or this Eltako smart plug was already "
                "in its target state (of course if this plug is functional)." );

        _ ->
           trace_bridge:error( Msg )

    end,

    ReqQueue = DevRecord#enocean_device.request_queue,

    % ReqInfo cleared, and ExpandedReqQueue to be set in
    % DevRecord next:
    %
    handle_next_request( _MaybeReqInfo=undefined, ReqQueue, DevRecord, State ).


% Trigger internal helpers.


-doc """
Triggers the specified actuator so that it performs the specified operation.
""".
-spec trigger_actuator_impl( device_designator(), option( device_operation() ),
                                    oceanic_state() ) -> oceanic_state().
trigger_actuator_impl( ActDesig, _MaybeDevOp=undefined, State ) ->
    DevOp = get_default_operation_for( ActDesig, State ),
    % A next clause:
    trigger_actuator_impl( ActDesig, DevOp, State );

trigger_actuator_impl( ActDesig, DevOp=switch_on,
                       State=#oceanic_state{ emitter_eurid=SourceEurid } ) ->

    DevRecord = case get_designated_device( ActDesig, State ) of

        undefined ->
            throw( { unknown_actuator_device, ActDesig } );

        DR ->
            DR

    end,

    ActEurid = DevRecord#enocean_device.eurid,

    % If the target device is known, and if its extra information tell that it
    % should be managed specifically, do so:
    %
    DevInfoTable = DevRecord#enocean_device.extra_info,

    % For example standard, eltako, etc.:
    Convention = table:get_value_with_default( convention, standard,
                                               DevInfoTable ),

    SwitchTel = case Convention of

        % Here spoofing a (previously-learnt, possibly virtual) double rocker:
        eltako ->

            % Currently relying on hardcoded defaults:

            % We designate the left rocker button:
            ButtonChannel = 1,
            %ButtonChannel = 2,

            ButtonPos = top,

            ButtonLoc = { ButtonChannel, ButtonPos },

            % Meaning "F6-02-01":
            EepId = double_rocker_switch_style_1,

            SourceAppStyle = oceanic:get_app_style_from_eep( EepId ),

            % Not need to send afterwards a corresponding "released" telegram:
            oceanic_encode:encode_double_rocker_switch_telegram( SourceEurid,
                SourceAppStyle, ButtonLoc, _ButTrans=pressed,
                _MaybeTargetEurid=ActEurid );

        % Here we act as a normal, taught-in gateway:
        _ -> % Most probably 'standard'
            oceanic_encode:encode_switch_dimmer_set_output( SourceEurid,
                _TargetStatus=on, _MaybeTargetEurid=ActEurid )

    end,

    cond_utils:if_defined( oceanic_debug_activity,
        trace_bridge:debug_fmt( "Enqueuing a request to actuator ~ts: "
            "switch-on telegram based on the ~ts convention.",
            [ oceanic_text:describe_device( ActEurid, State ), Convention ] ) ),

    ReqTrk = #request_tracking{ request_telegram=SwitchTel,
                                target_eurid=ActEurid,
                                operation=DevOp,
                                sent_count=1,
                                requester=internal },

    ReqQueue = DevRecord#enocean_device.request_queue,

	ExpandedReqQueue = queue:in( ReqTrk, ReqQueue ),

    % MaybeReqInfo and ExpandedReqQueue to be set in DevRecord next:
    MaybeReqInfo = DevRecord#enocean_device.waited_request_info,

	handle_next_request( MaybeReqInfo, ExpandedReqQueue, DevRecord, State );



trigger_actuator_impl( ActDesig, DevOp=switch_off,
                       State=#oceanic_state{ emitter_eurid=SourceEurid } ) ->

    DevRecord = case get_designated_device( ActDesig, State ) of

        undefined ->
            throw( { unknown_actuator_device, ActDesig } );

        DR ->
            DR

    end,

    ActEurid = DevRecord#enocean_device.eurid,

    % If the target device is known, and if its extra information tell that it
    % should be managed specifically, do so:
    %
    DevInfoTable = DevRecord#enocean_device.extra_info,

    % For example standard, eltako, etc.:
    Convention = table:get_value_with_default( convention, standard,
                                               DevInfoTable ),

    SwitchTel = case Convention of

        % Here we act as a normal, taught-in gateway:
        standard ->
            oceanic_encode:encode_switch_dimmer_set_output( SourceEurid,
                _TargetStatus=off, _MaybeTargetEurid=ActEurid );

        % Here spoofing a (previously-learnt, possibly virtual) double rocker:
        eltako ->

            % Currently relying on hardcoded defaults:

            % We designate the left rocker button:
            ButtonChannel = 1,
            %ButtonChannel = 2,

            ButtonPos = bottom,

            ButtonLoc = { ButtonChannel, ButtonPos },

            % Meaning "F6-02-01":
            EepId = double_rocker_switch_style_1,

            SourceAppStyle = oceanic:get_app_style_from_eep( EepId ),

            % Not need to send afterwards a correspoding "released" telegram:
            oceanic_encode:encode_double_rocker_switch_telegram( SourceEurid,
                SourceAppStyle, ButtonLoc, _ButTrans=pressed,
                _MaybeTargetEurid=ActEurid )

    end,

    cond_utils:if_defined( oceanic_debug_activity,
        trace_bridge:debug_fmt( "Enqueuing a request to actuator ~ts: "
            "switch-off telegram based on the ~ts convention.",
            [ oceanic_text:describe_device( ActEurid, State ), Convention ] ) ),

    ReqTrk = #request_tracking{ request_telegram=SwitchTel,
                                target_eurid=ActEurid,
                                operation=DevOp,
                                sent_count=1,
                                requester=internal },

    ReqQueue = DevRecord#enocean_device.request_queue,

	ExpandedReqQueue = queue:in( ReqTrk, ReqQueue ),

    MaybeReqInfo = DevRecord#enocean_device.waited_request_info,

	handle_next_request( MaybeReqInfo, ExpandedReqQueue, DevRecord, State );


trigger_actuator_impl ( ActEurid, DevOp, _State ) ->
   throw( { unsupported_device_operation, DevOp,
             { actuator, oceanic_text:eurid_to_string( ActEurid ) } } ).



-doc """
Triggers the specified actuators so that they perform their respective specified
operation.
""".
-spec trigger_actuators_impl( [ canon_emitted_event_spec() ],
                                    oceanic_state() ) -> oceanic_state().
trigger_actuators_impl( _CEESs=[], State ) ->
    State;

trigger_actuators_impl( _CEESs=[ { ActEurid, MaybeDevOp } | T ], State ) ->
    TrigState = trigger_actuator_impl( ActEurid, MaybeDevOp, State ),
    trigger_actuators_impl( T, TrigState ).



-doc """
Triggers the specified actuator so that it performs the reciprocal of the
specified operation.
""".
-spec trigger_actuator_reciprocal_impl( eurid(), option( device_operation() ),
                                    oceanic_state() ) -> oceanic_state().
trigger_actuator_reciprocal_impl( ActEurid, _MaybeDevOp=undefined, State ) ->
    DevOp = get_default_operation_for( ActEurid, State ),
    % A next clause:
    trigger_actuator_reciprocal_impl( ActEurid, DevOp, State );

trigger_actuator_reciprocal_impl( ActEurid, _DevOp=switch_on, State ) ->
    trigger_actuator_impl( ActEurid, switch_off, State );

trigger_actuator_reciprocal_impl( ActEurid, _DevOp=switch_off, State ) ->
    trigger_actuator_impl( ActEurid, switch_on, State );

trigger_actuator_reciprocal_impl ( ActEurid, DevOp, _State ) ->
    throw( { unsupported_device_reciprocal_operation, DevOp,
             { actuator, oceanic_text:eurid_to_string( ActEurid ) } } ).


-doc """
Triggers the specified actuators so that they perform the reciprocal of their
respective specified operation.
""".
-spec trigger_actuators_reciprocal_impl( [ canon_emitted_event_spec() ],
                                    oceanic_state() ) -> oceanic_state().
trigger_actuators_reciprocal_impl( _CEESs=[], State ) ->
    State;

trigger_actuators_reciprocal_impl( _CEESs=[ { ActEurid, MaybeDevOp } | T ],
                                   State ) ->
    TrigState = trigger_actuator_reciprocal_impl( ActEurid, MaybeDevOp, State ),
    trigger_actuators_reciprocal_impl( T, TrigState ).



-doc """
Handles, if appropriate, the sending of the next request, using the specified
queue for that.
""".
% MaybeWaitRqInfo shall apply from now:
-spec handle_next_request( option( waited_request_info() ), request_queue(),
    enocean_device(), oceanic_state() ) -> oceanic_state().

% Here no request is already waited, so we can send one directly - provided
% there is any.
%
handle_next_request( _MaybeWaitRqInfo=undefined, CurrentReqQueue, DevRecord,
                     State ) ->

	case queue:out( CurrentReqQueue ) of

		{ { value, OldestReqTrk=#request_tracking{
					request_telegram=ReqTelegram } },
				ShrunkReqQueue } ->

            cond_utils:if_defined( oceanic_debug_requests,
                trace_bridge:debug_fmt( "Dequeuing next request ~w, "
                    "enqueuing as a new command.", [ OldestReqTrk ] ) ),

            CmdState = execute_command_impl( _CmdType=telegram_sending,
                ReqTelegram, _Requester=internal, State ),

            ActEurid = OldestReqTrk#request_tracking.target_eurid,

            % Note that the request timer starts now, whereas the command queue
            % may delay the actual telegram sending:
            %
            { ok, TimerRef } = timer:send_after(
                _Duration=?default_max_request_response_waiting_duration,
                _Msg={ considerRequestTimeout, ActEurid } ),

            cond_utils:if_defined( oceanic_debug_requests,
                trace_bridge:debug_fmt( "No request was on the air for ~ts, "
                    "dequeued ~w posted as a command (size of shrunk "
                    "queue: ~B; timer ref: ~w).",
                    [ oceanic_text:eurid_to_string( ActEurid ), OldestReqTrk,
                      queue:len( ShrunkReqQueue ), TimerRef ] ) ),

			ReqInfo = { OldestReqTrk, TimerRef },

            % The ack of this sending will allow to further dequeue if possible.

            NewDevRecord = DevRecord#enocean_device{
                request_queue=ShrunkReqQueue,
                waited_request_info=ReqInfo },

            NewDeviceTable = table:add_entry( _K=DevRecord#enocean_device.eurid,
                _V=NewDevRecord, State#oceanic_state.device_table ),

            CmdState#oceanic_state{ device_table=NewDeviceTable };


		% Nothing that is already queued to send here:
		{ empty, _SameCurrentQueue } ->

            cond_utils:if_defined( oceanic_debug_requests,
                trace_bridge:debug( "(no request to dequeue)" ) ),

            NewDevRecord = DevRecord#enocean_device{
                waited_request_info=undefined },

            NewDeviceTable = table:add_entry( _K=DevRecord#enocean_device.eurid,
                _V=NewDevRecord, State#oceanic_state.device_table ),

            State#oceanic_state{ device_table=NewDeviceTable }

	end;

% Here there is already a waited request, we just update with the new queue and
% wait info:
%
handle_next_request( WaitRqInfo, CurrentReqQueue, DevRecord, State ) ->

    cond_utils:if_defined( oceanic_debug_requests, trace_bridge:debug_fmt(
        "(a request is already on the air, none of the ~B ones dequeued)",
        [ queue:len( CurrentReqQueue ) ] ) ),

    NewDevRecord = DevRecord#enocean_device{ request_queue=CurrentReqQueue,
                                             waited_request_info=WaitRqInfo },

    NewDeviceTable = table:add_entry( _K=DevRecord#enocean_device.eurid,
        _V=NewDevRecord, State#oceanic_state.device_table ),

    State#oceanic_state{ device_table=NewDeviceTable }.



-doc """
Returns the convention according to which a gateway shall interact with the
specified device.
""".
-spec get_device_convention( eurid(), device_table() ) -> device_convention().
get_device_convention( TargetEurid, DeviceTable ) ->
    case table:get_value_with_default( _K=TargetEurid,
            _Def=undefined, DeviceTable ) of

        % Device not known:
        undefined ->
            standard;

        #enocean_device{ extra_info=DevInfoTable } ->
             % For example standard, eltako, etc.:
             table:get_value_with_default( convention, standard, DevInfoTable )

    end.



-doc """
Checks the specified device operation.

Refer to the `device_operation/0` type.
""".
-spec check_device_operation( term() ) -> void().
check_device_operation( _DevOp=switch_on ) ->
    ok;

check_device_operation( _DevOp=switch_off ) ->
    ok;

check_device_operation( DevOp ) ->
    throw( { invalid_device_operation, DevOp } ).



-doc "Returns the default operation corresponding to the specified actuator.".
-spec get_default_operation_for( device_designator(), oceanic_state() ) ->
                                                device_operation().
get_default_operation_for( ActDesig, State ) ->

    case get_designated_device( ActDesig, State ) of

         undefined ->
            throw( { unregistered_actuator_device, ActDesig } );

        #enocean_device{ eep=Eep } ->
            get_base_operation_for_eep( Eep )

    end.



-doc "Returns the base operation corresponding to the specified EEP.".
-spec get_base_operation_for_eep( eep_id() ) -> device_operation().
get_base_operation_for_eep( _Eep=smart_plug ) ->
    switch_on;

get_base_operation_for_eep( _Eep=smart_plug_with_metering ) ->
    switch_on;

get_base_operation_for_eep( Eep ) ->
    throw( { no_base_operation_for_eep, Eep } ).



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
				"(decoded a command_processed event)" ) ),

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
						[ DiscStr,
                          oceanic_text:device_event_to_string( Event ) ] )

				end ),

			EventSrcEurid = get_source_eurid( Event ),

			% Reducing noise, by not relaying the telegrams that it received
			% (possibly through repeating) yet that were presumably sent by this
			% gateway itself:
			%
			case EventSrcEurid =:= State#oceanic_state.emitter_eurid of

				true ->
					%trace_bridge:debug_fmt( "Skipping following self-emitted "
					%   "event: ~ts",
                    % [ oceanic_text:device_event_to_string( Event ) ] ),

					integrate_all_telegrams( _SkipLen=0, NextMaybeTelTail,
											 _Chunk= <<>>, NewState );

				false ->

					DeviceMsg = case MaybeDiscoverOrigin of

						% Most common case (already detected, hence not set):
						undefined ->
							BackOnlineInfo = case IsBackOnline of

								true ->
									case MaybeDevice of

										undefined ->
											throw(
                                                { unexpected_undefined_device,
                                                  Event } );

										Device ->
											oceanic_text:get_device_description(
                                                Device )

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
								oceanic_text:get_device_description( Device )

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
										 _Chunk= <<>>, NewState )

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
-spec execute_command_impl( command_type(), telegram(), requester(),
                            oceanic_state() ) -> oceanic_state().
execute_command_impl( CmdType, CmdTelegram, Requester,
                      State=#oceanic_state{ command_queue=CmdQueue } ) ->

    cond_utils:if_defined( oceanic_debug_commands, trace_bridge:debug_fmt(
        "Enqueuing command of type '~ts' to execute: telegram is ~w, "
        "requester is ~w.", [ CmdType, CmdTelegram, Requester ] ) ),

	CmdTrk = #command_tracking{ command_type=CmdType,
                                command_telegram=CmdTelegram,
                                requester=Requester },

	ExpandedCmdQueue = queue:in( CmdTrk, CmdQueue ),

	handle_next_command( ExpandedCmdQueue, State ).



-doc """
Handles, if appropriate, the sending of the next command, using the specified
queue for that.
""".
-spec handle_next_command( command_queue(), oceanic_state() ) ->
										oceanic_state().

% Here no command is already waited, so we can send one directly - provided
% there is any.
%
handle_next_command( CurrentCmdQueue, State=#oceanic_state{
										waited_command_info=undefined } ) ->

	case queue:out( CurrentCmdQueue ) of

		{ { value, OldestCmdTrk=#command_tracking{
					command_telegram=CmdTelegram } },
				ShrunkCmdQueue } ->

            cond_utils:if_defined( oceanic_debug_commands,
                trace_bridge:debug_fmt( "Dequeuing next command ~w.",
                                        [ OldestCmdTrk ] ) ),

            % Comment to test time-out:
			% (not a record mistake)
			SentState = #oceanic_state{ command_wait_timeout=MaxWaitMs,
										command_count=CmdCount } =
				send_raw_telegram( CmdTelegram, State ),

            % Test code with no actual sending, meant to trigger a time-out:

            %MaxWaitMs = State#oceanic_state.command_wait_timeout,
            %CmdCount = State#oceanic_state.command_count,

            % Yet even for testing we need to wait until the base EURID has been
            % obtained:

            % InitialBogusEurid =
            %     oceanic_text:string_to_eurid( ?default_emitter_eurid ),
            %
            % SentState = case State#oceanic_state.emitter_eurid of
            %
            %     InitialBogusEurid ->
            %         trace_bridge:warning_fmt( "Sending first telegram ~w.",
            %             [ CmdTelegram ] ),
            %         send_raw_telegram( CmdTelegram, State );
            %
            %     Emitter ->
            %         trace_bridge:warning_fmt( "Skipping on purpose the "
            %             "sending of telegram ~w (emitter is ~ts).",
            %              [ CmdTelegram,
            %                oceanic_text:eurid_to_string( Emitter ) ] ),
            %         State
            %
            % end,

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

            cond_utils:if_defined( oceanic_debug_commands,
                trace_bridge:debug_fmt( "No command was on the air, dequeued "
                    "command to execute #~B: ~w (timer ref: ~w).",
                    [ NewCmdCount, OldestCmdTrk, TimerRef ] ) ),

			CmdInfo = { OldestCmdTrk, TimerRef },

            % The ack of this sending will allow to further dequeue if possible.

			SentState#oceanic_state{ command_queue=ShrunkCmdQueue,
									 waited_command_info=CmdInfo,
									 command_count=NewCmdCount };

		% Nothing that is already queued to send here:
		{ empty, SameCurrentCmdQueue } ->

            cond_utils:if_defined( oceanic_debug_commands,
                trace_bridge:debug( "(no command to dequeue)" ) ),

            State#oceanic_state{ command_queue=SameCurrentCmdQueue }

	end;

% Here there is already a waited command, we just update with the new queue:
handle_next_command( CurrentCmdQueue, State ) ->

    cond_utils:if_defined( oceanic_debug_commands, trace_bridge:debug_fmt(
        "(a command is already on the air, none of the ~B ones dequeued)",
        [ queue:len( CurrentCmdQueue ) ] ) ),

	State#oceanic_state{ command_queue=CurrentCmdQueue }.



-doc """
Sends, from this Oceanic server, the specified telegram on behalf on the
specified requester.

Note that this lowest-level function just sends a telegram with no flow control,
queuing, etc.; refer instead to the primitive that shall be generally used
whenever sending a telegram, `execute_command_impl/4` (which ultimately relies
on this function).

Therefore this function is not to be called directly, see
`send_tracked_telegram/3` instead.
""".
-spec send_raw_telegram( telegram(), oceanic_state() ) -> oceanic_state().
send_raw_telegram( Telegram, State=#oceanic_state{
                                        serial_server_pid=SerialPid,
										sent_count=SentCount } ) ->

	% Not useful: ActualSending = binary_to_list( Telegram ),
	ActualSending = Telegram,

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt(
			"Sending to serial server ~w the actual telegram ~w "
			"(hexadecimal form: '~ts').",
			[ SerialPid, ActualSending,
			  oceanic_text:telegram_to_hexastring( Telegram ) ] ) ),

	SerialPid ! { send, ActualSending },
	State#oceanic_state{ sent_count=SentCount+1 }.


-doc """
Sends, from this Oceanic server, the specified telegram on behalf on the
specified requester, and tracks its acknowledgement by the Enocean module (based
on a response_type telegram sent back).
""".
-spec send_tracked_telegram( telegram(), requester(), oceanic_state() ) ->
                                            oceanic_state().
send_tracked_telegram( Telegram, Requester, State ) ->
    % So not as simple as: send_raw_telegram(Telegram, State).
    execute_command_impl( _CmdType=telegram_sending, Telegram, Requester,
                          State ).


-doc "Helper introduced only to make the decoding logic available for tests.".
-spec test_decode( telegram_chunk() ) -> decoding_outcome().
test_decode( Chunk ) ->
	try_integrate_next_telegram( _ToSkipLen=0, _MaybeTelTail=undefined, Chunk,
								 get_test_state() ).


-doc "Helper introduced only to make the decoding logic available for tests.".
-spec test_describe( telegram_chunk() ) -> ustring().
test_describe( Chunk ) ->

    { decoded, DevEvent, _MaybeDiscoverOrigin, _IsBackOnline, _MaybeDevice,
      _NextMaybeTelTail=undefined, _OceanicState } = test_decode( Chunk ),

    oceanic_text:device_event_to_string( DevEvent ).



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
		emitter_eurid=oceanic_text:string_to_eurid( ?default_emitter_eurid ),
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
try_integrate_next_telegram( ToSkipLen, _MaybeTelTail=undefined, NewChunk,
                             State ) ->

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
-spec decode_after_start_byte( telegram_tail(), oceanic_state() ) ->
                                            decoding_outcome().
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

Refer to `[ESP3]` "1.6 UART synchronization (start of packet detection)".
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
							% next chunk, but we need any corresponding tail,
							% so:
							%
							NextMaybeTelTail = get_maybe_next_tail( NextChunk ),

							{ unsupported, _SkipLen=0, NextMaybeTelTail,
                              State };

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
								<<FullData:FullLen/binary,
                                  _NextChunk/binary>> ->
									examine_full_data( FullData, FullDataCRC,
										Data, OptData, PacketType, NextChunk,
                                        State );

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
	telegram_opt_data(), packet_type(), telegram_chunk(), oceanic_state() ) ->
                                            decoding_outcome().
examine_full_data( FullData, ExpectedFullDataCRC, Data, OptData, PacketType,
				   NextChunk, State ) ->

	case compute_crc( FullData ) of

		ExpectedFullDataCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Full-data CRC validated (~B).",
										[ ExpectedFullDataCRC ] ) ),

			% First point where it can be done once for all:
			NextMaybeTelTail = get_maybe_next_tail( NextChunk ),

			oceanic_decode:decode_packet( PacketType, Data, OptData,
                                          NextMaybeTelTail, State );

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
			% after the last (chopped) start byte, i.e. from FullData (not even
			% from the later beginning of this tail (so we are nevertheless
			% still progressing - not wanting to recurse infinitely on a chunk):
			%
            try_decode_chunk( FullData, State )

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
Records that a telegram could be successfully decoded for the specified device,
registering it if it was not already.
""".
-spec record_device_success( eurid(), device_table() ) -> recording_info().
record_device_success( Eurid, DeviceTable ) ->

	case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device whose EURID "
				"is ~ts through listening.",
                [ oceanic_text:eurid_to_string( Eurid ) ] ),

			Now = time_utils:get_timestamp(),

			DiscoverOrigin = listening,

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 short_name=undefined,
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
              _MaybeDeviceShortName=undefined, _MaybeEEPId=undefined };


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
		short_name=MaybeDeviceShortName,
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
	  IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId }.



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
				"is ~ts through failure.",
                [ oceanic_text:eurid_to_string( Eurid ) ] ),

			DiscoverOrigin = listening,

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 short_name=undefined,
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
              _MaybeDeviceShortName=undefined, _MaybeEEPId=undefined };


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
		short_name=MaybeDeviceShortName,
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
	  IsBackOnline, MaybeDeviceName, MaybeDeviceShortName, MaybeEepId }.



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
		trace_bridge:debug_fmt( "Setting an automatic activity timer for "
            "device whose EURID is ~ts, for a duration of ~ts.",
			[ oceanic_text:eurid_to_string( Eurid ),
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
			[ oceanic_text:eurid_to_string( Eurid ),
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
	2;

% Useful at least to decode state feedback from Eltako:
get_app_style_from_eep( _EEPId=smart_plug ) ->
    % Preferred to 1 in order to consider that the A button (not the B one) is
    % operated:
    %
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
	{ oceanic_text:string_to_eurid( EuridStr ), Channel };

interpret_button_ref_spec( { DevShortName, Channel } )
		when is_atom( DevShortName )
			 andalso is_integer( Channel ) andalso Channel > 0 ->
	{ DevShortName, Channel };

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




-doc "Returns the broadcast EURID, suitable to target all devices in range.".
-spec get_broadcast_eurid() -> eurid().
get_broadcast_eurid() ->
	?eurid_broadcast.




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
Returns the emitting device name (if any) stored in the specified device event.
""".
-spec get_maybe_device_name( device_event() ) -> option( device_name() ).
get_maybe_device_name( DevEventTuple ) ->
	erlang:element( _PosIdx=3, DevEventTuple ).


-doc """
Returns any short name known for the emitting device stored in the specified
device event.
""".
-spec get_maybe_device_short_name( device_event() ) ->
                                        option( device_short_name() ).
get_maybe_device_short_name( DevEventTuple ) ->
	erlang:element( _PosIdx=4, DevEventTuple ).



-doc """
Returns the EEP (if any is defined and registered) stored in the specified
device event.
""".
-spec get_maybe_eep( device_event() ) -> option( eep_id() ).
get_maybe_eep( DevEventTuple ) ->
	erlang:element( _PosIdx=5, DevEventTuple ).


-doc """
Returns the timestamp corresponding to the specified device event.
""".
-spec get_timestamp( device_event() ) -> timestamp().
get_timestamp( DevEventTuple ) ->
	erlang:element( _PosIdx=6, DevEventTuple ).


-doc """
Returns the timestamp corresponding to any previously last seen telegram from
that device.

Also useful to determine whether an event corresponds to a device discovery.
""".
-spec get_last_seen_info( device_event() ) -> option( timestamp() ).
get_last_seen_info( DevEventTuple ) ->
	erlang:element( _PosIdx=7, DevEventTuple ).



-doc """
Returns the number (if any) of subtelegrams stored in the specified device
event.
""".
-spec get_subtelegram_count( device_event() ) -> option( subtelegram_count() ).
get_subtelegram_count( DevEventTuple ) ->
	erlang:element( _PosIdx=8, DevEventTuple ).



-doc """
Returns the EURID of the target of this transmission (addressed or broadcast),
if any, stored in the specified device event.
""".
-spec get_maybe_destination_eurid( device_event() ) -> option( eurid() ).
get_maybe_destination_eurid( DevEventTuple ) ->
	erlang:element( _PosIdx=9, DevEventTuple ).



-doc """
Returns the best RSSI value (if any) stored in the specified device event.
""".
-spec get_maybe_dbm( device_event() ) -> option( dbm() ).
get_maybe_dbm( DevEventTuple ) ->
	erlang:element( _PosIdx=10, DevEventTuple ).



-doc "Returns the stored in the specified device event.".
-spec get_maybe_security_level( device_event() ) -> option( security_level() ).
get_maybe_security_level( DevEventTuple ) ->
	erlang:element( _PosIdx=11, DevEventTuple ).



-doc """
Returns the designator pair stored in the specified device event.
""".
-spec get_designator_pair( device_event() ) -> designator_pair().
get_designator_pair( DevEventTuple ) ->
    { get_source_eurid( DevEventTuple ),
      get_maybe_device_short_name( DevEventTuple ) }.



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
Returns the best name found for the emitting device stored in the specified
device event.
""".
-spec get_best_device_name_from( device_event() ) -> device_name().
get_best_device_name_from( DevEventTuple ) ->
	case get_maybe_device_name( DevEventTuple ) of

		undefined ->
			SrcEurid = get_source_eurid( DevEventTuple ),
			oceanic_text:eurid_to_bin_string( SrcEurid );

		BinDeviceName ->
			BinDeviceName

	end.



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
