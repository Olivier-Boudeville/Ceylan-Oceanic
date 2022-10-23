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


% @doc Main module of Ceylan-Oceanic, in order to drive Enocean communications
% through an Oceanic server.
%
-module(oceanic).


% Base API:
-export([ get_default_tty_path/0, has_tty/0, has_tty/1,

		  start/0, start/1, start/2,
		  start_link/0, start_link/1, start_link/2,

		  get_server_registration_name/0, get_server_pid/0,
		  load_configuration/0, load_configuration/1,

		  send/2,

		  encode_double_rocker_switch_telegram/4, decode_telegram/2,

		  stop/0, stop/1,

		  eurid_to_string/1, eurid_to_bin_string/1, eurid_to_bin_string/2,
		  string_to_eurid/1, get_broadcast_eurid/0,

		  get_best_naming/2,

		  get_device_table/1,

		  string_to_eep/1] ).


% API for module generation:
-export([ generate_support_modules/0 ]).

% Exported only for testing:
-export([ get_test_state/0, get_test_state/1,
		  test_decode/2, secure_tty/1, try_integrate_chunk/4 ]).

% Silencing:
-export([ record_device_failure/2,
		  state_to_string/1, device_table_to_string/1, device_to_string/1,
		  device_event_to_string/1 ]).


-type oceanic_server_pid() :: pid().
% The PID of an Oceanic server.


-type serial_server_pid() :: pid().
% The PID of a process in charge of a serial connection to the Enocean gateway
% (USB dongle).

-type event_listener_pid() :: pid().
% The PID of any process registered to an Oceanic server as a listener of
% Enocean events.


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


-type device_config() ::

	{ UserDefinedName :: ustring(), EURID :: ustring(), EEP :: ustring() }

  | { UserDefinedName :: ustring(), EURID :: ustring(), EEP :: ustring(),
	  Comment :: ustring() }.
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
% A telegram is a raw (non-decoded), full, unitary radio (ERP-level) message
% (ESP3 packet) received from an Enocean gateway.
%
% E.g. `<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>',
% `<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>' or
% `<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>'.


-type telegram_chunk() :: binary().
% A telegram chunk is a partial telegram, possibly a full ESP3 packet.


-type telegram_data() :: telegram_chunk().
% The part of a telegram with the base, normalised, stable data, possibly
% complemented with optional data.


-type telegram_data_tail() :: telegram_chunk().
% A base, normalised, stable data of a telegram once its initial R-ORG byte has
% already been chopped.


-type telegram_opt_data() :: telegram_chunk().
% The part of a telegram with the optional data that may complement/extend the
% base data.
%
% See also optional_data/0.


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



% Since EEP 3.0:

-type rorg() :: uint8().
% Radio ORG (organization number); describes the ERP radio telegram type, as an
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


-type enum() :: integer().
% An enumeration specified in the protocol.


-type button_designator() ::
		'button_ai'  % Switch light on / Dim light down / Move blind closed
	  | 'button_ao'  % Switch light off / Dim light up / Move blind open
	  | 'button_bi'  % Switch light on / Dim light down / Move blind closed
	  | 'button_bo'. % Switch light off / Dim light up / Move blind open
% Designates a button corresponding to a A or B channel.
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

-type decoding_error() ::
	  'not_reached'   % End of packet still ahead
	| 'incomplete'    % Truncated packet
	| 'invalid'       % Corrupted packet
	| 'unsupported'   % Type of packet (currently) unsupported by Oceanic
	| 'unconfigured'. % Device not configure (typically EEP not known)
% The various kinds of errors that may happen when decoding.


-type decoding_outcome() ::

	{ decoding_error(), ToSkipLen :: count(), NextChunk :: telegram_chunk(),
	  oceanic_state() }

  | { 'decoded', device_event(), NextChunk :: telegram_chunk(),
	  oceanic_state() }.
% The outcome of an attempt of integrating / decoding a telegram chunk.


-type decoding_result() :: decoding_error() | device_event().
% The result of a decoding request.


-type eurid() :: type_utils:uint32(). % Previously <<_:32>>
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


-type packet() :: binary().
% An ESP-level data unit.


-type crc() :: byte().
% The CRC (Cyclic Redundancy Check) or polynomial code checksum of a
% sub-telegram/packet can be computed.


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
% The payload of a (typically ESP3) packet, a sequence f bytes sometimes
% designated as 'DataTail', that is all bytes in the "data" chunk (as opposed to
% the "optional data" one) found after the R-ORG one.


%-type decode_result() :: 'ok' | 'incomplete' | 'crc_mismatch'.


%-type read_outcome() ::
%    { ReadEvent :: device_event(), AnyNextChunk :: telegram_chunk() }.
% Outcome of a (blocking) request of telegram reading.
%
% Exactly one event will be read (any remainding chunk returned), possibly
% waiting for it indefinitely.
%
% No content can remain to be skipped here, as by design we ended when having
% read a new event, returning any next chunk as it is.



% Event types still ordered by increasing EEP:


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



-type device_event() :: thermo_hygro_event()
					  | single_input_contact_event()
					  | push_button_event()
					  | double_rocker_switch_event()
					  | double_rocker_multipress_event().
% Any event notified by an EnOcean device.


-export_type([ oceanic_server_pid/0, serial_server_pid/0, event_listener_pid/0,

			   device_name/0, device_plain_name/0, device_any_name/0,
			   device_designator/0,

			   enocean_device/0, device_table/0, device_config/0,
			   tty_detection_outcome/0, serial_protocol/0,

			   telegram/0, telegram_chunk/0,
			   telegram_data/0, telegram_data_tail/0,
			   telegram_opt_data/0, decoded_optional_data/0,

			   subtelegram_count/0, dbm/0, security_level/0,

			   rorg/0, func/0, type/0, eep/0, eep_id/0, eep_string/0,

			   button_designator/0, button_transition/0, button_counting/0,

			   contact_status/0,
			   ptm_switch_module_type/0, nu_message_type/0, repetition_count/0,
			   temperature_range/0,
			   decoding_outcome/0,

			   eurid/0,
			   packet/0, crc/0, esp3_packet/0, packet_type/0, payload/0,

			   thermo_hygro_event/0, single_input_contact_event/0,
			   push_button_event/0,
			   double_rocker_switch_event/0, double_rocker_multipress_event/0,

			   device_event/0 ]).




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

%-type content() :: binary().

-type esp3_header() :: <<_:32>>.
% 32-bit.


% Defines.

% The name under which the Oceanic server will register locally:
-define( oceanic_server_reg_name, oceanic_server ).


% Transmission speed, in bits per second:
-define( esp2_speed, 9600 ).

% Matters, otherwise faulty content received:
-define( esp3_speed, 57600 ).


% Default EURID of the pseudo-device emitter (if any) of any telegram to be sent
% by Oceanic:
%
% (never gets old hexadecimal pun)
%
-define( default_emitter_eurid, "DEADBEEF" ).


% Denotes a broadcast transmission (as opposed to an Addressed Transmission,
% ADT):
%
-define( eurid_broadcast, 16#ffffffff ). % That is 4294967295


% Each telegram must start with:
-define( sync_byte, 85 ).


% The next defines could not have been specified, knowing that they are now
% directly referenced thanks to const, bijective, topic-based tables exposed by
% a generated module (refer to the end of this file.


% For packet types, as defined in [ESP3]:
-define( reserved_type,           16#00 ).
-define( radio_erp1_type,         16#01 ).
-define( response_type,           16#02 ).
-define( radio_sub_tel_type,      16#03 ).
-define( event_type,              16#04 ).
-define( common_command_type,     16#05 ).
-define( smart_ack_command_type,  16#06 ).
-define( remote_man_command_type, 16#07 ).
% Not existing: 16#08.
-define( radio_message_type,      16#09 ).
-define( radio_erp2_type,         16#0A ).
-define( radio_802_15_4_type,     16#10 ).
-define( command_2_4_type,        16#11 ).


% For return codes, as defined in [ESP3]:
-define( ok_return,              16#00 ).
-define( error_return,           16#01 ).
-define( not_supported_return,   16#02 ).
-define( wrong_parameter_return, 16#03 ).
-define( operation_denied,       16#04 ).


% For event codes, as defined in [ESP3]:
% Not existing: 16#00
-define( sa_reclaim_failed,       16#01 ).
-define( sa_confirm_learn,        16#02 ).
-define( sa_learn_ack,            16#03 ).
-define( co_ready,                16#04 ).
-define( co_event_secure_devices, 16#05 ).


% For the RORG field of an ERP radio telegram type, as defined in [EEP]:
-define( rorg_undefined,  16#00 ).
-define( rorg_rps,        16#F6 ).
-define( rorg_1bs,        16#D5 ).
-define( rorg_4bs,        16#A5 ).
-define( rorg_vld,        16#D2 ).
-define( rorg_msc,        16#D1 ).
-define( rorg_adt,        16#A6 ).
-define( rorg_sm_lrn_req, 16#C6 ).
-define( rorg_sm_lrn_ans, 16#C7 ).
-define( rorg_rec,        16#A7 ).
-define( rorg_ex,         16#C5 ).
-define( rorg_sec,        16#30 ).
-define( rorg_sec_encaps, 16#31 ).
-define( rorg_man,        16#34 ).
-define( rorg_signal,     16#D0 ).
-define( rorg_ute,        16#D4 ).


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


% Protocol notes:

% ERP means *EnOcean Radio Protocol*; discusses telegrams.
% The EnOcean Radio Protocol (ERP) covers the first three layers of the OSI
% model: Physical, Data Link and Network.
%
% By default, 8 bits of data, no parity, 1 stop bit.
% Optional repeater mode.
% Some modules support multiple channels.

% ESP means *EnOcean Serial Protocol*; discusses packets.

% EEP means *EnOcean Equipment Profiles*; discusses devices.


% Below, regarding documentation:
%
% - "[EEP-gen]" refers to the "EnOcean Equipment Profiles" (see
% EnOcean-Equipment-Profiles-3-1-1.pdf, available from
% https://www.enocean-alliance.org/eep/)
%
% - "[EEP-spec]" refers to the "EEP Specification" (see
% EnOcean_Equipment_Profiles_EEP_v2.6.7_public.pdf, available from
% https://www.enocean-alliance.org/wp-content/uploads/2017/05/)
%
% - "[ESP3]" refers to the "Enocean Serial Protocol (ESP3) Specification" (see
% EnOceanSerialProtocol3.pdf, available from https://www.enocean.com/esp,
% typically resolving in
% https://www.enocean.com/wp-content/uploads/Knowledge-Base/EnOceanSerialProtocol3.pdf)
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
% device/Sender ID of interest what is its corresponding EEP
%
% - for 1BS: a bit tells whether the telegram at hand is a teach-in one
%
% - for 4BS: 3 variations exist, some allowing to exchange EEPs
%
% See [EEP-gen] starting from p.18 for more details.



% Definition of the overall state of an Oceanic server.
-record( oceanic_state, {

	% The PID of the process in charge of the serial connection to the Enocean
	% gateway (USB dongle):
	%
	serial_server_pid :: serial_server_pid(),

	% To identify the pseudo-device emitter of any telegram to be sent by
	% Oceanic:
	%
	emitter_eurid :: eurid(),

	% A table recording all information regarding the known Enocean devices:
	device_table :: device_table(),

	% The number of telegrams count:
	send_count = 0 :: count(),

	% The PID of any process listening for Enocean events:
	event_listener_pid :: maybe( event_listener_pid() ) } ).


-type oceanic_state() :: #oceanic_state{}.
% An Oceanic state, including configuration typically loaded from an ETF file.



% Shorthands:

-type count() :: basic_utils:count().

-type any_file_path() :: file_utils:any_file_path().
-type device_path() :: file_utils:device_path().
-type entry_type() :: file_utils:entry_type().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type uint8() :: type_utils:uint8().

-type timestamp() :: time_utils:timestamp().

-type percent() :: math_utils:percent().

-type registration_name() :: naming_utils:registration_name().

-type celsius() :: unit_utils:celsius().


%-type time_out() :: time_utils:time_out().

-type topic_spec() :: const_bijective_topics:topic_spec().

%-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).


% For myriad_spawn*:
-include_lib("myriad/include/spawn_utils.hrl").



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
	% Better than, say, "/dev/ttyUSB0":
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

	case has_tty( TtyPath ) of

		true ->
			ok;

		{ false, non_existing } ->
			trace_bridge:error_fmt( "The specified TTY, '~ts', "
				"does not exist. Is the device for the Enocean gateway "
				"plugged in, and named accordingly?", [ TtyPath ] ),
			throw( { non_existing_tty, TtyPath } );


		{ false, { not_device, OtherType } } ->
			trace_bridge:error_fmt( "The specified TTY for the "
				"Enocean gateway, '~ts', is not a device but a ~ts.",
				[ TtyPath, OtherType ] ),

			throw( { not_a_device, TtyPath, OtherType } )

	end,

	% Symmetrical speed here (in bits per second):
	Speed = ?esp3_speed,

	% No parity (no {parity_even} / {parity_odd}).

	% Linked process;
	SerialPid = serial:start( [ { open, TtyPath },
								{ speed, _In=Speed, _Out=Speed } ] ),

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Using TTY '~ts' to connect to Enocean gateway,"
			" corresponding to serial server ~w.", [ TtyPath, SerialPid ] ) ),

	SerialPid.



% Oceanic server process subsection.

-spec oceanic_start( device_path(), maybe( event_listener_pid() )  ) ->
											no_return().
oceanic_start( TtyPath, MaybeEventListenerPid ) ->

	SerialPid = secure_tty( TtyPath ),

	LoadedState = load_configuration(),

	InitialState = LoadedState#oceanic_state{
		serial_server_pid=SerialPid,
		event_listener_pid=MaybeEventListenerPid },

	naming_utils:register_as( ?oceanic_server_reg_name, _RegScope=local_only ),

	oceanic_loop( _SkipLen=0, _AccChunk= <<>>, InitialState ).



% @doc Loads Oceanic configuration information from the default Ceylan
% preferences file, if any, otherwise returns an empty device table.
%
% See the 'preferences' module.
%
-spec load_configuration() -> oceanic_state().
load_configuration() ->
	case preferences:is_preferences_default_file_available() of

		{ true, PrefPath } ->

			LoadedState = load_configuration( PrefPath ),

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Initial state: ~ts",
					[ state_to_string( LoadedState ) ] ) ),

			LoadedState;

		{ false, PrefPath } ->

			trace_bridge:info_fmt( "No preferences file ('~ts') found.",
								   [ PrefPath ] ),

			#oceanic_state{ emitter_eurid=?default_emitter_eurid,
							device_table=table:new() }

	end.



% @doc Loads Oceanic configuration information from the specified ETF file.
%
% It is expected to contain up to one of the following entries whose (atom) key
% is:
%
% - oceanic_emitter: to specify the pseudo-device emitting any telegram to be
% sent by Oceanic

% - oceanic_config: to declare the known devices
%
-spec load_configuration( any_file_path() ) -> oceanic_state().
load_configuration( ConfFilePath ) ->

	file_utils:is_existing_file_or_link( ConfFilePath )
		orelse throw( { oceanic_config_file_not_found, ConfFilePath } ),

	Pairs = file_utils:read_etf_file( ConfFilePath ),

	EmitterEuridStr = list_table:get_value_with_default( _K=oceanic_emitter,
		_DefEmit=?default_emitter_eurid, _Table=Pairs ),

	EmitterEurid = text_utils:hexastring_to_integer( EmitterEuridStr ),

	DeviceEntries = list_table:get_value_with_default( oceanic_devices,
													   _DefDevs=[], Pairs ),

	% Device table indexed by device eurid(), which is duplicated in the record
	% values for convenience:
	%
	LoadedDeviceTable = declare_devices( DeviceEntries, table:new() ),

	#oceanic_state{ emitter_eurid=EmitterEurid,
					device_table=LoadedDeviceTable }.



% @doc Adds the specified devices in the specified device table.
-spec declare_devices( [ device_config() ], device_table() ) -> device_table().
declare_devices( _DeviceCfgs=[], DeviceTable ) ->
	DeviceTable;

declare_devices( _DeviceCfgs=[ { NameStr, EuridStr, EepStr } | T ],
				 DeviceTable ) ->

	Eurid = text_utils:hexastring_to_integer(
		text_utils:ensure_string( EuridStr ), _ExpectPrefix=false ),

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

	DeviceRec = #enocean_device{
		eurid=Eurid,
		name=text_utils:ensure_binary( NameStr ),
		eep=EepId },

	NewDeviceTable = table:add_new_entry( Eurid, DeviceRec, DeviceTable ),

	declare_devices( T, NewDeviceTable );

% Dropping comment (useful only for the user configuration):
declare_devices( _DeviceCfgs=[ { NameStr, EuridStr, EepStr, CommentStr } | T ],
				 DeviceTable ) when is_list( CommentStr ) ->
	declare_devices( [ { NameStr, EuridStr, EepStr } | T ], DeviceTable );


declare_devices( _DeviceCfgs=[ Other | _T ], _DeviceTable ) ->
	throw( { invalid_device_config, Other } ).



% @doc Sends the specified telegram, through the specified Oceanic server.
-spec send( telegram(), oceanic_server_pid() ) -> void().
send( Telegram, OcSrvPid ) ->
	OcSrvPid ! { send_oceanic, Telegram }.



% @doc Encodes a double-rocker switch telegram, from the specified device to the
% specified one, reporting the specified transition for the specified button.
%
% Event sent in the context of EEP F6-02-01 ("Light and Blind Control -
% Application Style 1"), for T21=1.
%
% See [EEP-spec] p.15 and its decode_rps_double_rocker_packet/7 counterpart.
%
-spec encode_double_rocker_switch_telegram( eurid(), eurid(),
		button_designator(), button_transition() ) -> telegram().
encode_double_rocker_switch_telegram( SourceEurid, TargetEurid,
									  ButtonDesignator, ButtonTransition ) ->

	% No EEP to be determined from double_rocker_switch (implicit in packet).

	% Best understood backwards, from the end of this function.

	% Radio, hence 1 here:
	PacketTypeNum = oceanic_generated:get_second_for_packet_type(
								_PacketType=radio_erp1_type ),
	PacketTypeNum = 1,

	% F6 here:
	RorgNum = oceanic_generated:get_second_for_rorg( _Rorg=rorg_rps ),
	RorgNum = 16#f6,

	R1Enum = get_designated_button_enum( ButtonDesignator ),

	EB = get_button_transition_enum( ButtonTransition ),

	IsSecondActionValid = false,

	% Not relevant here:
	R2Enum = R1Enum,

	SA = case IsSecondActionValid of

		true ->
			1;

		false ->
			0

	end,

	DB_0 = <<R1Enum:3, EB:1, R2Enum:3, SA:1>>,

	T21 = 1,
	NU = 1,

	Status = <<0:2, T21:1, NU:1, 0:4>>,

	Data = <<RorgNum:8, DB_0:1/binary, SourceEurid:32, Status:1/binary>>,

	% We are sending here:
	SubTelNum = 3,
	DBm = 16#ff,
	SecurityLevel = 0,

	OptData = <<SubTelNum:8, TargetEurid:32, DBm:8, SecurityLevel:8>>,

	FullData = <<Data/binary, OptData/binary>>,
	FullDataCRC = compute_crc( FullData ),

	DataLen = size( Data ),
	OptDataLen = size( OptData ),

	Header = <<DataLen:16, OptDataLen:8, PacketTypeNum:8>>,

	HeaderCRC = compute_crc( Header ),

	cond_utils:if_defined( oceanic_debug_decoding,
		begin
			<<DB_0AsInt>> = DB_0,
			<<StatusAsInt>> = Status,
			PadWidth = 8,
			trace_bridge:debug_fmt(
				"Generated packet: type=~B, RORG=~ts, DB_0=~ts, "
				"data size=~B, optional data size=~B, status=~ts.",
				[ PacketTypeNum, text_utils:integer_to_hexastring( RorgNum ),
				  text_utils:integer_to_bits( DB_0AsInt, PadWidth ),
				  DataLen, OptDataLen,
				  text_utils:integer_to_bits( StatusAsInt, PadWidth ) ] )
		end ),

	% This is an ESP3 packet:
	<<?sync_byte, Header/binary, HeaderCRC:8, FullData/binary, FullDataCRC:8>>.



% @doc Returns, if possible, the specified telegram once decoded as an event,
% using the specified Oceanic server for that.
%
% Mostly useful for testing purpose.
%
-spec decode_telegram( telegram(), oceanic_server_pid() ) -> decoding_result().
decode_telegram( Telegram, OcSrvPid ) ->
	OcSrvPid ! { decode_oceanic, Telegram, self() },
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
% account (hence AccChunk - which never includes the starting byte)
%
-spec oceanic_loop( count(), telegram_chunk(), oceanic_state() ) -> no_return().
oceanic_loop( ToSkipLen, AccChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Waiting for any message "
			"including a telegram chunk, whereas having "
			"~B bytes to skip, and having accumulated ~w.",
			[ ToSkipLen, AccChunk ] ) ),

	receive

		% Receives data from the serial port:
		{ data, NewChunk } ->
			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Received a telegram chunk "
					"of ~B bytes: ~w (whereas there are ~B bytes to skip).",
					[ size( NewChunk), NewChunk, ToSkipLen ] ) ),

			case try_integrate_chunk( ToSkipLen, AccChunk, NewChunk, State ) of

				{ decoded, Event, AnyNextChunk, NewState } ->

					trace_bridge:debug_fmt( "Decoded following event: ~ts.",
						[ device_event_to_string( Event ) ] ),

					case NewState#oceanic_state.event_listener_pid of

						undefined ->
							ok;

						ListenerPid ->
							ListenerPid ! { onEnoceanEvent, [ Event, self() ] }

					end,
					oceanic_loop( _SkipLen=0, AnyNextChunk, NewState );


				{ _Unsuccessful, NewToSkipLen, NewAccChunk, NewState } ->
					% when Unsuccessful     =:= not_reached
					%   orelse Unsuccessful =:= incomplete
					%   orelse Unsuccessful =:= invalid
					%   orelse Unsuccessful =:= unsupported ->
					%   orelse Unsuccessful =:= unconfigured ->
					oceanic_loop( NewToSkipLen, NewAccChunk, NewState )

			end;


		{ send_oceanic, Telegram } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Sending to serial server telegram ~w.",
										[ Telegram ] ) ),

			SerialPid = State#oceanic_state.serial_server_pid,
			SerialPid ! { send, Telegram },

			NewSendCount = State#oceanic_state.send_count + 1,

			NewState = State#oceanic_state{ send_count=NewSendCount },

			oceanic_loop( ToSkipLen, AccChunk, NewState );


		% Mostly useful for testing purpose:
		{ decode_oceanic, Telegram, SenderPid } ->

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Requested to decode telegram ~w.",
										[ Telegram ] ) ),

			% Not interfering with received bits (curret state used for that,
			% but will not be affected):
			%
			Res = case try_integrate_chunk( _ToSkipLen=0, _AccChunk= <<>>,
											Telegram, State ) of

				{ decoded, DeviceEvent, _NewNextChunk, _NewState } ->
					DeviceEvent;

				{ DecError, _NewToSkipLen, _NewNextChunk, _NewState } ->
					 DecError

			end,

			SenderPid ! { decoding_result, Res },

			% Strictly unaffected:
			oceanic_loop( ToSkipLen, AccChunk, State );


		terminate ->

			SerialPid = State#oceanic_state.serial_server_pid,

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Stopping serial server ~w, "
					"while in following state: ~ts",
					[ SerialPid, state_to_string( State ) ] ) ),

			SerialPid ! stop,

			trace_bridge:debug_fmt( "Oceanic server ~w terminated.",
									[ self() ] )

	end.



% @doc Helper introduced only to make the decoding logic available for tests.
-spec test_decode( telegram_chunk(), device_table() ) -> decoding_outcome().
test_decode( Chunk, DeviceTable ) ->
	try_integrate_chunk( _ToSkipLen=0, _AccChunk= <<>>, Chunk,
						 get_test_state( DeviceTable ) ).



% @doc Returns a pseudo-state; only useful for some tests.
-spec get_test_state() -> oceanic_state().
get_test_state() ->

	LoadState = load_configuration(),

	LoadState#oceanic_state{ serial_server_pid=self(), % for correct typing
							 event_listener_pid=undefined }.



% @doc Returns a pseudo-state; only useful for some tests.
-spec get_test_state( device_table() ) -> oceanic_state().
get_test_state( DeviceTable ) ->
	% Normally there is a real serial server:
	#oceanic_state{ serial_server_pid=self(), % for correct typing
					device_table=DeviceTable,
					event_listener_pid=undefined }.



% @doc Returns the device state in the specified state; only useful for some
% tests.
%
-spec get_device_table( oceanic_state() ) -> device_table().
get_device_table( #oceanic_state{ device_table=DeviceTable } ) ->
	DeviceTable.



% @doc Tries to integrate a new telegram chunk.
-spec try_integrate_chunk( count(), telegram_chunk(), telegram_chunk(),
						   oceanic_state() ) -> decoding_outcome().
% Special-casing "no skip" is clearer; guard needed to ensure we indeed already
% chopped a start byte:
%
try_integrate_chunk( _ToSkipLen=0, AccChunk, NewChunk, State )
									when AccChunk =/= <<>> ->

	% May happen (at least initially):
	%cond_utils:assert( oceanic_check_decoding, AccChunk =/= <<>> ),

	% Start byte was already chopped from AccChunk:
	scan_past_start( <<AccChunk/binary, NewChunk/binary>>, State );


try_integrate_chunk( ToSkipLen, AccChunk, NewChunk, State ) ->

	cond_utils:assert( oceanic_check_decoding, AccChunk =:= <<>> ),

	ChunkSize = size( NewChunk ),

	case ToSkipLen - ChunkSize of

		% Not having reached a new packet yet:
		StillToSkip when StillToSkip >= 0 ->
			{ not_reached, StillToSkip, _EmptyAcc= <<>>, State };

		% ChunkSize > ToSkipLen, so next packet already started in this
		% new chunk:
		%
		_ ->
			<<_Skipped:ToSkipLen/binary, TargetChunk/binary>> = NewChunk,
			try_decode_chunk( TargetChunk, State )

	end.



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
					"so dropping its ~B bytes)", [ DroppedCount ] ) ),
			{ invalid, _StillToSkipLen=0, <<>>, State };


		{ NewTelegramChunk, DroppedCount } ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Start byte found, retaining now chunk "
					"'~p' (of size ~B bytes; after dropping ~B byte(s)).",
					[ NewTelegramChunk, size( NewTelegramChunk ),
					  DroppedCount ] ) ),

			scan_past_start( NewTelegramChunk, State )

	end.



% @doc Scans the specified chunk, knowing that it used to begin with a start
% byte (which has already been chopped).
%
scan_past_start( NewTelegramChunk, State ) ->

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Examining now chunk '~p' (of size ~B bytes).",
			[ NewTelegramChunk, size( NewTelegramChunk ) ] ) ),

	% This new chunk corresponds to a telegram that is invalid, or unsupported,
	% or (currently) truncated, or valid (hence decoded):
	%
	case NewTelegramChunk of

		% First 32 bits:
		<<Header:4/binary, HeaderCRC, Rest/binary>> ->
			examine_header( Header, HeaderCRC, Rest, NewTelegramChunk, State );

			% So less than 5 bytes (yet), cannot be complete:
			_ ->
				cond_utils:if_defined( oceanic_debug_decoding,
					trace_bridge:debug( "(no complete header to decode)" ) ),

				% Waiting to concatenate any additional receiving:
			{ incomplete, _ToSkipLen=0, NewTelegramChunk, State }

	end.




% @doc Extracts the content from the specified telegram chunk, returning a chunk
% that is beginning just after any start byte (which is 0x55, i.e. 85).
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
-spec examine_header( esp3_header(), crc(), telegram_chunk(),
					  telegram_chunk(), oceanic_state() ) -> decoding_outcome().
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
							{ not_reached, StillToSkip, _AccChunk= <<>>,
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
							  FullDataCRC:8, AnyNextChunk/binary>> = Rest,

							% This CRC corresponds to the whole FullData, we
							% extract it (again) rather than concatenating
							% <<Data/binary, OptData/binary>>:

							FullLen = DataLen + OptDataLen,

							<<FullData:FullLen/binary, _>> = Rest,

							examine_full_data( FullData, FullDataCRC, Data,
								OptData, PacketType, FullTelegramChunk,
								AnyNextChunk, State )

					end

			end;

		OtherHeaderCRC ->

			cond_utils:if_defined( oceanic_debug_decoding,
				trace_bridge:debug_fmt( "Obtained other header CRC (~B), "
					"dropping this telegram candidate.", [ OtherHeaderCRC ] ) ),

			% Rather than discarding this chunk as a whole, tries to scavange
			% (very conservatively) any trailing element by reintroducing a
			% potentially valid new chunk - knowing that the past start byte has
			% already been chopped:
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
					[ OtherCRC, ExpectedFullDataCRC ] ) ),

			% Not expecting being fooled by data accidentally looking like a
			% legit CRC'ed header, so supposing this is just a valid telegram
			% that ended up to be corrupted, yet for extra safety we will
			% restart the decoding at the very first (yet still progressing -
			% not wanting to recurse infinitely) step:
			%
			scan_for_packet_start( FullTelegramChunk )

	end.



% @doc Decodes the specified packet, based on the specified data elements.
-spec decode_packet( packet_type(), telegram_data(), telegram_opt_data(),
					 telegram_chunk(), oceanic_state() ) -> decoding_outcome().
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

		rorg_4bs->
			decode_4bs_packet( DataTail, OptData, AnyNextChunk, State );

		_ ->
			trace_bridge:warning_fmt( "The decoding of ERP1 radio packets "
				"of R-ORG ~ts, hence ~ts (i.e. '~ts') is not implemented; "
				"the corresponding packet is thus dropped.",
				[ text_utils:integer_to_hexastring( RorgNum ), Rorg,
				  oceanic_generated:get_second_for_rorg_description( Rorg ) ] ),

			% Not even an EURID to track.

			{ unsupported, _ToSkipLen=0, AnyNextChunk, State }

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
			"message type is ~ts, ~ts~ts.",
			[ DB_0, get_best_naming( MaybeDeviceName, SenderEurid ),
			  ptm_module_to_string( PTMSwitchModuleType ),
			  nu_message_type_to_string( NuType ),
			  repeater_count_to_string( RepCount ),
			  maybe_optional_data_to_string( MaybeDecodedOptData, OptData )
			] ) ),

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
decode_rps_double_rocker_packet( DB_0= <<DB_0AsInt:8>>, SenderEurid,
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
			cond_utils:assert( oceanic_check_decoding,
							   DB_0AsInt band 2#00001111 =:= 0 ),

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



% @doc Decodes a rorg_1bs (D5) packet.
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
			"contact is ~ts), sender is ~ts, status is ~w, ~ts.",
			[ size( DataTail ), DB_0, learn_to_string( LearnActivated ),
			  ContactStatus, get_best_naming( MaybeDeviceName, SenderEurid ),
			  Status,
			  maybe_optional_data_to_string( MaybeDecodedOptData,
											 OptData ) ] ) ),

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



% @doc Decodes a rorg_4bs (A5) packet.
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
			] ) ),

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



% @doc Decodes the specified optional data.
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

decode_optional_data( Other ) ->
	trace_bridge:warning_fmt( "Unable to decode following optional data "
		"(of size ~B bytes): ~p.", [ size( Other ), Other ] ),
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



% @doc Helper to resolve correctly elements of optional data.
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
			trace_bridge:info_fmt( "Discovering Enocean device ~ts.",
								   [ eurid_to_string( Eurid ) ] ),

			Now = time_utils:get_timestamp(),

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 eep=undefined,
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
			trace_bridge:info_fmt( "Discovering Enocean device ~ts.",
								   [ eurid_to_string( Eurid ) ] ),

			NewDevice = #enocean_device{ eurid=Eurid,
										 name=undefined,
										 eep=undefined,
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



% @doc Returns a raw, (plain) textual description of the specified EURID.
-spec eurid_to_string( eurid() ) -> ustring().
eurid_to_string( _Eurid=?eurid_broadcast ) ->
	"the address for broadcast transmission";

eurid_to_string( Eurid ) ->

	% We want to return for example a correct "002ee196", not an
	% ambiguously-shortened "2ee196":
	%
	HexaStr = text_utils:integer_to_hexastring( Eurid ),

	% 32-bit:
	PaddedStr = text_utils:pad_string_right( HexaStr, _Width=8, $0 ),

	text_utils:flatten( PaddedStr ).


% @doc Returns the actual EURID corresponding to the specified (plain) EURID
% string.
%
% Ex: 3072406 = oceanic:string_to_eurid("002ee196")
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

	text_utils:format( "with ~ts, targeted to ~ts~ts~ts",
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
		"a ~ts ~ts~ts, declared ~ts; ~ts",
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

	text_utils:format( "single-contact device ~ts has been ~ts at ~ts~ts, "
		"declared ~ts; ~ts",
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

	SecondStr = case IsValid of

		true ->
			text_utils:format( " and its ~ts",
				[ button_designator_to_string( SecondButtonDesignator ) ] );

		false ->
			""

	end,

	text_utils:format( "double-rocker device ~ts has its ~ts~ts ~ts at ~ts, "
		"declared ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  button_designator_to_string( FirstButtonDesignator ), SecondStr,
		  get_button_transition_description( ButtonTransition ),
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
		"at ~ts, declared ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ), TransStr,
		  time_utils:timestamp_to_string( Timestamp ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),
		  get_eep_description( MaybeEepId, _DefaultDesc="F6-02-01" ) ] );


device_event_to_string( OtherEvent ) ->
	text_utils:format( "unknown event: ~p", [ OtherEvent ] ).




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
		send_count=SendCount,
		event_listener_pid=MaybeListenerPid } ) ->

	ListenStr = case MaybeListenerPid of

		undefined ->
			"not having a listener of Enocean events registered";

		ListenerPid ->
			text_utils:format( "having ~w registered as listener "
							   "of Enocean events", [ ListenerPid ] )

	end,

	SendStr = case SendCount of

		0 ->
			"not having sent any telegram";

		1 ->
			"having sent a single telegram";

		_ ->
			text_utils:format( "having sent ~B telegrams", [ SendCount ] )

	end,

	text_utils:format( "Oceanic server using serial server ~w, "
		"using emitter EURID ~ts, ~ts, ~ts, and knowing ~ts",
		[ SerialServerPid, eurid_to_string( EmitterEurid ), ListenStr,
		  SendStr, device_table_to_string( DeviceTable ) ] ).



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

	{ SeenStr, TeleStr, ErrStr } = case MaybeFirstTimestamp of

		undefined ->
			{ "never seen by this server", "", "" };

		FirstTimestamp ->
			SeenCountStr = case MaybeLastTimestamp of

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

			TeleCountStr = case TeleCount of

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
							" and no telegram decoding failed"
					end;

				1 ->
					" and a single telegram decoding failed";

				_ ->
					text_utils:format( " and ~B decodings failed",
									   [ ErrCount ] )

			end,

			{ SeenCountStr, TeleCountStr, ErrCountStr }

		end,

		text_utils:format( "~ts applying ~ts; it has been ~ts~ts~ts",
						   [ NameStr, EepDescStr, SeenStr, TeleStr, ErrStr ] ).



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




% Section for the build-time generation of support modules.


% @doc To be called by the 'oceanic_generated.beam' automatic make target in
% order to generate, here, a (single) module to share the Oceanic constants.
%
-spec generate_support_modules() -> no_return().
generate_support_modules() ->

	TargetModName = oceanic_generated,

	%trace_bridge:info_fmt( "Generating module '~ts'...", [ TargetModName ] ),

	TopicSpecs = [ get_packet_type_topic_spec(), get_return_code_topic_spec(),
		get_event_code_topic_spec(), get_rorg_topic_spec(),
		get_rorg_description_topic_spec() ] ++ get_eep_topic_specs(),

	_ModFilename =
		const_bijective_topics:generate_in_file( TargetModName, TopicSpecs ),

	%trace_bridge:info_fmt( "File '~ts' generated.", [ ModFilename ] ),

	erlang:halt().



% Defines are used below as they were already specified, otherwise of course
% they would not.


% @doc Returns the specification for the the 'packet_type' topic.
-spec get_packet_type_topic_spec() -> topic_spec().
get_packet_type_topic_spec() ->

	% We use our recommended order (first set for internal, second one for
	% third-party).

	% For packet types, as defined in [ESP3]:
	Entries = [
		{ reserved_type,           ?reserved_type },
		{ radio_erp1_type,         ?radio_erp1_type },
		{ response_type,           ?response_type },
		{ radio_sub_tel_type,      ?radio_sub_tel_type },
		{ event_type,              ?event_type },
		{ common_command_type,     ?common_command_type },
		{ smart_ack_command_type,  ?smart_ack_command_type },
		{ remote_man_command_type, ?remote_man_command_type },
		{ radio_message_type,      ?radio_message_type },
		{ radio_erp2_type,         ?radio_erp2_type },
		{ radio_802_15_4_type,     ?radio_802_15_4_type},
		{ command_2_4_type,        ?command_2_4_type } ],

	{ packet_type, Entries }.



% @doc Returns the specification for the the 'return_code' topic.
-spec get_return_code_topic_spec() -> topic_spec().
get_return_code_topic_spec() ->

	Entries = [
		{ ok_return,              ?ok_return  },
		{ error_return,           ?error_return },
		{ not_supported_return,   ?not_supported_return },
		{ wrong_parameter_return, ?wrong_parameter_return },
		{ operation_denied,       ?operation_denied  } ],

	{ return_code, Entries }.



% @doc Returns the specification for the the 'event_code' topic.
-spec get_event_code_topic_spec() -> topic_spec().
get_event_code_topic_spec() ->

	Entries = [
		{ sa_reclaim_failed,       ?sa_reclaim_failed  },
		{ sa_confirm_learn,        ?sa_confirm_learn },
		{ sa_learn_ack,            ?sa_learn_ack },
		{ co_ready,                ?co_ready },
		{ co_event_secure_devices, ?co_event_secure_devices } ],

	{ event_code, Entries }.



% @doc Returns the specification for the the 'rorg' topic.
-spec get_rorg_topic_spec() -> topic_spec().
get_rorg_topic_spec() ->

	Entries = [
		{ rorg_undefined,  ?rorg_undefined },
		{ rorg_rps,        ?rorg_rps },
		{ rorg_1bs,        ?rorg_1bs },
		{ rorg_4bs,        ?rorg_4bs },
		{ rorg_vld,        ?rorg_vld },
		{ rorg_msc,        ?rorg_msc },
		{ rorg_adt,        ?rorg_adt },
		{ rorg_sm_lrn_req, ?rorg_sm_lrn_req },
		{ rorg_sm_lrn_ans, ?rorg_sm_lrn_ans },
		{ rorg_rec,        ?rorg_rec },
		{ rorg_ex,         ?rorg_ex },
		{ rorg_sec,        ?rorg_sec },
		{ rorg_sec_encaps, ?rorg_sec_encaps },
		{ rorg_ute,        ?rorg_ute } ],

	{ rorg, Entries }.



% @doc Returns the specification for the 'rorg_description' topic.
-spec get_rorg_description_topic_spec() -> topic_spec().
get_rorg_description_topic_spec() ->

	Entries = [
		{ rorg_undefined,  <<"(undefined RORG)">> },
		{ rorg_rps,        <<"RPS (Repeated Switch Communication)">> },
		{ rorg_1bs,        <<"1BS (1-byte Communication)">> },
		{ rorg_4bs,        <<"4BS (4-byte Communication)">> },
		{ rorg_vld,        <<"VLD 'Variable Length Data)">> },
		{ rorg_msc,        <<"MSC (Manufacturer-Specific Communication)">> },
		{ rorg_adt,        <<"ADT (Addressing Destination Telegram)">> },
		{ rorg_sm_lrn_req, <<"SM_LRN_REQ (SMART ACK Learn Request)">> },
		{ rorg_sm_lrn_ans, <<"SM_LRN_ANS (SMART ACK Learn Answer)">> },
		{ rorg_rec,        <<"SM_REC (SMART ACK Reclaim)">> },
		{ rorg_ex,         <<"SYS_EX (Remote Management)">> },
		{ rorg_sec,        <<"SEC (Secure telegram)">> },
		{ rorg_sec_encaps,
			<<"SEC_ENCAPS (Secure telegram with RORG encapsulation)">> },
		{ rorg_man,        <<"SEC_MAN (Maintenance Security message)">> },
		{ rorg_signal,     <<"SIGNAL (Signal telegram)">> },
		{ rorg_ute,        <<"UTE (Universal Teach In)">> } ],

	{ rorg_description, Entries }.



% @doc Returns the specification for the 'eep' topics.
-spec get_eep_topic_specs() -> [ topic_spec() ].
get_eep_topic_specs() ->

	% We want to be able to associate one of our EEP identifiers
	% (e.g. 'single_input_contact') to either an internal triplet (e.g. {16#D5,
	% 16#00, 16#01}) or its counterpart string (e.g. "D5-00-01").

	% {eep_id, eep_string()} pairs:
	RawEntries = [

		% Temperature and humidity sensors:

		% Lower-range, 0°C to +40°C and 0% to 100%:
		{ thermo_hygro_low, "A5-04-01" },

		% Mid-range, -20°C to +60°C and 0% to 100%
		{ thermo_hygro_mid, "A5-04-02" },

		% Higher-range, -20°C to +60°C 10bit-measurement and 0% to 100%:
		{ thermo_hygro_high, "A5-04-03" },


		% Buttons:

		% Single button:
		{ push_button, "F6-01-01" },

		% Two rockers:
		{ double_rocker_switch, "F6-02-01" },

		% Contacts:
		{ single_input_contact, "D5-00-01" },

		% In-wall modules:
		{ single_channel_module, "D2-01-0E" },
		{ double_channel_module, "D2-01-12" } ],

	AsTripletsEntries = [ { EepId, string_to_eep( EepStr ) }
								|| { EepId, EepStr } <- RawEntries ],

	AsStringsEntries = [ { EepId, text_utils:string_to_binary( EepStr ) }
								|| { EepId, EepStr } <- RawEntries ],

	% As a user-specified EEP might not be found:
	ElementLookup = 'maybe',

	[ { eep_triplets, AsTripletsEntries, ElementLookup },
	  { eep_strings, AsStringsEntries, ElementLookup } ].
