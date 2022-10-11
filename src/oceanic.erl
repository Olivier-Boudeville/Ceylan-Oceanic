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

		  stop/0, stop/1,

		  eurid_to_bin_string/2,
		  string_to_eep/1] ).


% API for module generation:
-export([ generate_support_modules/0 ]).

% Exported only for testing:
-export([ get_test_state/0, test_decode/1, secure_tty/1,
		  try_integrate_chunk/4 ]).

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


% For the device-related records:
-include("oceanic.hrl").


-type enocean_device() :: #enocean_device{}.
% Information regarding an Enocean device, as known by the Oceanic server.


-type device_table() :: table( eurid(), enocean_device() ).
% A table recording information regarding Enocean devices.


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


-type telegram_chunk() :: binary().
% A telegram chunk is a partial telegram, possibly a full ESP3 packet.

-type telegram() :: binary().
% A telegram is a raw (non-decoded), full, unitary radio (ERP-level) message
% (ESP3 packet) received from an Enocean gateway.
%
% E.g. `<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>',
% `<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>' or
% `<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>'.


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


-type eep_id() :: 'single_input_contact'
				| atom().
% The (atom) identifier of an EnOcean Equipment Profile, corresponding to
% (R-ORG)-(FUNC)-(TYPE) triplet.
%
% For example the 'single_input_contact' EEP identifier corresponds to EEP
% D5-00-01.



-type contact_status() :: 'open' | 'closed'.
% Tells whether a contact is open or closed.


-type ptm_switch_module_type() :: 1  % synonymous for module PTM1xx
								| 2. % synonymous for module PTM2xx
% The types of PTM switch modules.


-type eurid() :: type_utils:uint32(). % Previously <<_:32>>
% EURID (EnOcean Unique Radio Identifier) is a unique and non-changeable
% identification number (as a 32-bit value) assigned to every EnOcean
% transmitter during its production process.
%
% The EURID corresponds to the hexadecimal identifier typically labelled at the
% back of devices (e.g. "ID: B50533EC").


-type packet() :: binary().
% An ESP-level data unit.


-type crc() :: byte().
% The CRC (Cyclic Redundancy Check) or polynomial code checksum of a
% sub-telegram/packet can be computed.


-type esp3_packet() :: binary().
% An ESP3 data unit, consisting of Header, Data and Optional Data.
%
% The semantics of the optional data is defined by the packet type.


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


%-type payload() :: binary().
% The payload of a (typically ESP3) packet, sometimes designated as 'Data'.

%-type decode_result() :: 'ok' | 'incomplete' | 'crc_mismatch'.


%-type read_outcome() ::
%		{ ReadEvent :: device_event(), AnyNextChunk :: telegram_chunk() }.
% Outcome of a (blocking) request of telegram reading.
%
% Exactly one event will be read (any remainding chunk returned), possibly
% waiting for it indefinitely.
%
% No content can remain to be skipped here, as by design we ended when having
% read a new event, returning any next chunk as it is.



-type switch_button_event() :: #switch_button_event{}.
% Event sent by EEP F6-01: Switch Buttons (with no rockers).
%
% Refer to [EEP-spec] for further details.

-type rocker_switch_event() :: #rocker_switch_event{}.
-type position_switch_event() :: #position_switch_event{}.


-type device_event() :: switch_button_event()
					  | rocker_switch_event()
					  | position_switch_event().
% Any event notified by an EnOcean device.


-export_type([ oceanic_server_pid/0, serial_server_pid/0, event_listener_pid/0,
			   device_name/0, enocean_device/0,
			   device_table/0,
			   tty_detection_outcome/0, serial_protocol/0, telegram/0,
			   rorg/0, func/0, type/0, eep/0, eep_id/0,
			   contact_status/0,
			   ptm_switch_module_type/0, eurid/0,
			   packet/0, esp3_packet/0, crc/0, packet_type/0,
			   %payload/0, decode_result/0,
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


-type decoding_outcome() ::

	{ 'not_reached' | 'incomplete' | 'invalid' | 'unsupported',
	  ToSkipLen :: count(), NextChunk :: telegram_chunk(), oceanic_state() }

  | { 'decoded', device_event(), NextChunk :: telegram_chunk(),
	  oceanic_state() }.
% The outcomes of an attempt of integrating / decoding a telegram chunk.


% Defines.

% The name under which the Oceanic server will register locally:
-define( oceanic_server_reg_name, oceanic_server ).


% Transmission speed, in bits per second:
-define( esp2_speed, 9600 ).
-define( esp3_speed, 57600 ).


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
% - for RPS: the controller must have been configured out of the band, to know a
% priori, for each device/Sender ID of interest what is its corresponding EEP
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

	% A table recording all information regarding the known Enocean devices:
	device_table :: device_table(),

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

-type uint8() :: type_utils:uint8().

-type timestamp() :: time_utils:timestamp().

-type registration_name() :: naming_utils:registration_name().


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

	% Linked process; symmetrical speed (in bits per second):
	SerialPid = serial:start( [ { open, TtyPath }, { speed, ?esp3_speed } ] ),

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Using TTY '~ts' to connect to Enocean gateway,"
			" corresponding to serial server ~w.", [ TtyPath, SerialPid ] ) ),

	SerialPid.



% Oceanic server process subsection.

-spec oceanic_start( device_path(), maybe( event_listener_pid() )  ) ->
											no_return().
oceanic_start( TtyPath, MaybeEventListenerPid ) ->

	SerialPid = secure_tty( TtyPath ),

	DeviceTable = load_configuration(),

	InitialState = #oceanic_state{
		serial_server_pid=SerialPid,
		device_table=DeviceTable,
		event_listener_pid=MaybeEventListenerPid },

	naming_utils:register_as( ?oceanic_server_reg_name, _RegScope=local_only ),

	oceanic_loop( _SkipLen=0, _AccChunk= <<>>, InitialState ).



% @doc Loads Oceanic configuration information from the default Ceylan
% preferences file, if any.
%
% See the 'preferences' module.
%
-spec load_configuration() -> device_table().
load_configuration() ->
	case preferences:is_preferences_default_file_available() of

		{ true, PrefPath } ->
			load_configuration( PrefPath );

		{ false, PrefPath } ->
			trace_bridge:info_fmt( "No preferences file ('~ts') found.",
								   [ PrefPath ] ),
			undefined

	end.



% @doc Loads Oceanic configuration information from the specified ETF file.
%
% It is expected to contain up to one entry whose key is the
% 'oceanic_config' atom.
%
-spec load_configuration( any_file_path() ) -> device_table().
load_configuration( ConfFilePath ) ->

	file_utils:is_existing_file_or_link( ConfFilePath )
		orelse throw( { oceanic_config_file_not_found, ConfFilePath } ),

	Pairs = file_utils:read_etf_file( ConfFilePath ),

	DeviceEntries = list_table:get_value_with_default( _K=oceanic_devices,
													   _Def=[], _Table=Pairs ),

	% Device table indexed by device eurid(), which is duplicated in the record
	% values for convenience:
	%
	table:new( [
		begin

			Eurid = text_utils:hexastring_to_integer(
				text_utils:ensure_string( EuridStr ), _ExpectPrefix=false ),

			EepBinStr = text_utils:ensure_binary( EepStr ),

			DeviceRec = #enocean_device{
				eurid=Eurid,
				name=text_utils:ensure_binary( NameStr ),
				eep=oceanic_generated:get_first_for_eep_strings( EepBinStr ) },
			{ Eurid, DeviceRec }

		end || { NameStr, EuridStr, EepStr } <- DeviceEntries ] ).



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
						[ oceanic:device_event_to_string( Event ) ] ),

					case NewState#oceanic_state.event_listener_pid of

						undefined ->
							ok;

						ListenerPid ->
							ListenerPid ! { onEnoceanEvent, [ Event, self() ] }

					end,
					oceanic_loop( _SkipLen=0, AnyNextChunk, NewState );


				{ _Unsuccessful, NewToSkipLen, NewAccChunk, NewState } ->
					% when Unsuccessful =:= not_reached
					%   orelse Unsuccessful =:= incomplete
					%   orelse Unsuccessful =:= invalid
					%   orelse Unsuccessful =:= unsupported ->
					oceanic_loop( NewToSkipLen, NewAccChunk, NewState )

			end;

		terminate ->

			SerialPid = State#oceanic_state.serial_server_pid,

			cond_utils:if_defined( oceanic_debug_tty,
				trace_bridge:debug_fmt( "Stopping serial server ~w.",
										[ SerialPid ] ) ),

			SerialPid ! stop,

			trace_bridge:debug_fmt( "Oceanic server ~w terminated.",
									[ self() ] )

	end.



% @doc Helper introduced only to make the decoding logic available for tests.
-spec test_decode( telegram_chunk() ) -> decoding_outcome().
test_decode( Chunk ) ->
	try_integrate_chunk( _ToSkipLen=0, _AccChunk= <<>>, Chunk,
						 get_test_state() ).



% @doc Returns a pseudo-state, only useful for some tests.
-spec get_test_state() -> oceanic_state().
get_test_state() ->
	% Normally there is a real serial server:
	#oceanic_state{ serial_server_pid=self(), % for correct typing
					device_table=table:new(),
					event_listener_pid=undefined }.



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
	% - byte #1: Packet start (0x55)
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

scan_for_packet_start( _Chunk= <<85, T/binary>>, DropCount ) ->
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
-spec examine_full_data( telegram_chunk(), crc(), telegram_chunk(),
	telegram_chunk(), packet_type(), telegram_chunk(), telegram_chunk(),
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
-spec decode_packet( packet_type(), telegram_chunk(), telegram_chunk(),
					 telegram_chunk(), oceanic_state() ) -> decoding_outcome().
decode_packet( _PacketType=radio_erp1_type,
			   _Data= <<RorgNum:8, DataRest/binary>>, OptData, AnyNextChunk,
			   State ) ->

	Rorg = oceanic_generated:get_first_for_rorg( RorgNum ),

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding an ERP1 radio packet of R-ORG ~ts, "
			"hence ~ts, i.e. '~ts'...",
			[ text_utils:integer_to_hexastring( RorgNum ), Rorg,
			  oceanic_generated:get_second_for_rorg_description( Rorg ) ] ) ),

	case Rorg of

		rorg_rps ->
			decode_rps_packet( DataRest, OptData, AnyNextChunk, State );

		rorg_1bs ->
			decode_1bs_packet( DataRest, OptData, AnyNextChunk, State );

		_ ->
			trace_bridge:warning_fmt( "Decoding of ERP1 radio packets "
				"of R-ORG ~ts, hence ~ts (i.e. '~ts') not implemented, "
				"the corresponding packet is thus dropped.",
				[ text_utils:integer_to_hexastring( RorgNum ), Rorg,
				  oceanic_generated:get_second_for_rorg_description( Rorg ) ] ),

			% Not even an EURID to track.

			{ unsupported, _ToSkipLen=0, AnyNextChunk, State }

	end;

decode_packet( PacketType, _Data, _OptData, AnyNextChunk, State ) ->

	trace_bridge:warning_fmt( "Unsupported packet type '~ts' (hence ignored).",
							 [ PacketType ] ),

	% Not even an EURID to track.

	{ unsupported, _ToSkipLen=0, AnyNextChunk, State }.



% @doc Decodes a rorg_rps packet.
%
% Discussed a bit in [ESP3] "2.1 Packet Type 1: RADIO_ERP1", p.18.
%
% See decode_1bs_packet/3 for more information.
%
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
-spec decode_rps_packet( telegram_chunk(), telegram_chunk(),
		telegram_chunk(), oceanic_state() ) -> decoding_outcome().
decode_rps_packet( _DataRest= <<DB0:8, SenderEurid:32,
								%Status:1/binary>>,
								T21:2, NU:2, RC:4 >>,
				   OptData, AnyNextChunk, State ) ->

	PTMSwitchModuleType = T21 + 1,

	PTMSwitchModuleTypeStr = case PTMSwitchModuleType of

		1 ->
			"PTM1xx";

		2 ->
			"PTM2xx"

	end,

	% NU expected to be 0 (Normal-message) of 1 (unassigned-message), yet found
	% to be 2 or 3.

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a R-ORG RPS packet, with DB0=~w, "
			"sender is '~ts', T21=~w (PTM switch module ~ts), NU=~w, "
			"Repeater count=~w and OptData=~w.",
			[ DB0, eurid_to_bin_string( SenderEurid, State ), T21,
			  PTMSwitchModuleTypeStr, NU, RC, OptData ] ) ),

	Event = #switch_button_event{},

	{ decoded, Event, AnyNextChunk, State };


decode_rps_packet( OtherDataRest, OptData, AnyNextChunk, State ) ->

	% Not even an EURID to track.

	trace_bridge:warning_fmt( "Non-matching R-ORG RPS packet whose data "
		"beyond R-ORG is ~w (and optional data is ~w); dropping this packet.",
		[ OtherDataRest, OptData ] ),

	{ unsupported, _ToSkipLen=0, AnyNextChunk, State }.



% @doc Decodes a rorg_1bs packet.
%
% Discussed in [EEP-spec] p.10.
%
% DB0 is the 1-byte user data, SenderEurid :: eurid() is 4, Status is 1:
-spec decode_1bs_packet( telegram_chunk(), telegram_chunk(),
			telegram_chunk(), oceanic_state() ) -> decoding_outcome().
decode_1bs_packet( DataRest= <<DB_0:8, SenderEurid:32, Status:8>>, OptData,
		AnyNextChunk, State=#oceanic_state{ device_table=DeviceTable } ) ->

	% 0xd5 is 213.

	% Apparently, at least in EEP 2.1, RPS/1BS packets are made of:
	%  1. RORG (1 byte)
	%  2. Data: DB0 (1 byte)
	%  3. Sender ID (4 bytes)
	%  4. Status (1 byte)
	%
	% DataRest is to contain fields 2, 3 and 4, and thus should be 6 bytes,
	% e.g. <<9,5,5,51,236,0>>.
	%
	% As for OptData, an example could be <<1,255,255,255,255,45,0>>.

	% For DB_0 for example, 8 bits:
	%  * bit name:   B7 - B6 - B5 - B4 - B3 - B2 - B1 - B0
	%  * bit offset:  0 -  1 -  2 -  3 -  4 -  5 -  6 -  7

	% DB_0.3 i.e. B3:
	LearnActivated = ( DB_0 band ?b3 == 0 ),

	ContactStatus = case DB_0 band ?b0 == 0 of

		true ->
			open;

		false ->
			closed

	end,

	{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId } =
		record_device_success( SenderEurid, DeviceTable ),

	NewState = State#oceanic_state{ device_table=NewDeviceTable },

	cond_utils:if_defined( oceanic_debug_decoding,
		trace_bridge:debug_fmt( "Decoding a R-ORG 1BS packet, "
			"with DataRest=~w (size: ~B bytes; DB_0=~w, "
			"so learn activated is ~ts, and contact is ~ts), sender is '~ts', "
			"status is ~w) and OptData=~w (size : ~B bytes).",
			[ DataRest, size( DataRest ), DB_0, LearnActivated, ContactStatus,
			  eurid_to_bin_string( SenderEurid, State ), Status, OptData,
			  size( OptData ) ] ) ),


	Event = #single_input_contact_event{ eurid=SenderEurid,
										 name=MaybeDeviceName,
										 eep=MaybeEepId,
										 timestamp=Now,
										 learn_activated=LearnActivated,
										 contact=ContactStatus },

	{ decoded, Event, AnyNextChunk, NewState }.




	% 0xa5 is 165.


% @doc Records that a telegram could be successfully decoded for the specified
% device, registering it if it was not already.
%
-spec record_device_success( eurid(), device_table() ) ->
	{ device_table(), timestamp(), maybe( device_name() ), maybe( eep_id() ) }.
record_device_success( Eurid, DeviceTable ) ->

	Now = time_utils:get_timestamp(),

	case table:lookup_entry( Eurid, DeviceTable ) of

		key_not_found ->
			trace_bridge:info_fmt( "Discovering Enocean device ~ts.",
								   [ eurid_to_bin_string( Eurid ) ] ),

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


		{ value, Device=#enocean_device{ name=MaybeDeviceName,
										 eep=MaybeEepId,
										 first_seen=MaybeFirstSeen,
										 telegram_count=TeleCount } } ->

			NewFirstSeen = case MaybeFirstSeen of

				undefined ->
					Now;

				FirstSeen ->
					FirstSeen

			end,

			UpdatedDevice = Device#enocean_device{ first_seen=NewFirstSeen,
												   last_seen=Now,
												   telegram_count=TeleCount+1 },

			NewDeviceTable =
				table:add_entry( Eurid, UpdatedDevice, DeviceTable ),

			{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId }

	end.



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
								   [ eurid_to_bin_string( Eurid ) ] ),

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


		{ value, Device=#enocean_device{ name=MaybeDeviceName,
										 eep=MaybeEepId,
										 first_seen=MaybeFirstSeen,
										 error_count=ErrCount } } ->

			NewFirstSeen = case MaybeFirstSeen of

				undefined ->
					Now;

				FirstSeen ->
					FirstSeen

			end,

			UpdatedDevice = Device#enocean_device{ first_seen=NewFirstSeen,
												   last_seen=Now,
												   error_count=ErrCount+1 },

			NewDeviceTable =
				table:add_entry( Eurid, UpdatedDevice, DeviceTable ),

			{ NewDeviceTable, Now, MaybeDeviceName, MaybeEepId }

	end.



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
-spec get_packet_type( integer() ) -> maybe( packet_type() ).
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




% @doc Returns a raw, direct (binary) textual description of the specified
% EURID.
%
-spec eurid_to_bin_string( eurid() ) -> bin_string().
eurid_to_bin_string( Eurid ) ->
	text_utils:integer_to_hexabinstring( Eurid, _AddHexPrefix=false ).


% @doc Returns a (binary) textual description of the specified EURID, possibly
% translated to a user-friendly device name if any is known for that device.
%
-spec eurid_to_bin_string( eurid(), oceanic_state() ) -> bin_string().
eurid_to_bin_string( Eurid, #oceanic_state{ device_table=DeviceTable } ) ->

	case table:get_value_with_default( Eurid, _Def=undefined, DeviceTable ) of

		undefined ->
			eurid_to_bin_string( Eurid );

		#enocean_device{ name=undefined } ->
			eurid_to_bin_string( Eurid );

		#enocean_device{ name=NameBinStr } ->
			NameBinStr

	end.



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



% @doc Returns a textual description of the specified device event.
-spec device_event_to_string( device_event() ) -> ustring().
device_event_to_string( #switch_button_event{ status=pressed } ) ->
	"switch button pressed";

device_event_to_string( #switch_button_event{ status=released } ) ->
	"switch button released";

device_event_to_string( #single_input_contact_event{
		eurid=Eurid,
		name=MaybeName,
		eep=MaybeEep,
		timestamp=Timestamp,
		learn_activated=LearnActivated,
		contact=ContactStatus } ) ->

	DescStr = case MaybeName of

		undefined ->
			text_utils:format( "of EURID ~ts",
							   [ eurid_to_bin_string( Eurid ) ] );

		Name ->
			text_utils:format( "named '~ts' (whose EURID is ~ts)",
							   [ Name, eurid_to_bin_string( Eurid ) ] )

	end,

	ChangeStr = case ContactStatus of

		open ->
			"opened";

		closed ->
			"closed"

	end,

	LearnStr = case LearnActivated of

		true ->
			", whereas its learn button was pressed";

		false ->
			" (whereas its learn button was not pressed)"

	end,

	EepStr = case MaybeEep of

		undefined ->
			"its EEP is not known (supposing D5-00-01)";

		EepId ->
			text_utils:format( "its EEP is ~ts (~ts)", [ EepId,
				oceanic_generated:get_second_for_eep_strings( EepId ) ] )

	end,

	text_utils:format( "contact device ~ts has been ~ts at ~ts~ts; ~ts",
		[ DescStr, ChangeStr, time_utils:timestamp_to_string( Timestamp ),
		  LearnStr, EepStr ] );

device_event_to_string( OtherEvent ) ->
	text_utils:format( "unknown event: ~p", [ OtherEvent ] ).



% @doc Returns a textual description of the specified state of the Oceanic
% server.
%
-spec state_to_string( oceanic_state() ) -> ustring().
state_to_string( #oceanic_state{
		serial_server_pid=SerialServerPid,
		device_table=DeviceTable,
		event_listener_pid=MaybeListenerPid } ) ->

	ListenStr = case MaybeListenerPid of

		undefined ->
			"not having a listener of Enocean events registered";

		ListenerPid ->
			text_utils:format( "having ~w registered as listener "
							   "of Enocean events", [ ListenerPid ] )

	end,

	text_utils:format( "Oceanic server using serial server ~w, ~ts, "
		"and knowing ~ts",
		[ SerialServerPid, ListenStr, device_table_to_string( DeviceTable ) ] ).



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
			text_utils:format( "device named '~ts' (EURID: ~ts)",
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

	SeenStr = case MaybeFirstTimestamp of

		undefined ->
			"never seen by this server";

		FirstTimestamp ->
			case MaybeLastTimestamp of

				FirstTimestamp ->
					text_utils:format( "seen only once by this server, on ~ts",
						[ text_utils:timestamp_to_string( FirstTimestamp ) ] );

				LastTimestamp ->
					text_utils:format( "seen firstly by this server on ~ts, "
						"and lastly on ~ts",
						[ text_utils:timestamp_to_string( FirstTimestamp ),
						  text_utils:timestamp_to_string( LastTimestamp ) ] )

			end

	end,

	TeleStr = case TeleCount of

		0 ->
			"none of its telegrams could be decoded";

		1 ->
			"a single of its telegrams could be decoded";

		_ ->
			text_utils:format( "~B of its telegrams could be decoded",
							   [ TeleCount ] )

	end,

	ErrStr = case ErrCount of

		0 ->
			"no telegram decoding failed";

		1 ->
			"a single telegram decoding failed";

		_ ->
			text_utils:format( "~B decodings failed", [ ErrCount ] )

	end,

	text_utils:format( "~ts respecting ~ts; it has been ~ts; ~ts and ~ts",
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
	Index = ( Checksum band 16#ff ) bxor ( HByte band 16#ff ),
	NewChecksum = element( Index + 1, CRCArray ),
	compute_crc( T, CRCArray, NewChecksum ).



% @doc Returns the array used to code/decode CRC.
-spec get_crc_array() -> tuple().
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
		get_rorg_description_topic_spec() ]	++ get_eep_topic_specs(),

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

	RawEntries = [

		% Contacts:
		{ single_input_contact, "D5-00-01" },


		% Temperature and humidity sensors:

		% Lower-range, 0°C to +40°C and 0% to 100%:
		{ thermo_hygro_low, "A5-04-01" },

		% Mid-range, -20°C to +60°C and 0% to 100%
		{ thermo_hygro_mid, "A5-04-02" },

		% Higher-range, -20°C to +60°C 10bit-measurement and 0% to 100%:
		{ thermo_hygro_high, "A5-04-03" }

				 ],

	AsTripletsEntries = [ { EepId, string_to_eep( EepStr ) }
								|| { EepId, EepStr } <- RawEntries ],

	AsStringsEntries = [ { EepId, text_utils:string_to_binary( EepStr ) }
								|| { EepId, EepStr } <- RawEntries ],

	[ { eep_triplets, AsTripletsEntries }, { eep_strings, AsStringsEntries } ].
