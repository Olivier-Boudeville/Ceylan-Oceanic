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


% @doc Main module of Ceylan-Oceanic, in order to drive Enocean communications.
-module(oceanic).


-export([ get_default_tty_path/0, has_tty/0, has_tty/1,
		  start/0, start/1, stop/1,
		  read_next_telegram/0, read_next_telegram_before/1 ] ).

% Temp:
-export([ decode_telegram/1 ]).


% Below:
%
% - "[ESP3]" refers to the "Enocean Serial Protocol (ESP3) Specification" (see
% EnOceanSerialProtocol3.pdf, available from https://www.enocean.com/esp)
%
% - "[EEP]" refers to the "EnOcean Equipment Profiles" (see
% EnOcean-Equipment-Profiles-3-1-1.pdf, avalaible from
% https://www.enocean-alliance.org/eep/)


-type serial_server_pid() :: pid().
% The PID of a process in charge of a serial connection.


-type tty_detection_outcome() ::
		'true' | { 'false', 'non_existing' | { 'not_device', entry_type() } }.
% The outcome of an attempt of TTY detection.


-type serial_protocol() :: 'esp2' | 'esp3'.
% Defines the serial, bidirectional communication between a host and EnOcean
% modules.
%
% There are two Enocean serial protocols: ESP2 and ESP3.
% OCeanic focuses primarily on newer, richer, ESP3.
%
% The physical interface between a host and a EnOcean RF module (UART) is a
% 3-wire connection (Rx, Tx, GND / software handshake / full-duplex), modelled
% on RS-232 serial interface.


-type telegram() :: binary().
% A telegram (or datagram, packet) is a unitary message received from an Enocean
% gateway.
%
% Ex: `<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>.'
	   <<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>,
	   <<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,57,0,3>>


-type rorg() :: uint8().
% Describes the ERP radio telegram type, as an identifier.

-type func() :: uint8().
% Describes the basic functionality of the data content.

-type type() :: uint8().
% Describes the type of device in its individual characteristics


-type eep() :: { rorg(), func(), type() }.
% An EEP defines the coding of the data to be exchanged, so that two devices
% complying to the same EEP can be interchanged.


-type eep_id() :: atom().
% The identifier of an EnOcean Equipment Profile, corresponding to
% (R-ORG)-(FUNC)-(TYPE), e.g. 'F6-02-01'.


-type packet() :: binary().
% A ESP-based data unit.

-type crc() :: byte().
% The CRC code corresponding to a packer.


-type esp3_packet() :: binary().
% An ESP3 data unit, consisting of Header, Data and Optional Data.


-type packet_type() :: 'reserved' | 'radio_erp1' | 'response'
					 | 'radio_sub_tel' | 'event' | 'common_command'
					 | 'smart_ack_command' | 'remote_man_command'
					 | 'radio_message' | 'radio_erp2' | 'radio_802_15_4'
					 | 'command_2_4'.
% The type of a (typically ESP3) packet.
%
% Refer to [ESP3] p.12.


% Would be overkill:
% -type packet_type_table() :: bijective_table( packet_type(), uint8() ).


-type payload() :: binary().
% The payload of a (typically ESP3) packet, sometimes designated as 'Data'.

-type decode_result() :: 'ok' | 'incomplete' | 'crc_mismatch'.


-export_type([ serial_server_pid/0, tty_detection_outcome/0,
			   serial_protocol/0, telegram/0,
			   rorg/0, func/0, type/0, eep/0, eep_id/0,
			   packet/0, esp3_packet/0, crc/0, packet_type/0, payload/0,
			   decode_result/0 ]).



% Implementation notes:


% Local types:

-type content() :: binary().

-type esp3_header() :: binary().
% 32-bit.


% Transmission speed, in bits per second:
-define( esp2_speed, 9600 ).
-define( esp3_speed, 57600 ).


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


% For the RORG field of an ERP radio telegram type, as defined in [EPP]::
-define( rorg_undefined,  16#00 ).
-define( rorg_rps,        16#F6 ).
-define( rorg_bs1,        16#D5 ).
-define( rorg_bs4,        16#A5 ).
-define( rorg_vld,        16#D2 ).
-define( rorg_msc,        16#D1 ).
-define( rorg_adt,        16#A6 ).
-define( rorg_sm_lrn_req, 16#C6 ).
-define( rorg_sm_lrn_ans, 16#C7 ).
-define( rorg_rec,        16#A7 ).
-define( rorg_ex,         16#C5 ).
-define( rorg_sec,        16#30 ).
-define( rorg_sec_encaps, 16#31 ).
-define( rorg_ute,        16#D4 ).






% The EnOcean Radio Protocol (ERP) covers the first three layers of the OSI
% model: Physical, Data Link and Network.
%
% The CRC (Cyclic Redundancy Check) or polynomial code checksum of a
% sub-telegram can be computed.

% EURID (EnOcean Unique Radio Identifier) is a unique and non-changeable
% identification number assigned every EnOcean transmitter during its production
% process.





% By default, 8 bits of data, no parity, 1 stop bit.
% Optional repeater mode.
% Some modules support multiple channels.


% Depends on Ceylan-Myriad and Serial; refer to http://oceanic.esperide.org)



% Shorthands:

-type file_path() :: file_utils:file_path().
-type entry_type() :: file_utils:entry_type().

-type uint8() :: type_utils:uint8().

-type time_out() :: time_utils:time_out().

%-type bijective_table( F, S ) :: bijective_table:bijective_table( F, S ).



% @doc Returns the path to the default allocated TTY to the USB Enocean gateway.
-spec get_default_tty_path() -> file_path().
get_default_tty_path() ->
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
-spec has_tty( file_path() ) -> tty_detection_outcome().
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



% @doc Starts the Enocean support, based on the default conventions regarding
% the TTY allocated to the USB Enocean gateway.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start() -> serial_server_pid().
start() ->
	start( get_default_tty_path() ).



% @doc Starts the Enocean support, based on the specified path to the TTY
% allocated to the USB Enocean gateway.
%
% Throws an exception if no relevant TTY can be used.
%
-spec start( file_path() ) -> serial_server_pid().
start( TtyPath ) ->

	case has_tty( TtyPath ) of

		true ->
			ok;

		{ false, non_existing } ->
			trace_bridge:error_fmt( "The specified TTY, '~ts', "
				"does not exist. Is the device for the Enocean gateway "
				"plugged in and named accordingly?", [ TtyPath ] ),
			throw( { non_existing_tty, TtyPath } );


		{ false, { not_device, OtherType } } ->

			trace_bridge:error_fmt( "The specified TTY for the "
				"Enocean gateway, '~ts', is not a device but a ~ts.",
				[ TtyPath, OtherType ] ),

			throw( { not_a_device, TtyPath, OtherType } )

	end,

	CHECK oceanic_constants module

	% Symmetrical speed (in bits per second):
	SerialPid = serial:start( [ { open, TtyPath }, { speed, ?esp3_speed }]),

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Using TTY '~ts' to connect to Enocean gateway,"
			" corresponding to serial server ~w.", [ TtyPath, SerialPid ] ) ),

	SerialPid.


% @doc Waits (potentially forever) for the next telegram, and returns it.
-spec read_next_telegram() -> content().
read_next_telegram() ->

  receive

	% Receives data from the serial port, sent to the creator PID:
	{ data, Telegram } ->

		cond_utils:if_defined( oceanic_debug_tty,
			trace_bridge:debug_fmt( "Received a telegram of ~B bytes: ~w.",
									[ size( Telegram ), Telegram ] ) ),

		decode_telegram( Telegram )

	end.


% @doc Waits for the next telegram, and returns it, unless the specified
% time-out is reached, in which case the 'undefined' atom is returned.
%
-spec read_next_telegram_before( time_out() ) -> maybe( content() ).
read_next_telegram_before( TimeOut ) ->

  receive

	% Receives data from the serial port, sent to the creator PID:
	{ data, Telegram } ->

		cond_utils:if_defined( oceanic_debug_tty,
			trace_bridge:debug_fmt( "Received a telegram of ~B bytes before "
				"~ts: ~w.",
				[ size( Telegram ), time_utils:time_out_to_string( TimeOut ),
				  Telegram ] ) ),

		decode_telegram( Telegram )

	after TimeOut ->

		undefined

	end.



% @doc Decodes the specified telegram.
-spec decode_telegram( telegram() ) -> maybe( binary() ).
decode_telegram( Telegram ) ->

	trace_bridge:debug_fmt( "Decoding '~p' (of size ~B bytes)",
							[ Telegram, size( Telegram ) ] ),

	% First 6 bytes are the serial synchronisation:
	% - byte #1: Packet start (0x55)
	% - bytes #2-5 (32 bits): Header, containing:
	%   * byte #2-3 (16 bits): byte count of DATA to interpret
	%   * byte #4: byte count of OPTIONAL_DATA to interpret
	%   * byte #5: packet type
	% - byte #6: CRC of header

	case scan_for_packet_start( Telegram ) of

		no_content ->
			trace_bridge:debug( "(no start byte found)" ),
			undefined;

		Content ->
			trace_bridge:debug_fmt( "Content found: '~p' (of size ~B bytes)",
									[ Content, size( Content ) ] ),

			case Content of

				<<Header:4/binary, HeaderCRC, Rest/binary>> ->
					examine_header( Header, HeaderCRC, Rest );

				_ ->
					trace_bridge:debug( "(no header to decode)" ),
					undefined

			end

	end.



% Extracts the content from the start byte, which is 0x55 (i.e. 85).
%
% At least most of the time, there are no leading bytes.
%
% Refer to [ESP3] "1.6 UART synchronization (start of packet detection)".
%
scan_for_packet_start( _ContentBytes= <<>> ) ->
	no_content;

scan_for_packet_start( _ContentBytes= <<85, T/binary>> ) ->
	T;

scan_for_packet_start( _ContentBytes= <<_OtherByte, T/binary>> ) ->
	scan_for_packet_start( T ).



-spec examine_header( esp3_header(), crc(), binary() ) -> maybe( term() ).
examine_header( Header= <<DataLen:16, OptDataLen, PacketTypeNum>>, HeaderCRC,
				Rest ) ->

	trace_bridge:debug_fmt( "~B bytes of data, ~B of optional data, "
		"packet type ~B, header CRC ~B",
		[ DataLen, OptDataLen, PacketTypeNum, HeaderCRC ] ),

	case compute_crc( Header ) of

		HeaderCRC ->
			trace_bridge:debug( "Header CRC validated." ),
			case get_packet_type( PacketTypeNum ) of

				undefined ->
					trace_bridge:debug_fmt( "Unknown packet type (~B), "
						"dropping content.", [ PacketTypeNum ] ),
					undefined;

				PacketType ->
					trace_bridge:debug_fmt( "Detectect packet type: ~ts.",
											[ PacketType ] ),

					examine_full_data( Rest, DataLen, OptDataLen, PacketType )

			end;

		OtherHeaderCRC ->
			trace_bridge:debug_fmt( "Obtained other header CRC (~B), "
				"dropping content.", [ OtherHeaderCRC ] ),
			undefined

	end.



-spec examine_full_data( binary(), count(), count(), packet_type() ) ->
		  maybe( term() ).
examine_full_data( <<FullData/binary, FullDataCRC>>, DataLen, OptDataLen,
				   PacketType ) ->

	case compute_crc( FullData ) of

		FullDataCRC ->
			trace_bridge:debug( "Full-data CRC validated." ),

			DataBitLen = 8*DataLen,
			OptBitLen = 8*OptDataLen,

			case FullData of

				<<Data:DataBitLen/binary, OptData:OptBitLen/binary>> ->
					trace_bridge:debug( "Obtained all data." );

				UnexpectedData ->
					trace_bridge:debug( "Unable to split data segments, "
										"dropping content." )

			end;

		OtherDataCRC ->
			trace_bridge:debug_fmt( "Obtained other full-data CRC (~B), "
				"dropping content.", [ OtherDataCRC ] ),
			undefined

	end.



-spec get_packet_type( integer() ) -> maybe( packet_type() ).
get_packet_type( ?reserved_type ) ->reserved_type
get_packet_type( ?radio_erp1_type ) ->radio_erp1_type
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( ? ) ->
get_packet_type( Unknown ) ->
	trace_utils:error_fmt( "Unknown packet type: ~B.", [ Unknown ] ),
	undefined.


% @doc Stops the Enocean support managed by the specified serial server.
-spec stop( serial_server_pid() ) -> void().
stop( SerialPid ) ->

	cond_utils:if_defined( oceanic_debug_tty,
		trace_bridge:debug_fmt( "Stopping serial server ~w.", [ SerialPid ] ) ),

	SerialPid ! stop.


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
