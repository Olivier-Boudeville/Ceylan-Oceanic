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
% Creation date: Wednesday, October 26, 2022.

-module(oceanic_constants).

-moduledoc """
Module defining the Oceanic constants.

Called by `oceanic:generate_support_modules/0`, and results in the
`oceanic_generated` module (thus in the `oceanic_generated.beam` file).
""".



-export([ get_maybe_packet_type_topic_spec/0,
		  get_maybe_return_code_topic_spec/0,
		  get_maybe_event_code_topic_spec/0, get_maybe_rorg_topic_spec/0,
		  get_maybe_rorg_description_topic_spec/0,
		  get_maybe_common_command_topic_spec/0,
		  get_maybe_vld_d2_00_cmd_topic_spec/0, get_maybe_eep_topic_specs/0 ] ).


% Implementation notes:
%
% Now all topics are maybe-ones, so that it is not possible to crash the caller
% by specifying an element that is not referenced there.


% Type shorthand:

-type topic_spec() :: const_bijective_topics:topic_spec().




-doc "Returns the specification for the `packet_type` topic.".
-spec get_maybe_packet_type_topic_spec() -> topic_spec().
get_maybe_packet_type_topic_spec() ->

	% We use our recommended order (first set for internal, second one for
	% third-party).

	% For packet types, as defined in [ESP3]:
	Entries = [
		{ reserved_type,           16#00 },
		{ radio_erp1_type,         16#01 },
		{ response_type,           16#02 },
		{ radio_sub_tel_type,      16#03 },
		{ event_type,              16#04 },
		{ common_command_type,     16#05 },
		{ smart_ack_command_type,  16#06 },
		{ remote_man_command_type, 16#07 },
		% Not existing: 16#08.
		{ radio_message_type,      16#09 },
		{ radio_erp2_type,         16#0A },
		{ radio_802_15_4_type,     16#10 },
		{ command_2_4_type,        16#11 } ],

	{ packet_type, Entries, _ElemLookup='maybe' }.



-doc """
Returns the specification for the `return_code` topic.

For return codes, as defined in `[ESP3]`.
""".
-spec get_maybe_return_code_topic_spec() -> topic_spec().
get_maybe_return_code_topic_spec() ->

	Entries = [
		{ ok_return,              16#00 },
		{ error_return,           16#01 },

        % Better than not_supported_return:
		{ unsupported_return,     16#02 },

		{ wrong_parameter_return, 16#03 },
		{ operation_denied,       16#04 } ],

	{ return_code, Entries, _ElemLookup='maybe' }.



-doc """
Returns the specification for the `event_code` topic.

For event codes, as defined in `[ESP3]`.
""".
-spec get_maybe_event_code_topic_spec() -> topic_spec().
get_maybe_event_code_topic_spec() ->

	Entries = [
		% Not existing: 16#00
		{ sa_reclaim_failed,       16#01 },
		{ sa_confirm_learn,        16#02 },
		{ sa_learn_ack,            16#03 },
		{ co_ready,                16#04 },
		{ co_event_secure_devices, 16#05 } ],

	{ event_code, Entries, _ElemLookup='maybe' }.



-doc """
Returns the specification for the `rorg` topic.

For the RORG field of an ERP radio telegram type, as defined in `[EEP]` p.14.
""".
-spec get_maybe_rorg_topic_spec() -> topic_spec().
get_maybe_rorg_topic_spec() ->

	Entries = [
		{ rorg_undefined,  16#00 },
		{ rorg_rps,        16#F6 },
		{ rorg_1bs,        16#D5 },
		{ rorg_4bs,        16#A5 },
		{ rorg_vld,        16#D2 },
		{ rorg_msc,        16#D1 },
		{ rorg_adt,        16#A6 },
		{ rorg_sm_lrn_req, 16#C6 },
		{ rorg_sm_lrn_ans, 16#C7 },
		{ rorg_rec,        16#A7 },
		{ rorg_ex,         16#C5 },
		{ rorg_sec,        16#30 },
		{ rorg_sec_encaps, 16#31 },
		{ rorg_sec_man,    16#34 },
		{ rorg_signal,     16#D0 },
		{ rorg_ute,        16#D4 } ],

	{ rorg, Entries, _ElemLookup='maybe' }.



-doc "Returns the specification for the `rorg_description` topic.".
-spec get_maybe_rorg_description_topic_spec() -> topic_spec().
get_maybe_rorg_description_topic_spec() ->

	Entries = [
		{ rorg_undefined,  <<"(undefined RORG)">> },
		{ rorg_rps,        <<"RPS (Repeated Switch Communication)">> },
		{ rorg_1bs,        <<"1BS (1-byte Communication)">> },
		{ rorg_4bs,        <<"4BS (4-byte Communication)">> },
		{ rorg_vld,        <<"VLD (Variable Length Data)">> },
		{ rorg_msc,        <<"MSC (Manufacturer-Specific Communication)">> },
		{ rorg_adt,        <<"ADT (Addressing Destination Telegram)">> },
		{ rorg_sm_lrn_req, <<"SM_LRN_REQ (SMART ACK Learn Request)">> },
		{ rorg_sm_lrn_ans, <<"SM_LRN_ANS (SMART ACK Learn Answer)">> },
		{ rorg_rec,        <<"SM_REC (SMART ACK Reclaim)">> },
		{ rorg_ex,         <<"SYS_EX (Remote Management)">> },
		{ rorg_sec,        <<"SEC (Secure telegram)">> },
		{ rorg_sec_encaps,
			<<"SEC_ENCAPS (Secure telegram with RORG encapsul ation)">> },
		{ rorg_man,        <<"SEC_MAN (Maintenance Security message)">> },
		{ rorg_signal,     <<"SIGNAL (Signal telegram)">> },
		{ rorg_ute,        <<"UTE (Universal Teach In)">> } ],

	{ rorg_description, Entries, _ElemLookup='maybe' }.



-doc """
Returns the specification for the `common_command` topic.

First elements are function codes, second ones are function names.
""".
-spec get_maybe_common_command_topic_spec() -> topic_spec().
get_maybe_common_command_topic_spec() ->

	Entries = [
		{ 1,  co_wr_sleep },          % Enter energy saving mode
		{ 2,  co_wr_reset },          % Reset the device
		{ 3,  co_rd_version },        % Read the device version information
		{ 4,  co_rd_sys_log },        % Read system log
		{ 5,  co_wr_sys_log },        % Reset system log
		{ 6,  co_wr_bist },           % Perform Self Test
		{ 7,  co_wr_idbase },         % Set ID range base address
		{ 8,  co_rd_idbase },         % Read ID range base address
		{ 9,  co_wr_repeater },       % Set Repeater Level
		{ 10, co_rd_repeater },       % Read Repeater Level
		{ 11, co_wr_filter_add },     % Add filter to filter list
		{ 12, co_wr_filter_del },     % Delete a specific filter from filter
									  % list
		{ 13, co_wr_filter_del_all }, % Delete all filters from filter list
		{ 14, co_wr_filter_enable },  % Enable / disable filter list
		{ 15, co_rd_filter },         % Read filters from filter list
		{ 16, co_wr_wait_maturity },  % Wait until the end of telegram maturity
									  % time before received radio telegrams
									  % will be forwarded to the external host
		{ 17, co_wr_subtel },         % Enable / Disable transmission of
									  % additional subtelegram info to the
									  % external host info to the external host
		{ 18, co_wr_mem },            % Write data to device memory
		{ 19, co_rd_mem },            % Read data from device memory
		{ 20, co_rd_mem_address },    % Read address and length of the
									  % configuration area and the Smart Ack
									  % Table
		{ 21, co_rd_security },       % DEPRECATED: Read own security
									  % information (level, key)
		{ 22, co_wr_security },       % DEPRECATED: Write own security
									  % information (level,key)
		{ 23, co_wr_learnmode },      % Enable / disable learn mode
		{ 24, co_rd_learnmode },      % Read learn mode status
		{ 25, co_wr_securedevice_add }, % DEPRECATED: Add a secure device
		{ 26, co_wr_securedevice_del }, % Delete a secure device from the link
										% table
		{ 27, co_rd_securedevice_by_index }, % DEPRECATED: Read secure device by
											 % index
		{ 28, co_wr_mode },                  % Set the gateway transceiver mode
		{ 29, co_rd_numsecuredevices },      % Read number of secure devices in
											 % the secure link table
		{ 30, co_rd_securedevice_by_id },    % Read information about a specific
											 % secure device from the secure
											 % link table using the device ID
		{ 31, co_wr_securedevice_add_psk },  % Add Pre-shared key for inbound
											 % secure device
		{ 32, co_wr_securedevice_sendteachin }, % Send Secure Teach-In message
		{ 33, co_wr_temporary_rlc_window },  % Set a temporary rolling-code
											 % window for every taught-in device
		{ 34, co_rd_securedevice_psk },      % Read PSK
		{ 35, co_rd_dutycycle_limit }, % Read the status of the duty cycle limit
									   % monitor
		{ 36, co_set_baudrate },       % Set the baud rate used to communicate
									   % with the external host
		{ 37, co_get_frequency_info }, % Read the radio frequency and protocol
									   % supported by the device
		%{ 38, }, % reserved
		{ 39, co_get_stepcode },       % Read Hardware Step code and Revision
									   % of the Device
		%{ 40, }, % reserved
		%{ 41, }, % reserved
		%{ 42, }, % reserved
		%{ 43, }, % reserved
		%{ 44, }, % reserved
		%{ 45, }, % reserved
		{ 46, co_wr_reman_code },      % Set the security code to unlock Remote
									   % Management functionality via radio
		{ 47, co_wr_startup_delay },   % Set the startup delay (time from power
									   % up until start of operation)
		{ 48, co_wr_reman_repeating }, % Select if REMAN telegrams originating
									   % from this module can be repeated
		{ 49, co_rd_reman_repeating }, % Check if REMAN telegrams originating
									   % from this module can be repeated
		{ 50, co_set_noisethreshold }, % Set the RSSI noise threshold level for
									   % telegram reception
		{ 51, co_get_noisethreshold }, % Read the RSSI noise threshold level
									   % for telegram reception
		%{ 52, }, % reserved
		%{ 53, }, % reserved
		{ 54, co_wr_rlc_save_period }, % Set the period in which outgoing RLCs
									   % are saved to the EEPROM
		{ 55, co_wr_rlc_legacy_mode }, % Activate the legacy RLC security mode
									   % allowing roll-over and using the RLC
									   % acceptance window for 24bit explicit
		{ 56, co_wr_securedevicev2_add }, % Add secure device to secure link
										  % table
		{ 57, co_rd_securedevicev2_by_ind }, % Read secure device from secure
											 % link table using the table index
		{ 58, co_wr_rssitest_mode }, % Control the state of the RSSI-Test mode.
		{ 59, co_rd_rssitest_mode }, % Read the state of the RSSI-Test Mode.
		{ 60, co_wr_securedevice_mainten }, % Add the maintenance key
											% information into the secure link
											% table.
		{ 61, co_rd_securedevice_mainten }, % Read by index the maintenance key
											% information from the secure link
											% table.
		{ 62, co_wr_transparent_mode },     % Control the state of the
											% transparent mode.
		{ 63, co_rd_transparent_mode },     % Read the state of the transparent
											% mode.
		{ 64, co_wr_tx_only_mode },   % Control the state of the TX only mode.
		{ 65, co_rd_tx_only_mode }    % Read the state of the TX only mode.
			  ],

	{ common_command, Entries, _ElemLookup='maybe' }.



-doc """
Returns the specification for the `vld_d2_00_cmd` topic.

First elements are VLD D2-00 command identifiers, designated by the CMD field of
these VLD telegrams, the 4 last bits of the first byte of the payload (hence 16
possible values)

Second elements are their higher-level command names.

Described in `[EEP-spec]` p.131.
""".
-spec get_maybe_vld_d2_00_cmd_topic_spec() -> topic_spec().
get_maybe_vld_d2_00_cmd_topic_spec() ->

	Entries = [
		{ 16#01, actuator_set_output },
		{ 16#02, actuator_set_local },
		{ 16#03, actuator_status_query },
		{ 16#04, actuator_status_response },
		{ 16#05, actuator_set_measurement },
		{ 16#06, actuator_measurement_query },
		{ 16#07, actuator_measurement_response },
		{ 16#08, actuator_set_pilot_wire_mode },
		{ 16#09, actuator_pilot_wire_mode_query },
		{ 16#0A, actuator_pilot_wire_mode_response },
		{ 16#0B, actuator_set_external_interface_settings },
		{ 16#0C, actuator_external_interface_settings_query },
		{ 16#0D, actuator_external_interface_settings_response } ],

	{ vld_d2_00_cmd, Entries, _ElemLookup='maybe' }.



-doc "Returns the specification for the `eep` topics.".
-spec get_maybe_eep_topic_specs() -> [ topic_spec() ].
get_maybe_eep_topic_specs() ->

	% We want to be able to associate one of our EEP identifiers
	% (e.g. 'single_input_contact') to either an internal triplet (e.g. {16#D5,
	% 16#00, 16#01}) or its counterpart string (e.g. "D5-00-01").

	% {eep_id, eep_string()} pairs:
	RawEntries = [

		% Temperature sensors:
        { thermometer, "A5-02-05" },


		% Temperature and humidity sensors:

		% Lower-range, 0°C to +40°C and 0% to 100%:
		{ thermo_hygro_low, "A5-04-01" },

		% Mid-range, -20°C to +60°C and 0% to 100%:
		{ thermo_hygro_mid, "A5-04-02" },

		% Higher-range, -20°C to +60°C 10bit-measurement and 0% to 100%:
		{ thermo_hygro_high, "A5-04-03" },


		% Buttons:

		% Single button:
		{ push_button, "F6-01-01" },

		% Two rockers (they differ in terms of application style):
		{ double_rocker_switch_style_1, "F6-02-01" },
		{ double_rocker_switch_style_2, "F6-02-02" },

		% Contacts:
		{ single_input_contact, "D5-00-01" },


		% Electronic switches and dimmers (e.g. smart plugs):
		{ smart_plug,               "D2-01-0A" },
		{ smart_plug_with_metering, "D2-01-0B" },


		% In-wall modules:
		{ single_channel_module, "D2-01-0E" },
		{ double_channel_module, "D2-01-12" } ],

	AsTripletsEntries = [ { EepId, oceanic_text:string_to_eep( EepStr ) }
								|| { EepId, EepStr } <- RawEntries ],

	AsStringsEntries = [ { EepId, text_utils:string_to_binary( EepStr ) }
								|| { EepId, EepStr } <- RawEntries ],

	% As a user-specified EEP might not be found:
	ElementLookup = 'maybe',

	[ { eep_triplets, AsTripletsEntries, ElementLookup },
	  { eep_strings,  AsStringsEntries,  ElementLookup } ].
