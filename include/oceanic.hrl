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
% Creation date: Tuesday, September 27, 2022.


% Defines of possible interest for the user:

% The name under which the Oceanic server will register locally:
-define( oceanic_server_reg_name, oceanic_server ).


% Transmission speed, in bits per second:
-define( esp2_speed, 9600 ).

% Matters, otherwise faulty content received:
-define( esp3_speed, 57600 ).


% Default EURID of the pseudo-device emitter (if any) of any telegram to be sent
% by Oceanic.
%
% (this hexadecimal pun never gets old)
%
% Translates to `<<222,173,190,239>>`.
%
% Note that now by default the base identifier of the USB gateway is used
% instead, as soon as it is determined (as read thanks to a co_rd_idbase common
% command).
%
-define( default_emitter_eurid, "DEADBEEF" ).


% Denotes a broadcast transmission (as opposed to an Addressed Transmission,
% ADT):
%
-define( eurid_broadcast, 16#ffffffff ). % That is 4294967295


% Information regarding an Enocean device.
-record( enocean_device, {

	% The EnOcean Unique Radio Identifier of this device:
	eurid :: oceanic:eurid(),

	% The user-specified name (if any) for that device:
	name :: option( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of this device:
	eep :: option( oceanic:eep_id() ),

	% Tells how this device was discovered:
	discovered_through :: oceanic:discovery_origin(),

	% The timestamp (if any) at which this device was first seen:
	first_seen = undefined :: option( time_utils:timestamp() ),

	% The timestamp (if any) at which this device was last seen:
	last_seen = undefined :: option( time_utils:timestamp() ),

	% Tells whether this device is considered by Oceanic to be online or lost:
	availability :: option( oceanic:availability_status() ),

	% The number of full telegrams successfully decoded for this device:
	telegram_count = 0 :: basic_utils:count(),

	% The number of decoding failures for this device:
	error_count = 0 :: basic_utils:count(),

	% The average duration expected to elapse between two signs of activity
	% (telegram receivings):
	%
	expected_periodicity :: oceanic:expected_periodicity(),

	% A timer, if any, set to detect whether this device vanished (ceased being
	% active), possibly being sabotaged, running out of energy, etc.:
	%
	activity_timer = undefined :: option( oceanic:timer_ref() ) } ).




% Record for commands of any type to be sent by Oceanic.
%
% Useful to report problems, typically time-out while waiting for a response
% (e.g. a mere aknowledgement).
%
%  - the identifier of the requester of that command:
%           requester :: oceanic:requester()


% Record allowing to keep track of a submitted command request.
-record( command_request, {

	% An identifier corresponding to the count of this command could be added
	% (yet only up to one can be in the air at a given time).

	% Type information about that pending command, to designate it:
	command_type :: oceanic:command_type(),

	% The telegram corresponding to that command:
	command_telegram :: oceanic:telegram(),

	% The requester of this command:
	requester :: oceanic:requester() } ).




% Definition of device events, preferably ordered by increasing EEP.


% By convention all of them start with the same first four, always-set, fields:
%
% - the EnOcean Unique Radio Identifier of the emitting device:
%           source_eurid :: oceanic:eurid()
%
% - the user-specified name (if any) for the emitting device:
%           name :: option( oceanic:device_name() )
%
% - the EEP (if any is defined and registered) of the emitting device:
%           eep :: option( oceanic:eep_id() )
%
% - the timestamp at which this event was triggered:
%           timestamp :: time_utils:timestamp()
%
% - the last timestamp (if any) at which a telegram from that device was
% intercepted before; mostly an informative way of reporting device discovery
%           last_seen :: option( time_utils:timestamp() )
%
% From here, the four next following fields, still common to all events, are
% maybe-values, as they are read from optional data:
%
% - the number of subtelegrams, if any:
%           subtelegram_count :: option( oceanic:subtelegram_count() )
%
% - the EURID of the target of this transmission (addressed or broadcast), if
% any:
%           destination_eurid :: option( oceanic:eurid() )
%
% - the best RSSI value (if any), expressed in decibels (dB) with reference to
% one milliwatt (mW), of all received subtelegrams:
%           dbm :: option( oceanic:dbm() )
%
% - the level of security (if any) of the received telegram:
%           security_level :: option( oceanic:security_level() )



% Event sent by EEP A5-04-*: "Temperature and Humidity Sensor"
%
% Refer to [EEP-spec] p.35 for further details.
%
-record( thermo_hygro_event, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: option( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: option( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The last timestamp (if any) at which a telegram from that device was
	% intercepted before; mostly an informative way of reporting whether this
	% device was just discovered
	%
	last_seen :: option( time_utils:timestamp() ),

	% The number of subtelegrams, if any:
	subtelegram_count :: option( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: option( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: option( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: option( oceanic:security_level() ),


	% Section specific to these events:

	% The percentage of relative humidity reported:
	relative_humidity :: math_utils:percent(),

	temperature :: option( unit_utils:celsius() ),

	% The range of the temperature sensor:
	temperature_range :: oceanic:temperature_range(),

	% Tells whether the learn button has been pressed:
	learn_activated :: boolean() } ).



% Event sent by EEP D5-00-01: Single Input Contact.
%
% D5-00 corresponds to Contacts and Switches.
%
% Refer to [EEP-spec] p.27 for further details.
%
% Note that, at least by default, most if not all opening detectors not only
% report state transitions (as soon as they happen; toggling between closed and
% opened), they also notify regularly (e.g. every 5-30 minutes, on average often
% 15 minutes) and spontaneously their current state (even if no specific
% transition happened), presumably to help overcoming any message loss.
%
% So any listener of these events shall store their current state, to be able to
% detect the actual transitions (even if they are late due to a prior message
% loss).
%
-record( single_input_contact_event, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: option( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: option( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The last timestamp (if any) at which a telegram from that device was
	% intercepted before; mostly an informative way of reporting whether this
	% device was just discovered
	%
	last_seen :: option( time_utils:timestamp() ),

	% The number of subtelegrams, if any:
	subtelegram_count :: option( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: option( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: option( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: option( oceanic:security_level() ),


	% Section specific to these events:

	% Tells whether the learn button has been pressed:
	learn_activated :: boolean(),

	% Tells whether the contact is open or closed:
	contact :: oceanic:contact_status() } ).



% Event sent in the context of EEP F6-01-01: "Switch Buttons (with no rockers)".
%
% Refer to [EEP-spec] p.15 for further details.
%
-record( push_button_event, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: option( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: option( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The last timestamp (if any) at which a telegram from that device was
	% intercepted before; mostly an informative way of reporting whether this
	% device was just discovered
	%
	last_seen :: option( time_utils:timestamp() ),

	% The number of subtelegrams, if any:
	subtelegram_count :: option( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: option( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: option( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: option( oceanic:security_level() ),


	% Section specific to these events:

	% Whether this switch button is pressed or released:
	transition :: oceanic:button_transition() } ).




% Event sent in the context of EEP F6-02-01: "Light and Blind Control -
% Application Style 1".
%
% There are two rockers, A and B, each to be understood as corresponding to two
% buttons (I and O), altough they cannot be pressed simultaneously.
%
% Here I corresponds to a bottom position, and O to a top one.
%
% So there is AI and AO, and BI and BO.
%
% Refer to [EEP-spec] p.15 for further details.
%
-record( double_rocker_switch_event, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: option( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: option( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The last timestamp (if any) at which a telegram from that device was
	% intercepted before; mostly an informative way of reporting whether this
	% device was just discovered
	%
	last_seen :: option( time_utils:timestamp() ),

	% The number of subtelegrams, if any:
	subtelegram_count :: option( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: option( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: option( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: option( oceanic:security_level() ),


	% Section specific to these events:

	% The button referenced by the first action:
	first_action_button :: oceanic:button_designator(),

	% Whether the button referenced by the first action is pressed or released:
	energy_bow :: oceanic:button_transition(),

	% The button referenced by the second action (if any):
	second_action_button :: oceanic:button_designator(),

	% Whether there is a second action reported:
	second_action_valid :: boolean() } ).



% Event sent in the context of EEP F6-02-01: "Light and Blind Control -
% Application Style 1".
%
% This event whether or not there are 3 or 4 buttons that are either pressed or
% released.
%
% Refer to [EEP-spec] p.16 for further details.
%
-record( double_rocker_multipress_event, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: option( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: option( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The last timestamp (if any) at which a telegram from that device was
	% intercepted before; mostly an informative way of reporting whether this
	% device was just discovered
	%
	last_seen :: option( time_utils:timestamp() ),

	% The number of subtelegrams, if any:
	subtelegram_count :: option( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: option( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: option( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: option( oceanic:security_level() ),


	% Section specific to these events:

	% Specifies (very roughly) the number of buttons involved:
	button_counting :: option( oceanic:button_counting() ),

	% Whether the buttons referenced by the counting are pressed or released:
	energy_bow :: oceanic:button_transition() } ).



% Message (that can be seen as an event) corresponding to the receiving a R-ORG
% telegram for an universal Teach-in/out request, EEP based (UTE), one way of
% pairing devices.
%
% Refer to [EEP-gen] p.17 for further details.
%
-record( teach_request, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: option( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: option( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The last timestamp (if any) at which a telegram from that device was
	% intercepted before; mostly an informative way of reporting whether this
	% device was just discovered
	%
	last_seen :: option( time_utils:timestamp() ),

	% The number of subtelegrams, if any:
	subtelegram_count :: option( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: option( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: option( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: option( oceanic:security_level() ),


	% Section specific to these events:

	comm_direction :: oceanic:communication_direction(),

	response_expected :: boolean(),

	request_type :: option( oceanic:teach_request_type() ),

	channel_taught :: oceanic:channel_taught(),

	manufacturer_id :: oceanic:manufacturer_id(),

	% The 5 bytes of that teach request that may be used directly for its
	% response:
	%
	echo_content :: binary() } ).




% Record for responses to common commands (seen as events).
%
% By convention all of them start with the same field:
%  - the identifier of the requester of that command:
%           requester :: oceanic:requester()


% Response to a successful 'read version' common command request.
-record( read_version_response, {

	% The version of the application.
	app_version :: oceanic:enocean_version(),

	% The version of the API.
	api_version :: oceanic:enocean_version(),

	chip_id :: type_utils:uint32(),
	% Unique identifier for the gateway chip.

	chip_version :: type_utils:uint32(),
	% Reserved for internal use.

	app_description :: text_utils:bin_string() } ).



% Response to a successful 'read logs' common command request.
-record( read_logs_response, {

	% This reverse order (like the read_version_response one) is preferred:

	app_counters :: oceanic:log_counters(),
	% Log counters of the application.

	api_counters :: oceanic:log_counters()
	% Logs counters for the API.

} ).



% Response to a successful 'read base ID information' (CO_RD_IDBASE) common
% command request.
%
-record( read_base_id_info_response, {

	% The start address of the Base ID Range (between 0xFF800000 and 0xFFFFFF80)
	% of the local emitting device (as read from the USB gateway):
	%
	base_eurid :: oceanic:eurid(),

	% Remaining write cycles for the Base ID.
	remaining_write_cycles :: type_utils:uint8() | 'unlimited' } ).
