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
% Creation date: Tuesday, September 27, 2022.



% Information regarding an Enocean device.
-record( enocean_device, {

	% The EnOcean Unique Radio Identifier of this device:
	eurid :: oceanic:eurid(),

	% The user-specified name (if any) for that device:
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of this device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp (if any) at which this device was first seen:
	first_seen = undefined :: maybe( time_utils:timestamp() ),

	% The timestamp (if any) at which this device was last seen:
	last_seen = undefined :: maybe( time_utils:timestamp() ),

	% The number of full telegrams successfully decoded for this device:
	telegram_count = 0 :: basic_utils:count(),

	% The number of decoding failures for this device:
	error_count = 0 :: basic_utils:count() } ).



% Definition of device events, preferably ordered by increasing EEP.


% All of them start with the same first four fields:
%
% - the EnOcean Unique Radio Identifier of the emitting device:
%           source_eurid :: oceanic:eurid()
%
% - the user-specified name (if any) for the emitting device:
%           name :: maybe( oceanic:device_name() )
%
% - the EEP (if any is defined and registered) of the emitting device:
%           eep :: maybe( oceanic:eep_id() )
%
% - the timestamp at which this event was triggered:
%           timestamp :: time_utils:timestamp()
%
%
% From here, the four next following fields, still common to all events, are
% maybe-values, as they are read from optional data:
%
% - the number of subtelegrams, if any:
%           subtelegram_count :: maybe( oceanic:subtelegram_count() )
%
% - the EURID of the target of this transmission (addressed or broadcast), if
% any:
%           destination_eurid :: maybe( oceanic:eurid() )
%
% - the best RSSI value (if any), expressed in decibels (dB) with reference to
% one milliwatt (mW), of all received subtelegrams:
%           dbm :: maybe( oceanic:dbm() )
%
% - the level of security (if any) of the received telegram:
%           security_level :: maybe( oceanic:security_level() )



% Event sent by EEP A5-04-*: "Temperature and Humidity Sensor"
%
% Refer to [EEP-spec] p.35 for further details.
%
-record( thermo_hygro_event, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The number of subtelegrams, if any:
	subtelegram_count :: maybe( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: maybe( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: maybe( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: maybe( oceanic:security_level() ),


	% Section specific to these events:

	% The percentage of relative humidity reported:
	relative_humidity :: math_utils:percent(),

	temperature :: maybe( unit_utils:celsius() ),

	% The range of the temperature sensor:
	temperature_range :: temperature_range(),

	% Tells whether the learn button has been pressed:
	learn_activated :: boolean() } ).



% Event sent by EEP D5-00-01: Single Input Contact.
%
% D5-00 corresponds to Contacts and Switches.
%
% Refer to [EEP-spec] p.27 for further details.
%
-record( single_input_contact_event, {

	% Section common to all events:

	% The EnOcean Unique Radio Identifier of the emitting device:
	source_eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The number of subtelegrams, if any:
	subtelegram_count :: maybe( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: maybe( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: maybe( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: maybe( oceanic:security_level() ),


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
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The number of subtelegrams, if any:
	subtelegram_count :: maybe( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: maybe( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: maybe( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: maybe( oceanic:security_level() ),


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
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The number of subtelegrams, if any:
	subtelegram_count :: maybe( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: maybe( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: maybe( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: maybe( oceanic:security_level() ),


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
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The number of subtelegrams, if any:
	subtelegram_count :: maybe( oceanic:subtelegram_count() ),

	% The EURID of the target of this transmission (addressed or broadcast), if
	% any:
	%
	destination_eurid :: maybe( oceanic:eurid() ),

	% The best RSSI value (if any), expressed in decibels (dB) with reference to
	% one milliwatt (mW), of all received subtelegrams:
	%
	dbm :: maybe( oceanic:dbm() ),

	% The level of security (if any) of the received telegram:
	security_level :: maybe( oceanic:security_level() ),


	% Section specific to these events:

	% Specifies (very roughly) the number of buttons involved:
	button_counting :: maybe( oceanic:button_counting() ),

	% Whether the buttons referenced by the counting are pressed or released:
	energy_bow :: oceanic:button_transition() } ).
