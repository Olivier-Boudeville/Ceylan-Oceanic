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



% Definition of device events.


% All of them start with the same four fields:
%
% - the EnOcean Unique Radio Identifier of the emitting device:
%           eurid :: oceanic:eurid()
%
% - the user-specified name (if any) for the emitting device:
%           name :: maybe( oceanic:device_name() )
%
% - the EEP (if any is defined and registered) of the emitting device:
%           eep :: maybe( oceanic:eep_id() )
%
% - the timestamp at which this event was triggered:
%           timestamp :: time_utils:timestamp()


% Event sent by EEP F6-01-01: "Switch Buttons (with no rockers)".
%
% Refer to [EEP-spec] p.15 for further details.
%
-record( push_button_event, {

	% The EnOcean Unique Radio Identifier of the emitting device:
	eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% The status of this switch button:
	status :: oceanic:button_transition() } ).



-record( rocker_switch_event, {

	% The EnOcean Unique Radio Identifier of the emitting device:
	eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp()

} ).



-record( position_switch_event, {

	% The EnOcean Unique Radio Identifier of the emitting device:
	eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp()

} ).



% Event sent by EEP D5-00-01: Single Input Contact.
%
% D5-00 corresponds to Contacts and Switches.

% Refer to [EEP-spec] for further details.
%
-record( single_input_contact_event, {

	% The EnOcean Unique Radio Identifier of the emitting device:
	eurid :: oceanic:eurid(),

	% The user-specified name (if any) of the emitting device:
	name :: maybe( oceanic:device_name() ),

	% The EEP (if any is defined and registered) of the emitting device:
	eep :: maybe( oceanic:eep_id() ),

	% The timestamp at which this event was triggered:
	timestamp :: time_utils:timestamp(),

	% Tells whether the learn button has been pressed:
	learn_activated :: boolean(),

	% Tells whether the contact is open or closed:
	contact :: oceanic:contact_status() } ).
