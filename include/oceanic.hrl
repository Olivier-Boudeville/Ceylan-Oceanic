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
	name :: maybe( text_utils:bin_string() ),

	% The EEP (if any is defined and registered) of this device:
	eep :: maybe( oceanic:eep_id() ) } ).



% Definition of device events.


% Event sent by EEP F6-01: Switch Buttons (with no rockers).
%
% Refer to [EEP-spec] for further details.
%
-record( switch_button_event, {

	status :: 'pressed' | 'released'

} ).



-record( rocker_switch_event, {

} ).



-record( position_switch_event, {

} ).



% Event sent by EEP D5-00-01: Single Input Contact.
%
% D5-00 correspondst to Contacts and Switches.

% Refer to [EEP-spec] for further details.
%
-record( single_input_contact_event, {

	% Tells whether the learn button has been pressed:
	learn_activated :: boolean(),

	% Tells whether the contact is open or closed:
	contact :: oceanic:contact_status() } ).
