% Copyright (C) 2025-2025 Olivier Boudeville
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
% Creation date: Wednesday, May 21, 2025.

-module(oceanic_text).

-moduledoc """
Module centralising **text-related operations** to be made on behalf of
Ceylan-Oceanic.
""".

% For the records and defines:
-include("oceanic.hrl").
-include("oceanic_internal.hrl").


-export_type([

 ]).




% Telegram-related conversions:

-export([
    telegram_to_string/1, telegram_to_hexastring/1, hexastring_to_telegram/1,
    optional_data_to_string/1, optional_data_to_string/4,
    optional_data_to_short_string/2, maybe_optional_data_to_string/2 ]).



% Elementary conversions:

-export([
    button_designator_to_string/1, button_locator_to_string/1,
    ptm_module_to_string/1, nu_message_type_to_string/1,
    interpret_power_failure/1, interpret_power_failure/2,
    interpret_overcurrent_trigger/1, interpret_hardware_status/1,
    interpret_local_control/1, interpret_power_report/1,
    temperature_to_string/1, relative_humidity_to_string/1,
    learn_to_string/1,

    eurid_to_string/1, eurid_to_short_string/1,
    eurid_to_bin_string/1, eurid_to_bin_string/2,
    string_to_eurid/1, maybe_string_to_eurid/1,

    button_ref_to_string/1, button_refs_to_string/1, string_to_eep/1,

    security_level_to_string/1, repeater_count_to_string/1,

    get_button_transition_description/1, get_contact_status_description/1,

    get_eep_description/1, get_eep_description/2,
    get_eep_short_description/1, get_eep_short_description/2,
    get_device_description/1 ]).



% Device-related descriptions:

-export([ get_best_naming/2, get_best_bin_naming/2, describe_device/2,

          device_event_to_string/1, device_event_to_short_string/1,
          get_name_description/2, device_table_to_string/1,
          device_to_string/1 ]).



% Higher-level management descriptions:

-export([ command_request_to_string/1, state_to_string/1 ]).

-export([
    cits_to_string/1, device_state_change_spec_to_string/2,

    canon_listened_event_spec_to_string/1,
    canon_listened_event_spec_to_string/2,

    canon_listened_event_specs_to_string/1,
    canon_listened_event_specs_to_string/2,

    virtual_emitter_info_to_string/1,

    canon_outgoing_trigger_spec_to_string/1,
    canon_emitted_event_spec_to_string/1,

    canon_emitted_event_specs_to_string/1,
    canon_emitted_event_specs_to_string/2 ]).



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().
-type bin_string() :: text_utils:bin_string().
-type any_string() :: text_utils:any_string().

-type timestamp() :: time_utils:timestamp().

-type percent() :: math_utils:percent().

-type celsius() :: unit_utils:celsius().

-type eurid() :: oceanic:eurid().

-type telegram() :: oceanic:telegram().
%-type telegram_chunk() :: oceanic:telegram_chunk().

%-type telegram_data() :: oceanic:telegram_data().
-type telegram_opt_data() :: oceanic:telegram_opt_data().
-type decoded_optional_data() :: oceanic:decoded_optional_data().

-type command_request() :: oceanic:command_request().

-type eep() :: oceanic:eep().
-type eep_id() :: oceanic:eep_id().

-type enocean_device() :: oceanic:enocean_device().
-type device_name() :: oceanic:device_name().
-type device_type() :: oceanic:device_type().
-type device_table() :: oceanic:device_table().
-type device_event() :: oceanic:device_event().
-type device_description() :: oceanic:device_description().
-type device_state_change_spec() :: oceanic:device_state_change_spec().

-type button_locator() :: oceanic:button_locator().
-type button_designator() :: oceanic: button_designator().
-type button_transition() :: oceanic:button_transition().

-type hardware_status() :: oceanic:hardware_status().
-type power_report() :: oceanic:power_report().
-type contact_status() :: oceanic:contact_status().
-type ptm_switch_module_type() :: oceanic:ptm_switch_module_type().

-type canon_listened_event_spec() :: oceanic:canon_listened_event_spec().
-type canon_emitted_event_spec() :: oceanic:canon_emitted_event_spec().

-type canon_incoming_trigger_spec() :: oceanic:canon_incoming_trigger_spec().
-type canon_outgoing_trigger_spec() :: oceanic:canon_outgoing_trigger_spec().

-type virtual_emitter_info() :: oceanic:virtual_emitter_info().

-type oceanic_state() :: oceanic:oceanic_state().
-type oceanic_server_pid() :: oceanic:oceanic_server_pid().

-type dbm() :: oceanic:dbm().
-type security_level() :: oceanic:security_level().
-type subtelegram_count() :: oceanic:subtelegram_count().

-type button_ref() :: oceanic:button_ref().
-type nu_message_type() :: oceanic:nu_message_type() .



% Section for telegram-related conversions.


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
Returns a telegram corresponding to the specified hexadecimal string.

Useful for testing with serial clients like cutecom.
""".
-spec hexastring_to_telegram( ustring() ) -> telegram().
hexastring_to_telegram( HexaStr ) ->
	text_utils:hexastring_to_binary( HexaStr ).




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

optional_data_to_string( MaybeSubTelNum, MaybeDestinationEurid, MaybeDBm,
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
	SubTelStr = case MaybeSubTelNum of

        undefined ->
           "an unknown number of subtelegrams";

		0 ->
			"no subtelegram";

		1 ->
			"a single subtelegram";

		SubTelNum ->
			text_utils:format( "~B subtelegrams", [ SubTelNum ] )

	end,

    DestEuridStr = case MaybeDestinationEurid of

        undefined ->
            "an unknown device";

        DestEurid ->
            text_utils:format( "device whose EURID is ~ts",
                [ eurid_to_string( DestEurid ) ] )

    end,

	text_utils:format( " with ~ts, targeted to ~ts~ts~ts",
		[ SubTelStr, DestEuridStr, DBmstr, SecStr ] ).



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




% Section for elementary conversions.


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



% Other string-related conversions:


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
Returns the best naming, as any kind of string, for a device designated directly
by its name (if any), otherwise by its EURID.
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
Returns a (rather complete) textual description of the specified device event.
""".
-spec device_event_to_string( device_event() ) -> ustring().
device_event_to_string( #thermometer_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
		temperature=Temp,
		temperature_range=TempRange,
		learn_activated=LearnActivated } ) ->

	TempStr = text_utils:format( "a ~ts (sensitivity range: ~ts)",
		[ temperature_to_string( Temp ), TempRange ] ),

	text_utils:format( "thermometer sensor device ~ts which reports at ~ts "
		"~ts ~ts; this is declared~ts; ~ts; ~ts",
		[ get_name_description( MaybeName, Eurid ),
		  time_utils:timestamp_to_string( Timestamp ),
          TempStr,
		  learn_to_string( LearnActivated ),
		  optional_data_to_string( MaybeTelCount, MaybeDestEurid, MaybeDBm,
								   MaybeSecLvl ),

		  last_seen_to_string( MaybeLastSeen ),

		  % Multiple A5-05-02-like candidates:
		  get_eep_description( MaybeEepId ) ] );

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

	%%  true ->
	%%      text_utils:format( " and its ~ts",
	%%         [ button_locator_to_string( SecondButtonLocator ) ] );

	%%  false ->
	%%      ""

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


device_event_to_string( #teach_request_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		timestamp=Timestamp,
		last_seen=MaybeLastSeen,
		subtelegram_count=MaybeTelCount,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		security_level=MaybeSecLvl,
        comm_direction=CommDirection,
        response_expected=ResponseExpected,
        request_type=ReqType,
        channel_taught=ChannelTaught,
        manufacturer_id=ManufId,
        echo_content=EchoContent } ) ->

    TeachType = case ReqType of

        teach_in ->
            "in";

        teach_out ->
            "out"

    end,

    ExpectStr = case ResponseExpected of

        true ->
            "expected";

        false ->
            "not expected"

    end,

    ChannelStr = case ChannelTaught of

        all ->
            "all channels";

        1 ->
            "a single channel";

        _ ->
            text_utils:format( "~B channels", [ ChannelTaught ] )

    end,

	text_utils:format( "~ts teach-~ts request sent by ~ts at ~ts "
        "(response message ~ts) about ~ts (manufacturer id #~B, echoed: ~w); "
        "this is declared~ts; ~ts; ~ts",
        [ CommDirection, TeachType, get_name_description( MaybeName, Eurid ),
          time_utils:timestamp_to_string( Timestamp ), ExpectStr, ChannelStr,
          ManufId, EchoContent,
          optional_data_to_string( MaybeTelCount, MaybeDestEurid,
                                            MaybeDBm, MaybeSecLvl ),
		  last_seen_to_string( MaybeLastSeen ),
          % Defaults is smart plug without metering:
		  get_eep_description( MaybeEepId, _DefaultDesc="D2-01-0A" ) ] );


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
device_event_to_short_string( #thermometer_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,
		temperature=Temp,
		temperature_range=TempRange } ) ->

	TempStr = text_utils:format( "a ~ts (sensitivity range: ~ts)",
                                 [ temperature_to_string( Temp ), TempRange ] ),

	% Timestamp already available:
	text_utils:format( "The thermometer sensor device ~ts reports "
		"a ~ts; ~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ), TempStr,
		  optional_data_to_short_string( MaybeDestEurid, MaybeDBm ),

		  % Multiple A5-02-05-like candidates:
		  get_eep_short_description( MaybeEepId ) ] );

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

device_event_to_short_string( #teach_request_event{
		source_eurid=Eurid,
		name=MaybeName,
		eep=MaybeEepId,
		destination_eurid=MaybeDestEurid,
		dbm=MaybeDBm,

        response_expected=ResponseExpected,
        request_type=ReqType } ) ->

    TeachType = case ReqType of

        teach_in ->
            "in";

        teach_out ->
            "out"

    end,

	text_utils:format( "The initiator device ~ts sent a teach-~ts request "
        "(response expected: ~ts); ~ts; EEP: ~ts.",
		[ get_name_description( MaybeName, Eurid ), TeachType,
          ResponseExpected,
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



-doc "Returns a textual description of the specified device name.".
-spec get_name_description( option( device_name() ), eurid() ) -> ustring().
get_name_description( _MaybeName=undefined, Eurid ) ->
	text_utils:format( "whose EURID is ~ts", [ eurid_to_string( Eurid ) ] );

get_name_description( Name, Eurid ) ->
	text_utils:format( "'~ts' (whose EURID is ~ts)",
					   [ Name, eurid_to_string( Eurid ) ] ).





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
                                   taught=IsTaught,
								   telegram_count=TeleCount,
								   error_count=ErrCount,
								   expected_periodicity=ActPeriod,
								   activity_timer=MaybeActTimer } ) ->

    TaughtStr = case IsTaught of

        true ->
            "taught ";

        false ->
            ""

    end,

	NameStr = case MaybeName of

		undefined ->
			text_utils:format( "unnamed ~tsdevice of EURID ~ts",
				 [ TaughtStr, eurid_to_bin_string( Eurid ) ] );

		Name ->
			text_utils:format( "~tsdevice '~ts' (EURID: ~ts)",
				[ TaughtStr, Name, eurid_to_bin_string( Eurid ) ] )

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

					NextDelayMs = oceanic:compute_next_timeout( FirstSeen,
                        TeleCount, ErrCount, _Now=time_utils:get_timestamp() ),

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




% Higher-level management descriptions:

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
		[ oceanic:get_device_description( EmitterDeviceEurid,
                                          OcSrvPid ),
          DevType,
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

No Oceanic server specified, hence EURIDs cannot be resolved to actual device
descriptions.
""".
-spec canon_emitted_event_spec_to_string( canon_emitted_event_spec() ) ->
												ustring().
canon_emitted_event_spec_to_string( _CEES={ Eurid, _MaybeDevOp=undefined } ) ->
    text_utils:format( "actuator whose EURID is ~ts, to perform "
                       "its default operation", [ eurid_to_string( Eurid ) ] );

canon_emitted_event_spec_to_string( _CEES={ Eurid, DevOp } ) ->
    text_utils:format( "actuator whose EURID is ~ts, to perform "
                       "a ~ts operation", [ eurid_to_string( Eurid ), DevOp ] ).



-doc """
Returns a textual description of the specified canonical emitting event
specification, enriched thanks to the specified Oceanic server.
""".
-spec canon_emitted_event_spec_to_string( canon_emitted_event_spec(),
										  oceanic_server_pid() ) -> ustring().
canon_emitted_event_spec_to_string( _CEES={ Eurid, _MaybeDevOp=undefined },
                                    OcSrvPid ) ->
    text_utils:format( "~ts, to perform its default operation",
        [ oceanic:get_device_description( Eurid, OcSrvPid ) ] );

canon_emitted_event_spec_to_string( _CEES={ Eurid, DevOp }, OcSrvPid ) ->
    text_utils:format( "~ts, to perform a ~ts operation",
        [ oceanic:get_device_description( Eurid, OcSrvPid ), DevOp ] ).


-doc """
Returns a textual description of the specified canonical emitting event
specifications.

No Oceanic server specified, hence EURIDs cannot be resolved in actual device
descriptions.
""".
-spec canon_emitted_event_specs_to_string( [ canon_emitted_event_spec() ] ) ->
												ustring().
canon_emitted_event_specs_to_string( CEESs ) ->
	text_utils:strings_to_string(
        [ canon_emitted_event_spec_to_string( CEES ) || CEES <- CEESs ] ).


-doc """
Returns a textual description of the specified canonical emitting event
specifications, enriched thanks to the specified Oceanic server.
""".
-spec canon_emitted_event_specs_to_string( [ canon_emitted_event_spec() ],
									oceanic_server_pid() ) -> ustring().
canon_emitted_event_specs_to_string( CEESs, OcSrvPid ) ->
	text_utils:strings_to_string( [ canon_emitted_event_spec_to_string( CEES,
										OcSrvPid ) || CEES <- CEESs ] ).
