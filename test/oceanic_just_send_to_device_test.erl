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
% Creation date: Thursday, October 20, 2022.

-module(oceanic_just_send_to_device_test).

-moduledoc """
Testing of the Ceylan-Oceanic **sending to actual devices telegrams**.

The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
prerequisites shall be already available.

This test mostly does not depend on Oceanic, it just focuses on raw sending, and
performs no actual encoding.
""".


% For oceanic_decode_recorded_test:
-export([ run/0 ]).

% Silencing if unused:
-export([ replay_telegrams/1, replay_telegrams_for_eltako_smart_plug/3,
          replay_telegrams_for_white_switch/3,

          emit_forged_telegrams/1,
          emit_forged_telegrams_for_eltako_smart_plug/5,
          emit_forged_telegrams_for_nodon_smart_plug/5 ]).


% For the enocean_device record:
-include("oceanic.hrl").


% Type shorthands:

-type device_path() :: file_utils:device_path().

-type ms_duration() :: time_utils:ms_duration().

-type eurid_string() :: oceanic:eurid_string().


% The tested Eltako smart plug can learn a new device by pressing a relatively
% long time (a bit more than 1 second) its left button (fixed LED lights up),
% then pressing (shortly) its right (LED blinks), until a telegram is received
% (then LED turns off).



-doc """
These telegrams were captured verbatim by oceanic_just_record_device_test.
""".
-spec replay_telegrams( pid() ) -> void().
replay_telegrams( SerialPid ) ->

    % Duration between a switch on then off command:
    InterCommandDuration = 2000,

    % Duration between a press and its release-like telegram:
    InterPressReleaseDuration = 500,

    replay_telegrams_for_eltako_smart_plug( InterCommandDuration,
        InterPressReleaseDuration, SerialPid ).

    %replay_telegrams_for_white_switch( InterCommandDuration,
    %    InterPressReleaseDuration, SerialPid ).



-doc "Replays telegrams for the Eltako smart plug.".
-spec replay_telegrams_for_eltako_smart_plug( ms_duration(), ms_duration(),
                                              pid() ) -> void().
replay_telegrams_for_eltako_smart_plug( InterCommandDuration,
        InterPressReleaseDuration, SerialPid ) ->

    % Was previously driven by a green (simple) switch, now with the white O2
    % Line (double) switch.

    test_facilities:display(
        "Testing the behaviour of the Eltako smart plug." ),

    % Defining first the telegrams to emit of interest:
    %
    % (for each telegram, an extra variation is also available for additional
    % testing; it only differs in terms of dbM and thus final CRC)

    % Short name: telegram "A"
    TopButtonPressedTelegram = oceanic:hexastring_to_telegram(
        %"55000707017af630002ef1963001ffffffff4600d4" ),
         "55000707017af630002ef1963001ffffffff4a0028" ),

    % Short name: telegram "B"
    TopButtonReleasedTelegram = oceanic:hexastring_to_telegram(
        %"55000707017af600002ef1962001ffffffff460062" ),
         "55000707017af600002ef1962001ffffffff4c00e0" ),

    % Short name: telegram "C"
    BottomButtonPressedTelegram = oceanic:hexastring_to_telegram(
        %"55000707017af610002ef1963001ffffffff43006b" ),
         "55000707017af610002ef1963001ffffffff47003f" ),

    % Short name: telegram "D"
    BottomButtonReleasedTelegram = oceanic:hexastring_to_telegram(
        %"55000707017af600002ef1962001ffffffff440048" ),
         "55000707017af600002ef1962001ffffffff460062" ),

    basic_utils:ignore_unused( [ TopButtonPressedTelegram,
        TopButtonReleasedTelegram, BottomButtonPressedTelegram,
        BottomButtonReleasedTelegram ] ),


    % What we found out, once this (unique) (green switch) rocker has been
    % learnt by the Eltako smart plug (by pressing once its top button when in
    % learn mode):
    %
    % - from an initially switched off plug:
    %   * just sending A (any number of times) will not trigger anything
    %   * same for B and D
    %   * the first sending of C will switch on the lamp, but the next ones will
    %   have no effect
    %
    % - from an initially switched on plug:
    %   * just sending A (any number of times) will not trigger anything
    %   * same for B and D
    %   * the first sending of C will switch off the lamp, but the next ones
    %   will have no effect

    % This looks illogical, as A, B and D would be useless, whereas C would have
    % inconsistent effects. The reason is that the interpretation of a telegram
    % depends on the one of the previous telegram(s).
    %
    % If we emulate more closely the behaviour of the rocker, the user presses
    % and releases first its top button (light turns on), then the user presses
    % and releases its bottom button (light becomes off). This corresponds to
    % sending A then B are sent, then C then D.
    %
    % Nevertheless if we play this scenario, we see a different outcome than
    % with the actual switch: each run of it will toggle (on/off) the lamp once
    % (not twice, as expected); we also notice that the actual switch trigger
    % happens when C is processed.
    %
    % If trying to simplify this scenario (to obtain the same effect with fewer
    % telegrams):
    %
    % - just removing the sending of D: scenario does nothing (C does not
    % operate) -> D necessary to "unblock" C, hence D kept
    % - just removing the sending of A: scenario OK
    % - just removing the sending of B: scenario OK
    % - removing the sending of A and B: scenario OK
    %
    % Moreover B and D are the same (if ignoring dBM, which does not matter).

    % In conclusion:
    %  - the switch action happens iff, and when, C is processed
    %  - D must be sent after C to "unblock it"
    %  - A and B are useless here
    %
    % So we *can* operate this plug (one stateful command, sending C then D for
    % each toggle), yet this is not the same behaviour as when using the switch
    % (two stateless commands, to force on or off), which is rather confusing.

    %test_facilities:display( "Emulating the top button of the green switch "
    %  "being pressed." ),
    %SerialPid ! { send, TopButtonPressedTelegram },

    % Sheer paranoia:
    timer:sleep( InterPressReleaseDuration ),

    %test_facilities:display( "Emulating the top button of the green switch "
    %  "being released." ),
    %SerialPid ! { send, TopButtonReleasedTelegram },


    test_facilities:display( "Pausing." ), timer:sleep( InterCommandDuration ),

    test_facilities:display( "Emulating the bottom button of the green switch "
        "being pressed." ),
    SerialPid ! { send, BottomButtonPressedTelegram },

    % Sheer paranoia:
    timer:sleep( InterPressReleaseDuration ),

    test_facilities:display( "Emulating the bottom button of the green switch "
        "being released." ),
    SerialPid ! { send, BottomButtonReleasedTelegram },

    test_facilities:display( "End of green switch test." ).




-doc """
Replays telegrams for the white switch, once its left rocker has been learnt by
the Eltako smart plug by pressing (once) its top button.
""".
-spec replay_telegrams_for_white_switch( ms_duration(), ms_duration(),
                                         pid() ) -> void().
replay_telegrams_for_white_switch( InterCommandDuration,
        _InterPressReleaseDuration, SerialPid ) ->

    % Double-rocker device (whose EURID is 002ef196) has its top A button
    % pressed, based on with a single subtelegram, targeted to the address for
    % broadcast transmission, best RSSI value being -68 dBm; security level:
    % telegram not processed; its EEP is double_rocker_switch (F6-02-01):
    %
    PressTelegram1 =
        %<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,68,0,254>>,
        %<<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,57,0,181>>,
        %<<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,58,0,60>>,
        <<85,0,7,7,1,122,246,48,0,47,80,214,48,1,255,255,255,255,61,0,177>>,

    test_facilities:display( "Sending first press event: ~ts.",
                             [ oceanic:telegram_to_string( PressTelegram1 ) ] ),

    SerialPid ! { send, PressTelegram1 },


    test_facilities:display( "Sending first release event." ),

    timer:sleep( InterCommandDuration ),

    % Double-rocker device (whose EURID is 002ef196) has no button released
    % simultaneously, based on with a single subtelegram, targeted to the
    % address for broadcast transmission, best RSSI value being -65 dBm;
    % security level: telegram not processed; its EEP is double_rocker_switch
    % (F6-02-01):
    %
    ReleaseTelegram1 =
        <<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,65,0,9>>,

    SerialPid ! { send, ReleaseTelegram1 },


    test_facilities:display( "Sending first press event." ),

    PressTelegram2 =
        <<85,0,7,7,1,122,246,48,0,46,225,150,48,1,255,255,255,255,73,0,23>>,

    SerialPid ! { send, PressTelegram2 },


    test_facilities:display( "Sending second release event." ),

    ReleaseTelegram2 =
        <<85,0,7,7,1,122,246,0,0,46,225,150,32,1,255,255,255,255,73,0,161>>,

    SerialPid ! { send, ReleaseTelegram2 }.




-spec emit_forged_telegrams( pid() ) -> void().
emit_forged_telegrams( SerialPid ) ->

    % If trying to impersonate directly a device of interest (e.g. my green
    % switch, expected to have already been learnt by the actuator), despite the
    % USB dongle having a different base ID (anyway the protocol will not be
    % fooled only by matching source EURIDs):
    %
    %SourceEuridStr = "002e0000",

    % If using our true own base EURID (as read from the USB dongle through a
    % Common Command) - thus requiring the actual actuator to have learnt
    % specifically that base EURID:
    %
    % Note that if the following bogus EURID is replaced with the actual one of
    % our Enocean gateway, then the smart plug is expected to be switched on
    % then off (otherwise, if using a bogus EURID, nothing is expected to
    % happen):
    %
    %
    % WARNING, this EURID is bogus and thus this test, if not modified to match
    % the one of the USB dongle, will NOT trigger anything, on any plug:
    %
    %SourceEuridStr = "ffa20000",
    SourceEuridStr = "ffa2df00",

    % Both seem to work identically:
    %PreferReleaseToMultipress = true,
    PreferReleaseToMultipress = false,

    % Duration between a switch on then off command:
    InterCommandDuration = 2000,

    % Duration between a press and its release-like telegram:
    %
    % (does not seem to have an impact; maybe shorter yet non-null is a tad
    % better)
    %
    %InterPressReleaseDuration = 500,
    InterPressReleaseDuration = 50,

    %TargetPlug = eltako,
    TargetPlug = nodon,

    case TargetPlug of

        eltako ->
            emit_forged_telegrams_for_eltako_smart_plug( SourceEuridStr,
                InterCommandDuration, InterPressReleaseDuration,
                PreferReleaseToMultipress, SerialPid );

        nodon ->
            emit_forged_telegrams_for_nodon_smart_plug( SourceEuridStr,
                InterCommandDuration, InterPressReleaseDuration,
                PreferReleaseToMultipress, SerialPid )

    end.



-spec emit_forged_telegrams_for_eltako_smart_plug( eurid_string(),
            ms_duration(), ms_duration(), boolean(), pid() ) -> void().
emit_forged_telegrams_for_eltako_smart_plug( SourceEuridStr,
        InterCommandDuration, InterPressReleaseDuration,
        PreferReleaseToMultipress, SerialPid ) ->

    test_facilities:display( "Emitting forged telegrams for Eltako smart plug, "
                             "expected to work as a rocker." ),

    SourceEurid = oceanic:string_to_eurid( SourceEuridStr ),


    % We create a device for the source, so that we can decode by ourselves the
    % telegram that we will forge (in order to check its generation):

    EepId = oceanic_generated:get_first_for_eep_strings( <<"F6-02-01">> ),

    SourceDeviceRec = #enocean_device{ eurid=SourceEurid,
                                       name= <<"Test Source Device">>,
                                       eep=EepId,
                                       discovered_through=configuration,
                                       expected_periodicity=none },

    InitialDeviceTable = table:new( [ { SourceEurid, SourceDeviceRec } ] ),

    % Both next target EURIDs will work the same; however, in some cases, the
    % operation will systematically dysfunction (switches on, but never off
    % afterwards):

    %PreferBroadcast = true,
    PreferBroadcast = false,

    { TargetEurid, TargetEuridStr } = case PreferBroadcast of

        true ->
            { oceanic:get_broadcast_eurid(), "all (broadcast)" };

        false ->
            { oceanic:string_to_eurid( "050e2000" ), "specific, random target" }

    end,


    SourceAppStyle = oceanic:get_app_style_from_eep( EepId ),

    % We designate the left rocker button:
    ButtonChannel = 1,
    %ButtonChannel = 2,

    % We designate its top button position:
    FirstButtonPos = top,
    %FirstButtonPos = bottom,

    FirstButtonLoc = { ButtonChannel, FirstButtonPos },

    SecondButtonPos = bottom,
    %SecondButtonPos = top,

    SecondButtonLoc = { ButtonChannel, SecondButtonPos },


    basic_utils:ignore_unused( [ InitialDeviceTable,
        TargetEurid, TargetEuridStr, SourceAppStyle,
        ButtonChannel, FirstButtonPos, SecondButtonPos,
        FirstButtonLoc, SecondButtonLoc ] ),

    % Double-rocker device has its top A button pressed, based on with a single
    % subtelegram, targeted to the address for broadcast transmission, best RSSI
    % value being -68 dBm; security level: telegram not processed; its EEP is
    % double_rocker_switch_style_1 (F6-02-01):
    %
    TopButtonPressedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, FirstButtonLoc, pressed, TargetEurid ),


    TopButtonReleasedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, FirstButtonLoc, released, TargetEurid ),


    % Now the bottom button:
    BottomButtonPressedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, SecondButtonLoc, pressed, TargetEurid ),

    BottomButtonReleasedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, SecondButtonLoc, released, TargetEurid ),


    % Rather than saying that a button was released, we tell that no button is
    % pressed:
    %
    AllButtonReleasedTelegram =
        oceanic_encode:encode_double_rocker_multipress_telegram( SourceEurid,
            _ButtonCounting=none, released, TargetEurid ),


    basic_utils:ignore_unused( [ TopButtonPressedTelegram,
        TopButtonReleasedTelegram, BottomButtonPressedTelegram,
        BottomButtonReleasedTelegram, AllButtonReleasedTelegram ] ),


    % Sheer paranoia:
    timer:sleep( InterPressReleaseDuration ),


    test_facilities:display( "Sending as ~ts, to ~ts, for double-rocker "
        "top button released following ~ts.",
        [ SourceEuridStr, TargetEuridStr,
          oceanic:telegram_to_string( TopButtonPressedTelegram ) ] ),

    SerialPid ! { send, TopButtonPressedTelegram },

    timer:sleep( InterPressReleaseDuration ),


    TopReleaseLikeTelegram = case PreferReleaseToMultipress of

        true ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "top button released following ~ts.",
                [ SourceEuridStr, TargetEuridStr, oceanic:telegram_to_string(
                    TopButtonReleasedTelegram ) ] ),

            TopButtonReleasedTelegram;

        false ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "all buttons released following ~ts.",
                [ SourceEuridStr, TargetEuridStr,
                  oceanic:telegram_to_string( AllButtonReleasedTelegram ) ] ),

            AllButtonReleasedTelegram

    end,

    SerialPid ! { send, TopReleaseLikeTelegram },
    SerialPid ! { send, TopReleaseLikeTelegram },


    test_facilities:display( "Pausing." ), timer:sleep( InterCommandDuration ),


    test_facilities:display( "Sending as ~ts, to ~ts, for double-rocker bottom "
        "button pressed following ~ts.",
        [ SourceEuridStr, TargetEuridStr,
          oceanic:telegram_to_string( BottomButtonPressedTelegram ) ] ),
    SerialPid ! { send, BottomButtonPressedTelegram },

    % Sheer paranoia:
    timer:sleep( InterPressReleaseDuration ),

    BottomReleaseLikeTelegram = case PreferReleaseToMultipress of

        true ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "bottom button released following ~ts.",
                [ SourceEuridStr, TargetEuridStr, oceanic:telegram_to_string(
                    BottomButtonReleasedTelegram ) ] ),
            BottomButtonReleasedTelegram;


        false ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "all buttons released following ~ts.",
                [ SourceEuridStr, TargetEuridStr,
                  oceanic:telegram_to_string( AllButtonReleasedTelegram ) ] ),
            AllButtonReleasedTelegram

    end,

    SerialPid ! { send, BottomReleaseLikeTelegram },

    basic_utils:ignore_unused(
        [ TopReleaseLikeTelegram, BottomReleaseLikeTelegram ] ),

    InitialTestState = oceanic:get_test_state( InitialDeviceTable ),

    %TelegramToDecode = TopReleaseLikeTelegram,
    TelegramToDecode = BottomReleaseLikeTelegram,

    % Trying to decode the telegram we just forged:
    { decoded, Event, _MaybeDiscoverOrigin, _IsBackOnline, _MaybeDevice,
      _AnyNextChunk, _NewState } = oceanic:try_integrate_chunk( _ToSkipLen=0,
           _MaybeAccChunk=undefined, TelegramToDecode, InitialTestState ),

    test_facilities:display( "Forged telegram corresponding to: ~ts.",
                             [ oceanic:device_event_to_string( Event ) ] ),

    ok.




-spec emit_forged_telegrams_for_nodon_smart_plug( eurid_string(),
        ms_duration(), ms_duration(), boolean(), pid() ) -> void().
emit_forged_telegrams_for_nodon_smart_plug( SourceEuridStr,
        InterCommandDuration, InterPressReleaseDuration,
        PreferReleaseToMultipress, SerialPid ) ->

    test_facilities:display(
        "Emitting forged telegrams for Nodon smart plug." ),

    SourceEurid = oceanic:string_to_eurid( SourceEuridStr ),


    % We create a device for the source, so that we can decode by ourselves the
    % telegram that we will forge (in order to check its generation):

    EepId = oceanic_generated:get_first_for_eep_strings( <<"F6-02-01">> ),

    SourceDeviceRec = #enocean_device{ eurid=SourceEurid,
                                       name= <<"Test Source Device">>,
                                       eep=EepId,
                                       discovered_through=configuration,
                                       expected_periodicity=none },

    _InitialDeviceTable = table:new( [ { SourceEurid, SourceDeviceRec } ] ),

    %TargetEurid = oceanic:get_broadcast_eurid(),
    %TargetEuridStr = "all (broadcast)",

    TargetEurid = oceanic:string_to_eurid( "05936eb8" ),
    TargetEuridStr = "Nodon metering plug",


    SourceAppStyle = oceanic:get_app_style_from_eep( EepId ),

    % We designate the right rocker button:
    %ButtonChannel = 1,
    ButtonChannel = 2,

    % We designate its top button position:
    FirstButtonPos = top,
    %FirstButtonPos = bottom,

    FirstButtonLoc = { ButtonChannel, FirstButtonPos },

    SecondButtonPos = bottom,
    %SecondButtonPos = top,

    SecondButtonLoc = { ButtonChannel, SecondButtonPos },


    basic_utils:ignore_unused( [ TargetEurid, TargetEuridStr, SourceAppStyle,
        ButtonChannel, FirstButtonPos, SecondButtonPos,
        FirstButtonLoc, SecondButtonLoc ] ),

    % Double-rocker device has its top A button pressed, based on with a single
    % subtelegram, targeted to the address for broadcast transmission, best RSSI
    % value being -68 dBm; security level: telegram not processed; its EEP is
    % double_rocker_switch_style_1 (F6-02-01):
    %
    TopButtonPressedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, FirstButtonLoc, pressed, TargetEurid ),


    TopButtonReleasedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, FirstButtonLoc, released, TargetEurid ),


    % Now the bottom button:
    BottomButtonPressedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, SecondButtonLoc, pressed, TargetEurid ),

    BottomButtonReleasedTelegram = oceanic_encode:encode_double_rocker_switch_telegram(
        SourceEurid, SourceAppStyle, SecondButtonLoc, released, TargetEurid ),


    % Rather than saying that a button was released, we tell that no button is
    % pressed:
    %
    AllButtonReleasedTelegram =
        oceanic_encode:encode_double_rocker_multipress_telegram( SourceEurid,
            _ButtonCounting=none, released, TargetEurid ),


    basic_utils:ignore_unused( [ TopButtonPressedTelegram,
        TopButtonReleasedTelegram, BottomButtonPressedTelegram,
        BottomButtonReleasedTelegram, AllButtonReleasedTelegram ] ),

    test_facilities:display( "Sending as ~ts, to ~ts, for double-rocker "
        "channel ~B ~ts button pressed following ~ts.",
        [ SourceEuridStr, TargetEuridStr, ButtonChannel, FirstButtonPos,
            oceanic:telegram_to_string( TopButtonPressedTelegram ) ] ),
    SerialPid ! { send, TopButtonPressedTelegram },

    % Sheer paranoia:
    timer:sleep( InterPressReleaseDuration ),

    case PreferReleaseToMultipress of

        true ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "top button released following ~ts.",
                [ SourceEuridStr, TargetEuridStr, oceanic:telegram_to_string(
                    TopButtonReleasedTelegram ) ] ),

            SerialPid ! { send, TopButtonReleasedTelegram };

        false ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "all buttons released following ~ts.",
                [ SourceEuridStr, TargetEuridStr,
                  oceanic:telegram_to_string( AllButtonReleasedTelegram ) ] ),

            SerialPid ! { send, AllButtonReleasedTelegram }

    end,



    test_facilities:display( "Pausing." ), timer:sleep( InterCommandDuration ),


    test_facilities:display( "Sending as ~ts, to ~ts, for double-rocker bottom "
        "button pressed following ~ts.",
        [ SourceEuridStr, TargetEuridStr,
          oceanic:telegram_to_string( BottomButtonPressedTelegram ) ] ),
    SerialPid ! { send, BottomButtonPressedTelegram },

    % Sheer paranoia:
    timer:sleep( InterPressReleaseDuration ),

    case PreferReleaseToMultipress of

        true ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "bottom button released following ~ts.",
                [ SourceEuridStr, TargetEuridStr, oceanic:telegram_to_string(
                    BottomButtonReleasedTelegram ) ] ),

            SerialPid ! { send, BottomButtonReleasedTelegram };

        false ->
            test_facilities:display(
                "Sending as ~ts, to ~ts, for double-rocker "
                "all buttons released following ~ts.",
                [ SourceEuridStr, TargetEuridStr,
                  oceanic:telegram_to_string( AllButtonReleasedTelegram ) ] ),
            SerialPid ! { send, AllButtonReleasedTelegram }

    end,

    %InitialTestState = oceanic:get_test_state( InitialDeviceTable ),

    % Trying to decode the telegram we just forged:
    %{ decoded, Event, _MaybeDiscoverOrigin, _IsBackOnline, _MaybeDevice,
    %  _AnyNextChunk, _NewState } = oceanic:try_integrate_chunk( _ToSkipLen=0,
    %       _MaybeAccChunk=undefined, Telegram, InitialTestState ),

    %test_facilities:display( "Forged telegram corresponding to: ~ts.",
    %                         [ oceanic:device_event_to_string( Event ) ] ),

    ok.



% Triggered iff a suitable environment is believed to be available.
-spec actual_test( device_path() ) -> void().
actual_test( TtyPath ) ->

    % No specific call to oceanic:start/*.

    test_facilities:display( "Starting the Enocean sending test based on the "
                             "gateway TTY '~ts'.", [ TtyPath ] ),


    % We hijack the Oceanic logic by interacting directly from this test process
    % with the serial server:
    %
    %TestTtyPath = "/dev/pts/10",

    SerialPid = oceanic:secure_tty( TtyPath ),

    %replay_telegrams( SerialPid ),
    emit_forged_telegrams( SerialPid ),

    SerialPid ! stop,

    % For any late printout:
    timer:sleep( 500 ).

    % No specific call to oceanic:stop/*.



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    TtyPath = oceanic:get_default_tty_path(),

    case oceanic:has_tty( TtyPath ) of

        true ->
            case executable_utils:is_batch() of

                true ->
                    test_facilities:display( "(not running the device "
                        "sending test, being in batch mode)" );

                false ->
                    actual_test( TtyPath )

            end;

        % For example in continuous integration:
        { false, Reason } ->
            test_facilities:display( "Warning: no suitable TTY environment "
                "found (cause: ~p; searched for device '~ts'), no test done.",
                [ Reason, TtyPath ] )

    end,

    test_facilities:stop().
