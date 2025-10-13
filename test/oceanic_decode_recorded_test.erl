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
% Creation date: Monday, September 26, 2022.

-module(oceanic_decode_recorded_test).

-moduledoc """
Testing of the Ceylan-Oceanic **decoding from in-file recorded datagrams**.

No specific need here for the various hardware (Enocean USB dongle) and software
(Myriad, Erlang-serial) Oceanic prerequisites.

Reads a file typically produced by the oceanic_record_device_test module. Run it
first with 'make oceanic_record_device_run' and ensure that at least a few
Enocean telegrams are received and stored, before trying to decode them here.

Very useful to debug corner cases of the decoding logic.
""".


-export([ run/0 ]).


% Type shorthand:

-type file_path() :: file_utils:file_path().



-doc "Decodes the (timestamped) telegrams stored in the specified ETF file.".
-spec decode_file( file_path() ) -> void().
decode_file( RecordPath ) ->

    % Reading {timestamp(), telegram()} pairs:
    Pairs = file_utils:read_etf_file( RecordPath ),

    Telegrams = [ T || { _Timestamp, T } <- Pairs ],

    Dups = list_utils:get_duplicates( Telegrams ),

    % Sorted by decreasing number of occurrences:
    Decs = lists:reverse( lists:keysort( _Index=2, Dups ) ),

    test_facilities:display( "Record file '~ts' read, "
        "found ~B telegrams, ~B of them having been recorded more than once: "
        "~ts",
        [ RecordPath, length( Telegrams ), length( Dups ),
          text_utils:strings_to_enumerated_string( [ text_utils:format(
            "telegram ~w recorded ~B times", [ T, C ] )
                || { T, C } <- Decs ] ) ] ),

    test_facilities:display( "Decoding this telegram stream now." ),

    DecodedEvents = decode_all( Telegrams, _AccEvents=[] ),

    test_facilities:display( "Decoded ~B (ordered) events: ~ts",
        [ length( DecodedEvents ), text_utils:strings_to_enumerated_string(
            [ oceanic:device_event_to_string( E )
                || E <- DecodedEvents ] ) ] ).



-doc """
Decodes in turn all specified telegrams, relying on the specified decoding
context.
""".
decode_all( _Telegrams=[], AccEvents ) ->
    lists:reverse( AccEvents );


decode_all( _Telegrams=[ Tl | T ], AccEvents ) ->

    test_facilities:display( "~nTest examining telegram ~ts now:",
                             [ oceanic:telegram_to_string( Tl ) ] ),

    case oceanic:test_decode( Tl ) of

        { decoded, Event, _MaybeDiscoverOrigin, _IsBackOnline, _MaybeDevice,
          _NextMaybeTelTail=undefined, _NewState } ->

            test_facilities:display( "Test decoded following event: ~ts.",
                [ oceanic:device_event_to_string( Event ) ] ),

            decode_all( T, [ Event | AccEvents ] );


        { decoded, Event, _MaybeDiscoverOrigin, _IsBackOnline, _MaybeDevice,
          NextTelTail, _NewState } ->

            trace_utils:error_fmt( "Test decoded following event: ~ts; "
                "however a telegram tail was detected:~n ~p.",
                [ oceanic:device_event_to_string( Event ), NextTelTail ] ),

            decode_all( T, [ Event | AccEvents ] );


        { Unsuccessful, _NewToSkipLen=0, _NextMaybeTelTail=undefined,
          _NewState } ->
            trace_utils:warning_fmt( "Telegram not decoded; outcome: ~p.",
                                     [ Unsuccessful ] ),
            decode_all( T, AccEvents );

        { Unsuccessful, NewToSkipLen, _NextMaybeTelTail=undefined,
          _NewState } ->
            trace_utils:warning_fmt( "Telegram not decoded; outcome: ~p, "
                "and ~B bytes to be skipped were declared.",
                [ Unsuccessful, NewToSkipLen ] ),
            decode_all( T, AccEvents );

        { Unsuccessful, _NewToSkipLen=0, NextTelTail, _NewState } ->
            trace_utils:warning_fmt( "Telegram not decoded; outcome: ~p, "
                "and a telegram tail of ~B bytes was detected:~n ~p.",
                [ Unsuccessful, size( NextTelTail ), NextTelTail ] ),
            decode_all( T, AccEvents );

        { Unsuccessful, NewToSkipLen, NextTelTail, _NewState } ->
            trace_utils:warning_fmt( "Telegram not decoded; outcome: ~p, "
                "a telegram tail was detected, ~p, "
                "and ~B bytes to be skipped were declared.",
                [ Unsuccessful, NextTelTail, NewToSkipLen ] ),
            decode_all( T, AccEvents )


    end.



-spec run() -> no_return().
run() ->

    test_facilities:start( ?MODULE ),

    %RecordPath = oceanic_record_device_test:get_record_file_path(),
    RecordPath = "my-other-test-recording.etf",

    case file_utils:is_existing_file_or_link( RecordPath ) of

        true ->
            decode_file( RecordPath );

        false ->
            test_facilities:display( "No '~ts' record file found, "
                "no decoding thereof.", [ RecordPath ] )

    end,

    test_facilities:stop().
