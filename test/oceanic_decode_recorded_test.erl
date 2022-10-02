% Copyright (C) 2022-2022 Olivier Boudeville
%
% This file is part of the Ceylan-OCEANIC library.
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


% @doc Testing of the Ceylan-Oceanic <b>decoding from in-file recorded
% datagrams</b>.
%
% The various hardware (Enocean USB dongle) and software (Myriad, Erlang-serial)
% prerequisites shall be already available.
%
% Reads a file typically produced by the oceanic_record_device_test module.
%
-module(oceanic_decode_recorded_test).


-export([ run/0 ]).


% Shorthand:

-type file_path() :: file_utils:file_path().



% @doc Decodes the telegrams in the specified ETF file.
-spec decode_file( file_path() ) -> void().
decode_file( RecordPath ) ->

	% Reading { timestamp(), telegram() } pairs:
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

	DecodedEvents = decode_all( Telegrams, _FirstNextChunk= <<>>,
								_AccEvents=[] ),

	test_facilities:display( "Decoded ~B (ordered) events: ~ts",
		[ length( DecodedEvents ), text_utils:strings_to_enumerated_string(
			[ oceanic:device_event_to_string( E ) || E <- DecodedEvents ] ) ] ).




% @doc Decodes in turn all specified telegrams, relying on the specified
% decoding context.
%
% First priority is to integrate any next chunk, otherwise to switch to the next
% recorded telegram:
%
decode_all( Telegrams, AccChunk, AccEvents ) when AccChunk =/= <<>> ->
	{ NewEvent, AnyNextChunk } =
		oceanic:read_next_event( _ToSkipLen=0, AccChunk ),
	decode_all( Telegrams, AnyNextChunk, [ NewEvent | AccEvents ] );

% From here AccChunk is <<>>:
decode_all( _Telegrams=[], _AccChunk, AccEvents ) ->
	lists:reverse( AccEvents );

decode_all( _Telegrams=[ Tl | T ], _AccChunk, AccEvents ) ->

	{ NewEvent, AnyNextChunk } =
		oceanic:read_next_event( _ToSkipLen=0, Tl ),

	decode_all( T, AnyNextChunk, [ NewEvent | AccEvents ] ).



-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	RecordPath = oceanic_record_device_test:get_record_file_path(),

	case file_utils:is_existing_file_or_link( RecordPath ) of

		true ->
			decode_file( RecordPath );

		false ->
			test_facilities:display( "No '~ts' record file found, "
				"no decoding thereof.", [ RecordPath ] )

	end,

	test_facilities:stop().
