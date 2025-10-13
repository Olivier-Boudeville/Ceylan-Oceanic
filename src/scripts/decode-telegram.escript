#!/usr/bin/env escript
%% -*- erlang -*-

% @doc Prefer using directly the `decode-telegram.sh' script.

%% Not used: ! -pz ../../../src/utils

% Additionally: this escript will only work when run from its current
% directory...


% Copyright (C) 2022-2022 Olivier Boudeville
% [olivier (dot) boudeville (at) esperide (dot) com]


% Released as LGPL software.

% Directly using the module-based version now, for an easier debugging (ex: with
% proper stack traces, comprising line numbers).

% This script depends on the 'Oceanic' and 'Myriad' layers and on our
% 'erlang-serial' fork, and only on that code (that shall be recompiled
% beforehand).


% For update_code_path_for_myriad/0 and all:
%
% (ugly hack, in link with '-pz ...' above, simply to be able to include an
% header file...)
%
-include_lib("../../../myriad/include/scripts/myriad_script_include.hrl").

-include_lib("../../../oceanic/include/oceanic_script_include.hrl").


% @doc Entry point of this escript.
main( ArgList ) ->

    % First, enable all possible helper code (hence to be done first of all):
    MyriadRootDir = update_code_path_for_myriad(),

    % Now that Myriad is available; obtained from Oceanic include:
    _OceanicRootDir = secure_from_escript( MyriadRootDir ),

    ArgTable = script_utils:get_arguments( ArgList ),

    telegram_decoding:main( ArgTable ).
