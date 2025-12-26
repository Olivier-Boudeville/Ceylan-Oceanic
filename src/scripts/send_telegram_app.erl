% Copyright (C) 2022-2026 Olivier Boudeville
%
% Module defined in order to be able to test the password generation services
% outside of an escript context.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
% Creation date: 2022.

-module(send_telegram_app).

-moduledoc """
An Oceanic application to **send Enocean telegrams from the command-line**.
""".


-export([ exec/0 ]).


% For update_code_path_for_myriad/0 and all:
-include_lib("myriad/include/myriad_script_include.hrl").



-doc "Executes this telegram-sending application.".
-spec exec() -> void().
exec() ->

    % First, enable all possible helper code (hence to be done first of all):
    update_code_path_for_myriad_from_module(),

    % To force options for testing:
    %ArgTable = cmd_line_utils:generate_argument_table( "--interactive" ),
    %ArgTable = cmd_line_utils:generate_argument_table( "-i" ),
    %ArgTable = cmd_line_utils:generate_argument_table( "-i --unexpected" ),
    %ArgTable = cmd_line_utils:generate_argument_table( "" ),
    ArgTable = cmd_line_utils:get_argument_table(),

    telegram_sending:main( ArgTable ).
