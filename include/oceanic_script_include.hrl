% Copyright (C) 2022-2024 Olivier Boudeville
%
% Include file meant to simplify the writing of Oceanic-using escripts.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
%
% Released as LGPL software.
% Creation date: November 2022.


% To silence any unused warning (yet Myriad counterpart include may already have
% defined functions):
%
%-export([ secure_from_escript/1 ]).


-doc """
Secures the usability of Oceanic, typically from an (e)script, from the
specified root of Myriad.

Returns the Oceanic root directory.

This code cannot be located in an Oceanic module, as they are not available yet.
""".
-spec secure_from_escript( file_utils:directory_path() ) ->
								file_utils:directory_path().
secure_from_escript( MyriadRootDir ) ->

	OceanicRootDir = file_utils:join( [ MyriadRootDir, "..", "oceanic" ] ),

	OceanicDirs = [ file_utils:join( OceanicRootDir, D )
						|| D <- [ "src", "src/scripts" ] ],

	code_utils:declare_beam_directories( OceanicDirs ),

	oceanic:secure_serial( OceanicRootDir ),

	OceanicRootDir.
