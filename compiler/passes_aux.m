%-----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% Author: zs

% This file contains auxiliary routines for the passes of the front and back
% ends of the compiler.

:- module passes_aux.

:- interface.

:- import_module hlds, string, io.

:- pred write_progress_message(string, pred_id, module_info,
	io__state, io__state).
:- mode write_progress_message(in, in, in, di, uo) is det.

:- implementation.

:- import_module options, globals, hlds_out, std_util.

write_progress_message(Message, PredId, ModuleInfo) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	( { VeryVerbose = yes } ->
		io__write_string(Message),
		hlds_out__write_pred_id(ModuleInfo, PredId),
		io__write_string("\n")
	;
		[]
	).
