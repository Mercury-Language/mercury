%-----------------------------------------------------------------------------%
% Copyright (C) 1997-1999 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: trans_opt.m
% main author: crs
%
% Transitive intermodule optimization allows the compiler to do
% intermodule optimization that depends on other .trans_opt files.  In
% comparison to .opt files, .trans_opt files allow much more accurate
% optimization to occur, but at the cost of an increased number of
% compilations required.  The fact that a .trans_opt file may depend on
% other .trans_opt files introduces the possibility of circular
% dependencies occuring. These circular dependencies would occur if the
% data in A.trans_opt depended on the data in B.trans_opt being correct,
% and vice-versa.  
%
% The following system is used to ensure that circular dependencies cannot
% occur:
% 	When mmake <module>.depend is run, mmc calculates a suitable
% 	ordering.  This ordering is then used to create each of the .d
% 	files.  This allows make to ensure that all necessary trans_opt
% 	files are up to date before creating any other trans_opt files.
% 	This same information is used by mmc to decide which trans_opt
% 	files may be imported when creating another .trans_opt file.  By
% 	observing the ordering decided upon when mmake module.depend was
% 	run, any circularities which may have been created are avoided.
%
% This module writes out the interface for transitive intermodule optimization.
% The .trans_opt file includes:
%	- pragma termination_info declarations for all exported preds
% All these items should be module qualified.
% Constructors should be explicitly type qualified.
%
% Note that the .trans_opt file does not (yet) include clauses,
% `pragma c_code' declarations, or any of the other information
% that would be needed for inlining or other optimizations;
% currently it is used *only* for termination analysis.
%
% This module also contains predicates to read in the .trans_opt files.
%
% See also intermod.m, which handles `.opt' files.
%
%-----------------------------------------------------------------------------%

:- module trans_opt.

%-----------------------------------------------------------------------------%

:- interface.

:- import_module io, bool, list.
:- import_module hlds_module, modules, prog_data.

:- pred trans_opt__write_optfile(module_info, io__state, io__state).
:- mode trans_opt__write_optfile(in, di, uo) is det.

	% trans_opt__grab_optfiles(ModuleImports0, ModuleList, ModuleImports, 
	% 	Error, IO0, IO).
	% Add the items from each of the modules in ModuleList.trans_opt to
	% the items in ModuleImports.
:- pred trans_opt__grab_optfiles(module_imports, list(module_name), 
	module_imports, bool, io__state, io__state).
:- mode trans_opt__grab_optfiles(in, in, out, out, di, uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds_pred, mercury_to_mercury.
:- import_module prog_io, globals, code_util.
:- import_module passes_aux, prog_out, options, termination.

:- import_module string, list, map, varset, term, std_util.

%-----------------------------------------------------------------------------%

% Open the file "<module-name>.trans_opt.tmp", and write out the
% declarations.

trans_opt__write_optfile(Module) -->
	{ module_info_name(Module, ModuleName) },
	module_name_to_file_name(ModuleName, ".trans_opt.tmp", yes,
					TmpOptName),
	io__open_output(TmpOptName, Result),
	(
		{ Result = error(Error) },
		{ io__error_message(Error, Msg) },
		io__progname_base("trans_opt.m", ProgName),
		io__write_string(ProgName),
		io__write_string(
			": cannot open transitive optimisation file `"),
		io__write_string(TmpOptName),
		io__write_string("' \n"),
		io__write_string(ProgName),
		io__write_string(": for output: "),
		io__write_string(Msg),
		io__nl,
		io__set_exit_status(1)
	;
		{ Result = ok(Stream) },
		io__set_output_stream(Stream, OldStream),
		{ module_info_name(Module, ModName) },
		io__write_string(":- module "),
		mercury_output_bracketed_sym_name(ModName),
		io__write_string(".\n"),

		% All predicates to write global items into the .trans_opt 
		% file should go here.

		{ module_info_predids(Module, PredIds) },
		trans_opt__write_preds(PredIds, Module),

		io__set_output_stream(OldStream, _),
		io__close_output(Stream),

		module_name_to_file_name(ModuleName, ".trans_opt", no,
				OptName),
		update_interface(OptName),
		touch_interface_datestamp(ModuleName, ".trans_opt_date")
	).
	
:- pred trans_opt__write_preds(list(pred_id), module_info, 
	io__state, io__state).
:- mode trans_opt__write_preds(in, in, di, uo) is det.
trans_opt__write_preds([], _) --> [].
trans_opt__write_preds([ PredId | PredIds ], Module) -->
	{ module_info_preds(Module, PredTable) },
	{ map__lookup(PredTable, PredId, PredInfo) },

	( 	
		{ pred_info_is_exported(PredInfo) },
		\+ { code_util__predinfo_is_builtin(PredInfo) },
		\+ { code_util__compiler_generated(PredInfo) }
	->
		% All predicates to write predicate info into the .trans_opt 
		% file should go here.

		{ pred_info_procedures(PredInfo, ProcTable) },
		{ map__keys(ProcTable, ProcIds) },
		trans_opt__write_procs(ProcIds, PredId, PredInfo)
	;
		[]
	),
	trans_opt__write_preds(PredIds, Module).
	
:- pred trans_opt__write_procs(list(proc_id), pred_id, pred_info, 
	io__state, io__state).
:- mode trans_opt__write_procs(in, in, in, di, uo) is det.

trans_opt__write_procs([], _, _) --> [].
trans_opt__write_procs([ProcId | ProcIds], PredId, PredInfo) -->
	{ pred_info_procedures(PredInfo, ProcTable) },
	{ map__lookup(ProcTable, ProcId, ProcInfo) },
	{ pred_info_name(PredInfo, PredName) },
	{ pred_info_module(PredInfo, ModuleName) },
	{ pred_info_get_is_pred_or_func(PredInfo, PredOrFunc) },
	{ pred_info_context(PredInfo, Context) },
	{ SymName = qualified(ModuleName, PredName) },
	{ proc_info_get_maybe_arg_size_info(ProcInfo, ArgSize) },
	{ proc_info_get_maybe_termination_info(ProcInfo, Termination) },
	{ proc_info_declared_argmodes(ProcInfo, ArgModes) },

	% All predicates to write procedure items into the .trans_opt file
	% should go here.
	termination__write_pragma_termination_info(PredOrFunc, SymName,
		ArgModes, Context, ArgSize, Termination),
	
	trans_opt__write_procs(ProcIds, PredId, PredInfo).

%-----------------------------------------------------------------------------%
	% Read in and process the transitive optimization interfaces.

trans_opt__grab_optfiles(Module0, TransOptDeps, Module, FoundError) -->
	globals__io_lookup_bool_option(verbose, Verbose),
	maybe_write_string(Verbose, "% Reading .trans_opt files..\n"),
	maybe_flush_output(Verbose),

	read_trans_opt_files(TransOptDeps, [], OptItems, no, FoundError),

	{ append_pseudo_decl(Module0, opt_imported, Module1) },
	{ module_imports_get_items(Module1, Items0) },
	{ list__append(Items0, OptItems, Items) },
	{ module_imports_set_items(Module1, Items, Module2) },
	{ module_imports_set_error(Module2, no, Module) },

	maybe_write_string(Verbose, "% Done.\n").

:- pred read_trans_opt_files(list(module_name), item_list,
	item_list, bool, bool, io__state, io__state).
:- mode read_trans_opt_files(in, in, out, in, out, di, uo) is det.

read_trans_opt_files([], Items, Items, Error, Error) --> [].
read_trans_opt_files([Import | Imports],
		Items0, Items, Error0, Error) -->
	globals__io_lookup_bool_option(very_verbose, VeryVerbose),
	maybe_write_string(VeryVerbose,
		"% Reading transitive optimization interface for module"),
	maybe_write_string(VeryVerbose, " `"),
	{ prog_out__sym_name_to_string(Import, ImportString) },
	maybe_write_string(VeryVerbose, ImportString),
	maybe_write_string(VeryVerbose, "'... "),
	maybe_flush_output(VeryVerbose),

	module_name_to_file_name(Import, ".trans_opt", no, FileName),
	prog_io__read_opt_file(FileName, Import, yes,
			ModuleError, Messages, Items1),

	maybe_write_string(VeryVerbose, " done.\n"),

	update_error_status(ModuleError, Messages, Error0, Error1),
	{ list__append(Items0, Items1, Items2) },
	read_trans_opt_files(Imports, Items2, Items, Error1, Error).

:- pred update_error_status(module_error, message_list, 
	bool, bool, io__state, io__state).
:- mode update_error_status(in, in, in, out, di, uo) is det.

update_error_status(ModuleError, Messages, Error0, Error1) -->
	(
		{ ModuleError = no },
		{ Error1 = Error0 }
	;
		{ ModuleError = yes },
		prog_out__write_messages(Messages),
		{ Error1 = yes }
	;
		{ ModuleError = fatal },
		{ Error1 = yes }	
	).
