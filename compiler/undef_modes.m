%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module undef_modes.
:- interface.
:- import_module hlds, io.

:- pred check_undefined_modes(module_info, module_info, io__state, io__state).
:- mode check_undefined_modes(in, out, di, uo) is det.

:- implementation.
:- import_module prog_io, prog_out, hlds_out, list, map, std_util, require.

	% Check for any possible undefined insts/modes.
	% Should we add a definition for undefined insts/modes?

check_undefined_modes(Module, Module) -->
	{ module_info_insts(Module, InstDefns) },
	{ inst_table_get_user_insts(InstDefns, UserInstDefns) },
	{ map__keys(UserInstDefns, InstIds) },
	find_undef_inst_bodies(InstIds, UserInstDefns),
	{ module_info_modes(Module, ModeDefns) },
	{ map__keys(ModeDefns, ModeIds) },
	find_undef_mode_bodies(ModeIds, ModeDefns, UserInstDefns),
	{ module_info_preds(Module, Preds) },
	{ module_info_predids(Module, PredIds) },
	find_undef_pred_modes(PredIds, Preds, ModeDefns, UserInstDefns).

	% Find any undefined insts/modes used in predicate mode declarations.

:- pred find_undef_pred_modes(list(pred_id), pred_table, mode_table,
				user_inst_table, io__state, io__state).
:- mode find_undef_pred_modes(in, in, in, in, di, uo) is det.

find_undef_pred_modes([], _Preds, _ModeDefns, _InstDefns) --> [].
find_undef_pred_modes([PredId | PredIds], Preds, ModeDefns, InstDefns) -->
	{ map__lookup(Preds, PredId, PredDefn) },
	{ pred_info_procedures(PredDefn, Procs) },
	{ map__keys(Procs, ProcIds) },
	find_undef_proc_modes(ProcIds, PredId, Procs, ModeDefns, InstDefns),
	find_undef_pred_modes(PredIds, Preds, ModeDefns, InstDefns).

:- pred find_undef_proc_modes(list(proc_id), pred_id, proc_table, mode_table,
				user_inst_table, io__state, io__state).
:- mode find_undef_proc_modes(in, in, in, in, in, di, uo) is det.

find_undef_proc_modes([], _PredId, _Procs, _ModeDefns, _InstDefns) --> [].
find_undef_proc_modes([ProcId | ProcIds], PredId, Procs, ModeDefns,
		InstDefns) -->
	{ map__lookup(Procs, ProcId, ProcDefn) },
	{ proc_info_argmodes(ProcDefn, ArgModes) },
	{ proc_info_context(ProcDefn, Context) },
	find_undef_mode_list(ArgModes, pred(PredId) - Context, ModeDefns, 
		InstDefns),
	find_undef_proc_modes(ProcIds, PredId, Procs, ModeDefns, InstDefns).

%-----------------------------------------------------------------------------%

	% Find any undefined insts/modes used in the bodies of other mode
	% declarations.

:- pred find_undef_mode_bodies(list(mode_id), mode_table, user_inst_table,
				io__state, io__state).
:- mode find_undef_mode_bodies(in, in, in, di, uo) is det.

find_undef_mode_bodies([], _, _) --> [].
find_undef_mode_bodies([ModeId | ModeIds], ModeDefns, InstDefns) -->
	{ map__lookup(ModeDefns, ModeId, HLDS_ModeDefn) },
		% XXX abstract hlds__mode_defn/5
	{ HLDS_ModeDefn = hlds__mode_defn(_, _, Mode, _, Context) },
	find_undef_mode_body(Mode, mode(ModeId) - Context, ModeDefns,
			InstDefns),
	find_undef_mode_bodies(ModeIds, ModeDefns, InstDefns).

	% Find any undefined insts/modes used in the given mode definition.

:- pred find_undef_mode_body(hlds__mode_body, mode_error_context,
			mode_table, user_inst_table, io__state, io__state).
:- mode find_undef_mode_body(in, in, in, in, di, uo) is det.

find_undef_mode_body(eqv_mode(Mode), ErrorContext, ModeDefns, InstDefns) -->
	find_undef_mode(Mode, ErrorContext, ModeDefns, InstDefns).

	% Find any undefined modes in a list of modes.

:- pred find_undef_mode_list(list(mode), mode_error_context,
			mode_table, user_inst_table, io__state, io__state).
:- mode find_undef_mode_list(in, in, in, in, di, uo) is det.

find_undef_mode_list([], _, _, _) --> [].
find_undef_mode_list([Mode|Modes], ErrorContext, ModeDefns, InstDefns) -->
	find_undef_mode(Mode, ErrorContext, ModeDefns, InstDefns),
	find_undef_mode_list(Modes, ErrorContext, ModeDefns, InstDefns).

	% Find any undefined modes/insts used in a mode.
	% The mode itself may be undefined, and also
	% any inst arguments may also be undefined.
	% (eg. the mode `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_mode(mode, mode_error_context, mode_table, user_inst_table,
				io__state, io__state).
:- mode find_undef_mode(in, in, in, in, di, uo) is det.

find_undef_mode((InstA -> InstB), ErrorContext, _ModeDefns, InstDefns) -->
	find_undef_inst(InstA, ErrorContext, InstDefns),
	find_undef_inst(InstB, ErrorContext, InstDefns).
find_undef_mode(user_defined_mode(Name, Args), ErrorContext, ModeDefns,
		InstDefns) -->
		  %%% no builtin modes as yet
	{ list__length(Args, Arity) },
	{ ModeId = Name - Arity },
	(
		{ map__contains(ModeDefns, ModeId) }
	->
		[]
	;
		report_undef_mode(ModeId, ErrorContext)
	),
	find_undef_inst_list(Args, ErrorContext, InstDefns).

%-----------------------------------------------------------------------------%

	% Find any undefined insts used in the bodies of other inst
	% declarations.

:- pred find_undef_inst_bodies(list(inst_id), user_inst_table,
				io__state, io__state).
:- mode find_undef_inst_bodies(in, in, di, uo) is det.

find_undef_inst_bodies([], _) --> [].
find_undef_inst_bodies([InstId | InstIds], InstDefns) -->
	{ map__lookup(InstDefns, InstId, HLDS_InstDefn) },
		% XXX abstract hlds__inst_defn/5
	{ HLDS_InstDefn = hlds__inst_defn(_, _, Inst, _, Context) },
	find_undef_inst_body(Inst, inst(InstId) - Context, InstDefns),
	find_undef_inst_bodies(InstIds, InstDefns).

	% Find any undefined insts used in the given inst definition.

:- pred find_undef_inst_body(hlds__inst_body, mode_error_context,
				user_inst_table, io__state, io__state).
:- mode find_undef_inst_body(in, in, in, di, uo) is det.

find_undef_inst_body(eqv_inst(Inst), ErrorContext, InstDefns) -->
	find_undef_inst(Inst, ErrorContext, InstDefns).
find_undef_inst_body(abstract_inst, _, _) --> [].

	% Find any undefined insts in a list of insts.

:- pred find_undef_inst_list(list(inst), mode_error_context, user_inst_table,
				io__state, io__state).
:- mode find_undef_inst_list(in, in, in, di, uo) is det.

find_undef_inst_list([], _ErrorContext, _InstDefns) --> [].
find_undef_inst_list([Inst|Insts], ErrorContext, InstDefns) -->
	find_undef_inst(Inst, ErrorContext, InstDefns),
	find_undef_inst_list(Insts, ErrorContext, InstDefns).

	% Find any undefined insts used in an inst.
	% The inst itself may be undefined, and also
	% any inst arguments may also be undefined.
	% (eg. the inst `undef1(undef2, undef3)' should generate 3 errors.)

:- pred find_undef_inst(inst, mode_error_context, user_inst_table,
				io__state, io__state).
:- mode find_undef_inst(in, in, in, di, uo) is det.

find_undef_inst(free, _, _) --> [].
find_undef_inst(ground, _, _) --> [].
find_undef_inst(inst_var(_), _, _) --> [].
find_undef_inst(bound(BoundInsts), ErrorContext, InstDefns) -->
	find_undef_bound_insts(BoundInsts, ErrorContext, InstDefns).
find_undef_inst(defined_inst(InstName), ErrorContext, InstDefns) -->
	find_undef_inst_name(InstName, ErrorContext, InstDefns).
find_undef_inst(abstract_inst(Name, Args), ErrorContext, InstDefns) -->
	find_undef_inst_name(user_inst(Name, Args), ErrorContext, InstDefns).
find_undef_inst(not_reached, _, _) --> [].
find_undef_inst(free(_), _, _) --> 
	{ error("compiler generated inst unexpected") }.

:- pred find_undef_inst_name(inst_name, mode_error_context, user_inst_table,
				io__state, io__state).
:- mode find_undef_inst_name(in, in, in, di, uo) is det.

find_undef_inst_name(user_inst(Name, Args), ErrorContext, InstDefns) -->
	{ list__length(Args, Arity) },
	{ InstId = Name - Arity },
	(
		{ map__contains(InstDefns, InstId) }
	->
		[]
	;
		report_undef_inst(InstId, ErrorContext)
	),
	find_undef_inst_list(Args, ErrorContext, InstDefns).
find_undef_inst_name(unify_inst(_, _, _), _, _) -->
	{ error("compiler generated inst unexpected") }.
find_undef_inst_name(merge_inst(_, _), _, _) -->
	{ error("compiler generated inst unexpected") }.
find_undef_inst_name(ground_inst(_), _, _) -->
	{ error("compiler generated inst unexpected") }.
find_undef_inst_name(typed_ground(_), _, _) -->
	{ error("compiler generated inst unexpected") }.
find_undef_inst_name(typed_inst(_,_), _, _) -->
	{ error("compiler generated inst unexpected") }.

:- pred find_undef_bound_insts(list(bound_inst), mode_error_context,
				user_inst_table, io__state, io__state).
:- mode find_undef_bound_insts(in, in, in, di, uo) is det.

find_undef_bound_insts([], _, _) --> [].
find_undef_bound_insts([functor(_Name, Args) | BoundInsts], ErrorContext,
		InstDefns) -->
	find_undef_inst_list(Args, ErrorContext, InstDefns),
	find_undef_bound_insts(BoundInsts, ErrorContext, InstDefns).

%-----------------------------------------------------------------------------%

:- type mode_error_context == pair(mode_error_context_2, term__context).
:- type mode_error_context_2	--->	inst(inst_id)
				;	mode(mode_id)
				;	pred(pred_id).

	% Output an error message about an undefined mode
	% in the specified context.

:- pred report_undef_mode(mode_id, mode_error_context, io__state, io__state).
:- mode report_undef_mode(in, in, di, uo) is det.
report_undef_mode(ModeId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In "),
	write_mode_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: undefined mode "),
	write_mode_id(ModeId),
	io__write_string(".\n").

	% Output an error message about an undefined inst
	% in the specified context.

:- pred report_undef_inst(inst_id, mode_error_context, io__state, io__state).
:- mode report_undef_inst(in, in, di, uo) is det.

report_undef_inst(InstId, ErrorContext - Context) -->
	prog_out__write_context(Context),
	io__write_string("In "),
	write_mode_error_context(ErrorContext),
	io__write_string(":\n"),
	prog_out__write_context(Context),
	io__write_string("  error: undefined inst "),
	write_inst_id(InstId),
	io__write_string(".\n").

	% Output a description of the context where an undefined mode was
	% used.

:- pred write_mode_error_context(mode_error_context_2, io__state, io__state).
:- mode write_mode_error_context(in, di, uo) is det.

write_mode_error_context(pred(_PredId)) -->
	io__write_string("mode declaration for predicate").
	% XXX hlds_out__write_pred_id(PredId).
write_mode_error_context(mode(ModeId)) -->
	io__write_string("definition of mode "),
	write_mode_id(ModeId).
write_mode_error_context(inst(InstId)) -->
	io__write_string("definition of inst "),
	write_inst_id(InstId).

%-----------------------------------------------------------------------------%

	% Predicates to output inst_ids and mode_ids.
	% XXX inst_ids and mode_ids should include the module.

:- pred write_mode_id(mode_id, io__state, io__state).
:- mode write_mode_id(in, di, uo) is det.

write_mode_id(F - N) -->
	prog_out__write_sym_name(F),
	io__write_string("/"),
	io__write_int(N).

	% XXX inst_ids should include the module.

:- pred write_inst_id(inst_id, io__state, io__state).
:- mode write_inst_id(in, di, uo) is det.

write_inst_id(F - N) -->
	prog_out__write_sym_name(F),
	io__write_string("/"),
	io__write_int(N).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
