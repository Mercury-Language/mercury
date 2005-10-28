%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%------------------------------------------------------------------------------%
% Copyright (C) 1997-2003, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%------------------------------------------------------------------------------%
%
% File: term_constr_util.m
% Main author: juliensf
%
% This module defines some utility predicates used by the termination analyser.
%
%------------------------------------------------------------------------------%

:- module transform_hlds.term_constr_util.

:- interface.

:- import_module hlds.hlds_pred. 
:- import_module hlds.hlds_module.
:- import_module libs.lp_rational.
:- import_module libs.polyhedron.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_main.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module std_util.

%------------------------------------------------------------------------------%
%
% Predicates for storing things in the HLDS.
%

	% This predicate sets the argument size info (in terms of constraints
	% on inter-argument size relationships) of a given list of procedures.
	%
:- pred set_pred_proc_ids_constr_arg_size_info(list(pred_proc_id)::in,
	constr_arg_size_info::in, module_info::in, module_info::out) is det.

:- func lookup_proc_constr_arg_size_info(module_info, pred_proc_id) = 
	maybe(constr_arg_size_info).

	% Retrieve the abstraction representation from the module_info.
	%
:- func get_abstract_scc(module_info, list(pred_proc_id)) = abstract_scc.

:- func get_abstract_proc(module_info, pred_proc_id) = abstract_proc.

%------------------------------------------------------------------------------%
%
% Predicates for size_vars. 
%

	% Given a list of prog_vars, allocate one size_var per prog_var.
	% Return the varset from which the size_vars were allocated and
	% a map between prog_vars and size_vars.
	%
:- pred make_size_var_map(list(prog_var)::in, size_varset::out,
	size_var_map::out) is det.

	% Given a list of prog_vars, allocate one size_var per prog_var.
	% Allocate the size_vars from the provided size_varset.  Return
	% a map between prog_vars and size_vars.
	%
:- pred make_size_var_map(list(prog_var)::in,
	size_varset::in, size_varset::out, size_var_map::out) is det.

	% Takes a list of prog_vars and outputs the corresponding
	% list of size_vars, based on the given map.
	%
:- func prog_vars_to_size_vars(size_var_map, prog_vars) = size_vars.
:- func prog_var_to_size_var(size_var_map, prog_var) = size_var.

	% Returns a set containing all the size_vars corresponding to prog_vars
	% that have a type that is always of zero size. i.e. all those for which
	% the functor norm returns zero for all values of the type.
	% 
:- func find_zero_size_vars(module_info, size_var_map, vartypes) = zero_vars.

	% create_nonneg_constraints(SizeVarMap, Zeros) = Constraints.
	%
	% Returns a list of constraints of the form "x >= 0" for every size_var
	% x that is is in `SizeVarMap' and is not in the set `Zeros'.
	%
:- func create_nonneg_constraints(size_var_map, zero_vars) = constraints.

:- type var_substitution == map(size_var, size_var).

    % create_var_substition(FromVars, ToVars) = Substitution.
    % Create a mapping that maps elements of `FromVars' to their
    % corresponding elements in `ToVars'.  This mapping is many-one.
    % An exception is thrown if `FromVars' contains any duplicate elements.
    %
:- func create_var_substitution(size_vars, size_vars) = var_substitution.

	% Create a non-negativity constraint for each size_var in the list,
	% *except* if it has zero size type.
	%
:- func make_arg_constraints(size_vars, zero_vars) = constraints.

    % Check that a size_var is a member of the set of zero size_vars.
    % XXX Ideally we would just use set.member directly but the arguments
    % of that procedure are around the wrong way for use in most higher
    % order procedures.
    %
:- pred is_zero_size_var(zero_vars::in, size_var::in) is semidet.

%-----------------------------------------------------------------------------%

:- pred add_context_to_constr_termination_info(
	maybe(pragma_termination_info)::in, prog_context::in,	
	maybe(constr_termination_info)::out) is det.

%------------------------------------------------------------------------------%

	% substitute_size_vars: Takes a list of constraints and a
	% var_substitution.  Returns the constraints with the specified
	% substitutions made.
	%
:- func substitute_size_vars(constraints, map(size_var, size_var)) 
	= constraints.

%------------------------------------------------------------------------------%
%
% Predicates for printing out debugging traces.
%
% In order to use these the option `--debug-term' must be set.

	% Call the specified predicate.
	% 
:- pred maybe_write_trace(pred(io, io), io, io).
:- mode maybe_write_trace(pred(di, uo) is det, di, uo) is det.

	% As above but if the boolean argument is `yes', print a newline
	% to stdout before flushing the output.
	% 
:- pred maybe_write_trace(pred(io, io), bool, io, io).
:- mode maybe_write_trace(pred(di, uo) is det, in, di, uo) is det.

	% Write the given string to stdout.
	% 
:- pred maybe_write_string(string::in, io::di, io::uo) is det.

	% Write a newline to stdout.
	% 
:- pred maybe_write_nl(io::di, io::uo) is det.

:- pred maybe_write_scc_procs(list(pred_proc_id)::in, module_info::in,
	int::in, io::di, io::uo) is det.

:- pred maybe_write_proc_name(pred_proc_id::in, string::in, module_info::in,
	int::in, io::di, io::uo) is det.

:- pred write_size_vars(size_varset::in, size_vars::in, io::di, io::uo) is det.

:- pred dump_size_varset(size_varset::in, io::di, io::uo) is det.

:- pred dump_size_vars(size_vars::in, size_varset::in, io::di, io::uo) is det.

%------------------------------------------------------------------------------%

:- pred update_arg_size_info(pred_proc_id::in, polyhedron::in, module_info::in,
	module_info::out) is det.

    % change_procs_constr_termination_info(SCC, Override, TermInfo,
    %   !ProcTable).
	%
    % If Override is yes, then this predicate overrides any existing
	% termination information. If Override is no, then it leaves the
	% proc_info of a procedure unchanged unless the proc_info had no
	% termination information (i.e. the maybe(termination_info)
	% field was set to "no").
	%
:- pred change_procs_constr_termination_info(list(proc_id)::in, bool::in,
	constr_termination_info::in, proc_table::in, proc_table::out) is det.

    % change_procs_constr_arg_size_info(SCC, Override, ArgSizeInfo,
    %   !ProcTable).
    %
	% This predicate sets the arg_size_info property of the given
	% list of procedures.  If Override is yes, then this predicate
	% overrides any existing arg_size information. If Override is
	% no, then it leaves the proc_info of a procedure unchanged
	% unless the proc_info had no arg_size information (i.e. the
	% maybe(arg_size_info) field was set to "no").
	%
:- pred change_procs_constr_arg_size_info(list(proc_id)::in, bool::in,
	constr_arg_size_info::in, proc_table::in, proc_table::out) is det.

:- pred all_args_input_or_zero_size(module_info::in, pred_info::in,
	proc_info::in) is semidet.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal. 
:- import_module hlds.hlds_module. 
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred. 
:- import_module hlds.quantification. 
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.rat.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_type. 
:- import_module transform_hlds.term_constr_errors.
:- import_module transform_hlds.term_norm.

:- import_module int.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%------------------------------------------------------------------------------%

set_pred_proc_ids_constr_arg_size_info([], _ArgSize, !ModuleInfo).
set_pred_proc_ids_constr_arg_size_info([PPId | PPIds], ArgSize, !ModuleInfo) :-
	PPId = proc(PredId, ProcId),
	module_info_preds(!.ModuleInfo, PredTable0),
	PredInfo0 = PredTable0 ^ det_elem(PredId),
	pred_info_procedures(PredInfo0, ProcTable0),
	ProcInfo0 = ProcTable0 ^ det_elem(ProcId),
	proc_info_get_termination2_info(ProcInfo0, TermInfo0),
	TermInfo = TermInfo0 ^ success_constrs := yes(ArgSize),
	proc_info_set_termination2_info(TermInfo, ProcInfo0, ProcInfo),
	ProcTable = ProcTable0 ^ det_elem(ProcId) := ProcInfo,
	pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
	PredTable = PredTable0 ^ det_elem(PredId) := PredInfo,
	module_info_set_preds(PredTable, !ModuleInfo),
	set_pred_proc_ids_constr_arg_size_info(PPIds, ArgSize, !ModuleInfo).

lookup_proc_constr_arg_size_info(ModuleInfo, PredProcId) = MaybeArgSizeInfo :-
	PredProcId = proc(PredId, ProcId),
	module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
	proc_info_get_termination2_info(ProcInfo, TermInfo),
	MaybeArgSizeInfo = TermInfo ^ success_constrs.

%------------------------------------------------------------------------------%

make_size_var_map(ProgVars, SizeVarset, SizeVarMap) :-
	make_size_var_map(ProgVars, varset.init, SizeVarset, SizeVarMap).

make_size_var_map(ProgVars, !SizeVarset, SizeVarMap) :-
	list.foldl2(make_size_var_map_2, ProgVars, 
		map.init, SizeVarMap, !SizeVarset).

:- pred make_size_var_map_2(prog_var::in, size_var_map::in, size_var_map::out,
	size_varset::in, size_varset::out) is det.

make_size_var_map_2(ProgVar, !SizeVarMap, !SizeVarset) :-
	svvarset.new_var(SizeVar, !SizeVarset),
	svmap.set(ProgVar, SizeVar, !SizeVarMap).

prog_vars_to_size_vars(SizeVarMap, Vars)
	= list.map(prog_var_to_size_var(SizeVarMap), Vars).

prog_var_to_size_var(SizeVarMap, Var) = SizeVar :-
	( if 	map.search(SizeVarMap, Var, SizeVar0) 
	  then	SizeVar = SizeVar0
	  else	unexpected(this_file,
		    "prog_var_to_size_var/2: prog_var not in size_var_map.")
	).

find_zero_size_vars(ModuleInfo, SizeVarMap, VarTypes) = Zeros :-
	ProgVars = map.keys(SizeVarMap),
	ZeroProgVars = list.filter(is_zero_size_prog_var(ModuleInfo, VarTypes),
		ProgVars),
    %
    % Build zeros from corresponding size_vars.
    %   
    ZerosList = prog_vars_to_size_vars(SizeVarMap, ZeroProgVars),
	Zeros = set.from_list(ZerosList).

:- pred is_zero_size_prog_var(module_info::in, vartypes::in,
	prog_var::in) is semidet.

is_zero_size_prog_var(ModuleInfo, VarTypes, Var) :-
	Type = VarTypes ^ det_elem(Var),
	(
		term_norm.zero_size_type(Type, ModuleInfo)
	;
		% We don't include dummy types in the constraints - they won't tell us
        % anything useful.
		is_dummy_argument_type(ModuleInfo, Type)
	).

add_context_to_constr_termination_info(no, _, no).
add_context_to_constr_termination_info(yes(cannot_loop(_)), _, 
		yes(cannot_loop(import_supplied))).
add_context_to_constr_termination_info(yes(can_loop(_)), Context,
		yes(can_loop([Context - imported_pred]))).

%------------------------------------------------------------------------------%

substitute_size_vars(Constraints0, SubstMap) = Constraints :-
	SubVarInCoeff = (func(OldVar - Rat) = NewVar - Rat :-
		NewVar = SubstMap ^ det_elem(OldVar)
	),
	SubVarInEqn = (func(Constr) = constraint(Coeffs, Op, Rat) :-
		constraint(Constr, Coeffs0, Op, Rat),
		Coeffs = list.map(SubVarInCoeff, Coeffs0)
	),
	Constraints = list.map(SubVarInEqn, Constraints0).

%------------------------------------------------------------------------------%
%
% Utility procedures used by various parts of the IR analysis.
%

create_nonneg_constraints(SizeVarMap, Zeros) = Constraints :-
	create_nonneg_constraints_2(SizeVarMap, Zeros, Constraints).

:- pred create_nonneg_constraints_2(size_var_map::in, zero_vars::in,
	constraints::out) is det.

create_nonneg_constraints_2(SizeVarMap, Zeros, NonNegs) :-
	SizeVars = map.values(SizeVarMap),
    list.filter(isnt(is_zero_size_var(Zeros)), SizeVars, NonZeroSizeVars),
	NonNegs = list.map(make_nonneg_constr, NonZeroSizeVars).

create_var_substitution(Args, HeadVars) = SubstMap :-
	create_var_substitution_2(Args, HeadVars, map.init, SubstMap).

:- pred create_var_substitution_2(size_vars::in, size_vars::in, 
	var_substitution::in, var_substitution::out) is det.

create_var_substitution_2([], [], !Subst).
create_var_substitution_2([_|_], [], _, _) :- 
	unexpected(this_file, "compose_bijections/5: unmatched lists.").
create_var_substitution_2([], [_|_], _, _) :- 
	unexpected(this_file, "compose_bijections/5: unmatched lists."). 
create_var_substitution_2([Arg | Args], [HeadVar | HeadVars],  !Subst) :-
	svmap.det_insert(HeadVar, Arg, !Subst),
	create_var_substitution_2(Args, HeadVars, !Subst). 

make_arg_constraints([], _) = [].
make_arg_constraints([Var | Vars], Zeros) = Constraints :-
 	Constraints0 = make_arg_constraints(Vars, Zeros),
 	( if 	set.member(Var, Zeros)
   	  then	Constraints = Constraints0
   	  else	Constraints =
                [ constraint([Var - one], (>=), zero) | Constraints0 ]
 	).

is_zero_size_var(Zeros, SizeVar) :- set.member(SizeVar, Zeros).

%------------------------------------------------------------------------------%
%
% Predicates for printing out debugging traces ...
%

maybe_write_trace(TracePred, !IO) :-
	maybe_write_trace(TracePred, no, !IO).

maybe_write_trace(TracePred, NewLine, !IO) :-
	globals.io_lookup_bool_option(debug_term, Trace, !IO),
	(
		Trace = yes,
		TracePred(!IO),
		( if NewLine = yes then	io.nl(!IO) else true ),
		io.flush_output(!IO)
	;
		Trace = no
	).

maybe_write_nl(!IO) :- maybe_write_string("\n", !IO).

maybe_write_string(String, !IO) :-
	globals.io_lookup_bool_option(debug_term, Debug, !IO),
	(
		Debug = yes,
		io.write_string(String, !IO),
		io.flush_output(!IO)
	;
		Debug = no
	).

maybe_write_scc_procs(SCC, ModuleInfo, _, !IO) :- 
	write_scc_procs_2(SCC, ModuleInfo, !IO),
	io.nl(!IO).

:- pred write_scc_procs_2(list(pred_proc_id)::in, module_info::in,
    io::di, io::uo) is det.

write_scc_procs_2([], _, !IO).
write_scc_procs_2([PPId | PPIds], ModuleInfo, !IO) :- 
	io.write_char('\t', !IO),
	hlds_out.write_pred_proc_id(ModuleInfo, PPId, !IO),
	io.nl(!IO),
	write_scc_procs_2(PPIds, ModuleInfo, !IO).

maybe_write_proc_name(PPId, String, ModuleInfo, _, !IO) :- 
	io.write_string(String, !IO),
	hlds_out.write_pred_proc_id(ModuleInfo, PPId, !IO),
	io.nl(!IO).

write_size_vars(Varset, Vars, !IO) :-
	WriteSizeVar = (pred(Var::in, !.IO::di, !:IO::uo) is det :-
		varset.lookup_name(Varset, Var, Name),
		io.write_string(Name, !IO)
	),
	io.write_char('[', !IO),
	io.write_list(Vars, ", ", WriteSizeVar, !IO),
	io.write_char(']', !IO).

%------------------------------------------------------------------------------%

dump_size_vars(Vars, Varset, !IO) :-
	dump_size_varset_2(Vars, Varset, !IO).

dump_size_varset(Varset, !IO) :- 
	Vars = varset.vars(Varset),
	dump_size_varset_2(Vars, Varset, !IO).

:- pred dump_size_varset_2(size_vars::in, size_varset::in, io::di, io::uo) 
	is det.

dump_size_varset_2([], _, !IO).
dump_size_varset_2([Var | Vars], Varset, !IO) :-
	Name = varset.lookup_name(Varset, Var),
	io.write(Var, !IO),
	io.format(" = %s\n", [s(Name)], !IO),
	dump_size_varset_2(Vars, Varset, !IO).

%------------------------------------------------------------------------------%

update_arg_size_info(PPID, Polyhedron, !ModuleInfo) :-
	set_pred_proc_ids_constr_arg_size_info([PPID], Polyhedron,
		!ModuleInfo). 

%------------------------------------------------------------------------------%

change_procs_constr_termination_info([], _, _, !ProcTable).
change_procs_constr_termination_info([ProcId | ProcIds], Override, Termination,
		!ProcTable) :-
	ProcInfo0 = !.ProcTable ^ det_elem(ProcId),
	proc_info_get_termination2_info(ProcInfo0, TermInfo0),
	( (Override = yes ; TermInfo0 ^ term_status = no) ->
		TermInfo = TermInfo0 ^ term_status := yes(Termination),
		proc_info_set_termination2_info(TermInfo, ProcInfo0, ProcInfo),
		svmap.det_update(ProcId, ProcInfo, !ProcTable)
	;
		true
	),
	change_procs_constr_termination_info(ProcIds, Override, Termination,
		!ProcTable).

change_procs_constr_arg_size_info([], _, _, !ProcTable).
change_procs_constr_arg_size_info([ProcId | ProcIds], Override, ArgSize,
		!ProcTable) :-
	ProcInfo0 = !.ProcTable ^ det_elem(ProcId),
	proc_info_get_termination2_info(ProcInfo0, TermInfo0),
	( (Override = yes ; TermInfo0 ^ success_constrs = no) ->
		TermInfo = TermInfo0 ^ success_constrs := yes(ArgSize),
		proc_info_set_termination2_info(TermInfo, ProcInfo0, ProcInfo),
		svmap.det_update(ProcId, ProcInfo, !ProcTable)
	;
		true
	),
	change_procs_constr_arg_size_info(ProcIds, Override, ArgSize, 
		!ProcTable).

all_args_input_or_zero_size(ModuleInfo, PredInfo, ProcInfo) :-
	pred_info_arg_types(PredInfo, TypeList),
	proc_info_argmodes(ProcInfo, ModeList),
	all_args_input_or_zero_size_2(TypeList, ModeList, ModuleInfo). 

:- pred all_args_input_or_zero_size_2(list(mer_type)::in, list(mer_mode)::in,
	module_info::in) is semidet.

all_args_input_or_zero_size_2([], [], _).
all_args_input_or_zero_size_2([], [_|_], _) :- 
	unexpected(this_file, 
		"all_args_input_or_zero_size_2/3: unmatched lists.").
all_args_input_or_zero_size_2([_|_], [], _) :- 
	unexpected(this_file, 
		"all_args_input_or_zero_size_2/3: unmatched lists.").
all_args_input_or_zero_size_2([Type | Types], [Mode | Modes], ModuleInfo) :-
	( mode_util.mode_is_input(ModuleInfo, Mode) ->
		all_args_input_or_zero_size_2(Types, Modes, ModuleInfo)
	;
		term_norm.zero_size_type(Type, ModuleInfo),
		all_args_input_or_zero_size_2(Types, Modes, ModuleInfo)
	).

%----------------------------------------------------------------------------%

get_abstract_scc(ModuleInfo, SCC) =
	list.map(get_abstract_proc(ModuleInfo), SCC).

get_abstract_proc(ModuleInfo, PPId) = AbstractProc :-
	module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
	proc_info_get_termination2_info(ProcInfo, TermInfo),
	MaybeAbstractProc = TermInfo ^ abstract_rep,
	(
		MaybeAbstractProc = yes(AbstractProc)
	;
		MaybeAbstractProc = no,
		unexpected(this_file,
			"get_abstract_proc/2: no abstract rep. for proc.")
	).

%------------------------------------------------------------------------------%

:- func this_file = string.

this_file = "term_constr_util.m".

%------------------------------------------------------------------------------%
:- end_module term_constr_util.
%------------------------------------------------------------------------------%
