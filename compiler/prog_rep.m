%---------------------------------------------------------------------------%
% Copyright (C) 2000-2005 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module generates a representation of HLDS goals for the declarative
% debugger. Since this representation is to be included in debuggable
% executables, it should be as compact as possible, and therefore contains
% only the information required by the declarative debugger. The structure
% of this representation is defined by mdbcomp/program_representation.m.
%
% Author: zs.
%
%---------------------------------------------------------------------------%

:- module ll_backend__prog_rep.

:- interface.

:- import_module hlds__hlds_goal.
:- import_module hlds__hlds_module.
:- import_module hlds__hlds_pred.
:- import_module hlds__instmap.
:- import_module parse_tree__prog_data.

:- import_module list.
:- import_module map.
:- import_module std_util.

% A var_num_map maps each variable that occurs in any of a procedure's layout
% structures to a number that uniquely identifies that variable, and to its
% name.
%
% The integer returned by term__var_to_int are a dense set when we consider
% all the original variables of a procedure. However, it can become less dense
% when an optimization removes all references to a variable, and becomes less
% dense still when we consider only variables that occur in a layout structure.
% This is why we allocate our own id numbers.

:- type var_num_map	== map(prog_var, pair(int, string)).

:- func prog_rep__represent_proc(list(prog_var), hlds_goal, instmap, vartypes,
	var_num_map, module_info) = list(int).

:- implementation.

:- import_module backend_libs__bytecode_data.
:- import_module hlds__code_model.
:- import_module hlds__hlds_data.
:- import_module mdbcomp__prim_data.
:- import_module parse_tree__error_util.
:- import_module parse_tree__prog_out.
:- import_module parse_tree__prog_util.

:- import_module mdbcomp.
:- import_module mdbcomp__program_representation.

:- import_module int.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.

:- type prog_rep__info
	--->	info(
			filename	:: string,
			vartypes	:: vartypes,
			var_num_map	:: var_num_map,
			module_info	:: module_info
		).

prog_rep__represent_proc(HeadVars, Goal, InstMap0, VarTypes, VarNumMap,
		ModuleInfo) = ProcRepBytes :-
	Goal = _ - GoalInfo,
	goal_info_get_context(GoalInfo, Context),
	term__context_file(Context, FileName),
	Info = info(FileName, VarTypes, VarNumMap, ModuleInfo),

	ProcRepBytes0 = string_to_byte_list(FileName) ++
		vars_to_byte_list(Info, HeadVars) ++
		goal_to_byte_list(Goal, InstMap0, Info),
	int32_to_byte_list(list__length(ProcRepBytes0) + 4, LimitBytes),
	ProcRepBytes = LimitBytes ++ ProcRepBytes0.

%---------------------------------------------------------------------------%

:- func goal_to_byte_list(hlds_goal, instmap, prog_rep__info) = list(int).

goal_to_byte_list(GoalExpr - GoalInfo, InstMap0, Info) =
	goal_expr_to_byte_list(GoalExpr, GoalInfo, InstMap0, Info).

:- func goal_expr_to_byte_list(hlds_goal_expr, hlds_goal_info, instmap,
	prog_rep__info) = list(int).

goal_expr_to_byte_list(conj(Goals), _, InstMap0, Info) = Bytes :-
	Bytes = [goal_type_to_byte(goal_conj)] ++
		length_to_byte_list(Goals) ++
		conj_to_byte_list(Goals, InstMap0, Info).
goal_expr_to_byte_list(par_conj(_), _, _, _) = _ :-
	sorry("prog_rep", "parallel conjunctions and declarative debugging").
goal_expr_to_byte_list(disj(Goals), _, InstMap0, Info) = Bytes :-
	Bytes = [goal_type_to_byte(goal_disj)] ++
		length_to_byte_list(Goals) ++
		disj_to_byte_list(Goals, InstMap0, Info).
goal_expr_to_byte_list(not(Goal), _GoalInfo, InstMap0, Info) = Bytes :-
	Bytes = [goal_type_to_byte(goal_neg)] ++
		goal_to_byte_list(Goal, InstMap0, Info).
goal_expr_to_byte_list(if_then_else(_, Cond, Then, Else), _, InstMap0, Info)
		= Bytes :-
	Cond = _ - CondGoalInfo,
	goal_info_get_instmap_delta(CondGoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	Bytes = [goal_type_to_byte(goal_ite)] ++
		goal_to_byte_list(Cond, InstMap0, Info) ++
		goal_to_byte_list(Then, InstMap1, Info) ++
		goal_to_byte_list(Else, InstMap0, Info).
goal_expr_to_byte_list(unify(_, _, _, Uni, _), GoalInfo, InstMap0, Info)
		= Bytes :-
	AtomicBytes = atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info),
	(
		Uni = assign(Target, Source),
		Bytes = [goal_type_to_byte(goal_assign)] ++
			var_to_byte_list(Info, Target) ++
			var_to_byte_list(Info, Source) ++
			AtomicBytes
	;
		Uni = construct(Var, ConsId, Args, _, _, _, _),
		Bytes = [goal_type_to_byte(goal_construct)] ++
			var_to_byte_list(Info, Var) ++
			cons_id_to_byte_list(ConsId) ++
			vars_to_byte_list(Info, Args) ++
			AtomicBytes
	;
		Uni = deconstruct(Var, ConsId, Args, _, _, _),
		Bytes = [goal_type_to_byte(goal_deconstruct)] ++
			var_to_byte_list(Info, Var) ++
			cons_id_to_byte_list(ConsId) ++
			vars_to_byte_list(Info, Args) ++
			AtomicBytes
	;
		Uni = simple_test(Var1, Var2),
		Bytes = [goal_type_to_byte(goal_simple_test)] ++
			var_to_byte_list(Info, Var1) ++
			var_to_byte_list(Info, Var2) ++
			AtomicBytes
	;
		Uni = complicated_unify(_, _, _),
		error("goal_expr_to_byte_list: complicated_unify")
	).
goal_expr_to_byte_list(switch(_, _, Cases), _, InstMap0, Info) = Bytes :-
	Bytes = [goal_type_to_byte(goal_switch)] ++
		length_to_byte_list(Cases) ++
		cases_to_byte_list(Cases, InstMap0, Info).
goal_expr_to_byte_list(scope(_, Goal), GoalInfo, InstMap0, Info) = Bytes :-
	Goal = _ - InnerGoalInfo,
	goal_info_get_determinism(GoalInfo, OuterDetism),
	goal_info_get_determinism(InnerGoalInfo, InnerDetism),
	( InnerDetism = OuterDetism ->
		MaybeCut = 0
	;
		MaybeCut = 1
	),
	Bytes = [goal_type_to_byte(goal_scope)] ++
		[MaybeCut] ++
		goal_to_byte_list(Goal, InstMap0, Info).
goal_expr_to_byte_list(generic_call(GenericCall, Args, _, _),
		GoalInfo, InstMap0, Info) = Bytes :-
	AtomicBytes = atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info),
	(
		GenericCall = higher_order(PredVar, _, _, _),
		Bytes = [goal_type_to_byte(goal_ho_call)] ++
			var_to_byte_list(Info, PredVar) ++
			vars_to_byte_list(Info, Args) ++
			AtomicBytes
	;
		GenericCall = class_method(Var, MethodNum, _, _),
		Bytes = [goal_type_to_byte(goal_method_call)] ++
			var_to_byte_list(Info, Var) ++
			method_num_to_byte_list(MethodNum) ++
			vars_to_byte_list(Info, Args) ++
			AtomicBytes
	;
		GenericCall = unsafe_cast,
		( Args = [InputArg, OutputArg] ->
			Bytes = [goal_type_to_byte(goal_unsafe_cast)] ++
				var_to_byte_list(Info, OutputArg) ++
				var_to_byte_list(Info, InputArg) ++
				AtomicBytes
		;
			error("goal_expr_to_byte_list: unsafe_cast arity != 2")
		)
	;
		GenericCall = aditi_builtin(_, _),
		error("Sorry, not yet implemented\n\
			Aditi and declarative debugging")
	).
goal_expr_to_byte_list(call(PredId, _, Args, Builtin, _, _),
		GoalInfo, InstMap0, Info) = Bytes :-
	AtomicBytes = atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info),
	module_info_pred_info(Info ^ module_info, PredId, PredInfo),
	ModuleSymName = pred_info_module(PredInfo),
	mdbcomp__prim_data__sym_name_to_string(ModuleSymName, ModuleName),
	PredName = pred_info_name(PredInfo),
	( Builtin = not_builtin ->
		Bytes = [goal_type_to_byte(goal_plain_call)] ++
			string_to_byte_list(ModuleName) ++
			string_to_byte_list(PredName) ++
			vars_to_byte_list(Info, Args) ++
			AtomicBytes
	;
		Bytes = [goal_type_to_byte(goal_builtin_call)] ++
			string_to_byte_list(ModuleName) ++
			string_to_byte_list(PredName) ++
			vars_to_byte_list(Info, Args) ++
			AtomicBytes
	).
goal_expr_to_byte_list(foreign_proc(_, _PredId, _, Args, _, _),
		GoalInfo, InstMap0, Info) = Bytes :-
	ArgVars = list__map(foreign_arg_var, Args),
	Bytes = [goal_type_to_byte(goal_foreign)] ++
		vars_to_byte_list(Info, ArgVars) ++
		atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info).
goal_expr_to_byte_list(shorthand(_), _, _, _) = _ :-
	% these should have been expanded out by now
	error("goal_expr_to_byte_list: unexpected shorthand").

%---------------------------------------------------------------------------%

:- func atomic_goal_info_to_byte_list(hlds_goal_info, instmap, prog_rep__info)
	= list(int).

atomic_goal_info_to_byte_list(GoalInfo, InstMap0, Info) = Bytes :-
	goal_info_get_determinism(GoalInfo, Detism),
	goal_info_get_context(GoalInfo, Context),
	term__context_file(Context, FileName0),
	( FileName0 = Info ^ filename ->
		FileName = ""
	;
		FileName = FileName0
	),
	term__context_line(Context, LineNo),
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap),
	instmap_changed_vars(InstMap0, InstMap, Info ^ vartypes,
		Info ^ module_info, ChangedVars),
	set__to_sorted_list(ChangedVars, ChangedVarList),
	Bytes = [represent_determinism(Detism)] ++
		string_to_byte_list(FileName) ++
		lineno_to_byte_list(LineNo) ++
		vars_to_byte_list(Info, ChangedVarList).

:- func cons_id_to_byte_list(cons_id) = list(int).

cons_id_to_byte_list(SymName) =
	string_to_byte_list(cons_id_to_string(SymName)).

:- func cons_id_to_string(cons_id) = string.

cons_id_to_string(cons(SymName, _)) =
	prog_rep__sym_base_name_to_string(SymName).
cons_id_to_string(int_const(Int)) =
	string__int_to_string(Int).
cons_id_to_string(float_const(Float)) =
	string__float_to_string(Float).
cons_id_to_string(string_const(String)) =
	string__append_list(["""", String, """"]).
cons_id_to_string(pred_const(_, _)) = "$pred_const".
cons_id_to_string(type_ctor_info_const(_, _, _)) =
	"$type_ctor_info_const".
cons_id_to_string(base_typeclass_info_const(_, _, _, _)) =
	"$base_typeclass_info_const".
cons_id_to_string(type_info_cell_constructor(_)) =
	"$type_info_cell_constructor".
cons_id_to_string(typeclass_info_cell_constructor) =
	"$typeclass_info_cell_constructor".
cons_id_to_string(tabling_pointer_const(_)) =
	"$tabling_pointer_const".
cons_id_to_string(deep_profiling_proc_layout(_)) =
	"$deep_profiling_procedure_data".
cons_id_to_string(table_io_decl(_)) =
	"$table_io_decl".

:- func sym_base_name_to_string(sym_name) = string.

sym_base_name_to_string(unqualified(String)) = String.
sym_base_name_to_string(qualified(_, String)) = String.

%---------------------------------------------------------------------------%

:- func conj_to_byte_list(hlds_goals, instmap, prog_rep__info) = list(int).

conj_to_byte_list([], _, _) = [].
conj_to_byte_list([Goal | Goals], InstMap0, Info) = Bytes :-
	GoalBytes = goal_to_byte_list(Goal, InstMap0, Info),
	Goal = _ - GoalInfo,
	goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
	instmap__apply_instmap_delta(InstMap0, InstMapDelta, InstMap1),
	GoalsBytes = conj_to_byte_list(Goals, InstMap1, Info),
	Bytes = GoalBytes ++ GoalsBytes.

%---------------------------------------------------------------------------%

:- func disj_to_byte_list(hlds_goals, instmap, prog_rep__info) = list(int).

disj_to_byte_list([], _, _) = [].
disj_to_byte_list([Goal | Goals], InstMap0, Info) = Bytes :-
	GoalBytes = goal_to_byte_list(Goal, InstMap0, Info),
	GoalsBytes = disj_to_byte_list(Goals, InstMap0, Info),
	Bytes = GoalBytes ++ GoalsBytes.

%---------------------------------------------------------------------------%

:- func cases_to_byte_list(list(case), instmap, prog_rep__info) = list(int).

cases_to_byte_list([], _, _) = [].
cases_to_byte_list([case(_ConsId, Goal) | Cases], InstMap0, Info) = Bytes :-
	GoalBytes = goal_to_byte_list(Goal, InstMap0, Info),
	GoalsBytes = cases_to_byte_list(Cases, InstMap0, Info),
	% XXX
	% Bytes = cons_id_and_arity_to_byte_list(ConsId)
	%	++ GoalBytes ++ GoalsBytes.
	Bytes = GoalBytes ++ GoalsBytes.

%---------------------------------------------------------------------------%

% The operations to convert primitive constructs to bytecode.
%
% We use the operations defined in bytecode_data. Each of the functions below
% stands for a given primitive construct. If we need to expand the number of
% bytes we use to represent one of these, it should be sufficient to change
% the number of bits here and in mdbcomp/program_representation.m.
%
% Warning: the predicates we use from bytecode_data deal with signed integers,
% but we here use them to represent unsigned quantities. This effectively
% halves their range.

:- func string_to_byte_list(string) = list(int).

string_to_byte_list(String) = Bytes :-
	string_to_byte_list(String, Bytes).

:- func vars_to_byte_list(prog_rep__info, list(prog_var)) = list(int).

vars_to_byte_list(Info, Vars) =
	length_to_byte_list(Vars) ++
	list__condense(list__map(var_to_byte_list(Info), Vars)).

:- func var_to_byte_list(prog_rep__info, prog_var) = list(int).

var_to_byte_list(Info, Var) = Bytes :-
	map__lookup(Info ^ var_num_map, Var, VarNum - _),
	short_to_byte_list(VarNum, Bytes).

:- func length_to_byte_list(list(T)) = list(int).

length_to_byte_list(List) = Bytes :-
	short_to_byte_list(list__length(List), Bytes).

:- func lineno_to_byte_list(int) = list(int).

lineno_to_byte_list(VarNum) = Bytes :-
	short_to_byte_list(VarNum, Bytes).

:- func method_num_to_byte_list(int) = list(int).

method_num_to_byte_list(VarNum) = Bytes :-
	short_to_byte_list(VarNum, Bytes).

%---------------------------------------------------------------------------%
