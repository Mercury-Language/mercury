%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% file: code_util.m.
%
% various utilities routines for code generation and recognition
% of builtins.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module ll_backend__code_util.

:- interface.

:- import_module parse_tree__prog_data, hlds__hlds_module, hlds__hlds_pred.
:- import_module hlds__hlds_goal, hlds__hlds_data.
:- import_module backend_libs__rtti, ll_backend__llds.

:- import_module bool, list, std_util.

	% Create a code address which holds the address of the specified
	% procedure.
	% The `immed' argument should be `no' if the the caller wants the
	% returned address to be valid from everywhere in the program.
	% If being valid from within the current procedure is enough,
	% this argument should be `yes' wrapped around the value of the
	% --procs-per-c-function option and the current procedure id.
	% Using an address that is only valid from within the current
	% procedure may make jumps more efficient.

:- type immed == maybe(pair(int, pred_proc_id)).

:- pred code_util__make_entry_label(module_info, pred_id, proc_id, 
		immed, code_addr).
:- mode code_util__make_entry_label(in, in, in, in, out) is det.

:- pred code_util__make_entry_label_from_rtti(rtti_proc_label, immed,
		code_addr).
:- mode code_util__make_entry_label_from_rtti(in, in, out) is det.

	% Create a label which holds the address of the specified procedure,
	% which must be defined in the current module (procedures that are
	% imported from other modules have representations only as code_addrs,
	% not as labels, since their address is not known at C compilation
	% time).
	% The fourth argument has the same meaning as for
	% code_util__make_entry_label.

:- pred code_util__make_local_entry_label(module_info, pred_id, proc_id,
		immed, label).
:- mode code_util__make_local_entry_label(in, in, in, in, out) is det.

	% Create a label internal to a Mercury procedure.
:- pred code_util__make_internal_label(module_info, pred_id, proc_id, int,
		label).
:- mode code_util__make_internal_label(in, in, in, in, out) is det.

:- pred code_util__make_proc_label(module_info, pred_id, proc_id, proc_label).
:- mode code_util__make_proc_label(in, in, in, out) is det.

:- func code_util__make_proc_label_from_rtti(rtti_proc_label) = proc_label.

	% code_util__make_user_proc_label(ModuleName, PredIsImported,
	%	PredOrFunc, ModuleName, PredName, Arity, ProcId, Label):
	% Make a proc_label for a user-defined procedure.
	%
	% The PredIsImported argument should be the result of
	% calling pred_info_is_imported.

:- pred code_util__make_user_proc_label(module_name, bool,
	pred_or_func, module_name, string, arity, proc_id, proc_label).
:- mode code_util__make_user_proc_label(in, in,
	in, in, in, in, in, out) is det.

:- pred code_util__make_uni_label(module_info, type_ctor, proc_id, proc_label).
:- mode code_util__make_uni_label(in, in, in, out) is det.

:- pred code_util__extract_proc_label_from_code_addr(code_addr, proc_label).
:- mode code_util__extract_proc_label_from_code_addr(in, out) is det.

:- pred code_util__extract_proc_label_from_label(label, proc_label).
:- mode code_util__extract_proc_label_from_label(in, out) is det.

:- pred code_util__arg_loc_to_register(arg_loc, lval).
:- mode code_util__arg_loc_to_register(in, out) is det.

:- pred code_util__max_mentioned_reg(list(lval), int).
:- mode code_util__max_mentioned_reg(in, out) is det.

	% Determine whether a goal might allocate some heap space,
	% i.e. whether it contains any construction unifications
	% or predicate calls.  BEWARE that this predicate is only
	% an approximation, used to decide whether or not to try to
	% reclaim the heap space; currently it fails even for some
	% goals which do allocate heap space, such as construction
	% of boxed constants.

:- pred code_util__goal_may_allocate_heap(hlds_goal).
:- mode code_util__goal_may_allocate_heap(in) is semidet.

:- pred code_util__goal_list_may_allocate_heap(list(hlds_goal)).
:- mode code_util__goal_list_may_allocate_heap(in) is semidet.

:- pred code_util__goal_may_alloc_temp_frame(hlds_goal).
:- mode code_util__goal_may_alloc_temp_frame(in) is semidet.

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

:- pred code_util__neg_rval(rval, rval).
:- mode code_util__neg_rval(in, out) is det.

:- pred code_util__negate_the_test(list(instruction), list(instruction)).
:- mode code_util__negate_the_test(in, out) is det.

	% code_util__compiler_generated(PredInfo) succeeds iff
	% the PredInfo is for a compiler generated instance of a
	% type-specific special_pred (i.e. one of the __Unify__,
	% __Index__, or __Compare__ predicates generated as a
	% type-specific instance of unify/2, index/2, or compare/3).
	% 
	% XXX The name of this predicate is misleading, because there
	% are other kinds of compiler-generated predicates, e.g. those
	% for lambda expressions, those generated by higher-order
	% specialization, ordinary type specialization, deforestation,
	% etc., for which this predicate does not succeed.

:- pred code_util__compiler_generated(pred_info).
:- mode code_util__compiler_generated(in) is semidet.

:- pred code_util__predinfo_is_builtin(pred_info).
:- mode code_util__predinfo_is_builtin(in) is semidet.

:- pred code_util__builtin_state(module_info, pred_id, proc_id, builtin_state).
:- mode code_util__builtin_state(in, in, in, out) is det.

	% Find out how a function symbol (constructor) is represented
	% in the given type.

:- pred code_util__cons_id_to_tag(cons_id, type, module_info, cons_tag).
:- mode code_util__cons_id_to_tag(in, in, in, out) is det.

	% Succeed if the given goal cannot encounter a context
	% that causes any variable to be flushed to its stack slot.
	% If such a goal needs a resume point, and that resume point cannot
	% be backtracked to once control leaves the goal, then the only entry
	% point we need for the resume point is the one with the resume
	% variables in their original locations.

:- pred code_util__cannot_stack_flush(hlds_goal).
:- mode code_util__cannot_stack_flush(in) is semidet.

	% Succeed if the given goal cannot fail before encountering a context
	% that forces all variables to be flushed to their stack slots.
	% If such a goal needs a resume point, the only entry point we need
	% is the stack entry point.

:- pred code_util__cannot_fail_before_stack_flush(hlds_goal).
:- mode code_util__cannot_fail_before_stack_flush(in) is semidet.

	% code_util__count_recursive_calls(Goal, PredId, ProcId, Min, Max)
	% Given that we are in predicate PredId and procedure ProcId,
	% return the minimum and maximum number of recursive calls that
	% an execution of Goal may encounter.

:- pred code_util__count_recursive_calls(hlds_goal, pred_id, proc_id,
	int, int).
:- mode code_util__count_recursive_calls(in, in, in, out, out) is det.

	% These predicates return the set of lvals referenced in an rval
	% and an lval respectively. Lvals referenced indirectly through
	% lvals of the form var(_) are not counted.

:- pred code_util__lvals_in_rval(rval, list(lval)).
:- mode code_util__lvals_in_rval(in, out) is det.

:- pred code_util__lvals_in_lval(lval, list(lval)).
:- mode code_util__lvals_in_lval(in, out) is det.

:- pred code_util__lvals_in_lvals(list(lval), list(lval)).
:- mode code_util__lvals_in_lvals(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_util, check_hlds__type_util.
:- import_module hlds__special_pred, backend_libs__builtin_ops.
:- import_module backend_libs__code_model.

:- import_module char, int, string, set, map, term, varset.
:- import_module require, std_util, assoc_list.

%---------------------------------------------------------------------------%

code_util__make_entry_label(ModuleInfo, PredId, ProcId, Immed, ProcAddr) :-
	RttiProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId),
	code_util__make_entry_label_from_rtti(RttiProcLabel, Immed, ProcAddr).

code_util__make_entry_label_from_rtti(RttiProcLabel, Immed, ProcAddr) :-
	(
		(
			RttiProcLabel^is_imported = yes
		;
			RttiProcLabel^is_pseudo_imported = yes,
			% only the (in, in) mode of unification is imported
			hlds_pred__in_in_unification_proc_id(
				RttiProcLabel^proc_id)
		)
	->
		code_util__make_proc_label_from_rtti(RttiProcLabel)
			= ProcLabel,
		ProcAddr = imported(ProcLabel)
	;
		code_util__make_local_entry_label_from_rtti(RttiProcLabel,
			Immed, Label),
		ProcAddr = label(Label)
	).

code_util__make_local_entry_label(ModuleInfo, PredId, ProcId, Immed, Label) :-
	RttiProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId),
	code_util__make_local_entry_label_from_rtti(RttiProcLabel,
		Immed, Label).

:- pred code_util__make_local_entry_label_from_rtti(rtti_proc_label, immed,
		label).
:- mode code_util__make_local_entry_label_from_rtti(in, in, out) is det.

code_util__make_local_entry_label_from_rtti(RttiProcLabel, Immed, Label) :-
	code_util__make_proc_label_from_rtti(RttiProcLabel) = ProcLabel,
	(
		Immed = no,
		% If we want to define the label or use it to put it
		% into a data structure, a label that is usable only
		% within the current C module won't do.
		( RttiProcLabel^is_exported = yes ->
			Label = exported(ProcLabel)
		;
			Label = local(ProcLabel)
		)
	;
		Immed = yes(ProcsPerFunc - proc(CurPredId, CurProcId)),
		choose_local_label_type(ProcsPerFunc, CurPredId, CurProcId,
			RttiProcLabel^pred_id, RttiProcLabel^proc_id,
			ProcLabel, Label)
	).


:- pred choose_local_label_type(int, pred_id, proc_id,
		pred_id, proc_id, proc_label, label).
:- mode choose_local_label_type(in, in, in, in, in, in, out) is det.

choose_local_label_type(ProcsPerFunc, CurPredId, CurProcId,
		PredId, ProcId, ProcLabel, Label) :-
	(
		% If we want to branch to the label now,
		% we prefer a form that are usable only within
		% the current C module, since it is likely
		% to be faster.
		(
			ProcsPerFunc = 0
		;
			PredId = CurPredId,
			ProcId = CurProcId
		)
	->
		Label = c_local(ProcLabel)
	;
		Label = local(ProcLabel)
	).

%-----------------------------------------------------------------------------%

code_util__make_internal_label(ModuleInfo, PredId, ProcId, LabelNum, Label) :-
	code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel),
	Label = local(LabelNum, ProcLabel).

code_util__make_proc_label(ModuleInfo, PredId, ProcId, ProcLabel) :-
	RttiProcLabel = rtti__make_proc_label(ModuleInfo, PredId, ProcId),
	code_util__make_proc_label_from_rtti(RttiProcLabel) = ProcLabel.

code_util__make_proc_label_from_rtti(RttiProcLabel) = ProcLabel :-
	RttiProcLabel = rtti_proc_label(PredOrFunc, ThisModule,
		PredModule, PredName, PredArity, ArgTypes, _PredId, ProcId,
		_VarSet, _HeadVars, _ArgModes, _CodeModel,
		IsImported, _IsPseudoImported, _IsExported,
		IsSpecialPredInstance),
	(
		IsSpecialPredInstance = yes
	->
		(
			special_pred_get_type(PredName, ArgTypes, Type),
			type_to_ctor_and_args(Type, TypeCtor, _),
			% All type_ctors other than tuples here should be
			% module qualified, since builtin types are
			% handled separately in polymorphism.m.
			(
				TypeCtor = unqualified(TypeName) - _,
				type_ctor_is_tuple(TypeCtor),
				mercury_public_builtin_module(TypeModule)
			;
				TypeCtor = qualified(TypeModule, TypeName) - _
			)
		->
			TypeCtor = _ - TypeArity,
			(
				ThisModule \= TypeModule,
				PredName = "__Unify__",
				\+ hlds_pred__in_in_unification_proc_id(ProcId)
			->
				DefiningModule = ThisModule
			;
				DefiningModule = TypeModule
			),
			ProcLabel = special_proc(DefiningModule, PredName,
				TypeModule, TypeName, TypeArity, ProcId)
		;
			string__append_list(["code_util__make_proc_label:\n",
				"cannot make label for special pred `",
				PredName, "'"], ErrorMessage),
			error(ErrorMessage)
		)
	;
		code_util__make_user_proc_label(ThisModule, IsImported,
			PredOrFunc, PredModule, PredName, PredArity,
			ProcId, ProcLabel)
	).

code_util__make_user_proc_label(ThisModule, PredIsImported,
		PredOrFunc, PredModule, PredName, PredArity,
		ProcId, ProcLabel) :-

	(
		% Work out which module supplies the code for
		% the predicate.
		ThisModule \= PredModule,
		PredIsImported = no
	->
		% This predicate is a specialized version of 
		% a pred from a `.opt' file.
		DefiningModule = ThisModule
	;	
		DefiningModule = PredModule
	),
	ProcLabel = proc(DefiningModule, PredOrFunc,
		PredModule, PredName, PredArity, ProcId).

code_util__make_uni_label(ModuleInfo, TypeCtor, UniModeNum, ProcLabel) :-
	module_info_name(ModuleInfo, ModuleName),
	( TypeCtor = qualified(TypeModule, TypeName) - Arity ->
		( hlds_pred__in_in_unification_proc_id(UniModeNum) ->
			Module = TypeModule
		;
			Module = ModuleName
		),
		ProcLabel = special_proc(Module, "__Unify__", TypeModule,
			TypeName, Arity, UniModeNum)
	;
		error("code_util__make_uni_label: unqualified type_ctor")
	).

code_util__extract_proc_label_from_code_addr(CodeAddr, ProcLabel) :-
	( code_util__proc_label_from_code_addr(CodeAddr, ProcLabelPrime) ->
		ProcLabel = ProcLabelPrime
	;
		error("code_util__extract_label_from_code_addr failed")
	).

:- pred code_util__proc_label_from_code_addr(code_addr::in,
	proc_label::out) is semidet.

code_util__proc_label_from_code_addr(CodeAddr, ProcLabel) :-
	(
		CodeAddr = label(Label),
		code_util__extract_proc_label_from_label(Label, ProcLabel)
	;
		CodeAddr = imported(ProcLabel)
	).

code_util__extract_proc_label_from_label(local(_, ProcLabel), ProcLabel).
code_util__extract_proc_label_from_label(c_local(ProcLabel), ProcLabel).
code_util__extract_proc_label_from_label(local(ProcLabel), ProcLabel).
code_util__extract_proc_label_from_label(exported(ProcLabel), ProcLabel).

%-----------------------------------------------------------------------------%

code_util__arg_loc_to_register(ArgLoc, reg(r, ArgLoc)).

%-----------------------------------------------------------------------------%

code_util__max_mentioned_reg(Lvals, MaxRegNum) :-
	code_util__max_mentioned_reg_2(Lvals, 0, MaxRegNum).

:- pred code_util__max_mentioned_reg_2(list(lval)::in, int::in, int::out)
	is det.

code_util__max_mentioned_reg_2([], MaxRegNum, MaxRegNum).
code_util__max_mentioned_reg_2([Lval | Lvals], MaxRegNum0, MaxRegNum) :-
	( Lval = reg(r, N) ->
		int__max(MaxRegNum0, N, MaxRegNum1)
	;
		MaxRegNum1 = MaxRegNum0
	),
	code_util__max_mentioned_reg_2(Lvals, MaxRegNum1, MaxRegNum).

%-----------------------------------------------------------------------------%

code_util__predinfo_is_builtin(PredInfo) :-
	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, Arity),
	hlds_pred__initial_proc_id(ProcId),
	code_util__is_inline_builtin(ModuleName, PredName, ProcId, Arity).

code_util__builtin_state(ModuleInfo, PredId, ProcId, BuiltinState) :-
	module_info_pred_info(ModuleInfo, PredId, PredInfo),
	pred_info_module(PredInfo, ModuleName),
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, Arity),
	( code_util__is_inline_builtin(ModuleName, PredName, ProcId, Arity) ->
		BuiltinState = inline_builtin
	;
		BuiltinState = not_builtin
	).

:- pred code_util__is_inline_builtin(module_name, string, proc_id, arity).
:- mode code_util__is_inline_builtin(in, in, in, in) is semidet.

code_util__is_inline_builtin(ModuleName, PredName, ProcId, Arity) :-
	Arity =< 3,
	prog_varset_init(VarSet),
	varset__new_vars(VarSet, Arity, Args, _),
	builtin_ops__translate_builtin(ModuleName, PredName, ProcId, Args, _).

:- pred prog_varset_init(prog_varset::out) is det.
prog_varset_init(VarSet) :- varset__init(VarSet).

%-----------------------------------------------------------------------------%

	% XXX The name of this predicate is misleading -- see the comment
	% in the declaration.
code_util__compiler_generated(PredInfo) :-
	pred_info_name(PredInfo, PredName),
	pred_info_arity(PredInfo, PredArity),
	special_pred_name_arity(_, _, PredName, PredArity).

%-----------------------------------------------------------------------------%

code_util__goal_may_allocate_heap(Goal) :-
	code_util__goal_may_allocate_heap(Goal, yes).

code_util__goal_list_may_allocate_heap(Goals) :-
	code_util__goal_list_may_allocate_heap(Goals, yes).

:- pred code_util__goal_may_allocate_heap(hlds_goal::in, bool::out) is det.

code_util__goal_may_allocate_heap(Goal - _GoalInfo, May) :-
	code_util__goal_may_allocate_heap_2(Goal, May).

:- pred code_util__goal_may_allocate_heap_2(hlds_goal_expr::in, bool::out)
	is det.

code_util__goal_may_allocate_heap_2(generic_call(_, _, _, _), yes).
code_util__goal_may_allocate_heap_2(call(_, _, _, Builtin, _, _), May) :-
	( Builtin = inline_builtin ->
		May = no
	;
		May = yes
	).
code_util__goal_may_allocate_heap_2(unify(_, _, _, Unification, _), May) :-
	( Unification = construct(_,_,Args,_,_,_,_), Args = [_|_] ->
		May = yes
	;
		May = no
	).
	% We cannot safely say that a foreign code fragment does not
	% allocate memory without knowing all the #defined macros that
	% expand to incr_hp and variants thereof.
	% XXX although you could make it an attribute of the foreign code and
	% trust the programmer
code_util__goal_may_allocate_heap_2(foreign_proc(_,_,_,_,_,_,_), yes).
code_util__goal_may_allocate_heap_2(some(_Vars, _, Goal), May) :-
	code_util__goal_may_allocate_heap(Goal, May).
code_util__goal_may_allocate_heap_2(not(Goal), May) :-
	code_util__goal_may_allocate_heap(Goal, May).
code_util__goal_may_allocate_heap_2(conj(Goals), May) :-
	code_util__goal_list_may_allocate_heap(Goals, May).
code_util__goal_may_allocate_heap_2(par_conj(_, _), yes).
code_util__goal_may_allocate_heap_2(disj(Goals, _), May) :-
	code_util__goal_list_may_allocate_heap(Goals, May).
code_util__goal_may_allocate_heap_2(switch(_Var, _Det, Cases, _), May) :-
	code_util__cases_may_allocate_heap(Cases, May).
code_util__goal_may_allocate_heap_2(if_then_else(_Vars, C, T, E, _), May) :-
	( code_util__goal_may_allocate_heap(C, yes) ->
		May = yes
	; code_util__goal_may_allocate_heap(T, yes) ->
		May = yes
	;
		code_util__goal_may_allocate_heap(E, May)
	).
code_util__goal_may_allocate_heap_2(shorthand(ShorthandGoal), May) :-
	code_util__goal_may_allocate_heap_2_shorthand(ShorthandGoal, May).

:- pred code_util__goal_may_allocate_heap_2_shorthand(shorthand_goal_expr::in, 
	bool::out) is det.
	
code_util__goal_may_allocate_heap_2_shorthand(bi_implication(G1, G2), May) :-
	( code_util__goal_may_allocate_heap(G1, yes) ->
		May = yes
	;
		code_util__goal_may_allocate_heap(G2, May)
	).



:- pred code_util__goal_list_may_allocate_heap(list(hlds_goal)::in, bool::out)
	is det.

code_util__goal_list_may_allocate_heap([], no).
code_util__goal_list_may_allocate_heap([Goal | Goals], May) :-
	( code_util__goal_may_allocate_heap(Goal, yes) ->
		May = yes
	;
		code_util__goal_list_may_allocate_heap(Goals, May)
	).

:- pred code_util__cases_may_allocate_heap(list(case)::in, bool::out) is det.

code_util__cases_may_allocate_heap([], no).
code_util__cases_may_allocate_heap([case(_, Goal) | Cases], May) :-
	( code_util__goal_may_allocate_heap(Goal, yes) ->
		May = yes
	;
		code_util__cases_may_allocate_heap(Cases, May)
	).

%-----------------------------------------------------------------------------%

code_util__goal_may_alloc_temp_frame(Goal) :-
	code_util__goal_may_alloc_temp_frame(Goal, yes).

:- pred code_util__goal_may_alloc_temp_frame(hlds_goal::in, bool::out) is det.

code_util__goal_may_alloc_temp_frame(Goal - _GoalInfo, May) :-
	code_util__goal_may_alloc_temp_frame_2(Goal, May).

:- pred code_util__goal_may_alloc_temp_frame_2(hlds_goal_expr::in, bool::out)
	is det.

code_util__goal_may_alloc_temp_frame_2(generic_call(_, _, _, _), no).
code_util__goal_may_alloc_temp_frame_2(call(_, _, _, _, _, _), no).
code_util__goal_may_alloc_temp_frame_2(unify(_, _, _, _, _), no).
	% We cannot safely say that a foreign code fragment does not allocate
	% temporary nondet frames without knowing all the #defined macros
	% that expand to mktempframe and variants thereof. The performance
	% impact of being too conservative is probably not too bad.
code_util__goal_may_alloc_temp_frame_2(foreign_proc(_,_,_,_,_,_,_),
		yes).
code_util__goal_may_alloc_temp_frame_2(some(_Vars, _, Goal), May) :-
	Goal = _ - GoalInfo,
	goal_info_get_code_model(GoalInfo, CodeModel),
	( CodeModel = model_non ->
		May = yes
	;
		code_util__goal_may_alloc_temp_frame(Goal, May)
	).
code_util__goal_may_alloc_temp_frame_2(not(Goal), May) :-
	code_util__goal_may_alloc_temp_frame(Goal, May).
code_util__goal_may_alloc_temp_frame_2(conj(Goals), May) :-
	code_util__goal_list_may_alloc_temp_frame(Goals, May).
code_util__goal_may_alloc_temp_frame_2(par_conj(Goals, _), May) :-
	code_util__goal_list_may_alloc_temp_frame(Goals, May).
code_util__goal_may_alloc_temp_frame_2(disj(Goals, _), May) :-
	code_util__goal_list_may_alloc_temp_frame(Goals, May).
code_util__goal_may_alloc_temp_frame_2(switch(_Var, _Det, Cases, _), May) :-
	code_util__cases_may_alloc_temp_frame(Cases, May).
code_util__goal_may_alloc_temp_frame_2(if_then_else(_Vars, C, T, E, _), May) :-
	( code_util__goal_may_alloc_temp_frame(C, yes) ->
		May = yes
	; code_util__goal_may_alloc_temp_frame(T, yes) ->
		May = yes
	;
		code_util__goal_may_alloc_temp_frame(E, May)
	).
code_util__goal_may_alloc_temp_frame_2(shorthand(ShorthandGoal), May) :-
	code_util__goal_may_alloc_temp_frame_2_shorthand(ShorthandGoal,May).
		
:- pred code_util__goal_may_alloc_temp_frame_2_shorthand(
		shorthand_goal_expr::in, bool::out) is det.

code_util__goal_may_alloc_temp_frame_2_shorthand(bi_implication(G1, G2), 
		May) :-
	( code_util__goal_may_alloc_temp_frame(G1, yes) ->
		May = yes
	;
		code_util__goal_may_alloc_temp_frame(G2, May)
	).


:- pred code_util__goal_list_may_alloc_temp_frame(list(hlds_goal)::in,
	bool::out) is det.

code_util__goal_list_may_alloc_temp_frame([], no).
code_util__goal_list_may_alloc_temp_frame([Goal | Goals], May) :-
	( code_util__goal_may_alloc_temp_frame(Goal, yes) ->
		May = yes
	;
		code_util__goal_list_may_alloc_temp_frame(Goals, May)
	).

:- pred code_util__cases_may_alloc_temp_frame(list(case)::in, bool::out)
	is det.

code_util__cases_may_alloc_temp_frame([], no).
code_util__cases_may_alloc_temp_frame([case(_, Goal) | Cases], May) :-
	( code_util__goal_may_alloc_temp_frame(Goal, yes) ->
		May = yes
	;
		code_util__cases_may_alloc_temp_frame(Cases, May)
	).

%-----------------------------------------------------------------------------%

	% Negate a condition.
	% This is used mostly just to make the generated code more readable.

code_util__neg_rval(Rval, NegRval) :-
	( code_util__neg_rval_2(Rval, NegRval0) ->
		NegRval = NegRval0
	;
		NegRval = unop(not, Rval)
	).

:- pred code_util__neg_rval_2(rval, rval).
:- mode code_util__neg_rval_2(in, out) is semidet.

code_util__neg_rval_2(const(Const), const(NegConst)) :-
	(
		Const = true, NegConst = false
	;
		Const = false, NegConst = true
	).
code_util__neg_rval_2(unop(not, Rval), Rval).
code_util__neg_rval_2(binop(Op, X, Y), binop(NegOp, X, Y)) :-
	code_util__neg_op(Op, NegOp).

:- pred code_util__neg_op(binary_op, binary_op).
:- mode code_util__neg_op(in, out) is semidet.

code_util__neg_op(eq, ne).
code_util__neg_op(ne, eq).
code_util__neg_op(<, >=).
code_util__neg_op(<=, >).
code_util__neg_op(>, <=).
code_util__neg_op(>=, <).
code_util__neg_op(str_eq, str_ne).
code_util__neg_op(str_ne, str_eq).
code_util__neg_op(str_lt, str_ge).
code_util__neg_op(str_le, str_gt).
code_util__neg_op(str_gt, str_le).
code_util__neg_op(str_ge, str_lt).
code_util__neg_op(float_eq, float_ne).
code_util__neg_op(float_ne, float_eq).
code_util__neg_op(float_lt, float_ge).
code_util__neg_op(float_le, float_gt).
code_util__neg_op(float_gt, float_le).
code_util__neg_op(float_ge, float_lt).

code_util__negate_the_test([], _) :-
	error("code_util__negate_the_test on empty list").
code_util__negate_the_test([Instr0 | Instrs0], Instrs) :-
	( Instr0 = if_val(Test, Target) - Comment ->
		code_util__neg_rval(Test, NewTest),
		Instrs = [if_val(NewTest, Target) - Comment]
	;
		code_util__negate_the_test(Instrs0, Instrs1),
		Instrs = [Instr0 | Instrs1]
	).

%-----------------------------------------------------------------------------%

code_util__cons_id_to_tag(int_const(X), _, _, int_constant(X)).
code_util__cons_id_to_tag(float_const(X), _, _, float_constant(X)).
code_util__cons_id_to_tag(string_const(X), _, _, string_constant(X)).
code_util__cons_id_to_tag(code_addr_const(P,M), _, _, code_addr_constant(P,M)).
code_util__cons_id_to_tag(pred_const(P,M,E), _, _, pred_closure_tag(P,M,E)).
code_util__cons_id_to_tag(type_ctor_info_const(M,T,A), _, _,
		type_ctor_info_constant(M,T,A)).
code_util__cons_id_to_tag(base_typeclass_info_const(M,C,_,N), _, _,
		base_typeclass_info_constant(M,C,N)).
code_util__cons_id_to_tag(tabling_pointer_const(PredId,ProcId), _, _,
		tabling_pointer_constant(PredId,ProcId)).
code_util__cons_id_to_tag(deep_profiling_proc_static(PPId), _, _,
		deep_profiling_proc_static_tag(PPId)).
code_util__cons_id_to_tag(table_io_decl(PPId), _, _, table_io_decl_tag(PPId)).
code_util__cons_id_to_tag(cons(Name, Arity), Type, ModuleInfo, Tag) :-
	(
			% handle the `character' type specially
		Type = term__functor(term__atom("character"), [], _),
		Name = unqualified(ConsName),
	 	string__char_to_string(Char, ConsName)
	->
		char__to_int(Char, CharCode),
		Tag = int_constant(CharCode)
	;
		% Tuples do not need a tag. Note that unary tuples are not
		% treated as no_tag types. There's no reason why they
		% couldn't be, it's just not worth the effort.
		type_is_tuple(Type, _)
	->
		Tag = single_functor
	;
			% Use the type to determine the type_ctor
		( type_to_ctor_and_args(Type, TypeCtor0, _) ->
			TypeCtor = TypeCtor0
		;
			% the type-checker should ensure that this never happens
			error("code_util__cons_id_to_tag: invalid type")
		),

			% Given the type_ctor, lookup up the constructor tag
			% table for that type
		module_info_types(ModuleInfo, TypeTable),
		map__lookup(TypeTable, TypeCtor, TypeDefn),
		hlds_data__get_type_defn_body(TypeDefn, TypeBody),
		(
			TypeBody = du_type(_, ConsTable0, _, _)
		->
			ConsTable = ConsTable0
		;
			% this should never happen
			error(
			"code_util__cons_id_to_tag: type is not d.u. type?"
			)
		),
			% Finally look up the cons_id in the table
		map__lookup(ConsTable, cons(Name, Arity), Tag)
	).

%-----------------------------------------------------------------------------%

code_util__cannot_stack_flush(GoalExpr - _) :-
	code_util__cannot_stack_flush_2(GoalExpr).

:- pred code_util__cannot_stack_flush_2(hlds_goal_expr).
:- mode code_util__cannot_stack_flush_2(in) is semidet.

code_util__cannot_stack_flush_2(unify(_, _, _, Unify, _)) :-
	Unify \= complicated_unify(_, _, _).
code_util__cannot_stack_flush_2(call(_, _, _, BuiltinState, _, _)) :-
	BuiltinState = inline_builtin.
code_util__cannot_stack_flush_2(conj(Goals)) :-
	code_util__cannot_stack_flush_goals(Goals).
code_util__cannot_stack_flush_2(switch(_, _, Cases, _)) :-
	code_util__cannot_stack_flush_cases(Cases).

:- pred code_util__cannot_stack_flush_goals(list(hlds_goal)).
:- mode code_util__cannot_stack_flush_goals(in) is semidet.

code_util__cannot_stack_flush_goals([]).
code_util__cannot_stack_flush_goals([Goal | Goals]) :-
	code_util__cannot_stack_flush(Goal),
	code_util__cannot_stack_flush_goals(Goals).

:- pred code_util__cannot_stack_flush_cases(list(case)).
:- mode code_util__cannot_stack_flush_cases(in) is semidet.

code_util__cannot_stack_flush_cases([]).
code_util__cannot_stack_flush_cases([case(_, Goal) | Cases]) :-
	code_util__cannot_stack_flush(Goal),
	code_util__cannot_stack_flush_cases(Cases).

%-----------------------------------------------------------------------------%

code_util__cannot_fail_before_stack_flush(GoalExpr - GoalInfo) :-
	goal_info_get_determinism(GoalInfo, Detism),
	determinism_components(Detism, CanFail, _),
	( CanFail = cannot_fail ->
		true
	;
		code_util__cannot_fail_before_stack_flush_2(GoalExpr)
	).

:- pred code_util__cannot_fail_before_stack_flush_2(hlds_goal_expr).
:- mode code_util__cannot_fail_before_stack_flush_2(in) is semidet.

code_util__cannot_fail_before_stack_flush_2(conj(Goals)) :-
	code_util__cannot_fail_before_stack_flush_conj(Goals).

:- pred code_util__cannot_fail_before_stack_flush_conj(list(hlds_goal)).
:- mode code_util__cannot_fail_before_stack_flush_conj(in) is semidet.

code_util__cannot_fail_before_stack_flush_conj([]).
code_util__cannot_fail_before_stack_flush_conj([Goal | Goals]) :-
	Goal = GoalExpr - GoalInfo,
	(
		(
			GoalExpr = call(_, _, _, BuiltinState, _, _),
			BuiltinState \= inline_builtin
		;
			GoalExpr = generic_call(_, _, _, _)
		)
	->
		true
	;
		goal_info_get_determinism(GoalInfo, Detism),
		determinism_components(Detism, cannot_fail, _)
	->
		code_util__cannot_fail_before_stack_flush_conj(Goals)
	;
		fail
	).

%-----------------------------------------------------------------------------%

code_util__count_recursive_calls(Goal - _, PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls_2(Goal, PredId, ProcId, Min, Max).

:- pred code_util__count_recursive_calls_2(hlds_goal_expr, pred_id, proc_id,
	int, int).
:- mode code_util__count_recursive_calls_2(in, in, in, out, out) is det.

code_util__count_recursive_calls_2(not(Goal), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls(Goal, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(some(_, _, Goal),
		PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls(Goal, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(unify(_, _, _, _, _), _, _, 0, 0).
code_util__count_recursive_calls_2(generic_call(_, _, _, _), _, _,
		0, 0).
code_util__count_recursive_calls_2(foreign_proc(_, _, _, _, _, _, _),
		_, _, 0, 0).
code_util__count_recursive_calls_2(call(CallPredId, CallProcId, _, _, _, _),
		PredId, ProcId, Count, Count) :-
	(
		PredId = CallPredId,
		ProcId = CallProcId
	->
		Count = 1
	;
		Count = 0
	).
code_util__count_recursive_calls_2(conj(Goals), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls_conj(Goals, PredId, ProcId, 0, 0,
		Min, Max).
code_util__count_recursive_calls_2(par_conj(Goals, _), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls_conj(Goals, PredId, ProcId, 0, 0, Min, Max).
code_util__count_recursive_calls_2(disj(Goals, _), PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls_disj(Goals, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(switch(_, _, Cases, _), PredId, ProcId,
		Min, Max) :-
	code_util__count_recursive_calls_cases(Cases, PredId, ProcId, Min, Max).
code_util__count_recursive_calls_2(if_then_else(_, Cond, Then, Else, _),
		PredId, ProcId, Min, Max) :-
	code_util__count_recursive_calls(Cond, PredId, ProcId, CMin, CMax),
	code_util__count_recursive_calls(Then, PredId, ProcId, TMin, TMax),
	code_util__count_recursive_calls(Else, PredId, ProcId, EMin, EMax),
	CTMin is CMin + TMin,
	CTMax is CMax + TMax,
	int__min(CTMin, EMin, Min),
	int__max(CTMax, EMax, Max).
code_util__count_recursive_calls_2(shorthand(_),
		_, _, _, _) :-
	% these should have been expanded out by now
	error("code_util__count_recursive_calls_2: unexpected shorthand").

:- pred code_util__count_recursive_calls_conj(list(hlds_goal),
	pred_id, proc_id, int, int, int, int).
:- mode code_util__count_recursive_calls_conj(in, in, in, in, in, out, out)
	is det.

code_util__count_recursive_calls_conj([], _, _, Min, Max, Min, Max).
code_util__count_recursive_calls_conj([Goal | Goals], PredId, ProcId,
		Min0, Max0, Min, Max) :-
	code_util__count_recursive_calls(Goal, PredId, ProcId, Min1, Max1),
	Min2 is Min0 + Min1,
	Max2 is Max0 + Max1,
	code_util__count_recursive_calls_conj(Goals, PredId, ProcId,
		Min2, Max2, Min, Max).

:- pred code_util__count_recursive_calls_disj(list(hlds_goal),
	pred_id, proc_id, int, int).
:- mode code_util__count_recursive_calls_disj(in, in, in, out, out) is det.

code_util__count_recursive_calls_disj([], _, _, 0, 0).
code_util__count_recursive_calls_disj([Goal | Goals], PredId, ProcId,
		Min, Max) :-
	( Goals = [] ->
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min, Max)
	;
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min0, Max0),
		code_util__count_recursive_calls_disj(Goals, PredId, ProcId,
			Min1, Max1),
		int__min(Min0, Min1, Min),
		int__max(Max0, Max1, Max)
	).

:- pred code_util__count_recursive_calls_cases(list(case),
	pred_id, proc_id, int, int).
:- mode code_util__count_recursive_calls_cases(in, in, in, out, out) is det.

code_util__count_recursive_calls_cases([], _, _, _, _) :-
	error("empty cases in code_util__count_recursive_calls_cases").
code_util__count_recursive_calls_cases([case(_, Goal) | Cases], PredId, ProcId,
		Min, Max) :-
	( Cases = [] ->
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min, Max)
	;
		code_util__count_recursive_calls(Goal, PredId, ProcId,
			Min0, Max0),
		code_util__count_recursive_calls_cases(Cases, PredId, ProcId,
			Min1, Max1),
		int__min(Min0, Min1, Min),
		int__max(Max0, Max1, Max)
	).

%-----------------------------------------------------------------------------%

code_util__lvals_in_lvals([], []).
code_util__lvals_in_lvals([First | Rest], Lvals) :-
	code_util__lvals_in_lval(First, FirstLvals),
	code_util__lvals_in_lvals(Rest, RestLvals),
	list__append(FirstLvals, RestLvals, Lvals).

code_util__lvals_in_rval(lval(Lval), [Lval | Lvals]) :-
	code_util__lvals_in_lval(Lval, Lvals).
code_util__lvals_in_rval(var(_), []).
code_util__lvals_in_rval(create(_, _, _, _, _, _, _), []).
code_util__lvals_in_rval(mkword(_, Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_rval(const(_), []).
code_util__lvals_in_rval(unop(_, Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_rval(binop(_, Rval1, Rval2), Lvals) :-
	code_util__lvals_in_rval(Rval1, Lvals1),
	code_util__lvals_in_rval(Rval2, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).
code_util__lvals_in_rval(mem_addr(MemRef), Lvals) :-
	code_util__lvals_in_mem_ref(MemRef, Lvals).

code_util__lvals_in_lval(reg(_, _), []).
code_util__lvals_in_lval(stackvar(_), []).
code_util__lvals_in_lval(framevar(_), []).
code_util__lvals_in_lval(succip, []).
code_util__lvals_in_lval(maxfr, []).
code_util__lvals_in_lval(curfr, []).
code_util__lvals_in_lval(succip(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(redofr(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(redoip(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(succfr(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(prevfr(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).
code_util__lvals_in_lval(hp, []).
code_util__lvals_in_lval(sp, []).
code_util__lvals_in_lval(field(_, Rval1, Rval2), Lvals) :-
	code_util__lvals_in_rval(Rval1, Lvals1),
	code_util__lvals_in_rval(Rval2, Lvals2),
	list__append(Lvals1, Lvals2, Lvals).
code_util__lvals_in_lval(lvar(_), []).
code_util__lvals_in_lval(temp(_, _), []).
code_util__lvals_in_lval(mem_ref(Rval), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).

:- pred code_util__lvals_in_mem_ref(mem_ref, list(lval)).
:- mode code_util__lvals_in_mem_ref(in, out) is det.

code_util__lvals_in_mem_ref(stackvar_ref(_), []).
code_util__lvals_in_mem_ref(framevar_ref(_), []).
code_util__lvals_in_mem_ref(heap_ref(Rval, _, _), Lvals) :-
	code_util__lvals_in_rval(Rval, Lvals).

%-----------------------------------------------------------------------------%
