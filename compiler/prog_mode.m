%-----------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Main author: fjh.
%
% Utility predicates dealing with modes and insts that do not require access
% to the HLDS. (The predicates that do are in mode_util.m.)

:- module parse_tree__prog_mode.

:- interface.

:- import_module parse_tree__prog_data.

:- import_module list.

	% Construct a mode corresponding to the standard
	% `in', `out', `uo' or `unused' mode.
:- pred in_mode((mode)::out) is det.
:- func in_mode = (mode).
:- pred out_mode((mode)::out) is det.
:- func out_mode = (mode).
:- pred uo_mode((mode)::out) is det.
:- func uo_mode = (mode).
:- pred unused_mode((mode)::out) is det.
:- func unused_mode = (mode).

:- func ground_inst = (inst).
:- func free_inst = (inst).

	% Construct the modes used for `aditi__state' arguments.
	% XXX These should be unique, but are not yet because that
	% would require alias tracking.
:- func aditi_mui_mode = (mode).
:- func aditi_ui_mode = (mode).
:- func aditi_di_mode = (mode).
:- func aditi_uo_mode = (mode).

:- pred make_std_mode(string::in, list(inst)::in, (mode)::out) is det.
:- func make_std_mode(string, list(inst)) = (mode).

%-----------------------------------------------------------------------------%

	% mode_substitute_arg_list(Mode0, Params, Args, Mode) is true
	% iff Mode is the mode that results from substituting all
	% occurrences of Params in Mode0 with the corresponding
	% value in Args.

:- pred mode_substitute_arg_list((mode)::in, list(inst_var)::in,
	list(inst)::in, (mode)::out) is det.

	% inst_lists_to_mode_list(InitialInsts, FinalInsts, Modes):
	%	Given two lists of corresponding initial and final
	%	insts, return a list of modes which maps from the
	%	initial insts to the final insts.
:- pred inst_lists_to_mode_list(list(inst)::in, list(inst)::in,
	list(mode)::out) is det.

:- pred insts_to_mode((inst)::in, (inst)::in, (mode)::out) is det.

%-----------------------------------------------------------------------------%

	% inst_substitute_arg_list(Inst0, Params, Args, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Params in Inst0 with the corresponding
	% value in Args.

:- pred inst_substitute_arg_list((inst)::in, list(inst_var)::in,
	list(inst)::in, (inst)::out) is det.

	% inst_list_apply_substitution(Insts0, Subst, Insts) is true
	% iff Inst is the inst that results from applying Subst to Insts0.

:- pred inst_list_apply_substitution(list(inst)::in, inst_var_sub::in,
	list(inst)::out) is det.

	% mode_list_apply_substitution(Modes0, Subst, Modes) is true
	% iff Mode is the mode that results from applying Subst to Modes0.

:- pred mode_list_apply_substitution(list(mode)::in, inst_var_sub::in,
	list(mode)::out) is det.

:- pred rename_apart_inst_vars(inst_varset::in, inst_varset::in,
	list(mode)::in, list(mode)::out) is det.

%-----------------------------------------------------------------------------%

	% Given an expanded inst and a cons_id and its arity, return the
	% insts of the arguments of the top level functor, failing if the
	% inst could not be bound to the functor.
:- pred get_arg_insts((inst)::in, cons_id::in, arity::in, list(inst)::out)
	is semidet.

        % Given a list of bound_insts, get the corresponding list of cons_ids
        %
:- pred functors_to_cons_ids(list(bound_inst)::in, list(cons_id)::out) is det.

:- pred mode_id_to_int(mode_id::in, int::out) is det.

	% Predicates to make error messages more readable by stripping
	% "builtin:" module qualifiers from modes.

:- pred strip_builtin_qualifier_from_cons_id(cons_id::in, cons_id::out) is det.

:- pred strip_builtin_qualifiers_from_mode_list(list(mode)::in,
	list(mode)::out) is det.

:- pred strip_builtin_qualifiers_from_inst_list(list(inst)::in,
	list(inst)::out) is det.

:- pred strip_builtin_qualifiers_from_inst((inst)::in, (inst)::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree__prog_util.

:- import_module map, set, require, std_util, varset, term.

in_mode(in_mode).
out_mode(out_mode).
uo_mode(uo_mode).
unused_mode(unused_mode).

in_mode = make_std_mode("in", []).
out_mode = make_std_mode("out", []).
uo_mode = make_std_mode("uo", []).
unused_mode = make_std_mode("unused", []).

aditi_mui_mode = Mode :- in_mode(Mode).
aditi_ui_mode = Mode :- in_mode(Mode).
aditi_di_mode = Mode :- in_mode(Mode).
aditi_uo_mode = Mode :- out_mode(Mode).

ground_inst = ground(shared, none).
free_inst = free.

make_std_mode(Name, Args, make_std_mode(Name, Args)).

make_std_mode(Name, Args) = Mode :-
	mercury_public_builtin_module(MercuryBuiltin),
	QualifiedName = qualified(MercuryBuiltin, Name),
	Mode = user_defined_mode(QualifiedName, Args).

%-----------------------------------------------------------------------------%

inst_lists_to_mode_list([], [_|_], _) :-
	error("inst_lists_to_mode_list: length mis-match").
inst_lists_to_mode_list([_|_], [], _) :-
	error("inst_lists_to_mode_list: length mis-match").
inst_lists_to_mode_list([], [], []).
inst_lists_to_mode_list([Initial|Initials], [Final|Finals], [Mode|Modes]) :-
	insts_to_mode(Initial, Final, Mode),
	inst_lists_to_mode_list(Initials, Finals, Modes).

insts_to_mode(Initial, Final, Mode) :-
	%
	% Use some abbreviations.
	% This is just to make error messages and inferred modes
	% more readable.
	%
	( Initial = free, Final = ground(shared, none) ->
		make_std_mode("out", [], Mode)
	; Initial = free, Final = ground(unique, none) ->
		make_std_mode("uo", [], Mode)
	; Initial = free, Final = ground(mostly_unique, none) ->
		make_std_mode("muo", [], Mode)
	; Initial = ground(shared, none), Final = ground(shared, none) ->
		make_std_mode("in", [], Mode)
	; Initial = ground(unique, none), Final = ground(clobbered, none) ->
		make_std_mode("di", [], Mode)
	; Initial = ground(mostly_unique, none),
	  Final = ground(mostly_clobbered, none) ->
		make_std_mode("mdi", [], Mode)
	; Initial = ground(unique, none), Final = ground(unique, none) ->
		make_std_mode("ui", [], Mode)
	; Initial = ground(mostly_unique, none),
	  Final = ground(mostly_unique, none) ->
		make_std_mode("mdi", [], Mode)
	; Initial = free ->
		make_std_mode("out", [Final], Mode)
	; Final = ground(clobbered, none) ->
		make_std_mode("di", [Initial], Mode)
	; Initial = Final ->
		make_std_mode("in", [Initial], Mode)
	;
		Mode = (Initial -> Final)
	).

%-----------------------------------------------------------------------------%

mode_substitute_arg_list(Mode0, Params, Args, Mode) :-
	( Params = [] ->
		Mode = Mode0	% optimize common case
	;
		map__from_corresponding_lists(Params, Args, Subst),
		mode_apply_substitution(Mode0, Subst, Mode)
	).

inst_substitute_arg_list(Inst0, Params, Args, Inst) :-
	( Params = [] ->
		Inst = Inst0	% optimize common case
	;
		map__from_corresponding_lists(Params, Args, Subst),
		inst_apply_substitution(Inst0, Subst, Inst)
	).

	% mode_apply_substitution(Mode0, Subst, Mode) is true iff
	% Mode is the mode that results from apply Subst to Mode0.

:- pred mode_apply_substitution((mode)::in, inst_var_sub::in, (mode)::out)
	is det.

mode_apply_substitution((I0 -> F0), Subst, (I -> F)) :-
	inst_apply_substitution(I0, Subst, I),
	inst_apply_substitution(F0, Subst, F).
mode_apply_substitution(user_defined_mode(Name, Args0), Subst,
		    user_defined_mode(Name, Args)) :-
	inst_list_apply_substitution_2(Args0, Subst, Args).

inst_list_apply_substitution(Insts0, Subst, Insts) :-
	( map__is_empty(Subst) ->
		Insts = Insts0
	;
		inst_list_apply_substitution_2(Insts0, Subst, Insts)
	).

:- pred inst_list_apply_substitution_2(list(inst)::in, inst_var_sub::in,
	list(inst)::out) is det.

inst_list_apply_substitution_2([], _, []).
inst_list_apply_substitution_2([A0 | As0], Subst, [A | As]) :-
	inst_apply_substitution(A0, Subst, A),
	inst_list_apply_substitution_2(As0, Subst, As).

	% inst_substitute_arg(Inst0, Subst, Inst) is true
	% iff Inst is the inst that results from substituting all
	% occurrences of Param in Inst0 with Arg.

:- pred inst_apply_substitution((inst)::in, inst_var_sub::in, (inst)::out)
	is det.

inst_apply_substitution(any(Uniq), _, any(Uniq)).
inst_apply_substitution(free, _, free).
inst_apply_substitution(free(T), _, free(T)).
inst_apply_substitution(ground(Uniq, GroundInstInfo0), Subst, Inst) :-
	ground_inst_info_apply_substitution(GroundInstInfo0, Subst, Uniq, Inst).
inst_apply_substitution(bound(Uniq, Alts0), Subst, bound(Uniq, Alts)) :-
	alt_list_apply_substitution(Alts0, Subst, Alts).
inst_apply_substitution(not_reached, _, not_reached).
inst_apply_substitution(inst_var(Var), Subst, Result) :-
	(
		map__search(Subst, Var, Replacement)
	->
		Result = Replacement
	;
		Result = inst_var(Var)
	).
inst_apply_substitution(constrained_inst_vars(Vars, Inst0), Subst, Result) :-
	( set__singleton_set(Vars, Var0) ->
		Var = Var0
	;
		error("inst_apply_substitution: multiple inst_vars found")
	),
	(
		map__search(Subst, Var, Replacement)
	->
		Result = Replacement
		% XXX Should probably have a sanity check here that
		% Replacement =< Inst0
	;
		inst_apply_substitution(Inst0, Subst, Result0),
		Result = constrained_inst_vars(Vars, Result0)
	).
inst_apply_substitution(defined_inst(InstName0), Subst,
		    defined_inst(InstName)) :-
	( inst_name_apply_substitution(InstName0, Subst, InstName1) ->
		InstName = InstName1
	;
		InstName = InstName0
	).
inst_apply_substitution(abstract_inst(Name, Args0), Subst,
		    abstract_inst(Name, Args)) :-
	inst_list_apply_substitution_2(Args0, Subst, Args).

	% This predicate fails if the inst_name is not one of user_inst,
	% typed_inst or typed_ground.  The other types of inst_names are just
	% used as keys in the inst_table so it does not make sense to apply
	% substitutions to them.
:- pred inst_name_apply_substitution(inst_name::in, inst_var_sub::in,
	inst_name::out) is semidet.

inst_name_apply_substitution(user_inst(Name, Args0), Subst,
		user_inst(Name, Args)) :-
	inst_list_apply_substitution_2(Args0, Subst, Args).
inst_name_apply_substitution(typed_inst(T, Inst0), Subst,
		typed_inst(T, Inst)) :-
	inst_name_apply_substitution(Inst0, Subst, Inst).
inst_name_apply_substitution(typed_ground(Uniq, T), _, typed_ground(Uniq, T)).

:- pred alt_list_apply_substitution(list(bound_inst)::in, inst_var_sub::in,
	list(bound_inst)::out) is det.

alt_list_apply_substitution([], _, []).
alt_list_apply_substitution([Alt0|Alts0], Subst, [Alt|Alts]) :-
	Alt0 = functor(Name, Args0),
	inst_list_apply_substitution_2(Args0, Subst, Args),
	Alt = functor(Name, Args),
	alt_list_apply_substitution(Alts0, Subst, Alts).

:- pred ground_inst_info_apply_substitution(ground_inst_info::in,
	inst_var_sub::in, uniqueness::in, (inst)::out) is det.

ground_inst_info_apply_substitution(none, _, Uniq, ground(Uniq, none)).
ground_inst_info_apply_substitution(GII0, Subst, Uniq, ground(Uniq, GII)) :-
	GII0 = higher_order(pred_inst_info(PredOrFunc, Modes0, Det)),
	mode_list_apply_substitution(Modes0, Subst, Modes),
	GII = higher_order(pred_inst_info(PredOrFunc, Modes, Det)).

mode_list_apply_substitution(Modes0, Subst, Modes) :-
	( map__is_empty(Subst) ->
		Modes = Modes0
	;
		mode_list_apply_substitution_2(Modes0, Subst, Modes)
	).

:- pred mode_list_apply_substitution_2(list(mode)::in, inst_var_sub::in,
	list(mode)::out) is det.

mode_list_apply_substitution_2([], _, []).
mode_list_apply_substitution_2([A0 | As0], Subst, [A | As]) :-
	mode_apply_substitution(A0, Subst, A),
	mode_list_apply_substitution_2(As0, Subst, As).

%-----------------------------------------------------------------------------%

rename_apart_inst_vars(VarSet, NewVarSet, Modes0, Modes) :-
	varset__merge_subst(VarSet, NewVarSet, _, Sub),
	list__map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes).

:- pred rename_apart_inst_vars_in_mode(substitution(inst_var_type)::in,
	(mode)::in, (mode)::out) is det.

rename_apart_inst_vars_in_mode(Sub, I0 -> F0, I -> F) :-
	rename_apart_inst_vars_in_inst(Sub, I0, I),
	rename_apart_inst_vars_in_inst(Sub, F0, F).
rename_apart_inst_vars_in_mode(Sub, user_defined_mode(Name, Insts0),
		user_defined_mode(Name, Insts)) :-
	list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst(substitution(inst_var_type)::in,
	(inst)::in, (inst)::out) is det.

rename_apart_inst_vars_in_inst(_, any(U), any(U)).
rename_apart_inst_vars_in_inst(_, free, free).
rename_apart_inst_vars_in_inst(_, free(T), free(T)).
rename_apart_inst_vars_in_inst(Sub, bound(U, BIs0), bound(U, BIs)) :-
	list__map((pred(functor(C, Is0)::in, functor(C, Is)::out) is det :-
		list__map(rename_apart_inst_vars_in_inst(Sub), Is0, Is)),
		BIs0, BIs).
rename_apart_inst_vars_in_inst(Sub, ground(U, GI0), ground(U, GI)) :-
	(
		GI0 = higher_order(pred_inst_info(PoF, Modes0, Det)),
		list__map(rename_apart_inst_vars_in_mode(Sub), Modes0, Modes),
		GI = higher_order(pred_inst_info(PoF, Modes, Det))
	;
		GI0 = none,
		GI = none
	).
rename_apart_inst_vars_in_inst(_, not_reached, not_reached).
rename_apart_inst_vars_in_inst(Sub, inst_var(Var0), inst_var(Var)) :-
	( map__search(Sub, Var0, term__variable(Var1)) ->
		Var = Var1
	;
		Var = Var0
	).
rename_apart_inst_vars_in_inst(Sub, constrained_inst_vars(Vars0, Inst0),
		constrained_inst_vars(Vars, Inst)) :-
	rename_apart_inst_vars_in_inst(Sub, Inst0, Inst),
	Vars = set__map(func(Var0) =
		( map__search(Sub, Var0, term__variable(Var)) ->
			Var
		;
			Var0
		), Vars0).
rename_apart_inst_vars_in_inst(Sub, defined_inst(Name0), defined_inst(Name)) :-
	( rename_apart_inst_vars_in_inst_name(Sub, Name0, Name1) ->
		Name = Name1
	;
		Name = Name0
	).
rename_apart_inst_vars_in_inst(Sub, abstract_inst(Sym, Insts0),
		abstract_inst(Sym, Insts)) :-
	list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).

:- pred rename_apart_inst_vars_in_inst_name(substitution(inst_var_type)::in,
	inst_name::in, inst_name::out) is semidet.

rename_apart_inst_vars_in_inst_name(Sub, user_inst(Sym, Insts0),
		user_inst(Sym, Insts)) :-
	list__map(rename_apart_inst_vars_in_inst(Sub), Insts0, Insts).
rename_apart_inst_vars_in_inst_name(Sub, typed_inst(Type, Name0),
		typed_inst(Type, Name)) :-
	rename_apart_inst_vars_in_inst_name(Sub, Name0, Name).
rename_apart_inst_vars_in_inst_name(_, typed_ground(U, T), typed_ground(U, T)).

%-----------------------------------------------------------------------------%

functors_to_cons_ids([], []).
functors_to_cons_ids([Functor | Functors], [ConsId | ConsIds]) :-
        Functor = functor(ConsId, _ArgInsts),
        functors_to_cons_ids(Functors, ConsIds).

%-----------------------------------------------------------------------------%

get_arg_insts(not_reached, _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, not_reached, ArgInsts).
get_arg_insts(ground(Uniq, _PredInst), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, ground(Uniq, none), ArgInsts).
get_arg_insts(bound(_Uniq, List), ConsId, Arity, ArgInsts) :-
	( get_arg_insts_2(List, ConsId, ArgInsts0) ->
		ArgInsts = ArgInsts0
	;
		% the code is unreachable
		list__duplicate(Arity, not_reached, ArgInsts)
	).
get_arg_insts(free, _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).
get_arg_insts(free(_Type), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, free, ArgInsts).
get_arg_insts(any(Uniq), _ConsId, Arity, ArgInsts) :-
	list__duplicate(Arity, any(Uniq), ArgInsts).

:- pred get_arg_insts_2(list(bound_inst)::in, cons_id::in, list(inst)::out)
	is semidet.

get_arg_insts_2([BoundInst | BoundInsts], ConsId, ArgInsts) :-
	( BoundInst = functor(ConsId, ArgInsts0) ->
		ArgInsts = ArgInsts0
	;
		get_arg_insts_2(BoundInsts, ConsId, ArgInsts)
	).

	% In case we later decided to change the representation
	% of mode_ids.

mode_id_to_int(_ - X, X).

%-----------------------------------------------------------------------------%

	%
	% Predicates to make error messages more readable by stripping
	% "builtin:" module qualifiers from modes and insts.
	% The interesting part is strip_builtin_qualifier_from_sym_name;
	% the rest is basically just recursive traversals.
	%

strip_builtin_qualifiers_from_mode_list(Modes0, Modes) :-
	list__map(strip_builtin_qualifiers_from_mode, Modes0, Modes).

:- pred strip_builtin_qualifiers_from_mode((mode)::in, (mode)::out) is det.

strip_builtin_qualifiers_from_mode((Initial0 -> Final0), (Initial -> Final)) :-
	strip_builtin_qualifiers_from_inst(Initial0, Initial),
	strip_builtin_qualifiers_from_inst(Final0, Final).

strip_builtin_qualifiers_from_mode(user_defined_mode(SymName0, Insts0),
				user_defined_mode(SymName, Insts)) :-
	strip_builtin_qualifiers_from_inst_list(Insts0, Insts),
	strip_builtin_qualifier_from_sym_name(SymName0, SymName).

strip_builtin_qualifier_from_cons_id(ConsId0, ConsId) :-
	( ConsId0 = cons(Name0, Arity) ->
		strip_builtin_qualifier_from_sym_name(Name0, Name),
		ConsId = cons(Name, Arity)
	;
		ConsId = ConsId0
	).

:- pred strip_builtin_qualifier_from_sym_name(sym_name::in, sym_name::out)
	is det.

strip_builtin_qualifier_from_sym_name(SymName0, SymName) :-
	(
		SymName0 = qualified(Module, Name),
		mercury_public_builtin_module(Module)
	->
		SymName = unqualified(Name)
	;
		SymName = SymName0
	).

strip_builtin_qualifiers_from_inst_list(Insts0, Insts) :-
	list__map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

strip_builtin_qualifiers_from_inst(inst_var(V), inst_var(V)).
strip_builtin_qualifiers_from_inst(constrained_inst_vars(Vars, Inst0),
		constrained_inst_vars(Vars, Inst)) :-
	strip_builtin_qualifiers_from_inst(Inst0, Inst).
strip_builtin_qualifiers_from_inst(not_reached, not_reached).
strip_builtin_qualifiers_from_inst(free, free).
strip_builtin_qualifiers_from_inst(free(Type), free(Type)).
strip_builtin_qualifiers_from_inst(any(Uniq), any(Uniq)).
strip_builtin_qualifiers_from_inst(ground(Uniq, GII0), ground(Uniq, GII)) :-
	strip_builtin_qualifiers_from_ground_inst_info(GII0, GII).
strip_builtin_qualifiers_from_inst(bound(Uniq, BoundInsts0),
					bound(Uniq, BoundInsts)) :-
	strip_builtin_qualifiers_from_bound_inst_list(BoundInsts0, BoundInsts).
strip_builtin_qualifiers_from_inst(defined_inst(Name0), defined_inst(Name)) :-
	strip_builtin_qualifiers_from_inst_name(Name0, Name).
strip_builtin_qualifiers_from_inst(abstract_inst(Name0, Args0),
				abstract_inst(Name, Args)) :-
	strip_builtin_qualifier_from_sym_name(Name0, Name),
	strip_builtin_qualifiers_from_inst_list(Args0, Args).

:- pred strip_builtin_qualifiers_from_bound_inst_list(list(bound_inst)::in,
	list(bound_inst)::out) is det.

strip_builtin_qualifiers_from_bound_inst_list(Insts0, Insts) :-
	list__map(strip_builtin_qualifiers_from_bound_inst, Insts0, Insts).

:- pred strip_builtin_qualifiers_from_bound_inst(bound_inst::in,
					bound_inst::out) is det.
strip_builtin_qualifiers_from_bound_inst(BoundInst0, BoundInst) :-
	BoundInst0 = functor(ConsId0, Insts0),
	strip_builtin_qualifier_from_cons_id(ConsId0, ConsId),
	BoundInst = functor(ConsId, Insts),
	list__map(strip_builtin_qualifiers_from_inst, Insts0, Insts).

:- pred strip_builtin_qualifiers_from_inst_name(inst_name::in, inst_name::out)
	is det.

strip_builtin_qualifiers_from_inst_name(user_inst(SymName0, Insts0),
		user_inst(SymName, Insts)) :-
	strip_builtin_qualifier_from_sym_name(SymName0, SymName),
	strip_builtin_qualifiers_from_inst_list(Insts0, Insts).
strip_builtin_qualifiers_from_inst_name(merge_inst(InstA0, InstB0),
		merge_inst(InstA, InstB)) :-
	strip_builtin_qualifiers_from_inst(InstA0, InstA),
	strip_builtin_qualifiers_from_inst(InstB0, InstB).
strip_builtin_qualifiers_from_inst_name(unify_inst(Live, InstA0, InstB0, Real),
		unify_inst(Live, InstA, InstB, Real)) :-
	strip_builtin_qualifiers_from_inst(InstA0, InstA),
	strip_builtin_qualifiers_from_inst(InstB0, InstB).
strip_builtin_qualifiers_from_inst_name(
		ground_inst(InstName0, Live, Uniq, Real),
		ground_inst(InstName, Live, Uniq, Real)) :-
	strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(
		any_inst(InstName0, Live, Uniq, Real),
		any_inst(InstName, Live, Uniq, Real)) :-
	strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(shared_inst(InstName0),
		shared_inst(InstName)) :-
	strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(mostly_uniq_inst(InstName0),
		mostly_uniq_inst(InstName)) :-
	strip_builtin_qualifiers_from_inst_name(InstName0, InstName).
strip_builtin_qualifiers_from_inst_name(typed_ground(Uniq, Type),
		typed_ground(Uniq, Type)).
strip_builtin_qualifiers_from_inst_name(typed_inst(Type, InstName0),
		typed_inst(Type, InstName)) :-
	strip_builtin_qualifiers_from_inst_name(InstName0, InstName).

:- pred strip_builtin_qualifiers_from_ground_inst_info(ground_inst_info::in,
		ground_inst_info::out) is det.

strip_builtin_qualifiers_from_ground_inst_info(none, none).
strip_builtin_qualifiers_from_ground_inst_info(higher_order(Pred0),
		higher_order(Pred)) :-
	Pred0 = pred_inst_info(PorF, Modes0, Det),
	Pred = pred_inst_info(PorF, Modes, Det),
	strip_builtin_qualifiers_from_mode_list(Modes0, Modes).

%-----------------------------------------------------------------------------%
