%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: prog_util.
% Main author: fjh.

% Various utility predicates acting on the parse tree data structure defined
% in prog_data.m and prog_item.m

%-----------------------------------------------------------------------------%

:- module parse_tree.prog_util.
:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Given a symbol name, return its unqualified name.
    %
:- pred unqualify_name(sym_name::in, string::out) is det.

    % sym_name_get_module_name(SymName, ModName):
    %
    % Given a symbol name, return the module qualifiers(s).
    % Fails if the symbol is unqualified.
    %
:- pred sym_name_get_module_name(sym_name::in, module_name::out) is semidet.

    % sym_name_get_module_name(SymName, DefaultModName, ModName):
    %
    % Given a symbol name, return the module qualifier(s).
    % If the symbol is unqualified, then return the specified default
    % module name.
    %
:- pred sym_name_get_module_name(sym_name::in, module_name::in,
    module_name::out) is det.

    % match_sym_name(PartialSymName, CompleteSymName):
    %
    % Succeeds iff there is some sequence of module qualifiers
    % which when prefixed to PartialSymName gives CompleteSymName.
    %
:- pred match_sym_name(sym_name::in, sym_name::in) is semidet.

    % remove_sym_name_prefix(SymName0, Prefix, SymName)
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName0 has the given prefix
    %   and the unqualified part of SymName is the unqualified
    %       part of SymName0 with the prefix removed.
    %
:- pred remove_sym_name_prefix(sym_name, string, sym_name).
:- mode remove_sym_name_prefix(in, in, out) is semidet.
:- mode remove_sym_name_prefix(out, in, in) is det.

    % remove_sym_name_suffix(SymName0, Suffix, SymName)
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName0 has the given suffix
    %   and the unqualified part of SymName is the unqualified
    %       part of SymName0 with the suffix removed.
    %
:- pred remove_sym_name_suffix(sym_name::in, string::in, sym_name::out)
    is semidet.

    % add_sym_name_suffix(SymName0, Suffix, SymName)
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName is the unqualified
    %       part of SymName0 with the suffix added.
    %
:- pred add_sym_name_suffix(sym_name::in, string::in, sym_name::out) is det.

    % transform_sym_base_name(TransformFunc, SymName0) = SymName
    % succeeds iff
    %   SymName and SymName0 have the same module qualifier
    %   and the unqualified part of SymName is the result of applying
    %   TransformFunc to the unqualified part of SymName0.
    %
:- func transform_sym_base_name(func(string) = string, sym_name) = sym_name.

    % Given a possible module qualified sym_name and a list of
    % argument types and a context, construct a term. This is
    % used to construct types.
    %
:- pred construct_qualified_term(sym_name::in, list(term(T))::in,
    term(T)::out) is det.

:- pred construct_qualified_term(sym_name::in, list(term(T))::in,
    prog_context::in, term(T)::out) is det.

    % Given a sym_name return the top level qualifier of that name.
    %
:- func outermost_qualifier(sym_name) = string.

%-----------------------------------------------------------------------------%

    % adjust_func_arity(PredOrFunc, FuncArity, PredArity).
    %
    % We internally store the arity as the length of the argument
    % list including the return value, which is one more than the
    % arity of the function reported in error messages.
    %
:- pred adjust_func_arity(pred_or_func, int, int).
:- mode adjust_func_arity(in, in, out) is det.
:- mode adjust_func_arity(in, out, in) is det.

%-----------------------------------------------------------------------------%

    % make_pred_name_with_context(ModuleName, Prefix, PredOrFunc, PredName,
    %   Line, Counter, SymName).
    %
    % Create a predicate name with context, e.g. for introduced
    % lambda or deforestation predicates.
    %
:- pred make_pred_name(module_name::in, string::in, maybe(pred_or_func)::in,
    string::in, new_pred_id::in, sym_name::out) is det.

    % make_pred_name_with_context(ModuleName, Prefix, PredOrFunc, PredName,
    %   Line, Counter, SymName).
    %
    % Create a predicate name with context, e.g. for introduced
    % lambda or deforestation predicates.
    %
:- pred make_pred_name_with_context(module_name::in, string::in,
    pred_or_func::in, string::in, int::in, int::in, sym_name::out) is det.

:- type new_pred_id
    --->    counter(int, int)                   % Line number, Counter
    ;       type_subst(tvarset, type_subst)
    ;       unused_args(list(int)).

%-----------------------------------------------------------------------------%

    % A pred declaration may contains just types, as in
    %   :- pred list.append(list(T), list(T), list(T)).
    % or it may contain both types and modes, as in
    %   :- pred list.append(list(T)::in, list(T)::in, list(T)::output).
    %
    % This predicate takes the argument list of a pred declaration, splits it
    % into two separate lists for the types and (if present) the modes.

:- type maybe_modes == maybe(list(mer_mode)).

:- pred split_types_and_modes(list(type_and_mode)::in, list(mer_type)::out,
    maybe_modes::out) is det.

:- pred split_type_and_mode(type_and_mode::in, mer_type::out,
    maybe(mer_mode)::out) is det.

%-----------------------------------------------------------------------------%

    % Perform a substitution on a goal.
    %
:- pred rename_in_goal(prog_var::in, prog_var::in,
    goal::in, goal::out) is det.

%-----------------------------------------------------------------------------%

    % Various predicates for accessing the cons_id type.

    % Given a cons_id and a list of argument terms, convert it into a
    % term. Fails if the cons_id is a pred_const, or type_ctor_info_const.
    %
:- pred cons_id_and_args_to_term(cons_id::in, list(term(T))::in, term(T)::out)
    is semidet.

    % Get the arity of a cons_id, aborting on pred_const and
    % type_ctor_info_const.
    %
:- func cons_id_arity(cons_id) = arity.

    % Get the arity of a cons_id. Return a `no' on those cons_ids
    % where cons_id_arity/2 would normally abort.
    %
:- func cons_id_maybe_arity(cons_id) = maybe(arity).

    % The reverse conversion - make a cons_id for a functor.
    % Given a const and an arity for the functor, create a cons_id.
    %
:- func make_functor_cons_id(const, arity) = cons_id.

    % Another way of making a cons_id from a functor.
    % Given the name, argument types, and type_ctor of a functor,
    % create a cons_id for that functor.
    %
:- func make_cons_id(sym_name, list(constructor_arg), type_ctor) = cons_id.

    % Another way of making a cons_id from a functor.
    % Given the name, argument types, and type_ctor of a functor,
    % create a cons_id for that functor.
    %
    % Differs from make_cons_id in that (a) it requires the sym_name
    % to be already module qualified, which means that it does not
    % need the module qualification of the type, (b) it can compute the
    % arity from any list of the right length.
    %
:- func make_cons_id_from_qualified_sym_name(sym_name, list(_)) = cons_id.

%-----------------------------------------------------------------------------%

    % make_n_fresh_vars(Name, N, VarSet0, Vars, VarSet):
    %   `Vars' is a list of `N' fresh variables allocated from
    %   `VarSet0'.  The variables will be named "<Name>1", "<Name>2",
    %   "<Name>3", and so on, where <Name> is the value of `Name'.
    %   `VarSet' is the resulting varset.
    %
:- pred make_n_fresh_vars(string::in, int::in, list(var(T))::out,
    varset(T)::in, varset(T)::out) is det.

    % Given the list of predicate arguments for a predicate that
    % is really a function, split that list into the function arguments
    % and the function return type.
    %
:- pred pred_args_to_func_args(list(T)::in, list(T)::out, T::out) is det.

    % Get the last two arguments from the list, failing if there
    % aren't at least two arguments.
    %
:- pred get_state_args(list(T)::in, list(T)::out, T::out, T::out) is semidet.

    % Get the last two arguments from the list, aborting if there
    % aren't at least two arguments.
    %
:- pred get_state_args_det(list(T)::in, list(T)::out, T::out, T::out) is det.

    % Parse a term of the form `Head :- Body', treating a term not in that form
    % as `Head :- true'.
    %
:- pred parse_rule_term(term.context::in, term(T)::in, term(T)::out,
    term(T)::out) is det.

%-----------------------------------------------------------------------------%

    % Add new type variables for those introduced by a type qualification.
    %
:- pred get_new_tvars(list(tvar)::in, tvarset::in, tvarset::in, tvarset::out,
    tvar_name_map::in, tvar_name_map::out,
    tvar_renaming::in, tvar_renaming::out) is det.

    % substitute_vars(Vars0, Subst, Vars):
    %
    % Apply substitution `Subst' (which must only rename vars) to `Vars0',
    % and return the result in `Vars'.
    %
:- pred substitute_vars(list(var(T))::in, substitution(T)::in,
    list(var(T))::out) is det.

%-----------------------------------------------------------------------------%

    % We need to "unparse" the sym_name to construct the properly
    % module qualified term.
    %
:- func sym_name_and_args_to_term(sym_name, list(term(T)), prog_context) =
    term(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_out.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module string.
:- import_module svmap.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

unqualify_name(unqualified(PredName), PredName).
unqualify_name(qualified(_ModuleName, PredName), PredName).

sym_name_get_module_name(unqualified(_), _) :- fail.
sym_name_get_module_name(qualified(ModuleName, _), ModuleName).

sym_name_get_module_name(unqualified(_), ModuleName, ModuleName).
sym_name_get_module_name(qualified(ModuleName, _PredName), _, ModuleName).

construct_qualified_term(qualified(Module, Name), Args, Context, Term) :-
    construct_qualified_term(Module, [], Context, ModuleTerm),
    UnqualifiedTerm = term.functor(term.atom(Name), Args, Context),
    Term = term.functor(term.atom("."),
        [ModuleTerm, UnqualifiedTerm], Context).
construct_qualified_term(unqualified(Name), Args, Context, Term) :-
    Term = term.functor(term.atom(Name), Args, Context).

construct_qualified_term(SymName, Args, Term) :-
    term.context_init(Context),
    construct_qualified_term(SymName, Args, Context, Term).

outermost_qualifier(unqualified(Name)) = Name.
outermost_qualifier(qualified(Module, _Name)) = outermost_qualifier(Module).

%-----------------------------------------------------------------------------%

adjust_func_arity(predicate, Arity, Arity).
adjust_func_arity(function, Arity - 1, Arity).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
    split_types_and_modes_2(TypesAndModes, yes, Types, Modes, Result),
    ( Result = yes ->
        MaybeModes = yes(Modes)
    ;
        MaybeModes = no
    ).

:- pred split_types_and_modes_2(list(type_and_mode)::in, bool::in,
    list(mer_type)::out, list(mer_mode)::out, bool::out) is det.

    % T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM|TMs], Result0, [T|Ts], [M|Ms], Result) :-
    split_type_and_mode(TM, Result0, T, M, Result1),
    split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

    % If a pred declaration specifies modes for some but not all of the
    % arguments, then the modes are ignored - should this be an error instead?
    % trd: this should never happen because prog_io.m will detect these cases.
    %
:- pred split_type_and_mode(type_and_mode::in, bool::in,
    mer_type::out, mer_mode::out, bool::out) is det.

split_type_and_mode(type_only(T), _, T, (free -> free), no).
split_type_and_mode(type_and_mode(T,M), R, T, M, R).

split_type_and_mode(type_only(T), T, no).
split_type_and_mode(type_and_mode(T,M), T, yes(M)).

%-----------------------------------------------------------------------------%

rename_in_goal(OldVar, NewVar, Goal0 - Context, Goal - Context) :-
    rename_in_goal_expr(OldVar, NewVar, Goal0, Goal).

:- pred rename_in_goal_expr(prog_var::in, prog_var::in,
    goal_expr::in, goal_expr::out) is det.

rename_in_goal_expr(OldVar, NewVar, conj_expr(GoalA0, GoalB0),
        conj_expr(GoalA, GoalB)) :-
    rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
    rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
rename_in_goal_expr(OldVar, NewVar, par_conj_expr(GoalA0, GoalB0),
        par_conj_expr(GoalA, GoalB)) :-
    rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
    rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
rename_in_goal_expr(_OldVar, _NewVar, true_expr, true_expr).
rename_in_goal_expr(OldVar, NewVar, disj_expr(GoalA0, GoalB0),
        disj_expr(GoalA, GoalB)) :-
    rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
    rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
rename_in_goal_expr(_Var, _NewVar, fail_expr, fail_expr).
rename_in_goal_expr(OldVar, NewVar, not_expr(Goal0), not_expr(Goal)) :-
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar, some_expr(Vars0, Goal0),
        some_expr(Vars, Goal)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar, some_state_vars_expr(Vars0, Goal0),
        some_state_vars_expr(Vars, Goal)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar, all_expr(Vars0, Goal0),
        all_expr(Vars, Goal)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar, all_state_vars_expr(Vars0, Goal0),
        all_state_vars_expr(Vars, Goal)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar,
        promise_purity_expr(Implicit, Purity, Goal0),
        promise_purity_expr(Implicit, Purity, Goal)) :-
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar,
        promise_equivalent_solutions_expr(Vars0, DotSVars0, ColonSVars0,
        Goal0),
        promise_equivalent_solutions_expr(Vars, DotSVars, ColonSVars,
        Goal)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_vars(OldVar, NewVar, DotSVars0, DotSVars),
    rename_in_vars(OldVar, NewVar, ColonSVars0, ColonSVars),
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar,
        promise_equivalent_solution_sets_expr(Vars0, DotSVars0, ColonSVars0,
        Goal0),
        promise_equivalent_solution_sets_expr(Vars, DotSVars, ColonSVars,
        Goal)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_vars(OldVar, NewVar, DotSVars0, DotSVars),
    rename_in_vars(OldVar, NewVar, ColonSVars0, ColonSVars),
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar,
        promise_equivalent_solution_arbitrary_expr(Vars0,
            DotSVars0, ColonSVars0, Goal0),
        promise_equivalent_solution_arbitrary_expr(Vars,
            DotSVars, ColonSVars, Goal)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_vars(OldVar, NewVar, DotSVars0, DotSVars),
    rename_in_vars(OldVar, NewVar, ColonSVars0, ColonSVars),
    rename_in_goal(OldVar, NewVar, Goal0, Goal).
rename_in_goal_expr(OldVar, NewVar, implies_expr(GoalA0, GoalB0),
        implies_expr(GoalA, GoalB)) :-
    rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
    rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
rename_in_goal_expr(OldVar, NewVar, equivalent_expr(GoalA0, GoalB0),
        equivalent_expr(GoalA, GoalB)) :-
    rename_in_goal(OldVar, NewVar, GoalA0, GoalA),
    rename_in_goal(OldVar, NewVar, GoalB0, GoalB).
rename_in_goal_expr(OldVar, NewVar,
        if_then_else_expr(Vars0, StateVars0, Cond0, Then0, Else0),
        if_then_else_expr(Vars, StateVars, Cond, Then, Else)) :-
    rename_in_vars(OldVar, NewVar, Vars0, Vars),
    rename_in_vars(OldVar, NewVar, StateVars0, StateVars),
    rename_in_goal(OldVar, NewVar, Cond0, Cond),
    rename_in_goal(OldVar, NewVar, Then0, Then),
    rename_in_goal(OldVar, NewVar, Else0, Else).
rename_in_goal_expr(OldVar, NewVar, call_expr(SymName, Terms0, Purity),
        call_expr(SymName, Terms, Purity)) :-
    term.substitute_list(Terms0, OldVar, term.variable(NewVar), Terms).
rename_in_goal_expr(OldVar, NewVar, unify_expr(TermA0, TermB0, Purity),
        unify_expr(TermA, TermB, Purity)) :-
    term.substitute(TermA0, OldVar, term.variable(NewVar), TermA),
    term.substitute(TermB0, OldVar, term.variable(NewVar), TermB).

:- pred rename_in_vars(prog_var::in, prog_var::in,
    list(prog_var)::in, list(prog_var)::out) is det.

rename_in_vars(_, _, [], []).
rename_in_vars(OldVar, NewVar, [Var0 | Vars0], [Var | Vars]) :-
    ( Var0 = OldVar ->
        Var = NewVar
    ;
        Var = Var0
    ),
    rename_in_vars(OldVar, NewVar, Vars0, Vars).

%-----------------------------------------------------------------------------%

    % match_sym_name(PartialSymName, CompleteSymName):
    %
    % Succeeds iff there is some sequence of module qualifiers
    % which when prefixed to PartialSymName gives CompleteSymName.
    %
match_sym_name(qualified(Module1, Name), qualified(Module2, Name)) :-
    match_sym_name(Module1, Module2).
match_sym_name(unqualified(Name), unqualified(Name)).
match_sym_name(unqualified(Name), qualified(_, Name)).

%-----------------------------------------------------------------------------%

remove_sym_name_prefix(qualified(Module, Name0), Prefix,
        qualified(Module, Name)) :-
    string.append(Prefix, Name, Name0).
remove_sym_name_prefix(unqualified(Name0), Prefix, unqualified(Name)) :-
    string.append(Prefix, Name, Name0).

remove_sym_name_suffix(qualified(Module, Name0), Suffix,
        qualified(Module, Name)) :-
    string.remove_suffix(Name0, Suffix, Name).
remove_sym_name_suffix(unqualified(Name0), Suffix, unqualified(Name)) :-
    string.remove_suffix(Name0, Suffix, Name).

add_sym_name_suffix(qualified(Module, Name0), Suffix,
        qualified(Module, Name)) :-
    string.append(Name0, Suffix, Name).
add_sym_name_suffix(unqualified(Name0), Suffix, unqualified(Name)) :-
    string.append(Name0, Suffix, Name).

transform_sym_base_name(TransformFunc, qualified(Module, Name0)) =
        qualified(Module, TransformFunc(Name0)).
transform_sym_base_name(TransformFunc, unqualified(Name0)) =
        unqualified(TransformFunc(Name0)).

%-----------------------------------------------------------------------------%

make_pred_name_with_context(ModuleName, Prefix,
        PredOrFunc, PredName, Line, Counter, SymName) :-
    make_pred_name(ModuleName, Prefix, yes(PredOrFunc), PredName,
        counter(Line, Counter), SymName).

make_pred_name(ModuleName, Prefix, MaybePredOrFunc, PredName,
        NewPredId, SymName) :-
    (
        MaybePredOrFunc = yes(PredOrFunc),
        (
            PredOrFunc = predicate,
            PFS = "pred"
        ;
            PredOrFunc = function,
            PFS = "func"
        )
    ;
        MaybePredOrFunc = no,
        PFS = "pred_or_func"
    ),
    (
        NewPredId = counter(Line, Counter),
        string.format("%d__%d", [i(Line), i(Counter)], PredIdStr)
    ;
        NewPredId = type_subst(VarSet, TypeSubst),
        SubstToString = (pred(SubstElem::in, SubstStr::out) is det :-
            SubstElem = Var - Type,
            varset.lookup_name(VarSet, Var, VarName),
            TypeString = mercury_type_to_string(VarSet, no, Type),
            string.append_list([VarName, " = ", TypeString], SubstStr)
        ),
        list_to_string(SubstToString, TypeSubst, PredIdStr)
    ;
        NewPredId = unused_args(Args),
        list_to_string(int_to_string, Args, PredIdStr)
    ),

    string.format("%s__%s__%s__%s",
        [s(Prefix), s(PFS), s(PredName), s(PredIdStr)], Name),
        SymName = qualified(ModuleName, Name).

:- pred list_to_string(pred(T, string)::in(pred(in, out) is det),
    list(T)::in, string::out) is det.

list_to_string(Pred, List, String) :-
    list_to_string_2(Pred, List, Strings, ["]"]),
    string.append_list(["[" | Strings], String).

:- pred list_to_string_2(pred(T, string)::in(pred(in, out) is det),
    list(T)::in, list(string)::out, list(string)::in) is det.

list_to_string_2(_, []) --> [].
list_to_string_2(Pred, [T | Ts]) -->
    { call(Pred, T, String) },
    [String],
    ( { Ts = [] } ->
        []
    ;
        [", "],
        list_to_string_2(Pred, Ts)
    ).

%-----------------------------------------------------------------------------%

cons_id_and_args_to_term(int_const(Int), [], Term) :-
    term.context_init(Context),
    Term = term.functor(term.integer(Int), [], Context).
cons_id_and_args_to_term(float_const(Float), [], Term) :-
    term.context_init(Context),
    Term = term.functor(term.float(Float), [], Context).
cons_id_and_args_to_term(string_const(String), [], Term) :-
    term.context_init(Context),
    Term = term.functor(term.string(String), [], Context).
cons_id_and_args_to_term(cons(SymName, _Arity), Args, Term) :-
    construct_qualified_term(SymName, Args, Term).

cons_id_arity(cons(_, Arity)) = Arity.
cons_id_arity(int_const(_)) = 0.
cons_id_arity(string_const(_)) = 0.
cons_id_arity(float_const(_)) = 0.
cons_id_arity(pred_const(_, _)) =
    unexpected(this_file, "cons_id_arity: can't get arity of pred_const").
cons_id_arity(type_ctor_info_const(_, _, _)) =
    unexpected(this_file,
        "cons_id_arity: can't get arity of type_ctor_info_const").
cons_id_arity(base_typeclass_info_const(_, _, _, _)) =
    unexpected(this_file, "cons_id_arity: " ++
        "can't get arity of base_typeclass_info_const").
cons_id_arity(type_info_cell_constructor(_)) =
    unexpected(this_file, "cons_id_arity: " ++
        "can't get arity of type_info_cell_constructor").
cons_id_arity(typeclass_info_cell_constructor) =
    unexpected(this_file, "cons_id_arity: " ++
        "can't get arity of typeclass_info_cell_constructor").
cons_id_arity(tabling_info_const(_)) =
    unexpected(this_file,
        "cons_id_arity: can't get arity of tabling_info_const").
cons_id_arity(deep_profiling_proc_layout(_)) =
    unexpected(this_file, "cons_id_arity: " ++
        "can't get arity of deep_profiling_proc_layout").
cons_id_arity(table_io_decl(_)) =
    unexpected(this_file, "cons_id_arity: can't get arity of table_io_decl").

cons_id_maybe_arity(cons(_, Arity)) = yes(Arity).
cons_id_maybe_arity(int_const(_)) = yes(0).
cons_id_maybe_arity(string_const(_)) = yes(0).
cons_id_maybe_arity(float_const(_)) = yes(0).
cons_id_maybe_arity(pred_const(_, _)) = no.
cons_id_maybe_arity(type_ctor_info_const(_, _, _)) = no.
cons_id_maybe_arity(base_typeclass_info_const(_, _, _, _)) = no.
cons_id_maybe_arity(type_info_cell_constructor(_)) = no.
cons_id_maybe_arity(typeclass_info_cell_constructor) = no.
cons_id_maybe_arity(tabling_info_const(_)) = no.
cons_id_maybe_arity(deep_profiling_proc_layout(_)) = no.
cons_id_maybe_arity(table_io_decl(_)) = no.

make_functor_cons_id(term.atom(Name), Arity) = cons(unqualified(Name), Arity).
make_functor_cons_id(term.integer(Int), _) = int_const(Int).
make_functor_cons_id(term.string(String), _) = string_const(String).
make_functor_cons_id(term.float(Float), _) = float_const(Float).

make_cons_id(SymName0, Args, TypeCtor) = cons(SymName, Arity) :-
    % Use the module qualifier on the SymName, if there is one,
    % otherwise use the module qualifier on the Type, if there is one,
    % otherwise leave it unqualified.
    % XXX is that the right thing to do?
    (
        SymName0 = qualified(_, _),
        SymName = SymName0
    ;
        SymName0 = unqualified(ConsName),
        (
            TypeCtor = type_ctor(unqualified(_), _),
            SymName = SymName0
        ;
            TypeCtor = type_ctor(qualified(TypeModule, _), _),
            SymName = qualified(TypeModule, ConsName)
        )
    ),
    list.length(Args, Arity).

make_cons_id_from_qualified_sym_name(SymName, Args) = cons(SymName, Arity) :-
    list.length(Args, Arity).

%-----------------------------------------------------------------------------%

make_n_fresh_vars(BaseName, N, Vars, VarSet0, VarSet) :-
    make_n_fresh_vars_2(BaseName, 0, N, Vars, VarSet0, VarSet).

:- pred make_n_fresh_vars_2(string::in, int::in, int::in, list(var(T))::out,
    varset(T)::in, varset(T)::out) is det.

make_n_fresh_vars_2(BaseName, N, Max, Vars, !VarSet) :-
    ( N = Max ->
        Vars = []
    ;
        N1 = N + 1,
        varset.new_var(!.VarSet, Var, !:VarSet),
        string.int_to_string(N1, Num),
        string.append(BaseName, Num, VarName),
        varset.name_var(!.VarSet, Var, VarName, !:VarSet),
        Vars = [Var | Vars1],
        make_n_fresh_vars_2(BaseName, N1, Max, Vars1, !VarSet)
    ).

pred_args_to_func_args(PredArgs, FuncArgs, FuncReturn) :-
    list.length(PredArgs, NumPredArgs),
    NumFuncArgs = NumPredArgs - 1,
    ( list.split_list(NumFuncArgs, PredArgs, FuncArgs0, [FuncReturn0]) ->
        FuncArgs = FuncArgs0,
        FuncReturn = FuncReturn0
    ;
        unexpected(this_file,
            "pred_args_to_func_args: function missing return value?")
    ).

get_state_args(Args0, Args, State0, State) :-
    list.reverse(Args0, RevArgs0),
    RevArgs0 = [State, State0 | RevArgs],
    list.reverse(RevArgs, Args).

get_state_args_det(Args0, Args, State0, State) :-
    ( get_state_args(Args0, Args1, State0A, StateA) ->
        Args = Args1,
        State0 = State0A,
        State = StateA
    ;
        unexpected(this_file, "get_state_args_det")
    ).

%-----------------------------------------------------------------------------%

parse_rule_term(Context, RuleTerm, HeadTerm, GoalTerm) :-
    ( RuleTerm = term.functor(term.atom(":-"), [HeadTerm0, GoalTerm0], _) ->
        HeadTerm = HeadTerm0,
        GoalTerm = GoalTerm0
    ;
        HeadTerm = RuleTerm,
        GoalTerm = term.functor(term.atom("true"), [], Context)
    ).

get_new_tvars([], _,  !TVarSet, !TVarNameMap, !TVarRenaming).
get_new_tvars([TVar | TVars], VarSet, !TVarSet, !TVarNameMap, !TVarRenaming) :-
    ( map.contains(!.TVarRenaming, TVar) ->
        true
    ;
        ( varset.search_name(VarSet, TVar, TVarName) ->
            ( map.search(!.TVarNameMap, TVarName, TVarSetVar) ->
                svmap.det_insert(TVar, TVarSetVar, !TVarRenaming)
            ;
                varset.new_var(!.TVarSet, NewTVar, !:TVarSet),
                varset.name_var(!.TVarSet, NewTVar, TVarName, !:TVarSet),
                svmap.det_insert(TVarName, NewTVar, !TVarNameMap),
                svmap.det_insert(TVar, NewTVar, !TVarRenaming)
            )
        ;
            varset.new_var(!.TVarSet, NewTVar, !:TVarSet),
            svmap.det_insert(TVar, NewTVar, !TVarRenaming)
        )
    ),
    get_new_tvars(TVars, VarSet, !TVarSet, !TVarNameMap, !TVarRenaming).

%-----------------------------------------------------------------------------%

substitute_vars(Vars0, Subst, Vars) :-
    Vars = list.map(substitute_var(Subst), Vars0).

:- func substitute_var(substitution(T), var(T)) = var(T).

substitute_var(Subst, Var0) = Var :-
    term.apply_substitution(term.variable(Var0), Subst, Term),
    ( Term = term.variable(Var1) ->
        Var = Var1
    ;
        unexpected(this_file, "substitute_var: invalid substitution")
    ).

%-----------------------------------------------------------------------------%

sym_name_and_args_to_term(unqualified(Name), Xs, Context) =
    term.functor(term.atom(Name), Xs, Context).

sym_name_and_args_to_term(qualified(ModuleNames, Name), Xs, Context) =
    sym_name_and_term_to_term(ModuleNames,
        term.functor(term.atom(Name), Xs, Context), Context).

:- func sym_name_and_term_to_term(module_specifier, term(T), prog_context) =
    term(T).

sym_name_and_term_to_term(unqualified(ModuleName), Term, Context) =
    term.functor(
        term.atom("."),
        [term.functor(term.atom(ModuleName), [], Context), Term],
        Context
    ).
sym_name_and_term_to_term(qualified(ModuleNames, ModuleName), Term, Context) =
    term.functor(
        term.atom("."),
        [sym_name_and_term_to_term(
            ModuleNames,
            term.functor(term.atom(ModuleName), [], Context),
            Context),
        Term],
        Context
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_util.m".

%-----------------------------------------------------------------------------%
:- end_module prog_util.
%-----------------------------------------------------------------------------%
