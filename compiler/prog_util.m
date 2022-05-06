%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2001, 2003-2012 The University of Melbourne.
% Copyright (C) 2014-2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_util.
% Main author: fjh.
%
% Various utility predicates acting on the parse tree data structure.
%
%-----------------------------------------------------------------------------%

:- module parse_tree.prog_util.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module integer.
:- import_module list.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % Given a possible module qualified sym_name and a list of argument types
    % and a context, construct a term. This is used to construct types.
    %
:- pred construct_qualified_term(sym_name::in, list(term(T))::in,
    term(T)::out) is det.
:- pred construct_qualified_term_with_context(sym_name::in, list(term(T))::in,
    prog_context::in, term(T)::out) is det.

%-----------------------------------------------------------------------------%

    % adjust_func_arity(PredOrFunc, FuncArity, PredArity).
    %
    % We internally store the arity as the length of the argument list
    % including the return value, which is one more than the arity
    % of the function reported in error messages.
    %
:- pred adjust_func_arity(pred_or_func, arity, arity).
:- mode adjust_func_arity(in, in, out) is det.
:- mode adjust_func_arity(in, out, in) is det.

:- pred user_arity_pred_form_arity(pred_or_func, user_arity, pred_form_arity).
:- mode user_arity_pred_form_arity(in, in, out) is det.
:- mode user_arity_pred_form_arity(in, out, in) is det.

%-----------------------------------------------------------------------------%

    % A pred declaration may contains just types, as in
    %   :- pred list.append(list(T), list(T), list(T)).
    % or it may contain both types and modes, as in
    %   :- pred list.append(list(T)::in, list(T)::in, list(T)::output).
    %
    % This predicate takes the argument list of a pred declaration, splits it
    % into two separate lists for the types and (if present) the modes.
    %
:- pred split_types_and_modes(list(type_and_mode)::in, list(mer_type)::out,
    maybe(list(mer_mode))::out) is det.

:- pred split_type_and_mode(type_and_mode::in, mer_type::out,
    maybe(mer_mode)::out) is det.

%-----------------------------------------------------------------------------%

    % Perform a substitution on a goal.
    %
:- pred rename_in_goal(prog_var::in, prog_var::in, goal::in, goal::out) is det.

%-----------------------------------------------------------------------------%

    % Various predicates for accessing the cons_id type.

    % Given a cons_id and a list of argument terms, convert it into a term.
    % Works only on the cons_ids that can be expressed in source programs,
    % so it fails e.g. on pred_consts and type_ctor_info_consts.
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

    % source_integer_to_int(Base, Integer, Int):
    %
    % Convert an arbitrary precision integer to a native int. For base 10, this
    % predicate succeeds iff the value of Integer does not exceed int.max_int.
    % For other bases, this predicate succeeds iff the value of Integer can be
    % represented by an unsigned integer of the same width as `int', and `Int'
    % is the signed integer with the same bit pattern as that unsigned value.
    % The rationale for this behaviour is that non base 10 integers are assumed
    % to denote bit patterns and that in Mercury source files it is useful to
    % be able to write values with the high bit set (e.g. 0x80000000 on 32-bit
    % machines) that would be greater than max_int if interpreted as a positive
    % integer.
    %
    % XXX UINT - we should revisit the above behaviour once support for
    % unsigned integers is stable.
    %
:- pred source_integer_to_int(integer_base::in, integer::in, int::out)
    is semidet.

%-----------------------------------------------------------------------------%

    % Strip the module qualifier from the given cons_id or sym_name.
    %
:- pred strip_module_qualifier_from_cons_id(cons_id::in, cons_id::out) is det.
:- pred strip_module_qualifier_from_sym_name(sym_name::in, sym_name::out)
    is det.

    % Strip the module qualifier from the given cons_id or sym_name, but
    % only if the module named by that qualifier is the public builtin module.
    %
:- pred strip_builtin_qualifier_from_cons_id(cons_id::in, cons_id::out) is det.
:- pred strip_builtin_qualifier_from_sym_name(sym_name::in, sym_name::out)
    is det.

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

%-----------------------------------------------------------------------------%

    % Add new type variables for those introduced by a type qualification.
    %
:- pred get_new_tvars(list(tvar)::in, tvarset::in, tvarset::in, tvarset::out,
    tvar_name_map::in, tvar_name_map::out,
    tvar_renaming::in, tvar_renaming::out) is det.

%-----------------------------------------------------------------------------%

    % Convert a list of goals into a conjunction.
    %
:- func goal_list_to_conj(prog_context, list(goal)) = goal.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module mdbcomp.builtin_modules.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

construct_qualified_term(SymName, ArgTerms, Term) :-
    term.context_init(Context),
    construct_qualified_term_with_context(SymName, ArgTerms, Context, Term).

construct_qualified_term_with_context(SymName, ArgTerms, Context, Term) :-
    (
        SymName = qualified(ModuleSymName, Name),
        construct_qualified_term_with_context(ModuleSymName, [], Context,
            ModuleTerm),
        UnqualifiedTerm = term.functor(term.atom(Name), ArgTerms, Context),
        Term = term.functor(term.atom("."),
            [ModuleTerm, UnqualifiedTerm], Context)
    ;
        SymName = unqualified(Name),
        Term = term.functor(term.atom(Name), ArgTerms, Context)
    ).

%-----------------------------------------------------------------------------%

adjust_func_arity(pf_predicate, Arity, Arity).
adjust_func_arity(pf_function, Arity - 1, Arity).

user_arity_pred_form_arity(pf_predicate, user_arity(A), pred_form_arity(A)).
user_arity_pred_form_arity(pf_function, user_arity(A - 1), pred_form_arity(A)).

%-----------------------------------------------------------------------------%

split_types_and_modes(TypesAndModes, Types, MaybeModes) :-
    split_types_and_modes_2(TypesAndModes, yes, Types, Modes, Result),
    (
        Result = yes,
        MaybeModes = yes(Modes)
    ;
        Result = no,
        MaybeModes = no
    ).

:- pred split_types_and_modes_2(list(type_and_mode)::in, bool::in,
    list(mer_type)::out, list(mer_mode)::out, bool::out) is det.

    % T = type, M = mode, TM = combined type and mode
split_types_and_modes_2([], Result, [], [], Result).
split_types_and_modes_2([TM | TMs], Result0, [T | Ts], [M | Ms], Result) :-
    split_type_and_mode(TM, Result0, T, M, Result1),
    split_types_and_modes_2(TMs, Result1, Ts, Ms, Result).

    % If a pred declaration specifies modes for some but not all of the
    % arguments, then the modes are ignored - should this be an error instead?
    % trd: this should never happen because the parser will detect these cases.
    %
:- pred split_type_and_mode(type_and_mode::in, bool::in,
    mer_type::out, mer_mode::out, bool::out) is det.

split_type_and_mode(type_only(T), _, T, from_to_mode(free, free), no).
split_type_and_mode(type_and_mode(T, M), R, T, M, R).

split_type_and_mode(type_only(T), T, no).
split_type_and_mode(type_and_mode(T, M), T, yes(M)).

%-----------------------------------------------------------------------------%

rename_in_goal(OldVar, NewVar, Goal0, Goal) :-
    (
        ( Goal0 = true_expr(_Context)
        ; Goal0 = fail_expr(_Context)
        ),
        Goal = Goal0
    ;
        Goal0 = conj_expr(Context, SubGoalA0, SubGoalB0),
        rename_in_goal(OldVar, NewVar, SubGoalA0, SubGoalA),
        rename_in_goal(OldVar, NewVar, SubGoalB0, SubGoalB),
        Goal = conj_expr(Context, SubGoalA, SubGoalB)
    ;
        Goal0 = par_conj_expr(Context, SubGoalA0, SubGoalB0),
        rename_in_goal(OldVar, NewVar, SubGoalA0, SubGoalA),
        rename_in_goal(OldVar, NewVar, SubGoalB0, SubGoalB),
        Goal = par_conj_expr(Context, SubGoalA, SubGoalB)
    ;
        Goal0 = disj_expr(Context, SubGoalA0, SubGoalB0, SubGoals0),
        rename_in_goal(OldVar, NewVar, SubGoalA0, SubGoalA),
        rename_in_goal(OldVar, NewVar, SubGoalB0, SubGoalB),
        rename_in_goals(OldVar, NewVar, SubGoals0, SubGoals),
        Goal = disj_expr(Context, SubGoalA, SubGoalB, SubGoals)
    ;
        Goal0 = not_expr(Context, SubGoal0),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = not_expr(Context, SubGoal)
    ;
        Goal0 = quant_expr(QuantType, QuantVarsKind, Context, Vars0, SubGoal0),
        rename_in_vars(OldVar, NewVar, Vars0, Vars),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = quant_expr(QuantType, QuantVarsKind, Context, Vars, SubGoal)
    ;
        Goal0 = promise_purity_expr(Context, Purity, SubGoal0),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = promise_purity_expr(Context, Purity, SubGoal)
    ;
        Goal0 = promise_equivalent_solutions_expr(Context,
            Vars0, StateVars0, DotSVars0, ColonSVars0, SubGoal0),
        rename_in_vars(OldVar, NewVar, Vars0, Vars),
        rename_in_vars(OldVar, NewVar, StateVars0, StateVars),
        rename_in_vars(OldVar, NewVar, DotSVars0, DotSVars),
        rename_in_vars(OldVar, NewVar, ColonSVars0, ColonSVars),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = promise_equivalent_solutions_expr(Context,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal)
    ;
        Goal0 = promise_equivalent_solution_sets_expr(Context,
            Vars0, StateVars0, DotSVars0, ColonSVars0, SubGoal0),
        rename_in_vars(OldVar, NewVar, Vars0, Vars),
        rename_in_vars(OldVar, NewVar, StateVars0, StateVars),
        rename_in_vars(OldVar, NewVar, DotSVars0, DotSVars),
        rename_in_vars(OldVar, NewVar, ColonSVars0, ColonSVars),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = promise_equivalent_solution_sets_expr(Context,
            Vars, StateVars, DotSVars, ColonSVars, SubGoal)
    ;
        Goal0 = promise_equivalent_solution_arbitrary_expr(Context,
            Vars0, StateVars0, DotSVars0, ColonSVars0, SubGoal0),
        rename_in_vars(OldVar, NewVar, Vars0, Vars),
        rename_in_vars(OldVar, NewVar, StateVars0, StateVars),
        rename_in_vars(OldVar, NewVar, DotSVars0, DotSVars),
        rename_in_vars(OldVar, NewVar, ColonSVars0, ColonSVars),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = promise_equivalent_solution_arbitrary_expr(Context,
            Vars, StateVars,
            DotSVars, ColonSVars, SubGoal)
    ;
        Goal0 = disable_warnings_expr(Context, HeadWarnings, TailWarnings,
            SubGoal0),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = disable_warnings_expr(Context, HeadWarnings, TailWarnings,
            SubGoal)
    ;
        Goal0 = require_detism_expr(Context, Detism, SubGoal0),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = require_detism_expr(Context, Detism, SubGoal)
    ;
        Goal0 = require_complete_switch_expr(Context, Var0, SubGoal0),
        rename_in_plain_or_dot_var(OldVar, NewVar, Var0, Var),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = require_complete_switch_expr(Context, Var, SubGoal)
    ;
        Goal0 = require_switch_arms_detism_expr(Context,
            Var0, Detism, SubGoal0),
        rename_in_plain_or_dot_var(OldVar, NewVar, Var0, Var),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = require_switch_arms_detism_expr(Context,
            Var, Detism, SubGoal)
    ;
        Goal0 = trace_expr(Context, CompileTime, RunTime, MaybeIO0, Mutables0,
            SubGoal0),
        (
            MaybeIO0 = no,
            MaybeIO = no
        ;
            MaybeIO0 = yes(IOStateVar0),
            rename_in_var(OldVar, NewVar, IOStateVar0, IOStateVar),
            MaybeIO = yes(IOStateVar)
        ),
        list.map(rename_in_trace_mutable_var(OldVar, NewVar),
            Mutables0, Mutables),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        Goal = trace_expr(Context, CompileTime, RunTime, MaybeIO, Mutables,
            SubGoal)
    ;
        Goal0 = atomic_expr(Context, InVars0, OutVars0, MaybeVars0,
            MainGoal0, OrElseGoal0),
        rename_in_atomic_varlist(OldVar, NewVar, InVars0, InVars),
        rename_in_atomic_varlist(OldVar, NewVar, OutVars0, OutVars),
        (
            MaybeVars0 = no,
            MaybeVars = no
        ;
            MaybeVars0 = yes(TransVars0),
            list.map(rename_in_var(OldVar, NewVar),
                TransVars0, TransVars),
            MaybeVars = yes(TransVars)
        ),
        rename_in_goal(OldVar, NewVar, MainGoal0, MainGoal),
        rename_in_goals(OldVar, NewVar, OrElseGoal0, OrElseGoal),
        Goal = atomic_expr(Context, InVars, OutVars, MaybeVars,
            MainGoal, OrElseGoal)
    ;
        Goal0 = try_expr(Context, MaybeIO0, SubGoal0, Then0, MaybeElse0,
            Catches0, MaybeCatchAny0),
        rename_in_maybe_var(OldVar, NewVar, MaybeIO0, MaybeIO),
        rename_in_goal(OldVar, NewVar, SubGoal0, SubGoal),
        rename_in_goal(OldVar, NewVar, Then0, Then),
        (
            MaybeElse0 = yes(Else0),
            rename_in_goal(OldVar, NewVar, Else0, Else),
            MaybeElse = yes(Else)
        ;
            MaybeElse0 = no,
            MaybeElse = no
        ),
        list.map(rename_in_catch_expr(OldVar, NewVar), Catches0, Catches),
        (
            MaybeCatchAny0 = yes(catch_any_expr(CatchAnyVar0, CatchAnyGoal0)),
            rename_in_var(OldVar, NewVar, CatchAnyVar0, CatchAnyVar),
            rename_in_goal(OldVar, NewVar, CatchAnyGoal0, CatchAnyGoal),
            MaybeCatchAny = yes(catch_any_expr(CatchAnyVar, CatchAnyGoal))
        ;
            MaybeCatchAny0 = no,
            MaybeCatchAny = no
        ),
        Goal = try_expr(Context, MaybeIO, SubGoal, Then, MaybeElse,
            Catches, MaybeCatchAny)
    ;
        Goal0 = implies_expr(Context, SubGoalA0, SubGoalB0),
        rename_in_goal(OldVar, NewVar, SubGoalA0, SubGoalA),
        rename_in_goal(OldVar, NewVar, SubGoalB0, SubGoalB),
        Goal = implies_expr(Context, SubGoalA, SubGoalB)
    ;
        Goal0 = equivalent_expr(Context, SubGoalA0, SubGoalB0),
        rename_in_goal(OldVar, NewVar, SubGoalA0, SubGoalA),
        rename_in_goal(OldVar, NewVar, SubGoalB0, SubGoalB),
        Goal = equivalent_expr(Context, SubGoalA, SubGoalB)
    ;
        Goal0 = if_then_else_expr(Context, Vars0, StateVars0,
            Cond0, Then0, Else0),
        rename_in_vars(OldVar, NewVar, Vars0, Vars),
        rename_in_vars(OldVar, NewVar, StateVars0, StateVars),
        rename_in_goal(OldVar, NewVar, Cond0, Cond),
        rename_in_goal(OldVar, NewVar, Then0, Then),
        rename_in_goal(OldVar, NewVar, Else0, Else),
        Goal = if_then_else_expr(Context, Vars, StateVars,
            Cond, Then, Else)
    ;
        Goal0 = event_expr(Context, Name, Terms0),
        term.rename_var_in_terms(OldVar, NewVar, Terms0, Terms),
        Goal = event_expr(Context, Name, Terms)
    ;
        Goal0 = call_expr(Context, SymName, Terms0, Purity),
        term.rename_var_in_terms(OldVar, NewVar, Terms0, Terms),
        Goal = call_expr(Context, SymName, Terms, Purity)
    ;
        Goal0 = unify_expr(Context, TermA0, TermB0, Purity),
        term.rename_var_in_term(OldVar, NewVar, TermA0, TermA),
        term.rename_var_in_term(OldVar, NewVar, TermB0, TermB),
        Goal = unify_expr(Context, TermA, TermB, Purity)
    ).

:- pred rename_in_goals(prog_var::in, prog_var::in,
    list(goal)::in, list(goal)::out) is det.

rename_in_goals(_, _, [], []).
rename_in_goals(OldVar, NewVar, [Goal0 | Goals0], [Goal | Goals]) :-
    rename_in_goal(OldVar, NewVar, Goal0, Goal),
    rename_in_goals(OldVar, NewVar, Goals0, Goals).

:- pred rename_in_atomic_varlist(prog_var::in, prog_var::in,
    atomic_component_state::in, atomic_component_state::out) is det.

rename_in_atomic_varlist(OldVar, NewVar, Comp0, Comp) :-
    (
        Comp0 = atomic_state_var(SVar0),
        rename_in_var(OldVar, NewVar, SVar0, SVar),
        Comp = atomic_state_var(SVar)
    ;
        Comp0 = atomic_var_pair(IVar0, OVar0),
        rename_in_var(OldVar, NewVar, IVar0, IVar),
        rename_in_var(OldVar, NewVar, OVar0, OVar),
        Comp = atomic_var_pair(IVar, OVar)
    ).

:- pred rename_in_trace_mutable_var(prog_var::in, prog_var::in,
    trace_mutable_var::in, trace_mutable_var::out) is det.

rename_in_trace_mutable_var(OldVar, NewVar, TMV0, TMV) :-
    TMV0 = trace_mutable_var(MutableName, StateVar0),
    rename_in_var(OldVar, NewVar, StateVar0, StateVar),
    TMV = trace_mutable_var(MutableName, StateVar).

:- pred rename_in_plain_or_dot_var(prog_var::in, prog_var::in,
    plain_or_dot_var::in, plain_or_dot_var::out) is det.

rename_in_plain_or_dot_var(OldVar, NewVar, PODVar0, PODVar) :-
    (
        PODVar0 = podv_plain(Var0),
        rename_in_var(OldVar, NewVar, Var0, Var),
        PODVar = podv_plain(Var)
    ;
        PODVar0 = podv_dot(DotVar0),
        rename_in_var(OldVar, NewVar, DotVar0, DotVar),
        PODVar = podv_dot(DotVar)
    ).

:- pred rename_in_vars(prog_var::in, prog_var::in,
    list(prog_var)::in, list(prog_var)::out) is det.

rename_in_vars(_, _, [], []).
rename_in_vars(OldVar, NewVar, [Var0 | Vars0], [Var | Vars]) :-
    rename_in_var(OldVar, NewVar, Var0, Var),
    rename_in_vars(OldVar, NewVar, Vars0, Vars).

:- pred rename_in_var(prog_var::in, prog_var::in,
    prog_var::in, prog_var::out) is det.

rename_in_var(OldVar, NewVar, Var0, Var) :-
    ( if Var0 = OldVar then
        Var = NewVar
    else
        Var = Var0
    ).

:- pred rename_in_maybe_var(prog_var::in, prog_var::in,
    maybe(prog_var)::in, maybe(prog_var)::out) is det.

rename_in_maybe_var(OldVar, NewVar, MaybeVar0, MaybeVar) :-
    (
        MaybeVar0 = yes(Var0),
        rename_in_var(OldVar, NewVar, Var0, Var),
        MaybeVar = yes(Var)
    ;
        MaybeVar0 = no,
        MaybeVar = no
    ).

:- pred rename_in_catch_expr(prog_var::in, prog_var::in,
    catch_expr::in, catch_expr::out) is det.

rename_in_catch_expr(OldVar, NewVar, Catch0, Catch) :-
    Catch0 = catch_expr(Term0, Goal0),
    term.rename_var_in_term(OldVar, NewVar, Term0, Term),
    rename_in_goal(OldVar, NewVar, Goal0, Goal),
    Catch = catch_expr(Term, Goal).

%-----------------------------------------------------------------------------%

cons_id_and_args_to_term(some_int_const(IntConst), [], Term) :-
    (
        IntConst = int_const(Int),
        Term = int_to_decimal_term(Int, term.context_init)
    ;
        IntConst = int8_const(Int8),
        Term = int8_to_decimal_term(Int8, term.context_init)
    ;
        IntConst = int16_const(Int16),
        Term = int16_to_decimal_term(Int16, term.context_init)
    ;
        IntConst = int32_const(Int32),
        Term = int32_to_decimal_term(Int32, term.context_init)
    ;
        IntConst = int64_const(Int64),
        Term = int64_to_decimal_term(Int64, term.context_init)
    ;
        IntConst = uint_const(UInt),
        Term = uint_to_decimal_term(UInt, term.context_init)
    ;
        IntConst = uint8_const(UInt8),
        Term = uint8_to_decimal_term(UInt8, term.context_init)
    ;
        IntConst = uint16_const(UInt16),
        Term = uint16_to_decimal_term(UInt16, term.context_init)
    ;
        IntConst = uint32_const(UInt32),
        Term = uint32_to_decimal_term(UInt32, term.context_init)
    ;
        IntConst = uint64_const(UInt64),
        Term = uint64_to_decimal_term(UInt64, term.context_init)
    ).
cons_id_and_args_to_term(float_const(Float), [], Term) :-
    Term = term.functor(term.float(Float), [], term.context_init).
cons_id_and_args_to_term(char_const(Char), [], Term) :-
    SymName = unqualified(string.from_char(Char)),
    construct_qualified_term(SymName, [], Term).
cons_id_and_args_to_term(string_const(String), [], Term) :-
    Term = term.functor(term.string(String), [], term.context_init).
cons_id_and_args_to_term(tuple_cons(_Arity), Args, Term) :-
    SymName = unqualified("{}"),
    construct_qualified_term(SymName, Args, Term).
cons_id_and_args_to_term(cons(SymName, _Arity, _TypeCtor), Args, Term) :-
    construct_qualified_term(SymName, Args, Term).

cons_id_arity(ConsId) = Arity :-
    (
        ConsId = cons(_, Arity, _)
    ;
        ConsId = tuple_cons(Arity)
    ;
        ConsId = ground_term_const(_, SubConsId),
        Arity = cons_id_arity(SubConsId)
    ;
        ( ConsId = some_int_const(_)
        ; ConsId = float_const(_)
        ; ConsId = char_const(_)
        ; ConsId = string_const(_)
        ; ConsId = impl_defined_const(_)
        ),
        Arity = 0
    ;
        ( ConsId = closure_cons(_, _)
        ; ConsId = type_ctor_info_const(_, _, _)
        ; ConsId = base_typeclass_info_const(_, _, _, _)
        ; ConsId = type_info_cell_constructor(_)
        ; ConsId = typeclass_info_cell_constructor
        ; ConsId = type_info_const(_)
        ; ConsId = typeclass_info_const(_)
        ; ConsId = tabling_info_const(_)
        ; ConsId = deep_profiling_proc_layout(_)
        ; ConsId = table_io_entry_desc(_)
        ),
        unexpected($pred, "unexpected cons_id")
    ).

cons_id_maybe_arity(cons(_, Arity, _)) = yes(Arity).
cons_id_maybe_arity(tuple_cons(Arity)) = yes(Arity).
cons_id_maybe_arity(some_int_const(_)) = yes(0).
cons_id_maybe_arity(float_const(_)) = yes(0).
cons_id_maybe_arity(char_const(_)) = yes(0).
cons_id_maybe_arity(string_const(_)) = yes(0).
cons_id_maybe_arity(impl_defined_const(_)) = yes(0).
cons_id_maybe_arity(closure_cons(_, _)) = no.
cons_id_maybe_arity(type_ctor_info_const(_, _, _)) = no.
cons_id_maybe_arity(base_typeclass_info_const(_, _, _, _)) = no.
cons_id_maybe_arity(type_info_cell_constructor(_)) = no.
cons_id_maybe_arity(typeclass_info_cell_constructor) = no.
cons_id_maybe_arity(type_info_const(_)) = no.
cons_id_maybe_arity(typeclass_info_const(_)) = no.
cons_id_maybe_arity(ground_term_const(_, ConsId)) =
    cons_id_maybe_arity(ConsId).
cons_id_maybe_arity(tabling_info_const(_)) = no.
cons_id_maybe_arity(deep_profiling_proc_layout(_)) = no.
cons_id_maybe_arity(table_io_entry_desc(_)) = no.

source_integer_to_int(Base, Integer, Int) :-
    require_complete_switch [Base]
    (
        Base = base_10,
        integer.to_int(Integer, Int)
    ;
        ( Base = base_2
        ; Base = base_8
        ; Base = base_16
        ),
        ( if Integer > integer(max_int) then
            NegInteger = Integer + integer(min_int) + integer(min_int),
            integer.to_int(NegInteger, Int),
            Int < 0
        else
            integer.to_int(Integer, Int)
        )
    ).

%-----------------------------------------------------------------------------%

strip_module_qualifier_from_cons_id(ConsId0, ConsId) :-
    ( if ConsId0 = cons(Name0, Arity, TypeCtor) then
        strip_module_qualifier_from_sym_name(Name0, Name),
        ConsId = cons(Name, Arity, TypeCtor)
    else
        ConsId = ConsId0
    ).

strip_module_qualifier_from_sym_name(SymName0, SymName) :-
    (
        SymName0 = qualified(_Module, Name),
        SymName = unqualified(Name)
    ;
        SymName0 = unqualified(_Name),
        SymName = SymName0
    ).

strip_builtin_qualifier_from_cons_id(ConsId0, ConsId) :-
    ( if ConsId0 = cons(Name0, Arity, TypeCtor) then
        strip_builtin_qualifier_from_sym_name(Name0, Name),
        ConsId = cons(Name, Arity, TypeCtor)
    else
        ConsId = ConsId0
    ).

strip_builtin_qualifier_from_sym_name(SymName0, SymName) :-
    ( if
        SymName0 = qualified(Module, Name),
        Module = mercury_public_builtin_module
    then
        SymName = unqualified(Name)
    else
        SymName = SymName0
    ).

%-----------------------------------------------------------------------------%

make_n_fresh_vars(BaseName, N, Vars, VarSet0, VarSet) :-
    make_n_fresh_vars_loop(BaseName, 1, N, Vars, VarSet0, VarSet).

:- pred make_n_fresh_vars_loop(string::in, int::in, int::in, list(var(T))::out,
    varset(T)::in, varset(T)::out) is det.

make_n_fresh_vars_loop(BaseName, Cur, Max, Vars, !VarSet) :-
    ( if Cur > Max then
        Vars = []
    else
        VarName = BaseName ++ string.int_to_string(Cur),
        varset.new_named_var(VarName, HeadVar, !VarSet),
        make_n_fresh_vars_loop(BaseName, Cur + 1, Max, TailVars, !VarSet),
        Vars = [HeadVar | TailVars]
    ).

pred_args_to_func_args(PredArgs, FuncArgs, FuncReturn) :-
    ( if list.split_last(PredArgs, FuncArgsPrime, FuncReturnPrime) then
        FuncArgs = FuncArgsPrime,
        FuncReturn = FuncReturnPrime
    else
        unexpected($pred, "function missing return value?")
    ).

get_state_args(Args0, Args, State0, State) :-
    list.reverse(Args0, RevArgs0),
    RevArgs0 = [State, State0 | RevArgs],
    list.reverse(RevArgs, Args).

get_state_args_det(Args0, Args, State0, State) :-
    ( if get_state_args(Args0, ArgsPrime, State0Prime, StatePrime) then
        Args = ArgsPrime,
        State0 = State0Prime,
        State = StatePrime
    else
        unexpected($pred, "get_state_args failed")
    ).

%-----------------------------------------------------------------------------%

get_new_tvars([], _,  !TVarSet, !TVarNameMap, !TVarRenaming).
get_new_tvars([TVar | TVars], VarSet, !TVarSet, !TVarNameMap, !TVarRenaming) :-
    ( if map.contains(!.TVarRenaming, TVar) then
        true
    else
        ( if varset.search_name(VarSet, TVar, TVarName) then
            ( if map.search(!.TVarNameMap, TVarName, TVarSetVar) then
                map.det_insert(TVar, TVarSetVar, !TVarRenaming)
            else
                varset.new_var(NewTVar, !TVarSet),
                varset.name_var(NewTVar, TVarName, !TVarSet),
                map.det_insert(TVarName, NewTVar, !TVarNameMap),
                map.det_insert(TVar, NewTVar, !TVarRenaming)
            )
        else
            varset.new_var(NewTVar, !TVarSet),
            map.det_insert(TVar, NewTVar, !TVarRenaming)
        )
    ),
    get_new_tvars(TVars, VarSet, !TVarSet, !TVarNameMap, !TVarRenaming).

%-----------------------------------------------------------------------------%

goal_list_to_conj(Context, []) = true_expr(Context).
goal_list_to_conj(Context, [Goal | Goals]) =
    goal_list_to_conj_2(Context, Goal, Goals).

:- func goal_list_to_conj_2(prog_context, goal, list(goal)) = goal.

goal_list_to_conj_2(_, Goal, []) = Goal.
goal_list_to_conj_2(Context, Goal0, [Goal1 | Goals]) =
    conj_expr(Context, Goal0, goal_list_to_conj_2(Context, Goal1, Goals)).

%-----------------------------------------------------------------------------%
:- end_module parse_tree.prog_util.
%-----------------------------------------------------------------------------%
