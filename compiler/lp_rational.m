%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2002, 2005-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: lp_rational.m.
% Main authors: conway, juliensf, vjteag.
%
% This module contains code for creating and manipulating systems of rational
% linear arithmetic constraints. It provides the following operations:
%
% * optimization (using the simplex method)
%
% * projection (using Fourier elimination).
%
% * an entailment test (using the above linear optimizer).
%
%-----------------------------------------------------------------------------%

:- module libs.lp_rational.
:- interface.

:- import_module libs.rat.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Linear constraints over Q^n.
%

:- type lp_constant == rat.
:- type lp_coefficient == rat.

:- type lp_var == var.
:- type lp_vars == list(lp_var).
:- type lp_varset == varset.

:- type lp_term == pair(lp_var, lp_coefficient).
:- type lp_terms == list(lp_term).

    % Create a term with a coefficient of 1.
    % For use with ho functions.
    %
:- func lp_term(lp_var) = lp_term.

:- type lp_operator
    --->    lp_lt_eq
    ;       lp_eq
    ;       lp_gt_eq.

:- inst lp_op_lt_eq_or_eq for lp_operator/0
    --->    lp_lt_eq
    ;       lp_eq.

    % A primitive linear arithmetic constraint.
    %
:- type constraint.

    % A conjunction of primitive constraints.
    %
:- type constraints == list(constraint).

    % Create a constraint from the given components.
    %
:- func construct_constraint(lp_terms, lp_operator, lp_constant) = constraint.

    % Create a constraint from the given components.
    % Throws an exception if the resulting constraint is trivially false.
    %
:- func construct_non_false_constraint(lp_terms, lp_operator, lp_constant)
    = constraint.

    % Deconstruct the given constraint.
    %
:- pred deconstruct_constraint(constraint::in,
    lp_terms::out, lp_operator::out, lp_constant::out) is det.

    % As above but throws an exception if the constraint is false.
    %
:- pred deconstruct_non_false_constraint(constraint::in,
    lp_terms::out, lp_operator::out(lp_op_lt_eq_or_eq), lp_constant::out)
    is det.

    % Succeeds iff the given constraint contains a single variable and
    % that variable is constrained to be a nonnegative value.
    %
:- pred nonneg_constr(constraint::in) is semidet.

    % Create a constraint that constrains the argument
    % have a non-negative value.
    %
:- func make_nonneg_constr(lp_var) = constraint.

    % Create a constraint that equates two variables.
    %
:- func make_vars_eq_constraint(lp_var, lp_var) = constraint.

    % Create constraints of the form:
    %
    %   Var = Constant or Var >= Constant
    %
    % These functions are useful with higher-order code.
    %
:- func make_var_const_eq_constraint(lp_var, rat) = constraint.
:- func make_var_const_gte_constraint(lp_var, rat) = constraint.

    % Create a constraint that is trivially true.
    %
:- func true_constraint = constraint.

    % Create a constraint that is trivially false.
    %
:- func false_constraint = constraint.

    % Succeeds if the constraint is trivially true.
    %
:- pred is_true(constraint::in) is semidet.

    % Succeeds if the constraint is trivially false.
    %
:- pred is_false(constraint::in) is semidet.

    % Takes a list of constraints and looks for equality constraints
    % that may be implicit in any inequalities.
    %
    % NOTE: this is only a syntactic check so it may miss
    % some equalities that are implicit in the system.
    %
:- pred restore_equalities(constraints::in, constraints::out) is det.

    % Succeed iff the given system of constraints is inconsistent.
    %
:- pred inconsistent(lp_varset::in, constraints::in) is semidet.

    % Remove those constraints from the system whose redundancy can be
    % trivially detected.
    %
    % NOTE: the resulting system of constraints may not be minimal.
    %
:- func simplify_constraints(constraints) = constraints.

    % substitute_vars(VarsA, VarsB, Constraints0) = Constraints:
    %
    % Perform variable substitution on the given system of constraints
    % based upon the mapping that is implicit between the corresponding
    % elements of the variable lists `VarsA' and `VarsB'.
    %
    % If length(VarsA) \= length(VarsB), then throw an exception.
    %
:- func substitute_vars(lp_vars, lp_vars, constraints) = constraints.
:- func substitute_vars(map(lp_var, lp_var), constraints) = constraints.

    % Make the values of all the variables in the set zero.
    %
:- func set_vars_to_zero(set(lp_var), constraints) = constraints.

%-----------------------------------------------------------------------------%
%
% Bounding boxes and other approximations.
%

    % Approximate the solution space of a set of constraints using
    % a bounding box. If the system is inconsistent then the resulting
    % system will also be inconsistent.
    %
:- func bounding_box(lp_varset, constraints) = constraints.

    % Create non-negativity constraints for all of the variables in the
    % given list of constraints, except for the variables specified
    % in the first argument.
    %
:- func nonneg_box(lp_vars, constraints) = constraints.

%-----------------------------------------------------------------------------%
%
% Linear solver.
%

:- type objective == lp_terms.

:- type direction
    --->    max
    ;       min.

:- type lp_result
    --->    lp_res_unbounded
    ;       lp_res_inconsistent
    ;       lp_res_satisfiable(rat, map(lp_var, rat)).
            % lp_res_satisfiable(ObjVal, MapFromObjVarsToVals)

    % Maximize (or minimize - depending on `direction') `objective'
    % subject to the given constraints. The variables in the objective
    % and the constraints *must* be from the given `lp_varset'.
    % This is passed to the solver so that it can allocate fresh variables
    % as required.
    %
    % The result is `unbounded' if the objective is not bounded by
    % the constraints, `inconsistent' if the given constraints are
    % inconsistent, or `satisfiable/2' otherwise.
    %
:- func solve(constraints, direction, objective, lp_varset) = lp_result.

%-----------------------------------------------------------------------------%
%
% Projection.
%

:- type projection_result
    --->    pr_res_ok(constraints)  % projection succeeded.
    ;       pr_res_inconsistent     % matrix was inconsistent.
    ;       pr_res_aborted.         % ran out of time/space and backed out.

    % project(Constraints0, Vars, Varset) = Result:
    %
    % Takes a list of constraints, `Constraints0', and eliminates the
    % variables in the list `Vars' using Fourier elimination.
    %
    % Returns `ok(Constraints)' if `Constraints' is the projection
    % of `Constraints0' over `Vars'. Returns `inconsistent' if
    % `Constraints0' is inconsistent. Returns `aborted' if the
    % intermediate matrices grow too large while performing Fourier
    % elimination.
    %
    % NOTE: this does not always detect that a constraint
    % set is inconsistent, so callers to this procedure may need
    % to do a consistency check on the result if they require
    % the resulting system of constraints to be consistent.
    %
:- func project(lp_vars, lp_varset, constraints) = projection_result.
:- pred project(lp_vars::in, lp_varset::in, constraints::in,
    projection_result::out) is det.

    % project(Vars, Varset, maybe(MaxMatrixSize), Matrix, Result):
    %
    % Same as above but if the size of the matrix ever exceeds
    % `MaxMatrixSize' we back out of the computation.
    %
:- pred project(lp_vars::in, lp_varset::in, maybe(int)::in,
    constraints::in, projection_result::out) is det.

%-----------------------------------------------------------------------------%
%
% Entailment.
%

:- type entailment_result
    --->    entailed
    ;       not_entailed
    ;       inconsistent.

    % entailed(Varset, Cs, C):
    %
    % Determines if the constraint `C' is implied by the set of
    % constraints `Cs'. Uses the simplex method to find the point `P'
    % satisfying `Cs' which maximizes (if `C' contains '=<') or
    % minimizes (if `C' contains '>=') a function parallel to `C'.
    % Returns `entailed' if `P' satisfies `C', `not_entailed' if it does not
    % and `inconsistent' if `Cs' is not a consistent system of constraints.
    %
    % This assumes that all variables are non-negative.
    %
:- func entailed(lp_varset, constraints, constraint) = entailment_result.

    % entailed(Varset, Cs, C):
    %
    % As above but fails if `C' is not implied by `Cs' and
    % throws an exception if `Cs' is inconsistent.
    %
:- pred entailed(lp_varset::in, constraints::in, constraint::in) is semidet.

    % Check if a constraint is entailed by all the others in the set.
    % If it is, then remove it from the set.
    %
    % NOTE: this can be very slow - also due to the order in which
    % the constraints are processed, it may not produce a minimal set.
    %
    % Fails if the system of constraints is inconsistent.
    %
:- pred remove_some_entailed_constraints(lp_varset::in, constraints::in,
    constraints::out) is semidet.

%-----------------------------------------------------------------------------%
%
% Stuff for intermodule optimization.
%

    % A function that converts an lp_var into a string.
    % XXX This is *not* a good name for this type.
    %
:- type output_var == (func(lp_var) = string).

    % Write out the constraints in a form we can read in using the
    % term parser from the standard library.
    %
:- pred output_constraints(io.text_output_stream::in, output_var::in,
    constraints::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%
% Debugging predicates.
%

    % Print out the constraints using the names in the varset. If the variable
    % has no name it will be given the name Temp<n>, where <n> is the
    % variable number.
    %
:- pred write_constraints(constraints::in, lp_varset::in, io::di, io::uo)
    is det.

    % Return the set of variables that are present in a list of constraints.
    %
    % XXX This shouldn't be exported but it is currently needed by the
    % workaround for the problem with head variables in term_constr_fixpoint.m.
    %
:- func get_vars_from_constraints(constraints) = set(lp_var).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
% undesirable dependency, for write_out_list

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module require.
:- import_module solutions.
:- import_module string.

%-----------------------------------------------------------------------------%
%
% Constraints.
%

    % The following properties should hold for each constraint:
    % - there is one instance of each variable in the term list.
    % - the terms are sorted in increasing order by variable.
    % - the terms should be normalized so that the leading term
    %   has a coefficient of +/-1 (unless all terms have a coefficient
    %   of zero - in which case the term list is empty).
    % - variables with coefficient zero are *not* included in the list
    %   of terms.
:- type constraint
    --->    lte(lp_terms, lp_constant)     % sumof(Terms) =< Constant
    ;       eq(lp_terms, lp_constant)      % sumof(Terms) =  Constant
    ;       gte(lp_terms, lp_constant).    % sumof(Terms) >= Constant

%-----------------------------------------------------------------------------%
%
% Procedures for constructing/deconstructing constraints.
%

lp_term(Var) = Var - one.

construct_constraint(Terms0, Op, Const0) = Constraint :-
    (
        Terms0 = [],
        (
            Op = lp_lt_eq,
            Constraint = lte([], Const0)
        ;
            Op = lp_eq,
            Constraint = eq([], Const0)
        ;
            Op = lp_gt_eq,
            Constraint = lte([], -Const0)
        )
    ;
        Terms0 = [_ | _],
        (
            Op = lp_lt_eq,
            Terms1 = sum_like_terms(Terms0),
            normalize_terms_and_const(yes, Terms1, Const0, Terms, Const),
            Constraint = lte(Terms, Const)
        ;
            Op = lp_eq,
            Terms1 = sum_like_terms(Terms0),
            normalize_terms_and_const(no, Terms1, Const0, Terms, Const),
            Constraint = eq(Terms, Const)
        ;
            Op = lp_gt_eq,
            Terms1 = sum_like_terms(Terms0),
            normalize_terms_and_const(yes, Terms1, Const0, Terms, Const),
            Constraint = lte(negate_lp_terms(Terms), -Const)
        )
    ).

    % This is for internal use only - it builds a constraint out of the parts
    % but does *not* attempt to perform any standardization. It is intended for
    % use in operations such as normalization.
    %
:- func unchecked_construct_constraint(lp_terms, lp_operator, lp_constant) =
    constraint.

unchecked_construct_constraint(Terms, lp_lt_eq, Constant) =
    lte(Terms, Constant).
unchecked_construct_constraint(Terms, lp_eq,    Constant) =
    eq(Terms, Constant).
unchecked_construct_constraint(Terms, lp_gt_eq, Constant) =
    gte(Terms, Constant).

:- func sum_like_terms(lp_terms) = lp_terms.

sum_like_terms(Terms) = map.to_assoc_list(lp_terms_to_map(Terms)).

    % Convert an association list of lp_vars and coefficients to a map
    % of the same. If there are duplicate keys in the list make sure that
    % eventual value in the map is the sum of the two coefficients.
    % Also if a coefficient is (or ends up being) zero, make sure that
    % the variable doesn't end up in the resulting map.
    %
:- func lp_terms_to_map(assoc_list(lp_var, lp_coefficient)) =
    map(lp_var, lp_coefficient).

lp_terms_to_map(Terms) = Map :-
    list.foldl(lp_terms_to_map_2, Terms, map.init, Map).

:- pred lp_terms_to_map_2(pair(lp_var, lp_coefficient)::in,
    map(lp_var, lp_coefficient)::in, map(lp_var, lp_coefficient)::out) is det.

lp_terms_to_map_2(Var - Coeff0, !Map) :-
    ( if map.search(!.Map, Var, MapCoeff) then
        Coeff = MapCoeff + Coeff0,
        ( if Coeff = zero then
            map.delete(Var, !Map)
        else
            map.set(Var, Coeff, !Map)
        )
    else
        ( if Coeff0 = zero then
            true
        else
            map.set(Var, Coeff0, !Map)
        )
    ).

construct_non_false_constraint(Terms, Op, Constant) = Constraint :-
    Constraint = construct_constraint(Terms, Op, Constant),
    ( if is_false(Constraint) then
        unexpected($pred, "false constraint")
    else
        true
    ).

deconstruct_constraint(lte(Terms, Constant), Terms, lp_lt_eq, Constant).
deconstruct_constraint(eq(Terms,  Constant), Terms, lp_eq,    Constant).
deconstruct_constraint(gte(Terms, Constant), Terms, lp_gt_eq, Constant).

deconstruct_non_false_constraint(Constraint, Terms, Operator, Constant) :-
    ( if is_false(Constraint) then
        unexpected($pred, "false_constraint")
    else
        true
    ),
    (
        Constraint = lte(Terms, Constant),
        Operator   = lp_lt_eq
    ;
        Constraint = eq(Terms, Constant),
        Operator   = lp_eq
    ;
        Constraint = gte(_, _),
        unexpected($pred, "gte encountered")
    ).

:- func lp_terms(constraint) = lp_terms.

lp_terms(lte(Terms, _)) = Terms.
lp_terms(eq(Terms,  _)) = Terms.
lp_terms(gte(Terms, _)) = Terms.

:- func constant(constraint) = lp_constant.

constant(lte(_, Constant)) = Constant.
constant(eq(_,  Constant)) = Constant.
constant(gte(_, Constant)) = Constant.

:- func operator(constraint) = lp_operator.

operator(lte(_, _)) = lp_lt_eq.
operator(eq(_,  _)) = lp_eq.
operator(gte(_,_))  = unexpected($pred, "gte").

:- func negate_operator(lp_operator) = lp_operator.

negate_operator(lp_lt_eq) = lp_gt_eq.
negate_operator(lp_eq)    = lp_eq.
negate_operator(lp_gt_eq) = lp_lt_eq.

nonneg_constr(lte([_ - (-rat.one)], rat.zero)).
nonneg_constr(gte(_, _)) :-
    unexpected($pred, "gte").

make_nonneg_constr(Var) =
    construct_constraint([Var - (-rat.one)], lp_lt_eq, rat.zero).

make_vars_eq_constraint(Var1, Var2) =
    construct_constraint([Var1 - rat.one, Var2 - (-rat.one)], lp_eq, rat.zero).

make_var_const_eq_constraint(Var, Constant) =
    construct_constraint([Var - rat.one], lp_eq, Constant).
make_var_const_gte_constraint(Var, Constant) =
    construct_constraint([Var - rat.one], lp_gt_eq, Constant).

true_constraint = eq([], rat.zero).

false_constraint = eq([], rat.one).

is_true(gte([], Const)) :- Const =< rat.zero.
is_true(lte([], Const)) :- Const >= rat.zero.
is_true(eq([],  Const)) :- Const =  rat.zero.

is_false(gte([], Const)) :- Const >  rat.zero.
is_false(lte([], Const)) :- Const <  rat.zero.
is_false(eq([],  Const)) :- Const \= rat.zero.

%-----------------------------------------------------------------------------%

restore_equalities([], []).
restore_equalities([E0 | Es0], [E | Es])  :-
    ( if check_for_equalities(E0, Es0, [], E1, Es1) then
        E = E1,
        Es2 = Es1
    else
        Es2 = Es0,
        E = E0
    ),
    restore_equalities(Es2, Es).

:- pred check_for_equalities(constraint::in, constraints::in, constraints::in,
    constraint::out, constraints::out) is semidet.

check_for_equalities(Eqn0, [Eqn | Eqns], SoFar, NewEqn, NewEqnSet) :-
    ( if opposing_inequalities(Eqn0 @ lte(Coeffs, Constant), Eqn) then
        NewEqn = standardize_constraint(eq(Coeffs, Constant)),
        NewEqnSet = SoFar ++ Eqns
    else
        check_for_equalities(Eqn0, Eqns, [Eqn | SoFar], NewEqn, NewEqnSet)
    ).

    % Checks if a pair of constraints are inequalities of the form:
    %
    %   -ax1 - ax2 - ... - axN  =< -C
    %    ax1 + ax2 + ... + axN  =<  C
    %
    % These can be converted into the equality:
    %
    %   ax1 + ... + axN = C
    %
    % NOTE: we don't check for gte constraints because these should
    % have been transformed away when we converted to standard form.
    %
:- pred opposing_inequalities(constraint::in, constraint::in) is semidet.

opposing_inequalities(lte(TermsA, Const), lte(TermsB, -Const)) :-
    TermsB = list.map((func(V - X) = V - (-X)), TermsA).

%-----------------------------------------------------------------------------%

    % Put a constraint into standard form. Every constraint has its terms list
    % in increasing order of variable name and then multiplied so that
    % the absolute value of the leading coefficient is one.
    % op_ge is converted to op_le by multiplying through by negative one.
    % op_eq constraints should have an initial coefficient of (positive) 1.
    %
:- func standardize_constraint(constraint) = constraint.

standardize_constraint(gte(Terms0, Const0)) = Constraint :-
    normalize_terms_and_const(yes, Terms0, Const0, Terms, Const),
    Constraint = lte(negate_lp_terms(Terms), -Const).
standardize_constraint(eq(Terms0,  Const0)) = eq(Terms,  Const) :-
    normalize_terms_and_const(no, Terms0, Const0, Terms, Const).
standardize_constraint(lte(Terms0, Const0)) = lte(Terms, Const) :-
    normalize_terms_and_const(yes, Terms0, Const0, Terms, Const).

    % Sort the list of terms in ascending order by variable and then
    % multiply through so that the first term has a coefficient of
    % one or negative one. If the first argument is `yes', then we multiply
    % through by the reciprocal of the absolute value of the coefficient,
    % otherwise we multiply through by the reciprocal of the value.
    %
:- pred normalize_terms_and_const(bool::in, lp_terms::in, lp_constant::in,
    lp_terms::out, lp_constant::out) is det.

normalize_terms_and_const(AbsVal, !.Terms, !.Const, !:Terms, !:Const) :-
    CompareTerms = (func(VarA - _, VarB - _) = Result :-
        compare(Result, VarA, VarB)
    ),
    !:Terms = list.sort(CompareTerms, !.Terms),
    ( if !.Terms = [_ - Coefficient0 | _] then
        (
            AbsVal = yes,
            Coefficient = rat.abs(Coefficient0)
        ;
            AbsVal = no,
            Coefficient = Coefficient0
        ),
        ( if Coefficient = rat.zero then
            unexpected($pred, "zero coefficient")
        else
            true
        ),
        DivideBy = (func(Var - Coeff) = Var - (Coeff / Coefficient)),
        !:Terms = list.map(DivideBy, !.Terms),
        !:Const = !.Const / Coefficient
    else
        true
    ).

    % Succeeds iff the constraint is implied by the assumption that
    % all variables are non-negative *and* the constraint is not one
    % used to force non-negativity of the variables.
    %
:- pred obvious_constraint(constraint::in) is semidet.

obvious_constraint(lte(Terms, Constant)) :-
    Constant >= rat.zero,
    list.length(Terms) >= 2,
    all [Term] list.member(Term, Terms) => snd(Term) < zero.

obvious_constraint(gte(Terms, Constant)) :-
    Constant =< rat.zero,
    list.length(Terms) >= 2,
    all [Term] (
        list.member(Term, Terms)
    =>
        snd(Term) > zero
    ).

inconsistent(Vars, Constraints @ [Constraint | _]) :-
    (
        is_false(Constraint)
    ;
        (
            Constraint = lte([Term | _], _)
        ;
            Constraint = eq([Term | _],  _)
        ;
            Constraint = gte([Term | _], _)
        ),
        DummyObjective = [Term],
        lp_rational.solve(Constraints, max, DummyObjective, Vars) =
            lp_res_inconsistent
    ).

simplify_constraints(Constraints) = remove_weaker(remove_trivial(Constraints)).

:- func remove_trivial(constraints) = constraints.

remove_trivial([]) = [].
remove_trivial([Constraint | Constraints]) = Result :-
    ( if is_false(Constraint) then
        Result = [ false_constraint ]
    else
        Result0 = remove_trivial(Constraints),
        ( if
            Result0 = [C],
            is_false(C)
        then
            Result = Result0
        else
            % Remove the constraint if it is trivially true or the result
            % of all the variables being non-negative.
            ( if
                ( is_true(Constraint)
                ; obvious_constraint(Constraint)
                )
            then
                Result = Result0
            else
                Result = [Constraint | Result0]
            )
        )
    ).

:- func remove_weaker(constraints) = constraints.

remove_weaker([]) = [].
remove_weaker([C | Cs0]) = Result :-
    list.foldl2(remove_weaker_2(C), Cs0, [], Cs, yes, Keep),
    Result0 = remove_weaker(Cs),
    (
        Keep = yes,
        Result = [C | Result0]
    ;
        Keep = no,
        Result = Result0
    ).

:- pred remove_weaker_2(constraint::in, constraint::in, constraints::in,
    constraints::out, bool::in, bool::out) is det.

remove_weaker_2(A, B, !Acc, !Keep) :-
    ( if is_stronger(A, B) then
        true
    else if is_stronger(B, A) then
        list.cons(B, !Acc),
        !:Keep = no
    else
        list.cons(B, !Acc)
    ).

:- pred is_stronger(constraint::in, constraint::in) is semidet.

is_stronger(eq(Terms, Const), gte(Terms, Const)).
is_stronger(eq(Terms, Const), lte(Terms, Const)).
is_stronger(eq(Terms, Const), gte(negate_lp_terms(Terms), -Const)).
is_stronger(eq(Terms, Const), lte(negate_lp_terms(Terms), -Const)).
is_stronger(lte([Var - (-one)], ConstA), lte([Var - (-one)], ConstB)) :-
    ConstA =< zero, ConstA =< ConstB.
is_stronger(eq(Terms, ConstA), lte(negate_lp_terms(Terms), ConstB)) :-
    ConstA >= (-one) * ConstB.
is_stronger(lte(Terms, ConstA), lte(Terms, ConstB)) :-
    ConstB =< zero, ConstA =< ConstB.

substitute_vars(Old, New, Constraints0) = Constraints :-
    SubstMap = map.from_corresponding_lists(Old, New),
    Constraints = list.map(substitute_vars_2(SubstMap), Constraints0).
substitute_vars(SubstMap, Constraints0) = Constraints :-
    Constraints = list.map(substitute_vars_2(SubstMap), Constraints0).

:- func substitute_vars_2(map(lp_var, lp_var), constraint) = constraint.

substitute_vars_2(SubstMap, lte(Terms0, Const)) = Result :-
    Terms = list.map(substitute_term(SubstMap), Terms0),
    Result = lte(sum_like_terms(Terms), Const).
substitute_vars_2(SubstMap, eq(Terms0, Const)) = Result :-
    Terms = list.map(substitute_term(SubstMap), Terms0),
    Result = eq(sum_like_terms(Terms), Const).
substitute_vars_2(_, gte(_, _)) =
    unexpected($pred, "gte").

:- func substitute_term(map(lp_var, lp_var), lp_term) = lp_term.

substitute_term(SubstMap, Term0) = Term :-
    Term0 = Var0 - Coeff,
    map.lookup(SubstMap, Var0, Var),
    Term = Var - Coeff.

set_vars_to_zero(Vars, Constraints) =
    list.map(set_vars_to_zero_2(Vars), Constraints).

:- func set_vars_to_zero_2(set(lp_var), constraint) = constraint.

set_vars_to_zero_2(Vars, lte(Terms0, Const)) = lte(Terms, Const) :-
    Terms = set_terms_to_zero(Vars, Terms0).
set_vars_to_zero_2(Vars, eq(Terms0,  Const)) = eq(Terms, Const)  :-
    Terms = set_terms_to_zero(Vars, Terms0).
set_vars_to_zero_2(Vars, gte(Terms0, Const)) = gte(Terms, Const) :-
    Terms = set_terms_to_zero(Vars, Terms0).

:- func set_terms_to_zero(set(lp_var), lp_terms) = lp_terms.

set_terms_to_zero(Vars, Terms0) = Terms :-
    IsNonZero =
        ( pred(Term::in) is semidet :-
            Term = Var - _Coeff,
            not set.member(Var, Vars)
        ),
    Terms = list.filter(IsNonZero, Terms0).

%-----------------------------------------------------------------------------%
%
% Bounding boxes and other weaker approximations of the convex union.
%

bounding_box(Varset, Constraints) = BoundingBox :-
    Vars = set.to_sorted_list(get_vars_from_constraints(Constraints)),
    CallProject =
        (func(Var, Constrs0) = Constrs :-
            Result = project([Var], Varset, Constrs0),
            (
                Result = pr_res_inconsistent,
                Constrs = [false_constraint]
            ;
                % If we needed to abort this computation we will just
                % approximate the whole lot by `true'.
                Result = pr_res_aborted,
                Constrs = []
            ;
                Result = pr_res_ok(Constrs)
            )
        ),
    BoundingBox = list.foldl(CallProject, Vars, Constraints).

nonneg_box(VarsToIgnore, Constraints) = NonNegConstraints :-
    Vars0 = get_vars_from_constraints(Constraints),
    MakeConstr =
        ( pred(Var::in, !.C::in, !:C::out) is det :-
            ( if list.member(Var, VarsToIgnore) then
                true
            else
                list.cons(make_nonneg_constr(Var), !C)
            )
        ),
    set.fold(MakeConstr, Vars0, [], NonNegConstraints).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Linear solver.
%

% XXX Most of this came from lp.m. We should try to remove a lot of
% nondeterminism here.

:- type lpr_info
    --->    lpr_info(
                lpr_varset     :: lp_varset,
                lpr_slack_vars :: lp_vars,
                lpr_art_vars   :: lp_vars
            ).

solve(Constraints, Direction, Objective, Varset) = Result :-
    Info0 = lpr_info_init(Varset),
    solve_2(Constraints, Direction, Objective, Result, Info0, _).

    % solve_2(Eqns, Dir, Obj, Res, LPRInfo0, LPRInfo) takes
    % a list of inequalities `Eqns', a direction for optimization `Dir',
    % an objective function `Obj' and an lpr_info structure `LPRInfo0'.
    % See inline comments for details on the algorithm.
    %
:- pred solve_2(constraints::in, direction::in, objective::in,
    lp_result::out, lpr_info::in, lpr_info::out) is det.

solve_2(!.Constraints, Direction, !.Objective, Result, !LPRInfo) :-
    % Simplify the inequalities and convert them to standard form by
    % introducing slack/artificial variables.

    Obj = !.Objective,
    lp_standardize_constraints(!Constraints, !LPRInfo),

    % If we are maximizing the objective function then we need to negate
    % all the coefficients in the objective.
    (
        Direction = max,
        ObjTerms = negate_constraint(eq(!.Objective, zero)),
        !:Objective = lp_terms(ObjTerms)
    ;
        Direction = min
    ),
    Rows = list.length(!.Constraints),
    Vars = collect_vars(!.Constraints, Obj),
    VarList = set.to_sorted_list(Vars),
    Columns = list.length(VarList),
    VarNums = number_vars(VarList, 0),
    ArtVars = !.LPRInfo ^ lpr_art_vars,
    Tableau0 = init_tableau(Rows, Columns, VarNums),
    insert_constraints(!.Constraints, 1, Columns, VarNums, Tableau0, Tableau),
    (
        ArtVars = [_ | _],
        % There are one or more artificial variables, so we use
        % the two-phase method for solving the system.
        Result0 = two_phase(Obj, !.Objective, ArtVars, VarNums, Tableau)
     ;
        ArtVars = [],
        Result0 = one_phase(Obj, !.Objective, VarNums, Tableau)
    ),
    (
        Direction = max,
        Result = Result0
    ;
        Direction = min,
        (
            ( Result0 = lp_res_unbounded
            ; Result0 = lp_res_inconsistent
            ),
            Result = Result0
        ;
            Result0 = lp_res_satisfiable(OptVal, OptCoffs),
            Result = lp_res_satisfiable(-OptVal, OptCoffs)
        )
    ).

%-----------------------------------------------------------------------------%

:- func one_phase(lp_terms, lp_terms, map(lp_var, int), tableau) = lp_result.

one_phase(Obj0, Obj, VarNums, !.Tableau) = Result :-
    insert_terms(Obj, 0, VarNums, !Tableau),
    get_vars_from_terms(Obj0, set.init, ObjVars0),
    ObjVars = set.to_sorted_list(ObjVars0),
    optimize(ObjVars, Result, !.Tableau, _).

%-----------------------------------------------------------------------------%

:- func two_phase(lp_terms, lp_terms, lp_vars, map(lp_var, int), tableau)
    = lp_result.

two_phase(Obj0, Obj, ArtVars, VarNums, !.Tableau) = Result :-
    % Phase 1: minimize the sum of the artificial variables.

    ArtObj = list.map(lp_term, ArtVars),
    insert_terms(ArtObj, 0, VarNums, !Tableau),
    ensure_zero_obj_coeffs(ArtVars, !Tableau),
    optimize(ArtVars, Result0, !Tableau),
    (
        Result0 = lp_res_unbounded,
        Result = lp_res_unbounded
    ;
        Result0 = lp_res_inconsistent,
        Result = lp_res_inconsistent
    ;
        Result0 = lp_res_satisfiable(Val, _ArtRes),
        ( if Val = zero then
            fix_basis_and_rem_cols(ArtVars, !.Tableau, Tableau1),

            % Phase 2:
            % Insert the real objective, zero the objective coefficients
            % of the basis variables and optimize the objective.

            insert_terms(Obj, 0, VarNums, Tableau1, Tableau2),
            BasisVars = get_basis_vars(Tableau2),
            ensure_zero_obj_coeffs(BasisVars, Tableau2, Tableau3),
            get_vars_from_terms(Obj0, set.init, ObjVars0),
            ObjVars = set.to_sorted_list(ObjVars0),
            optimize(ObjVars, Result, Tableau3, _)
        else
            Result = lp_res_inconsistent
         )
    ).

%-----------------------------------------------------------------------------%

:- pred lp_standardize_constraints(constraints::in, constraints::out,
    lpr_info::in, lpr_info::out) is det.

lp_standardize_constraints(!Constraints, !LPRInfo) :-
    list.map_foldl(lp_standardize_constraint, !Constraints, !LPRInfo).

    % standardize_constraint performs the following operations on a
    % constraint:
    %
    %   - ensures the constant is >= 0 (multiplying by -1 if necessary)
    %   - introduces slack and artificial variables
    %
:- pred lp_standardize_constraint(constraint::in, constraint::out,
    lpr_info::in, lpr_info::out) is det.

lp_standardize_constraint(Constr0 @ lte(Coeffs, Const), Constr, !LPRInfo) :-
    ( if Const < zero then
        Constr1 = negate_constraint(Constr0),
        lp_standardize_constraint(Constr1, Constr, !LPRInfo)
    else
        new_slack_var(Var, !LPRInfo),
        Constr = lte([Var - one | Coeffs], Const)
    ).
lp_standardize_constraint(Eqn0 @ eq(Coeffs, Const), Eqn, !LPRInfo) :-
    ( if Const < zero then
        Eqn1 = negate_constraint(Eqn0),
        lp_standardize_constraint(Eqn1, Eqn, !LPRInfo)
    else
        new_art_var(Var, !LPRInfo),
        Eqn = lte([Var - one | Coeffs], Const)
    ).
lp_standardize_constraint(Eqn0 @ gte(Coeffs, Const), Eqn, !LPRInfo) :-
    ( if Const < zero then
        Eqn1 = negate_constraint(Eqn0),
        lp_standardize_constraint(Eqn1, Eqn, !LPRInfo)
    else
        new_slack_var(SVar, !LPRInfo),
        new_art_var(AVar, !LPRInfo),
        Eqn = gte([AVar - one, SVar - (-one) | Coeffs], Const)
    ).

:- func negate_constraint(constraint) = constraint.

negate_constraint(lte(Terms, Const)) = gte(negate_lp_terms(Terms), -Const).
negate_constraint(eq(Terms,  Const)) = eq(negate_lp_terms(Terms),  -Const).
negate_constraint(gte(Terms, Const)) = lte(negate_lp_terms(Terms), -Const).

:- func negate_lp_terms(lp_terms) = lp_terms.

negate_lp_terms(Terms) = assoc_list.map_values_only((func(X) = (-X)), Terms).

%-----------------------------------------------------------------------------%

:- func collect_vars(constraints, objective) = set(lp_var).

collect_vars(Eqns, Obj) = Vars :-
    GetVar =
        ( pred(Var::out) is nondet :-
            (
                list.member(Eqn, Eqns),
                Coeffs = lp_terms(Eqn),
                list.member(Pair, Coeffs)
            ;
                list.member(Pair, Obj)
            ),
            Var = fst(Pair)
        ),
    solutions.solutions(GetVar, VarList),
    Vars = set.list_to_set(VarList).

:- type var_num_map == map(lp_var, int).

:- func number_vars(lp_vars, int) = var_num_map.

number_vars(Vars, N) = VarNum :-
    number_vars_2(Vars, N, map.init, VarNum).

:- pred number_vars_2(lp_vars::in, int::in,
    var_num_map::in, var_num_map::out) is det.

number_vars_2([], _, !VarNums).
number_vars_2([Var | Vars], N, !VarNums) :-
    map.det_insert(Var, N, !VarNums),
    number_vars_2(Vars, N + 1, !VarNums).

:- pred insert_constraints(constraints::in, int::in, int::in,
    var_num_map::in, tableau::in, tableau::out) is det.

insert_constraints([], _, _, _, !Tableau).
insert_constraints([C | Cs], Row, ConstCol, VarNums, !Tableau) :-
    insert_terms(lp_terms(C), Row, VarNums, !Tableau),
    set_cell(Row, ConstCol, constant(C), !Tableau),
    insert_constraints(Cs, Row + 1, ConstCol, VarNums, !Tableau).

:- pred insert_terms(lp_terms::in, int::in, var_num_map::in,
    tableau::in, tableau::out) is det.

insert_terms([], _,  _, !Tableau).
insert_terms([Var - Const | Coeffs], Row, VarNums, !Tableau) :-
    map.lookup(VarNums, Var, Col),
    set_cell(Row, Col, Const, !Tableau),
    insert_terms(Coeffs, Row, VarNums, !Tableau).

%-----------------------------------------------------------------------------%

:- pred optimize(lp_vars::in, lp_result::out, tableau::in, tableau::out)
    is det.

optimize(ObjVars, Result, !Tableau) :-
    simplex(Result0, !Tableau),
    (
        Result0 = no,
        Result = lp_res_unbounded
    ;
        Result0 = yes,
        ObjVal = !.Tableau ^ elem(0, !.Tableau ^ cols),
        ObjMap = extract_objective(ObjVars, !.Tableau),
        Result = lp_res_satisfiable(ObjVal, ObjMap)
    ).

:- func extract_objective(lp_vars, tableau) = map(lp_var, rat).

extract_objective(ObjVars, Tableau) = Objective :-
    Objective = list.foldl(extract_obj_var(Tableau), ObjVars, map.init).

:- func extract_obj_var(tableau, lp_var, map(lp_var, rat))
    = map(lp_var, rat).

extract_obj_var(Tableau, Var, Map0) = Map :-
    extract_obj_var2(Tableau, Var, Val),
    map.set(Var, Val, Map0, Map).

:- pred extract_obj_var2(tableau::in, lp_var::in, rat::out) is det.

extract_obj_var2(Tableau, Var, Val) :-
    Col = var_col(Tableau, Var),
    GetCell =
        ( pred(Val0::out) is nondet :-
            all_rows(Tableau, Row),
            one = Tableau ^ elem(Row, Col),
            Val0 = Tableau ^ elem(Row, Tableau ^ cols)
        ),
    solutions.solutions(GetCell, Solns),
    ( if Solns = [Val1] then Val = Val1 else Val = zero ).

:- pred simplex(bool::out, tableau::in, tableau::out) is det.

simplex(Result, !Tableau) :-
    AllColumns = all_cols(!.Tableau),
    MinAgg =
        ( pred(Col::in, !.Min::in, !:Min::out) is det :-
            (
                !.Min = no,
                MinVal = !.Tableau ^ elem(0, Col),
                ( if MinVal < zero then
                    !:Min = yes(Col - MinVal)
                else
                    !:Min = no
                )
            ;
                !.Min = yes(_ - MinVal0),
                CellVal = !.Tableau ^ elem(0, Col),
                ( if CellVal < MinVal0 then
                    !:Min = yes(Col - CellVal)
                else
                    true
                )
            )
        ),
    solutions.aggregate(AllColumns, MinAgg, no, MinResult),
    (
        MinResult = no,
        Result = yes
    ;
        MinResult = yes(Q - _Val),
        AllRows = all_rows(!.Tableau),
        MaxAgg =
            ( pred(Row::in, !.Max::in, !:Max::out) is det :-
                (
                    !.Max = no,
                    MaxVal = !.Tableau ^ elem(Row, Q),
                    ( if MaxVal > zero then
                        Col = !.Tableau ^ cols,
                        MVal = !.Tableau ^ elem(Row, Col),
                        ( if MaxVal = zero then
                            unexpected($pred, "zero divisor")
                        else
                            true
                        ),
                        CVal = MVal / MaxVal,
                        !:Max = yes(Row - CVal)
                    else
                        !:Max = no
                    )
                ;
                    !.Max = yes(_ - MaxVal0),
                    CellVal = !.Tableau ^ elem(Row, Q),
                    RHSC = rhs_col(!.Tableau),
                    MVal = !.Tableau ^ elem(Row, RHSC),
                    ( if CellVal =< zero then
                        % CellVal = 0 => multiple optimal sol'ns.
                        true
                    else
                        ( if CellVal = zero then
                            unexpected($pred, "zero divisor")
                        else
                            true
                        ),
                        MaxVal1 = MVal / CellVal,
                        ( if MaxVal1 =< MaxVal0 then
                            !:Max = yes(Row - MaxVal1)
                        else
                            true
                        )
                    )
                )
            ),
        solutions.aggregate(AllRows, MaxAgg, no, MaxResult),
        (
            MaxResult = no,
            Result = no
        ;
            MaxResult = yes(P - _),
            pivot(P, Q, !Tableau),
            disable_warning [suspicious_recursion] (
                simplex(Result, !Tableau)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- pred ensure_zero_obj_coeffs(lp_vars::in, tableau::in, tableau::out) is det.

ensure_zero_obj_coeffs([], !Tableau).
ensure_zero_obj_coeffs([Var | Vars], !Tableau) :-
    Col = var_col(!.Tableau, Var),
    Val = !.Tableau ^ elem(0, Col),
    ( if Val = zero then
        ensure_zero_obj_coeffs(Vars, !Tableau)
    else
        FindOne =
            ( pred(P::out) is nondet :-
                all_rows(!.Tableau, R),
                ValF0 = !.Tableau ^ elem(R, Col),
                ValF0 \= zero,
                P = R - ValF0
            ),
        solutions.solutions(FindOne, Ones),
        (
            Ones = [Row - Fac0 | _],
            ( if Fac0 = zero then
                unexpected($pred, "zero divisor")
            else
                true
            ),
            Fac = -Val / Fac0,
            row_op(Fac, Row, 0, !Tableau),
            ensure_zero_obj_coeffs(Vars, !Tableau)
        ;
            Ones = [],
            unexpected($pred, "problem with artificial variable")
        )
    ).

:- pred fix_basis_and_rem_cols(lp_vars::in, tableau::in, tableau::out) is det.

fix_basis_and_rem_cols([], !Tableau).
fix_basis_and_rem_cols([Var | Vars], !Tableau) :-
    Col = var_col(!.Tableau, Var),
    BasisAgg =
        ( pred(R::in, Ones0::in, Ones::out) is det :-
            Val = !.Tableau ^ elem(R, Col),
            Ones = ( if Val = zero then Ones0 else [Val - R | Ones0] )
        ),
    solutions.aggregate(all_rows(!.Tableau), BasisAgg, [], Res),
    ( if Res = [one - Row] then
        PivGoal =
            ( pred(Col1::out) is nondet :-
                all_cols(!.Tableau, Col1),
                Col \= Col1,
                Zz = !.Tableau ^ elem(Row, Col1),
                Zz \= zero
            ),
        solutions.solutions(PivGoal, PivSolns),
        (
            PivSolns = [],
            remove_col(Col, !Tableau),
            remove_row(Row, !Tableau)
        ;
            PivSolns = [Col2 | _],
            pivot(Row, Col2, !Tableau),
            remove_col(Col, !Tableau)
        )
    else
        true
    ),
    remove_col(Col, !Tableau),
    fix_basis_and_rem_cols(Vars, !Tableau).

%-----------------------------------------------------------------------------%

:- type cell
    --->    cell(int, int).

:- pred pivot(int::in, int::in, tableau::in, tableau::out) is det.

pivot(P, Q, !Tableau) :-
    Apq = !.Tableau ^ elem(P, Q),
    MostCells =
        ( pred(Cell::out) is nondet :-
            all_rows0(!.Tableau, J),
            J \= P,
            all_cols0(!.Tableau, K),
            K \= Q,
            Cell = cell(J, K)
        ),
    ScaleCell =
        ( pred(Cell::in, T0::in, T::out) is det :-
            Cell = cell(J, K),
            Ajk = T0 ^ elem(J, K),
            Ajq = T0 ^ elem(J, Q),
            Apk = T0 ^ elem(P, K),
            ( if Apq = zero then
                unexpected($pred, "ScaleCell: zero divisor")
            else
                true
            ),
            T = T0 ^ elem(J, K) := Ajk - Apk * Ajq / Apq
        ),
    solutions.aggregate(MostCells, ScaleCell, !Tableau),
    QColumn =
        ( pred(Cell::out) is nondet :-
            all_rows0(!.Tableau, J),
            Cell = cell(J, Q)
        ),
    Zero =
        ( pred(Cell::in, T0::in, T::out) is det :-
            Cell = cell(J, K),
            T = T0 ^ elem(J, K) := zero
        ),
    solutions.aggregate(QColumn, Zero, !Tableau),
    PRow = all_cols0(!.Tableau),
    ScaleRow =
        ( pred(K::in, T0::in, T::out) is det :-
            Apk = T0 ^ elem(P, K),
            ( if Apq = zero then
                unexpected($pred, "ScaleRow: zero divisor")
            else
                true
            ),
            T = T0 ^ elem(P, K) := Apk / Apq
        ),
    solutions.aggregate(PRow, ScaleRow, !Tableau),
    set_cell(P, Q, one, !Tableau).

:- pred row_op(rat::in, int::in, int::in, tableau::in,
    tableau::out) is det.

row_op(Scale, From, To, !Tableau) :-
    AllCols = all_cols0(!.Tableau),
    AddRow =
        ( pred(Col::in, T0::in, T::out) is det :-
            X = T0 ^ elem(From, Col),
            Y = T0 ^ elem(To, Col),
            Z = Y + (Scale * X),
            T = T0 ^ elem(To, Col) := Z
        ),
    solutions.aggregate(AllCols, AddRow, !Tableau).

%-----------------------------------------------------------------------------%

% XXX We should try using arrays or version_arrays for the simplex tableau.
% (We should try this in lp.m as well).

:- type tableau
    --->    tableau(
                rows         :: int,
                cols         :: int,
                var_nums     :: map(lp_var, int),
                shunned_rows :: list(int),
                shunned_cols :: list(int),
                cells        :: map(pair(int), rat)
            ).

:- func init_tableau(int, int, map(lp_var, int)) = tableau.

init_tableau(Rows, Cols, VarNums) = Tableau :-
    Tableau = tableau(Rows, Cols, VarNums, [], [], map.init).

:- func tableau ^ elem(int, int) = rat.

Tableau ^ elem(Row, Col) = get_cell(Tableau, Row, Col).

:- func tableau ^ elem(int, int) := rat = tableau.

Tableau0 ^ elem(Row, Col) := Cell = Tableau :-
    set_cell(Row, Col, Cell, Tableau0, Tableau).

:- func get_cell(tableau, int, int) = rat.

get_cell(Tableau, Row, Col) = Cell :-
    ( if
        ( list.member(Row, Tableau ^ shunned_rows)
        ; list.member(Col, Tableau ^ shunned_cols)
        )
    then
        unexpected($pred, "attempt to address shunned row/col")
    else
        true
    ),
    ( if Cell0 = Tableau ^ cells ^ elem(Row - Col) then
        Cell = Cell0
    else
        Cell = zero
    ).

:- pred set_cell(int::in, int::in, rat::in, tableau::in,
    tableau::out) is det.

set_cell(J, K, R, Tableau0, Tableau) :-
    Tableau0 = tableau(Rows, Cols, VarNums, SR, SC, Cells0),
    ( if
        ( list.member(J, SR)
        ; list.member(K, SC)
        )
    then
        unexpected($pred, "Attempt to write shunned row/col")
    else
        true
    ),
    ( if R = zero then
        Cells = map.delete(Cells0, J - K)
    else
        Cells = map.set(Cells0, J - K, R)
    ),
    Tableau = tableau(Rows, Cols, VarNums, SR, SC, Cells).

    % Returns the number of the RHS column in the tableau.
    %
:- func rhs_col(tableau) = int.

rhs_col(Tableau) = Tableau ^ cols.

:- pred all_rows0(tableau::in, int::out) is nondet.

all_rows0(Tableau, Row) :-
    between(0, Tableau ^ rows, Row),
    not list.member(Row, Tableau ^ shunned_rows).

:- pred all_rows(tableau::in, int::out) is nondet.

all_rows(Tableau, Row) :-
    between(1, Tableau ^ rows, Row),
    not list.member(Row, Tableau ^ shunned_rows).

:- pred all_cols0(tableau::in, int::out) is nondet.

all_cols0(Tableau, Col) :-
    between(0, Tableau ^ cols, Col),
    not list.member(Col, Tableau ^ shunned_cols).

:- pred all_cols(tableau::in, int::out) is nondet.

all_cols(Tableau, Col) :-
    Cols1 = Tableau ^ cols - 1,
    between(0, Cols1, Col),
    not list.member(Col, Tableau ^ shunned_cols).

:- func var_col(tableau, lp_var) = int.

var_col(Tableau, Var) = (Tableau ^ var_nums) ^ det_elem(Var).

:- pred remove_row(int::in, tableau::in, tableau::out) is det.

remove_row(Row, !Tableau) :-
    SR = !.Tableau ^ shunned_rows,
    !Tableau ^ shunned_rows := [Row | SR].

:- pred remove_col(int::in, tableau::in, tableau::out) is det.

remove_col(C, Tableau0, Tableau) :-
    Tableau0 = tableau(Rows, Cols, VarNums, SR, SC, Cells),
    Tableau = tableau(Rows, Cols, VarNums, SR, [C | SC], Cells).

:- func get_basis_vars(tableau) = lp_vars.

get_basis_vars(Tableau) = Vars :-
    BasisCol =
        ( pred(C::out) is nondet :-
            all_cols(Tableau, C),
            NonZeroGoal =
                ( pred(P::out) is nondet :-
                    all_rows(Tableau, R),
                    Z = Tableau ^ elem(R, C),
                    Z \= zero,
                    P = R - Z
                ),
            solutions.solutions(NonZeroGoal, Solns),
            Solns = [_ - one]
        ),
    solutions.solutions(BasisCol, Cols),
    BasisVars =
        ( pred(V::out) is nondet :-
            list.member(Col, Cols),
            map.member(Tableau ^ var_nums, V, Col)
        ),
    solutions.solutions(BasisVars, Vars).

%-----------------------------------------------------------------------------%

:- func lpr_info_init(lp_varset) = lpr_info.

lpr_info_init(Varset) = lpr_info(Varset, [], []).

:- pred new_slack_var(lp_var::out, lpr_info::in, lpr_info::out) is det.

new_slack_var(Var, !LPRInfo) :-
    varset.new_var(Var, !.LPRInfo ^ lpr_varset, Varset),
    !LPRInfo ^ lpr_varset := Varset,
    Vars = !.LPRInfo ^ lpr_slack_vars,
    !LPRInfo ^ lpr_slack_vars := [Var | Vars].

:- pred new_art_var(lp_var::out, lpr_info::in, lpr_info::out) is det.

new_art_var(Var, !LPRInfo) :-
    varset.new_var(Var, !.LPRInfo ^ lpr_varset, Varset),
    !LPRInfo ^ lpr_varset := Varset,
    Vars = !.LPRInfo ^ lpr_art_vars,
    !LPRInfo ^ lpr_art_vars := [Var | Vars].

%-----------------------------------------------------------------------------%

:- pred between(int::in, int::in, int::out) is nondet.

between(Min, Max, I) :-
    Min =< Max,
    (
        I = Min
    ;
        between(Min + 1, Max, I)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Projection.
%
%
% The following code more or less follows the algorithm described in:
% Joxan Jaffar, Michael Maher, Peter Stuckey and Roland Yap.
% Projecting CLP(R) Constraints. New Generation Computing 11(3): 449-469.
%
% * Linear equations (Gaussian elimination)
%   - substitutions need to be performed on the inequalities as well.
% * Linear inequalities (Fourier elimination)
%
% We next convert any remaining equations into opposing inequalities and
% then use Fourier elimination to try and eliminate any remaining target
% variables. The main problem here is ensuring that we don't get
% swamped by redundant constraints.
%
% The implementation here uses the extensions to FM elimination described by
% Cernikov as well as some other redundancy checks. Note that in general
% arbitrarily mixing redundancy elimination techniques with the Cernikov
% methods is unsound (See the above article for an example).
%
% In addition to Cernikov's methods and quasi-syntactic redundancy checks
% we also use a heuristic developed by Duffin to choose the order in
% which we eliminate variables (See below).
%
%-----------------------------------------------------------------------------%

:- type vector
    --->    vector(
                % The vector's label is for redundancy checking
                % during Fourier elimination - see below.
                label :: set(int),

                % A map from each variable in the vector to its coefficient.
                terms :: map(lp_var, lp_coefficient),

                const :: lp_constant
            ).

:- type matrix == list(vector).

project(Vars, Varset, Constraints) = Result :-
    project(Vars, Varset, no, Constraints, Result).

project(Vars, Varset, Constraints, Result) :-
    project(Vars, Varset, no, Constraints, Result).

    % For the first branch of this switch the `Constraints' may actually
    % be an inconsistent system - we don't bother checking that here though.
    % We instead delay that until we need to perform an entailment check.
    %
project([], _, _, Constraints, pr_res_ok(Constraints)).
project(!.Vars @ [_ | _], Varset, MaybeThreshold, Constraints0, Result) :-
    eliminate_equations(!Vars, Constraints0, EqlResult),
    (
        EqlResult = pr_res_inconsistent,
        Result = pr_res_inconsistent
    ;
        % Elimination of equations should not cause an abort since we always
        % make the matrix smaller.
        EqlResult = pr_res_aborted,
        unexpected($pred, "abort from eliminate_equations")
    ;
        EqlResult = pr_res_ok(Constraints1),

        % Skip the call to fourier_elimination/6 if there are no variables to
        % project - this avoids the transformation to vector form.
        (
            !.Vars = [_ | _],
            Matrix0 = constraints_to_matrix(Constraints1),
            fourier_elimination(!.Vars, Varset, MaybeThreshold, 0,
                Matrix0, FourierResult),
            (
                FourierResult = yes(Matrix),
                Constraints = matrix_to_constraints(Matrix),
                Result = pr_res_ok(Constraints)
            ;
                FourierResult = no,
                Result = pr_res_aborted
            )
        ;
            % NOTE: the matrix `Constraints1' may actually be inconsistent here
            % - we don't bother checking at this point because that would mean
            % traversing the matrix, so we wait until the next operation that
            % needs to traverse it anyway or until the next entailment check.
            !.Vars = [],
            Result = pr_res_ok(Constraints1)
        )
    ).

%-----------------------------------------------------------------------------%
%
% Convert each constraint into `=<' form and give each an initial label.
%

:- func constraints_to_matrix(constraints) = matrix.

constraints_to_matrix(Constraints) = Matrix :-
    list.foldl2(fm_standardize, Constraints, 0, _, [], Matrix).

:- pred fm_standardize(constraint::in, int::in, int::out, matrix::in,
    matrix::out) is det.

fm_standardize(lte(Terms0, Constant), !Labels, !Matrix) :-
    Terms = lp_terms_to_map(Terms0),
    make_label(Label, !Labels),
    list.cons(vector(Label, Terms, Constant), !Matrix).
fm_standardize(eq(Terms, Constant), !Labels, !Matrix) :-
    make_label(Label1, !Labels),
    make_label(Label2, !Labels),
    Vector1 = vector(Label1, lp_terms_to_map(Terms), Constant),
    Vector2 = vector(Label2, lp_terms_to_map(negate_lp_terms(Terms)),
        -Constant),
    list.append([Vector1, Vector2], !Matrix).
fm_standardize(gte(Terms0, Constant), !Labels, !Matrix) :-
    make_label(Label, !Labels),
    Terms = lp_terms_to_map(negate_lp_terms(Terms0)),
    list.cons(vector(Label, Terms, -Constant), !Matrix).

:- pred make_label(set(int)::out, int::in, int::out) is det.

make_label(Label, Labels, Labels + 1) :-
    Label = set.make_singleton_set(Labels).

:- func matrix_to_constraints(matrix) = constraints.

matrix_to_constraints(Matrix) = list.map(vector_to_constraint, Matrix).

:- func vector_to_constraint(vector) = constraint.

vector_to_constraint(vector(_, Terms0, Constant0)) = Constraint :-
    Terms1 = map.to_assoc_list(Terms0),
    normalize_terms_and_const(yes, Terms1, Constant0, Terms, Constant),
    Constraint = lte(Terms, Constant).

%-----------------------------------------------------------------------------%
%
% Predicates for eliminating equations from the constraints.
% (Gaussian elimination)
%

    % Split the constraints into a set of inequalities and a set of equalities.
    % For every variable in the set of target variables (i.e. those we are
    % eliminating), check if there is at least one equality that contains
    % that variable. If so, then substitute the value of that variable
    % into the other constraints. Return the set of target variables
    % that do not occur in any equality.
    %
:- pred eliminate_equations(lp_vars::in, lp_vars::out, constraints::in,
    projection_result::out) is det.

eliminate_equations(!Vars, Constraints0, Result) :-
    Constraints = simplify_constraints(Constraints0),
    list.filter((pred(eq(_, _)::in) is semidet), Constraints,
        Equalities0, Inequalities0),
    ( if
        eliminate_equations_2(!Vars, Equalities0, Equalities,
            Inequalities0, Inequalities)
    then
        Result = pr_res_ok(Equalities ++ Inequalities)
    else
        Result = pr_res_inconsistent
    ).

:- pred eliminate_equations_2(lp_vars::in, lp_vars::out,
    constraints::in, constraints::out, constraints::in,
    constraints::out) is semidet.

eliminate_equations_2([], [], !Equations, !Inequations).
eliminate_equations_2([Var | !.Vars], !:Vars, !Equations, !Inequations) :-
    eliminate_equations_2(!Vars, !Equations, !Inequations),
    ( if find_target_equality(Var, Target, !Equations) then
        substitute_variable(Target, Var, !Equations, !Inequations,
            SuccessFlag),
        (
            SuccessFlag = no,
            list.cons(Var, !Vars),
            list.cons(Target, !Equations)
        ;
            SuccessFlag = yes
        )
    else
        list.cons(Var, !Vars)
    ).

    % Find an equation that constrains a variable we are trying to eliminate.
    %
:- pred find_target_equality(lp_var::in, constraint::out,
    constraints::in, constraints::out) is semidet.

find_target_equality(Var, Target, Constraints0, Constraints) :-
    Result = find_target_equality(Var, Constraints0),
    Result = yes(Target - Constraints).

:- func find_target_equality(lp_var, constraints) =
    maybe(pair(constraint, constraints)).

find_target_equality(Var, Eqns) = find_target_equality_2(Var, Eqns, []).

:- func find_target_equality_2(lp_var, constraints, constraints) =
    maybe(pair(constraint, constraints)).

find_target_equality_2(_, [], _) = no.
find_target_equality_2(Var, [Eqn | Eqns], Acc) = MaybeTargetEqn :-
    ( if operator(Eqn) = lp_eq then
        true
    else
        unexpected($pred, "inequality encountered")
    ),
    Coeffs = lp_terms(Eqn),
    ( if list.member(Var - _, Coeffs) then
        MaybeTargetEqn = yes(Eqn - (Acc ++ Eqns))
    else
        MaybeTargetEqn = find_target_equality_2(Var, Eqns, [Eqn | Acc])
    ).

    % Given a target equation of the form a1x1 + .. + aNxN = C and
    % a target variable, say `x1', notionally rewrite the equation as:
    %
    % x1 = C - ... aN/a1 xN
    %
    % and then substitute that value for x1 in the supplied sets
    % of equations and inequations.
    %
:- pred substitute_variable(constraint::in, lp_var::in,
    constraints::in, constraints::out, constraints::in, constraints::out,
    bool::out) is semidet.

substitute_variable(Target0, Var, !Equations, !Inequations, Flag) :-
    normalize_constraint(Var, Target0, Target),
    deconstruct_constraint(Target, TargetCoeffs, Op, TargetConst),
    expect(unify(Op, lp_eq), $pred, "inequality encountered"),
    fix_coeff_and_const(Var, TargetCoeffs, TargetConst, Coeffs, Const),
    substitute_into_constraints(Var, Coeffs, Const, !Equations, EqlFlag),
    substitute_into_constraints(Var, Coeffs, Const, !Inequations, IneqlFlag),
    Flag = bool.or(EqlFlag, IneqlFlag).

    % Multiply the terms and constant except for the term containing
    % the specified variable in preparation for making a substitution
    % for that variable. Notionally this converts a constraint of the form:
    %       t + z + w = C   ... C is a constant
    %
    % into:
    %
    %       t = C - z - w
    %
:- pred fix_coeff_and_const(lp_var::in, lp_terms::in, lp_constant::in,
    lp_terms::out, lp_constant::out) is det.

fix_coeff_and_const(_, [], Const, [], -Const).
fix_coeff_and_const(Var, [Var1 - Coeff1 | Coeffs], Const0, FixedCoeffs,
        Const) :-
    fix_coeff_and_const(Var, Coeffs, Const0, FCoeffs0, Const),
    ( if Var = Var1 then
        FixedCoeffs = FCoeffs0
    else
        FixedCoeffs = [Var1 - (-Coeff1) | FCoeffs0]
    ).

    % The `Flag' argument is `yes' if one or more substitutions were made,
    % `no' otherwise. substitute_into_constraints/7 fails if a false constraint
    % is generated as a result of a substitution. This means that the original
    % matrix was inconsistent.
    %
:- pred substitute_into_constraints(lp_var::in, lp_terms::in,
    lp_constant::in, constraints::in, constraints::out, bool::out) is semidet.

substitute_into_constraints(_, _, _, [], [], no).
substitute_into_constraints(Var, Coeffs, Const, [Constr0 | Constrs0], Result,
        Flag) :-
    substitute_into_constraint(Var, Coeffs, Const, Constr0, Constr, Flag0),
    not is_false(Constr),
    substitute_into_constraints(Var, Coeffs, Const, Constrs0, Constrs, Flag1),
    Result = ( if is_true(Constr) then Constrs else [Constr | Constrs] ),
    Flag = bool.or(Flag0, Flag1).

:- pred substitute_into_constraint(lp_var::in, lp_terms::in,
    lp_constant::in, constraint::in, constraint::out, bool::out) is det.

substitute_into_constraint(Var, SubCoeffs, SubConst, !Constraint, Flag) :-
    normalize_constraint(Var, !Constraint),
    deconstruct_constraint(!.Constraint, TargetCoeffs, Op, TargetConst),
    ( if list.member(Var - one, TargetCoeffs) then
        FinalCoeffs0 = lp_terms_to_map(TargetCoeffs ++ SubCoeffs),

        % Delete the target variable from both constraints.
        FinalCoeffs1 = map.delete(FinalCoeffs0, Var),
        FinalCoeffs = map.to_assoc_list(FinalCoeffs1),
        FinalConst = TargetConst + SubConst,
        !:Constraint = construct_constraint(FinalCoeffs, Op, FinalConst),
        Flag = yes
    else
        Flag = no
    ).

%-----------------------------------------------------------------------------%
%
% Fourier elimination.
%

    % Will return `no' if it aborts otherwise `yes(Matrix)', where
    % `Matrix' is the result of the projection.
    %
:- pred fourier_elimination(lp_vars::in, lp_varset::in, maybe(int)::in,
    int::in, matrix::in, maybe(matrix)::out) is det.

fourier_elimination([], _, _, _, Matrix, yes(Matrix)).
fourier_elimination(Vars @ [Var0 | Vars0], Varset, MaybeThreshold, !.Step,
        Matrix0, Result) :-
    % Use Duffin's heuristic to try and find a "nice" variable to eliminate.
    %
    % NOTE: the heuristic will fail if none of the variables being projected
    % actually occur in the constraints. In that case, we just pick
    % the first one - it doesn't really matter since the projection
    % will be trivial.
    ( if duffin_heuristic(Vars, Matrix0, TargetVar0, OtherVars0) then
        Var = TargetVar0,
        OtherVars = OtherVars0
    else
        Var = Var0,
        OtherVars = Vars0
    ),
    separate_vectors(Matrix0, Var, PosMatrix, NegMatrix, ZeroMatrix,
        SizeZeroMatrix),

    % `Step' counts active Fourier eliminations only. An elimination is active
    % if at least one constraint contains a term that has a non-zero
    % coefficient for the variable being eliminated.

    ( if
        PosMatrix = [_ | _],
        NegMatrix = [_ | _]
    then
        !:Step = !.Step + 1,
        ( if
            list.foldl2(eliminate_var(!.Step, MaybeThreshold, NegMatrix),
                PosMatrix, ZeroMatrix, ResultMatrix, SizeZeroMatrix, _)
        then
            NewMatrix = yes(ResultMatrix)
        else
            NewMatrix = no
        )
    else
        NewMatrix = yes(ZeroMatrix)
    ),
    (
        NewMatrix = yes(Matrix),
        fourier_elimination(OtherVars, Varset, MaybeThreshold, !.Step,
            Matrix, Result)
    ;
        NewMatrix = no,
        Result = no
    ).

    % separate_vectors(Matrix, Var, Positive, Negative, Zero, Num).
    % `Positive' is a matrix containing those constraints of `Matrix' for
    % which the coefficient of `Var' is positive. `Negative' similarly
    % for those which the coefficient of `Var' is negative and `Zero'
    % those for which the coefficient of `Var' is zero. `Num' is the
    % number of constraints in `Zero'.
    %
:- pred separate_vectors(matrix::in, lp_var::in, matrix::out, matrix::out,
    matrix::out, int::out) is det.

separate_vectors(Matrix, Var, Pos, Neg, Zero, NumZeros) :-
    list.foldl4(classify_vector(Var), Matrix, [], Pos, [], Neg, [], Zero,
        0, NumZeros).

:- pred classify_vector(lp_var::in, vector::in, matrix::in,
    matrix::out, matrix::in, matrix::out, matrix::in, matrix::out,
    int::in, int::out) is det.

classify_vector(Var, Vector0, !Pos, !Neg, !Zero, !Num) :-
    ( if Coefficient = Vector0 ^ terms ^ elem(Var) then
        Vector0 = vector(Label, Terms0, Const0),
        normalize_vector(Var, Terms0, Terms, Const0, Const),
        Vector1 = vector(Label, Terms, Const),
        ( if Coefficient > zero then
            list.cons(Vector1, !Pos)
        else
            list.cons(Vector1, !Neg)
        )
    else
        list.cons(Vector0, !Zero),
        !:Num  = !.Num + 1
    ).

:- pred eliminate_var(int::in, maybe(int)::in, matrix::in,
    vector::in, matrix::in, matrix::out, int::in, int::out) is semidet.

eliminate_var(Step, MaybeThreshold, NegMatrix, PosVector, !Zeros,
        !ZerosSize) :-
    list.foldl2(combine_vectors(Step, MaybeThreshold, PosVector),
        NegMatrix, !Zeros, !ZerosSize).

:- pred combine_vectors(int::in, maybe(int)::in, vector::in,
    vector::in, matrix::in, matrix::out, int::in, int::out) is semidet.

combine_vectors(Step, MaybeThreshold, vector(LabelPos, TermsPos, ConstPos),
        vector(LabelNeg, TermsNeg, ConstNeg), !Zeros, !Num) :-
    LabelNew = set.union(LabelPos, LabelNeg),
    ( if
        % If the cardinality of the label set is greater than `Step + 2'
        % then the constraint we are trying to add is redundant.
        set.count(LabelNew) < Step + 2
    then
        add_vectors(TermsPos, ConstPos, TermsNeg, ConstNeg, Coeffs, Const),
        New = vector(LabelNew, Coeffs, Const),
        ( if
            (
                % Do not bother adding the new constraint
                % if it is just `true'.
                map.is_empty(Coeffs),
                Const >= zero
            ;
                list.member(Vec, !.Zeros),
                quasi_syntactic_redundant(New, Vec)
            )
        then
            % If the new constraint is `true' or is quasi-syntactic redundant
            % with something already there.
            true
        else
            % Remove anything in the matrix that is quasi-syntactic redundant
            % w.r.t the new constraint.
            filter_and_count(
                ( pred(Vec2::in) is semidet :-
                    not quasi_syntactic_redundant(Vec2, New)
                ),
                !.Zeros, [], !:Zeros, 0, !:Num),
            ( if
                list.member(Vec, !.Zeros),
                label_subsumed(New, Vec)
            then
                % Do not add the new constraint because it is label subsumed
                % by something already in the matrix.
                true
            else
                filter_and_count(
                    ( pred(Vec2::in) is semidet :-
                        not label_subsumed(Vec2, New)
                    ),
                    !.Zeros, [], !:Zeros, 0, !:Num),
                list.cons(New, !Zeros),
                !:Num = !.Num + 1
            )
        )
    else
        true
    ),
    % Check that the size of the matrix does not exceed the threshold
    % for aborting the projection.
    not (
        MaybeThreshold = yes(Threshold),
        !.Num > Threshold
    ).

%-----------------------------------------------------------------------------%

:- pred filter_and_count(pred(vector)::in(pred(in) is semidet),
    matrix::in, matrix::in, matrix::out, int::in, int::out) is det.

filter_and_count(_, [], !Acc, !Count).
filter_and_count(P, [X | Xs], !Acc, !Count) :-
    ( if P(X) then
        list.cons(X, !Acc),
        !:Count = !.Count + 1
    else
        true
    ),
    filter_and_count(P, Xs, !Acc, !Count).

%-----------------------------------------------------------------------------%
%
% Detection of quasi-syntactic redundancy.
%

    % Succeeds if the first vector is quasi-syntactic redundant wrt to the
    % second. That is c = c' + (0 < e), for e > 0.
    %
:- pred quasi_syntactic_redundant(vector::in, vector::in) is semidet.

quasi_syntactic_redundant(VecA, VecB) :-
    VecB ^ const < VecA ^ const,
    all [Var] (
        map.member(VecA ^ terms, Var, Coeff)
    <=>
        map.member(VecB ^ terms, Var, Coeff)
    ).

%-----------------------------------------------------------------------------%
%
% Label subsumption.
%

    % label_subsumed(A, B):
    %
    % Succeeds iff constraint A is label subsumed by constraint B.
    %
:- pred label_subsumed(vector::in, vector::in) is semidet.

label_subsumed(VectorA, VectorB) :-
    set.subset(VectorB ^ label, VectorA ^ label).

%-----------------------------------------------------------------------------%
%
% Duffin's heuristic.
%
%
% This attempts to find an order in which to eliminate variables such that
% the minimal number of redundant constraints are generated at each
% Fourier step. For each variable, x_h, to be eliminated, we
% calculate E(x_h) which is defined as follows:
%
%  E(x_h) = p(x_h)q(x_h) + r(x_h) ... if p(x_h) + q(x_h) > 0
%  E(x_h) = 0                     ... if p(x_h) + q(x_h) = 0
%
%  p, q, r are the number of positive, negative and zero coefficients
%  of the variable x_h respectively in the system of constraints under
%  consideration. E(x_h) is called the expansion number of x_h.
%
%  We eliminate the variable that has minimal expansion number.
%
% For further details see:
% R.J. Duffin. On Fourier's Analysis of Linear Inequality Systems.
% Mathematical Programming Study 1, 71 - 95 (1974).
%
%-----------------------------------------------------------------------------%

    % We only count the occurrences of positive and negative coefficients.
    % We can work out the zero occurrences by subtracting the two
    % previous totals from the total number of constraints.
    %
:- type coeff_info
    --->    coeff_info(
               pos :: int,
               neg :: int
            ).

:- type cc_map == map(lp_var, coeff_info).

    % Calculates the variable with the minimal expansion number and
    % returns that variable. (Removes those variables that have an
    % expansion number of zero, because there are no constraints on them
    % anyway). Fails if it can't find such a variable, ie. none of the
    % variables being eliminated actually occurs in the constraints.
    %
:- pred duffin_heuristic(lp_vars::in, matrix::in, lp_var::out,
    lp_vars::out) is semidet.

duffin_heuristic([Var], _, Var, []).
duffin_heuristic(Vars0 @ [_, _ | _], Matrix, TargetVar, Vars) :-
    VarsAndNums0 = generate_expansion_nums(Vars0, Matrix),
    VarsAndNums1 = list.filter(relevant, VarsAndNums0),
    VarsAndNums1 \= [],
    TargetVar = find_max(VarsAndNums1),
    Vars = collect_remaining_vars(VarsAndNums1, TargetVar).

:- func collect_remaining_vars(assoc_list(lp_var, int), lp_var) = lp_vars.

collect_remaining_vars([], _) = [].
collect_remaining_vars([Var - _ | Rest], TargetVar) = Result :-
    ( if Var = TargetVar then
        Result = collect_remaining_vars(Rest, TargetVar)
    else
        Result = [Var | collect_remaining_vars(Rest, TargetVar)]
    ).

:- func find_max(list(pair(lp_var, int))) = lp_var.

find_max([]) = unexpected($pred, "empty list").
find_max([Var0 - ExpnNum0 | Vars]) = fst(find_max_2(Vars, Var0 - ExpnNum0)).

:- func find_max_2(assoc_list(lp_var, int), pair(lp_var, int)) =
    pair(lp_var, int).

find_max_2([], Best) = Best.
find_max_2([Var1 - ExpnNum1 | Vars], Var0 - ExpnNum0) =
    ( if ExpnNum1 < ExpnNum0 then
        find_max_2(Vars, Var1 - ExpnNum1)
    else
        find_max_2(Vars, Var0 - ExpnNum0)
    ).

:- pred relevant(pair(lp_var, int)::in) is semidet.

relevant(Var) :-
    Var \= _ - 0.

    % Given a list of variables and a system of linear inequalities
    % generate the expansion number for each of the variables in the list.
    %
:- func generate_expansion_nums(lp_vars, matrix) = assoc_list(lp_var, int).

generate_expansion_nums(Vars0, Matrix) = ExpansionNums :-
    Vars = list.sort_and_remove_dups(Vars0),
    CoeffMap0 = init_cc_map(Vars),
    CoeffMap  = list.foldl(count_coeffs_in_vector, Matrix, CoeffMap0),
    CoeffList = map.to_assoc_list(CoeffMap),
    ConstrNum = list.length(Matrix),
    ExpansionNums = list.map(make_expansion_num(ConstrNum), CoeffList).

:- func make_expansion_num(int, pair(lp_var, coeff_info)) = pair(lp_var, int).

make_expansion_num(ConstrNum, Var - coeff_info(Pos, Neg)) = Var - ExpnNum :-
    PosAndNeg = Pos + Neg,
    ( if PosAndNeg = 0 then
        ExpnNum = 0
    else
        ExpnNum = (Pos * Neg) + (ConstrNum - PosAndNeg)
    ).

:- func count_coeffs_in_vector(vector, cc_map) = cc_map.

count_coeffs_in_vector(Vector, Map0) = Map :-
    CoeffList = map.to_assoc_list(Vector ^ terms),
    list.foldl(count_coeff, CoeffList, Map0, Map).

:- pred count_coeff(lp_term::in, cc_map::in, cc_map::out) is det.

count_coeff(Var - Coeff, !Map) :-
    ( if map.search(!.Map, Var, coeff_info(Pos0, Neg0)) then
        ( if Coeff > zero then
            Pos = Pos0 + 1,
            Neg = Neg0
        else if Coeff < zero then
            Pos = Pos0,
            Neg = Neg0 + 1
        else
            unexpected($pred, "zero coefficient")
        ),
        map.det_update(Var, coeff_info(Pos, Neg), !Map)
    else
        true
        % If the variable in the term was not in the map then it is not
        % one of the ones that is being eliminated.
    ).

:- func init_cc_map(lp_vars) = cc_map.

init_cc_map(Vars) = list.foldl(InitMap, Vars, map.init) :-
    InitMap = (func(Var, Map) =
        map.det_insert(Map, Var, coeff_info(0, 0))
    ).

%-----------------------------------------------------------------------------%
%
% Predicates for normalizing vectors and constraints.
%

    % normalize_vector(Var, Terms0, Terms, Const0, Const):
    %
    % Multiply the given vector by a scalar appropriate to make the
    % coefficient of the given variable in the vector one. Throws an exception
    % if `Var' has a zero coefficient.
    %
:- pred normalize_vector(lp_var::in,
    map(lp_var, lp_coefficient)::in, map(lp_var, lp_coefficient)::out,
    lp_constant::in, lp_constant::out) is det.

normalize_vector(Var, !Terms, !Constant) :-
    ( if map.search(!.Terms, Var, Coefficient) then
        ( if Coefficient = zero then
            unexpected($pred, "zero coefficient in vector")
        else
            true
        ),
        DivVal = rat.abs(Coefficient),
        !:Terms = map.map_values_only((func(C) = C / DivVal), !.Terms),
        !:Constant = !.Constant / DivVal
    else
        % In this case the coefficient of the variable was zero
        % (implicit in the fact that it is not in the map).
        true
    ).

    % Multiply the given constraint by a scalar appropriate to make the
    % coefficient of the given variable in the constraint one. If the variable
    % does not occur in the constraint then the constraint is unchanged.
    % If the constraint is an inequality the sign may be changed.
    % Throws an exception if the variable is found in the constraint
    % and it has a coefficient of zero.
    %
:- pred normalize_constraint(lp_var::in, constraint::in, constraint::out)
    is det.

normalize_constraint(Var, Constraint0, Constraint) :-
    deconstruct_constraint(Constraint0, Terms0, Op0, Constant0),
    ( if assoc_list.search(Terms0, Var, Coefficient) then
        ( if Coefficient = zero then
            unexpected($pred, "zero coefficient constraint")
        else
            true
        ),
        Terms = list.map((func(V - C) = V - (C / Coefficient)), Terms0),
        Constant = Constant0 / Coefficient,
        Op = ( if Coefficient < zero then negate_operator(Op0) else Op0 )
    else
        % In this case the coefficient of the variable was zero
        % (implicit in the fact that it is not in the list).
        Terms = Terms0,
        Op = Op0,
        Constant = Constant0
    ),
    Constraint = unchecked_construct_constraint(Terms, Op, Constant).

:- pred add_vectors(map(lp_var, lp_coefficient)::in, lp_constant::in,
    map(lp_var, lp_coefficient)::in, lp_constant::in,
    map(lp_var, lp_coefficient)::out, lp_constant::out) is det.

add_vectors(TermsA, ConstA, TermsB, ConstB, Terms, ConstA + ConstB) :-
    IsMapKey =
        ( pred(Var::out) is nondet :-
            map.member(TermsA, Var, _)
        ),
    AddVal =
        ( pred(Var::in, Coeffs0::in, Coeffs::out) is det :-
            map.lookup(TermsA, Var, NumA),
            ( if map.search(Coeffs0, Var, Num1) then
                ( if NumA + Num1 = zero then
                    Coeffs = map.delete(Coeffs0, Var)
                else
                    Coeffs = map.det_update(Coeffs0, Var, NumA + Num1)
                )
            else
                Coeffs = map.det_insert(Coeffs0, Var, NumA)
            )
        ),
    solutions.aggregate(IsMapKey, AddVal, TermsB, Terms).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Entailment test.
%

entailed(Varset, Constraints, lte(Objective, Constant)) = Result :-
    SolverResult = lp_rational.solve(Constraints, max, Objective, Varset),
    (
        SolverResult = lp_res_satisfiable(MaxVal, _),
        Result = ( if MaxVal =< Constant then entailed else not_entailed )
    ;
        SolverResult = lp_res_unbounded,
        Result = not_entailed
    ;
        SolverResult = lp_res_inconsistent,
        Result = inconsistent
    ).
entailed(Varset, Constraints, eq(Objective, Constant)) = Result :-
    Result0 = entailed(Varset, Constraints, lte(Objective, Constant)),
    (
        Result0 = entailed,
        Result  = entailed(Varset, Constraints, gte(Objective, Constant))
    ;
        ( Result0 = not_entailed
        ; Result0 = inconsistent
        ),
        Result0 = Result
    ).
entailed(Varset, Constraints, gte(Objective, Constant)) = Result :-
    SolverResult = lp_rational.solve(Constraints, min, Objective, Varset),
    (
        SolverResult = lp_res_satisfiable(MinVal, _),
        Result = ( if MinVal >= Constant then entailed else not_entailed )
    ;
        SolverResult = lp_res_unbounded,
        Result = not_entailed
    ;
        SolverResult = lp_res_inconsistent,
        Result = inconsistent
    ).

entailed(Varset, Constraints, Constraint) :-
    Result = entailed(Varset, Constraints, Constraint),
    (
        Result = entailed
    ;
        Result = inconsistent,
        unexpected($pred, "inconsistent constraint set")
    ;
        Result = not_entailed,
        fail
    ).

%-----------------------------------------------------------------------------%
%
% Redundancy checking using the linear solver.
%

    % Check if each constraint in the set is entailed by all the others.
    % XXX It would be preferable not to use this as it can be very slow.
    %
remove_some_entailed_constraints(Varset, Constraints0, Constraints) :-
    remove_some_entailed_constraints_2(Varset, Constraints0, [], Constraints).

:- pred remove_some_entailed_constraints_2(lp_varset::in, constraints::in,
    constraints::in, constraints::out) is semidet.

remove_some_entailed_constraints_2(_, [], !Constraints).
remove_some_entailed_constraints_2(_, [ E ], !Constraints) :-
    list.cons(E, !Constraints).
remove_some_entailed_constraints_2(Varset, [E, X | Es], !Constraints) :-
    ( if obvious_constraint(E) then
        true
    else
        RestOfMatrix = [X | Es] ++ !.Constraints,
        Result = entailed(Varset, RestOfMatrix, E),
        (
            Result = entailed
        ;
            Result = not_entailed,
            list.cons(E, !Constraints)
        ;
            Result = inconsistent,
            fail
        )
    ),
    remove_some_entailed_constraints_2(Varset, [X | Es], !Constraints).

%-----------------------------------------------------------------------------%
%
% Printing constraints.
%

    % Write out a term - outputs the empty string if the term
    % has a coefficient of zero.
    %
:- pred write_term(lp_varset::in, lp_term::in, io::di, io::uo) is det.
:- pragma consider_used(write_term/4).

write_term(Varset, Var - Coefficient, !IO) :-
    ( if Coefficient > zero then
        io.write_char('+', !IO)
    else
        io.write_char('-', !IO)
    ),
    io.write_string(" (", !IO),
    Num = abs(numer(Coefficient)),
    io.write_string(int_to_string(Num), !IO),
    ( if denom(Coefficient) = 1 then
        true
    else
        io.format("/%s", [s(int_to_string(denom(Coefficient)))], !IO)
    ),
    io.write_char(')', !IO),
    io.write_string(varset.lookup_name(Varset, Var), !IO).

%-----------------------------------------------------------------------------%
%
% Intermodule optimization stuff.
%

% The following predicates write out constraints in a form that is useful
% for (transitive) intermodule optimization.
% XXX This should not be needed; (transitive) intermodule optimization
% should output these constraints only as parts of termination pragmas,
% and that should be done by parse_tree_out_pragma.m.

output_constraints(Stream, OutputVar, Constraints, !IO) :-
    io.write_char(Stream, '[', !IO),
    write_out_list(output_constraint(OutputVar), ", ", Constraints,
        Stream, !IO),
    io.write_char(Stream, ']', !IO).

:- pred output_constraint(output_var::in, constraint::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_constraint(OutputVar, lte(Terms, Constant), Stream, !IO) :-
    io.write_string(Stream, "le(", !IO),
    output_constraint_2(OutputVar, Terms, Constant, Stream, !IO).
output_constraint(OutputVar, eq(Terms, Constant), Stream, !IO) :-
    io.write_string(Stream, "eq(", !IO),
    output_constraint_2(OutputVar, Terms, Constant, Stream, !IO).
output_constraint(_, gte(_,_), _, _, _) :-
    unexpected($pred, "gte").

:- pred output_constraint_2(output_var::in, lp_terms::in, lp_constant::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_constraint_2(OutputVar, Terms, Constant, Stream, !IO) :-
    output_terms(OutputVar, Terms, Stream, !IO),
    io.write_string(Stream, ", ", !IO),
    rat.write_rat(Stream, Constant, !IO),
    io.write_char(Stream, ')', !IO).

:- pred output_terms(output_var::in, lp_terms::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_terms(OutputVar, Terms, Stream, !IO) :-
    io.write_char(Stream, '[', !IO),
    write_out_list(output_term(OutputVar), ", ", Terms, Stream, !IO),
    io.write_char(Stream, ']', !IO).

:- pred output_term(output_var::in, lp_term::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_term(OutputVar, Var - Coefficient, Stream, !IO) :-
    io.format(Stream, "term(%s, ", [s(OutputVar(Var))], !IO),
    rat.write_rat(Stream, Coefficient, !IO),
    io.write_char(Stream, ')', !IO).

%-----------------------------------------------------------------------------%
%
% Debugging predicates for writing out constraints.
%

write_constraints(Constraints, Varset, !IO) :-
    list.foldl(write_constraint(Varset), Constraints, !IO).

:- pred write_constraint(lp_varset::in, constraint::in, io::di, io::uo) is det.

write_constraint(Varset, Constr, !IO) :-
    deconstruct_constraint(Constr, Coeffs, Operator, Constant),
    io.write_char('\t', !IO),
    list.foldl(write_constr_term(Varset), Coeffs, !IO),
    io.format("%s %s\n",
        [s(operator_to_string(Operator)), s(rat.to_string(Constant))], !IO).

:- pred write_constr_term(lp_varset::in, lp_term::in, io::di, io::uo) is det.

write_constr_term(Varset, Var - Coeff, !IO) :-
    VarName = varset.lookup_name(Varset, Var),
    io.format("%s%s ", [s(rat.to_string(Coeff)), s(VarName)], !IO).

:- func operator_to_string(lp_operator) = string.

operator_to_string(lp_lt_eq) = "=<".
operator_to_string(lp_eq )   = "=".
operator_to_string(lp_gt_eq) = ">=".

:- pred write_vars(varset::in, lp_vars::in, io::di, io::uo) is det.
:- pragma consider_used(write_vars/4).

write_vars(Varset, Vars, !IO) :-
    io.write_string("[ ", !IO),
    write_vars_2(Varset, Vars, !IO),
    io.write_string(" ]", !IO).

:- pred write_vars_2(lp_varset::in, lp_vars::in, io::di, io::uo) is det.

write_vars_2(_, [], !IO).
write_vars_2(Varset, [V | Vs], !IO) :-
    io.write_string(var_to_string(Varset, V), !IO),
    (
        Vs = []
    ;
        Vs = [_ | _],
        io.write_string(", ", !IO)
    ),
    write_vars_2(Varset, Vs, !IO).

:- func var_to_string(lp_varset, lp_var) = string.

var_to_string(Varset, Var) = varset.lookup_name(Varset, Var, "Unnamed").

    % Write out the matrix used during fourier elimination.
    % If `Labels' is `yes' then write out the label for each vector as well.
    %
:- pred write_matrix(lp_varset::in, bool::in, matrix::in, io::di, io::uo)
    is det.
:- pragma consider_used(write_matrix/5).

write_matrix(Varset, Labels, Matrix, !IO) :-
    io.write_list(Matrix, "\n", write_vector(Varset, Labels), !IO).

:- pred write_vector(lp_varset::in, bool::in, vector::in, io::di,
    io::uo) is det.

write_vector(Varset, _WriteLabels, vector(_Label, Terms0, Constant), !IO) :-
    Terms = map.to_assoc_list(Terms0),
    list.foldl(write_constr_term(Varset), Terms, !IO),
    io.write_string(" (=<) ", !IO),
    io.write_string(rat.to_string(Constant), !IO).

%-----------------------------------------------------------------------------%

get_vars_from_constraints(Constraints) = Vars :-
    list.foldl(get_vars_from_constraint, Constraints, set.init, Vars).

:- pred get_vars_from_constraint(constraint::in, set(lp_var)::in,
    set(lp_var)::out) is det.

get_vars_from_constraint(Constraint, !SetVar) :-
    get_vars_from_terms(lp_terms(Constraint), !SetVar).

:- pred get_vars_from_terms(lp_terms::in, set(lp_var)::in, set(lp_var)::out)
    is det.

get_vars_from_terms([], !SetVar).
get_vars_from_terms([Var - _ | Coeffs], !SetVar) :-
    set.insert(Var, !SetVar),
    get_vars_from_terms(Coeffs, !SetVar).

%-----------------------------------------------------------------------------%
:- end_module libs.lp_rational.
%-----------------------------------------------------------------------------%
