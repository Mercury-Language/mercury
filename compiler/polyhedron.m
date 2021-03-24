%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003, 2005-2007, 2009-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: polyhedron.m.
% Main author: juliensf.
%
% Provides closed convex polyhedra over Q^n.
% These are useful as an abstract domain for describing numerical relational
% information.
%
% The set of closed convex polyhedra is partially ordered by subset inclusion.
% It forms a lattice with intersection as the binary meet operation and convex
% hull as the binary join operation.
%
% This module includes procedures for:
%   - computing the intersection of two convex polyhedra
%   - computing the convex hull of two convex polyhedra
%   - approximating the convex union of two convex polyhedra by means
%     other than the convex hull when that becomes too computationally
%     expensive.
%   - converting a convex polyhedron to and from an equivalent system
%     of linear constraints.
%
% It also includes an implementation of widening for convex polyhedra.
%
% NOTE: many of the operations in this module require you to pass in
% the varset that the variables in the constraints that define the polyhedron
% were allocated from. This because the code for computing the convex hull
% and the linear solver in lp_rational.m need to allocate fresh temporary
% variables.
%
% XXX We could avoid this with some extra work.
%
% TODO:
%   * See if using the double description method is any faster.
%
%-----------------------------------------------------------------------------%

:- module libs.polyhedron.
:- interface.

:- import_module libs.lp_rational.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

:- type polyhedron.

:- type polyhedra == list(polyhedron).

%-----------------------------------------------------------------------------%

    % The `empty' polyhedron. Equivalent to the constraint `false'.
    %
:- func empty = polyhedron.

    % The `universe' polyhedron. Equivalent to the constraint `true'.
    %
:- func universe = polyhedron.

    % Constructs a convex polyhedron from a system of linear constraints.
    %
:- func from_constraints(constraints) = polyhedron.

    % Returns a system of constraints whose solution space defines
    % the given polyhedron.
    %
:- func constraints(polyhedron) = constraints.

    % As above but throws an exception if the given polyhedron is empty.
    %
:- func non_false_constraints(polyhedron) = constraints.

    % Succeeds iff the given polyhedron is the empty polyhedron.
    %
    % NOTE: this only succeeds if the polyhedron is actually *known*
    % to be empty. It might fail even when the constraint set is
    % is inconsistent. You currently need to call polyhedron.optimize
    % to force this to always work.
    %
:- pred is_empty(polyhedron::in) is semidet.

    % Succeeds iff the given polyhedron is the `universe' polyhedron,
    % that is the one whose constraint representation corresponds to `true'.
    % (ie. it is unbounded in all dimensions).
    %
:- pred is_universe(polyhedron::in) is semidet.

    % Optimizes the representation of a polyhedron.
    % At the moment this performs a consistency check and then replaces the
    % polyhedron by empty if necessary.
    %
:- pred optimize(lp_varset::in, polyhedron::in, polyhedron::out) is det.

    % intersection(A, B, C):
    % The polyhedron `C' is the intersection of  the polyhedra `A' and `B'.
    %
:- func intersection(polyhedron, polyhedron) = polyhedron.
:- pred intersection(polyhedron::in, polyhedron::in, polyhedron::out) is det.

    % Returns a polyhedron that is a closed convex approximation of
    % union of the two polyhedra.
    %
:- func convex_union(lp_varset, polyhedron, polyhedron) = polyhedron.
:- pred convex_union(lp_varset::in, polyhedron::in, polyhedron::in,
    polyhedron::out) is det.

    % As above but takes an extra argument that weakens the approximation even
    % further if the size of the internal matrices exceeds the supplied
    % threshold
    %
:- func convex_union(lp_varset, maybe(int), polyhedron, polyhedron)
    = polyhedron.
:- pred convex_union(lp_varset::in, maybe(int)::in, polyhedron::in,
    polyhedron::in, polyhedron::out) is det.

    % Approximate a (convex) polyhedron by a rectangular region
    % whose sides are parallel to the axes.
    %
:- func bounding_box(polyhedron, lp_varset) = polyhedron.

    % polyhedron.widen(A, B, VarSet) = C.
    % Remove faces from the polyhedron `A' to form the polyhedron `C'
    % according to the rules that the smallest number of faces
    % should be removed and that `C' must be a superset of `B'.
    % This operation is not commutative.
    %
:- func widen(polyhedron, polyhedron, lp_varset) = polyhedron.

    % project_all(VarSet, Variables, Polyhedra) returns a list
    % of polyhedra in which the variables listed have been eliminated
    % from each polyhedron.
    %
:- func project_all(lp_varset, lp_vars, polyhedra) = polyhedra.

:- pred project_polyhedron(lp_varset::in, lp_vars::in,
    polyhedron::in, polyhedron::out) is det.

    % XXX It might be nicer to think of this as relabelling the axes.
    % Conceptually it alters the names of the variables in the polyhedron
    % without explicitly converting it back into constraint form - this is
    % easy to do (at the moment) as the polyhedra are represented as
    % constraints anyway.
    %
:- func substitute_vars(lp_vars, lp_vars, polyhedron) = polyhedron.
:- func substitute_vars(map(lp_var, lp_var), polyhedron) = polyhedron.

    % polyhedron.zero_vars(Set, Polyhedron0) = Polyhedron <=>
    %
    %   Constraints0 = polyhedron.constraints(Polyhedron0),
    %   Constraints  = lp_rational.set_vars_to_zero(Set, Constraints0)
    %   Polyhedron   = polyhedron.from_constraints(Constraints)
    %
    % This is a little more efficient than the above because
    % we don't end up traversing the list of constraints as much.
    %
:- func zero_vars(set(lp_var), polyhedron) = polyhedron.

    % Print out the polyhedron using the names of the variables in the varset.
    %
:- pred write_polyhedron(io.text_output_stream::in, lp_varset::in,
    polyhedron::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.rat.

:- import_module pair.
:- import_module require.
:- import_module varset.

%-----------------------------------------------------------------------------%

    % XXX The constructor eqns/1 should really be called something
    % more meaningful.
    %
:- type polyhedron
    --->    eqns(constraints)
    ;       empty_poly.

%-----------------------------------------------------------------------------%
%
% Creation of polyhedra.
%

empty = empty_poly.

universe = eqns([]).

    % This does the following:
    %   - checks if the constraint is false.
    %   - simplifies the representation of the constraint.
    %    - calls intersection/3 (which does further simplifications).
    %
from_constraints([]) = eqns([]).
from_constraints([C | Cs]) = Polyhedron :-
    ( if lp_rational.is_false(C) then
        Polyhedron = empty
    else
        Polyhedron0 = polyhedron.from_constraints(Cs),
        Polyhedron = polyhedron.intersection(eqns([C]), Polyhedron0)
    ).

constraints(eqns(Constraints)) = Constraints.
constraints(empty_poly) = [lp_rational.false_constraint].

non_false_constraints(eqns(Constraints)) = Constraints.
non_false_constraints(empty_poly) =
    unexpected($pred, "empty polyhedron").

is_empty(empty_poly).

is_universe(eqns(Constraints)) :-
    list.all_true(lp_rational.nonneg_constr, Constraints).

optimize(_, empty_poly, empty_poly).
optimize(VarSet, eqns(Constraints0), Result) :-
    Constraints = simplify_constraints(Constraints0),
    ( if inconsistent(VarSet, Constraints) then
        Result = empty_poly
    else
        Result = eqns(Constraints)
    ).

%-----------------------------------------------------------------------------%
%
% Intersection.
%

intersection(empty_poly, _) = empty_poly.
intersection(eqns(_), empty_poly) = empty_poly.
intersection(eqns(MatrixA), eqns(MatrixB)) = eqns(Constraints) :-
    Constraints0 = MatrixA ++ MatrixB,
    restore_equalities(Constraints0, Constraints1),
    Constraints = simplify_constraints(Constraints1).

intersection(PolyA, PolyB, polyhedron.intersection(PolyA, PolyB)).

%-----------------------------------------------------------------------------%
%
% Convex union.
%
% XXX At the moment this just calls convex_hull; it should actually back
% out of the convex_hull calculation if it gets too expensive (we can
% keep track of the size of the matrices during projection) and use a
% bounding box approximation instead.
%

convex_union(VarSet, PolyhedronA, PolyhedronB) = Polyhedron :-
    convex_union(VarSet, no, PolyhedronA, PolyhedronB, Polyhedron).

convex_union(VarSet, PolyhedronA, PolyhedronB, Polyhedron) :-
    convex_union(VarSet, no, PolyhedronA, PolyhedronB, Polyhedron).

convex_union(VarSet, MaxMatrixSize, PolyhedronA, PolyhedronB) = Polyhedron :-
    convex_union(VarSet, MaxMatrixSize, PolyhedronA, PolyhedronB, Polyhedron).

convex_union(_, _, empty_poly, empty_poly, empty_poly).
convex_union(_, _, eqns(Constraints), empty_poly,
    eqns(Constraints)).
convex_union(_, _, empty_poly, eqns(Constraints),
    eqns(Constraints)).
convex_union(VarSet, MaybeMaxSize, eqns(ConstraintsA),
        eqns(ConstraintsB), Hull) :-
    convex_hull([ConstraintsA, ConstraintsB], Hull, MaybeMaxSize, VarSet).

%-----------------------------------------------------------------------------%
%
% Convex hull calculation.
%

% The following transformation for computing the convex hull is described in
% the paper:
%
% F. Benoy and A. King. Inferring Argument Size Relationships with CLPR(R).
% Logic-based Program Synthesis and Transformation,
% LNCS 1207: pp. 204-223, 1997.
%
% Further details can be found in:
%
% F. Benoy, A. King, and F. Mesnard.
% Computing Convex Hulls with a Linear Solver
% Theory and Practice of Logic Programming 5(1&2):259-271, 2005.

:- type convex_hull_result
    --->    ok(polyhedron)
    ;       aborted.

:- type var_map == map(lp_var, lp_var).

:- type var_maps == list(var_map).

    % We introduce sigma variables into the constraints as
    % part of the transformation (See the above papers for details).
    %
:- type sigma_var == lp_var.

:- type sigma_vars == list(sigma_var).

:- type polyhedra_info
    --->    polyhedra_info(
                % There is one of these for each polyhedron. It maps the
                % original variables in the constraints to the temporary
                % variables introduced by the transformation.
                % A variable that occurs in more than one polyhedron
                % is mapped to a separate temporary variable for each one.
                var_maps        :: var_maps,

                % The sigma variables introduced by the transformation.
                sigmas          :: sigma_vars,

                % The varset the variables are allocated. The temporary
                % and sigma variables need to be allocated from this as well
                % in order to prevent clashes when using the solver.
                poly_varset     :: lp_varset
            ).

:- type constr_info
    --->    constr_info(
                % Map from original variables to new (temporary) ones.
                % There is one of these for each constraint.
                var_map         :: var_map,

                constr_varset   :: lp_varset
            ).

:- pred convex_hull(list(constraints)::in, polyhedron::out, maybe(int)::in,
    lp_varset::in) is det.

convex_hull([], _, _, _) :-
    unexpected($pred, "empty list").
convex_hull([Poly], eqns(Poly), _, _).
convex_hull(Polys @ [_, _ | _], ConvexHull, MaybeMaxSize, VarSet0) :-
    % Perform the matrix transformation from the paper by Benoy and King.
    % Rename variables and add sigma constraints as necessary.
    PolyInfo0 = polyhedra_info([], [], VarSet0),
    transform_polyhedra(Polys, Matrix0, PolyInfo0, PolyInfo),
    PolyInfo = polyhedra_info(VarMaps, Sigmas, VarSet),
    add_sigma_constraints(Sigmas, Matrix0, Matrix1),
    Matrix   = add_last_constraints(Matrix1, VarMaps),
    AppendValues =
        ( func(Map, Varlist0) = Varlist :-
            Varlist = Varlist0 ++ map.values(Map)
        ),
    VarsToEliminate = Sigmas ++ list.foldl(AppendValues, VarMaps, []),

    % Calculate the closure of the convex hull of the original polyhedra by
    % projecting the constraints in the transformed matrix onto the original
    % variables (ie. eliminate all the sigma and temporary variables).
    % Since the resulting matrix tends to contain a large number of redundant
    % constraints, we need to do a redundancy check after this.
    project_constraints_maybe_size_limit(VarSet, MaybeMaxSize, VarsToEliminate,
        Matrix, ProjectionResult),
    (
        % XXX We should try using a bounding box first.
        ProjectionResult = pr_res_aborted,
        ConvexHull = eqns(lp_rational.nonneg_box(VarsToEliminate, Matrix))
    ;
        ProjectionResult = pr_res_inconsistent,
        ConvexHull = empty_poly
    ;
        some [!Hull] (
            ProjectionResult = pr_res_ok(!:Hull),
            restore_equalities(!Hull),
            % XXX We should try removing this call to simplify constraints.
            %     It seems unnecessary.
            !:Hull = simplify_constraints(!.Hull),
            ( if remove_some_entailed_constraints(VarSet, !Hull)
            then ConvexHull = eqns(!.Hull)
            else ConvexHull = empty_poly
            )
        )
    ).

:- pred transform_polyhedra(list(constraints)::in, constraints::out,
    polyhedra_info::in, polyhedra_info::out) is det.

transform_polyhedra(Polys, Eqns, !PolyInfo) :-
    list.foldl2(transform_polyhedron, Polys, [], Eqns, !PolyInfo).

:- pred transform_polyhedron(constraints::in, constraints::in,
    constraints::out, polyhedra_info::in, polyhedra_info::out) is det.

transform_polyhedron(Poly, Polys0, Polys, !PolyInfo) :-
    some [!VarSet] (
        !.PolyInfo = polyhedra_info(VarMaps, Sigmas, !:VarSet),
        varset.new_var(Sigma, !VarSet),
        list.map_foldl2(transform_constraint(Sigma), Poly, NewEqns,
            map.init, VarMap, !VarSet),
        Polys = NewEqns ++ Polys0,
        !:PolyInfo = polyhedra_info([VarMap | VarMaps], [Sigma | Sigmas],
            !.VarSet)
    ).

    % transform_constraint: takes a constraint (with original variables) and
    % the sigma variable to add, and returns the constraint where the original
    % variables are substituted for new ones and where the sigma variable is
    % included. The map of old to new variables is updated if necessary.
    %
:- pred transform_constraint(lp_var::in, constraint::in, constraint::out,
    var_map::in, var_map::out, lp_varset::in, lp_varset::out) is det.

transform_constraint(Sigma, !Constraint, !VarMap, !VarSet) :-
    some [!Terms] (
        deconstruct_constraint(!.Constraint, !:Terms, Op, Const),
        list.map_foldl2(change_var, !Terms, !VarMap, !VarSet),
        list.cons(Sigma - (-Const), !Terms),
        !:Constraint = construct_constraint(!.Terms, Op, zero)
    ).

    % change_var: takes a Var-Num pair with an old variable and returns one
    % with a new variable which the old variable maps to. Updates the map of
    % old to new variables if necessary.
    %
:- pred change_var(lp_term::in, lp_term::out, var_map::in, var_map::out,
    lp_varset::in, lp_varset::out) is det.

change_var(!Term, !VarMap, !VarSet) :-
    some [!Var] (
        !.Term = !:Var - Coefficient,

        % Have we already mapped this original variable to a new one?
        ( if map.search(!.VarMap, !.Var, !:Var) then
            true
        else
            varset.new_var(NewVar, !VarSet),
            map.det_insert(!.Var, NewVar, !VarMap),
            !:Var = NewVar
        ),
        !:Term = !.Var - Coefficient
    ).

:- pred add_sigma_constraints(sigma_vars::in,
    constraints::in, constraints::out) is det.

add_sigma_constraints(Sigmas, !Constraints) :-
    % Add non-negativity constraints for each sigma variable.
    SigmaConstraints = list.map(make_nonneg_constr, Sigmas),
    list.append(SigmaConstraints, !Constraints),

    % The sum of all the sigma variables is one.
    SigmaTerms = list.map(lp_term, Sigmas),
    list.cons(construct_constraint(SigmaTerms, lp_eq, one), !Constraints).

    % Add a constraint specifying that each variable is the sum of the
    % temporary variables to which it has been mapped.
    %
:- func add_last_constraints(constraints, var_maps) = constraints.

add_last_constraints(!.Constraints, VarMaps) = !:Constraints :-
    Keys = get_keys_from_maps(VarMaps),
    NewLastConstraints = set.filter_map(make_last_constraint(VarMaps), Keys),
    list.append(set.to_sorted_list(NewLastConstraints), !Constraints).

    % Return the set of keys in the given list of maps.
    %
:- func get_keys_from_maps(var_maps) = set(lp_var).

get_keys_from_maps(Maps) = list.foldl(get_keys_from_map, Maps, set.init).

:- func get_keys_from_map(var_map, set(lp_var)) = set(lp_var).

get_keys_from_map(Map, KeySet) = set.insert_list(KeySet, map.keys(Map)).

:- func make_last_constraint(var_maps, lp_var) = constraint is semidet.

make_last_constraint(VarMaps, OriginalVar) = Constraint :-
    list.foldl(make_last_terms(OriginalVar), VarMaps, [], LastTerms),
    AllTerms = [OriginalVar - one | LastTerms],
    Constraint = construct_constraint(AllTerms, lp_eq, zero).

:- pred make_last_terms(lp_var::in, var_map::in, lp_terms::in, lp_terms::out)
    is semidet.

make_last_terms(OriginalVar, VarMap, !Terms) :-
    map.search(VarMap, OriginalVar, NewVar),
    list.cons(NewVar - (-one), !Terms).

%-----------------------------------------------------------------------------%
%
% Approximation of a polyhedron by a bounding box.
%

bounding_box(empty_poly, _) = empty_poly.
bounding_box(eqns(Constraints), VarSet) =
    eqns(lp_rational.bounding_box(VarSet, Constraints)).

%-----------------------------------------------------------------------------%
%
% Widening.
%

widen(empty_poly, empty_poly, _) = empty_poly.
widen(eqns(_), empty_poly, _) =
    unexpected($pred, "empty polyhedron").
widen(empty_poly, eqns(_), _) =
    unexpected($pred, "empty polyhedron").
widen(eqns(Poly1), eqns(Poly2), VarSet) = eqns(WidenedEqns) :-
    WidenedEqns = list.filter(entailed(VarSet, Poly2), Poly1).

%-----------------------------------------------------------------------------%
%
% Projection.
%

project_all(VarSet, Locals, Polyhedra) =
    list.map(
        ( func(Poly0) = Poly :-
            (
                Poly0 = eqns(Constraints0),
                project_constraints(VarSet, Locals, Constraints0,
                    ProjectionResult),
                (
                    ProjectionResult = pr_res_aborted,
                    unexpected($pred, "abort from project")
                ;
                    ProjectionResult = pr_res_inconsistent,
                    Poly = empty_poly
                ;
                    ProjectionResult = pr_res_ok(Constraints1),
                    restore_equalities(Constraints1, Constraints),
                    Poly = eqns(Constraints)
                )
            ;
                Poly0 = empty_poly,
                Poly  = empty_poly
            )
        ), Polyhedra).

project_polyhedron(_, _, empty_poly, empty_poly).
project_polyhedron(VarSet, Vars, eqns(Constraints0), Result) :-
    lp_rational.project_constraints(VarSet, Vars,
        Constraints0, ProjectionResult),
    (
        ProjectionResult = pr_res_aborted,
        unexpected($pred, "abort from project")
    ;
        ProjectionResult = pr_res_inconsistent,
        Result = empty_poly
    ;
        ProjectionResult = pr_res_ok(Constraints1),
        restore_equalities(Constraints1, Constraints),
        Result = eqns(Constraints)
    ).

%-----------------------------------------------------------------------------%
%
% Variable substitution.
%

substitute_vars(OldVars, NewVars, Polyhedron0) = Polyhedron :-
    Constraints0 = polyhedron.non_false_constraints(Polyhedron0),
    Constraints = lp_rational.substitute_vars(OldVars, NewVars, Constraints0),
    Polyhedron = polyhedron.from_constraints(Constraints).

substitute_vars(SubstMap, Polyhedron0) = Polyhedron :-
    Constraints0 = polyhedron.non_false_constraints(Polyhedron0),
    Constraints = lp_rational.substitute_vars(SubstMap, Constraints0),
    Polyhedron = polyhedron.from_constraints(Constraints).

%-----------------------------------------------------------------------------%
%
% Zeroing out variables.
%

zero_vars(_, empty_poly) = empty_poly.
zero_vars(Vars, eqns(Constraints0)) = eqns(Constraints) :-
    Constraints = lp_rational.set_vars_to_zero(Vars, Constraints0).

%-----------------------------------------------------------------------------%
%
% Printing.
%

write_polyhedron(Stream, VarSet, Polyhedron, !IO) :-
    (
        Polyhedron = empty_poly,
        io.write_string(Stream, "\tEmpty\n", !IO)
    ;
        Polyhedron = eqns(Constraints),
        (
            Constraints = [],
            io.write_string(Stream, "\tUniverse\n", !IO)
        ;
            Constraints = [_ | _],
            lp_rational.write_constraints(Stream, VarSet, Constraints, !IO)
        )
    ).

%-----------------------------------------------------------------------------%
:- end_module libs.polyhedron.
%-----------------------------------------------------------------------------%
