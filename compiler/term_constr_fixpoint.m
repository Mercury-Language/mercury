%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2002, 2005-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_constr_fixpoint.m.
% Main author: juliensf.
%
% TODO:
% * code for handling calls could do with a cleanup.
%
% NOTE: the code in this module should not refer to things in the HLDS
% (with the exception of the termination2_info slots in the
%  proc_sub_info structure)
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_constr_fixpoint.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.term_constr_data.
:- import_module transform_hlds.term_constr_errors.

:- import_module list.
:- import_module set.

%-----------------------------------------------------------------------------%

    % Derive the argument size constraints for the procedures in this SCC
    % that need them.
    %
:- pred do_fixpoint_calculation(fixpoint_options::in, set(pred_proc_id)::in,
    int::in, list(term2_error)::out, module_info::in, module_info::out) is det.

    % This structure holds the values of options used to control
    % the fixpoint calculation.
    %
:- type fixpoint_options.

    % fixpoint_options_init(Widening, MaxMatrixSize):
    %
    % Initialise the fixpoint_options structure. `Widening' is the threshold
    % after which we invoke widening. `MaxMatrixSize' specifies the maximum
    % number of constraints we allow a matrix to grow to before we abort
    % and try other approximations.
    %
:- func fixpoint_options_init(widening, int) = fixpoint_options.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.lp_rational.
:- import_module libs.options.
:- import_module libs.polyhedron.
:- import_module transform_hlds.term_constr_main_types.
:- import_module transform_hlds.term_constr_util.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%
%
% Perform the fixpoint calculation on the AR.
%

    % The information for each procedure in the SCC returned by a single
    % iteration of the fixpoint calculation.
    %
:- type iteration_info
    --->    iteration_info(
                 ii_ppid            :: pred_proc_id,
                 ii_arg_size_poly   :: polyhedron,
                 ii_change_flag     :: bool
            ).

:- type iteration_infos == list(iteration_info).

do_fixpoint_calculation(Options, SCC, Iteration, [], !ModuleInfo) :-
    AbstractSCC = get_abstract_scc(!.ModuleInfo, SCC),

    % Carry out one iteration of fixpoint computation. We need to do this
    % for all SCCs at least once in order to obtain the argument size
    % constraints for non-recursive procedures. We could do that during
    % the build phase for non-recursive procedures (and in fact used to)
    % but the code ends up being a horrible mess.
    %
    set.foldl(
        term_iterate_over_abstract_proc(!.ModuleInfo, Options, Iteration),
        AbstractSCC, [], IterationInfos),
    ChangeFlag = or_flags(IterationInfos),
    (
        ChangeFlag = yes,
        list.foldl(update_size_info, IterationInfos, !ModuleInfo),
        do_fixpoint_calculation(Options, SCC, Iteration + 1,
            _, !ModuleInfo)
    ;
        ChangeFlag = no,
        % If one of the polyhedra in the SCC has `false' as its
        % argument size constraint then the analysis failed.  In that
        % case set the argument size constraints for every procedure
        % in the SCC to `true'.
        % XXX Should this be happening?
        %
        ( if
            list.member(OneInfo, IterationInfos),
            polyhedron.is_empty(OneInfo ^ ii_arg_size_poly)
        then
            ChangePoly =
                ( func(Info0) = Info :-
                    Identity = polyhedron.universe,
                    Info = Info0 ^ ii_arg_size_poly := Identity
                ),
            list.foldl(update_size_info, list.map(ChangePoly, IterationInfos),
                !ModuleInfo)
        else
            list.foldl(update_size_info, IterationInfos, !ModuleInfo)
        )
    ).

:- func or_flags(iteration_infos) = bool.

or_flags([]) = no.
or_flags([Info | Infos]) = bool.or(Info ^ ii_change_flag, or_flags(Infos)).

:- pred update_size_info(iteration_info::in, module_info::in, module_info::out)
    is det.

update_size_info(Info, !ModuleInfo) :-
    Info = iteration_info(PPId, Poly, _),
    update_arg_size_info(PPId, Poly, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred term_iterate_over_abstract_proc(module_info::in, fixpoint_options::in,
    int::in, abstract_proc::in,
    iteration_infos::in, iteration_infos::out) is det.

term_iterate_over_abstract_proc(ModuleInfo, Options, Iteration, Proc,
        !IterationInfo) :-
    WideningInfo = Options ^ fo_widening,
    MaxMatrixSize = Options ^ fo_max_size,
    AbstractPPId = Proc ^ ap_ppid,
    AbstractPPId = real(PPId),
    SizeVarSet = Proc ^ ap_size_varset,
    Zeros  = Proc ^ ap_zeros,
    HeadVars = Proc ^ ap_head_vars,

    % Print out the debugging traces.
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_term, DebugTerm),
    (
        DebugTerm = yes,
        trace [io(!IO)] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            PPIdStr = pred_proc_id_to_dev_string(ModuleInfo, PPId),
            io.write(DebugStream, PPId, !IO),
            io.format(DebugStream, ": %s ", [s(PPIdStr)], !IO),
            write_size_vars(DebugStream, SizeVarSet, HeadVars, !IO),
            io.format(DebugStream, "\nIteration %d:\n", [i(Iteration)], !IO),
            io.flush_output(DebugStream, !IO)
        )
    ;
        DebugTerm = no
    ),

    % Begin by traversing the procedure and calculating the
    % IR approximation for this iteration.

    Info = init_fixpoint_info(ModuleInfo, SizeVarSet, PPId, MaxMatrixSize,
        HeadVars, Zeros),

    some [!Polyhedron] (
        term_traverse_abstract_goal(Info, Proc ^ ap_body, polyhedron.universe,
            !:Polyhedron),
        polyhedron.optimize(SizeVarSet, !Polyhedron),

        % XXX Bug workaround - the build pass sometimes stuffs up
        % the local variable set for if-then-elses.
        % (See comments in term_constr_build.m).
        BugConstrs0 = polyhedron.constraints(!.Polyhedron),
        ConstrVarsSet = get_vars_from_constraints(BugConstrs0),
        HeadVarSet = set.list_to_set(HeadVars),
        BadVarsSet = set.difference(ConstrVarsSet, HeadVarSet),
        BadVars = set.to_sorted_list(BadVarsSet),
        polyhedron.project_polyhedron(SizeVarSet, BadVars, !Polyhedron),
        polyhedron.optimize(SizeVarSet, !Polyhedron),
        % XXX End of bug workaround.

        % Print out the polyhedron obtained during this iteration.
        (
            DebugTerm = yes,
            trace [io(!IO)] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                polyhedron.write_polyhedron(DebugStream, SizeVarSet,
                    !.Polyhedron, !IO),
                io.nl(DebugStream, !IO),
                io.flush_output(DebugStream, !IO)
            )
        ;
            DebugTerm = no
        ),

        % Look up the constraints obtained during the previous iteration.
        ArgSizeInfo = lookup_proc_constr_arg_size_info(ModuleInfo, PPId),

        % NOTE: `!.Polyhedron' is the set of constraints obtained by
        % *this* iteration. `OldPolyhedron' is the set of constraints
        % obtained by the *previous* iteration -- which may in fact be `empty'
        % (i.e. false).
        (
            % If there were no constraints for the procedure then
            % we are at the beginning of the analysis.
            ArgSizeInfo = no,
            OldPolyhedron = polyhedron.empty
        ;
            ArgSizeInfo = yes(SizeInfo),
            OldPolyhedron = SizeInfo
        ),
        ( if polyhedron.is_empty(!.Polyhedron) then
            ( if polyhedron.is_empty(OldPolyhedron) then
                ChangeFlag = no
            else
                unexpected($pred, "old polyhedron is empty")
            )
        else
            % If the procedure is not recursive then we need only perform one
            % pass over the AR - subsequent iterations will yield the same
            % result.
            ( if Proc ^ ap_recursion = none then
                ChangeFlag = no
            else if polyhedron.is_empty(OldPolyhedron) then
                ChangeFlag = yes
            else
                test_fixpoint_and_perhaps_widen(WideningInfo, SizeVarSet,
                   Iteration, OldPolyhedron, !Polyhedron, ChangeFlag)
            )
        ),
        ThisIterationInfo = iteration_info(PPId, !.Polyhedron, ChangeFlag)
    ),
    !:IterationInfo = [ThisIterationInfo | !.IterationInfo].

%-----------------------------------------------------------------------------%

:- type fixpoint_info
    --->    fixpoint_info(
                tcfi_module_info        :: module_info,
                tcfi_varset             :: size_varset,
                tcfi_ppid               :: pred_proc_id,
                tcfi_max_matrix_size    :: int,
                tcfi_curr_head_vars     :: head_vars,
                tcfi_zeros              :: zero_vars
            ).

:- func init_fixpoint_info(module_info, size_varset, pred_proc_id, int,
    head_vars, zero_vars) = fixpoint_info.

init_fixpoint_info(ModuleInfo, SizeVarSet, PPId, MaxMatrixSize, HeadVars,
        Zeros) =
    fixpoint_info(ModuleInfo, SizeVarSet, PPId, MaxMatrixSize, HeadVars,
        Zeros).

%-----------------------------------------------------------------------------%

:- pred term_traverse_abstract_goal(fixpoint_info::in, abstract_goal::in,
    polyhedron::in, polyhedron::out) is det.

term_traverse_abstract_goal(Info, Goal, !Polyhedron) :-
    (
        Goal = term_disj(Goals, _Size, Locals, _),
        % There are number of possible improvements that should be made here:
        %
        % - Take the intersection each disjunct with the constraints
        %   before the disjunction and compute the convex hull of that.
        %   This is more accurate but slower. (XXX There is some code for this
        %   in term_constr_data.m but it needs to be moved here). To do this
        %   you need to add the constraints that occur to left of the
        %   disjunctions to `PriorConstraints'.
        %
        % - Try computing the convex hull of large disjunctions pairwise
        %   rather than linearly. There is code to do this below but we
        %   currently don't use it.

        PriorConstraints = polyhedron.universe,
        term_traverse_abstract_disj_linearly(Goals, Locals, Info,
            PriorConstraints, Polyhedron0),
        post_process_abstract_goal(Locals, Info, Polyhedron0, !Polyhedron)
    ;
        Goal = term_conj(Goals, Locals, _),
        list.foldl(
            term_traverse_abstract_goal(Info), Goals, polyhedron.universe,
            Polyhedron0),
        post_process_abstract_goal(Locals, Info, Polyhedron0, !Polyhedron)
    ;
        Goal = term_call(CallPPId0, _, CallVars, CallZeros, Locals, _,
            CallArgsPoly),
        CallPPId0 = real(CallPPId),
        module_info_pred_proc_info(Info ^ tcfi_module_info, CallPPId, _,
            CallProcInfo),
        proc_info_get_termination2_info(CallProcInfo, CallTerm2Info),
        CallArgSizeInfo = term2_info_get_success_constrs(CallTerm2Info),
        (
            CallArgSizeInfo = no,
            !:Polyhedron = polyhedron.empty
        ;
            CallArgSizeInfo = yes(SizeInfo),
            ( if polyhedron.is_empty(SizeInfo) then
                !:Polyhedron = polyhedron.empty
            else if polyhedron.is_universe(SizeInfo) then
                % Constraint store += true
                true
            else
                HeadVars = term2_info_get_head_vars(CallTerm2Info),
                SubstMap = create_var_substitution(CallVars, HeadVars),
                Polyhedron0 = polyhedron.substitute_vars(SubstMap,
                    SizeInfo),
                Polyhedron1 = intersection(Polyhedron0, CallArgsPoly),
                % Set any zero_vars in the constraints to zero
                % (i.e. delete the terms). We need to do this
                % when polymorphic arguments are zero sized.
                Polyhedron2 = polyhedron.zero_vars(CallZeros, Polyhedron1),
                post_process_abstract_goal(Locals, Info,
                    Polyhedron2, !Polyhedron)
            )
        )
    ;
        Goal = term_primitive(Poly, Locals, _),
        post_process_abstract_goal(Locals, Info, Poly, !Polyhedron)
    ).

%-----------------------------------------------------------------------------%

:- pred post_process_abstract_goal(size_vars::in, fixpoint_info::in,
    polyhedron::in, polyhedron::in, polyhedron::out) is det.

post_process_abstract_goal(Locals, Info, GoalPolyhedron0, !Polyhedron) :-
    ( if polyhedron.is_empty(GoalPolyhedron0) then
        GoalPolyhedron = polyhedron.empty
    else
        project_polyhedron(Info ^ tcfi_varset, Locals,
            GoalPolyhedron0, GoalPolyhedron)
    ),
    polyhedron.intersection(GoalPolyhedron, !Polyhedron).

%-----------------------------------------------------------------------------%
%
% Predicates for handling disjunctions.
%

    % This version computes the convex hull linearly.
    % That is, ( A ; B ; C ; D) is processed as:
    %
    %  ((((empty \/ A ) \/ B ) \/ C ) \/ D)
    %
:- pred term_traverse_abstract_disj_linearly(abstract_goals::in,
    size_vars::in, fixpoint_info::in, polyhedron::in, polyhedron::out) is det.

term_traverse_abstract_disj_linearly(Goals, Locals, Info, !Polyhedron) :-
    list.foldl(term_traverse_abstract_disj_linearly_2(Info, Locals),
        Goals, polyhedron.empty, ConvexUnion),
    polyhedron.intersection(ConvexUnion, !Polyhedron).

:- pred term_traverse_abstract_disj_linearly_2(fixpoint_info::in,
    size_vars::in, abstract_goal::in, polyhedron::in, polyhedron::out) is det.

term_traverse_abstract_disj_linearly_2(Info, Locals, Goal, !Polyhedron) :-
    SizeVarSet = Info ^ tcfi_varset,
    term_traverse_abstract_goal(Info, Goal, polyhedron.universe, Polyhedron0),
    project_polyhedron(SizeVarSet, Locals, Polyhedron0, Polyhedron1),
    polyhedron.convex_union(SizeVarSet, yes(Info ^ tcfi_max_matrix_size),
        Polyhedron1, !Polyhedron).

    % This version computes the convex hull pairwise. That is
    % ( A ; B ; C ; D) is processed as: (( A \/ B ) \/ ( C \/ D)).
    %
    % XXX This code is currently unused.
    %
:- pred term_traverse_abstract_disj_pairwise(abstract_goals::in, size_vars::in,
    fixpoint_info::in, polyhedron::in, polyhedron::out) is det.
:- pragma consider_used(pred(term_traverse_abstract_disj_pairwise/5)).

term_traverse_abstract_disj_pairwise(Goals, Locals, Info, !Polyhedron) :-
    SizeVarSet = Info ^ tcfi_varset,
    % XXX at the moment, could be !.Poly...
    PolyToLeft = polyhedron.universe,

    % First convert the list of goals into their corresponding polyhedra.
    ToPoly =
        ( func(Goal) = Poly :-
            term_traverse_abstract_goal(Info, Goal, PolyToLeft, Poly0),
            polyhedron.project_polyhedron(SizeVarSet, Locals, Poly0, Poly)
        ),
    Polyhedra0 = list.map(ToPoly, Goals),

    % Now pairwise convex hull them.
    HullOp =
        ( func(A, B) = C :-
            polyhedron.convex_union(SizeVarSet,
                yes(Info ^ tcfi_max_matrix_size), A, B, C)
        ),
    ConvexUnion = pairwise_map(HullOp, [ polyhedron.empty | Polyhedra0]),
    polyhedron.intersection(ConvexUnion, !Polyhedron).

    % This assumes that the operation in question is associative and
    % commutative.
    %
 :- func pairwise_map(func(T, T) = T, list(T)) = T.

pairwise_map(_, []) = _ :-
    unexpected($pred, "empty list").
pairwise_map(_, [X]) = X.
pairwise_map(Op, List @ [_, _ | _]) = X :-
    pairwise_map_2(Op, List, [], X0),
    X = pairwise_map(Op, X0).

:- pred pairwise_map_2((func(T, T) = T)::in, list(T)::in,
    list(T)::in, list(T)::out) is det.

pairwise_map_2(_, [], !Acc).
pairwise_map_2(_, [X], Acc, [X | Acc]).
pairwise_map_2(Op, [X, Y | Rest], !Acc) :-
    !:Acc = [Op(X, Y) | !.Acc],
    pairwise_map_2(Op, Rest, !Acc).

%-----------------------------------------------------------------------------%
%
% Fixpoint test.
%

:- pred test_fixpoint_and_perhaps_widen(widening::in, size_varset::in, int::in,
    polyhedron::in, polyhedron::in, polyhedron::out, bool::out) is det.

test_fixpoint_and_perhaps_widen(after_fixed_cutoff(Threshold), SizeVarSet,
        Iteration, OldPoly, NewPoly, ResultPoly, ChangeFlag) :-
    ( if Iteration > Threshold then
        ResultPoly = widen(OldPoly, NewPoly, SizeVarSet)
    else
        ResultPoly = NewPoly
    ),
    ChangeFlag = test_fixpoint(NewPoly, OldPoly, SizeVarSet).

:- func test_fixpoint(polyhedron, polyhedron, size_varset) = bool.

test_fixpoint(NewPoly, OldPoly, SizeVarSet) = ChangeFlag :-
    % Constraints from this iteration.
    NewConstraints = polyhedron.non_false_constraints(NewPoly),
    % Constraints from previous iteration.
    OldConstraints = polyhedron.non_false_constraints(OldPoly),
    ( if
        some [OldConstraint] (
            list.member(OldConstraint, OldConstraints),
            not entailed(SizeVarSet, NewConstraints, OldConstraint)
        )
    then
        ChangeFlag = yes
    else
        ChangeFlag = no
    ).

%-----------------------------------------------------------------------------%

:- type fixpoint_options
    --->    fixpoint_options(
                fo_widening :: widening,
                fo_max_size :: int
            ).

fixpoint_options_init(Widening, MaxMatrixSize) =
    fixpoint_options(Widening, MaxMatrixSize).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_constr_fixpoint.
%-----------------------------------------------------------------------------%
