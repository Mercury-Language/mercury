%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: size_prof.m
% Author: zs.

% This module performs a source-to-source program transformation that
% implements term size profiling. The objective of the transformation is to
% make it possible to find out the size of every term in constant time, i.e.
% *without* traversing the term. (If finding out the size of a term required
% traversing the term, the cost of this traversal would dominate the cost of
% most procedures that took that term as input, and thus the traversal
% overhead would introduce a "noise" that would overwhelm the "signal" that
% profiling is trying to measure.) We can thus match the time taken by a
% procedure against the size of its inputs, and use curve-fitting to find out
% its actual complexity. (The theoretical minimum and maxiumum complexities of
% most real algorithms are so different that they are of no use.)
%
% The obvious way to avoid the traversal overhead on size lookup is to
% calculate the size when the term is being constructed, which requires
% traversing it anyway. In term profiling grades, we reserve an extra word at
% the start of every memory cell (with a few exceptions explained below) that
% stores the size of the whole term the memory cell is the top of.  The size
% is defined as the memory words in the term or the number of memory cells in
% the term, depending on the grade.
%
% The main job of this module is to annotate every construction unification
% with the information that the code generator needs to fill in the term size
% slot. In order to do this, it must be able to find out the sizes of the
% arguments, which in turn requires knowing the arguments' types.  (Without
% type information, we cannot distinguish a pointer from an integer.) Most of
% the code in this module is concerned with adding code to the procedure being
% transformed to find or construct the typeinfos we need in order to find out
% the sizes of subterms, mainly because we want to minimize the number of
% goals that construct typeinfos that we add to the procedure body.
%
% A minor job of this transformation is to look for places where the procedure
% fills in a previously undefined field in a cell, and to add code at those
% places to destructively increment the size slot of the cell by the size of
% the newly added subterm.
%
% In theory, when this happens, we should also increase the sizes of all the
% terms containing the term that had one or more of its fields instantiated.
% We do not do so, because doing that would require a lot more machinery.
% However, given our lack of support for partially instantiated data
% structures, and the fact that the correctness of the program does not in
% fact require term sizes to be computed accurately, the problem this poses
% can be safely ignored.
%
% The transformation we perform is not optimal: for example, if two branches
% of a switch bind a variable to terms of the same size, we don't exploit this
% fact. The transformation tries to get all the "low-hanging fruit"; we will
% go after higher hanging fruit if and when a performance evaluation says we
% need to.
%
% We do not associate sizes with the memory cells of a small set of types,
% including type_infos, type_class_infos, closures and boxed floats.  The two
% reasons for this are that (1) the sizes of values of these types practically
% never control the complexity of a procedure, so there is no need for their
% sizes, and (2) this allows us to create e.g. static type_info structures
% without worrying about term size slots. The set of type categories whose
% values are always considered zero sized is defined by the predicate
% zero_size_type in term_norm.m.
%
% We currently do not associate sizes with data types which are handled mostly
% by hand-written C code in the runtime system or in the standard library:
% strings, arrays, higher-order values and foreign types. Keeping their sizes
% would require a lot of extra work and (in the case of foreign types)
% cooperation from the programmer. In the case of arrays, their destructive
% updates also pose the same problems with respect to the propagation of size
% changes as the instantiation of free variables in cells. Maintaining sizes
% for strings and arrays would be desirable, since real programs do contain
% predicates whose complexity is governed by the length of an array or a
% string, but this remains future work.

%-----------------------------------------------------------------------------%

:- module transform_hlds__size_prof.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.

%-----------------------------------------------------------------------------%

    % Specifies how term sizes are to be measured.
    %
:- type construct_transform
    --->    term_words
    ;       term_cells.

    % Perform the transformation on the specified predicate.
    %
:- pred process_proc_msg(construct_transform::in, pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.simplify.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.term_norm.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module svmap.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

% The transformation maintains several maps that allows it to minimize
% the number of constructions of typeinfos it needs to add to the procedure
% body.
%
% If there is a variable live at the current program point that already
% contains a type_info for a given type, then the type_info_map will record
% its identity. The type_ctor_map does likewise for variables that contain the
% type_ctor_infos of type constructors. However, we treat type_ctor_map
% differently from type_info_maps, because the tradeoff are different.
% Creating a new type_ctor_info reference is cheap: just return a pointer to a
% static compiler-generated data structure. Creating a new type_info isn't
% cheap: it requires memory allocation. This is why in some places (calls,
% ends of branched control structures) we simply clean out the type_ctor_map:
% it is cheaper to recreate a type_ctor_info than to store it or move it
% around.
%
% The rev_type_info_map and rev_type_ctor_map contain the same information
% as type_info_map and type_ctor_map respectively, only indexed by the
% program variable, not by the type or type constructor.
%
% If each arm of a branched control structure creates a type_info for a given
% type but make different variables hold this type_info, then the code after
% the branched control structure, not knowing which branch was taken, cannot
% look up the type_info in any one of these variables, and is instead forced
% to allocate that same type_info anew. The purpose of the target_type_info_map
% is to minimize the number of places where we have to do this. If an earlier
% branch has put the typeinfo for a given type into a given variable, then we
% record this fact in the target_type_info_map, so that when a later branch
% needs a type_info for the same type, it will put it into the same variable.
% Of course, this pays off only if all branches allocate a type_info for the
% type, but this does happen reasonably often. When it doesn't, the variable
% that two or more branches use to store the type_into may need to be named
% apart, which is why we invoke quantification after our transformation is
% finished.
%
% It is of course better to find out the sizes of terms at compile time
% than at runtime. The known_size_map maps each variable whose size is known
% to its size.
%
% The varset and vartypes fields come from the proc_info of the procedure being
% transformed, and their modified versions (updated when the transformation
% creates new variables) are put back into the procedure's new proc_info.
%
% The construct_transform field specifies how term sizes are to be measured.
%
% The rtti_varmaps specifies which program variables hold the type_infos
% of which type variables.
%
% The module_info is needed by some utility predicates called by the
% transformation.

:- type type_info_map       == map(mer_type, prog_var).
:- type type_ctor_map       == map(type_ctor, prog_var).
:- type rev_type_info_map   == vartypes.
:- type rev_type_ctor_map   == map(prog_var, type_ctor).
:- type known_size_map      == map(prog_var, int).

:- type size_prof__info
    --->    size_prof_info(
                type_ctor_map           :: type_ctor_map,
                type_info_map           :: type_info_map,
                rev_type_ctor_map       :: rev_type_ctor_map,
                rev_type_info_map       :: rev_type_info_map,
                target_type_info_map    :: type_info_map,
                known_size_map          :: known_size_map,
                varset                  :: prog_varset,
                vartypes                :: vartypes,
                transform_op            :: construct_transform,
                rtti_varmaps            :: rtti_varmaps,
                module_info             :: module_info
            ).

process_proc_msg(Transform, PredId, ProcId, ProcInfo0, ProcInfo,
        !ModuleInfo, !IO) :-
    globals__io_lookup_bool_option(very_verbose, VeryVerbose, !IO),
    (
        VeryVerbose = yes,
        io__write_string("% Adding typeinfos in ", !IO),
        hlds_out__write_pred_proc_id(!.ModuleInfo, PredId, ProcId, !IO),
        io__write_string(": ", !IO),
        process_proc(Transform, PredId, ProcId, ProcInfo0, ProcInfo,
            !ModuleInfo, !IO),
        io__write_string("done.\n", !IO)
    ;
        VeryVerbose = no,
        process_proc(Transform, PredId, ProcId, ProcInfo0, ProcInfo,
            !ModuleInfo, !IO)
    ).

:- pred process_proc(construct_transform::in, pred_id::in, proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    io::di, io::uo) is det.

process_proc(Transform, PredId, ProcId, !ProcInfo, !ModuleInfo, !IO) :-
    Simplifications = [],
    simplify_proc_return_msgs(Simplifications, PredId, ProcId,
        !ModuleInfo, !ProcInfo, _Msgs, !IO),

    proc_info_goal(!.ProcInfo, Goal0),
    proc_info_varset(!.ProcInfo, VarSet0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
    proc_info_rtti_varmaps(!.ProcInfo, RttiVarMaps),
    % The with_types are needed to avoid a combinatorial explosion
    % of ambiguity in the type checker.
    TypeCtorMap0 = map__init `with_type` type_ctor_map,
    TypeInfoMap0 = map__init `with_type` type_info_map,
    RevTypeCtorMap0 = map__init `with_type` rev_type_ctor_map,
    RevTypeInfoMap0 = map__init `with_type` rev_type_info_map,
    TargetTypeInfoMap0 = map__init `with_type` type_info_map,
    KnownSizeMap0 = map__init `with_type` known_size_map,
    Info0 = size_prof_info(TypeCtorMap0, TypeInfoMap0,
        RevTypeCtorMap0, RevTypeInfoMap0, TargetTypeInfoMap0,
        KnownSizeMap0, VarSet0, VarTypes0, Transform, RttiVarMaps,
        !.ModuleInfo),
    rtti_varmaps_tvars(RttiVarMaps, TVars),
    list__foldl(record_typeinfo_in_type_info_varmap(RttiVarMaps), TVars,
        Info0, Info1),
    process_goal(Goal0, Goal1, Info1, Info),

        % We need to fix up goal_infos by recalculating
        % the nonlocal vars and the non-atomic instmap deltas.
    proc_info_headvars(!.ProcInfo, HeadVars),
    proc_info_inst_varset(!.ProcInfo, InstVarSet),
    implicitly_quantify_clause_body(HeadVars, _Warnings, Goal1, Goal2,
        Info ^ varset, VarSet, Info ^ vartypes, VarTypes),
    recompute_instmap_delta(no, Goal2, Goal, VarTypes, InstVarSet,
        InstMap0, !ModuleInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

:- pred process_goal(hlds_goal::in, hlds_goal::out, info::in, info::out)
    is det.

process_goal(Goal0, Goal, !Info) :-
    Goal0 = GoalExpr0 - GoalInfo0,
    (
        GoalExpr0 = unify(LHS, RHS, UniMode, Unify0, UnifyContext),
        (
            Unify0 = construct(Var, ConsId, Args, ArgModes, How, Unique, _)
        ->
            process_construct(LHS, RHS, UniMode, UnifyContext, Var, ConsId,
                Args, ArgModes, How, Unique, GoalInfo0, GoalExpr, !Info)
        ;
            Unify0 = deconstruct(Var, ConsId, Args, ArgModes,
                _CanFail, _CanCGC),
            % The following test is an optimization. If
            % BindingArgModes = [], which is almost 100% likely,
            % then process_deconstruct would return GoalExpr0 as
            % GoalExpr anyway, but would take longer.
            list__filter(binds_arg_in_cell(!.Info), ArgModes, BindingArgModes),
            BindingArgModes \= []
        ->
            process_deconstruct(Var, ConsId, Args, ArgModes,
                Goal0, GoalExpr, !Info)
        ;
            GoalExpr = GoalExpr0
        )
    ;
        GoalExpr0 = call(_, _, _, _, _, _),
        % We don't want to save type_ctor_info variables across calls,
        % because saving/restoring them is more expensive than defining
        % them again.
        !:Info = !.Info ^ type_ctor_map := map__init,
        !:Info = !.Info ^ rev_type_ctor_map := map__init,
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(_, _, _, _),
        % We don't want to save type_ctor_info variables across calls,
        % because saving/restoring them is more expensive than defining
        % them again.
        !:Info = !.Info ^ type_ctor_map := map__init,
        !:Info = !.Info ^ rev_type_ctor_map := map__init,
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = foreign_proc(_, _, _, _, _, _),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(Goals0),
        process_conj(Goals0, Goals, !Info),
        GoalExpr = conj(Goals)
    ;
        GoalExpr0 = par_conj(Goals0),
        % This transformation produces code that is much less than
        % optimal. However, it ought to be more robust than any better
        % transformation, and there is no point in spending time on a
        % better transformation while parallel conjunctions are rare.
        TargetTypeInfoMap0 = !.Info ^ target_type_info_map,
        TypeInfoMap0 = !.Info ^ type_info_map,
        RevTypeInfoMap0 = !.Info ^ rev_type_info_map,
        TypeCtorMap0 = !.Info ^ type_ctor_map,
        KnownSizeMap0 = !.Info ^ known_size_map,
        process_par_conj(Goals0, Goals, !Info, TargetTypeInfoMap0,
            TypeInfoMap0, TypeCtorMap0, KnownSizeMap0),
        !:Info = !.Info ^ target_type_info_map := TargetTypeInfoMap0,
        !:Info = !.Info ^ type_info_map := TypeInfoMap0,
        !:Info = !.Info ^ rev_type_info_map := RevTypeInfoMap0,
        !:Info = !.Info ^ type_ctor_map := map__init,
        !:Info = !.Info ^ rev_type_ctor_map := map__init,
        !:Info = !.Info ^ known_size_map := KnownSizeMap0,
        GoalExpr = par_conj(Goals)
    ;
        GoalExpr0 = switch(SwitchVar, CanFail, Cases0),
        (
            Cases0 = [First0 | Later0],
            TargetTypeInfoMap0 = !.Info ^ target_type_info_map,
            TypeInfoMap0 = !.Info ^ type_info_map,
            RevTypeInfoMap0 = !.Info ^ rev_type_info_map,
            TypeCtorMap0 = !.Info ^ type_ctor_map,
            RevTypeCtorMap0 = !.Info ^ rev_type_ctor_map,
            KnownSizeMap0 = !.Info ^ known_size_map,
            process_switch(First0, First, Later0, Later, !Info,
                TargetTypeInfoMap0,
                TypeInfoMap0, RevTypeInfoMap0,
                TypeCtorMap0, RevTypeCtorMap0,
                TypeInfoMap, KnownSizeMap0, KnownSizeMap),
            !:Info = !.Info ^ type_info_map := TypeInfoMap,
            % The rev_type_info_map field is updated by
            % the call to update_rev_maps below.
            !:Info = !.Info ^ type_ctor_map := map__init,
            !:Info = !.Info ^ rev_type_ctor_map := map__init,
            !:Info = !.Info ^ known_size_map := KnownSizeMap,
            Cases = [First | Later]
        ;
            Cases0 = [],
            unexpected(this_file, "size_prof__process_goal: empty switch")
        ),
        update_rev_maps(!Info),
        update_target_map(!Info),
        GoalExpr = switch(SwitchVar, CanFail, Cases)
    ;
        GoalExpr0 = disj(Disjuncts0),
        (
            Disjuncts0 = [First0 | Later0],
            TargetTypeInfoMap0 = !.Info ^ target_type_info_map,
            TypeInfoMap0 = !.Info ^ type_info_map,
            RevTypeInfoMap0 = !.Info ^ rev_type_info_map,
            TypeCtorMap0 = !.Info ^ type_ctor_map,
            RevTypeCtorMap0 = !.Info ^ rev_type_ctor_map,
            KnownSizeMap0 = !.Info ^ known_size_map,
            process_disj(First0, First, Later0, Later, !Info,
                TargetTypeInfoMap0,
                TypeInfoMap0, RevTypeInfoMap0,
                TypeCtorMap0, RevTypeCtorMap0,
                TypeInfoMap, KnownSizeMap0, KnownSizeMap),
            !:Info = !.Info ^ type_info_map := TypeInfoMap,
            % The rev_type_info_map field is updated by
            % the call to update_rev_maps below.
            !:Info = !.Info ^ type_ctor_map := map__init,
            !:Info = !.Info ^ rev_type_ctor_map := map__init,
            !:Info = !.Info ^ known_size_map := KnownSizeMap,
            Disjuncts = [First | Later]
        ;
            Disjuncts0 = [],
            % An empty disj represents `fail'.
            !:Info = !.Info ^ type_info_map := map__init,
            !:Info = !.Info ^ rev_type_ctor_map := map__init,
            !:Info = !.Info ^ type_info_map := map__init,
            !:Info = !.Info ^ rev_type_ctor_map := map__init,
            Disjuncts = []
        ),
        update_rev_maps(!Info),
        update_target_map(!Info),
        GoalExpr = disj(Disjuncts)
    ;
        GoalExpr0 = if_then_else(Quant, Cond0, Then0, Else0),
        TargetTypeInfoMap0 = !.Info ^ target_type_info_map,
        TypeInfoMap0 = !.Info ^ type_info_map,
        RevTypeInfoMap0 = !.Info ^ rev_type_info_map,
        TypeCtorMap0 = !.Info ^ type_ctor_map,
        RevTypeCtorMap0 = !.Info ^ rev_type_ctor_map,
        KnownSizeMap0 = !.Info ^ known_size_map,

        !:Info = !.Info ^ target_type_info_map := map__init,
        process_goal(Cond0, Cond, !Info),
        !:Info = !.Info ^ target_type_info_map := TargetTypeInfoMap0,
        process_goal(Then0, Then, !Info),
        TargetTypeInfoMapThen = !.Info ^ target_type_info_map,
        TypeInfoMapThen = !.Info ^ type_info_map,
        KnownSizeMapThen = !.Info ^ known_size_map,

        map__union(select_first, TargetTypeInfoMapThen,
            TargetTypeInfoMap0, ElseTargetTypeInfoMap),
        !:Info = !.Info ^ target_type_info_map :=
            ElseTargetTypeInfoMap,
        !:Info = !.Info ^ type_info_map := TypeInfoMap0,
        !:Info = !.Info ^ rev_type_info_map := RevTypeInfoMap0,
        !:Info = !.Info ^ type_ctor_map := TypeCtorMap0,
        !:Info = !.Info ^ rev_type_ctor_map := RevTypeCtorMap0,
        !:Info = !.Info ^ known_size_map := KnownSizeMap0,
        process_goal(Else0, Else, !Info),
        TypeInfoMapElse = !.Info ^ type_info_map,
        KnownSizeMapElse = !.Info ^ known_size_map,

        TypeInfoMap = map__common_subset(TypeInfoMapThen, TypeInfoMapElse),
        KnownSizeMap = map__common_subset(KnownSizeMapThen, KnownSizeMapElse),
        !:Info = !.Info ^ type_info_map := TypeInfoMap,
        !:Info = !.Info ^ type_ctor_map := map__init,
        !:Info = !.Info ^ known_size_map := KnownSizeMap,
        update_rev_maps(!Info),
        update_target_map(!Info),
        GoalExpr = if_then_else(Quant, Cond, Then, Else)
    ;
        GoalExpr0 = not(NegGoal0),
        TargetTypeInfoMap0 = !.Info ^ target_type_info_map,
        TypeInfoMap0 = !.Info ^ type_info_map,
        RevTypeInfoMap0 = !.Info ^ rev_type_info_map,
        TypeCtorMap0 = !.Info ^ type_ctor_map,
        RevTypeCtorMap0 = !.Info ^ rev_type_ctor_map,
        KnownSizeMap0 = !.Info ^ known_size_map,
        process_goal(NegGoal0, NegGoal, !Info),
        % Variables constructed in negated goals are not available after the
        % negated goal fails and the negation succeeds. The sizes we learn
        % in NegGoal0 don't apply after NegGoal0 fails.
        !:Info = !.Info ^ target_type_info_map := TargetTypeInfoMap0,
        !:Info = !.Info ^ type_info_map := TypeInfoMap0,
        !:Info = !.Info ^ rev_type_info_map := RevTypeInfoMap0,
        !:Info = !.Info ^ type_ctor_map := TypeCtorMap0,
        !:Info = !.Info ^ rev_type_ctor_map := RevTypeCtorMap0,
        !:Info = !.Info ^ known_size_map := KnownSizeMap0,
        GoalExpr = not(NegGoal)
    ;
        GoalExpr0 = scope(Reason, SomeGoal0),
        process_goal(SomeGoal0, SomeGoal, !Info),
        GoalExpr = scope(Reason, SomeGoal)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "size_prof__process_goal: shorthand")
    ),
    Goal = GoalExpr - GoalInfo0.

%---------------------------------------------------------------------------%

:- pred process_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    info::in, info::out) is det.

process_conj([], [], !Info).
process_conj([Goal0 | Goals0], Conj, !Info) :-
    process_goal(Goal0, Goal, !Info),
    process_conj(Goals0, Goals, !Info),
    ( Goal = conj(SubConj) - _ ->
        % Flatten out any conjunction introduced by process_goal.
        % We never create conjunctions more than one level deep,
        % so this single test is sufficient to ensure that we never
        % leave conjunctions nested more deeply than the input goal.
        Conj = list__append(SubConj, Goals)
    ;
        Conj = [Goal | Goals]
    ).

%---------------------------------------------------------------------------%

:- pred process_par_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    info::in, info::out, type_info_map::in, type_info_map::in,
    type_ctor_map::in, known_size_map::in) is det.

process_par_conj([], [], !Info, _, _, _, _).
process_par_conj([Goal0 | Goals0], [Goal | Goals], !Info, TargetTypeInfoMap0,
        TypeInfoMap0, TypeCtorMap0, KnownSizeMap0) :-
    !:Info = !.Info ^ target_type_info_map := TargetTypeInfoMap0,
    !:Info = !.Info ^ type_info_map := TypeInfoMap0,
    !:Info = !.Info ^ type_ctor_map := TypeCtorMap0,
    !:Info = !.Info ^ known_size_map := KnownSizeMap0,
    process_goal(Goal0, Goal, !Info),
    process_par_conj(Goals0, Goals, !Info, TargetTypeInfoMap0,
        TypeInfoMap0, TypeCtorMap0, KnownSizeMap0).

%---------------------------------------------------------------------------%

:- pred process_disj(hlds_goal::in, hlds_goal::out,
    list(hlds_goal)::in, list(hlds_goal)::out, info::in, info::out,
    type_info_map::in, type_info_map::in, rev_type_info_map::in,
    type_ctor_map::in, rev_type_ctor_map::in,
    type_info_map::out, known_size_map::in, known_size_map::out) is det.

process_disj(First0, First, Later0, Later, !Info, TargetTypeInfoMap,
        TypeInfoMap0, RevTypeInfoMap0, TypeCtorMap0, RevTypeCtorMap0,
        TypeInfoMap, KnownSizeMap0, KnownSizeMap) :-
    !:Info = !.Info ^ type_info_map := TypeInfoMap0,
    !:Info = !.Info ^ rev_type_info_map := RevTypeInfoMap0,
    !:Info = !.Info ^ type_ctor_map := TypeCtorMap0,
    !:Info = !.Info ^ rev_type_ctor_map := RevTypeCtorMap0,
    !:Info = !.Info ^ known_size_map := KnownSizeMap0,
    process_goal(First0, First, !Info),
    TypeInfoMapFirst = !.Info ^ type_info_map,
    KnownSizeMapFirst = !.Info ^ known_size_map,
    (
        Later0 = [Head0 | Tail0],
        map__union(select_first, TypeInfoMapFirst,
            TargetTypeInfoMap, LaterTargetTypeInfoMap),
        !:Info = !.Info ^ target_type_info_map := LaterTargetTypeInfoMap,
        process_disj(Head0, Head, Tail0, Tail, !Info, TargetTypeInfoMap,
            TypeInfoMap0, RevTypeInfoMap0, TypeCtorMap0, RevTypeCtorMap0,
            TypeInfoMapLater, KnownSizeMap0, KnownSizeMapLater),
        TypeInfoMap = map__common_subset(TypeInfoMapFirst, TypeInfoMapLater),
        KnownSizeMap = map__common_subset(KnownSizeMapFirst,
            KnownSizeMapLater),
        Later = [Head | Tail]
    ;
        Later0 = [],
        Later = [],
        TypeInfoMap = TypeInfoMapFirst,
        KnownSizeMap = KnownSizeMapFirst
    ).

%---------------------------------------------------------------------------%

:- pred process_switch(case::in, case::out,
    list(case)::in, list(case)::out, info::in, info::out,
    type_info_map::in, type_info_map::in, rev_type_info_map::in,
    type_ctor_map::in, rev_type_ctor_map::in,
    type_info_map::out, known_size_map::in, known_size_map::out) is det.

process_switch(First0, First, Later0, Later, !Info, TargetTypeInfoMap,
        TypeInfoMap0, RevTypeInfoMap0, TypeCtorMap0, RevTypeCtorMap0,
        TypeInfoMap, KnownSizeMap0, KnownSizeMap) :-
    !:Info = !.Info ^ type_info_map := TypeInfoMap0,
    !:Info = !.Info ^ rev_type_info_map := RevTypeInfoMap0,
    !:Info = !.Info ^ type_ctor_map := TypeCtorMap0,
    !:Info = !.Info ^ rev_type_ctor_map := RevTypeCtorMap0,
    !:Info = !.Info ^ known_size_map := KnownSizeMap0,
    First0 = case(FirstConsId, FirstGoal0),
    process_goal(FirstGoal0, FirstGoal, !Info),
    TypeInfoMapFirst = !.Info ^ type_info_map,
    KnownSizeMapFirst = !.Info ^ known_size_map,
    First = case(FirstConsId, FirstGoal),
    (
        Later0 = [Head0 | Tail0],
        map__union(select_first, TargetTypeInfoMap,
            TypeInfoMapFirst, LaterTargetTypeInfoMap),
        !:Info = !.Info ^ target_type_info_map := LaterTargetTypeInfoMap,
        process_switch(Head0, Head, Tail0, Tail, !Info, TargetTypeInfoMap,
            TypeInfoMap0, RevTypeInfoMap0, TypeCtorMap0, RevTypeCtorMap0,
            TypeInfoMapLater, KnownSizeMap0, KnownSizeMapLater),
        TypeInfoMap = map__common_subset(TypeInfoMapFirst, TypeInfoMapLater),
        KnownSizeMap = map__common_subset(KnownSizeMapFirst,
            KnownSizeMapLater),
        Later = [Head | Tail]
    ;
        Later0 = [],
        Later = [],
        TypeInfoMap = TypeInfoMapFirst,
        KnownSizeMap = KnownSizeMapFirst
    ).

%---------------------------------------------------------------------------%

:- pred process_construct(prog_var::in, unify_rhs::in, unify_mode::in,
    unify_context::in, prog_var::in, cons_id::in, list(prog_var)::in,
    list(uni_mode)::in, how_to_construct::in, cell_is_unique::in,
    hlds_goal_info::in, hlds_goal_expr::out, info::in, info::out) is det.

process_construct(LHS, RHS, UniMode, UnifyContext, Var, ConsId, Args, ArgModes,
        How, Unique, GoalInfo, GoalExpr, !Info) :-
    map__lookup(!.Info ^ vartypes, Var, VarType),
    ( type_to_ctor_and_args(VarType, VarTypeCtorPrime, _VarTypeArgs) ->
        VarTypeCtor = VarTypeCtorPrime
    ;
        unexpected(this_file,
            "size_prof__process_construct: constructing term of variable type")
    ),
    type_ctor_module(!.Info ^ module_info, VarTypeCtor, VarTypeCtorModule),
    type_ctor_name(!.Info ^ module_info, VarTypeCtor, VarTypeCtorName),
    (
        ctor_is_type_info_related(VarTypeCtorModule, VarTypeCtorName)
    ->
        ( VarTypeCtorName = "type_info" ->
            (
                ConsId = type_info_cell_constructor(_),
                Args = [TypeCtorInfoVar | ArgTypeInfoVars]
            ->
                record_known_type_info(Var, TypeCtorInfoVar, ArgTypeInfoVars,
                    !Info)
            ;
                ConsId = type_ctor_info_const(M, N, A)
            ->
                % When type specialization creates a procedure with a type
                % substitution such as K=int, it leaves the type of
                % TypeInfo_for_K as type_info, not type_ctor_info.
                record_known_type_ctor_info(Var, M, N, A, !Info)
            ;
                unexpected(this_file, "process_construct: bad type_info")
            )
        ; VarTypeCtorName = "type_ctor_info" ->
            ( ConsId = type_ctor_info_const(M, N, A) ->
                record_known_type_ctor_info(Var, M, N, A, !Info)
            ;
                unexpected(this_file, "process_construct: bad type_ctor_info")
            )
        ;
            !:Info = !.Info
        ),
        Unification = construct(Var, ConsId, Args, ArgModes, How, Unique,
            no_construct_sub_info),
        GoalExpr = unify(LHS, RHS, UniMode, Unification, UnifyContext)
    ;
        ConsId = cons(_Name, _Arity),
        Args = [_ | _]
    ->
        process_cons_construct(LHS, RHS, UniMode, UnifyContext, Var, VarType,
            ConsId, Args, ArgModes, How, Unique, GoalInfo, GoalExpr, !Info)
    ;
        % All ConsIds other than cons/2 with at least one argument
        % construct terms that we consider zero-sized.
        record_known_size(Var, 0, !Info),
        Unification = construct(Var, ConsId, Args, ArgModes, How, Unique,
            no_construct_sub_info),
        GoalExpr = unify(LHS, RHS, UniMode, Unification, UnifyContext)
    ).

%-----------------------------------------------------------------------------%

:- pred process_deconstruct(prog_var::in, cons_id::in, list(prog_var)::in,
    list(uni_mode)::in, hlds_goal::in, hlds_goal_expr::out,
    info::in, info::out) is det.

process_deconstruct(Var, ConsId, Args, ArgModes, Goal0, GoalExpr, !Info) :-
    map__lookup(!.Info ^ vartypes, Var, VarType),
    ( type_to_ctor_and_args(VarType, VarTypeCtorPrime, _VarTypeArgs) ->
        VarTypeCtor = VarTypeCtorPrime
    ;
        unexpected(this_file,
            "process_deconstruct: deconstructing term of variable type")
    ),
    type_ctor_module(!.Info ^ module_info, VarTypeCtor, VarTypeCtorModule),
    type_ctor_name(!.Info ^ module_info, VarTypeCtor, VarTypeCtorName),
    (
        ctor_is_type_info_related(VarTypeCtorModule, VarTypeCtorName)
    ->
        Goal0 = GoalExpr - _
    ;
        ConsId = cons(_Name, _Arity),
        Args = [_ | _]
    ->
        process_cons_deconstruct(Var, Args, ArgModes, Goal0, GoalExpr, !Info)
    ;
        % All ConsIds other than cons/2 deconstruct terms that we
        % consider zero-sized.
        record_known_size(Var, 0, !Info),
        Goal0 = GoalExpr - _
    ).

%-----------------------------------------------------------------------------%

:- pred process_cons_construct(prog_var::in, unify_rhs::in, unify_mode::in,
    unify_context::in, prog_var::in, mer_type::in, cons_id::in,
    list(prog_var)::in, list(uni_mode)::in, how_to_construct::in,
    cell_is_unique::in, hlds_goal_info::in, hlds_goal_expr::out,
    info::in, info::out) is det.

process_cons_construct(LHS, RHS, UniMode, UnifyContext, Var, _Type, ConsId,
        Args, ArgModes, How, Unique, GoalInfo0, GoalExpr, !Info) :-
    FunctorSize = compute_functor_size(Args, !.Info),
    find_defined_args(Args, ArgModes, DefinedArgs, NonDefinedArgs, !.Info),
    goal_info_get_context(GoalInfo0, Context),
    process_args(DefinedArgs, FunctorSize, KnownSize,
        no, MaybeDynamicSizeVar, Context, ArgGoals, !Info),
    (
        MaybeDynamicSizeVar = no,
        expect(unify(ArgGoals, []), this_file,
            "process_cons_construct: nonempty ArgGoals"),
        (
            NonDefinedArgs = [],
            record_known_size(Var, KnownSize, !Info)
        ;
            NonDefinedArgs = [_ | _],
            % The size of the term may change as some of its
            % currently free arguments become bound.
            true
        ),
        Unification = construct(Var, ConsId, Args, ArgModes, How, Unique,
            construct_sub_info(no, yes(known_size(KnownSize)))),
        GoalExpr = unify(LHS, RHS, UniMode, Unification, UnifyContext)
    ;
        MaybeDynamicSizeVar = yes(SizeVar0),
        generate_size_var(SizeVar0, KnownSize, Context, SizeVar, SizeGoals,
            !Info),
        Unification = construct(Var, ConsId, Args, ArgModes, How, Unique,
            construct_sub_info(no, yes(dynamic_size(SizeVar)))),
        UnifyExpr = unify(LHS, RHS, UniMode, Unification, UnifyContext),
        goal_info_get_nonlocals(GoalInfo0, NonLocals0),
        set__insert(NonLocals0, SizeVar, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        UnifyGoal = UnifyExpr - GoalInfo,
        Goals = list__condense([ArgGoals, SizeGoals, [UnifyGoal]]),
        GoalExpr = conj(Goals)
    ).

%-----------------------------------------------------------------------------%

:- pred process_cons_deconstruct(prog_var::in, list(prog_var)::in,
    list(uni_mode)::in, hlds_goal::in, hlds_goal_expr::out,
    info::in, info::out) is det.

process_cons_deconstruct(Var, Args, ArgModes, UnifyGoal, GoalExpr, !Info) :-
    find_defined_args(Args, ArgModes, DefinedArgs, _NonDefArgs, !.Info),
    UnifyGoal = GoalExpr0 - GoalInfo0,
    goal_info_get_context(GoalInfo0, Context),
    process_args(DefinedArgs, 0, KnownSize,
        no, MaybeDynamicSizeVar, Context, ArgGoals, !Info),
    (
        MaybeDynamicSizeVar = no,
        expect(unify(ArgGoals, []), this_file,
            "process_cons_deconstruct: nonempty ArgGoals"),
        GoalExpr = GoalExpr0
    ;
        MaybeDynamicSizeVar = yes(SizeVar0),
        generate_size_var(SizeVar0, KnownSize, Context, SizeVar, SizeGoals,
            !Info),
        % The increment_size primitive doesn't need Var's type_info,
        % so we make it a no_type_info_builtin.
        TermSizeProfBuiltin = mercury_term_size_prof_builtin_module,
        goal_util__generate_simple_call(TermSizeProfBuiltin,
            "increment_size", predicate, only_mode, det,
            [Var, SizeVar], [impure_goal], [], !.Info ^ module_info,
            Context, UpdateGoal),
        % Put UnifyGoal first in case it fails.
        Goals = [UnifyGoal] ++ ArgGoals ++ SizeGoals ++ [UpdateGoal],
        GoalExpr = conj(Goals)
    ).

%-----------------------------------------------------------------------------%

    % Process the variables representing the fields being bound in a memory
    % cell, computing the contribution they make to the increase in size of
    % that cell. The increase is in two parts: the statically known part,
    % and the part that can be known only at runtime. We record the former
    % in the KnownSize accumulator and the latter in the MaybeSizeVar
    % accumulator. We allocate a variable to hold the dynamically-computed part
    % of the size only if the sum of the arguments' sizes is not static.
    % In that case, the Goals we return will be nonempty.
    %
:- pred process_args(list(prog_var)::in, int::in, int::out,
    maybe(prog_var)::in, maybe(prog_var)::out, prog_context::in,
    list(hlds_goal)::out, info::in, info::out) is det.

process_args([], !KnownSize, !MaybeSizeVar, _, [], !Info).
process_args([Arg | Args], !KnownSize, !MaybeSizeVar, Context, Goals, !Info) :-
    map__lookup(!.Info ^ vartypes, Arg, Type),
    ( map__search(!.Info ^ known_size_map, Arg, ArgSize) ->
        !:KnownSize = !.KnownSize + ArgSize,
        ArgGoals = []
    ; zero_size_type(Type, !.Info ^ module_info) ->
        ArgGoals = []
    ;
        make_type_info(Context, Type, TypeInfoVar, TypeInfoGoals, !Info),
        make_size_goal(TypeInfoVar, Arg, Context, SizeGoal, !MaybeSizeVar,
            !Info),
        list__append(TypeInfoGoals, [SizeGoal], ArgGoals)
    ),
    process_args(Args, !KnownSize, !MaybeSizeVar, Context, LaterGoals, !Info),
    list__append(ArgGoals, LaterGoals, Goals).

%-----------------------------------------------------------------------------%

    % Given that KnownSize is the static part of the sum of sizes of the fields
    % being defined and SizeVar0 is a variable containing the dynamic part,
    % return a variable SizeVar that contains their sum and the Goals needed to
    % compute it.
    %
:- pred generate_size_var(prog_var::in, int::in, prog_context::in,
    prog_var::out, list(hlds_goal)::out, info::in, info::out) is det.

generate_size_var(SizeVar0, KnownSize, Context, SizeVar, Goals, !Info) :-
    ( KnownSize = 0 ->
        SizeVar = SizeVar0,
        Goals = []
    ;
        VarSet0 = !.Info ^ varset,
        VarTypes0 = !.Info ^ vartypes,
        make_int_const_construction_alloc(KnownSize,
            yes("KnownSize"), KnownSizeGoal, KnownSizeVar,
            VarTypes0, VarTypes1, VarSet0, VarSet1),
        !:Info = !.Info ^ varset := VarSet1,
        !:Info = !.Info ^ vartypes := VarTypes1,
        get_new_var(int_type, "FinalSizeVar", SizeVar, !Info),
        TermSizeProfModule = mercury_term_size_prof_builtin_module,
        goal_util__generate_simple_call(TermSizeProfModule,
            "term_size_plus", function, mode_no(0), det,
            [SizeVar0, KnownSizeVar, SizeVar], [],
            [SizeVar - ground(shared, none)],
            !.Info ^ module_info, Context, AddGoal),
        Goals = [KnownSizeGoal, AddGoal]
    ).

%-----------------------------------------------------------------------------%

    % Create a type_info for a given type as cheaply as possible, with the
    % cheapest methods involving the reuse of existing type_infos and/or
    % type_ctor_infos. Return the variable holding the type_info in
    % TypeInfoVar, and the goals needed to create it in TypeInfoGoals.

:- pred make_type_info(prog_context::in, mer_type::in, prog_var::out,
    list(hlds_goal)::out, info::in, info::out) is det.

make_type_info(Context, Type, TypeInfoVar, TypeInfoGoals, !Info) :-
    ( map__search(!.Info ^ type_info_map, Type, TypeInfoVarPrime) ->
        TypeInfoVar = TypeInfoVarPrime,
        TypeInfoGoals = []
    ; type_has_variable_arity_ctor(Type, TypeCtor, ArgTypes) ->
        construct_type_info(Context, Type, TypeCtor, ArgTypes, yes,
            TypeInfoVar, TypeInfoGoals, !Info)
    ; type_to_ctor_and_args(Type, TypeCtor, ArgTypes) ->
        (
            ArgTypes = [],
            make_type_ctor_info(TypeCtor, [], TypeCtorVar, TypeCtorGoals,
                !Info),
            TypeInfoVar = TypeCtorVar,
            TypeInfoGoals = TypeCtorGoals
        ;
            ArgTypes = [_ | _],
            construct_type_info(Context, Type, TypeCtor, ArgTypes,
                no, TypeInfoVar, TypeInfoGoals, !Info)
        )
    ; Type = variable(TVar, _) ->
        rtti_lookup_type_info_locn(!.Info ^ rtti_varmaps, TVar, TVarLocn),
        (
            TVarLocn = type_info(TypeInfoVar),
            TypeInfoGoals = []
        ;
            TVarLocn = typeclass_info(TypeClassInfoVar, Slot),
            TargetTypeInfoMap = !.Info ^ target_type_info_map,
            VarSet0 = !.Info ^ varset,
            VarTypes0 = !.Info ^ vartypes,
            ( map__search(TargetTypeInfoMap, Type, TargetVar) ->
                TypeInfoVar = TargetVar,
                VarSet1 = VarSet0,
                VarTypes1 = VarTypes0
            ;
                RttiVarMaps0 = !.Info ^ rtti_varmaps,
                polymorphism__new_type_info_var_raw(Type, type_info,
                    TypeInfoVar, VarSet0, VarSet1, VarTypes0, VarTypes1,
                    RttiVarMaps0, RttiVarMaps),
                !:Info = !.Info ^ rtti_varmaps := RttiVarMaps
            ),
            make_int_const_construction_alloc(Slot, yes("TypeClassInfoSlot"),
                SlotGoal, SlotVar, VarTypes1, VarTypes, VarSet1, VarSet),
            !:Info = !.Info ^ varset := VarSet,
            !:Info = !.Info ^ vartypes := VarTypes,
            PrivateBuiltin = mercury_private_builtin_module,
            goal_util__generate_simple_call(PrivateBuiltin,
                "type_info_from_typeclass_info", predicate, only_mode, det,
                [TypeClassInfoVar, SlotVar, TypeInfoVar], [],
                [TypeInfoVar - ground(shared, none)],
                !.Info ^ module_info, Context, ExtractGoal),
            record_type_info_var(Type, TypeInfoVar, !Info),
            TypeInfoGoals = [SlotGoal, ExtractGoal]
        )
    ;
        % Type_to_ctor_and_args can fail only if Type is a type variable,
        % or acts like one. The tests in our callers should have filtered
        % out both cases.
        unexpected(this_file, "size_prof__make_type_info: cannot happen")
    ).

    % Construct a type_info for Type = TypeCtor(ArgTypes), given that we know
    % there is no variable that currently holds a type_info for Type. Return
    % the variable holding the type_info in TypeInfoVar, and the goals needed
    % to create it in TypeInfoGoals.
    %
:- pred construct_type_info(prog_context::in, mer_type::in, type_ctor::in,
    list(mer_type)::in, bool::in, prog_var::out, list(hlds_goal)::out,
    info::in, info::out) is det.

construct_type_info(Context, Type, TypeCtor, ArgTypes, CtorIsVarArity,
        TypeInfoVar, TypeInfoGoals, !Info) :-
    list__map2_foldl(make_type_info(Context), ArgTypes,
        ArgTypeInfoVars, ArgTypeInfoGoalLists, !Info),
    ArgTypeInfoGoals = list__condense(ArgTypeInfoGoalLists),
    make_type_ctor_info(TypeCtor, ArgTypes, TypeCtorVar, TypeCtorGoals, !Info),
    (
        CtorIsVarArity = yes,
        list__length(ArgTypes, Arity),
        VarSet0 = !.Info ^ varset,
        VarTypes0 = !.Info ^ vartypes,
        make_int_const_construction_alloc(Arity, yes("TupleArity"), ArityGoal,
            ArityVar, VarTypes0, VarTypes1, VarSet0, VarSet1),
        !:Info = !.Info ^ varset := VarSet1,
        !:Info = !.Info ^ vartypes := VarTypes1,
        FrontGoals = list__append(TypeCtorGoals, [ArityGoal]),
        ArgVars = [TypeCtorVar, ArityVar | ArgTypeInfoVars]
    ;
        CtorIsVarArity = no,
        FrontGoals = TypeCtorGoals,
        ArgVars = [TypeCtorVar | ArgTypeInfoVars]
    ),
    VarSet2 = !.Info ^ varset,
    VarTypes2 = !.Info ^ vartypes,
    RttiVarMaps0 = !.Info ^ rtti_varmaps,
    TargetTypeInfoMap = !.Info ^ target_type_info_map,
    ( map__search(TargetTypeInfoMap, Type, PrefTIVar) ->
        MaybePreferredVar = yes(PrefTIVar)
    ;
        MaybePreferredVar = no
    ),
    polymorphism__init_type_info_var(Type, ArgVars, MaybePreferredVar,
        TypeInfoVar, TypeInfoGoal, VarSet2, VarSet, VarTypes2, VarTypes,
        RttiVarMaps0, RttiVarMaps),
    !:Info = !.Info ^ varset := VarSet,
    !:Info = !.Info ^ vartypes := VarTypes,
    !:Info = !.Info ^ rtti_varmaps := RttiVarMaps,
    TypeInfoGoals = list__condense([ArgTypeInfoGoals, FrontGoals,
        [TypeInfoGoal]]).

    % Create a type_ctor_info for a given type constructor as cheaply as
    % possible, with the cheapest method being the reuse of an existing
    % type_ctor_info. Return the variable holding the type_ctor_info in
    % TypeCtorVar, and the goals needed to create it in TypeCtorGoals.
    %
:- pred make_type_ctor_info(type_ctor::in, list(mer_type)::in, prog_var::out,
    list(hlds_goal)::out, info::in, info::out) is det.

make_type_ctor_info(TypeCtor, TypeArgs, TypeCtorVar, TypeCtorGoals, !Info) :-
    ( map__search(!.Info ^ type_ctor_map, TypeCtor, TypeCtorVarPrime) ->
        TypeCtorVar = TypeCtorVarPrime,
        TypeCtorGoals = []
    ;
        ( 
            type_ctor_is_higher_order(TypeCtor, Purity, PredOrFunc, EvalMethod)
        ->
            construct_higher_order_type(Purity, PredOrFunc, EvalMethod,
                TypeArgs, Type)
        ;
            construct_type(TypeCtor, [], Type)
        ),
        VarSet0 = !.Info ^ varset,
        VarTypes0 = !.Info ^ vartypes,
        RttiVarMaps0 = !.Info ^ rtti_varmaps,
        polymorphism__init_const_type_ctor_info_var(Type, TypeCtor,
            TypeCtorVar, TypeCtorGoal, !.Info ^ module_info,
            VarSet0, VarSet, VarTypes0, VarTypes,
            RttiVarMaps0, RttiVarMaps),
        TypeCtorGoals = [TypeCtorGoal],
        !:Info = !.Info ^ varset := VarSet,
        !:Info = !.Info ^ vartypes := VarTypes,
        !:Info = !.Info ^ rtti_varmaps := RttiVarMaps
    ).

%-----------------------------------------------------------------------------%

    % Generate a goal that looks up the size of Arg at runtime, given that the
    % type_info of Arg's type is in TypeInfoVar.
    %
    % We ultimately always want to compute the sum of the sizes of the fields
    % being defined, so if we have previously looked up the sizes of other
    % fields, then combine the operation of looking up Arg's size with adding
    % that size to the sum of the (dynamic) sizes so far: measure_size_acc
    % does both these operations.
    %
:- pred make_size_goal(prog_var::in, prog_var::in, prog_context::in,
    hlds_goal::out, maybe(prog_var)::in, maybe(prog_var)::out,
    info::in, info::out) is det.

make_size_goal(TypeInfoVar, Arg, Context, SizeGoal,
        MaybeSizeVar0, MaybeSizeVar, !Info) :-
    get_new_var(int_type, "SizeVar", SizeVar, !Info),
    (
        MaybeSizeVar0 = yes(SizeVar0),
        Pred = "measure_size_acc",
        Args = [TypeInfoVar, Arg, SizeVar0, SizeVar]
    ;
        MaybeSizeVar0 = no,
        Pred = "measure_size",
        Args = [TypeInfoVar, Arg, SizeVar]
    ),
    TermSizeProfBuiltin = mercury_term_size_prof_builtin_module,
    goal_util__generate_simple_call(TermSizeProfBuiltin, Pred, predicate,
        only_mode, det, Args, [], [SizeVar - ground(shared, none)],
        !.Info ^ module_info, Context, SizeGoal),
    MaybeSizeVar = yes(SizeVar).

%---------------------------------------------------------------------------%

    % Create a new variable with a name constructed from Prefix and the
    % variable number.
    %
:- pred get_new_var(mer_type::in, string::in, prog_var::out,
    info::in, info::out) is det.

get_new_var(Type, Prefix, Var, !Info) :-
    VarSet0 = !.Info ^ varset,
    VarTypes0 = !.Info ^ vartypes,
    varset__new_var(VarSet0, Var, VarSet1),
    term__var_to_int(Var, VarNum),
    string__int_to_string(VarNum, VarNumStr),
    string__append(Prefix, VarNumStr, Name),
    varset__name_var(VarSet1, Var, Name, VarSet),
    map__set(VarTypes0, Var, Type, VarTypes),
    !:Info = !.Info ^ varset := VarSet,
    !:Info = !.Info ^ vartypes := VarTypes.

%---------------------------------------------------------------------------%

    % These predicates record information about the procedure body (that was
    % either there originally or was made true by our transformation) for later
    % use in optimizating the transformation of the rest of the procedure body.
    %
    % The reason why the implementation uses map__set instead of
    % map__det_insert is that it is possible for Var to already exist in the
    % maps. This can happen e.g. when each branch of a branched structure
    % generates a value of an existential type, and thus also generates
    % the type_info describing that type. The first branch will insert the
    % variable holding that type_info into the maps, and the later branches
    % will be given the resulting map as guidance.
    %
    % We override any old settings here, for use in the rest of the current
    % branch. Other branches will do likewise. The correct handling of the code
    % after the branched structure is ensured by process_goal returning only
    % the common subsets of the maps constructed by the various branches to
    % be used when processing the following code.
    %
:- pred record_known_type_ctor_info(prog_var::in, module_name::in, string::in,
    int::in, info::in, info::out) is det.

record_known_type_ctor_info(Var, TypeCtorModule, TypeCtorName, TypeCtorArity,
        !Info) :-
    TypeCtor = qualified(TypeCtorModule, TypeCtorName) - TypeCtorArity,
    TypeCtorMap0 = !.Info ^ type_ctor_map,
    RevTypeCtorMap0 = !.Info ^ rev_type_ctor_map,
    map__set(TypeCtorMap0, TypeCtor, Var, TypeCtorMap),
    map__set(RevTypeCtorMap0, Var, TypeCtor, RevTypeCtorMap),
    !:Info = !.Info ^ type_ctor_map := TypeCtorMap,
    !:Info = !.Info ^ rev_type_ctor_map := RevTypeCtorMap.

:- pred record_known_type_info(prog_var::in, prog_var::in, list(prog_var)::in,
    info::in, info::out) is det.

record_known_type_info(Var, TypeCtorInfoVar, ArgTypeInfoVars, !Info) :-
    RevTypeCtorMap0 = !.Info ^ rev_type_ctor_map,
    ( map__search(RevTypeCtorMap0, TypeCtorInfoVar, TypeCtor0) ->
        RevTypeInfoMap0 = !.Info ^ rev_type_info_map,
        ( list__map(map__search(RevTypeInfoMap0), ArgTypeInfoVars, ArgTypes) ->
            list__length(ArgTypes, Arity),
            % Just in case TypeCtorInfo0 has fake arity, e.g. if it is a tuple.
            TypeCtor0 = SymName - _DeclArity,
            TypeCtor1 = SymName - Arity,
            construct_type(TypeCtor1, ArgTypes, Type),
            record_type_info_var(Type, Var, !Info)
        ;
            !:Info = !.Info
        )
    ;
        % We don't know what type is being constructed, because we have
        % forgotten what type constructor the type constructor variable
        % stands for due to an intervening call. This cannot happen
        % for HLDS code constructed directly by polymorphism.m, but
        % can happen after that code has been rearranged by other
        % compiler passes.
        true
    ).

:- pred record_type_info_var(mer_type::in, prog_var::in, info::in, info::out)
    is det.

record_type_info_var(Type, Var, !Info) :-
    RevTypeInfoMap0 = !.Info ^ rev_type_info_map,
    TypeInfoMap0 = !.Info ^ type_info_map,
    map__set(TypeInfoMap0, Type, Var, TypeInfoMap),
    ( map__insert(RevTypeInfoMap0, Var, Type, RevTypeInfoMap1) ->
        RevTypeInfoMap = RevTypeInfoMap1
    ;
        % This can happen because inlining XXX can leave a
        % type_info_varmap saying that one type_info variable
        % holds the typeinfo for more than one type.
        RevTypeInfoMap = RevTypeInfoMap0
    ),
    !:Info = !.Info ^ type_info_map := TypeInfoMap,
    !:Info = !.Info ^ rev_type_info_map := RevTypeInfoMap.

:- pred record_known_size(prog_var::in, int::in, info::in, info::out) is det.

record_known_size(Var, KnownSize, !Info) :-
    KnownSizeMap0 = !.Info ^ known_size_map,
    map__det_insert(KnownSizeMap0, Var, KnownSize, KnownSizeMap),
    !:Info = !.Info ^ known_size_map := KnownSizeMap.

:- pred record_typeinfo_in_type_info_varmap(rtti_varmaps::in, tvar::in,
    info::in, info::out) is det.

record_typeinfo_in_type_info_varmap(RttiVarMaps, TVar, !Info) :-
    rtti_lookup_type_info_locn(RttiVarMaps, TVar, TypeInfoLocn),
    % XXX kind inference:
    % we assume the kind is `star'.
    Type = variable(TVar, star),
    (
        TypeInfoLocn = type_info(TypeInfoVar),
        record_type_info_var(Type, TypeInfoVar, !Info)
    ;
        TypeInfoLocn = typeclass_info(_TypeClassInfoVar, _Offset)
        % We could record this information and then look for calls that
        % extract typeinfos from typeclass_infos, but code that does
        % that is rare enough that it is not worth optimizing.
        % TypeClassInfoMap0 = !.Info ^ type_class_info_map,
        % map__det_insert(TypeClassInfoMap0,
        %   TypeClassInfoVar - Offset, Type, TypeClassInfoMap),
        % !:Info = !.Info ^ type_class_info_map := TypeClassInfoMap
    ).

%---------------------------------------------------------------------------%

    % We must ensure that we record that a branched control structure is
    % considered to generate a type_ctor_info or type_info variable only
    % if all the branches generate it. The code above takes the intersections
    % of the forward maps (type to type_info or type_ctor_info var maps)
    % produced by different branches directly, but calls update_rev_maps
    % to ensure that the reverse maps
    %
    % (a) contain only entries that are also in the forward maps, i.e. do not
    %     contain entries that the intersection process removed, and
    % (b) do not contain entries derived from inconsistent forward map entries
    %     (since a forward map can say that e.g. both the type int and the type
    %     constructor int/0 have their typeinfo in the same variable).
    %
:- pred update_rev_maps(info::in, info::out) is det.

update_rev_maps(!Info) :-
    map__to_sorted_assoc_list(!.Info ^ type_info_map, TypeInfoList),
    map__to_sorted_assoc_list(!.Info ^ type_ctor_map, TypeCtorList),
    map__init(VarCounts0),
    count_appearances(TypeInfoList, VarCounts0, VarCounts1),
    count_appearances(TypeCtorList, VarCounts1, VarCounts),
    construct_rev_map(TypeInfoList, VarCounts, map__init, RevTypeInfoMap),
    construct_rev_map(TypeCtorList, VarCounts, map__init, RevTypeCtorMap),
    !:Info = !.Info ^ rev_type_info_map := RevTypeInfoMap,
    !:Info = !.Info ^ rev_type_ctor_map := RevTypeCtorMap.

:- pred count_appearances(assoc_list(T, prog_var)::in,
    map(prog_var, int)::in, map(prog_var, int)::out) is det.

count_appearances([], VarCounts, VarCounts).
count_appearances([_ - Var | AssocList], VarCounts0, VarCounts) :-
    ( map__search(VarCounts0, Var, Count) ->
        map__det_update(VarCounts0, Var, Count + 1, VarCounts1)
    ;
        map__det_insert(VarCounts0, Var, 1, VarCounts1)
    ),
    count_appearances(AssocList, VarCounts1, VarCounts).

:- pred construct_rev_map(assoc_list(T, prog_var)::in,
    map(prog_var, int)::in,
    map(prog_var, T)::in, map(prog_var, T)::out) is det.

construct_rev_map([], _, RevMap, RevMap).
construct_rev_map([T - Var | AssocList], VarCounts, RevMap0, RevMap) :-
    map__lookup(VarCounts, Var, Count),
    ( Count = 1 ->
        map__det_insert(RevMap0, Var, T, RevMap1)
    ;
        RevMap1 = RevMap0
    ),
    construct_rev_map(AssocList, VarCounts, RevMap1, RevMap).

%---------------------------------------------------------------------------%

    % During the processing of a branched control structure, we add entries
    % to the target type_info map in an effort to encourage different branches
    % to use the same variable to store the type_info for the same type,
    % since this increases the probability that all branches define a type_info
    % for the type and that thus we will be able to use the variable holding
    % that type_info after the branched control structure without recreating
    % it. However, if some branches define the target variable but others
    % don't, then the branched control structure cannot define the variable
    % for later code. We must therefore remove the variable from the target
    % type_info map used by later code.
    %
:- pred update_target_map(info::in, info::out) is det.

update_target_map(!Info) :-
    TargetTypeInfoMap0 = !.Info ^ target_type_info_map,
    TypeInfoMap = !.Info ^ type_info_map,
    map__to_sorted_assoc_list(TargetTypeInfoMap0, TargetTypeInfoList),
    list__foldl(include_in_target_map(TypeInfoMap), TargetTypeInfoList,
        map__init, TargetTypeInfoMap),
    !:Info = !.Info ^ target_type_info_map := TargetTypeInfoMap.

:- pred include_in_target_map(type_info_map::in, pair(mer_type, prog_var)::in,
    type_info_map::in, type_info_map::out) is det.

include_in_target_map(TypeInfoMap, Type - TypeInfoVar, !TargetTypeInfoMap) :-
    ( map__search(TypeInfoMap, Type, TypeInfoVar) ->
        svmap__det_insert(Type, TypeInfoVar, !TargetTypeInfoMap)
    ;
        true
    ).

%---------------------------------------------------------------------------%

:- func compute_functor_size(list(prog_var), info) = int.

compute_functor_size(Args, Info) = FunctorSize :-
    TransformOp = Info ^ transform_op,
    (
        TransformOp = term_cells,
        FunctorSize = 1
    ;
        TransformOp = term_words,
        FunctorSize = list__length(Args)
    ).

:- pred find_defined_args(list(prog_var)::in, list(uni_mode)::in,
    list(prog_var)::out, list(prog_var)::out, info::in) is det.

find_defined_args(Args, Modes, DefinedArgs, NonDefinedArgs, Info) :-
    (
        Args = [],
        Modes = [],
        DefinedArgs = [],
        NonDefinedArgs = []
    ;
        Args = [],
        Modes = [_ | _],
        unexpected(this_file, "size_prof__find_defined_args: length mismatch")
    ;
        Args = [_ | _],
        Modes = [],
        unexpected(this_file, "size_prof__find_defined_args: length mismatch")
    ;
        Args = [FirstArg | LaterArgs],
        Modes = [FirstMode | LaterModes],
        find_defined_args(LaterArgs, LaterModes, LaterDefinedArgs,
            LaterNonDefinedArgs, Info),
        ( binds_arg_in_cell(Info, FirstMode) ->
            DefinedArgs = [FirstArg | LaterDefinedArgs],
            NonDefinedArgs = LaterNonDefinedArgs
        ;
            DefinedArgs = LaterDefinedArgs,
            NonDefinedArgs = [FirstArg | LaterNonDefinedArgs]
        )
    ).

:- pred binds_arg_in_cell(info::in, uni_mode::in) is semidet.

binds_arg_in_cell(Info, (CellInitInst - _ArgInitInst) ->
        (CellFinalInst - _ArgFinalInst)) :-
    ModuleInfo = Info ^ module_info,
    inst_is_free(ModuleInfo, CellInitInst),
    inst_is_bound(ModuleInfo, CellFinalInst).

%---------------------------------------------------------------------------%

:- pred select_first(T::in, T::in, T::out) is det.

select_first(X, _, X).

%---------------------------------------------------------------------------%

:- pred ctor_is_type_info_related(module_name::in, string::in) is semidet.

ctor_is_type_info_related(VarTypeCtorModule, VarTypeCtorName) :-
    VarTypeCtorModule = mercury_private_builtin_module,
    ( VarTypeCtorName = "type_info"
    ; VarTypeCtorName = "type_ctor_info"
    ; VarTypeCtorName = "typeclass_info"
    ; VarTypeCtorName = "base_typeclass_info"
    ).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "size_prof.m".

%---------------------------------------------------------------------------%
:- end_module size_prof.
%---------------------------------------------------------------------------%
