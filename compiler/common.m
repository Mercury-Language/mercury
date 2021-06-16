%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: common.m.
% Original author: squirrel (Jane Anna Langley).
% Other authors: fjh, zs, stayl.
%
% The main task of this module is to look for conjoined goals that involve
% the same structure (the "common" structure the module is named after),
% and to optimize those goals. The reason why we created this module was
% code like this:
%
%   X => f(A, B, C),
%   ...
%   Y <= f(A, B, C)
%
% This module replaces this code with
%
%   X => f(A, B, C),
%   ...
%   Y := X
%
% since this allocates less memory on the heap.
%
% We want to perform this optimization even if the deconstruction of X and
% the construction of Y are not in the same conjunction, but are nevertheless
% conjoined (e.g. because the construction of Y is inside an if-then-else
% or a disjunction that is inside the conjunction containing the deconstruction
% of X). We also want to do it if the two argument lists are not equal
% syntactically, but instead look like this:
%
%   X => f(A, B, C1),
%   ...
%   C2 := C1
%   ...
%   Y <= f(A, B, C2)
%
% We therefore have to keep track of pretty much all unifications in the body
% of the procedure being optimized. Since we have this information laying
% around anyway, we also use to for two other purposes. The first is
% to eliminate unnecessary tests of function symbols, replacing
%
%   X => f(A1, B1, C1),
%   ...
%   X => f(A2, B2, C2)
%
% with
%
%   X => f(A1, B1, C1),
%   ...
%   A2 := A1,
%   B2 := B1,
%   C2 := C1
%
% provided that this does not increase the number of variables that
% have to be saved across calls and other stack flushes.
%
% The other is to detect and optimize duplicate calls, replacing
%
%   p(InA, InB, OutC1, OutD1),
%   ...
%   p(InA, InB, OutC2, OutD2)
%
% with
%
%   p(InA, InB, OutC1, OutD1),
%   ...
%   OutC2 := OutC1,
%   OutD2 := OutD1
%
% Since the author probably did not mean to write duplicate calls, we also
% generate a warning for such code, if the option asking for such warnings
% is set.
%
% IMPORTANT: This module does a small subset of the job of compile-time
% garbage collection, but it does so without paying attention to uniqueness
% information, since the compiler does not yet have such information.
% Once we implement ctgc, the assumptions made by this module
% will have to be revisited.
%
% NOTE: There is another compiler module, cse_detection.m, that looks for
% unifications involving common structures in *disjoined*, not *conjoined*
% goals. Its purpose is not optimization, but the generation of more precise
% determinism information.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.common.
:- interface.

:- import_module check_hlds.simplify.simplify_info.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Assorted stuff used here that the rest of the simplify package
    % does not need to know about.
    %
:- type common_info.
:- func common_info_init(simplify_tasks) = common_info.

    % Handle the effects of an operation that causes a stack flush.
    %
:- pred common_info_stack_flush(common_info::in, common_info::out) is det.

    % If we find a construction that constructs a cell identical to one we
    % have seen before, replace the construction with an assignment from the
    % variable that already holds that cell.
    %
    % If we find a deconstruction or a construction we cannot optimize, record
    % the details of the memory cell in the updated common_info.
    %
:- pred common_optimise_unification(unify_rhs::in, unify_mode::in,
    unification::in, unify_context::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Check whether this call has been seen before and is replaceable.
    % If it is, generate assignment unifications for the nonlocal output
    % variables (to remove the redundant call), and a warning (since the
    % programmer probably did not mean to write a redundant call).
    %
    % A call is considered replaceable if it is pure, and it has neither
    % destructive inputs nor uniquely moded outputs.
    %
:- pred common_optimise_call(pred_id::in, proc_id::in, list(prog_var)::in,
    purity::in, hlds_goal_info::in,
    hlds_goal_expr::in, maybe(hlds_goal_expr)::out,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

:- pred common_optimise_higher_order_call(prog_var::in, list(prog_var)::in,
    list(mer_mode)::in, determinism::in, purity::in, hlds_goal_info::in,
    hlds_goal_expr::in, maybe(hlds_goal_expr)::out,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Succeeds if the two variables are equivalent according to the
    % information in the specified common_info.
    %
:- pred common_vars_are_equivalent(common_info::in,
    prog_var::in, prog_var::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_report.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.options.
:- import_module libs.optimization_options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.pd_cost.

:- import_module bool.
:- import_module eqvclass.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.

%---------------------------------------------------------------------------%

    % This module can implement two related family of optimizations.
    %
    % The original family of optimizations that this module was created for
    % is described in the big comment at the top of this module.
    % This family of optimizations uses the information in the
    % common_struct_info, and is enabled if and only if the common_struct_info
    % is actually present.
    %
    % The second optimization is the replacement of code that constructs
    % ground constant structure dynamically (i.e. at runtime) with code
    % that constructs that same ground term statically (i.e. at compile time).
    % It uses the const_struct_info, and is enabled if and only if
    % the const_struct_info is present. The optimization is described
    % in more detail in the comment above the definition of that type.
    %
    % All four combinations of the two structures being absent vs present
    % are legal.
    % 
:- type common_info
    --->    common_info(
                maybe(common_struct_info),
                maybe(const_struct_info)
            ).

%---------------------%

    % The var_eqv field records information about which sets of variables are
    % known to be equivalent, usually because they have been unified. This is
    % useful when eliminating duplicate unifications and when eliminating
    % duplicate calls.
    %
    % The all_structs and since_call_structs fields record information about
    % the memory cells available for reuse. The all_structs field has info
    % about all the cells available at the current program point. The
    % since_call_structs field contains info about the subset of these cells
    % that have been seen since the last stack flush, which is usually a call.
    %
    % The reason why we make the distinction between structs seen before the
    % last call and structs seen after is best explained by these two program
    % fragments:
    %
    % fragment 1:
    %   X => f(A1, A2, A3, A4),
    %   X => f(B1, B2, B3, B4),
    %
    % fragment 2:
    %   X => f(A1, A2, A3, A4),
    %   p(...),
    %   X => f(B1, B2, B3, B4),
    %
    % In fragment 1, we want to replace the second deconstruction with
    % the assignments B1 = A1, ... B4 = A4, since this can avoid the
    % second check of X's function symbol. (If the inst of X at the start
    % of the second unification is `bound(f(...))', we can dispense with
    % this test anyway, but if the two unifications are brought together
    % by inlining, then X's inst then may simply be `ground'.)
    %
    % In fragment 2, we don't want make the same transformation, because
    % doing so would require storing A1 ... A4 across the call instead of
    % just X.
    %
    % If the second unification were a construction instead of a
    % deconstruction, we want to make the transformation in both cases,
    % because the heap allocation we thus avoid is quite expensive,
    % and because it actually reduces the number of stack slots we need
    % across the call (X instead of A1 .. A4). The exception is
    % constructions using function symbols of arity zero, which we
    % never need to eliminate. We process unifications with constants
    % only to update our information about variable equivalences: after
    % X = c and Y = c, X and Y are equivalent.
    %
    % The seen_calls field records which calls we have seen, which we use
    % to eliminate duplicate calls.
    %
    % XXX One struct_map should be enough. It should be handled as all_structs
    % if common_struct_task = common_task_extra, and as since_call_structs
    % if common_struct_task = common_task_std.

:- type common_struct_info
    --->    common_struct_info(
                common_struct_task      :: common_struct_task,
                var_eqv                 :: eqvclass(prog_var),
                all_structs             :: struct_map,
                since_call_structs      :: struct_map,
                since_call_vars         :: set_of_progvar,
                seen_calls              :: seen_calls
            ).

:- type common_struct_task
    --->    common_task_only_eqv
            % Only record var-to-var equivalences; do not optimise
            % constructions or deconstructions.
    ;       common_task_std
            % Do optimise construction unifications as described
            % in the comment above common_struct_info, but only if
            % it does not lead to storing more variables on the stack.
    ;       common_task_extra.
            % Do optimise construction unifications as described
            % in the comment above common_struct_info, even if
            % it leads to storing more variables on the stack.

    % A struct_map maps a principal type constructor and a cons_id of that
    % type to information about cells involving that cons_id.
    %
    % The reason why we need the principal type constructors is that
    % two syntactically identical structures are guaranteed to have
    % compatible representations if and ONLY if their principal type
    % constructors are the same. For example, if we have:
    %
    %   :- type maybe_err(T) ---> ok(T) ; err(string).
    %
    %   :- pred p(maybe_err(foo)::in, maybe_err(bar)::out) is semidet.
    %   p(err(X), err(X)).
    %
    % then we want to reuse the `err(X)' in the first arg rather than
    % constructing a new copy of it for the second arg.
    % The two occurrences of `err(X)' have types `maybe_err(int)' and
    % `maybe(float)', but we know that they have the same representation.
    %
    % Instead of a simple map whose keys are <type_ctor, cons_id> pairs,
    % we use a two-stage map, with the keys being type_ctors in the first stage
    % and cons_ids in the second. Having two stages makes the comparisons
    % cheaper, and we put the type_ctors first to avoid mixing together
    % cons_ids from different type constructors.

:- type struct_map == map(type_ctor, cons_id_map).
:- type cons_id_map == map(cons_id, structures).

    % Given a unification X = f(Y1, ... Yn), we record its availability for
    % reuse by creating structure(X, [Y1, ... Yn]), and putting it at the
    % front of the list of structures for the entry for f and X's type_ctor.

:- type structures == list(structure).
:- type structure
    --->    structure(prog_var, list(prog_var)).

:- type seen_calls == map(seen_call_id, list(call_args)).

:- type call_args
    --->    call_args(
                % The context of the call, for use in warnings about
                % duplicate calls.
                prog_context,

                % The input arguments. For higher-order calls, the closure
                % is the first input argument.
                list(prog_var),

                % The output arguments.
                list(prog_var)
            ).

%---------------------%

    % The const struct optimization, if enabled, looks for construction
    % unifications X = f(...) where all the RHS arguments are constant terms,
    % and replaces them with X = ground_term_const(N), where ground constant
    % term #N in the const_struct_db is f(...).
    %
    % The const_var_map, which maps each variable that contains a
    % known-to-be-ground term to its representation as an argument
    % in a const_struct, is stored in here, in the common_info.
    % Entries put into the common_info in one branch of a control structure
    % are used only in the rest of that branch; they are not used
    % either in other branches, or in code after the branched control
    % structure. (This means that we reset the common_info both when entering
    % a non-first branch of a branched control structure, and when leaving
    % a branched control structure.) However, we never reset the
    % const_struct_db, which is stored inside the module_info, which
    % in turn is inside the simplify_info. In other words, the common_info
    % is a program-point-specific data structure, but the simplify_info
    % is not.

:- type const_struct_info
    --->    const_struct_info(
                const_var_map           :: const_var_map
            ).

:- type const_var_map == map(prog_var, const_struct_arg).

%---------------------------------------------------------------------------%

common_info_init(SimplifyTasks) = Common :-
    OptCommonStructs = SimplifyTasks ^ do_opt_common_structs,
    (
        OptCommonStructs = opt_common_structs,
        OptExtraStructs = SimplifyTasks ^ do_opt_extra_structs,
        (
            OptExtraStructs = opt_extra_structs,
            MaybeCommonStructTask = yes(common_task_extra)
        ;
            OptExtraStructs = do_not_opt_extra_structs,
            MaybeCommonStructTask = yes(common_task_std)
        )
    ;
        OptCommonStructs = do_not_opt_common_structs,
        WarnDuplicateCalls = SimplifyTasks ^ do_warn_duplicate_calls,
        OptDuplicateCalls = SimplifyTasks ^ do_opt_duplicate_calls,
        ( if
            ( WarnDuplicateCalls = warn_duplicate_calls
            ; OptDuplicateCalls = opt_dup_calls
            )
        then
            MaybeCommonStructTask = yes(common_task_only_eqv)
        else
            MaybeCommonStructTask = no
        )
    ),
    (
        MaybeCommonStructTask = no,
        MaybeCommonStruct = no
    ;
        MaybeCommonStructTask = yes(CommonStructTask),
        eqvclass.init(VarEqv0),
        map.init(StructMap0),
        set_of_var.init(SinceCallVars0),
        map.init(SeenCalls0),
        CommonStruct = common_struct_info(CommonStructTask,
            VarEqv0, StructMap0, StructMap0, SinceCallVars0, SeenCalls0),
        MaybeCommonStruct = yes(CommonStruct)
    ),
    OptConstStruct = SimplifyTasks ^ do_opt_const_structs,
    (
        OptConstStruct = opt_const_structs,
        map.init(ConstVarMap0),
        ConstStruct = const_struct_info(ConstVarMap0),
        MaybeConstStruct = yes(ConstStruct)
    ;
        OptConstStruct = do_not_opt_const_structs,
        MaybeConstStruct = no
    ),
    Common = common_info(MaybeCommonStruct, MaybeConstStruct).

%---------------------------------------------------------------------------%

common_info_stack_flush(!Info) :-
    !.Info = common_info(MaybeCommonStruct0, ConstStruct),
    (
        MaybeCommonStruct0 = no
        % There is no information to flush.
    ;
        MaybeCommonStruct0 = yes(CommonStruct0),
        Task = CommonStruct0 ^ common_struct_task,
        (
            ( Task = common_task_only_eqv
            ; Task = common_task_std
            ),
            % Clear the common_info structs accumulated since the last goal
            % that could cause a stack flush. This is done to avoid replacing
            % a deconstruction with assignments to the arguments where this
            % would cause more variables to be live across the stack flush.
            % Calls and construction unifications are not treated in this way
            % since it is nearly always better to optimize them away.
            %
            % Clear the set of variables seen since the last stack flush,
            % for the same reason.
            CommonStruct = ((CommonStruct0
                ^ since_call_structs := map.init)
                ^ since_call_vars := set_of_var.init),
            !:Info = common_info(yes(CommonStruct), ConstStruct)
        ;
            Task = common_task_extra
            % When doing deforestation, which is the only compiler pass
            % that sets common_task_extra, we try to remove as many
            % common structures as possible, even when this causes
            % more variables to be stored on the stack.
        )
    ).

%---------------------------------------------------------------------------%

common_optimise_unification(RHS0, UnifyMode, Unification0, UnifyContext,
        !GoalExpr, !GoalInfo, !Common, !Info) :-
    (
        Unification0 = construct(_, _, _, _, _, _, SubInfo),
        ( if
            % The call to common_optimise_construct below will try to perform
            % one of two optimizations on this construction unification.
            % 
            % - The first is replacing a dynamic unification with an
            %   assignment whose right hand side is a reference to
            %   a constant structure. We try to do this if !.Common
            %   contains a const_struct_info.
            %
            % - The second is to replace the construction with an assignment
            %   from a variable that already contains the term that the
            %   construction would build. We try to do this if !.Common
            %   contains a common_struct_info.
            %
            % There are two tests that must pass before we can attempt
            % either optimization, and we test those here. Each optimization
            % also has a test that only it requires; those tests are done
            % inside common_optimise_construct.
            %
            % All these tests usually pass, so the order in which we test
            % for them does not matter much.

            % The first common test is that none of the arguments should have
            % their addresses taken. This is because the address being taken
            % signifies that the value being put into the argument now
            % is only a dummy, with the real value being supplied later
            % (as can happen with code that has been optimized with
            % last-call-modulo-construction).
            (
                SubInfo = no_construct_sub_info
            ;
                SubInfo = construct_sub_info(MaybeTakeAddr, _),
                MaybeTakeAddr = no
            ),

            % The second common test checks that we don't optimise partially
            % instantiated construction unifications, because it would be
            % tricky to work out how to mode the replacement assignment
            % unifications. In the vast majority of cases, the variable
            % is ground.
            simplify_info_get_module_info(!.Info, ModuleInfo),
            UnifyMode = unify_modes_li_lf_ri_rf(_, LVarFinalInst, _, _),
            inst_is_ground(ModuleInfo, LVarFinalInst)
        then
            common_optimise_construct(RHS0, UnifyMode, Unification0,
                UnifyContext, !GoalExpr, !GoalInfo, !Common, !Info)
        else
            true
        )
    ;
        Unification0 = deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail, _),
        !.Common = common_info(MaybeCommonStruct0, MaybeConstStruct0),
        some [!CommonStruct]
        (
            MaybeCommonStruct0 = no
        ;
            MaybeCommonStruct0 = yes(!:CommonStruct),
            GoalExpr0 = !.GoalExpr,
            GoalInfo0 = !.GoalInfo,
            UnifyMode = unify_modes_li_lf_ri_rf(LVarInitInst, _, _, _),
            simplify_info_get_module_info(!.Info, ModuleInfo),
            ( if
                % Don't optimise partially instantiated deconstruction
                % unifications, because it would be tricky to work out
                % how to mode the replacement assignment unifications.
                % In the vast majority of cases, the variable is ground.
                inst_is_ground(ModuleInfo, LVarInitInst)
                % XXX See the comment on how_to_construct_is_acceptable.
            then
                common_optimise_deconstruct(Var, ConsId, ArgVars, ArgModes,
                    CanFail, !GoalExpr, !GoalInfo, !CommonStruct, !Info),
                maybe_restore_original_goal(!.CommonStruct,
                    no_override_by_const_struct, GoalExpr0, GoalInfo0,
                    !GoalExpr, !GoalInfo)
            else
                true
            ),
            record_nonlocals_as_seen(!.GoalInfo, !CommonStruct),
            !:Common = common_info(yes(!.CommonStruct), MaybeConstStruct0)
        )
    ;
        ( Unification0 = assign(Var1, Var2)
        ; Unification0 = simple_test(Var1, Var2)
        ),
        !.Common = common_info(MaybeCommonStruct0, MaybeConstStruct0),
        some [!CommonStruct]
        (
            MaybeCommonStruct0 = no
        ;
            MaybeCommonStruct0 = yes(!:CommonStruct),
            record_equivalence(Var1, Var2, !CommonStruct),
            record_nonlocals_as_seen(!.GoalInfo, !CommonStruct),
            !:Common = common_info(yes(!.CommonStruct), MaybeConstStruct0)
        )
    ;
        Unification0 = complicated_unify(_, _, _),
        % The call in simplify_goal_unify.m to common_optimise_unification
        % is preceded by a test that prevents that call for complicated
        % unifications.
        unexpected($pred, "complicated_unify")
    ).

%---------------------------------------------------------------------------%

:- pred common_optimise_construct(unify_rhs::in, unify_mode::in,
    unification::in(unification_construct), unify_context::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

common_optimise_construct(RHS0, UnifyMode0, Unification0, UnifyContext0,
        !GoalExpr, !GoalInfo, !Common, !Info) :-
    Unification0 =
        construct(Var, ConsId, ArgVars, _ArgModes, How, _Uniq, _SubInfo),

    !.Common = common_info(MaybeCommonStruct0, MaybeConstStruct0),
    GoalExpr0 = !.GoalExpr,
    GoalInfo0 = !.GoalInfo,
    (
        MaybeConstStruct0 = no,
        MaybeConstStruct = no,
        Override = no_override_by_const_struct
    ;
        MaybeConstStruct0 = yes(ConstStruct0),
        ConstStruct0 = const_struct_info(VarMap0),
        (
            ArgVars = [],
            ( if ConsId = ground_term_const(ConstNum, _) then
                map.det_insert(Var, csa_const_struct(ConstNum),
                    VarMap0, VarMap)
            else
                simplify_info_get_var_types(!.Info, VarTypes),
                lookup_var_type(VarTypes, Var, Type),
                map.det_insert(Var, csa_constant(ConsId, Type),
                    VarMap0, VarMap)
            ),
            ConstStruct = const_struct_info(VarMap),
            MaybeConstStruct = yes(ConstStruct),
            Override = no_override_by_const_struct
        ;
            ArgVars = [_ | _],
            ( if
                all_vars_are_const_struct_args(VarMap0, ArgVars, CSAs),
                % In an is_exist_constr unification, the types of some
                % arguments are described by the values of other
                % (type_info and/or typeclass_info) arguments, and *not*
                % by the type recorded for a given const_struct.
                % We cannot apply this optimization to is_exist_constr
                % unifications unless we teach the backends about how
                % to handle this situation. That handling would be
                % highly nontrivial, and since the situation is very rare,
                % there is no point in expending the effort.
                RHS0 = rhs_functor(_, is_not_exist_constr, _)
            then
                generate_assign_from_const_struct(Unification0, UnifyMode0,
                    UnifyContext0, CSAs, GoalInfo0,
                    ConstGoalExpr, ConstGoalInfo, VarMap0, VarMap, !Info),
                ConstStruct = const_struct_info(VarMap),
                MaybeConstStruct = yes(ConstStruct),
                Override =
                    override_by_const_struct(ConstGoalExpr, ConstGoalInfo)
            else
                MaybeConstStruct = MaybeConstStruct0,
                Override = no_override_by_const_struct
            )
        )
    ),

    some [!CommonStruct]
    (
        MaybeCommonStruct0 = no,
        MaybeCommonStruct = no
    ;
        MaybeCommonStruct0 = yes(!:CommonStruct),
        ( if how_to_construct_is_acceptable(!.Info, How) then
            TypeCtor = lookup_var_type_ctor(!.Info, Var),
            VarEqv0 = !.CommonStruct ^ var_eqv,
            list.map_foldl(eqvclass.ensure_element_partition_id,
                ArgVars, ArgVarIds, VarEqv0, VarEqv1),
            AllStructMap0 = !.CommonStruct ^ all_structs,
            ( if
                map.search(AllStructMap0, TypeCtor, ConsIdMap0),
                map.search(ConsIdMap0, ConsId, Structs),
                find_matching_cell_construct(Structs, VarEqv1, ArgVarIds,
                    OldStruct),
                % generate_assign assumes that the output variable is in the
                % instmap_delta, which will not be true if the variable
                % is local to the unification. The optimization is pointless
                % in that case.
                %
                % This test is after find_matching_cell_construct, because
                % that call is *much* more likely to fail than this test,
                % even though it is also significantly more expensive.
                InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
                instmap_delta_search_var(InstMapDelta, Var, _)
            then
                OldStruct = structure(OldVar, _),
                eqvclass.ensure_equivalence(Var, OldVar, VarEqv1, VarEqv),
                !CommonStruct ^ var_eqv := VarEqv,
                (
                    ArgVars = []
                    % Constants don't use memory, so there is no point in
                    % optimizing away their construction; in fact, doing so
                    % could cause more stack usage.
                ;
                    ArgVars = [_ | _],
                    UnifyMode0 =
                        unify_modes_li_lf_ri_rf(_, LVarFinalInst, _, _),
                    VarFromToInsts =
                        from_to_insts(LVarFinalInst, LVarFinalInst),
                    generate_assign(Var, OldVar, VarFromToInsts, GoalInfo0,
                        !:GoalExpr, !:GoalInfo, !CommonStruct, !Info),
                    simplify_info_set_rerun_quant_instmap_delta(!Info),
                    goal_cost(hlds_goal(GoalExpr0, GoalInfo0), Cost),
                    simplify_info_incr_cost_delta(Cost, !Info)
                )
            else
                common_standardize_and_record_construct(Var, TypeCtor, ConsId,
                    ArgVars, VarEqv1, !GoalExpr, !GoalInfo,
                    !CommonStruct, !Info)
            ),
            maybe_restore_original_goal(!.CommonStruct, Override,
                GoalExpr0, GoalInfo0, !GoalExpr, !GoalInfo),
            record_nonlocals_as_seen(!.GoalInfo, !CommonStruct),
            MaybeCommonStruct = yes(!.CommonStruct)
        else
            MaybeCommonStruct = MaybeCommonStruct0
        )
    ),
    !:Common = common_info(MaybeCommonStruct, MaybeConstStruct).

:- pred all_vars_are_const_struct_args(const_var_map::in, list(prog_var)::in,
    list(const_struct_arg)::out) is semidet.

all_vars_are_const_struct_args(_VarMap, [], []).
all_vars_are_const_struct_args(VarMap, [ArgVar | ArgVars], [CSA | CSAs]) :-
    map.search(VarMap, ArgVar, CSA),
    all_vars_are_const_struct_args(VarMap, ArgVars, CSAs).

    % The third test, applied specifically to the MLDS backend,
    % is that mark_static_terms.m should not have already decided
    % that we construct Var statically. This is because if it has,
    % then it may have *also* decided that a term where Var occurs
    % on the right hand side should *also* be constructed statically.
    % If we replace the static construction of Var with an assign
    % to Var from a coincidentally-guaranteed-to-be-identical term
    % from somewhere else, as in tests/valid/bug493.m, then Var
    % won't be marked as a static term in the MLDS code generator
    % (the only backend that gets its info about what terms should be
    % static from mark_static_terms.m.), and we get a compiler abort
    % when we get to the occurrence of Var on the right hand side
    % of the later term.
    %
    % The LLDS backend decides what terms it can allocate statically
    % in var_locn.m, during code generation; it does not pay attention
    % to the construct_how field. When targeting this backend, the
    % compiler does not invoke the mark_static_terms pass at the
    % default optimization level, but it does invoke it when the
    % --loop-invariants option is set. To reflect the fact that
    % the LLDS code generator will treat construction unifications
    % marked static by mark_static_terms.m the same way it would treat
    % construction unifications with construct_dynamically, we set
    % the maybe_ignore_marked_static field of the simplify_info to 
    % ignore_marked_static when targeting the LLDS backend.
    %
    % Note also that the problem we have described above for the
    % MLDS backend can happen *only* in procedure bodies that
    % have been modified after semantic analysis, e.g. by inlining.
    % This is because
    %
    % - we can see How = construct_statically only *after* the
    %   mark_static_terms pass has been run, which is way after
    %   the first simplification pass, which is run just after
    %   semantic analysis;
    %
    % - the common struct optimization we are implementing here
    %   is idempotent, so it can find new optimization opportunities
    %   on its second invocation only if the code has been modified
    %   after its first invocation.
    %
    % XXX This is only an instance of a more general problem.
    % We should replace X = f(...) with X = Y *only* if the location
    % of Y in terms of what memory area it is in (the heap, static
    % data, or a region) satisfies the constraints imposed by the code
    % that deals with X.
    %
    % Traditionally, except for the third test, the code we use here
    % has worked in the usual case where How says that Var should be
    % constructed either dynamically (on the heap) or statically.
    % However, I (zs) have grave doubts about whether it does
    % the right thing when either X or Y is supposed to be allocated
    % in a region. This is because (a) the optimization is valid
    % only if X and Y are supposed to be allocated from the *same*
    % region; and (b) common_optimise_deconstruct does not record
    % anything about Y, so we cannot possibly test for that here.
    %
:- pred how_to_construct_is_acceptable(simplify_info::in, how_to_construct::in)
    is semidet.

how_to_construct_is_acceptable(Info, How) :-
    (
        How = construct_dynamically
    ;
        How = construct_statically(_),
        simplify_info_get_ignore_marked_static(Info, ignore_marked_static)
    ).

%---------------------------------------------------------------------------%

    % The purpose of this predicate is to short-circuit variable-to-variable
    % equivalences in structure arguments.
    %
    % The kind of situation where this matters is a sequence of
    % updates to various fields of a structure. Consider the code
    %
    %   !S ^ f1 = F1,
    %   !S ^ f2 = F2
    %
    % where S has four fields. The compiler represents those two lines as
    %
    %   ( % removable barrier scope
    %       S0 = struct(_V11, V12, V13, V14),
    %       S1 = struct(  F1, V12, V13, V14)
    %   ),
    %   ( % removable barrier scope
    %       S1 = struct(V_21, _V22, V23, V24),
    %       S2 = struct(V_21,   F2, V13, V14)
    %   ),
    %
    % The compiler knows that V_21 is equivalent to F1, since both
    % occur in the same place, the first argument of S1. But as long as
    % the first argument of S2 is recorded as V_21, the compiler will
    % need to keep the goal that defines V_21, the deconstruction of S1,
    % which means that it also needs to keep the *construction* of S1.
    % This means that the compiler cannot optimize a sequence of field
    % assignments into the single construction of a new cell with all
    % the updated field values.
    %
    % We handle this by replacing each argument variable in a construction
    % unification with the lowest-numbered (and therefore earliest-introduced)
    % variable in its equivalence class (but see next paragraph). That means
    % that we would make the first argument of S2 be F1, not V_21. And since
    % we know that V23 and V24 are equivalent to V13 and V14 respectively
    % (due to their appearance in the third and fourth slots of S1), the args
    % from which we construct S2 would be F1, F2, V13 and V14.
    %
    % There is one qualification to the above. When we look for the lowest
    % numbered variable in the argument variable's equivalence class,
    % we confine our attention to the variables that we have seen since
    % the last call. This is because reading the value of a variable
    % that we last saw before a call will require the code generator
    % to save the value of that variable on the stack, which has costs
    % of its own. Between (a) saving the values of three fields in stack slots
    % and later loading those values from their stack slots, and (b) saving
    % just the cell variable on the stack, and later loading it from the stack
    % and then reading the fields from the heap, (b) is almost certainly
    % faster, since it does 1 store and 3 loads vs 3 stores and 3 loads.
    % When reusing just one or two fields, the difference almost certainly
    % going to be minor, and its direction (which approach is better) will
    % probably depend on information we don't have right now. I (zs) think
    % that not requiring extra variables to be stored in stack slots is
    % probably the better approach overall.
    %
:- pred common_standardize_and_record_construct(prog_var::in, type_ctor::in,
    cons_id::in, list(prog_var)::in, eqvclass(prog_var)::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    common_struct_info::in, common_struct_info::out,
    simplify_info::in, simplify_info::out) is det.

common_standardize_and_record_construct(Var, TypeCtor, ConsId, ArgVars, VarEqv,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, !CommonStruct, !Info) :-
    SinceCallVars = !.CommonStruct ^ since_call_vars,
    list.map(find_representative(SinceCallVars, VarEqv),
        ArgVars, ArgRepnVars),
    ( if
        ArgRepnVars = ArgVars
    then
        GoalExpr = GoalExpr0,
        GoalInfo = GoalInfo0
    else if
        GoalExpr0 = unify(Var, RHS0, UnifyMode, Unification0, Ctxt),
        RHS0 = rhs_functor(ConsId, IsExistConstr, ArgVars),
        Unification0 = construct(Var, ConsId, ArgVars, ArgModes, How,
            Uniq, SubInfo)
    then
        Unification = construct(Var, ConsId, ArgRepnVars, ArgModes, How,
            Uniq, SubInfo),
        RHS = rhs_functor(ConsId, IsExistConstr, ArgRepnVars),
        GoalExpr = unify(Var, RHS, UnifyMode, Unification, Ctxt),
        set_of_var.list_to_set([Var | ArgRepnVars], NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo0, GoalInfo),
        !CommonStruct ^ var_eqv := VarEqv,
        simplify_info_set_rerun_quant_instmap_delta(!Info)
    else
        unexpected($pred, "GoalExpr0 has unexpected shape")
    ),
    Struct = structure(Var, ArgRepnVars),
    record_cell_in_maps(TypeCtor, ConsId, Struct, VarEqv, !CommonStruct).

%---------------------%

    % Given a variable, return the lowest numbered variable in its
    % equivalence class that we have seen since the last stack flush.
    % See the comment on common_standardize_and_record_construct
    % for the reason why we do this.
    % 
:- pred find_representative(set_of_progvar::in,
    eqvclass(prog_var)::in, prog_var::in, prog_var::out) is det.

find_representative(SinceCallVars, VarEqv, Var, RepnVar) :-
    EqvVarsSet = get_equivalent_elements(VarEqv, Var),
    set.to_sorted_list(EqvVarsSet, EqvVars),
    ( if find_representative_loop(SinceCallVars, EqvVars, RepnVarPrime) then
        RepnVar = RepnVarPrime
    else
        RepnVar = Var
    ).

:- pred find_representative_loop(set_of_progvar::in, list(prog_var)::in,
    prog_var::out) is semidet.

find_representative_loop(SinceCallVars, [Var | Vars], RepnVar) :-
    ( if set_of_var.contains(SinceCallVars, Var) then
        RepnVar = Var
    else
        find_representative_loop(SinceCallVars, Vars, RepnVar)
    ).

%---------------------------------------------------------------------------%

:- pred common_optimise_deconstruct(prog_var::in, cons_id::in,
    list(prog_var)::in, list(unify_mode)::in, can_fail::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    common_struct_info::in, common_struct_info::out,
    simplify_info::in, simplify_info::out) is det.

common_optimise_deconstruct(Var, ConsId, ArgVars, ArgModes, CanFail,
        GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, !CommonStruct, !Info) :-
    TypeCtor = lookup_var_type_ctor(!.Info, Var),
    VarEqv0 = !.CommonStruct ^ var_eqv,
    eqvclass.ensure_element_partition_id(Var, VarId, VarEqv0, VarEqv1),
    SinceCallStructMap0 = !.CommonStruct ^ since_call_structs,
    ( if
        % Do not delete deconstruction unifications inserted by
        % stack_opt.m or tupling.m, which have done a more comprehensive
        % cost analysis than common.m can do.
        not goal_info_has_feature(GoalInfo, feature_stack_opt),
        not goal_info_has_feature(GoalInfo, feature_tuple_opt),

        map.search(SinceCallStructMap0, TypeCtor, ConsIdMap0),
        map.search(ConsIdMap0, ConsId, Structs),
        find_matching_cell_deconstruct(Structs, VarEqv1, VarId, OldStruct)
    then
        OldStruct = structure(_, OldArgVars),
        eqvclass.ensure_corresponding_equivalences(ArgVars,
            OldArgVars, VarEqv1, VarEqv),
        !CommonStruct ^ var_eqv := VarEqv,
        RHSFromToInsts = list.map(unify_mode_to_rhs_from_to_insts,
            ArgModes),
        create_output_unifications(GoalInfo0, ArgVars, OldArgVars,
            RHSFromToInsts, Goals, !CommonStruct, !Info),
        GoalExpr = conj(plain_conj, Goals),
        goal_cost(hlds_goal(GoalExpr0, GoalInfo0), Cost),
        simplify_info_incr_cost_delta(Cost, !Info),
        simplify_info_set_rerun_quant_instmap_delta(!Info),
        (
            CanFail = can_fail,
            simplify_info_set_rerun_det(!Info)
        ;
            CanFail = cannot_fail
        )
    else
        GoalExpr = GoalExpr0,
        Struct = structure(Var, ArgVars),
        record_cell_in_maps(TypeCtor, ConsId, Struct, VarEqv1, !CommonStruct)
    ),
    GoalInfo = GoalInfo0.

:- func lookup_var_type_ctor(simplify_info, prog_var) = type_ctor.

lookup_var_type_ctor(Info, Var) = TypeCtor :-
    simplify_info_get_var_types(Info, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    % If we unify a variable with a function symbol, we *must* know
    % what the principal type constructor of its type is.
    type_to_ctor_det(Type, TypeCtor).

%---------------------------------------------------------------------------%

:- pred find_matching_cell_construct(structures::in, eqvclass(prog_var)::in,
    list(partition_id)::in, structure::out) is semidet.

find_matching_cell_construct([Struct | Structs], VarEqv, ArgVarIds, Match) :-
    Struct = structure(_Var, Vars),
    ( if ids_vars_match(ArgVarIds, Vars, VarEqv) then
        Match = Struct
    else
        find_matching_cell_construct(Structs, VarEqv, ArgVarIds, Match)
    ).

:- pred find_matching_cell_deconstruct(structures::in, eqvclass(prog_var)::in,
    partition_id::in, structure::out) is semidet.

find_matching_cell_deconstruct([Struct | Structs], VarEqv, VarId, Match) :-
    Struct = structure(Var, _Vars),
    ( if id_var_match(VarId, Var, VarEqv) then
        Match = Struct
    else
        find_matching_cell_deconstruct(Structs, VarEqv, VarId, Match)
    ).

:- pred ids_vars_match(list(partition_id)::in, list(prog_var)::in,
    eqvclass(prog_var)::in) is semidet.

ids_vars_match([], [], _VarEqv).
ids_vars_match([Id | Ids], [Var | Vars], VarEqv) :-
    id_var_match(Id, Var, VarEqv),
    ids_vars_match(Ids, Vars, VarEqv).

:- pred id_var_match(partition_id::in, prog_var::in, eqvclass(prog_var)::in)
    is semidet.
:- pragma inline(pred(id_var_match/3)).

id_var_match(Id, Var, VarEqv) :-
    eqvclass.partition_id(VarEqv, Var, VarId),
    Id = VarId.

%---------------------------------------------------------------------------%

:- pred record_cell_in_maps(type_ctor::in, cons_id::in, structure::in,
    eqvclass(prog_var)::in,
    common_struct_info::in, common_struct_info::out) is det.

record_cell_in_maps(TypeCtor, ConsId, Struct, VarEqv, !CommonStruct) :-
    AllStructMap0 = !.CommonStruct ^ all_structs,
    SinceCallStructMap0 = !.CommonStruct ^ since_call_structs,
    do_record_cell_in_struct_map(TypeCtor, ConsId, Struct,
        AllStructMap0, AllStructMap),
    do_record_cell_in_struct_map(TypeCtor, ConsId, Struct,
        SinceCallStructMap0, SinceCallStructMap),
    !CommonStruct ^ var_eqv := VarEqv,
    !CommonStruct ^ all_structs := AllStructMap,
    !CommonStruct ^ since_call_structs := SinceCallStructMap.

:- pred do_record_cell_in_struct_map(type_ctor::in, cons_id::in,
    structure::in, struct_map::in, struct_map::out) is det.

do_record_cell_in_struct_map(TypeCtor, ConsId, Struct, !StructMap) :-
    ( if map.search(!.StructMap, TypeCtor, ConsIdMap0) then
        ( if map.search(ConsIdMap0, ConsId, Structs0) then
            Structs = [Struct | Structs0],
            map.det_update(ConsId, Structs, ConsIdMap0, ConsIdMap)
        else
            map.det_insert(ConsId, [Struct], ConsIdMap0, ConsIdMap)
        ),
        map.det_update(TypeCtor, ConsIdMap, !StructMap)
    else
        ConsIdMap = map.singleton(ConsId, [Struct]),
        map.det_insert(TypeCtor, ConsIdMap, !StructMap)
    ).

%---------------------------------------------------------------------------%

:- pred record_equivalence(prog_var::in, prog_var::in,
    common_struct_info::in, common_struct_info::out) is det.

record_equivalence(VarA, VarB, !CommonStruct) :-
    VarEqv0 = !.CommonStruct ^ var_eqv,
    eqvclass.ensure_equivalence(VarA, VarB, VarEqv0, VarEqv),
    !CommonStruct ^ var_eqv := VarEqv.

%---------------------------------------------------------------------------%

:- type maybe_override_by_const_struct
    --->    no_override_by_const_struct
    ;       override_by_const_struct(hlds_goal_expr, hlds_goal_info).

:- pred maybe_restore_original_goal(common_struct_info::in,
    maybe_override_by_const_struct::in,
    hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out) is det.

maybe_restore_original_goal(CommonStruct, Override, GoalExpr0, GoalInfo0,
        !GoalExpr, !GoalInfo) :-
    CommonStructTask = CommonStruct ^ common_struct_task,
    (
        ( CommonStructTask = common_task_std
        ; CommonStructTask = common_task_extra
        )
    ;
        CommonStructTask = common_task_only_eqv,
        % We keep the update of !Common, but we throw away any update
        % of the goal.
        !:GoalExpr = GoalExpr0,
        !:GoalInfo = GoalInfo0
    ),
    (
        Override = no_override_by_const_struct
    ;
        Override = override_by_const_struct(!:GoalExpr, !:GoalInfo)
    ).

:- pred record_nonlocals_as_seen(hlds_goal_info::in,
    common_struct_info::in, common_struct_info::out) is det.

record_nonlocals_as_seen(GoalInfo, !CommonStruct) :-
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    SinceCallVars0 = !.CommonStruct ^ since_call_vars,
    set_of_var.union(NonLocals, SinceCallVars0, SinceCallVars),
    !CommonStruct ^ since_call_vars := SinceCallVars.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

common_optimise_call(PredId, ProcId, Args, Purity, GoalInfo,
        GoalExpr0, MaybeAssignsGoalExpr, !Common, !Info) :-
    !.Common = common_info(MaybeCommonStruct0, ConstStruct),
    ( if
        MaybeCommonStruct0 = yes(CommonStruct0),
        Purity = purity_pure,
        Det = goal_info_get_determinism(GoalInfo),
        check_call_detism(Det),
        simplify_info_get_var_types(!.Info, VarTypes),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_get_argmodes(ProcInfo, ArgModes),
        partition_call_args(VarTypes, ModuleInfo, ArgModes, Args,
            InputArgs, OutputArgs, OutputModes)
    then
        common_do_optimise_call(seen_call(PredId, ProcId), InputArgs,
            OutputArgs, OutputModes, GoalInfo, GoalExpr0, MaybeAssignsGoalExpr,
            CommonStruct0, CommonStruct, !Info),
        !:Common = common_info(yes(CommonStruct), ConstStruct)
    else
        MaybeAssignsGoalExpr = no
    ).

common_optimise_higher_order_call(Closure, Args, Modes, Det, Purity, GoalInfo,
        GoalExpr0, MaybeAssignsGoalExpr, !Common, !Info) :-
    !.Common = common_info(MaybeCommonStruct0, ConstStruct),
    ( if
        MaybeCommonStruct0 = yes(CommonStruct0),
        Purity = purity_pure,
        check_call_detism(Det),
        simplify_info_get_var_types(!.Info, VarTypes),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        partition_call_args(VarTypes, ModuleInfo, Modes, Args,
            InputArgs, OutputArgs, OutputModes)
    then
        common_do_optimise_call(higher_order_call, [Closure | InputArgs],
            OutputArgs, OutputModes, GoalInfo, GoalExpr0, MaybeAssignsGoalExpr,
            CommonStruct0, CommonStruct, !Info),
        !:Common = common_info(yes(CommonStruct), ConstStruct)
    else
        MaybeAssignsGoalExpr = no
    ).

:- pred check_call_detism(determinism::in) is semidet.

check_call_detism(Det) :-
    determinism_components(Det, _, SolnCount),
    % Replacing nondet or multi calls would cause loss of solutions.
    ( SolnCount = at_most_one
    ; SolnCount = at_most_many_cc
    ).

:- pred common_do_optimise_call(seen_call_id::in, list(prog_var)::in,
    list(prog_var)::in, list(mer_mode)::in, hlds_goal_info::in,
    hlds_goal_expr::in, maybe(hlds_goal_expr)::out,
    common_struct_info::in, common_struct_info::out,
    simplify_info::in, simplify_info::out) is det.

common_do_optimise_call(SeenCall, InputArgs, OutputArgs, Modes, GoalInfo,
        GoalExpr0, MaybeAssignsGoalExpr, CommonStruct0, CommonStruct, !Info) :-
    Eqv0 = CommonStruct0 ^ var_eqv,
    SeenCalls0 = CommonStruct0 ^ seen_calls,
    ( if map.search(SeenCalls0, SeenCall, SeenCallsList0) then
        ( if
            find_previous_call(SeenCallsList0, InputArgs, Eqv0,
                OutputArgs2, PrevContext)
        then
            simplify_info_get_module_info(!.Info, ModuleInfo),
            list.map(mode_get_from_to_insts(ModuleInfo), Modes, FromToInsts),
            create_output_unifications(GoalInfo, OutputArgs, OutputArgs2,
                FromToInsts, AssignGoals, CommonStruct0, CommonStruct, !Info),
            ( if AssignGoals = [hlds_goal(OnlyGoalExpr, _OnlyGoalInfo)] then
                AssignsGoalExpr = OnlyGoalExpr
            else
                AssignsGoalExpr = conj(plain_conj, AssignGoals)
            ),
            MaybeAssignsGoalExpr = yes(AssignsGoalExpr),
            simplify_info_get_var_types(!.Info, VarTypes),
            ( if
                simplify_do_warn_duplicate_calls(!.Info),
                % Don't warn for cases such as:
                % set.init(Set1 : set(int)),
                % set.init(Set2 : set(float)).
                lookup_var_types(VarTypes, OutputArgs, OutputArgTypes1),
                lookup_var_types(VarTypes, OutputArgs2, OutputArgTypes2),
                types_match_exactly_list(OutputArgTypes1, OutputArgTypes2)
            then
                Context = goal_info_get_context(GoalInfo),
                CallPieces = det_report_seen_call_id(ModuleInfo, SeenCall),
                CurPieces = [words("Warning: redundant") | CallPieces]
                    ++ [suffix(".")],
                PrevPieces = [words("Here is the previous") | CallPieces]
                    ++ [suffix(".")],
                Msg = simplest_msg(Context, CurPieces),
                PrevMsg = error_msg(yes(PrevContext), treat_as_first, 0,
                    [always(PrevPieces)]),
                Spec = conditional_spec($pred, warn_duplicate_calls, yes,
                    severity_warning, phase_simplify(report_in_any_mode),
                    [Msg, PrevMsg]),
                simplify_info_add_message(Spec, !Info)
            else
                true
            ),
            goal_cost(hlds_goal(GoalExpr0, GoalInfo), Cost),
            simplify_info_incr_cost_delta(Cost, !Info),
            simplify_info_set_rerun_quant_instmap_delta(!Info),
            Detism0 = goal_info_get_determinism(GoalInfo),
            (
                Detism0 = detism_det
            ;
                ( Detism0 = detism_semi
                ; Detism0 = detism_non
                ; Detism0 = detism_multi
                ; Detism0 = detism_failure
                ; Detism0 = detism_erroneous
                ; Detism0 = detism_cc_non
                ; Detism0 = detism_cc_multi
                ),
                simplify_info_set_rerun_det(!Info)
            )
        else
            Context = goal_info_get_context(GoalInfo),
            ThisCall = call_args(Context, InputArgs, OutputArgs),
            map.det_update(SeenCall, [ThisCall | SeenCallsList0],
                SeenCalls0, SeenCalls),
            CommonStruct = CommonStruct0 ^ seen_calls := SeenCalls,
            MaybeAssignsGoalExpr = no
        )
    else
        Context = goal_info_get_context(GoalInfo),
        ThisCall = call_args(Context, InputArgs, OutputArgs),
        map.det_insert(SeenCall, [ThisCall], SeenCalls0, SeenCalls),
        CommonStruct = CommonStruct0 ^ seen_calls := SeenCalls,
        MaybeAssignsGoalExpr = no
    ).

%---------------------------------------------------------------------------%

    % Partition the arguments of a call into inputs and outputs,
    % failing if any of the outputs have a unique component
    % or if any of the outputs contain any `any' insts.
    %
:- pred partition_call_args(vartypes::in, module_info::in,
    list(mer_mode)::in, list(prog_var)::in, list(prog_var)::out,
    list(prog_var)::out, list(mer_mode)::out) is semidet.

partition_call_args(_, _, [], [], [], [], []).
partition_call_args(_, _, [], [_ | _], _, _, _) :-
    unexpected($pred, "length mismatch (1)").
partition_call_args(_, _, [_ | _], [], _, _, _) :-
    unexpected($pred, "length mismatch (2)").
partition_call_args(VarTypes, ModuleInfo, [ArgMode | ArgModes],
        [Arg | Args], InputArgs, OutputArgs, OutputModes) :-
    partition_call_args(VarTypes, ModuleInfo, ArgModes, Args,
        InputArgs1, OutputArgs1, OutputModes1),
    mode_get_insts(ModuleInfo, ArgMode, InitialInst, FinalInst),
    lookup_var_type(VarTypes, Arg, Type),
    ( if inst_matches_binding(ModuleInfo, Type, InitialInst, FinalInst) then
        InputArgs = [Arg | InputArgs1],
        OutputArgs = OutputArgs1,
        OutputModes = OutputModes1
    else
        % Calls with partly unique outputs cannot be replaced,
        % since a unique copy of the outputs must be produced.
        inst_is_not_partly_unique(ModuleInfo, FinalInst),

        % Don't optimize calls whose outputs include any `any' insts, since
        % that would create false aliasing between the different variables.
        % (inst_matches_binding applied to identical insts fails only for
        % `any' insts.)
        inst_matches_binding(ModuleInfo, Type, FinalInst, FinalInst),

        % Don't optimize calls where a partially instantiated variable is
        % further instantiated. That case is difficult to test properly
        % because mode analysis currently rejects most potential test cases.
        inst_is_free(ModuleInfo, InitialInst),

        InputArgs = InputArgs1,
        OutputArgs = [Arg | OutputArgs1],
        OutputModes = [ArgMode | OutputModes1]
    ).

%---------------------------------------------------------------------------%

:- pred find_previous_call(list(call_args)::in, list(prog_var)::in,
    eqvclass(prog_var)::in, list(prog_var)::out,
    prog_context::out) is semidet.

find_previous_call([SeenCall | SeenCalls], InputArgs, Eqv, OutputArgs,
        PrevContext) :-
    SeenCall = call_args(PrevContext, InputArgs1, OutputArgs1),
    ( if common_var_lists_are_equiv(Eqv, InputArgs, InputArgs1) then
        OutputArgs = OutputArgs1
    else
        find_previous_call(SeenCalls, InputArgs, Eqv, OutputArgs, PrevContext)
    ).

%---------------------------------------------------------------------------%

common_vars_are_equivalent(Common, Xs, Ys) :-
    Common = common_info(MaybeCommonStruct, _ConstStruct),
    (
        MaybeCommonStruct = no,
        Xs = Ys
    ;
        MaybeCommonStruct = yes(CommonStruct),
        EqvVars = CommonStruct ^ var_eqv,
        common_vars_are_equiv(EqvVars, Xs, Ys)
    ).

    % Succeeds if the two lists of variables are equivalent
    % according to the specified equivalence class.
    %
:- pred common_var_lists_are_equiv(eqvclass(prog_var)::in,
    list(prog_var)::in, list(prog_var)::in) is semidet.

common_var_lists_are_equiv(_VarEqv, [], []).
common_var_lists_are_equiv(VarEqv, [X | Xs], [Y | Ys]) :-
    common_vars_are_equiv(VarEqv, X, Y),
    common_var_lists_are_equiv(VarEqv, Xs, Ys).

    % Succeeds if the two variables are equivalent according to the
    % specified equivalence class.
    %
:- pred common_vars_are_equiv(eqvclass(prog_var)::in,
    prog_var::in, prog_var::in) is semidet.

common_vars_are_equiv(VarEqv, X, Y) :-
    (
        X = Y
    ;
        eqvclass.partition_id(VarEqv, X, Id),
        eqvclass.partition_id(VarEqv, Y, Id)
    ).

%---------------------------------------------------------------------------%

    % Create unifications to assign the vars in OutputArgs from the
    % corresponding var in OldOutputArgs. This needs to be done even if
    % OutputArg is not a nonlocal in the original goal, because later goals
    % in the conjunction may match against the cell and need all the output
    % arguments. Any unneeded assignments will be removed later.
    %
:- pred create_output_unifications(hlds_goal_info::in, list(prog_var)::in,
    list(prog_var)::in, list(from_to_insts)::in, list(hlds_goal)::out,
    common_struct_info::in, common_struct_info::out,
    simplify_info::in, simplify_info::out) is det.

create_output_unifications(OldGoalInfo, OutputArgs, OldOutputArgs, FromToInsts,
        AssignGoals, !CommonStruct, !Info) :-
    ( if
        OutputArgs = [HeadOutputArg | TailOutputArgs],
        OldOutputArgs = [HeadOldOutputArg | TailOldOutputArgs],
        FromToInsts = [HeadFromToInsts | TailFromToInsts]
    then
        ( if HeadOutputArg = HeadOldOutputArg then
            % This can happen if the first cell was created
            % with a partially instantiated deconstruction.
            create_output_unifications(OldGoalInfo,
                TailOutputArgs, TailOldOutputArgs, TailFromToInsts,
                AssignGoals, !CommonStruct, !Info)
        else
            generate_assign(HeadOutputArg, HeadOldOutputArg, HeadFromToInsts,
                OldGoalInfo, HeadAssignGoalExpr, HeadAssignGoalInfo,
                !CommonStruct, !Info),
            HeadAssignGoal = hlds_goal(HeadAssignGoalExpr, HeadAssignGoalInfo),
            create_output_unifications(OldGoalInfo,
                TailOutputArgs, TailOldOutputArgs, TailFromToInsts,
                TailAssignGoals, !CommonStruct, !Info),
            AssignGoals = [HeadAssignGoal | TailAssignGoals]
        )
    else if
        OutputArgs = [],
        OldOutputArgs = [],
        FromToInsts = []
    then
        AssignGoals = []
    else
        unexpected($pred, "mode mismatch")
    ).

%---------------------------------------------------------------------------%

:- pred generate_assign_from_const_struct(
    unification::in(unification_construct), unify_mode::in,
    unify_context::in,
    list(const_struct_arg)::in,
    hlds_goal_info::in, hlds_goal_expr::out, hlds_goal_info::out,
    const_var_map::in, const_var_map::out,
    simplify_info::in, simplify_info::out) is det.

generate_assign_from_const_struct(Unification0, UnifyMode0, UnifyContext0,
        CSAs, OldGoalInfo, ConstGoalExpr, ConstGoalInfo,
        VarMap0, VarMap, !Info) :-
    Unification0 =
        construct(Var, ConsId, _ArgVars, _ArgModes, _How, _Uniq, SubInfo),

    simplify_info_get_var_types(!.Info, VarTypes),
    lookup_var_type(VarTypes, Var, Type),
    UnifyMode0 = unify_modes_li_lf_ri_rf(ToVarInit, ToVarFinal,
        _FromTermInit, _FromTermFinal),
    simplify_info_get_module_info(!.Info, ModuleInfo0),
    simplify_info_get_pred_proc_id(!.Info, proc(PredId, _ProcId)),
    module_info_pred_info(ModuleInfo0, PredId, PredInfo),
    pred_info_get_status(PredInfo, PredStatus),
    DefnThisModule = pred_status_defined_in_this_module(PredStatus),
    ( DefnThisModule = no,  Where = defined_in_other_module
    ; DefnThisModule = yes, Where = defined_in_this_module
    ),
    Struct = const_struct(ConsId, CSAs, Type, ToVarFinal, Where),
    module_info_get_const_struct_db(ModuleInfo0, ConstStructDb0),
    lookup_insert_const_struct(Struct, ConstNum,
        ConstStructDb0, ConstStructDb),
    module_info_set_const_struct_db(ConstStructDb, ModuleInfo0, ModuleInfo),
    simplify_info_set_module_info(ModuleInfo, !Info),
    map.det_insert(Var, csa_const_struct(ConstNum), VarMap0, VarMap),

    ConstConsId = ground_term_const(ConstNum, ConsId),
    ConstRHS = rhs_functor(ConstConsId, is_not_exist_constr, []),
    ConstUnifyMode = unify_modes_li_lf_ri_rf(ToVarInit, ToVarFinal,
        ToVarFinal, ToVarFinal),
    % The how_to_construct field is not meaningful for construction
    % unifications without arguments, and the ConstUnification we are building
    % has no arguments.
    ConstHow = construct_dynamically,
    ConstUniq = cell_is_shared,
    ConstUnification =
        construct(Var, ConstConsId, [], [], ConstHow, ConstUniq, SubInfo),
    ConstGoalExpr = unify(Var, ConstRHS, ConstUnifyMode,
        ConstUnification, UnifyContext0),

    set_of_var.make_singleton(Var, NonLocals),
    InstMapDelta = instmap_delta_from_assoc_list([Var - ToVarFinal]),
    Context = goal_info_get_context(OldGoalInfo),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        ConstGoalInfo).

%---------------------------------------------------------------------------%

:- pred generate_assign(prog_var::in, prog_var::in, from_to_insts::in,
    hlds_goal_info::in, hlds_goal_expr::out, hlds_goal_info::out,
    common_struct_info::in, common_struct_info::out,
    simplify_info::in, simplify_info::out) is det.

generate_assign(ToVar, FromVar, ToVarMode, OldGoalInfo, GoalExpr, GoalInfo,
        !CommonStruct, !Info) :-
    apply_induced_substitutions(ToVar, FromVar, !Info),
    simplify_info_get_var_types(!.Info, VarTypes),
    lookup_var_type(VarTypes, ToVar, ToVarType),
    lookup_var_type(VarTypes, FromVar, FromVarType),

    set_of_var.list_to_set([ToVar, FromVar], NonLocals),
    ToVarMode = from_to_insts(ToVarInit, ToVarFinal),
    ( if types_match_exactly(ToVarType, FromVarType) then
        UnifyMode = unify_modes_li_lf_ri_rf(ToVarInit, ToVarFinal,
            ToVarFinal, ToVarFinal),
        UnifyContext = unify_context(umc_explicit, []),
        GoalExpr = unify(ToVar, rhs_var(FromVar), UnifyMode,
            assign(ToVar, FromVar), UnifyContext)
    else
        % If the cells we are optimizing don't have exactly the same type,
        % we insert explicit type casts to ensure type correctness.
        % This avoids problems with HLDS optimizations such as inlining
        % which expect the HLDS to be well-typed. Unfortunately, this loses
        % information for other optimizations, since the cast hides the
        % equivalence of the input and output.
        Modes =
            [from_to_mode(ToVarFinal, ToVarFinal),
            from_to_mode(free, ToVarFinal)],
        GoalExpr = generic_call(cast(unsafe_type_cast), [FromVar, ToVar],
            Modes, arg_reg_types_unset, detism_det)
    ),

    % `ToVar' may not appear in the original instmap_delta, so we can't just
    % use instmap_delta_restrict on the original instmap_delta here.
    InstMapDelta = instmap_delta_from_assoc_list([ToVar - ToVarFinal]),
    Context = goal_info_get_context(OldGoalInfo),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure, Context,
        GoalInfo),

    record_equivalence(ToVar, FromVar, !CommonStruct).

:- pred types_match_exactly(mer_type::in, mer_type::in) is semidet.

types_match_exactly(TypeA, TypeB) :-
    require_complete_switch [TypeA]
    (
        TypeA = type_variable(TVar, _),
        TypeB = type_variable(TVar, _)
    ;
        TypeA = defined_type(Name, ArgTypesA, _),
        TypeB = defined_type(Name, ArgTypesB, _),
        types_match_exactly_list(ArgTypesA, ArgTypesB)
    ;
        TypeA = builtin_type(BuiltinType),
        TypeB = builtin_type(BuiltinType)
    ;
        TypeA = higher_order_type(PorF, ArgTypesA, H, P, E),
        TypeB = higher_order_type(PorF, ArgTypesB, H, P, E),
        types_match_exactly_list(ArgTypesA, ArgTypesB)
    ;
        TypeA = tuple_type(ArgTypesA, _),
        TypeB = tuple_type(ArgTypesB, _),
        types_match_exactly_list(ArgTypesA, ArgTypesB)
    ;
        TypeA = apply_n_type(TVar, ArgTypesA, _),
        TypeB = apply_n_type(TVar, ArgTypesB, _),
        types_match_exactly_list(ArgTypesA, ArgTypesB)
    ;
        TypeA = kinded_type(_, _),
        unexpected($pred, "kind annotation")
    ).

:- pred types_match_exactly_list(list(mer_type)::in, list(mer_type)::in)
    is semidet.

types_match_exactly_list([], []).
types_match_exactly_list([TypeA | TypesA], [TypeB | TypesB]) :-
    types_match_exactly(TypeA, TypeB),
    types_match_exactly_list(TypesA, TypesB).

%---------------------------------------------------------------------------%

    % Two existentially quantified type variables may become aliased if two
    % calls or two deconstructions are merged together. We detect this
    % situation here and apply the appropriate tsubst to the vartypes and
    % rtti_varmaps. This allows us to avoid an unsafe cast, and also may
    % allow more opportunities for simplification.
    %
    % If we do need to apply a type substitution, then we also apply the
    % substitution ToVar -> FromVar to the RttiVarMaps, then duplicate
    % FromVar's information for ToVar. This ensures we always refer to the
    % "original" variables, not the copies created by generate_assign.
    %
    % Note that this relies on the assignments for type_infos and
    % typeclass_infos to be generated before other arguments with these
    % existential types are processed. In other words, the arguments of
    % calls and deconstructions must be processed in left to right order.
    %
:- pred apply_induced_substitutions(prog_var::in, prog_var::in,
    simplify_info::in, simplify_info::out) is det.

apply_induced_substitutions(ToVar, FromVar, !Info) :-
    simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
    rtti_varmaps_var_info(RttiVarMaps0, FromVar, FromVarRttiInfo),
    rtti_varmaps_var_info(RttiVarMaps0, ToVar, ToVarRttiInfo),
    ( if calculate_induced_tsubst(ToVarRttiInfo, FromVarRttiInfo, TSubst) then
        ( if map.is_empty(TSubst) then
            true
        else
            simplify_info_apply_substitutions_and_duplicate(ToVar, FromVar,
                TSubst, !Info)
        )
    else
        % Update the rtti_varmaps with new information if only one of the
        % variables has rtti_var_info recorded. This can happen if a new
        % variable has been introduced, eg in quantification, without
        % being recorded in the rtti_varmaps.
        (
            FromVarRttiInfo = non_rtti_var,
            rtti_var_info_duplicate(ToVar, FromVar,
                RttiVarMaps0, RttiVarMaps),
            simplify_info_set_rtti_varmaps(RttiVarMaps, !Info)
        ;
            ( FromVarRttiInfo = type_info_var(_)
            ; FromVarRttiInfo = typeclass_info_var(_)
            ),
            (
                ToVarRttiInfo = non_rtti_var,
                rtti_var_info_duplicate(FromVar, ToVar,
                    RttiVarMaps0, RttiVarMaps),
                simplify_info_set_rtti_varmaps(RttiVarMaps, !Info)
            ;
                ( ToVarRttiInfo = type_info_var(_)
                ; ToVarRttiInfo = typeclass_info_var(_)
                ),
                % Calculate_induced_tsubst failed for a different reason,
                % either because unification failed or because one variable
                % was a type_info and the other was a typeclass_info.
                unexpected($pred, "inconsistent info")
            )
        )
    ).

    % Calculate the induced substitution by unifying the types or constraints,
    % if they exist. Fail if given non-matching rtti_var_infos.
    %
:- pred calculate_induced_tsubst(rtti_var_info::in, rtti_var_info::in,
    tsubst::out) is semidet.

calculate_induced_tsubst(ToVarRttiInfo, FromVarRttiInfo, TSubst) :-
    (
        FromVarRttiInfo = type_info_var(FromVarTypeInfoType),
        ToVarRttiInfo = type_info_var(ToVarTypeInfoType),
        type_subsumes(ToVarTypeInfoType, FromVarTypeInfoType, TSubst)
    ;
        FromVarRttiInfo = typeclass_info_var(FromVarConstraint),
        ToVarRttiInfo = typeclass_info_var(ToVarConstraint),
        FromVarConstraint = constraint(Name, FromArgs),
        ToVarConstraint = constraint(Name, ToArgs),
        type_list_subsumes(ToArgs, FromArgs, TSubst)
    ;
        FromVarRttiInfo = non_rtti_var,
        ToVarRttiInfo = non_rtti_var,
        map.init(TSubst)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.common.
%---------------------------------------------------------------------------%
