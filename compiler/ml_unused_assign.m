%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_unused_assign.m.
%
% This module optimizes away assignments in MLDS code if the variable
% they assign to is not used.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_unused_assign.
:- interface.

:- import_module ml_backend.mlds.

:- import_module list.
:- import_module map.
:- import_module set.

    % The set of the local variables we have seen occur
    % after a given program point.
    %
:- type seen_set == set(mlds_local_var_name).

    % Maps each label we have seen so far in our backwards traversal
    % to the set of local variables we have seen occur after that label.
    %
:- type seen_at_label_map == map(mlds_label, seen_set).

    % optimize_away_unused_assigns_in_proc_body(ParamLocalVars,
    %   SeenAtLabelMap, !LocalVarDefns, !FuncDefns, !Stmts):
    %
    % Given the MLDS translation of HLDS procedure body in
    % !.LocalVarDefns, !.FuncDefns and !.Stmts, optimize away any
    % assignment statements whose target is a local variable that is
    % not used before its next unambiguous definition.
    %
    % For procedures that are inside a TSCC, the code in !.Stmts can perform
    %
    % - backward jumps to labels in !.Stmts, and
    % - jumps to labels that are not inside !.Stmts.
    %
    % (This is the *only* situation in which MLDS code should *ever* perform
    % a goto to an earlier label.)
    %
    % The optimization needs to know what local variables are needed
    % at the labels that are targets of such jumps; the caller should
    % specify this in SeenAtLabelMap.
    %
:- pred optimize_away_unused_assigns_in_proc_body(
    list(mlds_local_var_name)::in, seen_at_label_map::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module ml_backend.ml_util.

:- import_module cord.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

:- type original_or_optimized_code
    --->    may_use_optimized_code
    ;       must_use_original_code.

:- type ua_info
    --->    ua_info(
                % The seen_set at each label that we know about.
                uai_seen_at_label_map       :: seen_at_label_map,

                % When inside a switch, this should specify the seen_set
                % immediately after the switch. Otherwise, it should be `no'.
                uai_seen_at_break_switch    :: maybe(seen_set),

                % When inside a loop, this should specify the seen_set
                % immediately after the loop. Otherwise, it should be `no'.
                uai_seen_at_break_loop      :: maybe(seen_set),

                % When inside a loop, this should specify (a superset of)
                % the seen_set immediately before the loop. Otherwise,
                % it should be `no'.
                uai_seen_at_continue_loop   :: maybe(seen_set),

                % Have we found a situation in which the information we needed
                % to make the correct decision was not available? If yes,
                % this should be `must_use_original_code'; otherwise,
                % it should be `may_use_optimized_code'.
                uai_orig_opt_code           :: original_or_optimized_code
            ).

%---------------------------------------------------------------------------%

optimize_away_unused_assigns_in_proc_body(ParamLocalVars, SeenAtLabelMap0,
        LocalVarDefns0, LocalVarDefns, FuncDefns0, FuncDefns, Stmts0, Stmts) :-
    % Really, what we want as SeenAfter are the *output* variables.
    % Hovever, including the input and ignored arguments has no effect
    % (XXX except for performance), because those local variables will
    % never be assigned to.
    set.list_to_set(ParamLocalVars, SeenAfter),

    % The procedure body is not inside a switch. Non-tail-recursive procedure
    % bodies are not inside a loop either, but it is harmless to set
    % SeenAtBreakLoop0 and SeenAtContinueLoop0 for them. Tail-recursive
    % procedures are not in a loop *yet*, but they *will* be put into
    % a loop by our caller after we are done here, and Stmts0 *will* contain
    % continue statements to implement self-tail-calls, so for them, we *have*
    % to set SeenAtBreakLoop0 and SeenAtContinueLoop0 to the parameter list.
    SeenAtBreakSwitch0 = no,
    SeenAtBreakLoop0 = yes(SeenAfter),
    SeenAtContinueLoop0 = yes(SeenAfter),
    Info0 = ua_info(SeenAtLabelMap0, SeenAtBreakSwitch0, SeenAtBreakLoop0,
        SeenAtContinueLoop0, may_use_optimized_code),

    delete_unused_in_stmts(Stmts0, Stmts1, SeenAfter, SeenBefore0,
        Info0, Info),
    delete_unused_in_func_defns(FuncDefns0, FuncDefns1,
        SeenBefore0, SeenBefore),
    OrigOrOpt = Info ^ uai_orig_opt_code,
    ( if
        OrigOrOpt = may_use_optimized_code,
        FuncDefns0 = []
    then
        keep_only_seen_local_var_defns(SeenBefore, _,
            LocalVarDefns0, LocalVarDefns),
        FuncDefns = FuncDefns1,
        Stmts = Stmts1
    else
        LocalVarDefns = LocalVarDefns0,
        FuncDefns = FuncDefns0,
        Stmts = Stmts0
    ).

:- pred delete_unused_in_stmts(list(mlds_stmt)::in, list(mlds_stmt)::out,
    seen_set::in, seen_set::out, ua_info::in, ua_info::out) is det.

delete_unused_in_stmts(Stmts0, Stmts, SeenAfter, SeenBefore, !Info) :-
    list.reverse(Stmts0, RevStmts0),
    delete_unused_in_rev_stmts(RevStmts0, RevStmtsCord, SeenAfter, SeenBefore,
        !Info),
    Stmts = cord.rev_cord_list_to_list(RevStmtsCord).

:- pred delete_unused_in_rev_stmts(
    list(mlds_stmt)::in, list(cord(mlds_stmt))::out,
    seen_set::in, seen_set::out, ua_info::in, ua_info::out) is det.

delete_unused_in_rev_stmts([], [], SeenAfter, SeenBefore, !Info) :-
    SeenAfter = SeenBefore.
delete_unused_in_rev_stmts([RevStmt0 | RevStmts0],
        [RevStmtCord | RevStmtsCord], SeenAfter, SeenBefore, !Info) :-
    delete_unused_in_stmt_return_cord(RevStmt0, RevStmtCord,
        SeenAfter, SeenBetween, !Info),
    delete_unused_in_rev_stmts(RevStmts0, RevStmtsCord,
        SeenBetween, SeenBefore, !Info).

:- pred delete_unused_in_stmt(mlds_stmt::in, mlds_stmt::out,
    seen_set::in, seen_set::out, ua_info::in, ua_info::out) is det.

delete_unused_in_stmt(Stmt0, Stmt, SeenAfter, SeenBefore, !Info) :-
    delete_unused_in_stmt_return_cord(Stmt0, StmtCord, SeenAfter, SeenBefore,
        !Info),
    stmt_cord_to_stmt(Stmt0, StmtCord, Stmt).

:- pred delete_unused_in_stmt_return_cord(mlds_stmt::in, cord(mlds_stmt)::out,
    seen_set::in, seen_set::out, ua_info::in, ua_info::out) is det.

delete_unused_in_stmt_return_cord(Stmt0, StmtCord, SeenAfter, SeenBefore,
        !Info) :-
    (
        Stmt0 = ml_stmt_block(LocalVarDefns0, FuncDefns0, BlockStmts0, Ctxt),
        delete_unused_in_stmts(BlockStmts0, BlockStmts, SeenAfter, SeenBefore0,
            !Info),
        (
            FuncDefns0 = [_ | _],
            % XXX ARG_PACK We don't yet handle nested functions correctly.
            % delete_unused_in_func_defns(FuncDefns0, FuncDefns,
            %     SeenBefore0, SeenBefore1),
            !Info ^ uai_orig_opt_code := must_use_original_code
        ;
            FuncDefns0 = []
        ),
        FuncDefns = FuncDefns0,
        keep_only_seen_local_var_defns(SeenBefore0, SeenBefore,
            LocalVarDefns0, LocalVarDefns),
        ( if
            LocalVarDefns = [],
            FuncDefns = []
        then
            StmtCord = cord.from_list(BlockStmts)
        else
            Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, BlockStmts, Ctxt),
            StmtCord = cord.singleton(Stmt)
        )
    ;
        Stmt0 = ml_stmt_while(LoopKind, CondRval, BodyStmt0, LoopLocalVars,
            Ctxt),
        set.insert_list(LoopLocalVars, SeenAfter, SeenAfterBody),
        % Override the seen_at_{break,continue}_loop fields of !Info
        % while we are inside this loop (which may be inside an outer loop).
        SeenAtBreakLoop0 = !.Info ^ uai_seen_at_break_loop,
        SeenAtContinueLoop0 = !.Info ^ uai_seen_at_continue_loop,
        !Info ^ uai_seen_at_break_loop := yes(SeenAfter),
        !Info ^ uai_seen_at_continue_loop := yes(SeenAfterBody),
        delete_unused_in_stmt(BodyStmt0, BodyStmt,
            SeenAfterBody, SeenBeforeBody, !Info),
        see_in_rval(CondRval, SeenBeforeBody, SeenBefore),
        !Info ^ uai_seen_at_break_loop := SeenAtBreakLoop0,
        !Info ^ uai_seen_at_continue_loop := SeenAtContinueLoop0,
        Stmt = ml_stmt_while(LoopKind, CondRval, BodyStmt, LoopLocalVars,
            Ctxt),
        StmtCord = cord.singleton(Stmt)
    ;
        Stmt0 = ml_stmt_if_then_else(CondRval, ThenStmt0, MaybeElseStmt0,
            Ctxt),
        delete_unused_in_stmt(ThenStmt0, ThenStmt, SeenAfter, SeenBeforeThen,
            !Info),
        (
            MaybeElseStmt0 = no,
            MaybeElseStmt = no,
            SeenBeforeThenElse = SeenBeforeThen
        ;
            MaybeElseStmt0 = yes(ElseStmt0),
            delete_unused_in_stmt(ElseStmt0, ElseStmt,
                SeenAfter, SeenBeforeElse, !Info),
            ( if ElseStmt = ml_stmt_block([], [], [], _) then
                MaybeElseStmt = no
            else
                MaybeElseStmt = yes(ElseStmt)
            ),
            set.union(SeenBeforeThen, SeenBeforeElse, SeenBeforeThenElse)
        ),
        see_in_rval(CondRval, SeenBeforeThenElse, SeenBefore),
        Stmt = ml_stmt_if_then_else(CondRval, ThenStmt, MaybeElseStmt, Ctxt),
        StmtCord = cord.singleton(Stmt)
    ;
        Stmt0 = ml_stmt_switch(SwitchValueType, SwitchRval, Range, Cases0,
            Default0, Ctxt),
        % Override the seen_at_break_switch field of !Info while we are
        % inside this switch (which may be inside an outer switch).
        SeenAtBreakSwitch0 = !.Info ^ uai_seen_at_break_switch,
        !Info ^ uai_seen_at_break_switch := yes(SeenAfter),
        delete_unused_in_switch_default(Default0, Default,
            SeenAfter, SeenBeforeDefault, !Info),
        delete_unused_in_switch_cases(Cases0, Cases,
            SeenAfter, SeenBeforeDefault, SeenBeforeDefaultCases, !Info),
        !Info ^ uai_seen_at_break_switch := SeenAtBreakSwitch0,
        see_in_rval(SwitchRval, SeenBeforeDefaultCases, SeenBefore),
        Stmt = ml_stmt_switch(SwitchValueType, SwitchRval, Range, Cases,
            Default, Ctxt),
        StmtCord = cord.singleton(Stmt)
    ;
        Stmt0 = ml_stmt_label(Label, _Ctxt),
        SeenAtLabelMap0 = !.Info ^ uai_seen_at_label_map,
        map.det_insert(Label, SeenAfter, SeenAtLabelMap0, SeenAtLabelMap),
        !Info ^ uai_seen_at_label_map := SeenAtLabelMap,
        StmtCord = cord.singleton(Stmt0),
        SeenBefore = SeenAfter
    ;
        Stmt0 = ml_stmt_goto(Target, _Ctxt),
        StmtCord = cord.singleton(Stmt0),
        (
            Target = goto_label(Label),
            get_seen_set_at_label(Label, SeenBefore, !Info)
        ;
            Target = goto_break_switch,
            SeenAtBreakSwitch = !.Info ^ uai_seen_at_break_switch,
            (
                SeenAtBreakSwitch = yes(SeenBefore)
            ;
                SeenAtBreakSwitch = no,
                % A goto_break_switch target should occur only inside a switch,
                % and inside a switch SeenAtBreakSwitch should always be `yes'.
                unexpected($pred, "SeenAtBreakSwitch = no")
            )
        ;
            Target = goto_break_loop,
            SeenAtBreakLoop = !.Info ^ uai_seen_at_break_loop,
            (
                SeenAtBreakLoop = yes(SeenBefore)
            ;
                SeenAtBreakLoop = no,
                % A goto_break_loop target should occur only inside a loop,
                % and inside a loop SeenAtBreakLoop should always be `yes'.
                unexpected($pred, "SeenAtBreakLoop = no")
            )
        ;
            Target = goto_continue_loop,
            SeenAtContinueLoop = !.Info ^ uai_seen_at_continue_loop,
            (
                SeenAtContinueLoop = yes(SeenBefore)
            ;
                SeenAtContinueLoop = no,
                % A goto_continue_loop target should occur only inside a loop,
                % and inside a loop SeenAtContinueLoop should always be `yes'.
                unexpected($pred, "SeenAtContinueLoop = no")
            )
        )
    ;
        Stmt0 = ml_stmt_computed_goto(Rval, Labels, _Ctxt),
        StmtCord = cord.singleton(Stmt0),
        accumulate_label_seen_sets(Labels, set.init, SeenBefore0, !Info),
        see_in_rval(Rval, SeenBefore0, SeenBefore)
    ;
        Stmt0 = ml_stmt_call(_Signature, FuncRval, ArgRvals, ReturnValueLvals,
            _CallKind, _Ctxt),
        StmtCord = cord.singleton(Stmt0),
        list.foldl(see_in_lval, ReturnValueLvals, SeenAfter, SeenBefore0),
        see_in_rval(FuncRval, SeenBefore0, SeenBefore1),
        list.foldl(see_in_rval, ArgRvals, SeenBefore1, SeenBefore)
    ;
        Stmt0 = ml_stmt_return(ReturnValueRvals, _Ctxt),
        StmtCord = cord.singleton(Stmt0),
        % The current value of SeenAfter does not matter.
        list.foldl(see_in_rval, ReturnValueRvals, set.init, SeenBefore)
    ;
        Stmt0 = ml_stmt_try_commit(RefLval, GoalStmt0, CommitStmt0, Ctxt),
        delete_unused_in_stmt(GoalStmt0, GoalStmt,
            SeenAfter, SeenBeforeGoal, !Info),
        delete_unused_in_stmt(CommitStmt0, CommitStmt,
            SeenAfter, SeenBeforeCommit, !Info),
        Stmt = ml_stmt_try_commit(RefLval, GoalStmt, CommitStmt, Ctxt),
        StmtCord = cord.singleton(Stmt),
        set.union(SeenBeforeGoal, SeenBeforeCommit, SeenBeforeGoalCommit),
        see_in_lval(RefLval, SeenBeforeGoalCommit, SeenBefore)
    ;
        Stmt0 = ml_stmt_do_commit(RefRval, _Ctxt),
        StmtCord = cord.singleton(Stmt0),
        see_in_rval(RefRval, SeenAfter, SeenBefore)
    ;
        Stmt0 = ml_stmt_atomic(_AtomicStmt0, _Ctxt),
        delete_unused_in_atomic_stmt_return_cord(Stmt0, StmtCord,
            SeenAfter, SeenBefore)
    ).

:- pred delete_unused_in_switch_default(
    mlds_switch_default::in, mlds_switch_default::out,
    seen_set::in, seen_set::out, ua_info::in, ua_info::out) is det.

delete_unused_in_switch_default(Default0, Default, SeenAfter, SeenBefore,
        !Info) :-
    (
        ( Default0 = default_is_unreachable
        ; Default0 = default_do_nothing
        ),
        Default = Default0,
        SeenBefore = SeenAfter
    ;
        Default0 = default_case(Stmt0),
        delete_unused_in_stmt(Stmt0, Stmt, SeenAfter, SeenBefore, !Info),
        Default = default_case(Stmt)
    ).

:- pred delete_unused_in_switch_cases(
    list(mlds_switch_case)::in, list(mlds_switch_case)::out, seen_set::in,
    seen_set::in, seen_set::out, ua_info::in, ua_info::out) is det.

delete_unused_in_switch_cases([], [], _SeenAfter, !SeenBefore, !Info).
delete_unused_in_switch_cases([Case0 | Cases0], [Case | Cases],
        SeenAfter, !SeenBefore, !Info) :-
    Case0 = mlds_switch_case(FirstMatchCond, LaterMatchConds, Stmt0),
    see_in_match_cond(FirstMatchCond, set.init, SeenInMatch0),
    list.foldl(see_in_match_cond, LaterMatchConds, SeenInMatch0, SeenInMatch),
    expect(set.is_empty(SeenInMatch), $pred,
        "a variable occurs in a supposedly-constant match condition"),

    delete_unused_in_stmt(Stmt0, Stmt, SeenAfter, SeenBeforeStmt, !Info),
    Case = mlds_switch_case(FirstMatchCond, LaterMatchConds, Stmt),

    set.union(SeenBeforeStmt, !SeenBefore),
    delete_unused_in_switch_cases(Cases0, Cases,
        SeenAfter, !SeenBefore, !Info).

:- pred delete_unused_in_atomic_stmt_return_cord(
    mlds_stmt::in(ml_stmt_is_atomic), cord(mlds_stmt)::out,
    seen_set::in, seen_set::out) is det.

delete_unused_in_atomic_stmt_return_cord(Stmt0, StmtCord,
        SeenAfter, SeenBefore) :-
    Stmt0 = ml_stmt_atomic(AtomicStmt0, _Ctxt),
    (
        ( AtomicStmt0 = comment(_Comment)
        ; AtomicStmt0 = gc_check
        ; AtomicStmt0 = trail_op(_TrailOp)
        ),
        StmtCord = cord.singleton(Stmt0),
        SeenAfter = SeenBefore
    ;
        ( AtomicStmt0 = assign(TargetLval, SourceRval)
        ; AtomicStmt0 = assign_if_in_heap(TargetLval, SourceRval)
        ),
        ( if
            TargetLval = ml_local_var(TargetLocalVarName, _Type),
            not set.member(TargetLocalVarName, SeenAfter),
            (
                ( TargetLocalVarName = lvn_prog_var(_, _)
                ; TargetLocalVarName = lvn_prog_var_boxed(_, _)
                ; TargetLocalVarName = lvn_prog_var_conv(_, _, _)
                )
            ;
                TargetLocalVarName = lvn_comp_var(CompVar),
                CompVar = lvnc_packed_word(_)
            )
        then
            % We are deleting this statement, because the value it defines
            % (TargetLocalVarName) is unused.
            StmtCord = cord.empty,
            SeenBefore = SeenAfter
        else
            StmtCord = cord.singleton(Stmt0),
            see_in_lval(TargetLval, SeenAfter, SeenBefore0),
            see_in_rval(SourceRval, SeenBefore0, SeenBefore)
        )
    ;
        AtomicStmt0 = delete_object(Rval),
        StmtCord = cord.singleton(Stmt0),
        see_in_rval(Rval, SeenAfter, SeenBefore)
    ;
        AtomicStmt0 = new_object(TargetLval, _PTag, _HasSecTag, _Type,
            MaybeNumWordsRval, _MaybeCtorId, ArgRvals, _MayUseAtomicAlloc,
            _MaybeAllocId),
        StmtCord = cord.singleton(Stmt0),
        see_in_lval(TargetLval, SeenAfter, SeenBefore0),
        (
            MaybeNumWordsRval = no,
            SeenBefore1 = SeenBefore0
        ;
            MaybeNumWordsRval = yes(NumWordsRval),
            see_in_rval(NumWordsRval, SeenBefore0, SeenBefore1)
        ),
        list.foldl(see_in_typed_rval, ArgRvals, SeenBefore1, SeenBefore)
    ;
        AtomicStmt0 = mark_hp(TargetLval),
        StmtCord = cord.singleton(Stmt0),
        see_in_lval(TargetLval, SeenAfter, SeenBefore)
    ;
        AtomicStmt0 = restore_hp(SourceRval),
        StmtCord = cord.singleton(Stmt0),
        see_in_rval(SourceRval, SeenAfter, SeenBefore)
    ;
        AtomicStmt0 = inline_target_code(_Lang, Components),
        StmtCord = cord.singleton(Stmt0),
        list.foldl(see_in_target_component, Components, SeenAfter, SeenBefore)
    ;
        AtomicStmt0 = outline_foreign_proc(_Lang, OutlineArgs, ResultLvals,
            _Code),
        StmtCord = cord.singleton(Stmt0),
        list.foldl(see_in_lval, ResultLvals, SeenAfter, SeenBefore0),
        list.foldl(see_in_outline_arg, OutlineArgs, SeenBefore0, SeenBefore)
    ).

:- pred delete_unused_in_func_defns(list(mlds_function_defn)::in,
    list(mlds_function_defn)::out, seen_set::in, seen_set::out) is det.

delete_unused_in_func_defns([], [], !SeenBefore).
delete_unused_in_func_defns([FuncDefn0 | FuncDefns0],
        [FuncDefn | FuncDefns], !SeenBefore) :-
    delete_unused_in_func_defn(FuncDefn0, FuncDefn, SeenBeforeFunc),
    set.union(SeenBeforeFunc, !SeenBefore),
    delete_unused_in_func_defns(FuncDefns0, FuncDefns, !SeenBefore).

:- pred delete_unused_in_func_defn(mlds_function_defn::in,
    mlds_function_defn::out, seen_set::out) is det.

delete_unused_in_func_defn(FuncDefn0, FuncDefn, SeenBefore) :-
    FuncDefn0 = mlds_function_defn(FuncName, Ctxt, Flags,
        MaybeOrigPredProcId, Params, Body0, EnvVars, MaybeTailRec),
    (
        Body0 = body_external,
        % External bodies can never occur nested inside another MLDS function.
        % They can occur only if a HLDS procedure is external, and if it is,
        % the compiler won't call this module on its MLDS implementation.
        % (In fact, it won't even know where that implementation actually is.)
        unexpected($pred, "body_external")
    ;
        Body0 = body_defined_here(Stmt0),
        map.init(SeenAtLabelMap0),
        SeenAtBreakSwitch0 = no,
        SeenAtBreakLoop0 = no,
        SeenAtContinueLoop0 = no,
        Info0 = ua_info(SeenAtLabelMap0, SeenAtBreakSwitch0, SeenAtBreakLoop0,
            SeenAtContinueLoop0, may_use_optimized_code),
        Params = mlds_func_params(Args, _ReturnTypes),
        ArgLocalVars = list.map(project_mlds_argument_name, Args),
        set.list_to_set(ArgLocalVars, SeenAfter),
        delete_unused_in_stmt(Stmt0, Stmt, SeenAfter, SeenBefore,
            Info0, Info),
        OrigOrOpt = Info ^ uai_orig_opt_code,
        (
            OrigOrOpt = may_use_optimized_code,
            Body = body_defined_here(Stmt),
            FuncDefn = mlds_function_defn(FuncName, Ctxt, Flags,
                MaybeOrigPredProcId, Params, Body, EnvVars, MaybeTailRec)
        ;
            OrigOrOpt = must_use_original_code,
            FuncDefn = FuncDefn0
        )
    ).

%---------------------------------------------------------------------------%

    % keep_only_seen_local_var_defns(SeenBefore0, SeenBefore,
    %   LocalVarDefns0, LocalVarDefns):
    %
    % Compute LocalVarDefns by deleting from LocalVarDefns0 all the definitions
    % of variable that do NOT occur in SeenBefore. There is no point in
    % defining a variable if that variable is never used.
    %
    % Compute SeenBefore by deleting from SeenBefore0 all the variables
    % that are defined by LocalVarDefns. Our caller passes us as SeenBefore0
    % the set of variables that are seen at the point just before the first
    % statement of a block, and it uses the value of SeenBefore we return
    % as the set of variables that are seen at the point just before
    % entry to the block itself. Outside the block, the variables defined
    % inside the block aren't relevant, and their presence in such seen_sets
    % misleads some sanity checks.
    %
:- pred keep_only_seen_local_var_defns(seen_set::in, seen_set::out,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out) is det.

keep_only_seen_local_var_defns(!SeenBefore, [], []).
keep_only_seen_local_var_defns(!SeenBefore,
        [HeadLocalVarDefn0 | TailLocalVarDefns0], LocalVarDefns) :-
    LocalVarName = HeadLocalVarDefn0 ^ mlvd_name,
    ( if set.remove(LocalVarName, !SeenBefore) then
        % LocalVarName was in the initial SeenBefore; keep it.
        keep_only_seen_local_var_defns(!SeenBefore,
            TailLocalVarDefns0, TailLocalVarDefns),
        LocalVarDefns = [HeadLocalVarDefn0 | TailLocalVarDefns]
    else
        % LocalVarName was NOT in the initial SeenBefore; delete it.
        keep_only_seen_local_var_defns(!SeenBefore,
            TailLocalVarDefns0, LocalVarDefns)
    ).

%---------------------------------------------------------------------------%

:- pred get_seen_set_at_label(mlds_label::in, seen_set::out,
    ua_info::in, ua_info::out) is det.

get_seen_set_at_label(Label, SeenSetAtLabel, !Info) :-
    SeenAtLabelMap0 = !.Info ^ uai_seen_at_label_map,
    ( if map.search(SeenAtLabelMap0, Label, SeenSetAtLabelPrime) then
        SeenSetAtLabel = SeenSetAtLabelPrime
    else
        % We don't know what local variables are needed at this label.
        % Since the optimization *depends* on us knowing this info,
        % we cannot use the "optimized" code.
        !Info ^ uai_orig_opt_code := must_use_original_code,
        % Since we *won't* be using the "optimized" code,
        % it does not matter what lie we return here.
        set.init(SeenSetAtLabel)
    ).

:- pred accumulate_label_seen_sets(list(mlds_label)::in,
    seen_set::in, seen_set::out, ua_info::in, ua_info::out) is det.

accumulate_label_seen_sets([], !Seen, !Info).
accumulate_label_seen_sets([Label | Labels], !Seen, !Info) :-
    get_seen_set_at_label(Label, SeenAtLabel, !Info),
    set.union(SeenAtLabel, !Seen),
    accumulate_label_seen_sets(Labels, !Seen, !Info).

%---------------------------------------------------------------------------%

:- pred see_in_match_cond(mlds_case_match_cond::in,
    seen_set::in, seen_set::out) is det.

see_in_match_cond(MatchCond, !Seen) :-
    (
        MatchCond = match_value(MatchRval),
        see_in_rval(MatchRval, !Seen)
    ;
        MatchCond = match_range(MatchMinRval, MatchMaxRval),
        see_in_rval(MatchMinRval, !Seen),
        see_in_rval(MatchMaxRval, !Seen)
    ).

:- pred see_in_target_component(target_code_component::in,
    seen_set::in, seen_set::out) is det.

see_in_target_component(Component, !Seen) :-
    (
        ( Component = user_target_code(_Code, _MaybeCtxt)
        ; Component = raw_target_code(_Code)
        ; Component = target_code_type(_Type)
        ; Component = target_code_function_name(_Name)
        ; Component = target_code_alloc_id(_AllocId)
        )
    ;
        Component = target_code_input(Rval),
        see_in_rval(Rval, !Seen)
    ;
        Component = target_code_output(Lval),
        see_in_lval(Lval, !Seen)
    ).

:- pred see_in_outline_arg(outline_arg::in,
    seen_set::in, seen_set::out) is det.

see_in_outline_arg(OutlineArg, !Seen) :-
    (
        OutlineArg = ola_in(_Type, _Name, Rval),
        see_in_rval(Rval, !Seen)
    ;
        OutlineArg = ola_out(_Type, _Name, Lval),
        see_in_lval(Lval, !Seen)
    ;
        OutlineArg = ola_unused
    ).

%---------------------------------------------------------------------------%

    % Record that we have seen a use of the given typed rval.
    %
:- pred see_in_typed_rval(mlds_typed_rval::in,
    seen_set::in, seen_set::out) is det.

see_in_typed_rval(ml_typed_rval(Rval, _Type), !Seen) :-
    see_in_rval(Rval, !Seen).

    % Record that we have seen a use of the given rval.
    %
:- pred see_in_rval(mlds_rval::in, seen_set::in, seen_set::out) is det.

see_in_rval(Rval, !Seen) :-
    (
        ( Rval = ml_lval(Lval)
        ; Rval = ml_mem_addr(Lval)
        ),
        see_in_lval(Lval, !Seen)
    ;
        ( Rval = ml_mkword(_Ptag, SubRval)
        ; Rval = ml_vector_common_row_addr(_, SubRval)
        ),
        see_in_rval(SubRval, !Seen)
    ;
        ( Rval = ml_box(_Type, BaseRvalA)
        ; Rval = ml_unbox(_Type, BaseRvalA)
        ; Rval = ml_cast(_Type, BaseRvalA)
        ; Rval = ml_unop(_Unop, BaseRvalA)
        ),
        see_in_rval(BaseRvalA, !Seen)
    ;
        Rval = ml_binop(_Binop, BaseRvalA, BaseRvalB),
        see_in_rval(BaseRvalA, !Seen),
        see_in_rval(BaseRvalB, !Seen)
    ;
        ( Rval = ml_const(_)
        ; Rval = ml_scalar_common(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_self(_)
        )
    ).

    % Record that we have seen a use of the given lval.
    %
:- pred see_in_lval(mlds_lval::in, seen_set::in, seen_set::out) is det.

see_in_lval(Lval, !Seen) :-
    (
        Lval = ml_field(_MaybePtag, BaseRval, _PtrType, FieldId, _FielType),
        see_in_rval(BaseRval, !Seen),
        (
            FieldId = ml_field_offset(FieldRval),
            see_in_rval(FieldRval, !Seen)
        ;
            FieldId = ml_field_named(_FieldName, _FieldType)
        )
    ;
        Lval = ml_mem_ref(AddrRval, _Type),
        see_in_rval(AddrRval, !Seen)
    ;
        Lval = ml_local_var(LocalVarName, _Type),
        set.insert(LocalVarName, !Seen)
    ;
        ( Lval = ml_target_global_var_ref(_)
        ; Lval = ml_global_var(_, _)
        )
    ).

%---------------------------------------------------------------------------%

:- pred stmt_cord_to_stmt(mlds_stmt::in, cord(mlds_stmt)::in, mlds_stmt::out)
    is det.

stmt_cord_to_stmt(ContextStmt, StmtCord, Stmt) :-
    Stmts = cord.to_list(StmtCord),
    (
        Stmts = [Stmt]
    ;
        ( Stmts = []
        ; Stmts = [_, _ | _]
        ),
        Context = get_mlds_stmt_context(ContextStmt),
        Stmt = ml_stmt_block([], [], Stmts, Context)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_unused_assign.
%---------------------------------------------------------------------------%
