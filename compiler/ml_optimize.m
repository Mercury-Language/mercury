% ---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_optimize.m.
% Main author: trd, fjh.
%
% This module runs various optimizations on the MLDS.
%
% Currently the optimizations we do here are
%
%   - converting assignments to local variables into variable initializers.
%   - eliminating initialized local variables entirely,
%     by replacing occurrences of such variables with their initializer
%
% Ultimately this module should just consist of a skeleton to traverse
% the MLDS, and should call various optimization modules along the way.
%
% It would probably be a good idea to make each transformation optional.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_optimize.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds.

:- pred mlds_optimize(globals::in, mlds::in, mlds::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_dump.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module set.

%---------------------------------------------------------------------------%

:- type opt_info
    --->    opt_info(
                oi_globals          :: globals,
                oi_module_name      :: mlds_module_name,
                oi_func_name        :: mlds_function_name,
                oi_func_params      :: mlds_func_params
            ).

mlds_optimize(Globals, !MLDS) :-
    ModuleName = mercury_module_name_to_mlds(!.MLDS ^ mlds_name),
    % XXX We should optimize functions stored inside classes as well.
    FuncDefns0 = !.MLDS ^ mlds_proc_defns,
    list.map(optimize_in_function_defn(ModuleName, Globals),
        FuncDefns0, FuncDefns),
    !MLDS ^ mlds_proc_defns := FuncDefns.

:- pred optimize_in_function_defn(mlds_module_name::in, globals::in,
    mlds_function_defn::in, mlds_function_defn::out) is det.

optimize_in_function_defn(ModuleName, Globals, FuncDefn0, FuncDefn) :-
    FuncDefn0 = mlds_function_defn(Name, Context, Flags, PredProcId,
        Params, FuncBody0, EnvVarNames, MaybeRequireTailrecInfo),
    OptInfo = opt_info(Globals, ModuleName, Name, Params),
    optimize_in_function_body(OptInfo, FuncBody0, FuncBody),
    FuncDefn = mlds_function_defn(Name, Context, Flags, PredProcId,
        Params, FuncBody, EnvVarNames, MaybeRequireTailrecInfo).

:- pred optimize_in_function_body(opt_info::in,
    mlds_function_body::in, mlds_function_body::out) is det.

optimize_in_function_body(OptInfo, !Body) :-
    (
        !.Body = body_external
    ;
        !.Body = body_defined_here(Stmt0),
        optimize_in_stmt(OptInfo, Stmt0, Stmt),
        trace [compile_time(flag("debug_ml_optimize")), io(!IO)] (
            Globals = OptInfo ^ oi_globals,
            ModuleName =
                mlds_module_name_to_sym_name(OptInfo ^ oi_module_name),
            get_debug_output_stream(Globals, ModuleName, Stream, !IO),
            io.write_string(Stream, "\nfunction body before\n\n", !IO),
            dump_mlds_stmt(Stream, 1, Stmt0, !IO),
            io.write_string(Stream, "\nfunction body after\n\n", !IO),
            dump_mlds_stmt(Stream, 1, Stmt, !IO)
        ),
        !:Body = body_defined_here(Stmt)
    ).

:- pred optimize_in_maybe_stmt(opt_info::in,
    maybe(mlds_stmt)::in, maybe(mlds_stmt)::out) is det.

optimize_in_maybe_stmt(OptInfo, !MaybeStmt) :-
    (
        !.MaybeStmt = no
    ;
        !.MaybeStmt = yes(Stmt0),
        optimize_in_stmt(OptInfo, Stmt0, Stmt),
        !:MaybeStmt = yes(Stmt)
    ).

:- pred optimize_in_stmts(opt_info::in,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

optimize_in_stmts(OptInfo, !Stmts) :-
    list.map(optimize_in_stmt(OptInfo), !Stmts),
    Globals = OptInfo ^ oi_globals,
    globals.get_opt_tuple(Globals, OptTuple),
    OptPeep = OptTuple ^ ot_opt_peep,
    (
        OptPeep = do_not_opt_peep
    ;
        OptPeep = opt_peep,
        peephole_opt_statements(!Stmts)
    ).

:- pred optimize_in_stmt(opt_info::in,
    mlds_stmt::in, mlds_stmt::out) is det.

optimize_in_stmt(OptInfo, Stmt0, Stmt) :-
    (
        Stmt0 = ml_stmt_call(_, _, _, _, _, _),
        optimize_in_call_stmt(OptInfo, Stmt0, Stmt)
    ;
        Stmt0 = ml_stmt_block(LocalVarDefns0, FuncDefns0, SubStmts0, Context),
        maybe_convert_assignments_into_initializers(OptInfo,
            LocalVarDefns0, LocalVarDefns1, SubStmts0, SubStmts1),
        maybe_eliminate_locals(OptInfo, LocalVarDefns1, LocalVarDefns,
            FuncDefns0, FuncDefns, SubStmts1, SubStmts2),
        maybe_flatten_block(SubStmts2, SubStmts3),
        optimize_in_stmts(OptInfo, SubStmts3, SubStmts),
        % XXX We should also optimize in FuncDefns.
        ( if
            LocalVarDefns = [],
            FuncDefns = [],
            SubStmts = [SubStmt]
        then
            Stmt = SubStmt
        else
            Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context)
        )
    ;
        Stmt0 = ml_stmt_while(Kind, Rval, SubStmts0, LocalLoopVars, Context),
        optimize_in_stmt(OptInfo, SubStmts0, SubStmts),
        Stmt = ml_stmt_while(Kind, Rval, SubStmts, LocalLoopVars, Context)
    ;
        Stmt0 = ml_stmt_if_then_else(Rval, Then0, MaybeElse0, Context),
        optimize_in_stmt(OptInfo, Then0, Then),
        optimize_in_maybe_stmt(OptInfo, MaybeElse0, MaybeElse),
        ( if
            Then = ml_stmt_block([], [], [], _),
            MaybeElse = yes(Else)
        then
            NotRval = ml_unop(logical_not, Rval),
            Stmt = ml_stmt_if_then_else(NotRval, Else, no, Context)
        else
            Stmt = ml_stmt_if_then_else(Rval, Then, MaybeElse, Context)
        )
    ;
        Stmt0 = ml_stmt_switch(Type, Rval, Range, Cases0, Default0, Context),
        list.map(optimize_in_case(OptInfo), Cases0, Cases),
        optimize_in_default(OptInfo, Default0, Default),
        Stmt = ml_stmt_switch(Type, Rval, Range, Cases, Default, Context)
    ;
        Stmt0 = ml_stmt_try_commit(Ref, BodyStmt0, HandlerStmt0, Context),
        optimize_in_stmt(OptInfo, BodyStmt0, BodyStmt),
        optimize_in_stmt(OptInfo, HandlerStmt0, HandlerStmt),
        Stmt = ml_stmt_try_commit(Ref, BodyStmt, HandlerStmt, Context)
    ;
        ( Stmt0 = ml_stmt_do_commit(_, _)
        ; Stmt0 = ml_stmt_return(_, _)
        ; Stmt0 = ml_stmt_label(_Label, _)
        ; Stmt0 = ml_stmt_goto(_Label, _)
        ; Stmt0 = ml_stmt_computed_goto(_Rval, _Label, _)
        ),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_atomic(Atomic0, Context),
        ( if Atomic0 = assign(TargetLval, SourceRval) then
            ( if
                % Optimize away assignments to the dummy var, since it *should*
                % only ever be assigned to; it should never be read.
                TargetLval = ml_global_var(TargetGlobalVar, _TargetType),
                TargetGlobalVar = global_dummy_var
            then
                Stmt = ml_stmt_block([], [], [], Context)
            else if
                % The only time the global dummy var seems to be read
                % (during a bootcheck, at least) is to test whether it is
                % equal to itself.
                %
                % We transform A == A into TRUE even if A is not a reference
                % to the dummy variable, since the transformation is valid for
                % any A (since mlds rvals can't cause side-effects), and
                % thus testing for the dummy variable is an unnecessary cost.
                %
                % We don't test for other patterns (such as transforming
                % A != A into FALSE) because I (zs) haven't seen the
                % Mercury compiler generating such code.
                SourceRval = ml_binop(BinOp, RvalA, RvalB),
                RvalA = RvalB,
                BinOp = eq(_)
            then
                Result = ml_const(mlconst_true),
                Atomic = assign(TargetLval, Result),
                Stmt = ml_stmt_atomic(Atomic, Context)
            else
                Stmt = Stmt0
            )
        else
            Stmt = Stmt0
        )
    ).

:- pred optimize_in_case(opt_info::in,
    mlds_switch_case::in, mlds_switch_case::out) is det.

optimize_in_case(OptInfo, Case0, Case) :-
    Case0 = mlds_switch_case(FirstCond, LaterConds, Stmt0),
    optimize_in_stmt(OptInfo, Stmt0, Stmt),
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt).

:- pred optimize_in_default(opt_info::in,
    mlds_switch_default::in, mlds_switch_default::out) is det.

optimize_in_default(OptInfo, Default0, Default) :-
    (
        Default0 = default_is_unreachable,
        Default = default_is_unreachable
    ;
        Default0 = default_do_nothing,
        Default = default_do_nothing
    ;
        Default0 = default_case(Stmt0),
        optimize_in_stmt(OptInfo, Stmt0, Stmt),
        Default = default_case(Stmt)
    ).

%---------------------------------------------------------------------------%

:- pred optimize_in_call_stmt(opt_info::in,
    mlds_stmt::in(ml_stmt_is_call), mlds_stmt::out) is det.

optimize_in_call_stmt(_OptInfo, Stmt0, Stmt) :-
    Stmt0 = ml_stmt_call(_Signature, FuncRval, CallArgRvals, _Results,
        _IsTailCall, Context),
    % If we have a self-tailcall, assign to the arguments and
    % then goto the top of the tailcall loop.
    ( if
        % Convert calls to `mark_hp' and `restore_hp' to the corresponding
        % MLDS instructions. This ensures that they get generated as
        % inline code. (Without this they won't, since HLDS inlining doesn't
        % get run again after the add_heap_ops pass that adds these calls.)
        % This approach is better than running HLDS inlining again,
        % because it cheaper in compilation time.

        FuncRval = ml_const(mlconst_code_addr(CodeAddr)),
        CodeAddr = mlds_code_addr(QualFuncLabel, _CodeAddrSignature),
        QualFuncLabel = qual_func_label(ModName, FuncLabel),
        FuncLabel = mlds_func_label(ProcLabel, MaybeAux),
        MaybeAux = proc_func,
        ProcLabel = mlds_proc_label(PredLabel, _ProcId),
        PredLabel = mlds_user_pred_label(pf_predicate, _DefnModName, PredName,
            _Arity),
        (
            PredName = "mark_hp",
            CallArgRvals = [ml_mem_addr(Lval)],
            AtomicStmt = mark_hp(Lval)
        ;
            PredName = "restore_hp",
            CallArgRvals = [Rval],
            AtomicStmt = restore_hp(Rval)
        ),
        PrivateBuiltin = mercury_private_builtin_module,
        ModName = mercury_module_name_to_mlds(PrivateBuiltin)
    then
        Stmt = ml_stmt_atomic(AtomicStmt, Context)
    else
        Stmt = Stmt0
    ).

%----------------------------------------------------------------------------

    % If the list of statements contains a block with no local variables,
    % then bring the block up one level. This optimization is needed to avoid
    % a compiler limit in the Microsoft C compiler (version 13.10.3077) for
    % too deeply nested blocks.
    %
:- pred maybe_flatten_block(list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

maybe_flatten_block(!Stmts) :-
    !:Stmts = list.condense(list.map(flatten_block, !.Stmts)).

:- func flatten_block(mlds_stmt) = list(mlds_stmt).

flatten_block(Stmt) = Stmts :-
    ( if Stmt = ml_stmt_block([], [], BlockStmts, _) then
        Stmts = BlockStmts
    else
        Stmts = [Stmt]
    ).

%---------------------------------------------------------------------------%

:- pred peephole_opt_statements(list(mlds_stmt)::in, list(mlds_stmt)::out)
    is det.

peephole_opt_statements([], []).
peephole_opt_statements([Stmt0], [Stmt0]).
peephole_opt_statements([Stmt0, Stmt1 | Stmts2], Stmts) :-
    ( if peephole_opt_statement(Stmt0, Stmt1, Stmts2, ReplStmts) then
        peephole_opt_statements(ReplStmts, Stmts)
    else
        peephole_opt_statements([Stmt1 | Stmts2], StmtsTail),
        Stmts = [Stmt0 | StmtsTail]
    ).

:- pred peephole_opt_statement(mlds_stmt::in, mlds_stmt::in,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is semidet.

peephole_opt_statement(Stmt0, Stmt1, Stmts2, Stmts) :-
    ( if
        % This pattern optimizes redundant tests like this:
        %
        % if (TestRval) {
        %     ...
        % } else {
        %     ...
        % }
        % if (TestRval) {
        %     ...
        % } else {
        %     ...
        % }
        %
        % If neither the then-part nor the else-part of the first if-then-else
        % can update any lval in TestRval, then the second test is redundant,
        % and we therefore optimize it away.
        %
        % The pattern seems to occur mostly with semidet deconstructions.
        % The semidet deconstruction tests whether the variable has the right
        % functor, the then-part of that if-then-else picks up its argument
        % values, there is no else part, and the next statement starts by
        % testing whether the previous one succeeded, using the exact same
        % condition as the semidet deconstruction.
        %
        % In theory, we could also apply the pattern if there were some other
        % non-TestRval-affecting statements between the if-then-elses. However,
        % I (zs) have not (yet) seen any code like that.

        Stmt0 = ml_stmt_if_then_else(TestRval, StmtThen0, MaybeStmtElse0,
            Context0),
        Stmt1 = ml_stmt_if_then_else(TestRval, StmtThen1, MaybeStmtElse1,
            _Context1),
        find_rval_component_lvals(TestRval, set.init, TestRvalComponents),
        statement_affects_lvals(TestRvalComponents, StmtThen0, no),
        (
            MaybeStmtElse0 = no
        ;
            MaybeStmtElse0 = yes(StmtElse0),
            statement_affects_lvals(TestRvalComponents, StmtElse0, no)
        )
    then
        ContextThen = get_mlds_stmt_context(StmtThen0),
        ThenBlockStmts0 = [StmtThen0, StmtThen1],
        maybe_flatten_block(ThenBlockStmts0, ThenBlockStmts),
        Then = ml_stmt_block([], [], ThenBlockStmts, ContextThen),
        (
            MaybeStmtElse0 = no,
            (
                MaybeStmtElse1 = no,
                MaybeElse = no
            ;
                MaybeStmtElse1 = yes(_),
                MaybeElse = MaybeStmtElse1
            )
        ;
            MaybeStmtElse0 = yes(Else0),
            (
                MaybeStmtElse1 = no,
                MaybeElse = MaybeStmtElse0
            ;
                MaybeStmtElse1 = yes(Else1),
                ElseBlockStmts0 = [Else0, Else1],
                maybe_flatten_block(ElseBlockStmts0, ElseBlockStmts),
                Else = ml_stmt_block([], [], ElseBlockStmts, Context0),
                MaybeElse = yes(Else)
            )
        ),
        Stmt = ml_stmt_if_then_else(TestRval, Then, MaybeElse, Context0),
        Stmts = [Stmt | Stmts2]
    else if
        % This pattern optimizes code like this, which we generate often
        % in automatically defined type-specific comparison predicates:
        %
        % succeeded = (X == Y);
        % succeeded = !(succeeded);
        %
        % This pattern replaces that with
        %
        % succeeded = (X != Y);
        %
        % because it reduces .c file size, because it may yield a speedup
        % (depending on C compiler optimizations), and because it is easier
        % to read when debugging generated C code.
        %
        % We do this only if the assignment operation is assign itself.
        % We could do the same with assign_if_in_heap operations as well,
        % but I (zs) have seen no need for that.
        %
        % Likewise, we could check for {float,str}_{eq,ne,lt,le,gt,ge}
        % as well as for their integer versions, but again, I have seen
        % no need for that.

        % We test for the negation operation first, since we want to fail fast
        % in the usual case where the pattern does not apply, and unary ops,
        % and self-negations in particular, occur less frequently than
        % binary ops and comparisons.
        Stmt1 = ml_stmt_atomic(Atomic1, _Context1),
        Atomic1 = assign(Lval, ml_unop(logical_not, ml_lval(Lval))),

        Stmt0 = ml_stmt_atomic(Atomic0, Context0),
        Atomic0 = assign(Lval, ml_binop(CompareOp, CmpRvalA, CmpRvalB)),

        ( CompareOp = eq(IntType), NegCompareOp = ne(IntType)
        ; CompareOp = ne(IntType), NegCompareOp = eq(IntType)
        ; CompareOp = int_lt(IntType), NegCompareOp = int_ge(IntType)
        ; CompareOp = int_le(IntType), NegCompareOp = int_gt(IntType)
        ; CompareOp = int_gt(IntType), NegCompareOp = int_le(IntType)
        ; CompareOp = int_ge(IntType), NegCompareOp = int_lt(IntType)
        )
    then
        Atomic = assign(Lval, ml_binop(NegCompareOp, CmpRvalA, CmpRvalB)),
        Stmt = ml_stmt_atomic(Atomic, Context0),
        Stmts = [Stmt | Stmts2]
    else
        fail
    ).

:- pred find_rval_component_lvals(mlds_rval::in,
    set(mlds_lval)::in, set(mlds_lval)::out) is det.

find_rval_component_lvals(Rval, !Components) :-
    (
        Rval = ml_lval(Lval),
        set.insert(Lval, !Components),
        find_lval_component_lvals(Lval, !Components)
    ;
        Rval = ml_mkword(_, SubRval),
        find_rval_component_lvals(SubRval, !Components)
    ;
        ( Rval = ml_box(_, SubRvalA)
        ; Rval = ml_unbox(_, SubRvalA)
        ; Rval = ml_cast(_, SubRvalA)
        ; Rval = ml_unop(_, SubRvalA)
        ),
        find_rval_component_lvals(SubRvalA, !Components)
    ;
        Rval = ml_binop(_, SubRvalA, SubRvalB),
        find_rval_component_lvals(SubRvalA, !Components),
        find_rval_component_lvals(SubRvalB, !Components)
    ;
        Rval = ml_mem_addr(Lval),
        set.insert(Lval, !Components),
        find_lval_component_lvals(Lval, !Components)
    ;
        ( Rval = ml_const(_)
        ; Rval = ml_scalar_common(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_vector_common_row_addr(_, _)
        ; Rval = ml_self(_)
        )
    ).

:- pred find_lval_component_lvals(mlds_lval::in,
    set(mlds_lval)::in, set(mlds_lval)::out) is det.

find_lval_component_lvals(Lval, !Components) :-
    (
        ( Lval = ml_field(_, Rval, _, _, _)
        ; Lval = ml_mem_ref(Rval, _)
        ),
        find_rval_component_lvals(Rval, !Components)
    ;
        ( Lval = ml_target_global_var_ref(_)
        ; Lval = ml_global_var(_, _)
        ; Lval = ml_local_var(_, _)
        )
    ).

:- pred statement_affects_lvals(set(mlds_lval)::in,
    mlds_stmt::in, bool::out) is det.

statement_affects_lvals(Lvals, Stmt, Affects) :-
    (
        Stmt = ml_stmt_block(_, _, SubStmts, _),
        statements_affect_lvals(Lvals, SubStmts, Affects)
    ;
        Stmt = ml_stmt_while(_, _, SubStmt, _, _),
        statement_affects_lvals(Lvals, SubStmt, Affects)
    ;
        Stmt = ml_stmt_if_then_else(_, Then, MaybeElse, _),
        (
            MaybeElse = no,
            Stmts = [Then]
        ;
            MaybeElse = yes(Else),
            Stmts = [Then, Else]
        ),
        statements_affect_lvals(Lvals, Stmts, Affects)
    ;
        Stmt = ml_stmt_switch(_, _, _, Cases, Default, _),
        cases_affect_lvals(Lvals, Cases, Affects0),
        (
            Affects0 = yes,
            Affects = yes
        ;
            Affects0 = no,
            (
                ( Default = default_is_unreachable
                ; Default = default_do_nothing
                ),
                Affects = no
            ;
                Default = default_case(DefaultStmt),
                statement_affects_lvals(Lvals, DefaultStmt, Affects)
            )
        )
    ;
        Stmt = ml_stmt_label(_, _),
        Affects = no
    ;
        ( Stmt = ml_stmt_goto(_, _)
        ; Stmt = ml_stmt_computed_goto(_, _, _)
        ; Stmt = ml_stmt_try_commit(_, _, _, _)
        ; Stmt = ml_stmt_do_commit(_, _)
        ; Stmt = ml_stmt_return(_, _)
        ),
        Affects = yes
    ;
        Stmt = ml_stmt_call(_, _, _, _, _, _),
        % A call can update local variables even without referring to them
        % explicitly, by referring to the environment in which they reside.
        Affects = yes
    ;
        Stmt = ml_stmt_atomic(AtomicStmt, _),
        (
            ( AtomicStmt = comment(_)
            ; AtomicStmt = delete_object(_)
            ; AtomicStmt = gc_check
            ; AtomicStmt = restore_hp(_)
            ; AtomicStmt = trail_op(_)
            ),
            Affects = no
        ;
            ( AtomicStmt = assign(Lval, _)
            ; AtomicStmt = assign_if_in_heap(Lval, _)
            ; AtomicStmt = new_object(Lval, _, _, _, _, _, _, _, _)
            ; AtomicStmt = mark_hp(Lval)
            ),
            ( if set.contains(Lvals, Lval) then
                Affects = yes
            else
                Affects = no
            )
        ;
            ( AtomicStmt = inline_target_code(_, _)     % XXX could be improved
            ; AtomicStmt = outline_foreign_proc(_, _, _, _)
            ),
            Affects = yes
        )
    ).

:- pred statements_affect_lvals(set(mlds_lval)::in,
    list(mlds_stmt)::in, bool::out) is det.

statements_affect_lvals(_, [], no).
statements_affect_lvals(Lvals, [Head | Tail], Affects) :-
    statement_affects_lvals(Lvals, Head, HeadAffects),
    (
        HeadAffects = yes,
        Affects = yes
    ;
        HeadAffects = no,
        statements_affect_lvals(Lvals, Tail, Affects)
    ).

:- pred cases_affect_lvals(set(mlds_lval)::in,
    list(mlds_switch_case)::in, bool::out) is det.

cases_affect_lvals(_, [], no).
cases_affect_lvals(Lvals, [Head | Tail], Affects) :-
    Head = mlds_switch_case(_, _, Stmt),
    statement_affects_lvals(Lvals, Stmt, HeadAffects),
    (
        HeadAffects = yes,
        Affects = yes
    ;
        HeadAffects = no,
        cases_affect_lvals(Lvals, Tail, Affects)
    ).

%---------------------------------------------------------------------------%

%
% This code implements the --optimize-initializations option.
% It converts MLDS code using assignments, e.g.
%
%   {
%       int v1;  // or any other type -- it doesn't have to be int
%       int v2;
%       int v3;
%       int v4;
%       int v5;
%
%       v1 = 1;
%       v2 = 2;
%       v3 = 3;
%       foo();
%       v4 = 4;
%       ...
%   }
%
% into code that instead uses initializers, e.g.
%
%   {
%       int v1 = 1;
%       int v2 = 2;
%       int v3 = 3;
%       int v4;
%
%       foo();
%       v4 = 4;
%       ...
%   }
%
% Note that if there are multiple initializations of the same variable,
% then we will apply the optimization successively, replacing the existing
% initializers as we go, and keeping only the last, e.g.
%
%       int v = 1;
%       v = 2;
%       v = 3;
%       ...
%
% will get replaced with
%
%       int v = 3;
%       ...
%
% We need to watch out for some tricky cases that can't be safely optimized.
% If the RHS of the assignment refers to a variable which was declared after
% the variable whose initialization we are optimizing, e.g.
%
%       int v1 = 1;
%       int v2 = 0;
%       v1 = v2 + 1;    // RHS refers to variable declared after v1
%
% then we can't do the optimization because it would cause a forward reference:
%
%       int v1 = v2 + 1;    // error -- v2 not declared yet!
%       int v2 = 0;
%
% Likewise if the RHS refers to the variable itself
%
%       int v1 = 1;
%       v1 = v1 + 1;
%
% then we can't optimize it, because that would be bogus:
%
%       int v1 = v1 + 1;    // error -- v1 not initialized yet!
%
% Similarly, if the initializers of the variables that follow
% the one we are trying to optimize refer to it, e.g.
%
%       int v1 = 1;
%       int v2 = v1 + 1;    // here v2 == 2
%       v1 = 0;
%       ...
%
% then we can't eliminate the assignment, because that would produce
% different results:
%
%       int v1 = 0;
%       int v2 = v1 + 1;    // wrong -- v2 == 1
%       ...

:- pred maybe_convert_assignments_into_initializers(opt_info::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

maybe_convert_assignments_into_initializers(OptInfo, !Defns, !Stmts) :-
    Globals = OptInfo ^ oi_globals,
    % Check if --optimize-initializations is enabled.
    globals.get_opt_tuple(Globals, OptTuple),
    OptInit = OptTuple ^ ot_opt_initializations,
    (
        OptInit = opt_initializations,
        convert_assignments_into_initializers(OptInfo, !Defns, !Stmts)
    ;
        OptInit = do_not_opt_initializations
    ).

:- pred convert_assignments_into_initializers(opt_info::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

convert_assignments_into_initializers(_OptInfo, !LocalVarDefns, [], []).
convert_assignments_into_initializers(OptInfo, !LocalVarDefns,
        [HeadStmt0 | TailStmts0], Stmts) :-
    ( if HeadStmt0 = ml_stmt_atomic(AtomicHeadStmt0, _) then
        ( if
            % Check if the first statement in the block is an assignment
            % to one of the variables declared in the block.
            AtomicHeadStmt0 = assign(LHS, RHS),
            LHS = ml_local_var(ThisVarName, _ThisVarType),

            % We must check that the value being assigned doesn't refer to the
            % variable itself.
            rval_contains_var(RHS, ThisVarName) = no,

            % We must check that the value being assigned doesn't refer to any
            % of the variables which are declared after this one. We must also
            % check that the initializers (if any) of the variables that follow
            % this one don't refer to this variable.
            find_this_var_defn(ThisVarName, !.LocalVarDefns, [], RevPrevDefns,
                ThisVarDefn0, LaterDefns),
            Filter =
                ( pred(OtherLocalVarDefn::in) is semidet :-
                    OtherLocalVarDefn = mlds_local_var_defn(OtherVarName, _,
                        _Type, OtherInitializer, _GC),
                    (
                        rval_contains_var(RHS, OtherVarName) = yes
                    ;
                        initializer_contains_var(OtherInitializer, ThisVarName)
                            = yes
                    )
                ),
            not list.find_first_match(Filter, LaterDefns, _)
        then
            % Replace the assignment statement with an initializer
            % on the variable declaration.
            ThisVarDefn = ThisVarDefn0 ^ mlvd_init := init_obj(RHS),
            !:LocalVarDefns = list.reverse(RevPrevDefns) ++
                [ThisVarDefn | LaterDefns],

            % Now try to apply the same optimization again.
            convert_assignments_into_initializers(OptInfo, !LocalVarDefns,
                TailStmts0, Stmts)
        else if
            AtomicHeadStmt0 = comment(_)
        then
            convert_assignments_into_initializers(OptInfo, !LocalVarDefns,
                TailStmts0, TailStmts),
            Stmts = [HeadStmt0 | TailStmts]
        else
            % No optimization possible -- leave the block unchanged.
            Stmts = [HeadStmt0 | TailStmts0]
        )
    else
        % No optimization possible -- leave the block unchanged.
        Stmts = [HeadStmt0 | TailStmts0]
    ).

:- pred find_this_var_defn(mlds_local_var_name::in,
    list(mlds_local_var_defn)::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    mlds_local_var_defn::out, list(mlds_local_var_defn)::out) is semidet.

find_this_var_defn(VarName, [LocalVarDefn | LocalVarDefns], !RevPrevDefns,
        ThisVarDefn, LaterDefns) :-
    ( if LocalVarDefn = mlds_local_var_defn(VarName, _, _, _, _) then
        ThisVarDefn = LocalVarDefn,
        LaterDefns = LocalVarDefns
    else
        !:RevPrevDefns = [LocalVarDefn | !.RevPrevDefns],
        find_this_var_defn(VarName, LocalVarDefns, !RevPrevDefns,
            ThisVarDefn, LaterDefns)
    ).

%---------------------------------------------------------------------------%
%
% This is a pass to eliminate initialized local variable definitions,
% by substituting the value of the initializer for occurrences of the variable.
%
% XXX This is quadratic in the number of variable definitions, since we do
% one pass over the block per variable definition. A more efficient algorithm
% would be to do one pass to figure out which variables could be eliminated,
% and then do another pass to actually eliminate them.

:- pred maybe_eliminate_locals(opt_info::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

maybe_eliminate_locals(OptInfo, !LocalVarDefns, !FuncDefns, !Stmts) :-
    Globals = OptInfo ^ oi_globals,
    globals.get_opt_tuple(Globals, OptTuple),
    EliminateLocalVars = OptTuple ^ ot_elim_local_vars,
    (
        EliminateLocalVars = elim_local_vars,
        eliminate_locals(OptInfo, !LocalVarDefns, !FuncDefns, !Stmts)
    ;
        EliminateLocalVars = do_not_elim_local_vars
    ).

:- pred eliminate_locals(opt_info::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

eliminate_locals(_OptInfo, [], [], !FuncDefns, !Stmts).
eliminate_locals(OptInfo, [LocalVarDefn0 | LocalVarDefns0], LocalVarDefns,
        !FuncDefns, !Stmts) :-
    ( if
        try_to_eliminate_defn(OptInfo, LocalVarDefn0, LocalVarDefns0,
            LocalVarDefns1, !FuncDefns, !Stmts)
    then
        eliminate_locals(OptInfo, LocalVarDefns1, LocalVarDefns,
            !FuncDefns, !Stmts)
    else
        eliminate_locals(OptInfo, LocalVarDefns0, LocalVarDefns2,
            !FuncDefns, !Stmts),
        LocalVarDefns = [LocalVarDefn0 | LocalVarDefns2]
    ).

    % This data structure holds information that we use in this pass
    % to eliminate initialized local variable definitions.
:- type var_elim_info
    --->    var_elim_info(
                % These fields remain constant.

                % The name of the variable to eliminate.
                var_name        :: mlds_local_var_name,

                % The value to replace the eliminated variable with.
                var_value       :: mlds_rval,

                % These get updated as we go along.

                % The number of occurrences of the variable.
                replace_count   :: int,

                % `yes' if the optimization can't be applied, e.g. because
                % the variable was assigned to, or because its address
                % was taken.
                invalidated     :: bool
            ).

    % Check if this definition is a variable that we can eliminate.
    % If so, replace uses of this variable with the variable's value.
    % This will fail if the definition is not a variable definition,
    % or if any of the statements or definitions take the address
    % of the variable, or assign to it.
    %
:- pred try_to_eliminate_defn(opt_info::in, mlds_local_var_defn::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is semidet.

try_to_eliminate_defn(OptInfo, LocalVarDefn0, LocalVarDefns0, LocalVarDefns,
        !FuncDefns, !Stmts) :-
    LocalVarDefn0 = mlds_local_var_defn(VarName, _Context,
        _Type, Initializer, _GCStmt),

    % Check if this definition has a known initial value.
    (
        Initializer = init_obj(Rval)
    ;
        Initializer = no_initializer,
        find_initial_val_in_stmts(VarName, Rval, !Stmts)
    ;
        ( Initializer = init_array(_)
        ; Initializer = init_struct(_, _)
        ),
        % Should we try to eliminate definitions with these kinds of
        % initializers?
        fail
    ),

    % It is only safe to do this transformation if the variable's value
    % is constant, otherwise we might end up moving the rvalue across
    % a statement which modifies it.
    rval_will_not_change(Rval),

    % This transformation moves evaluation of the rvalue later in the
    % computation. If the rvalue is something which might loop, throw an
    % exception, or abort (e.g. for division by zero), then this might change
    % the behaviour of the program. In such cases, we can only do the
    % transformation if reordering of both conjunctions and disjunctions
    % is allowed. (We need both permissions because can't tell here
    % whether this MLDS code came from a conjunction or a disjunction.)
    (
        rval_cannot_throw(Rval)
    ;
        Globals = OptInfo ^ oi_globals,
        globals.lookup_bool_option(Globals, reorder_conj, yes),
        globals.lookup_bool_option(Globals, reorder_disj, yes)
    ),

    % Replace uses of this variable with the variable's value,
    % checking that none of the statements or definitions took the
    % address of the variable, or assigned to it.
    eliminate_var(VarName, Rval, LocalVarDefns0, LocalVarDefns,
        !FuncDefns, !Stmts, Count, Invalidated),
    Invalidated = no,

    % Make sure that we didn't duplicate the rval, unless it is just a constant
    % or a variable, because duplicating any real operation would be
    % a pessimization.
    ( Count =< 1
    ; rval_is_cheap_enough_to_duplicate(Rval) = yes
    ).

:- func rval_is_cheap_enough_to_duplicate(mlds_rval) = bool.

rval_is_cheap_enough_to_duplicate(Rval) = CheapEnough :-
    (
        ( Rval = ml_const(_)
        ; Rval = ml_mem_addr(_)
        ; Rval = ml_self(_)
        ; Rval = ml_scalar_common(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_vector_common_row_addr(_, _)
        ),
        CheapEnough = yes
    ;
        Rval = ml_lval(Lval),
        (
            ( Lval = ml_local_var(_, _)
            ; Lval = ml_global_var(_, _)
            ),
            CheapEnough = yes
        ;
            ( Lval = ml_mem_ref(_, _)
            ; Lval = ml_field(_, _, _, _, _)
            ; Lval = ml_target_global_var_ref(_)
            ),
            CheapEnough = no
        )
    ;
        ( Rval = ml_mkword(_, _)
        ; Rval = ml_box(_, _)
        ; Rval = ml_unbox(_, _)
        ; Rval = ml_unop(_, _)
        ; Rval = ml_binop(_, _, _)
        ),
        % NOTE Some instances of the box and unbox operations are zero cost,
        % but others are not. Since we cannot distinguish between them
        % using purely local data, we take the conservative approach.
        CheapEnough = no
    ;
        Rval = ml_cast(_, SubRval),
        CheapEnough = rval_is_cheap_enough_to_duplicate(SubRval)
    ).

    % Succeed only if the specified rval definitely won't change in value.
    %
:- pred rval_will_not_change(mlds_rval::in) is semidet.

rval_will_not_change(Rval) :-
    require_complete_switch [Rval]
    (
        ( Rval = ml_const(_)
        ; Rval = ml_scalar_common(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_vector_common_row_addr(_, _)
        )
    ;
        Rval = ml_mem_addr(Lval),
        require_complete_switch [Lval]
        (
            ( Lval = ml_local_var(_, _)
            ; Lval = ml_global_var(_, _)
            )
        ;
            ( Lval = ml_mem_ref(SubRval, _Type)
            ; Lval = ml_field(_, SubRval, _, _, _)
            ),
            rval_will_not_change(SubRval)
        ;
            Lval = ml_target_global_var_ref(_),
            % XXX How can the address of a target language global variable
            % change?
            fail
        )
    ;
        ( Rval = ml_mkword(_Tag, SubRval)
        ; Rval = ml_box(_Type, SubRval)
        ; Rval = ml_unbox(_Type, SubRval)
        ; Rval = ml_cast(_Type, SubRval)
        ; Rval = ml_unop(_Op, SubRval)
        ),
        rval_will_not_change(SubRval)
    ;
        Rval = ml_binop(_Op, SubRvalA, SubRvalB),
        rval_will_not_change(SubRvalA),
        rval_will_not_change(SubRvalB)
    ;
        ( Rval = ml_lval(_)
        ; Rval = ml_self(_)
        ),
        fail
    ).

    % Succeed only if the given rval definitely can't loop,
    % throw an exception, or abort.
    % We use a pretty conservative approximation...
    %
:- pred rval_cannot_throw(mlds_rval::in) is semidet.

rval_cannot_throw(Rval) :-
    require_complete_switch [Rval]
    (
        ( Rval = ml_const(_)
        ; Rval = ml_scalar_common(_)
        ; Rval = ml_scalar_common_addr(_)
        ; Rval = ml_self(_)
        ; Rval = ml_mem_addr(_)
        )
    ;
        ( Rval = ml_vector_common_row_addr(_, SubRval)
        ; Rval = ml_mkword(_Tag, SubRval)
        ; Rval = ml_box(_, SubRval)
        ; Rval = ml_unbox(_, SubRval)
        ; Rval = ml_cast(_, SubRval)
        ),
        rval_cannot_throw(SubRval)
    ;
        ( Rval = ml_lval(_)
        ; Rval = ml_unop(_, _)
        ; Rval = ml_binop(_, _, _)
        ),
        % Some unary and binary ops may throw, though others may not.
        fail
    ).

    % Search through a list of statements, trying to find the first assignment
    % to the specified variable. Return the initial value, and a modified list
    % of statements with the initial assignment deleted. Fail if the first
    % value can't be determined.
    %
:- pred find_initial_val_in_stmts(mlds_local_var_name::in, mlds_rval::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out) is semidet.

find_initial_val_in_stmts(VarName, Rval, [Stmt0 | Stmts0], Stmts) :-
    ( if find_initial_val_in_stmt(VarName, Rval1, Stmt0, Stmt1) then
        Rval = Rval1,
        ( if Stmt1 = ml_stmt_block([], [], [], _) then
            Stmts = Stmts0
        else
            Stmts = [Stmt1 | Stmts0]
        )
    else
        % Check that Stmt0 doesn't modify the value of the variable
        % -- this includes checking that there are no labels via which code
        % could branch into the middle of Stmt0. Only if we are sure
        % that Stmt0 can't modify the variable's value is it safe to go on
        % and look for the initial value in Stmts0.
        statement_contains_var(Stmt0, VarName) = no,
        not (
            statement_is_or_contains_statement(Stmt0, Label),
            Label = ml_stmt_label(_, _)
        ),
        find_initial_val_in_stmts(VarName, Rval, Stmts0, Stmts1),
        Stmts = [Stmt0 | Stmts1]
    ).

:- pred find_initial_val_in_stmt(mlds_local_var_name::in, mlds_rval::out,
    mlds_stmt::in, mlds_stmt::out) is semidet.

find_initial_val_in_stmt(Var, Rval, Stmt0, Stmt) :-
    ( if
        Stmt0 = ml_stmt_atomic(AtomicStmt, Context),
        AtomicStmt = assign(ml_local_var(Var, _Type), Rval0)
    then
        Rval = Rval0,
        % Delete the assignment, by replacing it with an empty block.
        Stmt = ml_stmt_block([], [], [], Context)
    else if
        Stmt0 = ml_stmt_block(LocalVarDefns0, FuncDefns0, SubStmts0, Context)
    then
        local_var_defns_contains_var(LocalVarDefns0, Var) = no,
        function_defns_contains_var(FuncDefns0, Var) = no,
        find_initial_val_in_stmts(Var, Rval, SubStmts0, SubStmts),
        Stmt = ml_stmt_block(LocalVarDefns0, FuncDefns0, SubStmts, Context)
    else
        fail
    ).

    % Replace uses of this variable with the variable's value in the specified
    % definitions and statements. This will return a count of how many
    % occurrences of the variable there were. It will also return
    % Invalidated = yes if any of the statements or definitions take
    % the address of the variable, or assign to it; in that case, the
    % transformation should not be performed.
    %
:- pred eliminate_var(mlds_local_var_name::in, mlds_rval::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out,
    int::out, bool::out) is det.

eliminate_var(QualVarName, VarRval, !LocalVarDefns, !FuncDefns, !Stmts,
        Count, Invalidated) :-
    Count0 = 0,
    Invalidated0 = no,
    VarElimInfo0 = var_elim_info(QualVarName, VarRval, Count0, Invalidated0),
    eliminate_var_in_block(!LocalVarDefns, !FuncDefns, !Stmts,
        VarElimInfo0, VarElimInfo),
    Count = VarElimInfo ^ replace_count,
    Invalidated = VarElimInfo ^ invalidated.

% eliminate_var_in_*:
%
% Process the specified construct, replacing all rvalue occurrences of the
% variable (^var_name) with its value (^var_value), incrementing the
% ^replace_count field for each occurrence as an rvalue, and setting
% ^invalidated to yes if the variable occurs as an lvalue.

:- pred eliminate_var_in_block(
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    list(mlds_stmt)::in, list(mlds_stmt)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_block(!LocalVarDefns, !FuncDefns, !Stmts, !VarElimInfo) :-
    list.map_foldl(eliminate_var_in_local_var_defn,
        !LocalVarDefns, !VarElimInfo),
    list.map_foldl(eliminate_var_in_function_defn,
        !FuncDefns, !VarElimInfo),
    eliminate_var_in_stmts(!Stmts, !VarElimInfo).

:- pred eliminate_var_in_local_var_defn(
    mlds_local_var_defn::in, mlds_local_var_defn::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_local_var_defn(LocalVarDefn0, LocalVarDefn, !VarElimInfo) :-
    LocalVarDefn0 = mlds_local_var_defn(Name, Context,
        Type, Initializer0, GCStmt),
    eliminate_var_in_initializer(Initializer0, Initializer, !VarElimInfo),
    LocalVarDefn = mlds_local_var_defn(Name, Context,
        Type, Initializer, GCStmt).

:- pred eliminate_var_in_function_defn(
    mlds_function_defn::in, mlds_function_defn::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_function_defn(FuncDefn0, FuncDefn, !VarElimInfo) :-
    FuncDefn0 = mlds_function_defn(Name, Context, Flags, Id, Params,
        Body0, EnvVarNames, MaybeRequireTailrecInfo),
    (
        Body0 = body_external,
        Body = Body0
    ;
        Body0 = body_defined_here(Stmt0),
        eliminate_var_in_stmt(Stmt0, Stmt, !VarElimInfo),
        Body = body_defined_here(Stmt)
    ),
    FuncDefn = mlds_function_defn(Name, Context, Flags, Id, Params,
        Body, EnvVarNames, MaybeRequireTailrecInfo).

:- pred eliminate_var_in_initializer(
    mlds_initializer::in, mlds_initializer::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_initializer(Init0, Init, !VarElimInfo) :-
    (
        Init0 = no_initializer,
        Init = Init0
    ;
        Init0 = init_obj(Rval0),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Init = init_obj(Rval)
    ;
        Init0 = init_array(Elements0),
        list.map_foldl(eliminate_var_in_initializer, Elements0, Elements,
            !VarElimInfo),
        Init = init_array(Elements)
    ;
        Init0 = init_struct(Type, Members0),
        list.map_foldl(eliminate_var_in_initializer, Members0, Members,
            !VarElimInfo),
        Init = init_struct(Type, Members)
    ).

:- pred eliminate_var_in_typed_rvals(
    list(mlds_typed_rval)::in, list(mlds_typed_rval)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_typed_rvals([], [], !VarElimInfo).
eliminate_var_in_typed_rvals([TypedRval0 | TypedRvals0],
        [TypedRval | TypedRvals], !VarElimInfo) :-
    TypedRval0 = ml_typed_rval(Rval0, Type),
    eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
    TypedRval = ml_typed_rval(Rval, Type),
    eliminate_var_in_typed_rvals(TypedRvals0, TypedRvals, !VarElimInfo).

:- pred eliminate_var_in_rvals(list(mlds_rval)::in, list(mlds_rval)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_rvals(!Rvals, !VarElimInfo) :-
    list.map_foldl(eliminate_var_in_rval, !Rvals, !VarElimInfo).

:- pred eliminate_var_in_rval(mlds_rval::in, mlds_rval::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_rval(Rval0, Rval, !VarElimInfo) :-
    (
        Rval0 = ml_lval(Lval0),
        VarName = !.VarElimInfo ^ var_name,
        ( if Lval0 = ml_local_var(VarName, _) then
            % We found an rvalue occurrence of the variable -- replace it
            % with the rval for the variable's value, and increment the counter
            % for the number of occurrences that we have replaced.
            Rval = !.VarElimInfo ^ var_value,
            Count0 = !.VarElimInfo ^ replace_count,
            !VarElimInfo ^ replace_count := Count0 + 1
        else
            eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
            Rval = ml_lval(Lval)
        )
    ;
        Rval0 = ml_mkword(Tag, ArgRval0),
        eliminate_var_in_rval(ArgRval0, ArgRval, !VarElimInfo),
        Rval = ml_mkword(Tag, ArgRval)
    ;
        Rval0 = ml_box(Type, ArgRval0),
        eliminate_var_in_rval(ArgRval0, ArgRval, !VarElimInfo),
        Rval = ml_box(Type, ArgRval)
    ;
        Rval0 = ml_unbox(Type, ArgRval0),
        eliminate_var_in_rval(ArgRval0, ArgRval, !VarElimInfo),
        Rval = ml_unbox(Type, ArgRval)
    ;
        Rval0 = ml_cast(Type, ArgRval0),
        eliminate_var_in_rval(ArgRval0, ArgRval, !VarElimInfo),
        Rval = ml_cast(Type, ArgRval)
    ;
        Rval0 = ml_unop(Op, ArgRval0),
        eliminate_var_in_rval(ArgRval0, ArgRval, !VarElimInfo),
        Rval = ml_unop(Op, ArgRval)
    ;
        Rval0 = ml_binop(Op, ArgRvalA0, ArgRvalB0),
        eliminate_var_in_rval(ArgRvalA0, ArgRvalA, !VarElimInfo),
        eliminate_var_in_rval(ArgRvalB0, ArgRvalB, !VarElimInfo),
        Rval = ml_binop(Op, ArgRvalA, ArgRvalB)
    ;
        Rval0 = ml_mem_addr(Lval0),
        eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
        Rval = ml_mem_addr(Lval)
    ;
        Rval0 = ml_vector_common_row_addr(VectorCommon, RowRval0),
        eliminate_var_in_rval(RowRval0, RowRval, !VarElimInfo),
        Rval = ml_vector_common_row_addr(VectorCommon, RowRval)
    ;
        ( Rval0 = ml_const(_)
        ; Rval0 = ml_scalar_common(_)
        ; Rval0 = ml_scalar_common_addr(_)
        ; Rval0 = ml_self(_)
        ),
        Rval = Rval0
    ).

:- pred eliminate_var_in_lvals(list(mlds_lval)::in, list(mlds_lval)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_lvals(!Lvals, !VarElimInfo) :-
    list.map_foldl(eliminate_var_in_lval, !Lvals, !VarElimInfo).

:- pred eliminate_var_in_lval(mlds_lval::in, mlds_lval::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_lval(Lval0, Lval, !VarElimInfo) :-
    (
        Lval0 = ml_field(MaybeTag, Rval0, PtrType, FieldId, FieldType),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Lval = ml_field(MaybeTag, Rval, PtrType, FieldId, FieldType)
    ;
        Lval0 = ml_mem_ref(Rval0, Type),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Lval = ml_mem_ref(Rval, Type)
    ;
        ( Lval0 = ml_target_global_var_ref(_Ref)
        ; Lval0 = ml_global_var(_, _)
        ),
        Lval = Lval0
    ;
        Lval0 = ml_local_var(VarName, _Type),
        invalidate_if_eliminating_local_loop_var(VarName, !VarElimInfo),
        Lval = Lval0
    ).

:- pred eliminate_var_in_stmts(
    list(mlds_stmt)::in, list(mlds_stmt)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_stmts(!Stmts, !VarElimInfo) :-
    list.map_foldl(eliminate_var_in_stmt, !Stmts, !VarElimInfo).

:- pred eliminate_var_in_maybe_stmt(
    maybe(mlds_stmt)::in, maybe(mlds_stmt)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_maybe_stmt(no, no, !VarElimInfo).
eliminate_var_in_maybe_stmt(yes(Stmt0), yes(Stmt), !VarElimInfo) :-
    eliminate_var_in_stmt(Stmt0, Stmt, !VarElimInfo).

:- pred eliminate_var_in_stmt(mlds_stmt::in, mlds_stmt::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_stmt(Stmt0, Stmt, !VarElimInfo) :-
    (
        Stmt0 = ml_stmt_block(LocalVarDefns0, FuncDefns0, SubStmts0, Context),
        eliminate_var_in_block(LocalVarDefns0, LocalVarDefns,
            FuncDefns0, FuncDefns, SubStmts0, SubStmts, !VarElimInfo),
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context)
    ;
        Stmt0 = ml_stmt_while(Kind, Rval0, SubStmts0, LocalLoopVars, Context),
        list.foldl(invalidate_if_eliminating_local_loop_var, LocalLoopVars,
            !VarElimInfo),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        eliminate_var_in_stmt(SubStmts0, SubStmts, !VarElimInfo),
        Stmt = ml_stmt_while(Kind, Rval, SubStmts, LocalLoopVars, Context)
    ;
        Stmt0 = ml_stmt_if_then_else(Cond0, Then0, MaybeElse0, Context),
        eliminate_var_in_rval(Cond0, Cond, !VarElimInfo),
        eliminate_var_in_stmt(Then0, Then, !VarElimInfo),
        eliminate_var_in_maybe_stmt(MaybeElse0, MaybeElse, !VarElimInfo),
        Stmt = ml_stmt_if_then_else(Cond, Then, MaybeElse, Context)
    ;
        Stmt0 = ml_stmt_switch(Type, Val0, Range, Cases0, Default0, Context),
        eliminate_var_in_rval(Val0, Val, !VarElimInfo),
        list.map_foldl(eliminate_var_in_case, Cases0, Cases, !VarElimInfo),
        eliminate_var_in_default(Default0, Default, !VarElimInfo),
        Stmt = ml_stmt_switch(Type, Val, Range, Cases, Default, Context)
    ;
        Stmt0 = ml_stmt_label(_, _),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_goto(_, _),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_computed_goto(Rval0, Labels, Context),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Stmt = ml_stmt_computed_goto(Rval, Labels, Context)
    ;
        Stmt0 = ml_stmt_call(Sig, Func0, Args0, RetLvals0, TailCall,
            Context),
        eliminate_var_in_rval(Func0, Func, !VarElimInfo),
        eliminate_var_in_rvals(Args0, Args, !VarElimInfo),
        eliminate_var_in_lvals(RetLvals0, RetLvals, !VarElimInfo),
        Stmt = ml_stmt_call(Sig, Func, Args, RetLvals, TailCall,
            Context)
    ;
        Stmt0 = ml_stmt_return(Rvals0, Context),
        eliminate_var_in_rvals(Rvals0, Rvals, !VarElimInfo),
        Stmt = ml_stmt_return(Rvals, Context)
    ;
        Stmt0 = ml_stmt_do_commit(Ref0, Context),
        eliminate_var_in_rval(Ref0, Ref, !VarElimInfo),
        Stmt = ml_stmt_do_commit(Ref, Context)
    ;
        Stmt0 = ml_stmt_try_commit(Ref0, BodyStmt0, HandlerStmt0, Context),
        eliminate_var_in_lval(Ref0, Ref, !VarElimInfo),
        eliminate_var_in_stmt(BodyStmt0, BodyStmt, !VarElimInfo),
        eliminate_var_in_stmt(HandlerStmt0, HandlerStmt, !VarElimInfo),
        Stmt = ml_stmt_try_commit(Ref, BodyStmt, HandlerStmt, Context)
    ;
        Stmt0 = ml_stmt_atomic(AtomicStmt0, Context),
        eliminate_var_in_atomic_stmt(AtomicStmt0, AtomicStmt, !VarElimInfo),
        Stmt = ml_stmt_atomic(AtomicStmt, Context)
    ).

:- pred eliminate_var_in_case(mlds_switch_case::in, mlds_switch_case::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_case(Case0, Case, !VarElimInfo) :-
    Case0 = mlds_switch_case(FirstCond0, LaterConds0, Stmt0),
    eliminate_var_in_case_cond(FirstCond0, FirstCond, !VarElimInfo),
    list.map_foldl(eliminate_var_in_case_cond, LaterConds0, LaterConds,
        !VarElimInfo),
    eliminate_var_in_stmt(Stmt0, Stmt, !VarElimInfo),
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt).

:- pred eliminate_var_in_default(
    mlds_switch_default::in, mlds_switch_default::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_default(Default0, Default, !VarElimInfo) :-
    (
        ( Default0 = default_is_unreachable
        ; Default0 = default_do_nothing
        ),
        Default = Default0
    ;
        Default0 = default_case(Stmt0),
        eliminate_var_in_stmt(Stmt0, Stmt, !VarElimInfo),
        Default = default_case(Stmt)
    ).

:- pred eliminate_var_in_atomic_stmt(
    mlds_atomic_statement::in, mlds_atomic_statement::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_atomic_stmt(Stmt0, Stmt, !VarElimInfo) :-
    (
        ( Stmt0 = comment(_)
        ; Stmt0 = gc_check
        ),
        Stmt = Stmt0
    ;
        Stmt0 = assign(Lval0, Rval0),
        eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Stmt = assign(Lval, Rval)
    ;
        Stmt0 = assign_if_in_heap(Lval0, Rval0),
        eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Stmt = assign_if_in_heap(Lval, Rval)
    ;
        Stmt0 = delete_object(Rval0),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Stmt = delete_object(Rval)
    ;
        Stmt0 = new_object(Target0, Ptag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, ArgRvalsTypes0, MayUseAtomic,
            MaybeAllocId),
        eliminate_var_in_lval(Target0, Target, !VarElimInfo),
        eliminate_var_in_typed_rvals(ArgRvalsTypes0, ArgRvalsTypes,
            !VarElimInfo),
        Stmt = new_object(Target, Ptag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, ArgRvalsTypes, MayUseAtomic,
            MaybeAllocId)
    ;
        Stmt0 = mark_hp(Lval0),
        eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
        Stmt = mark_hp(Lval)
    ;
        Stmt0 = restore_hp(Rval0),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Stmt = restore_hp(Rval)
    ;
        Stmt0 = trail_op(TrailOp0),
        eliminate_var_in_trail_op(TrailOp0, TrailOp, !VarElimInfo),
        Stmt = trail_op(TrailOp)
    ;
        Stmt0 = inline_target_code(Lang, Components0),
        list.map_foldl(eliminate_var_in_target_code_component,
            Components0, Components, !VarElimInfo),
        Stmt = inline_target_code(Lang, Components)
    ;
        Stmt0 = outline_foreign_proc(Lang, Vs, Lvals0, Code),
        eliminate_var_in_lvals(Lvals0, Lvals, !VarElimInfo),
        Stmt = outline_foreign_proc(Lang, Vs, Lvals, Code)
    ).

:- pred eliminate_var_in_case_cond(
    mlds_case_match_cond::in, mlds_case_match_cond::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_case_cond(Cond0, Cond, !VarElimInfo) :-
    (
        Cond0 = match_value(Rval0),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Cond = match_value(Rval)
    ;
        Cond0 = match_range(Low0, High0),
        eliminate_var_in_rval(Low0, Low, !VarElimInfo),
        eliminate_var_in_rval(High0, High, !VarElimInfo),
        Cond = match_range(Low, High)
    ).

:- pred eliminate_var_in_target_code_component(
    target_code_component::in, target_code_component::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_target_code_component(Component0, Component, !VarElimInfo) :-
    (
        ( Component0 = raw_target_code(_Code)
        ; Component0 = user_target_code(_Code, _Context)
        ; Component0 = target_code_type(_Type)
        ; Component0 = target_code_function_name(_Name)
        ; Component0 = target_code_alloc_id(_AllocId)
        ),
        Component = Component0
    ;
        Component0 = target_code_input(Rval0),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Component = target_code_input(Rval)
    ;
        Component0 = target_code_output(Lval0),
        eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
        Component = target_code_output(Lval)
    ).

:- pred eliminate_var_in_trail_op(trail_op::in, trail_op::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_trail_op(Op0, Op, !VarElimInfo) :-
    (
        Op0 = store_ticket(Lval0),
        eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
        Op = store_ticket(Lval)
    ;
        Op0 = reset_ticket(Rval0, Reason),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Op = reset_ticket(Rval, Reason)
    ;
        ( Op0 = discard_ticket
        ; Op0 = prune_ticket
        ),
        Op = Op0
    ;
        Op0 = mark_ticket_stack(Lval0),
        eliminate_var_in_lval(Lval0, Lval, !VarElimInfo),
        Op = mark_ticket_stack(Lval)
    ;
        Op0 = prune_tickets_to(Rval0),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Op = prune_tickets_to(Rval)
    ).

:- pred invalidate_if_eliminating_local_loop_var(mlds_local_var_name::in,
    var_elim_info::in, var_elim_info::out) is det.

invalidate_if_eliminating_local_loop_var(VarName, !VarElimInfo) :-
    ( if VarName = !.VarElimInfo ^ var_name then
        % We found an lvalue occurrence of the variable.
        % If the variable that we are trying to eliminate
        % - has its address taken, or
        % - is assigned to, or
        % - in general if it is used as an lvalue,
        % then it is NOT safe to eliminate it.
        !VarElimInfo ^ invalidated := yes
    else
        true
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_optimize.
%---------------------------------------------------------------------------%
