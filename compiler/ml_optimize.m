%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_optimize.m.
% Main author: trd, fjh.
%
% This module runs various optimizations on the MLDS.
%
% Currently the optimizations we do here are
%   - turning self-tailcalls into loops;
%   - converting assignments to local variables into variable initializers.
%   - eliminating initialized local variables entirely,
%     by replacing occurrences of such variables with their initializer
%
% Note that tailcall detection is done in ml_tailcall.m.
% It might be nice to move the detection here, and do both the
% loop transformation (in the case of self-tailcalls) and marking
% tailcalls at the same time.
%
% Ultimately this module should just consist of a skeleton to traverse
% the MLDS, and should call various optimization modules along the way.
%
% It would probably be a good idea to make each transformation optional.
%
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_optimize.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.mlds.

:- pred mlds_optimize(globals::in, mlds::in, mlds::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_target_util.
:- import_module ml_backend.ml_util.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module std_util.

%-----------------------------------------------------------------------------%

:- type opt_info
    --->    opt_info(
                oi_globals          :: globals,
                oi_module_name      :: mlds_module_name,
                oi_entity_name      :: mlds_entity_name,
                oi_func_params      :: mlds_func_params,
                oi_context          :: mlds_context
            ).

mlds_optimize(Globals, !MLDS) :-
    Defns0 = !.MLDS ^ mlds_proc_defns,
    ModuleName = mercury_module_name_to_mlds(!.MLDS ^ mlds_name),
    optimize_in_defns(Globals, ModuleName, Defns0, Defns),
    !MLDS ^ mlds_proc_defns := Defns.

:- pred optimize_in_defns(globals::in, mlds_module_name::in,
    list(mlds_defn)::in, list(mlds_defn)::out) is det.

optimize_in_defns(Globals, ModuleName, !Defns) :-
    list.map(optimize_in_defn(ModuleName, Globals), !Defns).

:- pred optimize_in_defn(mlds_module_name::in, globals::in,
    mlds_defn::in, mlds_defn::out) is det.

optimize_in_defn(ModuleName, Globals, Defn0, Defn) :-
    (
        Defn0 = mlds_data(_),
        Defn = Defn0
    ;
        Defn0 = mlds_function(FunctionDefn0),
        FunctionDefn0 = mlds_function_defn(Name, Context, Flags,
            PredProcId, Params, FuncBody0, Attributes, EnvVarNames,
            MaybeRequireTailrecInfo),
        OptInfo = opt_info(Globals, ModuleName, Name, Params, Context),

        optimize_func(OptInfo, FuncBody0, FuncBody1),
        optimize_in_function_body(OptInfo, FuncBody1, FuncBody),

        FunctionDefn = mlds_function_defn(Name, Context, Flags,
            PredProcId, Params, FuncBody, Attributes, EnvVarNames,
            MaybeRequireTailrecInfo),
        Defn = mlds_function(FunctionDefn)
    ;
        Defn0 = mlds_class(ClassDefn0),
        ClassDefn0 = mlds_class_defn(Name, Context, Flags, Kind,
            Imports, BaseClasses, Implements,
            TypeParams, CtorDefns0, MemberDefns0),
        optimize_in_defns(Globals, ModuleName, MemberDefns0, MemberDefns),
        optimize_in_defns(Globals, ModuleName, CtorDefns0, CtorDefns),
        ClassDefn = mlds_class_defn(Name, Context, Flags, Kind,
            Imports, BaseClasses, Implements,
            TypeParams, CtorDefns, MemberDefns),
        Defn = mlds_class(ClassDefn)
    ).

:- pred optimize_in_function_body(opt_info::in,
    mlds_function_body::in, mlds_function_body::out) is det.

optimize_in_function_body(OptInfo, !Body) :-
    (
        !.Body = body_external
    ;
        !.Body = body_defined_here(Statement0),
        optimize_in_statement(OptInfo, Statement0, Statement),
        !:Body = body_defined_here(Statement)
    ).

:- pred optimize_in_maybe_statement(opt_info::in,
    maybe(statement)::in, maybe(statement)::out) is det.

optimize_in_maybe_statement(OptInfo, !MaybeStatement) :-
    (
        !.MaybeStatement = no
    ;
        !.MaybeStatement = yes(Statement0),
        optimize_in_statement(OptInfo, Statement0, Statement),
        !:MaybeStatement = yes(Statement)
    ).

:- pred optimize_in_statements(opt_info::in,
    list(statement)::in, list(statement)::out) is det.

optimize_in_statements(OptInfo, !Statements) :-
    list.map(optimize_in_statement(OptInfo), !Statements),
    Globals = OptInfo ^ oi_globals,
    globals.lookup_bool_option(Globals, optimize_peep, OptPeep),
    (
        OptPeep = no
    ;
        OptPeep = yes,
        peephole_opt_statements(!Statements)
    ).

:- pred optimize_in_statement(opt_info::in,
    statement::in, statement::out) is det.

optimize_in_statement(!.OptInfo, !Statement) :-
    !.Statement = statement(Stmt0, Context),
    !OptInfo ^ oi_context := Context,
    optimize_in_stmt(!.OptInfo, Stmt0, Stmt),
    !:Statement = statement(Stmt, Context).

:- pred optimize_in_stmt(opt_info::in,
    mlds_stmt::in, mlds_stmt::out) is det.

optimize_in_stmt(OptInfo, Stmt0, Stmt) :-
    (
        Stmt0 = ml_stmt_call(_, _, _, _, _, _, _),
        optimize_in_call_stmt(OptInfo, Stmt0, Stmt)
    ;
        Stmt0 = ml_stmt_block(Defns0, Statements0),
        maybe_convert_assignments_into_initializers(OptInfo,
            Defns0, Defns1, Statements0, Statements1),
        maybe_eliminate_locals(OptInfo, Defns1, Defns,
            Statements1, Statements2),
        maybe_flatten_block(Statements2, Statements3),
        optimize_in_statements(OptInfo, Statements3, Statements),
        Stmt = ml_stmt_block(Defns, Statements)
    ;
        Stmt0 = ml_stmt_while(Kind, Rval, Statement0),
        optimize_in_statement(OptInfo, Statement0, Statement),
        Stmt = ml_stmt_while(Kind, Rval, Statement)
    ;
        Stmt0 = ml_stmt_if_then_else(Rval, Then0, MaybeElse0),
        optimize_in_statement(OptInfo, Then0, Then),
        optimize_in_maybe_statement(OptInfo, MaybeElse0, MaybeElse),
        Stmt = ml_stmt_if_then_else(Rval, Then, MaybeElse)
    ;
        Stmt0 = ml_stmt_switch(Type, Rval, Range, Cases0, Default0),
        list.map(optimize_in_case(OptInfo), Cases0, Cases),
        optimize_in_default(OptInfo, Default0, Default),
        Stmt = ml_stmt_switch(Type, Rval, Range, Cases, Default)
    ;
        Stmt0 = ml_stmt_try_commit(Ref, TryGoal0, HandlerGoal0),
        optimize_in_statement(OptInfo, TryGoal0, TryGoal),
        optimize_in_statement(OptInfo, HandlerGoal0, HandlerGoal),
        Stmt = ml_stmt_try_commit(Ref, TryGoal, HandlerGoal)
    ;
        ( Stmt0 = ml_stmt_do_commit(_)
        ; Stmt0 = ml_stmt_return(_)
        ; Stmt0 = ml_stmt_label(_Label)
        ; Stmt0 = ml_stmt_goto(_Label)
        ; Stmt0 = ml_stmt_computed_goto(_Rval, _Label)
        ; Stmt0 = ml_stmt_atomic(_Atomic)
        ),
        Stmt = Stmt0
    ).

:- pred optimize_in_case(opt_info::in,
    mlds_switch_case::in, mlds_switch_case::out) is det.

optimize_in_case(OptInfo, Case0, Case) :-
    Case0 = mlds_switch_case(FirstCond, LaterConds, Statement0),
    optimize_in_statement(OptInfo, Statement0, Statement),
    Case = mlds_switch_case(FirstCond, LaterConds, Statement).

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
        Default0 = default_case(Statement0),
        optimize_in_statement(OptInfo, Statement0, Statement),
        Default = default_case(Statement)
    ).

%-----------------------------------------------------------------------------%

:- pred optimize_in_call_stmt(opt_info::in,
    mlds_stmt::in(ml_stmt_is_call), mlds_stmt::out) is det.

optimize_in_call_stmt(OptInfo, Stmt0, Stmt) :-
    Stmt0 = ml_stmt_call(_Signature, FuncRval, _MaybeObject, CallArgs,
        _Results, _IsTailCall, _Markers),
    % If we have a self-tailcall, assign to the arguments and
    % then goto the top of the tailcall loop.
    Globals = OptInfo ^ oi_globals,
    globals.lookup_bool_option(Globals, optimize_tailcalls, OptTailCalls),
    ( if
        OptTailCalls = yes,
        ModuleName = OptInfo ^ oi_module_name,
        EntityName = OptInfo ^ oi_entity_name,
        stmt_is_self_recursive_call_replaceable_with_jump_to_top(ModuleName,
            EntityName, Stmt0)
    then
        Context = OptInfo ^ oi_context,
        CommentStatement = statement(
            ml_stmt_atomic(comment("direct tailcall eliminated")),
            Context),
        GotoStatement = statement(ml_stmt_goto(tailcall_loop_top(Globals)),
            Context),
        OptInfo ^ oi_func_params = mlds_func_params(FuncArgs, _RetTypes),
        generate_assign_args(OptInfo, FuncArgs, CallArgs,
            AssignStatements, AssignDefns),
        % XXX MLDS_DEFN
        AssignVarsStatement = statement(ml_stmt_block(
            list.map(wrap_data_defn, AssignDefns), AssignStatements),
            Context),

        CallReplaceStatements = [CommentStatement, AssignVarsStatement,
            GotoStatement],
        Stmt = ml_stmt_block([], CallReplaceStatements)
    else if
        % Convert calls to `mark_hp' and `restore_hp' to the corresponding
        % MLDS instructions. This ensures that they get generated as
        % inline code. (Without this they won't, since HLDS inlining doesn't
        % get run again after the add_heap_ops pass that adds these calls.)
        % This approach is better than running HLDS inlining again,
        % because it cheaper in compilation time.

        FuncRval = ml_const(mlconst_code_addr(
            code_addr_proc(qual(ModName, module_qual, ProcLabel),
                _FuncSignature))),
        ProcLabel = mlds_proc_label(PredLabel, _ProcId),
        PredLabel = mlds_user_pred_label(pf_predicate, _DefnModName, PredName,
            _Arity, _CodeModel, _NonOutputFunc),
        (
            PredName = "mark_hp",
            CallArgs = [ml_mem_addr(Lval)],
            AtomicStmt = mark_hp(Lval)
        ;
            PredName = "restore_hp",
            CallArgs = [Rval],
            AtomicStmt = restore_hp(Rval)
        ),
        PrivateBuiltin = mercury_private_builtin_module,
        ModName = mercury_module_name_to_mlds(PrivateBuiltin)
    then
        Stmt = ml_stmt_atomic(AtomicStmt)
    else
        Stmt = Stmt0
    ).

    % This specifies how we should branch to the top of the loop
    % introduced by tailcall optimization.
    %
:- func tailcall_loop_top(globals) = mlds_goto_target.

tailcall_loop_top(Globals) = Target :-
    SupportsBreakContinue =
        globals_target_supports_break_and_continue(Globals),
    (
        SupportsBreakContinue = yes,
        % The function body has been wrapped inside
        % `while (true) { ... break; }', and so to branch to the top of the
        % function, we just do a `continue' which will continue the next
        % iteration of the loop.
        Target = goto_continue
    ;
        SupportsBreakContinue = no,
        % A label has been inserted at the start of the function, and so to
        % branch to the top of the function, we just branch to that label.
        Target = goto_label(tailcall_loop_label_name)
    ).

    % The label name we use for the top of the loop introduced by
    % tailcall optimization, when we are doing it with labels & gotos.
    %
:- func tailcall_loop_label_name = string.

tailcall_loop_label_name = "loop_top".

%----------------------------------------------------------------------------

    % Assign the specified list of rvals to the arguments.
    % This is used as part of tail recursion optimization (see above).
    %
:- pred generate_assign_args(opt_info::in, list(mlds_argument)::in,
    list(mlds_rval)::in, list(statement)::out, list(mlds_data_defn)::out)
    is det.

generate_assign_args(_, [], [], [], []).
generate_assign_args(_, [_|_], [], [], []) :-
    unexpected($module, $pred, "length mismatch").
generate_assign_args(_, [], [_|_], [], []) :-
    unexpected($module, $pred, "length mismatch").
generate_assign_args(OptInfo, [Arg | Args], [ArgRval | ArgRvals],
        Statements, TempDefns) :-
    Arg = mlds_argument(VarName, Type, _ArgGCStatement),
    ModuleName = OptInfo ^ oi_module_name,
    QualVarName = qual(ModuleName, module_qual, VarName),
    ( if
        % Don't bother assigning a variable to itself.
        ArgRval = ml_lval(ml_var(QualVarName, _VarType))
    then
        generate_assign_args(OptInfo, Args, ArgRvals, Statements, TempDefns)
    else
        % Declare a temporary variable to hold the new value of the argument,
        % assign it the new value, recursively process the remaining args,
        % and then assign the new value to the parameter's actual variable:
        %
        %   SomeType new_value_of_argN;
        %   new_value_of_argN = <actual new value of argN>
        %   ...
        %   argN = new_value_of_argN;
        %
        % The temporaries are needed for the case where we are e.g.
        % assigning v1, v2 to v2, v1; they ensure that we don't try
        % to reference the old value of a parameter after it has already
        % been clobbered by the new value.
        %
        % Note that we have to use an assignment rather than an initializer
        % to initialize the temp, because this pass comes before
        % ml_elem_nested.m, and ml_elim_nested.m doesn't handle code
        % containing initializers.
        % XXX We should teach it to handle them.

        ( if VarName = mlds_prog_var(VarNameStr, VarNum) then
            NextValueName = mlds_prog_var_next_value(VarNameStr, VarNum)
        else
            % This should not happen; the head variables of a procedure
            % should all be mlds_prog_vars, even the ones representing
            % the typeinfos and typeclassinfos added by the compiler.
            % However, better safe than sorry.
            VarNameStr = ml_var_name_to_string(VarName),
            NextValueName =
                mlds_comp_var(mcv_non_prog_var_next_value(VarNameStr))
        ),
        QualNextValueName = qual(ModuleName, module_qual, NextValueName),
        Initializer = no_initializer,
        % We don't need to trace the temporary variables for GC, since they
        % are not live across a call or a heap allocation.
        GCStatement = gc_no_stmt,
        Context = OptInfo ^ oi_context,
        TempDefn = ml_gen_mlds_var_decl_init(mlds_data_var(NextValueName),
            Type, Initializer, GCStatement, Context),
        NextValueInitStatement = statement(
            ml_stmt_atomic(assign(ml_var(QualNextValueName, Type), ArgRval)),
            Context),
        AssignStatement = statement(
            ml_stmt_atomic(assign(
                ml_var(QualVarName, Type),
                ml_lval(ml_var(QualNextValueName, Type)))),
            Context),
        generate_assign_args(OptInfo, Args, ArgRvals, Statements0, TempDefns0),
        Statements =
            [NextValueInitStatement | Statements0] ++ [AssignStatement],
        TempDefns = [TempDefn | TempDefns0]
    ).

%----------------------------------------------------------------------------

:- pred optimize_func(opt_info::in,
    mlds_function_body::in, mlds_function_body::out) is det.

optimize_func(OptInfo, Body0, Body) :-
    (
        Body0 = body_external,
        Body = body_external
    ;
        Body0 = body_defined_here(Statement0),
        optimize_func_stmt(OptInfo, Statement0, Statement),
        Body = body_defined_here(Statement)
    ).

:- pred optimize_func_stmt(opt_info::in,
    statement::in, statement::out) is det.

optimize_func_stmt(OptInfo, Statement0, Statement) :-
    Statement0 = statement(Stmt0, Context),

    Globals = OptInfo ^ oi_globals,
    ( if
        globals.lookup_bool_option(Globals, optimize_tailcalls, yes),
        % Does Stmt0 contain a call that we will turn into a tail call?
        %
        % We *could* avoid the non-deterministic enumeration of all the
        % statements nested inside Stmt0 by (a) optimizing Stmt0 first,
        % and (b) recording whether we *did* turn a call into a tail call.
        % However, at the moment the optimize_in_* predicates don't return
        % any outputs, so making them return this flag would have a
        % nontrivial cost. At the moment, the nondet enumeration here
        % is not expensive enough to warrant testing whether using the flag
        % would be faster.
        ModuleName = OptInfo ^ oi_module_name,
        EntityName = OptInfo ^ oi_entity_name,
        some [Call] (
            stmt_contains_statement(Stmt0, SubStatement),
            SubStatement = statement(SubStmt, _),
            stmt_is_self_recursive_call_replaceable_with_jump_to_top(
                ModuleName, EntityName, SubStmt)
        )
    then
        Comment = ml_stmt_atomic(comment("tailcall optimized into a loop")),
        CommentStmt = statement(Comment, Context),
        % The loop can be defined either using while, break, and continue,
        % or using a label and goto. We prefer to use the former, if possible,
        % since it is a higher-level construct that may help the back-end
        % compiler's optimizer.
        SupportsBreakContinue =
            globals_target_supports_break_and_continue(Globals),
        (
            SupportsBreakContinue = yes,
            % Wrap a while loop around the function body:
            %
            %   while (true) {
            %       /* tailcall optimized into a loop */
            %       <function body goes here>
            %       break;
            %   }
            %
            % Any tail calls in the function body will be replaced
            % with `continue' statements.
            Stmt = ml_stmt_while(may_loop_zero_times, ml_const(mlconst_true),
                statement(ml_stmt_block([],
                    [CommentStmt,
                    statement(Stmt0, Context),
                    statement(ml_stmt_goto(goto_break), Context)]),
                Context))
        ;
            SupportsBreakContinue = no,
            % Add a loop_top label at the start of the function body:
            %
            %   {
            %   loop_top:
            %       /* tailcall optimized into a loop */
            %       <function body goes here>
            %   }
            %
            % Any tail calls in the function body will be replaced
            % with `goto loop_top' statements.
            Label = ml_stmt_label(tailcall_loop_label_name),
            Stmt = ml_stmt_block([],
                [CommentStmt,
                statement(Label, Context),
                statement(Stmt0, Context)])
        )
    else
        Stmt = Stmt0
    ),
    Statement = statement(Stmt, Context).

    % stmt_is_self_recursive_call_replaceable_with_jump_to_top(ModuleName,
    %   FuncName, Stmt):
    %
    % True if Stmt is a directly recursive call
    % (to qual(ModuleName, module_qual, FuncName)) which we can optimize
    % into a jump back to the start of the function.
    %
:- pred stmt_is_self_recursive_call_replaceable_with_jump_to_top(
    mlds_module_name::in, mlds_entity_name::in, mlds_stmt::in) is semidet.

stmt_is_self_recursive_call_replaceable_with_jump_to_top(ModuleName, FuncName,
        Stmt) :-
    Stmt = ml_stmt_call(_Signature, CalleeRval, MaybeObject, _CallArgs,
        _Results, CallKind, _Markers),

    % Check if this call has been marked by ml_tailcall.m as one that
    % can be optimized as a tail call.
    %
    % Note that a no_return_call may allocate lots of stack frames
    % before it aborts the program. Optimizing such calls as tail calls
    % allows the running program to give the user the program's *intended*
    % abort message, not a message about stack exhaustion.
    (
        ( CallKind = tail_call
        ; CallKind = no_return_call
        ),
        CallKindIsReplaceable = yes
    ;
        CallKind = ordinary_call,
        CallKindIsReplaceable = no
    ),
    CallKindIsReplaceable = yes,

    % In C++, `this' is a constant, so our usual technique of assigning
    % the arguments won't work if it is a member function. Thus we don't do
    % this optimization if we are optimizing a member function call.
    % XXX We don't generate managed C++ anymore. Is this a problem for
    % the other MLDS target languages?
    MaybeObject = no,

    % Is this a self-recursive call?
    % We test this *after* we test CallKind, because the CallKind test
    % is both (a) cheaper, and (b) significantly more likely to fail.
    CalleeRval = ml_const(mlconst_code_addr(CalleeCodeAddr)),
    code_address_is_for_this_function(CalleeCodeAddr, ModuleName, FuncName).

%-----------------------------------------------------------------------------%

    % If the list of statements contains a block with no local variables,
    % then bring the block up one level. This optimization is needed to avoid
    % a compiler limit in the Microsoft C compiler (version 13.10.3077) for
    % too deeply nested blocks.
    %
:- pred maybe_flatten_block(list(statement)::in, list(statement)::out) is det.

maybe_flatten_block(!Stmts) :-
    !:Stmts = list.condense(list.map(flatten_block, !.Stmts)).

:- func flatten_block(statement) = list(statement).

flatten_block(Statement) = Statements :-
    ( if Statement = statement(ml_stmt_block([], BlockStatements), _) then
        Statements = BlockStatements
    else
        Statements = [Statement]
    ).

%-----------------------------------------------------------------------------%

:- pred peephole_opt_statements(list(statement)::in, list(statement)::out)
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

:- pred peephole_opt_statement(statement::in, statement::in,
    list(statement)::in, list(statement)::out) is semidet.

peephole_opt_statement(Statement0, Statement1, Statements2, Statements) :-
    Statement0 = statement(Stmt0, Context0),
    Statement1 = statement(Stmt1, _Context1),
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

        Stmt0 = ml_stmt_if_then_else(TestRval, StmtThen0, MaybeStmtElse0),
        Stmt1 = ml_stmt_if_then_else(TestRval, StmtThen1, MaybeStmtElse1),
        find_rval_component_lvals(TestRval, set.init, TestRvalComponents),
        statement_affects_lvals(TestRvalComponents, StmtThen0, no),
        (
            MaybeStmtElse0 = no
        ;
            MaybeStmtElse0 = yes(StmtElse0),
            statement_affects_lvals(TestRvalComponents, StmtElse0, no)
        )
    then
        StmtThen0 = statement(_, ContextT),
        Then = statement(ml_stmt_block([], [StmtThen0, StmtThen1]), ContextT),
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
                MaybeElse = yes(statement(
                    ml_stmt_block([], [Else0, Else1]), Context0))
            )
        ),
        Stmt = ml_stmt_if_then_else(TestRval, Then, MaybeElse),
        Statement = statement(Stmt, Context0),
        Statements = [Statement | Statements2]
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
        Rval = ml_unop(_, SubRvalA),
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
        ; Rval = ml_vector_common_row(_, _)
        ; Rval = ml_self(_)
        )
    ).

:- pred find_lval_component_lvals(mlds_lval::in,
    set(mlds_lval)::in, set(mlds_lval)::out) is det.

find_lval_component_lvals(Lval, !Components) :-
    (
        Lval = ml_field(_, Rval, _, _, _),
        find_rval_component_lvals(Rval, !Components)
    ;
        Lval = ml_mem_ref(Rval, _),
        find_rval_component_lvals(Rval, !Components)
    ;
        Lval = ml_global_var_ref(_)
    ;
        Lval = ml_var(_, _)
    ).

:- pred statement_affects_lvals(set(mlds_lval)::in,
    statement::in, bool::out) is det.

statement_affects_lvals(Lvals, Statement, Affects) :-
    Statement = statement(Stmt, _),
    (
        Stmt = ml_stmt_block(_, Statements),
        statements_affect_lvals(Lvals, Statements, Affects)
    ;
        Stmt = ml_stmt_while(_, _, SubStatement),
        statement_affects_lvals(Lvals, SubStatement, Affects)
    ;
        Stmt = ml_stmt_if_then_else(_, Then, MaybeElse),
        (
            MaybeElse = no,
            Statements = [Then]
        ;
            MaybeElse = yes(Else),
            Statements = [Then, Else]
        ),
        statements_affect_lvals(Lvals, Statements, Affects)
    ;
        Stmt = ml_stmt_switch(_, _, _, Cases, Default),
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
                Default = default_case(DefaultStatement),
                statement_affects_lvals(Lvals, DefaultStatement, Affects)
            )
        )
    ;
        Stmt = ml_stmt_label(_),
        Affects = no
    ;
        ( Stmt = ml_stmt_goto(_)
        ; Stmt = ml_stmt_computed_goto(_, _)
        ; Stmt = ml_stmt_try_commit(_, _, _)
        ; Stmt = ml_stmt_do_commit(_)
        ; Stmt = ml_stmt_return(_)
        ),
        Affects = yes
    ;
        Stmt = ml_stmt_call(_, _, _, _, _, _, _),
        % A call can update local variables even without referring to them
        % explicitly, by referring to the environment in which they reside.
        Affects = yes
    ;
        Stmt = ml_stmt_atomic(AtomicStmt),
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
            ; AtomicStmt = new_object(Lval, _, _, _, _, _, _, _, _, _)
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
    list(statement)::in, bool::out) is det.

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
    Head = mlds_switch_case(_, _, Statement),
    statement_affects_lvals(Lvals, Statement, HeadAffects),
    (
        HeadAffects = yes,
        Affects = yes
    ;
        HeadAffects = no,
        cases_affect_lvals(Lvals, Tail, Affects)
    ).

%-----------------------------------------------------------------------------%

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
    list(mlds_defn)::in, list(mlds_defn)::out,
    list(statement)::in, list(statement)::out) is det.

maybe_convert_assignments_into_initializers(OptInfo, !Defns, !Statements) :-
    Globals = OptInfo ^ oi_globals,
    % Check if --optimize-initializations is enabled.
    globals.lookup_bool_option(Globals, optimize_initializations, OptInit),
    (
        OptInit = yes,
        convert_assignments_into_initializers(OptInfo, !Defns, !Statements)
    ;
        OptInit = no
    ).

:- pred convert_assignments_into_initializers(opt_info::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    list(statement)::in, list(statement)::out) is det.

convert_assignments_into_initializers(OptInfo, !Defns, !Statements) :-
    ( if
        % Check if the first statement in the block is an assignment to one
        % of the variables declared in the block.
        !.Statements = [AssignStatement | !:Statements],
        AssignStatement = statement(ml_stmt_atomic(assign(LHS, RHS)), _),
        LHS = ml_var(ThisVar, _ThisType),
        ThisVar = qual(Qualifier, QualKind, VarName),
        ThisData = qual(Qualifier, QualKind, mlds_data_var(VarName)),

        % We must check that the value being assigned doesn't refer to the
        % variable itself.
        rval_contains_var(RHS, ThisData) = no,

        % We must check that the value being assigned doesn't refer to any
        % of the variables which are declared after this one. We must also
        % check that the initializers (if any) of the variables that follow
        % this one don't refer to this variable.
        Qualifier = OptInfo ^ oi_module_name,
        list.drop_while(isnt(var_defn(VarName)), !.Defns,
            [_VarDefn | FollowingDefns]),
        Filter =
            ( pred(OtherDefn::in) is semidet :-
                OtherDefn = mlds_data(mlds_data_defn(OtherVarName, _, _, 
                    _Type, OtherInitializer, _GC)),
                (
                    QualOtherVar = qual(Qualifier, QualKind, OtherVarName),
                    rval_contains_var(RHS, QualOtherVar) = yes
                ;
                    initializer_contains_var(OtherInitializer, ThisData) = yes
                )
            ),
        not list.find_first_match(Filter, FollowingDefns, _)
    then
        % Replace the assignment statement with an initializer
        % on the variable declaration.
        set_initializer(VarName, RHS, !Defns),

        % Now try to apply the same optimization again.
        convert_assignments_into_initializers(OptInfo, !Defns, !Statements)
    else
        % No optimization possible -- leave the block unchanged.
        true
    ).

:- pred var_defn(mlds_var_name::in, mlds_defn::in) is semidet.

var_defn(VarName, Defn) :-
    defn_entity_name(Defn) = entity_data(mlds_data_var(VarName)).

    % set_initializer(VarName, Rval, Defns0, Defns):
    %
    % Finds the first definition of the specified variable in Defns0,
    % and replaces the initializer of that definition with init_obj(Rval).
    %
:- pred set_initializer(mlds_var_name::in, mlds_rval::in,
    list(mlds_defn)::in, list(mlds_defn)::out) is det.

set_initializer(_, _, [], _) :-
    unexpected($module, $pred, "var not found").
set_initializer(VarName, Rval, [Defn0 | Defns0], [Defn | Defns]) :-
    ( if
        Defn0 = mlds_data(DataDefn0),
        DataDefn0 = mlds_data_defn(Name, Context, Flags, Type,
            _OldInitializer, GCStatement),
        Name = mlds_data_var(VarName)
    then
        DataDefn = mlds_data_defn(Name, Context, Flags, Type,
            init_obj(Rval), GCStatement),
        Defn = mlds_data(DataDefn),
        Defns = Defns0
    else
        Defn = Defn0,
        set_initializer(VarName, Rval, Defns0, Defns)
    ).

%-----------------------------------------------------------------------------%
%
% This is a pass to eliminate initialized local variable definitions,
% by substituting the value of the initializer for occurrences of the variable.
%
% XXX This is quadratic in the number of variable definitions, since we do
% one pass over the block per variable definition. A more efficient algorithm
% would be to do one pass to figure out which variables could be eliminated,
% and then do another pass to actually eliminate them.

:- pred maybe_eliminate_locals(opt_info::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    list(statement)::in, list(statement)::out) is det.

maybe_eliminate_locals(OptInfo, !Defns, !Statements) :-
    globals.lookup_bool_option(OptInfo ^ oi_globals, eliminate_local_vars,
        EliminateLocalVars),
    (
        EliminateLocalVars = yes,
        eliminate_locals(OptInfo, !Defns, !Statements)
    ;
        EliminateLocalVars = no
    ).

:- pred eliminate_locals(opt_info::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    list(statement)::in, list(statement)::out) is det.

eliminate_locals(_OptInfo, [], [], !Statements).
eliminate_locals(OptInfo, [Defn0 | Defns0], Defns, !Statements) :-
    ( if
        try_to_eliminate_defn(OptInfo, Defn0, Defns0, Defns1, !Statements)
    then
        eliminate_locals(OptInfo, Defns1, Defns, !Statements)
    else
        eliminate_locals(OptInfo, Defns0, Defns2, !Statements),
        Defns = [Defn0 | Defns2]
    ).

    % This data structure holds information that we use in this pass
    % to eliminate initialized local variable definitions.
:- type var_elim_info
    --->    var_elim_info(
                % These fields remain constant.

                % The name of the variable to eliminate.
                var_name        :: mlds_var,

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
:- pred try_to_eliminate_defn(opt_info::in, mlds_defn::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    list(statement)::in, list(statement)::out) is semidet.

try_to_eliminate_defn(OptInfo, Defn0, Defns0, Defns, !Statements) :-
    Defn0 = mlds_data(DataDefn0),
    DataDefn0 = mlds_data_defn(Name, _Context, Flags,
        _Type, Initializer, _GCStatement),

    % Check if this definition is a local variable definition...
    Name = mlds_data_var(VarName),
    Flags = ml_gen_local_var_decl_flags,

    % ... with a known initial value.
    QualVarName = qual(OptInfo ^ oi_module_name, module_qual, VarName),
    (
        Initializer = init_obj(Rval)
    ;
        Initializer = no_initializer,
        find_initial_val_in_statements(QualVarName, Rval, !Statements)
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
    % (we can't tell here whether this MLDS code came from a conjunction
    % or a disjunction) is allowed.
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
    eliminate_var(QualVarName, Rval, Defns0, Defns, !Statements,
        Count, Invalidated),
    Invalidated = no,

    % Make sure that we didn't duplicate the rval, unless it is just a constant
    % or a variable, because duplicating any real operation would be
    % a pessimization.
    ( Count =< 1
    ; rval_is_cheap_enough_to_duplicate(Rval)
    ).

:- pred rval_is_cheap_enough_to_duplicate(mlds_rval::in) is semidet.

rval_is_cheap_enough_to_duplicate(Rval) :-
    require_complete_switch [Rval]
    (
        ( Rval = ml_const(_)
        ; Rval = ml_mem_addr(_)
        ; Rval = ml_self(_)
        ; Rval = ml_scalar_common(_)
        ; Rval = ml_vector_common_row(_, _)
        )
    ;
        Rval = ml_lval(Lval),
        require_complete_switch [Lval]
        (
            Lval = ml_var(_, _)
        ;
            ( Lval = ml_mem_ref(_, _)
            ; Lval = ml_field(_, _, _, _, _)
            ; Lval = ml_global_var_ref(_)
            ),
            fail
        )
    ;
        ( Rval = ml_mkword(_, _)
        ; Rval = ml_unop(_, _)
        ; Rval = ml_binop(_, _, _)
        ),
        fail
    ).

    % Succeed only if the specified rval definitely won't change in value.
    %
:- pred rval_will_not_change(mlds_rval::in) is semidet.

rval_will_not_change(Rval) :-
    require_complete_switch [Rval]
    (
        ( Rval = ml_const(_)
        ; Rval = ml_scalar_common(_)
        ; Rval = ml_vector_common_row(_, _)
        )
    ;
        Rval = ml_mem_addr(Lval),
        require_complete_switch [Lval]
        (
            Lval = ml_var(_, _)
        ;
            ( Lval = ml_mem_ref(Address, _Type)
            ; Lval = ml_field(_, Address, _, _, _)
            ),
            rval_will_not_change(Address)
        ;
            Lval = ml_global_var_ref(_),
            fail
        )
    ;
        ( Rval = ml_mkword(_Tag, SubRval)
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
        ; Rval = ml_vector_common_row(_, _)
        ; Rval = ml_self(_)
        ; Rval = ml_mem_addr(_)
        )
    ;
        Rval = ml_mkword(_Tag, SubRval),
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
:- pred find_initial_val_in_statements(mlds_var::in, mlds_rval::out,
    list(statement)::in, list(statement)::out) is semidet.

find_initial_val_in_statements(VarName, Rval, [Statement0 | Statements0],
        Statements) :-
    ( if
        find_initial_val_in_statement(VarName, Rval1, Statement0, Statement1)
    then
        Rval = Rval1,
        ( if Statement1 = statement(ml_stmt_block([], []), _) then
            Statements = Statements0
        else
            Statements = [Statement1 | Statements0]
        )
    else
        % Check that Statement0 doesn't modify the value of the variable
        % -- this includes checking that there are no labels via which code
        % could branch into the middle of Statement0. Only if we are sure
        % that Statement0 can't modify the variable's value is it safe to go on
        % and look for the initial value in Statements0.
        VarName = qual(Mod, QualKind, UnqualVarName),
        DataName = qual(Mod, QualKind, mlds_data_var(UnqualVarName)),
        statement_contains_var(Statement0, DataName) = no,
        not (
            statement_contains_statement(Statement0, Label),
            Label = statement(ml_stmt_label(_), _)
        ),
        find_initial_val_in_statements(VarName, Rval,
            Statements0, Statements1),
        Statements = [Statement0 | Statements1]
    ).

:- pred find_initial_val_in_statement(mlds_var::in, mlds_rval::out,
    statement::in, statement::out) is semidet.

find_initial_val_in_statement(Var, Rval, Statement0, Statement) :-
    Statement0 = statement(Stmt0, Context),
    ( if Stmt0 = ml_stmt_atomic(assign(ml_var(Var, _Type), Rval0)) then
        Rval = Rval0,
        % Delete the assignment, by replacing it with an empty block.
        Stmt = ml_stmt_block([], [])
    else if Stmt0 = ml_stmt_block(Defns0, SubStatements0) then
        Var = qual(Mod, QualKind, UnqualVarName),
        Data = qual(Mod, QualKind, mlds_data_var(UnqualVarName)),
        defns_contains_var(Defns0, Data) = no,
        find_initial_val_in_statements(Var, Rval,
            SubStatements0, SubStatements),
        Stmt = ml_stmt_block(Defns0, SubStatements)
    else
        fail
    ),
    Statement = statement(Stmt, Context).

    % Replace uses of this variable with the variable's value in the specified
    % definitions and statements. This will return a count of how many
    % occurrences of the variable there were. It will also return
    % Invalidated = yes if any of the statements or definitions take
    % the address of the variable, or assign to it; in that case, the
    % transformation should not be performed.
    %
:- pred eliminate_var(mlds_var::in, mlds_rval::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    list(statement)::in, list(statement)::out,
    int::out, bool::out) is det.

eliminate_var(QualVarName, VarRval, !Defns, !Statements, Count, Invalidated) :-
    Count0 = 0,
    Invalidated0 = no,
    VarElimInfo0 = var_elim_info(QualVarName, VarRval, Count0, Invalidated0),
    eliminate_var_in_block(!Defns, !Statements, VarElimInfo0, VarElimInfo),
    Count = VarElimInfo ^ replace_count,
    Invalidated = VarElimInfo ^ invalidated.

% eliminate_var_in_*:
%
% Process the specified construct, replacing all rvalue occurrences of the
% variable (^var_name) with its value (^var_value), incrementing the
% ^replace_count field for each occurrence as an rvalue, and setting
% ^invalidated to yes if the variable occurs as an lvalue.

:- pred eliminate_var_in_block(list(mlds_defn)::in, list(mlds_defn)::out,
    list(statement)::in, list(statement)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_block(!Defns, !Statements, !VarElimInfo) :-
    eliminate_var_in_defns(!Defns, !VarElimInfo),
    eliminate_var_in_statements(!Statements, !VarElimInfo).

:- pred eliminate_var_in_defns(list(mlds_defn)::in, list(mlds_defn)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_defns(!Defns, !VarElimInfo) :-
    list.map_foldl(eliminate_var_in_defn, !Defns, !VarElimInfo).

:- pred eliminate_var_in_defn(mlds_defn::in, mlds_defn::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_defn(Defn0, Defn, !VarElimInfo) :-
    (
        Defn0 = mlds_data(DataDefn0),
        DataDefn0 = mlds_data_defn(Name, Context, Flags,
            Type, Initializer0, GCStatement),
        eliminate_var_in_initializer(Initializer0, Initializer, !VarElimInfo),
        DataDefn = mlds_data_defn(Name, Context, Flags,
            Type, Initializer, GCStatement),
        Defn = mlds_data(DataDefn)
    ;
        Defn0 = mlds_class(_),
        % We assume that nested classes don't refer to local variables
        % in the containing scope.
        Defn = Defn0
    ;
        Defn0 = mlds_function(FunctionDefn0),
        FunctionDefn0 = mlds_function_defn(Name, Context, Flags, Id, Params,
            Body0, Attributes, EnvVarNames, MaybeRequireTailrecInfo),
        eliminate_var_in_function_body(Body0, Body, !VarElimInfo),
        FunctionDefn = mlds_function_defn(Name, Context, Flags, Id, Params,
            Body, Attributes, EnvVarNames, MaybeRequireTailrecInfo),
        Defn = mlds_function(FunctionDefn)
    ).

:- pred eliminate_var_in_function_body(
    mlds_function_body::in, mlds_function_body::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_function_body(Body0, Body, !VarElimInfo) :-
    (
        Body0 = body_external,
        Body = Body0
    ;
        Body0 = body_defined_here(Stmt0),
        eliminate_var_in_statement(Stmt0, Stmt, !VarElimInfo),
        Body = body_defined_here(Stmt)
    ).

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

:- pred eliminate_var_in_rvals(list(mlds_rval)::in, list(mlds_rval)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_rvals(!Rvals, !VarElimInfo) :-
    list.map_foldl(eliminate_var_in_rval, !Rvals, !VarElimInfo).

:- pred eliminate_var_in_maybe_rval(
    maybe(mlds_rval)::in, maybe(mlds_rval)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_maybe_rval(no, no, !VarElimInfo).
eliminate_var_in_maybe_rval(yes(Rval0), yes(Rval), !VarElimInfo) :-
    eliminate_var_in_rval(Rval0, Rval, !VarElimInfo).

:- pred eliminate_var_in_rval(mlds_rval::in, mlds_rval::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_rval(Rval0, Rval, !VarElimInfo) :-
    (
        Rval0 = ml_lval(Lval0),
        VarName = !.VarElimInfo ^ var_name,
        ( if Lval0 = ml_var(VarName, _) then
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
        Rval0 = ml_vector_common_row(VectorCommon, RowRval0),
        eliminate_var_in_rval(RowRval0, RowRval, !VarElimInfo),
        Rval = ml_vector_common_row(VectorCommon, RowRval)
    ;
        ( Rval0 = ml_const(_)
        ; Rval0 = ml_scalar_common(_)
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
        Lval0 = ml_field(MaybeTag, Rval0, FieldId, FieldType, PtrType),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Lval = ml_field(MaybeTag, Rval, FieldId, FieldType, PtrType)
    ;
        Lval0 = ml_mem_ref(Rval0, Type),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Lval = ml_mem_ref(Rval, Type)
    ;
        Lval0 = ml_global_var_ref(_Ref),
        Lval = Lval0
    ;
        Lval0 = ml_var(VarName, _Type),
        ( if VarName = !.VarElimInfo ^ var_name then
            % We found an lvalue occurrence of the variable.
            % If the variable that we are trying to eliminate has its
            % address is taken, or is assigned to, or in general if it is
            % used as an lvalue, then it is NOT safe to eliminate it.
            !VarElimInfo ^ invalidated := yes
        else
            true
        ),
        Lval = Lval0
    ).

:- pred eliminate_var_in_statements(
    list(statement)::in, list(statement)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_statements(!Statements, !VarElimInfo) :-
    list.map_foldl(eliminate_var_in_statement, !Statements, !VarElimInfo).

:- pred eliminate_var_in_maybe_statement(
    maybe(statement)::in, maybe(statement)::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_maybe_statement(no, no, !VarElimInfo).
eliminate_var_in_maybe_statement(yes(Statement0), yes(Statement),
        !VarElimInfo) :-
    eliminate_var_in_statement(Statement0, Statement, !VarElimInfo).

:- pred eliminate_var_in_statement(statement::in, statement::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_statement(Statement0, Statement, !VarElimInfo) :-
    Statement0 = statement(Stmt0, Context),
    eliminate_var_in_stmt(Stmt0, Stmt, !VarElimInfo),
    Statement = statement(Stmt, Context).

:- pred eliminate_var_in_stmt(mlds_stmt::in, mlds_stmt::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_stmt(Stmt0, Stmt, !VarElimInfo) :-
    (
        Stmt0 = ml_stmt_block(Defns0, Statements0),
        eliminate_var_in_block(Defns0, Defns, Statements0, Statements,
            !VarElimInfo),
        Stmt = ml_stmt_block(Defns, Statements)
    ;
        Stmt0 = ml_stmt_while(Kind, Rval0, Statement0),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        eliminate_var_in_statement(Statement0, Statement, !VarElimInfo),
        Stmt = ml_stmt_while(Kind, Rval, Statement)
    ;
        Stmt0 = ml_stmt_if_then_else(Cond0, Then0, MaybeElse0),
        eliminate_var_in_rval(Cond0, Cond, !VarElimInfo),
        eliminate_var_in_statement(Then0, Then, !VarElimInfo),
        eliminate_var_in_maybe_statement(MaybeElse0, MaybeElse, !VarElimInfo),
        Stmt = ml_stmt_if_then_else(Cond, Then, MaybeElse)
    ;
        Stmt0 = ml_stmt_switch(Type, Val0, Range, Cases0, Default0),
        eliminate_var_in_rval(Val0, Val, !VarElimInfo),
        list.map_foldl(eliminate_var_in_case, Cases0, Cases, !VarElimInfo),
        eliminate_var_in_default(Default0, Default, !VarElimInfo),
        Stmt = ml_stmt_switch(Type, Val, Range, Cases, Default)
    ;
        Stmt0 = ml_stmt_label(_),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_goto(_),
        Stmt = Stmt0
    ;
        Stmt0 = ml_stmt_computed_goto(Rval0, Labels),
        eliminate_var_in_rval(Rval0, Rval, !VarElimInfo),
        Stmt = ml_stmt_computed_goto(Rval, Labels)
    ;
        Stmt0 = ml_stmt_call(Sig, Func0, Obj0, Args0, RetLvals0, TailCall,
            Markers),
        eliminate_var_in_rval(Func0, Func, !VarElimInfo),
        eliminate_var_in_maybe_rval(Obj0, Obj, !VarElimInfo),
        eliminate_var_in_rvals(Args0, Args, !VarElimInfo),
        eliminate_var_in_lvals(RetLvals0, RetLvals, !VarElimInfo),
        Stmt = ml_stmt_call(Sig, Func, Obj, Args, RetLvals, TailCall, Markers)
    ;
        Stmt0 = ml_stmt_return(Rvals0),
        eliminate_var_in_rvals(Rvals0, Rvals, !VarElimInfo),
        Stmt = ml_stmt_return(Rvals)
    ;
        Stmt0 = ml_stmt_do_commit(Ref0),
        eliminate_var_in_rval(Ref0, Ref, !VarElimInfo),
        Stmt = ml_stmt_do_commit(Ref)
    ;
        Stmt0 = ml_stmt_try_commit(Ref0, Statement0, Handler0),
        eliminate_var_in_lval(Ref0, Ref, !VarElimInfo),
        eliminate_var_in_statement(Statement0, Statement, !VarElimInfo),
        eliminate_var_in_statement(Handler0, Handler, !VarElimInfo),
        Stmt = ml_stmt_try_commit(Ref, Statement, Handler)
    ;
        Stmt0 = ml_stmt_atomic(AtomicStmt0),
        eliminate_var_in_atomic_stmt(AtomicStmt0, AtomicStmt, !VarElimInfo),
        Stmt = ml_stmt_atomic(AtomicStmt)
    ).

:- pred eliminate_var_in_case(mlds_switch_case::in, mlds_switch_case::out,
    var_elim_info::in, var_elim_info::out) is det.

eliminate_var_in_case(Case0, Case, !VarElimInfo) :-
    Case0 = mlds_switch_case(FirstCond0, LaterConds0, Statement0),
    eliminate_var_in_case_cond(FirstCond0, FirstCond, !VarElimInfo),
    list.map_foldl(eliminate_var_in_case_cond, LaterConds0, LaterConds,
        !VarElimInfo),
    eliminate_var_in_statement(Statement0, Statement, !VarElimInfo),
    Case = mlds_switch_case(FirstCond, LaterConds, Statement).

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
        Default0 = default_case(Statement0),
        eliminate_var_in_statement(Statement0, Statement, !VarElimInfo),
        Default = default_case(Statement)
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
        Stmt0 = new_object(Target0, MaybeTag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, Args0, ArgTypes, MayUseAtomic,
            MaybeAllocId),
        eliminate_var_in_lval(Target0, Target, !VarElimInfo),
        eliminate_var_in_rvals(Args0, Args, !VarElimInfo),
        Stmt = new_object(Target, MaybeTag, ExplicitSecTag, Type,
            MaybeSize, MaybeCtorName, Args, ArgTypes, MayUseAtomic,
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
        ; Component0 = target_code_name(_Name)
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

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_optimize.
%-----------------------------------------------------------------------------%
