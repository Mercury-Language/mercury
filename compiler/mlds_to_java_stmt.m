%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS statements in Java.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_java_stmt.
:- interface.

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_target_util.
:- import_module ml_backend.mlds_to_java_util.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- pred output_statements_for_java(java_out_info::in, indent::in,
    func_info_csj::in, list(mlds_stmt)::in, exit_methods::out,
    io::di, io::uo) is det.

:- pred output_statement_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in, exit_methods::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module ml_backend.ml_util.
:- import_module ml_backend.mlds_to_java_data.
:- import_module ml_backend.mlds_to_java_func.  % undesirable circular dep
:- import_module ml_backend.mlds_to_java_name.
:- import_module ml_backend.mlds_to_java_type.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_statements_for_java(_, _, _, [], ExitMethods, !IO) :-
    ExitMethods = set.make_singleton_set(can_fall_through).
output_statements_for_java(Info, Indent, FuncInfo, [Stmt | Stmts],
        ExitMethods, !IO) :-
    output_statement_for_java(Info, Indent, FuncInfo, Stmt,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        output_statements_for_java(Info, Indent, FuncInfo, Stmts,
            StmtsExitMethods, !IO),
        ExitMethods0 = set.union(StmtExitMethods, StmtsExitMethods),
        ( if set.member(can_fall_through, StmtsExitMethods) then
            ExitMethods = ExitMethods0
        else
            % If the last statement could not complete normally
            % the current block can no longer complete normally.
            ExitMethods = set.delete(ExitMethods0, can_fall_through)
        )
    else
        % Don't output any more statements from the current list since
        % the previous statement cannot complete.
        ExitMethods = StmtExitMethods
    ).

output_statement_for_java(Info, Indent, FuncInfo, Stmt, ExitMethods, !IO) :-
    Context = get_mlds_stmt_context(Stmt),
    output_context_for_java(Info ^ joi_line_numbers, marker_comment,
        Context, !IO),
    (
        Stmt = ml_stmt_block(_, _, _, _),
        output_stmt_block_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_while(_, _, _, _, _),
        output_stmt_while_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_if_then_else(_, _, _, _),
        output_stmt_if_then_else_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_switch(_, _, _, _, _, _),
        output_stmt_switch_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_label(_, _),
        unexpected($pred, "labels not supported in Java.")
    ;
        Stmt = ml_stmt_goto(Target, _),
        (
            Target = goto_label(_),
            unexpected($pred, "gotos not supported in Java.")
        ;
            Target = goto_break_switch,
            BreakContext = Info ^ joi_break_context,
            (
                BreakContext = bc_switch,
                output_n_indents(Indent, !IO),
                io.write_string("break;\n", !IO),
                ExitMethods = set.make_singleton_set(can_break)
            ;
                ( BreakContext = bc_none
                ; BreakContext = bc_loop
                ),
                unexpected($pred, "goto_break_switch not in switch")
            )
        ;
            Target = goto_break_loop,
            BreakContext = Info ^ joi_break_context,
            (
                BreakContext = bc_loop,
                output_n_indents(Indent, !IO),
                io.write_string("break;\n", !IO),
                ExitMethods = set.make_singleton_set(can_break)
            ;
                ( BreakContext = bc_none
                ; BreakContext = bc_switch
                ),
                unexpected($pred, "goto_break_loop not in loop")
            )
        ;
            Target = goto_continue_loop,
            output_n_indents(Indent, !IO),
            io.write_string("continue;\n", !IO),
            ExitMethods = set.make_singleton_set(can_continue)
        )
    ;
        Stmt = ml_stmt_computed_goto(_, _, _),
        unexpected($pred, "computed gotos not supported in Java.")
    ;
        Stmt = ml_stmt_call(_, _, _, _, _, _),
        output_stmt_call_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_return(_, _),
        output_stmt_return_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_do_commit(_, _),
        output_stmt_do_commit_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_try_commit(_, _, _, _),
        output_stmt_try_commit_for_java(Info, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_atomic(AtomicStmt, _Context),
        output_atomic_stmt_for_java(Info, Indent, AtomicStmt, Context, !IO),
        ExitMethods = set.make_singleton_set(can_fall_through)
    ).

:- pred output_stmt_block_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_block),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_block_for_java/7).

output_stmt_block_for_java(Info, Indent, FuncInfo, Stmt, ExitMethods, !IO) :-
    Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context),
    BraceIndent = Indent,
    BlockIndent = Indent + 1,
    output_n_indents(BraceIndent, !IO),
    io.write_string("{\n", !IO),
    (
        LocalVarDefns = [_ | _],
        list.foldl(
            output_local_var_defn_for_java(Info, BlockIndent, oa_force_init),
            LocalVarDefns, !IO),
        io.write_string("\n", !IO)
    ;
        LocalVarDefns = []
    ),
    (
        FuncDefns = [_ | _],
        list.foldl(
            output_function_defn_for_java(Info, BlockIndent, oa_force_init),
            FuncDefns, !IO),
        io.write_string("\n", !IO)
    ;
        FuncDefns = []
    ),
    output_statements_for_java(Info, BlockIndent, FuncInfo, SubStmts,
        ExitMethods, !IO),
    indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
        Context, BraceIndent, !IO),
    io.write_string("}\n", !IO).

:- pred output_local_var_defn_for_java(java_out_info::in, indent::in,
    output_aux::in, mlds_local_var_defn::in, io::di, io::uo) is det.

output_local_var_defn_for_java(Info, Indent, OutputAux, LocalVarDefn, !IO) :-
    LocalVarDefn = mlds_local_var_defn(LocalVarName, Context, Type,
        Initializer, _),
    indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
        Context, Indent, !IO),
    output_local_var_decl_for_java(Info, LocalVarName, Type, !IO),
    output_initializer_for_java(Info, OutputAux, Type, Initializer, !IO),
    io.write_string(";\n", !IO).

:- pred output_local_var_decl_for_java(java_out_info::in,
    mlds_local_var_name::in, mlds_type::in, io::di, io::uo) is det.

output_local_var_decl_for_java(Info, LocalVarName, Type, !IO) :-
    output_type_for_java(Info, Type, !IO),
    io.write_char(' ', !IO),
    output_local_var_name_for_java(LocalVarName, !IO).

:- pred output_stmt_while_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_while),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_while_for_java/7).

output_stmt_while_for_java(Info, Indent, FuncInfo, Stmt, ExitMethods, !IO) :-
    Stmt = ml_stmt_while(Kind, Cond, BodyStmt, _LoopLocalVars, Context),
    scope_indent(BodyStmt, Indent, ScopeIndent),
    (
        Kind = may_loop_zero_times,
        output_n_indents(Indent, !IO),
        io.write_string("while (", !IO),
        output_rval_for_java(Info, Cond, !IO),
        io.write_string(")\n", !IO),
        % The contained statement is reachable iff the while statement
        % is reachable the condition is not a constant expression
        % whose value is false.
        ( if Cond = ml_const(mlconst_false) then
            output_n_indents(Indent, !IO),
            io.write_string("{\n", !IO),
            output_n_indents(Indent + 1, !IO),
            io.write_string("/* Unreachable code */\n", !IO),
            output_n_indents(Indent, !IO),
            io.write_string("}\n", !IO),
            ExitMethods = set.make_singleton_set(can_fall_through)
        else
            BodyInfo = Info ^ joi_break_context := bc_loop,
            output_statement_for_java(BodyInfo, ScopeIndent, FuncInfo,
                BodyStmt, StmtExitMethods, !IO),
            ExitMethods = while_exit_methods_for_java(Cond, StmtExitMethods)
        )
    ;
        Kind = loop_at_least_once,
        output_n_indents(Indent, !IO),
        io.write_string("do\n", !IO),
        BodyInfo = Info ^ joi_break_context := bc_loop,
        output_statement_for_java(BodyInfo, ScopeIndent, FuncInfo, BodyStmt,
            StmtExitMethods, !IO),
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("while (", !IO),
        output_rval_for_java(Info, Cond, !IO),
        io.write_string(");\n", !IO),
        ExitMethods = while_exit_methods_for_java(Cond, StmtExitMethods)
    ).

:- pred output_stmt_if_then_else_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_if_then_else),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_if_then_else_for_java/7).

output_stmt_if_then_else_for_java(Info, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_if_then_else(Cond, Then0, MaybeElse, Context),
    % We need to take care to avoid problems caused by the dangling else
    % ambiguity.
    ( if
        % For statements of the form
        %
        %   if (...)
        %       if (...)
        %           ...
        %   else
        %       ...
        %
        % we need braces around the inner `if', otherwise they wouldn't
        % parse they way we want them to: Java would match the `else'
        % with the inner `if' rather than the outer `if'.

        MaybeElse = yes(_),
        Then0 = ml_stmt_if_then_else(_, _, no, ThenContext)
    then
        Then = ml_stmt_block([], [], [Then0], ThenContext)
    else
        Then = Then0
    ),

    output_n_indents(Indent, !IO),
    io.write_string("if (", !IO),
    output_rval_for_java(Info, Cond, !IO),
    io.write_string(")\n", !IO),
    scope_indent(Then, Indent, ThenScopeIndent),
    output_statement_for_java(Info, ThenScopeIndent, FuncInfo, Then,
        ThenExitMethods, !IO),
    (
        MaybeElse = yes(Else),
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("else\n", !IO),
        scope_indent(Else, Indent, ElseScopeIndent),
        output_statement_for_java(Info, ElseScopeIndent, FuncInfo, Else,
            ElseExitMethods, !IO),
        % An if-then-else statement can complete normally iff the
        % then-statement can complete normally or the else-statement
        % can complete normally.
        ExitMethods = set.union(ThenExitMethods, ElseExitMethods)
    ;
        MaybeElse = no,
        % An if-then statement can complete normally iff it is reachable.
        ExitMethods = set.insert(ThenExitMethods, can_fall_through)
    ).

:- pred output_stmt_switch_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_switch),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_switch_for_java/7).

output_stmt_switch_for_java(Info, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_switch(_Type, Val, _Range, Cases, Default,
        Context),
    indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
        Context, Indent, !IO),
    io.write_string("switch (", !IO),
    output_rval_maybe_with_enum_for_java(Info, Val, !IO),
    io.write_string(") {\n", !IO),
    CaseInfo = Info ^ joi_break_context := bc_switch,
    output_switch_cases_for_java(CaseInfo, Indent + 1, FuncInfo, Context,
        Cases, Default, ExitMethods, !IO),
    indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
        Context, Indent, !IO),
    io.write_string("}\n", !IO).

:- pred output_stmt_call_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_call),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_call_for_java/7).

output_stmt_call_for_java(Info, Indent, _FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_call(Signature, FuncRval, CallArgs, Results,
        _IsTailCall, Context),
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
        Context, Indent + 1, !IO),
    (
        Results = []
    ;
        Results = [Lval],
        output_lval_for_java(Info, Lval, !IO),
        io.write_string(" = ", !IO)
    ;
        Results = [_, _ | _],
        % for multiple return values,
        % we generate the following code:
        %   { java.lang.Object [] result = <func>(<args>);
        %     <output1> = (<type1>) result[0];
        %     <output2> = (<type2>) result[1];
        %     ...
        %   }
        %
        io.write_string("java.lang.Object [] result = ", !IO)
    ),
    ( if FuncRval = ml_const(mlconst_code_addr(_)) then
        % This is a standard function call.
        output_call_rval_for_java(Info, FuncRval, !IO),
        io.write_string("(", !IO),
        io.write_list(CallArgs, ", ", output_rval_for_java(Info), !IO),
        io.write_string(")", !IO)
    else
        % This is a call using a method pointer.
        %
        % Here we do downcasting, as a call will always return
        % something of type java.lang.Object
        %
        % XXX This is a hack, I can't see any way to do this downcasting
        % nicely, as it needs to effectively be wrapped around the method
        % call itself, so it acts before this predicate's solution to
        % multiple return values, see above.

        (
            RetTypes = []
        ;
            RetTypes = [RetType],
            boxed_type_to_string_for_java(Info, RetType, RetTypeString),
            io.format("((%s) ", [s(RetTypeString)], !IO)
        ;
            RetTypes = [_, _ | _],
            io.write_string("((java.lang.Object[]) ", !IO)
        ),

        list.length(CallArgs, Arity),
        ( if is_specialised_method_ptr_arity(Arity) then
            io.write_string("((jmercury.runtime.MethodPtr", !IO),
            io.write_int(Arity, !IO),
            io.write_string(") ", !IO),
            output_bracketed_rval_for_java(Info, FuncRval, !IO),
            io.write_string(").call___0_0(", !IO),
            output_boxed_args(Info, CallArgs, ArgTypes, !IO)
        else
            io.write_string("((jmercury.runtime.MethodPtrN) ", !IO),
            output_bracketed_rval_for_java(Info, FuncRval, !IO),
            io.write_string(").call___0_0(", !IO),
            output_args_as_array(Info, CallArgs, ArgTypes, !IO)
        ),

        % Closes brackets, and calls unbox methods for downcasting.
        % XXX This is a hack, see the above comment.
        io.write_string(")", !IO),
        (
            RetTypes = []
        ;
            RetTypes = [RetType2],
            ( if java_builtin_type(RetType2, _, _, UnboxMethod) then
                io.write_string(").", !IO),
                io.write_string(UnboxMethod, !IO),
                io.write_string("()", !IO)
            else
                io.write_string(")", !IO)
            )
        ;
            RetTypes = [_, _ | _],
            io.write_string(")", !IO)
        )
    ),
    io.write_string(";\n", !IO),

    ( if Results = [_, _ | _] then
        % Copy the results from the "result" array into the Result lvals
        % (unboxing them as we go).
        output_assign_results(Info, Results, RetTypes, 0, Indent + 1,
            Context, !IO)
    else
        true
    ),
    % XXX Is this needed? If present, it causes compiler errors for a
    % couple of files in the benchmarks directory. -mjwybrow
    %
    % ( if IsTailCall = tail_call, Results = [] then
    %   indent_line_after_context(Context, Indent + 1, !IO),
    %   io.write_string("return;\n", !IO)
    % else
    %   true
    % ),
    %
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO),
    ExitMethods = set.make_singleton_set(can_fall_through).

:- pred output_stmt_return_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_return),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_return_for_java/7).

output_stmt_return_for_java(Info, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_return(Results, _Context),
    output_n_indents(Indent, !IO),
    (
        Results = [],
        io.write_string("return;\n", !IO)
    ;
        Results = [Rval],
        io.write_string("return ", !IO),
        output_rval_for_java(Info, Rval, !IO),
        io.write_string(";\n", !IO)
    ;
        Results = [_, _ | _],
        FuncInfo = func_info_csj(Params),
        Params = mlds_func_params(_Args, ReturnTypes),
        TypesAndResults = assoc_list.from_corresponding_lists(
            ReturnTypes, Results),
        io.write_string("return new java.lang.Object[] {\n", !IO),
        output_n_indents(Indent + 1, !IO),
        Separator = ",\n" ++ duplicate_char(' ', (Indent + 1) * 2),
        io.write_list(TypesAndResults, Separator,
            ( pred((Type - Result)::in, !.IO::di, !:IO::uo) is det :-
                output_boxed_rval_for_java(Info, Type, Result, !IO)
            ), !IO),
        io.write_string("\n", !IO),
        output_n_indents(Indent, !IO),
        io.write_string("};\n", !IO)
    ),
    ExitMethods = set.make_singleton_set(can_return).

:- pred output_stmt_do_commit_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_do_commit),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_do_commit_for_java/7).

output_stmt_do_commit_for_java(Info, Indent, _FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_do_commit(Ref, _Context),
    output_n_indents(Indent, !IO),
    output_rval_for_java(Info, Ref, !IO),
    io.write_string(" = new jmercury.runtime.Commit();\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("throw ", !IO),
    output_rval_for_java(Info, Ref, !IO),
    io.write_string(";\n", !IO),
    ExitMethods = set.make_singleton_set(can_throw).

:- pred output_stmt_try_commit_for_java(java_out_info::in, indent::in,
    func_info_csj::in, mlds_stmt::in(ml_stmt_is_try_commit),
    exit_methods::out, io::di, io::uo) is det.
:- pragma inline(output_stmt_try_commit_for_java/7).

output_stmt_try_commit_for_java(Info, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_try_commit(_Ref, BodyStmt, HandlerStmt, _Context),
    output_n_indents(Indent, !IO),
    io.write_string("try\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    output_statement_for_java(Info, Indent + 1, FuncInfo, BodyStmt,
        TryExitMethods0, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("catch (jmercury.runtime.Commit commit_variable)\n", !IO),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),
    output_n_indents(Indent + 1, !IO),
    output_statement_for_java(Info, Indent + 1, FuncInfo, HandlerStmt,
        CatchExitMethods, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO),
    ExitMethods = set.union(set.delete(TryExitMethods0, can_throw),
        CatchExitMethods).

%---------------------------------------------------------------------------%
%
% Extra code for handling while-loops.
%

:- func while_exit_methods_for_java(mlds_rval, exit_methods) = exit_methods.

while_exit_methods_for_java(Cond, BlockExitMethods) = ExitMethods :-
    % A while statement cannot complete normally if its condition expression
    % is a constant expression with value true, and it doesn't contain
    % a reachable break statement that exits the while statement.
    ( if
        % XXX This is not a sufficient way of testing for a Java
        % "constant expression", though determining these accurately
        % is a little difficult to do here.
        Cond = ml_const(mlconst_true),
        not set.member(can_break, BlockExitMethods)
    then
        % Cannot complete normally.
        ExitMethods0 = set.delete(BlockExitMethods, can_fall_through)
    else
        ExitMethods0 = set.insert(BlockExitMethods, can_fall_through)
    ),
    ExitMethods = set.delete_list(ExitMethods0, [can_continue, can_break]).

%---------------------------------------------------------------------------%
%
% Extra code for handling function calls/returns.
%

:- pred output_args_as_array(java_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_args_as_array(Info, CallArgs, CallArgTypes, !IO) :-
    io.write_string("new java.lang.Object[] { ", !IO),
    output_boxed_args(Info, CallArgs, CallArgTypes, !IO),
    io.write_string("} ", !IO).

:- pred output_boxed_args(java_out_info::in, list(mlds_rval)::in,
    list(mlds_type)::in, io::di, io::uo) is det.

output_boxed_args(_, [], [], !IO).
output_boxed_args(_, [_ | _], [], !IO) :-
    unexpected($pred, "length mismatch").
output_boxed_args(_, [], [_ | _], !IO) :-
    unexpected($pred, "length mismatch").
output_boxed_args(Info, [CallArg | CallArgs], [CallArgType | CallArgTypes],
        !IO) :-
    output_boxed_rval_for_java(Info, CallArgType, CallArg, !IO),
    (
        CallArgs = []
    ;
        CallArgs = [_ | _],
        io.write_string(", ", !IO),
        output_boxed_args(Info, CallArgs, CallArgTypes, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Code for handling multiple return values.
%

% When returning multiple values,
% we generate the following code:
%   { java.lang.Object [] result = <func>(<args>);
%     <output1> = (<type1>) result[0];
%     <output2> = (<type2>) result[1];
%     ...
%   }
%

    % This procedure generates the assignments to the outputs.
    %
:- pred output_assign_results(java_out_info::in, list(mlds_lval)::in,
    list(mlds_type)::in, int::in, indent::in, prog_context::in,
    io::di, io::uo) is det.

output_assign_results(_, [], [], _, _, _, !IO).
output_assign_results(Info, [Lval | Lvals], [Type | Types], ResultIndex,
        Indent, Context, !IO) :-
    indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
        Context, Indent, !IO),
    output_lval_for_java(Info, Lval, !IO),
    io.write_string(" = ", !IO),
    output_unboxed_result(Info, Type, ResultIndex, !IO),
    io.write_string(";\n", !IO),
    output_assign_results(Info, Lvals, Types, ResultIndex + 1,
        Indent, Context, !IO).
output_assign_results(_, [_ | _], [], _, _, _, _, _) :-
    unexpected($pred, "list length mismatch").
output_assign_results(_, [], [_ | _], _, _, _, _, _) :-
    unexpected($pred, "list length mismatch").

:- pred output_unboxed_result(java_out_info::in, mlds_type::in, int::in,
    io::di, io::uo) is det.

output_unboxed_result(Info, Type, ResultIndex, !IO) :-
    ( if java_builtin_type(Type, _, JavaBoxedName, UnboxMethod) then
        io.write_string("((", !IO),
        io.write_string(JavaBoxedName, !IO),
        io.write_string(") ", !IO),
        io.format("result[%d]).%s()", [i(ResultIndex), s(UnboxMethod)], !IO)
    else
        io.write_string("(", !IO),
        output_type_for_java(Info, Type, !IO),
        io.write_string(") ", !IO),
        io.format("result[%d]", [i(ResultIndex)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Extra code for outputting switch statements.
%

:- pred output_switch_cases_for_java(java_out_info::in, indent::in,
    func_info_csj::in, prog_context::in, list(mlds_switch_case)::in,
    mlds_switch_default::in, exit_methods::out, io::di, io::uo) is det.

output_switch_cases_for_java(Info, Indent, FuncInfo, Context,
        [], Default, ExitMethods, !IO) :-
    output_switch_default_for_java(Info, Indent, FuncInfo, Context, Default,
        ExitMethods, !IO).
output_switch_cases_for_java(Info, Indent, FuncInfo, Context,
        [Case | Cases], Default, ExitMethods, !IO) :-
    output_switch_case_for_java(Info, Indent, FuncInfo, Context, Case,
        CaseExitMethods0, !IO),
    output_switch_cases_for_java(Info, Indent, FuncInfo, Context, Cases,
        Default, CasesExitMethods, !IO),
    ( if set.member(can_break, CaseExitMethods0) then
        CaseExitMethods = set.insert(set.delete(CaseExitMethods0, can_break),
            can_fall_through)
    else
        CaseExitMethods = CaseExitMethods0
    ),
    ExitMethods = set.union(CaseExitMethods, CasesExitMethods).

:- pred output_switch_case_for_java(java_out_info::in, indent::in,
    func_info_csj::in, prog_context::in, mlds_switch_case::in,
    exit_methods::out, io::di, io::uo) is det.

output_switch_case_for_java(Info, Indent, FuncInfo, Context, Case,
        ExitMethods, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt),
    output_case_cond_for_java(Info, Indent, Context, FirstCond, !IO),
    list.foldl(output_case_cond_for_java(Info, Indent, Context), LaterConds,
        !IO),
    output_statement_for_java(Info, Indent + 1, FuncInfo, Stmt,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent + 1, !IO),
        io.write_string("break;\n", !IO),
        ExitMethods = set.delete(set.insert(StmtExitMethods, can_break),
            can_fall_through)
    else
        % Don't output `break' since it would be unreachable.
        ExitMethods = StmtExitMethods
    ).

:- pred output_case_cond_for_java(java_out_info::in, indent::in,
    prog_context::in, mlds_case_match_cond::in, io::di, io::uo) is det.

output_case_cond_for_java(Info, Indent, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("case ", !IO),
        ( if Val = ml_const(mlconst_enum(N, _)) then
            io.write_int(N, !IO)
        else
            output_rval_for_java(Info, Val, !IO)
        ),
        io.write_string(":\n", !IO)
    ;
        Match = match_range(_, _),
        unexpected($pred, "cannot match ranges in Java cases")
    ).

:- pred output_switch_default_for_java(java_out_info::in, indent::in,
    func_info_csj::in, prog_context::in, mlds_switch_default::in,
    exit_methods::out, io::di, io::uo) is det.

output_switch_default_for_java(Info, Indent, FuncInfo, Context, Default,
        ExitMethods, !IO) :-
    (
        Default = default_do_nothing,
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        Default = default_case(Stmt),
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("default:\n", !IO),
        output_statement_for_java(Info, Indent + 1, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Default = default_is_unreachable,
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent, !IO),
        io.write_string("default: /*NOTREACHED*/\n", !IO),
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent + 1, !IO),
        io.write_string("throw new jmercury.runtime.UnreachableDefault();\n",
            !IO),
        ExitMethods = set.make_singleton_set(can_throw)
    ).

%---------------------------------------------------------------------------%
%
% Code for outputting atomic statements.
%

:- pred output_atomic_stmt_for_java(java_out_info::in, indent::in,
    mlds_atomic_statement::in, prog_context::in, io::di, io::uo) is det.

output_atomic_stmt_for_java(Info, Indent, AtomicStmt, Context, !IO) :-
    (
        AtomicStmt = comment(Comment),
        ( if Comment = "" then
            io.nl(!IO)
        else
            % XXX We should escape any "*/"'s in the Comment. We should also
            % split the comment into lines and indent each line appropriately.
            output_n_indents(Indent, !IO),
            io.write_string("/* ", !IO),
            io.write_string(Comment, !IO),
            io.write_string(" */\n", !IO)
        )
    ;
        AtomicStmt = assign(Lval, Rval),
        output_n_indents(Indent, !IO),
        output_lval_for_java(Info, Lval, !IO),
        io.write_string(" = ", !IO),
        output_rval_for_java(Info, Rval, !IO),
        io.write_string(";\n", !IO)
    ;
        AtomicStmt = assign_if_in_heap(_, _),
        sorry($pred, "assign_if_in_heap")
    ;
        AtomicStmt = delete_object(_Lval),
        unexpected($pred, "delete_object not supported in Java.")
    ;
        AtomicStmt = new_object(Target, _Ptag, ExplicitSecTag, Type,
            _MaybeSize, MaybeCtorName, ArgRvalsTypes, _MayUseAtomic, _AllocId),
        (
            ExplicitSecTag = yes,
            unexpected($pred, "explicit secondary tag")
        ;
            ExplicitSecTag = no
        ),

        output_n_indents(Indent, !IO),
        io.write_string("{\n", !IO),
        indent_line_after_context(Info ^ joi_line_numbers, marker_comment,
            Context, Indent + 1, !IO),
        output_lval_for_java(Info, Target, !IO),
        io.write_string(" = new ", !IO),
        % Generate class constructor name.
        ( if
            MaybeCtorName = yes(QualifiedCtorId),
            not (
                Type = mercury_nb_type(MerType, CtorCat),
                hand_defined_type_for_java(MerType, CtorCat, _, _)
            )
        then
            output_type_for_java(Info, Type, !IO),
            io.write_char('.', !IO),
            QualifiedCtorId = qual_ctor_id(_ModuleName, _QualKind, CtorDefn),
            CtorDefn = ctor_id(CtorName, CtorArity),
            output_unqual_class_name_for_java(CtorName, CtorArity, !IO)
        else
            output_type_for_java(Info, Type, !IO)
        ),
        IsArray = type_is_array_for_java(Type),
        (
            IsArray = is_array,
            % The new object will be an array, so we need to initialise it
            % using array literals syntax.
            io.write_string(" {", !IO),
            output_init_args_for_java(Info, ArgRvalsTypes, !IO),
            io.write_string("};\n", !IO)
        ;
            IsArray = not_array,
            % Generate constructor arguments.
            io.write_string("(", !IO),
            output_init_args_for_java(Info, ArgRvalsTypes, !IO),
            io.write_string(");\n", !IO)
        ),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)
    ;
        AtomicStmt = gc_check,
        unexpected($pred, "gc_check not implemented.")
    ;
        AtomicStmt = mark_hp(_Lval),
        unexpected($pred, "mark_hp not implemented.")
    ;
        AtomicStmt = restore_hp(_Rval),
        unexpected($pred, "restore_hp not implemented.")
    ;
        AtomicStmt = trail_op(_TrailOp),
        unexpected($pred, "trail_ops not implemented.")
    ;
        AtomicStmt = inline_target_code(TargetLang, Components),
        (
            TargetLang = ml_target_java,
            output_n_indents(Indent, !IO),
            list.foldl(output_target_code_component_for_java(Info),
                Components, !IO)
        ;
            ( TargetLang = ml_target_c
            ; TargetLang = ml_target_csharp
            ),
            unexpected($pred, "inline_target_code only works for lang_java")
        )
    ;
        AtomicStmt = outline_foreign_proc(_TargetLang, _Vs, _Lvals, _Code),
        unexpected($pred, "foreign language interfacing not implemented")
    ).

%---------------------------------------------------------------------------%

:- pred output_target_code_component_for_java(java_out_info::in,
    target_code_component::in, io::di, io::uo) is det.

output_target_code_component_for_java(Info, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext),
        (
            MaybeUserContext = yes(ProgContext),
            write_string_with_context_block(Info, 0, CodeString,
                ProgContext, !IO)
        ;
            MaybeUserContext = no,
            io.write_string(CodeString, !IO)
        )
    ;
        TargetCode = raw_target_code(CodeString),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        output_rval_for_java(Info, Rval, !IO)
    ;
        TargetCode = target_code_output(Lval),
        output_lval_for_java(Info, Lval, !IO)
    ;
        TargetCode = target_code_type(Type),
        InfoGenerics = Info ^ joi_output_generics := do_output_generics,
        output_type_for_java(InfoGenerics, Type, !IO)
    ;
        TargetCode = target_code_function_name(FuncName),
        output_maybe_qualified_function_name_for_java(Info, FuncName, !IO)
    ;
        TargetCode = target_code_alloc_id(_),
        unexpected($pred, "target_code_alloc_id not implemented")
    ).

%---------------------------------------------------------------------------%

    % Output initial values of an object's fields as arguments for the
    % object's class constructor.
    %
:- pred output_init_args_for_java(java_out_info::in,
    list(mlds_typed_rval)::in, io::di, io::uo) is det.

output_init_args_for_java(_, [], !IO).
output_init_args_for_java(Info, [ArgRvalType | ArgRvalsTypes], !IO) :-
    ArgRvalType = ml_typed_rval(ArgRval, _ArgType),
    output_rval_for_java(Info, ArgRval, !IO),
    (
        ArgRvalsTypes = []
    ;
        ArgRvalsTypes = [_ | _],
        io.write_string(", ", !IO)
    ),
    output_init_args_for_java(Info, ArgRvalsTypes, !IO).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_java_stmt.
%---------------------------------------------------------------------------%
