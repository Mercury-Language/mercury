%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2010-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

% Output MLDS statements in C#.
%
% About multiple outputs: C# supports pass-by-reference, so the first thought
% is to just let the MLDS code generator use `out' parameters to handle
% multiple outputs. But to reference a method, we have to assign it
% to a delegate with a matching signature, which needs to be declared.
% Although we can parameterise the types, we cannot parameterise
% the argument modes (in/out), so we would need 2^n delegates to cover
% all methods of arity n.
%
% Instead, we generate code as if C# supported multiple return values.
% The first return value is returned as usual. The second and later return
% values are assigned to `out' parameters, which we always place after any
% input arguments. That way we don't need to declare delegates with all
% possible permutations of in/out parameters.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_cs_stmt.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_cs_util.
:- import_module ml_backend.mlds_to_target_util.

:- import_module io.

%---------------------------------------------------------------------------%

:- pred output_stmt_for_csharp(csharp_out_info::in, io.text_output_stream::in,
    indent::in, func_info_csj::in, mlds_stmt::in, exit_methods::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module libs.globals.
:- import_module ml_backend.mlds_to_cs_data.
:- import_module ml_backend.mlds_to_cs_func.    % undesirable circular dep
:- import_module ml_backend.mlds_to_cs_name.
:- import_module ml_backend.mlds_to_cs_type.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

output_stmt_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    (
        Stmt = ml_stmt_block(_, _, _, _),
        output_stmt_block_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_while(_, _, _, _, _),
        output_stmt_while_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_if_then_else(_, _, _, _),
        output_stmt_if_then_else_for_csharp(Info, Stream, Indent, FuncInfo,
            Stmt, ExitMethods, !IO)
    ;
        Stmt = ml_stmt_switch(_, _, _, _, _, _),
        output_stmt_switch_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_label(_, _),
        unexpected($pred, "labels not supported in C#.")
    ;
        Stmt = ml_stmt_goto(_, _),
        output_stmt_goto_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_computed_goto(_, _, _),
        unexpected($pred, "computed gotos not supported in C#.")
    ;
        Stmt = ml_stmt_call(_, _, _, _, _, _),
        output_stmt_call_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_return(_, _),
        output_stmt_return_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_do_commit(_, _),
        output_stmt_do_commit_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_try_commit(_, _, _, _),
        output_stmt_try_commit_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
            ExitMethods, !IO)
    ;
        Stmt = ml_stmt_atomic(AtomicStatement, Context),
        output_atomic_stmt_for_csharp(Info, Stream, Indent, AtomicStatement,
            Context, !IO),
        ExitMethods = set.make_singleton_set(can_fall_through)
    ).

:- pred output_stmt_block_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_block), exit_methods::out, io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_block_for_csharp/8)).

output_stmt_block_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, Stmts, Context),
    BraceIndent = Indent,
    BlockIndent = Indent + 1,
    LineNumbers = Info ^ csoi_line_numbers,
    BraceIndentStr = indent2_string(BraceIndent),

    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%s{\n", [s(BraceIndentStr)], !IO),
    (
        LocalVarDefns = [_ | _],
        list.foldl(
            output_local_var_defn_for_csharp(Info, Stream, BlockIndent),
            LocalVarDefns, !IO),
        io.write_string(Stream, "\n", !IO)
    ;
        LocalVarDefns = []
    ),
    (
        FuncDefns = [_ | _],
        list.foldl(
            output_function_defn_for_csharp(Info, Stream, BlockIndent,
                oa_force_init),
            FuncDefns, !IO),
        io.write_string(Stream, "\n", !IO)
    ;
        FuncDefns = []
    ),
    output_stmts_for_csharp(Info, Stream, BlockIndent, FuncInfo, Stmts,
        ExitMethods, !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(BraceIndentStr)], !IO).

:- pred output_local_var_defn_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_local_var_defn::in,
    io::di, io::uo) is det.

output_local_var_defn_for_csharp(Info, Stream, Indent, LocalVarDefn, !IO) :-
    LocalVarDefn = mlds_local_var_defn(LocalVarName, _Context,
        Type, Initializer, _),
    IndentStr = indent2_string(Indent),
    TypeStr = type_to_string_for_csharp(Info, Type),
    LocalVarNameStr = local_var_name_to_ll_string_for_csharp(LocalVarName),
    io.format(Stream, "%s%s %s",
        [s(IndentStr), s(TypeStr), s(LocalVarNameStr)], !IO),
    output_initializer_for_csharp(Info, Stream, oa_force_init, Indent + 1,
        Type, Initializer, ";", !IO).

:- pred output_stmts_for_csharp(csharp_out_info::in, io.text_output_stream::in,
    indent::in, func_info_csj::in, list(mlds_stmt)::in, exit_methods::out,
    io::di, io::uo) is det.

output_stmts_for_csharp(_, _, _, _, [], ExitMethods, !IO) :-
    ExitMethods = set.make_singleton_set(can_fall_through).
output_stmts_for_csharp(Info, Stream, Indent, FuncInfo, [Stmt | Stmts],
        ExitMethods, !IO) :-
    output_stmt_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        output_stmts_for_csharp(Info, Stream, Indent, FuncInfo, Stmts,
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
        % the preceding statement cannot complete.
        ExitMethods = StmtExitMethods
    ).

%---------------------------------------------------------------------------%

:- pred output_stmt_while_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_while), exit_methods::out, io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_while_for_csharp/8)).

output_stmt_while_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_while(Kind, Cond, BodyStmt, _LoopLocalVars, Context),
    LineNumbers = Info ^ csoi_line_numbers,
    scope_indent(BodyStmt, Indent, ScopeIndent),
    IndentStr = indent2_string(Indent),
    (
        Kind = may_loop_zero_times,
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%swhile (", [s(IndentStr)], !IO),
        output_rval_for_csharp(Info, Cond, Stream, !IO),
        io.write_string(Stream, ")\n", !IO),
        % The contained statement is reachable iff the while statement is
        % reachable and the condition expression is not a constant
        % expression whose value is false.
        ( if Cond = ml_const(mlconst_false) then
            cs_output_context(Stream, LineNumbers, Context, !IO),
            io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
            io.format(Stream, "%s  /* Unreachable code */\n",
                [s(IndentStr)], !IO),
            io.format(Stream, "%s}\n", [s(IndentStr)], !IO),
            ExitMethods = set.make_singleton_set(can_fall_through)
        else
            BodyInfo = Info ^ csoi_break_context := bc_loop,
            output_stmt_for_csharp(BodyInfo, Stream, ScopeIndent, FuncInfo,
                BodyStmt, StmtExitMethods, !IO),
            ExitMethods = while_exit_methods_for_csharp(Cond, StmtExitMethods)
        )
    ;
        Kind = loop_at_least_once,
        BodyInfo = Info ^ csoi_break_context := bc_loop,
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%sdo\n", [s(IndentStr)], !IO),
        output_stmt_for_csharp(BodyInfo, Stream, ScopeIndent, FuncInfo,
            BodyStmt, StmtExitMethods, !IO),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%swhile (", [s(IndentStr)], !IO),
        output_rval_for_csharp(Info, Cond, Stream, !IO),
        io.write_string(Stream, ");\n", !IO),
        ExitMethods = while_exit_methods_for_csharp(Cond, StmtExitMethods)
    ).

:- func while_exit_methods_for_csharp(mlds_rval, exit_methods) = exit_methods.

while_exit_methods_for_csharp(Cond, BlockExitMethods) = ExitMethods :-
    % A while statement cannot complete normally if its condition
    % expression is a constant expression with value true, and it
    % doesn't contain a reachable break statement that exits the
    % while statement.
    ( if
        % XXX This is not a sufficient way of testing for a Java
        % "constant expression", though determining these accurately
        % is a little difficult to do here.
        % XXX C# is not Java
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

:- pred output_stmt_if_then_else_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_if_then_else), exit_methods::out,
    io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_if_then_else_for_csharp/8)).

output_stmt_if_then_else_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_if_then_else(Cond, Then0, MaybeElse, Context),
    LineNumbers = Info ^ csoi_line_numbers,
    IndentStr = indent2_string(Indent),
    % We need to take care to avoid problems caused by the dangling else
    % ambiguity.
    ( if
        % For examples of the form
        %
        %   if (...)
        %       if (...)
        %           ...
        %   else
        %       ...
        %
        % we need braces around the inner `if', otherwise they wouldn't
        % parse they way we want them to.

        MaybeElse = yes(_),
        Then0 = ml_stmt_if_then_else(_, _, no, ThenContext)
    then
        Then = ml_stmt_block([], [], [Then0], ThenContext)
    else
        Then = Then0
    ),

    scope_indent(Then, Indent, ThenScopeIndent),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%sif (", [s(IndentStr)], !IO),
    output_rval_for_csharp(Info, Cond, Stream, !IO),
    io.write_string(Stream, ")\n", !IO),
    output_stmt_for_csharp(Info, Stream, ThenScopeIndent, FuncInfo, Then,
        ThenExitMethods, !IO),
    (
        MaybeElse = yes(Else),
        scope_indent(Else, Indent, ElseScopeIndent),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%selse\n", [s(IndentStr)], !IO),
        output_stmt_for_csharp(Info, Stream, ElseScopeIndent, FuncInfo, Else,
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

%---------------------------------------------------------------------------%

:- pred output_stmt_switch_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_switch), exit_methods::out,
    io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_switch_for_csharp/8)).

output_stmt_switch_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_switch(_Type, Val, _Range, Cases, Default, Context),
    LineNumbers = Info ^ csoi_line_numbers,
    IndentStr = indent2_string(Indent),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%sswitch (", [s(IndentStr)], !IO),
    output_rval_for_csharp(Info, Val, Stream, !IO),
    io.write_string(Stream, ") {\n", !IO),
    CaseInfo = Info ^ csoi_break_context := bc_switch,
    output_switch_cases_for_csharp(CaseInfo, Stream, Indent + 1, FuncInfo,
        Context, Cases, Default, ExitMethods, !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred output_switch_cases_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in, prog_context::in,
    list(mlds_switch_case)::in, mlds_switch_default::in, exit_methods::out,
    io::di, io::uo) is det.

output_switch_cases_for_csharp(Info, Stream, Indent, FuncInfo, Context,
        [], Default, ExitMethods, !IO) :-
    output_switch_default_for_csharp(Info, Stream, Indent, FuncInfo, Context,
        Default, ExitMethods, !IO).
output_switch_cases_for_csharp(Info, Stream, Indent, FuncInfo, Context,
        [Case | Cases], Default, ExitMethods, !IO) :-
    output_switch_case_for_csharp(Info, Stream, Indent, FuncInfo, Context,
        Case, CaseExitMethods0, !IO),
    output_switch_cases_for_csharp(Info, Stream, Indent, FuncInfo, Context,
        Cases, Default, CasesExitMethods, !IO),
    ( if set.member(can_break, CaseExitMethods0) then
        CaseExitMethods = set.insert(set.delete(CaseExitMethods0, can_break),
            can_fall_through)
    else
        CaseExitMethods = CaseExitMethods0
    ),
    ExitMethods = set.union(CaseExitMethods, CasesExitMethods).

:- pred output_switch_case_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in, prog_context::in,
    mlds_switch_case::in, exit_methods::out, io::di, io::uo) is det.

output_switch_case_for_csharp(Info, Stream, Indent, FuncInfo, Context, Case,
        ExitMethods, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Statement),
    output_case_cond_for_csharp(Info, Stream, Indent, Context,
        FirstCond, !IO),
    list.foldl(output_case_cond_for_csharp(Info, Stream, Indent, Context),
        LaterConds, !IO),
    output_stmt_for_csharp(Info, Stream, Indent + 1, FuncInfo, Statement,
        StmtExitMethods, !IO),
    ( if set.member(can_fall_through, StmtExitMethods) then
        LineNumbers = Info ^ csoi_line_numbers,
        Indent1Str = indent2_string(Indent + 1),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%sbreak;\n", [s(Indent1Str)], !IO),
        ExitMethods = set.delete(set.insert(StmtExitMethods, can_break),
            can_fall_through)
    else
        % Don't output `break' since it would be unreachable.
        ExitMethods = StmtExitMethods
    ).

:- pred output_case_cond_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, prog_context::in,
    mlds_case_match_cond::in, io::di, io::uo) is det.

output_case_cond_for_csharp(Info, Stream, Indent, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        LineNumbers = Info ^ csoi_line_numbers,
        IndentStr = indent2_string(Indent),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%scase ", [s(IndentStr)], !IO),
        output_rval_for_csharp(Info, Val, Stream, !IO),
        io.write_string(Stream, ":\n", !IO)
    ;
        Match = match_range(_, _),
        unexpected($pred, "cannot match ranges in C# cases")
    ).

:- pred output_switch_default_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in, prog_context::in,
    mlds_switch_default::in, exit_methods::out, io::di, io::uo) is det.

output_switch_default_for_csharp(Info, Stream, Indent, FuncInfo, Context,
        Default, ExitMethods, !IO) :-
    (
        Default = default_do_nothing,
        ExitMethods = set.make_singleton_set(can_fall_through)
    ;
        Default = default_case(Statement),
        LineNumbers = Info ^ csoi_line_numbers,
        IndentStr = indent2_string(Indent),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%sdefault:\n", [s(IndentStr)], !IO),
        output_stmt_for_csharp(Info, Stream, Indent + 1, FuncInfo, Statement,
            ExitMethods, !IO),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%sbreak;\n", [s(IndentStr)], !IO)
    ;
        Default = default_is_unreachable,
        LineNumbers = Info ^ csoi_line_numbers,
        IndentStr = indent2_string(Indent),
        io.format(Stream,
            "%sdefault: /*NOTREACHED*/\n", [s(IndentStr)], !IO),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream,
            "%s  throw new runtime.UnreachableDefault();\n",
            [s(IndentStr)], !IO),
        ExitMethods = set.make_singleton_set(can_throw)
    ).

%---------------------------------------------------------------------------%

:- pred output_stmt_goto_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_goto), exit_methods::out, io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_goto_for_csharp/8)).

output_stmt_goto_for_csharp(Info, Stream, Indent, _FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_goto(Target, Context),
    LineNumbers = Info ^ csoi_line_numbers,
    IndentStr = indent2_string(Indent),
    (
        Target = goto_label(_),
        unexpected($pred, "gotos not supported in C#.")
    ;
        Target = goto_break_switch,
        BreakContext = Info ^ csoi_break_context,
        (
            BreakContext = bc_switch,
            cs_output_context(Stream, LineNumbers, Context, !IO),
            io.format(Stream, "%sbreak;\n", [s(IndentStr)], !IO),
            ExitMethods = set.make_singleton_set(can_break)
        ;
            ( BreakContext = bc_none
            ; BreakContext = bc_loop
            ),
            unexpected($pred, "goto_break_switch not in switch")
        )
    ;
        Target = goto_break_loop,
        BreakContext = Info ^ csoi_break_context,
        (
            BreakContext = bc_loop,
            cs_output_context(Stream, LineNumbers, Context, !IO),
            io.format(Stream, "%sbreak;\n", [s(IndentStr)], !IO),
            ExitMethods = set.make_singleton_set(can_break)
        ;
            ( BreakContext = bc_none
            ; BreakContext = bc_switch
            ),
            unexpected($pred, "goto_break_loop not in loop")
        )
    ;
        Target = goto_continue_loop,
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%scontinue;\n", [s(IndentStr)], !IO),
        ExitMethods = set.make_singleton_set(can_continue)
    ).

%---------------------------------------------------------------------------%

:- pred output_stmt_call_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_call), exit_methods::out, io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_call_for_csharp/8)).

output_stmt_call_for_csharp(Info, Stream, Indent, _FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_call(Signature, FuncRval, CallArgs, Results,
        IsTailCall, Context),
    Signature = mlds_func_signature(ArgTypes, RetTypes),
    LineNumbers = Info ^ csoi_line_numbers,
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent + 1),

    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.write_string(Stream, Indent1Str, !IO),
    (
        Results = [],
        OutArgs = []
    ;
        Results = [Lval | Lvals],
        output_lval_for_csharp(Info, Lval, Stream, !IO),
        io.write_string(Stream, " = ", !IO),
        OutArgs = list.map(func(X) = ml_mem_addr(X), Lvals)
    ),
    ( if FuncRval = ml_const(mlconst_code_addr(_)) then
        % This is a standard method call.
        output_call_rval_for_csharp(Info, FuncRval, Stream, !IO)
    else
        % This is a call using a method pointer.
        PtrTypeName = method_ptr_type_to_string(Info, ArgTypes, RetTypes),
        io.format(Stream, "((%s) ", [s(PtrTypeName)], !IO),
        output_call_rval_for_csharp(Info, FuncRval, Stream, !IO),
        io.write_string(Stream, ")", !IO)
    ),
    % XXX This list of rvals on one line can result in *very* long lines.
    io.write_string(Stream, "(", !IO),
    write_out_list(output_rval_for_csharp(Info), ", ", CallArgs ++ OutArgs,
        Stream, !IO),
    io.write_string(Stream, ");\n", !IO),

    % This is to tell the C# compiler that a call to an erroneous procedure
    % will not fall through. --pw
    (
        IsTailCall = ordinary_call
    ;
        IsTailCall = tail_call
    ;
        IsTailCall = no_return_call,
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%sthrow new runtime.UnreachableDefault();\n",
            [s(Indent1Str)], !IO)
    ),

    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO),
    ExitMethods = set.make_singleton_set(can_fall_through).

%---------------------------------------------------------------------------%

:- pred output_stmt_return_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_return), exit_methods::out,
    io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_return_for_csharp/8)).

output_stmt_return_for_csharp(Info, Stream, Indent, _FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_return(Results, Context),
    LineNumbers = Info ^ csoi_line_numbers,
    IndentStr = indent2_string(Indent),
    (
        Results = [],
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%sreturn;\n", [s(IndentStr)], !IO)
    ;
        Results = [Rval | Rvals],
        % The first return value is returned directly.
        % Subsequent return values are assigned to out parameters.
        list.foldl2(output_assign_out_params(Info, Stream, Indent),
            Rvals, 2, _, !IO),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%sreturn ", [s(IndentStr)], !IO),
        output_rval_for_csharp(Info, Rval, Stream, !IO),
        io.write_string(Stream, ";\n", !IO)
    ),
    ExitMethods = set.make_singleton_set(can_return).

:- pred output_assign_out_params(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_rval::in, int::in, int::out,
    io::di, io::uo) is det.

output_assign_out_params(Info, Stream, Indent, Rval, Num, Num + 1, !IO) :-
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sout_param_%d = ", [s(IndentStr), i(Num)], !IO),
    output_rval_for_csharp(Info, Rval, Stream, !IO),
    io.write_string(Stream, ";\n", !IO).

%---------------------------------------------------------------------------%

:- pred output_stmt_do_commit_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_do_commit), exit_methods::out,
    io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_do_commit_for_csharp/8)).

output_stmt_do_commit_for_csharp(Info, Stream, Indent, _FuncInfo, Stmt,
        ExitMethods, !IO) :-
    Stmt = ml_stmt_do_commit(Ref, Context),
    LineNumbers = Info ^ csoi_line_numbers,
    IndentStr = indent2_string(Indent),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.write_string(Stream, IndentStr, !IO),
    output_rval_for_csharp(Info, Ref, Stream, !IO),
    io.write_string(Stream, " = new runtime.Commit();\n", !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%sthrow ", [s(IndentStr)], !IO),
    output_rval_for_csharp(Info, Ref, Stream, !IO),
    io.write_string(Stream, ";\n", !IO),
    ExitMethods = set.make_singleton_set(can_throw).

%---------------------------------------------------------------------------%

:- pred output_stmt_try_commit_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, func_info_csj::in,
    mlds_stmt::in(ml_stmt_is_try_commit), exit_methods::out,
    io::di, io::uo) is det.
:- pragma inline(pred(output_stmt_try_commit_for_csharp/8)).

output_stmt_try_commit_for_csharp(Info, Stream, Indent, FuncInfo, Stmt,
        ExitMethods, !IO) :-
    % XXX I (zs) reduced the number of times this code writes out the
    % context (if LineNumbers = yes) from seven to four, but I think
    % even the remaining set is probably more than is needed.
    Stmt = ml_stmt_try_commit(_Ref, BodyStmt, HandlerStmt, Context),
    LineNumbers = Info ^ csoi_line_numbers,
    IndentStr = indent2_string(Indent),
    io.format(Stream, "%stry\n", [s(IndentStr)], !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    output_stmt_for_csharp(Info, Stream, Indent + 1, FuncInfo, BodyStmt,
        TryExitMethods0, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%scatch (runtime.Commit commit_variable)\n",
        [s(IndentStr)], !IO),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%s  ", [s(IndentStr)], !IO),
    output_stmt_for_csharp(Info, Stream, Indent + 1, FuncInfo, HandlerStmt,
        CatchExitMethods, !IO),
    cs_output_context(Stream, LineNumbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO),
    ExitMethods = set.union(set.delete(TryExitMethods0, can_throw),
        CatchExitMethods).

%---------------------------------------------------------------------------%

:- pred output_atomic_stmt_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, indent::in, mlds_atomic_statement::in,
    prog_context::in, io::di, io::uo) is det.

output_atomic_stmt_for_csharp(Info, Stream, Indent, AtomicStmt,
        Context, !IO) :-
    (
        AtomicStmt = comment(Comment),
        ( if Comment = "" then
            io.nl(Stream, !IO)
        else
            IndentStr = indent2_string(Indent),
            % XXX We should escape any "*/"'s in the Comment. We should also
            % split the comment into lines and indent each line appropriately.
            io.format(Stream, "%s/* %s */\n", [s(IndentStr), s(Comment)], !IO)
        )
    ;
        AtomicStmt = assign(Lval, Rval),
        LineNumbers = Info ^ csoi_line_numbers,
        IndentStr = indent2_string(Indent),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.write_string(Stream, IndentStr, !IO),
        output_lval_for_csharp(Info, Lval, Stream, !IO),
        io.write_string(Stream, " = ", !IO),
        output_rval_for_csharp(Info, Rval, Stream, !IO),
        io.write_string(Stream, ";\n", !IO)
    ;
        AtomicStmt = assign_if_in_heap(_, _),
        sorry($pred, "assign_if_in_heap")
    ;
        AtomicStmt = delete_object(_Lval),
        unexpected($pred, "delete_object not supported in C#.")
    ;
        AtomicStmt = new_object(Target, _Ptag, ExplicitSecTag, Type,
            _MaybeSize, MaybeCtorName, ArgRvalsTypes, _MayUseAtomic, _AllocId),
        (
            ExplicitSecTag = yes,
            unexpected($pred, "explicit secondary tag")
        ;
            ExplicitSecTag = no
        ),
        LineNumbers = Info ^ csoi_line_numbers,
        IndentStr = indent2_string(Indent),

        io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%s  ", [s(IndentStr)], !IO),
        output_lval_for_csharp(Info, Target, Stream, !IO),
        % Generate class constructor name.
        type_to_string_and_dims_for_csharp(Info, Type, TypeStr0, ArrayDims),
        TypeStr = add_array_dimensions(TypeStr0, ArrayDims),
        ( if
            MaybeCtorName = yes(QualCtorId),
            not (
                Type = mercury_nb_type(MerType, CtorCat),
                hand_defined_type_for_csharp(MerType, CtorCat, _, _)
            )
        then
            QualCtorId = qual_ctor_id(_ModuleName, _QualKind, CtorId),
            CtorId = ctor_id(CtorName, CtorArity),
            CtorClassNameStr =
                unqual_class_name_to_ll_string_for_csharp(CtorName, CtorArity),
            io.format(Stream, " = new %s.%s",
                [s(TypeStr), s(CtorClassNameStr)], !IO)
        else
            io.format(Stream, " = new %s", [s(TypeStr)], !IO)
        ),
        init_arg_wrappers_cs_java(ArrayDims, LParen, RParen),
        % Generate constructor arguments.
        (
            ArgRvalsTypes = [],
            io.format(Stream, "%s%s;\n", [s(LParen), s(RParen)], !IO)
        ;
            ArgRvalsTypes = [HeadArgRvalType | TailArgRvalsTypes],
            io.format(Stream, "%s\n", [s(LParen)], !IO),
            Indent2Str = indent2_string(Indent + 1),
            output_init_args_for_csharp(Info, Stream, Indent2Str,
                HeadArgRvalType, TailArgRvalsTypes, !IO),
            io.format(Stream, "%s  %s;\n", [s(IndentStr), s(RParen)], !IO)
        ),
        cs_output_context(Stream, LineNumbers, Context, !IO),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
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
            TargetLang = ml_target_csharp,
            list.foldl(output_target_code_component_for_csharp(Info, Stream),
                Components, !IO)
        ;
            ( TargetLang = ml_target_c
            ; TargetLang = ml_target_java
            ),
            unexpected($pred, "inline_target_code only works for lang_java")
        )
    ;
        AtomicStmt = outline_foreign_proc(_TargetLang, _Vs, _Lvals, _Code),
        unexpected($pred, "foreign language interfacing not implemented")
    ).

%---------------------%

    % Output initial values of an object's fields as arguments for the
    % object's class constructor.
    %
:- pred output_init_args_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, string::in, mlds_typed_rval::in,
    list(mlds_typed_rval)::in, io::di, io::uo) is det.

output_init_args_for_csharp(Info, Stream, IndentStr, HeadArg, TailArgs, !IO) :-
    HeadArg = ml_typed_rval(HeadArgRval, _HeadArgType),
    io.write_string(Stream, IndentStr, !IO),
    output_rval_for_csharp(Info, HeadArgRval, Stream, !IO),
    (
        TailArgs = [],
        io.write_string(Stream, "\n", !IO)
    ;
        TailArgs = [HeadTailArg | TailTailArgs],
        io.write_string(Stream, ",\n", !IO),
        output_init_args_for_csharp(Info, Stream, IndentStr,
            HeadTailArg, TailTailArgs, !IO)
    ).

%---------------------%

:- pred output_target_code_component_for_csharp(csharp_out_info::in,
    io.text_output_stream::in, target_code_component::in,
    io::di, io::uo) is det.

output_target_code_component_for_csharp(Info, Stream, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext),
        ForeignLineNumbers = Info ^ csoi_foreign_line_numbers,
        output_pragma_warning_restore(Stream, !IO),
        io.write_string(Stream, "{\n", !IO),
        (
            MaybeUserContext = yes(ProgContext),
            cs_output_context(Stream, ForeignLineNumbers, ProgContext, !IO)
        ;
            MaybeUserContext = no
        ),
        io.format(Stream, "%s}\n", [s(CodeString)], !IO),
        cs_output_default_context(Stream, ForeignLineNumbers, !IO),
        output_pragma_warning_disable(Stream, !IO)
    ;
        TargetCode = raw_target_code(CodeString),
        io.write_string(Stream, CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        output_rval_for_csharp(Info, Rval, Stream, !IO)
    ;
        TargetCode = target_code_output(Lval),
        output_lval_for_csharp(Info, Lval, Stream, !IO)
    ;
        TargetCode = target_code_type(Type),
        % XXX enable generics here
        io.write_string(Stream, type_to_string_for_csharp(Info, Type), !IO)
    ;
        TargetCode = target_code_function_name(FuncName),
        FuncNameStr = maybe_qualified_function_name_to_ll_string_for_csharp(
            Info, FuncName),
        io.write_string(Stream, FuncNameStr, !IO)
    ;
        TargetCode = target_code_alloc_id(_),
        unexpected($pred, "target_code_alloc_id not implemented")
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_cs_stmt.
%---------------------------------------------------------------------------%
