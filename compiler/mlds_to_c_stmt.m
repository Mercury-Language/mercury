%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Output MLDS statements.
%
%---------------------------------------------------------------------------%

:- module ml_backend.mlds_to_c_stmt.
:- interface.

:- import_module libs.
:- import_module libs.indent.
:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.

%---------------------------------------------------------------------------%

:- type func_info_c
    --->    func_info_c(qual_function_name, mlds_func_signature).

:- pred mlds_output_statement(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, func_info_c::in, mlds_stmt::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output an instruction which informs the runtime which procedure
    % we are currently located in.
    %
:- pred mlds_output_time_profile_instr(mlds_to_c_opts::in,
    io.text_output_stream::in, prog_context::in, indent::in,
    qual_function_name::in, io::di, io::uo) is det.

:- pred mlds_output_gc_statement(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_gc_statement::in, string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds_to_c_data.
:- import_module ml_backend.mlds_to_c_func.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_type.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

:- pred mlds_output_statements(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, func_info_c::in, list(mlds_stmt)::in, io::di, io::uo) is det.

mlds_output_statements(_, _, _, _, [], !IO).
mlds_output_statements(Opts, Stream, Indent, FuncInfo, [Stmt | Stmts], !IO) :-
    mlds_output_statement(Opts, Stream, Indent, FuncInfo, Stmt, !IO),
    mlds_output_statements(Opts, Stream, Indent, FuncInfo, Stmts, !IO).

mlds_output_statement(Opts, Stream, Indent, FuncInfo, Stmt, !IO) :-
    c_output_stmt_context(Stream, Opts ^ m2co_line_numbers, Stmt, !IO),
    (
        Stmt = ml_stmt_block(_LocalVarDefns, _FuncDefns, _SubStmts, _Context),
        mlds_output_stmt_block(Opts, Stream, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_while(_Kind, _Cond, _BodyStmt, _LoopLocalVars,
            _Context),
        mlds_output_stmt_while(Opts, Stream, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_if_then_else(_Cond, _Then, _MaybeElse, _Context),
        mlds_output_stmt_if_then_else(Opts, Stream, Indent, FuncInfo,
            Stmt, !IO)
    ;
        Stmt = ml_stmt_switch(_Type, _Val, _Range, _Cases, _Default, _Context),
        mlds_output_stmt_switch(Opts, Stream, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_label(_LabelName, _Context),
        mlds_output_stmt_label(Stream, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_goto(_Target, _Context),
        mlds_output_stmt_goto(Opts, Stream, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_computed_goto(_Expr, _Labels, _Context),
        mlds_output_stmt_computed_goto(Opts, Stream, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_call(_Signature, _FuncRval, _CallArgs,
            _Results, _IsTailCall, _Context),
        mlds_output_stmt_call(Opts, Stream, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_return(_Results, _Context),
        mlds_output_stmt_return(Opts, Stream, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_do_commit(_Ref, _Context),
        mlds_output_stmt_do_commit(Opts, Stream, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_try_commit(_Ref, _BodyStmt0, _HandlerStmt, _Context),
        mlds_output_stmt_try_commit(Opts, Stream, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_atomic(_AtomicStmt, _Context),
        mlds_output_stmt_atomic(Opts, Stream, Indent, Stmt, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output blocks.
%

:- pred mlds_output_stmt_block(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, func_info_c::in, mlds_stmt::in(ml_stmt_is_block),
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_block/7)).

mlds_output_stmt_block(Opts, Stream, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context),
    BraceIndent = Indent,
    BlockIndent = Indent + 1,
    BraceIndentStr = indent2_string(BraceIndent),

    io.format(Stream, "%s{\n", [s(BraceIndentStr)], !IO),

    % Output forward declarations for any nested functions defined in
    % this block, in case they are referenced before they are defined.
    %
    % We do not support high level C code grades with nested functions,
    % but we *do* generate nested functions internally that we later
    % flatten out. Until we step away from generating nested functions
    % entirely, we will need to be print out C code with nested functions
    % for MLDS dumps.
    FuncInfo = func_info_c(FuncName, _),
    FuncName = qual_function_name(ModuleName, _),
    (
        FuncDefns = [_ | _],
        expect(unify(Opts ^ m2co_target_or_dump, tod_dump), $pred,
            "nested functions in target C code"),
        list.foldl(
            mlds_output_function_decl_opts(Opts, Stream, BlockIndent,
                ModuleName),
            FuncDefns, !IO),
        PrintedSomething0 = yes
    ;
        FuncDefns = [],
        PrintedSomething0 = no
    ),
    (
        LocalVarDefns = [_ | _],
        (
            PrintedSomething0 = no
        ;
            PrintedSomething0 = yes,
            io.nl(Stream, !IO)
        ),
        mlds_output_local_var_defns(Opts, Stream, BlockIndent, no,
            LocalVarDefns, !IO),
        PrintedSomething1 = yes
    ;
        LocalVarDefns = [],
        PrintedSomething1 = PrintedSomething0
    ),
    (
        FuncDefns = [_ | _],
        (
            PrintedSomething1 = no
        ;
            PrintedSomething1 = yes,
            io.nl(Stream, !IO)
        ),
        mlds_output_function_defns(Opts, no_blank_line_start, Stream,
            BlockIndent, ModuleName, FuncDefns, !IO),
        PrintedSomething2 = yes
    ;
        FuncDefns = [],
        PrintedSomething2 = PrintedSomething1
    ),
    (
        PrintedSomething2 = no
    ;
        PrintedSomething2 = yes,
        io.nl(Stream, !IO)
    ),
    mlds_output_statements(Opts, Stream, BlockIndent, FuncInfo, SubStmts, !IO),
    % XXX Why print a context for a right brace?
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(BraceIndentStr)], !IO).

:- pred mlds_output_local_var_defns(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, bool::in,
    list(mlds_local_var_defn)::in, io::di, io::uo) is det.

mlds_output_local_var_defns(_, _, _, _, [], !IO).
mlds_output_local_var_defns(Opts, Stream, Indent, Separate,
        [LocalVarDefn | LocalVarDefns], !IO) :-
    mlds_output_local_var_defn(Opts, Stream, Indent, Separate,
        LocalVarDefn, !IO),
    mlds_output_local_var_defns(Opts, Stream, Indent, Separate,
        LocalVarDefns, !IO).

:- pred mlds_output_local_var_defn(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, bool::in,
    mlds_local_var_defn::in, io::di, io::uo) is det.

mlds_output_local_var_defn(Opts, Stream, Indent, Separate, LocalVarDefn,
        !IO) :-
    LocalVarDefn = mlds_local_var_defn(LocalVarName, Context,
        Type, Initializer, GCStmt),
    IndentStr = indent2_string(Indent),
    LocalVarTypeName = local_var_decl_to_type_name(Opts, LocalVarName, Type,
        get_initializer_array_size(Initializer)),
    (
        Separate = yes,
        io.nl(Stream, !IO)
    ;
        Separate = no
    ),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%s%s", [s(IndentStr), s(LocalVarTypeName)], !IO),
    mlds_output_initializer(Opts, Stream, Type, Initializer, !IO),
    io.write_string(Stream, ";\n", !IO),
    mlds_output_gc_statement(Opts, Stream, Indent, GCStmt, "", !IO).

:- func local_var_decl_to_type_name(mlds_to_c_opts, mlds_local_var_name,
    mlds_type, initializer_array_size) = string.

local_var_decl_to_type_name(Opts, LocalVarName, Type, InitializerSize)
        = TypeNameStr :-
    type_to_prefix_suffix_for_c(Opts, Type, InitializerSize,
        TypePrefix, TypeSuffix),
    LocalVarNameStr = local_var_name_to_string_for_c(LocalVarName),
    string.format("%s %s%s",
        [s(TypePrefix), s(LocalVarNameStr), s(TypeSuffix)], TypeNameStr).

%---------------------------------------------------------------------------%
%
% Output while loops.
%

:- pred mlds_output_stmt_while(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, func_info_c::in, mlds_stmt::in(ml_stmt_is_while),
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_while/7)).

mlds_output_stmt_while(Opts, Stream, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_while(Kind, Cond, BodyStmt, _LoopLocalVars, Context),
    scope_indent(BodyStmt, Indent, ScopeIndent),
    BodyOpts = Opts ^ m2co_break_context := bc_loop,
    IndentStr = indent2_string(Indent),
    (
        Kind = may_loop_zero_times,
        io.format(Stream, "%swhile (", [s(IndentStr)], !IO),
        mlds_output_rval(Opts, Cond, Stream, !IO),
        io.write_string(Stream, ")\n", !IO),
        mlds_output_statement(BodyOpts, Stream, ScopeIndent, FuncInfo,
            BodyStmt, !IO)
    ;
        Kind = loop_at_least_once,
        io.format(Stream, "%sdo\n", [s(IndentStr)], !IO),
        mlds_output_statement(BodyOpts, Stream, ScopeIndent, FuncInfo,
            BodyStmt, !IO),
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%swhile (", [s(IndentStr)], !IO),
        mlds_output_rval(Opts, Cond, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output if-then-elses.
%

:- pred mlds_output_stmt_if_then_else(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_if_then_else), io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_if_then_else/7)).

mlds_output_stmt_if_then_else(Opts, Stream, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_if_then_else(Cond, Then0, MaybeElse, Context),
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
        % we need braces around the inner `if', otherwise they wouldn't parse
        % they way we want them to: C would match the `else' with the
        % inner `if' rather than the outer `if'.

        MaybeElse = yes(_),
        Then0 = ml_stmt_if_then_else(_, _, no, ThenContext)
    then
        Then = ml_stmt_block([], [], [Then0], ThenContext)
    else if
        % For examples of the form
        %
        %   if (...)
        %       if (...)
        %           ...
        %       else
        %           ...
        %
        % we do not _need_ braces around the inner `if', since C will match
        % the else with the inner `if', but we add braces anyway, to avoid
        % a warning from gcc.

        MaybeElse = no,
        Then0 = ml_stmt_if_then_else(_, _, yes(_), ThenContext)
    then
        Then = ml_stmt_block([], [], [Then0], ThenContext)
    else
        Then = Then0
    ),

    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sif (", [s(IndentStr)], !IO),
    mlds_output_rval(Opts, Cond, Stream, !IO),
    io.write_string(Stream, ")\n", !IO),
    scope_indent(Then, Indent, ScopeIndent),
    mlds_output_statement(Opts, Stream, ScopeIndent, FuncInfo, Then, !IO),
    (
        MaybeElse = yes(Else),
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%selse\n", [s(IndentStr)], !IO),
        ( if Else = ml_stmt_if_then_else(_, _, _, _) then
            % Indent each if-then-else in a if-then-else chain
            % to the same depth.
            ElseScopeIndent = Indent
        else
            scope_indent(Else, Indent, ElseScopeIndent)
        ),
        mlds_output_statement(Opts, Stream, ElseScopeIndent, FuncInfo,
            Else, !IO)
    ;
        MaybeElse = no
    ).

%---------------------------------------------------------------------------%
%
% Output switch statements.
%

:- pred mlds_output_stmt_switch(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, func_info_c::in, mlds_stmt::in(ml_stmt_is_switch),
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_switch/7)).

mlds_output_stmt_switch(Opts, Stream, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_switch(_Type, Val, _Range, Cases, Default, Context),
    IndentStr = indent2_string(Indent),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%sswitch (", [s(IndentStr)], !IO),
    mlds_output_rval(Opts, Val, Stream, !IO),
    io.write_string(Stream, ") {\n", !IO),
    CaseOpts = Opts ^ m2co_break_context := bc_switch,
    % We put the default case first, so that if it is unreachable,
    % it will get merged in with the first case.
    mlds_output_switch_default(CaseOpts, Stream, Indent + 1, FuncInfo,
        Context, Default, !IO),
    list.foldl(
        mlds_output_switch_case(CaseOpts, Stream, Indent + 1, FuncInfo,
            Context),
        Cases, !IO),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred mlds_output_switch_case(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, func_info_c::in, prog_context::in, mlds_switch_case::in,
    io::di, io::uo) is det.

mlds_output_switch_case(Opts, Stream, Indent, FuncInfo, Context, Case, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt),
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent + 1),
    mlds_output_case_cond(Opts, Stream, IndentStr, Context, FirstCond, !IO),
    list.foldl(mlds_output_case_cond(Opts, Stream, IndentStr, Context),
        LaterConds, !IO),
    mlds_output_statement(Opts, Stream, Indent + 1, FuncInfo, Stmt, !IO),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%sbreak;\n", [s(Indent1Str)], !IO).

:- pred mlds_output_case_cond(mlds_to_c_opts::in, io.text_output_stream::in,
    string::in, prog_context::in, mlds_case_match_cond::in,
    io::di, io::uo) is det.

mlds_output_case_cond(Opts, Stream, IndentStr, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%scase ", [s(IndentStr)], !IO),
        mlds_output_rval(Opts, Val, Stream, !IO),
        io.write_string(Stream, ":\n", !IO)
    ;
        Match = match_range(Low, High),
        % This uses the GNU C extension `case <Low> ... <High>:'.
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%scase ", [s(IndentStr)], !IO),
        mlds_output_rval(Opts, Low, Stream, !IO),
        io.write_string(Stream, " ... ", !IO),
        mlds_output_rval(Opts, High, Stream, !IO),
        io.write_string(Stream, ":\n", !IO)
    ).

:- pred mlds_output_switch_default(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, func_info_c::in, prog_context::in,
    mlds_switch_default::in, io::di, io::uo) is det.

mlds_output_switch_default(Opts, Stream, Indent, FuncInfo, Context,
        Default, !IO) :-
    (
        Default = default_is_unreachable,
        IndentStr = indent2_string(Indent),
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%sdefault: /*NOTREACHED*/ MR_assert(0);\n",
            [s(IndentStr)], !IO)
    ;
        Default = default_do_nothing
    ;
        Default = default_case(Stmt),
        IndentStr = indent2_string(Indent),
        Indent1Str = indent2_string(Indent + 1),
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%sdefault:\n", [s(IndentStr)], !IO),
        mlds_output_statement(Opts, Stream, Indent + 1, FuncInfo, Stmt, !IO),
        % XXX Why put a context on a non-user-provide code such as "break"?
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%sbreak;\n", [s(Indent1Str)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output labels.
%

:- pred mlds_output_stmt_label(io.text_output_stream::in, indent::in,
    mlds_stmt::in(ml_stmt_is_label), io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_label/5)).

mlds_output_stmt_label(Stream, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_label(Label, _Context),
    IndentM1Str = indent2_string(Indent - 1),
    % Note: MLDS allows labels at the end of blocks. C does not.
    % Hence we need to insert a semicolon after the colon to ensure that
    % there is a statement to attach the label to.
    LabelStr = label_to_string(Label),
    io.format(Stream, "%s%s:;\n", [s(IndentM1Str), s(LabelStr)], !IO).

:- func label_to_string(mlds_label) = string.

label_to_string(mlds_label(LabelName)) = LabelStr :-
    LabelStr= name_mangle(LabelName).

%---------------------------------------------------------------------------%
%
% Output gotos.
%

:- pred mlds_output_stmt_goto(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_stmt::in(ml_stmt_is_goto), io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_goto/6)).

mlds_output_stmt_goto(Opts, Stream, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_goto(Target, _Context),
    IndentStr = indent2_string(Indent),
    (
        Target = goto_label(Label),
        LabelStr = label_to_string(Label),
        io.format(Stream, "%sgoto %s;\n", [s(IndentStr), s(LabelStr)], !IO)
    ;
        Target = goto_break_switch,
        BreakContext = Opts ^ m2co_break_context,
        (
            BreakContext = bc_switch,
            io.format(Stream, "%sbreak;\n", [s(IndentStr)], !IO)
        ;
            ( BreakContext = bc_none
            ; BreakContext = bc_loop
            ),
            unexpected($pred, "goto_break_switch not in switch")
        )
    ;
        Target = goto_break_loop,
        BreakContext = Opts ^ m2co_break_context,
        (
            BreakContext = bc_loop,
            io.format(Stream, "%sbreak;\n", [s(IndentStr)], !IO)
        ;
            ( BreakContext = bc_none
            ; BreakContext = bc_switch
            ),
            unexpected($pred, "goto_break_loop not in loop")
        )
    ;
        Target = goto_continue_loop,
        io.format(Stream, "%scontinue;\n", [s(IndentStr)], !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output computed gotos.
%

:- pred mlds_output_stmt_computed_goto(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in,
    mlds_stmt::in(ml_stmt_is_computed_goto), io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_computed_goto/6)).

mlds_output_stmt_computed_goto(Opts, Stream, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_computed_goto(Expr, Labels, Context),
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent + 1),
    % XXX For GNU C, we could output potentially more efficient code
    % by using an array of labels; this would tell the compiler that
    % it did not need to do any range check.
    io.format(Stream, "%sswitch (", [s(IndentStr)], !IO),
    mlds_output_rval(Opts, Expr, Stream, !IO),
    io.write_string(Stream, ") {\n", !IO),
    list.foldl2(
        mlds_output_computed_goto_label(Opts, Stream, Context, Indent1Str),
        Labels, 0, _FinalCount, !IO),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%sdefault: /*NOTREACHED*/ MR_assert(0);\n",
        [s(Indent1Str)], !IO),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- pred mlds_output_computed_goto_label(mlds_to_c_opts::in,
    io.text_output_stream::in, prog_context::in, string::in,
    mlds_label::in, int::in, int::out, io::di, io::uo) is det.

mlds_output_computed_goto_label(Opts, Stream, Context, IndentStr, Label,
        !Count, !IO) :-
    LabelStr = label_to_string(Label),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%scase %d: goto %s;\n",
        [s(IndentStr), i(!.Count), s(LabelStr)], !IO),
    !:Count = !.Count + 1.

%---------------------------------------------------------------------------%
%
% Output calls.
%

:- pred mlds_output_stmt_call(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, func_info_c::in, mlds_stmt::in(ml_stmt_is_call),
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_call/7)).

mlds_output_stmt_call(Opts, Stream, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_call(Signature, FuncRval, CallArgs, Results,
        IsTailCall, Context),
    FuncInfo = func_info_c(CallerName, CallerSignature),

    % We need to ensure that we generate a single C statement here,
    % in case the generated code is e.g. one arm of an if-then-else.
    %
    % If we need to put profiling code before or after the call,
    % or if we want to put a return statement after the call, we must
    % enclose them, and the call, inside an extra pair of curly braces.
    % However, in the common case where none of that is needed,
    % we do not want the extra clutter of an unnecessary pair of braces.

    ProfileCalls = Opts ^ m2co_profile_calls,
    ProfileTime = Opts ^ m2co_profile_time,
    CallHasReturn = find_out_if_call_has_return(IsTailCall, Results,
        Signature, CallerSignature),
    ( if
        ProfileCalls = no,
        ProfileTime = no,
        ( CallHasReturn = call_has_no_return
        ; CallHasReturn = call_has_return_expr_prefix
        )
    then
        mlds_output_call(Opts, Stream, Context, Indent, CallHasReturn,
            FuncRval, CallArgs, Results, !IO)
    else
        BodyIndent = Indent + 1,
        IndentStr = indent2_string(Indent),
        BodyIndentStr = indent2_string(BodyIndent),

        io.format(Stream, "%s{\n", [s(IndentStr)], !IO),
        (
            ProfileCalls = yes,
            mlds_output_call_profile_instr(Opts, Stream, Context, BodyIndent,
                FuncRval, CallerName, !IO)
        ;
            ProfileCalls = no
        ),
        mlds_output_call(Opts, Stream, Context, BodyIndent, CallHasReturn,
            FuncRval, CallArgs, Results, !IO),
        (
            CallHasReturn = call_has_return_stmt_suffix,
            c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
            io.format(Stream, "%sreturn;\n", [s(BodyIndentStr)], !IO)
        ;
            ( CallHasReturn = call_has_no_return
            ; CallHasReturn = call_has_return_expr_prefix
            ),
            (
                ProfileTime = yes,
                mlds_output_time_profile_instr(Opts, Stream, Context,
                    BodyIndent, CallerName, !IO)
            ;
                ProfileTime = no
            )
        ),
        io.format(Stream, "%s}\n", [s(IndentStr)], !IO)
    ).

:- pred mlds_output_call(mlds_to_c_opts::in, io.text_output_stream::in,
    prog_context::in, indent::in, maybe_call_has_return::in, mlds_rval::in,
    list(mlds_rval)::in, list(mlds_lval)::in, io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_call/10)).

mlds_output_call(Opts, Stream, Context, Indent, CallHasReturn, FuncRval,
        CallArgs, Results, !IO) :-
    IndentStr = indent2_string(Indent),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.write_string(Stream, IndentStr, !IO),
    (
        CallHasReturn = call_has_return_expr_prefix,
        io.write_string(Stream, "return ", !IO)
    ;
        ( CallHasReturn = call_has_no_return
        ; CallHasReturn = call_has_return_stmt_suffix
        )
    ),
    (
        Results = []
    ;
        Results = [Lval],
        mlds_output_lval(Opts, Lval, Stream, !IO),
        io.write_string(Stream, " = ", !IO)
    ;
        Results = [_, _ | _],
        mlds_output_return_list(Stream, mlds_output_lval(Opts), Results, !IO),
        io.write_string(Stream, " = ", !IO)
    ),
    mlds_output_bracketed_rval(Opts, Stream, FuncRval, !IO),
    io.write_string(Stream, "(", !IO),
    write_out_list(mlds_output_rval(Opts), ", ", CallArgs, Stream, !IO),
    io.write_string(Stream, ");\n", !IO).

:- type maybe_call_has_return
    --->    call_has_no_return
    ;       call_has_return_expr_prefix
    ;       call_has_return_stmt_suffix.

    % "Optimize" general tail calls by asking our caller to give hints
    % to the C compiler in the form of "return" prefixes on call expressions.
    % XXX This optimization should be disable-able.
    %
    % If Results = [], i.e. the function has `void' return type, then this
    % would result in code that is not legal ANSI C (although it _is_ legal
    % in GNU C and in C++), so for that case, we return
    % call_has_return_stmt_suffix to ask our caller to put the return
    % statement after the call.
    %
    % Note that it is only safe to add such a return statement if the
    % calling procedure has the same return types as the callee, or if
    % the calling procedure has no return value. (Calls where the types
    % are different can be marked as tail calls if they are known
    % to never return.)
    % XXX That should be "marked as no_return_calls".
    %
:- func find_out_if_call_has_return(ml_call_kind, list(mlds_lval),
    mlds_func_signature, mlds_func_signature) = maybe_call_has_return.

find_out_if_call_has_return(CallKind, Results,
        CalleeSignature, CallerSignature) = CallHasReturn :-
    (
        ( CallKind = tail_call
        ; CallKind = no_return_call
        ),
        CalleeSignature = mlds_func_signature(_, CalleeReturnTypes),
        CallerSignature = mlds_func_signature(_, CallerReturnTypes),
        ( if
            Results = [_ | _],
            CalleeReturnTypes = CallerReturnTypes
        then
            CallHasReturn = call_has_return_expr_prefix
        else if
            CallerReturnTypes = []
        then
            CallHasReturn = call_has_return_stmt_suffix
        else
            CallHasReturn = call_has_no_return
        )
    ;
        CallKind = ordinary_call,
        CallHasReturn = call_has_no_return
    ).

    % Output an instruction to record an arc in the call profile
    % between the callee and caller.
    %
:- pred mlds_output_call_profile_instr(mlds_to_c_opts::in,
    io.text_output_stream::in, prog_context::in, indent::in, mlds_rval::in,
    qual_function_name::in, io::di, io::uo) is det.

mlds_output_call_profile_instr(Opts, Stream, Context, Indent,
        CalleeFuncRval, CallerName, !IO) :-
    IndentStr = indent2_string(Indent),
    CallerNameStr = qual_function_name_to_string_for_c(CallerName),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%sMR_prof_call_profile(", [s(IndentStr)], !IO),
    mlds_output_bracketed_rval(Opts, Stream, CalleeFuncRval, !IO),
    io.format(Stream, ", %s);\n", [s(CallerNameStr)], !IO).

mlds_output_time_profile_instr(Opts, Stream, Context, Indent, FuncName, !IO) :-
    IndentStr = indent2_string(Indent),
    FuncNameStr = qual_function_name_to_string_for_c(FuncName),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%sMR_set_prof_current_proc(%s);\n",
        [s(IndentStr), s(FuncNameStr)], !IO).

%---------------------------------------------------------------------------%
%
% Output returns.
%

:- pred mlds_output_stmt_return(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_stmt::in(ml_stmt_is_return), io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_return/6)).

mlds_output_stmt_return(Opts, Stream, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_return(Results, _Context),
    IndentStr = indent2_string(Indent),
    (
        Results = [],
        io.format(Stream, "%sreturn;\n", [s(IndentStr)], !IO)
    ;
        Results = [Rval],
        io.format(Stream, "%sreturn ", [s(IndentStr)], !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_string(Stream, ";\n", !IO)
    ;
        Results = [_, _ | _],
        io.format(Stream, "%sreturn", [s(IndentStr)], !IO),
        mlds_output_return_list(Stream, mlds_output_rval(Opts), Results, !IO),
        io.write_string(Stream, ";\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output commits.
%

:- pred mlds_output_stmt_do_commit(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, mlds_stmt::in(ml_stmt_is_do_commit),
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_do_commit/6)).

mlds_output_stmt_do_commit(Opts, Stream, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_do_commit(Ref, _Context),
    IndentStr = indent2_string(Indent),
    % Output "MR_builtin_longjmp(<Ref>, 1)". This is a macro that expands
    % to either the standard longjmp() or the GNU C's __builtin_longjmp().
    % Note that the second argument to GNU C's __builtin_longjmp()
    % *must* be `1'.
    io.format(Stream, "%sMR_builtin_longjmp(", [s(IndentStr)], !IO),
    mlds_output_rval(Opts, Ref, Stream, !IO),
    io.write_string(Stream, ", 1);\n", !IO).

%---------------------------------------------------------------------------%
%
% Output try commits.
%

:- pred mlds_output_stmt_try_commit(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_try_commit), io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_try_commit/7)).

mlds_output_stmt_try_commit(Opts, Stream, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_try_commit(Ref, BodyStmt0, HandlerStmt, Context),
    % Output the following:
    %
    %   if (MR_builtin_setjmp(<Ref>) == 0)
    %       <Stmt>
    %   else
    %       <Handler>
    %
    % MR_builtin_setjmp() expands to either the standard setjmp()
    % or GNU C's __builtin_setjmp().
    %
    % Note that ISO C says that any non-volatile variables that are local
    % to the function containing the setjmp() and which are modified between
    % the setjmp() and the longjmp() become indeterminate after the longjmp().
    % The MLDS code generator handles that by generating each commit
    % in its own nested function, with the local variables remaining
    % in the containing function. This ensures that none of the variables
    % which get modified between the setjmp() and the longjmp() and which get
    % referenced after the longjmp() are local variables in the function
    % containing the setjmp(), so we do not need to mark them as volatile.
    % XXX There have not been any nested functions for a while now.

    % We need to take care to avoid problems caused by the
    % dangling else ambiguity.
    ( if BodyStmt0 = ml_stmt_if_then_else(_, _, no, Context) then
        BodyStmt = ml_stmt_block([], [], [BodyStmt0], Context)
    else
        BodyStmt = BodyStmt0
    ),

    IndentStr = indent2_string(Indent),
    io.format(Stream, "%sif (MR_builtin_setjmp(", [s(IndentStr)], !IO),
    mlds_output_lval(Opts, Ref, Stream, !IO),
    io.write_string(Stream, ") == 0)\n", !IO),

    mlds_output_statement(Opts, Stream, Indent + 1, FuncInfo, BodyStmt, !IO),

    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%selse\n", [s(IndentStr)], !IO),

    mlds_output_statement(Opts, Stream, Indent + 1, FuncInfo, HandlerStmt,
        !IO).

%---------------------------------------------------------------------------%
%
% Output atomic statements.
%

:- pred mlds_output_stmt_atomic(mlds_to_c_opts::in, io.text_output_stream::in,
    indent::in, mlds_stmt::in(ml_stmt_is_atomic), io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_atomic/6)).

mlds_output_stmt_atomic(Opts, Stream, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_atomic(AtomicStmt, Context),
    IndentStr = indent2_string(Indent),
    (
        AtomicStmt = comment(Comment),
        ( if Comment = "" then
            true
        else
            CommentLines = split_at_separator(char.is_line_separator, Comment),
            write_comment_lines(Stream, IndentStr, CommentLines, !IO)
        ),
        % If a comment statement somehow ends up constituting
        % the entirety of e.g. an if-then-else statement's then part,
        % then it needs to be a C statement syntactically.
        io.format(Stream, "%s;\n", [s(IndentStr)], !IO)
    ;
        AtomicStmt = assign(Lval, Rval),
        io.write_string(Stream, IndentStr, !IO),
        mlds_output_lval(Opts, Lval, Stream, !IO),
        io.write_string(Stream, " = ", !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_string(Stream, ";\n", !IO)
    ;
        AtomicStmt = assign_if_in_heap(Lval, Rval),
        io.format(Stream, "%sMR_assign_if_in_heap(", [s(IndentStr)], !IO),
        mlds_output_lval(Opts, Lval, Stream, !IO),
        io.write_string(Stream, ", ", !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        AtomicStmt = delete_object(Rval),
        io.format(Stream, "%sMR_free_heap(", [s(IndentStr)], !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        AtomicStmt = new_object(_, _, _, _, _, _, _, _, _),
        mlds_output_stmt_atomic_new_object(Opts, Stream, Indent, AtomicStmt,
            Context, !IO)
    ;
        AtomicStmt = gc_check,
        io.format(Stream, "%sMR_GC_check();\n", [s(IndentStr)], !IO)
    ;
        AtomicStmt = mark_hp(Lval),
        io.format(Stream, "%sMR_mark_hp(", [s(IndentStr)], !IO),
        mlds_output_lval(Opts, Lval, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        AtomicStmt = restore_hp(Rval),
        io.format(Stream, "%sMR_restore_hp(", [s(IndentStr)], !IO),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_string(Stream, ");\n", !IO)
    ;
        AtomicStmt = trail_op(_TrailOp),
        sorry($pred, "trail_ops not implemented")
    ;
        AtomicStmt = inline_target_code(TargetLang, Components),
        (
            TargetLang = ml_target_c,
            list.foldl(
                mlds_output_target_code_component(Opts, Stream, Context),
                Components, !IO)
        ;
            ( TargetLang = ml_target_csharp
            ; TargetLang = ml_target_java
            ),
            sorry($pred, "inline_target_code only works for language C")
        )
    ;
        AtomicStmt = outline_foreign_proc(_Lang, _Vs, _Lvals, _Code),
        unexpected($pred, "outline_foreign_proc is not used in C backend")
    ).

:- pred write_comment_lines(io.text_output_stream::in, string::in,
    list(string)::in, io::di, io::uo) is det.

write_comment_lines(_, _, [], !IO).
write_comment_lines(Stream, IndentStr, [CommentLine | CommentLines], !IO) :-
    ( if CommentLine = "" then
        io.nl(Stream, !IO)
    else
        io.format(Stream, "%s// %s\n", [s(IndentStr), s(CommentLine)], !IO)
    ),
    write_comment_lines(Stream, IndentStr, CommentLines, !IO).

:- pred mlds_output_stmt_atomic_new_object(mlds_to_c_opts::in,
    io.text_output_stream::in, indent::in,
    mlds_atomic_statement::in(atomic_stmt_is_new_object), prog_context::in,
    io::di, io::uo) is det.
:- pragma inline(pred(mlds_output_stmt_atomic_new_object/7)).

mlds_output_stmt_atomic_new_object(Opts, Stream, Indent, AtomicStmt,
        Context, !IO) :-
    AtomicStmt = new_object(Target, Ptag, _ExplicitSecTag, Type,
        MaybeSize, _MaybeCtorName, ArgRvalsTypes, MayUseAtomic, MaybeAllocId),
    IndentStr = indent2_string(Indent),
    Indent1Str = indent2_string(Indent + 1),
    io.format(Stream, "%s{\n", [s(IndentStr)], !IO),

    % When filling in the fields of a newly allocated cell, use a fresh
    % local variable as the base address for the field references in
    % preference to an lval that is more expensive to access. This yields
    % a speedup of about 0.3%.

    ( if Target = ml_local_var(_, _) then
        Base = ls_lval(Target)
    else
        % It does not matter what string we pick for BaseVarName,
        % as long as its declaration does not hide any of the variables
        % inside ArgRvalsTypes. This is not hard to ensure, since the printed
        % forms of the variables inside ArgRvalsTypes all include "__".
        % XXX Actually, they do not include "__" anymore.
        BaseVarName = "base",
        Base = ls_string(BaseVarName),
        type_to_prefix_suffix_for_c(Opts, Type, no_size,
            TypePrefix, TypeSuffix),

        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%s%s %s%s;\n",
            [s(Indent1Str), s(TypePrefix), s(BaseVarName), s(TypeSuffix)], !IO)
    ),

    % For --gc accurate, we need to insert a call to GC_check()
    % before every allocation.
    GC_Method = Opts ^ m2co_gc_method,
    (
        GC_Method = gc_accurate,
        % XXX What is this context for?
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.format(Stream, "%sMR_GC_check();\n", [s(Indent1Str)], !IO),
        % For types which hold RTTI that will be traversed by the collector
        % at GC-time, we need to allocate an extra word at the start,
        % to hold the forwarding pointer. Normally we would just overwrite
        % the first word of the object in the "from" space, but this
        % cannot be done for objects which will be referenced during
        % the garbage collection process.
        NeedsForwardingSpace = type_needs_forwarding_pointer_space(Type),
        (
            NeedsForwardingSpace = yes,
            % XXX What is this context for?
            c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
            io.format(Stream, "%s// reserve space for GC forwarding pointer\n",
                [s(Indent1Str)], !IO),
            % XXX What is this context for?
            c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
            io.format(Stream, "%sMR_hp_alloc(1);\n", [s(Indent1Str)], !IO)
        ;
            NeedsForwardingSpace = no
        )
    ;
        ( GC_Method = gc_none
        ; GC_Method = gc_boehm
        ; GC_Method = gc_boehm_debug
        ; GC_Method = gc_hgc
        ; GC_Method = gc_automatic
        )
    ),

    TypeStr = type_to_string_for_c(Opts, Type),
    (
        MayUseAtomic = may_not_use_atomic_alloc,
        NewObjectMacro = "MR_new_object"
    ;
        MayUseAtomic = may_use_atomic_alloc,
        NewObjectMacro = "MR_new_object_atomic"
    ),
    MaybeAllocIdStr = maybe_alloc_id_to_string(MaybeAllocId),

    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.write_string(Stream, Indent1Str, !IO),
    write_lval_or_string(Opts, Stream, Base, !IO),
    io.write_string(Stream, " = ", !IO),
    Ptag = ptag(PtagUInt8),

    % This if-then-else prints the first arg of NewObjectMacro, the type,
    % and the comma after it.
    ( if PtagUInt8 = 0u8 then
        % XXX We should not need the cast here, but currently the type that
        % we include in the call to MR_new_object() is not always correct.
        io.format(Stream, "(%s) %s(%s, ",
            [s(TypeStr), s(NewObjectMacro), s(TypeStr)], !IO),
        EndMkword = ""
    else
        io.format(Stream, "(%s) MR_mkword(%u, %s(%s, ",
            [s(TypeStr), u8(PtagUInt8), s(NewObjectMacro), s(TypeStr)], !IO),
        EndMkword = ")"
    ),
    % This if-then-else prints the second arg of NewObjectMacro, the size,
    % but not the comma after it.
    (
        MaybeSize = yes(Size),
        io.write_string(Stream, "(", !IO),
        ( if Size = ml_const(mlconst_int(SizeInt)) then
            io.write_int(Stream, SizeInt, !IO)
        else
            mlds_output_rval(Opts, Size, Stream, !IO)
        ),
        io.write_string(Stream, " * sizeof(MR_Word))", !IO)
    ;
        MaybeSize = no,
        % XXX what should we do here?
        io.write_int(Stream, -1, !IO)
    ),
    % This prints the third and fourth args of NewObjectMacro,
    % the alloc id and the name. The name, which we always set to NULL,
    % is used only for memory profiling.
    io.format(Stream, ", %s, NULL)%s;\n",
        [s(MaybeAllocIdStr), s(EndMkword)], !IO),
    (
        Base = ls_lval(_)
    ;
        Base = ls_string(BaseVarName1),
        c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
        io.write_string(Stream, Indent1Str, !IO),
        mlds_output_lval(Opts, Target, Stream, !IO),
        io.format(Stream, " = %s;\n", [s(BaseVarName1)], !IO)
    ),
    mlds_output_init_args(Opts, Stream, ArgRvalsTypes, Context, 0, Base, Ptag,
        Indent + 1, !IO),
    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%s}\n", [s(IndentStr)], !IO).

:- func maybe_alloc_id_to_string(maybe(mlds_alloc_id)) = string.

maybe_alloc_id_to_string(MaybeAllocId) = MaybeAllocIdStr :-
    (
        MaybeAllocId = yes(mlds_alloc_id(Num)),
        string.format("&MR_alloc_sites[%d]", [i(Num)], MaybeAllocIdStr)
    ;
        MaybeAllocId = no,
        MaybeAllocIdStr = "NULL"
    ).

:- pred mlds_output_target_code_component(mlds_to_c_opts::in,
    io.text_output_stream::in, prog_context::in, target_code_component::in,
    io::di, io::uo) is det.

mlds_output_target_code_component(Opts, Stream, Context, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext),
        LineNumbers = Opts ^ m2co_line_numbers,
        (
            MaybeUserContext = yes(UserContext),
            InitContext = UserContext
        ;
            MaybeUserContext = no,
            InitContext = Context
        ),
        c_output_context(Stream, LineNumbers, InitContext, !IO),
        io.write_string(Stream, CodeString, !IO),
        io.write_string(Stream, "\n", !IO),
        c_reset_context(Stream, LineNumbers, !IO)
    ;
        TargetCode = raw_target_code(CodeString),
        io.write_string(Stream, CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        mlds_output_rval(Opts, Rval, Stream, !IO),
        io.write_string(Stream, " ", !IO)
    ;
        TargetCode = target_code_output(Lval),
        mlds_output_lval(Opts, Lval, Stream, !IO),
        io.write_string(Stream, " ", !IO)
    ;
        TargetCode = target_code_type(Type),
        TypeStr = type_to_string_for_c(Opts, Type),
        io.format(Stream, "%s ", [s(TypeStr)], !IO)
    ;
        % Note: `target_code_name(Name)' target_code_components are used to
        % generate the #define for `MR_PROC_LABEL'.
        % The fact that they are used in a #define means that we cannot do
        % an output_context(Context) here, since #line directives
        % are not allowed inside #defines.
        % Similarly, all the target_code_components except user_target_code
        % can get emitted inside calls to the MR_BOX_FOREIGN_TYPE
        % or MR_UNBOX_FOREIGN_TYPE macros, which means that we cannot output
        % the contexts for those either, since #line directives are
        % not allowed inside macro invocations in standard C
        % (although some compilers, e.g. gcc 3.2, do allow it).

        TargetCode = target_code_function_name(FuncName),
        FuncNameStr = qual_function_name_to_string_for_c(FuncName),
        io.format(Stream, "%s\n", [s(FuncNameStr)], !IO)
    ;
        TargetCode = target_code_alloc_id(AllocId),
        MaybeAllocIdStr = maybe_alloc_id_to_string(yes(AllocId)),
        io.write_string(Stream, MaybeAllocIdStr, !IO)
    ).

:- func type_needs_forwarding_pointer_space(mlds_type) = bool.

type_needs_forwarding_pointer_space(Type) = NeedsForwardingPtrSpace :-
    (
        ( Type = mlds_type_info_type
        ; Type = mlds_pseudo_type_info_type
        ),
        NeedsForwardingPtrSpace = yes
    ;
        ( Type = mlds_mercury_array_type(_)
        ; Type = mlds_cont_type(_)
        ; Type = mlds_commit_type
        ; Type = mlds_native_bool_type
        ; Type = mlds_builtin_type_int(_)
        ; Type = mlds_builtin_type_float
        ; Type = mlds_builtin_type_string
        ; Type = mlds_builtin_type_char
        ; Type = mlds_foreign_type(_)
        ; Type = mlds_class_type(_)
        ; Type = mlds_enum_class_type(_)
        ; Type = mlds_env_type(_)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ),
        NeedsForwardingPtrSpace = no
    ;
        Type = mercury_nb_type(_, TypeCategory),
        NeedsForwardingPtrSpace =
            is_introduced_type_info_type_category(TypeCategory)
    ;
        Type = mlds_rtti_type(_),
        % These should all be statically allocated, not dynamically allocated,
        % so we should never get here.
        unexpected($pred, "rtti_type")
    ;
        Type = mlds_tabling_type(_),
        % These should all be statically allocated, not dynamically allocated,
        % so we should never get here.
        unexpected($pred, "tabling_type")
    ;
        Type = mlds_unknown_type,
        unexpected($pred, "unknown_type")
    ).

:- type lval_or_string
    --->    ls_lval(mlds_lval)
    ;       ls_string(string).

:- pred mlds_output_init_args(mlds_to_c_opts::in, io.text_output_stream::in,
    list(mlds_typed_rval)::in, prog_context::in, int::in, lval_or_string::in,
    ptag::in, indent::in, io::di, io::uo) is det.

mlds_output_init_args(_, _, [], _, _, _, _, _, !IO).
mlds_output_init_args(Opts, Stream, [ArgRvalType | ArgRvalsTypes], Context,
        ArgNum, Base, Ptag, Indent, !IO) :-
    % The MR_hl_field() macro expects its argument to have type MR_Box,
    % so we need to box the arguments if they are not already boxed.
    % Hence the use of mlds_output_boxed_rval below.
    IndentStr = indent2_string(Indent),
    Ptag = ptag(PtagUInt8),
    ArgRvalType = ml_typed_rval(ArgRval, ArgType),

    c_output_context(Stream, Opts ^ m2co_line_numbers, Context, !IO),
    io.format(Stream, "%sMR_hl_field(%u, ",
        [s(IndentStr), u8(PtagUInt8)], !IO),
    write_lval_or_string(Opts, Stream, Base, !IO),
    io.format(Stream, ", %d) = ", [i(ArgNum)], !IO),
    mlds_output_boxed_rval(Opts, Stream, ArgType, ArgRval, !IO),
    io.write_string(Stream, ";\n", !IO),
    mlds_output_init_args(Opts, Stream, ArgRvalsTypes, Context,
        ArgNum + 1, Base, Ptag, Indent, !IO).

:- pred write_lval_or_string(mlds_to_c_opts::in, io.text_output_stream::in,
    lval_or_string::in, io::di, io::uo) is det.

write_lval_or_string(Opts, Stream, Base, !IO) :-
    (
        Base = ls_lval(Target),
        mlds_output_lval(Opts, Target, Stream, !IO)
    ;
        Base = ls_string(BaseVarName),
        io.write_string(Stream, BaseVarName, !IO)
    ).

%---------------------------------------------------------------------------%

mlds_output_gc_statement(Opts, Stream, Indent, GCStmt, MaybeNewLine, !IO) :-
    (
        GCStmt = gc_no_stmt
    ;
        (
            GCStmt = gc_trace_code(Stmt),
            Label = "#if 0 // GC trace code\n"
        ;
            GCStmt = gc_initialiser(Stmt),
            Label = "#if 0 // GC initialiser\n"
        ),
        io.write_string(Stream, MaybeNewLine, !IO),
        io.write_string(Stream, Label, !IO),
        % XXX This value for FuncInfo is bogus. However, this output is only
        % for debugging anyway, so it does not really matter.
        ModuleName = mercury_module_name_to_mlds(unqualified("")),
        FuncName = mlds_function_export("dummy"),
        QualFuncName = qual_function_name(ModuleName, FuncName),
        FuncInfo = func_info_c(QualFuncName, mlds_func_signature([], [])),
        mlds_output_statement(Opts, Stream, Indent, FuncInfo, Stmt, !IO),
        io.write_string(Stream, "#endif\n", !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_stmt.
%---------------------------------------------------------------------------%
