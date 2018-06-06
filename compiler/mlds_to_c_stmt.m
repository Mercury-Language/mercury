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

:- import_module ml_backend.mlds.
:- import_module ml_backend.mlds_to_c_util.
:- import_module ml_backend.mlds_to_target_util.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.

%---------------------------------------------------------------------------%

:- type func_info_c
    --->    func_info_c(qual_function_name, mlds_func_signature).

:- pred mlds_output_statement(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

    % Output an instruction which informs the runtime which procedure
    % we are currently located in.
    %
:- pred mlds_output_time_profile_instr(mlds_to_c_opts::in,
    prog_context::in, indent::in, qual_function_name::in,
    io::di, io::uo) is det.

:- pred mlds_output_gc_statement(mlds_to_c_opts::in, indent::in,
    mlds_gc_statement::in, string::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module libs.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.mlds_to_c_data.
:- import_module ml_backend.mlds_to_c_name.
:- import_module ml_backend.mlds_to_c_type.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module char.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

:- pred mlds_output_statements(mlds_to_c_opts::in, indent::in, func_info_c::in,
    list(mlds_stmt)::in, io::di, io::uo) is det.

mlds_output_statements(_Opts, _Indent, _FuncInfo, [], !IO).
mlds_output_statements(Opts, Indent, FuncInfo, [Stmt | Stmts], !IO) :-
    mlds_output_statement(Opts, Indent, FuncInfo, Stmt, !IO),
    mlds_output_statements(Opts, Indent, FuncInfo, Stmts, !IO).

mlds_output_statement(Opts, Indent, FuncInfo, Stmt, !IO) :-
    c_output_stmt_context(Opts ^ m2co_line_numbers, Stmt, !IO),
    (
        Stmt = ml_stmt_block(_LocalVarDefns, _FuncDefns, _SubStmts, _Context),
        mlds_output_stmt_block(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_while(_Kind, _Cond, _BodyStmt, _LoopLocalVars, _Context),
        mlds_output_stmt_while(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_if_then_else(_Cond, _Then, _MaybeElse, _Context),
        mlds_output_stmt_if_then_else(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_switch(_Type, _Val, _Range, _Cases, _Default, _Context),
        mlds_output_stmt_switch(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_label(_LabelName, _Context),
        mlds_output_stmt_label(Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_goto(_Target, _Context),
        mlds_output_stmt_goto(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_computed_goto(_Expr, _Labels, _Context),
        mlds_output_stmt_computed_goto(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_call(_Signature, _FuncRval, _CallArgs,
            _Results, _IsTailCall, _Context),
        mlds_output_stmt_call(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_return(_Results, _Context),
        mlds_output_stmt_return(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_do_commit(_Ref, _Context),
        mlds_output_stmt_do_commit(Opts, Indent, Stmt, !IO)
    ;
        Stmt = ml_stmt_try_commit(_Ref, _BodyStmt0, _HandlerStmt, _Context),
        mlds_output_stmt_try_commit(Opts, Indent, FuncInfo, Stmt, !IO)
    ;
        Stmt = ml_stmt_atomic(_AtomicStmt, _Context),
        mlds_output_stmt_atomic(Opts, Indent, Stmt, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output blocks.
%

:- pred mlds_output_stmt_block(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_block), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_block/6).

mlds_output_stmt_block(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, Context),
    BraceIndent = Indent,
    BlockIndent = Indent + 1,
    output_n_indents(BraceIndent, !IO),
    io.write_string("{\n", !IO),

    % Output forward declarations for any nested functions defined in
    % this block, in case they are referenced before they are defined.
    (
        FuncDefns = [_ | _],
        unexpected($pred, "FuncDefns != []")
    ;
        FuncDefns = []
    ),
    (
        LocalVarDefns = [_ | _],
        mlds_output_local_var_defns(Opts, BlockIndent, no, LocalVarDefns, !IO),
        io.write_string("\n", !IO)
    ;
        LocalVarDefns = []
    ),
    mlds_output_statements(Opts, BlockIndent, FuncInfo, SubStmts, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(BraceIndent, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_local_var_defns(mlds_to_c_opts::in, indent::in, bool::in,
    list(mlds_local_var_defn)::in, io::di, io::uo) is det.

mlds_output_local_var_defns(_Opts, _Indent, _Separate, [], !IO).
mlds_output_local_var_defns(Opts, Indent, Separate,
        [LocalVarDefn | LocalVarDefns], !IO) :-
    mlds_output_local_var_defn(Opts, Indent, Separate, LocalVarDefn, !IO),
    mlds_output_local_var_defns(Opts, Indent, Separate, LocalVarDefns, !IO).

:- pred mlds_output_local_var_defn(mlds_to_c_opts::in, indent::in, bool::in,
    mlds_local_var_defn::in, io::di, io::uo) is det.

mlds_output_local_var_defn(Opts, Indent, Separate, LocalVarDefn, !IO) :-
    LocalVarDefn = mlds_local_var_defn(LocalVarName, Context,
        Type, Initializer, GCStmt),
    (
        Separate = yes,
        io.nl(!IO)
    ;
        Separate = no
    ),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    mlds_output_local_var_decl(Opts, LocalVarName, Type,
        get_initializer_array_size(Initializer), !IO),
    mlds_output_initializer(Opts, Type, Initializer, !IO),
    io.write_string(";\n", !IO),
    mlds_output_gc_statement(Opts, Indent, GCStmt, "", !IO).

:- pred mlds_output_local_var_decl(mlds_to_c_opts::in, mlds_local_var_name::in,
    mlds_type::in, initializer_array_size::in, io::di, io::uo) is det.

mlds_output_local_var_decl(Opts, LocalVarName, Type, InitializerSize, !IO) :-
    mlds_output_type_prefix(Opts, Type, !IO),
    io.write_char(' ', !IO),
    mlds_output_local_var_name(LocalVarName, !IO),
    mlds_output_type_suffix(Opts, Type, InitializerSize, !IO).

%---------------------------------------------------------------------------%
%
% Output while loops.
%

:- pred mlds_output_stmt_while(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_while), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_while/6).

mlds_output_stmt_while(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_while(Kind, Cond, BodyStmt, _LoopLocalVars, Context),
    scope_indent(BodyStmt, Indent, ScopeIndent),
    BodyOpts = Opts ^ m2co_break_context := bc_loop,
    (
        Kind = may_loop_zero_times,
        output_n_indents(Indent, !IO),
        io.write_string("while (", !IO),
        mlds_output_rval(Opts, Cond, !IO),
        io.write_string(")\n", !IO),
        mlds_output_statement(BodyOpts, ScopeIndent, FuncInfo, BodyStmt, !IO)
    ;
        Kind = loop_at_least_once,
        output_n_indents(Indent, !IO),
        io.write_string("do\n", !IO),
        mlds_output_statement(BodyOpts, ScopeIndent, FuncInfo, BodyStmt, !IO),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("while (", !IO),
        mlds_output_rval(Opts, Cond, !IO),
        io.write_string(");\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output if-then-elses.
%

:- pred mlds_output_stmt_if_then_else(mlds_to_c_opts::in, indent::in,
    func_info_c::in, mlds_stmt::in(ml_stmt_is_if_then_else),
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_if_then_else/6).

mlds_output_stmt_if_then_else(Opts, Indent, FuncInfo, Stmt, !IO) :-
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

    output_n_indents(Indent, !IO),
    io.write_string("if (", !IO),
    mlds_output_rval(Opts, Cond, !IO),
    io.write_string(")\n", !IO),
    scope_indent(Then, Indent, ScopeIndent),
    mlds_output_statement(Opts, ScopeIndent, FuncInfo, Then, !IO),
    (
        MaybeElse = yes(Else),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("else\n", !IO),
        ( if Else = ml_stmt_if_then_else(_, _, _, _) then
            % Indent each if-then-else in a if-then-else chain
            % to the same depth.
            ElseScopeIndent = Indent
        else
            scope_indent(Else, Indent, ElseScopeIndent)
        ),
        mlds_output_statement(Opts, ElseScopeIndent, FuncInfo, Else, !IO)
    ;
        MaybeElse = no
    ).

%---------------------------------------------------------------------------%
%
% Output switch statements.
%

:- pred mlds_output_stmt_switch(mlds_to_c_opts::in, indent::in,
    func_info_c::in, mlds_stmt::in(ml_stmt_is_switch), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_switch/6).

mlds_output_stmt_switch(Opts, Indent, FuncInfo, Stmt, !IO) :-
    Stmt = ml_stmt_switch(_Type, Val, _Range, Cases, Default, Context),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("switch (", !IO),
    mlds_output_rval(Opts, Val, !IO),
    io.write_string(") {\n", !IO),
    CaseOpts = Opts ^ m2co_break_context := bc_switch,
    % We put the default case first, so that if it is unreachable,
    % it will get merged in with the first case.
    mlds_output_switch_default(CaseOpts, Indent + 1, FuncInfo, Context,
        Default, !IO),
    list.foldl(
        mlds_output_switch_case(CaseOpts, Indent + 1, FuncInfo, Context),
        Cases, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_switch_case(mlds_to_c_opts::in, indent::in,
    func_info_c::in, prog_context::in, mlds_switch_case::in,
    io::di, io::uo) is det.

mlds_output_switch_case(Opts, Indent, FuncInfo, Context, Case, !IO) :-
    Case = mlds_switch_case(FirstCond, LaterConds, Stmt),
    mlds_output_case_cond(Opts, Indent, Context, FirstCond, !IO),
    list.foldl(mlds_output_case_cond(Opts, Indent, Context), LaterConds, !IO),
    mlds_output_statement(Opts, Indent + 1, FuncInfo, Stmt, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("break;\n", !IO).

:- pred mlds_output_case_cond(mlds_to_c_opts::in, indent::in, prog_context::in,
    mlds_case_match_cond::in, io::di, io::uo) is det.

mlds_output_case_cond(Opts, Indent, Context, Match, !IO) :-
    (
        Match = match_value(Val),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("case ", !IO),
        mlds_output_rval(Opts, Val, !IO),
        io.write_string(":\n", !IO)
    ;
        Match = match_range(Low, High),
        % This uses the GNU C extension `case <Low> ... <High>:'.
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("case ", !IO),
        mlds_output_rval(Opts, Low, !IO),
        io.write_string(" ... ", !IO),
        mlds_output_rval(Opts, High, !IO),
        io.write_string(":\n", !IO)
    ).

:- pred mlds_output_switch_default(mlds_to_c_opts::in, indent::in,
    func_info_c::in, prog_context::in, mlds_switch_default::in, io::di, io::uo)
    is det.

mlds_output_switch_default(Opts, Indent, FuncInfo, Context, Default, !IO) :-
    (
        Default = default_is_unreachable,
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("default: /*NOTREACHED*/ MR_assert(0);\n", !IO)
    ;
        Default = default_do_nothing
    ;
        Default = default_case(Stmt),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent, !IO),
        io.write_string("default:\n", !IO),
        mlds_output_statement(Opts, Indent + 1, FuncInfo, Stmt, !IO),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        io.write_string("break;\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output labels.
%

:- pred mlds_output_stmt_label(indent::in, mlds_stmt::in(ml_stmt_is_label),
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_label/4).

mlds_output_stmt_label(Indent, Stmt, !IO) :-
    Stmt = ml_stmt_label(LabelName, _Context),
    % Note: MLDS allows labels at the end of blocks. C does not.
    % Hence we need to insert a semi-colon after the colon to ensure that
    % there is a statement to attach the label to.

    output_n_indents(Indent - 1, !IO),
    mlds_output_label_name(LabelName, !IO),
    io.write_string(":;\n", !IO).

:- pred mlds_output_label_name(mlds_label::in, io::di, io::uo) is det.

mlds_output_label_name(LabelName, !IO) :-
    io.write_string(name_mangle(LabelName), !IO).

%---------------------------------------------------------------------------%
%
% Output gotos.
%

:- pred mlds_output_stmt_goto(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_goto), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_goto/5).

mlds_output_stmt_goto(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_goto(Target, _Context),
    output_n_indents(Indent, !IO),
    (
        Target = goto_label(LabelName),
        io.write_string("goto ", !IO),
        mlds_output_label_name(LabelName, !IO),
        io.write_string(";\n", !IO)
    ;
        Target = goto_break_switch,
        BreakContext = Opts ^ m2co_break_context,
        (
            BreakContext = bc_switch,
            io.write_string("break;\n", !IO)
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
            io.write_string("break;\n", !IO)
        ;
            ( BreakContext = bc_none
            ; BreakContext = bc_switch
            ),
            unexpected($pred, "goto_break_loop not in loop")
        )
    ;
        Target = goto_continue_loop,
        io.write_string("continue;\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Output computed gotos.
%

:- pred mlds_output_stmt_computed_goto(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_computed_goto), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_computed_goto/5).

mlds_output_stmt_computed_goto(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_computed_goto(Expr, Labels, Context),
    % XXX For GNU C, we could output potentially more efficient code
    % by using an array of labels; this would tell the compiler that
    % it did not need to do any range check.
    output_n_indents(Indent, !IO),
    io.write_string("switch (", !IO),
    mlds_output_rval(Opts, Expr, !IO),
    io.write_string(") {\n", !IO),
    list.foldl2(mlds_output_computed_goto_label(Opts, Context, Indent),
        Labels, 0, _FinalCount, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("default: /*NOTREACHED*/ MR_assert(0);\n", !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_computed_goto_label(mlds_to_c_opts::in, prog_context::in,
    int::in, mlds_label::in, int::in, int::out, io::di, io::uo) is det.

mlds_output_computed_goto_label(Opts, Context, Indent, Label, Count0, Count,
        !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    io.write_string("case ", !IO),
    io.write_int(Count0, !IO),
    io.write_string(": goto ", !IO),
    mlds_output_label_name(Label, !IO),
    io.write_string(";\n", !IO),
    Count = Count0 + 1.

%---------------------------------------------------------------------------%
%
% Output calls.
%

:- pred mlds_output_stmt_call(mlds_to_c_opts::in, indent::in, func_info_c::in,
    mlds_stmt::in(ml_stmt_is_call), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_call/6).

mlds_output_stmt_call(Opts, Indent, FuncInfo, Stmt, !IO) :-
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
        mlds_output_call(Opts, Context, Indent, CallHasReturn,
            FuncRval, CallArgs, Results, !IO)
    else
        BodyIndent = Indent + 1,

        output_n_indents(Indent, !IO),
        io.write_string("{\n", !IO),
        (
            ProfileCalls = yes,
            mlds_output_call_profile_instr(Opts, Context, BodyIndent,
                FuncRval, CallerName, !IO)
        ;
            ProfileCalls = no
        ),
        mlds_output_call(Opts, Context, BodyIndent, CallHasReturn,
            FuncRval, CallArgs, Results, !IO),
        (
            CallHasReturn = call_has_return_stmt_suffix,
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
            output_n_indents(BodyIndent, !IO),
            io.write_string("return;\n", !IO)
        ;
            ( CallHasReturn = call_has_no_return
            ; CallHasReturn = call_has_return_expr_prefix
            ),
            (
                ProfileTime = yes,
                mlds_output_time_profile_instr(Opts, Context, BodyIndent,
                    CallerName, !IO)
            ;
                ProfileTime = no
            )
        ),
        output_n_indents(Indent, !IO),
        io.write_string("}\n", !IO)
    ).

:- pred mlds_output_call(mlds_to_c_opts::in, prog_context::in, indent::in,
    maybe_call_has_return::in, mlds_rval::in,
    list(mlds_rval)::in, list(mlds_lval)::in, io::di, io::uo) is det.
:- pragma inline(mlds_output_call/9).

mlds_output_call(Opts, Context, Indent, CallHasReturn, FuncRval,
        CallArgs, Results, !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    (
        CallHasReturn = call_has_return_expr_prefix,
        io.write_string("return ", !IO)
    ;
        ( CallHasReturn = call_has_no_return
        ; CallHasReturn = call_has_return_stmt_suffix
        )
    ),
    (
        Results = []
    ;
        Results = [Lval],
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(" = ", !IO)
    ;
        Results = [_, _ | _],
        mlds_output_return_list(Results, mlds_output_lval(Opts), !IO),
        io.write_string(" = ", !IO)
    ),
    mlds_output_bracketed_rval(Opts, FuncRval, !IO),
    io.write_string("(", !IO),
    io.write_list(CallArgs, ", ", mlds_output_rval(Opts), !IO),
    io.write_string(");\n", !IO).

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

find_out_if_call_has_return(IsTailCall, Results,
        CalleeSignature, CallerSignature) = CallHasReturn :-
    ( if
        ( IsTailCall = tail_call
        ; IsTailCall = no_return_call
        )
    then
        CalleeSignature = mlds_func_signature(_, CalleeRetTypes),
        CallerSignature = mlds_func_signature(_, CallerRetTypes),
        ( if
            Results = [_ | _],
            CalleeRetTypes = CallerRetTypes
        then
            CallHasReturn = call_has_return_expr_prefix
        else if
            CallerRetTypes = []
        then
            CallHasReturn = call_has_return_stmt_suffix
        else
            CallHasReturn = call_has_no_return
        )
    else
        CallHasReturn = call_has_no_return
    ).

    % Output an instruction to record an arc in the call profile
    % between the callee and caller.
    %
:- pred mlds_output_call_profile_instr(mlds_to_c_opts::in,
    prog_context::in, indent::in, mlds_rval::in,
    qual_function_name::in, io::di, io::uo) is det.

mlds_output_call_profile_instr(Opts, Context, Indent,
        CalleeFuncRval, CallerName, !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("MR_prof_call_profile(", !IO),
    mlds_output_bracketed_rval(Opts, CalleeFuncRval, !IO),
    io.write_string(", ", !IO),
    mlds_output_fully_qualified_function_name(CallerName, !IO),
    io.write_string(");\n", !IO).

mlds_output_time_profile_instr(Opts, Context, Indent, QualFuncName, !IO) :-
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("MR_set_prof_current_proc(", !IO),
    mlds_output_fully_qualified_function_name(QualFuncName, !IO),
    io.write_string(");\n", !IO).

%---------------------------------------------------------------------------%
%
% Output returns.
%

:- pred mlds_output_stmt_return(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_return), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_return/5).

mlds_output_stmt_return(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_return(Results, _Context),
    output_n_indents(Indent, !IO),
    io.write_string("return", !IO),
    (
        Results = []
    ;
        Results = [Rval],
        io.write_char(' ', !IO),
        mlds_output_rval(Opts, Rval, !IO)
    ;
        Results = [_, _ | _],
        mlds_output_return_list(Results, mlds_output_rval(Opts), !IO)
    ),
    io.write_string(";\n", !IO).

%---------------------------------------------------------------------------%
%
% Output commits.
%

:- pred mlds_output_stmt_do_commit(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_do_commit), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_do_commit/5).

mlds_output_stmt_do_commit(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_do_commit(Ref, _Context),
    output_n_indents(Indent, !IO),
    % Output "MR_builtin_longjmp(<Ref>, 1)". This is a macro that expands
    % to either the standard longjmp() or the GNU C's __builtin_longjmp().
    % Note that the second argument to GNU C's __builtin_longjmp()
    % *must* be `1'.
    io.write_string("MR_builtin_longjmp(", !IO),
    mlds_output_rval(Opts, Ref, !IO),
    io.write_string(", 1);\n", !IO).

%---------------------------------------------------------------------------%
%
% Output try commits.
%

:- pred mlds_output_stmt_try_commit(mlds_to_c_opts::in, indent::in,
    func_info_c::in, mlds_stmt::in(ml_stmt_is_try_commit),
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_try_commit/6).

mlds_output_stmt_try_commit(Opts, Indent, FuncInfo, Stmt, !IO) :-
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

    output_n_indents(Indent, !IO),
    io.write_string("if (MR_builtin_setjmp(", !IO),
    mlds_output_lval(Opts, Ref, !IO),
    io.write_string(") == 0)\n", !IO),

    mlds_output_statement(Opts, Indent + 1, FuncInfo, BodyStmt, !IO),

    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("else\n", !IO),

    mlds_output_statement(Opts, Indent + 1, FuncInfo, HandlerStmt, !IO).

%---------------------------------------------------------------------------%
%
% Output atomic statements.
%

:- pred mlds_output_stmt_atomic(mlds_to_c_opts::in, indent::in,
    mlds_stmt::in(ml_stmt_is_atomic), io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_atomic/5).

mlds_output_stmt_atomic(Opts, Indent, Stmt, !IO) :-
    Stmt = ml_stmt_atomic(AtomicStmt, Context),
    (
        AtomicStmt = comment(Comment),
        ( if Comment = "" then
            true
        else
            CommentLines = split_at_separator(char.is_line_separator, Comment),
            write_comment_lines(Indent, CommentLines, !IO)
        ),
        % If a comment statement somehow ends up constituting
        % the entirety of e.g. an if-then-else statement's then part,
        % then it needs to be a C statement syntactically.
        output_n_indents(Indent, !IO),
        io.write_string(";\n", !IO)
    ;
        AtomicStmt = assign(Lval, Rval),
        output_n_indents(Indent, !IO),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(" = ", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(";\n", !IO)
    ;
        AtomicStmt = assign_if_in_heap(Lval, Rval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_assign_if_in_heap(", !IO),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(", ", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = delete_object(Rval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_free_heap(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = new_object(_Target, _Ptag, _ExplicitSecTag, _Type,
            _MaybeSize, _MaybeCtorName, _ArgRvalsTypes, _MayUseAtomic,
            _MaybeAllocId),
        mlds_output_stmt_atomic_new_object(Opts, Indent, AtomicStmt, Context,
            !IO)
    ;
        AtomicStmt = gc_check,
        output_n_indents(Indent, !IO),
        io.write_string("MR_GC_check();\n", !IO)
    ;
        AtomicStmt = mark_hp(Lval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_mark_hp(", !IO),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = restore_hp(Rval),
        output_n_indents(Indent, !IO),
        io.write_string("MR_restore_hp(", !IO),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(");\n", !IO)
    ;
        AtomicStmt = trail_op(_TrailOp),
        sorry($pred, "trail_ops not implemented")
    ;
        AtomicStmt = inline_target_code(TargetLang, Components),
        (
            TargetLang = ml_target_c,
            list.foldl(mlds_output_target_code_component(Opts, Context),
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

:- pred write_comment_lines(int::in, list(string)::in, io::di, io::uo) is det.

write_comment_lines(_Indent, [], !IO).
write_comment_lines(Indent, [CommentLine | CommentLines], !IO) :-
    ( if CommentLine = "" then
        io.nl(!IO)
    else
        output_n_indents(Indent, !IO),
        io.write_string("// ", !IO),
        io.write_string(CommentLine, !IO),
        io.nl(!IO)
    ),
    write_comment_lines(Indent, CommentLines, !IO).

:- pred mlds_output_stmt_atomic_new_object(mlds_to_c_opts::in, indent::in,
    mlds_atomic_statement::in(atomic_stmt_is_new_object), prog_context::in,
    io::di, io::uo) is det.
:- pragma inline(mlds_output_stmt_atomic_new_object/6).

mlds_output_stmt_atomic_new_object(Opts, Indent, AtomicStmt, Context, !IO) :-
    AtomicStmt = new_object(Target, Ptag, _ExplicitSecTag, Type,
        MaybeSize, _MaybeCtorName, ArgRvalsTypes, MayUseAtomic, MaybeAllocId),
    output_n_indents(Indent, !IO),
    io.write_string("{\n", !IO),

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
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        mlds_output_type_prefix(Opts, Type, !IO),
        io.write_string(" ", !IO),
        io.write_string(BaseVarName, !IO),
        mlds_output_type_suffix(Opts, Type, no_size, !IO),
        io.write_string(";\n", !IO)
    ),

    % For --gc accurate, we need to insert a call to GC_check()
    % before every allocation.
    GC_Method = Opts ^ m2co_gc_method,
    (
        GC_Method = gc_accurate,
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        io.write_string("MR_GC_check();\n", !IO),
        % For types which hold RTTI that will be traversed by the collector
        % at GC-time, we need to allocate an extra word at the start,
        % to hold the forwarding pointer. Normally we would just overwrite
        % the first word of the object in the "from" space, but this
        % cannot be done for objects which will be referenced during
        % the garbage collection process.
        NeedsForwardingSpace = type_needs_forwarding_pointer_space(Type),
        (
            NeedsForwardingSpace = yes,
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
            output_n_indents(Indent + 1, !IO),
            io.write_string("// reserve space for GC forwarding pointer\n",
                !IO),
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
            output_n_indents(Indent + 1, !IO),
            io.write_string("MR_hp_alloc(1);\n", !IO)
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

    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent + 1, !IO),
    write_lval_or_string(Opts, Base, !IO),
    io.write_string(" = ", !IO),
    ( if Ptag = ptag(0u8) then
        % XXX We should not need the cast here, but currently the type that
        % we include in the call to MR_new_object() is not always correct.
        mlds_output_cast(Opts, Type, !IO),
        EndMkword = ""
    else
        mlds_output_cast(Opts, Type, !IO),
        io.write_string("MR_mkword(", !IO),
        mlds_output_ptag(Ptag, !IO),
        io.write_string(", ", !IO),
        EndMkword = ")"
    ),
    (
        MayUseAtomic = may_not_use_atomic_alloc,
        io.write_string("MR_new_object(", !IO)
    ;
        MayUseAtomic = may_use_atomic_alloc,
        io.write_string("MR_new_object_atomic(", !IO)
    ),
    mlds_output_type(Opts, Type, !IO),
    io.write_string(", ", !IO),
    (
        MaybeSize = yes(Size),
        io.write_string("(", !IO),
        mlds_output_rval(Opts, Size, !IO),
        io.write_string(" * sizeof(MR_Word))", !IO)
    ;
        MaybeSize = no,
        % XXX what should we do here?
        io.write_int(-1, !IO)
    ),
    io.write_string(", ", !IO),
    mlds_output_maybe_alloc_id(MaybeAllocId, !IO),
    io.write_string(", NULL)", !IO),
    io.write_string(EndMkword, !IO),
    io.write_string(";\n", !IO),
    (
        Base = ls_lval(_)
    ;
        Base = ls_string(BaseVarName1),
        c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
        output_n_indents(Indent + 1, !IO),
        mlds_output_lval(Opts, Target, !IO),
        io.write_string(" = ", !IO),
        io.write_string(BaseVarName1, !IO),
        io.write_string(";\n", !IO)
    ),
    mlds_output_init_args(ArgRvalsTypes, Context, 0, Base, Ptag,
        Opts, Indent + 1, !IO),
    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("}\n", !IO).

:- pred mlds_output_maybe_alloc_id(maybe(mlds_alloc_id)::in, io::di, io::uo)
    is det.

mlds_output_maybe_alloc_id(MaybeAllocId, !IO) :-
    (
        MaybeAllocId = yes(mlds_alloc_id(Num)),
        io.format("&MR_alloc_sites[%d]", [i(Num)], !IO)
    ;
        MaybeAllocId = no,
        io.write_string("NULL", !IO)
    ).

:- pred mlds_output_target_code_component(mlds_to_c_opts::in, prog_context::in,
    target_code_component::in, io::di, io::uo) is det.

mlds_output_target_code_component(Opts, Context, TargetCode, !IO) :-
    (
        TargetCode = user_target_code(CodeString, MaybeUserContext),
        (
            MaybeUserContext = yes(UserContext),
            c_output_context(Opts ^ m2co_line_numbers, UserContext, !IO)
        ;
            MaybeUserContext = no,
            c_output_context(Opts ^ m2co_line_numbers, Context, !IO)
        ),
        io.write_string(CodeString, !IO),
        io.write_string("\n", !IO),
        c_reset_context(Opts ^ m2co_line_numbers, !IO)
    ;
        TargetCode = raw_target_code(CodeString),
        io.write_string(CodeString, !IO)
    ;
        TargetCode = target_code_input(Rval),
        mlds_output_rval(Opts, Rval, !IO),
        io.write_string(" ", !IO)
    ;
        TargetCode = target_code_output(Lval),
        mlds_output_lval(Opts, Lval, !IO),
        io.write_string(" ", !IO)
    ;
        TargetCode = target_code_type(Type),
        mlds_output_type(Opts, Type, !IO),
        io.write_string(" ", !IO)
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
        mlds_output_fully_qualified_function_name(FuncName, !IO),
        io.write_string("\n", !IO)
    ;
        TargetCode = target_code_alloc_id(AllocId),
        mlds_output_maybe_alloc_id(yes(AllocId), !IO)
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
        ; Type = mlds_native_int_type
        ; Type = mlds_native_uint_type
        ; Type = mlds_native_float_type
        ; Type = mlds_native_char_type
        ; Type = mlds_foreign_type(_)
        ; Type = mlds_class_type(_)
        ; Type = mlds_array_type(_)
        ; Type = mlds_mostly_generic_array_type(_)
        ; Type = mlds_ptr_type(_)
        ; Type = mlds_func_type(_)
        ; Type = mlds_generic_type
        ; Type = mlds_generic_env_ptr_type
        ),
        NeedsForwardingPtrSpace = no
    ;
        Type = mercury_type(_, _, TypeCategory),
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

:- pred mlds_output_init_args(list(mlds_typed_rval)::in,
    prog_context::in, int::in, lval_or_string::in, ptag::in,
    mlds_to_c_opts::in, indent::in, io::di, io::uo) is det.

mlds_output_init_args([], _, _, _, _, _, _, !IO).
mlds_output_init_args([ArgRvalType | ArgRvalsTypes], Context,
        ArgNum, Base, Ptag, Opts, Indent, !IO) :-
    % The MR_hl_field() macro expects its argument to have type MR_Box,
    % so we need to box the arguments if they are not already boxed.
    % Hence the use of mlds_output_boxed_rval below.

    % XXX For --high-level-data, we ought to generate assignments to the fields
    % (or perhaps a call to a constructor function) rather than using the
    % MR_hl_field() macro.

    c_output_context(Opts ^ m2co_line_numbers, Context, !IO),
    output_n_indents(Indent, !IO),
    io.write_string("MR_hl_field(", !IO),
    mlds_output_ptag(Ptag, !IO),
    io.write_string(", ", !IO),
    write_lval_or_string(Opts, Base, !IO),
    io.write_string(", ", !IO),
    io.write_int(ArgNum, !IO),
    io.write_string(") = ", !IO),
    ArgRvalType = ml_typed_rval(ArgRval, ArgType),
    mlds_output_boxed_rval(Opts, ArgType, ArgRval, !IO),
    io.write_string(";\n", !IO),
    mlds_output_init_args(ArgRvalsTypes, Context,
        ArgNum + 1, Base, Ptag, Opts, Indent, !IO).

:- pred write_lval_or_string(mlds_to_c_opts::in, lval_or_string::in,
    io::di, io::uo) is det.

write_lval_or_string(Opts, Base, !IO) :-
    (
        Base = ls_lval(Target),
        mlds_output_lval(Opts, Target, !IO)
    ;
        Base = ls_string(BaseVarName),
        io.write_string(BaseVarName, !IO)
    ).

%---------------------------------------------------------------------------%

mlds_output_gc_statement(Opts, Indent, GCStmt, MaybeNewLine, !IO) :-
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
        io.write_string(MaybeNewLine, !IO),
        io.write_string(Label, !IO),
        % XXX This value for FuncInfo is bogus. However, this output is only
        % for debugging anyway, so it does not really matter.
        ModuleName = mercury_module_name_to_mlds(unqualified("")),
        FuncName = mlds_function_export("dummy"),
        QualFuncName = qual_function_name(ModuleName, FuncName),
        FuncInfo = func_info_c(QualFuncName, mlds_func_signature([], [])),
        mlds_output_statement(Opts, Indent, FuncInfo, Stmt, !IO),
        io.write_string("#endif\n", !IO)
    ).
%---------------------------------------------------------------------------%
:- end_module ml_backend.mlds_to_c_stmt.
%---------------------------------------------------------------------------%
