%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2007,2009,2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_util.m.
% Main author: fjh, trd.
%
% This module contains utility predicates for manipulating the MLDS.
%
%---------------------------------------------------------------------------%

:- module ml_backend.ml_util.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module libs.
:- import_module libs.globals.          % for foreign_language
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % Succeeds iff the definitions contain the entry point to
    % the main predicate.
    %
:- pred func_defns_contain_main(list(mlds_function_defn)::in) is semidet.

%---------------------------------------------------------------------------%

    % code_address_is_for_this_function(CodeAddr, ModuleName, FuncName):
    %
    % True if CodeAddr, if used as the callee of an ml_stmt_call statement,
    % would call FuncName in ModuleName.
    %
:- pred code_address_is_for_this_function(mlds_code_addr::in,
    mlds_module_name::in, mlds_function_name::in) is semidet.

%---------------------------------------------------------------------------%
%
% Routines that deal with statements.
%

    % Nondeterministically generates sub-statements from statements.
    %
:- pred statement_is_or_contains_statement(mlds_stmt::in, mlds_stmt::out)
    is multi.

:- pred stmt_contains_statement(mlds_stmt::in, mlds_stmt::out) is nondet.

    % Succeeds iff this statement contains a reference to the
    % specified variable.
    %
:- func statement_contains_var(mlds_stmt, mlds_local_var_name) = bool.

:- pred has_foreign_languages(mlds_stmt::in, list(foreign_language)::out)
    is det.

%---------------------------------------------------------------------------%

    % Says whether these definitions contains a reference to
    % the specified variable.
    %
:- func local_var_defns_contains_var(list(mlds_local_var_defn),
    mlds_local_var_name) = bool.
:- func function_defns_contains_var(list(mlds_function_defn),
    mlds_local_var_name) = bool.

    % Says whether this definition contains a reference to
    % the specified variable.
    %
:- func local_var_defn_contains_var(mlds_local_var_defn, mlds_local_var_name)
    = bool.
:- func function_defn_contains_var(mlds_function_defn, mlds_local_var_name)
    = bool.

%---------------------------------------------------------------------------%
%
% Routines that deal with lvals/rvals.
%

% initializer_contains_var:
% rvals_contains_var:
% maybe_rval_contains_var:
% rval_contains_var:
% lvals_contains_var:
% lval_contains_var:
%
% Succeed iff the specified construct contains a reference to
% the specified variable.

:- func initializer_contains_var(mlds_initializer, mlds_local_var_name) = bool.

:- func rvals_contains_var(list(mlds_rval), mlds_local_var_name) = bool.

:- func maybe_rval_contains_var(maybe(mlds_rval), mlds_local_var_name) = bool.

:- func rval_contains_var(mlds_rval, mlds_local_var_name) = bool.

:- func lvals_contains_var(list(mlds_lval), mlds_local_var_name) = bool.

:- func lval_contains_var(mlds_lval, mlds_local_var_name) = bool.

%---------------------------------------------------------------------------%
%
% Functions for generating initializers.
%
% This handles arrays, maybe, null pointers, strings, ints, and builtin enums.

:- func gen_init_bool(bool) = mlds_initializer.

:- func gen_init_int(int) = mlds_initializer.
:- func gen_init_uint(uint) = mlds_initializer.

:- func gen_init_int8(int8) = mlds_initializer.
:- func gen_init_uint8(uint8) = mlds_initializer.

:- func gen_init_int16(int16) = mlds_initializer.
:- func gen_init_uint16(uint16) = mlds_initializer.

:- func gen_init_int32(int32) = mlds_initializer.
:- func gen_init_uint32(uint32) = mlds_initializer.

:- func gen_init_boxed_int(int) = mlds_initializer.

:- func gen_init_string(string) = mlds_initializer.

:- func gen_init_builtin_const(target_prefixes, string) = mlds_initializer.

:- func gen_init_foreign(foreign_language, string) = mlds_initializer.

:- func gen_init_null_pointer(mlds_type) = mlds_initializer.

:- func gen_init_maybe(mlds_type, func(T) = mlds_initializer, maybe(T)) =
    mlds_initializer.

:- func gen_init_array(func(T) = mlds_initializer, list(T)) = mlds_initializer.

:- func wrap_init_obj(mlds_rval) = mlds_initializer.

%---------------------------------------------------------------------------%

:- func get_mlds_stmt_context(mlds_stmt) = prog_context.

%---------------------------------------------------------------------------%

:- func project_mlds_argument_name(mlds_argument) = mlds_local_var_name.

%---------------------------------------------------------------------------%

:- func mlds_maybe_aux_func_id_to_suffix(mlds_maybe_aux_func_id) = string.

%---------------------------------------------------------------------------%

:- func mlds_target_high_level_data(mlds_target_lang) = bool.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.
:- import_module hlds.hlds_data.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module int.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

func_defns_contain_main([FuncDefn | FuncDefns]) :-
    ( if
        FuncDefn = mlds_function_defn(FuncName, _, _, _, _, _, _, _),
        FuncName = mlds_function_name(PlainFuncName),
        PlainFuncName = mlds_plain_func_name(FuncLabel, _),
        FuncLabel = mlds_func_label(ProcLabel, _MaybeSeqNum),
        ProcLabel = mlds_proc_label(PredLabel, _ProcId),
        PredLabel = mlds_user_pred_label(pf_predicate, _, "main",
            pred_form_arity(2), _, _)
    then
        true
    else
        func_defns_contain_main(FuncDefns)
    ).

code_address_is_for_this_function(CodeAddr, ModuleName, FuncName) :-
    % Check if the callee address is the same as the caller.
    CodeAddr = mlds_code_addr(QualFuncLabel, _Signature),
    QualFuncLabel = qual_func_label(FuncModuleName, FuncLabel),

    % Check that the module name matches.
    FuncModuleName = ModuleName,

    % Check that the function name (PredLabel, ProcId, MaybeSeqNum) matches.
    FuncName = mlds_function_name(mlds_plain_func_name(FuncLabel, _PredId)).

%---------------------------------------------------------------------------%
%
% Routines that deal with statements.
%

% statement_contains_statement:
% statements_contains_statement:
% maybe_statement_contains_statement:
%
% Nondeterministically generate sub-statements from statements.

:- pred statements_contains_statement(list(mlds_stmt)::in,
    mlds_stmt::out) is nondet.

statements_contains_statement(Stmts, SubStmt) :-
    list.member(Stmt, Stmts),
    statement_is_or_contains_statement(Stmt, SubStmt).

:- pred maybe_statement_contains_statement(maybe(mlds_stmt)::in,
    mlds_stmt::out) is nondet.

maybe_statement_contains_statement(no, _Stmt) :- fail.
maybe_statement_contains_statement(yes(Stmt), SubStmt) :-
    statement_is_or_contains_statement(Stmt, SubStmt).

statement_is_or_contains_statement(Stmt, Stmt).
statement_is_or_contains_statement(Stmt, SubStmt) :-
    stmt_contains_statement(Stmt, SubStmt).

stmt_contains_statement(Stmt, SubStmt) :-
    require_complete_switch [Stmt]
    (
        Stmt = ml_stmt_block(_LocalVarDefns, _FuncDefns, Stmts, _Context),
        statements_contains_statement(Stmts, SubStmt)
    ;
        Stmt = ml_stmt_while(_Kind, _Rval, BodyStmt, _LoopLocalVars, _Context),
        statement_is_or_contains_statement(BodyStmt, SubStmt)
    ;
        Stmt = ml_stmt_if_then_else(_Cond, ThenStmt, MaybeElseStmt, _Context),
        ( statement_is_or_contains_statement(ThenStmt, SubStmt)
        ; maybe_statement_contains_statement(MaybeElseStmt, SubStmt)
        )
    ;
        Stmt = ml_stmt_switch(_Type, _Val, _Range, Cases, Default, _Context),
        ( cases_contains_statement(Cases, SubStmt)
        ; default_contains_statement(Default, SubStmt)
        )
    ;
        Stmt = ml_stmt_try_commit(_Ref, BodyStmt, HandlerStmt, _Context),
        ( statement_is_or_contains_statement(BodyStmt, SubStmt)
        ; statement_is_or_contains_statement(HandlerStmt, SubStmt)
        )
    ;
        ( Stmt = ml_stmt_label(_Label, _Context)
        ; Stmt = ml_stmt_goto(_, _Context)
        ; Stmt = ml_stmt_computed_goto(_Rval, _Labels, _Context)
        ; Stmt = ml_stmt_call(_Sig, _Func, _Args, _RetLvals, _TailCall,
            _Context)
        ; Stmt = ml_stmt_return(_Rvals, _Context)
        ; Stmt = ml_stmt_do_commit(_Ref, _Context)
        ; Stmt = ml_stmt_atomic(_AtomicStmt, _Context)
        ),
        fail
    ).

:- pred cases_contains_statement(list(mlds_switch_case)::in,
    mlds_stmt::out) is nondet.

cases_contains_statement(Cases, SubStmt) :-
    list.member(Case, Cases),
    Case = mlds_switch_case(_FirstCond, _LaterConds, Stmt),
    statement_is_or_contains_statement(Stmt, SubStmt).

:- pred default_contains_statement(mlds_switch_default::in,
    mlds_stmt::out) is nondet.

default_contains_statement(default_do_nothing, _) :- fail.
default_contains_statement(default_is_unreachable, _) :- fail.
default_contains_statement(default_case(Stmt), SubStmt) :-
    statement_is_or_contains_statement(Stmt, SubStmt).

%---------------------------------------------------------------------------%
%
% statements_contains_var:
% maybe_statement_contains_var:
% statement_contains_var:
% trail_op_contains_var:
% atomic_stmt_contains_var:
%
% Succeed iff the specified construct contains a reference to
% the specified variable.

:- func statements_contains_var(list(mlds_stmt), mlds_local_var_name) = bool.

statements_contains_var([], _DataName) = no.
statements_contains_var([Stmt | Stmts], DataName) = ContainsVar :-
    StmtContainsVar = statement_contains_var(Stmt, DataName),
    (
        StmtContainsVar = yes,
        ContainsVar = yes
    ;
        StmtContainsVar = no,
        ContainsVar = statements_contains_var(Stmts, DataName)
    ).

:- func maybe_statement_contains_var(maybe(mlds_stmt), mlds_local_var_name)
    = bool.

maybe_statement_contains_var(no, _) = no.
maybe_statement_contains_var(yes(Stmt), DataName) = ContainsVar :-
    ContainsVar = statement_contains_var(Stmt, DataName).

statement_contains_var(Stmt, SearchVarName) = ContainsVar :-
    (
        Stmt = ml_stmt_block(LocalVarDefns, FuncDefns, SubStmts, _Context),
        LocalVarDefnsContainVar =
            local_var_defns_contains_var(LocalVarDefns, SearchVarName),
        (
            LocalVarDefnsContainVar = yes,
            ContainsVar = yes
        ;
            LocalVarDefnsContainVar = no,
            FuncDefnsContainVar =
                function_defns_contains_var(FuncDefns, SearchVarName),
            (
                FuncDefnsContainVar = yes,
                ContainsVar = yes
            ;
                FuncDefnsContainVar = no,
                ContainsVar = statements_contains_var(SubStmts, SearchVarName)
            )
        )
    ;
        Stmt = ml_stmt_while(_Kind, Rval, BodyStmt, _LoopLocalVars, _Context),
        % _LoopLocalVars should contain a variable only if BodyStmt does too.
        RvalContainsVar = rval_contains_var(Rval, SearchVarName),
        (
            RvalContainsVar = yes,
            ContainsVar = yes
        ;
            RvalContainsVar = no,
            ContainsVar = statement_contains_var(BodyStmt, SearchVarName)
        )
    ;
        Stmt = ml_stmt_if_then_else(Cond, ThenStmt, MaybeElseStmt, _Context),
        CondContainsVar = rval_contains_var(Cond, SearchVarName),
        (
            CondContainsVar = yes,
            ContainsVar = yes
        ;
            CondContainsVar = no,
            ThenContainsVar = statement_contains_var(ThenStmt, SearchVarName),
            (
                ThenContainsVar = yes,
                ContainsVar = yes
            ;
                ThenContainsVar = no,
                ContainsVar =
                    maybe_statement_contains_var(MaybeElseStmt, SearchVarName)
            )
        )
    ;
        Stmt = ml_stmt_switch(_Type, Val, _Range, Cases, Default, _Context),
        ValContainsVar = rval_contains_var(Val, SearchVarName),
        (
            ValContainsVar = yes,
            ContainsVar = yes
        ;
            ValContainsVar = no,
            CasesContainsVar = cases_contains_var(Cases, SearchVarName),
            (
                CasesContainsVar = yes,
                ContainsVar = yes
            ;
                CasesContainsVar = no,
                ContainsVar = default_contains_var(Default, SearchVarName)
            )
        )
    ;
        ( Stmt = ml_stmt_label(_Label, _Context)
        ; Stmt = ml_stmt_goto(_, _Context)
        ),
        ContainsVar = no
    ;
        Stmt = ml_stmt_computed_goto(Rval, _Labels, _Context),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        Stmt = ml_stmt_call(_Sig, Func, Args, RetLvals, _TailCall, _Context),
        FuncContainsVar = rval_contains_var(Func, SearchVarName),
        (
            FuncContainsVar = yes,
            ContainsVar = yes
        ;
            FuncContainsVar = no,
            ArgsContainVar = rvals_contains_var(Args, SearchVarName),
            (
                ArgsContainVar = yes,
                ContainsVar = yes
            ;
                ArgsContainVar = no,
                ContainsVar = lvals_contains_var(RetLvals, SearchVarName)
            )
        )
    ;
        Stmt = ml_stmt_return(Rvals, _Context),
        ContainsVar = rvals_contains_var(Rvals, SearchVarName)
    ;
        Stmt = ml_stmt_do_commit(Ref, _Context),
        ContainsVar = rval_contains_var(Ref, SearchVarName)
    ;
        Stmt = ml_stmt_try_commit(Ref, BodyStmt, HandlerStmt, _Context),
        RefContainsVar = lval_contains_var(Ref, SearchVarName),
        (
            RefContainsVar = yes,
            ContainsVar = yes
        ;
            RefContainsVar = no,
            StmtContainsVar = statement_contains_var(BodyStmt, SearchVarName),
            (
                StmtContainsVar = yes,
                ContainsVar = yes
            ;
                StmtContainsVar = no,
                ContainsVar =
                    statement_contains_var(HandlerStmt, SearchVarName)
            )
        )
    ;
        Stmt = ml_stmt_atomic(AtomicStmt, _Context),
        ContainsVar = atomic_stmt_contains_var(AtomicStmt, SearchVarName)
    ).

:- func cases_contains_var(list(mlds_switch_case), mlds_local_var_name) = bool.

cases_contains_var([], _SearchVarName) = no.
cases_contains_var([Case | Cases], SearchVarName) = ContainsVar :-
    Case = mlds_switch_case(_FirstCond, _LaterConds, Stmt),
    StmtContainsVar = statement_contains_var(Stmt, SearchVarName),
    (
        StmtContainsVar = yes,
        ContainsVar = yes
    ;
        StmtContainsVar = no,
        ContainsVar = cases_contains_var(Cases, SearchVarName)
    ).

:- func default_contains_var(mlds_switch_default, mlds_local_var_name) = bool.

default_contains_var(Default, SearchVarName) = ContainsVar :-
    (
        ( Default = default_do_nothing
        ; Default = default_is_unreachable
        ),
        ContainsVar = no
    ;
        Default = default_case(Stmt),
        ContainsVar = statement_contains_var(Stmt, SearchVarName)
    ).

:- func atomic_stmt_contains_var(mlds_atomic_statement, mlds_local_var_name)
    = bool.

atomic_stmt_contains_var(AtomicStmt, SearchVarName) = ContainsVar :-
    (
        AtomicStmt = comment(_),
        ContainsVar = no
    ;
        ( AtomicStmt = assign(Lval, Rval)
        ; AtomicStmt = assign_if_in_heap(Lval, Rval)
        ),
        LvalContainsVar = lval_contains_var(Lval, SearchVarName),
        (
            LvalContainsVar = yes,
            ContainsVar = yes
        ;
            LvalContainsVar = no,
            ContainsVar = rval_contains_var(Rval, SearchVarName)
        )
    ;
        AtomicStmt = delete_object(Rval),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        AtomicStmt = new_object(Target, _MaybeTag, _ExplicitSecTag, _Type,
            _MaybeSize, _MaybeCtorName, ArgRvalsTypes, _MayUseAtomic,
            _AllocId),
        TargetContainsVar = lval_contains_var(Target, SearchVarName),
        (
            TargetContainsVar = yes,
            ContainsVar = yes
        ;
            TargetContainsVar = no,
            ContainsVar = typed_rvals_contains_var(ArgRvalsTypes,
                SearchVarName)
        )
    ;
        AtomicStmt = gc_check,
        ContainsVar = no
    ;
        AtomicStmt = mark_hp(Lval),
        ContainsVar = lval_contains_var(Lval, SearchVarName)
    ;
        AtomicStmt = restore_hp(Rval),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        AtomicStmt = trail_op(TrailOp),
        ContainsVar = trail_op_contains_var(TrailOp, SearchVarName)
    ;
        AtomicStmt = inline_target_code(_Lang, Components),
        ContainsVar =
            target_code_components_contains_var(Components, SearchVarName)
    ;
        AtomicStmt = outline_foreign_proc(_Lang, OutlineArgs, ReturnLvals,
            _Code),
        OutlineArgsContainVar =
            outline_args_contains_var(OutlineArgs, SearchVarName),
        (
            OutlineArgsContainVar = yes,
            ContainsVar = yes
        ;
            OutlineArgsContainVar = no,
            ContainsVar = lvals_contains_var(ReturnLvals, SearchVarName)
        )
    ).

:- func trail_op_contains_var(trail_op, mlds_local_var_name) = bool.

trail_op_contains_var(TrailOp, SearchVarName) = ContainsVar :-
    (
        TrailOp = store_ticket(Lval),
        ContainsVar = lval_contains_var(Lval, SearchVarName)
    ;
        TrailOp = reset_ticket(Rval, _Reason),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        ( TrailOp = discard_ticket
        ; TrailOp = prune_ticket
        ),
        ContainsVar = no
    ;
        TrailOp = mark_ticket_stack(Lval),
        ContainsVar = lval_contains_var(Lval, SearchVarName)
    ;
        TrailOp = prune_tickets_to(Rval),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ).

:- func target_code_components_contains_var(list(target_code_component),
    mlds_local_var_name) = bool.

target_code_components_contains_var([], _SearchVarName) = no.
target_code_components_contains_var([TargetCode | TargetCodes], SearchVarName)
        = ContainsVar :-
    TargetCodeContainsVar =
        target_code_component_contains_var(TargetCode, SearchVarName),
    (
        TargetCodeContainsVar = yes,
        ContainsVar = yes
    ;
        TargetCodeContainsVar = no,
        ContainsVar =
            target_code_components_contains_var(TargetCodes, SearchVarName)
    ).

:- func target_code_component_contains_var(target_code_component,
    mlds_local_var_name) = bool.

target_code_component_contains_var(TargetCode, SearchVarName) = ContainsVar :-
    (
        ( TargetCode = user_target_code(_, _)
        ; TargetCode = raw_target_code(_)
        ; TargetCode = target_code_type(_)
        ; TargetCode = target_code_alloc_id(_)
        ; TargetCode = target_code_function_name(_)
        ),
        ContainsVar = no
    ;
        TargetCode = target_code_input(Rval),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        TargetCode = target_code_output(Lval),
        ContainsVar = lval_contains_var(Lval, SearchVarName)
    ).

:- func outline_args_contains_var(list(outline_arg), mlds_local_var_name)
    = bool.

outline_args_contains_var([], _SearchVarName) = no.
outline_args_contains_var([OutlineArg | OutlineArgs], SearchVarName) =
        ContainsVar :-
    OutlineArgContainsVar =
        outline_arg_contains_var(OutlineArg, SearchVarName),
    (
        OutlineArgContainsVar = yes,
        ContainsVar = yes
    ;
        OutlineArgContainsVar = no,
        ContainsVar = outline_args_contains_var(OutlineArgs, SearchVarName)
    ).

:- func outline_arg_contains_var(outline_arg, mlds_local_var_name) = bool.

outline_arg_contains_var(OutlineArg, SearchVarName) = ContainsVar :-
    (
        OutlineArg = ola_in(_Type, _Str, Rval),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        OutlineArg = ola_out(_Type, _Str, Lval),
        ContainsVar = lval_contains_var(Lval, SearchVarName)
    ;
        OutlineArg = ola_unused,
        ContainsVar = no
    ).

%---------------------------------------------------------------------------%

has_foreign_languages(Stmt, Langs) :-
    GetTargetCode =
        ( pred(Lang::out) is nondet :-
            statement_is_or_contains_statement(Stmt, SubStmt),
            SubStmt = ml_stmt_atomic(outline_foreign_proc(Lang, _, _, _), _)
        ),
    solutions.solutions(GetTargetCode, Langs).

%---------------------------------------------------------------------------%
%
% defns_contains_var:
% defn_contains_var:
% defn_body_contains_var:
% function_defns_contains_var:
% function_defn_contains_var:
%
% Succeed iff the specified construct contains a reference to
% the specified variable.

local_var_defns_contains_var([], _SearchVarName) = no.
local_var_defns_contains_var([LocalVarDefn | LocalVarDefns], SearchVarName)
        = ContainsVar :-
    LocalVarDefnContainsVar =
        local_var_defn_contains_var(LocalVarDefn, SearchVarName),
    (
        LocalVarDefnContainsVar = yes,
        ContainsVar = yes
    ;
        LocalVarDefnContainsVar = no,
        ContainsVar =
            local_var_defns_contains_var(LocalVarDefns, SearchVarName)
    ).

function_defns_contains_var([], _SearchVarName) = no.
function_defns_contains_var([FuncDefn | FuncDefns], SearchVarName)
        = ContainsVar :-
    FuncDefnContainsVar = function_defn_contains_var(FuncDefn, SearchVarName),
    (
        FuncDefnContainsVar = yes,
        ContainsVar = yes
    ;
        FuncDefnContainsVar = no,
        ContainsVar = function_defns_contains_var(FuncDefns, SearchVarName)
    ).

local_var_defn_contains_var(LocalVarDefn, SearchVarName) = ContainsVar :-
    LocalVarDefn = mlds_local_var_defn(_Name, _Ctxt,
        _Type, Initializer, _GCStmt),
    % XXX Should we include variables in the GCStmt field here?
    ContainsVar = initializer_contains_var(Initializer, SearchVarName).

function_defn_contains_var(FuncDefn, SearchVarName) = ContainsVar :-
    FuncDefn = mlds_function_defn(_Name, _Ctxt, _Flags, _PredProcId, _Params,
        Body, _EnvVarNames, _MaybeRequireTailrecInfo),
    (
        Body = body_external,
        ContainsVar = no
    ;
        Body = body_defined_here(Stmt),
        ContainsVar = statement_contains_var(Stmt, SearchVarName)
    ).

%---------------------------------------------------------------------------%
%
% Routines that deal with lvals/rvals.
%

% initializer_contains_var:
% initializers_contains_var:
% typed_rvals_contains_var:
% rvals_contains_var:
% maybe_rval_contains_var:
% rval_contains_var:
% lvals_contains_var:
% lval_contains_var:
%
% Say whether the specified construct contains a reference to
% the specified variable.

initializer_contains_var(Initializer, SearchVarName) = ContainsVar :-
    (
        Initializer = no_initializer,
        ContainsVar = no
    ;
        Initializer = init_obj(Rval),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        Initializer = init_struct(_Type, FieldInitializers),
        ContainsVar =
            initializers_contains_var(FieldInitializers, SearchVarName)
    ;
        Initializer = init_array(ElementInitializers),
        ContainsVar =
            initializers_contains_var(ElementInitializers, SearchVarName)
    ).

:- func initializers_contains_var(list(mlds_initializer), mlds_local_var_name)
    = bool.

initializers_contains_var([], _SearchVarName) = no.
initializers_contains_var([Initializer | Initializers], SearchVarName) =
        ContainsVar :-
    InitializerContainsVar =
        initializer_contains_var(Initializer, SearchVarName),
    (
        InitializerContainsVar = yes,
        ContainsVar = yes
    ;
        InitializerContainsVar = no,
        ContainsVar = initializers_contains_var(Initializers, SearchVarName)
    ).

:- func typed_rvals_contains_var(list(mlds_typed_rval), mlds_local_var_name)
    = bool.

typed_rvals_contains_var([], _SearchVarName) = no.
typed_rvals_contains_var([TypedRval | TypedRvals], SearchVarName)
        = ContainsVar :-
    TypedRval = ml_typed_rval(Rval, _Type),
    RvalContainsVar = rval_contains_var(Rval, SearchVarName),
    (
        RvalContainsVar = yes,
        ContainsVar = yes
    ;
        RvalContainsVar = no,
        ContainsVar = typed_rvals_contains_var(TypedRvals, SearchVarName)
    ).

rvals_contains_var([], _SearchVarName) = no.
rvals_contains_var([Rval | Rvals], SearchVarName) = ContainsVar :-
    RvalContainsVar = rval_contains_var(Rval, SearchVarName),
    (
        RvalContainsVar = yes,
        ContainsVar = yes
    ;
        RvalContainsVar = no,
        ContainsVar = rvals_contains_var(Rvals, SearchVarName)
    ).

maybe_rval_contains_var(no, _SearchVarName) = no.
maybe_rval_contains_var(yes(Rval), SearchVarName) =
    rval_contains_var(Rval, SearchVarName).

rval_contains_var(Rval, SearchVarName) = ContainsVar :-
    (
        Rval = ml_lval(Lval),
        ContainsVar = lval_contains_var(Lval, SearchVarName)
    ;
        Rval = ml_mkword(_Tag, SubRval),
        ContainsVar = rval_contains_var(SubRval, SearchVarName)
    ;
        Rval = ml_const(Const),
        (
            Const = mlconst_data_addr_local_var(RawVarName),
            ( if SearchVarName = RawVarName then
                % This is a place where we can succeed.
                ContainsVar = yes
            else
                ContainsVar = no
            )
        ;
            ( Const = mlconst_true
            ; Const = mlconst_false
            ; Const = mlconst_int(_)
            ; Const = mlconst_uint(_)
            ; Const = mlconst_int8(_)
            ; Const = mlconst_uint8(_)
            ; Const = mlconst_int16(_)
            ; Const = mlconst_uint16(_)
            ; Const = mlconst_int32(_)
            ; Const = mlconst_uint32(_)
            ; Const = mlconst_int64(_)
            ; Const = mlconst_uint64(_)
            ; Const = mlconst_enum(_, _)
            ; Const = mlconst_char(_)
            ; Const = mlconst_float(_)
            ; Const = mlconst_string(_)
            ; Const = mlconst_multi_string(_)
            ; Const = mlconst_foreign(_, _, _)
            ; Const = mlconst_named_const(_, _)
            ; Const = mlconst_code_addr(_)
            ; Const = mlconst_data_addr_rtti(_, _)
            ; Const = mlconst_data_addr_tabling(_, _)
            ; Const = mlconst_data_addr_global_var(_, _)
            ; Const = mlconst_null(_)
            ),
            ContainsVar = no
        )
    ;
        ( Rval = ml_box(_Type, RvalA)
        ; Rval = ml_unbox(_Type, RvalA)
        ; Rval = ml_cast(_Type, RvalA)
        ; Rval = ml_unop(_Op, RvalA)
        ),
        ContainsVar = rval_contains_var(RvalA, SearchVarName)
    ;
        Rval = ml_binop(_Op, RvalA, RvalB),
        RvalAContainsVar = rval_contains_var(RvalA, SearchVarName),
        (
            RvalAContainsVar = yes,
            ContainsVar = yes
        ;
            RvalAContainsVar = no,
            ContainsVar = rval_contains_var(RvalB, SearchVarName)
        )
    ;
        Rval = ml_mem_addr(Lval),
        ContainsVar = lval_contains_var(Lval, SearchVarName)
    ;
        Rval = ml_vector_common_row_addr(_VectorCommon, IndexRval),
        ContainsVar = rval_contains_var(IndexRval, SearchVarName)
    ;
        ( Rval = ml_scalar_common(_ScalarCommon)
        ; Rval = ml_scalar_common_addr(_ScalarCommon)
        ; Rval = ml_self(_)
        ),
        ContainsVar = no
    ).

lvals_contains_var([], _SearchVarName) = no.
lvals_contains_var([Lval | Lvals], SearchVarName) = ContainsVar :-
    LvalContainsVar = lval_contains_var(Lval, SearchVarName),
    (
        LvalContainsVar = yes,
        ContainsVar = yes
    ;
        LvalContainsVar = no,
        ContainsVar = lvals_contains_var(Lvals, SearchVarName)
    ).

lval_contains_var(Lval, SearchVarName) = ContainsVar :-
    (
        ( Lval = ml_field(_MaybeTag, Rval, _, _, _)
        ; Lval = ml_mem_ref(Rval, _Type)
        ),
        ContainsVar = rval_contains_var(Rval, SearchVarName)
    ;
        ( Lval = ml_global_var(_, _)
        ; Lval = ml_target_global_var_ref(_)
        ),
        ContainsVar = no
    ;
        Lval = ml_local_var(VarName, _Type),
        % This is another place where we can succeed.
        ( if VarName = SearchVarName then
            ContainsVar = yes
        else
            ContainsVar =no
        )
    ).

%---------------------------------------------------------------------------%

gen_init_bool(no) = init_obj(ml_const(mlconst_false)).
gen_init_bool(yes) = init_obj(ml_const(mlconst_true)).

gen_init_int(Int) = init_obj(ml_const(mlconst_int(Int))).
gen_init_uint(Int) = init_obj(ml_const(mlconst_uint(Int))).

gen_init_int8(Int) = init_obj(ml_const(mlconst_int8(Int))).
gen_init_uint8(Int) = init_obj(ml_const(mlconst_uint8(Int))).

gen_init_int16(Int) = init_obj(ml_const(mlconst_int16(Int))).
gen_init_uint16(Int) = init_obj(ml_const(mlconst_uint16(Int))).

gen_init_int32(Int) = init_obj(ml_const(mlconst_int32(Int))).
gen_init_uint32(Int) = init_obj(ml_const(mlconst_uint32(Int))).

gen_init_boxed_int(Int) =
    init_obj(ml_box(mlds_builtin_type_int(int_type_int),
        ml_const(mlconst_int(Int)))).

gen_init_string(String) = init_obj(ml_const(mlconst_string(String))).

gen_init_builtin_const(TargetPrefixes, Name) = init_obj(Rval) :-
    Rval = ml_const(mlconst_named_const(TargetPrefixes, Name)).

gen_init_foreign(Lang, String) =
    init_obj(ml_const(mlconst_foreign(Lang, String,
        mlds_builtin_type_int(int_type_int)))).

gen_init_null_pointer(Type) = init_obj(ml_const(mlconst_null(Type))).

gen_init_maybe(_Type, Conv, yes(X)) = Conv(X).
gen_init_maybe(Type, _Conv, no) = gen_init_null_pointer(Type).

gen_init_array(Conv, List) = init_array(list.map(Conv, List)).

wrap_init_obj(Rval) = init_obj(Rval).

%---------------------------------------------------------------------------%

get_mlds_stmt_context(Stmt) = Context :-
    ( Stmt = ml_stmt_block(_, _, _, Context)
    ; Stmt = ml_stmt_while(_, _, _, _, Context)
    ; Stmt = ml_stmt_if_then_else(_, _, _, Context)
    ; Stmt = ml_stmt_switch(_, _, _, _, _, Context)
    ; Stmt = ml_stmt_label(_, Context)
    ; Stmt = ml_stmt_goto(_, Context)
    ; Stmt = ml_stmt_computed_goto(_, _, Context)
    ; Stmt = ml_stmt_try_commit(_, _, _, Context)
    ; Stmt = ml_stmt_do_commit(_, Context)
    ; Stmt = ml_stmt_return(_, Context)
    ; Stmt = ml_stmt_call(_, _, _, _, _, Context)
    ; Stmt = ml_stmt_atomic(_, Context)
    ).

%---------------------------------------------------------------------------%

project_mlds_argument_name(mlds_argument(LocalVarName, _, _)) = LocalVarName.

%---------------------------------------------------------------------------%

mlds_maybe_aux_func_id_to_suffix(MaybeAux) = Suffix :-
    % This function preserves Fergus's original scheme of computing
    % a sequence number for gc trace functions by adding 10000 to the
    % sequence number (if any) of the MLDS function they were for.
    %
    % A cleaner, more readable scheme would be nice, but the question
    % will become even semi-interesting only if the accurate collector
    % is ever used in practice.
    (
        MaybeAux = proc_func,
        Suffix = ""
    ;
        MaybeAux = proc_aux_func(SeqNum),
        Suffix = string.format("_%d", [i(SeqNum)])
    ;
        MaybeAux = gc_trace_for_proc_func,
        Suffix = string.format("_%d", [i(10000)])
    ;
        MaybeAux = gc_trace_for_proc_aux_func(SeqNum),
        Suffix = string.format("_%d", [i(10001 + SeqNum)])
    ).

%---------------------------------------------------------------------------%

mlds_target_high_level_data(Target) = HighLevelData :-
    (
        Target = ml_target_c,
        HighLevelData = no
    ;
        ( Target = ml_target_java
        ; Target = ml_target_csharp
        ),
        HighLevelData = yes
    ).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_util.
%---------------------------------------------------------------------------%
