%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: ml_util.m.
% Main author: fjh, trd.
% 
% This module contains utility predicates for manipulating the MLDS.
% 
%-----------------------------------------------------------------------------%

:- module ml_backend.ml_util.
:- interface.

:- import_module libs.globals.  % for foreign_language
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_data.
:- import_module ml_backend.mlds.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

    % Succeeds iff the definitions contain the entry point to
    % the a main predicate.
    %
:- pred defns_contain_main(mlds_defns::in) is semidet.

%-----------------------------------------------------------------------------%

    % Return `true' if the statement is a tail call which can be optimized
    % into a jump back to the start of the function.
    %
:- pred can_optimize_tailcall(mlds_qualified_entity_name::in, mlds_stmt::in)
    is semidet.

%-----------------------------------------------------------------------------%
%
% Routines that deal with statements.
%

    % Nondeterministically generates sub-statements from statements.
    %
:- pred statements_contains_statement(statements::in,
    statement::out) is nondet.

:- pred statement_contains_statement(statement::in, statement::out)
    is multi.

:- pred stmt_contains_statement(mlds_stmt::in, statement::out)
    is nondet.

    % Succeeds iff this statement contains a reference to the
    % specified variable.
    %
:- pred statement_contains_var(statement::in, mlds_data::in) is semidet.

:- pred has_foreign_languages(statement::in, list(foreign_language)::out)
    is det.

%-----------------------------------------------------------------------------%
%
% Routines that deal with definitions.
%

    % defn_contains_foreign_code(NativeTargetLang, Defn):
    %
    % Succeeds iff this definition contains outline_foreign_proc statements,
    % or inline_target_code statements in a target language other than the
    % specified native target language.
    %
    % XXX perhaps we should eliminate the need to check for inline_target_code,
    % because it shouldn't be generated with target language different to the
    % native target language in the long run.
    %
:- pred defn_contains_foreign_code(target_lang::in, mlds_defn::in) is semidet.

    % defn_contains_foreign_code(ForeignLang, Defn):
    %
    % Succeeds iff this definition contains outline_foreign_proc statements
    % for the given foreign language.
    %
:- pred defn_contains_outline_foreign_proc(foreign_language::in,
    mlds_defn::in) is semidet.

    % Succeeds iff this definition is a type definition.
    %
:- pred defn_is_type(mlds_defn::in) is semidet.

    % Succeeds iff this definition is a function definition.
    %
:- pred defn_is_function(mlds_defn::in) is semidet.

    % Succeeds iff this definition is a data definition which
    % defines a type_ctor_info constant.
    %
:- pred defn_is_type_ctor_info(mlds_defn::in) is semidet.

    % Succeeds iff this definition is a data definition which
    % defines a variable whose type is mlds_commit_type.
    %
:- pred defn_is_commit_type_var(mlds_defn::in) is semidet.

    % Succeeds iff this definition has `public' in the access
    % field in its decl_flags.
    %
:- pred defn_is_public(mlds_defn::in) is semidet.

    % Succeeds iff these definitions contains a reference to
    % the specified variable.
    %
:- pred defns_contains_var(mlds_defns::in, mlds_data::in) is semidet.

    % Succeeds iff this definition contains a reference to
    % the specified variable.
    %
:- pred defn_contains_var(mlds_defn::in, mlds_data::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Routines that deal with lvals/rvals.
%

% initializer_contains_var:
% rvals_contains_var:
% maybe_rval_contains_var:
% rval_contains_var:
% lvals_contains_var:
% lval_contains_var:
%   Succeeds iff the specified construct contains a reference to
%   the specified variable.

:- pred initializer_contains_var(mlds_initializer::in, mlds_data::in)
    is semidet.

:- pred rvals_contains_var(list(mlds_rval)::in, mlds_data::in) is semidet.

:- pred maybe_rval_contains_var(maybe(mlds_rval)::in, mlds_data::in)
    is semidet.

:- pred rval_contains_var(mlds_rval::in, mlds_data::in) is semidet.

:- pred lvals_contains_var(list(mlds_lval)::in, mlds_data::in) is semidet.

:- pred lval_contains_var(mlds_lval::in, mlds_data::in) is semidet.

%-----------------------------------------------------------------------------%

    % Does the type require the lowlevel representation on the indicated
    % backend?
    %
:- pred type_needs_lowlevel_rep(compilation_target::in, mer_type::in)
    is semidet.

:- pred type_ctor_needs_lowlevel_rep(compilation_target::in,
    type_ctor::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Functions for generating initializers.
%
% This handles arrays, maybe, null pointers, strings, ints, and builtin enums.

:- func gen_init_builtin_const(string) = mlds_initializer.

:- func gen_init_array(func(T) = mlds_initializer, list(T)) = mlds_initializer.

:- func gen_init_maybe(mlds_type, func(T) = mlds_initializer, maybe(T)) =
    mlds_initializer.

:- func gen_init_null_pointer(mlds_type) = mlds_initializer.

:- func gen_init_string(string) = mlds_initializer.

:- func gen_init_int(int) = mlds_initializer.

:- func gen_init_bool(bool) = mlds_initializer.

:- func gen_init_boxed_int(int) = mlds_initializer.

:- func gen_init_reserved_address(module_info, reserved_address) =
    mlds_initializer.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.rtti.
:- import_module check_hlds.type_util.
:- import_module mdbcomp.prim_data.
:- import_module ml_backend.ml_unify_gen.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module list.
:- import_module pair.
:- import_module solutions.

%-----------------------------------------------------------------------------%

defns_contain_main(Defns) :-
    list.member(Defn, Defns),
    Defn = mlds_defn(Name, _, _, _),
    Name = entity_function(FuncName, _, _, _),
    FuncName = mlds_user_pred_label(predicate, _, "main", 2, _, _).

can_optimize_tailcall(Name, Call) :-
    Call = mlcall(_Signature, FuncRval, MaybeObject, _CallArgs,
        _Results, CallKind),
    % Check if this call can be optimized as a tail call.
    ( CallKind = tail_call ; CallKind = no_return_call ),

    % Check if the callee address is the same as the caller.
    FuncRval = const(code_addr_const(CodeAddr)),
    (
        CodeAddr = proc(QualifiedProcLabel, _Sig),
        MaybeSeqNum = no
    ;
        CodeAddr = internal(QualifiedProcLabel, SeqNum, _Sig),
        MaybeSeqNum = yes(SeqNum)
    ),
    ProcLabel = mlds_proc_label(PredLabel, ProcId),
    QualifiedProcLabel = qual(ModuleName, module_qual, ProcLabel),

    % Check that the module name matches.
    Name = qual(ModuleName, module_qual, FuncName),

    % Check that the PredLabel, ProcId, and MaybeSeqNum match.
    FuncName = entity_function(PredLabel, ProcId, MaybeSeqNum, _),

    % In C++, `this' is a constant, so our usual technique of assigning
    % the arguments won't work if it is a member function. Thus we don't do
    % this optimization if we're optimizing a member function call.
    MaybeObject = no.

%-----------------------------------------------------------------------------%
%
% Routines that deal with statements.
%

% statement_contains_statement:
% statements_contains_statement:
% maybe_statement_contains_statement:
%   nondeterministically generates sub-statements from statements.

statements_contains_statement(Statements, SubStatement) :-
    list.member(Statement, Statements),
    statement_contains_statement(Statement, SubStatement).

:- pred maybe_statement_contains_statement(maybe(statement)::in,
    statement::out) is nondet.

maybe_statement_contains_statement(no, _Statement) :- fail.
maybe_statement_contains_statement(yes(Statement), SubStatement) :-
    statement_contains_statement(Statement, SubStatement).

statement_contains_statement(Statement, Statement).
statement_contains_statement(Statement, SubStatement) :-
    Statement = statement(Stmt, _Context),
    stmt_contains_statement(Stmt, SubStatement).

stmt_contains_statement(Stmt, SubStatement) :-
    (
        Stmt = block(_Defns, Statements),
        statements_contains_statement(Statements, SubStatement)
    ;
        Stmt = while(_Rval, Statement, _Once),
        statement_contains_statement(Statement, SubStatement)
    ;
        Stmt = if_then_else(_Cond, Then, MaybeElse),
        ( statement_contains_statement(Then, SubStatement)
        ; maybe_statement_contains_statement(MaybeElse, SubStatement)
        )
    ;
        Stmt = switch(_Type, _Val, _Range, Cases, Default),
        ( cases_contains_statement(Cases, SubStatement)
        ; default_contains_statement(Default, SubStatement)
        )
    ;
        Stmt = label(_Label),
        fail
    ;
        Stmt = goto(_),
        fail
    ;
        Stmt = computed_goto(_Rval, _Labels),
        fail
    ;
        Stmt = mlcall(_Sig, _Func, _Obj, _Args, _RetLvals, _TailCall),
        fail
    ;
        Stmt = return(_Rvals),
        fail
    ;
        Stmt = do_commit(_Ref),
        fail
    ;
        Stmt = try_commit(_Ref, Statement, Handler),
        ( statement_contains_statement(Statement, SubStatement)
        ; statement_contains_statement(Handler, SubStatement)
        )
    ;
        Stmt = atomic(_AtomicStmt),
        fail
    ).

:- pred cases_contains_statement(list(mlds_switch_case)::in,
    statement::out) is nondet.

cases_contains_statement(Cases, SubStatement) :-
    list.member(Case, Cases),
    Case = _MatchCond - Statement,
    statement_contains_statement(Statement, SubStatement).

:- pred default_contains_statement(mlds_switch_default::in,
    statement::out) is nondet.

default_contains_statement(default_do_nothing, _) :- fail.
default_contains_statement(default_is_unreachable, _) :- fail.
default_contains_statement(default_case(Statement), SubStatement) :-
    statement_contains_statement(Statement, SubStatement).

% statements_contains_var:
% maybe_statement_contains_var:
% statement_contains_var:
% trail_op_contains_var:
% atomic_stmt_contains_var:
%   Succeeds iff the specified construct contains a reference to
%   the specified variable.

:- pred statements_contains_var(statements::in, mlds_data::in)
    is semidet.

statements_contains_var(Statements, Name) :-
    list.member(Statement, Statements),
    statement_contains_var(Statement, Name).

:- pred maybe_statement_contains_var(maybe(statement)::in,
    mlds_data::in) is semidet.

% maybe_statement_contains_var(no, _) :- fail.
maybe_statement_contains_var(yes(Statement), Name) :-
    statement_contains_var(Statement, Name).

statement_contains_var(Statement, Name) :-
    Statement = statement(Stmt, _Context),
    stmt_contains_var(Stmt, Name).

:- pred stmt_contains_var(mlds_stmt::in, mlds_data::in) is semidet.

stmt_contains_var(Stmt, Name) :-
    (
        Stmt = block(Defns, Statements),
        ( defns_contains_var(Defns, Name)
        ; statements_contains_var(Statements, Name)
        )
    ;
        Stmt = while(Rval, Statement, _Once),
        ( rval_contains_var(Rval, Name)
        ; statement_contains_var(Statement, Name)
        )
    ;
        Stmt = if_then_else(Cond, Then, MaybeElse),
        ( rval_contains_var(Cond, Name)
        ; statement_contains_var(Then, Name)
        ; maybe_statement_contains_var(MaybeElse, Name)
        )
    ;
        Stmt = switch(_Type, Val, _Range, Cases, Default),
        ( rval_contains_var(Val, Name)
        ; cases_contains_var(Cases, Name)
        ; default_contains_var(Default, Name)
        )
    ;
        Stmt = label(_Label),
        fail
    ;
        Stmt = goto(_),
        fail
    ;
        Stmt = computed_goto(Rval, _Labels),
        rval_contains_var(Rval, Name)
    ;
        Stmt = mlcall(_Sig, Func, Obj, Args, RetLvals, _TailCall),
        ( rval_contains_var(Func, Name)
        ; maybe_rval_contains_var(Obj, Name)
        ; rvals_contains_var(Args, Name)
        ; lvals_contains_var(RetLvals, Name)
        )
    ;
        Stmt = return(Rvals),
        rvals_contains_var(Rvals, Name)
    ;
        Stmt = do_commit(Ref),
        rval_contains_var(Ref, Name)
    ;
        Stmt = try_commit(Ref, Statement, Handler),
        ( lval_contains_var(Ref, Name)
        ; statement_contains_var(Statement, Name)
        ; statement_contains_var(Handler, Name)
        )
    ;
        Stmt = atomic(AtomicStmt),
        atomic_stmt_contains_var(AtomicStmt, Name)
    ).

:- pred cases_contains_var(list(mlds_switch_case)::in, mlds_data::in)
    is semidet.

cases_contains_var(Cases, Name) :-
    list.member(Case, Cases),
    Case = _MatchConds - Statement,
    statement_contains_var(Statement, Name).

:- pred default_contains_var(mlds_switch_default::in, mlds_data::in)
    is semidet.

% default_contains_var(default_do_nothing, _) :- fail.
% default_contains_var(default_is_unreachable, _) :- fail.
default_contains_var(default_case(Statement), Name) :-
    statement_contains_var(Statement, Name).

:- pred atomic_stmt_contains_var(mlds_atomic_statement::in, mlds_data::in)
    is semidet.

% atomic_stmt_contains_var(comment(_), _Name) :- fail.
atomic_stmt_contains_var(assign(Lval, Rval), Name) :-
    ( lval_contains_var(Lval, Name)
    ; rval_contains_var(Rval, Name)
    ).
atomic_stmt_contains_var(new_object(Target, _MaybeTag, _HasSecTag, _Type,
        _MaybeSize, _MaybeCtorName, Args, _ArgTypes, _MayUseAtomic), Name) :-
    ( lval_contains_var(Target, Name)
    ; rvals_contains_var(Args, Name)
    ).
% atomic_stmt_contains_var(gc_check, _) :- fail.
atomic_stmt_contains_var(mark_hp(Lval), Name) :-
    lval_contains_var(Lval, Name).
atomic_stmt_contains_var(restore_hp(Rval), Name) :-
    rval_contains_var(Rval, Name).
atomic_stmt_contains_var(trail_op(TrailOp), Name) :-
    trail_op_contains_var(TrailOp, Name).
atomic_stmt_contains_var(inline_target_code(_Lang, Components), Name) :-
    list.member(Component, Components),
    target_code_component_contains_var(Component, Name).

:- pred trail_op_contains_var(trail_op::in, mlds_data::in) is semidet.

trail_op_contains_var(store_ticket(Lval), Name) :-
    lval_contains_var(Lval, Name).
trail_op_contains_var(reset_ticket(Rval, _Reason), Name) :-
    rval_contains_var(Rval, Name).
% trail_op_contains_var(discard_ticket, _Name) :- fail.
% trail_op_contains_var(prune_ticket, _Name) :- fail.
trail_op_contains_var(mark_ticket_stack(Lval), Name) :-
    lval_contains_var(Lval, Name).
trail_op_contains_var(prune_tickets_to(Rval), Name) :-
    rval_contains_var(Rval, Name).

:- pred target_code_component_contains_var(target_code_component::in,
    mlds_data::in) is semidet.

%target_code_component_contains_var(raw_target_code(_Code), _Name) :-
%   fail.
%target_code_component_contains_var(user_target_code(_Code, _Ctxt), _Name) :-
%   fail.
target_code_component_contains_var(target_code_input(Rval), Name) :-
    rval_contains_var(Rval, Name).
target_code_component_contains_var(target_code_output(Lval), Name) :-
    lval_contains_var(Lval, Name).
target_code_component_contains_var(name(EntityName), DataName) :-
    EntityName = qual(ModuleName, QualKind, entity_data(UnqualDataName)),
    DataName = qual(ModuleName, QualKind, UnqualDataName),
    % This is a place where we can succeed.
    true.

has_foreign_languages(Statement, Langs) :-
    GetTargetCode = (pred(Lang::out) is nondet :-
        statement_contains_statement(Statement, SubStatement),
        SubStatement = statement(atomic(
            outline_foreign_proc(Lang, _, _, _)), _)
        ),
    solutions.solutions(GetTargetCode, Langs).

%-----------------------------------------------------------------------------%
%
% Routines that deal with definitions.
%

defn_contains_foreign_code(NativeTargetLang, Defn) :-
    Defn = mlds_defn(_Name, _Context, _Flags, Body),
    Body = mlds_function(_, _, body_defined_here(FunctionBody), _, _),
    statement_contains_statement(FunctionBody, Statement),
    Statement = statement(Stmt, _),
    (
        Stmt = atomic(inline_target_code(TargetLang, _)),
        TargetLang \= NativeTargetLang
    ;
        Stmt = atomic(outline_foreign_proc(_, _, _, _))
    ).

defn_contains_outline_foreign_proc(ForeignLang, Defn) :-
    Defn = mlds_defn(_Name, _Context, _Flags, Body),
    Body = mlds_function(_, _, body_defined_here(FunctionBody), _, _),
    statement_contains_statement(FunctionBody, Statement),
    Statement = statement(Stmt, _),
    Stmt = atomic(outline_foreign_proc(ForeignLang, _, _, _)).

defn_is_type(Defn) :-
    Defn = mlds_defn(Name, _Context, _Flags, _Body),
    Name = entity_type(_, _).

defn_is_function(Defn) :-
    Defn = mlds_defn(Name, _Context, _Flags, _Body),
    Name = entity_function(_, _, _, _).

defn_is_type_ctor_info(Defn) :-
    Defn = mlds_defn(_Name, _Context, _Flags, Body),
    Body = mlds_data(Type, _, _),
    Type = mlds_rtti_type(item_type(RttiId)),
    RttiId = ctor_rtti_id(_, RttiName),
    RttiName = type_ctor_info.

defn_is_commit_type_var(Defn) :-
    Defn = mlds_defn(_Name, _Context, _Flags, Body),
    Body = mlds_data(Type, _, _),
    Type = mlds_commit_type.

defn_is_public(Defn) :-
    Defn = mlds_defn(_Name, _Context, Flags, _Body),
    access(Flags) = public.

% defns_contains_var:
% defn_contains_var:
% defn_body_contains_var:
% function_body_contains_var:
%   Succeeds iff the specified construct contains a reference to
%   the specified variable.
%
defns_contains_var(Defns, Name) :-
    list.member(Defn, Defns),
    defn_contains_var(Defn, Name).

defn_contains_var(mlds_defn(_Name, _Context, _Flags, DefnBody), Name) :-
    defn_body_contains_var(DefnBody, Name).

:- pred defn_body_contains_var(mlds_entity_defn::in, mlds_data::in)
    is semidet.

    % XXX Should we include variables in the GC_TraceCode field here?
defn_body_contains_var(mlds_data(_Type, Initializer, _GC_TraceCode), Name) :-
    initializer_contains_var(Initializer, Name).
defn_body_contains_var(mlds_function(_PredProcId, _Params, FunctionBody,
        _Attrs, _EnvVarNames), Name) :-
    function_body_contains_var(FunctionBody, Name).
defn_body_contains_var(mlds_class(ClassDefn), Name) :-
    ClassDefn = mlds_class_defn(_Kind, _Imports, _Inherits, _Implements,
        CtorDefns, FieldDefns),
    ( defns_contains_var(FieldDefns, Name)
    ; defns_contains_var(CtorDefns, Name)
    ).

:- pred function_body_contains_var(mlds_function_body::in, mlds_data::in)
    is semidet.

% function_body_contains_var(body_external, _) :- fail.
function_body_contains_var(body_defined_here(Statement), Name) :-
    statement_contains_var(Statement, Name).

%-----------------------------------------------------------------------------%
%
% Routines that deal with lvals/rvals.
%

% initializer_contains_var:
% rvals_contains_var:
% maybe_rval_contains_var:
% rval_contains_var:
% lvals_contains_var:
% lval_contains_var:
%   Succeeds iff the specified construct contains a reference to
%   the specified variable.

% initializer_contains_var(no_initializer, _) :- fail.
initializer_contains_var(init_obj(Rval), Name) :-
    rval_contains_var(Rval, Name).
initializer_contains_var(init_struct(_Type, Inits), Name) :-
    list.member(Init, Inits),
    initializer_contains_var(Init, Name).
initializer_contains_var(init_array(Inits), Name) :-
    list.member(Init, Inits),
    initializer_contains_var(Init, Name).

rvals_contains_var(Rvals, Name) :-
    list.member(Rval, Rvals),
    rval_contains_var(Rval, Name).

% maybe_rval_contains_var(no, _Name) :- fail.
maybe_rval_contains_var(yes(Rval), Name) :-
    rval_contains_var(Rval, Name).

rval_contains_var(lval(Lval), Name) :-
    lval_contains_var(Lval, Name).
rval_contains_var(mkword(_Tag, Rval), Name) :-
    rval_contains_var(Rval, Name).
rval_contains_var(const(Const), QualDataName) :-
    Const = data_addr_const(DataAddr),
    DataAddr = data_addr(ModuleName, DataName),
    QualDataName = qual(ModuleName, _QualKind, DataName),
    % this is a place where we can succeed
    true.
rval_contains_var(unop(_Op, Rval), Name) :-
    rval_contains_var(Rval, Name).
rval_contains_var(binop(_Op, X, Y), Name) :-
    ( rval_contains_var(X, Name)
    ; rval_contains_var(Y, Name)
    ).
rval_contains_var(mem_addr(Lval), Name) :-
    lval_contains_var(Lval, Name).

lvals_contains_var(Lvals, Name) :-
    list.member(Lval, Lvals),
    lval_contains_var(Lval, Name).

lval_contains_var(field(_MaybeTag, Rval, _FieldId, _, _), Name) :-
    rval_contains_var(Rval, Name).
lval_contains_var(mem_ref(Rval, _Type), Name) :-
    rval_contains_var(Rval, Name).
lval_contains_var(var(qual(ModuleName, QualKind, Name), _Type),
        qual(ModuleName, QualKind, var(Name))) :-
    % This is another place where we can succeed.
    true.

%-----------------------------------------------------------------------------%

type_needs_lowlevel_rep(Target, Type) :-
    type_to_ctor_and_args(Type, TypeCtor, _Args),
    type_ctor_needs_lowlevel_rep(Target, TypeCtor).

    % XXX Do we need to do the same for the Java back-end?
type_ctor_needs_lowlevel_rep(target_il, type_ctor(TypeName, _Arity)) :-
    mercury_public_builtin_module(Builtin),
    mercury_private_builtin_module(PrivateBuiltin),
    RttiImplementation = unqualified("rtti_implementation"),
    Univ = unqualified("univ"),
    MutVar = unqualified("mutvar"),
    TypeDesc = unqualified("type_desc"),
    ( TypeName = qualified(PrivateBuiltin, "base_typeclass_info")
    ; TypeName = qualified(PrivateBuiltin, "type_ctor_info")
    ; TypeName = qualified(PrivateBuiltin, "typeclass_info")
    ; TypeName = qualified(PrivateBuiltin, "type_info")

        % Use lowlevel types for all types in rtti_implementation
        % as this allows as to add new types needed to manipulate
        % the RTTI type safely easily.
    ; TypeName = qualified(RttiImplementation, _)

    ; TypeName = qualified(TypeDesc, "type_desc")
    ; TypeName = qualified(TypeDesc, "pseudo_type_desc")
    ; TypeName = qualified(TypeDesc, "type_ctor_desc")

        % Types which don't have a Mercury representation.
    ; TypeName = qualified(PrivateBuiltin, "ref")
    ; TypeName = qualified(PrivateBuiltin, "heap_pointer")
    ; TypeName = qualified(Builtin, "c_pointer")

        % XXX These types are referenced in IL and C# code,
        % so it is easier to just keep their low level representation
        % for the moment.
    ; TypeName = qualified(Builtin, "comparison_result")
    ; TypeName = qualified(Univ, "univ")
    ; TypeName = qualified(MutVar, "mutvar")
    ).

%-----------------------------------------------------------------------------%

gen_init_builtin_const(Name) = init_obj(Rval) :-
    mercury_private_builtin_module(PrivateBuiltin),
    MLDS_Module = mercury_module_name_to_mlds(PrivateBuiltin),
    % XXX These are actually enumeration constants.
    % Perhaps we should be using an enumeration type here,
    % rather than `mlds_native_int_type'.
    Type = mlds_native_int_type,
    Rval = lval(var(qual(MLDS_Module, module_qual, mlds_var_name(Name, no)),
        Type)).

gen_init_array(Conv, List) = init_array(list.map(Conv, List)).

gen_init_maybe(_Type, Conv, yes(X)) = Conv(X).
gen_init_maybe(Type, _Conv, no) = gen_init_null_pointer(Type).

gen_init_null_pointer(Type) = init_obj(const(null(Type))).

gen_init_string(String) = init_obj(const(string_const(String))).

gen_init_int(Int) = init_obj(const(int_const(Int))).

gen_init_bool(no) = init_obj(const(false_const)).
gen_init_bool(yes) = init_obj(const(true_const)).

gen_init_boxed_int(Int) =
    init_obj(unop(box(mlds_native_int_type), const(int_const(Int)))).

gen_init_reserved_address(ModuleInfo, ReservedAddress) =
    % XXX using `mlds_generic_type' here is probably wrong
    init_obj(ml_gen_reserved_address(ModuleInfo, ReservedAddress,
        mlds_generic_type)).

%-----------------------------------------------------------------------------%
