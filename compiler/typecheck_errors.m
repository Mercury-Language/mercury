%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: typecheck_errors.m.
% Main author: fjh.
%
% This file contains predicates to report errors and debugging messages for
% typechecking.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.typecheck_errors.
:- interface.

:- import_module check_hlds.typecheck_info.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

:- type cons_error
    --->    foreign_type_constructor(type_ctor, hlds_type_defn)
    ;       abstract_imported_type
    ;       invalid_field_update(ctor_field_name, hlds_ctor_field_defn,
                tvarset, list(tvar))
    ;       new_on_non_existential_type(type_ctor).

%-----------------------------------------------------------------------------%

:- pred report_pred_call_error(simple_call_id::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

:- pred report_no_clauses(string::in, pred_id::in, pred_info::in,
    module_info::in, io::di, io::uo) is det.

:- pred report_warning_too_much_overloading(typecheck_info::in,
    io::di, io::uo) is det.

:- pred report_error_unif_var_var(typecheck_info::in,
    prog_var::in, prog_var::in, type_assign_set::in, io::di, io::uo)
    is det.

:- pred report_error_lambda_var(typecheck_info::in, pred_or_func::in,
    lambda_eval_method::in, prog_var::in, list(prog_var)::in,
    type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_functor_type(typecheck_info::in,
    prog_var::in, list(cons_type_info)::in, cons_id::in, int::in,
    type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_functor_arg_types(typecheck_info::in, prog_var::in,
    list(cons_type_info)::in, cons_id::in, list(prog_var)::in,
    args_type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_var(typecheck_info::in, prog_var::in, mer_type::in,
    type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_arg_var(typecheck_info::in, prog_var::in,
    args_type_assign_set::in, io::di, io::uo) is det.

:- pred report_error_undef_cons(typecheck_info::in, list(cons_error)::in,
    cons_id::in, int::in, io::di, io::uo) is det.

:- pred report_ambiguity_error(typecheck_info::in,
    type_assign::in, type_assign::in, io::di, io::uo) is det.

:- pred report_unsatisfiable_constraints(type_assign_set::in,
    typecheck_info::in, typecheck_info::out, io::di, io::uo) is det.

:- pred report_missing_tvar_in_foreign_code(typecheck_info::in, string::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Write out the inferred `pred' or `func' declarations
    % for a list of predicates.  Don't write out the inferred types
    % for assertions.
    %
:- pred write_inference_messages(list(pred_id)::in, module_info::in,
    io::di, io::uo) is det.

    % Used for debugging typechecking.
    %
:- pred checkpoint(string::in, typecheck_info::in, typecheck_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.type_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.pred_table.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_io_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

report_pred_call_error(PredCallId, !Info, !IO) :-
    PredCallId = simple_call_id(PredOrFunc0, SymName, _Arity),
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    (
        predicate_table_search_pf_sym(PredicateTable,
            calls_are_fully_qualified(!.Info ^ pred_markers),
            PredOrFunc0, SymName, OtherIds),
        predicate_table_get_preds(PredicateTable, Preds),
        OtherIds \= []
    ->
        typecheck_find_arities(Preds, OtherIds, Arities),
        report_error_pred_num_args(!.Info, PredCallId, Arities, !IO)
    ;
        ( PredOrFunc0 = predicate, PredOrFunc = function
        ; PredOrFunc0 = function, PredOrFunc = predicate
        ),
        predicate_table_search_pf_sym(PredicateTable,
            calls_are_fully_qualified(!.Info ^ pred_markers),
            PredOrFunc, SymName, OtherIds),
        OtherIds \= []
    ->
        report_error_func_instead_of_pred(!.Info, PredOrFunc,
            PredCallId, !IO)
    ;
        report_error_undef_pred(!.Info, PredCallId, !IO)
    ),
    typecheck_info_set_found_error(yes, !Info).

:- pred typecheck_find_arities(pred_table::in, list(pred_id)::in,
    list(int)::out) is det.

typecheck_find_arities(_, [], []).
typecheck_find_arities(Preds, [PredId | PredIds], [Arity | Arities]) :-
    map.lookup(Preds, PredId, PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    typecheck_find_arities(Preds, PredIds, Arities).

:- pred report_error_pred_num_args(typecheck_info::in, simple_call_id::in,
    list(int)::in, io::di, io::uo) is det.

report_error_pred_num_args(Info, SimpleCallId, Arities, !IO) :-
    SimpleCallId = simple_call_id(PredOrFunc, SymName, Arity),
    write_context_and_pred_id(Info, !IO),
    typecheck_info_get_context(Info, Context),
    prog_out.write_context(Context, !IO),
    io.write_string("  error: ", !IO),
    report_error_num_args(yes(PredOrFunc), Arity, Arities, !IO),
    io.nl(!IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  in call to ", !IO),
    prog_out.write_pred_or_func(PredOrFunc, !IO),
    io.write_string(" `", !IO),
    prog_out.write_sym_name(SymName, !IO),
    io.write_string("'.\n", !IO).

:- pred report_error_func_instead_of_pred(typecheck_info::in, pred_or_func::in,
    simple_call_id::in, io::di, io::uo) is det.

report_error_func_instead_of_pred(Info, PredOrFunc, PredCallId, !IO) :-
    report_error_undef_pred(Info, PredCallId, !IO),
    typecheck_info_get_context(Info, Context),
    PredOrFuncStr = pred_or_func_to_full_str(PredOrFunc),
    (
        PredOrFunc = function,
        Pieces = [words("(There is a *" ++ PredOrFuncStr ++ "*"),
            words("with that name, however."), nl,
            words("Perhaps you forgot to add"),
            fixed("` = ...'?)")]
    ;
        PredOrFunc = predicate,
        Pieces = [words("(There is a *" ++ PredOrFuncStr ++ "*"),
            words("with that name, however.)")]
    ),
    write_error_pieces_not_first_line(Context, 0, Pieces, !IO).

:- pred report_error_undef_pred(typecheck_info::in, simple_call_id::in,
    io::di, io::uo) is det.

report_error_undef_pred(Info, SimpleCallId, !IO) :-
    SimpleCallId = simple_call_id(_PredOrFunc, PredName, Arity),
    typecheck_info_get_context(Info, Context),
    write_typecheck_info_context(Info, !IO),
    (
        PredName = unqualified("->"),
        ( Arity = 2 ; Arity = 4 )
    ->
        io.write_string("  error: `->' without `;'.\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string("  Note: the else part is not optional.\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string("  Every if-then must have an else.\n", !IO)
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        )
    ;
        PredName = unqualified("else"),
        ( Arity = 2 ; Arity = 4 )
    ->
        io.write_string("  error: unmatched `else'.\n", !IO)
    ;
        PredName = unqualified("if"),
        ( Arity = 2 ; Arity = 4 )
    ->
        io.write_string("  error: `if' without `then' or `else'.\n", !IO)
    ;
        PredName = unqualified("then"),
        ( Arity = 2 ; Arity = 4 )
    ->
        io.write_string("  error: `then' without `if' or `else'.\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string("  Note: the `else' part is not optional.\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string("  Every if-then must have an `else'.\n", !IO)
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        )
    ;
        PredName = unqualified("apply"),
        Arity >= 1
    ->
        report_error_apply_instead_of_pred(Info, !IO)
    ;
        PredName = unqualified(PurityString),
        Arity = 1,
        ( PurityString = "impure" ; PurityString = "semipure" )
    ->
        io.write_string("  error: `", !IO),
        io.write_string(PurityString, !IO),
        io.write_string("' marker in an inappropriate place.\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string("  Such markers only belong " ++
                "before predicate calls.\n", !IO)
        ;
            VerboseErrors = no,
            globals.io_set_extra_error_info(yes, !IO)
        )
    ;
        PredName = unqualified("some"),
        Arity = 2
    ->
        io.write_string("  syntax error in existential " ++
            "quantification: first\n", !IO),
        prog_out.write_context(Context, !IO),
        io.write_string("  argument of `some' should be " ++
            "a list of variables.\n", !IO)
    ;
        io.write_string("  error: undefined ", !IO),
        write_simple_call_id(SimpleCallId, !IO),
        ( PredName = qualified(ModuleQualifier, _) ->
            maybe_report_missing_import(Info, ModuleQualifier, !IO)
        ;
            io.write_string(".\n", !IO)
        )
    ).

:- pred report_error_apply_instead_of_pred(typecheck_info::in, io::di, io::uo)
    is det.

report_error_apply_instead_of_pred(Info, !IO) :-
    typecheck_info_get_context(Info, Context),
    Pieces1 = [words("error: the language construct `apply'"),
        words("should be used as an expression, not as a goal."), nl],
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        Pieces2 = [words("Perhaps you forgot to add"),
            fixed("` = ...'?)"), nl,
            words("If you're trying to invoke"),
            words("a higher-order predicate,"),
            words("use `call', not `apply'."), nl,
            words("If you're trying to curry"),
            words("a higher-order function,"),
            words("use a forwarding function:"), nl,
            words("e.g. instead of "),
            fixed("`NewFunc = apply(OldFunc, X)'"),
            words("use"),
            fixed("`NewFunc = my_apply(OldFunc, X)'"), 
            words("where `my_apply' is defined"),
            words("with the appropriate arity, e.g."),
            fixed("`my_apply(Func, X, Y) :- apply(Func, X, Y).'")]
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO),
        Pieces2 = []
    ),
    write_error_pieces_not_first_line(Context, 0, Pieces1 ++ Pieces2, !IO).

%-----------------------------------------------------------------------------%

report_no_clauses(MessageKind, PredId, PredInfo, ModuleInfo, !IO) :-
    io.get_exit_status(Status, !IO),
    ( Status = 0 ->
        pred_info_context(PredInfo, Context),
        PredPieces = describe_one_pred_name(ModuleInfo,
            should_not_module_qualify, PredId),
        ErrorMsg = [words(MessageKind ++ ": no clauses for ") | PredPieces] ++
            [suffix(".")],
        error_util.write_error_pieces(Context, 0, ErrorMsg, !IO)
    ;
        % It is possible (and even likely) that the error that got the exit
        % status set was caused by a syntax error in a clause defining this
        % predicate or function. Reporting a missing clause would therefore
        % be quite likely to be redundant and misleading. Even if this
        % predicate or function truly has no clauses, this error would be
        % caught once the other errors (the ones leading to the exit status)
        % are fixed by the programmer.
        true
    ).

%-----------------------------------------------------------------------------%

report_warning_too_much_overloading(Info, !IO) :-
    typecheck_info_get_context(Info, Context),
    make_pred_id_preamble(Info, Preamble),
    InitWarning = [fixed(Preamble),
        words("warning: highly ambiguous overloading."), nl],

    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        VerboseWarning = [
            words("This may cause type-checking to be very slow."),
            words("It may also make your code difficult to understand."), nl],
        InitVerboseWarning = InitWarning ++ VerboseWarning
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO),
        InitVerboseWarning = InitWarning
    ),
    FirstSpec = error_msg_spec(yes, Context, 0, InitVerboseWarning),

    typecheck_info_get_overloaded_symbols(Info, OverloadedSymbolSet),
    map.to_assoc_list(OverloadedSymbolSet, OverloadedSymbols),
    OverloadedSymbolsSortedContexts =
        assoc_list.map_values_only(sort_and_remove_dups, OverloadedSymbols),
    (
        OverloadedSymbolsSortedContexts = [],
        Specs = [FirstSpec]
    ;
        (
            OverloadedSymbolsSortedContexts = [_ - Contexts],
            (
                Contexts = [],
                unexpected(this_file,
                    "report_warning_too_much_overloading: no contexts")
            ;
                Contexts = [_],
                SecondSpecPieces =
                    [words("The following symbol was overloaded"),
                    words("in the following context."), nl]
            ;
                Contexts = [_, _ | _],
                SecondSpecPieces =
                    [words("The following symbol was overloaded"),
                    words("in the following contexts."), nl]
            )
        ;
            OverloadedSymbolsSortedContexts = [_, _ | _],
            SecondSpecPieces =
                [words("The following symbols were overloaded"),
                words("in the following contexts."), nl]
        ),
        SecondSpec = error_msg_spec(no, Context, 0, SecondSpecPieces),
        typecheck_info_get_module_info(Info, ModuleInfo),
        DetailSpecsList = list.map(describe_overloaded_symbol(ModuleInfo),
            OverloadedSymbolsSortedContexts),
        list.condense(DetailSpecsList, DetailSpecs),
        Specs = [FirstSpec, SecondSpec | DetailSpecs]
    ),
    record_warning(!IO),
    write_error_specs(Specs, !IO).

:- func describe_overloaded_symbol(module_info,
    pair(overloaded_symbol, list(prog_context))) = list(error_msg_spec).

describe_overloaded_symbol(ModuleInfo, Symbol - SortedContexts) = Specs :-
    (
        SortedContexts = [],
        unexpected(this_file, "describe_overloaded_symbol: no context")
    ;
        SortedContexts = [FirstContext | LaterContexts],
        % We print a detailed message for the first context, but omit
        % repeating the list of possible matches for any later contexts.
        (
            Symbol = overloaded_pred(CallId, PredIds),
            StartPieces = [words("The predicate symbol"),
                simple_call_id(CallId), suffix("."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            PredIdPiecesList = list.map(describe_one_pred_name(ModuleInfo,
                should_module_qualify), PredIds),
            PredIdPieces = component_list_to_line_pieces(PredIdPiecesList,
                [suffix(".")]),
            FirstPieces = StartPieces ++ PredIdPieces,
            LaterPieces = [words("The predicate symbol"),
                simple_call_id(CallId), words("is also overloaded here.")]
        ;
            Symbol = overloaded_func(ConsId, Sources),
            ( ConsId = cons(SymName, Arity) ->
                ConsIdPiece = sym_name_and_arity(SymName / Arity)
            ;
                ConsIdPiece = fixed(cons_id_to_string(ConsId))
            ),
            StartPieces = [words("The function symbol"), ConsIdPiece,
                suffix("."), nl,
                words("The possible matches are:"), nl_indent_delta(1)],
            SourcePiecesList = list.map(
                describe_cons_type_info_source(ModuleInfo), Sources),
            SourcePieces = component_list_to_line_pieces(SourcePiecesList,
                [suffix(".")]),
            FirstPieces = StartPieces ++ SourcePieces,
            LaterPieces = [words("The function symbol"), ConsIdPiece,
                words("is also overloaded here.")]
        ),
        FirstSpec = error_msg_spec(no, FirstContext, 0, FirstPieces),
        LaterSpecs = list.map(context_to_error_msg_spec(LaterPieces),
            LaterContexts),
        Specs = [FirstSpec | LaterSpecs]
    ).

:- func context_to_error_msg_spec(list(format_component), prog_context)
    = error_msg_spec.

context_to_error_msg_spec(Pieces, Context) =
    error_msg_spec(no, Context, 0, Pieces).

:- func describe_cons_type_info_source(module_info, cons_type_info_source)
    = list(format_component).

describe_cons_type_info_source(ModuleInfo, Source) = Pieces :-
    (
        Source = source_type(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("the type constructor"),
            sym_name_and_arity(SymName / Arity)]
    ;
        Source = source_builtin_type(TypeCtorName),
        Pieces = [words("the builtin type constructor"), quote(TypeCtorName)]
    ;
        Source = source_get_field_access(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("a `get' field access function"),
            words("for the type constructor"),
            sym_name_and_arity(SymName / Arity)]
    ;
        Source = source_set_field_access(TypeCtor),
        TypeCtor = type_ctor(SymName, Arity),
        Pieces = [words("a `set' field access function"),
            words("for the type constructor"),
            sym_name_and_arity(SymName / Arity)]
    ;
        Source = source_pred(PredId),
        Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)
    ;
        Source = source_apply(ApplyOp),
        Pieces = [words("the builtin operator constructor"), quote(ApplyOp)]
    ).

%-----------------------------------------------------------------------------%

report_error_unif_var_var(Info, X, Y, TypeAssignSet, !IO) :-
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_varset(Info, VarSet),
    typecheck_info_get_unify_context(Info, UnifyContext),

    write_context_and_pred_id(Info, !IO),
    hlds_out.write_unify_context(UnifyContext, Context, !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  type error in unification of variable `", !IO),
    mercury_output_var(X, VarSet, no, !IO),
    io.write_string("'\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  and variable `", !IO),
    mercury_output_var(Y, VarSet, no, !IO),
    io.write_string("'.\n", !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  `", !IO),
    mercury_output_var(X, VarSet, no, !IO),
    io.write_string("'", !IO),
    write_type_of_var(Info, Context, TypeAssignSet, X, !IO),
    io.write_string(",\n", !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  `", !IO),
    mercury_output_var(Y, VarSet, no, !IO),
    io.write_string("'", !IO),
    write_type_of_var(Info, Context, TypeAssignSet, Y, !IO),
    io.write_string(".\n", !IO),

    write_type_assign_set_msg(TypeAssignSet, VarSet, !IO).

report_error_lambda_var(Info, PredOrFunc, _EvalMethod, Var, ArgVars,
        TypeAssignSet, !IO) :-
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_varset(Info, VarSet),
    typecheck_info_get_unify_context(Info, UnifyContext),

    write_context_and_pred_id(Info, !IO),
    hlds_out.write_unify_context(UnifyContext, Context, !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  type error in unification of ", !IO),
    write_argument_name(VarSet, Var, !IO),
    io.write_string("\n", !IO),
    prog_out.write_context(Context, !IO),

    (
        PredOrFunc = predicate,
        io.write_string("  and `", !IO),
        io.write_string("pred(", !IO),
        mercury_output_vars(ArgVars, VarSet, no, !IO),
        io.write_string(") :- ...':\n", !IO)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(ArgVars, FuncArgs, RetVar),
        io.write_string("  and `", !IO),
        io.write_string("func(", !IO),
        mercury_output_vars(FuncArgs, VarSet, no, !IO),
        io.write_string(") = ", !IO),
        mercury_output_var(RetVar, VarSet, no, !IO),
        io.write_string(" :- ...':\n", !IO)
    ),

    prog_out.write_context(Context, !IO),
    io.write_string("  ", !IO),
    write_argument_name(VarSet, Var, !IO),
    write_type_of_var(Info, Context, TypeAssignSet, Var, !IO),
    io.write_string(",\n", !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  lambda expression has type `", !IO),
    (
        PredOrFunc = predicate,
        io.write_string("pred", !IO),
        ( ArgVars = [] ->
            true
        ;
            io.write_string("(_", !IO),
            list.length(ArgVars, NumArgVars),
            NumArgVars1 = NumArgVars - 1,
            list.duplicate(NumArgVars1, ", _", Strings),
            io.write_strings(Strings, !IO),
            io.write_string(")", !IO)
        )
    ;
        PredOrFunc = function,
        io.write_string("func", !IO),
        pred_args_to_func_args(ArgVars, FuncArgs2, _),
        ( FuncArgs2 = [] ->
            true
        ;
            io.write_string("(_", !IO),
            list.length(FuncArgs2, NumArgVars),
            NumArgVars1 = NumArgVars - 1,
            list.duplicate(NumArgVars1, ", _", Strings),
            io.write_strings(Strings, !IO),
            io.write_string(")", !IO)
        ),
        io.write_string(" = _", !IO)
    ),
    io.write_string("'.\n", !IO),
    write_type_assign_set_msg(TypeAssignSet, VarSet, !IO).

report_error_functor_type(Info, Var, ConsDefnList, Functor, Arity,
        TypeAssignSet, !IO) :-
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_varset(Info, VarSet),
    typecheck_info_get_unify_context(Info, UnifyContext),

    write_context_and_pred_id(Info, !IO),
    hlds_out.write_unify_context(UnifyContext, Context, !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  type error in unification of ", !IO),
    write_argument_name(VarSet, Var, !IO),
    io.write_string("\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  and ", !IO),
    write_functor_name(Functor, Arity, !IO),
    io.write_string(".\n", !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  ", !IO),
    write_argument_name(VarSet, Var, !IO),
    write_type_of_var(Info, Context, TypeAssignSet, Var, !IO),
    io.write_string(",\n", !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  ", !IO),
    write_functor_name(Functor, Arity, !IO),
    write_type_of_functor(Functor, Arity, Context, ConsDefnList, !IO),
    io.write_string(".\n", !IO),

    write_type_assign_set_msg(TypeAssignSet, VarSet, !IO).

report_error_functor_arg_types(Info, Var, ConsDefnList, Functor, Args,
        ArgsTypeAssignSet, !IO) :-
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_varset(Info, VarSet),
    typecheck_info_get_unify_context(Info, UnifyContext),
    typecheck_info_get_module_info(Info, ModuleInfo),
    list.length(Args, Arity),

    write_context_and_pred_id(Info, !IO),
    hlds_out.write_unify_context(UnifyContext, Context, !IO),

    prog_out.write_context(Context, !IO),
    io.write_string("  in unification of ", !IO),
    write_argument_name(VarSet, Var, !IO),
    io.write_string("\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  and term `", !IO),
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    hlds_out.write_functor_cons_id(StrippedFunctor, Args, VarSet,
        ModuleInfo, no, !IO),
    io.write_string("':\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  type error in argument(s) of ", !IO),
    write_functor_name(StrippedFunctor, Arity, !IO),
    io.write_string(".\n", !IO),

    ConsArgTypesSet = list.map(get_callee_arg_types, ArgsTypeAssignSet),

    % If we know the type of the function symbol, and each argument
    % also has at most one possible type, then we prefer to print an
    % error message that mentions the actual and expected types of the
    % arguments only for the arguments in which the two types differ.
    (
        list.all_same(ConsArgTypesSet),
        ConsArgTypesSet = [ConsArgTypes | _],
        assoc_list.from_corresponding_lists(Args, ConsArgTypes, ArgExpTypes),
        TypeAssigns = list.map(get_caller_arg_assign, ArgsTypeAssignSet),
        find_mismatched_args(ArgExpTypes, TypeAssigns, 1,
            SimpleMismatches, ComplexMismatches, AllMismatches),
        expect(list.is_not_empty(AllMismatches), this_file,
            "report_error_functor_arg_types: no mismatches"),
        ComplexMismatches = []
    ->
        report_mismatched_args(SimpleMismatches, yes, VarSet, Functor,
            Context, !IO)
    ;
        % XXX If we can compute AllMismatches, then we should use it
        % to report which arguments are OK, and which are suspect.

        convert_args_type_assign_set(ArgsTypeAssignSet, TypeAssignSet),

        %
        % For polymorphic data structures,
        % the type of `Var' (the functor's result type)
        % can affect the valid types for the arguments.
        %
        (
            % Could the type of the functor be polymorphic?
            list.member(ConsDefn, ConsDefnList),
            ConsDefn ^ cti_arg_types = [_ | _]
        ->
            % If so, print out the type of `Var'.
            prog_out.write_context(Context, !IO),
            io.write_string("  ", !IO),
            write_argument_name(VarSet, Var, !IO),
            write_type_of_var(Info, Context, TypeAssignSet, Var, !IO),
            io.write_string(",\n", !IO)
        ;
            true 
        ),

        prog_out.write_context(Context, !IO),
        io.write_string("  ", !IO),
        write_functor_name(Functor, Arity, !IO),
        write_type_of_functor(Functor, Arity, Context, ConsDefnList, !IO),
        write_types_of_vars(Args, VarSet, Context, Info, TypeAssignSet, !IO),
        write_type_assign_set_msg(TypeAssignSet, VarSet, !IO)
    ).

:- type mismatch_info
    --->    mismatch_info(
                int,        % argument number, starting from 1
                prog_var,   % variable in that position
                list(type_mismatch)
                    % list of possible type mismatches
            ).

:- type type_mismatch
    --->    type_mismatch(
                mer_type,   % actual type of that variable
                mer_type,   % expected type of that variable
                tvarset,    % the type vars in the expected
                            % and expected types
                head_type_params % existentially quantified type vars
            ).

:- pred find_mismatched_args(assoc_list(prog_var, mer_type)::in,
    type_assign_set::in, int::in, list(mismatch_info)::out,
    list(mismatch_info)::out, list(mismatch_info)::out) is det.

find_mismatched_args([], _, _, [], [], []).
find_mismatched_args([Arg - ExpType | ArgExpTypes], TypeAssignSet, ArgNum0,
        SimpleMismatches, ComplexMismatches, AllMismatches) :-
    ArgNum1 = ArgNum0 + 1,
    find_mismatched_args(ArgExpTypes, TypeAssignSet, ArgNum1,
        SimpleMismatchesTail, ComplexMismatchesTail,
        AllMismatchesTail),
    get_type_stuff(TypeAssignSet, Arg, TypeStuffList),
    list.filter_map(substitute_types_check_match(ExpType), TypeStuffList,
        TypeMismatches0),
    list.sort_and_remove_dups(TypeMismatches0, TypeMismatches),
    (
        TypeMismatches = [],
        SimpleMismatches = SimpleMismatchesTail,
        ComplexMismatches = ComplexMismatchesTail,
        AllMismatches = AllMismatchesTail
    ;
        TypeMismatches = [_],
        Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
        SimpleMismatches = [Mismatch | SimpleMismatchesTail],
        ComplexMismatches = ComplexMismatchesTail,
        AllMismatches = [Mismatch | AllMismatchesTail]
    ;
        TypeMismatches = [_, _ | _],
        Mismatch = mismatch_info(ArgNum0, Arg, TypeMismatches),
        SimpleMismatches = SimpleMismatchesTail,
        ComplexMismatches = [Mismatch | ComplexMismatchesTail],
        AllMismatches = [Mismatch | AllMismatchesTail]
    ).

:- pred substitute_types_check_match(mer_type::in, type_stuff::in,
    type_mismatch::out) is semidet.

substitute_types_check_match(ExpType, TypeStuff, TypeMismatch) :-
    TypeStuff = type_stuff(ArgType, TVarSet, TypeBindings, HeadTypeParams),
    apply_rec_subst_to_type(TypeBindings, ArgType, FullArgType),
    apply_rec_subst_to_type(TypeBindings, ExpType, FullExpType),
    (
        (
            % There is no mismatch if the actual type of the
            % argument is the same as the expected type.
            identical_types(FullArgType, FullExpType)
        ;
            % There is no mismatch if the actual type of the
            % argument has no constraints on it.
            FullArgType = defined(unqualified("<any>"), [], _)
        )
    ->
        fail
    ;
        TypeMismatch = type_mismatch(FullArgType, FullExpType,
            TVarSet, HeadTypeParams)
    ).

:- pred report_mismatched_args(list(mismatch_info)::in, bool::in,
    prog_varset::in, cons_id::in, prog_context::in, io::di, io::uo) is det.

report_mismatched_args([], _, _, _, _, !IO).
report_mismatched_args([Mismatch | Mismatches], First, VarSet, Functor,
        Context, !IO) :-
    Mismatch = mismatch_info(ArgNum, Var, TypeMismatches),
    ( TypeMismatches = [TypeMismatch] ->
        TypeMismatch = type_mismatch(ActType, ExpType, TVarSet, HeadTypeParams)
    ;
        unexpected(this_file,
            "report_mismatched_args: more than one type mismatch")
    ),
    prog_out.write_context(Context, !IO),
    (
        % Handle higher-order syntax such as ''(F, A) specially:
        % output
        %   Functor (F) has type ...;
        %   argument 1 (A) has type ...
        % instead of
        %   Argument 1 (F) has type ...;
        %   argument 2 (A) has type ...
        Functor = cons(unqualified(""), Arity),
        Arity > 0
    ->
        (
            First = yes,
            io.write_string("  Functor", !IO)
        ;
            First = no,
            io.write_string("  argument ", !IO),
            io.write_int(ArgNum - 1, !IO)
        )
    ;
        (
            First = yes,
            io.write_string("  Argument ", !IO)
        ;
            First = no,
            io.write_string("  argument ", !IO)
        ),
        io.write_int(ArgNum, !IO)
    ),
    ( varset.search_name(VarSet, Var, _) ->
        io.write_string(" (", !IO),
        mercury_output_var(Var, VarSet, no, !IO),
        io.write_string(")", !IO)
    ;
        true
    ),
    io.write_string(" has type `", !IO),
    output_type(ActType, TVarSet, HeadTypeParams, !IO),
    io.write_string("',\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  expected type was `", !IO),
    output_type(ExpType, TVarSet, HeadTypeParams, !IO),
    (
        Mismatches = [],
        io.write_string("'.\n", !IO)
    ;
        Mismatches = [_ | _],
        io.write_string("';\n", !IO),
        report_mismatched_args(Mismatches, no, VarSet, Functor,
            Context, !IO)
    ).

%-----------------------------------------------------------------------------%

report_error_var(Info, Var, Type, TypeAssignSet0, !IO) :-
    typecheck_info_get_pred_markers(Info, PredMarkers),
    typecheck_info_get_called_predid(Info, CalledPredId),
    typecheck_info_get_arg_num(Info, ArgNum),
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_unify_context(Info, UnifyContext),
    get_type_stuff(TypeAssignSet0, Var, TypeStuffList),
    typecheck_info_get_varset(Info, VarSet),
    write_context_and_pred_id(Info, !IO),
    write_call_context(Context, PredMarkers, CalledPredId, ArgNum,
        UnifyContext, !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  type error: ", !IO),
    ( TypeStuffList = [SingleTypeStuff] ->
        write_argument_name(VarSet, Var, !IO),
        SingleTypeStuff = type_stuff(VType, TVarSet, TBinding, HeadTypeParams),
        io.write_string(" has type `", !IO),
        write_type_b(VType, TVarSet, TBinding, HeadTypeParams, !IO),
        io.write_string("',\n", !IO),
        prog_out.write_context(Context, !IO),
        io.write_string("  expected type was `", !IO),
        write_type_b(Type, TVarSet, TBinding, HeadTypeParams, !IO),
        io.write_string("'.\n", !IO)
    ;
        io.write_string("type of ", !IO),
        write_argument_name(VarSet, Var, !IO),
        io.write_string(" does not match its expected type;\n", !IO),

        prog_out.write_context(Context, !IO),
        io.write_string("  ", !IO),
        write_argument_name(VarSet, Var, !IO),
        io.write_string(" has overloaded actual/expected types {\n", !IO),

        write_var_type_stuff_list(Context, TypeStuffList, Type, !IO),
        io.write_string("\n", !IO),

        prog_out.write_context(Context, !IO),
        io.write_string("  }.\n", !IO)
    ),
    write_type_assign_set_msg(TypeAssignSet0, VarSet, !IO).

report_error_arg_var(Info, Var, ArgTypeAssignSet0) -->
    { typecheck_info_get_pred_markers(Info, PredMarkers) },
    { typecheck_info_get_called_predid(Info, CalledPredId) },
    { typecheck_info_get_arg_num(Info, ArgNum) },
    { typecheck_info_get_context(Info, Context) },
    { typecheck_info_get_unify_context(Info, UnifyContext) },
    { get_arg_type_stuff(ArgTypeAssignSet0, Var, ArgTypeStuffList) },
    { typecheck_info_get_varset(Info, VarSet) },
    write_context_and_pred_id(Info),
    write_call_context(Context, PredMarkers, CalledPredId, ArgNum,
        UnifyContext),
    prog_out.write_context(Context),
    io.write_string("  type error: "),
    ( { ArgTypeStuffList = [SingleArgTypeStuff] } ->
        write_argument_name(VarSet, Var),
        { SingleArgTypeStuff = arg_type_stuff(Type0, VType0, TVarSet,
            HeadTypeParams) },
        io.write_string(" has type `"),
        output_type(VType0, TVarSet, HeadTypeParams),
        io.write_string("',\n"),
        prog_out.write_context(Context),
        io.write_string("  expected type was `"),
        output_type(Type0, TVarSet, HeadTypeParams),
        io.write_string("'.\n")
    ;
        io.write_string("type of "),
        write_argument_name(VarSet, Var),
        io.write_string(" does not match its expected type;\n"),

        prog_out.write_context(Context),
        io.write_string("  "),
        write_argument_name(VarSet, Var),
        io.write_string(" has overloaded actual/expected types {\n"),

        write_arg_type_stuff_list(Context, ArgTypeStuffList),
        io.write_string("\n"),

        prog_out.write_context(Context),
        io.write_string("  }.\n")
    ),
    write_args_type_assign_set_msg(ArgTypeAssignSet0, VarSet).

%-----------------------------------------------------------------------------%

report_error_undef_cons(Info, ConsErrors, Functor, Arity, !IO) :-
    typecheck_info_get_pred_markers(Info, PredMarkers),
    typecheck_info_get_called_predid(Info, CalledPredId),
    typecheck_info_get_arg_num(Info, ArgNum),
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_unify_context(Info, UnifyContext),
    write_context_and_pred_id(Info, !IO),
    write_call_context(Context, PredMarkers, CalledPredId, ArgNum,
        UnifyContext, !IO),
    prog_out.write_context(Context, !IO),
    %
    % Check for some special cases, so that we can give
    % clearer error messages.
    %
    (
        Functor = cons(unqualified(Name), _),
        language_builtin(Name, Arity)
    ->
        io.write_string("  error: the language construct ", !IO),
        hlds_out.write_cons_id(Functor, !IO),
        io.write_string(" should be\n", !IO),
        prog_out.write_context(Context, !IO),
        io.write_string("  used as a goal, not as an expression.\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string(
        "  If you are trying to use a goal as a boolean function,\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string(
        "  you should write `if <goal> then yes else no' instead.\n", !IO),
            ( Name = "call" ->
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  If you are trying to invoke a higher-order\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  function, you should use `apply', not `call'.\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  If you're trying to curry a higher-order predicate,\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  see the ""Creating higher-order terms"" section of the\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  Mercury Language Reference Manual.\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  If you really are trying to use `call' as an expression\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  and not as an application of the language builtin\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  call/N, make sure that you have the arity correct, and\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  that the functor `call' is actually defined (if it is\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  defined in a separate module, check that the module is\n", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(
        "  correctly imported).\n", !IO)
            ;
                true 
            )
        ;
            VerboseErrors = no
        )
    ; Functor = cons(unqualified("else"), 2) ->
        io.write_string("  error: unmatched `else'.\n", !IO)
    ; Functor = cons(unqualified("if"), 2) ->
        io.write_string("  error: `if' without `then' or `else'.\n", !IO)
    ; Functor = cons(unqualified("then"), 2) ->
        io.write_string("  error: `then' without `if' or `else'.\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (   
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string(
                "  Note: the `else' part is not optional.\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string(
                "  Every if-then must have an `else'.\n", !IO)
        ;
            VerboseErrors = no  
        )
    ; Functor = cons(unqualified("->"), 2) ->
        io.write_string("  error: `->' without `;'.\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string(
                "  Note: the else part is not optional.\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string(
                "  Every if-then must have an else.\n", !IO)
        ;
            VerboseErrors = no 
        )
    ; Functor = cons(unqualified("^"), 2) ->
        io.write_string("  error: invalid use of field selection " ++
            "operator (`^').\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        (
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string("  This is probably some kind " ++
                "of syntax error.\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string("  The field name must be an " ++
                "atom, not a variable or other term.\n", !IO)
        ;
            VerboseErrors = no 
        )
    ; Functor = cons(unqualified(":="), 2) ->
        io.write_string("  error: invalid use of field update " ++
            "operator (`:=').\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        ( 
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string("  This is probably some kind " ++
                "of syntax error.\n", !IO)
        ;
            VerboseErrors = no 
        )
    ; Functor = cons(unqualified(":-"), 2) ->
        io.write_string("  syntax error in lambda expression " ++
            "(`:-').\n", !IO)
    ; Functor = cons(unqualified("-->"), 2) ->
        io.write_string("  syntax error in DCG lambda expression " ++
            "(`-->').\n", !IO)
    ; Functor = cons(unqualified("."), 2) ->
        io.write_string("  error: the list constructor is " ++
            "now `[|]/2', not `./2'.\n", !IO)
    ; Functor = cons(unqualified("!"), 1) ->
        io.write_string("  error: invalid use of `!' " ++
            "state variable operator.\n", !IO),
        globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
        ( 
            VerboseErrors = yes,
            prog_out.write_context(Context, !IO),
            io.write_string("  You probably meant to use " ++
                "`!.' or `!:'.\n", !IO)
        ;
            VerboseErrors = no 
        )
    ;
        (
            Functor = cons(Constructor, Arity),
            typecheck_info_get_ctors(Info, ConsTable),
            solutions.solutions(
                (pred(N::out) is nondet :-
                    map.member(ConsTable, cons(Constructor, N), _),
                    N \= Arity
                ), ActualArities),
            ActualArities \= []
        ->
            report_wrong_arity_constructor(Constructor, Arity,
                ActualArities, Context, !IO)
        ;
            io.write_string("  error: undefined symbol `", !IO),
            strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
            hlds_out.write_cons_id(StrippedFunctor, !IO),
            io.write_string("'", !IO),
            (
                Functor = cons(Constructor, _),
                Constructor = qualified(ModQual, _)
            ->
                maybe_report_missing_import(Info, ModQual, !IO)
            ;
                Functor = cons(unqualified("[|]"), 2)
            ->
                maybe_report_missing_import(Info, unqualified("list"), !IO)
            ;
                io.write_string(".\n", !IO)
            )
        ),
        (
            ConsErrors = [_|_],
            list.foldl(report_cons_error(Context), ConsErrors, !IO)
        ;
            ConsErrors = []
        )
    ).

    % language_builtin(Name, Arity) is true iff Name/Arity is the name
    % of a builtin language construct that should be used as a goal,
    % not as an expression.
    %
:- pred language_builtin(string::in, arity::in) is semidet.

language_builtin("=", 2).
language_builtin("\\=", 2).
language_builtin(",", 2).
language_builtin(";", 2).
language_builtin("\\+", 1).
language_builtin("not", 1).
language_builtin("<=>", 2).
language_builtin("=>", 2).
language_builtin("<=", 2).
language_builtin("call", _).
language_builtin("impure", 1).
language_builtin("semipure", 1).
language_builtin("all", 2).
language_builtin("some", 2).

:- pred report_wrong_arity_constructor(sym_name::in, arity::in, list(int)::in,
    prog_context::in, io::di, io::uo) is det.

report_wrong_arity_constructor(Name, Arity, ActualArities, Context, !IO) :-
    io.write_string("  error: ", !IO),
    MaybePredOrFunc = no,
    report_error_num_args(MaybePredOrFunc, Arity, ActualArities, !IO),
    io.nl(!IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  in use of constructor `", !IO),
    prog_out.write_sym_name(Name, !IO),
    io.write_string("'.\n", !IO).

:- pred report_cons_error(prog_context::in, cons_error::in, io::di, io::uo)
    is det.

report_cons_error(Context, ConsError, !IO) :-
    (
        ConsError = foreign_type_constructor(TypeCtor, _),
        TypeCtor = type_ctor(TypeName, TypeArity),
        Pieces = [words("There are"),
            fixed("`:- pragma foreign_type'"),
            words("declarations for type"),
            sym_name_and_arity(TypeName / TypeArity),
            suffix(","),
            words("so it is treated as an abstract type"),
            words("in all predicates and functions"),
            words("which are not implemented"),
            words("for those foreign types.")],
        write_error_pieces_not_first_line(Context, 0, Pieces, !IO)
    ;
        ConsError = abstract_imported_type
        % For `abstract_imported_type' errors, the "undefined symbol"
        % error written by `report_error_undef_cons' is sufficient so
        % we do not print an additional error message here.
    ;
        ConsError = invalid_field_update(FieldName, FieldDefn,
            TVarSet, TVars),
        FieldDefn = hlds_ctor_field_defn(DefnContext, _, _, ConsId, _),
        Pieces1 = [words("Field"), sym_name(FieldName),
            words("cannot be updated because"),
            words("the existentially quantified type")],
        (
            TVars = [],
            unexpected(this_file, "report_invalid_field_update:"
                ++ " no type variables")
        ;
            TVars = [TVar],
            TVarsStr = mercury_var_to_string(TVar, TVarSet, no),
            Pieces2 = [words("variable"),
                words("`" ++ TVarsStr ++ "'"),
                words("occurs")]
        ;
            TVars = [_, _ | _],
            TVarsStr = mercury_vars_to_string(TVars, TVarSet, no),
            Pieces2 = [words("variables"),
                words("`" ++ TVarsStr ++ "'"),
                words("occur")]
        ),
        ConsIdStr = cons_id_to_string(ConsId),
        Pieces3 = [words("in the types of field"),
            sym_name(FieldName),
            words("and some other field"),
            words("in definition of constructor"),
            fixed("`" ++ ConsIdStr ++ "'.")],
        Pieces = Pieces1 ++ Pieces2 ++ Pieces3,
        write_error_pieces_not_first_line(DefnContext, 0, Pieces, !IO)
    ;
        ConsError = new_on_non_existential_type(TypeCtor),
        TypeCtor = type_ctor(TypeName, TypeArity),
        Pieces = [words("Invalid use of `new'"),
            words("on a constructor of type"),
            sym_name_and_arity(TypeName / TypeArity),
            words("which is not existentially typed.")],
        write_error_pieces_not_first_line(Context, 0, Pieces, !IO)
    ).

%-----------------------------------------------------------------------------%

report_ambiguity_error(Info, TypeAssign1, TypeAssign2, !IO) :-
    write_typecheck_info_context(Info, !IO),
    io.write_string(
        "  error: ambiguous overloading causes type ambiguity.\n", !IO),
    typecheck_info_get_varset(Info, VarSet),
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    map.keys(VarTypes1, Vars1),
    report_ambiguity_error_2(Vars1, VarSet, Info, TypeAssign1, TypeAssign2,
        no, Found, !IO),
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    ( Found = no ->
        report_warning_too_much_overloading(Info, !IO)
    ; VerboseErrors = yes ->
        io.write_strings([
"\tYou will need to add an explicit type qualification to resolve the\n",
"\ttype ambiguity.\n",
"\tThe way to add an explicit type qualification is to use ""with_type"".\n",
"\tFor details see the ""Explicit type qualification"" sub-section\n",
"\tof the ""Data-terms"" section of the ""Syntax"" chapter\n",
"\tof the Mercury language reference manual.\n"
        ], !IO)
    ;
        true
    ).

:- pred report_ambiguity_error_2(list(prog_var)::in, prog_varset::in,
    typecheck_info::in, type_assign::in, type_assign::in,
    bool::in, bool::out, io::di, io::uo) is det.

report_ambiguity_error_2([], _VarSet, _, _TypeAssign1, _TypeAssign2,
        !Found, !IO).
report_ambiguity_error_2([V | Vs], VarSet, Info, TypeAssign1,
        TypeAssign2, !Found, !IO) :-
    type_assign_get_var_types(TypeAssign1, VarTypes1),
    type_assign_get_var_types(TypeAssign2, VarTypes2),
    type_assign_get_type_bindings(TypeAssign1, TypeBindings1),
    type_assign_get_type_bindings(TypeAssign2, TypeBindings2),
    type_assign_get_head_type_params(TypeAssign1, HeadTypeParams1),
    type_assign_get_head_type_params(TypeAssign2, HeadTypeParams2),
    (
        map.search(VarTypes1, V, Type1),
        map.search(VarTypes2, V, Type2),
        apply_rec_subst_to_type(TypeBindings1, Type1, T1),
        apply_rec_subst_to_type(TypeBindings2, Type2, T2),
        \+ identical_types(T1, T2)
    ->
        typecheck_info_get_context(Info, Context),
        (
            !.Found = no,
            prog_out.write_context(Context, !IO),
            io.write_string("  Possible type assignments include:\n", !IO)
        ;
            !.Found = yes
        ),
        !:Found = yes,
        prog_out.write_context(Context, !IO),
        mercury_output_var(V, VarSet, no, !IO),
        io.write_string(": ", !IO),
        type_assign_get_typevarset(TypeAssign1, TVarSet1),
        output_type(T1, TVarSet1, HeadTypeParams1, !IO),
        io.write_string(" or ", !IO),
        type_assign_get_typevarset(TypeAssign2, TVarSet2),
        output_type(T2, TVarSet2, HeadTypeParams2, !IO),
        io.write_string("\n", !IO)
    ;
        true
    ),
    report_ambiguity_error_2(Vs, VarSet, Info, TypeAssign1, TypeAssign2,
        !Found, !IO).

%-----------------------------------------------------------------------------%

report_unsatisfiable_constraints(TypeAssignSet, !Info, !IO) :-
    typecheck_info_get_context(!.Info, Context),
    write_context_and_pred_id(!.Info, !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  unsatisfiable typeclass constraint(s):\n", !IO),

        % XXX this won't be very pretty when there are
        % XXX multiple type_assigns.
    io.write_list(TypeAssignSet, "\n", write_constraints(Context), !IO),
    typecheck_info_set_found_error(yes, !Info).

:- pred write_constraints(prog_context::in, type_assign::in, io::di, io::uo)
    is det.

write_constraints(Context, TypeAssign, !IO) :-
    type_assign_get_typeclass_constraints(TypeAssign, Constraints),
    UnprovenConstraints = Constraints ^ unproven,
    retrieve_prog_constraint_list(UnprovenConstraints,
        UnprovenProgConstraints0),

    type_assign_get_typevarset(TypeAssign, VarSet),
    type_assign_get_type_bindings(TypeAssign, Bindings),
    apply_rec_subst_to_prog_constraint_list(Bindings,
        UnprovenProgConstraints0, UnprovenProgConstraints1),
    list.sort_and_remove_dups(UnprovenProgConstraints1,
        UnprovenProgConstraints),
    prog_out.write_context(Context, !IO),
    io.write_string("  `", !IO),
    AppendVarnums = no,
    io.write_list(UnprovenProgConstraints, "', `",
        mercury_output_constraint(VarSet, AppendVarnums), !IO),
    io.write_string("'.\n", !IO).

%-----------------------------------------------------------------------------%

report_missing_tvar_in_foreign_code(Info, VarName, !IO) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_predid(Info, PredId),
    Pieces = [words("The foreign language code for") |
        describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)] ++
        [words("should define the variable"),
        fixed(add_quotes(VarName)), suffix(".")],
    write_error_pieces(Context, 0, Pieces, !IO),
    io.set_exit_status(1, !IO).

%-----------------------------------------------------------------------------%

:- pred write_types_of_vars(list(prog_var)::in, prog_varset::in,
    prog_context::in, typecheck_info::in, type_assign_set::in,
    io::di, io::uo) is det.

write_types_of_vars([], _, _, _, _, !IO) :-
    io.write_string(".\n", !IO).
write_types_of_vars([Var | Vars], VarSet, Context, Info, TypeAssignSet, !IO) :-
    io.write_string(",\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("  ", !IO),
    write_argument_name(VarSet, Var, !IO),
    write_type_of_var(Info, Context, TypeAssignSet, Var, !IO),
    write_types_of_vars(Vars, VarSet, Context, Info, TypeAssignSet, !IO).

:- pred write_argument_name(prog_varset::in, prog_var::in, io::di, io::uo)
    is det.

write_argument_name(VarSet, Var, !IO) :-
    ( varset.search_name(VarSet, Var, _) ->
        io.write_string("variable `", !IO),
        mercury_output_var(Var, VarSet, no, !IO),
        io.write_string("'", !IO)
    ;
        io.write_string("argument", !IO)
    ).

:- pred write_functor_name(cons_id::in, int::in, io::di, io::uo) is det.

write_functor_name(Functor, Arity, !IO) :-
    strip_builtin_qualifier_from_cons_id(Functor, StrippedFunctor),
    ( Arity = 0 ->
        io.write_string("constant `", !IO),
        ( Functor = cons(Name, _) ->
            prog_out.write_sym_name(Name, !IO)
        ;
            hlds_out.write_cons_id(StrippedFunctor, !IO)
        ),
        io.write_string("'", !IO)
    ; Functor = cons(unqualified(""), _) ->
        io.write_string("higher-order term (with arity ", !IO),
        io.write_int(Arity - 1, !IO),
        io.write_string(")", !IO)
    ;
        io.write_string("functor `", !IO),
        hlds_out.write_cons_id(StrippedFunctor, !IO),
        io.write_string("'", !IO)
    ).

:- pred write_type_of_var(typecheck_info::in, prog_context::in,
    type_assign_set::in, prog_var::in, io::di, io::uo) is det.

write_type_of_var(_Info, Context, TypeAssignSet, Var, !IO) :-
    get_type_stuff(TypeAssignSet, Var, TypeStuffList),
    TypeStrs0 = list.map(typestuff_to_typestr, TypeStuffList),
    list.sort_and_remove_dups(TypeStrs0, TypeStrs),
    ( TypeStrs = [TypeStr] ->
        io.write_string(" has type `", !IO),
        io.write_string(TypeStr, !IO),
        io.write_string("'", !IO)
    ;
        io.write_string(" has overloaded type {\n", !IO),
        write_types_list(Context, TypeStrs, !IO),
        prog_out.write_context(Context, !IO),
        io.write_string("  }", !IO)
    ).

:- pred write_type_of_functor(cons_id::in, int::in, prog_context::in,
    list(cons_type_info)::in, io::di, io::uo) is det.

write_type_of_functor(Functor, Arity, Context, ConsDefnList, !IO) :-
    ( ConsDefnList = [SingleDefn] ->
        io.write_string(" has type ", !IO),
        ( Arity \= 0 ->
            io.write_string("\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string("  `", !IO)
        ;
            io.write_string("`", !IO)
        ),
        write_cons_type(SingleDefn, Functor, Context, !IO),
        io.write_string("'", !IO)
    ;
        io.write_string(" has overloaded type\n", !IO),
        prog_out.write_context(Context, !IO),
        io.write_string("  { ", !IO),
        write_cons_type_list(ConsDefnList, Functor, Arity, Context, !IO),
        io.write_string(" }", !IO)
    ).

:- pred write_cons_type(cons_type_info::in, cons_id::in, prog_context::in,
    io::di, io::uo) is det.

    % XXX Should we mention the context here?
write_cons_type(ConsInfo, Functor, _, !IO) :-
    ConsInfo = cons_type_info(TVarSet, ExistQVars, ConsType, ArgTypes, _, _),
    (
        ArgTypes = [_ | _],
        (
            Functor = cons(SymName, _Arity)
        ->
            Type = defined(SymName, ArgTypes, star),
            output_type(Type, TVarSet, ExistQVars, !IO)
        ;
            unexpected(this_file, "write_cons_type: invalid cons_id")
        ),
        io.write_string(": ", !IO)
    ;
        ArgTypes = []
    ),
    output_type(ConsType, TVarSet, ExistQVars, !IO).

:- pred write_cons_type_list(list(cons_type_info)::in, cons_id::in, int::in,
    prog_context::in, io::di, io::uo) is det.

write_cons_type_list([], _, _, _, !IO).
write_cons_type_list([ConsDefn | ConsDefns], Functor, Arity, Context, !IO) :-
    write_cons_type(ConsDefn, Functor, Context, !IO),
    (
        ConsDefns = []
    ;
        ConsDefns = [_ | _],
        ( Arity = 0 ->
            io.write_string(", ", !IO)
        ;
            io.write_string(",\n", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string("  ", !IO)
        ),
        write_cons_type_list(ConsDefns, Functor, Arity, Context, !IO)
    ).

:- pred write_type_assign_set_msg(type_assign_set::in, prog_varset::in,
    io::di, io::uo) is det.

write_type_assign_set_msg(TypeAssignSet, VarSet, !IO) :-
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        ( TypeAssignSet = [_] ->
            io.write_string(
                "\tThe partial type assignment was:\n", !IO)
        ;
            io.write_string("\tThe possible partial type " ++
                "assignments were:\n", !IO)
        ),
        write_type_assign_set(TypeAssignSet, VarSet, !IO)
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO)
    ).

:- pred write_args_type_assign_set_msg(args_type_assign_set::in,
    prog_varset::in, io::di, io::uo) is det.

write_args_type_assign_set_msg(ArgTypeAssignSet, VarSet, !IO) :-
    globals.io_lookup_bool_option(verbose_errors, VerboseErrors, !IO),
    (
        VerboseErrors = yes,
        ( ArgTypeAssignSet = [_] ->
            io.write_string(
                "\tThe partial type assignment was:\n", !IO)
        ;
            io.write_string("\tThe possible partial type " ++
                "assignments were:\n", !IO)
        ),
        write_args_type_assign_set(ArgTypeAssignSet, VarSet, !IO)
    ;
        VerboseErrors = no,
        globals.io_set_extra_error_info(yes, !IO)
    ).

:- pred output_type(mer_type::in, tvarset::in, head_type_params::in,
    io::di, io::uo) is det.

output_type(Type0, TVarSet, HeadTypeParams, !IO) :-
    strip_builtin_qualifiers_from_type(Type0, Type),
    unparse_type(Type, Term0),
    list.map(term.coerce_var, HeadTypeParams, ExistQVars),
    maybe_add_existential_quantifier(ExistQVars, Term0, Term),
    varset.coerce(TVarSet, VarSet),
    mercury_output_term(Term, VarSet, no, !IO).

:- pred write_types_list(prog_context::in, list(string)::in,
    io::di, io::uo) is det.

write_types_list(_Context, [], !IO).
write_types_list(Context, [Type | Types], !IO) :-
    prog_out.write_context(Context, !IO),
    io.write_string("    ", !IO),
    io.write_string(Type, !IO),
    (
        Types = [],
        io.write_string("\n", !IO)
    ;
        Types = [_ | _],
        io.write_string(",\n", !IO),
        write_types_list(Context, Types, !IO)
    ).

:- pred write_type_stuff(type_stuff::in, io::di, io::uo) is det.

write_type_stuff(type_stuff(Type, TVarSet, TypeBinding, HeadTypeParams),
        !IO) :-
    write_type_b(Type, TVarSet, TypeBinding, HeadTypeParams, !IO).

:- pred write_var_type_stuff_list(prog_context::in, list(type_stuff)::in,
    mer_type::in, io::di, io::uo) is det.

write_var_type_stuff_list(Context, TypeStuffs, Type, !IO) :-
    io.write_list(TypeStuffs, ",\n", write_var_type_stuff(Context, Type),
        !IO).

:- pred write_var_type_stuff(prog_context::in, mer_type::in, type_stuff::in,
    io::di, io::uo) is det.

write_var_type_stuff(Context, Type, VarTypeStuff, !IO) :-
    VarTypeStuff = type_stuff(VarType, TVarSet, TypeBinding, HeadTypeParams),
    prog_out.write_context(Context, !IO),
    io.write_string("    (inferred) ", !IO),
    write_type_b(VarType, TVarSet, TypeBinding, HeadTypeParams, !IO),
    io.write_string(",\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("    (expected) ", !IO),
    write_type_b(Type, TVarSet, TypeBinding, HeadTypeParams, !IO).

:- pred write_type_b(mer_type::in, tvarset::in, tsubst::in,
    head_type_params::in, io::di, io::uo) is det.

write_type_b(Type0, TypeVarSet, TypeBindings, HeadTypeParams, !IO) :-
    apply_rec_subst_to_type(TypeBindings, Type0, Type),
    output_type(Type, TypeVarSet, HeadTypeParams, !IO).

:- pred write_arg_type_stuff_list(prog_context::in, list(arg_type_stuff)::in,
    io::di, io::uo) is det.

write_arg_type_stuff_list(Context, TypeStuffs, !IO) :-
    io.write_list(TypeStuffs, ",\n", write_arg_type_stuff(Context), !IO).

:- pred write_arg_type_stuff(prog_context::in, arg_type_stuff::in,
    io::di, io::uo) is det.

write_arg_type_stuff(Context, ArgTypeStuff, !IO) :-
    ArgTypeStuff = arg_type_stuff(Type, VarType, TVarSet, HeadTypeParams),
    prog_out.write_context(Context, !IO),
    io.write_string("    (inferred) ", !IO),
    output_type(VarType, TVarSet, HeadTypeParams, !IO),
    io.write_string(",\n", !IO),
    prog_out.write_context(Context, !IO),
    io.write_string("    (expected) ", !IO),
    output_type(Type, TVarSet, HeadTypeParams, !IO).

%-----------------------------------------------------------------------------%

:- pred maybe_report_missing_import(typecheck_info::in, module_specifier::in,
    io::di, io::uo) is det.

maybe_report_missing_import(Info, ModuleQualifier, !IO) :-
    typecheck_info_get_context(Info, Context),
    %
    % First check if this module wasn't imported.
    %
    typecheck_info_get_module_info(Info, ModuleInfo),
    (
        % If the module qualifier couldn't match any of the visible
        % modules, then we report that the module has not been
        % imported.
        \+ (
            visible_module(VisibleModule, ModuleInfo),
            match_sym_name(ModuleQualifier, VisibleModule)
        )
    ->
        io.write_string("\n", !IO),
        error_util.write_error_pieces(Context, 2,
            [words("(the module "),
            fixed(error_util.describe_sym_name(ModuleQualifier)),
            words("has not been imported).")], !IO)
    ;
        % The module qualifier matches one or more of the
        % visible modules.  But maybe the user forgot to
        % import the parent module(s) of that module...
        solutions.solutions(get_unimported_parent(ModuleQualifier,
            ModuleInfo), UnimportedParents),
        UnimportedParents \= []
    ->
        io.write_string("\n", !IO),
        report_unimported_parents(Context, UnimportedParents, !IO)
    ;
        io.write_string(".\n", !IO)
    ).

    % Nondeterministically return all the possible parent modules which could
    % be parent modules of the given module qualifier, and which are not
    % imported.
    %
:- pred get_unimported_parent(module_name::in, module_info::in,
    module_name::out) is nondet.

get_unimported_parent(ModuleQualifier, ModuleInfo, UnimportedParent) :-
    visible_module(ModuleName, ModuleInfo),
    match_sym_name(ModuleQualifier, ModuleName),
    ParentModules = get_ancestors(ModuleName),
    list.member(UnimportedParent, ParentModules),
    \+ visible_module(UnimportedParent, ModuleInfo).

:- pred report_unimported_parents(prog_context::in, list(module_name)::in,
    io::di, io::uo) is det.

report_unimported_parents(Context, UnimportedParents, !IO) :-
    UnimportedParentDescs = list.map(describe_sym_name, UnimportedParents),
    AllUnimportedParents = list_to_pieces(UnimportedParentDescs),
    error_util.write_error_pieces(Context, 2,
        ( AllUnimportedParents = [_] ->
            [words("(the possible parent module ")]
            ++ AllUnimportedParents
            ++ [words("has not been imported).")]
        ;
            [words("(the possible parent modules ")]
            ++ AllUnimportedParents
            ++ [words("have not been imported).")]
        ), !IO).

%-----------------------------------------------------------------------------%

:- pred write_call_context(prog_context::in, pred_markers::in,
    call_id::in, int::in, unify_context::in, io::di, io::uo) is det.

write_call_context(Context, PredMarkers, CallId, ArgNum, UnifyContext) -->
    ( { ArgNum = 0 } ->
        hlds_out.write_unify_context(UnifyContext, Context)
    ;
        prog_out.write_context(Context),
        io.write_string("  in "),
        hlds_out.write_call_arg_id(CallId, ArgNum, PredMarkers),
        io.write_string(":\n")
    ).

:- pred write_typecheck_info_context(typecheck_info::in, io::di, io::uo)
    is det.

write_typecheck_info_context(Info, !IO) :-
    write_context_and_pred_id(Info, !IO),
    typecheck_info_get_context(Info, Context),
    prog_out.write_context(Context, !IO).

:- pred write_context_and_pred_id(typecheck_info::in, io::di, io::uo) is det.

write_context_and_pred_id(Info, !IO) :-
    typecheck_info_get_module_info(Info, ModuleInfo),
    typecheck_info_get_context(Info, Context),
    typecheck_info_get_predid(Info, PredId),
    prog_out.write_context(Context, !IO),
    io.write_string("In clause for ", !IO),
    hlds_out.write_pred_id(ModuleInfo, PredId, !IO),
    io.write_string(":\n", !IO).

    % This is intended to supercede the above predicate - It performs the
    % same action, but instead of just writing to the output straight away
    % the resultant string is passed back to the caller to deal with.
    % This allows `nicer' handling of error messages, since this string
    % can be used by the predicates in error_util.m
    %
    % The string generated by this predicate is of the form:
    % In clause for module.pred/N:
    %
:- pred make_pred_id_preamble(typecheck_info::in, string::out) is det.

make_pred_id_preamble(Info, Preamble) :-
    typecheck_info_get_module_info(Info, Module),
    typecheck_info_get_predid(Info, PredId),
    PredPieces = describe_one_pred_name(Module, should_not_module_qualify,
        PredId),
    PredName = error_pieces_to_string(PredPieces),
    Preamble = "In clause for " ++ PredName ++ ":".

    % Check whether two types are identical ignoring their
    % prog_contexts, i.e. whether they can be unified without
    % binding any type parameters.
    %
:- pred identical_types(mer_type::in, mer_type::in) is semidet.

identical_types(Type1, Type2) :-
    map.init(TypeSubst0),
    type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
    TypeSubst = TypeSubst0.

%-----------------------------------------------------------------------------%

    % Given a type assignment set and a variable, return the list of possible
    % different types for the variable.
    %
:- type type_stuff
    --->    type_stuff(
                mer_type,
                tvarset,
                tsubst,
                head_type_params
            ).

:- pred get_type_stuff(type_assign_set::in, prog_var::in,
    list(type_stuff)::out) is det.

get_type_stuff([], _Var, []).
get_type_stuff([TypeAssign | TypeAssigns], Var, TypeStuffs) :-
    get_type_stuff(TypeAssigns, Var, TailTypeStuffs),
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( map.search(VarTypes, Var, Type0) ->
        Type = Type0
    ;
        % This shouldn't happen - how can a variable which has
        % not yet been assigned a type variable fail to have
        % the correct type?
        Type = defined(unqualified("<any>"), [], star)
    ),
    TypeStuff = type_stuff(Type, TVarSet, TypeBindings, HeadTypeParams),
    ( list.member(TypeStuff, TailTypeStuffs) ->
        TypeStuffs = TailTypeStuffs
    ;
        TypeStuffs = [TypeStuff | TailTypeStuffs]
    ).

:- func typestuff_to_typestr(type_stuff) = string.

typestuff_to_typestr(TypeStuff) = TypeStr :-
    TypeStuff = type_stuff(Type0, TypeVarSet, TypeBindings, HeadTypeParams),
    apply_rec_subst_to_type(TypeBindings, Type0, Type1),
    strip_builtin_qualifiers_from_type(Type1, Type),
    unparse_type(Type, Term0),
    list.map(term.coerce_var, HeadTypeParams, ExistQVars),
    maybe_add_existential_quantifier(ExistQVars, Term0, Term),
    varset.coerce(TypeVarSet, VarSet),
    TypeStr = mercury_term_to_string(Term, VarSet, no).

    % Given an arg type assignment set and a variable id, return the list of
    % possible different types for the argument and the variable.
    %
:- type arg_type_stuff
    --->    arg_type_stuff(
                mer_type,
                mer_type,
                tvarset,
                head_type_params
            ).

:- pred get_arg_type_stuff(args_type_assign_set::in, prog_var::in,
    list(arg_type_stuff)::out) is det.

get_arg_type_stuff([], _Var, []).
get_arg_type_stuff([ArgTypeAssign | ArgTypeAssigns], Var, ArgTypeStuffs) :-
    ArgTypeAssign = args(TypeAssign, ArgTypes, _),
    get_arg_type_stuff(ArgTypeAssigns, Var, TailArgTypeStuffs),
    type_assign_get_head_type_params(TypeAssign, HeadTypeParams),
    type_assign_get_type_bindings(TypeAssign, TypeBindings),
    type_assign_get_typevarset(TypeAssign, TVarSet),
    type_assign_get_var_types(TypeAssign, VarTypes),
    ( map.search(VarTypes, Var, VarType0) ->
        VarType = VarType0
    ;
        % This shouldn't happen - how can a variable which has
        % not yet been assigned a type variable fail to have
        % the correct type?
        VarType = defined(unqualified("<any>"), [], star)
    ),
    list.index0_det(ArgTypes, 0, ArgType),
    apply_rec_subst_to_type(TypeBindings, ArgType, ArgType2),
    apply_rec_subst_to_type(TypeBindings, VarType, VarType2),
    ArgTypeStuff = arg_type_stuff(ArgType2, VarType2, TVarSet,
        HeadTypeParams),
    ( list.member(ArgTypeStuff, TailArgTypeStuffs) ->
        ArgTypeStuffs = TailArgTypeStuffs
    ;
        ArgTypeStuffs = [ArgTypeStuff | TailArgTypeStuffs]
    ).

    % Check if any of the variables in the term are existentially
    % quantified (occur in the first argument), and if so, add the
    % appropriate quantification to the term.  Otherwise return the term
    % unchanged.
    %
:- pred maybe_add_existential_quantifier(list(var)::in, term::in, term::out)
    is det.

maybe_add_existential_quantifier(HeadTypeParams, !Term) :-
    term.vars(!.Term, Vars),
    ExistQVars = set.to_sorted_list(set.intersect(
        set.list_to_set(HeadTypeParams), set.list_to_set(Vars))),
    (
        ExistQVars = []
    ;
        ExistQVars = [_ | _],
        QTerm = make_list_term(ExistQVars),
        !:Term = term.functor(term.atom("some"), [QTerm, !.Term],
            term.context_init)
    ).

:- func make_list_term(list(var)) = term.

make_list_term([]) = term.functor(term.atom("[]"), [], term.context_init).
make_list_term([Var | Vars]) = term.functor(term.atom("[|]"),
    [term.variable(Var), make_list_term(Vars)], term.context_init).

%-----------------------------------------------------------------------------%

write_inference_messages([], _, !IO).
write_inference_messages([PredId | PredIds], ModuleInfo, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    (
        check_marker(Markers, marker_infer_type),
        module_info_predids(ModuleInfo, ValidPredIds),
        list.member(PredId, ValidPredIds),
        \+ pred_info_get_goal_type(PredInfo, goal_type_promise(_))
    ->
        write_inference_message(PredInfo, !IO)
    ;
        true
    ),
    write_inference_messages(PredIds, ModuleInfo, !IO).

    % Write out the inferred `pred' or `func' declaration
    % for a single predicate.
    %
:- pred write_inference_message(pred_info::in, io::di, io::uo) is det.

write_inference_message(PredInfo, !IO) :-
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Name = unqualified(PredName),
    pred_info_context(PredInfo, Context),
    pred_info_get_arg_types(PredInfo, VarSet, ExistQVars, Types0),
    strip_builtin_qualifiers_from_type_list(Types0, Types),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_purity(PredInfo, Purity),
    MaybeDet = no,
    prog_out.write_context(Context, !IO),
    io.write_string("Inferred ", !IO),
    AppendVarNums = no,
    (
        PredOrFunc = predicate,
        mercury_output_pred_type(VarSet, ExistQVars, Name, Types,
            MaybeDet, Purity, ClassContext, Context, AppendVarNums, !IO)
    ;
        PredOrFunc = function,
        pred_args_to_func_args(Types, ArgTypes, RetType),
        mercury_output_func_type(VarSet, ExistQVars, Name, ArgTypes, RetType,
            MaybeDet, Purity, ClassContext, Context, AppendVarNums, !IO)
    ).

checkpoint(Msg, !Info, !IO) :-
    typecheck_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_types, DoCheckPoint),
    (
        DoCheckPoint = yes,
        checkpoint_2(Msg, !.Info, !IO)
    ;
        DoCheckPoint = no
    ).

:- pred checkpoint_2(string::in, typecheck_info::in, io::di, io::uo) is det.

checkpoint_2(Msg, T0, !IO) :-
    io.write_string("At ", !IO),
    io.write_string(Msg, !IO),
    io.write_string(": ", !IO),
    globals.io_lookup_bool_option(detailed_statistics, Statistics, !IO),
    maybe_report_stats(Statistics, !IO),
    io.write_string("\n", !IO),
    typecheck_info_get_type_assign_set(T0, TypeAssignSet),
    (
        Statistics = yes,
        TypeAssignSet = [TypeAssign | _]
    ->
        type_assign_get_var_types(TypeAssign, VarTypes),
        checkpoint_tree_stats("\t`var -> type' map", VarTypes, !IO),
        type_assign_get_type_bindings(TypeAssign, TypeBindings),
        checkpoint_tree_stats("\t`type var -> type' map", TypeBindings, !IO)
    ;
        true
    ),
    typecheck_info_get_varset(T0, VarSet),
    write_type_assign_set(TypeAssignSet, VarSet, !IO).

:- pred checkpoint_tree_stats(string::in, map(_K, _V)::in, io::di, io::uo)
    is det.

checkpoint_tree_stats(Description, Tree, !IO) :-
    map.count(Tree, Count),
    io.write_string(Description, !IO),
    io.write_string(": count = ", !IO),
    io.write_int(Count, !IO),
    io.write_string("\n", !IO).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "typecheck_errors.m".

%-----------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_errors.
%-----------------------------------------------------------------------------%
