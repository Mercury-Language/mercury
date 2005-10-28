%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1996-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: prog_io_util.m.
% Main author: fjh.
%
% This module defines the types used by prog_io and its subcontractors
% to return the results of parsing, and some utility predicates needed
% by several of prog_io's submodules.
%
% Most parsing predicates must check for errors. They return either the
% item(s) they were looking for, or an error indication.
%
% Most of the parsing predicates return a `maybe1(T)' or a `maybe2(T1, T2)',
% which will either be the `ok(ParseTree)' (or `ok(ParseTree1, ParseTree2)'),
% if the parse is successful, or `error(Message, Term)' if it is not.
% The `Term' there should be the term which is syntactically incorrect.

:- module parse_tree__prog_io_util.

:- interface.

:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module map.
:- import_module std_util.
:- import_module term.

:- type maybe2(T1, T2)
    --->    error(string, term)
    ;       ok(T1, T2).

:- type maybe3(T1, T2, T3)
    --->    error(string, term)
    ;       ok(T1, T2, T3).

:- type maybe1(T)   ==  maybe1(T, generic).
:- type maybe1(T, U)
    --->    error(string, term(U))
    ;       ok(T).

:- type maybe_functor    ==  maybe_functor(generic).
:- type maybe_functor(T) ==  maybe2(sym_name, list(term(T))).

    % ok(SymName, Args - MaybeFuncRetArg) ; error(Msg, Term).
:- type maybe_pred_or_func(T) == maybe2(sym_name, pair(list(T), maybe(T))).

:- type maybe_item_and_context ==  maybe2(item, prog_context).

:- type var2tvar    ==  map(var, tvar).

:- type var2pvar    ==  map(var, prog_var).

:- type parser(T) == pred(term, maybe1(T)).
:- mode parser == (pred(in, out) is det).

:- pred add_context(maybe1(item)::in, prog_context::in,
    maybe_item_and_context::out) is det.

% Various predicates to parse small bits of syntax.
% These predicates simply fail if they encounter a syntax error.

:- pred parse_list_of_vars(term(T)::in, list(var(T))::out) is semidet.

    % Parse a list of quantified variables, splitting it into
    % state variables and ordinary logic variables, respectively.
    %
:- pred parse_quantifier_vars(term(T)::in, list(var(T))::out,
    list(var(T))::out) is semidet.

    % Parse a list of quantified variables.
    %
:- pred parse_vars(term(T)::in, list(var(T))::out) is semidet.

    % parse_vars_and_state_vars(Term, OrdinaryVars, DotStateVars,
    %   ColonStateVars):
    %
    % Similar to parse_vars, but also allow state variables to appear
    % in the list. The outputs separate the parsed variables into ordinary
    % variables, state variables listed as !.X, and state variables
    % listed as !:X.
    %
:- pred parse_vars_and_state_vars(term(T)::in, list(var(T))::out,
    list(var(T))::out, list(var(T))::out) is semidet.

:- pred parse_name_and_arity(module_name::in, term(_T)::in,
    sym_name::out, arity::out) is semidet.

:- pred parse_name_and_arity(term(_T)::in, sym_name::out, arity::out)
    is semidet.

:- pred parse_pred_or_func_name_and_arity(module_name::in,
    term(_T)::in, pred_or_func::out, sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_name_and_arity(term(_T)::in, pred_or_func::out,
    sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_and_args(maybe(module_name)::in, term(_T)::in,
    term(_T)::in, string::in, maybe_pred_or_func(term(_T))::out) is det.

:- pred parse_pred_or_func_and_args(term(_T)::in, pred_or_func::out,
    sym_name::out, list(term(_T))::out) is semidet.

:- pred parse_type(term::in, maybe1(mer_type)::out) is det.

:- pred parse_types(list(term)::in, maybe1(list(mer_type))::out) is det.

:- pred unparse_type(mer_type::in, term::out) is det.

:- pred parse_purity_annotation(term(T)::in, purity::out, term(T)::out) is det.

:- type allow_constrained_inst_var
    --->    allow_constrained_inst_var
    ;       no_allow_constrained_inst_var.

:- pred convert_mode_list(allow_constrained_inst_var::in, list(term)::in,
    list(mer_mode)::out) is semidet.

:- pred convert_mode(allow_constrained_inst_var::in, term::in, mer_mode::out)
    is semidet.

:- pred convert_inst_list(allow_constrained_inst_var::in, list(term)::in,
    list(mer_inst)::out) is semidet.

:- pred convert_inst(allow_constrained_inst_var::in, term::in, mer_inst::out)
    is semidet.

:- pred standard_det(string::in, determinism::out) is semidet.

    % Convert a "disjunction" (bunch of terms separated by ';'s) to a list.
    %
:- pred disjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % Convert a "conjunction" (bunch of terms separated by ','s) to a list.
    %
:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % list_to_conjunction(Context, First, Rest, Term):
    % Convert a list to a "conjunction" (bunch of terms separated by ','s).
    %
:- pred list_to_conjunction(prog_context::in, term(T)::in, list(term(T))::in,
    term(T)::out) is det.

    % Convert a "sum" (bunch of terms separated by '+' operators) to a list.
    %
:- pred sum_to_list(term(T)::in, list(term(T))::out) is det.

    % Parse a comma-separated list (misleading described as a "conjunction")
    % of things.
    %
:- pred parse_list(parser(T)::parser, term::in, maybe1(list(T))::out) is det.

:- pred map_parser(parser(T)::parser, list(term)::in, maybe1(list(T))::out)
    is det.

:- pred list_term_to_term_list(term::in, list(term)::out) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.prog_io.
:- import_module parse_tree.prog_io_goal.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module term.

add_context(error(M, T), _, error(M, T)).
add_context(ok(Item), Context, ok(Item, Context)).

parse_name_and_arity(ModuleName, PredAndArityTerm, SymName, Arity) :-
    PredAndArityTerm = term__functor(term__atom("/"),
        [PredNameTerm, ArityTerm], _),
    parse_implicitly_qualified_term(ModuleName,
        PredNameTerm, PredNameTerm, "", ok(SymName, [])),
    ArityTerm = term__functor(term__integer(Arity), [], _).

parse_name_and_arity(PredAndArityTerm, SymName, Arity) :-
    parse_name_and_arity(unqualified(""),
        PredAndArityTerm, SymName, Arity).

parse_pred_or_func_name_and_arity(ModuleName, PorFPredAndArityTerm,
        PredOrFunc, SymName, Arity) :-
    PorFPredAndArityTerm = term__functor(term__atom(PredOrFuncStr), Args, _),
    ( PredOrFuncStr = "pred", PredOrFunc = predicate
    ; PredOrFuncStr = "func", PredOrFunc = function
    ),
    Args = [Arg],
    parse_name_and_arity(ModuleName, Arg, SymName, Arity).

parse_pred_or_func_name_and_arity(PorFPredAndArityTerm,
        PredOrFunc, SymName, Arity) :-
    parse_pred_or_func_name_and_arity(unqualified(""),
        PorFPredAndArityTerm, PredOrFunc, SymName, Arity).

parse_pred_or_func_and_args(Term, PredOrFunc, SymName, ArgTerms) :-
    parse_pred_or_func_and_args(no, Term, Term, "",
        ok(SymName, ArgTerms0 - MaybeRetTerm)),
    (
        MaybeRetTerm = yes(RetTerm),
        PredOrFunc = function,
        list__append(ArgTerms0, [RetTerm], ArgTerms)
    ;
        MaybeRetTerm = no,
        PredOrFunc = predicate,
        ArgTerms = ArgTerms0
    ).

parse_pred_or_func_and_args(MaybeModuleName, PredAndArgsTerm, ErrorTerm,
        Msg, PredAndArgsResult) :-
    (
        PredAndArgsTerm = term__functor(term__atom("="),
            [FuncAndArgsTerm, FuncResultTerm], _)
    ->
        FunctorTerm = FuncAndArgsTerm,
        MaybeFuncResult = yes(FuncResultTerm)
    ;
        FunctorTerm = PredAndArgsTerm,
        MaybeFuncResult = no
    ),
    (
        MaybeModuleName = yes(ModuleName),
        parse_implicitly_qualified_term(ModuleName, FunctorTerm,
            ErrorTerm, Msg, Result)
    ;
        MaybeModuleName = no,
        parse_qualified_term(FunctorTerm, ErrorTerm, Msg, Result)
    ),
    (
        Result = ok(SymName, Args),
        PredAndArgsResult = ok(SymName, Args - MaybeFuncResult)
    ;
        Result = error(ErrorMsg, Term),
        PredAndArgsResult = error(ErrorMsg, Term)
    ).

parse_list_of_vars(term__functor(term__atom("[]"), [], _), []).
parse_list_of_vars(term__functor(term__atom("[|]"), [Head, Tail], _),
        [V | Vs]) :-
    Head = term__variable(V),
    parse_list_of_vars(Tail, Vs).

    % XXX kind inference: We currently give all types kind `star'.
    % This will be different when we have a kind system.
    %
parse_type(Term, Result) :-
    (
        Term = term__variable(Var0)
    ->
        term__coerce_var(Var0, Var),
        Result = ok(variable(Var, star))
    ;
        parse_builtin_type(Term, BuiltinType)
    ->
        Result = ok(builtin(BuiltinType))
    ;
        parse_higher_order_type(Term, HOArgs, MaybeRet, Purity, EvalMethod)
    ->
        Result = ok(higher_order(HOArgs, MaybeRet, Purity, EvalMethod))
    ;
        Term = term__functor(term__atom("{}"), Args, _)
    ->
        parse_types(Args, ArgsResult),
        (
            ArgsResult = ok(ArgTypes),
            Result = ok(tuple(ArgTypes, star))
        ;
            ArgsResult = error(Msg, ErrorTerm),
            Result = error(Msg, ErrorTerm)
        )
    ;
        % We don't support apply/N types yet, so we just detect them
        % and report an error message.
        Term = term__functor(term__atom(""), _, _)
    ->
        Result = error("ill-formed type", Term)
    ;
        % We don't support kind annotations yet, and we don't report
        % an error either. Perhaps we should?
        parse_qualified_term(Term, Term, "type", NameResult),
        (
            NameResult = ok(SymName, ArgTerms),
            parse_types(ArgTerms, ArgsResult),
            (
                ArgsResult = ok(ArgTypes),
                Result = ok(defined(SymName, ArgTypes, star))
            ;
                ArgsResult = error(Msg, ErrorTerm),
                Result = error(Msg, ErrorTerm)
            )
        ;
            NameResult = error(Msg, ErrorTerm),
            Result = error(Msg, ErrorTerm)
        )
    ).

parse_types(Terms, Result) :-
    parse_types_2(Terms, [], Result).

:- pred parse_types_2(list(term)::in, list(mer_type)::in,
    maybe1(list(mer_type))::out) is det.

parse_types_2([], RevTypes, ok(Types)) :-
    list__reverse(RevTypes, Types).
parse_types_2([Term | Terms], RevTypes, Result) :-
    parse_type(Term, Result0),
    (
        Result0 = ok(Type),
        parse_types_2(Terms, [Type | RevTypes], Result)
    ;
        Result0 = error(Msg, ErrorTerm),
        Result = error(Msg, ErrorTerm)
    ).

:- pred parse_builtin_type(term::in, builtin_type::out) is semidet.

parse_builtin_type(Term, BuiltinType) :-
    Term = term__functor(term__atom(Name), [], _),
    builtin_type_to_string(BuiltinType, Name).

    % If there are any ill-formed types in the argument then we just fail.
    % The predicate parse_type will then try to parse the term as an ordinary
    % defined type and will produce the required error message.
    %
:- pred parse_higher_order_type(term::in, list(mer_type)::out,
    maybe(mer_type)::out, purity::out, lambda_eval_method::out) is semidet.

parse_higher_order_type(Term0, ArgTypes, MaybeRet, Purity, EvalMethod) :-
    parse_purity_annotation(Term0, Purity, Term1),
    ( Term1 = term__functor(term__atom("="), [FuncAndArgs0, Ret], _) ->
        parse_lambda_eval_method(FuncAndArgs0, EvalMethod, FuncAndArgs),
        FuncAndArgs = term__functor(term__atom("func"), Args, _),
        parse_type(Ret, ok(RetType)),
        MaybeRet = yes(RetType)
    ;
        parse_lambda_eval_method(Term1, EvalMethod, PredTerm),
        PredTerm = term__functor(term__atom("pred"), Args, _),
        MaybeRet = no
    ),
    parse_types(Args, ok(ArgTypes)).

parse_purity_annotation(Term0, Purity, Term) :-
    (
        Term0 = term__functor(term__atom(PurityName), [Term1], _),
        purity_name(Purity0, PurityName)
    ->
        Purity = Purity0,
        Term = Term1
    ;
        Purity = purity_pure,
        Term = Term0
    ).

unparse_type(variable(TVar, _), term__variable(Var)) :-
    Var = term__coerce_var(TVar).
unparse_type(defined(SymName, Args, _), Term) :-
    unparse_type_list(Args, ArgTerms),
    unparse_qualified_term(SymName, ArgTerms, Term).
unparse_type(builtin(BuiltinType), Term) :-
    Context = term__context_init,
    builtin_type_to_string(BuiltinType, Name),
    Term = term__functor(term__atom(Name), [], Context).
unparse_type(higher_order(Args, MaybeRet, Purity, EvalMethod), Term) :-
    Context = term__context_init,
    unparse_type_list(Args, ArgTerms),
    (
        MaybeRet = yes(Ret),
        Term0 = term__functor(term__atom("func"), ArgTerms, Context),
        maybe_add_lambda_eval_method(EvalMethod, Term0, Term1),
        unparse_type(Ret, RetTerm),
        Term2 = term__functor(term__atom("="), [Term1, RetTerm], Context)
    ;
        MaybeRet = no,
        Term0 = term__functor(term__atom("pred"), ArgTerms, Context),
        maybe_add_lambda_eval_method(EvalMethod, Term0, Term2)
    ),
    maybe_add_purity_annotation(Purity, Term2, Term).
unparse_type(tuple(Args, _), Term) :-
    Context = term__context_init,
    unparse_type_list(Args, ArgTerms),
    Term = term__functor(term__atom("{}"), ArgTerms, Context).
unparse_type(apply_n(TVar, Args, _), Term) :-
    Context = term__context_init,
    Var = term__coerce_var(TVar),
    unparse_type_list(Args, ArgTerms),
    Term = term__functor(term__atom(""), [term__variable(Var) | ArgTerms],
        Context).
unparse_type(kinded(_, _), _) :-
    unexpected(this_file, "prog_io_util: kind annotation").

:- pred unparse_type_list(list(mer_type)::in, list(term)::out) is det.

unparse_type_list(Types, Terms) :-
    list__map(unparse_type, Types, Terms).

:- pred unparse_qualified_term(sym_name::in, list(term)::in, term::out) is det.

unparse_qualified_term(unqualified(Name), Args, Term) :-
    Context = term__context_init,
    Term = term__functor(term__atom(Name), Args, Context).
unparse_qualified_term(qualified(Qualifier, Name), Args, Term) :-
    Context = term__context_init,
    unparse_qualified_term(Qualifier, [], QualTerm),
    Term0 = term__functor(term__atom(Name), Args, Context),
    Term = term__functor(term__atom("."), [QualTerm, Term0], Context).

:- pred maybe_add_lambda_eval_method(lambda_eval_method::in, term::in,
    term::out) is det.

maybe_add_lambda_eval_method(lambda_normal, Term, Term).
maybe_add_lambda_eval_method(lambda_aditi_bottom_up, Term0, Term) :-
    Context = term__context_init,
    Term = term__functor(term__atom("aditi_bottom_up"), [Term0], Context).

:- pred maybe_add_purity_annotation(purity::in, term::in, term::out) is det.

maybe_add_purity_annotation(purity_pure, Term, Term).
maybe_add_purity_annotation(purity_semipure, Term0, Term) :-
    Context = term__context_init,
    Term = term__functor(term__atom("semipure"), [Term0], Context).
maybe_add_purity_annotation(purity_impure, Term0, Term) :-
    Context = term__context_init,
    Term = term__functor(term__atom("impure"), [Term0], Context).

convert_mode_list(_, [], []).
convert_mode_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
    convert_mode(AllowConstrainedInstVar, H0, H),
    convert_mode_list(AllowConstrainedInstVar, T0, T).

convert_mode(AllowConstrainedInstVar, Term, Mode) :-
    (
        Term = term__functor(term__atom(">>"), [InstA, InstB], _)
    ->
        convert_inst(AllowConstrainedInstVar, InstA, ConvertedInstA),
        convert_inst(AllowConstrainedInstVar, InstB, ConvertedInstB),
        Mode = (ConvertedInstA -> ConvertedInstB)
    ;
        % Handle higher-order predicate modes:
        % a mode of the form
        %   pred(<Mode1>, <Mode2>, ...) is <Det>
        % is an abbreviation for the inst mapping
        %   (  pred(<Mode1>, <Mode2>, ...) is <Det>
        %   -> pred(<Mode1>, <Mode2>, ...) is <Det>
        %   )

        Term = term__functor(term__atom("is"), [PredTerm, DetTerm], _),
        PredTerm = term__functor(term__atom("pred"), ArgModesTerms, _)
    ->
        DetTerm = term__functor(term__atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerms, ArgModes),
        PredInstInfo = pred_inst_info(predicate, ArgModes, Detism),
        Inst = ground(shared, higher_order(PredInstInfo)),
        Mode = (Inst -> Inst)
    ;
        % Handle higher-order function modes:
        % a mode of the form
        %   func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
        % is an abbreviation for the inst mapping
        %   (  func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
        %   -> func(<Mode1>, <Mode2>, ...) = <RetMode> is <Det>
        %   )

        Term = term__functor(term__atom("is"), [EqTerm, DetTerm], _),
        EqTerm = term__functor(term__atom("="), [FuncTerm, RetModeTerm], _),
        FuncTerm = term__functor(term__atom("func"), ArgModesTerms, _)
    ->
        DetTerm = term__functor(term__atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerms, ArgModes0),
        convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
        list__append(ArgModes0, [RetMode], ArgModes),
        FuncInstInfo = pred_inst_info(function, ArgModes, Detism),
        Inst = ground(shared, higher_order(FuncInstInfo)),
        Mode = (Inst -> Inst)
    ;
        parse_qualified_term(Term, Term, "mode definition", R),
        R = ok(Name, Args), % should improve error reporting
        convert_inst_list(AllowConstrainedInstVar, Args, ConvertedArgs),
        Mode = user_defined_mode(Name, ConvertedArgs)
    ).

convert_inst_list(_, [], []).
convert_inst_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
    convert_inst(AllowConstrainedInstVar, H0, H),
    convert_inst_list(AllowConstrainedInstVar, T0, T).

convert_inst(_, term__variable(V0), inst_var(V)) :-
    term__coerce_var(V0, V).
convert_inst(AllowConstrainedInstVar, Term, Result) :-
    Term = term__functor(term__atom(Name), Args0, _Context),
    (
        convert_simple_builtin_inst(Name, Args0, Result0)
    ->
        Result = Result0
    ;
        % The syntax for a higher-order pred inst is
        %
        %   pred(<Mode1>, <Mode2>, ...) is <Detism>
        %
        % where <Mode1>, <Mode2>, ... are a list of modes,
        % and <Detism> is a determinism.

        Name = "is", Args0 = [PredTerm, DetTerm],
        PredTerm = term__functor(term__atom("pred"), ArgModesTerm, _)
    ->
        DetTerm = term__functor(term__atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerm, ArgModes),
        PredInst = pred_inst_info(predicate, ArgModes, Detism),
        Result = ground(shared, higher_order(PredInst))
    ;
        % The syntax for a higher-order func inst is
        %
        %   func(<Mode1>, <Mode2>, ...) = <RetMode> is <Detism>
        %
        % where <Mode1>, <Mode2>, ... are a list of modes,
        % <RetMode> is a mode, and <Detism> is a determinism.

        Name = "is", Args0 = [EqTerm, DetTerm],
        EqTerm = term__functor(term__atom("="), [FuncTerm, RetModeTerm], _),
        FuncTerm = term__functor(term__atom("func"), ArgModesTerm, _)
    ->
        DetTerm = term__functor(term__atom(DetString), [], _),
        standard_det(DetString, Detism),
        convert_mode_list(AllowConstrainedInstVar, ArgModesTerm, ArgModes0),
        convert_mode(AllowConstrainedInstVar, RetModeTerm, RetMode),
        list__append(ArgModes0, [RetMode], ArgModes),
        FuncInst = pred_inst_info(function, ArgModes, Detism),
        Result = ground(shared, higher_order(FuncInst))

    ; Name = "bound", Args0 = [Disj] ->
        % `bound' insts
        parse_bound_inst_list(AllowConstrainedInstVar, Disj, shared, Result)
    ; Name = "bound_unique", Args0 = [Disj] ->
        % `bound_unique' is for backwards compatibility - use `unique' instead.
        parse_bound_inst_list(AllowConstrainedInstVar, Disj, unique, Result)
    ; Name = "unique", Args0 = [Disj] ->
        parse_bound_inst_list(AllowConstrainedInstVar, Disj, unique, Result)
    ; Name = "mostly_unique", Args0 = [Disj] ->
        parse_bound_inst_list(AllowConstrainedInstVar, Disj, mostly_unique,
            Result)
    ; Name = "=<", Args0 = [VarTerm, InstTerm] ->
        AllowConstrainedInstVar = allow_constrained_inst_var,
        VarTerm = term__variable(Var),
        % Do not allow nested constrained_inst_vars.
        convert_inst(no_allow_constrained_inst_var, InstTerm, Inst),
        Result = constrained_inst_vars(set__make_singleton_set(
            term__coerce_var(Var)), Inst)
    ;
        % Anything else must be a user-defined inst.
        parse_qualified_term(Term, Term, "inst", ok(QualifiedName, Args1)),
        (
            mercury_public_builtin_module(BuiltinModule),
            sym_name_get_module_name(QualifiedName, unqualified(""),
                BuiltinModule),
            % If the term is qualified with the `builtin' module
            % then it may be one of the simple builtin insts.
            % We call convert_inst recursively to check for this.
            unqualify_name(QualifiedName, UnqualifiedName),
            convert_simple_builtin_inst(UnqualifiedName, Args1, Result0),

            % However, if the inst is a user_inst defined inside
            % the `builtin' module then we need to make sure it is
            % properly module-qualified.
            Result0 \= defined_inst(user_inst(_, _))
        ->
            Result = Result0
        ;
            convert_inst_list(AllowConstrainedInstVar, Args1, Args),
            Result = defined_inst(user_inst(QualifiedName, Args))
        )
    ).

    % A "simple" builtin inst is one that has no arguments and no special
    % syntax.
    %
:- pred convert_simple_builtin_inst(string::in, list(term)::in, mer_inst::out)
    is semidet.

convert_simple_builtin_inst(Name, [], Inst) :-
    convert_simple_builtin_inst_2(Name, Inst).

:- pred convert_simple_builtin_inst_2(string::in, mer_inst::out) is semidet.

    % `free' insts
convert_simple_builtin_inst_2("free", free).

    % `any' insts
convert_simple_builtin_inst_2("any",                    any(shared)).
convert_simple_builtin_inst_2("unique_any",             any(unique)).
convert_simple_builtin_inst_2("mostly_unique_any",      any(mostly_unique)).
convert_simple_builtin_inst_2("clobbered_any",          any(clobbered)).
convert_simple_builtin_inst_2("mostly_clobbered_any",   any(mostly_clobbered)).

    % `ground' insts
convert_simple_builtin_inst_2("ground",         ground(shared, none)).
convert_simple_builtin_inst_2("unique",         ground(unique, none)).
convert_simple_builtin_inst_2("mostly_unique",  ground(mostly_unique, none)).
convert_simple_builtin_inst_2("clobbered",      ground(clobbered, none)).
convert_simple_builtin_inst_2("mostly_clobbered",
                        ground(mostly_clobbered, none)).

    % `not_reached' inst
convert_simple_builtin_inst_2("not_reached", not_reached).

standard_det("det", det).
standard_det("cc_nondet", cc_nondet).
standard_det("cc_multi", cc_multidet).
standard_det("nondet", nondet).
standard_det("multi", multidet).
standard_det("multidet", multidet).
standard_det("semidet", semidet).
standard_det("erroneous", erroneous).
standard_det("failure", failure).

:- pred parse_bound_inst_list(allow_constrained_inst_var::in, term::in,
    uniqueness::in, mer_inst::out) is semidet.

parse_bound_inst_list(AllowConstrainedInstVar, Disj, Uniqueness,
        bound(Uniqueness, Functors)) :-
    disjunction_to_list(Disj, List),
    convert_bound_inst_list(AllowConstrainedInstVar, List, Functors0),
    list__sort(Functors0, Functors),
    % Check that the list doesn't specify the same functor twice.
    \+ (
        list__append(_, SubList, Functors),
        SubList = [F1, F2 | _],
        F1 = functor(ConsId, _),
        F2 = functor(ConsId, _)
    ).

:- pred convert_bound_inst_list(allow_constrained_inst_var::in, list(term)::in,
    list(bound_inst)::out) is semidet.

convert_bound_inst_list(_, [], []).
convert_bound_inst_list(AllowConstrainedInstVar, [H0 | T0], [H | T]) :-
    convert_bound_inst(AllowConstrainedInstVar, H0, H),
    convert_bound_inst_list(AllowConstrainedInstVar, T0, T).

:- pred convert_bound_inst(allow_constrained_inst_var::in, term::in,
    bound_inst::out) is semidet.

convert_bound_inst(AllowConstrainedInstVar, InstTerm, functor(ConsId, Args)) :-
    InstTerm = term__functor(Functor, Args0, _),
    ( Functor = term__atom(_) ->
        parse_qualified_term(InstTerm, InstTerm, "inst", ok(SymName, Args1)),
        list__length(Args1, Arity),
        ConsId = cons(SymName, Arity)
    ;
        Args1 = Args0,
        list__length(Args1, Arity),
        ConsId = make_functor_cons_id(Functor, Arity)
    ),
    convert_inst_list(AllowConstrainedInstVar, Args1, Args).

disjunction_to_list(Term, List) :-
    binop_term_to_list(";", Term, List).

conjunction_to_list(Term, List) :-
    binop_term_to_list(",", Term, List).

list_to_conjunction(_, Term, [], Term).
list_to_conjunction(Context, First, [Second | Rest], Term) :-
    list_to_conjunction(Context, Second, Rest, Tail),
    Term = term__functor(term__atom(","), [First, Tail], Context).

sum_to_list(Term, List) :-
    binop_term_to_list("+", Term, List).

    % general predicate to convert terms separated by any specified
    % operator into a list

:- pred binop_term_to_list(string::in, term(T)::in, list(term(T))::out) is det.

binop_term_to_list(Op, Term, List) :-
    binop_term_to_list_2(Op, Term, [], List).

:- pred binop_term_to_list_2(string::in, term(T)::in, list(term(T))::in,
    list(term(T))::out) is det.

binop_term_to_list_2(Op, Term, !List) :-
    ( Term = term__functor(term__atom(Op), [L, R], _Context) ->
        binop_term_to_list_2(Op, R, !List),
        binop_term_to_list_2(Op, L, !List)
    ;
        !:List = [Term | !.List]
    ).

parse_list(Parser, Term, Result) :-
    conjunction_to_list(Term, List),
    map_parser(Parser, List, Result).

map_parser(_, [], ok([])).
map_parser(Parser, [X | Xs], Result) :-
    call(Parser, X, X_Result),
    map_parser(Parser, Xs, Xs_Result),
    combine_list_results(X_Result, Xs_Result, Result).

    % If a list of things contains multiple errors, then we only
    % report the first one.
    %
:- pred combine_list_results(maybe1(T)::in, maybe1(list(T))::in,
    maybe1(list(T))::out) is det.

combine_list_results(error(Msg, Term), _, error(Msg, Term)).
combine_list_results(ok(_), error(Msg, Term), error(Msg, Term)).
combine_list_results(ok(X), ok(Xs), ok([X | Xs])).

%-----------------------------------------------------------------------------%

parse_quantifier_vars(functor(atom("[]"),  [],     _), [],  []).
parse_quantifier_vars(functor(atom("[|]"), [H, T], _), !:SVs, !:Vs) :-
    parse_quantifier_vars(T, !:SVs, !:Vs),
    (
        H = functor(atom("!"), [variable(SV)], _),
        !:SVs = [SV | !.SVs]
    ;
        H = variable(V),
        !:Vs = [V | !.Vs]
    ).

parse_vars(functor(atom("[]"),  [],     _), []).
parse_vars(functor(atom("[|]"), [H, T], _), !:Vs) :-
    parse_vars(T, !:Vs),
    H = variable(V),
    !:Vs = [V | !.Vs].

parse_vars_and_state_vars(functor(atom("[]"),  [],     _), [],   [],   []).
parse_vars_and_state_vars(functor(atom("[|]"), [H, T], _), !:Os, !:Ds, !:Cs) :-
    parse_vars_and_state_vars(T, !:Os, !:Ds, !:Cs),
    (
        H = functor(atom("!"), [variable(V)], _),
        !:Ds = [V | !.Ds],
        !:Cs = [V | !.Cs]
    ;
        H = functor(atom("!."), [variable(V)], _),
        !:Ds = [V | !.Ds]
    ;
        H = functor(atom("!:"), [variable(V)], _),
        !:Cs = [V | !.Cs]
    ;
        H = variable(V),
        !:Os = [V | !.Os]
    ).

%-----------------------------------------------------------------------------%

list_term_to_term_list(Methods, MethodList) :-
    (
        Methods = term__functor(term__atom("[|]"), [Head, Tail0], _),
        list_term_to_term_list(Tail0, Tail),
        MethodList = [Head|Tail]
    ;
        Methods = term__functor(term__atom("[]"), [], _),
        MethodList = []
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "prog_io_util.m".

%-----------------------------------------------------------------------------%
:- end_module parse_tree__prog_io_util.
%-----------------------------------------------------------------------------%
