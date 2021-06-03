%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2015-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_util.m.
% Main author: fjh.
%
% This module defines the types used by parse_module.m and its subcontractors
% to return the results of parsing, and some utility predicates needed
% by several of the parser modules.
%
% Most parsing predicates must check for errors. They return either the
% item(s) they were looking for, or an error indication.
%
% Most of the parsing predicates return a `maybe1(T)' or a `maybe2(T1, T2)',
% which will either be the `ok(ParseTree)' (or `ok(ParseTree1, ParseTree2)'),
% if the parse is successful, or `error(Message, Term)' if it is not.
% The `Term' there should be the term which is syntactically incorrect.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_util.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module cord.
:- import_module list.
:- import_module integer.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

:- pred parse_implicitly_qualified_name_and_arity(module_name::in, term(T)::in,
    sym_name::out, arity::out) is semidet.

:- pred parse_unqualified_name_and_arity(term(T)::in,
    sym_name::out, arity::out) is semidet.

:- pred parse_pred_or_func_name_and_arity(term(T)::in,
    pred_or_func::out, sym_name::out, arity::out) is semidet.

%---------------------------------------------------------------------------%

    % Either ok2(SymName, Args - MaybeFuncRetArg) or error2(Specs).
:- type maybe_pred_or_func(T) == maybe2(sym_name, pair(list(T), maybe(T))).

:- pred parse_pred_or_func_and_args(term(T)::in,
    pred_or_func::out, sym_name::out, list(term(T))::out) is semidet.

:- pred parse_pred_or_func_and_args_general(maybe(module_name)::in,
    term(T)::in, varset(T)::in, cord(format_component)::in,
    maybe_pred_or_func(term(T))::out) is det.

%---------------------------------------------------------------------------%

:- pred parse_pred_pf_name_arity(module_name::in, string::in,
    varset::in, term::in, maybe1(pred_pf_name_arity)::out) is det.

:- pred parse_pred_pfu_name_arity(module_name::in, string::in,
    varset::in, term::in, maybe1(pred_pfu_name_arity)::out) is det.

:- pred parse_pred_pfu_name_arity_maybe_modes(module_name::in,
    cord(format_component)::in, varset::in, term::in, 
    maybe1(pred_or_proc_pfumm_name)::out) is det.

:- type maybe_pred_or_func_modes ==
    maybe3(sym_name, pred_or_func, list(mer_mode)).

:- pred parse_pred_or_func_and_arg_modes(maybe(module_name)::in,
    cord(format_component)::in, varset::in, term::in,
    maybe_pred_or_func_modes::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred list_term_to_term_list(term::in, list(term)::out) is semidet.

%---------------------------------------------------------------------------%

    % Convert a "disjunction" (bunch of terms separated by ';'s) to a list.
    %
:- pred disjunction_to_one_or_more(term(T)::in, one_or_more(term(T))::out)
    is det.
:- pred disjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % Convert a "conjunction" (bunch of terms separated by ','s) to a list.
    %
:- pred conjunction_to_one_or_more(term(T)::in, one_or_more(term(T))::out)
    is det.
:- pred conjunction_to_list(term(T)::in, list(term(T))::out) is det.

    % one_or_more_to_conjunction(Context, List, Term):
    %
    % Convert a nonempty list to a "conjunction", i.e. a bunch of terms
    % separated by commas.
    %
:- pred one_or_more_to_conjunction(prog_context::in, one_or_more(term(T))::in,
    term(T)::out) is det.

    % Convert a "sum", i.e. a bunch of terms separated by '+' operators
    % to a nonempty list.
    %
:- pred sum_to_one_or_more(term(T)::in, one_or_more(term(T))::out) is det.
:- pred sum_to_list(term(T)::in, list(term(T))::out) is det.

%---------------------------------------------------------------------------%

:- type parser(T) == pred(term, maybe1(T)).
:- mode parser == (pred(in, out) is det).

    % Parse a comma-separated list (misleading described as a "conjunction")
    % of things.
    %
:- pred parse_comma_separated_one_or_more(parser(T)::parser, term::in,
    maybe1(one_or_more(T))::out) is det.
:- pred parse_comma_separated_list(parser(T)::parser, term::in,
    maybe1(list(T))::out) is det.

:- pred map_parser(parser(T)::parser, list(term)::in, maybe1(list(T))::out)
    is det.

%---------------------------------------------------------------------------%

    % parse_list_elements(What, VarSet, Term, Pred, Result):
    %
    % Convert Term into a list of elements, where Pred converts each element
    % of the list into the correct type. Result will hold the list if the
    % conversion succeeded for each every of M, otherwise it will hold
    % the errors resulting from the failed conversion attempts.
    %
    % This predicate generates error messages for malformed lists. To do that,
    % it uses the What argument, which should have the form "a list of xyzs".
    % The job of generating error messages for any malformed elements
    % is up to Pred.
    %
:- pred parse_list_elements(string::in,
    pred(varset, term, maybe1(T))::(pred(in, in, out) is det),
    varset::in, term::in, maybe1(list(T))::out) is det.

%---------------------------------------------------------------------------%

    % A value of this type such as
    %
    %   conflict(single_prec_float, double_prec_float,
    %       "floats cannot be both single- and double-precision")
    %
    % gives two different options that may not be specified together
    % in a list of options, together with the error message to print
    % if a user nevertheless does specify them together.
    %
:- type conflict(T)
    --->    conflict(T, T, string).

    % report_any_conflicts(Context, ConflictingWhatInWhat, Conflicts,
    %   Specified, Spec):
    %
    % For each pair of elements in Specified that Conflicts says should
    % *not* be present together, generate an error message from the third
    % field of the relevent member of Conflicts.
    %
:- pred report_any_conflicts(prog_context::in, string::in,
    list(conflict(T))::in, list(T)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

    % Given the arguments of the term.integer function symbol and a context,
    % return the corresponding cons_id, if there is one.
    %
:- pred parse_integer_cons_id(integer_base::in, integer::in, signedness::in,
    integer_size::in, term.context::in, maybe1(cons_id)::out) is det.

%---------------------------------------------------------------------------%

:- pred parse_decimal_int(cord(format_component)::in, varset::in, term::in,
    maybe1(int)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.

:- import_module string.
:- import_module term_io.

%---------------------------------------------------------------------------%

parse_implicitly_qualified_name_and_arity(ModuleName, PredAndArityTerm,
        SymName, Arity) :-
    PredAndArityTerm = term.functor(term.atom("/"),
        [PredNameTerm, ArityTerm], _),
    try_parse_implicitly_qualified_sym_name_and_no_args(ModuleName,
        PredNameTerm, SymName),
    decimal_term_to_int(ArityTerm, Arity).

parse_unqualified_name_and_arity(Term, SymName, Arity) :-
    parse_implicitly_qualified_name_and_arity(unqualified(""),
        Term, SymName, Arity).

parse_pred_or_func_name_and_arity(PorFPredAndArityTerm,
        PredOrFunc, SymName, Arity) :-
    PorFPredAndArityTerm = term.functor(term.atom(PredOrFuncStr), Args, _),
    ( PredOrFuncStr = "pred", PredOrFunc = pf_predicate
    ; PredOrFuncStr = "func", PredOrFunc = pf_function
    ),
    Args = [Arg],
    ModuleName = unqualified(""),
    parse_implicitly_qualified_name_and_arity(ModuleName, Arg, SymName, Arity).

%---------------------------------------------------------------------------%

parse_pred_or_func_and_args(PredAndArgsTerm, PredOrFunc, SymName, ArgTerms) :-
    ( if
        PredAndArgsTerm = term.functor(term.atom("="),
            [FuncAndArgsTerm, FuncResultTerm], _)
    then
        try_parse_sym_name_and_args(FuncAndArgsTerm, SymName, ArgTerms0),
        PredOrFunc = pf_function,
        ArgTerms = ArgTerms0 ++ [FuncResultTerm]
    else
        try_parse_sym_name_and_args(PredAndArgsTerm, SymName, ArgTerms),
        PredOrFunc = pf_predicate
    ).

parse_pred_or_func_and_args_general(MaybeModuleName, PredAndArgsTerm,
        VarSet, ContextPieces, PredAndArgsResult) :-
    ( if
        PredAndArgsTerm = term.functor(term.atom("="),
            [FuncAndArgsTerm, FuncResultTerm], _)
    then
        FunctorTerm = FuncAndArgsTerm,
        MaybeFuncResult = yes(FuncResultTerm)
    else
        FunctorTerm = PredAndArgsTerm,
        MaybeFuncResult = no
    ),
    varset.coerce(VarSet, GenericVarSet),
    (
        MaybeModuleName = yes(ModuleName),
        parse_implicitly_qualified_sym_name_and_args(ModuleName, FunctorTerm,
            GenericVarSet, ContextPieces, Result)
    ;
        MaybeModuleName = no,
        parse_sym_name_and_args(GenericVarSet, ContextPieces,
            FunctorTerm, Result)
    ),
    (
        Result = ok2(SymName, Args),
        PredAndArgsResult = ok2(SymName, Args - MaybeFuncResult)
    ;
        Result = error2(Specs),
        PredAndArgsResult = error2(Specs)
    ).

%---------------------------------------------------------------------------%

parse_pred_pf_name_arity(ModuleName, PragmaName, VarSet,
        Term, MaybePredSpec) :-
    ( if
        Term = term.functor(term.atom(Functor), [SubTerm], _),
        ( Functor = "pred", PorF = pf_predicate
        ; Functor = "func", PorF = pf_function
        ),
        parse_implicitly_qualified_name_and_arity(ModuleName, SubTerm,
            SymName, Arity)
    then
        PredSpec = pred_pf_name_arity(PorF, SymName, user_arity(Arity)),
        MaybePredSpec = ok1(PredSpec)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error in"),
            pragma_decl(PragmaName), words("declaration:"), nl,
            words("expected one of"), nl,
            quote("pred(name/arity)"), words("and"), nl,
            quote("func(name/arity)"), suffix(","), nl,
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybePredSpec = error1([Spec])
    ).

parse_pred_pfu_name_arity(ModuleName, PragmaName, VarSet,
        Term, MaybePredSpec) :-
    ( if is_term_symname_arity_maybe_pf(ModuleName, Term, PredSpec) then
        MaybePredSpec = ok1(PredSpec)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = [words("Error in"),
            pragma_decl(PragmaName), words("declaration:"),
            words("expected one of"),
            quote("pred(name/arity)"), suffix(","), nl,
            quote("func(name/arity)"), words("and"), nl,
            quote("name/arity"), suffix(","), nl,
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            get_term_context(Term), Pieces),
        MaybePredSpec = error1([Spec])
    ).

parse_pred_pfu_name_arity_maybe_modes(ModuleName, ContextPieces, VarSet,
        Term, MaybePredOrProcSpec) :-
    ( if
        is_term_symname_arity_maybe_pf(ModuleName, Term, PredSpec)
    then
        PredSpec = pred_pfu_name_arity(PFU, SymName, Arity),
        (
            PFU = pfu_unknown,
            PFUMM = pfumm_unknown(Arity)
        ;
            PFU = pfu_predicate,
            PFUMM = pfumm_predicate(moa_arity(Arity))
        ;
            PFU = pfu_function,
            PFUMM = pfumm_function(moa_arity(Arity))
        ),
        PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, SymName),
        MaybePredOrProcSpec = ok1(PredOrProcSpec)
    else
        parse_pred_or_func_and_arg_modes(yes(ModuleName), ContextPieces,
            VarSet, Term, MaybePredAndModes),
        (
            MaybePredAndModes = ok3(SymName, PredOrFunc, Modes),
            (
                PredOrFunc = pf_predicate,
                PFUMM = pfumm_predicate(moa_modes(Modes))
            ;
                PredOrFunc = pf_function,
                PFUMM = pfumm_function(moa_modes(Modes))
            ),
            PredOrProcSpec = pred_or_proc_pfumm_name(PFUMM, SymName),
            MaybePredOrProcSpec = ok1(PredOrProcSpec)
        ;
            MaybePredAndModes = error3(PredAndModeSpecs),
            % XXX We should try to
            %
            % - return the expected/got error message if Term *does not*
            %   look to like an attempt to specify argument modes, but
            % - return PredAndModeSpecs if Term *does* look to be
            %   such an attempt.
            %
            % Our heuristic for trying to pick between the two is very crude.
            ( if Term = term.functor(term.atom("/"), _, _) then
                TermStr = describe_error_term(VarSet, Term),
                Pieces = cord.list(ContextPieces) ++
                    [lower_case_next_if_not_first,
                    words("Error: expected one of"), nl_indent_delta(1),
                    quote("name/arity"), suffix(","), nl,
                    quote("pred(name/arity)"), suffix(","), nl,
                    quote("func(name/arity)"), suffix(","), nl,
                    quote("pred(mode1, mode2, ..., moden)"), words("or"), nl,
                    quote("func(mode1, mode2, ..., moden) = retmode"),
                        suffix(","), nl,
                    words("got"), quote(TermStr), suffix("."), nl],
                Spec = simplest_spec($pred, severity_error,
                    phase_term_to_parse_tree, get_term_context(Term), Pieces),
                MaybePredOrProcSpec = error1([Spec])
            else
                MaybePredOrProcSpec = error1(PredAndModeSpecs)
            )
        )
    ).

:- pred is_term_symname_arity_maybe_pf(module_name::in, term::in,
    pred_pfu_name_arity::out) is semidet.

is_term_symname_arity_maybe_pf(ModuleName, Term, PredSpec) :-
    ( if
        Term = term.functor(term.atom(Functor), [SubTerm], _),
        ( Functor = "pred", PFU = pfu_predicate
        ; Functor = "func", PFU = pfu_function
        ),
        parse_implicitly_qualified_name_and_arity(ModuleName, SubTerm,
            SymName, Arity)
    then
        PredSpec = pred_pfu_name_arity(PFU, SymName, user_arity(Arity))
    else if
        parse_implicitly_qualified_name_and_arity(ModuleName, Term,
            SymName, Arity)
    then
        PredSpec = pred_pfu_name_arity(pfu_unknown, SymName, user_arity(Arity))
    else
        fail
    ).

parse_pred_or_func_and_arg_modes(MaybeModuleName, ContextPieces, VarSet,
        PredAndModesTerm, MaybeNameAndModes) :-
    parse_pred_or_func_and_args_general(MaybeModuleName, PredAndModesTerm,
        VarSet, ContextPieces, MaybePredAndArgs),
    (
        MaybePredAndArgs = ok2(PredName, ArgModeTerms - MaybeRetModeTerm),
        (
            MaybeRetModeTerm = no,
            parse_modes(allow_constrained_inst_var, VarSet, ContextPieces,
                ArgModeTerms, MaybeArgModes),
            (
                MaybeArgModes = ok1(ArgModes),
                % For predicates, we don't call constrain_inst_vars_in_mode
                % on ArgModes. XXX Why precisely?
                MaybeNameAndModes = ok3(PredName, pf_predicate, ArgModes)
            ;
                MaybeArgModes = error1(Specs),
                MaybeNameAndModes = error3(Specs)
            )
        ;
            MaybeRetModeTerm = yes(RetModeTerm),
            parse_modes(allow_constrained_inst_var, VarSet, ContextPieces,
                ArgModeTerms, MaybeArgModes0),
            RetContextPieces = ContextPieces ++
                cord.singleton(words("in the return value:")),
            parse_mode(allow_constrained_inst_var, VarSet, RetContextPieces,
                RetModeTerm, MaybeRetMode),
            ( if
                MaybeArgModes0 = ok1(ArgModes0),
                MaybeRetMode = ok1(RetMode)
            then
                ArgModes1 = ArgModes0 ++ [RetMode],
                list.map(constrain_inst_vars_in_mode, ArgModes1, ArgModes),
                MaybeNameAndModes = ok3(PredName, pf_function, ArgModes)
            else
                Specs = get_any_errors1(MaybeArgModes0)
                    ++ get_any_errors1(MaybeRetMode),
                MaybeNameAndModes = error3(Specs)
            )
        )
    ;
        MaybePredAndArgs = error2(Specs),
        MaybeNameAndModes = error3(Specs)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

list_term_to_term_list(Term, Terms) :-
    (
        Term = term.functor(term.atom("[|]"), [HeadTerm, TailTerm], _),
        list_term_to_term_list(TailTerm, TailTerms),
        Terms = [HeadTerm | TailTerms]
    ;
        Term = term.functor(term.atom("[]"), [], _),
        Terms = []
    ).

%---------------------------------------------------------------------------%

disjunction_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more(";", Term, OneOrMore).

disjunction_to_list(Term, List) :-
    binop_term_to_one_or_more(";", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

conjunction_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more(",", Term, OneOrMore).

conjunction_to_list(Term, List) :-
    binop_term_to_one_or_more(",", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

one_or_more_to_conjunction(_, one_or_more(Term, []), Term).
one_or_more_to_conjunction(Context, one_or_more(First, [Second | Rest]),
        Term) :-
    one_or_more_to_conjunction(Context, one_or_more(Second, Rest), Tail),
    Term = term.functor(term.atom(","), [First, Tail], Context).

sum_to_one_or_more(Term, OneOrMore) :-
    binop_term_to_one_or_more("+", Term, OneOrMore).

sum_to_list(Term, List) :-
    binop_term_to_one_or_more("+", Term, one_or_more(Head, Tail)),
    List = [Head | Tail].

    % General predicate to convert terms separated by any specified operator
    % into a list.
    %
:- pred binop_term_to_one_or_more(string::in, term(T)::in,
    one_or_more(term(T))::out) is det.

binop_term_to_one_or_more(Op, Term, OneOrMore) :-
    binop_term_to_one_or_more_loop(Op, Term, [], OneOrMore).

:- pred binop_term_to_one_or_more_loop(string::in, term(T)::in,
    list(term(T))::in, one_or_more(term(T))::out) is det.

binop_term_to_one_or_more_loop(Op, Term, List0, OneOrMore) :-
    ( if Term = term.functor(term.atom(Op), [L, R], _Context) then
        binop_term_to_one_or_more_loop(Op, R, List0,
            one_or_more(RHead, RTail)),
        binop_term_to_one_or_more_loop(Op, L, [RHead | RTail], OneOrMore)
    else
        OneOrMore = one_or_more(Term, List0)
    ).

%---------------------------------------------------------------------------%

parse_comma_separated_one_or_more(Parser, Term, Result) :-
    conjunction_to_one_or_more(Term, one_or_more(Head, Tail)),
    map_parser_one_or_more(Parser, Head, Tail, Result).

parse_comma_separated_list(Parser, Term, Result) :-
    conjunction_to_list(Term, List),
    map_parser(Parser, List, Result).

:- pred map_parser_one_or_more(parser(T)::parser, term::in, list(term)::in,
    maybe1(one_or_more(T))::out) is det.

map_parser_one_or_more(Parser, Head, Tail, Result) :-
    call(Parser, Head, HeadResult),
    (
        Tail = [],
        (
            HeadResult = error1(Specs),
            Result = error1(Specs)
        ;
            HeadResult = ok1(Item),
            Result = ok1(one_or_more(Item, []))
        )
    ;
        Tail = [HeadTail | TailTail],
        map_parser_one_or_more(Parser, HeadTail, TailTail, TailResult),
        (
            HeadResult = error1(HeadSpecs),
            TailResult = error1(TailSpecs),
            Result = error1(HeadSpecs ++ TailSpecs)
        ;
            HeadResult = error1(Specs),
            TailResult = ok1(_),
            Result = error1(Specs)
        ;
            HeadResult = ok1(_),
            TailResult = error1(Specs),
            Result = error1(Specs)
        ;
            HeadResult = ok1(HeadItem),
            TailResult = ok1(TailItems),
            Result = ok1(one_or_more.cons(HeadItem, TailItems))
        )
    ).

map_parser(_, [], ok1([])).
map_parser(Parser, [Head | Tail], Result) :-
    call(Parser, Head, HeadResult),
    map_parser(Parser, Tail, TailResult),
    (
        HeadResult = error1(HeadSpecs),
        TailResult = error1(TailSpecs),
        Result = error1(HeadSpecs ++ TailSpecs)
    ;
        HeadResult = error1(Specs),
        TailResult = ok1(_),
        Result = error1(Specs)
    ;
        HeadResult = ok1(_),
        TailResult = error1(Specs),
        Result = error1(Specs)
    ;
        HeadResult = ok1(HeadItem),
        TailResult = ok1(TailItems),
        Result = ok1([HeadItem | TailItems])
    ).

%---------------------------------------------------------------------------%

parse_list_elements(What, Pred, VarSet, Term, Result) :-
    (
        Term = term.variable(_, _),
        make_expected_got_spec(VarSet, What, Term, Spec),
        Result = error1([Spec])
    ;
        Term = term.functor(Functor, Args, _Context),
        ( if
            Functor = term.atom("[|]"),
            Args = [HeadTerm, TailTerm]
        then
            Pred(VarSet, HeadTerm, HeadResult),
            parse_list_elements(What, Pred, VarSet, TailTerm, TailResult),
            ( if
                HeadResult = ok1(HeadElement),
                TailResult = ok1(TailElements)
            then
                Result = ok1([HeadElement | TailElements])
            else
                Specs = get_any_errors1(HeadResult) ++
                    get_any_errors1(TailResult),
                Result = error1(Specs)
            )
        else if
            Functor = term.atom("[]"),
            Args = []
        then
            Result = ok1([])
        else
            make_expected_got_spec(VarSet, What, Term, Spec),
            Result = error1([Spec])
        )
    ).

:- pred make_expected_got_spec(varset::in, string::in, term::in,
    error_spec::out) is det.

make_expected_got_spec(VarSet, What, Term, Spec) :-
    TermStr = describe_error_term(VarSet, Term),
    Pieces = [words("Error: expected"), words(What), suffix(","),
        words("got"), quote(TermStr), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error,
        phase_term_to_parse_tree, get_term_context(Term), Pieces).

%---------------------------------------------------------------------------%

report_any_conflicts(Context, ConflictingWhatInWhat, Conflicts, Specified,
        Specs) :-
    list.foldl(
        accumulate_conflict_specs(Context, ConflictingWhatInWhat, Specified),
        Conflicts, [], Specs).

:- pred accumulate_conflict_specs(prog_context::in, string::in,
    list(T)::in, conflict(T)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

accumulate_conflict_specs(Context, ConflictingWhatInWhat, Specified,
        Conflict, !Specs) :-
    Conflict = conflict(A, B, Diagnosis),
    ( if
        list.member(A, Specified),
        list.member(B, Specified)
    then
        Pieces = [words("Error:"), words(ConflictingWhatInWhat),
            suffix(":"), nl, words(Diagnosis), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error, phase_term_to_parse_tree,
            Context, Pieces),
        !:Specs = [Spec | !.Specs]
    else
        true
    ).

%---------------------------------------------------------------------------%

parse_integer_cons_id(Base, Integer, Signedness, Size, Context, MaybeConsId) :-
    (
        Size = size_word,
        (
            Signedness = signed,
            parse_integer_const(Context, Base, Integer,
                "", "", source_integer_to_int(Base),
                (func(I) = int_const(I)), MaybeConsId)
        ;
            Signedness = unsigned,
            parse_integer_const(Context, Base, Integer,
                "unsigned", "u", integer.to_uint,
                (func(I) = uint_const(I)), MaybeConsId)
        )
    ;
        Size = size_8_bit,
        (
            Signedness = signed,
            parse_integer_const(Context, Base, Integer,
                "8-bit", "i8", integer.to_int8,
                (func(I) = int8_const(I)), MaybeConsId)
        ;
            Signedness = unsigned,
            parse_integer_const(Context, Base, Integer,
                "unsigned 8-bit", "u8", integer.to_uint8,
                (func(I) = uint8_const(I)), MaybeConsId)
        )
    ;
        Size = size_16_bit,
        (
            Signedness = signed,
            parse_integer_const(Context, Base, Integer,
                "16-bit", "i16", integer.to_int16,
                (func(I) = int16_const(I)), MaybeConsId)
        ;
            Signedness = unsigned,
            parse_integer_const(Context, Base, Integer,
                "unsigned 16-bit", "u16", integer.to_uint16,
                (func(I) = uint16_const(I)), MaybeConsId)
        )
    ;
        Size = size_32_bit,
        (
            Signedness = signed,
            parse_integer_const(Context, Base, Integer,
                "32-bit", "i32", integer.to_int32,
                (func(I) = int32_const(I)), MaybeConsId)
        ;
            Signedness = unsigned,
            parse_integer_const(Context, Base, Integer,
                "unsigned 32-bit", "u32", integer.to_uint32,
                (func(I) = uint32_const(I)), MaybeConsId)
        )
    ;
        Size = size_64_bit,
        (
            Signedness = signed,
            parse_integer_const(Context, Base, Integer,
                "64-bit", "i64", integer.to_int64,
                (func(I) = int64_const(I)), MaybeConsId)
        ;
            Signedness = unsigned,
            parse_integer_const(Context, Base, Integer,
                "unsigned 64-bit", "u64", integer.to_uint64,
                (func(I) = uint64_const(I)), MaybeConsId)
        )
    ).

:- pred parse_integer_const(term.context::in, integer_base::in,
    integer::in, string::in, string::in,
    pred(integer, T)::in(pred(in, out) is semidet),
    (func(T) = some_int_const)::in,
    maybe1(cons_id)::out) is det.

parse_integer_const(Context, Base, Integer, IntTypeDesc, IntSuffixStr,
        ConvPred, ToIntConstPred, MaybeConsId) :-
    ( if ConvPred(Integer, Int) then
        ConsId = some_int_const(ToIntConstPred(Int)),
        MaybeConsId = ok1(ConsId)
    else
        BasePrefix = integer_base_prefix(Base),
        IntStr = integer.to_base_string(Integer, integer_base_int(Base)),
        LiteralStr = BasePrefix ++ IntStr ++ IntSuffixStr,
        Pieces = [words("Error: the"),
            words(IntTypeDesc), words("integer literal"), quote(LiteralStr),
            words("is outside the range of that type."), nl],
        Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
            Context, Pieces),
        MaybeConsId = error1([Spec])
    ).

%---------------------------------------------------------------------------%

parse_decimal_int(ContextPieces, VarSet, Term, MaybeInt) :-
    ( if decimal_term_to_int(Term, Int) then
        MaybeInt = ok1(Int)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected a decimal integer,"),
            words("got"), quote(TermStr), suffix("."), nl],
        Spec = simplest_spec($pred, severity_error,
            phase_term_to_parse_tree, get_term_context(Term), Pieces),
        MaybeInt = error1([Spec])
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_util.
%---------------------------------------------------------------------------%
