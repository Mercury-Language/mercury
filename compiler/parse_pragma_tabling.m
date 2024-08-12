%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 expandtab
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: parse_pragma.m.
%
% This module parses tabling pragmas.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_pragma_tabling.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module list.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % Parse a tabling pragma.
    %
:- pred parse_tabling_pragma(module_name::in, varset::in, term::in,
    string::in, list(term)::in, prog_context::in, item_seq_num::in,
    tabled_eval_method::in, maybe1(item_or_marker)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_util.
:- import_module parse_tree.prog_item.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module maybe.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

parse_tabling_pragma(ModuleName, VarSet, ErrorTerm, PragmaName, PragmaTerms,
        Context, SeqNum, TabledMethod0, MaybeIOM) :-
    (
        (
            PragmaTerms = [PredOrProcSpecTerm0],
            MaybeAttrs = no
        ;
            PragmaTerms = [PredOrProcSpecTerm0, AttrListTerm0],
            MaybeAttrs = yes(AttrListTerm0)
        ),
        ContextPieces = cord.from_list([words("In the first argument of"),
            pragma_decl(PragmaName), words("declaration:"), nl]),
        parse_pred_pfu_name_arity_maybe_modes(ModuleName, ContextPieces,
            VarSet, PredOrProcSpecTerm0, MaybePredOrProcSpec),
        (
            MaybePredOrProcSpec = ok1(PredOrProcSpec),
            (
                MaybeAttrs = no,
                Tabled = impl_pragma_tabled_info(TabledMethod0, PredOrProcSpec,
                    no, Context, SeqNum),
                ImplPragma = impl_pragma_tabled(Tabled),
                Item = item_impl_pragma(ImplPragma),
                MaybeIOM = ok1(iom_item(Item))
            ;
                MaybeAttrs = yes(AttrsListTerm),
                AttrContextPieces = cord.from_list(
                    [words("In the second argument of"),
                    pragma_decl(PragmaName), words("declaration:"), nl]),
                parse_list_elements(AttrContextPieces, "tabling attributes",
                    parse_tabling_attribute(AttrContextPieces, TabledMethod0),
                    VarSet, AttrsListTerm, MaybeAttributeList),
                (
                    MaybeAttributeList = ok1(AttributeList),
                    update_tabling_attributes(AttributeList,
                        default_memo_table_attributes, Attributes,
                        [], DuplicateSpecs),
                    (
                        DuplicateSpecs = [],
                        DisableWarning =
                            Attributes ^ table_attr_backend_warning,
                        (
                            DisableWarning = table_attr_ignore_with_warning,
                            TabledMethod = TabledMethod0
                        ;
                            DisableWarning = table_attr_ignore_without_warning,
                            (
                                TabledMethod0 = tabled_memo(_),
                                TabledMethod = tabled_memo(
                                    table_attr_ignore_without_warning)
                            ;
                                ( TabledMethod0 = tabled_loop_check
                                ; TabledMethod0 = tabled_minimal(_)
                                ),
                                TabledMethod = TabledMethod0
                            ;
                                TabledMethod0 = tabled_io(_, _),
                                unexpected($pred, "non-pragma eval method")
                            )
                        ),
                        Tabled = impl_pragma_tabled_info(TabledMethod,
                            PredOrProcSpec, yes(Attributes), Context, SeqNum),
                        ImplPragma = impl_pragma_tabled(Tabled),
                        Item = item_impl_pragma(ImplPragma),
                        MaybeIOM = ok1(iom_item(Item))
                    ;
                        DuplicateSpecs = [_ | _],
                        MaybeIOM = error1(DuplicateSpecs)
                    )
                ;
                    MaybeAttributeList = error1(Specs),
                    MaybeIOM = error1(Specs)
                )
            )
        ;
            MaybePredOrProcSpec = error1(Specs),
            MaybeIOM = error1(Specs)
        )
    ;
        ( PragmaTerms = []
        ; PragmaTerms = [_, _, _ | _]
        ),
        Pieces = [words("Error: a")] ++
            color_as_subject([pragma_decl(PragmaName),
                words("declaration")]) ++
            [words("must have")] ++
            color_as_incorrect([words("one or two arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(ErrorTerm), Pieces),
        MaybeIOM = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred parse_tabling_attribute(cord(format_piece)::in,
    tabled_eval_method::in, varset::in, term::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attribute(ContextPieces, TabledMethod, VarSet, Term,
        MaybeContextAttribute) :-
    ( if
        Term = term.functor(term.atom(Functor), ArgTerms, Context),
        ( Functor = "fast_loose"
        ; Functor = "specified"
        ; Functor = "size_limit"
        ; Functor = "statistics"
        ; Functor = "allow_reset"
        ; Functor = "disable_warning_if_ignored"
        )
    then
        (
            Functor = "fast_loose",
            parse_tabling_attr_fast_loose(ContextPieces, TabledMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "specified",
            parse_tabling_attr_specified(ContextPieces, TabledMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "size_limit",
            parse_tabling_attr_size_limit(ContextPieces, TabledMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "statistics",
            parse_tabling_attr_statistics(ContextPieces, TabledMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "allow_reset",
            parse_tabling_attr_allow_reset(ContextPieces, TabledMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        ;
            Functor = "disable_warning_if_ignored",
            parse_tabling_attr_backend_warning(ContextPieces, TabledMethod,
                VarSet, Context, ArgTerms, MaybeContextAttribute)
        )
    else
        TermStr = describe_error_term(VarSet, Term),
        Form1 = "fast_loose",
        Form2 = "specified(...)",
        Form3 = "size_limit(...)",
        Form4 = "statistics",
        Form5 = "allow_reset",
        Form6 = "disable_warning_if_ignored",
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected one of"), nl_indent_delta(1)] ++
            color_as_correct([quote(Form1), suffix(",")]) ++ [nl] ++
            color_as_correct([quote(Form2), suffix(",")]) ++ [nl] ++
            color_as_correct([quote(Form3), suffix(",")]) ++ [nl] ++
            color_as_correct([quote(Form4), suffix(",")]) ++ [nl] ++
            color_as_correct([quote(Form5), suffix(",")]) ++
                [words("and"), nl] ++
            color_as_correct([quote(Form6), suffix(",")]) ++
            [nl_indent_delta(-1),
            words("got")] ++
            color_as_incorrect([quote(TermStr), suffix(".")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

%---------------------%

:- pred parse_tabling_attr_fast_loose(cord(format_piece)::in,
    tabled_eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_fast_loose(ContextPieces, TabledMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        require_tabling_fast_loose(ContextPieces, TabledMethod, Context,
            FastLooseSpecs),
        (
            FastLooseSpecs = [],
            Attribute = attr_strictness(cts_all_fast_loose),
            MaybeContextAttribute = ok1(Context - Attribute)
        ;
            FastLooseSpecs = [_ | _],
            MaybeContextAttribute = error1(FastLooseSpecs)
        )
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error:")] ++
            color_as_subject([quote("fast_loose")]) ++
            [words("must have")] ++
            color_as_incorrect([words("no arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

%---------------------%

:- pred parse_tabling_attr_specified(cord(format_piece)::in,
    tabled_eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_specified(ContextPieces, TabledMethod, VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        (
            ArgTerms = [MethodsTerm],
            MaybeHiddenArg = ok1(table_hidden_arg_value)
        ;
            ArgTerms = [MethodsTerm, HiddenArgTerm],
            ( if
                HiddenArgTerm = term.functor(
                    term.atom("hidden_arg_value"), [], _)
            then
                MaybeHiddenArg = ok1(table_hidden_arg_value)
            else if
                HiddenArgTerm = term.functor(
                    term.atom("hidden_arg_addr"), [], _)
            then
                MaybeHiddenArg = ok1(table_hidden_arg_addr)
            else
                HiddenArgTermStr = describe_error_term(VarSet, HiddenArgTerm),
                HiddenArgPieces = cord.list(ContextPieces) ++
                    [lower_case_next_if_not_first,
                    words("In the second argument of"),
                        quote("specified"), suffix(":"), nl,
                    words("error: expected either")] ++
                    color_as_correct([quote("hidden_arg_value")]) ++
                    [words("or")] ++
                    color_as_correct([quote("hidden_arg_addr"),
                        suffix(",")]) ++
                    [words("got")] ++
                    color_as_incorrect([quote(HiddenArgTermStr),
                        suffix(".")]) ++
                    [nl],
                HiddenArgSpec = spec($pred, severity_error, phase_t2pt,
                    get_term_context(HiddenArgTerm), HiddenArgPieces),
                MaybeHiddenArg = error1([HiddenArgSpec])
            )
        ),
        MethodsContextPieces = ContextPieces ++
            cord.from_list([lower_case_next_if_not_first,
            words("In the first argument of"), quote("specified"),
            suffix(":"), nl]),
        parse_list_elements(MethodsContextPieces, "argument tabling methods",
            parse_arg_tabling_method(MethodsContextPieces),
            VarSet, MethodsTerm, MaybeMaybeArgMethods),
        require_tabling_fast_loose(ContextPieces, TabledMethod, Context,
            FastLooseSpecs),
        ( if
            MaybeMaybeArgMethods = ok1(MaybeArgMethods),
            MaybeHiddenArg = ok1(HiddenArg),
            FastLooseSpecs = []
        then
            Attribute = attr_strictness(
                cts_specified(MaybeArgMethods, HiddenArg)),
            MaybeContextAttribute = ok1(Context - Attribute)
        else
            Specs = get_any_errors1(MaybeMaybeArgMethods) ++
                get_any_errors1(MaybeHiddenArg) ++
                FastLooseSpecs,
            MaybeContextAttribute = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _, _ | _]
        ),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error:")] ++
            color_as_subject([quote("specified")]) ++
            [words("must have")] ++
            color_as_incorrect([words("one or two arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

:- pred parse_arg_tabling_method(cord(format_piece)::in,
    varset::in, term::in, maybe1(maybe(arg_tabling_method))::out) is det.

parse_arg_tabling_method(ContextPieces, VarSet, Term,
        MaybeMaybeArgTablingMethod) :-
    ( if
        Term = term.functor(term.atom(Functor), [], _),
        (
            Functor = "value",
            MaybeArgTablingMethod = yes(arg_value)
        ;
            Functor = "addr",
            MaybeArgTablingMethod = yes(arg_addr)
        ;
            Functor = "promise_implied",
            MaybeArgTablingMethod = yes(arg_promise_implied)
        ;
            Functor = "output",
            MaybeArgTablingMethod = no
        )
    then
        MaybeMaybeArgTablingMethod = ok1(MaybeArgTablingMethod)
    else
        TermStr = describe_error_term(VarSet, Term),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: expected one of")]  ++
            color_as_correct([quote("value"), suffix(",")]) ++
            color_as_correct([quote("addr"), suffix(",")]) ++
            color_as_correct([quote("promise_implied"), suffix(",")]) ++
            [words("and")] ++
            color_as_correct([quote("output"), suffix(",")]) ++
            [words("got")] ++
            color_as_incorrect([quote(TermStr), suffix(".")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt,
            get_term_context(Term), Pieces),
        MaybeMaybeArgTablingMethod = error1([Spec])
    ).

%---------------------%

:- pred parse_tabling_attr_size_limit(cord(format_piece)::in,
    tabled_eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_size_limit(ContextPieces, TabledMethod, VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [LimitTerm],
        LimitContextPieces = ContextPieces ++ cord.from_list(
            [lower_case_next_if_not_first,
            words("In the first argument of size_limit:"), nl]),
        parse_decimal_int(LimitContextPieces, VarSet, LimitTerm, MaybeLimit),
        AllowsSizeLimit = eval_method_allows_size_limit(TabledMethod),
        (
            AllowsSizeLimit = yes,
            AllowSpecs = []
        ;
            AllowsSizeLimit = no,
            TabledMethodStr = tabled_eval_method_to_string(TabledMethod),
            AllowPieces = cord.list(ContextPieces) ++
                [lower_case_next_if_not_first,
                words("Error: evaluation method")] ++
                color_as_subject([quote(TabledMethodStr)]) ++
                color_as_incorrect([words("does not allow size limits.")]) ++
                [nl],
            AllowSpec = spec($pred, severity_error, phase_t2pt,
                Context, AllowPieces),
            AllowSpecs = [AllowSpec]
        ),
        ( if
            MaybeLimit = ok1(Limit),
            AllowSpecs = []
        then
            Attribute = attr_size_limit(Limit),
            MaybeContextAttribute = ok1(Context - Attribute)
        else
            Specs = get_any_errors1(MaybeLimit) ++ AllowSpecs,
            MaybeContextAttribute = error1(Specs)
        )
    ;
        ( ArgTerms = []
        ; ArgTerms = [_, _ | _]
        ),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error:")] ++
            color_as_subject([quote("size_limit")]) ++
            [words("must have")] ++
            color_as_incorrect([words("one argument.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

%---------------------%

:- pred parse_tabling_attr_statistics(cord(format_piece)::in,
    tabled_eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_statistics(ContextPieces, _TabledMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        Attribute = attr_statistics,
        MaybeContextAttribute = ok1(Context - Attribute)
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error:")] ++
            color_as_subject([quote("statistics")]) ++
            [words("must have")] ++
            color_as_incorrect([words("no arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

%---------------------%

:- pred parse_tabling_attr_allow_reset(cord(format_piece)::in,
    tabled_eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_allow_reset(ContextPieces, _TabledMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        Attribute = attr_allow_reset,
        MaybeContextAttribute = ok1(Context - Attribute)
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error:")] ++
            color_as_subject([quote("allow_reset")]) ++
            [words("must have")] ++
            color_as_incorrect([words("no arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

%---------------------%

:- pred parse_tabling_attr_backend_warning(cord(format_piece)::in,
    tabled_eval_method::in, varset::in, term.context::in, list(term)::in,
    maybe1(pair(term.context, single_tabling_attribute))::out) is det.

parse_tabling_attr_backend_warning(ContextPieces, TabledMethod, _VarSet,
        Context, ArgTerms, MaybeContextAttribute) :-
    (
        ArgTerms = [],
        AllowsDisableWarning =
            eval_method_allows_disable_warning_if_ignored(TabledMethod),
        (
            AllowsDisableWarning = yes,
            Attribute = attr_ignore_without_warning,
            MaybeContextAttribute = ok1(Context - Attribute)
        ;
            AllowsDisableWarning = no,
            TabledMethodStr = tabled_eval_method_to_string(TabledMethod),
            Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
                words("Error: evaluation method")] ++
                color_as_subject([quote(TabledMethodStr)]) ++
                color_as_incorrect(
                    [words("does not allow disable_warning_if_ignored.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
            MaybeContextAttribute = error1([Spec])
        )
    ;
        ArgTerms = [_ | _],
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error:")] ++
            color_as_subject([quote("allow_reset")]) ++
            [words("must have")] ++
            color_as_incorrect([words("no arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        MaybeContextAttribute = error1([Spec])
    ).

%---------------------------------------------------------------------------%

:- pred require_tabling_fast_loose(cord(format_piece)::in,
    tabled_eval_method::in, term.context::in, list(error_spec)::out) is det.

require_tabling_fast_loose(ContextPieces, TabledMethod, Context, Specs) :-
    AllowsFastLoose = eval_method_allows_fast_loose(TabledMethod),
    (
        AllowsFastLoose = yes,
        Specs = []
    ;
        AllowsFastLoose = no,
        TabledMethodStr = tabled_eval_method_to_string(TabledMethod),
        Pieces = cord.list(ContextPieces) ++ [lower_case_next_if_not_first,
            words("Error: evaluation method")] ++
            color_as_subject([quote(TabledMethodStr)]) ++
            color_as_incorrect(
                [words("does not allow fast_loose tabling.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
        Specs = [Spec]
    ).

%---------------------------------------------------------------------------%

:- func eval_method_allows_fast_loose(tabled_eval_method) = bool.

eval_method_allows_fast_loose(tabled_loop_check) = yes.
eval_method_allows_fast_loose(tabled_memo(_)) = yes.
eval_method_allows_fast_loose(tabled_io(_, _)) = no.
eval_method_allows_fast_loose(tabled_minimal(_)) = no.

:- func eval_method_allows_size_limit(tabled_eval_method) = bool.

eval_method_allows_size_limit(tabled_loop_check) = yes.
eval_method_allows_size_limit(tabled_memo(_)) = yes.
eval_method_allows_size_limit(tabled_io(_, _)) = no.
eval_method_allows_size_limit(tabled_minimal(_)) = no.

:- func eval_method_allows_disable_warning_if_ignored(tabled_eval_method)
    = bool.

eval_method_allows_disable_warning_if_ignored(tabled_loop_check) = no.
eval_method_allows_disable_warning_if_ignored(tabled_memo(_)) = yes.
eval_method_allows_disable_warning_if_ignored(tabled_io(_, _)) = no.
eval_method_allows_disable_warning_if_ignored(tabled_minimal(_)) = no.

%---------------------------------------------------------------------------%

:- type single_tabling_attribute
    --->    attr_strictness(call_table_strictness)
    ;       attr_size_limit(int)
    ;       attr_statistics
    ;       attr_allow_reset
    ;       attr_ignore_without_warning.

:- pred update_tabling_attributes(
    assoc_list(term.context, single_tabling_attribute)::in,
    table_attributes::in, table_attributes::out,
    list(error_spec)::in, list(error_spec)::out) is det.

update_tabling_attributes([], !Attributes, !Specs).
update_tabling_attributes([Context - Attr | ContextAttrs],
        !Attributes, !Specs) :-
    (
        Attr = attr_strictness(Strictness),
        ( if !.Attributes ^ table_attr_strictness = cts_all_strict then
            !Attributes ^ table_attr_strictness := Strictness
        else
            report_duplicate_memo_attribute(Context,
                "argument tabling methods", !Specs)
        )
    ;
        Attr = attr_size_limit(Limit),
        ( if !.Attributes ^ table_attr_size_limit = no then
            !Attributes ^ table_attr_size_limit := yes(Limit)
        else
            report_duplicate_memo_attribute(Context, "size limits", !Specs)
        )
    ;
        Attr = attr_statistics,
        ( if
            !.Attributes ^ table_attr_statistics =
                table_do_not_gather_statistics
        then
            !Attributes ^ table_attr_statistics := table_gather_statistics
        else
            report_duplicate_memo_attribute(Context, "statistics", !Specs)
        )
    ;
        Attr = attr_allow_reset,
        ( if
            !.Attributes ^ table_attr_allow_reset = table_do_not_allow_reset
        then
            !Attributes ^ table_attr_allow_reset := table_allow_reset
        else
            report_duplicate_memo_attribute(Context, "allow_reset", !Specs)
        )
    ;
        Attr = attr_ignore_without_warning,
        ( if
            !.Attributes ^ table_attr_backend_warning =
                table_attr_ignore_with_warning
        then
            !Attributes ^ table_attr_backend_warning :=
                table_attr_ignore_without_warning
        else
            report_duplicate_memo_attribute(Context,
                "disable_warning_if_ignored", !Specs)
        )
    ),
    update_tabling_attributes(ContextAttrs, !Attributes, !Specs).

:- pred report_duplicate_memo_attribute(prog_context::in, string::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_duplicate_memo_attribute(Context, Attr, !Specs) :-
    Pieces = [words("In"), pragma_decl("memo"), words("declaration:"), nl,
        words("error:")] ++
        color_as_incorrect([words("duplicate")]) ++
        color_as_subject([words(Attr), words("attribute.")]) ++
        [nl],
    Spec = spec($pred, severity_error, phase_t2pt, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_pragma_tabling.
%---------------------------------------------------------------------------%
