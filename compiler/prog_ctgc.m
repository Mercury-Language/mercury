%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006, 2008-2012 The University of Melbourne.
% Copyright (C) 2015-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_ctgc.m.
% Main author: nancy.
%
% Utility operations (parsing, printing, renaming) for compile-time garbage
% collection related information, i.e. structure sharing and structure reuse.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_ctgc.
:- interface.

:- import_module parse_tree.parse_tree_output.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.var_db.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Parsing routines.
%

:- func parse_unit_selector(term(T)) = unit_selector.

:- func parse_selector(term(T)) = selector.

:- func parse_datastruct(term(T)) = datastruct.

:- func parse_structure_sharing_pair(term(T)) = structure_sharing_pair.

:- func parse_structure_sharing(term(T)) = structure_sharing.

:- func parse_structure_sharing_domain(term(T)) = structure_sharing_domain.

:- func parse_structure_reuse_condition(term(T)) = structure_reuse_condition.

:- func parse_structure_reuse_conditions(term(T)) = structure_reuse_conditions.

:- func parse_structure_reuse_domain(term(T)) = structure_reuse_domain.

:- pred parse_user_annotated_sharing(varset::in, term::in,
    user_annotated_sharing::out) is semidet.

%---------------------------------------------------------------------------%
%
% Printing routines.
%

    % Print structure sharing or reuse information as a Mercury comment.
    % This is used in HLDS dumps.
    %
:- pred dump_structure_sharing_domain(S::in, var_name_source::in, tvarset::in,
    structure_sharing_domain::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred format_interface_structure_sharing_domain(S::in,
    var_name_source::in, tvarset::in, maybe(structure_sharing_domain)::in,
    U::di, U::uo) is det <= pt_output(S, U).

    % Print structure sharing or reuse information as a Mercury comment.
    % This is used in HLDS dumps.
    %
:- pred dump_structure_reuse_domain(S::in, var_name_source::in, tvarset::in,
    structure_reuse_domain::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred format_interface_maybe_structure_reuse_domain(S::in,
    var_name_source::in, tvarset::in, maybe(structure_reuse_domain)::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Renaming operations.
%

:- pred rename_unit_selector(tsubst::in, unit_selector::in,
    unit_selector::out) is det.

:- pred rename_selector(tsubst::in, selector::in, selector::out) is det.

:- pred rename_datastruct(map(prog_var, prog_var)::in, tsubst::in,
    datastruct::in, datastruct::out) is det.

:- func rename_datastruct(map(prog_var, prog_var), tsubst, datastruct)
    = datastruct.

:- pred rename_structure_sharing_pair(map(prog_var, prog_var)::in,
    tsubst::in, structure_sharing_pair::in, structure_sharing_pair::out)
    is det.

:- pred rename_structure_sharing(map(prog_var, prog_var)::in,
    tsubst::in, structure_sharing::in, structure_sharing::out) is det.

:- pred rename_structure_sharing_domain(map(prog_var, prog_var)::in,
    tsubst::in, structure_sharing_domain::in,
    structure_sharing_domain::out) is det.

:- pred rename_user_annotated_sharing(list(prog_var)::in, list(prog_var)::in,
    list(mer_type)::in, user_annotated_sharing::in,
    user_annotated_sharing::out) is det.

:- pred rename_structure_reuse_condition(map(prog_var, prog_var)::in,
    tsubst::in, structure_reuse_condition::in,
    structure_reuse_condition::out) is det.

:- pred rename_structure_reuse_conditions(map(prog_var, prog_var)::in,
    tsubst::in, structure_reuse_conditions::in,
    structure_reuse_conditions::out) is det.

:- pred rename_structure_reuse_domain(map(prog_var, prog_var)::in,
    tsubst::in, structure_reuse_domain::in,
    structure_reuse_domain::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.parse_sym_name.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_type_name.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.prog_type_unify.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module int.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_int.
:- import_module term_vars.

%---------------------------------------------------------------------------%
%
% Parsing routines.
%

parse_unit_selector(Term) = UnitSelector :-
    ( if Term = term.functor(term.atom(Cons), Args, _) then
        % XXX We should include non-dummy type_ctors in cons ConsIds.
        % XXX Why do we parse int, float, and string ConsIds when they
        % never have any arguments and thus cannot select anything?
        ( if
            Cons = "sel",
            Args = [ConsTerm, ArityTerm, PosTerm]
        then
            ( if
                try_parse_sym_name_and_no_args(ConsTerm, ConsIdName),
                term_int.decimal_term_to_int(ArityTerm, Arity),
                term_int.decimal_term_to_int(PosTerm, Pos)
            then
                ConsId = du_data_ctor(du_ctor(ConsIdName, Arity,
                    cons_id_dummy_type_ctor)),
                UnitSelector = termsel(ConsId, Pos)
            else if
                % XXX UINT, presuambly we need to handle uints here too.
                term_int.decimal_term_to_int(ConsTerm, Int)
            then
                ConsId = some_int_const(int_const(Int)),
                UnitSelector = termsel(ConsId, 0)
            else if
                ConsTerm = term.functor(term.float(Float), _, _)
            then
                ConsId = float_const(Float),
                UnitSelector = termsel(ConsId, 0)
            else if
                ConsTerm = term.functor(term.string(Str), _, _)
            then
                ConsId = string_const(Str),
                UnitSelector = termsel(ConsId, 0)
            else
                unexpected($pred, "unknown cons_id in unit selector")
            )
        else if
            Cons = "typesel",
            Args = [TypeSelectorTerm]
        then
            ( if
                maybe_parse_type(
                    no_allow_ho_inst_info(wnhii_ctgc_type_selector),
                    term.coerce(TypeSelectorTerm), TypeSelector)
            then
                UnitSelector = typesel(TypeSelector)
            else
                unexpected($pred, "error in parsing type selector")
            )
        else
            unexpected($pred, "selector is neither sel/3 nor typesel/1.")
        )
    else
        unexpected($pred, "term not a functor")
    ).

parse_selector(Term) = Selector :-
    ( if Term = term.functor(term.atom(Cons), Args, _) then
        ( if
            Cons = "[|]",
            Args = [First, Rest]
        then
            Selector = [parse_unit_selector(First) | parse_selector(Rest)]
        else
            Selector = []
        )
    else
        unexpected($pred, "term not a functor")
    ).

parse_datastruct(Term) = Datastruct :-
    ( if
        Term = term.functor(term.atom(Cons), Args, _),
        Cons = "cel",
        Args = [VarTerm, SelectorTerm],
        VarTerm = term.variable(Var, _)
    then
        Datastruct = selected_cel(term.coerce_var(Var),
            parse_selector(SelectorTerm))
    else
        unexpected($pred, "error while parsing datastruct.")
    ).

:- func parse_datastruct_list(term(T)) = list(datastruct).

parse_datastruct_list(Term) = Datastructs :-
    ( if Term = term.functor(term.atom(Cons), Args, _) then
        ( if
            Cons = "[|]",
            Args = [FirstDataTerm, RestDataTerm]
        then
            Datastructs = [parse_datastruct(FirstDataTerm) |
                parse_datastruct_list(RestDataTerm)]
        else if
            Cons = "[]"
        then
            Datastructs = []
        else
            unexpected($pred, "error while parsing list of datastructs")
        )
    else
        unexpected($pred,
            "error while parsing list of datastructs (term not a functor)")
    ).

parse_structure_sharing_pair(Term) = SharingPair :-
    ( if
        Term = term.functor(term.atom(Cons), Args, _),
        Cons = "pair",
        Args = [First, Second]
    then
        SharingPair = parse_datastruct(First) - parse_datastruct(Second)
    else
        unexpected($pred, "error while parsing structure sharing pair")
    ).

parse_structure_sharing(Term) = SharingPairs :-
    ( if
        Term = term.functor(term.atom(Cons), Args, _),
        (
            Cons = "[|]",
            Args = [SharingPairTerm, Rest],
            SharingPairs0 = [parse_structure_sharing_pair(SharingPairTerm) |
                parse_structure_sharing(Rest)]
        ;
            Cons = "[]",
            SharingPairs0 = []
        )
    then
        SharingPairs = SharingPairs0
    else
        unexpected($pred,
            "error while parsing list of structure sharing pairs")
    ).

parse_structure_sharing_domain(Term) = SharingAs :-
    ( if
        Term = term.functor(term.atom(Cons), _, _Context),
        (
            Cons = "[|]",
            SharingAs0 = structure_sharing_real(parse_structure_sharing(Term))
        ;
            Cons = "bottom",
            SharingAs0 = structure_sharing_bottom
        ;
            Cons = "top",
            SharingAs0 = structure_sharing_top(
                set.make_singleton_set(
                    top_cannot_improve("from parse_structure_sharing_domain")))
        )
    then
        SharingAs = SharingAs0
    else
        unexpected($pred, "error while parsing structure sharing domain")
    ).

parse_structure_reuse_condition(Term) = ReuseCondition :-
    ( if Term = term.functor(term.atom(Cons), Args, _) then
        ( if
            Cons = "condition",
            Args = [DeadNodesTerm, InUseNodesTerm, SharingTerm]
        then
            DeadNodesList = parse_datastruct_list(DeadNodesTerm),
            DeadNodes = set.list_to_set(DeadNodesList),
            InUseNodes = parse_datastruct_list(InUseNodesTerm),
            Sharing = parse_structure_sharing_domain(SharingTerm),
            ReuseCondition = structure_reuse_condition(DeadNodes,
                InUseNodes, Sharing)
        else
            unexpected($pred, "error while parsing reuse condition")
        )
    else
        unexpected($pred,
            "error while parsing reuse condition (term not a functor)")
    ).

parse_structure_reuse_conditions(Term) = ReuseConditions :-
    ( if Term = term.functor(term.atom(Cons), Args, _) then
        ( if
            Cons = "[|]",
            Args = [FirstTupleTerm, RestTuplesTerm]
        then
            ReuseConditions =
                [parse_structure_reuse_condition(FirstTupleTerm) |
                parse_structure_reuse_conditions(RestTuplesTerm)]
        else if
            Cons = "[]"
        then
            ReuseConditions = []
        else
            unexpected($pred, "error while parsing reuse conditions")
        )
    else
        unexpected($pred,
            "error while parsing reuse conditions (term not a functor)")
    ).

parse_structure_reuse_domain(Term) = ReuseDomain :-
    ( if
        Term = term.functor(term.atom(Cons), Args, _)
    then
        ( if
            Cons = "has_no_reuse"
        then
            ReuseDomain = has_no_reuse
        else if
            Cons = "has_only_unconditional_reuse"
        then
            ReuseDomain = has_only_unconditional_reuse
        else if
            Cons = "has_conditional_reuse",
            Args = [ReuseConditionsTerm]
        then
            ReuseDomain = has_conditional_reuse(
                parse_structure_reuse_conditions(ReuseConditionsTerm))
        else
            unexpected($pred, "error while parsing reuse domain")
        )
    else
        unexpected($pred,
            "error while parsing reuse domain (term not a functor)")
    ).

%---------------------------------------------------------------------------%

parse_user_annotated_sharing(!.Varset, Term, UserSharing) :-
    (
        Term = term.functor(term.atom("no_sharing"), [], _),
        UserSharing = user_sharing(structure_sharing_bottom, no)
    ;
        Term = term.functor(term.atom("unknown_sharing"), [], Context),
        ContextStr = context_to_string(Context),
        Msg = "user declared top(" ++ ContextStr ++ ")",
        Reason = top_cannot_improve(Msg),
        UserSharing = user_sharing(structure_sharing_top(
            set.make_singleton_set(Reason)), no)
    ;
        Term = term.functor(term.atom("sharing"),
            [TypesTerm, UserSharingTerm], _),
        (
            TypesTerm = term.functor(term.atom("yes"), ListTypeTerms, _),
            maybe_parse_types(no_allow_ho_inst_info(wnhii_user_struct_sharing),
                ListTypeTerms, Types),
            term_vars.vars_in_terms(ListTypeTerms, TypeVars),
            varset.select(set.list_to_set(TypeVars), !Varset),
            MaybeUserTypes = yes(user_type_info(Types,
                varset.coerce(!.Varset)))
        ;
            TypesTerm = term.functor(term.atom("no"), _, _),
            MaybeUserTypes = no
        ),
        parse_user_annotated_sharing_term(UserSharingTerm, Sharing),
        UserSharing = user_sharing(Sharing, MaybeUserTypes)
    ).

:- pred parse_user_annotated_sharing_term(term::in,
    structure_sharing_domain::out) is semidet.

parse_user_annotated_sharing_term(SharingDomainUserTerm, SharingDomain) :-
    get_list_term_arguments(SharingDomainUserTerm, SharingPairTerms),
    (
        SharingPairTerms = [],
        SharingDomain = structure_sharing_bottom
    ;
        SharingPairTerms = [_ | _],
        list.map(parse_user_annotated_sharing_pair_term, SharingPairTerms,
            SharingPairs),
        SharingDomain = structure_sharing_real(SharingPairs)
    ).

:- pred get_list_term_arguments(term::in, list(term)::out) is semidet.

get_list_term_arguments(ListTerm, ArgumentTerms) :-
    ListTerm = term.functor(term.atom(Cons), Args, _),
    (
        Cons = "[|]",
        Args = [FirstTerm, RestTerm],
        get_list_term_arguments(RestTerm, RestList),
        ArgumentTerms = [FirstTerm | RestList]
    ;
        Cons = "[]",
        ArgumentTerms = []
    ).

:- pred parse_user_annotated_sharing_pair_term(term::in,
    structure_sharing_pair::out) is semidet.

parse_user_annotated_sharing_pair_term(Term, SharingPair) :-
    Term = term.functor(term.atom("-"), [Left, Right], _),
    parse_user_annotated_datastruct_term(Left, LeftData),
    parse_user_annotated_datastruct_term(Right, RightData),
    SharingPair = LeftData - RightData.

:- pred parse_user_annotated_datastruct_term(term::in, datastruct::out)
    is semidet.

parse_user_annotated_datastruct_term(Term, Datastruct) :-
    Term = term.functor(term.atom("cel"), [VarTerm, TypesTerm], _),
    VarTerm = term.variable(GenericVar, _),
    term.coerce_var(GenericVar, ProgVar),
    get_list_term_arguments(TypesTerm, TypeTermsList),
    maybe_parse_types(no_allow_ho_inst_info(wnhii_user_struct_sharing),
        TypeTermsList, Types),
    list.map(mer_type_to_typesel, Types, Selector),
    Datastruct = selected_cel(ProgVar, Selector).

:- pred mer_type_to_typesel(mer_type::in, unit_selector::out) is det.

mer_type_to_typesel(Type, typesel(Type)).

%---------------------------------------------------------------------------%
%
% Printing routines.
%

:- pred format_selector(S::in, tvarset::in, selector::in,
    U::di, U::uo) is det <= pt_output(S, U).

format_selector(S, TVarSet, Selector, !U) :-
    add_string(selector_to_string(TVarSet, Selector), S, !U).

:- func selector_to_string(tvarset, selector) = string.

selector_to_string(TVarSet, Selector) = String :-
    (
        Selector = [],
        String = "[]"
    ;
        Selector = [_ | _],
        SelectorStrings = list.map(unit_selector_to_string(TVarSet),
            Selector),
        string.append_list(["[", string.join_list(",", SelectorStrings), "]"],
            String)
    ).

:- func unit_selector_to_string(tvarset, unit_selector) = string.

unit_selector_to_string(_, termsel(ConsId, Index)) =
    string.append_list(["sel(",
        mercury_cons_id_to_string(output_mercury, needs_brackets, ConsId),
        ",",
        int_to_string(cons_id_arity(ConsId)),
        ",",
        int_to_string(Index),
        ")"]).

unit_selector_to_string(TVarSet, typesel(TypeSel)) =
    string.append_list(["typesel(",
        mercury_type_to_string(TVarSet, print_name_only, TypeSel),
        ")"]).

%---------------------%

:- pred format_datastructs(S::in, var_name_source::in, tvarset::in,
    list(datastruct)::in, U::di, U::uo) is det <= pt_output(S, U).

format_datastructs(S, VarNameSrc, TypeVarSet, Datastructs, !U) :-
    add_string("[", S, !U),
    list.gap_foldl(format_datastruct(S, VarNameSrc, TypeVarSet),
        add_string(", ", S), Datastructs, !U),
    add_string("]", S, !U).

:- pred format_datastruct(S::in, var_name_source::in, tvarset::in,
    datastruct::in, U::di, U::uo) is det <= pt_output(S, U).

format_datastruct(S, VarNameSrc, TypeVarSet, DataStruct, !U) :-
    lookup_var_name_in_source(VarNameSrc, DataStruct ^ sc_var, VarName),
    add_string("cel(", S, !U),
    add_string(VarName, S, !U),
    add_string(", ", S, !U),
    format_selector(S, TypeVarSet, DataStruct ^ sc_selector, !U),
    add_string(")", S, !U).

%---------------------%

:- pred format_structure_sharing_pair(S::in, var_name_source::in, tvarset::in,
    structure_sharing_pair::in, U::di, U::uo) is det <= pt_output(S, U).

format_structure_sharing_pair(S, VarNameSrc, TypeVarSet, SharingPair, !U) :-
    SharingPair = D1 - D2,
    add_string("pair(", S, !U),
    format_datastruct(S, VarNameSrc, TypeVarSet, D1, !U),
    add_string(", ", S, !U),
    format_datastruct(S, VarNameSrc, TypeVarSet, D2, !U),
    add_string(")", S, !U).

    % Print list of structure sharing pairs.
    %
    % format_structure_sharing(Varset, TVarset, MaybeThreshold,
    %   StartingString, Separator, EndingString, SharingPairs, !IO):
    %
    % Print the list of sharing pairs using StartString to precede the
    % list, using EndString to end the list, using Separator to separate
    % each of the sharing pairs occurring in the list. If a threshold, say
    % N, is given, then only the first N pairs are printed (and "..."
    % is shown if there are more than N pairs).
    %
:- pred format_structure_sharing(S::in, var_name_source::in, tvarset::in,
    maybe(int)::in, string::in, string::in, string::in, structure_sharing::in,
    U::di, U::uo) is det <= pt_output(S, U).

format_structure_sharing(S, VarNameSrc, TypeVarSet, MaybeLimit,
        Start, Sep, End, SharingPairs0, !U) :-
    (
        MaybeLimit = yes(Limit),
        list.take_upto(Limit, SharingPairs0, SharingPairs),
        ( if Limit >= list.length(SharingPairs0) then
            CompleteList = yes
        else
            CompleteList = no
        )
    ;
        MaybeLimit = no,
        SharingPairs = SharingPairs0,
        CompleteList = yes
    ),
    add_string(Start, S, !U),
    list.gap_foldl(format_structure_sharing_pair(S, VarNameSrc, TypeVarSet),
        add_string(Sep, S), SharingPairs, !U),
    (
        CompleteList = no,
        add_string(Sep, S, !U),
        add_string("...", S, !U)
    ;
        CompleteList = yes
    ),
    add_string(End, S, !U).

    % Print complete list of structure sharing pairs as a list (using "[",
    % ",", and "]"). This can later be parsed automatically.
    %
:- pred format_structure_sharing_as_list(S::in, var_name_source::in,
    tvarset::in, structure_sharing::in, U::di, U::uo) is det
    <= pt_output(S, U).
:- pragma consider_used(pred(format_structure_sharing_as_list/6)).

format_structure_sharing_as_list(S, VarNameSrc, TypeVarSet,
        SharingPairs, !U) :-
    format_structure_sharing(S, VarNameSrc, TypeVarSet, no, "[", ",", "]",
        SharingPairs, !U).

    % Print structure sharing domain.
    %
    % format_structure_sharing_domain(P, T, VerboseTop, MaybeThreshold,
    %   Sharing, !IO):
    %
    % If VerboseTop = yes, then the full list of reasons why sharing is
    % top is printed as "top([ ... Messages ... ])". If VerboseTop = no,
    % then top is printed as "top".
    % If a threshold is given, say N, then only the first N structure
    % sharing pairs are printed.
    %
    % The output can later be parsed again only if VerboseTop = no and
    % MaybeThreshold = no.
    %
:- pred format_structure_sharing_domain(S::in, var_name_source::in,
    tvarset::in, bool::in, maybe(int)::in, structure_sharing_domain::in,
    U::di, U::uo) is det <= pt_output(S, U).

format_structure_sharing_domain(S, VarNameSrc, TypeVarSet, VerboseTop,
        MaybeThreshold, SharingAs, !U) :-
    do_format_structure_sharing_domain(S, VarNameSrc, TypeVarSet,
        VerboseTop, MaybeThreshold, "", ", ", "", SharingAs, !U).

:- pred do_format_structure_sharing_domain(S::in, var_name_source::in,
    tvarset::in, bool::in, maybe(int)::in, string::in, string::in, string::in,
    structure_sharing_domain::in, U::di, U::uo) is det <= pt_output(S, U).

do_format_structure_sharing_domain(S, VarNameSrc, TypeVarSet, VerboseTop,
        MaybeThreshold, Start, Separator, End, SharingAs, !U) :-
    add_string(Start, S, !U),
    (
        SharingAs = structure_sharing_top(Msgs),
        (
            VerboseTop = no,
            add_string("top", S, !U)
        ;
            VerboseTop = yes,
            MsgStrs = list.map(string.string, set.to_sorted_list(Msgs)),
            add_string("top([", S, !U),
            add_list(add_string, Separator, MsgStrs, S, !U),
            add_string("])", S, !U)
        )
    ;
        SharingAs = structure_sharing_bottom,
        add_string("bottom", S, !U)
    ;
        SharingAs = structure_sharing_real(SharingPairs),
        format_structure_sharing(S, VarNameSrc, TypeVarSet,
            MaybeThreshold, "[", Separator, "]", SharingPairs, !U)
    ),
    add_string(End, S, !U).

dump_structure_sharing_domain(S, VarNameSrc, TypeVarSet, SharingAs, !U) :-
    do_format_structure_sharing_domain(S, VarNameSrc, TypeVarSet, yes, no,
        "%\t ", "\n%\t", "\n", SharingAs, !U).

format_interface_structure_sharing_domain(S, VarNameSrc, TypeVarSet,
        MaybeSharingAs, !U) :-
    (
        MaybeSharingAs = no,
        add_string("not_available", S, !U)
    ;
        MaybeSharingAs = yes(SharingAs),
        add_string("yes(", S, !U),
        format_structure_sharing_domain(S, VarNameSrc, TypeVarSet,
            no, no, SharingAs, !U),
        add_string(")", S, !U)
    ).

dump_structure_reuse_domain(S, VarNameSrc, TypeVarSet, ReuseAs, !U) :-
    format_structure_reuse_domain(S, VarNameSrc, TypeVarSet, ReuseAs,
        "%\t ", ", \n%\t ", "\n", !U).

:- pred format_structure_reuse_condition(S::in, var_name_source::in,
    tvarset::in, structure_reuse_condition::in, U::di, U::uo) is det
    <= pt_output(S, U).

format_structure_reuse_condition(S, VarNameSrc, TypeVarSet, ReuseCond, !U) :-
    ReuseCond = structure_reuse_condition(DeadNodes, InUseNodes, Sharing),
    DeadNodesList = set.to_sorted_list(DeadNodes),
    add_string("condition(", S, !U),
    format_datastructs(S, VarNameSrc, TypeVarSet, DeadNodesList, !U),
    add_string(", ", S, !U),
    format_datastructs(S, VarNameSrc, TypeVarSet, InUseNodes, !U),
    add_string(", ", S, !U),
    format_structure_sharing_domain(S, VarNameSrc, TypeVarSet, no, no,
        Sharing, !U),
    add_string(")", S, !U).

:- pred format_structure_reuse_conditions(S::in, var_name_source::in,
    tvarset::in, string::in, list(structure_reuse_condition)::in,
    U::di, U::uo) is det <= pt_output(S, U).

format_structure_reuse_conditions(S, VarNameSrc, TypeVarSet, Separator,
        ReuseConds, !U) :-
    list.gap_foldl(
        format_structure_reuse_condition(S, VarNameSrc, TypeVarSet),
        add_string(Separator, S), ReuseConds, !U).

:- pred format_structure_reuse_domain(S::in, var_name_source::in, tvarset::in,
    structure_reuse_domain::in, string::in, string::in, string::in,
    U::di, U::uo) is det <= pt_output(S, U).

format_structure_reuse_domain(S, VarNameSrc, TypeVarSet, ReuseDomain,
        Start, Separator, End, !U) :-
    add_string(Start, S, !U),
    (
        ReuseDomain = has_no_reuse,
        add_string("has_no_reuse", S, !U)
    ;
        ReuseDomain = has_only_unconditional_reuse,
        add_string("has_only_unconditional_reuse", S, !U)
    ;
        ReuseDomain = has_conditional_reuse(ReuseConditions),
        add_string("has_conditional_reuse([", S, !U),
        format_structure_reuse_conditions(S, VarNameSrc, TypeVarSet,
            Separator, ReuseConditions, !U),
        add_string("])", S, !U)
    ),
    add_string(End, S, !U).

format_interface_maybe_structure_reuse_domain(S, VarNameSrc, TypeVarSet,
        MaybeReuseDomain, !U) :-
    (
        MaybeReuseDomain = no,
        add_string("not_available", S, !U)
    ;
        MaybeReuseDomain = yes(ReuseDomain),
        add_string("yes(", S, !U),
        format_structure_reuse_domain(S, VarNameSrc, TypeVarSet,
            ReuseDomain, "", ", ", "", !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%
%
% Renaming operations.
%

rename_unit_selector(Subst, !UnitSelector) :-
    (
        !.UnitSelector = termsel(_,_)
    ;
        !.UnitSelector = typesel(Type0),
        prog_type_subst.apply_subst_to_type(Subst, Type0, Type),
        !:UnitSelector = typesel(Type)
    ).

rename_selector(TypeSubst, !Selector) :-
    list.map(rename_unit_selector(TypeSubst), !Selector).

rename_datastruct(Dict, Subst, !Data) :-
    !.Data = selected_cel(Var0, Sel0),
    map.lookup(Dict, Var0, Var),
    rename_selector(Subst, Sel0, Sel),
    !:Data = selected_cel(Var, Sel).

rename_datastruct(Dict, Subst, Data0) = Data :-
    rename_datastruct(Dict, Subst, Data0, Data).

rename_structure_sharing_pair(Dict, TypeSubst, !Pair) :-
    !.Pair = D1 - D2,
    rename_datastruct(Dict, TypeSubst, D1, Da),
    rename_datastruct(Dict, TypeSubst, D2, Db),
    !:Pair = Da - Db.

rename_structure_sharing(Dict, TypeSubst, !List) :-
    list.map(rename_structure_sharing_pair(Dict, TypeSubst), !List).

rename_structure_sharing_domain(_, _, X @ structure_sharing_bottom, X).
rename_structure_sharing_domain(_, _, X @ structure_sharing_top(_), X).
rename_structure_sharing_domain(Dict, TypeSubst,
        structure_sharing_real(!.List), structure_sharing_real(!:List)):-
    rename_structure_sharing(Dict, TypeSubst, !List).

%---------------------------------------------------------------------------%

rename_user_annotated_sharing(HeadVars, NewHeadVars, NewTypes,
        !UserSharing) :-
    (
        !.UserSharing = no_user_annotated_sharing
    ;
        !.UserSharing = user_sharing(Sharing, MaybeTypes),
        some [!SharingDomain] (
            !:SharingDomain = Sharing,
            (
                !.SharingDomain = structure_sharing_bottom
            ;
                !.SharingDomain = structure_sharing_top(_)
            ;
                !.SharingDomain = structure_sharing_real(SharingPairs),
                map.from_corresponding_lists(HeadVars, NewHeadVars,
                    VarRenaming),
                ( if
                    MaybeTypes = yes(user_type_info(UserSharingTypes,
                        _UserSharingTVarSet))
                then
                    type_list_subsumes_det(UserSharingTypes, NewTypes,
                        TypeSubst)
                else
                    TypeSubst = map.init
                ),
                rename_structure_sharing(VarRenaming, TypeSubst,
                        SharingPairs, NewSharingPairs),
                !:SharingDomain = structure_sharing_real(NewSharingPairs)
            ),
            !:UserSharing = user_sharing(!.SharingDomain, no)
        )
    ).

%---------------------------------------------------------------------------%

rename_structure_reuse_condition(Dict, TypeSubst,
        structure_reuse_condition(DeadNodes, LiveNodes, Sharing),
        structure_reuse_condition(RenDeadNodes, RenLiveNodes, RenSharing)) :-
    RenDeadNodes = set.map(rename_datastruct(Dict, TypeSubst), DeadNodes),
    RenLiveNodes = list.map(rename_datastruct(Dict, TypeSubst), LiveNodes),
    rename_structure_sharing_domain(Dict, TypeSubst, Sharing, RenSharing).

rename_structure_reuse_conditions(Dict, TypeSubst, Conds, RenConds) :-
    list.map(rename_structure_reuse_condition(Dict, TypeSubst),
        Conds, RenConds).

rename_structure_reuse_domain(_, _, has_no_reuse, has_no_reuse).
rename_structure_reuse_domain(_, _, has_only_unconditional_reuse,
        has_only_unconditional_reuse).
rename_structure_reuse_domain(Dict, TypeSubst, has_conditional_reuse(Conds),
        has_conditional_reuse(RenConds)):-
    rename_structure_reuse_conditions(Dict, TypeSubst, Conds, RenConds).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_ctgc.
%---------------------------------------------------------------------------%
