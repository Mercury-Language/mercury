%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021-2024 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_type_table.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.hlds_out_util.

:- import_module bool.
:- import_module io.
:- import_module string.
:- import_module string.builder.

:- pred write_type_table(hlds_out_info::in,  io.text_output_stream::in,
    bool::in, type_table::in, io::di, io::uo) is det.
:- pred format_type_table(hlds_out_info::in, bool::in, type_table::in,
    string.builder.state::di, string.builder.state::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_item.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_tree_out_type_repn.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module term.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

write_type_table(Info, Stream, LocalOnly, TypeTable, !IO) :-
    State0 = string.builder.init,
    format_type_table(Info, LocalOnly, TypeTable, State0, State),
    Str = string.builder.to_string(State),
    io.write_string(Stream, Str, !IO).

format_type_table(Info, LocalOnly, TypeTable, !State) :-
    string.builder.append_string("%-------- Types --------\n", !State),
    get_all_type_ctor_defns(TypeTable, TypeAssocList),
    list.sort(TypeAssocList, SortedTypeAssocList),
    (
        LocalOnly = no,
        PrintedTypeAssocList = SortedTypeAssocList
    ;
        LocalOnly = yes,
        list.filter(type_table_entry_is_local, SortedTypeAssocList,
            PrintedTypeAssocList)
    ),
    format_type_table_entries(Info, PrintedTypeAssocList, !State),
    string.builder.append_string("\n", !State).

:- pred type_table_entry_is_local(pair(type_ctor, hlds_type_defn)::in)
    is semidet.

type_table_entry_is_local(_TypeCtor - TypeDefn) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    type_status_defined_in_this_module(TypeStatus) = yes.

:- pred format_type_table_entries(hlds_out_info::in,
    assoc_list(type_ctor, hlds_type_defn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_type_table_entries(_, [], !State).
format_type_table_entries(Info, [Type | Types], !State) :-
    format_type_table_entry(Info, Type, !State),
    format_type_table_entries(Info, Types, !State).

:- pred format_type_table_entry(hlds_out_info::in,
    pair(type_ctor, hlds_type_defn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_type_table_entry(Info, TypeCtor - TypeDefn, !State) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    hlds_data.get_type_defn_context(TypeDefn, Context),
    % Write the context.
    string.builder.append_char('\n', !State),
    maybe_format_context_comment(0u, "", Context, !State),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'c') then
        string.builder.format("%% status %s\n",
            [s(type_import_status_to_string(TypeStatus))], !State)
    else
        true
    ),
    ( if
        ( TypeBody = hlds_solver_type(_)
        ; TypeBody = hlds_abstract_type(abstract_solver_type)
        )
    then
        string.builder.append_string(":- solver type ", !State)
    else
        string.builder.append_string(":- type ", !State)
    ),
    format_type_name(string.builder.handle, TypeCtor, !State),
    format_type_params(TVarSet, TypeParams, !State),
    format_type_body(Info, TypeCtor, TypeBody, TVarSet, !State).

:- pred format_type_params(tvarset::in, list(type_param)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_type_params(TVarSet, TypeParams, !State) :-
    (
        TypeParams = []
    ;
        TypeParams = [HeadParam | TailParams],
        string.builder.append_string("(", !State),
        mercury_format_var_vs(TVarSet, print_name_only, HeadParam,
            string.builder.handle, !State),
        format_comma_type_params_loop(TVarSet, TailParams, !State),
        string.builder.append_string(")", !State)
    ).

:- pred format_comma_type_params_loop(tvarset::in, list(type_param)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_comma_type_params_loop(_TVarSet, [], !State).
format_comma_type_params_loop(TVarSet, [Param | Params], !State) :-
    string.builder.append_string(", ", !State),
    mercury_format_var_vs(TVarSet, print_name_only, Param,
        string.builder.handle, !State),
    format_comma_type_params_loop(TVarSet, Params, !State).

:- pred format_type_body(hlds_out_info::in,
    type_ctor::in, hlds_type_body::in, tvarset::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_type_body(Info, _TypeCtor, TypeBody, TVarSet, !State) :-
    BaseIndent = 1u,
    IndentStr = indent2_string(BaseIndent),
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(Ctors, MaybeSuperType, MaybeUserEqComp,
            MaybeRepn, Foreign),
        string.builder.append_string("\n", !State),
        (
            MaybeSuperType = subtype_of(SuperType),
            SuperTypeStr = mercury_type_to_string(TVarSet,
                print_name_only, SuperType),
            string.builder.format("%s%% subtype of %s\n",
                [s(IndentStr), s(SuperTypeStr)], !State)
        ;
            MaybeSuperType = not_a_subtype
        ),
        MaybeSolverTypeDetails = no,
        MercInfo = Info ^ hoi_merc_out_info,
        (
            MaybeRepn = no,
            Ctors = one_or_more(HeadCtor, TailCtors),
            format_constructors(TVarSet, HeadCtor, TailCtors, !State),
            MaybeDirectArgCtors = no,
            mercury_format_where_attributes(MercInfo, TVarSet,
                MaybeSolverTypeDetails, MaybeUserEqComp, MaybeDirectArgCtors,
                string.builder.handle, !State),
            string.builder.format(
                "%s%% no type representation information yet\n",
                [s(IndentStr)], !State)
        ;
            MaybeRepn = yes(Repn),
            Repn = du_type_repn(CtorRepns, CtorRepnMap, CheaperTagTest,
                DuTypeKind, MaybeDirectArgCtors),
            format_constructor_repns(TVarSet, CtorRepns, !State),
            (
                CheaperTagTest = no_cheaper_tag_test
            ;
                CheaperTagTest = cheaper_tag_test(ExpConsId, ExpConsTag,
                    CheapConsId, CheapConsTag),
                ExpConsIdStr = cons_id_and_arity_to_string(
                    unqual_cons_id(ExpConsId)),
                CheapConsIdStr = cons_id_and_arity_to_string(
                    unqual_cons_id(CheapConsId)),
                string.builder.format("%s%% cheaper tag test:\n",
                    [s(IndentStr)], !State),
                string.builder.format("%s%%   from %s\n",
                    [s(IndentStr), s(ExpConsIdStr)], !State),
                string.builder.format("%s%%      %s\n",
                    [s(IndentStr), s(du_cons_tag_to_string(ExpConsTag))],
                    !State),
                string.builder.format("%s%%   to %s\n",
                    [s(IndentStr), s(CheapConsIdStr)], !State),
                string.builder.format("%s%%      %s\n",
                    [s(IndentStr), s(du_cons_tag_to_string(CheapConsTag))],
                    !State)
            ),
            (
                DuTypeKind = du_type_kind_mercury_enum,
                string.builder.format("%s%% KIND enumeration\n",
                    [s(IndentStr)], !State)
            ;
                DuTypeKind = du_type_kind_foreign_enum(Lang),
                string.builder.format("%s%% KIND foreign enumeration for %s\n",
                    [s(IndentStr), s(foreign_language_string(Lang))], !State)
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                string.builder.format("%s%% KIND dummy\n",
                    [s(IndentStr)], !State)
            ;
                DuTypeKind = du_type_kind_notag(FunctorName, ArgType,
                    MaybeArgName),
                ArgTypeStr = mercury_type_to_string(TVarSet, print_name_only,
                    ArgType),
                (
                    MaybeArgName = yes(ArgName)
                ;
                    MaybeArgName = no,
                    ArgName = "no arg name"
                ),
                string.builder.format("%s%% KIND notag: %s, %s, %s\n",
                    [s(IndentStr), s(escaped_sym_name_to_string(FunctorName)),
                    s(ArgTypeStr), s(ArgName)], !State)
            ;
                DuTypeKind = du_type_kind_general,
                string.builder.format("%s%% KIND general\n",
                    [s(IndentStr)], !State)
            ),
            mercury_format_where_attributes(MercInfo, TVarSet,
                MaybeSolverTypeDetails, MaybeUserEqComp, MaybeDirectArgCtors,
                string.builder.handle, !State),
            (
                Foreign = yes(_),
                string.builder.format("%s%% has foreign_type\n",
                    [s(IndentStr)], !State)
            ;
                Foreign = no
            ),
            trace [compile_time(flag("ctor_repn_invariant_check")), io(!IO)] (
                list.sort(CtorRepns, SortedCtorRepns),
                map.foldl_values(accumulate_ctor_repns, CtorRepnMap,
                    [], MapCtorRepns),
                list.sort(MapCtorRepns, SortedMapCtorRepns),
                ( if SortedCtorRepns = SortedMapCtorRepns then
                    true
                else
                    io.output_stream(Stream, !IO),
                    io.format(Stream,
                        "%s%% BUG SortedCtorRepns != SortedMapCtorRepns\n",
                        [s(IndentStr)], !IO)
                )
            )
        )
    ;
        TypeBody = hlds_eqv_type(Type),
        string.builder.append_string(" == ", !State),
        mercury_format_type(TVarSet, print_name_only, Type,
            string.builder.handle, !State),
        string.builder.append_string(".\n", !State)
    ;
        TypeBody = hlds_abstract_type(_IsSolverType),
        string.builder.append_string(".\n", !State)
    ;
        TypeBody = hlds_foreign_type(ForeignTypeBody),
        ForeignTypeBody = foreign_type_body(MaybeC, MaybeJava, MaybeCsharp),
        (
            MaybeC = no,
            MaybeCStr = "no_c"
        ;
            MaybeC = yes(C),
            C = type_details_foreign(c_type(CTypeName),
                CCanonical, CAssertions),
            MaybeCStr = string.format("c(%s, %s, %s)",
                [s(CTypeName),
                s(maybe_canonical_to_simple_string(CCanonical)),
                s(foreign_type_assertions_to_simple_string(CAssertions))])
        ),
        (
            MaybeJava = no,
            MaybeJavaStr = "no_java"
        ;
            MaybeJava = yes(Java),
            Java = type_details_foreign(java_type(JavaTypeName),
                JavaCanonical, JavaAssertions),
            MaybeJavaStr = string.format("java(%s, %s, %s)",
                [s(JavaTypeName),
                s(maybe_canonical_to_simple_string(JavaCanonical)),
                s(foreign_type_assertions_to_simple_string(JavaAssertions))])
        ),
        (
            MaybeCsharp = no,
            MaybeCsharpStr = "no_csharp"
        ;
            MaybeCsharp = yes(Csharp),
            Csharp = type_details_foreign(csharp_type(CsharpTypeName),
                CsharpCanonical, CsharpAssertions),
            MaybeCsharpStr = string.format("csharp(%s, %s, %s)",
                [s(CsharpTypeName),
                s(maybe_canonical_to_simple_string(CsharpCanonical)),
                s(foreign_type_assertions_to_simple_string(CsharpAssertions))])
        ),
        % What we output is not valid Mercury syntax, but it is easier
        % to read than valid Mercury syntax would be.
        Indent1Str = indent2_string(BaseIndent + 1u),
        string.builder.format(" is foreign_type(\n%s%s,\n%s%s,\n%s%s\n%s).\n",
            [s(Indent1Str), s(MaybeCStr),
            s(Indent1Str), s(MaybeJavaStr),
            s(Indent1Str), s(MaybeCsharpStr),
            s(IndentStr)], !State)
    ;
        TypeBody = hlds_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(SolverTypeDetails, MaybeUserEqComp),
        MercInfo = Info ^ hoi_merc_out_info,
        mercury_format_where_attributes(MercInfo, TVarSet,
            yes(SolverTypeDetails), MaybeUserEqComp, no,
            string.builder.handle, !State),
        string.builder.append_string(".\n", !State)
    ).

:- func unqual_cons_id(cons_id) = cons_id.

unqual_cons_id(ConsId) = UnQualConsId :-
    ( if ConsId = cons(SymName, Arity, TypeCtor) then
        UnQualConsId =
            cons(unqualified(unqualify_name(SymName)), Arity, TypeCtor)
    else
        UnQualConsId = ConsId
    ).

:- func maybe_canonical_to_simple_string(maybe_canonical) = string.

maybe_canonical_to_simple_string(MaybeCanonical) = String :-
    (
        MaybeCanonical = canon,
        String = "canon"
    ;
        MaybeCanonical = noncanon(NonCanonical),
        (
            NonCanonical = noncanon_uni_cmp(EqSymName, CmpSymName),
            String = string.format("eq_cmp(%s, %s)",
                [s(sym_name_to_string(EqSymName)),
                s(sym_name_to_string(CmpSymName))])
        ;
            NonCanonical = noncanon_uni_only(EqSymName),
            String = string.format("eq(%s)",
                [s(sym_name_to_string(EqSymName))])
        ;
            NonCanonical = noncanon_cmp_only(CmpSymName),
            String = string.format("cmp(%s)",
                [s(sym_name_to_string(CmpSymName))])
        ;
            NonCanonical = noncanon_abstract(IsSolver),
            (
                IsSolver = non_solver_type,
                String = "noncanon_abstract"
            ;
                IsSolver = solver_type,
                String = "noncanon_abstract_solver"
            )
        ;
            NonCanonical = noncanon_subtype,
            String = "noncanon_subtype"
        )
    ).

:- func foreign_type_assertions_to_simple_string(foreign_type_assertions)
    = string.

foreign_type_assertions_to_simple_string(ForeignTypeAssertions) = String :-
    ForeignTypeAssertions = foreign_type_assertions(AssertionSet),
    Assertions = set.to_sorted_list(AssertionSet),
    AssertionStrs = list.map(foreign_type_assertion_to_string, Assertions),
    String = "[" ++ string.join_list(", ", AssertionStrs) ++ "]".

:- pred accumulate_ctor_repns(one_or_more(constructor_repn)::in,
    list(constructor_repn)::in, list(constructor_repn)::out) is det.

accumulate_ctor_repns(one_or_more(HeadCR, TailCRs), !AccCRs) :-
    !:AccCRs = [HeadCR | TailCRs] ++ !.AccCRs.

%---------------------%

:- pred format_constructors(tvarset::in, constructor::in, list(constructor)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constructors(TVarSet, HeadCtor, TailCtors, !State) :-
    ArrowOrSemi0 = "--->    ",
    format_constructors_loop(TVarSet, ArrowOrSemi0,
        HeadCtor, TailCtors, !State).

:- pred format_constructor_repns(tvarset::in, list(constructor_repn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constructor_repns(TVarSet, CtorRepns, !State) :-
    (
        CtorRepns = [],
        unexpected($pred, "empty constructor list")
    ;
        CtorRepns = [HeadCtorRepn | TailCtorRepns],
        ArrowOrSemi0 = "--->    ",
        format_constructor_repns_loop(TVarSet, ArrowOrSemi0,
            HeadCtorRepn, TailCtorRepns, !State)
    ).

%---------------------%

:- pred format_constructors_loop(tvarset::in, string::in,
    constructor::in, list(constructor)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constructors_loop(TVarSet, ArrowOrSemi0, HeadCtor, TailCtors, !State) :-
    format_indent2(1u, !State),
    string.builder.append_string(ArrowOrSemi0, !State),
    (
        TailCtors = [],
        format_ctor(TVarSet, HeadCtor, !State)
    ;
        TailCtors = [HeadTailCtor | TailTailCtors],
        format_ctor(TVarSet, HeadCtor, !State),
        ArrowOrSemi = ";       ",
        format_constructors_loop(TVarSet, ArrowOrSemi,
            HeadTailCtor, TailTailCtors, !State)
    ).

:- pred format_constructor_repns_loop(tvarset::in,
    string::in, constructor_repn::in, list(constructor_repn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_constructor_repns_loop(TVarSet, ArrowOrSemi0,
        HeadCtorRepn, TailCtorRepns, !State) :-
    format_indent2(1u, !State),
    string.builder.append_string(ArrowOrSemi0, !State),
    (
        TailCtorRepns = [],
        format_ctor_repn(TVarSet, HeadCtorRepn, !State)
    ;
        TailCtorRepns = [HeadTailCtorRepn | TailTailCtorRepns],
        format_ctor_repn(TVarSet, HeadCtorRepn, !State),
        ArrowOrSemi = ";       ",
        format_constructor_repns_loop(TVarSet, ArrowOrSemi,
            HeadTailCtorRepn, TailTailCtorRepns, !State)
    ).

%---------------------%

:- pred format_ctor(tvarset::in, constructor::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_ctor(TVarSet, Ctor, !State) :-
    % NOTE The code of this predicate is almost identical to the code of
    % format_ctor_repn below and format_ctor in parse_tree_out.m.
    % Any changes made here will probably need to be made there as well.
    Ctor = ctor(_Ordinal, MaybeExistConstraints, SymName, Args, Arity, _Ctxt),

    % The module name in SymName must be the same as the module qualifier
    % of the type_ctor, so there is no point in printing it.
    Name = unqualify_name(SymName),
    NameStr =
        mercury_bracketed_atom_to_string(not_next_to_graphic_token, Name),
    % The width of ArrowOrSemi is eight spaces, which is the same as
    % four indents. This comes after the original one indent.
    BaseIndent = 1u,
    ASIndent = 4u,
    BaseASIndentStr = indent2_string(BaseIndent + ASIndent),
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet,
        BaseASIndentStr, "\n", MaybeExistConstraints,
        ExistConstraintsPrefix, ExistConstraintsSuffix),
    maybe_brace_for_name_prefix_suffix(Arity, Name, BracePrefix, BraceSuffix),
    string.builder.append_string(ExistConstraintsPrefix, !State),
    (
        Args = [],
        string.builder.format("%s%s%s",
            [s(BracePrefix), s(NameStr), s(BraceSuffix)], !State)
    ;
        Args = [HeadArg | TailArgs],
        string.builder.format("%s%s(\n", [s(BracePrefix), s(NameStr)], !State),
        AnyFieldName = does_any_arg_have_a_field_name(Args),
        BaseASIndent1Str = indent2_string(BaseIndent + ASIndent + 1u),
        format_ctor_args(TVarSet, BaseASIndent1Str, AnyFieldName,
            HeadArg, TailArgs, !State),
        string.builder.format("%s)%s\n",
            [s(BaseASIndentStr), s(BraceSuffix)], !State)
    ),
    string.builder.format("%s%s\n",
        [s(BraceSuffix), s(ExistConstraintsSuffix)], !State).

:- pred format_ctor_repn(tvarset::in, constructor_repn::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_ctor_repn(TVarSet, CtorRepn, !State) :-
    % NOTE The code of this predicate is almost identical to the code of
    % format_ctor_repn below and format_ctor in parse_tree_out.m.
    % Any changes made here will probably need to be made there as well.
    CtorRepn = ctor_repn(_Ordinal, MaybeExistConstraints, SymName,
        ConsTag, ArgRepns, Arity, _Ctxt),

    % The module name in SymName must be the same as the module qualifier
    % of the type_ctor, so there is no point in printing it.
    Name = unqualify_name(SymName),
    NameStr =
        mercury_bracketed_atom_to_string(not_next_to_graphic_token, Name),
    % The width of ArrowOrSemi is eight spaces, which is the same as
    % four indents. This comes after the original one indent.
    BaseIndent = 1u,
    ASIndent = 4u,
    BaseASIndentStr = indent2_string(BaseIndent + ASIndent),
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet,
        BaseASIndentStr, "\n", MaybeExistConstraints,
        ExistConstraintsPrefix, ExistConstraintsSuffix),
    maybe_brace_for_name_prefix_suffix(Arity, Name, BracePrefix, BraceSuffix),
    string.builder.append_string(ExistConstraintsPrefix, !State),
    string.builder.append_string(BracePrefix, !State),
    ConsTagString = string.format("%s%% tag: %s\n",
        [s(BaseASIndentStr), s(du_cons_tag_to_string(ConsTag))]),
    (
        ArgRepns = [],
        string.builder.format("%s%s%s\n%s",
            [s(BracePrefix), s(NameStr), s(BraceSuffix), s(ConsTagString)],
            !State)
    ;
        ArgRepns = [HeadArgRepn | TailArgRepns],
        BaseASIndent1Str = indent2_string(BaseIndent + ASIndent + 1u),
        string.builder.format("%s%s(\n%s",
            [s(BracePrefix), s(NameStr), s(ConsTagString)], !State),
        AnyFieldName = does_any_arg_repn_have_a_field_name(ArgRepns),
        format_ctor_arg_repns(TVarSet, BaseASIndent1Str,
            AnyFieldName, 1, HeadArgRepn, TailArgRepns, !State),
        string.builder.format("%s)%s\n",
            [s(BaseASIndentStr), s(BraceSuffix)], !State)
    ),
    string.builder.append_string(ExistConstraintsSuffix, !State).

%---------------------%

:- pred format_ctor_args(tvarset::in, string::in, bool::in,
    constructor_arg::in, list(constructor_arg)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_ctor_args(TVarSet, IndentStr, AnyFieldName,
        HeadArg, TailArgs, !State) :-
    HeadArg = ctor_arg(MaybeFieldName, Type, _Context),
    string.builder.append_string(IndentStr, !State),
    (
        AnyFieldName = no
    ;
        AnyFieldName = yes,
        (
            MaybeFieldName = no,
            string.builder.format("%24s", [s("")], !State)
        ;
            MaybeFieldName = yes(ctor_field_name(FieldName, _Ctxt)),
            string.builder.format("%-20s :: ",
                [s(unqualify_name(FieldName))], !State)
        )
    ),
    mercury_format_type(TVarSet, print_name_only, Type,
        string.builder.handle, !State),
    (
        TailArgs = [],
        string.builder.append_string("\n", !State)
    ;
        TailArgs = [HeadTailArg | TailTailArgs],
        string.builder.append_string(",\n", !State),
        format_ctor_args(TVarSet, IndentStr, AnyFieldName,
            HeadTailArg, TailTailArgs, !State)
    ).

:- pred format_ctor_arg_repns(tvarset::in, string::in, bool::in,
    int::in, constructor_arg_repn::in, list(constructor_arg_repn)::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_ctor_arg_repns(TVarSet, IndentStr, AnyFieldName,
        CurArgNum, HeadArgRepn, TailArgRepns, !State) :-
    HeadArgRepn = ctor_arg_repn(MaybeFieldName, _MaybeBaseCtorArg, Type,
        ArgPosWidth, _Context),
    string.builder.append_string(IndentStr, !State),
    (
        AnyFieldName = no
    ;
        AnyFieldName = yes,
        (
            MaybeFieldName = no,
            string.builder.format("%24s", [s("")], !State)
        ;
            MaybeFieldName = yes(ctor_field_name(FieldName, _Ctxt)),
            string.builder.format("%-20s :: ",
                [s(unqualify_name(FieldName))], !State)
        )
    ),
    mercury_format_type(TVarSet, print_name_only, Type,
        string.builder.handle, !State),
    (
        TailArgRepns = [],
        string.builder.append_string("\n", !State),
        format_arg_pos_width(IndentStr, CurArgNum, ArgPosWidth, !State)
    ;
        TailArgRepns = [HeadTailArgRepn | TailTailArgRepns],
        string.builder.append_string(",\n", !State),
        format_arg_pos_width(IndentStr, CurArgNum, ArgPosWidth, !State),
        format_ctor_arg_repns(TVarSet, IndentStr, AnyFieldName,
            CurArgNum + 1, HeadTailArgRepn, TailTailArgRepns, !State)
    ).

%---------------------%

:- func does_any_arg_have_a_field_name(list(constructor_arg)) = bool.

does_any_arg_have_a_field_name([]) = no.
does_any_arg_have_a_field_name([Arg | Args]) = SomeArgHasFieldName :-
    Arg = ctor_arg(MaybeFieldName, _, _),
    (
        MaybeFieldName = yes(_),
        SomeArgHasFieldName = yes
    ;
        MaybeFieldName = no,
        SomeArgHasFieldName = does_any_arg_have_a_field_name(Args)
    ).

:- func does_any_arg_repn_have_a_field_name(list(constructor_arg_repn)) = bool.

does_any_arg_repn_have_a_field_name([]) = no.
does_any_arg_repn_have_a_field_name([ArgRepn | ArgRepns])
        = SomeArgHasFieldName :-
    ArgRepn = ctor_arg_repn(MaybeFieldName, _, _, _, _),
    (
        MaybeFieldName = yes(_),
        SomeArgHasFieldName = yes
    ;
        MaybeFieldName = no,
        SomeArgHasFieldName = does_any_arg_repn_have_a_field_name(ArgRepns)
    ).

%---------------------%

:- func du_cons_tag_to_string(cons_tag) = string.

du_cons_tag_to_string(ConsTag) = String :-
    (
        ConsTag = shared_local_tag_no_args(ptag(Ptag),
            LocalSectag, SectagMask),
        (
            SectagMask = lsectag_always_rest_of_word,
            MaskString = "rest of word"
        ;
            SectagMask = lsectag_must_be_masked,
            MaskString = "must be masked"
        ),
        String = string.format("ptag %u, local sectag %s, no args, %s",
            [u8(Ptag), s(local_sectag_to_string(LocalSectag)), s(MaskString)])
    ;
        ConsTag = local_args_tag(LocalArgsTagInfo),
        (
            LocalArgsTagInfo = local_args_only_functor,
            String = "ptag 0, local sectag none, only functor"
        ;
            LocalArgsTagInfo = local_args_not_only_functor(ptag(Ptag),
                LocalSectag),
            String = string.format("ptag %u, local sectag %s",
                [u8(Ptag), s(local_sectag_to_string(LocalSectag))])
        )
    ;
        ConsTag = remote_args_tag(RemoteArgsTagInfo),
        (
            RemoteArgsTagInfo = remote_args_only_functor,
            String = "ptag 0, remote sectag none, only functor"
        ;
            RemoteArgsTagInfo = remote_args_unshared(ptag(Ptag)),
            String = string.format("ptag %u, remote sectag none, unshared",
                [u8(Ptag)])
        ;
            RemoteArgsTagInfo = remote_args_shared(ptag(Ptag), RemoteSectag),
            RemoteSectag = remote_sectag(SectagValue, SectagSize),
            (
                SectagSize = rsectag_word,
                String = string.format("ptag %u, remote sectag %u full word",
                    [u8(Ptag), u(SectagValue)])
            ;
                SectagSize = rsectag_subword(SectagBits),
                SectagBits = sectag_bits(NumRemoteSectagBits, Mask),
                String = string.format(
                    "ptag %u, remote sectag %u in %u bits, mask %x",
                    [u8(Ptag), u(SectagValue), u8(NumRemoteSectagBits),
                    u(Mask)])
            )
        ;
            RemoteArgsTagInfo = remote_args_ctor(Data),
            String = string.format("ctor %u", [u(Data)])
        )
    ;
        ConsTag = no_tag,
        String = "notag"
    ;
        ConsTag = direct_arg_tag(ptag(Ptag)),
        String = string.format("direct arg tag %u", [u8(Ptag)])
    ;
        ConsTag = dummy_tag,
        String = "dummy tag"
    ;
        ConsTag = foreign_tag(Lang, ForeignName),
        String = string.format("foreign %s for %s",
            [s(ForeignName), s(foreign_language_string(Lang))])
    ;
        ConsTag = int_tag(IntTag),
        (
            IntTag = int_tag_int(N),
            String = string.format("enum %d", [i(N)])
        ;
            ( IntTag = int_tag_uint(_)
            ; IntTag = int_tag_int8(_)
            ; IntTag = int_tag_uint8(_)
            ; IntTag = int_tag_int16(_)
            ; IntTag = int_tag_uint16(_)
            ; IntTag = int_tag_int32(_)
            ; IntTag = int_tag_uint32(_)
            ; IntTag = int_tag_int64(_)
            ; IntTag = int_tag_uint64(_)
            ),
            unexpected($pred, "non-du cons_tag")
        )
    ;
        ( ConsTag = float_tag(_)
        ; ConsTag = string_tag(_)
        ; ConsTag = ground_term_const_tag(_, _)
        ; ConsTag = type_info_const_tag(_)
        ; ConsTag = typeclass_info_const_tag(_)
        ; ConsTag = type_ctor_info_tag(_, _, _)
        ; ConsTag = base_typeclass_info_tag(_, _, _)
        ; ConsTag = deep_profiling_proc_layout_tag(_, _)
        ; ConsTag = tabling_info_tag(_, _)
        ; ConsTag = table_io_entry_tag(_, _)
        ; ConsTag = closure_tag(_, _)
        ),
        unexpected($pred, "non-du cons_tag")
    ).

:- func local_sectag_to_string(local_sectag) = string.

local_sectag_to_string(LocalSectag) = String :-
    % NOTE _PrimSec and _Mask are computable from the other parts
    % of the cons_tag. This means that printing them would just be clutter,
    % *except* in the case where they are computed *incorrectly*
    % from those other parts.
    LocalSectag = local_sectag(SectagValue, _PrimSec, SectagBits),
    SectagBits = sectag_bits(NumBits, _Mask),
    ( if NumBits = 0u8 then
        String = "none"
    else
        String = string.format("%u in %u bits", [u(SectagValue), u8(NumBits)])
    ).

:- pred format_arg_pos_width(string::in, int::in, arg_pos_width::in,
    string.builder.state::di, string.builder.state::uo) is det.

format_arg_pos_width(IndentStr, CurArgNum, ArgPosWidth, !State) :-
    string.builder.append_string(IndentStr, !State),
    (
        ArgPosWidth = apw_full(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(AOWordNum),
        CellOffset = cell_offset(CellWordNum),
        string.builder.format("%% arg %d: full word, offset %d/%d\n",
            [i(CurArgNum), i(AOWordNum), i(CellWordNum)], !State)
    ;
        ArgPosWidth = apw_double(ArgOnlyOffset, CellOffset, DoubleWordKind),
        ArgOnlyOffset = arg_only_offset(AOWordNum),
        CellOffset = cell_offset(CellWordNum),
        (
            DoubleWordKind = dw_float,
            KindStr = "float"
        ;
            DoubleWordKind = dw_int64,
            KindStr = "int64"
        ;
            DoubleWordKind = dw_uint64,
            KindStr = "uint64"
        ),
        string.builder.format(
            "%% arg %d: double word %s, offsets %d/%d to %d/%d\n",
            [i(CurArgNum), s(KindStr), i(AOWordNum), i(CellWordNum),
            i(AOWordNum + 1), i(CellWordNum + 1)], !State)
    ;
        (
            ArgPosWidth = apw_partial_first(ArgOnlyOffset, CellOffset, Shift,
                NumBits, Mask, FillKind),
            FirstShifted = "first"
        ;
            ArgPosWidth = apw_partial_shifted(ArgOnlyOffset, CellOffset, Shift,
                NumBits, Mask, FillKind),
            FirstShifted = "later"
        ),
        ArgOnlyOffset = arg_only_offset(AOWordNum),
        CellOffset = cell_offset(CellWordNum),
        Shift = arg_shift(ShiftInt),
        NumBits = arg_num_bits(NumBitsInt),
        Mask = arg_mask(MaskInt),
        FillStr = fill_kind_to_string(FillKind),
        string.builder.format("%% arg %d: partial %s, " ++
            "offset %d/%d, shift %2d #bits %2d mask %x %s\n",
            [i(CurArgNum), s(FirstShifted), i(AOWordNum), i(CellWordNum),
            i(ShiftInt), i(NumBitsInt), i(MaskInt), s(FillStr)], !State)
    ;
        ArgPosWidth = apw_none_shifted(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(AOWordNum),
        CellOffset = cell_offset(CellWordNum),
        string.builder.format("%% arg %d: none shifted, offset %d/%d\n",
            [i(CurArgNum), i(AOWordNum), i(CellWordNum)], !State)
    ;
        ArgPosWidth = apw_none_nowhere,
        string.builder.format("%% arg %d: none_nowhere\n",
            [i(CurArgNum)], !State)
    ).

:- func fill_kind_to_string(fill_kind) = string.

fill_kind_to_string(fill_enum) = "enum".
fill_kind_to_string(fill_int8) = "int8".
fill_kind_to_string(fill_int16) = "int16".
fill_kind_to_string(fill_int32) = "int32".
fill_kind_to_string(fill_uint8) = "uint8".
fill_kind_to_string(fill_uint16) = "uint16".
fill_kind_to_string(fill_uint32) = "uint32".
fill_kind_to_string(fill_char21) = "char21".

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_type_table.
%---------------------------------------------------------------------------%
