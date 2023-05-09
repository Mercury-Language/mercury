%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_type_table.
:- interface.

:- import_module hlds.hlds_data.
:- import_module hlds.hlds_out.hlds_out_util.

:- import_module bool.
:- import_module io.

:- pred write_type_table(hlds_out_info::in, io.text_output_stream::in,
    bool::in, type_table::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.indent.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_cons_id.
:- import_module parse_tree.parse_tree_out_info.
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
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

write_type_table(Info, Stream, LocalOnly, TypeTable, !IO) :-
    io.write_string(Stream, "%-------- Types --------\n", !IO),
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
    write_type_table_entries(Info, Stream, PrintedTypeAssocList, !IO),
    io.nl(Stream, !IO).

:- pred type_table_entry_is_local(pair(type_ctor, hlds_type_defn)::in)
    is semidet.

type_table_entry_is_local(_TypeCtor - TypeDefn) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    type_status_defined_in_this_module(TypeStatus) = yes.

:- pred write_type_table_entries(hlds_out_info::in, io.text_output_stream::in,
    assoc_list(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

write_type_table_entries(_, _, [], !IO).
write_type_table_entries(Info, Stream, [Type | Types], !IO) :-
    write_type_table_entry(Info, Stream, Type, !IO),
    write_type_table_entries(Info, Stream, Types, !IO).

:- pred write_type_table_entry(hlds_out_info::in, io.text_output_stream::in,
    pair(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

write_type_table_entry(Info, Stream, TypeCtor - TypeDefn, !IO) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    hlds_data.get_type_defn_context(TypeDefn, Context),
    % Write the context.
    io.write_char(Stream, '\n', !IO),
    maybe_output_context_comment(Stream, 0, "", Context, !IO),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'c') then
        io.format(Stream, "%% status %s\n",
            [s(type_import_status_to_string(TypeStatus))], !IO)
    else
        true
    ),
    ( if
        ( TypeBody = hlds_solver_type(_)
        ; TypeBody = hlds_abstract_type(abstract_solver_type)
        )
    then
        io.write_string(Stream, ":- solver type ", !IO)
    else
        io.write_string(Stream, ":- type ", !IO)
    ),
    write_type_name(Stream, TypeCtor, !IO),
    write_type_params(Stream, TVarSet, TypeParams, !IO),
    write_type_body(Info, Stream, TypeCtor, TypeBody, TVarSet, !IO).

:- pred write_type_params(io.text_output_stream::in, tvarset::in,
    list(type_param)::in, io::di, io::uo) is det.

write_type_params(Stream, TVarSet, TypeParams, !IO) :-
    (
        TypeParams = []
    ;
        TypeParams = [HeadParam | TailParams],
        io.write_string(Stream, "(", !IO),
        mercury_output_var_vs(TVarSet, print_name_only, HeadParam,
            Stream, !IO),
        write_comma_type_params_loop(Stream, TVarSet, TailParams, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred write_comma_type_params_loop(io.text_output_stream::in, tvarset::in,
    list(type_param)::in, io::di, io::uo) is det.

write_comma_type_params_loop(_Stream, _TVarSet, [], !IO).
write_comma_type_params_loop(Stream, TVarSet, [Param | Params], !IO) :-
    io.write_string(Stream, ", ", !IO),
    mercury_output_var_vs(TVarSet, print_name_only, Param, Stream, !IO),
    write_comma_type_params_loop(Stream, TVarSet, Params, !IO).

:- pred write_type_body(hlds_out_info::in, io.text_output_stream::in,
    type_ctor::in, hlds_type_body::in, tvarset::in,
    io::di, io::uo) is det.

write_type_body(Info, Stream, _TypeCtor, TypeBody, TVarSet, !IO) :-
    BaseIndent = 1,
    IndentStr = indent2_string(BaseIndent),
    (
        TypeBody = hlds_du_type(TypeBodyDu),
        TypeBodyDu = type_body_du(Ctors, MaybeSuperType, MaybeUserEqComp,
            MaybeRepn, Foreign),
        io.nl(Stream, !IO),
        (
            MaybeSuperType = subtype_of(SuperType),
            SuperTypeStr = mercury_type_to_string(TVarSet,
                print_name_only, SuperType),
            io.format(Stream, "%s%% subtype of %s\n",
                [s(IndentStr), s(SuperTypeStr)], !IO)
        ;
            MaybeSuperType = not_a_subtype
        ),
        MaybeSolverTypeDetails = no,
        MercInfo = Info ^ hoi_merc_out_info,
        (
            MaybeRepn = no,
            Ctors = one_or_more(HeadCtor, TailCtors),
            write_constructors(Stream, TVarSet, HeadCtor, TailCtors, !IO),
            MaybeDirectArgCtors = no,
            mercury_output_where_attributes(MercInfo, TVarSet,
                MaybeSolverTypeDetails, MaybeUserEqComp, MaybeDirectArgCtors,
                Stream, !IO),
            io.format(Stream, "%s%% no type representation information yet\n",
                [s(IndentStr)], !IO)
        ;
            MaybeRepn = yes(Repn),
            Repn = du_type_repn(CtorRepns, CtorRepnMap, CheaperTagTest,
                DuTypeKind, MaybeDirectArgCtors),
            write_constructor_repns(Stream, TVarSet, CtorRepns, !IO),
            (
                CheaperTagTest = no_cheaper_tag_test
            ;
                CheaperTagTest = cheaper_tag_test(ExpConsId, ExpConsTag,
                    CheapConsId, CheapConsTag),
                ExpConsIdStr = cons_id_and_arity_to_string(
                    unqual_cons_id(ExpConsId)),
                CheapConsIdStr = cons_id_and_arity_to_string(
                    unqual_cons_id(CheapConsId)),
                io.format(Stream, "%s%% cheaper tag test:\n",
                    [s(IndentStr)], !IO),
                io.format(Stream, "%s%%   from %s\n",
                    [s(IndentStr), s(ExpConsIdStr)], !IO),
                io.format(Stream, "%s%%      %s\n",
                    [s(IndentStr), s(du_cons_tag_to_string(ExpConsTag))], !IO),
                io.format(Stream, "%s%%   to %s\n",
                    [s(IndentStr), s(CheapConsIdStr)], !IO),
                io.format(Stream, "%s%%      %s\n",
                    [s(IndentStr), s(du_cons_tag_to_string(CheapConsTag))],
                    !IO)
            ),
            (
                DuTypeKind = du_type_kind_mercury_enum,
                io.format(Stream, "%s%% KIND enumeration\n",
                    [s(IndentStr)], !IO)
            ;
                DuTypeKind = du_type_kind_foreign_enum(Lang),
                io.format(Stream,
                    "%s%% KIND foreign enumeration for %s\n",
                    [s(IndentStr), s(foreign_language_string(Lang))], !IO)
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                io.format(Stream, "%s%% KIND dummy\n",
                    [s(IndentStr)], !IO)
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
                io.format(Stream, "%s%% KIND notag: %s, %s, %s\n",
                    [s(IndentStr), s(sym_name_to_escaped_string(FunctorName)),
                    s(ArgTypeStr), s(ArgName)], !IO)
            ;
                DuTypeKind = du_type_kind_general,
                io.format(Stream, "%s%% KIND general\n",
                    [s(IndentStr)], !IO)
            ),
            mercury_output_where_attributes(MercInfo, TVarSet,
                MaybeSolverTypeDetails, MaybeUserEqComp, MaybeDirectArgCtors,
                Stream, !IO),
            (
                Foreign = yes(_),
                io.format(Stream, "%s%% has foreign_type\n",
                    [s(IndentStr)], !IO)
            ;
                Foreign = no
            ),
            trace [compile_time(flag("ctor_repn_invariant_check")), io(!TIO)] (
                list.sort(CtorRepns, SortedCtorRepns),
                map.foldl_values(accumulate_ctor_repns, CtorRepnMap,
                    [], MapCtorRepns),
                list.sort(MapCtorRepns, SortedMapCtorRepns),
                ( if SortedCtorRepns = SortedMapCtorRepns then
                    true
                else
                    io.format(Stream, 
                        "%s%% BUG SortedCtorRepns != SortedMapCtorRepns\n",
                        [s(IndentStr)], !TIO)
                )
            )
        )
    ;
        TypeBody = hlds_eqv_type(Type),
        io.write_string(Stream, " == ", !IO),
        mercury_output_type(TVarSet, print_name_only, Type, Stream, !IO),
        io.write_string(Stream, ".\n", !IO)
    ;
        TypeBody = hlds_abstract_type(_IsSolverType),
        io.write_string(Stream, ".\n", !IO)
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
        Indent1Str = indent2_string(BaseIndent + 1),
        io.format(Stream, " is foreign_type(\n%s%s,\n%s%s,\n%s%s\n%s).\n",
            [s(Indent1Str), s(MaybeCStr),
            s(Indent1Str), s(MaybeJavaStr),
            s(Indent1Str), s(MaybeCsharpStr),
            s(IndentStr)], !IO)
    ;
        TypeBody = hlds_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(SolverTypeDetails, MaybeUserEqComp),
        MercInfo = Info ^ hoi_merc_out_info,
        mercury_output_where_attributes(MercInfo, TVarSet,
            yes(SolverTypeDetails), MaybeUserEqComp, no, Stream, !IO),
        io.write_string(Stream, ".\n", !IO)
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

:- pred write_constructors(io.text_output_stream::in, tvarset::in,
    constructor::in, list(constructor)::in, io::di, io::uo) is det.

write_constructors(Stream, TVarSet, HeadCtor, TailCtors, !IO) :-
    ArrowOrSemi0 = "--->    ",
    write_constructors_loop(Stream, TVarSet, ArrowOrSemi0,
        HeadCtor, TailCtors, !IO).

:- pred write_constructor_repns(io.text_output_stream::in, tvarset::in,
    list(constructor_repn)::in, io::di, io::uo) is det.

write_constructor_repns(Stream, TVarSet, CtorRepns, !IO) :-
    (
        CtorRepns = [],
        unexpected($pred, "empty constructor list")
    ;
        CtorRepns = [HeadCtorRepn | TailCtorRepns],
        ArrowOrSemi0 = "--->    ",
        write_constructor_repns_loop(Stream, TVarSet, ArrowOrSemi0,
            HeadCtorRepn, TailCtorRepns, !IO)
    ).

%---------------------%

:- pred write_constructors_loop(io.text_output_stream::in, tvarset::in,
    string::in, constructor::in, list(constructor)::in, io::di, io::uo) is det.

write_constructors_loop(Stream, TVarSet, ArrowOrSemi0,
        HeadCtor, TailCtors, !IO) :-
    write_indent2(Stream, 1, !IO),
    io.write_string(Stream, ArrowOrSemi0, !IO),
    (
        TailCtors = [],
        write_ctor(Stream, TVarSet, HeadCtor, !IO)
    ;
        TailCtors = [HeadTailCtor | TailTailCtors],
        write_ctor(Stream, TVarSet, HeadCtor, !IO),
        ArrowOrSemi = ";       ",
        write_constructors_loop(Stream, TVarSet, ArrowOrSemi,
            HeadTailCtor, TailTailCtors, !IO)
    ).

:- pred write_constructor_repns_loop(io.text_output_stream::in, tvarset::in,
    string::in, constructor_repn::in, list(constructor_repn)::in,
    io::di, io::uo) is det.

write_constructor_repns_loop(Stream, TVarSet, ArrowOrSemi0,
        HeadCtorRepn, TailCtorRepns, !IO) :-
    write_indent2(Stream, 1, !IO),
    io.write_string(Stream, ArrowOrSemi0, !IO),
    (
        TailCtorRepns = [],
        write_ctor_repn(Stream, TVarSet, HeadCtorRepn, !IO)
    ;
        TailCtorRepns = [HeadTailCtorRepn | TailTailCtorRepns],
        write_ctor_repn(Stream, TVarSet, HeadCtorRepn, !IO),
        ArrowOrSemi = ";       ",
        write_constructor_repns_loop(Stream, TVarSet, ArrowOrSemi,
            HeadTailCtorRepn, TailTailCtorRepns, !IO)
    ).

%---------------------%

:- pred write_ctor(io.text_output_stream::in, tvarset::in,
    constructor::in, io::di, io::uo) is det.

write_ctor(Stream, TVarSet, Ctor, !IO) :-
    % NOTE The code of this predicate is almost identical to the code of
    % write_ctor_repn below and mercury_output_ctor in parse_tree_out.m.
    % Any changes made here will probably need to be made there as well.
    Ctor = ctor(_Ordinal, MaybeExistConstraints, SymName, Args, Arity, _Ctxt),

    % The module name in SymName must be the same as the module qualifier
    % of the type_ctor, so there is no point in printing it.
    Name = unqualify_name(SymName),
    NameStr =
        mercury_bracketed_atom_to_string(not_next_to_graphic_token, Name),
    % The width of ArrowOrSemi is eight spaces, which is the same as
    % four indents. This comes after the original one indent.
    BaseIndent = 1,
    ASIndent = 4,
    BaseASIndentStr = indent2_string(BaseIndent + ASIndent),
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet,
        BaseASIndentStr, "\n", MaybeExistConstraints,
        ExistConstraintsPrefix, ExistConstraintsSuffix),
    maybe_brace_for_name_prefix_suffix(Arity, Name, BracePrefix, BraceSuffix),
    io.write_string(Stream, ExistConstraintsPrefix, !IO),
    (
        Args = [],
        io.format(Stream, "%s%s%s",
            [s(BracePrefix), s(NameStr), s(BraceSuffix)], !IO)
    ;
        Args = [HeadArg | TailArgs],
        io.format(Stream, "%s%s(\n", [s(BracePrefix), s(NameStr)], !IO),
        AnyFieldName = does_any_arg_have_a_field_name(Args),
        BaseASIndent1Str = indent2_string(BaseIndent + ASIndent + 1),
        mercury_output_ctor_args(Stream, TVarSet, BaseASIndent1Str,
            AnyFieldName, HeadArg, TailArgs, !IO),
        io.format(Stream, "%s)%s\n",
            [s(BaseASIndentStr), s(BraceSuffix)], !IO)
    ),
    io.format(Stream, "%s%s\n",
        [s(BraceSuffix), s(ExistConstraintsSuffix)], !IO).

:- pred write_ctor_repn(io.text_output_stream::in, tvarset::in,
    constructor_repn::in, io::di, io::uo) is det.

write_ctor_repn(Stream, TVarSet, CtorRepn, !IO) :-
    % NOTE The code of this predicate is almost identical to the code of
    % write_ctor_repn below and mercury_output_ctor in parse_tree_out.m.
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
    BaseIndent = 1,
    ASIndent = 4,
    BaseASIndentStr = indent2_string(BaseIndent + ASIndent),
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet,
        BaseASIndentStr, "\n", MaybeExistConstraints,
        ExistConstraintsPrefix, ExistConstraintsSuffix),
    maybe_brace_for_name_prefix_suffix(Arity, Name, BracePrefix, BraceSuffix),
    io.write_string(Stream, ExistConstraintsPrefix, !IO),
    io.write_string(Stream, BracePrefix, !IO),
    ConsTagString = string.format("%s%% tag: %s\n",
        [s(BaseASIndentStr), s(du_cons_tag_to_string(ConsTag))]),
    (
        ArgRepns = [],
        io.format(Stream, "%s%s%s\n%s",
            [s(BracePrefix), s(NameStr), s(BraceSuffix), s(ConsTagString)],
            !IO)
    ;
        ArgRepns = [HeadArgRepn | TailArgRepns],
        BaseASIndent1Str = indent2_string(BaseIndent + ASIndent + 1),
        io.format(Stream, "%s%s(\n%s",
            [s(BracePrefix), s(NameStr), s(ConsTagString)], !IO),
        AnyFieldName = does_any_arg_repn_have_a_field_name(ArgRepns),
        mercury_output_ctor_arg_repns(Stream, TVarSet, BaseASIndent1Str,
            AnyFieldName, 1, HeadArgRepn, TailArgRepns, !IO),
        io.format(Stream, "%s)%s\n", [s(BaseASIndentStr), s(BraceSuffix)], !IO)
    ),
    io.write_string(Stream, ExistConstraintsSuffix, !IO).

%---------------------%

:- pred mercury_output_ctor_args(io.text_output_stream::in, tvarset::in,
    string::in, bool::in, constructor_arg::in, list(constructor_arg)::in,
    io::di, io::uo) is det.

mercury_output_ctor_args(Stream, TVarSet, IndentStr, AnyFieldName,
        HeadArg, TailArgs, !IO) :-
    HeadArg = ctor_arg(MaybeFieldName, Type, _Context),
    io.write_string(Stream, IndentStr, !IO),
    (
        AnyFieldName = no
    ;
        AnyFieldName = yes,
        (
            MaybeFieldName = no,
            io.format(Stream, "%24s", [s("")], !IO)
        ;
            MaybeFieldName = yes(ctor_field_name(FieldName, _Ctxt)),
            io.format(Stream, "%-20s :: ",
                [s(unqualify_name(FieldName))], !IO)
        )
    ),
    mercury_output_type(TVarSet, print_name_only, Type, Stream, !IO),
    (
        TailArgs = [],
        io.write_string(Stream, "\n", !IO)
    ;
        TailArgs = [HeadTailArg | TailTailArgs],
        io.write_string(Stream, ",\n", !IO),
        mercury_output_ctor_args(Stream, TVarSet, IndentStr, AnyFieldName,
            HeadTailArg, TailTailArgs, !IO)
    ).

:- pred mercury_output_ctor_arg_repns(io.text_output_stream::in, tvarset::in,
    string::in, bool::in, int::in, constructor_arg_repn::in,
    list(constructor_arg_repn)::in, io::di, io::uo) is det.

mercury_output_ctor_arg_repns(Stream, TVarSet, IndentStr, AnyFieldName,
        CurArgNum, HeadArgRepn, TailArgRepns, !IO) :-
    HeadArgRepn = ctor_arg_repn(MaybeFieldName, Type, ArgPosWidth, _Context),
    io.write_string(Stream, IndentStr, !IO),
    (
        AnyFieldName = no
    ;
        AnyFieldName = yes,
        (
            MaybeFieldName = no,
            io.format(Stream, "%24s", [s("")], !IO)
        ;
            MaybeFieldName = yes(ctor_field_name(FieldName, _Ctxt)),
            io.format(Stream, "%-20s :: ",
                [s(unqualify_name(FieldName))], !IO)
        )
    ),
    mercury_output_type(TVarSet, print_name_only, Type, Stream, !IO),
    (
        TailArgRepns = [],
        io.write_string(Stream, "\n", !IO),
        write_arg_pos_width(Stream, IndentStr, CurArgNum, ArgPosWidth, !IO)
    ;
        TailArgRepns = [HeadTailArgRepn | TailTailArgRepns],
        io.write_string(Stream, ",\n", !IO),
        write_arg_pos_width(Stream, IndentStr, CurArgNum, ArgPosWidth, !IO),
        mercury_output_ctor_arg_repns(Stream, TVarSet, IndentStr, AnyFieldName,
            CurArgNum + 1, HeadTailArgRepn, TailTailArgRepns, !IO)
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
    ArgRepn = ctor_arg_repn(MaybeFieldName, _, _, _),
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
        ; ConsTag = closure_tag(_, _, _)
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

:- pred write_arg_pos_width(io.text_output_stream::in, string::in, int::in,
    arg_pos_width::in, io::di, io::uo) is det.

write_arg_pos_width(Stream, IndentStr, CurArgNum, ArgPosWidth, !IO) :-
    io.write_string(Stream, IndentStr, !IO),
    (
        ArgPosWidth = apw_full(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(AOWordNum),
        CellOffset = cell_offset(CellWordNum),
        io.format(Stream, "%% arg %d: full word, offset %d/%d\n",
            [i(CurArgNum), i(AOWordNum), i(CellWordNum)], !IO)
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
        io.format(Stream,
            "%% arg %d: double word %s, offsets %d/%d to %d/%d\n",
            [i(CurArgNum), s(KindStr), i(AOWordNum), i(CellWordNum),
            i(AOWordNum + 1), i(CellWordNum + 1)], !IO)
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
        io.format(Stream, "%% arg %d: partial %s, " ++
            "offset %d/%d, shift %2d #bits %2d mask %x %s\n",
            [i(CurArgNum), s(FirstShifted), i(AOWordNum), i(CellWordNum),
            i(ShiftInt), i(NumBitsInt), i(MaskInt), s(FillStr)], !IO)
    ;
        ArgPosWidth = apw_none_shifted(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(AOWordNum),
        CellOffset = cell_offset(CellWordNum),
        io.format(Stream, "%% arg %d: none shifted, offset %d/%d\n",
            [i(CurArgNum), i(AOWordNum), i(CellWordNum)], !IO)
    ;
        ArgPosWidth = apw_none_nowhere,
        io.format(Stream, "%% arg %d: none_nowhere\n", [i(CurArgNum)], !IO)
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
