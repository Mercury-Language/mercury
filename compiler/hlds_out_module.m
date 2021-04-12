%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_out_module.m.
% Main authors: conway, fjh.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_out.hlds_out_module.
:- interface.

:- import_module hlds.hlds_clauses.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

    % Print out an entire HLDS structure.
    %
:- pred write_hlds(io.text_output_stream::in, int::in, module_info::in,
    io::di, io::uo) is det.

    % Exported for intermod.m.
    %
:- pred write_promise(hlds_out_info::in, io.text_output_stream::in,
    module_info::in, prog_varset::in, maybe_vartypes::in, var_name_print::in,
    int::in, promise_type::in, pred_id::in, pred_or_func::in,
    list(prog_var)::in, clause::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.const_struct.
:- import_module hlds.hlds_class.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_inst_mode.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_out.hlds_out_mode.
:- import_module hlds.hlds_out.hlds_out_pred.
:- import_module hlds.status.
% XXX :- import_module hlds.pred_table.
% We actually use a type equivalence from pred_table.m (specifically,
% the fact that pred_table is a map), but we get an unused import warning
% for the above line anyway. The import is commented out until the code
% that generates the warning is fixed.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_tree_out.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_to_term.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_out.

:- import_module assoc_list.
:- import_module bool.
:- import_module digraph.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module varset.

%---------------------------------------------------------------------------%
%
% Write out (selected parts of) the entire HLDS.
%

write_hlds(Stream, Indent, ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_id,
        DumpPredIdStrs),
    globals.lookup_accumulating_option(Globals, dump_hlds_pred_name,
        DumpPredNames),
    globals.lookup_bool_option(Globals, dump_hlds_spec_preds, DumpSpecPreds0),
    globals.lookup_accumulating_option(Globals, dump_hlds_spec_preds_for,
        DumpSpecPredTypeNames),
    write_header(Stream, Indent, ModuleInfo, !IO),
    Info = init_hlds_out_info(Globals, output_debug),
    Lang = output_debug,
    DumpOptions0 = Info ^ hoi_dump_hlds_options,
    (
        DumpSpecPredTypeNames = [],
        DumpSpecPreds = DumpSpecPreds0
    ;
        DumpSpecPredTypeNames = [_ | _],
        DumpSpecPreds = yes
    ),
    (
        DumpSpecPreds = no,
        DumpOptions = DumpOptions0
    ;
        DumpSpecPreds = yes,
        % Print unify (and compare and index) predicates.
        DumpOptions = DumpOptions0 ++ "U"
    ),
    ( if
        % If the user specifically requested one or more predicates and/or
        % functions to be dumped, they won't be interested in the types,
        % insts etc.
        ( DumpPredIdStrs = [_ | _]
        ; DumpPredNames = [_ | _]
        ; DumpSpecPreds = yes
        )
    then
        true
    else
        ( if string.contains_char(DumpOptions, 'I') then
            module_info_get_avail_module_map(ModuleInfo, AvailModuleMap),
            map.foldl(write_avail_entry(Stream, Indent), AvailModuleMap, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'T') then
            ( if string.contains_char(DumpOptions, 'L') then
                LocalOnly = yes
            else
                LocalOnly = no
            ),
            module_info_get_type_table(ModuleInfo, TypeTable),
            module_info_get_instance_table(ModuleInfo, InstanceTable),
            module_info_get_class_table(ModuleInfo, ClassTable),
            write_type_table(Info, Stream, Indent, LocalOnly, TypeTable, !IO),
            write_classes(Info, Stream, Indent, ClassTable, !IO),
            write_instances(Info, Stream, Indent, InstanceTable, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'M') then
            module_info_get_inst_table(ModuleInfo, InstTable),
            module_info_get_mode_table(ModuleInfo, ModeTable),
            globals.lookup_int_option(Globals, dump_hlds_inst_limit,
                InstLimit),
            write_inst_table(Stream, Lang, Indent, InstLimit, InstTable, !IO),
            write_mode_table(Stream, Indent, ModeTable, !IO)
        else
            true
        ),
        ( if string.contains_char(DumpOptions, 'Z') then
            module_info_get_table_struct_map(ModuleInfo, TableStructMap),
            write_table_structs(Stream, ModuleInfo, TableStructMap, !IO)
        else
            true
        )
    ),
    ( if string.contains_char(DumpOptions, 'X') then
        module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
        write_const_struct_db(Stream, ConstStructDb, !IO)
    else
        true
    ),
    ( if
        ( string.contains_char(DumpOptions, 'x')
        ; DumpSpecPreds = yes
        )
    then
        write_preds(Info, Stream, DumpSpecPreds, DumpSpecPredTypeNames,
            Lang, Indent, ModuleInfo, !IO)
    else
        true
    ),
    ( if string.contains_char(DumpOptions, 'O') then
        module_info_get_maybe_dependency_info(ModuleInfo, MaybeDependencyInfo),
        (
            MaybeDependencyInfo = no,
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "% No dependency info\n\n", !IO)
        ;
            MaybeDependencyInfo = yes(DependencyInfo),
            write_dependency_info(Info, Stream, Indent, ModuleInfo,
                DependencyInfo, !IO)
        )
    else
        true
    ),
    write_footer(Stream, Indent, ModuleInfo, !IO).

%---------------------------------------------------------------------------%

:- pred write_header(io.text_output_stream::in, int::in, module_info::in,
    io::di, io::uo) is det.

write_header(Stream, Indent, Module, !IO) :-
    module_info_get_name(Module, ModuleName),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% vim: ts=2 sw=2 ft=mercury\n\n", !IO),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, ":- module %s.\n\n",
        [s(sym_name_to_escaped_string(ModuleName))], !IO).

:- pred write_footer(io.text_output_stream::in, int::in, module_info::in,
    io::di, io::uo) is det.

write_footer(Stream, Indent, Module, !IO) :-
    module_info_get_name(Module, ModuleName),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, ":- end_module %s.\n",
        [s(sym_name_to_escaped_string(ModuleName))], !IO).

%---------------------------------------------------------------------------%
%
% Write out the imports and uses.
%

:- pred write_avail_entry(io.text_output_stream::in, int::in,
    module_name::in, avail_module_entry::in, io::di, io::uo) is det.

write_avail_entry(Stream, Indent, ModuleName, Entry, !IO) :-
    Entry = avail_module_entry(Section, ImportOrUse, Avails),
    (
        ImportOrUse = import_decl,
        ImportOrUseDecl = "import_module"
    ;
        ImportOrUse = use_decl,
        ImportOrUseDecl = "use_module"
    ),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, ":- %s %s.\n",
        [s(ImportOrUseDecl), s(sym_name_to_escaped_string(ModuleName))], !IO),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% ", !IO),
    io.write(Stream, Section, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write(Stream, Avails, !IO),
    io.write_string(Stream, "\n", !IO).

%---------------------------------------------------------------------------%
%
% Write out the type table.
%

:- pred write_type_table(hlds_out_info::in, io.text_output_stream::in,
    int::in, bool::in, type_table::in, io::di, io::uo) is det.

write_type_table(Info, Stream, Indent, LocalOnly, TypeTable, !IO) :-
    write_indent(Stream, Indent, !IO),
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
    write_type_table_entries(Info, Stream, Indent, PrintedTypeAssocList, !IO),
    io.nl(Stream, !IO).

:- pred type_table_entry_is_local(pair(type_ctor, hlds_type_defn)::in)
    is semidet.

type_table_entry_is_local(_TypeCtor - TypeDefn) :-
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    type_status_defined_in_this_module(TypeStatus) = yes.

:- pred write_type_table_entries(hlds_out_info::in, io.text_output_stream::in,
    int::in, assoc_list(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

write_type_table_entries(_, _, _, [], !IO).
write_type_table_entries(Info, Stream, Indent, [Type | Types], !IO) :-
    write_type_table_entry(Info, Stream, Indent, Type, !IO),
    write_type_table_entries(Info, Stream, Indent, Types, !IO).

:- pred write_type_table_entry(hlds_out_info::in, io.text_output_stream::in,
    int::in, pair(type_ctor, hlds_type_defn)::in, io::di, io::uo) is det.

write_type_table_entry(Info, Stream, Indent, TypeCtor - TypeDefn, !IO) :-
    hlds_data.get_type_defn_tvarset(TypeDefn, TVarSet),
    hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    hlds_data.get_type_defn_status(TypeDefn, TypeStatus),
    hlds_data.get_type_defn_context(TypeDefn, Context),
    % Write the context.
    io.write_char(Stream, '\n', !IO),
    maybe_output_context_comment(Stream, Indent, "", Context, !IO),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'c') then
        io.format(Stream, "%% status %s\n",
            [s(type_import_status_to_string(TypeStatus))], !IO)
    else
        true
    ),
    write_indent(Stream, Indent, !IO),
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
    write_type_body(Info, Stream, TypeCtor, TypeBody, Indent + 1,
        TVarSet, !IO).

:- pred write_type_params(io.text_output_stream::in, tvarset::in,
    list(type_param)::in, io::di, io::uo) is det.

write_type_params(Stream, TVarSet, TypeParams, !IO) :-
    (
        TypeParams = []
    ;
        TypeParams = [HeadParam | TailParams],
        io.write_string(Stream, "(", !IO),
        write_type_params_loop(Stream, TVarSet, HeadParam, TailParams, !IO),
        io.write_string(Stream, ")", !IO)
    ).

:- pred write_type_params_loop(io.text_output_stream::in, tvarset::in,
    type_param::in, list(type_param)::in, io::di, io::uo) is det.

write_type_params_loop(Stream, TVarSet, HeadParam, TailParams, !IO) :-
    mercury_output_var(TVarSet, print_name_only, HeadParam, Stream, !IO),
    (
        TailParams = []
    ;
        TailParams = [HeadTailParam | TailTailParams],
        io.write_string(Stream, ", ", !IO),
        write_type_params_loop(Stream, TVarSet,
            HeadTailParam, TailTailParams, !IO)
    ).

:- pred write_type_body(hlds_out_info::in, io.text_output_stream::in,
    type_ctor::in, hlds_type_body::in, int::in, tvarset::in,
    io::di, io::uo) is det.

write_type_body(Info, Stream, _TypeCtor, TypeBody, Indent, TVarSet, !IO) :-
    (
        TypeBody = hlds_du_type(Ctors, MaybeSuperType, MaybeUserEqComp,
            MaybeRepn, Foreign),
        io.nl(Stream, !IO),
        (
            MaybeSuperType = yes(SuperType),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream, "=< ", !IO),
            mercury_output_type(TVarSet, print_name_only, SuperType,
                Stream, !IO),
            io.nl(Stream, !IO)
        ;
            MaybeSuperType = no
        ),
        MaybeSolverTypeDetails = no,
        MercInfo = Info ^ hoi_merc_out_info,
        (
            MaybeRepn = no,
            Ctors = one_or_more(HeadCtor, TailCtors),
            write_constructors(Stream, TVarSet, Indent,
                HeadCtor, TailCtors, !IO),
            MaybeDirectArgCtors = no,
            mercury_output_where_attributes(MercInfo, TVarSet,
                MaybeSolverTypeDetails, MaybeUserEqComp, MaybeDirectArgCtors,
                Stream, !IO),
            write_indent(Stream, Indent, !IO),
            io.write_string(Stream,
                "% no type representation information yet\n", !IO)
        ;
            MaybeRepn = yes(Repn),
            Repn = du_type_repn(CtorRepns, CtorRepnMap, CheaperTagTest,
                DuTypeKind, MaybeDirectArgCtors),
            write_constructor_repns(Stream, TVarSet, Indent, CtorRepns, !IO),
            (
                CheaperTagTest = no_cheaper_tag_test
            ;
                CheaperTagTest = cheaper_tag_test(ExpConsId, ExpConsTag,
                    CheapConsId, CheapConsTag),
                ExpConsIdStr = cons_id_and_arity_to_string(
                    unqual_cons_id(ExpConsId)),
                CheapConsIdStr = cons_id_and_arity_to_string(
                    unqual_cons_id(CheapConsId)),
                IndentStr = indent_string(Indent),
                io.format(Stream, "%s%% cheaper tag test:\n",
                    [s(IndentStr)], !IO),
                io.format(Stream, "%s%%   from %s\n",
                    [s(IndentStr), s(ExpConsIdStr)], !IO),
                io.format(Stream, "%s%%      %s\n",
                    [s(IndentStr), s(du_cons_tag_to_string(ExpConsTag))], !IO),
                io.format(Stream, "%s%%   to %s\n",
                    [s(IndentStr), s(CheapConsIdStr)], !IO),
                io.format(Stream, "%s%%      %s\n",
                    [s(IndentStr), s(du_cons_tag_to_string(CheapConsTag))], !IO)
            ),
            (
                DuTypeKind = du_type_kind_mercury_enum,
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% KIND enumeration\n", !IO)
            ;
                DuTypeKind = du_type_kind_foreign_enum(Lang),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% KIND foreign enumeration for ", !IO),
                io.write_string(Stream, foreign_language_string(Lang), !IO),
                io.nl(Stream, !IO)
            ;
                DuTypeKind = du_type_kind_direct_dummy,
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% KIND dummy\n", !IO)
            ;
                DuTypeKind = du_type_kind_notag(FunctorName, ArgType,
                    MaybeArgName),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% KIND notag: ", !IO),
                io.write_string(Stream,
                    sym_name_to_escaped_string(FunctorName), !IO),
                io.write_string(Stream, ", ", !IO),
                mercury_output_type(TVarSet, print_name_only, ArgType,
                    Stream, !IO),
                io.write_string(Stream, ", ", !IO),
                (
                    MaybeArgName = yes(ArgName),
                    io.write_string(Stream, ArgName, !IO)
                ;
                    MaybeArgName = no,
                    io.write_string(Stream, "no arg name", !IO)
                ),
                io.nl(Stream, !IO)
            ;
                DuTypeKind = du_type_kind_general,
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% KIND general\n", !IO)
            ),
            mercury_output_where_attributes(MercInfo, TVarSet,
                MaybeSolverTypeDetails, MaybeUserEqComp, MaybeDirectArgCtors,
                Stream, !IO),
            (
                Foreign = yes(_),
                write_indent(Stream, Indent, !IO),
                io.write_string(Stream, "% has foreign_type\n", !IO)
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
                    write_indent(Stream, Indent, !TIO),
                    io.write_string(Stream, 
                        "% BUG SortedCtorRepns != SortedMapCtorRepns\n", !TIO)
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
        IndentStr = indent_string(Indent),
        Indent1Str = indent_string(Indent + 1),
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

:- func foreign_type_assertion_to_string(foreign_type_assertion)
    = string.

foreign_type_assertion_to_string(Assertion) = String :-
    (
        Assertion = foreign_type_can_pass_as_mercury_type,
        String = "pass_as_mercury"
    ;
        Assertion = foreign_type_word_aligned_pointer,
        String = "word_aligned_ptr"
    ;
        Assertion = foreign_type_stable,
        String = "stable"
    ).

:- pred accumulate_ctor_repns(one_or_more(constructor_repn)::in,
    list(constructor_repn)::in, list(constructor_repn)::out) is det.

accumulate_ctor_repns(one_or_more(HeadCR, TailCRs), !AccCRs) :-
    !:AccCRs = [HeadCR | TailCRs] ++ !.AccCRs.

%---------------------%

:- pred write_constructors(io.text_output_stream::in, tvarset::in, int::in,
    constructor::in, list(constructor)::in, io::di, io::uo) is det.

write_constructors(Stream, TVarSet, Indent, HeadCtor, TailCtors, !IO) :-
    ArrowOrSemi0 = "--->    ",
    write_constructors_loop(Stream, TVarSet, Indent, ArrowOrSemi0,
        HeadCtor, TailCtors, !IO).

:- pred write_constructor_repns(io.text_output_stream::in, tvarset::in,
    int::in, list(constructor_repn)::in, io::di, io::uo) is det.

write_constructor_repns(Stream, TVarSet, Indent, CtorRepns, !IO) :-
    (
        CtorRepns = [],
        unexpected($pred, "empty constructor list")
    ;
        CtorRepns = [HeadCtorRepn | TailCtorRepns],
        ArrowOrSemi0 = "--->    ",
        write_constructor_repns_loop(Stream, TVarSet, Indent, ArrowOrSemi0,
            HeadCtorRepn, TailCtorRepns, !IO)
    ).

%---------------------%

:- pred write_constructors_loop(io.text_output_stream::in, tvarset::in,
    int::in, string::in, constructor::in, list(constructor)::in,
    io::di, io::uo) is det.

write_constructors_loop(Stream, TVarSet, Indent, ArrowOrSemi0,
        HeadCtor, TailCtors, !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, ArrowOrSemi0, !IO),
    (
        TailCtors = [],
        write_ctor(Stream, TVarSet, Indent, HeadCtor, !IO)
    ;
        TailCtors = [HeadTailCtor | TailTailCtors],
        write_ctor(Stream, TVarSet, Indent, HeadCtor, !IO),
        ArrowOrSemi = ";       ",
        write_constructors_loop(Stream, TVarSet, Indent, ArrowOrSemi,
            HeadTailCtor, TailTailCtors, !IO)
    ).

:- pred write_constructor_repns_loop(io.text_output_stream::in, tvarset::in,
    int::in, string::in, constructor_repn::in, list(constructor_repn)::in,
    io::di, io::uo) is det.

write_constructor_repns_loop(Stream, TVarSet, Indent, ArrowOrSemi0,
        HeadCtorRepn, TailCtorRepns, !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, ArrowOrSemi0, !IO),
    (
        TailCtorRepns = [],
        write_ctor_repn(Stream, TVarSet, Indent, HeadCtorRepn, !IO)
    ;
        TailCtorRepns = [HeadTailCtorRepn | TailTailCtorRepns],
        write_ctor_repn(Stream, TVarSet, Indent, HeadCtorRepn, !IO),
        ArrowOrSemi = ";       ",
        write_constructor_repns_loop(Stream, TVarSet, Indent, ArrowOrSemi,
            HeadTailCtorRepn, TailTailCtorRepns, !IO)
    ).

%---------------------%

:- pred write_ctor(io.text_output_stream::in, tvarset::in, int::in,
    constructor::in, io::di, io::uo) is det.

write_ctor(Stream, TVarSet, Indent, Ctor, !IO) :-
    % NOTE The code of this predicate is almost identical to the code of
    % write_ctor_repn below and mercury_output_ctor in parse_tree_out.m.
    % Any changes made here will probably need to be made there as well.
    Ctor = ctor(_Ordinal, MaybeExistConstraints, SymName, Args, Arity, _Ctxt),

    % The module name in SymName must be the same as the module qualifier
    % of the type_ctor, so there is no point in printing it.
    Name = unqualify_name(SymName),
    NameStr = mercury_bracketed_atom_to_string(not_next_to_graphic_token, Name),
    % The width of ArrowOrSemi is eight spaces, which is the same as
    % four indents.
    ASIndent = 4,
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet,
        indent_string(Indent + ASIndent), "\n", MaybeExistConstraints,
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
        mercury_output_ctor_args(Stream, TVarSet, Indent + ASIndent + 1,
            AnyFieldName, HeadArg, TailArgs, !IO),
        write_indent(Stream, Indent + ASIndent, !IO),
        io.format(Stream, ")%s\n", [s(BraceSuffix)], !IO)
    ),
    io.write_string(Stream, BraceSuffix, !IO),
    io.write_string(Stream, ExistConstraintsSuffix, !IO),
    io.write_string(Stream, "\n", !IO).

:- pred write_ctor_repn(io.text_output_stream::in, tvarset::in, int::in,
    constructor_repn::in, io::di, io::uo) is det.

write_ctor_repn(Stream, TVarSet, Indent, CtorRepn, !IO) :-
    % NOTE The code of this predicate is almost identical to the code of
    % write_ctor_repn below and mercury_output_ctor in parse_tree_out.m.
    % Any changes made here will probably need to be made there as well.
    CtorRepn = ctor_repn(_Ordinal, MaybeExistConstraints, SymName,
        ConsTag, ArgRepns, Arity, _Ctxt),

    % The module name in SymName must be the same as the module qualifier
    % of the type_ctor, so there is no point in printing it.
    Name = unqualify_name(SymName),
    NameStr = mercury_bracketed_atom_to_string(not_next_to_graphic_token, Name),
    % The width of ArrowOrSemi is eight spaces, which is the same as
    % four indents.
    ASIndent = 4,
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet,
        indent_string(Indent + ASIndent), "\n", MaybeExistConstraints,
        ExistConstraintsPrefix, ExistConstraintsSuffix),
    maybe_brace_for_name_prefix_suffix(Arity, Name, BracePrefix, BraceSuffix),
    io.write_string(Stream, ExistConstraintsPrefix, !IO),
    io.write_string(Stream, BracePrefix, !IO),
    ConsTagString = string.format("%s%% tag: %s\n",
        [s(indent_string(Indent + ASIndent)),
        s(du_cons_tag_to_string(ConsTag))]),
    (
        ArgRepns = [],
        io.format(Stream, "%s%s%s\n%s",
            [s(BracePrefix), s(NameStr), s(BraceSuffix), s(ConsTagString)], !IO)
    ;
        ArgRepns = [HeadArgRepn | TailArgRepns],
        io.format(Stream, "%s%s(\n%s",
            [s(BracePrefix), s(NameStr), s(ConsTagString)], !IO),
        AnyFieldName = does_any_arg_repn_have_a_field_name(ArgRepns),
        mercury_output_ctor_arg_repns(Stream, TVarSet, Indent + ASIndent + 1,
            AnyFieldName, 1, HeadArgRepn, TailArgRepns, !IO),
        write_indent(Stream, Indent + ASIndent, !IO),
        io.format(Stream, ")%s\n", [s(BraceSuffix)], !IO)
    ),
    io.write_string(Stream, ExistConstraintsSuffix, !IO).

%---------------------%

:- pred mercury_output_ctor_args(io.text_output_stream::in, tvarset::in,
    int::in, bool::in, constructor_arg::in, list(constructor_arg)::in,
    io::di, io::uo) is det.

mercury_output_ctor_args(Stream, TVarSet, Indent, AnyFieldName,
        HeadArg, TailArgs, !IO) :-
    HeadArg = ctor_arg(MaybeFieldName, Type, _Context),
    write_indent(Stream, Indent, !IO),
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
        mercury_output_ctor_args(Stream, TVarSet, Indent, AnyFieldName,
            HeadTailArg, TailTailArgs, !IO)
    ).

:- pred mercury_output_ctor_arg_repns(io.text_output_stream::in, tvarset::in,
    int::in, bool::in, int::in, constructor_arg_repn::in,
    list(constructor_arg_repn)::in, io::di, io::uo) is det.

mercury_output_ctor_arg_repns(Stream, TVarSet, Indent, AnyFieldName,
        CurArgNum, HeadArgRepn, TailArgRepns, !IO) :-
    HeadArgRepn = ctor_arg_repn(MaybeFieldName, Type, ArgPosWidth, _Context),
    write_indent(Stream, Indent, !IO),
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
        write_arg_pos_width(Stream, Indent, CurArgNum, ArgPosWidth, !IO)
    ;
        TailArgRepns = [HeadTailArgRepn | TailTailArgRepns],
        io.write_string(Stream, ",\n", !IO),
        write_arg_pos_width(Stream, Indent, CurArgNum, ArgPosWidth, !IO),
        mercury_output_ctor_arg_repns(Stream, TVarSet, Indent, AnyFieldName,
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
        ConsTag = shared_local_tag_no_args(ptag(Ptag), LocalSectag, SectagMask),
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

:- pred write_arg_pos_width(io.text_output_stream::in, int::in, int::in,
    arg_pos_width::in, io::di, io::uo) is det.

write_arg_pos_width(Stream, Indent, CurArgNum, ArgPosWidth, !IO) :-
    write_indent(Stream, Indent, !IO),
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
%
% Write out the typeclass table.
%

:- pred write_classes(hlds_out_info::in, io.text_output_stream::in,
    int::in, class_table::in, io::di, io::uo) is det.

write_classes(Info, Stream, Indent, ClassTable, !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "%-------- Classes --------\n", !IO),
    map.to_assoc_list(ClassTable, ClassTableList),
    list.foldl(write_class_defn(Info, Stream, Indent), ClassTableList, !IO),
    io.nl(Stream, !IO).

:- pred write_class_defn(hlds_out_info::in, io.text_output_stream::in, int::in,
    pair(class_id, hlds_class_defn)::in, io::di, io::uo) is det.

write_class_defn(Info, Stream, Indent, ClassId - ClassDefn, !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% ", !IO),

    write_class_id(Stream, ClassId, !IO),
    io.write_string(Stream, ":\n", !IO),

    ClassDefn = hlds_class_defn(_, Constraints, FunDeps, _, Vars, _, _,
        Interface, VarSet, Context, _),

    maybe_output_context_comment(Stream, Indent, "", Context, !IO),
    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Vars: ", !IO),
    mercury_output_vars(VarSet, VarNamePrint, Vars, Stream, !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Functional dependencies: ", !IO),
    write_out_list(hlds_output_fundep, ", ", FunDeps, Stream, !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Constraints: ", !IO),
    write_out_list(mercury_output_constraint(VarSet, VarNamePrint),
        ", ", Constraints, Stream, !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Class Methods: ", !IO),
    write_out_list(write_class_proc, ", ", Interface, Stream, !IO),
    io.nl(Stream, !IO),
    io.nl(Stream, !IO).

:- pred hlds_output_fundep(hlds_class_fundep::in,
    io.text_output_stream::in, io::di, io::uo) is det.

hlds_output_fundep(fundep(Domain, Range), Stream, !IO) :-
    DomainList = set.to_sorted_list(Domain),
    RangeList = set.to_sorted_list(Range),
    DomainStrs = list.map(string.int_to_string, DomainList),
    RangeStrs = list.map(string.int_to_string, RangeList),
    DomainStr = string.join_list(", ", DomainStrs),
    RangeStr = string.join_list(", ", RangeStrs),
    io.format(Stream, "(%s -> %s)",
        [s(DomainStr), s(RangeStr)], !IO).

    % Just output the class methods as pred_ids and proc_ids because it is
    % probably not that useful to have the names. If that information is
    % needed, it shouldn't be a very difficult fix.
    %
:- pred write_class_proc(pred_proc_id::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_class_proc(proc(PredId, ProcId), Stream, !IO) :-
    pred_id_to_int(PredId, PredInt),
    proc_id_to_int(ProcId, ProcInt),
    io.format(Stream, "proc(pred_id:%d, proc_id:%d)",
        [i(PredInt), i(ProcInt)], !IO).

%---------------------------------------------------------------------------%
%
% Write out the instance table.
%

:- pred write_instances(hlds_out_info::in, io.text_output_stream::in,
    int::in, instance_table::in, io::di, io::uo) is det.

write_instances(Info, Stream, Indent, InstanceTable, !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "%-------- Instances --------\n", !IO),
    map.to_assoc_list(InstanceTable, InstanceTableList),
    list.foldl(write_instance_defns(Info, Stream, Indent),
        InstanceTableList, !IO),
    io.nl(Stream, !IO).

:- pred write_instance_defns(hlds_out_info::in, io.text_output_stream::in,
    int::in, pair(class_id, list(hlds_instance_defn))::in,
    io::di, io::uo) is det.

write_instance_defns(Info, Stream, Indent, ClassId - InstanceDefns, !IO) :-
    io.nl(Stream, !IO),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Instances for class ", !IO),
    write_class_id(Stream, ClassId, !IO),
    io.write_string(Stream, ":\n", !IO),
    list.foldl(write_instance_defn(Info, Stream, Indent + 1),
        InstanceDefns, !IO).

:- pred write_instance_defn(hlds_out_info::in, io.text_output_stream::in,
    int::in, hlds_instance_defn::in, io::di, io::uo) is det.

write_instance_defn(Info, Stream, Indent, InstanceDefn, !IO) :-
    InstanceDefn = hlds_instance_defn(_InstanceModule, Types, OriginalTypes,
        InstanceStatus, Context, Constraints, Body, MaybePredProcIds,
        VarSet, ProofMap),

    % Separate this instance from any previous ones, or the class id.
    io.nl(Stream, !IO),
    maybe_output_context_comment(Stream, Indent, "", Context, !IO),

    DumpOptions = Info ^ hoi_dump_hlds_options,
    ( if string.contains_char(DumpOptions, 'v') then
        VarNamePrint = print_name_and_num
    else
        VarNamePrint = print_name_only
    ),

    % Curry the varset for term_io.write_variable/4.
    PrintTerm = mercury_output_type(VarSet, VarNamePrint),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Types: ", !IO),
    write_out_list(mercury_output_type(VarSet, VarNamePrint), ", ", Types,
        Stream, !IO),
    io.nl(Stream, !IO),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Original types: ", !IO),
    write_out_list(PrintTerm, ", ", OriginalTypes, Stream, !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Status: ", !IO),
    io.write_string(Stream,
        instance_import_status_to_string(InstanceStatus), !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Constraints: ", !IO),
    write_out_list(mercury_output_constraint(VarSet, VarNamePrint),
        ", ", Constraints, Stream, !IO),
    io.nl(Stream, !IO),

    write_indent(Stream, Indent, !IO),
    (
        Body = instance_body_abstract,
        io.write_string(Stream, "% abstract", !IO)
    ;
        Body = instance_body_concrete(Methods),
        io.write_string(Stream, "% Instance methods:\n", !IO),
        write_instance_methods(Stream, Methods, Indent, 1, !IO)
    ),
    io.nl(Stream, !IO),

    (
        MaybePredProcIds = yes(PredProcIds),
        write_indent(Stream, Indent, !IO),
        io.write_string(Stream, "% Procedures: ", !IO),
        io.write(Stream, PredProcIds, !IO),
        io.nl(Stream, !IO)
    ;
        MaybePredProcIds = no
    ),
    write_constraint_proof_map(Stream, Indent, VarNamePrint, VarSet,
        ProofMap, !IO),
    io.nl(Stream, !IO).

:- pred write_instance_methods(io.text_output_stream::in,
    list(instance_method)::in, int::in, int::in, io::di, io::uo) is det.

write_instance_methods(_, [], _, _, !IO).
write_instance_methods(Stream, [Method | Methods], Indent,
        !.CurMethodNum, !IO) :-
    Method = instance_method(PredOrFunc, MethodName, _Defn, Arity, _Context),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, "%% method %d, %s %s/%d\n",
        [i(!.CurMethodNum), s(pred_or_func_to_str(PredOrFunc)),
        s(sym_name_to_string(MethodName)), i(Arity)], !IO),
    mercury_output_instance_method(Method, Stream, !IO),
    (
        Methods = [_ | _],
        io.write_string(Stream, ",\n", !IO),
        !:CurMethodNum = !.CurMethodNum + 1,
        write_instance_methods(Stream, Methods, Indent, !.CurMethodNum, !IO)
    ;
        Methods = []
    ).

%---------------------------------------------------------------------------%
%
% Write out the inst table.
%

:- pred write_inst_table(io.text_output_stream::in, output_lang::in,
    int::in, int::in, inst_table::in, io::di, io::uo) is det.

write_inst_table(Stream, Lang, Indent, Limit, InstTable, !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "%-------- Insts --------\n", !IO),

    inst_table_get_user_insts(InstTable, UserInstTable),
    inst_table_get_unify_insts(InstTable, UnifyInstTable),
    inst_table_get_merge_insts(InstTable, MergeInstTable),
    inst_table_get_ground_insts(InstTable, GroundInstTable),
    inst_table_get_any_insts(InstTable, AnyInstTable),
    inst_table_get_shared_insts(InstTable, SharedInstTable),
    inst_table_get_mostly_uniq_insts(InstTable, MostlyUniqInstTable),

    map.to_sorted_assoc_list(UserInstTable, UserInstPairs),
    unify_insts_to_sorted_pairs(UnifyInstTable, UnifyInstPairs),
    merge_insts_to_sorted_pairs(MergeInstTable, MergeInstPairs),
    ground_insts_to_sorted_pairs(GroundInstTable, GroundInstPairs),
    any_insts_to_sorted_pairs(AnyInstTable, AnyInstPairs),
    shared_insts_to_sorted_pairs(SharedInstTable, SharedInstPairs),
    mostly_uniq_insts_to_sorted_pairs(MostlyUniqInstTable,
        MostlyUniqInstPairs),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "%-------- User defined insts --------\n", !IO),
    list.foldl(write_user_inst(Stream, Indent), UserInstPairs, !IO),

    io.write_string(Stream, "%-------- Unify insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(Stream, Lang, Limit, write_key_unify_inst),
        UnifyInstPairs, 0, NumUnifyInsts, !IO),
    io.format(Stream,
        "\nTotal number of unify insts: %d\n", [i(NumUnifyInsts)], !IO),

    io.write_string(Stream, "%-------- Merge insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(Stream, Lang, Limit, write_key_merge_inst),
        MergeInstPairs, 0, NumMergeInsts, !IO),
    io.format(Stream,
        "\nTotal number of merge insts: %d\n", [i(NumMergeInsts)], !IO),

    io.write_string(Stream, "%-------- Ground insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(Stream, Lang, Limit, write_key_ground_inst),
        GroundInstPairs, 0, NumGroundInsts, !IO),
    io.format(Stream, "\nTotal number of ground insts: %d\n",
        [i(NumGroundInsts)], !IO),

    io.write_string(Stream, "%-------- Any insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst_det(Stream, Lang, Limit, write_key_any_inst),
        AnyInstPairs, 0, NumAnyInsts, !IO),
    io.format(Stream,
        "\nTotal number of any insts: %d\n", [i(NumAnyInsts)], !IO),

    io.write_string(Stream, "%-------- Shared insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(Stream, Lang, Limit, write_inst_name_nl),
        SharedInstPairs, 0, NumSharedInsts, !IO),
    io.format(Stream, "\nTotal number of shared insts: %d\n",
        [i(NumSharedInsts)], !IO),

    io.write_string(Stream, "%-------- MostlyUniq insts --------\n", !IO),
    list.foldl2(
        write_key_maybe_inst(Stream, Lang, Limit, write_inst_name_nl),
        MostlyUniqInstPairs, 0, NumMostlyUniqInsts, !IO),
    io.format(Stream, "\nTotal number of mostly uniq insts: %d\n",
        [i(NumMostlyUniqInsts)], !IO),

    io.nl(Stream, !IO).

:- pred write_user_inst(io.text_output_stream::in, int::in,
    pair(inst_ctor, hlds_inst_defn)::in, io::di, io::uo) is det.

write_user_inst(Stream, Indent, InstCtor - InstDefn, !IO) :-
    InstCtor = inst_ctor(InstName, _InstArity),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, "\n:- inst %s", [s(sym_name_to_string(InstName))], !IO),
    InstDefn = hlds_inst_defn(InstVarSet, InstParams, InstBody,
        _MaybeMatchingTypeCtors, _Context, Status),
    (
        InstParams = []
    ;
        InstParams = [HeadInstParam | TailInstParams],
        io.write_string(Stream, "(", !IO),
        write_inst_params(Stream, HeadInstParam, TailInstParams, InstVarSet,
            !IO),
        io.write_string(Stream, ")", !IO)
    ),
    InstBody = eqv_inst(EqvInst),
    io.write_string(Stream, ":\n", !IO),
    write_indent(Stream, Indent, !IO),
    mercury_output_inst(Stream, output_debug, InstVarSet, EqvInst, !IO),
    io.write_string(Stream, "\n", !IO),
    write_indent(Stream, Indent, !IO),
    StatusStr = inst_import_status_to_string(Status),
    io.format(Stream, "%% status %s\n", [s(StatusStr)], !IO).

:- pred write_inst_params(io.text_output_stream::in, inst_var::in,
    list(inst_var)::in, inst_varset::in, io::di, io::uo) is det.

write_inst_params(Stream, InstVar, InstVars, InstVarSet, !IO) :-
    varset.lookup_name(InstVarSet, InstVar, InstVarName),
    io.write_string(Stream, InstVarName, !IO),
    (
        InstVars = []
    ;
        InstVars = [HeadInstVar | TailInstVars],
        io.write_string(Stream, ", ", !IO),
        write_inst_params(Stream, HeadInstVar, TailInstVars, InstVarSet, !IO)
    ).

:- pred write_key_maybe_inst(io.text_output_stream::in, output_lang::in,
    int::in,
    pred(output_lang, Key, io.text_output_stream, io, io)::
        in(pred(in, in, in, di, uo) is det),
    pair(Key, maybe_inst)::in, int::in, int::out, io::di, io::uo) is det.

write_key_maybe_inst(Stream, Lang, Limit, WriteKey, Key - MaybeInst,
        !N, !IO) :-
    !:N = !.N + 1,
    ( if !.N =< Limit then
        io.format(Stream, "\nEntry %d key\n", [i(!.N)], !IO),
        WriteKey(Lang, Key, Stream, !IO),
        (
            MaybeInst = inst_unknown,
            io.format(Stream, "Entry %d value UNKNOWN\n", [i(!.N)], !IO)
        ;
            MaybeInst = inst_known(Inst),
            io.format(Stream, "Entry %d value:\n", [i(!.N)], !IO),
            write_inst(Stream, Lang, Inst, !IO),
            io.nl(Stream, !IO)
        )
    else
        true
    ).

:- pred write_key_maybe_inst_det(io.text_output_stream::in, output_lang::in,
    int::in,
    pred(output_lang, Key, io.text_output_stream, io, io)::
        in(pred(in, in, in, di, uo) is det),
    pair(Key, maybe_inst_det)::in, int::in, int::out,
    io::di, io::uo) is det.

write_key_maybe_inst_det(Stream, Lang, Limit, WriteKey, Key - MaybeInstDet,
        !N, !IO) :-
    !:N = !.N + 1,
    ( if !.N =< Limit then
        io.format(Stream, "\nEntry %d key\n", [i(!.N)], !IO),
        WriteKey(Lang, Key, Stream, !IO),
        (
            MaybeInstDet = inst_det_unknown,
            io.format(Stream, "Entry %d value UNKNOWN\n", [i(!.N)], !IO)
        ;
            MaybeInstDet = inst_det_known(Inst, Detism),
            DetismStr = determinism_to_string(Detism),
            io.format(Stream, "Entry %d value (%s):\n",
                [i(!.N), s(DetismStr)], !IO),
            write_inst(Stream, Lang, Inst, !IO),
            io.nl(Stream, !IO)
        )
    else
        true
    ).

:- pred write_key_unify_inst(output_lang::in, unify_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_unify_inst(Lang, UnifyInstInfo, Stream, !IO) :-
    UnifyInstInfo = unify_inst_info(Live, Real, InstA, InstB),
    (
        Live = is_live,
        io.write_string(Stream, "live ", !IO)
    ;
        Live = is_dead,
        io.write_string(Stream, "dead ", !IO)
    ),
    (
        Real = real_unify,
        io.write_string(Stream, "real unify\n", !IO)
    ;
        Real = fake_unify,
        io.write_string(Stream, "fake unify\n", !IO)
    ),
    io.write_string(Stream, "InstA: ", !IO),
    write_inst(Stream, Lang, InstA, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, "InstB: ", !IO),
    write_inst(Stream, Lang, InstB, !IO),
    io.nl(Stream, !IO).

:- pred write_key_merge_inst(output_lang::in, merge_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_merge_inst(Lang, MergeInstInfo, Stream, !IO) :-
    MergeInstInfo = merge_inst_info(InstA, InstB),
    io.write_string(Stream, "InstA: ", !IO),
    write_inst(Stream, Lang, InstA, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, "InstB: ", !IO),
    write_inst(Stream, Lang, InstB, !IO),
    io.nl(Stream, !IO).

:- pred write_key_ground_inst(output_lang::in, ground_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_ground_inst(Lang, GroundInstInfo, Stream, !IO) :-
    GroundInstInfo = ground_inst_info(InstName, Uniq, Live, Real),
    write_uniq_live_real(Stream, Uniq, Live, Real, !IO),
    write_inst_name_nl(Lang, InstName, Stream, !IO).

:- pred write_key_any_inst(output_lang::in, any_inst_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_key_any_inst(Lang, AnyInstInfo, Stream, !IO) :-
    AnyInstInfo = any_inst_info(InstName, Uniq, Live, Real),
    write_uniq_live_real(Stream, Uniq, Live, Real, !IO),
    write_inst_name_nl(Lang, InstName, Stream, !IO).

:- pred write_uniq_live_real(io.text_output_stream::in,
    uniqueness::in, is_live::in, unify_is_real::in, io::di, io::uo) is det.

write_uniq_live_real(Stream, Uniq, Live, Real, !IO) :-
    (
        Uniq = shared,
        io.write_string(Stream, "shared ", !IO)
    ;
        Uniq = unique,
        io.write_string(Stream, "unique ", !IO)
    ;
        Uniq = mostly_unique,
        io.write_string(Stream, "mostly_unique ", !IO)
    ;
        Uniq = clobbered,
        io.write_string(Stream, "clobbered", !IO)
    ;
        Uniq = mostly_clobbered,
        io.write_string(Stream, "mostly_clobbered", !IO)
    ),
    (
        Live = is_live,
        io.write_string(Stream, "live ", !IO)
    ;
        Live = is_dead,
        io.write_string(Stream, "dead ", !IO)
    ),
    (
        Real = real_unify,
        io.write_string(Stream, "real unify\n", !IO)
    ;
        Real = fake_unify,
        io.write_string(Stream, "fake unify\n", !IO)
    ).

:- pred write_inst_name_nl(output_lang::in, inst_name::in,
    io.text_output_stream::in, io::di, io::uo) is det.

write_inst_name_nl(Lang, InstName, Stream, !IO) :-
    InstNameTerm = inst_name_to_term(Lang, InstName),
    varset.init(VarSet),
    mercury_output_term(VarSet, print_name_only, InstNameTerm, Stream, !IO),
    io.nl(Stream, !IO).

:- pred write_inst(io.text_output_stream::in, output_lang::in, mer_inst::in,
    io::di, io::uo) is det.

write_inst(Stream, Lang, Inst, !IO) :-
    InstTerm = inst_to_term(Lang, Inst),
    varset.init(VarSet),
    mercury_output_term(VarSet, print_name_only, InstTerm, Stream, !IO).

%---------------------------------------------------------------------------%
%
% Write out the mode table.
%

:- pred write_mode_table(io.text_output_stream::in, int::in, mode_table::in,
    io::di, io::uo) is det.

write_mode_table(Stream, Indent, ModeTable, !IO) :-
    mode_table_get_mode_defns(ModeTable, ModeDefns),
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "%-------- Modes --------\n", !IO),
    write_indent(Stream, Indent, !IO),
    map.foldl(write_mode_table_entry(Stream, Indent), ModeDefns, !IO),
    io.nl(Stream, !IO).

:- pred write_mode_table_entry(io.text_output_stream::in, int::in,
    mode_ctor::in, hlds_mode_defn::in, io::di, io::uo) is det.

write_mode_table_entry(Stream, Indent, ModeCtor, ModeDefn, !IO) :-
    ModeCtor = mode_ctor(ModeName, _ModeArity),
    write_indent(Stream, Indent, !IO),
    io.format(Stream, "\n:- mode %s", [s(sym_name_to_string(ModeName))], !IO),
    ModeDefn = hlds_mode_defn(InstVarSet, InstParams, ModeBody, _Context,
        Status),
    (
        InstParams = []
    ;
        InstParams = [HeadInstParam | TailInstParams],
        io.write_string(Stream, "(", !IO),
        write_inst_params(Stream, HeadInstParam, TailInstParams,
            InstVarSet, !IO),
        io.write_string(Stream, ")", !IO)
    ),
    ModeBody = hlds_mode_body(EqvMode),
    io.write_string(Stream, ":\n", !IO),
    write_indent(Stream, Indent, !IO),
    mercury_output_mode(Stream, output_debug, InstVarSet, EqvMode, !IO),
    io.write_string(Stream, "\n", !IO),
    write_indent(Stream, Indent, !IO),
    StatusStr = mode_import_status_to_string(Status),
    io.format(Stream, "%% status %s\n", [s(StatusStr)], !IO).

%---------------------------------------------------------------------------%
%
% Write out constant structs defined in the module.
%

:- pred write_const_struct_db(io.text_output_stream::in, const_struct_db::in,
    io::di, io::uo) is det.

write_const_struct_db(Stream, ConstStructDb, !IO) :-
    const_struct_db_get_structs(ConstStructDb, ConstStructs),
    io.write_string(Stream, "%-------- Const structs --------\n\n", !IO),
    list.foldl(write_const_struct(Stream), ConstStructs, !IO),
    io.nl(Stream, !IO).

:- pred write_const_struct(io.text_output_stream::in,
    pair(int, const_struct)::in, io::di, io::uo) is det.

write_const_struct(Stream, N - ConstStruct, !IO) :-
    io.format(Stream, "\nconst_struct %d:\n", [i(N)], !IO),
    ConstStruct = const_struct(ConsId, ConstArgs, Type, Inst, DefinedWhere),
    mercury_output_cons_id(output_debug, does_not_need_brackets, ConsId,
        Stream, !IO),
    (
        ConstArgs = [],
        io.nl(Stream, !IO)
    ;
        ConstArgs = [HeadConstArg | TailConstArgs],
        io.write_string(Stream, "(\n", !IO),
        write_const_struct_args(Stream, HeadConstArg, TailConstArgs, !IO),
        io.write_string(Stream, ")\n", !IO)
    ),
    io.write_string(Stream, "type: ", !IO),
    mercury_output_type(varset.init, print_name_only, Type, Stream, !IO),
    io.nl(Stream, !IO),
    io.write_string(Stream, "inst: ", !IO),
    mercury_output_structured_inst(Stream, Inst, 0, output_debug,
        do_not_incl_addr, varset.init, !IO),
    (
        DefinedWhere = defined_in_this_module,
        io.write_string(Stream, "defined_in_this_module\n", !IO)
    ;
        DefinedWhere = defined_in_other_module,
        io.write_string(Stream, "defined_in_other_module\n", !IO)
    ).

:- pred write_const_struct_args(io.text_output_stream::in,
    const_struct_arg::in, list(const_struct_arg)::in, io::di, io::uo) is det.

write_const_struct_args(Stream, HeadConstArg, TailConstArgs, !IO) :-
    io.write_string(Stream, "    ", !IO),
    (
        HeadConstArg = csa_const_struct(N),
        io.format(Stream, "cs(%d)", [i(N)], !IO)
    ;
        HeadConstArg = csa_constant(ConsId, Type),
        mercury_output_cons_id(output_debug, does_not_need_brackets, ConsId,
            Stream, !IO),
        io.write_string(Stream, "\n        with type ", !IO),
        mercury_output_type(varset.init, print_name_only, Type, Stream, !IO)
    ),
    (
        TailConstArgs = [],
        io.write_string(Stream, "\n", !IO)
    ;
        TailConstArgs = [HeadTailConstArg | TailTailConstArgs],
        io.write_string(Stream, ",\n", !IO),
        write_const_struct_args(Stream,
            HeadTailConstArg, TailTailConstArgs, !IO)
    ).

%---------------------------------------------------------------------------%
%
% Write out tabling structs defined in the module.
%

:- pred write_table_structs(io.text_output_stream::in, module_info::in,
    table_struct_map::in, io::di, io::uo) is det.

write_table_structs(Stream, ModuleInfo, TableStructMap, !IO) :-
    map.to_assoc_list(TableStructMap, TableStructs),
    io.write_string(Stream, "%-------- Table structs --------\n", !IO),
    list.foldl(write_table_struct_info(Stream, ModuleInfo), TableStructs, !IO),
    io.nl(Stream, !IO).

:- pred write_table_struct_info(io.text_output_stream::in, module_info::in,
    pair(pred_proc_id, table_struct_info)::in, io::di, io::uo) is det.

write_table_struct_info(Stream, ModuleInfo, PredProcId - TableStructInfo,
        !IO) :-
    PredProcIdStr = pred_proc_id_to_string(ModuleInfo, PredProcId),
    io.format(Stream, "\n%% table struct info for %s\n",
        [s(PredProcIdStr)], !IO),
    TableStructInfo = table_struct_info(ProcTableStructInfo, Attributes),
    ProcTableStructInfo = proc_table_struct_info(_ProcLabel, TVarSet, _Context,
        NumInputs, NumOutputs, InputSteps, MaybeOutputSteps, ArgInfos,
        _EvalMethod),
    io.format(Stream, "%% #inputs: %d, #outputs: %d\n",
        [i(NumInputs), i(NumOutputs)], !IO),
    io.write_string(Stream, "% input steps:", !IO),
    list.foldl(write_space_and_table_trie_step(Stream, TVarSet),
        InputSteps, !IO),
    io.nl(Stream, !IO),
    (
        MaybeOutputSteps = yes(OutputSteps),
        io.write_string(Stream, "% output steps:", !IO),
        list.foldl(write_space_and_table_trie_step(Stream, TVarSet),
            OutputSteps, !IO),
        io.nl(Stream, !IO)
    ;
        MaybeOutputSteps = no,
        io.write_string(Stream, "% no output steps", !IO)
    ),
    write_table_arg_infos(Stream, TVarSet, ArgInfos, !IO),

    Attributes = table_attributes(Strictness, SizeLimit, Stats, AllowReset,
        BackendWarning),
    (
        Strictness = cts_all_strict,
        io.write_string(Stream, "% all strict\n", !IO)
    ;
        Strictness = cts_all_fast_loose,
        io.write_string(Stream, "% all fast_loose\n", !IO)
    ;
        Strictness = cts_specified(ArgMethods, HiddenArgMethod),
        io.write_string(Stream, "% specified [", !IO),
        write_arg_tabling_methods(Stream, "", ArgMethods, !IO),
        io.write_string(Stream, "]", !IO),
        (
            HiddenArgMethod = table_hidden_arg_value,
            io.write_string(Stream, ", hidden args by value\n", !IO)
        ;
            HiddenArgMethod = table_hidden_arg_addr,
            io.write_string(Stream, ", hidden args by addr\n", !IO)
        )
    ),
    (
        SizeLimit = no,
        io.write_string(Stream, "% no size limit\n", !IO)
    ;
        SizeLimit = yes(Limit),
        io.format(Stream, "%% size limit %d\n", [i(Limit)], !IO)
    ),
    (
        Stats = table_gather_statistics,
        io.write_string(Stream, "% gather statistics\n", !IO)
    ;
        Stats = table_dont_gather_statistics,
        io.write_string(Stream, "% do not gather statistics\n", !IO)
    ),
    (
        AllowReset = table_allow_reset,
        io.write_string(Stream, "% allow reset\n", !IO)
    ;
        AllowReset = table_dont_allow_reset,
        io.write_string(Stream, "% do not allow reset\n", !IO)
    ),
    (
        BackendWarning = table_attr_ignore_with_warning,
        io.write_string(Stream, "% ignore only with warning\n", !IO)
    ;
        BackendWarning = table_attr_ignore_without_warning,
        io.write_string(Stream, "% may ignore without warning\n", !IO)
    ).

:- pred write_arg_tabling_methods(io.text_output_stream::in, string::in,
    list(maybe(arg_tabling_method))::in, io::di, io::uo) is det.

write_arg_tabling_methods(_, _, [], !IO).
write_arg_tabling_methods(Stream, Prefix, [MaybeMethod | MaybeMethods], !IO) :-
    io.write_string(Stream, Prefix, !IO),
    (
        MaybeMethod = no,
        io.write_string(Stream, "output", !IO)
    ;
        MaybeMethod = yes(arg_value),
        io.write_string(Stream, "value", !IO)
    ;
        MaybeMethod = yes(arg_addr),
        io.write_string(Stream, "addr", !IO)
    ;
        MaybeMethod = yes(arg_promise_implied),
        io.write_string(Stream, "promise_implied", !IO)
    ),
    write_arg_tabling_methods(Stream, ", ", MaybeMethods, !IO).

%---------------------------------------------------------------------------%
%
% Write out the predicate table.
%

:- pred write_preds(hlds_out_info::in, io.text_output_stream::in,
    bool::in, list(string)::in, output_lang::in, int::in, module_info::in,
    io::di, io::uo) is det.

write_preds(Info, Stream, DumpSpecPreds, DumpSpecPredTypeNames, Lang, Indent,
        ModuleInfo, !IO) :-
    io.write_string(Stream, "%-------- Predicates --------\n\n", !IO),
    write_indent(Stream, Indent, !IO),
    module_info_get_preds(ModuleInfo, PredTable),
    map.to_assoc_list(PredTable, PredIdsInfos),
    (
        DumpSpecPreds = no,
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, dump_hlds_pred_name_order,
            NameOrder),
        (
            NameOrder = no,
            PrintPredIdsInfos = PredIdsInfos
        ;
            NameOrder = yes,
            list.sort(compare_in_name_order, PredIdsInfos, PrintPredIdsInfos)
        )
    ;
        DumpSpecPreds = yes,
        map.init(SpecPredMap0),
        list.foldl(add_spec_preds_to_map(DumpSpecPredTypeNames), PredIdsInfos,
            SpecPredMap0, SpecPredMap),
        map.values(SpecPredMap, PrintPredIdsInfos)
    ),
    list.foldl(maybe_write_pred(Info, Stream, Lang, Indent, ModuleInfo),
        PrintPredIdsInfos, !IO).

:- pred compare_in_name_order(
    pair(pred_id, pred_info)::in,
    pair(pred_id, pred_info)::in,
    comparison_result::out) is det.

compare_in_name_order(PredIdA - PredInfoA, PredIdB - PredInfoB, Result) :-
    pred_info_get_name(PredInfoA, PredNameA),
    pred_info_get_name(PredInfoB, PredNameB),
    compare(NameResult, PredNameA, PredNameB),
    (
        ( NameResult = (<)
        ; NameResult = (>)
        ),
        Result = NameResult
    ;
        NameResult = (=),
        compare(Result, PredIdA, PredIdB)
    ).

:- pred add_spec_preds_to_map(list(string)::in, pair(pred_id, pred_info)::in,
    map({type_ctor, special_pred_id}, pair(pred_id, pred_info))::in,
    map({type_ctor, special_pred_id}, pair(pred_id, pred_info))::out) is det.

add_spec_preds_to_map(DumpSpecPredTypeNames, PredIdInfo, !SpecPredMap) :-
    PredIdInfo = _PredId - PredInfo,
    pred_info_get_origin(PredInfo, Origin),
    ( if Origin = origin_special_pred(SpecialPredId, TypeCtor) then
        (
            DumpSpecPredTypeNames = [],
            map.det_insert({TypeCtor, SpecialPredId}, PredIdInfo, !SpecPredMap)
        ;
            DumpSpecPredTypeNames = [_ | _],
            TypeCtor = type_ctor(TypeCtorSymName, _TypeCtorArity),
            TypeCtorName = unqualify_name(TypeCtorSymName),
            ( if list.member(TypeCtorName, DumpSpecPredTypeNames) then
                map.det_insert({TypeCtor, SpecialPredId}, PredIdInfo,
                    !SpecPredMap)
            else
                true
            )
        )
    else
        true
    ).

:- pred maybe_write_pred(hlds_out_info::in, io.text_output_stream::in,
    output_lang::in, int::in, module_info::in, pair(pred_id, pred_info)::in,
    io::di, io::uo) is det.

maybe_write_pred(Info, Stream, Lang, Indent, ModuleInfo, PredId - PredInfo,
        !IO) :-
    DumpOptions = Info ^ hoi_dump_hlds_options,
    DumpPredIdStrs = Info ^ hoi_dump_hlds_pred_ids,
    DumpPredNames = Info ^ hoi_dump_hlds_pred_names,
    pred_id_to_int(PredId, PredIdInt),
    ( if
        % If the user requested one or more predicates/functions to be dumped,
        % we dump them even if the condition of the nested if-then-else below
        % would say they shouldn't be dumped, and we don't dump anything else.
        ( DumpPredIdStrs = [_ | _]
        ; DumpPredNames = [_ | _]
        )
    then
        ( if
            (
                some [DumpPredIdStr, DumpPredId] (
                    list.member(DumpPredIdStr, DumpPredIdStrs),
                    string.to_int(DumpPredIdStr, DumpPredId),
                    PredIdInt = DumpPredId
                )
            ;
                PredName = pred_info_name(PredInfo),
                list.member(PredName, DumpPredNames)
            )
        then
            write_pred(Info, Stream, Lang, ModuleInfo, Indent,
                PredId, PredInfo, !IO)
        else
            true
        )
    else
        ( if
            (
                not string.contains_char(DumpOptions, 'I'),
                pred_info_is_imported(PredInfo)
            ;
                % For pseudo-imported predicates (i.e. unification preds),
                % only print them if we are using a local mode for them.
                not string.contains_char(DumpOptions, 'I'),
                pred_info_is_pseudo_imported(PredInfo),
                ProcIds = pred_info_all_procids(PredInfo),
                hlds_pred.in_in_unification_proc_id(ProcId),
                ProcIds = [ProcId]
            ;
                % We dump unification and other compiler-generated special
                % predicates if suboption 'U' is on. We don't need that
                % information to understand how the program has been
                % transformed.
                not string.contains_char(DumpOptions, 'U'),
                is_unify_index_or_compare_pred(PredInfo)
            )
        then
            true
        else
            write_pred(Info, Stream, Lang, ModuleInfo, Indent,
                PredId, PredInfo, !IO)
        )
    ).

%---------------------------------------------------------------------------%
%
% Write out a promise.
%

write_promise(Info, Stream, ModuleInfo, VarSet, TypeQual, VarNamePrint, Indent,
        PromiseType, _PredId, _PredOrFunc, HeadVars, Clause, !IO) :-
    % Please *either* keep this code in sync with mercury_output_item_promise
    % in parse_tree_out.m, *or* rewrite it to forward the work to that
    % predicate.

    % Curry the varset for term_io.write_variable/4.
    PrintVar =
        ( pred(VarName::in, S::in, IOState0::di, IOState::uo) is det :-
            term_io.write_variable(S, VarName, VarSet, IOState0, IOState)
        ),

    write_indent(Stream, Indent, !IO),

    % Print initial formatting differently for assertions.
    (
        PromiseType = promise_type_true,
        io.write_string(Stream, ":- promise all [", !IO),
        write_out_list(PrintVar, ", ", HeadVars, Stream, !IO),
        io.write_string(Stream, "] (\n", !IO)
    ;
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        io.write_string(Stream, ":- all [", !IO),
        write_out_list(PrintVar, ", ", HeadVars, Stream, !IO),
        io.write_string(Stream, "]", !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, promise_to_string(PromiseType), !IO),
        mercury_output_newline(Indent, Stream, !IO),
        io.write_string(Stream, "(\n", !IO)
    ),

    Goal = Clause ^ clause_body,
    do_write_goal(Info, Stream, ModuleInfo, VarSet, TypeQual, VarNamePrint,
        Indent+1, ").\n", Goal, !IO).

%---------------------------------------------------------------------------%
%
% Write out dependency information.
%

:- pred write_dependency_info(hlds_out_info::in, io.text_output_stream::in,
    int::in, module_info::in, hlds_dependency_info::in, io::di, io::uo) is det.

write_dependency_info(_Info, Stream, Indent, ModuleInfo, DependencyInfo,
        !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% Dependency graph\n\n", !IO),
    Graph = dependency_info_get_graph(DependencyInfo),
    digraph.traverse(Graph,
        write_dep_graph_node(Stream, Indent, ModuleInfo),
        write_dep_graph_edge(Stream, Indent, ModuleInfo), !IO),

% If needed, this code can be used to check the raw behavior
% of digraph operations.
%
%   ( if tsort(Graph, TSort) then
%       write_indent(Stream, Indent, !IO),
%       io.write_string(Stream, "\n% TSORT ordering\n\n", !IO),
%       list.foldl(write_dependency_proc(Indent, "", ModuleInfo), TSort, !IO)
%   else
%       io.write_string(Stream, "\n% NO TSORT ordering\n\n", !IO)
%   ),
%
%   write_indent(Stream, Indent, !IO),
%   io.write_string(Stream, "\n% ATSORT ordering\n\n", !IO),
%   AtSort = digraph.atsort(Graph),
%   list.foldl(write_dependency_scc(Indent, ModuleInfo), AtSort, !IO),

    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "\n% Bottom up dependency sccs\n\n", !IO),
    Ordering = dependency_info_get_bottom_up_sccs(DependencyInfo),
    list.foldl(write_dependency_scc(Stream, Indent, ModuleInfo), Ordering, !IO).

:- pred write_dep_graph_node(io.text_output_stream::in, int::in,
    module_info::in, pred_proc_id::in, io::di, io::uo) is det.

write_dep_graph_node(Stream, Indent, ModuleInfo, Proc, !IO) :-
    write_dependency_proc(Stream, Indent, "calls from ", ModuleInfo, Proc,
        !IO).

:- pred write_dep_graph_edge(io.text_output_stream::in, int::in,
    module_info::in, pred_proc_id::in, pred_proc_id::in,
    io::di, io::uo) is det.

write_dep_graph_edge(Stream, Indent, ModuleInfo, _ParentProc, ChildProc,
        !IO) :-
    write_dependency_proc(Stream, Indent, "  to ", ModuleInfo, ChildProc, !IO).

:- pred write_dependency_scc(io.text_output_stream::in, int::in,
    module_info::in, scc::in, io::di, io::uo) is det.

write_dependency_scc(Stream, Indent, ModuleInfo, SCC, !IO) :-
    write_indent(Stream, Indent, !IO),
    io.write_string(Stream, "% SCC\n", !IO),
    set.foldl(write_dependency_proc(Stream, Indent, "  ", ModuleInfo),
        SCC, !IO).

:- pred write_dependency_proc(io.text_output_stream::in, int::in, string::in,
    module_info::in, pred_proc_id::in, io::di, io::uo) is det.

write_dependency_proc(Stream, Indent, Prefix, ModuleInfo, PredProcId, !IO) :-
    PredProcId = proc(PredId, ProcId),
    Pieces = describe_one_proc_name(ModuleInfo,
        should_not_module_qualify, PredProcId),
    Desc = error_pieces_to_string(Pieces),

    write_indent(Stream, Indent, !IO),
    io.format(Stream, "%% %spred %d proc %d, %s\n",
        [s(Prefix), i(pred_id_to_int(PredId)), i(proc_id_to_int(ProcId)),
        s(Desc)], !IO).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_out.hlds_out_module.
%---------------------------------------------------------------------------%
