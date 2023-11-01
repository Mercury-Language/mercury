%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015-2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module converts the top levels of the parse tree structure
% back into Mercury source text.
%
%---------------------------------------------------------------------------%

:- module parse_tree.parse_tree_out.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % output_parse_tree_*(ProgressStream, Globals,
    %   OutputFileName, ParseTree, Succeeded, !IO).

:- pred output_parse_tree_src(io.text_output_stream::in, globals::in,
    string::in, parse_tree_src::in, maybe_succeeded::out,
    io::di, io::uo) is det.

:- pred output_parse_tree_int0(io.text_output_stream::in, globals::in,
    string::in, parse_tree_int0::in, maybe_succeeded::out,
    io::di, io::uo) is det.
:- pred output_parse_tree_int1(io.text_output_stream::in, globals::in,
    string::in, parse_tree_int1::in, maybe_succeeded::out,
    io::di, io::uo) is det.
:- pred output_parse_tree_int2(io.text_output_stream::in, globals::in,
    string::in, parse_tree_int2::in, maybe_succeeded::out,
    io::di, io::uo) is det.
:- pred output_parse_tree_int3(io.text_output_stream::in, globals::in,
    string::in, parse_tree_int3::in, maybe_succeeded::out,
    io::di, io::uo) is det.

:- pred output_parse_tree_plain_opt(io.text_output_stream::in, globals::in,
    string::in, parse_tree_plain_opt::in, maybe_succeeded::out,
    io::di, io::uo) is det.
:- pred output_parse_tree_trans_opt(io.text_output_stream::in, globals::in,
    string::in, parse_tree_trans_opt::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_parse_tree_src(merc_out_info::in,
    io.text_output_stream::in, parse_tree_src::in, io::di, io::uo) is det.
:- pred mercury_format_parse_tree_src(merc_out_info::in,
    S::in, parse_tree_src::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_output_parse_tree_module_src(merc_out_info::in,
    io.text_output_stream::in, parse_tree_module_src::in,
    io::di, io::uo) is det.
:- pred mercury_format_parse_tree_module_src(merc_out_info::in, S::in,
    parse_tree_module_src::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_output_ancestor_int_spec(merc_out_info::in,
    io.text_output_stream::in, ancestor_int_spec::in, io::di, io::uo) is det.
:- pred mercury_output_direct_int1_spec(merc_out_info::in,
    io.text_output_stream::in, direct_int1_spec::in, io::di, io::uo) is det.
:- pred mercury_output_direct_int3_spec(merc_out_info::in,
    io.text_output_stream::in, direct_int3_spec::in, io::di, io::uo) is det.
:- pred mercury_output_indirect_int2_spec(merc_out_info::in,
    io.text_output_stream::in, indirect_int2_spec::in, io::di, io::uo) is det.
:- pred mercury_output_indirect_int3_spec(merc_out_info::in,
    io.text_output_stream::in, indirect_int3_spec::in, io::di, io::uo) is det.
:- pred mercury_output_int_for_opt_spec(merc_out_info::in,
    io.text_output_stream::in, int_for_opt_spec::in, io::di, io::uo) is det.
:- pred mercury_output_type_repn_spec(merc_out_info::in,
    io.text_output_stream::in, type_repn_spec::in, io::di, io::uo) is det.

:- pred mercury_output_parse_tree_int0(merc_out_info::in,
    io.text_output_stream::in, parse_tree_int0::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_int1(merc_out_info::in,
    io.text_output_stream::in, parse_tree_int1::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_int2(merc_out_info::in,
    io.text_output_stream::in, parse_tree_int2::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_int3(merc_out_info::in,
    io.text_output_stream::in, parse_tree_int3::in, io::di, io::uo) is det.

:- func parse_tree_int0_to_string(merc_out_info, parse_tree_int0) = string.
:- func parse_tree_int1_to_string(merc_out_info, parse_tree_int1) = string.
:- func parse_tree_int2_to_string(merc_out_info, parse_tree_int2) = string.
:- func parse_tree_int3_to_string(merc_out_info, parse_tree_int3) = string.

:- pred mercury_format_parse_tree_int0(merc_out_info::in, S::in,
    parse_tree_int0::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_parse_tree_int1(merc_out_info::in, S::in,
    parse_tree_int1::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_parse_tree_int2(merc_out_info::in, S::in,
    parse_tree_int2::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_parse_tree_int3(merc_out_info::in, S::in,
    parse_tree_int3::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_output_parse_tree_plain_opt(merc_out_info::in,
    io.text_output_stream::in, parse_tree_plain_opt::in,
    io::di, io::uo) is det.
:- pred mercury_output_parse_tree_trans_opt(merc_out_info::in,
    io.text_output_stream::in, parse_tree_trans_opt::in,
    io::di, io::uo) is det.

:- pred mercury_format_parse_tree_plain_opt(merc_out_info::in, S::in,
    parse_tree_plain_opt::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_parse_tree_trans_opt(merc_out_info::in, S::in,
    parse_tree_trans_opt::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % mercury_output_module_decl(Stream, Decl, ModuleName, !IO)
    %
:- pred mercury_output_module_decl(io.text_output_stream::in,
    string::in, module_name::in, io::di, io::uo) is det.
:- pred mercury_format_module_decl(S::in, string::in, module_name::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

:- pred mercury_format_item(merc_out_info::in, S::in, item::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output some components of type definitions.
%

:- pred mercury_format_item_type_defn(merc_out_info::in, S::in,
    item_type_defn_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_where_attributes(merc_out_info::in, tvarset::in,
    maybe(solver_type_details)::in, maybe_canonical::in,
    maybe(list(sym_name_arity))::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_format_ctor(tvarset::in, constructor::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred maybe_cons_exist_constraints_to_prefix_suffix(tvarset::in,
    string::in, string::in, maybe_cons_exist_constraints::in,
    string::out, string::out) is det.

:- pred maybe_brace_for_name_prefix_suffix(arity::in, string::in,
    string::out, string::out) is det.

%---------------------------------------------------------------------------%

:- pred mercury_format_item_inst_defn(merc_out_info::in, S::in,
    item_inst_defn_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_mode_defn(merc_out_info::in, S::in,
    item_mode_defn_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_pred_decl(output_lang::in, var_name_print::in,
    S::in, item_pred_decl_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_mode_decl(merc_out_info::in,
    S::in, item_mode_decl_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_foreign_enum(merc_out_info::in, S::in,
    item_foreign_enum_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_format_item_typeclass(merc_out_info::in,
    S::in, item_typeclass_info::in, U::di, U::uo) is det <= pt_output(S, U).
:- pred mercury_format_item_abstract_typeclass(merc_out_info::in,
    S::in, item_abstract_typeclass_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output some components of an instance definition.
%

:- pred mercury_format_item_instance(merc_out_info::in, S::in,
    item_instance_info::in, U::di, U::uo) is det <= pt_output(S, U).

:- func item_abstract_instance_to_string(merc_out_info,
    item_abstract_instance_info) = string.
:- pred mercury_format_item_abstract_instance(merc_out_info::in,
    S::in, item_abstract_instance_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

:- pred mercury_format_instance_method(instance_method::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%
% Output a foreign_import_module pragma.
%

:- pred mercury_output_fim_spec(io.text_output_stream::in, fim_spec::in,
    io::di, io::uo) is det.
:- pred mercury_format_fim_spec(S::in, fim_spec::in, U::di, U::uo) is det
    <= pt_output(S, U).

%---------------------------------------------------------------------------%

    % Print a blank line if the given list is not empty.
    %
:- pred maybe_format_block_start_blank_line(S::in, list(T)::in,
    U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.canonicalize_interface.
:- import_module parse_tree.item_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.parse_tree_out_type_repn.
:- import_module parse_tree.prog_util.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module char.
:- import_module cord.
:- import_module map.
:- import_module one_or_more.
:- import_module ops.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module string.builder.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

output_parse_tree_src(ProgressStream, Globals,
        OutputFileName, ParseTreeSrc, Succeeded, !IO) :-
    output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, mercury_output_parse_tree_src, ParseTreeSrc,
        Succeeded, !IO).

%---------------------%

output_parse_tree_int0(ProgressStream, Globals,
        OutputFileName, ParseTreeInt0, Succeeded, !IO) :-
    output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, mercury_output_parse_tree_int0, ParseTreeInt0,
        Succeeded, !IO).

output_parse_tree_int1(ProgressStream, Globals,
        OutputFileName, ParseTreeInt1, Succeeded, !IO) :-
    output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, mercury_output_parse_tree_int1, ParseTreeInt1,
        Succeeded, !IO).

output_parse_tree_int2(ProgressStream, Globals,
        OutputFileName, ParseTreeInt2, Succeeded, !IO) :-
    output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, mercury_output_parse_tree_int2, ParseTreeInt2,
        Succeeded, !IO).

output_parse_tree_int3(ProgressStream, Globals,
        OutputFileName, ParseTreeInt3, Succeeded, !IO) :-
    output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, mercury_output_parse_tree_int3, ParseTreeInt3,
        Succeeded, !IO).

%---------------------%

output_parse_tree_plain_opt(ProgressStream, Globals,
        OutputFileName, ParseTreePlainOpt, Succeeded, !IO) :-
    output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, mercury_output_parse_tree_plain_opt, ParseTreePlainOpt,
        Succeeded, !IO).

output_parse_tree_trans_opt(ProgressStream, Globals,
        OutputFileName, ParseTreeTransOpt, Succeeded, !IO) :-
    output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, mercury_output_parse_tree_trans_opt, ParseTreeTransOpt,
        Succeeded, !IO).

%---------------------------------------------------------------------------%

:- type output_parse_tree(PT) ==
    pred(merc_out_info, io.text_output_stream, PT, io, io).
:- inst output_parse_tree == (pred(in, in, in, di, uo) is det).

:- pred output_some_parse_tree(io.text_output_stream::in, globals::in,
    string::in, output_parse_tree(PT)::in(output_parse_tree),
    PT::in, maybe_succeeded::out, io::di, io::uo) is det.

output_some_parse_tree(ProgressStream, Globals,
        OutputFileName, OutputParseTree, ParseTree, Succeeded, !IO) :-
    io.open_output(OutputFileName, Res, !IO),
    (
        Res = ok(FileStream),
        globals.lookup_bool_option(Globals, verbose, Verbose),
        (
            Verbose = yes,
            io.format(ProgressStream, "%% Writing output to %s...",
                [s(OutputFileName)], !IO),
            io.flush_output(ProgressStream, !IO)
        ;
            Verbose = no
        ),

        % Module qualifiers on items are redundant after the
        % declaration above.
        % XXX What declaration?
        Info = init_merc_out_info(Globals, unqualified_item_names,
            output_mercury),
        OutputParseTree(Info, FileStream, ParseTree, !IO),
        io.close_output(FileStream, !IO),
        (
            Verbose = yes,
            io.write_string(ProgressStream, " done\n", !IO)
        ;
            Verbose = no
        ),
        Succeeded = succeeded
    ;
        Res = error(_),
        io.format(ProgressStream,
            "Error: couldn't open file `%s' for output.\n",
            [s(OutputFileName)], !IO),
        Succeeded = did_not_succeed
    ).

%---------------------------------------------------------------------------%

mercury_output_parse_tree_src(Info, Stream, ParseTree, !IO) :-
    mercury_format_parse_tree_src(Info, Stream, ParseTree, !IO).

mercury_format_parse_tree_src(Info, S, ParseTree, !U) :-
    ParseTree = parse_tree_src(ModuleName, _Context, ModuleComponentsCord),
    mercury_format_module_decl(S, "module", ModuleName, !U),
    ModuleComponents = cord.list(ModuleComponentsCord),
    mercury_format_module_components(Info, S, no, ModuleComponents, !U),
    mercury_format_module_decl(S, "end_module", ModuleName, !U).

%---------------------------------------------------------------------------%

mercury_output_parse_tree_module_src(Info, Stream, ParseTreeModuleSrc, !IO) :-
    mercury_format_parse_tree_module_src(Info, Stream,
        ParseTreeModuleSrc, !IO).

mercury_format_parse_tree_module_src(Info, S, ParseTreeModuleSrc, !U) :-
    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, _ModuleContext,
        InclMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, IntSelfFIMLangs, ImpSelfFIMLangs,

        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        _TypeSpecs, _InstModeSpecs,

        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises, _IntBadPreds,

        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpClauses, ImpForeignProcs, ImpForeignExportEnums,
        ImpDeclPragmas, ImpDeclMarkers, ImpImplPragmas, ImpImplMarkers,
        ImpPromises, ImpInitialises, ImpFinalises, ImpMutables),

    include_map_to_int_imp_modules(InclMap, IntInclModules, ImpInclModules),
    import_and_or_use_map_to_explicit_int_imp_import_use_maps(ImportUseMap,
        _SectionImportUseMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap),
    IntImportMap = int_import_context_map(IntImportMap0),
    IntUseMap = int_use_context_map(IntUseMap0),
    ImpImportMap = imp_import_context_map(ImpImportMap0),
    ImpUseMap = imp_use_context_map(ImpUseMap0),

    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, ImpModeDefns),

    add_string("% module src\n", S, !U),
    mercury_format_module_decl(S, "module", ModuleName, !U),

    add_string("% include_module_map\n", S, !U),
    map.foldl(format_include_module_map_entry(S), InclMap, !U),
    add_string("% section_import_and_or_use_map\n", S, !U),
    map.foldl(format_import_use_map_entry(S), ImportUseMap, !U),

    mercury_format_section_marker(S, ms_interface, !U),
    set.foldl(mercury_format_module_decl(S, "include_module"),
        IntInclModules, !U),
    list.foldl(mercury_format_module_decl(S, "import_module"),
        map.keys(IntImportMap0), !U),
    list.foldl(mercury_format_module_decl(S, "use_module"),
        map.keys(IntUseMap0), !U),
    list.foldl(mercury_format_fim_spec(S), map.keys(IntFIMSpecMap), !U),
    IntSelfFIMLangStrs = list.map(mercury_foreign_language_to_string,
        set.to_sorted_list(IntSelfFIMLangs)),
    ImpSelfFIMLangStrs = list.map(mercury_foreign_language_to_string,
        set.to_sorted_list(ImpSelfFIMLangs)),
    add_string("% implicit interface FIM self-import languages: ", S, !U),
    add_string(string.join_list(", ", IntSelfFIMLangStrs), S, !U),
    add_string("\n", S, !U),
    add_string("% implicit implementation FIM self-import languages: ", S, !U),
    add_string(string.join_list(", ", ImpSelfFIMLangStrs), S, !U),
    add_string("\n", S, !U),
    list.foldl(mercury_format_item_type_defn(Info, S),
        IntTypeDefns, !U),
    list.foldl(mercury_format_item_inst_defn(Info, S),
        IntInstDefns, !U),
    list.foldl(mercury_format_item_mode_defn(Info, S),
        IntModeDefns, !U),
    list.foldl(mercury_format_item_typeclass(Info, S),
        IntTypeClasses, !U),
    list.foldl(mercury_format_item_instance(Info, S),
        IntInstances, !U),
    list.foldl(
        mercury_format_item_pred_decl_mu_mc(Info, print_name_only, S),
        IntPredDecls, !U),
    list.foldl(mercury_format_item_mode_decl(Info, S),
        IntModeDecls, !U),
    list.foldl(mercury_format_item_decl_pragma(Info, S),
        IntDeclPragmas, !U),
    list.foldl(mercury_format_item_decl_marker(S),
        IntDeclMarkers, !U),
    list.foldl(mercury_format_item_promise(Info, S),
        IntPromises, !U),

    mercury_format_section_marker(S, ms_implementation, !U),
    set.foldl(mercury_format_module_decl(S, "include_module"),
        ImpInclModules, !U),
    list.foldl(mercury_format_module_decl(S, "import_module"),
        map.keys(ImpImportMap0), !U),
    list.foldl(mercury_format_module_decl(S, "use_module"),
        map.keys(ImpUseMap0), !U),
    list.foldl(mercury_format_fim_spec(S), map.keys(ImpFIMSpecMap), !U),
    list.foldl(mercury_format_item_type_defn(Info, S),
        ImpTypeDefns, !U),
    list.foldl(mercury_format_item_inst_defn(Info, S),
        ImpInstDefns, !U),
    list.foldl(mercury_format_item_mode_defn(Info, S),
        ImpModeDefns, !U),
    list.foldl(mercury_format_item_typeclass(Info, S),
        ImpTypeClasses, !U),
    list.foldl(mercury_format_item_instance(Info, S),
        ImpInstances, !U),
    list.foldl(
        mercury_format_item_pred_decl_mu_mc(Info, print_name_only, S),
        ImpPredDecls, !U),
    list.foldl(mercury_format_item_mode_decl(Info, S),
        ImpModeDecls, !U),
    list.foldl(mercury_format_item_clause(Info, S),
        ImpClauses, !U),
    list.foldl(mercury_format_item_foreign_proc(S, get_output_lang(Info)),
        ImpForeignProcs, !U),
    list.foldl(mercury_format_item_foreign_enum(Info, S),
        ImpForeignEnums, !U),
    list.foldl(mercury_format_item_foreign_export_enum(Info, S),
        ImpForeignExportEnums, !U),
    list.foldl(mercury_format_item_decl_pragma(Info, S),
        ImpDeclPragmas, !U),
    list.foldl(mercury_format_item_decl_marker(S),
        ImpDeclMarkers, !U),
    list.foldl(mercury_format_item_impl_pragma(Info, S),
        ImpImplPragmas, !U),
    list.foldl(mercury_format_item_impl_marker(S),
        ImpImplMarkers, !U),
    list.foldl(mercury_format_item_promise(Info, S),
        ImpPromises, !U),
    list.foldl(mercury_format_item_initialise(Info, S),
        ImpInitialises, !U),
    list.foldl(mercury_format_item_finalise(Info, S),
        ImpFinalises, !U),
    list.foldl(mercury_format_item_mutable(Info, S),
        ImpMutables, !U),
    mercury_format_module_decl(S, "end_module", ModuleName, !U),
    add_string("\n", S, !U).

:- pred format_include_module_map_entry(S::in,
    module_name::in, include_module_info::in, U::di, U::uo) is det
    <= pt_output(S, U).

format_include_module_map_entry(S, ModuleName, InclInfo, !U) :-
    InclInfo = include_module_info(Section, _Context),
    (
        Section = ms_interface,
        MarkerStr = "interface"
    ;
        Section = ms_implementation,
        MarkerStr = "implementation"
    ),
    add_string("% ", S, !U),
    mercury_format_bracketed_sym_name(ModuleName, S, !U),
    add_string(" -> ", S, !U),
    add_string(MarkerStr, S, !U),
    add_string("\n", S, !U).

:- pred format_import_use_map_entry(S::in, module_name::in,
    maybe_implicit_import_and_or_use::in, U::di, U::uo) is det
    <= pt_output(S, U).

format_import_use_map_entry(S, ModuleName, ImportAndOrUse, !U) :-
    (
        ImportAndOrUse = explicit_avail(SectionImportAndOrUse),
        (
            SectionImportAndOrUse = int_import(_),
            KindStr = "int_import"
        ;
            SectionImportAndOrUse = int_use(_),
            KindStr = "int_use"
        ;
            SectionImportAndOrUse = imp_import(_),
            KindStr = "imp_import"
        ;
            SectionImportAndOrUse = imp_use(_),
            KindStr = "imp_use"
        ;
            SectionImportAndOrUse = int_use_imp_import(_, _),
            KindStr = "int_use_imp_import"
        )
    ;
        ImportAndOrUse = implicit_avail(ImplicitImportAndOrUse, _),
        (
            ImplicitImportAndOrUse = implicit_int_import,
            KindStr = "implicit_int_import"
        ;
            ImplicitImportAndOrUse = implicit_int_use,
            KindStr = "implicit_int_use"
        ;
            ImplicitImportAndOrUse = implicit_imp_use,
            KindStr = "implicit_imp_use"
        )
    ),
    add_string("% ", S, !U),
    mercury_format_bracketed_sym_name(ModuleName, S, !U),
    add_string(" -> ", S, !U),
    add_string(KindStr, S, !U),
    add_string("\n", S, !U).

%---------------------------------------------------------------------------%

mercury_output_ancestor_int_spec(Info, Stream, AncestorIntSpec, !IO) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, _),
    mercury_output_parse_tree_int0(Info, Stream, ParseTreeInt0, !IO).

mercury_output_direct_int1_spec(Info, Stream, DirectInt1Spec, !IO) :-
    DirectInt1Spec = direct_int1(ParseTreeInt1, _),
    mercury_output_parse_tree_int1(Info, Stream, ParseTreeInt1, !IO).

mercury_output_direct_int3_spec(Info, Stream, DirectInt3Spec, !IO) :-
    DirectInt3Spec = direct_int3(ParseTreeInt3, _),
    mercury_output_parse_tree_int3(Info, Stream, ParseTreeInt3, !IO).

mercury_output_indirect_int2_spec(Info, Stream, IndirectInt2Spec, !IO) :-
    IndirectInt2Spec = indirect_int2(ParseTreeInt2, _),
    mercury_output_parse_tree_int2(Info, Stream, ParseTreeInt2, !IO).

mercury_output_indirect_int3_spec(Info, Stream, IndirectInt3Spec, !IO) :-
    IndirectInt3Spec = indirect_int3(ParseTreeInt3, _),
    mercury_output_parse_tree_int3(Info, Stream, ParseTreeInt3, !IO).

mercury_output_int_for_opt_spec(Info, Stream, ForOptIntSpec, !IO) :-
    (
        ForOptIntSpec = for_opt_int0(ParseTreeInt0, _),
        mercury_output_parse_tree_int0(Info, Stream, ParseTreeInt0, !IO)
    ;
        ForOptIntSpec = for_opt_int1(ParseTreeInt1, _),
        mercury_output_parse_tree_int1(Info, Stream, ParseTreeInt1, !IO)
    ;
        ForOptIntSpec = for_opt_int2(ParseTreeInt2, _),
        mercury_output_parse_tree_int2(Info, Stream, ParseTreeInt2, !IO)
    ).

mercury_output_type_repn_spec(Info, Stream, TypeRepnSpec, !IO) :-
    TypeRepnSpec = type_repn_spec_int1(ParseTreeInt1),
    mercury_output_parse_tree_int1(Info, Stream, ParseTreeInt1, !IO).

%---------------------------------------------------------------------------%

mercury_output_parse_tree_int0(Info, Stream, ParseTreeInt0, !IO) :-
    mercury_format_parse_tree_int0(Info, Stream, ParseTreeInt0, !IO).

mercury_output_parse_tree_int1(Info, Stream, ParseTreeInt1, !IO) :-
    mercury_format_parse_tree_int1(Info, Stream, ParseTreeInt1, !IO).

mercury_output_parse_tree_int2(Info, Stream, ParseTreeInt2, !IO) :-
    mercury_format_parse_tree_int2(Info, Stream, ParseTreeInt2, !IO).

mercury_output_parse_tree_int3(Info, Stream, ParseTreeInt3, !IO) :-
    mercury_format_parse_tree_int3(Info, Stream, ParseTreeInt3, !IO).

%---------------------------------------------------------------------------%

parse_tree_int0_to_string(Info, ParseTreeInt0) = Str :-
    State0 = string.builder.init,
    mercury_format_parse_tree_int0(Info, string.builder.handle, ParseTreeInt0,
        State0, State),
    Str = string.builder.to_string(State).

parse_tree_int1_to_string(Info, ParseTreeInt1) = Str :-
    State0 = string.builder.init,
    mercury_format_parse_tree_int1(Info, string.builder.handle, ParseTreeInt1,
        State0, State),
    Str = string.builder.to_string(State).

parse_tree_int2_to_string(Info, ParseTreeInt2) = Str :-
    State0 = string.builder.init,
    mercury_format_parse_tree_int2(Info, string.builder.handle, ParseTreeInt2,
        State0, State),
    Str = string.builder.to_string(State).

parse_tree_int3_to_string(Info, ParseTreeInt3) = Str :-
    State0 = string.builder.init,
    mercury_format_parse_tree_int3(Info, string.builder.handle, ParseTreeInt3,
        State0, State),
    Str = string.builder.to_string(State).

%---------------------------------------------------------------------------%

mercury_format_parse_tree_int0(Info, S, ParseTreeInt0, !U) :-
    ParseTreeInt0 = parse_tree_int0(ModuleName, _ModuleContext,
        MaybeVersionNumbers, InclMap,
        ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpDeclPragmas, ImpDeclMarkers, ImpPromises),
    include_map_to_int_imp_modules(InclMap, IntIncls, ImpIncls),
    map.foldl4(get_imports_uses, ImportUseMap,
        set.init, IntImports, set.init, ImpImports,
        set.init, IntUses, set.init, ImpUses),
    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, ImpModeDefns),

    mercury_format_module_decl(S, "module", ModuleName, !U),
    mercury_format_maybe_module_version_numbers(S, ModuleName,
        MaybeVersionNumbers, !U),

    mercury_format_section_marker(S, ms_interface, !U),
    set.foldl(mercury_format_module_decl(S, "include_module"),
        IntIncls, !U),
    set.foldl(mercury_format_module_decl(S, "import_module"),
        IntImports, !U),
    set.foldl(mercury_format_module_decl(S, "use_module"),
        IntUses, !U),
    set.foldl(mercury_format_fim_spec(S), IntFIMSpecs, !U),
    list.foldl(mercury_format_item_type_defn(Info, S),
        IntTypeDefns, !U),
    list.foldl(mercury_format_item_inst_defn(Info, S),
        IntInstDefns, !U),
    list.foldl(mercury_format_item_mode_defn(Info, S),
        IntModeDefns, !U),
    list.foldl(mercury_format_item_typeclass(Info, S),
        list.sort(IntTypeClasses), !U),
    list.foldl(mercury_format_item_abstract_instance(Info, S),
        list.sort(IntInstances), !U),
    order_pred_and_mode_decls(IntPredDecls, IntModeDecls, IntPredOrModeDecls),
    mercury_format_pred_or_mode_decls(Info, print_name_only, S,
        IntPredOrModeDecls, !U),
    list.foldl(mercury_format_item_decl_pragma(Info, S),
        list.sort(IntDeclPragmas), !U),
    list.foldl(mercury_format_item_decl_marker(S),
        list.sort(IntDeclMarkers), !U),
    list.foldl(mercury_format_item_promise(Info, S),
        list.sort(IntPromises), !U),

    ( if
        set.is_empty(ImpIncls),
        set.is_empty(ImpImports),
        set.is_empty(ImpUses),
        set.is_empty(ImpFIMSpecs),
        ImpTypeDefns = [],
        ImpInstDefns = [],
        ImpModeDefns = [],
        ImpTypeClasses = [],
        ImpInstances = [],
        ImpPredDecls = [],
        ImpModeDecls = [],
        ImpForeignEnums = [],
        ImpDeclPragmas = [],
        ImpDeclMarkers = [],
        ImpPromises = []
    then
        true
    else
        mercury_format_section_marker(S, ms_implementation, !U),
        set.foldl(mercury_format_module_decl(S, "include_module"),
            ImpIncls, !U),
        set.foldl(mercury_format_module_decl(S, "import_module"),
            ImpImports, !U),
        set.foldl(mercury_format_module_decl(S, "use_module"),
            ImpUses, !U),
        set.foldl(mercury_format_fim_spec(S), ImpFIMSpecs, !U),
        list.foldl(mercury_format_item_type_defn(Info, S),
            ImpTypeDefns, !U),
        list.foldl(mercury_format_item_inst_defn(Info, S),
            ImpInstDefns, !U),
        list.foldl(mercury_format_item_mode_defn(Info, S),
            ImpModeDefns, !U),
        list.foldl(mercury_format_item_typeclass(Info, S),
            list.sort(ImpTypeClasses), !U),
        list.foldl(mercury_format_item_abstract_instance(Info, S),
            list.sort(ImpInstances), !U),
        order_pred_and_mode_decls(ImpPredDecls, ImpModeDecls,
            ImpPredOrModeDecls),
        mercury_format_pred_or_mode_decls(Info, print_name_only, S,
            ImpPredOrModeDecls, !U),
        list.foldl(mercury_format_item_foreign_enum(Info, S),
            ImpForeignEnums, !U),
        list.foldl(mercury_format_item_decl_pragma(Info, S),
            list.sort(ImpDeclPragmas), !U),
        list.foldl(mercury_format_item_decl_marker(S),
            list.sort(ImpDeclMarkers), !U),
        list.foldl(mercury_format_item_promise(Info, S),
            list.sort(ImpPromises), !U)
    ).

mercury_format_parse_tree_int1(Info, S, ParseTreeInt1, !U) :-
    ParseTreeInt1 = parse_tree_int1(ModuleName, _ModuleContext,
        MaybeVersionNumbers, InclMap, UseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntDeclPragmas, IntDeclMarkers, IntPromises, IntTypeRepnMap,
        ImpTypeClasses),
    include_map_to_int_imp_modules(InclMap, IntIncls, ImpIncls),
    map.foldl2(get_uses, UseMap, set.init, IntUses, set.init, ImpUses),
    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, _ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, _ImpModeDefns),

    mercury_format_module_decl(S, "module", ModuleName, !U),
    mercury_format_maybe_module_version_numbers(S, ModuleName,
        MaybeVersionNumbers, !U),
    mercury_format_section_marker(S, ms_interface, !U),
    set.foldl(mercury_format_module_decl(S, "include_module"),
        IntIncls, !U),
    set.foldl(mercury_format_module_decl(S, "use_module"),
        IntUses, !U),
    set.foldl(mercury_format_fim_spec(S), IntFIMSpecs, !U),
    list.foldl(mercury_format_item_type_defn(Info, S),
        IntTypeDefns, !U),
    list.foldl(mercury_format_item_inst_defn(Info, S),
        IntInstDefns, !U),
    list.foldl(mercury_format_item_mode_defn(Info, S),
        IntModeDefns, !U),
    list.foldl(mercury_format_item_typeclass(Info, S),
        list.sort(IntTypeClasses), !U),
    list.foldl(mercury_format_item_abstract_instance(Info, S),
        list.sort(IntInstances), !U),
    order_pred_and_mode_decls(IntPredDecls, IntModeDecls, IntPredOrModeDecls),
    mercury_format_pred_or_mode_decls(Info, print_name_only, S,
        IntPredOrModeDecls, !U),
    list.foldl(mercury_format_item_decl_pragma(Info, S),
        list.sort(IntDeclPragmas), !U),
    list.foldl(mercury_format_item_decl_marker(S),
        list.sort(IntDeclMarkers), !U),
    list.foldl(mercury_format_item_promise(Info, S),
        list.sort(IntPromises), !U),
    map.foldl_values(mercury_format_item_type_repn(Info, S),
        IntTypeRepnMap, !U),

    ( if
        set.is_empty(ImpIncls),
        set.is_empty(ImpUses),
        set.is_empty(ImpFIMSpecs),
        ImpTypeDefns = [],
        ImpForeignEnums = [],
        ImpTypeClasses = []
    then
        true
    else
        mercury_format_section_marker(S, ms_implementation, !U),
        set.foldl(mercury_format_module_decl(S, "include_module"),
            ImpIncls, !U),
        set.foldl(mercury_format_module_decl(S, "use_module"),
            ImpUses, !U),
        set.foldl(mercury_format_fim_spec(S), ImpFIMSpecs, !U),
        list.foldl(mercury_format_item_type_defn(Info, S),
            ImpTypeDefns, !U),
        list.foldl(mercury_format_item_foreign_enum(Info, S),
            ImpForeignEnums, !U),
        list.foldl(mercury_format_item_abstract_typeclass(Info, S),
            list.sort(ImpTypeClasses), !U)
    ).

mercury_format_parse_tree_int2(Info, S, ParseTreeInt2, !U) :-
    ParseTreeInt2 = parse_tree_int2(ModuleName, _ModuleContext,
        MaybeVersionNumbers, IntInclMap, UseMap, IntFIMSpecs, ImpFIMSpecs,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),
    InclMap = coerce(IntInclMap),
    include_map_to_int_imp_modules(InclMap, IntIncls, _ImpIncls),
    map.foldl2(get_uses, UseMap, set.init, IntUses, set.init, ImpUses),
    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, ImpTypeDefns, _ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, _ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, _ImpModeDefns),

    mercury_format_module_decl(S, "module", ModuleName, !U),
    mercury_format_maybe_module_version_numbers(S, ModuleName,
        MaybeVersionNumbers, !U),
    mercury_format_section_marker(S, ms_interface, !U),
    set.foldl(mercury_format_module_decl(S, "include_module"),
        IntIncls, !U),
    set.foldl(mercury_format_module_decl(S, "use_module"),
        IntUses, !U),
    set.foldl(mercury_format_fim_spec(S), IntFIMSpecs, !U),
    list.foldl(mercury_format_item_type_defn(Info, S),
        IntTypeDefns, !U),
    list.foldl(mercury_format_item_inst_defn(Info, S),
        IntInstDefns, !U),
    list.foldl(mercury_format_item_mode_defn(Info, S),
        IntModeDefns, !U),
    list.foldl(mercury_format_item_typeclass(Info, S),
        list.sort(IntTypeClasses), !U),
    list.foldl(mercury_format_item_abstract_instance(Info, S),
        list.sort(IntInstances), !U),
    map.foldl_values(mercury_format_item_type_repn(Info, S),
        IntTypeRepnMap, !U),

    % XXX Currently, ImpUses will always be empty, but the fix for
    % Mantis bug #563 will require allowing ImpUses to be nonempty.
    ( if
        set.is_empty(ImpFIMSpecs),
        set.is_empty(ImpUses),
        ImpTypeDefns = []
    then
        true
    else
        mercury_format_section_marker(S, ms_implementation, !U),
        set.foldl(mercury_format_module_decl(S, "use_module"),
            ImpUses, !U),
        set.foldl(mercury_format_fim_spec(S), ImpFIMSpecs, !U),
        list.foldl(mercury_format_item_type_defn(Info, S),
            ImpTypeDefns, !U)
    ).

mercury_format_parse_tree_int3(Info, S, ParseTreeInt3, !U) :-
    ParseTreeInt3 = parse_tree_int3(ModuleName, _ModuleContext,
        IntInclMap, IntImportMap,
        TypeCtorCheckedMap, InstCtorCheckedMap, ModeCtorCheckedMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),
    InclMap = coerce(IntInclMap),
    include_map_to_int_imp_modules(InclMap, IntInclModules, ImpInclModules),
    expect(set.is_empty(ImpInclModules), $pred, "ImpInclModules not empty"),
    type_ctor_checked_map_get_src_defns(TypeCtorCheckedMap,
        IntTypeDefns, _ImpTypeDefns, _ImpForeignEnums),
    inst_ctor_checked_map_get_src_defns(InstCtorCheckedMap,
        IntInstDefns, _ImpInstDefns),
    mode_ctor_checked_map_get_src_defns(ModeCtorCheckedMap,
        IntModeDefns, _ImpModeDefns),
    mercury_format_module_decl(S, "module", ModuleName, !U),
    mercury_format_section_marker(S, ms_interface, !U),
    set.foldl(mercury_format_module_decl(S, "include_module"),
        IntInclModules, !U),
    list.foldl(mercury_format_module_decl(S, "import_module"),
        map.sorted_keys(IntImportMap), !U),
    list.foldl(mercury_format_item_type_defn(Info, S), IntTypeDefns, !U),
    list.foldl(mercury_format_item_inst_defn(Info, S), IntInstDefns, !U),
    list.foldl(mercury_format_item_mode_defn(Info, S), IntModeDefns, !U),
    list.foldl(mercury_format_item_abstract_typeclass(Info, S),
        list.sort(IntTypeClasses), !U),
    list.foldl(mercury_format_item_abstract_instance(Info, S),
        list.sort(IntInstances), !U),
    map.foldl_values(mercury_format_item_type_repn(Info, S),
        IntTypeRepnMap, !U).

%---------------------------------------------------------------------------%

mercury_output_parse_tree_plain_opt(Info, Stream, ParseTree, !IO) :-
    mercury_format_parse_tree_plain_opt(Info, Stream, ParseTree, !IO).

mercury_output_parse_tree_trans_opt(Info, Stream, ParseTree, !IO) :-
    mercury_format_parse_tree_trans_opt(Info, Stream, ParseTree, !IO).

%---------------------------------------------------------------------------%

mercury_format_parse_tree_plain_opt(Info, S, ParseTree, !U) :-
    ParseTree = parse_tree_plain_opt(ModuleName, _Context,
        UseMap, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        DeclMarkers, ImplMarkers, TypeSpecs, UnusedArgs, Terms, Term2s,
        Exceptions, Trailings, MMTablings, Sharings, Reuses),
    Lang = get_output_lang(Info),
    add_string("% .opt file\n", S, !U),
    mercury_format_module_decl(S, "module", ModuleName, !U),
    list.foldl(mercury_format_module_decl(S, "use_module"),
        map.keys(UseMap), !U),
    set.foldl(mercury_format_fim_spec(S), FIMSpecs, !U),
    list.foldl(mercury_format_item_type_defn(Info, S), TypeDefns, !U),
    list.foldl(mercury_format_item_foreign_enum(Info, S),
        ForeignEnums, !U),
    list.foldl(mercury_format_item_inst_defn(Info, S), InstDefns, !U),
    list.foldl(mercury_format_item_mode_defn(Info, S), ModeDefns, !U),
    list.foldl(mercury_format_item_typeclass(Info, S), TypeClasses, !U),
    list.foldl(mercury_format_item_instance(Info, S), Instances, !U),
    % NOTE: The names of type variables in type_spec pragmas must match
    % *exactly* the names of the corresponding type variables in the
    % predicate declaration to which they apply. This is why one variable,
    % VarNamePrint, controls both.
    %
    % If a predicate is defined by a foreign_proc, then its declaration
    % *must* be printed with print_name_only, because that is the only way
    % that any reference to the type_info variable in the foreign code
    % in the body of the foreign_proc will match the declared name of the
    % type variable that it is for.
    %
    % We used to print the predicate declarations with print_name_only
    % for such predicates (predicates defined by foreign_procs) and with
    % print_name_and_num for all other predicates. (That included predicates
    % representing promises.) However, the predicates whose declarations
    % we are writing out have not been through any transformation that
    % would have either (a) changed the names of any existing type variables,
    % or (b) introduced any new type variables, so the mapping between
    % type variable numbers and names should be the same now as when the
    % the predicate declaration was first parsed. And at that time, two
    % type variable occurrences with the same name obviously referred to the
    % same type variable, so the numeric suffix added by print_name_and_num
    % was obviously not needed.
    VarNamePrintPredDecl = print_name_only,
    list.foldl(
        mercury_format_item_pred_decl(Lang, VarNamePrintPredDecl, S),
        PredDecls, !U),
    list.foldl(mercury_format_item_mode_decl(Info, S), ModeDecls, !U),
    list.foldl(mercury_format_item_decl_marker(S),
        coerce(DeclMarkers), !U),
    list.foldl(mercury_format_item_impl_marker(S),
        coerce(ImplMarkers), !U),
    list.foldl(mercury_format_pragma_type_spec(S, Lang), TypeSpecs, !U),
    list.foldl(mercury_format_item_clause(Info, S), Clauses, !U),
    list.foldl(mercury_format_item_foreign_proc(S, Lang), ForeignProcs, !U),
    list.foldl(mercury_format_item_promise(Info, S), Promises, !U),

    maybe_format_block_start_blank_line(S, UnusedArgs, !U),
    list.foldl(mercury_format_pragma_unused_args(S), UnusedArgs, !U),
    maybe_format_block_start_blank_line(S, Terms, !U),
    list.foldl(mercury_format_pragma_termination(S, Lang), Terms, !U),
    maybe_format_block_start_blank_line(S, Term2s, !U),
    list.foldl(mercury_format_pragma_termination2(S, Lang), Term2s, !U),
    maybe_format_block_start_blank_line(S, Exceptions, !U),
    list.foldl(mercury_format_pragma_exceptions(S), Exceptions, !U),
    maybe_format_block_start_blank_line(S, Trailings, !U),
    list.foldl(mercury_format_pragma_trailing(S), Trailings, !U),
    maybe_format_block_start_blank_line(S, MMTablings, !U),
    list.foldl(mercury_format_pragma_mm_tabling(S), MMTablings, !U),
    maybe_format_block_start_blank_line(S, Sharings, !U),
    list.foldl(mercury_format_pragma_struct_sharing(S, Lang), Sharings, !U),
    maybe_format_block_start_blank_line(S, Reuses, !U),
    list.foldl(mercury_format_pragma_struct_reuse(S, Lang), Reuses, !U).

mercury_format_parse_tree_trans_opt(Info, S, ParseTree, !U) :-
    ParseTree = parse_tree_trans_opt(ModuleName, _Context,
        Terms, Term2s, Exceptions, Trailings, MMTablings, Sharings, Reuses),
    Lang = get_output_lang(Info),
    add_string("% .trans_opt file\n", S, !U),
    mercury_format_module_decl(S, "module", ModuleName, !U),
    maybe_format_block_start_blank_line(S, Terms, !U),
    list.foldl(mercury_format_pragma_termination(S, Lang), Terms, !U),
    maybe_format_block_start_blank_line(S, Term2s, !U),
    list.foldl(mercury_format_pragma_termination2(S, Lang), Term2s, !U),
    maybe_format_block_start_blank_line(S, Exceptions, !U),
    list.foldl(mercury_format_pragma_exceptions(S), Exceptions, !U),
    maybe_format_block_start_blank_line(S, Trailings, !U),
    list.foldl(mercury_format_pragma_trailing(S), Trailings, !U),
    maybe_format_block_start_blank_line(S, MMTablings, !U),
    list.foldl(mercury_format_pragma_mm_tabling(S), MMTablings, !U),
    maybe_format_block_start_blank_line(S, Sharings, !U),
    list.foldl(mercury_format_pragma_struct_sharing(S, Lang), Sharings, !U),
    maybe_format_block_start_blank_line(S, Reuses, !U),
    list.foldl(mercury_format_pragma_struct_reuse(S, Lang), Reuses, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_maybe_module_version_numbers(S::in, module_name::in,
    maybe_version_numbers::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_maybe_module_version_numbers(S, ModuleName,
        MaybeVersionNumbers, !U) :-
    (
        MaybeVersionNumbers = no_version_numbers
    ;
        MaybeVersionNumbers = version_numbers(VersionNumbers),
        mercury_format_module_version_numbers(S, ModuleName,
            VersionNumbers, !U)
    ).

:- pred mercury_format_module_version_numbers(S::in, module_name::in,
    module_item_version_numbers::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_module_version_numbers(S, ModuleName,
        ModuleItemVersionNumbers, !U) :-
    string.format(":- version_numbers(%d, %s,\n%s).\n",
        [i(module_item_version_numbers_version_number),
        s(mercury_bracketed_sym_name_to_string(ModuleName)),
        s(module_item_version_numbers_to_string(ModuleItemVersionNumbers))],
        Str),
    add_string(Str, S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_module_components(merc_out_info::in,
    S::in, maybe(module_section)::in, list(module_component)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_module_components(_, _, _, [], !U).
mercury_format_module_components(Info, S, MaybePrevSectionKind,
        [Component | Components], !U) :-
    (
        Component = mc_section(_, SectionKind, _SectionContext,
            InclsCord, AvailsCord, FIMsCord, ItemsCord),
        mercury_format_section_marker(S, SectionKind, !U),
        list.foldl(mercury_format_item_include(Info, S),
            cord.list(InclsCord), !U),
        list.foldl(mercury_format_item_avail(Info, S),
            cord.list(AvailsCord), !U),
        list.foldl(mercury_format_item_foreign_import_module(S),
            cord.list(FIMsCord), !U),
        mercury_format_items(Info, S, cord.list(ItemsCord), !U),
        MaybeCurSectionKind = yes(SectionKind)
    ;
        Component = mc_nested_submodule(_, SectionKind, _, SubParseTree),
        Lang = get_output_lang(Info),
        (
            Lang = output_mercury,
            ( if
                MaybePrevSectionKind = yes(PrevSectionKind),
                PrevSectionKind = SectionKind
            then
                true
            else
                mercury_format_section_marker(S, SectionKind, !U)
            )
        ;
            Lang = output_debug,
            mercury_format_section_marker(S, SectionKind, !U),
            (
                SectionKind = ms_interface,
                add_string("% nested submodule in interface\n", S, !U)
            ;
                SectionKind = ms_implementation,
                add_string("% nested submodule in implementation\n", S, !U)
            )
        ),
        mercury_format_parse_tree_src(Info, S, SubParseTree, !U),
        MaybeCurSectionKind = MaybePrevSectionKind
    ),
    mercury_format_module_components(Info, S, MaybeCurSectionKind,
        Components, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_section_marker(S::in, module_section::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_section_marker(S, Section, !U) :-
    (
        Section = ms_interface,
        add_string(":- interface.\n", S, !U)
    ;
        Section = ms_implementation,
        add_string(":- implementation.\n", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_item_include(merc_out_info::in, S::in, item_include::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_include(Info, S, ItemInclude, !U) :-
    ItemInclude = item_include(ModuleName, Context, _SeqNum),
    Decl = "include_module",
    maybe_format_line_number(Info, Context, S, !U),
    mercury_format_module_decl(S, Decl, ModuleName, !U).

:- pred mercury_format_item_avail(merc_out_info::in, S::in, item_avail::in, 
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_avail(Info, S, Avail, !U) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _SeqNum)),
        Decl = "import_module"
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _SeqNum)),
        Decl = "use_module"
    ),
    maybe_format_line_number(Info, Context, S, !U),
    mercury_format_module_decl(S, Decl, ModuleName, !U).

mercury_output_module_decl(Stream, Decl, ModuleName, !IO) :-
    mercury_format_module_decl(Stream, Decl, ModuleName, !IO).

mercury_format_module_decl(S, Decl, ModuleName, !U) :-
    ModuleNameStr = mercury_bracketed_sym_name_to_string(ModuleName),
    string.format(":- %s %s.\n", [s(Decl), s(ModuleNameStr)], DeclStr),
    add_string(DeclStr, S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_items(merc_out_info::in, S::in, list(item)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_items(_, _, [], !U).
mercury_format_items(Info, S, [Item | Items], !U) :-
    mercury_format_item(Info, S, Item, !U),
    mercury_format_items(Info, S, Items, !U).

mercury_format_item(Info, S, Item, !U) :-
    (
        Item = item_type_defn(ItemTypeDefn),
        mercury_format_item_type_defn(Info, S, ItemTypeDefn, !U)
    ;
        Item = item_inst_defn(ItemInstDefn),
        mercury_format_item_inst_defn(Info, S, ItemInstDefn, !U)
    ;
        Item = item_mode_defn(ItemModeDefn),
        mercury_format_item_mode_defn(Info, S, ItemModeDefn, !U)
    ;
        Item = item_pred_decl(ItemPredDecl),
        mercury_format_item_pred_decl_mu_mc(Info, print_name_only, S,
            ItemPredDecl, !U)
    ;
        Item = item_mode_decl(ItemModeDecl),
        mercury_format_item_mode_decl(Info, S, ItemModeDecl, !U)
    ;
        Item = item_clause(ItemClause),
        mercury_format_item_clause(Info, S, ItemClause, !U)
    ;
        Item = item_foreign_proc(ItemForeignProc),
        mercury_format_item_foreign_proc(S, get_output_lang(Info),
            ItemForeignProc, !U)
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        mercury_format_item_foreign_enum(Info, S, ItemForeignEnum, !U)
    ;
        Item = item_foreign_export_enum(ItemForeignExportEnum),
        mercury_format_item_foreign_export_enum(Info, S,
            ItemForeignExportEnum, !U)
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        mercury_format_item_decl_pragma(Info, S, ItemDeclPragma, !U)
    ;
        Item = item_decl_marker(ItemDeclPragma),
        mercury_format_item_decl_marker(S, ItemDeclPragma, !U)
    ;
        Item = item_impl_pragma(ItemImplPragma),
        mercury_format_item_impl_pragma(Info, S, ItemImplPragma, !U)
    ;
        Item = item_impl_marker(ItemImplPragma),
        mercury_format_item_impl_marker(S, ItemImplPragma, !U)
    ;
        Item = item_generated_pragma(ItemGenPragma),
        mercury_format_item_generated_pragma(Info, S, ItemGenPragma, !U)
    ;
        Item = item_promise(ItemPromise),
        mercury_format_item_promise(Info, S, ItemPromise, !U)
    ;
        Item = item_typeclass(ItemTypeClass),
        mercury_format_item_typeclass(Info, S, ItemTypeClass, !U)
    ;
        Item = item_instance(ItemInstance),
        mercury_format_item_instance(Info, S, ItemInstance, !U)
    ;
        Item = item_initialise(ItemInitialise),
        mercury_format_item_initialise(Info, S, ItemInitialise, !U)
    ;
        Item = item_finalise(ItemFinalise),
        mercury_format_item_finalise(Info, S, ItemFinalise, !U)
    ;
        Item = item_mutable(ItemMutable),
        mercury_format_item_mutable(Info, S, ItemMutable, !U)
    ;
        Item = item_type_repn(ItemTypeRepn),
        mercury_format_item_type_repn(Info, S, ItemTypeRepn, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_pred_or_mode_decls(merc_out_info::in,
    var_name_print::in, S::in, list(pred_or_mode_decl_item)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pred_or_mode_decls(_, _, _, [], !U).
mercury_format_pred_or_mode_decls(Info, VarNamePrint, S, [Item | Items], !U) :-
    mercury_format_pred_or_mode_decl(Info, VarNamePrint, S, Item, !U),
    mercury_format_pred_or_mode_decls(Info, VarNamePrint, S, Items, !U).

:- pred mercury_format_pred_or_mode_decl(merc_out_info::in, var_name_print::in,
    S::in, pred_or_mode_decl_item::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_pred_or_mode_decl(Info, VarNamePrint, S, Item, !U) :-
    (
        Item = pomd_pred(ItemPredDecl),
        mercury_format_item_pred_decl_mu_mc(Info, VarNamePrint, S,
            ItemPredDecl, !U)
    ;
        Item = pomd_mode(ItemModeDecl),
        mercury_format_item_mode_decl(Info, S, ItemModeDecl, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_type_defn(Info, S, ItemTypeDefn, !U) :-
    % XXX We should not use the tvar names in TypeVarSet; we should be
    % using standard tvar names such as TV1, TV2 etc. This should allow
    % any automatically generated interface files to remain unchanged
    % when the names of the type variables change in the source code,
    % thus avoiding the cascade of module recompilations that would
    % otherwise result.
    ItemTypeDefn = item_type_defn_info(SymName0, TypeParams, TypeDefn,
        TypeVarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, SymName0, SymName),
    maybe_format_line_number(Info, Context, S, !U),
    Args = list.map((func(V) = term.variable(V, Context)), TypeParams),
    construct_qualified_term_with_context(SymName, Args, Context, TypeTerm),
    (
        TypeDefn = parse_tree_abstract_type(DetailsAbstract),
        (
            ( DetailsAbstract = abstract_type_general
            ; DetailsAbstract = abstract_dummy_type
            ; DetailsAbstract = abstract_notag_type
            ; DetailsAbstract = abstract_type_fits_in_n_bits(_)
            ; DetailsAbstract = abstract_subtype(_)
            ),
            add_string(":- type ", S, !U)
        ;
            DetailsAbstract = abstract_solver_type,
            add_string(":- solver type ", S, !U)
        ),
        mercury_format_term_nq_vs(TypeVarSet, print_name_only,
            next_to_graphic_token, TypeTerm, S, !U),
        (
            DetailsAbstract = abstract_type_fits_in_n_bits(NumBits),
            % XXX TYPE_REPN Instead of adding this information to the
            % generated type definition, generate and write out
            % a separate type_repn item instead.
            mercury_format_where_abstract_enum_type(S, NumBits, !U)
        ;
            ( DetailsAbstract = abstract_dummy_type
            ; DetailsAbstract = abstract_notag_type
            )
            % XXX TYPE_REPN The same concern applies here, but these
            % kinds of abstract types are not yet generated anywhere,
            % so we don't have anything to do for them.
        ;
            DetailsAbstract = abstract_subtype(SuperTypeCtor),
            mercury_format_where_abstract_subtype(S, SuperTypeCtor, !U)
        ;
            ( DetailsAbstract = abstract_type_general
            ; DetailsAbstract = abstract_solver_type
            )
        ),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_eqv_type(DetailsEqv),
        DetailsEqv = type_details_eqv(EqvType),
        add_string(":- type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        add_string(" == ", S, !U),
        mercury_format_type(TypeVarSet, print_name_only, EqvType, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(OoMCtors, MaybeCanonical, MaybeDirectArgs),
        add_string(":- type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        mercury_format_ctors(TypeVarSet, yes, HeadCtor, TailCtors, S, !U),
        mercury_format_where_attributes(Info, TypeVarSet, maybe.no,
            MaybeCanonical, MaybeDirectArgs, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_sub_type(DetailsDu),
        DetailsDu = type_details_sub(SuperType, OoMCtors),
        add_string(":- type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        add_string(" =< ", S, !U),
        mercury_format_type(TypeVarSet, print_name_only, SuperType, S, !U),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        mercury_format_ctors(TypeVarSet, yes, HeadCtor, TailCtors, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(SolverTypeDetails, MaybeCanonical),
        add_string(":- solver type ", S, !U),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        mercury_format_where_attributes(Info, TypeVarSet,
            yes(SolverTypeDetails), MaybeCanonical, maybe.no, S, !U),
        add_string(".\n", S, !U)
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, MaybeCanonical,
            foreign_type_assertions(Assertions)),
        add_string(":- pragma foreign_type(", S, !U),
        (
            ForeignType = c(_),
            add_string("c, ", S, !U)
        ;
            ForeignType = java(_),
            add_string("java, ", S, !U)
        ;
            ForeignType = csharp(_),
            add_string("csharp, ", S, !U)
        ),
        mercury_format_term_vs(TypeVarSet, print_name_only, TypeTerm, S, !U),
        add_string(", \"", S, !U),
        (
            ForeignType = c(c_type(ForeignTypeStr))
        ;
            ForeignType = java(java_type(ForeignTypeStr))
        ;
            ForeignType = csharp(csharp_type(ForeignTypeStr))
        ),
        add_string(ForeignTypeStr, S, !U),
        add_string("\"", S, !U),
        set.to_sorted_list(Assertions, AssertionsList),
        (
            AssertionsList = []
        ;
            AssertionsList = [_ | _],
            AssertionStrs =
                list.map(foreign_type_assertion_to_string, AssertionsList),
            AssertionsStr = string.join_list(", ", AssertionStrs),
            add_string(", [", S, !U),
            add_string(AssertionsStr, S, !U),
            add_string("]", S, !U)
        ),
        add_string(")", S, !U),
        mercury_format_where_attributes(Info, TypeVarSet, no,
            MaybeCanonical, no, S, !U),
        add_string(".\n", S, !U)
    ).

%---------------------%
%
% Predicates needed to output more than one kind of type.
%

mercury_format_where_attributes(Info, TypeVarSet, MaybeSolverTypeDetails,
        MaybeCanonical, MaybeDirectArgs, S, !U) :-
    some [!LineCord]
    (
        !:LineCord = cord.init,
        (
            MaybeCanonical = canon
        ;
            MaybeCanonical = noncanon(NonCanon),
            (
                NonCanon = noncanon_abstract(_),
                cord.snoc("type_is_abstract_noncanonical", !LineCord)
            ;
                NonCanon = noncanon_subtype
            ;
                NonCanon = noncanon_uni_cmp(UniPred, CmpPred),
                UniPredStr = mercury_bracketed_sym_name_to_string(UniPred),
                CmpPredStr = mercury_bracketed_sym_name_to_string(CmpPred),
                UniPredLine = "equality is " ++ UniPredStr,
                CmpPredLine = "comparison is " ++ CmpPredStr,
                cord.snoc(UniPredLine, !LineCord),
                cord.snoc(CmpPredLine, !LineCord)
            ;
                NonCanon = noncanon_uni_only(UniPred),
                UniPredStr = mercury_bracketed_sym_name_to_string(UniPred),
                UniPredLine = "equality is " ++ UniPredStr,
                cord.snoc(UniPredLine, !LineCord)
            ;
                NonCanon = noncanon_cmp_only(CmpPred),
                CmpPredStr = mercury_bracketed_sym_name_to_string(CmpPred),
                CmpPredLine = "comparison is " ++ CmpPredStr,
                cord.snoc(CmpPredLine, !LineCord)
            )
        ),
        (
            MaybeDirectArgs = yes(DirectArgFunctors),
            FunctorStrs =
                list.map(mercury_bracketed_sym_name_arity_to_string,
                    DirectArgFunctors),
            FunctorsStr = string.join_list(", ", FunctorStrs),
            string.format("direct_arg is [%s]", [s(FunctorsStr)],
                DirectArgLine),
            cord.snoc(DirectArgLine, !LineCord)
        ;
            MaybeDirectArgs = no
        ),
        Lines = cord.list(!.LineCord),
        ( if
            MaybeSolverTypeDetails = no,
            Lines = []
        then
            true
        else
            add_string("\n    where\n", S, !U),
            (
                MaybeSolverTypeDetails = yes(SolverTypeDetails),
                mercury_format_solver_type_details(Info, S, TypeVarSet,
                    SolverTypeDetails, !U),
                (
                    Lines = []
                ;
                    Lines = [_ | _],
                    add_string(",\n", S, !U)
                )
            ;
                MaybeSolverTypeDetails = no
            ),
            % We cannot curry string.append, because it has several modes.
            IndentLine =
                ( func(Line) = IndentedLine :-
                    string.append("        ", Line, IndentedLine)
                ),
            IndentedLines = list.map(IndentLine, Lines),
            AllLines = string.join_list(",\n", IndentedLines),
            add_string(AllLines, S, !U)
        )
    ).

:- pred mercury_format_solver_type_details(merc_out_info::in,
    S::in, tvarset::in, solver_type_details::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_solver_type_details(Info, S, TypeVarSet, Details, !U) :-
    Details = solver_type_details(RepresentationType, GroundInst,
        AnyInst, MutableInfos),
    add_string("        representation is ", S, !U),
    mercury_format_type(TypeVarSet, print_name_only, RepresentationType,
        S, !U),
    Lang = get_output_lang(Info),
    varset.init(EmptyInstVarSet),
    add_string(",\n        ground is ", S, !U),
    mercury_format_inst(Lang, EmptyInstVarSet, GroundInst, S, !U),
    add_string(",\n        any is ", S, !U),
    mercury_format_inst(Lang, EmptyInstVarSet, AnyInst, S, !U),
    (
        MutableInfos = []
    ;
        MutableInfos = [_ | _],
        add_string(",\n        constraint_store is [\n            ", S, !U),
        list.gap_foldl(mercury_format_item_mutable(Info, S),
            add_string(",\n            ", S), MutableInfos, !U),
        add_string("\n        ]", S, !U)
    ).

%---------------------%
%
% Predicates needed to output abstract types.
%

:- pred mercury_format_where_abstract_enum_type(S::in, int::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_where_abstract_enum_type(S, NumBits, !U) :-
    add_string("\n\twhere\t", S, !U),
    add_string("type_is_abstract_enum(", S, !U),
    % XXX TYPE_REPN
    % add_string("type_is_representable_in_n_bits(", S, !U),
    add_int(NumBits, S, !U),
    add_string(")", S, !U).

:- pred mercury_format_where_abstract_subtype(S::in, type_ctor::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_where_abstract_subtype(S, TypeCtor, !U) :-
    add_string("\n\twhere\t", S, !U),
    add_string("type_is_abstract_subtype(", S, !U),
    TypeCtor = type_ctor(SymName, Arity),
    mercury_format_sym_name(SymName, S, !U),
    add_string("/", S, !U),
    add_int(Arity, S, !U),
    add_string(")", S, !U).

%---------------------%
%
% Predicates needed to output discriminated union types.
%

:- pred mercury_format_ctors(tvarset::in, bool::in,
    constructor::in, list(constructor)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctors(VarSet, First, HeadCtor, TailCtors, S, !U) :-
    (
        First = yes,
        add_string("\n    --->    ", S, !U)
    ;
        First = no,
        add_string("\n    ;       ", S, !U)
    ),
    mercury_format_ctor(VarSet, HeadCtor, S, !U),
    (
        TailCtors = []
    ;
        TailCtors = [HeadTailCtor | TailTailCtors],
        mercury_format_ctors(VarSet, no, HeadTailCtor, TailTailCtors, S, !U)
    ).

mercury_format_ctor(TVarSet, Ctor, S, !U) :-
    % NOTE The code of this predicate is almost identical to the
    % code of write_ctor and write_ctor_repn in hlds_out_module.m.
    % Any changes made here will probably need to be made there as well.
    Ctor = ctor(_Ordinal, MaybeExistConstraints, SymName, Args, Arity, _Ctxt),

    % The module name in SymName must be the same as the module qualifier
    % of the type_ctor, so there is no point in printing it.
    Name = unqualify_name(SymName),
    maybe_cons_exist_constraints_to_prefix_suffix(TVarSet, "", "",
        MaybeExistConstraints, ExistConstraintsPrefix, ExistConstraintsSuffix),
    maybe_brace_for_name_prefix_suffix(Arity, Name, BracePrefix, BraceSuffix),
    add_string(ExistConstraintsPrefix, S, !U),
    add_string(BracePrefix, S, !U),
    (
        Args = [],
        mercury_format_bracketed_sym_name(unqualified(Name), S, !U),
        % This space prevents a terminating full stop from being confused
        % as part of the sym_name if the sym_name contains graphical
        % characters.
        add_string(" ", S, !U)
    ;
        Args = [HeadArg | TailArgs],
        mercury_format_sym_name(unqualified(Name), S, !U),
        add_string("(", S, !U),
        mercury_format_ctor_args(S, TVarSet, HeadArg, TailArgs, !U),
        add_string(")", S, !U)
    ),
    add_string(BraceSuffix, S, !U),
    add_string(ExistConstraintsSuffix, S, !U).

maybe_cons_exist_constraints_to_prefix_suffix(TVarSet, SuffixStart, SuffixEnd,
        MaybeExistConstraints, Prefix, Suffix) :-
    (
        MaybeExistConstraints = no_exist_constraints,
        Prefix = "",
        Suffix = ""
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            _UnconstrainedQVars, _ConstrainedQVars),
        ExistQVarsStr = mercury_quantifier_to_string(TVarSet,
            print_name_only, ExistQVars),
        ConstraintsStr = mercury_prog_constraint_list_to_string(TVarSet,
            print_name_only, "=>", Constraints),
        Prefix = ExistQVarsStr ++ "(",
        Suffix = SuffixStart ++ ConstraintsStr ++ ")" ++ SuffixEnd
    ).

maybe_brace_for_name_prefix_suffix(Arity, Name, Prefix, Suffix) :-
    % We need to quote ';'/2, '{}'/2, '=>'/2, and 'some'/2.
    % XXX I (zs) think that we should not allow these as constructor names.
    ( if
        Arity = 2,
        ( Name = ";"
        ; Name = "{}"
        ; Name = "some"
        ; Name = "=>"
        )
    then
        Prefix = "{ ",
        Suffix = " }"
    else
        Prefix = "",
        Suffix = ""
    ).

:- pred mercury_format_ctor_args(S::in, tvarset::in,
    constructor_arg::in, list(constructor_arg)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctor_args(S, TVarSet, HeadArg, TailArgs, !U) :-
    mercury_format_ctor_arg(S, TVarSet, HeadArg, !U),
    (
        TailArgs = []
    ;
        TailArgs = [HeadTailArg | TailTailArgs],
        add_string(", ", S, !U),
        mercury_format_ctor_args(S, TVarSet, HeadTailArg, TailTailArgs, !U)
    ).

:- pred mercury_format_ctor_arg(S::in, tvarset::in, constructor_arg::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctor_arg(S, TVarSet, Arg, !U) :-
    Arg = ctor_arg(Name, Type, _Context),
    mercury_format_ctor_arg_name_prefix(S, Name, !U),
    mercury_format_type(TVarSet, print_name_only, Type, S, !U).

:- pred mercury_format_ctor_arg_name_prefix(S::in, maybe(ctor_field_name)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_ctor_arg_name_prefix(_S, no, !U).
mercury_format_ctor_arg_name_prefix(S, yes(FieldName), !U) :-
    FieldName = ctor_field_name(Name, _Ctxt),
    mercury_format_bracketed_sym_name(Name, S, !U),
    add_string(" :: ", S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_inst_defn(Info, S, ItemInstDefn, !U) :-
    ItemInstDefn = item_inst_defn_info(SymName0, InstParams, MaybeForTypeCtor,
        MaybeAbstractInstDefn, InstVarSet, Context, _SeqNum),
    % If the unqualified name is a builtin inst, then output the qualified
    % name. This prevents the compiler giving an error about redefining
    % builtin insts when an interface file is read back in.
    maybe_unqualify_sym_name(Info, SymName0, UnQualSymName),
    ( if is_builtin_inst_name(InstVarSet, UnQualSymName, InstParams) then
        SymName = SymName0
    else
        SymName = UnQualSymName
    ),
    maybe_format_line_number(Info, Context, S, !U),
    Lang = get_output_lang(Info),
    ArgTerms = list.map(func(V) = variable(V, Context), InstParams),
    construct_qualified_term_with_context(SymName, ArgTerms, Context,
        InstTerm),
    (
        MaybeAbstractInstDefn = abstract_inst_defn,
        add_string(":- abstract_inst((", S, !U),
        mercury_format_term_vs(InstVarSet, print_name_only, InstTerm,
            S, !U),
        add_string(")).\n", S, !U)
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(eqv_inst(Inst)),
        ( if
            % Is it safe to print the inst name without parentheses around it?
            sym_name_is_simple(SymName),
            not (
                SymName = unqualified(Name),
                mercury_op(Name)
            )
        then
            % Yes it is, so print the inst and its parameters without
            % extra parentheses around them.
            add_string(":- inst ", S, !U),
            add_string(sym_name_to_string(SymName), S, !U),
            (
                ArgTerms = []
            ;
                ArgTerms = [HeadArgTerm | TailArgTerms],
                add_string("(", S, !U),
                mercury_format_comma_separated_terms_vs(InstVarSet,
                    print_name_only, HeadArgTerm, TailArgTerms, S, !U),
                add_string(")", S, !U)
            )
        else
            % No it isn't, so print the extra parentheses.
            add_string(":- inst (", S, !U),
            mercury_format_term_vs(InstVarSet, print_name_only, InstTerm,
                S, !U),
            add_string(")", S, !U)
        ),
        (
            MaybeForTypeCtor = no
        ;
            MaybeForTypeCtor = yes(ForTypeCtor),
            ForTypeCtor = type_ctor(ForTypeCtorSymName, ForTypeCtorArity),
            add_string(" for ", S, !U),
            mercury_format_sym_name(ForTypeCtorSymName, S, !U),
            add_string("/", S, !U),
            add_int(ForTypeCtorArity, S, !U)
        ),
        ( if
            % Can we print the inst using the syntax that resembles
            % type definitions?
            Inst = bound(Uniq, _, BoundInsts),
            Uniq = shared,
            bound_inst_cons_ids_are_all_simple(BoundInsts, SimpleBIs),
            SimpleBIs = [HeadSimpleBI | TailSimpleBIs]
        then
            % Yes, so use that syntax, which is more readable, partly
            % because it has less clutter, and partly because it can be
            % formatted to have meaningful indentation.
            add_string("\n", S, !U),
            format_bound_inst_being_defined(S, Lang, InstVarSet,
                "    --->    ", HeadSimpleBI, TailSimpleBIs, !U)
        else
            % No, so fall back to the less readable but more general syntax.
            add_string(" == ", S, !U),
            mercury_format_inst(Lang, InstVarSet, Inst, S, !U),
            add_string(".\n", S, !U)
        )
    ).

    % Succeed if the sym_name describes a builtin inst.
    %
:- pred is_builtin_inst_name(inst_varset::in, sym_name::in, list(inst_var)::in)
    is semidet.

is_builtin_inst_name(InstVarSet, unqualified(Name), Args0) :-
    Args1 = list.map(func(V) = variable(coerce_var(V), dummy_context), Args0),
    Term = term.functor(term.atom(Name), Args1, dummy_context),
    varset.coerce(InstVarSet, VarSet),
    ContextPieces = cord.init,  % Dummy; not used.
    parse_inst(no_allow_constrained_inst_var(wnciv_inst_defn_lhs), VarSet,
        ContextPieces, Term, MaybeInst),
    MaybeInst = ok1(Inst),
    Inst \= defined_inst(user_inst(_, _)).

:- type simple_bound_inst
    --->    simple_bound_functor(string, list(mer_inst)).

:- pred bound_inst_cons_ids_are_all_simple(list(bound_inst)::in,
    list(simple_bound_inst)::out) is semidet.

bound_inst_cons_ids_are_all_simple([], []).
bound_inst_cons_ids_are_all_simple([HeadBI | TailBIs],
        [HeadSimpleBI | TailSimpleBIs])  :-
    HeadBI = bound_functor(ConsId, ArgInsts),
    ConsId = cons(SymName, _, _),
    sym_name_is_simple(SymName),
    SimpleName = sym_name_to_string(SymName),
    HeadSimpleBI = simple_bound_functor(SimpleName, ArgInsts),
    bound_inst_cons_ids_are_all_simple(TailBIs, TailSimpleBIs).

:- pred sym_name_is_simple(sym_name::in) is semidet.

sym_name_is_simple(SymName) :-
    Names = sym_name_to_list(SymName),
    all_true(name_is_simple, Names).

:- pred name_is_simple(string::in) is semidet.

name_is_simple(Name) :-
    string.to_char_list(Name, Chars),
    Chars = [HeadChar | TailChars],
    char.is_lower(HeadChar),
    all_true(char.is_alnum_or_underscore, TailChars).

:- pred format_bound_inst_being_defined(S::in,
    output_lang::in, inst_varset::in, string::in,
    simple_bound_inst::in, list(simple_bound_inst)::in,
    U::di, U::uo) is det <= pt_output(S, U).

format_bound_inst_being_defined(S, Lang, InstVarSet, ArrowOrSemi,
        HeadBI, TailBIs, !U) :-
    HeadBI = simple_bound_functor(Name, ArgInsts),
    string.format("%s%s", [s(ArrowOrSemi), s(Name)], InitStr),
    add_string(InitStr, S, !U),
    (
        ArgInsts = []
    ;
        ArgInsts = [_ | _],
        add_string("(", S, !U),
        mercury_format_inst_list(Lang, InstVarSet, ArgInsts, S, !U),
        add_string(")", S, !U)
    ),
    (
        TailBIs = [],
        add_string(".\n", S, !U)
    ;
        TailBIs = [HeadTailBI | TailTailBIs],
        add_string("\n", S, !U),
        format_bound_inst_being_defined(S, Lang, InstVarSet,
            "    ;       ", HeadTailBI, TailTailBIs, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_mode_defn(Info, S, ItemModeDefn, !U) :-
    ItemModeDefn = item_mode_defn_info(SymName, InstParams,
        MaybeAbstractModeDefn, VarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, SymName, UnQualSymName),
    maybe_format_line_number(Info, Context, S, !U),
    Lang = get_output_lang(Info),
    mercury_format_mode_defn(Lang, VarSet, Context, UnQualSymName, InstParams,
        MaybeAbstractModeDefn, S, !U).

    % This is defined to work on !U instead of !IO so that we can call
    % mercury_format_mode with simple_inst_info.
    %
:- pred mercury_format_mode_defn(output_lang::in, inst_varset::in,
    prog_context::in, sym_name::in, list(inst_var)::in,
    maybe_abstract_mode_defn::in, S::in, U::di, U::uo) is det
    <= pt_output(S, U).

mercury_format_mode_defn(Lang, InstVarSet, Context, Name, Args,
        MaybeAbstractModeDefn, S, !U) :-
    (
        MaybeAbstractModeDefn = abstract_mode_defn,
        add_string(":- abstract_mode((", S, !U),
        mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, S, !U),
        add_string(")).\n", S, !U)
    ;
        MaybeAbstractModeDefn = nonabstract_mode_defn(eqv_mode(Mode)),
        add_string(":- mode (", S, !U),
        mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, S, !U),
        add_string(") == ", S, !U),
        mercury_format_mode(Lang, InstVarSet, Mode, S, !U),
        add_string(".\n", S, !U)
    ).

:- pred mercury_format_mode_defn_head(inst_varset::in, prog_context::in,
    sym_name::in, list(inst_var)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, S, !U) :-
    ArgTerms = list.map(func(V) = variable(V, Context), Args),
    construct_qualified_term_with_context(Name, ArgTerms, Context, ModeTerm),
    mercury_format_term_vs(InstVarSet, print_name_only, ModeTerm, S, !U).

%---------------------------------------------------------------------------%

    % Output the given predicate declaration, after
    %
    % - Maybe Unqualifying the predicate name, and
    % - Maybe writing out the line number Context.
    %
:- pred mercury_format_item_pred_decl_mu_mc(merc_out_info::in,
    var_name_print::in, S::in, item_pred_decl_info::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_pred_decl_mu_mc(Info, VarNamePrint, S,
        ItemPredDecl0, !U) :-
    MaybeQualifiedItemNames = get_maybe_qualified_item_names(Info),
    (
        MaybeQualifiedItemNames = qualified_item_names,
        ItemPredDecl = ItemPredDecl0
    ;
        MaybeQualifiedItemNames = unqualified_item_names,
        PredSymName0 = ItemPredDecl0 ^ pf_name,
        PredSymName = unqualified(unqualify_name(PredSymName0)),
        ItemPredDecl = ItemPredDecl0 ^ pf_name := PredSymName
    ),
    maybe_format_line_number(Info, ItemPredDecl ^ pf_context, S, !U),
    Lang = get_output_lang(Info),
    mercury_format_item_pred_decl(Lang, VarNamePrint, S, ItemPredDecl, !U).

mercury_format_item_pred_decl(Lang, VarNamePrint, S, ItemPredDecl, !U) :-
    % Most of the code that outputs pred declarations is in
    % parse_tree_out_pred_decl.m.
    ItemPredDecl = item_pred_decl_info(PredSymName, PredOrFunc, TypesAndModes,
        WithType, WithInst, MaybeDetism, _Origin, TypeVarSet, InstVarSet,
        ExistQVars, Purity, Constraints, _Context, _SeqNum),
    ( if
        % Function declarations using `with_type` have the same format
        % as predicate declarations, but with `func' instead of `pred'.
        PredOrFunc = pf_function,
        WithType = no
    then
        pred_args_to_func_args(TypesAndModes, FuncTypesAndModes,
            RetTypeAndMode),
        mercury_format_func_decl(Lang, VarNamePrint,
            TypeVarSet, InstVarSet, ExistQVars,
            PredSymName, FuncTypesAndModes, RetTypeAndMode, MaybeDetism,
            Purity, Constraints, ":- ", ".\n", ".\n", S, !U)
    else
        mercury_format_pred_or_func_decl(Lang, VarNamePrint,
            TypeVarSet, InstVarSet, PredOrFunc, ExistQVars,
            PredSymName, TypesAndModes, WithType, WithInst, MaybeDetism,
            Purity, Constraints, ":- ", ".\n", ".\n", S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_mode_decl(Info, S, ItemModeDecl, !U) :-
    % Most of the code that outputs mode declarations is in
    % parse_tree_out_pred_decl.m.
    ItemModeDecl = item_mode_decl_info(PredSymName0, MaybePredOrFunc, ArgModes,
        MaybeWithInst, MaybeDetism, InstVarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, PredSymName0, PredSymName),
    maybe_format_line_number(Info, Context, S, !U),
    Lang = get_output_lang(Info),
    ( if
        MaybePredOrFunc = yes(pf_function),
        % Function mode declarations using `with_type` have the same format
        % as predicate mode declarations.
        MaybeWithInst = no
    then
        pred_args_to_func_args(ArgModes, FuncArgModes, ReturnMode),
        mercury_format_func_mode_decl(Lang, InstVarSet, PredSymName,
            FuncArgModes, ReturnMode, MaybeDetism, S, !U)
    else
        mercury_format_pred_mode_decl(Lang, InstVarSet, PredSymName,
            ArgModes, MaybeWithInst, MaybeDetism, S, !U)
    ).

%---------------------------------------------------------------------------%

mercury_format_item_foreign_enum(_Info, S, ItemForeignEnum, !U) :-
    ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, OoMValues,
        _Context, _SeqNum),
    add_string(":- pragma foreign_enum(", S, !U),
    mercury_format_foreign_language_string(Lang, S, !U),
    add_string(", ", S, !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName,
        S, !U),
    add_string("/", S, !U),
    add_int(TypeArity, S, !U),
    add_string(", ", S, !U),
    Values = one_or_more_to_list(OoMValues),
    mercury_format_unqual_sym_name_string_assoc_list(Values, S, !U),
    add_string(").\n", S, !U).

    % Output an association list of to-be-unqualified sym_names and strings.
    % The strings will be quoted in the output.
    %
:- pred mercury_format_unqual_sym_name_string_assoc_list(
    assoc_list(sym_name, string)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_unqual_sym_name_string_assoc_list(AssocList, S, !U) :-
    add_char('[', S, !U),
    add_list(mercury_format_unqual_sym_name_string_pair, ", ",
        AssocList, S, !U),
    add_char(']', S, !U).

:- pred mercury_format_unqual_sym_name_string_pair(
    pair(sym_name, string)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_unqual_sym_name_string_pair(SymName0 - String, S, !U) :-
    Name = unqualify_name(SymName0),
    SymName = unqualified(Name),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, SymName,
        S, !U),
    add_string(" - ", S, !U),
    add_quoted_string(String, S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_item_foreign_export_enum(merc_out_info::in,
    S::in, item_foreign_export_enum_info::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_foreign_export_enum(_Info, S, ItemForeignExportEnum, !U) :-
    ItemForeignExportEnum = item_foreign_export_enum_info(Lang, TypeCtor,
        Attributes, Overrides, _Context, _SeqNum),
    add_string(":- pragma foreign_export_enum(", S, !U),
    mercury_format_foreign_language_string(Lang, S, !U),
    add_string(", ", S, !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName,
        S, !U),
    add_string("/", S, !U),
    add_int(TypeArity, S, !U),
    add_string(", ", S, !U),
    mercury_format_foreign_export_enum_attributes(Attributes, S, !U),
    add_string(", ", S, !U),
    mercury_format_sym_name_string_assoc_list(Overrides, S, !U),
    add_string(").\n", S, !U).

:- pred mercury_format_foreign_export_enum_attributes(
    export_enum_attributes::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_foreign_export_enum_attributes(Attributes, S, !U) :-
    MaybePrefix = Attributes ^ ee_attr_prefix,
    add_string("[", S, !U),
    (
        MaybePrefix = no
    ;
        MaybePrefix = yes(Prefix),
        add_string("prefix(", S, !U),
        add_quoted_string(Prefix, S, !U),
        add_char(')', S, !U)
    ),
    add_string("]", S, !U).

    % Output an association list of sym_names and strings.
    % The strings will be quoted in the output.
    %
:- pred mercury_format_sym_name_string_assoc_list(
    assoc_list(sym_name, string)::in, S::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_sym_name_string_assoc_list(AssocList, S, !U) :-
    add_char('[', S, !U),
    add_list(mercury_format_sym_name_string_pair, ", ", AssocList, S, !U),
    add_char(']', S, !U).

:- pred mercury_format_sym_name_string_pair(
    pair(sym_name, string)::in, S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_sym_name_string_pair(SymName - String, S, !U) :-
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, SymName,
        S, !U),
    add_string(" - ", S, !U),
    add_quoted_string(String, S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_item_promise(merc_out_info::in, S::in,
    item_promise_info::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_promise(_, S, ItemPromise, !U) :-
    % Any changes here may require similar changes in the write_promise
    % predicate in intermod.m.
    ItemPromise = item_promise_info(PromiseType, Goal, VarSet, UnivVars,
        _Context, _SeqNum),
    UnivVarStrs = list.map(varset.lookup_name(VarSet), UnivVars),
    UnivVarsStr = string.join_list(", ", UnivVarStrs),
    % The parentheses around the goal are required; without them,
    % operator precedence problems prevent the parser from being able
    % to read back in the promises we write out.
    (
        PromiseType = promise_type_true,
        string.format(":- promise all [%s] ", [s(UnivVarsStr)], PrefixStr)
    ;
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        string.format(":- all [%s]\n%s\n",
            [s(UnivVarsStr), s(promise_to_string(PromiseType))], PrefixStr)
    ),
    add_string(PrefixStr, S, !U),
    add_string("(\n", S, !U),
    Indent = 1,
    mercury_format_goal(S, VarSet, Indent, Goal, !U),
    add_string("\n).\n", S, !U).

%---------------------------------------------------------------------------%

mercury_format_item_typeclass(Info, S, ItemTypeClass, !U) :-
    ItemTypeClass = item_typeclass_info(ClassName0, Vars, Constraints, FunDeps,
        Interface, VarSet, _Context, _SeqNum),
    maybe_unqualify_sym_name(Info, ClassName0, ClassName),
    ClassNameStr = mercury_sym_name_to_string(ClassName),
    VarStrs = list.map(varset.lookup_name(VarSet), Vars),
    VarsStr = string.join_list(", ", VarStrs),
    string.format(":- typeclass %s(%s)",
        [s(ClassNameStr), s(VarsStr)], DeclStr),
    add_string(DeclStr, S, !U),
    mercury_format_fundeps_and_prog_constraint_list(VarSet, print_name_only,
        FunDeps, Constraints, S, !U),
    (
        Interface = class_interface_abstract,
        add_string(".\n", S, !U)
    ;
        Interface = class_interface_concrete(ClassDecls),
        (
            ClassDecls = [],
            add_string(" where [].\n", S, !U)
        ;
            ClassDecls = [HeadClassDecl | TailClassDecls],
            add_string(" where [\n", S, !U),
            Lang = get_output_lang(Info),
            format_class_decls(S, Lang, print_name_only,
                HeadClassDecl, TailClassDecls, !U),
            add_string("].\n", S, !U)
        )
    ).

mercury_format_item_abstract_typeclass(Info, S, ItemTypeClass, !U) :-
    ItemTypeClass = item_typeclass_info(ClassName0, Vars, Constraints, FunDeps,
        class_interface_abstract, VarSet, _Context, _SeqNum),
    maybe_unqualify_sym_name(Info, ClassName0, ClassName),
    ClassNameStr = mercury_sym_name_to_string(ClassName),
    VarStrs = list.map(varset.lookup_name(VarSet), Vars),
    VarsStr = string.join_list(", ", VarStrs),
    string.format(":- typeclass %s(%s)", [s(ClassNameStr), s(VarsStr)],
        StartStr),
    add_string(StartStr, S, !U),
    mercury_format_fundeps_and_prog_constraint_list(VarSet, print_name_only,
        FunDeps, Constraints, S, !U),
    add_string(".\n", S, !U).

:- pred mercury_format_fundeps_and_prog_constraint_list(tvarset::in,
    var_name_print::in, list(prog_fundep)::in, list(prog_constraint)::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_fundeps_and_prog_constraint_list(VarSet, VarNamePrint,
        FunDeps, Constraints, S, !U) :-
    ( if
        FunDeps = [],
        Constraints = []
    then
        true
    else
        add_string(" <= (", S, !U),
        add_list(mercury_format_fundep(VarSet, VarNamePrint), ", ", FunDeps,
            S, !U),
        (
            Constraints = []
        ;
            Constraints = [_ | _],
            (
                FunDeps = []
            ;
                FunDeps = [_ | _],
                add_string(", ", S, !U)
            ),
            add_list(mercury_format_constraint(VarSet, VarNamePrint),
                ", ", Constraints, S, !U)
        ),
        add_string(")", S, !U)
    ).

:- pred mercury_format_fundep(tvarset::in, var_name_print::in, prog_fundep::in,
    S::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_fundep(TypeVarSet, VarNamePrint, fundep(Domain, Range),
        S, !U) :-
    add_string("(", S, !U),
    add_list(mercury_format_var_vs(TypeVarSet, VarNamePrint), ", ", Domain,
        S, !U),
    add_string(" -> ", S, !U),
    add_list(mercury_format_var_vs(TypeVarSet, VarNamePrint), ", ", Range,
        S, !U),
    add_string(")", S, !U).

:- pred format_class_decls(S::in, output_lang::in, var_name_print::in,
    class_decl::in, list(class_decl)::in, U::di, U::uo) is det
    <= pt_output(S, U).

format_class_decls(S, Lang, VarNamePrint, HeadClassDecl, TailClassDecls, !U) :-
    format_class_decl(S, Lang, VarNamePrint, HeadClassDecl, !U),
    (
        TailClassDecls = [],
        add_string("\n", S, !U)
    ;
        TailClassDecls = [HeadTailClassDecl | TailTailClassDecls],
        add_string(",\n", S, !U),
        format_class_decls(S, Lang, VarNamePrint,
            HeadTailClassDecl, TailTailClassDecls, !U)
    ).

:- pred format_class_decl(S::in, output_lang::in, var_name_print::in,
    class_decl::in, U::di, U::uo) is det <= pt_output(S, U).

format_class_decl(S, Lang, VarNamePrint, Decl, !U) :-
    add_string("\t", S, !U),
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(SymName0, PredOrFunc,
            TypesAndModes, WithType, WithInst, MaybeDetism,
            TypeVarSet, InstVarSet, ExistQVars, Purity,
            Constraints, _Context),

        % The module name is implied by the qualifier of the
        % `:- typeclass declaration'.
        SymName = unqualified(unqualify_name(SymName0)),
        ( if
            % Function declarations using `with_type` have the same format
            % as predicate declarations, but with `func' instead of `pred'.
            PredOrFunc = pf_function,
            WithType = no
        then
            pred_args_to_func_args(TypesAndModes,
                FuncTypesAndModes, RetTypeAndMode),
            mercury_format_func_decl(Lang, VarNamePrint,
                TypeVarSet, InstVarSet, ExistQVars,
                SymName, FuncTypesAndModes, RetTypeAndMode, MaybeDetism,
                Purity, Constraints, "", ",\n\t", "", S, !U)
        else
            mercury_format_pred_or_func_decl(Lang, VarNamePrint,
                TypeVarSet, InstVarSet, PredOrFunc, ExistQVars,
                SymName, TypesAndModes, WithType, WithInst, MaybeDetism,
                Purity, Constraints, "", ",\n\t", "", S, !U)
        )
    ;
        Decl = class_decl_mode(ModeInfo),
        ModeInfo = class_mode_info(SymName0, PredOrFunc, Modes,
            WithInst, MaybeDetism, InstVarSet, _Context),

        % The module name is implied by the qualifier of the
        % `:- typeclass declaration'.
        SymName = unqualified(unqualify_name(SymName0)),
        ( if
            % Function mode declarations using `with_type` have the same format
            % as predicate mode declarations.
            PredOrFunc = yes(pf_function),
            WithInst = no
        then
            pred_args_to_func_args(Modes, FuncModes, RetMode),
            mercury_format_func_mode_decl_gen(Lang, InstVarSet,
                SymName, FuncModes, RetMode, MaybeDetism, "", "", S, !U)
        else
            mercury_format_pred_or_func_mode_decl_gen(Lang, InstVarSet,
                SymName, Modes, WithInst, MaybeDetism, "", "", S, !U)
        )
    ).

%---------------------------------------------------------------------------%

mercury_format_item_instance(_Info, S, ItemInstance, !U) :-
    ItemInstance = item_instance_info(_ClassName, _Types, _OriginalTypes,
        _Constraints, Body, _VarSet, _InstanceModuleName, _Context, _SeqNum),
    HeaderStr = mercury_instance_header_to_string(ItemInstance),
    (
        Body = instance_body_abstract,
        string.format("%s.\n", [s(HeaderStr)], DeclStr),
        add_string(DeclStr, S, !U)
    ;
        Body = instance_body_concrete(Methods),
        (
            Methods = [],
            string.format("%s where [].\n", [s(HeaderStr)], DeclStr),
            add_string(DeclStr, S, !U)
        ;
            Methods = [HeadMethod | TailMethods],
            string.format("%s where [\n", [s(HeaderStr)], DeclStartStr),
            add_string(DeclStartStr, S, !U),
            mercury_format_instance_methods(S, HeadMethod, TailMethods, !U),
            add_string("].\n", S, !U)
        )
    ).

item_abstract_instance_to_string(Info, ItemAbstractInstance) = Str :-
    State0 = string.builder.init,
    mercury_format_item_abstract_instance(Info, string.builder.handle,
        ItemAbstractInstance, State0, State),
    Str = string.builder.to_string(State).

mercury_format_item_abstract_instance(_Info, S, ItemAbstractInstance, !U) :-
    HeaderStr =
        mercury_instance_header_to_string(coerce(ItemAbstractInstance)),
    string.format("%s.\n", [s(HeaderStr)], DeclStr),
    add_string(DeclStr, S, !U).

:- func mercury_instance_header_to_string(item_instance_info) = string.

mercury_instance_header_to_string(ItemInstance) = Str :-
    % XXX When prettyprinting a Mercury module, we want to print the original
    % types. When generating interface files, we want to print the
    % equiv-type-expanded types. We do the latter.
    % XXX We could add an argument, or a field to merc_out_info,
    % that says which kind of file we are generating.
    ItemInstance = item_instance_info(ClassName, Types, _OriginalTypes,
        Constraints, _Body, VarSet, _InstanceModuleName, _Context, _SeqNum),
    ClassNameStr = mercury_sym_name_to_string(ClassName),
    TypeStrs =
        list.map(mercury_type_to_string(VarSet, print_name_only), Types),
    TypesStr = string.join_list(", ", TypeStrs),
    ConstraintsStr = mercury_prog_constraint_list_to_string(VarSet,
        print_name_only, "<=", Constraints),
    ( if
        ( ops.mercury_op_table_is_op(ClassNameStr)
        ; not is_all_alnum_or_underscore(ClassNameStr)
        )
    then
        % We put an extra set of brackets around the class name
        % if the name is an operator or contains non-alphanumeric characters.
        string.format(":- instance (%s(%s))%s",
            [s(ClassNameStr), s(TypesStr), s(ConstraintsStr)], Str)
    else
        string.format(":- instance %s(%s)%s",
            [s(ClassNameStr), s(TypesStr), s(ConstraintsStr)], Str)
    ).

:- pred mercury_format_instance_methods(S::in,
    instance_method::in, list(instance_method)::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_instance_methods(S, HeadMethod, TailMethods, !U) :-
    mercury_format_instance_method(HeadMethod, S, !U),
    (
        TailMethods = [],
        add_string("\n", S, !U)
    ;
        TailMethods = [HeadTailMethod | TailTailMethods],
        add_string(",\n", S, !U),
        mercury_format_instance_methods(S, HeadTailMethod, TailTailMethods, !U)
    ).

mercury_format_instance_method(Method, S, !U) :-
    Method = instance_method(MethodId, Defn, _Context),
    MethodId = pred_pf_name_arity(PredOrFunc, MethodSymName, UserArity),
    UserArity = user_arity(UserArityInt),
    (
        Defn = instance_proc_def_name(PredName),
        PFStr = pred_or_func_to_str(PredOrFunc),
        MethodSymNameStr = mercury_bracketed_sym_name_to_string_ngt(
            next_to_graphic_token, MethodSymName),
        PredNameStr = mercury_bracketed_sym_name_to_string(PredName),
        string.format("\t%s(%s/%d) is %s",
            [s(PFStr), s(MethodSymNameStr), i(UserArityInt), s(PredNameStr)],
            DeclStr),
        add_string(DeclStr, S, !U)
    ;
        Defn = instance_proc_def_clauses(ItemsCord),
        Items = cord.list(ItemsCord),
        % XXX should we output the term contexts?
        add_string("\t(", S, !U),
        add_list(mercury_format_instance_method_clause(MethodSymName),
            "),\n\t(", Items, S, !U),
        add_string(")", S, !U)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_format_item_initialise(merc_out_info::in, S::in,
    item_initialise_info::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_initialise(_, S, ItemInitialise, !IO) :-
    ItemInitialise = item_initialise_info(PredSymName, UserArity, _, _, _),
    PredSymNameStr = mercury_bracketed_sym_name_to_string(PredSymName),
    UserArity = user_arity(UserArityInt),
    string.format(":- initialise %s/%d.\n",
        [s(PredSymNameStr), i(UserArityInt)], DeclStr),
    add_string(DeclStr, S, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_format_item_finalise(merc_out_info::in, S::in,
    item_finalise_info::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_finalise(_, S, ItemFinalise, !IO) :-
    ItemFinalise = item_finalise_info(PredSymName, UserArity, _, _, _),
    PredSymNameStr = mercury_bracketed_sym_name_to_string(PredSymName),
    UserArity = user_arity(UserArityInt),
    string.format(":- finalise %s/%d.\n",
        [s(PredSymNameStr), i(UserArityInt)], DeclStr),
    add_string(DeclStr, S, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_format_item_mutable(merc_out_info::in, S::in,
    item_mutable_info::in, U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_mutable(Info, S, ItemMutable, !U) :-
    ItemMutable = item_mutable_info(Name, _OrigType, Type, _OrigInst, Inst,
        InitTerm, MutVarSet, Attrs, _Context, _SeqNum),
    add_string(":- mutable(", S, !U),
    add_string(Name, S, !U),
    add_string(", ", S, !U),
    mercury_format_type(varset.init, print_name_only, Type, S, !U),
    add_string(", ", S, !U),

    % See the comments for read_mutable_decl for the reason we _must_ use
    % MutVarSet here.
    mercury_format_term_vs(MutVarSet, print_name_only, InitTerm, S, !U),
    add_string(", ", S, !U),
    Lang = get_output_lang(Info),
    mercury_format_inst(Lang, varset.init, Inst, S, !U),
    add_string(", ", S, !U),
    add_string(string.string(Attrs), S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%

:- pred mercury_format_item_foreign_import_module(S::in, item_fim::in,
    U::di, U::uo) is det <= pt_output(S, U).

mercury_format_item_foreign_import_module(S, ItemFIM, !U) :-
    ItemFIM = item_fim(Lang, ModuleName, _Context, _SeqNum),
    FIMSpec = fim_spec(Lang, ModuleName),
    mercury_format_fim_spec(S, FIMSpec, !U).

mercury_output_fim_spec(Stream, FIMSpec, !IO) :-
    mercury_format_fim_spec(Stream, FIMSpec, !IO).

mercury_format_fim_spec(S, FIMSpec, !U) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    add_string(":- pragma foreign_import_module(", S, !U),
    mercury_format_foreign_language_string(Lang, S, !U),
    add_string(", ", S, !U),
    mercury_format_bracketed_sym_name_ngt(not_next_to_graphic_token,
        ModuleName, S, !U),
    add_string(").\n", S, !U).

%---------------------------------------------------------------------------%

maybe_format_block_start_blank_line(S, Items, !U) :-
    (
        Items = []
    ;
        Items = [_ | _],
        add_string("\n", S, !U)
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out.
%---------------------------------------------------------------------------%
