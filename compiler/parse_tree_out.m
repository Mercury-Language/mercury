%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2015 The Mercury team.
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
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

    % output_parse_tree_*(Globals, OutputFileName, ParseTree, !IO).
    % XXX output_parse_tree_opt is unused. intermod.m should be updated
    % to use it.
    %
:- pred output_parse_tree_src(globals::in, string::in, parse_tree_src::in,
    io::di, io::uo) is det.
:- pred output_parse_tree_int(globals::in, string::in, parse_tree_int::in,
    io::di, io::uo) is det.
:- pred output_parse_tree_opt(globals::in, string::in, parse_tree_opt::in,
    io::di, io::uo) is det.

:- pred output_parse_tree_int0(globals::in, string::in,
    parse_tree_int0::in, io::di, io::uo) is det.
:- pred output_parse_tree_int1(globals::in, string::in,
    parse_tree_int1::in, io::di, io::uo) is det.
:- pred output_parse_tree_int2(globals::in, string::in,
    parse_tree_int2::in, io::di, io::uo) is det.
:- pred output_parse_tree_int3(globals::in, string::in,
    parse_tree_int3::in, io::di, io::uo) is det.

:- pred output_parse_tree_plain_opt(globals::in, string::in,
    parse_tree_plain_opt::in, io::di, io::uo) is det.
:- pred output_parse_tree_trans_opt(globals::in, string::in,
    parse_tree_trans_opt::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_parse_tree_src(merc_out_info::in,
    parse_tree_src::in, io::di, io::uo) is det.

:- pred mercury_output_parse_tree_module_src(merc_out_info::in,
    parse_tree_module_src::in, io::di, io::uo) is det.

:- pred mercury_output_ancestor_int_spec(merc_out_info::in,
    ancestor_int_spec::in, io::di, io::uo) is det.
:- pred mercury_output_direct_int_spec(merc_out_info::in,
    direct_int_spec::in, io::di, io::uo) is det.
:- pred mercury_output_indirect_int_spec(merc_out_info::in,
    indirect_int_spec::in, io::di, io::uo) is det.
:- pred mercury_output_int_for_opt_spec(merc_out_info::in,
    int_for_opt_spec::in, io::di, io::uo) is det.

:- pred mercury_output_parse_tree_int(merc_out_info::in,
    parse_tree_int::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_int0(merc_out_info::in,
    parse_tree_int0::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_int1(merc_out_info::in,
    parse_tree_int1::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_int2(merc_out_info::in,
    parse_tree_int2::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_int3(merc_out_info::in,
    parse_tree_int3::in, io::di, io::uo) is det.

:- pred mercury_output_parse_tree_opt(merc_out_info::in,
    parse_tree_opt::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_plain_opt(merc_out_info::in,
    parse_tree_plain_opt::in, io::di, io::uo) is det.
:- pred mercury_output_parse_tree_trans_opt(merc_out_info::in,
    parse_tree_trans_opt::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_raw_compilation_unit(merc_out_info::in,
    raw_compilation_unit::in, io::di, io::uo) is det.

:- pred mercury_output_aug_compilation_unit(merc_out_info::in,
    aug_compilation_unit::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_raw_item_blocks(merc_out_info::in,
    list(raw_item_block)::in, io::di, io::uo) is det.
:- pred mercury_output_raw_item_block(merc_out_info::in,
    raw_item_block::in, io::di, io::uo) is det.

:- pred mercury_output_src_item_blocks(merc_out_info::in,
    list(src_item_block)::in, io::di, io::uo) is det.
:- pred mercury_output_src_item_block(merc_out_info::in,
    src_item_block::in, io::di, io::uo) is det.
:- pred mercury_output_int_item_blocks(merc_out_info::in,
    list(int_item_block)::in, io::di, io::uo) is det.
:- pred mercury_output_int_item_block(merc_out_info::in,
    int_item_block::in, io::di, io::uo) is det.
:- pred mercury_output_opt_item_blocks(merc_out_info::in,
    list(opt_item_block)::in, io::di, io::uo) is det.
:- pred mercury_output_opt_item_block(merc_out_info::in,
    opt_item_block::in, io::di, io::uo) is det.
:- pred mercury_output_int_for_opt_item_blocks(merc_out_info::in,
    list(int_for_opt_item_block)::in, io::di, io::uo) is det.
:- pred mercury_output_int_for_opt_item_block(merc_out_info::in,
    int_for_opt_item_block::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred mercury_output_item(merc_out_info::in, item::in, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%
%
% Output some components of type definitions.
%

:- pred mercury_output_where_attributes(merc_out_info::in, tvarset::in,
    maybe(solver_type_details)::in, maybe_canonical::in,
    maybe(list(sym_name_arity))::in, io::di, io::uo) is det.

:- pred mercury_output_ctor(tvarset::in, constructor::in, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%
%
% Output some components of an instance definition.
%

:- pred mercury_output_instance_method(instance_method::in, io::di, io::uo)
    is det.

%---------------------------------------------------------------------------%
%
% Output a foreign_import_module pragma.
%

:- pred mercury_output_fim_spec(fim_spec::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.canonicalize_interface.
:- import_module parse_tree.file_kind.
:- import_module parse_tree.item_util.
:- import_module parse_tree.maybe_error.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.parse_inst_mode_name.
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_term.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module recompilation.
:- import_module recompilation.version.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_io.
:- import_module uint.
:- import_module varset.

%---------------------------------------------------------------------------%

output_parse_tree_src(Globals, OutputFileName, ParseTreeSrc, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_src, ParseTreeSrc, !IO).

output_parse_tree_int(Globals, OutputFileName, ParseTreeInt, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_int, ParseTreeInt, !IO).

output_parse_tree_opt(Globals, OutputFileName, ParseTreeOpt, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_opt, ParseTreeOpt, !IO).

%---------------------%

output_parse_tree_int0(Globals, OutputFileName, ParseTreeInt0, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_int0, ParseTreeInt0, !IO).

output_parse_tree_int1(Globals, OutputFileName, ParseTreeInt1, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_int1, ParseTreeInt1, !IO).

output_parse_tree_int2(Globals, OutputFileName, ParseTreeInt2, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_int2, ParseTreeInt2, !IO).

output_parse_tree_int3(Globals, OutputFileName, ParseTreeInt3, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_int3, ParseTreeInt3, !IO).

%---------------------%

output_parse_tree_plain_opt(Globals, OutputFileName, ParseTreePlainOpt, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_plain_opt, ParseTreePlainOpt, !IO).

output_parse_tree_trans_opt(Globals, OutputFileName, ParseTreeTransOpt, !IO) :-
    output_some_parse_tree(Globals, OutputFileName,
        mercury_output_parse_tree_trans_opt, ParseTreeTransOpt, !IO).

%---------------------------------------------------------------------------%

:- type output_parse_tree(PT) == pred(merc_out_info, PT, io, io).
:- inst output_parse_tree == (pred(in, in, di, uo) is det).

:- pred output_some_parse_tree(globals::in, string::in,
    output_parse_tree(PT)::in(output_parse_tree),
    PT::in, io::di, io::uo) is det.

output_some_parse_tree(Globals, OutputFileName, OutputParseTree,
        ParseTree, !IO) :-
    io.open_output(OutputFileName, Res, !IO),
    (
        Res = ok(FileStream),
        globals.lookup_bool_option(Globals, verbose, Verbose),
        (
            Verbose = yes,
            io.format("%% Writing output to %s...", [s(OutputFileName)], !IO),
            io.flush_output(!IO)
        ;
            Verbose = no
        ),
        io.set_output_stream(FileStream, OutputStream, !IO),

        % Module qualifiers on items are redundant after the
        % declaration above.
        % XXX What declaration?
        Info = init_merc_out_info(Globals, unqualified_item_names,
            output_mercury),
        OutputParseTree(Info, ParseTree, !IO),
        io.set_output_stream(OutputStream, _, !IO),
        io.close_output(FileStream, !IO),
        (
            Verbose = yes,
            io.write_string(" done\n", !IO)
        ;
            Verbose = no
        )
    ;
        Res = error(_),
        io.format("Error: couldn't open file `%s' for output.\n",
            [s(OutputFileName)], !IO)
    ).

%---------------------------------------------------------------------------%

mercury_output_parse_tree_src(Info, ParseTree, !IO) :-
    ParseTree = parse_tree_src(ModuleName, _Context, ModuleComponentsCord),
    mercury_output_module_decl("module", ModuleName, !IO),
    ModuleComponents = cord.list(ModuleComponentsCord),
    mercury_output_module_components(Info, no, ModuleComponents, !IO),
    mercury_output_module_decl("end_module", ModuleName, !IO).

mercury_output_parse_tree_module_src(Info, ParseTreeModuleSrc, !IO) :-
    ParseTreeModuleSrc = parse_tree_module_src(ModuleName, _ModuleContext,
        IntIncludeMap, ImpIncludeMap, InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, ImportUseMap,
        IntFIMSpecMap, ImpFIMSpecMap, MaybeImplicitFIMLangs,

        IntTypeDefnsAbs, IntTypeDefnsMer, IntTypeDefnsForeign,
        IntInstDefns, IntModeDefns, IntTypeClasses, IntInstances,
        IntPredDecls, IntModeDecls,
        IntForeignExportEnums, IntDeclPragmas, IntPromises, _IntBadPreds,

        ImpTypeDefnsAbs, ImpTypeDefnsMer, ImpTypeDefnsForeign,
        ImpInstDefns, ImpModeDefns, ImpTypeClasses, ImpInstances,
        ImpPredDecls, ImpModeDecls, ImpClauses,
        ImpForeignEnums, ImpForeignExportEnums,
        ImpDeclPragmas, ImpImplPragmas, ImpPromises,
        ImpInitialises, ImpFinalises, ImpMutables),

    io.write_string("% module src\n", !IO),
    mercury_output_module_decl("module", ModuleName, !IO),

    io.write_string("% include_module_map\n", !IO),
    map.foldl(write_include_module_map_entry, InclMap, !IO),
    io.write_string("% section_import_and_or_use_map\n", !IO),
    map.foldl(write_import_use_map_entry, ImportUseMap, !IO),

    mercury_output_section_marker(ms_interface, !IO),
    list.foldl(mercury_output_module_decl("include_module"),
        map.keys(IntIncludeMap), !IO),
    list.foldl(mercury_output_module_decl("import_module"),
        map.keys(IntImportMap), !IO),
    list.foldl(mercury_output_module_decl("use_module"),
        map.keys(IntUseMap), !IO),
    list.foldl(mercury_output_fim_spec, map.keys(IntFIMSpecMap), !IO),
    (
        MaybeImplicitFIMLangs = no,
        io.write_string("% implicit FIM self-import languages not set\n", !IO)
    ;
        MaybeImplicitFIMLangs = yes(ImplicitFIMLangs),
        ImplicitFIMLangStrs = list.map(mercury_foreign_language_to_string,
            set.to_sorted_list(ImplicitFIMLangs)),
        io.format("%% implicit FIM self-import languages: %s\n",
           [s(join_list(", ", ImplicitFIMLangStrs))], !IO)
    ),
    list.foldl(mercury_output_item_type_defn(Info), IntTypeDefnsAbs, !IO),
    list.foldl(mercury_output_item_type_defn(Info), IntTypeDefnsMer, !IO),
    list.foldl(mercury_output_item_type_defn(Info), IntTypeDefnsForeign, !IO),
    list.foldl(mercury_output_item_inst_defn(Info), IntInstDefns, !IO),
    list.foldl(mercury_output_item_mode_defn(Info), IntModeDefns, !IO),
    list.foldl(mercury_output_item_typeclass(Info), IntTypeClasses, !IO),
    list.foldl(mercury_output_item_instance(Info), IntInstances, !IO),
    list.foldl(mercury_output_item_pred_decl(Info), IntPredDecls, !IO),
    list.foldl(mercury_output_item_mode_decl(Info), IntModeDecls, !IO),
    list.foldl(mercury_output_item_foreign_export_enum(Info),
        IntForeignExportEnums, !IO),
    list.foldl(mercury_output_item_decl_pragma(Info), IntDeclPragmas, !IO),
    list.foldl(mercury_output_item_promise(Info), IntPromises, !IO),

    mercury_output_section_marker(ms_implementation, !IO),
    list.foldl(mercury_output_module_decl("include_module"),
        map.keys(ImpIncludeMap), !IO),
    list.foldl(mercury_output_module_decl("import_module"),
        map.keys(ImpImportMap), !IO),
    list.foldl(mercury_output_module_decl("use_module"),
        map.keys(ImpUseMap), !IO),
    list.foldl(mercury_output_fim_spec, map.keys(ImpFIMSpecMap), !IO),
    list.foldl(mercury_output_item_type_defn(Info), ImpTypeDefnsAbs, !IO),
    list.foldl(mercury_output_item_type_defn(Info), ImpTypeDefnsMer, !IO),
    list.foldl(mercury_output_item_type_defn(Info), ImpTypeDefnsForeign, !IO),
    list.foldl(mercury_output_item_inst_defn(Info), ImpInstDefns, !IO),
    list.foldl(mercury_output_item_mode_defn(Info), ImpModeDefns, !IO),
    list.foldl(mercury_output_item_typeclass(Info), ImpTypeClasses, !IO),
    list.foldl(mercury_output_item_instance(Info), ImpInstances, !IO),
    list.foldl(mercury_output_item_pred_decl(Info), ImpPredDecls, !IO),
    list.foldl(mercury_output_item_mode_decl(Info), ImpModeDecls, !IO),
    list.foldl(mercury_output_item_clause(Info), ImpClauses, !IO),
    list.foldl(mercury_output_item_foreign_enum(Info), ImpForeignEnums, !IO),
    list.foldl(mercury_output_item_foreign_export_enum(Info),
        ImpForeignExportEnums, !IO),
    list.foldl(mercury_output_item_decl_pragma(Info), ImpDeclPragmas, !IO),
    list.foldl(mercury_output_item_impl_pragma(Info), ImpImplPragmas, !IO),
    list.foldl(mercury_output_item_promise(Info), ImpPromises, !IO),
    list.foldl(mercury_output_item_initialise(Info), ImpInitialises, !IO),
    list.foldl(mercury_output_item_finalise(Info), ImpFinalises, !IO),
    list.foldl(mercury_output_item_mutable(Info), ImpMutables, !IO),
    mercury_output_module_decl("end_module", ModuleName, !IO),
    io.nl(!IO).

:- pred write_include_module_map_entry(module_name::in,
    include_module_info::in, io::di, io::uo) is det.

write_include_module_map_entry(ModuleName, InclInfo, !IO) :-
    InclInfo = include_module_info(Section, _Context),
    io.write_string("% ", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io.write_string(" -> ", !IO),
    (
        Section = ms_interface,
        io.write_string("interface", !IO)
    ;
        Section = ms_implementation,
        io.write_string("implementation", !IO)
    ),
    io.nl(!IO).

:- pred write_import_use_map_entry(module_name::in,
    maybe_implicit_import_and_or_use::in, io::di, io::uo) is det.

write_import_use_map_entry(ModuleName, ImportAndOrUse, !IO) :-
    io.write_string("% ", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io.write_string(" -> ", !IO),
    (
        ImportAndOrUse = explicit_avail(SectionImportAndOrUse),
        (
            SectionImportAndOrUse = int_import(_),
            io.write_string("int_import", !IO)
        ;
            SectionImportAndOrUse = int_use(_),
            io.write_string("int_use", !IO)
        ;
            SectionImportAndOrUse = imp_import(_),
            io.write_string("imp_import", !IO)
        ;
            SectionImportAndOrUse = imp_use(_),
            io.write_string("imp_use", !IO)
        ;
            SectionImportAndOrUse = int_use_imp_import(_, _),
            io.write_string("int_use_imp_import", !IO)
        )
    ;
        ImportAndOrUse = implicit_avail(ImplicitImportAndOrUse, _),
        (
            ImplicitImportAndOrUse = implicit_int_import,
            io.write_string("implicit_int_import", !IO)
        ;
            ImplicitImportAndOrUse = implicit_int_use,
            io.write_string("implicit_int_use", !IO)
        ;
            ImplicitImportAndOrUse = implicit_imp_use,
            io.write_string("implicit_imp_use", !IO)
        )
    ),
    io.nl(!IO).

%---------------------------------------------------------------------------%

mercury_output_ancestor_int_spec(Info, AncestorIntSpec, !IO) :-
    AncestorIntSpec = ancestor_int0(ParseTreeInt0, _),
    mercury_output_parse_tree_int0(Info, ParseTreeInt0, !IO).

mercury_output_direct_int_spec(Info, DirectIntSpec, !IO) :-
    (
        DirectIntSpec = direct_int1(ParseTreeInt1, _),
        mercury_output_parse_tree_int1(Info, ParseTreeInt1, !IO)
    ;
        DirectIntSpec = direct_int3(ParseTreeInt3, _),
        mercury_output_parse_tree_int3(Info, ParseTreeInt3, !IO)
    ).

mercury_output_indirect_int_spec(Info, IndirectIntSpec, !IO) :-
    (
        IndirectIntSpec = indirect_int2(ParseTreeInt2, _),
        mercury_output_parse_tree_int2(Info, ParseTreeInt2, !IO)
    ;
        IndirectIntSpec = indirect_int3(ParseTreeInt3, _),
        mercury_output_parse_tree_int3(Info, ParseTreeInt3, !IO)
    ).

mercury_output_int_for_opt_spec(Info, ForOptIntSpec, !IO) :-
    (
        ForOptIntSpec = for_opt_int0(ParseTreeInt0, _),
        mercury_output_parse_tree_int0(Info, ParseTreeInt0, !IO)
    ;
        ForOptIntSpec = for_opt_int1(ParseTreeInt1, _),
        mercury_output_parse_tree_int1(Info, ParseTreeInt1, !IO)
    ;
        ForOptIntSpec = for_opt_int2(ParseTreeInt2, _),
        mercury_output_parse_tree_int2(Info, ParseTreeInt2, !IO)
    ).

%---------------------------------------------------------------------------%

mercury_output_parse_tree_int(Info, ParseTree, !IO) :-
    ParseTree = parse_tree_int(ModuleName, _IntFileKind, _ModuleContext,
        MaybeVersionNumbers, IntIncls, ImpIncls, IntAvails, ImpAvails,
        IntFIMs, ImpFIMs, IntItems, ImpItems),
    mercury_output_module_decl("module", ModuleName, !IO),
    mercury_output_maybe_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !IO),
    ( if
        IntIncls = [],
        IntAvails = [],
        IntFIMs = [],
        IntItems = []
    then
        true
    else
        IntItemBlock = item_block(ModuleName, ms_interface,
            IntIncls, IntAvails, IntFIMs, IntItems),
        mercury_output_raw_item_block(Info, IntItemBlock, !IO)
    ),
    ( if
        ImpIncls = [],
        ImpAvails = [],
        ImpFIMs = [],
        ImpItems = []
    then
        true
    else
        ImpItemBlock = item_block(ModuleName, ms_implementation,
            ImpIncls, ImpAvails, ImpFIMs, ImpItems),
        mercury_output_raw_item_block(Info, ImpItemBlock, !IO)
    ).

mercury_output_parse_tree_int0(Info, ParseTreeInt0, !IO) :-
    ParseTreeInt0 = parse_tree_int0(ModuleName, _ModuleContext,
        MaybeVersionNumbers, IntInclMap, ImpInclMap, _InclMap,
        IntImportMap, IntUseMap, ImpImportMap, ImpUseMap, _ImportUseMap,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntDeclPragmas, IntPromises,
        ImpTypeDefnMap, ImpInstDefnMap, ImpModeDefnMap,
        ImpTypeClasses, ImpInstances, ImpPredDecls, ImpModeDecls,
        ImpForeignEnumMap, ImpDeclPragmas, ImpPromises),
    mercury_output_module_decl("module", ModuleName, !IO),
    mercury_output_maybe_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !IO),

    mercury_output_section_marker(ms_interface, !IO),
    list.foldl(mercury_output_module_decl("include_module"),
        map.sorted_keys(IntInclMap), !IO),
    list.foldl(mercury_output_module_decl("import_module"),
        map.sorted_keys(IntImportMap), !IO),
    list.foldl(mercury_output_module_decl("use_module"),
        map.sorted_keys(IntUseMap), !IO),
    set.foldl(mercury_output_fim_spec, IntFIMSpecs, !IO),
    map.foldl_values(mercury_output_type_ctor_all_defns(Info),
        IntTypeDefnMap, !IO),
    map.foldl_values(mercury_output_inst_ctor_all_defns(Info),
        IntInstDefnMap, !IO),
    map.foldl_values(mercury_output_mode_ctor_all_defns(Info),
        IntModeDefnMap, !IO),
    list.foldl(mercury_output_item_typeclass(Info),
        list.sort(IntTypeClasses), !IO),
    list.foldl(mercury_output_item_instance(Info),
        list.sort(IntInstances), !IO),
    order_pred_and_mode_decls(IntPredDecls, IntModeDecls, IntPredOrModeDecls),
    mercury_output_pred_or_mode_decls(Info, IntPredOrModeDecls, !IO),
    map.foldl_values(mercury_output_foreign_enums(Info),
        IntForeignEnumMap, !IO),
    list.foldl(mercury_output_item_decl_pragma(Info),
        list.sort(IntDeclPragmas), !IO),
    list.foldl(mercury_output_item_promise(Info),
        list.sort(IntPromises), !IO),

    ( if
        map.is_empty(ImpInclMap),
        map.is_empty(ImpImportMap),
        map.is_empty(ImpUseMap),
        set.is_empty(ImpFIMSpecs),
        map.is_empty(ImpTypeDefnMap),
        map.is_empty(ImpInstDefnMap),
        map.is_empty(ImpModeDefnMap),
        ImpTypeClasses = [],
        ImpInstances = [],
        ImpPredDecls = [],
        ImpModeDecls = [],
        map.is_empty(ImpForeignEnumMap),
        ImpDeclPragmas = [],
        ImpPromises = []
    then
        true
    else
        mercury_output_section_marker(ms_implementation, !IO),
        list.foldl(mercury_output_module_decl("include_module"),
            map.sorted_keys(ImpInclMap), !IO),
        list.foldl(mercury_output_module_decl("import_module"),
            map.sorted_keys(ImpImportMap), !IO),
        list.foldl(mercury_output_module_decl("use_module"),
            map.sorted_keys(ImpUseMap), !IO),
        set.foldl(mercury_output_fim_spec, ImpFIMSpecs, !IO),
        map.foldl_values(mercury_output_type_ctor_all_defns(Info),
            ImpTypeDefnMap, !IO),
        map.foldl_values(mercury_output_inst_ctor_all_defns(Info),
            ImpInstDefnMap, !IO),
        map.foldl_values(mercury_output_mode_ctor_all_defns(Info),
            ImpModeDefnMap, !IO),
        list.foldl(mercury_output_item_typeclass(Info),
            list.sort(ImpTypeClasses), !IO),
        list.foldl(mercury_output_item_instance(Info),
            list.sort(ImpInstances), !IO),
        order_pred_and_mode_decls(ImpPredDecls, ImpModeDecls,
            ImpPredOrModeDecls),
        mercury_output_pred_or_mode_decls(Info, ImpPredOrModeDecls, !IO),
        map.foldl_values(mercury_output_foreign_enums(Info),
            ImpForeignEnumMap, !IO),
        list.foldl(mercury_output_item_decl_pragma(Info),
            list.sort(ImpDeclPragmas), !IO),
        list.foldl(mercury_output_item_promise(Info),
            list.sort(ImpPromises), !IO)
    ).

mercury_output_parse_tree_int1(Info, ParseTreeInt1, !IO) :-
    ParseTreeInt1 = parse_tree_int1(ModuleName, _ModuleContext,
        MaybeVersionNumbers, IntInclMap, ImpInclMap, _InclMap,
        IntUseMap, ImpUseMap, _ImportUseMap, IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntPredDecls, IntModeDecls,
        IntForeignEnumMap, IntDeclPragmas, IntPromises, IntTypeRepnMap,
        ImpTypeDefnMap, ImpForeignEnumMap, ImpTypeClasses),
    mercury_output_module_decl("module", ModuleName, !IO),
    mercury_output_maybe_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !IO),

    mercury_output_section_marker(ms_interface, !IO),
    list.foldl(mercury_output_module_decl("include_module"),
        map.sorted_keys(IntInclMap), !IO),
    list.foldl(mercury_output_module_decl("use_module"),
        map.sorted_keys(IntUseMap), !IO),
    set.foldl(mercury_output_fim_spec, IntFIMSpecs, !IO),
    map.foldl_values(mercury_output_type_ctor_all_defns(Info),
        IntTypeDefnMap, !IO),
    map.foldl_values(mercury_output_inst_ctor_all_defns(Info),
        IntInstDefnMap, !IO),
    map.foldl_values(mercury_output_mode_ctor_all_defns(Info),
        IntModeDefnMap, !IO),
    list.foldl(mercury_output_item_typeclass(Info),
        list.sort(IntTypeClasses), !IO),
    list.foldl(mercury_output_item_instance(Info),
        list.sort(IntInstances), !IO),
    order_pred_and_mode_decls(IntPredDecls, IntModeDecls, IntPredOrModeDecls),
    mercury_output_pred_or_mode_decls(Info, IntPredOrModeDecls, !IO),
    map.foldl_values(mercury_output_foreign_enums(Info),
        IntForeignEnumMap, !IO),
    list.foldl(mercury_output_item_decl_pragma(Info),
        list.sort(IntDeclPragmas), !IO),
    list.foldl(mercury_output_item_promise(Info),
        list.sort(IntPromises), !IO),
    map.foldl_values(mercury_output_item_type_repn(Info), IntTypeRepnMap, !IO),

    ( if
        map.is_empty(ImpInclMap),
        map.is_empty(ImpUseMap),
        set.is_empty(ImpFIMSpecs),
        map.is_empty(ImpTypeDefnMap),
        map.is_empty(ImpForeignEnumMap),
        ImpTypeClasses = []
    then
        true
    else
        mercury_output_section_marker(ms_implementation, !IO),
        list.foldl(mercury_output_module_decl("include_module"),
            map.sorted_keys(ImpInclMap), !IO),
        list.foldl(mercury_output_module_decl("use_module"),
            map.sorted_keys(ImpUseMap), !IO),
        set.foldl(mercury_output_fim_spec, ImpFIMSpecs, !IO),
        map.foldl_values(mercury_output_type_ctor_all_defns(Info),
            ImpTypeDefnMap, !IO),
        map.foldl_values(mercury_output_foreign_enums(Info),
            ImpForeignEnumMap, !IO),
        list.foldl(mercury_output_item_typeclass(Info),
            list.sort(ImpTypeClasses), !IO)
    ).

mercury_output_parse_tree_int2(Info, ParseTreeInt2, !IO) :-
    ParseTreeInt2 = parse_tree_int2(ModuleName, _ModuleContext,
        MaybeVersionNumbers, IntInclMap, _InclMap, IntUseMap, _ImportUseMap,
        IntFIMSpecs, ImpFIMSpecs,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap,
        ImpTypeDefnMap),
    mercury_output_module_decl("module", ModuleName, !IO),
    mercury_output_maybe_module_version_numbers(ModuleName,
        MaybeVersionNumbers, !IO),

    mercury_output_section_marker(ms_interface, !IO),
    list.foldl(mercury_output_module_decl("include_module"),
        map.sorted_keys(IntInclMap), !IO),
    list.foldl(mercury_output_module_decl("use_module"),
        map.sorted_keys(IntUseMap), !IO),
    set.foldl(mercury_output_fim_spec, IntFIMSpecs, !IO),
    map.foldl_values(mercury_output_type_ctor_all_defns(Info),
        IntTypeDefnMap, !IO),
    map.foldl_values(mercury_output_inst_ctor_all_defns(Info),
        IntInstDefnMap, !IO),
    map.foldl_values(mercury_output_mode_ctor_all_defns(Info),
        IntModeDefnMap, !IO),
    list.foldl(mercury_output_item_typeclass(Info),
        list.sort(IntTypeClasses), !IO),
    list.foldl(mercury_output_item_instance(Info),
        list.sort(IntInstances), !IO),
    map.foldl_values(mercury_output_item_type_repn(Info), IntTypeRepnMap, !IO),

    ( if
        set.is_empty(ImpFIMSpecs),
        map.is_empty(ImpTypeDefnMap)
    then
        true
    else
        mercury_output_section_marker(ms_implementation, !IO),
        set.foldl(mercury_output_fim_spec, ImpFIMSpecs, !IO),
        map.foldl_values(mercury_output_type_ctor_all_defns(Info),
            ImpTypeDefnMap, !IO)
    ).

mercury_output_parse_tree_int3(Info, ParseTreeInt3, !IO) :-
    ParseTreeInt3 = parse_tree_int3(ModuleName, _ModuleContext,
        IntInclMap, _InclMap, IntImportMap, _ImportUseMap,
        IntTypeDefnMap, IntInstDefnMap, IntModeDefnMap,
        IntTypeClasses, IntInstances, IntTypeRepnMap),
    mercury_output_module_decl("module", ModuleName, !IO),
    mercury_output_section_marker(ms_interface, !IO),
    list.foldl(mercury_output_module_decl("include_module"),
        map.sorted_keys(IntInclMap), !IO),
    list.foldl(mercury_output_module_decl("import_module"),
        map.sorted_keys(IntImportMap), !IO),
    map.foldl_values(mercury_output_type_ctor_all_defns(Info),
        IntTypeDefnMap, !IO),
    map.foldl_values(mercury_output_inst_ctor_all_defns(Info),
        IntInstDefnMap, !IO),
    map.foldl_values(mercury_output_mode_ctor_all_defns(Info),
        IntModeDefnMap, !IO),
    list.foldl(mercury_output_item_typeclass(Info),
        list.sort(IntTypeClasses), !IO),
    list.foldl(mercury_output_item_instance(Info),
        list.sort(IntInstances), !IO),
    map.foldl_values(mercury_output_item_type_repn(Info), IntTypeRepnMap, !IO).

%---------------------------------------------------------------------------%

mercury_output_parse_tree_opt(Info, ParseTree, !IO) :-
    ParseTree = parse_tree_opt(ModuleName, _OptFileKind, _Context,
        Use, FIMs, Items),
    io.write_string("% generic optimization file\n", !IO),
    mercury_output_module_decl("module", ModuleName, !IO),
    list.foldl(mercury_output_item_use(Info), Use, !IO),
    list.foldl(mercury_output_item_foreign_import_module, FIMs, !IO),
    mercury_output_items(Info, Items, !IO).

mercury_output_parse_tree_plain_opt(Info, ParseTree, !IO) :-
    ParseTree = parse_tree_plain_opt(ModuleName, _Context,
        UseMap, FIMSpecs, TypeDefns, ForeignEnums,
        InstDefns, ModeDefns, TypeClasses, Instances,
        PredDecls, ModeDecls, Clauses, ForeignProcs, Promises,
        PredMarkers, TypeSpecs, UnusedArgs, Terms, Term2s,
        Exceptions, Trailings, MMTablings, Sharings, Reuses),
    Lang = get_output_lang(Info),
    io.write_string("% .opt file\n", !IO),
    mercury_output_module_decl("module", ModuleName, !IO),
    list.foldl(mercury_output_module_decl("use_module"),
        map.keys(UseMap), !IO),
    list.foldl(mercury_output_item_type_defn(Info), TypeDefns, !IO),
    list.foldl(mercury_output_item_foreign_enum(Info), ForeignEnums, !IO),
    list.foldl(mercury_output_item_inst_defn(Info), InstDefns, !IO),
    list.foldl(mercury_output_item_mode_defn(Info), ModeDefns, !IO),
    list.foldl(mercury_output_item_typeclass(Info), TypeClasses, !IO),
    list.foldl(mercury_output_item_instance(Info), Instances, !IO),
    % XXX FIMSpecs should be output just after UsedModuleNames, but
    % the existing code whose output we want to compare the output
    % of this code to prints them in this position.
    set.foldl(mercury_output_fim_spec, FIMSpecs, !IO),
    list.foldl(mercury_output_item_pred_decl(Info), PredDecls, !IO),
    list.foldl(mercury_output_item_mode_decl(Info), ModeDecls, !IO),
    list.foldl(mercury_output_item_pred_marker,
        list.map(project_pragma_type, PredMarkers), !IO),
    list.foldl(mercury_output_pragma_type_spec(print_name_only, Lang),
        list.map(project_pragma_type, TypeSpecs), !IO),
    list.foldl(mercury_output_item_clause(Info), Clauses, !IO),
    list.foldl(mercury_output_pragma_foreign_proc(Lang),
        list.map(project_pragma_type, ForeignProcs), !IO),
    list.foldl(mercury_output_item_promise(Info), Promises, !IO),

    list.foldl(mercury_output_pragma_unused_args,
        list.map(project_pragma_type, UnusedArgs), !IO),
    list.foldl(write_pragma_termination_info(Lang),
        list.map(project_pragma_type, Terms), !IO),
    list.foldl(write_pragma_termination2_info(Lang),
        list.map(project_pragma_type, Term2s), !IO),
    list.foldl(mercury_output_pragma_exceptions,
        list.map(project_pragma_type, Exceptions), !IO),
    list.foldl(mercury_output_pragma_trailing_info,
        list.map(project_pragma_type, Trailings), !IO),
    list.foldl(mercury_output_pragma_mm_tabling_info,
        list.map(project_pragma_type, MMTablings), !IO),
    list.foldl(mercury_output_pragma_mm_tabling_info,
        list.map(project_pragma_type, MMTablings), !IO),
    list.foldl(write_pragma_structure_sharing_info(Lang),
        list.map(project_pragma_type, Sharings), !IO),
    list.foldl(write_pragma_structure_reuse_info(Lang),
        list.map(project_pragma_type, Reuses), !IO),
    io.nl(!IO).

mercury_output_parse_tree_trans_opt(Info, ParseTree, !IO) :-
    ParseTree = parse_tree_trans_opt(ModuleName, _Context,
        Terms, Term2s, Exceptions, Trailings, MMTablings, Sharings, Reuses),
    Lang = get_output_lang(Info),
    io.write_string("% .trans_opt file\n", !IO),
    mercury_output_module_decl("module", ModuleName, !IO),
    list.foldl(write_pragma_termination_info(Lang),
        list.map(project_pragma_type, Terms), !IO),
    list.foldl(write_pragma_termination2_info(Lang),
        list.map(project_pragma_type, Term2s), !IO),
    list.foldl(mercury_output_pragma_exceptions,
        list.map(project_pragma_type, Exceptions), !IO),
    list.foldl(mercury_output_pragma_trailing_info,
        list.map(project_pragma_type, Trailings), !IO),
    list.foldl(mercury_output_pragma_mm_tabling_info,
        list.map(project_pragma_type, MMTablings), !IO),
    list.foldl(mercury_output_pragma_mm_tabling_info,
        list.map(project_pragma_type, MMTablings), !IO),
    list.foldl(write_pragma_structure_sharing_info(Lang),
        list.map(project_pragma_type, Sharings), !IO),
    list.foldl(write_pragma_structure_reuse_info(Lang),
        list.map(project_pragma_type, Reuses), !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

mercury_output_raw_compilation_unit(Info, CompUnit, !IO) :-
    CompUnit = raw_compilation_unit(ModuleName, _Context, ItemBlocks),
    io.write_string("% raw compilation unit\n", !IO),
    mercury_output_module_decl("module", ModuleName, !IO),
    mercury_output_raw_item_blocks(Info, ItemBlocks, !IO).

mercury_output_aug_compilation_unit(Info, AugCompUnit, !IO) :-
    AugCompUnit = aug_compilation_unit(ModuleName, _Context,
        ModuleVersionNumbers, ParseTreeModuleSrc,
        AncestorIntSpecs, DirectIntSpecs, IndirectIntSpecs,
        PlainOptSpecs, TransOptSpecs, IntForOptSpecs),
    io.write_string("% augmented compilation unit\n", !IO),
    mercury_output_module_decl("module", ModuleName, !IO),
    io.write_string("% The module version numbers.\n", !IO),
    map.foldl(mercury_output_module_version_numbers,
        ModuleVersionNumbers, !IO),
    io.write_string("% The main module.\n", !IO),
    mercury_output_parse_tree_module_src(Info, ParseTreeModuleSrc, !IO),
    io.write_string("% The ancestor interfaces.\n", !IO),
    map.foldl_values(mercury_output_ancestor_int_spec(Info),
        AncestorIntSpecs, !IO),
    io.write_string("% The directly imported interfaces.\n", !IO),
    map.foldl_values(mercury_output_direct_int_spec(Info),
        DirectIntSpecs, !IO),
    io.write_string("% The indirectly imported interfaces.\n", !IO),
    map.foldl_values(mercury_output_indirect_int_spec(Info),
        IndirectIntSpecs, !IO),
    io.write_string("% The plain optimization files.\n", !IO),
    map.foldl_values(mercury_output_parse_tree_plain_opt(Info),
        PlainOptSpecs, !IO),
    io.write_string("% The transitive optimization files.\n", !IO),
    map.foldl_values(mercury_output_parse_tree_trans_opt(Info),
        TransOptSpecs, !IO),
    io.write_string("% The interface item blocks for optimization.\n", !IO),
    map.foldl_values(mercury_output_int_for_opt_spec(Info),
        IntForOptSpecs, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_maybe_module_version_numbers(module_name::in,
    maybe_version_numbers::in, io::di, io::uo) is det.

mercury_output_maybe_module_version_numbers(ModuleName, MaybeVersionNumbers,
        !IO) :-
    (
        MaybeVersionNumbers = no_version_numbers
    ;
        MaybeVersionNumbers = version_numbers(VersionNumbers),
        mercury_output_module_version_numbers(ModuleName, VersionNumbers, !IO)
    ).

:- pred mercury_output_module_version_numbers(module_name::in,
    version_numbers::in, io::di, io::uo) is det.

mercury_output_module_version_numbers(ModuleName, VersionNumbers, !IO) :-
    io.write_string(":- version_numbers(", !IO),
    io.write_int(version_numbers_version_number, !IO),
    io.write_string(", ", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io.write_string(",\n", !IO),
    recompilation.version.write_version_numbers(VersionNumbers, !IO),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_module_components(merc_out_info::in,
    maybe(module_section)::in, list(module_component)::in,
    io::di, io::uo) is det.

mercury_output_module_components(_, _, [], !IO).
mercury_output_module_components(Info, MaybePrevSectionKind,
        [Component | Components], !IO) :-
    (
        Component = mc_section(_, SectionKind, _SectionContext,
            InclsCord, AvailsCord, FIMsCord, ItemsCord),
        mercury_output_section_marker(SectionKind, !IO),
        list.foldl(mercury_output_item_include(Info),
            cord.list(InclsCord), !IO),
        list.foldl(mercury_output_item_avail(Info),
            cord.list(AvailsCord), !IO),
        list.foldl(mercury_output_item_foreign_import_module,
            cord.list(FIMsCord), !IO),
        mercury_output_items(Info, cord.list(ItemsCord), !IO),
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
                mercury_output_section_marker(SectionKind, !IO)
            )
        ;
            Lang = output_debug,
            mercury_output_section_marker(SectionKind, !IO),
            (
                SectionKind = ms_interface,
                io.write_string("% nested submodule in interface\n", !IO)
            ;
                SectionKind = ms_implementation,
                io.write_string("% nested submodule in implementation\n", !IO)
            )
        ),
        mercury_output_parse_tree_src(Info, SubParseTree, !IO),
        MaybeCurSectionKind = MaybePrevSectionKind
    ),
    mercury_output_module_components(Info, MaybeCurSectionKind,
        Components, !IO).

%---------------------------------------------------------------------------%

mercury_output_raw_item_blocks(_, [], !IO).
mercury_output_raw_item_blocks(Info, [RawItemBlock | RawItemBlocks], !IO) :-
    mercury_output_raw_item_block(Info, RawItemBlock, !IO),
    mercury_output_raw_item_blocks(Info, RawItemBlocks, !IO).

mercury_output_raw_item_block(Info, RawItemBlock, !IO) :-
    RawItemBlock = item_block(_, SectionKind, Incls, Avails, FIMs, Items),
    mercury_output_section_marker(SectionKind, !IO),
    list.foldl(mercury_output_item_include(Info), Incls, !IO),
    list.foldl(mercury_output_item_avail(Info), Avails, !IO),
    list.foldl(mercury_output_item_foreign_import_module, FIMs, !IO),
    mercury_output_items(Info, Items, !IO).

%---------------------------------------------------------------------------%

mercury_output_src_item_blocks(_, [], !IO).
mercury_output_src_item_blocks(Info, [SrcItemBlock | SrcItemBlocks], !IO) :-
    mercury_output_src_item_block(Info, SrcItemBlock, !IO),
    mercury_output_src_item_blocks(Info, SrcItemBlocks, !IO).

mercury_output_src_item_block(Info, SrcItemBlock, !IO) :-
    SrcItemBlock = item_block(_, SrcSectionKind, Incls, Avails, FIMs, Items),
    mercury_output_src_section_marker(SrcSectionKind, !IO),
    list.foldl(mercury_output_item_include(Info), Incls, !IO),
    list.foldl(mercury_output_item_avail(Info), Avails, !IO),
    list.foldl(mercury_output_item_foreign_import_module, FIMs, !IO),
    mercury_output_items(Info, Items, !IO).

mercury_output_int_item_blocks(_, [], !IO).
mercury_output_int_item_blocks(Info, [IntItemBlock | IntItemBlocks], !IO) :-
    mercury_output_int_item_block(Info, IntItemBlock, !IO),
    mercury_output_int_item_blocks(Info, IntItemBlocks, !IO).

mercury_output_int_item_block(Info, IntItemBlock, !IO) :-
    IntItemBlock = item_block(_, IntSectionKind, Incls, Avails, FIMs, Items),
    mercury_output_int_section_marker(IntSectionKind, !IO),
    list.foldl(mercury_output_item_include(Info), Incls, !IO),
    list.foldl(mercury_output_item_avail(Info), Avails, !IO),
    list.foldl(mercury_output_item_foreign_import_module, FIMs, !IO),
    mercury_output_items(Info, Items, !IO).

mercury_output_opt_item_blocks(_, [], !IO).
mercury_output_opt_item_blocks(Info, [OptItemBlock | OptItemBlocks], !IO) :-
    mercury_output_opt_item_block(Info, OptItemBlock, !IO),
    mercury_output_opt_item_blocks(Info, OptItemBlocks, !IO).

mercury_output_opt_item_block(Info, OptItemBlock, !IO) :-
    OptItemBlock = item_block(_, OptSectionKind, Incls, Avails, FIMs, Items),
    mercury_output_opt_section_marker(OptSectionKind, !IO),
    expect(unify(Incls, []), $pred, "Incls != []"),
    list.foldl(mercury_output_item_avail(Info), Avails, !IO),
    list.foldl(mercury_output_item_foreign_import_module, FIMs, !IO),
    mercury_output_items(Info, Items, !IO).

mercury_output_int_for_opt_item_blocks(_, [], !IO).
mercury_output_int_for_opt_item_blocks(Info,
        [IntForOptItemBlock | IntForOptItemBlocks], !IO) :-
    mercury_output_int_for_opt_item_block(Info, IntForOptItemBlock, !IO),
    mercury_output_int_for_opt_item_blocks(Info, IntForOptItemBlocks, !IO).

mercury_output_int_for_opt_item_block(Info, IntForOptItemBlock, !IO) :-
    IntForOptItemBlock = item_block(_, IntForOptSectionKind,
        Incls, Avails, FIMs, Items),
    mercury_output_int_for_opt_section_marker(IntForOptSectionKind, !IO),
    list.foldl(mercury_output_item_include(Info), Incls, !IO),
    list.foldl(mercury_output_item_avail(Info), Avails, !IO),
    list.foldl(mercury_output_item_foreign_import_module, FIMs, !IO),
    mercury_output_items(Info, Items, !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_section_marker(module_section::in, io::di, io::uo)
    is det.

mercury_output_section_marker(Section, !IO) :-
    (
        Section = ms_interface,
        io.write_string(":- interface.\n", !IO)
    ;
        Section = ms_implementation,
        io.write_string(":- implementation.\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_src_section_marker(src_module_section::in,
    io::di, io::uo) is det.

mercury_output_src_section_marker(SrcSection, !IO) :-
    (
        SrcSection = sms_interface,
        io.write_string(":- interface.\n", !IO)
    ;
        SrcSection = sms_implementation,
        io.write_string(":- implementation.\n", !IO)
    ;
        SrcSection = sms_impl_but_exported_to_submodules,
        io.write_string(":- ams_impl_but_exported_to_submodules.\n", !IO)
    ).

:- pred mercury_output_int_section_marker(int_module_section::in,
    io::di, io::uo) is det.

mercury_output_int_section_marker(IntSection, !IO) :-
    (
        IntSection = ims_imported_or_used(ModuleName, IntFileKind,
            ImportLocn, ImportedOrUsed),
        (
            ImportedOrUsed = iou_imported,
            io.write_string(":- ims_imported", !IO)
        ;
            ImportedOrUsed = iou_used,
            io.write_string(":- ims_used", !IO)
        ;
            ImportedOrUsed = iou_used_and_imported,
            io.write_string(":- ims_used_and_imported", !IO)
        ),
        io.write_string("(", !IO),
        io.write_string(sym_name_to_string(ModuleName), !IO),
        io.write_string(int_file_kind_to_extension(IntFileKind), !IO),
        io.write_string(", ", !IO),
        io.write(ImportLocn, !IO),
        io.write_string(").\n", !IO)
    ;
        IntSection = ims_abstract_imported(ModuleName, IntFileKind),
        io.write_string(":- ims_abstract_imported(", !IO),
        io.write_string(sym_name_to_string(ModuleName), !IO),
        io.write_string(int_file_kind_to_extension(IntFileKind), !IO),
        io.write_string(").\n", !IO)
    ).

:- pred mercury_output_opt_section_marker(opt_module_section::in,
    io::di, io::uo) is det.

mercury_output_opt_section_marker(OptSection, !IO) :-
    (
        OptSection = oms_opt_imported(ModuleName, OptFileKind),
        io.write_string(":- oms_opt_imported(", !IO),
        io.write_string(sym_name_to_string(ModuleName), !IO),
        io.write_string(opt_file_kind_to_extension(OptFileKind), !IO),
        io.write_string(").\n", !IO)
    ).

:- pred mercury_output_int_for_opt_section_marker(
    int_for_opt_module_section::in, io::di, io::uo) is det.

mercury_output_int_for_opt_section_marker(IntForOptSection, !IO) :-
    (
        IntForOptSection = ioms_opt_imported(ModuleName, IntFileKind),
        io.write_string(":- ioms_opt_imported(", !IO),
        io.write_string(sym_name_to_string(ModuleName), !IO),
        io.write_string(int_file_kind_to_extension(IntFileKind), !IO),
        io.write_string(").\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_include(merc_out_info::in,
    item_include::in, io::di, io::uo) is det.

mercury_output_item_include(Info, ItemInclude, !IO) :-
    ItemInclude = item_include(ModuleName, Context, _SeqNum),
    Decl = "include_module",
    maybe_output_line_number(Info, Context, !IO),
    mercury_output_module_decl(Decl, ModuleName, !IO).

:- pred mercury_output_item_avail(merc_out_info::in,
    item_avail::in, io::di, io::uo) is det.

mercury_output_item_avail(Info, Avail, !IO) :-
    (
        Avail = avail_import(avail_import_info(ModuleName, Context, _SeqNum)),
        Decl = "import_module"
    ;
        Avail = avail_use(avail_use_info(ModuleName, Context, _SeqNum)),
        Decl = "use_module"
    ),
    maybe_output_line_number(Info, Context, !IO),
    mercury_output_module_decl(Decl, ModuleName, !IO).

:- pred mercury_output_item_use(merc_out_info::in,
    avail_use_info::in, io::di, io::uo) is det.

mercury_output_item_use(Info, Use, !IO) :-
    Use = avail_use_info(ModuleName, Context, _SeqNum),
    Decl = "use_module",
    maybe_output_line_number(Info, Context, !IO),
    mercury_output_module_decl(Decl, ModuleName, !IO).

:- pred mercury_output_module_decl(string::in, module_name::in,
    io::di, io::uo) is det.

mercury_output_module_decl(Decl, ModuleName, !IO) :-
    io.write_string(":- ", !IO),
    io.write_string(Decl, !IO),
    io.write_string(" ", !IO),
    mercury_output_bracketed_sym_name(ModuleName, !IO),
    io.write_string(".\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_items(merc_out_info::in, list(item)::in,
    io::di, io::uo) is det.

mercury_output_items(_, [], !IO).
mercury_output_items(Info, [Item | Items], !IO) :-
    mercury_output_item(Info, Item, !IO),
    mercury_output_items(Info, Items, !IO).

mercury_output_item(Info, Item, !IO) :-
    (
        Item = item_clause(ItemClause),
        mercury_output_item_clause(Info, ItemClause, !IO)
    ;
        Item = item_type_defn(ItemTypeDefn),
        mercury_output_item_type_defn(Info, ItemTypeDefn, !IO)
    ;
        Item = item_inst_defn(ItemInstDefn),
        mercury_output_item_inst_defn(Info, ItemInstDefn, !IO)
    ;
        Item = item_mode_defn(ItemModeDefn),
        mercury_output_item_mode_defn(Info, ItemModeDefn, !IO)
    ;
        Item = item_pred_decl(ItemPredDecl),
        mercury_output_item_pred_decl(Info, ItemPredDecl, !IO)
    ;
        Item = item_mode_decl(ItemModeDecl),
        mercury_output_item_mode_decl(Info, ItemModeDecl, !IO)
    ;
        Item = item_foreign_enum(ItemForeignEnum),
        mercury_output_item_foreign_enum(Info, ItemForeignEnum, !IO)
    ;
        Item = item_foreign_export_enum(ItemForeignExportEnum),
        mercury_output_item_foreign_export_enum(Info, ItemForeignExportEnum,
            !IO)
    ;
        Item = item_decl_pragma(ItemDeclPragma),
        mercury_output_item_decl_pragma(Info, ItemDeclPragma, !IO)
    ;
        Item = item_impl_pragma(ItemImplPragma),
        mercury_output_item_impl_pragma(Info, ItemImplPragma, !IO)
    ;
        Item = item_generated_pragma(ItemGenPragma),
        mercury_output_item_generated_pragma(Info, ItemGenPragma, !IO)
    ;
        Item = item_promise(ItemPromise),
        mercury_output_item_promise(Info, ItemPromise, !IO)
    ;
        Item = item_typeclass(ItemTypeClass),
        mercury_output_item_typeclass(Info, ItemTypeClass, !IO)
    ;
        Item = item_instance(ItemInstance),
        mercury_output_item_instance(Info, ItemInstance, !IO)
    ;
        Item = item_initialise(ItemInitialise),
        mercury_output_item_initialise(Info, ItemInitialise, !IO)
    ;
        Item = item_finalise(ItemFinalise),
        mercury_output_item_finalise(Info, ItemFinalise, !IO)
    ;
        Item = item_mutable(ItemMutable),
        mercury_output_item_mutable(Info, ItemMutable, !IO)
    ;
        Item = item_type_repn(ItemTypeRepn),
        mercury_output_item_type_repn(Info, ItemTypeRepn, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_pred_or_mode_decls(merc_out_info::in,
    list(pred_or_mode_decl_item)::in, io::di, io::uo) is det.

mercury_output_pred_or_mode_decls(_, [], !IO).
mercury_output_pred_or_mode_decls(Info, [Item | Items], !IO) :-
    mercury_output_pred_or_mode_decl(Info, Item, !IO),
    mercury_output_pred_or_mode_decls(Info, Items, !IO).

:- pred mercury_output_pred_or_mode_decl(merc_out_info::in,
    pred_or_mode_decl_item::in, io::di, io::uo) is det.

mercury_output_pred_or_mode_decl(Info, Item, !IO) :-
    (
        Item = pomd_pred(ItemPredDecl),
        mercury_output_item_pred_decl(Info, ItemPredDecl, !IO)
    ;
        Item = pomd_mode(ItemModeDecl),
        mercury_output_item_mode_decl(Info, ItemModeDecl, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_type_ctor_all_defns(merc_out_info::in,
    type_ctor_all_defns::in, io::di, io::uo) is det.

mercury_output_type_ctor_all_defns(Info, TypeCtorAllDefns, !IO) :-
    TypeCtorAllDefns = type_ctor_all_defns(SolverAbs, SolverNonAbs,
        StdAbs, StdEqv, StdDu, CJCsE),
    CJCsE = c_java_csharp_erlang(ForeignC, ForeignJava,
        ForeignCsharp, ForeignErlang),
    AbsToGen =
        ( func(Item) = Item ^ td_ctor_defn :=
            parse_tree_abstract_type(Item ^ td_ctor_defn)
        ),
    SolverToGen =
        ( func(Item) = Item ^ td_ctor_defn :=
            parse_tree_solver_type(Item ^ td_ctor_defn)
        ),
    EqvToGen =
        ( func(Item) = Item ^ td_ctor_defn :=
            parse_tree_eqv_type(Item ^ td_ctor_defn)
        ),
    DuToGen =
        ( func(Item) = Item ^ td_ctor_defn :=
            parse_tree_du_type(Item ^ td_ctor_defn)
        ),
    ForeignToGen =
        ( func(Item) = Item ^ td_ctor_defn :=
            parse_tree_foreign_type(Item ^ td_ctor_defn)
        ),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(AbsToGen, SolverAbs), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(SolverToGen, SolverNonAbs), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(AbsToGen, StdAbs), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(EqvToGen, StdEqv), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(DuToGen, StdDu), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(ForeignToGen, ForeignC), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(ForeignToGen, ForeignJava), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(ForeignToGen, ForeignCsharp), !IO),
    list.foldl(mercury_output_item_type_defn(Info),
        list.map(ForeignToGen, ForeignErlang), !IO).

:- pred mercury_output_item_type_defn(merc_out_info::in,
    item_type_defn_info::in, io::di, io::uo) is det.

mercury_output_item_type_defn(Info, ItemTypeDefn, !IO) :-
    % ZZZ TypeVarSet
    ItemTypeDefn = item_type_defn_info(SymName0, TypeParams, TypeDefn,
        TypeVarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, SymName0, SymName),
    maybe_output_line_number(Info, Context, !IO),
    Args = list.map((func(V) = term.variable(V, Context)), TypeParams),
    construct_qualified_term_with_context(SymName, Args, Context, TypeTerm),
    (
        TypeDefn = parse_tree_abstract_type(DetailsAbstract),
        (
            ( DetailsAbstract = abstract_type_general
            ; DetailsAbstract = abstract_dummy_type
            ; DetailsAbstract = abstract_notag_type
            ; DetailsAbstract = abstract_type_fits_in_n_bits(_)
            ),
            IsSolverType = non_solver_type
        ;
            DetailsAbstract = abstract_solver_type,
            IsSolverType = solver_type
        ),
        mercury_output_begin_type_decl(IsSolverType, !IO),
        mercury_output_term_nq(TypeVarSet, print_name_only,
            next_to_graphic_token, TypeTerm, !IO),
        (
            DetailsAbstract = abstract_type_fits_in_n_bits(NumBits),
            % XXX TYPE_REPN Instead of adding this information to the
            % generated type definition, generate and write out
            % a separate type_repn item instead.
            mercury_output_where_abstract_enum_type(NumBits, !IO)
        ;
            ( DetailsAbstract = abstract_dummy_type
            ; DetailsAbstract = abstract_notag_type
            )
            % XXX TYPE_REPN The same concern applies here, but these
            % kinds of abstract types are not yet generated anywhere,
            % so we don't have anything to do for them.
        ;
            ( DetailsAbstract = abstract_type_general
            ; DetailsAbstract = abstract_solver_type
            )
        ),
        io.write_string(".\n", !IO)
    ;
        TypeDefn = parse_tree_eqv_type(DetailsEqv),
        DetailsEqv = type_details_eqv(EqvType),
        mercury_output_begin_type_decl(non_solver_type, !IO),
        mercury_output_term(TypeVarSet, print_name_only, TypeTerm, !IO),
        io.write_string(" == ", !IO),
        mercury_output_type(TypeVarSet, print_name_only, EqvType, !IO),
        io.write_string(".\n", !IO)
    ;
        TypeDefn = parse_tree_du_type(DetailsDu),
        DetailsDu = type_details_du(OoMCtors, MaybeCanonical, MaybeDirectArgs),
        mercury_output_begin_type_decl(non_solver_type, !IO),
        mercury_output_term(TypeVarSet, print_name_only, TypeTerm, !IO),
        OoMCtors = one_or_more(HeadCtor, TailCtors),
        mercury_output_ctors(TypeVarSet, yes, HeadCtor, TailCtors, !IO),
        mercury_output_where_attributes(Info, TypeVarSet, no, MaybeCanonical,
            MaybeDirectArgs, !IO),
        io.write_string(".\n", !IO)
    ;
        TypeDefn = parse_tree_solver_type(DetailsSolver),
        DetailsSolver =
            type_details_solver(SolverTypeDetails, MaybeCanonical),
        mercury_output_begin_type_decl(solver_type, !IO),
        mercury_output_term(TypeVarSet, print_name_only, TypeTerm, !IO),
        mercury_output_where_attributes(Info, TypeVarSet,
            yes(SolverTypeDetails), MaybeCanonical, no, !IO),
        io.write_string(".\n", !IO)
    ;
        TypeDefn = parse_tree_foreign_type(DetailsForeign),
        DetailsForeign = type_details_foreign(ForeignType, MaybeCanonical,
            foreign_type_assertions(Assertions)),
        io.write_string(":- pragma foreign_type(", !IO),
        (
            ForeignType = c(_),
            io.write_string("c, ", !IO)
        ;
            ForeignType = java(_),
            io.write_string("java, ", !IO)
        ;
            ForeignType = csharp(_),
            io.write_string("csharp, ", !IO)
        ;
            ForeignType = erlang(_),
            io.write_string("erlang, ", !IO)
        ),
        mercury_output_term(TypeVarSet, print_name_only, TypeTerm, !IO),
        io.write_string(", \"", !IO),
        (
            ForeignType = c(c_type(ForeignTypeStr))
        ;
            ForeignType = java(java_type(ForeignTypeStr))
        ;
            ForeignType = csharp(csharp_type(ForeignTypeStr))
        ;
            ForeignType = erlang(erlang_type),
            ForeignTypeStr = ""
        ),
        io.write_string(ForeignTypeStr, !IO),
        io.write_string("\"", !IO),
        set.to_sorted_list(Assertions, AssertionsList),
        (
            AssertionsList = []
        ;
            AssertionsList = [_ | _],
            io.write_string(", [", !IO),
            io.write_list(AssertionsList, ", ",
                mercury_output_foreign_type_assertion, !IO),
            io.write_string("]", !IO)
        ),
        io.write_string(")", !IO),
        mercury_output_where_attributes(Info, TypeVarSet, no, MaybeCanonical,
            no, !IO),
        io.write_string(".\n", !IO)
    ).

%---------------------%
%
% Predicates needed to output more than one kind of type.
%

:- pred mercury_output_begin_type_decl(is_solver_type::in,
    io::di, io::uo) is det.

mercury_output_begin_type_decl(IsSolverType, !IO) :-
    (
        IsSolverType = solver_type,
        io.write_string(":- solver type ", !IO)
    ;
        IsSolverType = non_solver_type,
        io.write_string(":- type ", !IO)
    ).

mercury_output_where_attributes(Info, TypeVarSet,
        MaybeSolverTypeDetails, MaybeCanonical, MaybeDirectArgs, !IO) :-
    ( if
        MaybeSolverTypeDetails = no,
        MaybeCanonical = canon,
        MaybeDirectArgs = no
    then
        true
    else
        io.write_string("\n    where   ", !IO),
        (
            MaybeCanonical = noncanon(noncanon_abstract(_)),
            MaybeUniPred = no,
            MaybeCmpPred = no,
            io.write_string("type_is_abstract_noncanonical", !IO)
        ;
            (
                MaybeCanonical = canon,
                MaybeUniPred = no,
                MaybeCmpPred = no
            ;
                MaybeCanonical = noncanon(noncanon_uni_cmp(UniPred, CmpPred)),
                MaybeUniPred = yes(UniPred),
                MaybeCmpPred = yes(CmpPred)
            ;
                MaybeCanonical = noncanon(noncanon_uni_only(UniPred)),
                MaybeUniPred = yes(UniPred),
                MaybeCmpPred = no
            ;
                MaybeCanonical = noncanon(noncanon_cmp_only(CmpPred)),
                MaybeUniPred = no,
                MaybeCmpPred = yes(CmpPred)
            ),
            (
                MaybeSolverTypeDetails = yes(SolverTypeDetails),
                mercury_output_solver_type_details(Info, TypeVarSet,
                    SolverTypeDetails, !IO),
                ( if
                    MaybeUniPred = no,
                    MaybeCmpPred = no,
                    MaybeDirectArgs = no
                then
                    true
                else
                    io.write_string(",\n\t\t", !IO)
                )
            ;
                MaybeSolverTypeDetails = no
            )
        ),
        (
            MaybeUniPred = yes(UniPredName),
            io.write_string("equality is ", !IO),
            mercury_output_bracketed_sym_name(UniPredName, !IO),
            ( if
                MaybeCmpPred = no,
                MaybeDirectArgs = no
            then
                true
            else
                io.write_string(",\n\t\t", !IO)
            )
        ;
            MaybeUniPred = no
        ),
        (
            MaybeCmpPred = yes(CmpPredName),
            io.write_string("comparison is ", !IO),
            mercury_output_bracketed_sym_name(CmpPredName, !IO),
            (
                MaybeDirectArgs = no
            ;
                MaybeDirectArgs = yes(_),
                io.write_string(",\n\t\t", !IO)
            )
        ;
            MaybeCmpPred = no
        ),
        (
            MaybeDirectArgs = yes(DirectArgFunctors),
            io.write_string("direct_arg is [", !IO),
            mercury_output_direct_arg_functors(DirectArgFunctors, !IO),
            io.write_string("]", !IO)
        ;
            MaybeDirectArgs = no
        )
        % If you add code to print any more atttributes here, you must change
        % the conditions above for printing the commas before them.
    ).

:- pred mercury_output_solver_type_details(merc_out_info::in, tvarset::in,
    solver_type_details::in, io::di, io::uo) is det.

mercury_output_solver_type_details(Info, TypeVarSet, Details, !IO) :-
    Details = solver_type_details(RepresentationType, GroundInst,
        AnyInst, MutableInfos),
    io.write_string("representation is ", !IO),
    mercury_output_type(TypeVarSet, print_name_only, RepresentationType, !IO),
    Lang = get_output_lang(Info),
    varset.init(EmptyInstVarSet),
    io.write_string(",\n\t\tground is ", !IO),
    mercury_output_inst(Lang, EmptyInstVarSet, GroundInst, !IO),
    io.write_string(",\n\t\tany is ", !IO),
    mercury_output_inst(Lang, EmptyInstVarSet, AnyInst, !IO),
    (
        MutableInfos = []
    ;
        MutableInfos = [_ | _],
        io.write_string(",\n\t\tconstraint_store is [\n\t\t\t", !IO),
        io.write_list(MutableInfos, ",\n\t\t\t",
            mercury_output_item_mutable(Info), !IO),
        io.write_string("\n\t\t]", !IO)
    ).

%---------------------%
%
% Predicates needed to output abstract types.
%

:- pred mercury_output_where_abstract_enum_type(int::in, io::di, io::uo)
    is det.

mercury_output_where_abstract_enum_type(NumBits, !IO) :-
    io.write_string("\n\twhere\t", !IO),
    io.write_string("type_is_abstract_enum(", !IO),
    % XXX TYPE_REPN
    % io.write_string("type_is_representable_in_n_bits(", !IO),
    io.write_int(NumBits, !IO),
    io.write_string(")", !IO).

%---------------------%
%
% Predicates needed to output discriminated union types.
%

:- pred mercury_output_ctors(tvarset::in, bool::in,
    constructor::in, list(constructor)::in, io::di, io::uo) is det.

mercury_output_ctors(VarSet, First, HeadCtor, TailCtors, !IO) :-
    (
        First = yes,
        io.write_string("\n    --->    ", !IO)
    ;
        First = no,
        io.write_string("\n    ;       ", !IO)
    ),
    mercury_output_ctor(VarSet, HeadCtor, !IO),
    (
        TailCtors = []
    ;
        TailCtors = [HeadTailCtor | TailTailCtors],
        mercury_output_ctors(VarSet, no, HeadTailCtor, TailTailCtors, !IO)
    ).

mercury_output_ctor(TypeVarSet, Ctor, !IO) :-
    Ctor = ctor(_Ordinal, MaybeExistConstraints, SymName, Args, Arity, _Ctxt),

    % We will have attached the module name to the type definition,
    % so there is no point adding it to the constructor as well.
    Name = unqualify_name(SymName),
    (
        MaybeExistConstraints = no_exist_constraints,
        Constraints = [],
        ParenWrap = no
    ;
        MaybeExistConstraints = exist_constraints(ExistConstraints),
        ExistConstraints = cons_exist_constraints(ExistQVars, Constraints,
            _UnconstrainedQVars, _ConstrainedQVars),
        mercury_output_quantifier(TypeVarSet, print_name_only, ExistQVars,
            !IO),
        io.write_string("(", !IO),
        ParenWrap = yes
    ),
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
        BraceWrap = yes,
        io.write_string("{ ", !IO)
    else
        BraceWrap = no
    ),
    (
        Args = [Arg | Rest],
        mercury_output_sym_name(unqualified(Name), !IO),
        io.write_string("(", !IO),
        mercury_output_ctor_arg(TypeVarSet, Arg, !IO),
        mercury_output_remaining_ctor_args(TypeVarSet, Rest, !IO),
        io.write_string(")", !IO)
    ;
        Args = [],
        mercury_output_bracketed_sym_name(unqualified(Name), !IO),
        % This space prevents a terminating full stop from being confused
        % as part of the sym_name if the sym_name contains graphical
        % characters.
        io.write_string(" ", !IO)
    ),
    (
        BraceWrap = yes,
        io.write_string(" }", !IO)
    ;
        BraceWrap = no
    ),
    mercury_format_prog_constraint_list(TypeVarSet, print_name_only, "=>",
        Constraints, !IO),
    (
        ParenWrap = no
    ;
        ParenWrap = yes,
        io.write_string(")", !IO)
    ).

:- pred mercury_output_ctor_arg(tvarset::in, constructor_arg::in,
    io::di, io::uo) is det.

mercury_output_ctor_arg(TVarSet, Arg, !IO) :-
    Arg = ctor_arg(Name, Type, _Context),
    mercury_output_ctor_arg_name_prefix(Name, !IO),
    mercury_output_type(TVarSet, print_name_only, Type, !IO).

:- pred mercury_output_remaining_ctor_args(tvarset::in,
    list(constructor_arg)::in, io::di, io::uo) is det.

mercury_output_remaining_ctor_args(_TVarSet, [], !IO).
mercury_output_remaining_ctor_args(TVarSet, [Arg | Args], !IO) :-
    io.write_string(", ", !IO),
    mercury_output_ctor_arg(TVarSet, Arg, !IO),
    mercury_output_remaining_ctor_args(TVarSet, Args, !IO).

:- pred mercury_output_ctor_arg_name_prefix(maybe(ctor_field_name)::in,
    io::di, io::uo) is det.

mercury_output_ctor_arg_name_prefix(no, !IO).
mercury_output_ctor_arg_name_prefix(yes(FieldName), !IO) :-
    FieldName = ctor_field_name(Name, _Ctxt),
    mercury_output_bracketed_sym_name(Name, !IO),
    io.write_string(" :: ", !IO).

:- pred mercury_output_direct_arg_functors(list(sym_name_arity)::in,
    io::di, io::uo) is det.

mercury_output_direct_arg_functors(Ctors, !IO) :-
    io.write_list(Ctors, ", ", mercury_format_sym_name_arity, !IO).

%---------------------%
%
% Predicates needed to output foreign types.
%

:- pred mercury_output_foreign_type_assertion(foreign_type_assertion::in,
    io::di, io::uo) is det.

mercury_output_foreign_type_assertion(Assertion, !IO) :-
    (
        Assertion = foreign_type_can_pass_as_mercury_type,
        io.write_string("can_pass_as_mercury_type", !IO)
    ;
        Assertion = foreign_type_stable,
        io.write_string("stable", !IO)
    ;
        Assertion = foreign_type_word_aligned_pointer,
        io.write_string("word_aligned_pointer", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_inst_ctor_all_defns(merc_out_info::in,
    inst_ctor_all_defns::in, io::di, io::uo) is det.

mercury_output_inst_ctor_all_defns(Info, InstCtorAllDefns, !IO) :-
    InstCtorAllDefns = inst_ctor_all_defns(Abs, Eqv),
    list.foldl(mercury_output_item_inst_defn(Info), Abs, !IO),
    list.foldl(mercury_output_item_inst_defn(Info), Eqv, !IO).

:- pred mercury_output_item_inst_defn(merc_out_info::in,
    item_inst_defn_info::in, io::di, io::uo) is det.

mercury_output_item_inst_defn(Info, ItemInstDefn, !IO) :-
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
    maybe_output_line_number(Info, Context, !IO),
    Lang = get_output_lang(Info),
    ArgTerms = list.map(func(V) = variable(V, Context), InstParams),
    construct_qualified_term_with_context(SymName, ArgTerms, Context,
        InstTerm),
    (
        MaybeAbstractInstDefn = abstract_inst_defn,
        io.write_string(":- abstract_inst((", !IO),
        mercury_output_term(InstVarSet, print_name_only, InstTerm, !IO),
        io.write_string(")).\n", !IO)
    ;
        MaybeAbstractInstDefn = nonabstract_inst_defn(eqv_inst(Inst)),
        % XXX The parentheses around the inst name and its arguments
        % is redundant *most* of the time, in which case it is only clutter.
        % It would be nice to eliminate this clutter.
        io.write_string(":- inst (", !IO),
        mercury_output_term(InstVarSet, print_name_only, InstTerm, !IO),
        io.write_string(") ", !IO),
        (
            MaybeForTypeCtor = no
        ;
            MaybeForTypeCtor = yes(ForTypeCtor),
            ForTypeCtor = type_ctor(ForTypeCtorSymName, ForTypeCtorArity),
            io.write_string("for ", !IO),
            mercury_output_sym_name(ForTypeCtorSymName, !IO),
            io.write_string("/", !IO),
            io.write_int(ForTypeCtorArity, !IO),
            io.write_string(" ", !IO)
        ),
        % XXX If Inst is bound(...), it would be nice to print the inst
        % definition using the easier-to-read
        %
        %   :- inst i
        %       --->    f1(...)
        %       ;       f2(...).
        %
        % syntax.
        io.write_string("== ", !IO),
        mercury_output_inst(Lang, InstVarSet, Inst, !IO),
        io.write_string(".\n", !IO)
    ).

    % Succeed if the sym_name describes a builtin inst.
    %
:- pred is_builtin_inst_name(inst_varset::in, sym_name::in, list(inst_var)::in)
    is semidet.

is_builtin_inst_name(InstVarSet, unqualified(Name), Args0) :-
    Args1 = list.map(func(V) = variable(coerce_var(V), context_init), Args0),
    Term = term.functor(term.atom(Name), Args1, term.context_init),
    varset.coerce(InstVarSet, VarSet),
    ContextPieces = cord.init,  % Dummy; not used.
    parse_inst(no_allow_constrained_inst_var(wnciv_inst_defn_lhs), VarSet,
        ContextPieces, Term, MaybeInst),
    MaybeInst = ok1(Inst),
    Inst \= defined_inst(user_inst(_, _)).

%---------------------------------------------------------------------------%

:- pred mercury_output_mode_ctor_all_defns(merc_out_info::in,
    mode_ctor_all_defns::in, io::di, io::uo) is det.

mercury_output_mode_ctor_all_defns(Info, ModeCtorAllDefns, !IO) :-
    ModeCtorAllDefns = mode_ctor_all_defns(Abs, Eqv),
    list.foldl(mercury_output_item_mode_defn(Info), Abs, !IO),
    list.foldl(mercury_output_item_mode_defn(Info), Eqv, !IO).

:- pred mercury_output_item_mode_defn(merc_out_info::in,
    item_mode_defn_info::in, io::di, io::uo) is det.

mercury_output_item_mode_defn(Info, ItemModeDefn, !IO) :-
    ItemModeDefn = item_mode_defn_info(SymName, InstParams,
        MaybeAbstractModeDefn, VarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, SymName, UnQualSymName),
    maybe_output_line_number(Info, Context, !IO),
    Lang = get_output_lang(Info),
    mercury_format_mode_defn(Lang, VarSet, Context, UnQualSymName, InstParams,
        MaybeAbstractModeDefn, !IO).

    % This is defined to work on !U instead of !IO so that we can call
    % mercury_format_mode with simple_inst_info.
    %
:- pred mercury_format_mode_defn(output_lang::in, inst_varset::in,
    prog_context::in, sym_name::in, list(inst_var)::in,
    maybe_abstract_mode_defn::in, U::di, U::uo) is det <= output(U).

mercury_format_mode_defn(Lang, InstVarSet, Context, Name, Args,
        MaybeAbstractModeDefn, !U) :-
    (
        MaybeAbstractModeDefn = abstract_mode_defn,
        add_string(":- abstract_mode((", !U),
        mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, !U),
        add_string(")).\n", !U)
    ;
        MaybeAbstractModeDefn = nonabstract_mode_defn(eqv_mode(Mode)),
        add_string(":- mode (", !U),
        mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, !U),
        add_string(") == ", !U),
        mercury_format_mode(Lang, InstVarSet, Mode, !U),
        add_string(".\n", !U)
    ).

:- pred mercury_format_mode_defn_head(inst_varset::in, prog_context::in,
    sym_name::in, list(inst_var)::in, U::di, U::uo) is det <= output(U).

mercury_format_mode_defn_head(InstVarSet, Context, Name, Args, !U) :-
    ArgTerms = list.map(func(V) = variable(V, Context), Args),
    construct_qualified_term_with_context(Name, ArgTerms, Context, ModeTerm),
    mercury_format_term(InstVarSet, print_name_only, ModeTerm, !U).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_pred_decl(merc_out_info::in,
    item_pred_decl_info::in, io::di, io::uo) is det.

mercury_output_item_pred_decl(Info, ItemPredDecl, !IO) :-
    % Most of the code that outputs pred declarations is in
    % parse_tree_out_pred_decl.m.
    ItemPredDecl = item_pred_decl_info(PredName0, PredOrFunc, TypesAndModes,
        WithType, WithInst, MaybeDetism, _Origin, TypeVarSet, InstVarSet,
        ExistQVars, Purity, Constraints, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, PredName0, PredName),
    maybe_output_line_number(Info, Context, !IO),
    Lang = get_output_lang(Info),
    ( if
        % Function declarations using `with_type` have the same format
        % as predicate declarations, but with `func' instead of `pred'.
        PredOrFunc = pf_function,
        WithType = no
    then
        pred_args_to_func_args(TypesAndModes, FuncTypesAndModes,
            RetTypeAndMode),
        mercury_format_func_decl(Lang, TypeVarSet, InstVarSet,
            ExistQVars, PredName, FuncTypesAndModes, RetTypeAndMode,
            MaybeDetism, Purity, Constraints,
            ":- ", ".\n", ".\n", !IO)
    else
        mercury_format_pred_or_func_decl(Lang, TypeVarSet, InstVarSet,
            PredOrFunc, ExistQVars, PredName, TypesAndModes,
            WithType, WithInst, MaybeDetism, Purity, Constraints,
            ":- ", ".\n", ".\n", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_mode_decl(merc_out_info::in,
    item_mode_decl_info::in, io::di, io::uo) is det.

mercury_output_item_mode_decl(Info, ItemModeDecl, !IO) :-
    % Most of the code that outputs mode declarations is in
    % parse_tree_out_pred_decl.m.
    ItemModeDecl = item_mode_decl_info(PredName0, PredOrFunc, Modes,
        WithInst, MaybeDet, VarSet, Context, _SeqNum),
    maybe_unqualify_sym_name(Info, PredName0, PredName),
    maybe_output_line_number(Info, Context, !IO),
    Lang = get_output_lang(Info),
    ( if
        % Function mode declarations using `with_type` have the same format
        % as predicate mode declarations.
        PredOrFunc = yes(pf_function),
        WithInst = no
    then
        pred_args_to_func_args(Modes, FuncModes, RetMode),
        mercury_output_func_mode_decl(Lang, VarSet, PredName,
            FuncModes, RetMode, MaybeDet, !IO)
    else
        mercury_output_pred_mode_decl(Lang, VarSet, PredName,
            Modes, WithInst, MaybeDet, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_foreign_enums(merc_out_info::in,
    c_j_cs_e_enums::in, U::di, U::uo) is det <= output(U).

mercury_output_foreign_enums(Info, CJCsEEnums, !U) :-
    CJCsEEnums = c_java_csharp_erlang(CEnums, JavaEnums,
        CsharpEnums, ErlangEnums),
    list.foldl(mercury_output_item_foreign_enum(Info), CEnums, !U),
    list.foldl(mercury_output_item_foreign_enum(Info), JavaEnums, !U),
    list.foldl(mercury_output_item_foreign_enum(Info), CsharpEnums, !U),
    list.foldl(mercury_output_item_foreign_enum(Info), ErlangEnums, !U).

:- pred mercury_output_item_foreign_enum(merc_out_info::in,
    item_foreign_enum_info::in, U::di, U::uo) is det <= output(U).

mercury_output_item_foreign_enum(_Info, ItemForeignEnum, !U) :-
    ItemForeignEnum = item_foreign_enum_info(Lang, TypeCtor, OoMValues,
        _Context, _SeqNum),
    add_string(":- pragma foreign_enum(", !U),
    mercury_format_foreign_language_string(Lang, !U),
    add_string(", ", !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName, !U),
    add_string("/", !U),
    add_int(TypeArity, !U),
    add_string(", ", !U),
    Values = one_or_more_to_list(OoMValues),
    mercury_format_unqual_sym_name_string_assoc_list(Values, !U),
    add_string(").\n", !U).

    % Output an association list of to-be-unqualified sym_names and strings.
    % The strings will be quoted in the output.
    %
:- pred mercury_format_unqual_sym_name_string_assoc_list(
    assoc_list(sym_name, string)::in, U::di, U::uo) is det <= output(U).

mercury_format_unqual_sym_name_string_assoc_list(AssocList, !U) :-
    add_char('[', !U),
    add_list(AssocList, ", ", mercury_format_unqual_sym_name_string_pair, !U),
    add_char(']', !U).

:- pred mercury_format_unqual_sym_name_string_pair(
    pair(sym_name, string)::in, U::di, U::uo) is det <= output(U).

mercury_format_unqual_sym_name_string_pair(SymName0 - String, !U) :-
    Name = unqualify_name(SymName0),
    SymName = unqualified(Name),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, SymName, !U),
    add_string(" - ", !U),
    add_quoted_string(String, !U).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_foreign_export_enum(merc_out_info::in,
    item_foreign_export_enum_info::in, U::di, U::uo) is det <= output(U).

mercury_output_item_foreign_export_enum(_Info, ItemForeignExportEnum, !U) :-
    ItemForeignExportEnum = item_foreign_export_enum_info(Lang, TypeCtor,
        Attributes, Overrides, _Context, _SeqNum),
    add_string(":- pragma foreign_export_enum(", !U),
    mercury_format_foreign_language_string(Lang, !U),
    add_string(", ", !U),
    TypeCtor = type_ctor(TypeName, TypeArity),
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, TypeName, !U),
    add_string("/", !U),
    add_int(TypeArity, !U),
    add_string(", ", !U),
    mercury_format_foreign_export_enum_attributes(Attributes, !U),
    add_string(", ", !U),
    mercury_format_sym_name_string_assoc_list(Overrides, !U),
    add_string(").\n", !U).

:- pred mercury_format_foreign_export_enum_attributes(
    export_enum_attributes::in, U::di, U::uo) is det <= output(U).

mercury_format_foreign_export_enum_attributes(Attributes, !U) :-
    MaybePrefix = Attributes ^ ee_attr_prefix,
    add_string("[", !U),
    (
        MaybePrefix = no
    ;
        MaybePrefix = yes(Prefix),
        add_string("prefix(", !U),
        add_quoted_string(Prefix, !U),
        add_char(')', !U)
    ),
    add_string("]", !U).

    % Output an association list of sym_names and strings.
    % The strings will be quoted in the output.
    %
:- pred mercury_format_sym_name_string_assoc_list(
    assoc_list(sym_name, string)::in, U::di, U::uo) is det <= output(U).

mercury_format_sym_name_string_assoc_list(AssocList, !U) :-
    add_char('[', !U),
    add_list(AssocList, ", ", mercury_format_sym_name_string_pair, !U),
    add_char(']', !U).

:- pred mercury_format_sym_name_string_pair(
    pair(sym_name, string)::in, U::di, U::uo) is det <= output(U).

mercury_format_sym_name_string_pair(SymName - String, !U) :-
    mercury_format_bracketed_sym_name_ngt(next_to_graphic_token, SymName, !U),
    add_string(" - ", !U),
    add_quoted_string(String, !U).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_promise(merc_out_info::in, item_promise_info::in,
    io::di, io::uo) is det.

mercury_output_item_promise(_, ItemPromise, !IO) :-
    % Any changes here may require similar changes in the write_promise
    % predicate in hlds_out_module.m.

    ItemPromise = item_promise_info(PromiseType, Goal0, VarSet, UnivVars,
        _Context, _SeqNum),
    Indent = 1,
    (
        PromiseType = promise_type_true,
        % For an assertion, we put back any universally quantified variables
        % that were stripped off during parsing so that the clause will
        % output correctly.
        io.write_string(":- promise ", !IO),
        (
            UnivVars = [_ | _],
            Goal = quant_expr(quant_all, quant_ordinary_vars,
                get_goal_context(Goal0), UnivVars, Goal0)
        ;
            UnivVars = [],
            Goal = Goal0
        )
    ;
        ( PromiseType = promise_type_exclusive
        ; PromiseType = promise_type_exhaustive
        ; PromiseType = promise_type_exclusive_exhaustive
        ),
        % A promise ex declaration has a slightly different standard formatting
        % from an assertion; the universal quantification comes before the rest
        % of the declaration.
        io.write_string(":- all [", !IO),
        VarNamePrint = print_name_only,
        mercury_output_vars(VarSet, VarNamePrint, UnivVars, !IO),
        io.write_string("]", !IO),
        mercury_output_newline(Indent, !IO),
        prog_out.write_promise_type(PromiseType, !IO),
        Goal0 = Goal
    ),
    mercury_output_newline(Indent, !IO),
    mercury_output_goal(VarSet, Indent, Goal, !IO),
    io.write_string(".\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_typeclass(merc_out_info::in,
    item_typeclass_info::in, io::di, io::uo) is det.

mercury_output_item_typeclass(Info, ItemTypeClass, !IO) :-
    ItemTypeClass = item_typeclass_info(ClassName0, Vars, Constraints, FunDeps,
        Interface, VarSet, _Context, _SeqNum),
    maybe_unqualify_sym_name(Info, ClassName0, ClassName),
    io.write_string(":- typeclass ", !IO),

    % We put an extra set of brackets around the class name in
    % case the name is an operator.
    mercury_output_sym_name(ClassName, !IO),
    io.write_char('(', !IO),
    io.write_list(Vars, ", ",
        ( pred(V::in, IO0::di, IO::uo) is det :-
            varset.lookup_name(VarSet, V, VarName),
            io.write_string(VarName, IO0, IO)
        ), !IO),
    io.write_char(')', !IO),
    mercury_format_fundeps_and_prog_constraint_list(VarSet, print_name_only,
        FunDeps, Constraints, !IO),
    (
        Interface = class_interface_abstract,
        io.write_string(".\n", !IO)
    ;
        Interface = class_interface_concrete(ClassDecls),
        io.write_string(" where [\n", !IO),
        Lang = get_output_lang(Info),
        output_class_decls(Lang, ClassDecls, !IO),
        io.write_string("\n].\n", !IO)
    ).

:- pred mercury_format_fundeps_and_prog_constraint_list(tvarset::in,
    var_name_print::in, list(prog_fundep)::in, list(prog_constraint)::in,
    U::di, U::uo) is det
    <= output(U).

mercury_format_fundeps_and_prog_constraint_list(VarSet, VarNamePrint,
        FunDeps, Constraints, !U) :-
    ( if
        FunDeps = [],
        Constraints = []
    then
        true
    else
        add_string(" <= (", !U),
        add_list(FunDeps, ", ",
            mercury_format_fundep(VarSet, VarNamePrint), !U),
        (
            Constraints = []
        ;
            Constraints = [_ | _],
            (
                FunDeps = []
            ;
                FunDeps = [_ | _],
                add_string(", ", !U)
            ),
            add_list(Constraints, ", ",
                mercury_format_constraint(VarSet, VarNamePrint), !U)
        ),
        add_string(")", !U)
    ).

:- pred mercury_format_fundep(tvarset::in, var_name_print::in, prog_fundep::in,
    U::di, U::uo) is det <= output(U).

mercury_format_fundep(TypeVarSet, VarNamePrint, fundep(Domain, Range), !U) :-
    add_string("(", !U),
    add_list(Domain, ", ", mercury_format_var(TypeVarSet, VarNamePrint), !U),
    add_string(" -> ", !U),
    add_list(Range, ", ", mercury_format_var(TypeVarSet, VarNamePrint), !U),
    add_string(")", !U).

:- pred output_class_decls(output_lang::in, list(class_decl)::in,
    io::di, io::uo) is det.

output_class_decls(Lang, ClassDecls, !IO) :-
    io.write_list(ClassDecls, ",\n", output_class_decl(Lang), !IO).

:- pred output_class_decl(output_lang::in, class_decl::in,
    io::di, io::uo) is det.

output_class_decl(Lang, Decl, !IO) :-
    io.write_string("\t", !IO),
    (
        Decl = class_decl_pred_or_func(PredOrFuncInfo),
        PredOrFuncInfo = class_pred_or_func_info(SymName, PredOrFunc,
            TypesAndModes, WithType, WithInst, MaybeDetism,
            TypeVarSet, InstVarSet, ExistQVars, Purity,
            Constraints, _Context),

        % The module name is implied by the qualifier of the
        % `:- typeclass declaration'.
        Name = unqualify_name(SymName),
        ( if
            % Function declarations using `with_type` have the same format
            % as predicate declarations, but with `func' instead of `pred'.
            PredOrFunc = pf_function,
            WithType = no
        then
            pred_args_to_func_args(TypesAndModes,
                FuncTypesAndModes, RetTypeAndMode),
            mercury_format_func_decl(Lang, TypeVarSet, InstVarSet, ExistQVars,
                unqualified(Name), FuncTypesAndModes, RetTypeAndMode,
                MaybeDetism, Purity, Constraints, "", ",\n\t", "", !IO)
        else
            mercury_format_pred_or_func_decl(Lang, TypeVarSet, InstVarSet,
                PredOrFunc, ExistQVars, unqualified(Name), TypesAndModes,
                WithType, WithInst, MaybeDetism, Purity,
                Constraints, "", ",\n\t", "", !IO)
        )
    ;
        Decl = class_decl_mode(ModeInfo),
        ModeInfo = class_mode_info(SymName, PredOrFunc, Modes,
            WithInst, MaybeDetism, InstVarSet, _Context),

        % The module name is implied by the qualifier of the
        % `:- typeclass declaration'.
        Name = unqualify_name(SymName),
        ( if
            % Function mode declarations using `with_type` have the same format
            % as predicate mode declarations.
            PredOrFunc = yes(pf_function),
            WithInst = no
        then
            pred_args_to_func_args(Modes, FuncModes, RetMode),
            mercury_format_func_mode_decl(Lang, InstVarSet,
                unqualified(Name), FuncModes, RetMode, MaybeDetism,
                "", "", !IO)
        else
            mercury_format_pred_or_func_mode_decl(Lang, InstVarSet,
                unqualified(Name), Modes, WithInst, MaybeDetism,
                "", "", !IO)
        )
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_instance(merc_out_info::in, item_instance_info::in,
    io::di, io::uo) is det.

mercury_output_item_instance(_, ItemInstance, !IO) :-
    % XXX When prettyprinting a Mercury module, we want to print the original
    % types. When generating interface types, we want to print the
    % equiv-type-expanded types. We do the latter.
    ItemInstance = item_instance_info(ClassName,Types, _OriginalTypes,
        Constraints, Body, VarSet, _InstanceModuleName, _Context, _SeqNum),
    io.write_string(":- instance ", !IO),
    % We put an extra set of brackets around the class name in case
    % the name is an operator.
    io.write_char('(', !IO),
    mercury_output_sym_name(ClassName, !IO),
    io.write_char('(', !IO),
    io.write_list(Types, ", ",
        mercury_output_type(VarSet, print_name_only), !IO),
    io.write_char(')', !IO),
    io.write_char(')', !IO),
    mercury_format_prog_constraint_list(VarSet, print_name_only, "<=",
        Constraints, !IO),
    (
        Body = instance_body_abstract
    ;
        Body = instance_body_concrete(Methods),
        io.write_string(" where [\n", !IO),
        mercury_output_instance_methods(Methods, !IO),
        io.write_string("\n]", !IO)
    ),
    io.write_string(".\n", !IO).

:- pred mercury_output_instance_methods(list(instance_method)::in,
    io::di, io::uo) is det.

mercury_output_instance_methods(Methods, !IO) :-
    io.write_list(Methods, ",\n", mercury_output_instance_method, !IO).

mercury_output_instance_method(Method, !IO) :-
    Method = instance_method(PredOrFunc, MethodName, Defn, Arity, _Context),
    (
        Defn = instance_proc_def_name(PredName),
        io.write_char('\t', !IO),
        (
            PredOrFunc = pf_function,
            io.write_string("func(", !IO)
        ;
            PredOrFunc = pf_predicate,
            io.write_string("pred(", !IO)
        ),
        mercury_output_bracketed_sym_name_ngt(next_to_graphic_token,
            MethodName, !IO),
        io.write_string("/", !IO),
        io.write_int(Arity, !IO),
        io.write_string(") is ", !IO),
        mercury_output_bracketed_sym_name(PredName, !IO)
    ;
        Defn = instance_proc_def_clauses(Items),
        % XXX should we output the term contexts?
        io.write_string("\t(", !IO),
        io.write_list(Items, "),\n\t(",
            output_instance_method_clause(MethodName), !IO),
        io.write_string(")", !IO)
    ).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_initialise(merc_out_info::in,
    item_initialise_info::in, io::di, io::uo) is det.

mercury_output_item_initialise(_, ItemInitialise, !IO) :-
    ItemInitialise = item_initialise_info(PredSymName, Arity, _, _Context,
        _SeqNum),
    io.write_string(":- initialise ", !IO),
    mercury_output_sym_name(PredSymName, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(".\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_finalise(merc_out_info::in, item_finalise_info::in,
    io::di, io::uo) is det.

mercury_output_item_finalise(_, ItemFinalise, !IO) :-
    ItemFinalise = item_finalise_info(PredSymName, Arity, _, _Context,
        _SeqNum),
    io.write_string(":- finalise ", !IO),
    mercury_output_sym_name(PredSymName, !IO),
    io.write_string("/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(".\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_mutable(merc_out_info::in, item_mutable_info::in,
    io::di, io::uo) is det.

mercury_output_item_mutable(Info, ItemMutable, !IO) :-
    ItemMutable = item_mutable_info(Name, _OrigType, Type, _OrigInst, Inst,
        InitTerm, MutVarSet, Attrs, _Context, _SeqNum),
    io.write_string(":- mutable(", !IO),
    io.write_string(Name, !IO),
    io.write_string(", ", !IO),
    mercury_output_type(varset.init, print_name_only, Type, !IO),
    io.write_string(", ", !IO),

    % See the comments for read_mutable_decl for the reason we _must_ use
    % MutVarSet here.
    mercury_output_term(MutVarSet, print_name_only, InitTerm, !IO),
    io.write_string(", ", !IO),
    Lang = get_output_lang(Info),
    mercury_output_inst(Lang, varset.init, Inst, !IO),
    io.write_string(", ", !IO),
    io.print(Attrs, !IO),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_foreign_import_module(item_fim::in,
    io::di, io::uo) is det.

mercury_output_item_foreign_import_module(ItemFIM, !IO) :-
    ItemFIM = item_fim(Lang, ModuleName, _Context, _SeqNum),
    FIMSpec = fim_spec(Lang, ModuleName),
    mercury_output_fim_spec(FIMSpec, !IO).

mercury_output_fim_spec(FIMSpec, !IO) :-
    FIMSpec = fim_spec(Lang, ModuleName),
    io.write_string(":- pragma foreign_import_module(", !IO),
    mercury_format_foreign_language_string(Lang, !IO),
    io.write_string(", ", !IO),
    mercury_output_bracketed_sym_name_ngt(not_next_to_graphic_token,
        ModuleName, !IO),
    io.write_string(").\n", !IO).

%---------------------------------------------------------------------------%

:- pred mercury_output_item_type_repn(merc_out_info::in,
    item_type_repn_info::in, io::di, io::uo) is det.

mercury_output_item_type_repn(Info, ItemTypeRepn, !IO) :-
    CommaSep = get_human_comma_sep(Info),
    ItemTypeRepn = item_type_repn_info(TypeCtorSymName, ArgTVars, RepnInfo,
        TVarSet, _Context, _SeqNum),
    io.write_string(":- type_representation(", !IO),
    mercury_output_sym_name(TypeCtorSymName, !IO),
    (
        ArgTVars = []
    ;
        ArgTVars = [_ | _],
        io.write_string("(", !IO),
        io.write_list(ArgTVars, CommaSep,
            mercury_output_var(TVarSet, print_num_only), !IO),
        io.write_string(")", !IO)
    ),
    io.write_string(CommaSep, !IO),
    (
        RepnInfo = tcrepn_is_direct_dummy,
        io.write_string("is_direct_dummy", !IO)
    ;
        RepnInfo = tcrepn_is_notag,
        io.write_string("is_notag", !IO)
    ;
        RepnInfo = tcrepn_fits_in_n_bits(NumBits, FillKind),
        fill_kind_string(FillKind, FillKindStr),
        io.format("fits_in_n_bits(%d%s%s)",
            [i(NumBits), s(CommaSep), s(FillKindStr)], !IO)
    ;
        RepnInfo = tcrepn_is_eqv_to(EqvType),
        io.write_string("is_eqv_to(", !IO),
        mercury_output_type(TVarSet, print_num_only, EqvType, !IO),
        io.write_string(")", !IO)
    ;
        RepnInfo = tcrepn_is_word_aligned_ptr,
        io.write_string("is_word_aligned_ptr", !IO)
    ;
        RepnInfo = tcrepn_has_direct_arg_functors(SymNameAndArities),
        io.write_string("has_direct_arg_functors([", !IO),
        io.write_list(SymNameAndArities, CommaSep, write_sym_name_arity, !IO),
        io.write_string("])", !IO)
    ;
        RepnInfo = tcrepn_du(DuRepn),
        io.write_string("du_repn(", !IO),
        mercury_output_du_type_repn(CommaSep, DuRepn, !IO),
        io.write_string(")", !IO)
    ;
        RepnInfo = tcrepn_foreign(MaybeCJCsERepn),
        io.write_string("foreign_type_repn(", !IO),
        mercury_output_c_j_cs_e_repn(CommaSep, MaybeCJCsERepn, !IO),
        io.write_string(")", !IO)
    ),
    io.write_string(").\n", !IO).

%---------------------%

:- pred mercury_output_du_type_repn(string::in, du_repn::in,
    io::di, io::uo) is det.

mercury_output_du_type_repn(CommaSep, DuRepn, !IO) :-
    % XXX We output the names of types and function symbols as plain atoms.
    % This works when those names follow the usual pattern, and contain
    % only alphanumeric characters and start with a lower case letter,
    % but may pose problems when they violate that pattern. This needs
    % further testing, and may require changes, both here and in
    % parse_type_repn.m.
    %
    % XXX At the moment, we write out *everything* on one line. This format
    % makes automatic comparisons between type_repn items in different
    % interface files easy, but it also makes these items very hard to read
    % for humans. We should provide an option to switch to an format
    % with structured indentation and limited length lines.
    (
        DuRepn = dur_notag(NotagRepn),
        NotagRepn = notag_repn(FunctorName, MaybeCJCsERepn),
        io.write_string("notag(", !IO),
        term_io.quote_string(FunctorName, !IO),
        io.write_string(CommaSep, !IO),
        mercury_output_c_j_cs_e_repn(CommaSep, MaybeCJCsERepn, !IO),
        io.write_string(")", !IO)
    ;
        DuRepn = dur_direct_dummy(DummyRepn),
        DummyRepn = direct_dummy_repn(FunctorName, MaybeCJCsERepnOrEnum),
        io.write_string("direct_dummy(", !IO),
        term_io.quote_string(FunctorName, !IO),
        io.write_string(CommaSep, !IO),
        mercury_output_c_j_cs_e_repn_or_enum(CommaSep,
            MaybeCJCsERepnOrEnum, !IO),
        io.write_string(")", !IO)
    ;
        DuRepn = dur_enum(EnumRepn),
        EnumRepn = enum_repn(Functor1, Functor2, OtherFunctors,
            MaybeCJCsERepnOrEnum),
        io.write_string("enum(", !IO),
        mercury_output_enum_functor_name(Functor1, !IO),
        io.write_string(CommaSep, !IO),
        mercury_output_enum_functor_name(Functor2, !IO),
        io.write_string(CommaSep, !IO),
        (
            OtherFunctors = [],
            io.write_string("[]", !IO)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            io.write_string("[", !IO),
            mercury_output_enum_functor_names(CommaSep,
                HeadFunctor, TailFunctors, !IO),
            io.write_string("]", !IO)
        ),
        io.write_string(CommaSep, !IO),
        mercury_output_c_j_cs_e_repn_or_enum(CommaSep,
            MaybeCJCsERepnOrEnum, !IO),
        io.write_string(")", !IO)
    ;
        DuRepn = dur_gen_more_functors(MoreFunctors),
        MoreFunctors = gen_du_repn_more_functors(Functor1, Functor2,
            OtherFunctors, MaybeCJCsERepn),
        io.write_string("gen_du(", !IO),
        mercury_output_gen_du_functor(Functor1, CommaSep, !IO),
        mercury_output_gen_du_functor(Functor2, CommaSep, !IO),
        (
            OtherFunctors = [],
            io.write_string("[]", !IO)
        ;
            OtherFunctors = [HeadFunctor | TailFunctors],
            io.write_string("[", !IO),
            mercury_output_gen_du_functors(HeadFunctor, TailFunctors, !IO),
            io.write_string("]", !IO)
        ),
        io.write_string(CommaSep, !IO),
        mercury_output_c_j_cs_e_repn(CommaSep, MaybeCJCsERepn, !IO),
        io.write_string(")", !IO)
    ;
        DuRepn = dur_gen_only_functor(OnlyFunctor),
        OnlyFunctor = gen_du_repn_only_functor(FunctorName,
            OnlyFunctorArgs64, OnlyFunctorArgs32, MaybeCJCsERepn),
        io.write_string("gen_du_only_functor(", !IO),
        term_io.quote_string(FunctorName, !IO),
        io.write_string(CommaSep, !IO),
        mercury_output_only_functor_args(OnlyFunctorArgs64, !IO),
        io.write_string(CommaSep, !IO),
        mercury_output_only_functor_args(OnlyFunctorArgs32, !IO),
        io.write_string(CommaSep, !IO),
        mercury_output_c_j_cs_e_repn(CommaSep, MaybeCJCsERepn, !IO),
        io.write_string(")", !IO)
    ).

:- pred mercury_output_enum_functor_name(string::in, io::di, io::uo) is det.

mercury_output_enum_functor_name(FunctorName, !IO) :-
    term_io.quote_string(FunctorName, !IO).

:- pred mercury_output_enum_functor_names(string::in,
    string::in, list(string)::in, io::di, io::uo) is det.

mercury_output_enum_functor_names(CommaSep, FunctorName, FunctorNames, !IO) :-
    mercury_output_enum_functor_name(FunctorName, !IO),
    (
        FunctorNames = []
    ;
        FunctorNames = [HeadFunctorName | TailFunctorNames],
        io.write_string(CommaSep, !IO),
        mercury_output_enum_functor_names(CommaSep,
            HeadFunctorName, TailFunctorNames, !IO)
    ).

:- pred mercury_output_c_j_cs_e_repn_or_enum(string::in,
    c_j_cs_e_enum_repn::in, io::di, io::uo) is det.

mercury_output_c_j_cs_e_repn_or_enum(CommaSep, MaybeCJCsERepnOrEnum, !IO) :-
    MaybeCJCsERepnOrEnum = c_java_csharp_erlang(MaybeRepnOrEnumC,
        MaybeRepnOrEnumJava, MaybeRepnOrEnumCsharp, MaybeRepnOrEnumErlang),
    io.write_string("[", !IO),
    some [!Separator] (
        !:Separator = "",
        (
            MaybeRepnOrEnumC = no
        ;
            MaybeRepnOrEnumC = yes(RepnOrEnumC),
            mercury_output_foreign_lang_type_repn_or_enum(CommaSep,
                lang_c, RepnOrEnumC, !IO),
            !:Separator = CommaSep
        ),
        (
            MaybeRepnOrEnumJava = no
        ;
            MaybeRepnOrEnumJava = yes(RepnOrEnumJava),
            io.write_string(!.Separator, !IO),
            mercury_output_foreign_lang_type_repn_or_enum(CommaSep,
                lang_java, RepnOrEnumJava, !IO),
            !:Separator = CommaSep
        ),
        (
            MaybeRepnOrEnumCsharp = no
        ;
            MaybeRepnOrEnumCsharp = yes(RepnOrEnumCsharp),
            io.write_string(!.Separator, !IO),
            mercury_output_foreign_lang_type_repn_or_enum(CommaSep,
                lang_csharp, RepnOrEnumCsharp, !IO),
            !:Separator = CommaSep
        ),
        (
            MaybeRepnOrEnumErlang = no
        ;
            MaybeRepnOrEnumErlang = yes(RepnOrEnumErlang),
            io.write_string(!.Separator, !IO),
            mercury_output_foreign_lang_type_repn_or_enum(CommaSep,
                lang_erlang, RepnOrEnumErlang, !IO)
        )
    ),
    io.write_string("]", !IO).

:- pred mercury_output_c_j_cs_e_repn(string::in, c_j_cs_e_repn::in,
    io::di, io::uo) is det.

mercury_output_c_j_cs_e_repn(CommaSep, MaybeCJCsERepn, !IO) :-
    MaybeCJCsERepn = c_java_csharp_erlang(MaybeRepnC, MaybeRepnJava,
        MaybeRepnCsharp, MaybeRepnErlang),
    io.write_string("[", !IO),
    some [!Separator] (
        !:Separator = "",
        (
            MaybeRepnC = no
        ;
            MaybeRepnC = yes(RepnC),
            mercury_output_foreign_lang_type_repn(lang_c, RepnC, !IO),
            !:Separator = CommaSep
        ),
        (
            MaybeRepnJava = no
        ;
            MaybeRepnJava = yes(RepnJava),
            io.write_string(!.Separator, !IO),
            mercury_output_foreign_lang_type_repn(lang_java, RepnJava, !IO),
            !:Separator = CommaSep
        ),
        (
            MaybeRepnCsharp = no
        ;
            MaybeRepnCsharp = yes(RepnCsharp),
            io.write_string(!.Separator, !IO),
            mercury_output_foreign_lang_type_repn(lang_csharp, RepnCsharp,
                !IO),
            !:Separator = CommaSep
        ),
        (
            MaybeRepnErlang = no
        ;
            MaybeRepnErlang = yes(RepnErlang),
            io.write_string(!.Separator, !IO),
            mercury_output_foreign_lang_type_repn(lang_erlang, RepnErlang, !IO)
        )
    ),
    io.write_string("]", !IO).

:- pred mercury_output_foreign_lang_type_repn_or_enum(string::in,
    foreign_language::in, enum_foreign_repn::in, io::di, io::uo) is det.

mercury_output_foreign_lang_type_repn_or_enum(CommaSep, Lang,
        TypeRepnOrEnum, !IO) :-
    (
        TypeRepnOrEnum = enum_foreign_type(TypeRepn),
        io.write_string("foreign_type(", !IO),
        mercury_output_foreign_lang_type_repn(Lang, TypeRepn, !IO),
        io.write_string(")", !IO)
    ;
        TypeRepnOrEnum = enum_foreign_enum(Enums),
        Enums = one_or_more(HeadEnum, TailEnums),
        simple_foreign_language_string(Lang, LangStr),
        io.format("foreign_enum(%s, [", [s(LangStr)], !IO),
        mercury_output_enum_functor_names(CommaSep, HeadEnum, TailEnums, !IO),
        io.write_string("])", !IO)
    ).

:- pred mercury_output_foreign_lang_type_repn(foreign_language::in,
    foreign_type_repn::in, io::di, io::uo) is det.

mercury_output_foreign_lang_type_repn(Lang, TypeRepn, !IO) :-
    TypeRepn = foreign_type_repn(ForeignTypeName, ForeignTypeAssertions),
    ForeignTypeAssertions = foreign_type_assertions(Assertions),
    set.to_sorted_list(Assertions, AssertionsList),
    ( Lang = lang_c, LangStr = "c"
    ; Lang = lang_java, LangStr = "java"
    ; Lang = lang_csharp, LangStr = "csharp"
    ; Lang = lang_erlang, LangStr = "erlang"
    ),
    io.format("%s(\"%s\", [", [s(LangStr), s(ForeignTypeName)], !IO),
    io.write_list(AssertionsList, ", ",
        mercury_output_foreign_type_assertion, !IO),
    io.write_string("])", !IO).

%---------------------%

:- pred mercury_output_gen_du_functors(gen_du_functor::in,
    list(gen_du_functor)::in, io::di, io::uo) is det.

mercury_output_gen_du_functors(Functor, Functors, !IO) :-
    (
        Functors = [],
        mercury_output_gen_du_functor(Functor, "", !IO)
    ;
        Functors = [HeadFunctor | TailFunctors],
        mercury_output_gen_du_functor(Functor, ",\n", !IO),
        mercury_output_gen_du_functors(HeadFunctor, TailFunctors, !IO)
    ).

:- pred mercury_output_gen_du_functor(gen_du_functor::in, string::in,
    io::di, io::uo) is det.

mercury_output_gen_du_functor(Functor, Suffix, !IO) :-
    (
        Functor = gen_du_constant_functor(FunctorName,
            SectagSize64, SectagSize32),
        io.write_string("constant_functor(", !IO),
        term_io.quote_string(FunctorName, !IO),
        io.write_string(", ", !IO),
        mercury_output_sectag_size(SectagSize64, !IO),
        io.write_string(", ", !IO),
        mercury_output_sectag_size(SectagSize32, !IO),
        io.format(")%s", [s(Suffix)], !IO)
    ;
        Functor = gen_du_nonconstant_functor(FunctorName,
            Tags64, Tags32, Args64, Args32),
        Args64 = one_or_more(HeadArg64, TailArgs64),
        Args32 = one_or_more(HeadArg32, TailArgs32),
        io.write_string("nonconstant_functor(", !IO),
        term_io.quote_string(FunctorName, !IO),
        io.write_string(", ", !IO),
        mercury_output_ptag_sectag(Tags64, !IO),
        io.write_string(", ", !IO),
        mercury_output_ptag_sectag(Tags32, !IO),
        io.write_string(", ", !IO),
        mercury_output_maybe_direct_args(HeadArg64, TailArgs64, !IO),
        io.write_string(", ", !IO),
        mercury_output_maybe_direct_args(HeadArg32, TailArgs32, !IO),
        io.format(")%s", [s(Suffix)], !IO)
    ).

:- pred mercury_output_maybe_direct_args(maybe_direct_arg::in,
    list(maybe_direct_arg)::in, io::di, io::uo) is det.

mercury_output_maybe_direct_args(Arg, Args, !IO) :-
    (
        Arg = direct_arg(Ptag),
        io.format("direct_arg(%d)", [i(uint.cast_to_int(Ptag))], !IO)
    ;
        Arg = nondirect_arg(ArgPosSize),
        io.write_string("nondirect_arg(", !IO),
        mercury_output_arg_pos_size(ArgPosSize, !IO),
        io.write_string(")", !IO)
    ),
    (
        Args = []
    ;
        Args = [HeadArg | TailArgs],
        io.write_string(", ", !IO),
        mercury_output_maybe_direct_args(HeadArg, TailArgs, !IO)
    ).

%---------------------%

:- pred mercury_output_only_functor_args(gen_du_only_functor_args::in,
    io::di, io::uo) is det.

mercury_output_only_functor_args(OnlyFunctorArgs, !IO) :-
    (
        OnlyFunctorArgs = gen_du_only_functor_local_args(LocalArgs),
        LocalArgs = one_or_more(HeadLocalArg, TailLocalArgs),
        io.write_string("local_args([", !IO),
        mercury_output_only_functor_local_args(HeadLocalArg, TailLocalArgs,
            !IO),
        io.write_string("])", !IO)
    ;
        OnlyFunctorArgs = gen_du_only_functor_remote_args(RemoteArgs),
        RemoteArgs = one_or_more(HeadRemoteArg, TailRemoteArgs),
        io.write_string("remote_args([", !IO),
        mercury_output_only_functor_remote_args(HeadRemoteArg, TailRemoteArgs,
            !IO),
        io.write_string("])", !IO)
    ).

:- pred mercury_output_only_functor_local_args(local_pos_size::in,
    list(local_pos_size)::in, io::di, io::uo) is det.

mercury_output_only_functor_local_args(LocalArg, LocalArgs, !IO) :-
    mercury_output_local_pos_size(LocalArg, !IO),
    (
        LocalArgs = []
    ;
        LocalArgs = [HeadLocalArg | TailLocalArgs],
        io.write_string(", ", !IO),
        mercury_output_only_functor_local_args(HeadLocalArg,
            TailLocalArgs, !IO)
    ).

:- pred mercury_output_only_functor_remote_args(arg_pos_size::in,
    list(arg_pos_size)::in, io::di, io::uo) is det.

mercury_output_only_functor_remote_args(RemoteArg, RemoteArgs, !IO) :-
    mercury_output_arg_pos_size(RemoteArg, !IO),
    (
        RemoteArgs = []
    ;
        RemoteArgs = [HeadRemoteArg | TailRemoteArgs],
        io.write_string(", ", !IO),
        mercury_output_only_functor_remote_args(HeadRemoteArg,
            TailRemoteArgs, !IO)
    ).

%---------------------%

:- pred mercury_output_ptag_sectag(ptag_sectag::in, io::di, io::uo) is det.

mercury_output_ptag_sectag(PtagSectag, !IO) :-
    PtagSectag = ptag_sectag(PtagUint, MaybeSectag),
    Ptag = uint.cast_to_int(PtagUint),
    (
        MaybeSectag = no_sectag,
        io.format("ptag_only(%d)", [i(Ptag)], !IO)
    ;
        (
            MaybeSectag = local_sectag(SectagUint, SectagSize),
            Locn = "local"
        ;
            MaybeSectag = remote_sectag(SectagUint, SectagSize),
            Locn = "remote"
        ),
        Sectag = uint.cast_to_int(SectagUint),
        (
            SectagSize = sectag_rest_of_word,
            io.format("ptag_%s_sectag(%d, %d)",
                [s(Locn), i(Ptag), i(Sectag)], !IO)
        ;
            SectagSize = sectag_bits(NumBitsUint),
            NumBits = uint.cast_to_int(NumBitsUint),
            io.format("ptag_%s_sectag_bits(%d, %d, %d)",
                [s(Locn), i(Ptag), i(Sectag), i(NumBits)], !IO)
        )
    ).

:- pred mercury_output_sectag_size(sectag_size::in, io::di, io::uo) is det.

mercury_output_sectag_size(SectagSize, !IO) :-
    (
        SectagSize = sectag_rest_of_word,
        io.write_string("sectag_rest_of_word", !IO)
    ;
        SectagSize = sectag_bits(NumBits),
        io.format("sectag_bits(%d)", [i(uint.cast_to_int(NumBits))], !IO)
    ).

:- pred mercury_output_local_pos_size(local_pos_size::in,
    io::di, io::uo) is det.

mercury_output_local_pos_size(ShiftWidthFill, !IO) :-
    ShiftWidthFill = local_pos_size(Shift, FillKindSize),
    io.format("local(%d, %s)",
        [i(uint.cast_to_int(Shift)),
        s(fill_kind_size_to_string(FillKindSize))], !IO).

:- pred mercury_output_arg_pos_size(arg_pos_size::in, io::di, io::uo) is det.

mercury_output_arg_pos_size(ArgPosSize, !IO) :-
    (
        ArgPosSize = pos_full(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        io.format("full(%d, %d)",
            [i(ArgOnlyOffsetInt), i(CellOffsetInt)], !IO)
    ;
        ArgPosSize = pos_double(ArgOnlyOffset, CellOffset, DoubleKind),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        double_word_kind_string(DoubleKind, DoubleKindStr),
        io.format("double(%d, %d, %s)",
            [i(ArgOnlyOffsetInt), i(CellOffsetInt), s(DoubleKindStr)], !IO)
    ;
        (
            ArgPosSize = pos_partial_first(ArgOnlyOffset, CellOffset,
                Shift, FillKindSize),
            FirstOrShifted = "first"
        ;
            ArgPosSize = pos_partial_shifted(ArgOnlyOffset, CellOffset,
                Shift, FillKindSize),
            FirstOrShifted = "shifted"
        ),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        io.format("partial_%s(%d, %d, %d, %s)",
            [s(FirstOrShifted), i(ArgOnlyOffsetInt), i(CellOffsetInt),
            i(uint.cast_to_int(Shift)),
            s(fill_kind_size_to_string(FillKindSize))], !IO)
    ;
        ArgPosSize = pos_none_shifted(ArgOnlyOffset, CellOffset),
        ArgOnlyOffset = arg_only_offset(ArgOnlyOffsetInt),
        CellOffset = cell_offset(CellOffsetInt),
        io.format("none_shifted(%d, %d)",
            [i(ArgOnlyOffsetInt), i(CellOffsetInt)], !IO)
    ;
        ArgPosSize = pos_none_nowhere,
        io.write_string("none_nowhere", !IO)
    ).

:- func fill_kind_size_to_string(fill_kind_size) = string.

fill_kind_size_to_string(FillKindSize) = Str :-
    (
        FillKindSize = fk_enum(NumBits),
        string.format("enum(%d)", [i(uint.cast_to_int(NumBits))], Str)
    ;
        ( FillKindSize = fk_int8,   Str = "int8"
        ; FillKindSize = fk_int16,  Str = "int16"
        ; FillKindSize = fk_int32,  Str = "int32"
        ; FillKindSize = fk_uint8,  Str = "uint8"
        ; FillKindSize = fk_uint16, Str = "uint16"
        ; FillKindSize = fk_uint32, Str = "uint32"
        ; FillKindSize = fk_char21, Str = "char21"
        )
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out.
%---------------------------------------------------------------------------%
