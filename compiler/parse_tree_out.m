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
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_output.
:- import_module parse_tree.prog_item.

:- import_module io.
:- import_module string.
:- import_module string.builder.

%---------------------------------------------------------------------------%

    % output_parse_tree_*(ProgressStream, Info, Globals, OutputFileName,
    %   ParseTree, Succeeded, !IO)

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

    % mercury_output_parse_tree_*(Info, FileStream, ParseTree, !IO)
    % mercury_format_parse_tree_*(Info, S, ParseTree, !U)

:- pred mercury_output_parse_tree_src(merc_out_info::in,
    io.text_output_stream::in, parse_tree_src::in, io::di, io::uo) is det.
:- pred mercury_format_parse_tree_src(merc_out_info::in,
    S::in, parse_tree_src::in, U::di, U::uo) is det <= pt_output(S, U).

:- pred mercury_output_parse_tree_module_src(merc_out_info::in,
    io.text_output_stream::in, parse_tree_module_src::in,
    io::di, io::uo) is det.
:- pred mercury_format_parse_tree_module_src(merc_out_info::in, S::in,
    parse_tree_module_src::in, U::di, U::uo) is det <= pt_output(S, U).

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

%---------------------------------------------------------------------------%

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

:- pragma type_spec_constrained_preds([pt_output(Stream, State)],
    apply_to_superclasses,
    [subst([Stream => io.text_output_stream, State = io.state]),
    subst([Stream => string.builder.handle, State = string.builder.state])]).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.canonicalize_interface.
:- import_module parse_tree.item_util.
:- import_module parse_tree.parse_tree_out_clause.
:- import_module parse_tree.parse_tree_out_item.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.parse_tree_out_pragma.
:- import_module parse_tree.parse_tree_out_sym_name.
:- import_module parse_tree.parse_tree_out_type_repn.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module recompilation.
:- import_module recompilation.item_types.
:- import_module recompilation.version.

:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module term_context.

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

        Info = init_merc_out_info(Globals, unqualified_item_names,
            output_mercury),
        OutputParseTree(Info, FileStream, ParseTree, !IO),
        io.close_output(FileStream, !IO),
        (
            Verbose = yes,
            io.write_string(ProgressStream, " done\n", !IO),
            io.flush_output(ProgressStream, !IO)
        ;
            Verbose = no
        ),
        Succeeded = succeeded
    ;
        Res = error(_),
        io.format(ProgressStream,
            "Error: couldn't open file `%s' for output.\n",
            [s(OutputFileName)], !IO),
        io.flush_output(ProgressStream, !IO),
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

%---------------------------------------------------------------------------%
:- end_module parse_tree.parse_tree_out.
%---------------------------------------------------------------------------%
