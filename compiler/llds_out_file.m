%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009-2011 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: llds_out_file.m.
% Main authors: conway, fjh, zs.
%
% This module defines the top level routines for printing out LLDS modules.
% It looks after printing out global data, procedures (but not instructions),
% and module initialization functions.
%
%----------------------------------------------------------------------------%

:- module ll_backend.llds_out.llds_out_file.
:- interface.

:- import_module libs.
:- import_module libs.globals.
:- import_module libs.maybe_util.
:- import_module ll_backend.llds.

:- import_module bool.
:- import_module io.

%----------------------------------------------------------------------------%

    % Given a c_file structure, output the LLDS code inside it into a .c file.
    %
:- pred output_llds(globals::in, c_file::in, maybe_succeeded::out,
    io::di, io::uo) is det.

%----------------------------------------------------------------------------%

    % c_data_linkage_string(DefaultLinkage, BeingDefined):
    %
    % Return a C string that gives the storage class appropriate for the
    % definition of a global variable with the specified properties.
    %
:- func c_data_linkage_string(linkage, bool) = string.

    % Given a boolean that states whether a data item includes code
    % addresses or not, return a C string that gives its "const-ness".
    %
:- func c_data_const_string(globals, bool) = string.

%----------------------------------------------------------------------------%

% Note that we need to know the linkage not just at the definition,
% but also at every use, because if the use is prior to the definition,
% then we need to declare the name first, and the linkage used in that
% declaration must be consistent with the linkage in the definition.
% For this reason, the field in c_data (which holds the information about
% the definition) which says whether or not a data name is exported
% is not useful. Instead, we need to determine whether or not something
% is exported from its `data_name'.

:- type linkage
    --->    extern
    ;       static.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.optimization_options.
:- import_module libs.trace_params.
:- import_module ll_backend.exprn_aux.
:- import_module ll_backend.layout.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module ll_backend.llds_out.llds_out_global.
:- import_module ll_backend.llds_out.llds_out_instr.
:- import_module ll_backend.llds_out.llds_out_util.
:- import_module ll_backend.rtti_out.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.file_names.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_foreign.

:- import_module cord.
:- import_module int.
:- import_module library.   % for the version number.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module term_context.

%----------------------------------------------------------------------------%

output_llds(Globals, CFile, Succeeded, !IO) :-
    ModuleName = CFile ^ cfile_modulename,
    module_name_to_file_name(Globals, $pred, do_create_dirs,
        ext_other(other_ext(".c")), ModuleName, FileName, !IO),
    output_to_file_stream(Globals, ModuleName, FileName,
        output_llds_2(Globals, CFile), Succeeded, !IO).

:- pred output_llds_2(globals::in, c_file::in, io.text_output_stream::in,
    list(string)::out, io::di, io::uo) is det.

output_llds_2(Globals, CFile, Stream, Errors, !IO) :-
    decl_set_init(DeclSet0),
    output_single_c_file(Globals, Stream, CFile, Errors, DeclSet0, _, !IO).

:- pred output_single_c_file(globals::in, io.text_output_stream::in,
    c_file::in, list(string)::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_single_c_file(Globals, Stream, CFile, Errors, !DeclSet, !IO) :-
    CFile = c_file(ModuleName, C_HeaderLines, ForeignBodyCodes, Exports,
        TablingInfoStructs, ScalarCommonDatas, VectorCommonDatas,
        RttiDatas, PseudoTypeInfos, HLDSVarNums, ShortLocns, LongLocns,
        UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TSStringTable,
        TableIoEntries, TableIoEntryMap, ProcEventLayouts, ExecTraces,
        ProcLayoutDatas, ModuleLayoutDatas, ClosureLayoutDatas,
        AllocSites, AllocSiteMap,
        Modules, UserInitPredCNames, UserFinalPredCNames, ComplexityProcs),
    library.version(Version, Fullarch),
    module_name_to_source_file_name(ModuleName, SourceFileName, !IO),
    output_c_file_intro_and_grade(Globals, Stream, SourceFileName,
        Version, Fullarch, !IO),

    Info = init_llds_out_info(ModuleName, SourceFileName, Globals,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap, TableIoEntryMap,
        AllocSiteMap),
    annotate_c_modules(Info, Modules, AnnotatedModules,
        cord.init, EntryLabelsCord, cord.init, InternalLabelsCord,
        set.init, EnvVarNameSet),
    EntryLabels = cord.list(EntryLabelsCord),
    InternalLabels = cord.list(InternalLabelsCord),
    EnvVarNames = set.to_sorted_list(EnvVarNameSet),
    output_init_c_comment(Stream, ModuleName,
        UserInitPredCNames, UserFinalPredCNames, EnvVarNames, !IO),
    output_c_file_mercury_headers(Info, Stream, !IO),

    output_foreign_header_include_lines(Info, Stream, C_HeaderLines,
        ForeignIncludeResults, !IO),
    io.write_string(Stream, "\n", !IO),

    output_static_linkage_define(Stream, !IO),
    list.foldl2(output_scalar_common_data_decl(Stream), ScalarCommonDatas,
        !DeclSet, !IO),
    list.foldl2(output_vector_common_data_decl(Stream), VectorCommonDatas,
        !DeclSet, !IO),
    output_rtti_data_decl_list(Info, Stream, RttiDatas, !DeclSet, !IO),
    output_record_c_label_decls(Info, Stream, EntryLabels, InternalLabels,
        !DeclSet, !IO),
    list.foldl2(output_tabling_info_struct(Info, Stream), TablingInfoStructs,
        !DeclSet, !IO),
    list.foldl2(output_scalar_common_data_defn(Info, Stream),
        ScalarCommonDatas, !DeclSet, !IO),
    list.foldl2(output_vector_common_data_defn(Info, Stream),
        VectorCommonDatas, !DeclSet, !IO),
    list.foldl2(output_rtti_data_defn(Info, Stream), RttiDatas, !DeclSet, !IO),

    io.nl(Stream, !IO),
    output_layout_array_decls(Info, Stream, PseudoTypeInfos, HLDSVarNums,
        ShortLocns, LongLocns, UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TableIoEntries,
        ProcEventLayouts, ExecTraces, AllocSites, !IO),

    list.foldl2(output_proc_layout_data_defn(Info, Stream),
        ProcLayoutDatas, !DeclSet, !IO),
    list.foldl2(output_module_layout_data_defn(Info, Stream),
        ModuleLayoutDatas, !DeclSet, !IO),
    list.foldl2(output_closure_layout_data_defn(Info, Stream),
        ClosureLayoutDatas, !DeclSet, !IO),
    io.nl(Stream, !IO),

    output_record_rvals_decls(Info, Stream, PseudoTypeInfos, !DeclSet, !IO),
    output_layout_array_defns(Info, Stream, PseudoTypeInfos, HLDSVarNums,
        ShortLocns, LongLocns, UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TableIoEntries,
        ProcEventLayouts, ExecTraces, TSStringTable, AllocSites,
        !DeclSet, !IO),

    list.map_foldl(output_foreign_body_code(Info, Stream), ForeignBodyCodes,
        ForeignCodeResults, !IO),
    list.foldl2(output_annotated_c_module(Info, Stream), AnnotatedModules,
        !DeclSet, !IO),
    WriteForeignExportDefn =
        (pred(ForeignExportDefn::in, IO0::di, IO::uo) is det :-
            ForeignExportDefn = foreign_export_defn(ForeignExportCode),
            io.write_string(Stream, ForeignExportCode, IO0, IO)
        ),
    list.foldl(WriteForeignExportDefn, Exports, !IO),
    io.write_string(Stream, "\n", !IO),
    output_c_module_init_list(Info, Stream, ModuleName, AnnotatedModules,
        RttiDatas, ProcLayoutDatas, ModuleLayoutDatas, ComplexityProcs,
        TSStringTable, AllocSites, UserInitPredCNames, UserFinalPredCNames,
        !DeclSet, !IO),

    list.filter_map(maybe_is_error, ForeignIncludeResults, ErrorsA),
    list.filter_map(maybe_is_error, ForeignCodeResults, ErrorsB),
    Errors = ErrorsA ++ ErrorsB.

%-----------------------------------------------------------------------------%

:- pred output_c_file_mercury_headers(llds_out_info::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_c_file_mercury_headers(Info, Stream, !IO) :-
    io.write_string(Stream, "#define MR_ALLOW_RESET\n", !IO),
    io.write_string(Stream, "#include ""mercury_imp.h""\n", !IO),
    TraceLevel = Info ^ lout_trace_level,
    TraceEnabled = is_exec_trace_enabled_at_given_trace_level(TraceLevel),
    (
        TraceEnabled = exec_trace_is_enabled,
        io.write_string(Stream, "#include ""mercury_trace_base.h""\n", !IO)
    ;
        TraceEnabled = exec_trace_is_not_enabled
    ),
    DeepProfile = Info ^ lout_profile_deep,
    (
        DeepProfile = yes,
        io.write_string(Stream, "#include ""mercury_deep_profiling.h""\n", !IO)
    ;
        DeepProfile = no
    ),
    GenerateBytecode = Info ^ lout_generate_bytecode,
    (
        GenerateBytecode = yes,
        io.write_string(Stream, "#include ""mb_interface_stub.h""\n", !IO)
    ;
        GenerateBytecode = no
    ).

%----------------------------------------------------------------------------%

% We need the set of entry and internal labels in a C module in several places
% for several purposes, so we compute them just once, and record the results.
%
% The set of internal labels that the generated C code actually defines
% will NOT include labels that actually occur in the LLDS code, but which
% start C loops that, and which are not referred to from anywhere except
% inside the loop; all those references get translated to C "continue"
% statements, and the label is optimized away. This is why the computation
% of the set of internal labels defined by C module must also compute
% the set of while labels, and undefined while labels. We record the results
% of this computation in the label_output_info next to the c_procedure.

:- type annotated_c_module
    --->    annotated_c_module(
                acm_name                :: string,
                acm_procs               :: list(annotated_c_procedure),
                acm_entry_labels        :: list(label),
                acm_internal_labels     :: list(label)
            ).

:- type annotated_c_procedure
    --->    annotated_c_procedure(
                acp_proc                :: c_procedure,
                acp_label_output_info   :: label_output_info
            ).

:- pred annotate_c_modules(llds_out_info::in,
    list(comp_gen_c_module)::in, list(annotated_c_module)::out,
    cord(label)::in, cord(label)::out, cord(label)::in, cord(label)::out,
    set(string)::in, set(string)::out) is det.

annotate_c_modules(_, [], [], !EntryLabels, !InternalLabels, !EnvVarNames).
annotate_c_modules(Info,
        [Module | Modules], [AnnotatedModule | AnnotatedModules],
        !EntryLabels, !InternalLabels, !EnvVarNames) :-
    annotate_c_module(Info, Module, AnnotatedModule,
        !EntryLabels, !InternalLabels, !EnvVarNames),
    annotate_c_modules(Info, Modules, AnnotatedModules,
        !EntryLabels, !InternalLabels, !EnvVarNames).

:- pred annotate_c_module(llds_out_info::in,
    comp_gen_c_module::in, annotated_c_module::out,
    cord(label)::in, cord(label)::out, cord(label)::in, cord(label)::out,
    set(string)::in, set(string)::out) is det.

annotate_c_module(Info, Module, AnnotatedModule,
        !AllEntryLabels, !AllInternalLabels, !EnvVarNames) :-
    Module = comp_gen_c_module(ModuleName, Procs),
    annotate_c_procedures(Info, Procs, AnnotatedProcs,
        cord.init, ModuleEntryLabels, cord.init, ModuleInternalLabels,
        !EnvVarNames),
    ModuleEntryLabelList = cord.list(ModuleEntryLabels),
    ModuleInternalLabelList = cord.list(ModuleInternalLabels),
    AnnotatedModule = annotated_c_module(ModuleName, AnnotatedProcs,
        ModuleEntryLabelList, ModuleInternalLabelList),
    !:AllEntryLabels = !.AllEntryLabels ++ ModuleEntryLabels,
    !:AllInternalLabels = !.AllInternalLabels ++ ModuleInternalLabels.

:- pred annotate_c_procedures(llds_out_info::in,
    list(c_procedure)::in, list(annotated_c_procedure)::out,
    cord(label)::in, cord(label)::out, cord(label)::in, cord(label)::out,
    set(string)::in, set(string)::out) is det.

annotate_c_procedures(_, [], [],
        !AllEntryLabels, !AllInternalLabels, !EnvVarNames).
annotate_c_procedures(Info, [Proc | Procs], [AnnotatedProc | AnnotatedProcs],
        !AllEntryLabels, !AllInternalLabels, !EnvVarNames) :-
    annotate_c_procedure(Info, Proc, AnnotatedProc,
        !AllEntryLabels, !AllInternalLabels, !EnvVarNames),
    annotate_c_procedures(Info, Procs, AnnotatedProcs,
        !AllEntryLabels, !AllInternalLabels, !EnvVarNames).

:- pred annotate_c_procedure(llds_out_info::in,
    c_procedure::in, annotated_c_procedure::out,
    cord(label)::in, cord(label)::out, cord(label)::in, cord(label)::out,
    set(string)::in, set(string)::out) is det.

annotate_c_procedure(Info, Proc, AnnotatedProc,
        !AllEntryLabels, !AllInternalLabels, !EnvVarNames) :-
    ProcEnvVarNames = Proc ^ cproc_c_global_vars,
    set.union(ProcEnvVarNames, !EnvVarNames),

    Instrs = Proc ^ cproc_code,
    gather_labels_from_instrs_acc(Instrs,
        [], RevEntryLabels, [], RevInternalLabels0),
    list.reverse(RevEntryLabels, EntryLabels),
    list.reverse(RevInternalLabels0, InternalLabels0),

    find_caller_label(Instrs, CallerLabel),
    find_cont_labels(Instrs, set_tree234.init, ContLabels),
    EmitCLoops = Info ^ lout_emit_c_loops,
    (
        EmitCLoops = do_not_emit_c_loops,
        WhileLabels = set_tree234.init,
        UndefWhileLabels = set_tree234.init
    ;
        EmitCLoops = emit_c_loops,
        find_while_labels(Instrs, set_tree234.init, WhileLabels),

        % We compute UndefWhileLabels by starting with an overapproximation,
        % which we then whittle down in two steps. Each whittling step
        % removes from our initial UndefWhileLabels set some labels that
        % actually do need to be defined.
        %
        % Step 1: if a label is in ContLabels, we cannot avoid defining it.
        set_tree234.difference(WhileLabels, ContLabels, UndefWhileLabels0),
        ( if set_tree234.is_empty(UndefWhileLabels0) then
            UndefWhileLabels = UndefWhileLabels0
        else
            % Step 2: if a label that starts a while loop is branched to
            % from outside the while loop it starts, we cannot avoid
            % defining it.
            find_while_labels_to_define(Instrs, no, WhileLabels,
                UndefWhileLabels0, UndefWhileLabels)
        )
    ),
    LabelOutputInfo = label_output_info(CallerLabel, ContLabels,
        WhileLabels, UndefWhileLabels),
    ( if set_tree234.is_empty(UndefWhileLabels) then
        InternalLabels = InternalLabels0
    else
        list.negated_filter(set_tree234.contains(UndefWhileLabels),
            InternalLabels0, InternalLabels)
    ),
    AnnotatedProc = annotated_c_procedure(Proc, LabelOutputInfo),

    !:AllEntryLabels = !.AllEntryLabels ++ cord.from_list(EntryLabels),
    !:AllInternalLabels = !.AllInternalLabels ++
        cord.from_list(InternalLabels).

:- pred gather_labels_from_instrs_acc(list(instruction)::in,
    list(label)::in, list(label)::out,
    list(label)::in, list(label)::out) is det.

gather_labels_from_instrs_acc([], !RevEntryLabels, !RevInternalLabels).
gather_labels_from_instrs_acc([Instr | Instrs],
        !RevEntryLabels, !RevInternalLabels) :-
    ( if Instr = llds_instr(label(Label), _) then
        (
            Label = entry_label(_, _),
            !:RevEntryLabels = [Label | !.RevEntryLabels]
        ;
            Label = internal_label(_, _),
            !:RevInternalLabels = [Label | !.RevInternalLabels]
        )
    else
        true
    ),
    gather_labels_from_instrs_acc(Instrs,
        !RevEntryLabels, !RevInternalLabels).

%----------------------------------------------------------------------------%

:- pred output_c_module_init_list(llds_out_info::in,
    io.text_output_stream::in, module_name::in,
    list(annotated_c_module)::in, list(rtti_data)::in,
    list(proc_layout_data)::in, list(module_layout_data)::in,
    list(complexity_proc_info)::in, list(string)::in,
    list(alloc_site_info)::in, list(string)::in, list(string)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_module_init_list(Info, Stream, ModuleName, AnnotatedModules,
        RttiDatas, ProcLayoutDatas, ModuleLayoutDatas, ComplexityProcs,
        TSStringTable, AllocSites, InitPredNames, FinalPredNames,
        !DeclSet, !IO) :-
    MustInit =
        ( pred(Module::in) is semidet :-
            module_defines_label_with_layout(Info, Module)
        ),
    list.filter(MustInit, AnnotatedModules,
        AlwaysInitAnnotatedModules, MaybeInitAnnotatedModules),
    list.chunk(AlwaysInitAnnotatedModules, 40,
        AlwaysInitAnnotatedModuleBunches),
    list.chunk(MaybeInitAnnotatedModules, 40,
        MaybeInitAnnotatedModuleBunches),

    output_init_bunch_defs(Info, Stream, "always", 0,
        AlwaysInitAnnotatedModuleBunches, !IO),
    (
        MaybeInitAnnotatedModuleBunches = []
    ;
        MaybeInitAnnotatedModuleBunches = [_ | _],
        output_init_bunch_defs(Info, Stream, "maybe", 0,
            MaybeInitAnnotatedModuleBunches, !IO)
    ),

    ModuleInitName = make_init_name(ModuleName),

    io.write_string(Stream,
        "/* suppress gcc -Wmissing-decls warnings */\n", !IO),
    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init(void);\n", !IO),

    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_type_tables(void);\n", !IO),
    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_debugger(void);\n", !IO),

    io.write_string(Stream, "#ifdef MR_DEEP_PROFILING\n", !IO),
    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, 
        "write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp);\n", !IO),
    io.write_string(Stream, "#endif\n", !IO),

    io.write_string(Stream, "#ifdef MR_RECORD_TERM_SIZES\n", !IO),
    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_complexity_procs(void);\n", !IO),
    io.write_string(Stream, "#endif\n", !IO),

    io.write_string(Stream, "#ifdef MR_THREADSCOPE\n", !IO),
    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_threadscope_string_table(void);\n", !IO),
    io.write_string(Stream, "#endif\n", !IO),

    (
        InitPredNames = []
    ;
        InitPredNames = [_ | _],
        io.format(Stream, "void %srequired_init(void);\n",
            [s(ModuleInitName)], !IO)
    ),

    (
        FinalPredNames = []
    ;
        FinalPredNames = [_ | _],
        io.format(Stream, "void %srequired_final(void);\n",
            [s(ModuleInitName)], !IO)
    ),

    io.format(Stream, "const char *%sgrade_check(void);\n",
        [s(ModuleInitName)], !IO),

    io.write_string(Stream, "\n", !IO),

    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init(void)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    io.write_string(Stream, "\tstatic MR_bool done = MR_FALSE;\n", !IO),
    io.write_string(Stream, "\tif (done) {\n", !IO),
    io.write_string(Stream, "\t\treturn;\n", !IO),
    io.write_string(Stream, "\t}\n", !IO),
    io.write_string(Stream, "\tdone = MR_TRUE;\n", !IO),

    output_init_bunch_calls(Info, Stream, "always", 0,
        AlwaysInitAnnotatedModuleBunches, !IO),
    (
        MaybeInitAnnotatedModuleBunches = []
    ;
        MaybeInitAnnotatedModuleBunches = [_ | _],
        output_init_bunch_calls(Info, Stream, "maybe", 0,
            MaybeInitAnnotatedModuleBunches, !IO)
    ),

    output_c_data_init_list(Stream, RttiDatas, !IO),
    output_alloc_sites_init(Info, Stream, AllocSites, !IO),

    % The call to the debugger initialization function is for bootstrapping;
    % once the debugger has been modified to call do_init_modules_debugger()
    % and all debuggable object files created before this change have been
    % overwritten, it can be deleted.
    io.write_string(Stream, "\t", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_debugger();\n", !IO),
    io.write_string(Stream, "}\n\n", !IO),

    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_type_tables(void)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    io.write_string(Stream, "\tstatic MR_bool done = MR_FALSE;\n", !IO),
    io.write_string(Stream, "\tif (done) {\n", !IO),
    io.write_string(Stream, "\t\treturn;\n", !IO),
    io.write_string(Stream, "\t}\n", !IO),
    io.write_string(Stream, "\tdone = MR_TRUE;\n", !IO),
    output_type_tables_init_list(Stream, RttiDatas, !IO),
    io.write_string(Stream, "}\n\n", !IO),

    io.write_string(Stream, "\n", !IO),
    io.write_string(Stream, "void ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_debugger(void)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    io.write_string(Stream, "\tstatic MR_bool done = MR_FALSE;\n", !IO),
    io.write_string(Stream, "\tif (done) {\n", !IO),
    io.write_string(Stream, "\t\treturn;\n", !IO),
    io.write_string(Stream, "\t}\n", !IO),
    io.write_string(Stream, "\tdone = MR_TRUE;\n", !IO),
    output_debugger_init_list(Stream, ModuleLayoutDatas, !IO),
    io.write_string(Stream, "}\n\n", !IO),

    io.write_string(Stream, "#ifdef MR_DEEP_PROFILING\n", !IO),
    io.write_string(Stream, "\nvoid ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, 
        "write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    io.write_string(Stream,
        "\tMR_write_out_module_proc_reps_start(procrep_fp, &", !IO),
    ModuleLayoutName = module_layout(ModuleName),
    output_layout_name(Stream, ModuleLayoutName, !IO),
    io.write_string(Stream, ");\n", !IO),
    output_write_proc_static_list(Stream, ProcLayoutDatas, !IO),
    io.write_string(Stream,
        "\tMR_write_out_module_proc_reps_end(procrep_fp);\n", !IO),
    io.write_string(Stream, "}\n", !IO),
    io.write_string(Stream, "\n#endif\n\n", !IO),

    io.write_string(Stream, "#ifdef MR_RECORD_TERM_SIZES\n", !IO),
    output_complexity_arg_info_arrays(Stream, ComplexityProcs, !IO),
    io.write_string(Stream, "\nvoid ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_complexity_procs(void)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    output_init_complexity_proc_list(Stream, ComplexityProcs, !IO),
    io.write_string(Stream, "}\n", !IO),
    io.write_string(Stream, "\n#endif\n\n", !IO),

    io.write_string(Stream, "#ifdef MR_THREADSCOPE\n", !IO),
    io.write_string(Stream, "\nvoid ", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "init_threadscope_string_table(void)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    (
        TSStringTable = []
    ;
        TSStringTable = [_ | _],
        TSStringTableSize = length(TSStringTable),
        io.write_string(Stream,
            "\tMR_threadscope_register_strings_array(\n", !IO),
        io.write_string(Stream, "\t\t", !IO),
        MangledModuleName = Info ^ lout_mangled_module_name,
        output_layout_array_name(Stream, use_layout_macro, MangledModuleName,
            threadscope_string_table_array, !IO),
        io.format(Stream, ", %d);\n", [i(TSStringTableSize)], !IO)
    ),
    io.write_string(Stream, "}\n", !IO),
    io.write_string(Stream, "\n#endif\n\n", !IO),

    (
        InitPredNames = []
    ;
        InitPredNames = [_ | _],
        io.write_string(Stream, "void ", !IO),
        io.write_string(Stream, ModuleInitName,  !IO),
        io.write_string(Stream, "required_init(void)\n", !IO),
        io.write_string(Stream, "{\n", !IO),
        output_required_init_or_final_calls(Stream, InitPredNames, !IO),
        io.write_string(Stream, "}\n", !IO),
        io.nl(Stream, !IO)
    ),

    (
        FinalPredNames = []
    ;
        FinalPredNames = [_ | _],
        io.write_string(Stream, "void ", !IO),
        io.write_string(Stream, ModuleInitName,  !IO),
        io.write_string(Stream, "required_final(void)\n", !IO),
        io.write_string(Stream, "{\n", !IO),
        output_required_init_or_final_calls(Stream, FinalPredNames, !IO),
        io.write_string(Stream, "}\n", !IO),
        io.nl(Stream, !IO)
    ),

    io.write_string(Stream, 
        "// Ensure everything is compiled with the same grade.\n", !IO),
    io.write_string(Stream, "const char *", !IO),
    io.write_string(Stream, ModuleInitName,  !IO),
    io.write_string(Stream, "grade_check(void)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    io.write_string(Stream, "    return &MR_GRADE_VAR;\n", !IO),
    io.write_string(Stream, "}\n", !IO).

:- pred module_defines_label_with_layout(llds_out_info::in,
    annotated_c_module::in) is semidet.

module_defines_label_with_layout(Info, AnnotatedModule) :-
    AnnotatedModule = annotated_c_module(_, _, EntryLabels, InternalLabels),
    % If a map is empty, there is no point in using it as a filter,
    % and we can save ourselves the cost of a list traversal; we know
    % the traversal wouldn't be able to find anything.
    (
        InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
        \+ map.is_empty(InternalLabelToLayoutMap),
        find_first_match(internal_label_has_layout(InternalLabelToLayoutMap),
            InternalLabels, _)
    ;
        EntryLabelToLayoutMap = Info ^ lout_entry_label_to_layout,
        \+ map.is_empty(EntryLabelToLayoutMap),
        find_first_match(entry_label_has_layout(EntryLabelToLayoutMap),
            EntryLabels, _)
    ).

:- pred internal_label_has_layout(map(label, layout_slot_name)::in, label::in)
    is semidet.

internal_label_has_layout(InternalLabelToLayoutMap, Label) :-
    map.search(InternalLabelToLayoutMap, Label, _).

:- pred entry_label_has_layout(map(label, data_id)::in, label::in)
    is semidet.

entry_label_has_layout(EntryLabelToLayoutMap, Label) :-
    map.search(EntryLabelToLayoutMap, Label, _).

%----------------------------------------------------------------------------%

:- pred output_init_bunch_defs(llds_out_info::in, io.text_output_stream::in,
    string::in, int::in, list(list(annotated_c_module))::in,
    io::di, io::uo) is det.

output_init_bunch_defs(_, _, _, _, [], !IO).
output_init_bunch_defs(Info, Stream, InitStatus, Seq,
        [Bunch | Bunches], !IO) :-
    io.write_string(Stream, "static void ", !IO),
    output_bunch_name(Info, Stream, InitStatus, Seq, !IO),
    io.write_string(Stream, "(void)\n", !IO),
    io.write_string(Stream, "{\n", !IO),
    output_init_bunch_def(Stream, Bunch, !IO),
    io.write_string(Stream, "}\n\n", !IO),
    NextSeq = Seq + 1,
    output_init_bunch_defs(Info, Stream, InitStatus, NextSeq, Bunches, !IO).

:- pred output_init_bunch_def(io.text_output_stream::in,
    list(annotated_c_module)::in, io::di, io::uo) is det.

output_init_bunch_def(_, [], !IO).
output_init_bunch_def(Stream, [AnnotatedModule | AnnotatedModules], !IO) :-
    C_ModuleName = AnnotatedModule ^ acm_name,
    io.format(Stream, "\t%s();\n", [s(C_ModuleName)], !IO),
    output_init_bunch_def(Stream, AnnotatedModules, !IO).

:- pred output_init_bunch_calls(llds_out_info::in, io.text_output_stream::in,
    string::in, int::in, list(list(annotated_c_module))::in,
    io::di, io::uo) is det.

output_init_bunch_calls(_, _, _, _, [], !IO).
output_init_bunch_calls(Info, Stream, InitStatus, Seq, [_ | Bunches], !IO) :-
    io.write_string(Stream, "\t", !IO),
    output_bunch_name(Info, Stream, InitStatus, Seq, !IO),
    io.write_string(Stream, "();\n", !IO),
    NextSeq = Seq + 1,
    output_init_bunch_calls(Info, Stream, InitStatus, NextSeq, Bunches, !IO).

:- pred output_bunch_name(llds_out_info::in, io.text_output_stream::in,
    string::in, int::in, io::di, io::uo) is det.

output_bunch_name(Info, Stream, InitStatus, Number, !IO) :-
    io.write_string(Stream, "mercury__", !IO),
    MangledModuleName = Info ^ lout_mangled_module_name,
    io.format(Stream, "%s_%s_bunch_%d",
        [s(MangledModuleName), s(InitStatus), i(Number)], !IO).

    % Output MR_INIT_TYPE_CTOR_INFO(TypeCtorInfo, Typector);
    % for each type_ctor_info defined in this module.
    %
:- pred output_c_data_init_list(io.text_output_stream::in,
    list(rtti_data)::in, io::di, io::uo) is det.

output_c_data_init_list(_, [], !IO).
output_c_data_init_list(Stream, [Data | Datas], !IO) :-
    rtti_out.init_rtti_data_if_nec(Stream, Data, !IO),
    output_c_data_init_list(Stream, Datas, !IO).

    % Output code to register the allocation sites defined in this module.
    %
:- pred output_alloc_sites_init(llds_out_info::in, io.text_output_stream::in,
    list(alloc_site_info)::in, io::di, io::uo) is det.

output_alloc_sites_init(Info, Stream, AllocSites, !IO) :-
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        MangledModuleName = Info ^ lout_mangled_module_name,
        NumAllocSites = list.length(AllocSites),
        io.write_string(Stream,
            "#ifdef MR_MPROF_PROFILE_MEMORY_ATTRIBUTION\n", !IO),
        io.write_string(Stream, "\tMR_register_alloc_sites(", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro,
            MangledModuleName, alloc_site_array, !IO),
        io.write_string(Stream, ", ", !IO),
        io.write_int(Stream, NumAllocSites, !IO),
        io.write_string(Stream, ");\n", !IO),
        io.write_string(Stream, "#endif\n", !IO)
    ).

    % Output code to register each type_ctor_info defined in this module.
    %
:- pred output_type_tables_init_list(io.text_output_stream::in,
    list(rtti_data)::in, io::di, io::uo) is det.

output_type_tables_init_list(_, [], !IO).
output_type_tables_init_list(Stream, [Data | Datas], !IO) :-
    rtti_out.register_rtti_data_if_nec(Stream, Data, !IO),
    output_type_tables_init_list(Stream, Datas, !IO).

    % Output calls to MR_register_module_layout()
    % for each module layout defined in this module
    % (there should only be one, of course).
    %
:- pred output_debugger_init_list(io.text_output_stream::in,
    list(module_layout_data)::in, io::di, io::uo) is det.

output_debugger_init_list(_, [], !IO).
output_debugger_init_list(Stream, [Data | Datas], !IO) :-
    Data = module_layout_data(ModuleName, _, _, _, _),
    io.write_string(Stream,
        "\tif (MR_register_module_layout != NULL) {\n", !IO),
    io.write_string(Stream, "\t\t(*MR_register_module_layout)(", !IO),
    io.write_string(Stream, "\n\t\t\t&", !IO),
    output_layout_name(Stream, module_layout(ModuleName), !IO),
    io.write_string(Stream, ");\n\t}\n", !IO),
    output_debugger_init_list(Stream, Datas, !IO).

:- pred output_write_proc_static_list(io.text_output_stream::in,
    list(proc_layout_data)::in, io::di, io::uo) is det.

output_write_proc_static_list(_, [], !IO).
output_write_proc_static_list(Stream, [ProcLayout | ProcLayouts], !IO) :-
    ProcLayout = proc_layout_data(RttiProcLabel, _, MaybeMore),
    ( if MaybeMore = proc_id_and_more(yes(_ProcStatic), _, _, _) then
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        UserOrUCI = proc_label_user_or_uci(ProcLabel),
        Kind = proc_layout_proc_id(UserOrUCI),
        (
            UserOrUCI = user,
            io.write_string(Stream, 
                "\tMR_write_out_user_proc_static(deep_fp, procrep_fp,\n\t\t&",
                !IO)
        ;
            UserOrUCI = uci,
            io.write_string(Stream, 
                "\tMR_write_out_uci_proc_static(deep_fp, procrep_fp,\n\t\t&",
                !IO)
        ),
        output_layout_name(Stream, proc_layout(RttiProcLabel, Kind), !IO),
        io.write_string(Stream, ");\n", !IO)
    else
        true
    ),
    output_write_proc_static_list(Stream, ProcLayouts, !IO).

%----------------------------------------------------------------------------%

:- pred output_required_init_or_final_calls(io.text_output_stream::in,
    list(string)::in, io::di, io::uo) is det.

output_required_init_or_final_calls(_, [], !IO).
output_required_init_or_final_calls(Stream, [Name | Names], !IO) :-
    io.format(Stream, "\t%s();\n", [s(Name)], !IO),
    output_required_init_or_final_calls(Stream, Names, !IO).

%----------------------------------------------------------------------------%

:- pred output_annotated_c_module(llds_out_info::in, io.text_output_stream::in,
    annotated_c_module::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_annotated_c_module(Info, Stream, AnnotatedModule, !DeclSet, !IO) :-
    AnnotatedModule = annotated_c_module(ModuleName, AnnotatedProcedures,
        EntryLabels, InternalLabels),
    io.write_string(Stream, "\n", !IO),
    list.foldl2(output_record_c_procedure_decls(Info, Stream),
        AnnotatedProcedures, !DeclSet, !IO),
    io.write_string(Stream, "\n", !IO),
    io.write_string(Stream, "MR_BEGIN_MODULE(", !IO),
    io.write_string(Stream, ModuleName, !IO),
    io.write_string(Stream, ")\n", !IO),
    output_c_label_inits(Info, Stream, EntryLabels, InternalLabels, !IO),
    io.write_string(Stream, "MR_BEGIN_CODE\n", !IO),
    list.foldl(output_annotated_c_procedure(Info, Stream),
        AnnotatedProcedures, !IO),
    io.write_string(Stream, "MR_END_MODULE\n", !IO).

%----------------------------------------------------------------------------%

:- pred output_static_linkage_define(io.text_output_stream::in,
    io::di, io::uo) is det.

output_static_linkage_define(Stream, !IO) :-
    % The MS Visual C compiler treats the following declarations as
    % definitions, for which it cannot determine the size and hence aborts:
    %   static const struct s_name typename[];
    % However if we mark the linkage as extern, it treats it as a declaration.
    io.write_string(Stream, "#ifdef _MSC_VER\n", !IO),
    io.write_string(Stream, "#define MR_STATIC_LINKAGE extern\n", !IO),
    io.write_string(Stream, "#else\n", !IO),
    io.write_string(Stream, "#define MR_STATIC_LINKAGE static\n", !IO),
    io.write_string(Stream, "#endif\n", !IO).

%----------------------------------------------------------------------------%

:- pred output_foreign_body_code(llds_out_info::in, io.text_output_stream::in,
    foreign_body_code::in, maybe_error::out, io::di, io::uo) is det.

output_foreign_body_code(Info, Stream, ForeignBodyCode, Res, !IO) :-
    ForeignBodyCode = foreign_body_code(Lang, LiteralOrInclude, Context),
    (
        Lang = lang_c,
        output_foreign_decl_or_code(Info, Stream, "foreign_code", Lang,
            LiteralOrInclude, Context, Res, !IO)
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ),
        unexpected($pred, "unimplemented: foreign code other than C")
    ).

:- pred output_foreign_header_include_lines(llds_out_info::in,
    io.text_output_stream::in, list(foreign_decl_code)::in,
    list(maybe_error)::out, io::di, io::uo) is det.

output_foreign_header_include_lines(Info, Stream, Decls, Results, !IO) :-
    list.map_foldl2(output_foreign_header_include_line(Info, Stream),
        Decls, Results, set.init, _, !IO).

:- pred output_foreign_header_include_line(llds_out_info::in,
    io.text_output_stream::in, foreign_decl_code::in, maybe_error::out,
    set(foreign_literal_or_include)::in, set(foreign_literal_or_include)::out,
    io::di, io::uo) is det.

output_foreign_header_include_line(Info, Stream, Decl, Res,
        !AlreadyDone, !IO) :-
    Decl = foreign_decl_code(Lang, _IsLocal, LiteralOrInclude, Context),
    (
        Lang = lang_c,
        % This will not deduplicate the content of included files.
        ( if set.insert_new(LiteralOrInclude, !AlreadyDone) then
            output_foreign_decl_or_code(Info, Stream, "foreign_decl", Lang,
                LiteralOrInclude, Context, Res, !IO)
        else
            Res = ok
        )
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ),
        unexpected($pred, "foreign decl code other than C")
    ).

:- pred output_foreign_decl_or_code(llds_out_info::in,
    io.text_output_stream::in, string::in, foreign_language::in,
    foreign_literal_or_include::in, prog_context::in,
    maybe_error::out, io::di, io::uo) is det.

output_foreign_decl_or_code(Info, Stream, PragmaType, Lang, LiteralOrInclude,
        Context, Res, !IO) :-
    AutoComments = Info ^ lout_auto_comments,
    ForeignLineNumbers = Info ^ lout_foreign_line_numbers,
    ( if
        AutoComments = auto_comments,
        ForeignLineNumbers = yes
    then
        io.write_string(Stream, "/* ", !IO),
        parse_tree_out_misc.write_context(Stream, Context, !IO),
        io.write_string(Stream, " pragma ", !IO),
        io.write_string(Stream, PragmaType, !IO),
        io.write_string(Stream, "(", !IO),
        io.write(Stream, Lang, !IO),
        io.write_string(Stream, ") */\n", !IO)
    else
        true
    ),
    (
        LiteralOrInclude = floi_literal(Code),
        output_set_line_num(Stream, ForeignLineNumbers, Context, !IO),
        io.write_string(Stream, Code, !IO),
        Res = ok
    ;
        LiteralOrInclude = floi_include_file(IncludeFileName),
        SourceFileName = Info ^ lout_source_file_name,
        make_include_file_path(SourceFileName, IncludeFileName, IncludePath),
        output_set_line_num(Stream, ForeignLineNumbers,
            context(IncludePath, 1), !IO),
        write_include_file_contents(Stream, IncludePath, Res, !IO)
    ),
    io.nl(Stream, !IO),
    output_reset_line_num(Stream, ForeignLineNumbers, !IO).

:- pred output_record_c_label_decls(llds_out_info::in,
    io.text_output_stream::in, list(label)::in, list(label)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_c_label_decls(Info, Stream, EntryLabels, InternalLabels,
        !DeclSet, !IO) :-
    group_decl_c_labels(InternalLabels, multi_map.init, InternalLabelMap),
    multi_map.to_assoc_list(InternalLabelMap, InternalLabelList),
    list.foldl2(output_record_internal_label_decls(Stream), InternalLabelList,
        !DeclSet, !IO),
    list.foldl2(output_record_entry_label_decl(Info, Stream), EntryLabels,
        !DeclSet, !IO).

:- pred group_decl_c_labels(list(label)::in,
    multi_map(proc_label, int)::in, multi_map(proc_label, int)::out) is det.

group_decl_c_labels([], !InternalLabelMap).
group_decl_c_labels([Label | Labels], !InternalLabelMap) :-
    (
        Label = internal_label(LabelNum, ProcLabel),
        multi_map.set(ProcLabel, LabelNum, !InternalLabelMap)
    ;
        Label = entry_label(_, _),
        unexpected($pred, "entry label")
    ),
    group_decl_c_labels(Labels, !InternalLabelMap).

:- pred output_record_internal_label_decls(io.text_output_stream::in,
    pair(proc_label, list(int))::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_internal_label_decls(Stream, ProcLabel - RevLabelNums,
        !DeclSet, !IO) :-
    list.reverse(RevLabelNums, LabelNums),
    % There must be a macro of the form MR_decl_label<n> for every <n>
    % up to MaxChunkSize.
    MaxChunkSize = 10,
    list.chunk(LabelNums, MaxChunkSize, LabelNumChunks),
    list.foldl2(output_record_internal_label_decl_group(Stream, ProcLabel),
        LabelNumChunks, !DeclSet, !IO),
    list.foldl(insert_internal_label_code_addr_decl(ProcLabel), LabelNums,
        !DeclSet).

:- pred output_record_internal_label_decl_group(io.text_output_stream::in,
    proc_label::in, list(int)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_internal_label_decl_group(Stream, ProcLabel, LabelNums,
        !DeclSet, !IO) :-
    io.format(Stream, "MR_decl_label%d(%s, ",
        [i(list.length(LabelNums)),
        s(proc_label_to_c_string(do_not_add_label_prefix, ProcLabel))], !IO),
    write_out_list(add_int, ",", LabelNums, Stream, !IO),
    io.write_string(Stream, ")\n", !IO),
    list.foldl(insert_internal_label_code_addr_decl(ProcLabel), LabelNums,
        !DeclSet).

:- pred insert_internal_label_code_addr_decl(proc_label::in, int::in,
    decl_set::in, decl_set::out) is det.

insert_internal_label_code_addr_decl(ProcLabel, LabelNum, !DeclSet) :-
    DeclId = decl_code_addr(code_label(internal_label(LabelNum, ProcLabel))),
    decl_set_insert(DeclId, !DeclSet).

:- pred output_record_entry_label_decl(llds_out_info::in,
    io.text_output_stream::in, label::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_entry_label_decl(_Info, Stream, Label, !DeclSet, !IO) :-
    (
        Label = entry_label(entry_label_exported, _),
        DeclMacro = "MR_def_extern_entry"
    ;
        Label = entry_label(entry_label_local, _),
        DeclMacro = "MR_decl_static"
    ;
        Label = entry_label(entry_label_c_local, _),
        DeclMacro = "MR_decl_local"
    ;
        Label = internal_label(_, _),
        unexpected($pred, "internal label")
    ),
    io.write_string(Stream, DeclMacro, !IO),
    io.write_string(Stream, "(", !IO),
    output_label_no_prefix(Stream, Label, !IO),
    io.write_string(Stream, ")\n", !IO),
    decl_set_insert(decl_code_addr(code_label(Label)), !DeclSet).

%----------------------------------------------------------------------------%

:- pred output_c_label_inits(llds_out_info::in, io.text_output_stream::in,
    list(label)::in, list(label)::in, io::di, io::uo) is det.

output_c_label_inits(Info, Stream, EntryLabels, InternalLabels, !IO) :-
    EntryLabelToLayoutMap = Info ^ lout_entry_label_to_layout,
    list.foldl(output_c_entry_label_init(Stream, EntryLabelToLayoutMap),
        EntryLabels, !IO),

    InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
    group_init_c_labels(InternalLabelToLayoutMap, InternalLabels,
        multi_map.init, NoLayoutInternalMap,
        multi_map.init, NoVarLayoutInternalMap,
        multi_map.init, SVarLayoutInternalMap,
        multi_map.init, LVarLayoutInternalMap),

    multi_map.to_assoc_list(NoLayoutInternalMap, NoLayoutInternalList),
    list.foldl(output_c_internal_label_no_layout_init_group(Stream),
        NoLayoutInternalList, !IO),

    multi_map.to_assoc_list(NoVarLayoutInternalMap, NoVarLayoutInternalList),
    list.foldl(output_c_internal_label_layout_init_group(Info, Stream, "_nvi"),
        NoVarLayoutInternalList, !IO),

    multi_map.to_assoc_list(SVarLayoutInternalMap, SVarLayoutInternalList),
    list.foldl(output_c_internal_label_layout_init_group(Info, Stream, "_svi"),
        SVarLayoutInternalList, !IO),

    multi_map.to_assoc_list(LVarLayoutInternalMap, LVarLayoutInternalList),
    list.foldl(output_c_internal_label_layout_init_group(Info, Stream, "_lvi"),
        LVarLayoutInternalList, !IO).

:- pred group_init_c_labels(map(label, layout_slot_name)::in, list(label)::in,
    multi_map(proc_label, int)::in, multi_map(proc_label, int)::out,
    multi_map(proc_label, {int, int})::in,
    multi_map(proc_label, {int, int})::out,
    multi_map(proc_label, {int, int})::in,
    multi_map(proc_label, {int, int})::out,
    multi_map(proc_label, {int, int})::in,
    multi_map(proc_label, {int, int})::out) is det.

group_init_c_labels(_InternalLabelToLayoutMap, [],
        !NoLayoutMap, !NoVarLayoutMap, !SVarLayoutMap, !LVarLayoutMap).
group_init_c_labels(InternalLabelToLayoutMap, [Label | Labels],
        !NoLayoutMap, !NoVarLayoutMap, !SVarLayoutMap, !LVarLayoutMap) :-
    (
        Label = internal_label(LabelNum, ProcLabel),
        ( if map.search(InternalLabelToLayoutMap, Label, Slot) then
            Slot = layout_slot(ArrayName, SlotNum),
            ( if ArrayName = label_layout_array(Vars) then
                Pair = {LabelNum, SlotNum},
                (
                    Vars = label_has_no_var_info,
                    multi_map.set(ProcLabel, Pair, !NoVarLayoutMap)
                ;
                    Vars = label_has_short_var_info,
                    multi_map.set(ProcLabel, Pair, !SVarLayoutMap)
                ;
                    Vars = label_has_long_var_info,
                    multi_map.set(ProcLabel, Pair, !LVarLayoutMap)
                )
            else
                unexpected($pred, "bad slot type")
            )
        else
            multi_map.set(ProcLabel, LabelNum, !NoLayoutMap)
        )
    ;
        Label = entry_label(_, _),
        unexpected($pred, "entry label")
    ),
    group_init_c_labels(InternalLabelToLayoutMap, Labels,
        !NoLayoutMap, !NoVarLayoutMap, !SVarLayoutMap, !LVarLayoutMap).

:- pred output_c_internal_label_no_layout_init_group(
    io.text_output_stream::in, pair(proc_label, list(int))::in,
    io::di, io::uo) is det.

output_c_internal_label_no_layout_init_group(Stream, ProcLabel - RevLabelNums,
        !IO) :-
    list.reverse(RevLabelNums, LabelNums),
    % There must be macros of the form MR_init_label_nvi<n> for every <n>
    % up to MaxChunkSize.
    MaxChunkSize = 10,
    list.chunk(LabelNums, MaxChunkSize, LabelNumChunks),
    list.foldl(
        output_c_internal_label_no_layout_init_chunk(Stream, ProcLabel),
        LabelNumChunks, !IO).

:- pred output_c_internal_label_no_layout_init_chunk(io.text_output_stream::in,
    proc_label::in, list(int)::in, io::di, io::uo) is det.

output_c_internal_label_no_layout_init_chunk(Stream, ProcLabel, LabelNums,
        !IO) :-
    io.format(Stream, "\tMR_init_label%d(%s, ",
        [i(list.length(LabelNums)),
        s(proc_label_to_c_string(do_not_add_label_prefix, ProcLabel))], !IO),
    write_out_list(add_int, ",", LabelNums, Stream, !IO),
    io.write_string(Stream, ")\n", !IO).

:- pred output_c_internal_label_layout_init_group(llds_out_info::in,
    io.text_output_stream::in, string::in,
    pair(proc_label, list({int, int}))::in, io::di, io::uo) is det.

output_c_internal_label_layout_init_group(Info, Stream, Suffix,
        ProcLabel - RevLabelSlotNums, !IO) :-
    list.reverse(RevLabelSlotNums, LabelSlotNums),
    % There must be macros of the form MR_init_label_vi<n> for every <n>
    % up to MaxChunkSize.
    MaxChunkSize = 10,
    list.chunk(LabelSlotNums, MaxChunkSize, LabelSlotNumChunks),
    list.foldl(
        output_c_internal_label_layout_init_chunk(Info, Stream,
            Suffix, ProcLabel),
        LabelSlotNumChunks, !IO).

:- pred output_c_internal_label_layout_init_chunk(llds_out_info::in,
    io.text_output_stream::in, string::in, proc_label::in,
    list({int, int})::in, io::di, io::uo) is det.

output_c_internal_label_layout_init_chunk(Info, Stream, Suffix, ProcLabel,
        LabelSlotNums, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    io.format(Stream, "\tMR_init_label%s%d(%s, %s,\n\t\t",
        [s(Suffix), i(list.length(LabelSlotNums)),
        s(proc_label_to_c_string(do_not_add_label_prefix, ProcLabel)),
        s(ModuleName)], !IO),
    write_out_list(write_int_pair, ", ", LabelSlotNums, Stream, !IO),
    io.write_string(Stream, ")\n", !IO).

:- pred write_int_pair({int, int}::in, io.text_output_stream::in,
    io::di, io::uo) is det.

write_int_pair({LabelNum, SlotNum}, Stream, !IO) :-
    io.write_int(Stream, LabelNum, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, SlotNum, !IO).

:- pred output_c_entry_label_init(io.text_output_stream::in,
    map(label, data_id)::in, label::in, io::di, io::uo) is det.

output_c_entry_label_init(Stream, EntryLabelToLayoutMap, Label, !IO) :-
    ( if map.search(EntryLabelToLayoutMap, Label, _LayoutId) then
        SuffixOpen = "_sl("
    else
        SuffixOpen = "("
        % This label has no stack layout to initialize.
    ),
    (
        Label = entry_label(entry_label_exported, ProcLabel),
        TabInitMacro = "\tMR_init_entry1"
    ;
        Label = entry_label(entry_label_local, ProcLabel),
        TabInitMacro = "\tMR_init_entry1"
    ;
        Label = entry_label(entry_label_c_local, ProcLabel),
        TabInitMacro = "\tMR_init_local1"
    ;
        Label = internal_label(_, _),
        % These should have been separated out by group_c_labels.
        unexpected($pred, "internal label")
    ),
    io.write_string(Stream, TabInitMacro, !IO),
    io.write_string(Stream, SuffixOpen, !IO),
    io.write_string(Stream,
        proc_label_to_c_string(do_not_add_label_prefix, ProcLabel), !IO),
    io.write_string(Stream, ");\n", !IO),

    io.write_string(Stream, "\tMR_INIT_PROC_LAYOUT_ADDR(", !IO),
    output_label(Stream, Label, !IO),
    io.write_string(Stream, ");\n", !IO).

%----------------------------------------------------------------------------%

:- pred output_record_c_procedure_decls(llds_out_info::in,
    io.text_output_stream::in, annotated_c_procedure::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_c_procedure_decls(Info, Stream, AnnotatedProc, !DeclSet, !IO) :-
    Proc = AnnotatedProc ^ acp_proc,
    Instrs = Proc ^ cproc_code,
    CGlobalVarSet = Proc ^ cproc_c_global_vars,
    set.to_sorted_list(CGlobalVarSet, CGlobalVars),
    list.foldl2(output_c_global_var_decl(Stream), CGlobalVars,
        !DeclSet, !IO),
    list.foldl2(output_record_instruction_decls(Info, Stream), Instrs,
        !DeclSet, !IO),
    true.

:- pred output_c_global_var_decl(io.text_output_stream::in, string::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_global_var_decl(Stream, VarName, !DeclSet, !IO) :-
    GlobalVar = env_var_ref(VarName),
    ( if decl_set_insert_new(decl_c_global_var(GlobalVar), !DeclSet) then
        io.format(Stream, "extern MR_Word %s;\n",
            [s(c_global_var_name(GlobalVar))], !IO)
    else
        true
    ).

:- pred output_annotated_c_procedure(llds_out_info::in,
    io.text_output_stream::in, annotated_c_procedure::in,
    io::di, io::uo) is det.

output_annotated_c_procedure(Info, Stream, AnnotatedProc, !IO) :-
    Proc = AnnotatedProc ^ acp_proc,
    LabelOutputInfo = AnnotatedProc ^ acp_label_output_info,

    Name = Proc ^ cproc_name,
    Arity = Proc ^ cproc_orig_arity,
    Instrs = Proc ^ cproc_code,
    PredProcId = Proc ^ cproc_id,
    PredProcId = proc(_, ProcId),
    proc_id_to_int(ProcId, ModeNum),

    io.write_string(Stream, "\n/*-------------------------------------", !IO),
    io.write_string(Stream, "------------------------------------*/\n", !IO),
    % Now that we have unused_args.m mangling predicate names,
    % we should probably demangle them here.
    io.format(Stream, "/* code for '%s'/%d mode %d */\n",
        [s(Name), i(Arity), i(ModeNum)], !IO),

    LocalThreadEngineBase = Info ^ lout_local_thread_engine_base,
    (
        LocalThreadEngineBase = use_local_thread_engine_base,
        io.write_string(Stream,
            "#ifdef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string(Stream,
            "\t#undef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string(Stream,
            "\t#define MR_maybe_local_thread_engine_base " ++
            "MR_local_thread_engine_base\n", !IO),
        io.write_string(Stream, "#endif\n", !IO)
    ;
        LocalThreadEngineBase = do_not_use_local_thread_engine_base
    ),
    output_instruction_list(Info, Stream, Instrs, LabelOutputInfo,
        not_after_layout_label, !IO),
    (
        LocalThreadEngineBase = use_local_thread_engine_base,
        io.write_string(Stream,
            "#ifdef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string(Stream,
            "\t#undef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string(Stream,
            "\t#define MR_maybe_local_thread_engine_base " ++
            "MR_thread_engine_base\n", !IO),
        io.write_string(Stream, "#endif\n", !IO)
    ;
        LocalThreadEngineBase = do_not_use_local_thread_engine_base
    ).

    % Find the entry label for the procedure, for use as the profiling
    % "caller label" field in calls within this procedure.
    %
:- pred find_caller_label(list(instruction)::in, label::out) is det.

find_caller_label([], _) :-
    unexpected($pred, "cannot find caller label").
find_caller_label([llds_instr(Uinstr, _) | Instrs], CallerLabel) :-
    ( if Uinstr = label(Label) then
        (
            Label = internal_label(_, _),
            unexpected($pred, "caller label is internal label")
        ;
            Label = entry_label(_, _),
            CallerLabel = Label
        )
    else
        find_caller_label(Instrs, CallerLabel)
    ).

    % Locate all the labels which are the continuation labels for calls,
    % nondet disjunctions, forks or joins, and store them in ContLabels.
    %
:- pred find_cont_labels(list(instruction)::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

find_cont_labels([], !ContLabels).
find_cont_labels([Instr | Instrs], !ContLabels) :-
    Instr = llds_instr(Uinstr, _),
    ( if
        (
            Uinstr = llcall(_, code_label(ContLabel), _, _, _, _)
        ;
            Uinstr = mkframe(_, yes(code_label(ContLabel)))
        ;
            Uinstr = join_and_continue(_, ContLabel)
        ;
            Uinstr = assign(redoip_slot(_), const(Const)),
            Const = llconst_code_addr(code_label(ContLabel))
        )
    then
        set_tree234.insert(ContLabel, !ContLabels)
    else if
        Uinstr = fork_new_child(_, Label1)
    then
        set_tree234.insert(Label1, !ContLabels)
    else if
        Uinstr = block(_, _, Block)
    then
        find_cont_labels(Block, !ContLabels)
    else
        true
    ),
    find_cont_labels(Instrs, !ContLabels).

    % Locate all the labels which can be profitably turned into
    % labels starting while loops. The idea is to do this transform:
    %
    % L1:                       L1:
    %                           while (1) {
    %   ...                     ...
    %   if (...) goto L1        if (...) continue
    %   ...                 =>  ...
    %   if (...) goto L?        if (...) goto L?
    %   ...                     ...
    %   if (...) goto L1        if (...) continue
    %   ...                     ...
    %                           break;
    %                           }
    % L2:                       L2:
    %
    % The second of these is better if we don't have fast jumps.
    %
:- pred find_while_labels(list(instruction)::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

find_while_labels([], !WhileSet).
find_while_labels([llds_instr(Uinstr0, _) | Instrs0], !WhileSet) :-
    ( if
        Uinstr0 = label(Label),
        is_while_label(Label, Instrs0, Instrs1, 0, UseCount),
        UseCount > 0
    then
        set_tree234.insert(Label, !WhileSet),
        find_while_labels(Instrs1, !WhileSet)
    else
        find_while_labels(Instrs0, !WhileSet)
    ).

:- pred is_while_label(label::in,
    list(instruction)::in, list(instruction)::out, int::in, int::out) is det.

is_while_label(_, [], [], !Count).
is_while_label(Label, [Instr0 | Instrs0], Instrs, !Count) :-
    Instr0 = llds_instr(Uinstr0, _),
    ( if Uinstr0 = label(_) then
        Instrs = [Instr0 | Instrs0]
    else
        ( if Uinstr0 = goto(code_label(Label)) then
            !:Count = !.Count + 1
        else if Uinstr0 = if_val(_, code_label(Label)) then
            !:Count = !.Count + 1
        else if Uinstr0 = block(_, _, BlockInstrs) then
            count_while_label_in_block(Label, BlockInstrs, !Count)
        else
            true
        ),
        is_while_label(Label, Instrs0, Instrs, !Count)
    ).

:- pred count_while_label_in_block(label::in, list(instruction)::in,
    int::in, int::out) is det.

count_while_label_in_block(_, [], !Count).
count_while_label_in_block(Label, [Instr0 | Instrs0], !Count) :-
    Instr0 = llds_instr(Uinstr0, _),
    ( if Uinstr0 = label(_) then
        unexpected($pred, "label in block")
    else
        ( if Uinstr0 = goto(code_label(Label)) then
            !:Count = !.Count + 1
        else if Uinstr0 = if_val(_, code_label(Label)) then
            !:Count = !.Count + 1
        else if Uinstr0 = block(_, _, _) then
            unexpected($pred, "block in block")
        else
            true
        ),
        count_while_label_in_block(Label, Instrs0, !Count)
    ).

    % Given WhileLabels, a set of labels that we know should start while loops,
    % and !.UndefWhileLabels, our current guess as to which of these labels
    % we can avoid defining, remove from UndefWhileLabels the labels
    % that we cannot avoid defining because they are used in ways that are
    % incompatible with the labels elimination. This can happen because
    % the label is branched to from outside its while loop, or because
    % its address is treated as data. To help us with the former,
    % MaybeCurWhileLabel tells us which while loop (if any) we are now in.
    %
    % Basically, *every* reference to a label will cause us to delete that
    % label from UndefWhileLabels, with the *exception* of a goto to the label
    % from *within* the while loop that it starts.
    %
:- pred find_while_labels_to_define(list(instruction)::in,
    maybe(label)::in, set_tree234(label)::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

find_while_labels_to_define([], _, _, !UndefWhileLabels).
find_while_labels_to_define([Instr0 | Instrs0], MaybeCurWhileLabel0,
        WhileLabels, !UndefWhileLabels) :-
    Instr0 = llds_instr(Uinstr0, _) ,
    (
        Uinstr0 = label(Label),
        ( if set_tree234.contains(WhileLabels, Label) then
            MaybeCurWhileLabel = yes(Label)
        else
            MaybeCurWhileLabel = no
        )
    ;
        Uinstr0 = if_val(Rval, Target),
        rval_addrs(Rval, RvalCodeAddrs, _),
        delete_any_labels(RvalCodeAddrs, !UndefWhileLabels),
        ( if Target = code_label(TargetLabel) then
            ( if MaybeCurWhileLabel0 = yes(TargetLabel) then
                % This reference will be turned into a continue statement.
                true
            else
                set_tree234.delete(TargetLabel, !UndefWhileLabels)
            )
        else
            true
        ),
        MaybeCurWhileLabel = no
    ;
        Uinstr0 = goto(Target),
        ( if Target = code_label(TargetLabel) then
            set_tree234.delete(TargetLabel, !UndefWhileLabels)
        else
            true
        ),
        MaybeCurWhileLabel = no
    ;
        Uinstr0 = computed_goto(Rval, MaybeTargets),
        rval_addrs(Rval, RvalCodeAddrs, _),
        delete_any_labels(RvalCodeAddrs, !UndefWhileLabels),
        delete_any_maybe_labels(MaybeTargets, !UndefWhileLabels),
        MaybeCurWhileLabel = no
    ;
        Uinstr0 = llcall(Target, Continuation, _, _, _, _),
        delete_any_label(Target, !UndefWhileLabels),
        delete_any_label(Continuation, !UndefWhileLabels),
        MaybeCurWhileLabel = no
    ;
        Uinstr0 = block(_, _, BlockInstrs),
        find_while_labels_to_define(BlockInstrs, MaybeCurWhileLabel0,
            WhileLabels, !UndefWhileLabels),
        % The block is guaranteed not to contain any labels.
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        Uinstr0 = mkframe(_, MaybeNextCodeAddr),
        (
            MaybeNextCodeAddr = yes(NextCodeAddr),
            delete_any_label(NextCodeAddr, !UndefWhileLabels)
        ;
            MaybeNextCodeAddr = no
        ),
        MaybeCurWhileLabel = no
    ;
        ( Uinstr0 = assign(Lval, Rval)
        ; Uinstr0 = keep_assign(Lval, Rval)
        ),
        lval_addrs(Lval, LvalCodeAddrs, _),
        rval_addrs(Rval, RvalCodeAddrs, _),
        delete_any_labels(LvalCodeAddrs, !UndefWhileLabels),
        delete_any_labels(RvalCodeAddrs, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        ( Uinstr0 = save_maxfr(Lval)
        ; Uinstr0 = restore_maxfr(Lval)
        ; Uinstr0 = mark_hp(Lval)
        ; Uinstr0 = store_ticket(Lval)
        ; Uinstr0 = mark_ticket_stack(Lval)
        ; Uinstr0 = init_sync_term(Lval, _, _)
        ; Uinstr0 = fork_new_child(Lval, _)
        ; Uinstr0 = join_and_continue(Lval, _)
        ; Uinstr0 = lc_create_loop_control(_, Lval)
        ),
        lval_addrs(Lval, LvalCodeAddrs, _),
        delete_any_labels(LvalCodeAddrs, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        ( Uinstr0 = restore_hp(Rval)
        ; Uinstr0 = free_heap(Rval)
        ; Uinstr0 = region_set_fixed_slot(_, _, Rval)
        ; Uinstr0 = reset_ticket(Rval, _)
        ; Uinstr0 = prune_tickets_to(Rval)
        ),
        rval_addrs(Rval, RvalCodeAddrs, _),
        delete_any_labels(RvalCodeAddrs, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        Uinstr0 = incr_hp(TargetLval, _, _, SizeRval, _, _,
            MaybeRegionIdRval, _),
        lval_addrs(TargetLval, TargetLvalCodeAddrs, _),
        rval_addrs(SizeRval, SizeRvalCodeAddrs, _),
        delete_any_labels(TargetLvalCodeAddrs, !UndefWhileLabels),
        delete_any_labels(SizeRvalCodeAddrs, !UndefWhileLabels),
        (
            MaybeRegionIdRval = no
        ;
            MaybeRegionIdRval = yes(RegionIdRval),
            rval_addrs(RegionIdRval, RegionIdRvalCodeAddrs, _),
            delete_any_labels(RegionIdRvalCodeAddrs, !UndefWhileLabels)
        ),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        Uinstr0 = foreign_proc_code(_, _, _, MaybeFixNoLayoutLabel,
            MaybeFixLayoutLabel, MaybeFixOnlyLayoutLabel, MaybeNoFixLabel,
            MaybeHashDefLabel, _, _),
        delete_any_maybe_label(MaybeFixNoLayoutLabel, !UndefWhileLabels),
        delete_any_maybe_label(MaybeFixLayoutLabel, !UndefWhileLabels),
        delete_any_maybe_label(MaybeFixOnlyLayoutLabel, !UndefWhileLabels),
        delete_any_maybe_label(MaybeNoFixLabel, !UndefWhileLabels),
        delete_any_maybe_label(MaybeHashDefLabel, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        Uinstr0 = region_fill_frame(_, _, RegionIdRval, NumLval, AddrLval),
        rval_addrs(RegionIdRval, RegionIdRvalCodeAddrs, _),
        lval_addrs(NumLval, NumLvalCodeAddrs, _),
        lval_addrs(AddrLval, AddrLvalCodeAddrs, _),
        delete_any_labels(RegionIdRvalCodeAddrs, !UndefWhileLabels),
        delete_any_labels(NumLvalCodeAddrs, !UndefWhileLabels),
        delete_any_labels(AddrLvalCodeAddrs, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        Uinstr0 = lc_wait_free_slot(LoopControlRval, SlotLval, Label),
        rval_addrs(LoopControlRval, LoopControlRvalCodeAddrs, _),
        lval_addrs(SlotLval, SlotLvalCodeAddrs, _),
        delete_any_labels(LoopControlRvalCodeAddrs, !UndefWhileLabels),
        delete_any_labels(SlotLvalCodeAddrs, !UndefWhileLabels),
        set_tree234.delete(Label, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        Uinstr0 = lc_spawn_off(LoopControlRval, SlotRval, Label),
        rval_addrs(LoopControlRval, LoopControlRvalCodeAddrs, _),
        rval_addrs(SlotRval, SlotRvalCodeAddrs, _),
        delete_any_labels(LoopControlRvalCodeAddrs, !UndefWhileLabels),
        delete_any_labels(SlotRvalCodeAddrs, !UndefWhileLabels),
        set_tree234.delete(Label, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        Uinstr0 = lc_join_and_terminate(LoopControlRval, SlotRval),
        rval_addrs(LoopControlRval, LoopControlRvalCodeAddrs, _),
        rval_addrs(SlotRval, SlotRvalCodeAddrs, _),
        delete_any_labels(LoopControlRvalCodeAddrs, !UndefWhileLabels),
        delete_any_labels(SlotRvalCodeAddrs, !UndefWhileLabels),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ;
        ( Uinstr0 = comment(_)
        ; Uinstr0 = livevals(_)
        ; Uinstr0 = arbitrary_c_code(_, _, _)
        ; Uinstr0 = push_region_frame(_, _)
        ; Uinstr0 = use_and_maybe_pop_region_frame(_, _)
        ; Uinstr0 = prune_ticket
        ; Uinstr0 = discard_ticket
        ; Uinstr0 = incr_sp(_, _, _)
        ; Uinstr0 = decr_sp(_)
        ; Uinstr0 = decr_sp_and_return(_)
        ),
        MaybeCurWhileLabel = MaybeCurWhileLabel0
    ),
    find_while_labels_to_define(Instrs0, MaybeCurWhileLabel,
        WhileLabels, !UndefWhileLabels).

%----------------------------------------------------------------------------%
%
% The job of all these predicates is to remove any labels in their first
% argument from !.UndefWhileLabels.
%

:- pred delete_any_label(code_addr::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

delete_any_label(CodeAddr, !UndefWhileLabels) :-
    ( if CodeAddr = code_label(Label) then
        set_tree234.delete(Label, !UndefWhileLabels)
    else
        true
    ).

:- pred delete_any_labels(list(code_addr)::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

delete_any_labels([], !UndefWhileLabels).
delete_any_labels([CodeAddr | CodeAddrs], !UndefWhileLabels) :-
    delete_any_label(CodeAddr, !UndefWhileLabels),
    delete_any_labels(CodeAddrs, !UndefWhileLabels).

:- pred delete_any_maybe_label(maybe(label)::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

delete_any_maybe_label(MaybeLabel, !UndefWhileLabels) :-
    (
        MaybeLabel = no
    ;
        MaybeLabel = yes(Label),
        set_tree234.delete(Label, !UndefWhileLabels)
    ).

:- pred delete_any_maybe_labels(list(maybe(label))::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

delete_any_maybe_labels([], !UndefWhileLabels).
delete_any_maybe_labels([MaybeLabel | MaybeLabels], !UndefWhileLabels) :-
    delete_any_maybe_label(MaybeLabel, !UndefWhileLabels),
    delete_any_maybe_labels(MaybeLabels, !UndefWhileLabels).

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

c_data_linkage_string(DefaultLinkage, BeingDefined) = LinkageStr :-
    (
        DefaultLinkage = extern,
        (
            BeingDefined = yes,
            LinkageStr = ""
        ;
            BeingDefined = no,
            LinkageStr = "extern "
        )
    ;
        DefaultLinkage = static,
        % Previously we used to always write `extern' here, but declaring
        % something `extern' and then later defining it as `static' causes
        % undefined behavior -- on many systems, it works, but on some systems
        % such as RS/6000s running AIX, it results in link errors.
        %
        LinkageStr = "static "
    ).

c_data_const_string(Globals, InclCodeAddr) =
    ( if
        InclCodeAddr = yes,
        globals.get_opt_tuple(Globals, OptTuple),
        OptTuple ^ ot_use_static_code_addresses =
            do_not_use_static_code_addresses
    then
        ""
    else
        "const "
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.llds_out.llds_out_file.
%---------------------------------------------------------------------------%
