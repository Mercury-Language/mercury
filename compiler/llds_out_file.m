%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009-2011 The University of Melbourne.
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

:- import_module libs.globals.
:- import_module ll_backend.llds.

:- import_module bool.
:- import_module io.

%----------------------------------------------------------------------------%

    % Given a 'c_file' structure, output the LLDS code inside it
    % into a .c file. The second argument gives the set of labels that have
    % layout structures.
    %
:- pred output_llds(globals::in, c_file::in, io::di, io::uo) is det.

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

:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module backend_libs.rtti.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module ll_backend.layout.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module ll_backend.llds_out.llds_out_global.
:- import_module ll_backend.llds_out.llds_out_instr.
:- import_module ll_backend.llds_out.llds_out_util.
:- import_module ll_backend.rtti_out.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.file_names.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.

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

%----------------------------------------------------------------------------%

output_llds(Globals, CFile, !IO) :-
    ModuleName = CFile ^ cfile_modulename,
    module_name_to_file_name(Globals, ModuleName, ".c", do_create_dirs,
        FileName, !IO),
    io.open_output(FileName, Result, !IO),
    (
        Result = ok(FileStream),
        decl_set_init(DeclSet0),
        output_single_c_file(Globals, CFile, FileStream, DeclSet0, _, !IO),
        io.close_output(FileStream, !IO)
    ;
        Result = error(Error),
        io.progname_base("llds.m", ProgName, !IO),
        io.format("\n%s: can't open `%s' for output:\n%s\n",
            [s(ProgName), s(FileName), s(io.error_message(Error))], !IO),
        io.set_exit_status(1, !IO)
    ).

:- pred output_c_file_mercury_headers(llds_out_info::in,
    io::di, io::uo) is det.

output_c_file_mercury_headers(Info, !IO) :-
    io.write_string("#define MR_ALLOW_RESET\n", !IO),
    TraceLevel = Info ^ lout_trace_level,
    ( given_trace_level_is_none(TraceLevel) = no ->
        io.write_string("#include ""mercury_imp.h""\n", !IO),
        io.write_string("#include ""mercury_trace_base.h""\n", !IO)
    ;
        io.write_string("#include ""mercury_imp.h""\n", !IO)
    ),
    DeepProfile = Info ^ lout_profile_deep,
    (
        DeepProfile = yes,
        io.write_string("#include ""mercury_deep_profiling.h""\n", !IO)
    ;
        DeepProfile = no
    ),
    GenerateBytecode = Info ^ lout_generate_bytecode,
    (
        GenerateBytecode = yes,
        io.write_string("#include ""mb_interface_stub.h""\n", !IO)
    ;
        GenerateBytecode = no
    ).

:- pred output_single_c_file(globals::in, c_file::in, io.output_stream::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_single_c_file(Globals, CFile, FileStream, !DeclSet, !IO) :-
    CFile = c_file(ModuleName, C_HeaderLines, UserForeignCode, Exports,
        TablingInfoStructs, ScalarCommonDatas, VectorCommonDatas,
        RttiDatas, PseudoTypeInfos, HLDSVarNums, ShortLocns, LongLocns,
        UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TSStringTable,
        TableIoDecls, TableIoDeclMap, ProcEventLayouts, ExecTraces,
        ProcLayoutDatas, ModuleLayoutDatas, ClosureLayoutDatas,
        Modules, UserInitPredCNames, UserFinalPredCNames, ComplexityProcs),
    Info = init_llds_out_info(ModuleName, Globals,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap, TableIoDeclMap),
    library.version(Version),
    io.set_output_stream(FileStream, OutputStream, !IO),
    module_name_to_file_name(Globals, ModuleName, ".m", do_not_create_dirs,
        SourceFileName, !IO),
    output_c_file_intro_and_grade(Globals, SourceFileName, Version, !IO),
    module_gather_env_var_names(Modules, set.init, EnvVarNameSet),
    EnvVarNames = set.to_sorted_list(EnvVarNameSet),
    output_init_comment(ModuleName, UserInitPredCNames, UserFinalPredCNames,
        EnvVarNames, !IO),
    output_c_file_mercury_headers(Info, !IO),

    output_foreign_header_include_lines(Info, C_HeaderLines, !IO),
    io.write_string("\n", !IO),

    gather_c_file_labels(Modules, EntryLabels, InternalLabels),

    output_static_linkage_define(!IO),
    list.foldl2(output_scalar_common_data_decl, ScalarCommonDatas,
        !DeclSet, !IO),
    list.foldl2(output_vector_common_data_decl, VectorCommonDatas,
        !DeclSet, !IO),
    output_rtti_data_decl_list(Info, RttiDatas, !DeclSet, !IO),
    output_record_c_label_decls(Info, EntryLabels, InternalLabels,
        !DeclSet, !IO),
    list.foldl2(output_tabling_info_struct(Info), TablingInfoStructs,
        !DeclSet, !IO),
    list.foldl2(output_scalar_common_data_defn(Info), ScalarCommonDatas,
        !DeclSet, !IO),
    list.foldl2(output_vector_common_data_defn(Info), VectorCommonDatas,
        !DeclSet, !IO),
    list.foldl2(output_rtti_data_defn(Info), RttiDatas, !DeclSet, !IO),

    io.nl(!IO),
    output_layout_array_decls(Info, PseudoTypeInfos, HLDSVarNums,
        ShortLocns, LongLocns, UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TableIoDecls,
        ProcEventLayouts, ExecTraces, !IO),

    list.foldl2(output_proc_layout_data_defn(Info), ProcLayoutDatas,
        !DeclSet, !IO),
    list.foldl2(output_module_layout_data_defn(Info), ModuleLayoutDatas,
        !DeclSet, !IO),
    list.foldl2(output_closure_layout_data_defn(Info), ClosureLayoutDatas,
        !DeclSet, !IO),
    io.nl(!IO),

    output_record_rvals_decls(Info, PseudoTypeInfos, !DeclSet, !IO),
    output_layout_array_defns(Info, PseudoTypeInfos, HLDSVarNums,
        ShortLocns, LongLocns, UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TableIoDecls,
        ProcEventLayouts, ExecTraces, TSStringTable, !DeclSet, !IO),

    list.foldl2(output_comp_gen_c_module(Info), Modules, !DeclSet, !IO),
    list.foldl(output_user_foreign_code(Info), UserForeignCode, !IO),
    list.foldl(io.write_string, Exports, !IO),
    io.write_string("\n", !IO),
    output_c_module_init_list(Info, ModuleName, Modules, RttiDatas,
        ProcLayoutDatas, ModuleLayoutDatas, ComplexityProcs, TSStringTable,
        UserInitPredCNames, UserFinalPredCNames, !DeclSet, !IO),
    io.set_output_stream(OutputStream, _, !IO).

:- pred module_gather_env_var_names(list(comp_gen_c_module)::in,
    set(string)::in, set(string)::out) is det.

module_gather_env_var_names([], !EnvVarNames).
module_gather_env_var_names([Module | Modules], !EnvVarNames) :-
    proc_gather_env_var_names(Module ^ cgcm_procs, !EnvVarNames),
    module_gather_env_var_names(Modules, !EnvVarNames).

:- pred proc_gather_env_var_names(list(c_procedure)::in,
    set(string)::in, set(string)::out) is det.

proc_gather_env_var_names([], !EnvVarNames).
proc_gather_env_var_names([Proc | Procs], !EnvVarNames) :-
    set.union(Proc ^ cproc_c_global_vars, !EnvVarNames),
    proc_gather_env_var_names(Procs, !EnvVarNames).

:- pred output_c_module_init_list(llds_out_info::in, module_name::in,
    list(comp_gen_c_module)::in, list(rtti_data)::in,
    list(proc_layout_data)::in, list(module_layout_data)::in,
    list(complexity_proc_info)::in, list(string)::in, list(string)::in,
    list(string)::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_module_init_list(Info, ModuleName, Modules, RttiDatas,
        ProcLayoutDatas, ModuleLayoutDatas, ComplexityProcs, TSStringTable,
        InitPredNames, FinalPredNames, !DeclSet, !IO) :-
    MustInit = (pred(Module::in) is semidet :-
        module_defines_label_with_layout(Info, Module)
    ),
    list.filter(MustInit, Modules, AlwaysInitModules, MaybeInitModules),
    list.chunk(AlwaysInitModules, 40, AlwaysInitModuleBunches),
    list.chunk(MaybeInitModules, 40, MaybeInitModuleBunches),

    output_init_bunch_defs(Info, "always", 0, AlwaysInitModuleBunches, !IO),

    (
        MaybeInitModuleBunches = []
    ;
        MaybeInitModuleBunches = [_ | _],
        output_init_bunch_defs(Info, "maybe", 0,MaybeInitModuleBunches, !IO)
    ),

    io.write_string("/* suppress gcc -Wmissing-decls warnings */\n", !IO),
    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init(void);\n", !IO),

    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_type_tables(void);\n", !IO),
    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_debugger(void);\n", !IO),

    io.write_string("#ifdef MR_DEEP_PROFILING\n", !IO),
    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string(
        "write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp);\n", !IO),
    io.write_string("#endif\n", !IO),

    io.write_string("#ifdef MR_RECORD_TERM_SIZES\n", !IO),
    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_complexity_procs(void);\n", !IO),
    io.write_string("#endif\n", !IO),

    io.write_string("#ifdef MR_THREADSCOPE\n", !IO),
    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_threadscope_string_table(void);\n", !IO),
    io.write_string("#endif\n", !IO),

    (
        InitPredNames = []
    ;
        InitPredNames = [_ | _],
        io.write_string("void ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_init(void);\n", !IO)
    ),

    (
        FinalPredNames = []
    ;
        FinalPredNames = [_ | _],
        io.write_string("void ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_final(void);\n", !IO)
    ),

    io.write_string("\n", !IO),

    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init(void)\n", !IO),
    io.write_string("{\n", !IO),
    io.write_string("\tstatic MR_bool done = MR_FALSE;\n", !IO),
    io.write_string("\tif (done) {\n", !IO),
    io.write_string("\t\treturn;\n", !IO),
    io.write_string("\t}\n", !IO),
    io.write_string("\tdone = MR_TRUE;\n", !IO),

    output_init_bunch_calls(Info, "always", 0,AlwaysInitModuleBunches, !IO),

    (
        MaybeInitModuleBunches = []
    ;
        MaybeInitModuleBunches = [_ | _],
        output_init_bunch_calls(Info, "maybe", 0, MaybeInitModuleBunches, !IO)
    ),

    output_c_data_init_list(RttiDatas, !IO),
    % The call to the debugger initialization function is for bootstrapping;
    % once the debugger has been modified to call do_init_modules_debugger()
    % and all debuggable object files created before this change have been
    % overwritten, it can be deleted.
    io.write_string("\t", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_debugger();\n", !IO),
    io.write_string("}\n\n", !IO),

    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_type_tables(void)\n", !IO),
    io.write_string("{\n", !IO),
    io.write_string("\tstatic MR_bool done = MR_FALSE;\n", !IO),
    io.write_string("\tif (done) {\n", !IO),
    io.write_string("\t\treturn;\n", !IO),
    io.write_string("\t}\n", !IO),
    io.write_string("\tdone = MR_TRUE;\n", !IO),
    output_type_tables_init_list(RttiDatas, !IO),
    io.write_string("}\n\n", !IO),

    io.write_string("\n", !IO),
    io.write_string("void ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_debugger(void)\n", !IO),
    io.write_string("{\n", !IO),
    io.write_string("\tstatic MR_bool done = MR_FALSE;\n", !IO),
    io.write_string("\tif (done) {\n", !IO),
    io.write_string("\t\treturn;\n", !IO),
    io.write_string("\t}\n", !IO),
    io.write_string("\tdone = MR_TRUE;\n", !IO),
    output_debugger_init_list(ModuleLayoutDatas, !IO),
    io.write_string("}\n\n", !IO),

    io.write_string("#ifdef MR_DEEP_PROFILING\n", !IO),
    io.write_string("\nvoid ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string(
        "write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp)\n", !IO),
    io.write_string("{\n", !IO),
    ModuleCommonLayoutName = module_common_layout(ModuleName),
    io.write_string("\tMR_write_out_module_proc_reps_start(procrep_fp, &",
        !IO),
    output_layout_name(ModuleCommonLayoutName, !IO),
    io.write_string(");\n", !IO),
    output_write_proc_static_list(ProcLayoutDatas, !IO),
    io.write_string("\tMR_write_out_module_proc_reps_end(procrep_fp);\n",
        !IO),
    io.write_string("}\n", !IO),
    io.write_string("\n#endif\n\n", !IO),

    io.write_string("#ifdef MR_RECORD_TERM_SIZES\n", !IO),
    output_complexity_arg_info_arrays(ComplexityProcs, !IO),
    io.write_string("\nvoid ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_complexity_procs(void)\n", !IO),
    io.write_string("{\n", !IO),
    output_init_complexity_proc_list(ComplexityProcs, !IO),
    io.write_string("}\n", !IO),
    io.write_string("\n#endif\n\n", !IO),

    io.write_string("#ifdef MR_THREADSCOPE\n", !IO),
    io.write_string("\nvoid ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init_threadscope_string_table(void)\n", !IO),
    io.write_string("{\n", !IO),
    (
        TSStringTable = []
    ;
        TSStringTable = [_ | _],
        TSStringTableSize = length(TSStringTable),
        io.write_string("\tMR_threadscope_register_strings_array(\n", !IO),
        io.write_string("\t\t", !IO),
        MangledModuleName = Info ^ lout_mangled_module_name,
        output_layout_array_name(use_layout_macro, MangledModuleName,
            threadscope_string_table_array, !IO),
        io.format(", %d);\n", [i(TSStringTableSize)], !IO)
    ),
    io.write_string("}\n", !IO),
    io.write_string("\n#endif\n\n", !IO),

    (
        InitPredNames = []
    ;
        InitPredNames = [_ | _],
        io.write_string("void ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_init(void)\n", !IO),
        io.write_string("{\n", !IO),
        output_required_init_or_final_calls(InitPredNames, !IO),
        io.write_string("}\n", !IO),
        io.nl(!IO)
    ),

    (
        FinalPredNames = []
    ;
        FinalPredNames = [_ | _],
        io.write_string("void ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_final(void)\n", !IO),
        io.write_string("{\n", !IO),
        output_required_init_or_final_calls(FinalPredNames, !IO),
        io.write_string("}\n", !IO)
    ),

    io.write_string(
        "/* ensure everything is compiled with the same grade */\n",
        !IO),
    io.write_string(
        "static const void *const MR_grade = &MR_GRADE_VAR;\n", !IO).

:- pred module_defines_label_with_layout(llds_out_info::in,
    comp_gen_c_module::in) is semidet.

module_defines_label_with_layout(Info, Module) :-
    % Checking whether the set is empty or not
    % allows us to avoid calling gather_c_module_labels.
    InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
    EntryLabelToLayoutMap = Info ^ lout_entry_label_to_layout,
    (
        \+ map.is_empty(InternalLabelToLayoutMap)
    ;
        \+ map.is_empty(EntryLabelToLayoutMap)
    ),
    Module = comp_gen_c_module(_, Procedures),
    gather_c_module_labels(Procedures, EntryLabels, InternalLabels),
    (
        find_first_match(internal_label_has_layout(InternalLabelToLayoutMap),
            InternalLabels, _)
    ;
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

:- pred output_init_bunch_defs(llds_out_info::in, string::in, int::in,
    list(list(comp_gen_c_module))::in, io::di, io::uo) is det.

output_init_bunch_defs(_, _, _, [], !IO).
output_init_bunch_defs(Info, InitStatus, Seq, [Bunch | Bunches], !IO) :-
    io.write_string("static void ", !IO),
    output_bunch_name(Info, InitStatus, Seq, !IO),
    io.write_string("(void)\n", !IO),
    io.write_string("{\n", !IO),
    output_init_bunch_def(Bunch, !IO),
    io.write_string("}\n\n", !IO),
    NextSeq = Seq + 1,
    output_init_bunch_defs(Info, InitStatus, NextSeq, Bunches, !IO).

:- pred output_init_bunch_def(list(comp_gen_c_module)::in,
    io::di, io::uo) is det.

output_init_bunch_def([], !IO).
output_init_bunch_def([Module | Modules], !IO) :-
    Module = comp_gen_c_module(C_ModuleName, _),
    io.write_string("\t", !IO),
    io.write_string(C_ModuleName, !IO),
    io.write_string("();\n", !IO),
    output_init_bunch_def(Modules, !IO).

:- pred output_init_bunch_calls(llds_out_info::in, string::in, int::in,
    list(list(comp_gen_c_module))::in, io::di, io::uo) is det.

output_init_bunch_calls(_, _, _, [], !IO).
output_init_bunch_calls(Info, InitStatus, Seq, [_ | Bunches], !IO) :-
    io.write_string("\t", !IO),
    output_bunch_name(Info, InitStatus, Seq, !IO),
    io.write_string("();\n", !IO),
    NextSeq = Seq + 1,
    output_init_bunch_calls(Info, InitStatus, NextSeq, Bunches, !IO).

:- pred output_bunch_name(llds_out_info::in, string::in, int::in,
    io::di, io::uo) is det.

output_bunch_name(Info, InitStatus, Number, !IO) :-
    io.write_string("mercury__", !IO),
    MangledModuleName = Info ^ lout_mangled_module_name,
    io.write_string(MangledModuleName, !IO),
    io.write_string("_", !IO),
    io.write_string(InitStatus, !IO),
    io.write_string("_bunch_", !IO),
    io.write_int(Number, !IO).

    % Output MR_INIT_TYPE_CTOR_INFO(TypeCtorInfo, Typector);
    % for each type_ctor_info defined in this module.
    %
:- pred output_c_data_init_list(list(rtti_data)::in, io::di, io::uo) is det.

output_c_data_init_list([], !IO).
output_c_data_init_list([Data | Datas], !IO) :-
    rtti_out.init_rtti_data_if_nec(Data, !IO),
    output_c_data_init_list(Datas, !IO).

    % Output code to register each type_ctor_info defined in this module.
    %
:- pred output_type_tables_init_list(list(rtti_data)::in, io::di, io::uo)
    is det.

output_type_tables_init_list([], !IO).
output_type_tables_init_list([Data | Datas], !IO) :-
    rtti_out.register_rtti_data_if_nec(Data, !IO),
    output_type_tables_init_list(Datas, !IO).

    % Output calls to MR_register_module_layout()
    % for each module layout defined in this module
    % (there should only be one, of course).
    %
:- pred output_debugger_init_list(list(module_layout_data)::in,
    io::di, io::uo) is det.

output_debugger_init_list([], !IO).
output_debugger_init_list([Data | Datas], !IO) :-
    ( Data = module_layout_data(ModuleName, _, _, _, _, _, _, _) ->
        io.write_string("\tif (MR_register_module_layout != NULL) {\n", !IO),
        io.write_string("\t\t(*MR_register_module_layout)(", !IO),
        io.write_string("\n\t\t\t&", !IO),
        output_layout_name(module_layout(ModuleName), !IO),
        io.write_string(");\n\t}\n", !IO)
    ;
        true
    ),
    output_debugger_init_list(Datas, !IO).

:- pred output_write_proc_static_list(list(proc_layout_data)::in,
    io::di, io::uo) is det.

output_write_proc_static_list([], !IO).
output_write_proc_static_list([ProcLayout | ProcLayouts], !IO) :-
    ProcLayout = proc_layout_data(RttiProcLabel, _, MaybeMore),
    ( MaybeMore = proc_id_and_more(yes(_ProcStatic), _, _, _) ->
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        UserOrUCI = proc_label_user_or_uci(ProcLabel),
        Kind = proc_layout_proc_id(UserOrUCI),
        (
            UserOrUCI = user,
            io.write_string(
                "\tMR_write_out_user_proc_static(deep_fp, procrep_fp,\n\t\t&",
                !IO)
        ;
            UserOrUCI = uci,
            io.write_string(
                "\tMR_write_out_uci_proc_static(deep_fp, procrep_fp,\n\t\t&",
                !IO)
        ),
        output_layout_name(proc_layout(RttiProcLabel, Kind), !IO),
        io.write_string(");\n", !IO)
    ;
        true
    ),
    output_write_proc_static_list(ProcLayouts, !IO).

%----------------------------------------------------------------------------%

    % Output a comment to tell mkinit what functions to call from
    % <module>_init.c.
    %
:- pred output_init_comment(module_name::in, list(string)::in,
    list(string)::in, list(string)::in, io::di, io::uo) is det.

output_init_comment(ModuleName, UserInitPredCNames, UserFinalPredCNames,
        EnvVarNames, !IO) :-
    io.write_string("/*\n", !IO),
    io.write_string("INIT ", !IO),
    output_init_name(ModuleName, !IO),
    io.write_string("init\n", !IO),
    % We only print out the REQUIRED_INIT and REQUIRED_FINAL comments
    % if there are user initialisation/finalisation predicates.
    (
        UserInitPredCNames = []
    ;
        UserInitPredCNames = [_ | _],
        io.write_string("REQUIRED_INIT ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_init\n", !IO)
    ),
    (
        UserFinalPredCNames = []
    ;
        UserFinalPredCNames = [_ | _],
        io.write_string("REQUIRED_FINAL ", !IO),
        output_init_name(ModuleName, !IO),
        io.write_string("required_final\n", !IO)
    ),
    list.foldl(output_env_var_init, EnvVarNames, !IO),
    io.write_string("ENDINIT\n", !IO),
    io.write_string("*/\n\n", !IO).

:- pred output_env_var_init(string::in, io::di, io::uo) is det.

output_env_var_init(EnvVarName, !IO) :-
    io.write_string("ENVVAR ", !IO),
    io.write_string(EnvVarName, !IO),
    io.nl(!IO).

:- pred output_required_init_or_final_calls(list(string)::in, io::di, io::uo)
    is det.

output_required_init_or_final_calls([], !IO).
output_required_init_or_final_calls([ Name | Names ], !IO) :-
    io.write_string("\t" ++ Name ++ "();\n", !IO),
    output_required_init_or_final_calls(Names, !IO).

%----------------------------------------------------------------------------%

:- pred output_comp_gen_c_module(llds_out_info::in, comp_gen_c_module::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_comp_gen_c_module(Info, CompGenCModule, !DeclSet, !IO) :-
    CompGenCModule = comp_gen_c_module(ModuleName, Procedures),
    io.write_string("\n", !IO),
    list.foldl2(output_record_c_procedure_decls(Info), Procedures,
        !DeclSet, !IO),
    io.write_string("\n", !IO),
    io.write_string("MR_BEGIN_MODULE(", !IO),
    io.write_string(ModuleName, !IO),
    io.write_string(")\n", !IO),
    gather_c_module_labels(Procedures, EntryLabels, InternalLabels),
    output_c_label_inits(Info, EntryLabels, InternalLabels, !IO),
    io.write_string("MR_BEGIN_CODE\n", !IO),
    list.foldl(output_c_procedure(Info), Procedures, !IO),
    io.write_string("MR_END_MODULE\n", !IO).

%----------------------------------------------------------------------------%

:- pred output_static_linkage_define(io::di, io::uo) is det.

output_static_linkage_define(!IO) :-
    % The MS Visual C compiler treats the following declarations as
    % definitions, for which it cannot determine the size and hence aborts:
    %   static const struct s_name typename[];
    % However if we mark the linkage as extern, it treats it as a declaration.

    io.write_string("#ifdef _MSC_VER\n", !IO),
    io.write_string("#define MR_STATIC_LINKAGE extern\n", !IO),
    io.write_string("#else\n", !IO),
    io.write_string("#define MR_STATIC_LINKAGE static\n", !IO),
    io.write_string("#endif\n", !IO).

%----------------------------------------------------------------------------%

:- pred output_user_foreign_code(llds_out_info::in, user_foreign_code::in,
    io::di, io::uo) is det.

output_user_foreign_code(Info, UserForeignCode, !IO) :-
    UserForeignCode = user_foreign_code(Lang, Foreign_Code, Context),
    (
        Lang = lang_c,
        AutoComments = Info ^ lout_auto_comments,
        LineNumbers = Info ^ lout_line_numbers,
        (
            AutoComments = yes,
            LineNumbers = yes
        ->
            io.write_string("/* ", !IO),
            prog_out.write_context(Context, !IO),
            io.write_string(" pragma foreign_code */\n", !IO)
        ;
            true
        ),
        output_set_line_num(Info, Context, !IO),
        io.write_string(Foreign_Code, !IO),
        io.write_string("\n", !IO),
        output_reset_line_num(Info, !IO)
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        unexpected(this_file, "output_user_foreign_code: unimplemented: " ++
            "foreign code other than C")
    ).

:- pred output_foreign_header_include_lines(llds_out_info::in,
    list(foreign_decl_code)::in, io::di, io::uo) is det.

output_foreign_header_include_lines(Info, Decls, !IO) :-
    list.foldl2(output_foreign_header_include_line(Info), Decls,
        set.init, _, !IO).

:- pred output_foreign_header_include_line(llds_out_info::in,
    foreign_decl_code::in, set(string)::in, set(string)::out,
    io::di, io::uo) is det.

output_foreign_header_include_line(Info, Decl, !AlreadyDone, !IO) :-
    Decl = foreign_decl_code(Lang, _IsLocal, Code, Context),
    (
        Lang = lang_c,
        ( set.member(Code, !.AlreadyDone) ->
            true
        ;
            set.insert(Code, !AlreadyDone),
            AutoComments = Info ^ lout_auto_comments,
            LineNumbers = Info ^ lout_line_numbers,
            (
                AutoComments = yes,
                LineNumbers = yes
            ->
                io.write_string("/* ", !IO),
                prog_out.write_context(Context, !IO),
                io.write_string(" pragma foreign_decl_code(", !IO),
                io.write(Lang, !IO),
                io.write_string(") */\n", !IO)
            ;
                true
            ),
            output_set_line_num(Info, Context, !IO),
            io.write_string(Code, !IO),
            io.write_string("\n", !IO),
            output_reset_line_num(Info, !IO)
        )
    ;
        ( Lang = lang_java
        ; Lang = lang_csharp
        ; Lang = lang_il
        ; Lang = lang_erlang
        ),
        unexpected(this_file, "output_user_foreign_code: unexpected: " ++
            "foreign decl code other than C")
    ).

:- pred output_record_c_label_decls(llds_out_info::in,
    list(label)::in, list(label)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_c_label_decls(Info, EntryLabels, InternalLabels,
        !DeclSet, !IO) :-
    group_decl_c_labels(InternalLabels, multi_map.init, InternalLabelMap),
    multi_map.to_assoc_list(InternalLabelMap, InternalLabelList),
    list.foldl2(output_record_internal_label_decls, InternalLabelList,
        !DeclSet, !IO),
    list.foldl2(output_record_entry_label_decl(Info), EntryLabels,
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
        unexpected(this_file, "group_decl_c_labels: entry label")
    ),
    group_decl_c_labels(Labels, !InternalLabelMap).

:- pred output_record_internal_label_decls(pair(proc_label, list(int))::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_internal_label_decls(ProcLabel - RevLabelNums, !DeclSet, !IO) :-
    list.reverse(RevLabelNums, LabelNums),
    % There must be a macro of the form MR_decl_label<n> for every <n>
    % up to MaxChunkSize.
    MaxChunkSize = 10,
    list.chunk(LabelNums, MaxChunkSize, LabelNumChunks),
    list.foldl2(output_record_internal_label_decl_group(ProcLabel),
        LabelNumChunks, !DeclSet, !IO),
    list.foldl(insert_internal_label_code_addr_decl(ProcLabel), LabelNums,
        !DeclSet).

:- pred output_record_internal_label_decl_group(proc_label::in, list(int)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_internal_label_decl_group(ProcLabel, LabelNums,
        !DeclSet, !IO) :-
    io.write_string("MR_decl_label", !IO),
    io.write_int(list.length(LabelNums), !IO),
    io.write_string("(", !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(", ", !IO),
    io.write_list(LabelNums, ",", io.write_int, !IO),
    io.write_string(")\n", !IO),
    list.foldl(insert_internal_label_code_addr_decl(ProcLabel), LabelNums,
        !DeclSet).

:- pred insert_internal_label_code_addr_decl(proc_label::in, int::in,
    decl_set::in, decl_set::out) is det.

insert_internal_label_code_addr_decl(ProcLabel, LabelNum, !DeclSet) :-
    DeclId = decl_code_addr(code_label(internal_label(LabelNum, ProcLabel))),
    decl_set_insert(DeclId, !DeclSet).

:- pred output_record_entry_label_decl(llds_out_info::in, label::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_entry_label_decl(_Info, Label, !DeclSet, !IO) :-
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
        unexpected(this_file,
            "output_record_entry_label_decl: internal label")
    ),
    io.write_string(DeclMacro, !IO),
    io.write_string("(", !IO),
    output_label_no_prefix(Label, !IO),
    io.write_string(")\n", !IO),
    decl_set_insert(decl_code_addr(code_label(Label)), !DeclSet).

:- pred output_record_stack_layout_decl(llds_out_info::in, data_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_stack_layout_decl(Info, DataId, !DeclSet, !IO) :-
    output_record_data_id_decls(Info, DataId, !DeclSet, !IO).

%----------------------------------------------------------------------------%

:- pred output_c_label_inits(llds_out_info::in,
    list(label)::in, list(label)::in, io::di, io::uo) is det.

output_c_label_inits(Info, EntryLabels, InternalLabels, !IO) :-
    EntryLabelToLayoutMap = Info ^ lout_entry_label_to_layout,
    list.foldl(output_c_entry_label_init(EntryLabelToLayoutMap),
        EntryLabels, !IO),

    InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
    group_init_c_labels(InternalLabelToLayoutMap, InternalLabels,
        multi_map.init, NoLayoutInternalMap,
        multi_map.init, NoVarLayoutInternalMap,
        multi_map.init, SVarLayoutInternalMap,
        multi_map.init, LVarLayoutInternalMap),

    multi_map.to_assoc_list(NoLayoutInternalMap, NoLayoutInternalList),
    list.foldl(output_c_internal_label_no_layout_init_group,
        NoLayoutInternalList, !IO),

    multi_map.to_assoc_list(NoVarLayoutInternalMap, NoVarLayoutInternalList),
    list.foldl(output_c_internal_label_layout_init_group(Info, "_nvi"),
        NoVarLayoutInternalList, !IO),

    multi_map.to_assoc_list(SVarLayoutInternalMap, SVarLayoutInternalList),
    list.foldl(output_c_internal_label_layout_init_group(Info, "_svi"),
        SVarLayoutInternalList, !IO),

    multi_map.to_assoc_list(LVarLayoutInternalMap, LVarLayoutInternalList),
    list.foldl(output_c_internal_label_layout_init_group(Info, "_lvi"),
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
        ( map.search(InternalLabelToLayoutMap, Label, Slot) ->
            Slot = layout_slot(ArrayName, SlotNum),
            ( ArrayName = label_layout_array(Vars) ->
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
            ;
                unexpected(this_file, "group_init_c_labels: bad slot type")
            )
        ;
            multi_map.set(ProcLabel, LabelNum, !NoLayoutMap)
        )
    ;
        Label = entry_label(_, _),
        unexpected(this_file, "group_init_c_labels: entry label")
    ),
    group_init_c_labels(InternalLabelToLayoutMap, Labels,
        !NoLayoutMap, !NoVarLayoutMap, !SVarLayoutMap, !LVarLayoutMap).

:- pred output_c_internal_label_no_layout_init_group(
    pair(proc_label, list(int))::in, io::di, io::uo) is det.

output_c_internal_label_no_layout_init_group(ProcLabel - RevLabelNums, !IO) :-
    list.reverse(RevLabelNums, LabelNums),
    % There must be macros of the form MR_init_label_nvi<n> for every <n>
    % up to MaxChunkSize.
    MaxChunkSize = 10,
    list.chunk(LabelNums, MaxChunkSize, LabelNumChunks),
    list.foldl(output_c_internal_label_no_layout_init_chunk(ProcLabel),
        LabelNumChunks, !IO).

:- pred output_c_internal_label_no_layout_init_chunk(proc_label::in,
    list(int)::in, io::di, io::uo) is det.

output_c_internal_label_no_layout_init_chunk(ProcLabel, LabelNums, !IO) :-
    io.write_string("\tMR_init_label", !IO),
    io.write_int(list.length(LabelNums), !IO),
    io.write_string("(", !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(",", !IO),
    io.write_list(LabelNums, ",", io.write_int, !IO),
    io.write_string(")\n", !IO).

:- pred output_c_internal_label_layout_init_group(llds_out_info::in,
    string::in, pair(proc_label, list({int, int}))::in,
    io::di, io::uo) is det.

output_c_internal_label_layout_init_group(Info, Suffix,
        ProcLabel - RevLabelSlotNums, !IO) :-
    list.reverse(RevLabelSlotNums, LabelSlotNums),
    % There must be macros of the form MR_init_label_vi<n> for every <n>
    % up to MaxChunkSize.
    MaxChunkSize = 10,
    list.chunk(LabelSlotNums, MaxChunkSize, LabelSlotNumChunks),
    list.foldl(
        output_c_internal_label_layout_init_chunk(Info, Suffix, ProcLabel),
        LabelSlotNumChunks, !IO).

:- pred output_c_internal_label_layout_init_chunk(llds_out_info::in,
    string::in, proc_label::in, list({int, int})::in, io::di, io::uo) is det.

output_c_internal_label_layout_init_chunk(Info, Suffix, ProcLabel,
        LabelSlotNums, !IO) :-
    io.write_string("\tMR_init_label", !IO),
    io.write_string(Suffix, !IO),
    io.write_int(list.length(LabelSlotNums), !IO),
    io.write_string("(", !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(", ", !IO),
    ModuleName = Info ^ lout_mangled_module_name,
    io.write_string(ModuleName, !IO),
    io.write_string(",\n\t\t", !IO),
    io.write_list(LabelSlotNums, ", ", write_int_pair, !IO),
    io.write_string(")\n", !IO).

:- pred write_int_pair({int, int}::in, io::di, io::uo) is det.

write_int_pair({LabelNum, SlotNum}, !IO) :-
    io.write_int(LabelNum, !IO),
    io.write_string(",", !IO),
    io.write_int(SlotNum, !IO).

:- pred output_c_entry_label_init(map(label, data_id)::in, label::in,
    io::di, io::uo) is det.

output_c_entry_label_init(EntryLabelToLayoutMap, Label, !IO) :-
    ( map.search(EntryLabelToLayoutMap, Label, _LayoutId) ->
        SuffixOpen = "_sl("
    ;
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
        unexpected(this_file, "output_c_entry_label_init: internal/2")
    ),
    io.write_string(TabInitMacro, !IO),
    io.write_string(SuffixOpen, !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(");\n", !IO),

    io.write_string("\tMR_INIT_PROC_LAYOUT_ADDR(", !IO),
    output_label(Label, !IO),
    io.write_string(");\n", !IO).

%----------------------------------------------------------------------------%

:- pred output_record_c_procedure_decls(llds_out_info::in, c_procedure::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_c_procedure_decls(Info, Proc, !DeclSet, !IO) :-
    Instrs = Proc ^ cproc_code,
    CGlobalVarSet = Proc ^ cproc_c_global_vars,
    set.to_sorted_list(CGlobalVarSet, CGlobalVars),
    list.foldl2(output_c_global_var_decl, CGlobalVars, !DeclSet, !IO),
    list.foldl2(output_record_instruction_decls(Info), Instrs, !DeclSet, !IO).

:- pred output_c_global_var_decl(string::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_global_var_decl(VarName, !DeclSet, !IO) :-
    GlobalVar = env_var_ref(VarName),
    ( decl_set_is_member(decl_c_global_var(GlobalVar), !.DeclSet) ->
        true
    ;
        decl_set_insert(decl_c_global_var(GlobalVar), !DeclSet),
        io.write_string("extern MR_Word ", !IO),
        io.write_string(c_global_var_name(GlobalVar), !IO),
        io.write_string(";\n", !IO)
    ).

:- pred output_c_procedure(llds_out_info::in, c_procedure::in,
    io::di, io::uo) is det.

output_c_procedure(Info, Proc, !IO) :-
    Name = Proc ^ cproc_name,
    Arity = Proc ^ cproc_orig_arity,
    Instrs = Proc ^ cproc_code,
    PredProcId = Proc ^ cproc_id,
    PredProcId = proc(_, ProcId),
    proc_id_to_int(ProcId, ModeNum),

    io.write_string("\n/*-------------------------------------", !IO),
    io.write_string("------------------------------------*/\n", !IO),
    io.write_string("/* code for '", !IO),
    % Now that we have unused_args.m mangling predicate names,
    % we should probably demangle them here.
    io.write_string(Name, !IO),
    io.write_string("'/", !IO),
    io.write_int(Arity, !IO),
    io.write_string(" mode ", !IO),
    io.write_int(ModeNum, !IO),
    io.write_string(" */\n", !IO),

    find_caller_label(Instrs, CallerLabel),
    find_cont_labels(Instrs, set_tree234.init, ContLabelSet),
    EmitCLoops = Info ^ lout_emit_c_loops,
    (
        EmitCLoops = yes,
        find_while_labels(Instrs, set_tree234.init, WhileSet)
    ;
        EmitCLoops = no,
        WhileSet = set_tree234.init
    ),
    LocalThreadEngineBase = Info ^ lout_local_thread_engine_base,
    (
        LocalThreadEngineBase = yes,
        io.write_string("#ifdef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string("\t#undef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string("\t#define MR_maybe_local_thread_engine_base " ++
            "MR_local_thread_engine_base\n", !IO),
        io.write_string("#endif\n", !IO)
    ;
        LocalThreadEngineBase = no
    ),
    output_instruction_list(Info, Instrs, CallerLabel - ContLabelSet,
        WhileSet, not_after_layout_label, !IO),
    (
        LocalThreadEngineBase = yes,
        io.write_string("#ifdef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string("\t#undef MR_maybe_local_thread_engine_base\n", !IO),
        io.write_string("\t#define MR_maybe_local_thread_engine_base " ++
            "MR_thread_engine_base\n", !IO),
        io.write_string("#endif\n", !IO)
    ;
        LocalThreadEngineBase = no
    ).

    % Find the entry label for the procedure, for use as the profiling
    % "caller label" field in calls within this procedure.
    %
:- pred find_caller_label(list(instruction)::in, label::out) is det.

find_caller_label([], _) :-
    unexpected(this_file, "cannot find caller label").
find_caller_label([llds_instr(Uinstr, _) | Instrs], CallerLabel) :-
    ( Uinstr = label(Label) ->
        (
            Label = internal_label(_, _),
            unexpected(this_file, "caller label is internal label")
        ;
            Label = entry_label(_, _),
            CallerLabel = Label
        )
    ;
        find_caller_label(Instrs, CallerLabel)
    ).

    % Locate all the labels which are the continuation labels for calls,
    % nondet disjunctions, forks or joins, and store them in ContLabelSet.
    %
:- pred find_cont_labels(list(instruction)::in,
    set_tree234(label)::in, set_tree234(label)::out) is det.

find_cont_labels([], !ContLabelSet).
find_cont_labels([Instr | Instrs], !ContLabelSet) :-
    Instr = llds_instr(Uinstr, _),
    (
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
    ->
        set_tree234.insert(ContLabel, !ContLabelSet)
    ;
        Uinstr = fork_new_child(_, Label1)
    ->
        set_tree234.insert(Label1, !ContLabelSet)
    ;
        Uinstr = block(_, _, Block)
    ->
        find_cont_labels(Block, !ContLabelSet)
    ;
        true
    ),
    find_cont_labels(Instrs, !ContLabelSet).

    % Locate all the labels which can be profitably turned into
    % labels starting while loops. The idea is to do this transform:
    %
    % L1:                       L1:
    %                           while (1) {
    %   ...                     ...
    %   if (...) goto L1        if (...) continue
    %   ...        =>       ...
    %   if (...) goto L?        if (...) goto L?
    %   ...             ...
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
    (
        Uinstr0 = label(Label),
        is_while_label(Label, Instrs0, Instrs1, 0, UseCount),
        UseCount > 0
    ->
        set_tree234.insert(Label, !WhileSet),
        find_while_labels(Instrs1, !WhileSet)
    ;
        find_while_labels(Instrs0, !WhileSet)
    ).

:- pred is_while_label(label::in,
    list(instruction)::in, list(instruction)::out, int::in, int::out) is det.

is_while_label(_, [], [], !Count).
is_while_label(Label, [Instr0 | Instrs0], Instrs, !Count) :-
    Instr0 = llds_instr(Uinstr0, _),
    ( Uinstr0 = label(_) ->
        Instrs = [Instr0 | Instrs0]
    ;
        ( Uinstr0 = goto(code_label(Label)) ->
            !:Count = !.Count + 1
        ; Uinstr0 = if_val(_, code_label(Label)) ->
            !:Count = !.Count + 1
        ; Uinstr0 = block(_, _, BlockInstrs) ->
            count_while_label_in_block(Label, BlockInstrs, !Count)
        ;
            true
        ),
        is_while_label(Label, Instrs0, Instrs, !Count)
    ).

:- pred count_while_label_in_block(label::in, list(instruction)::in,
    int::in, int::out) is det.

count_while_label_in_block(_, [], !Count).
count_while_label_in_block(Label, [Instr0 | Instrs0], !Count) :-
    Instr0 = llds_instr(Uinstr0, _),
    ( Uinstr0 = label(_) ->
        unexpected(this_file, "label in block")
    ;
        ( Uinstr0 = goto(code_label(Label)) ->
            !:Count = !.Count + 1
        ; Uinstr0 = if_val(_, code_label(Label)) ->
            !:Count = !.Count + 1
        ; Uinstr0 = block(_, _, _) ->
            unexpected(this_file, "block in block")
        ;
            true
        ),
        count_while_label_in_block(Label, Instrs0, !Count)
    ).

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
    (
        InclCodeAddr = yes,
        globals.lookup_bool_option(Globals, static_code_addresses, no)
    ->
        ""
    ;
        "const "
    ).

%----------------------------------------------------------------------------%

:- pred output_label_defn(label::in, io::di, io::uo) is det.

output_label_defn(entry_label(entry_label_exported, ProcLabel), !IO) :-
    io.write_string("MR_define_entry(", !IO),
    output_label(entry_label(entry_label_exported, ProcLabel), !IO),
    io.write_string(");\n", !IO).
output_label_defn(entry_label(entry_label_local, ProcLabel), !IO) :-
    io.write_string("MR_def_static(", !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(")\n", !IO).
output_label_defn(entry_label(entry_label_c_local, ProcLabel), !IO) :-
    io.write_string("MR_def_local(", !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(")\n", !IO).
output_label_defn(internal_label(Num, ProcLabel), !IO) :-
    io.write_string("MR_def_label(", !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(",", !IO),
    io.write_int(Num, !IO),
    io.write_string(")\n", !IO).

%----------------------------------------------------------------------------%

:- pred gather_c_file_labels(list(comp_gen_c_module)::in,
    list(label)::out, list(label)::out) is det.

gather_c_file_labels(Modules, EntryLabels, InternalLabels) :-
    gather_labels_from_c_modules_acc(Modules,
        [], RevEntryLabels, [], RevInternalLabels),
    list.reverse(RevEntryLabels, EntryLabels),
    list.reverse(RevInternalLabels, InternalLabels).

:- pred gather_c_module_labels(list(c_procedure)::in,
    list(label)::out, list(label)::out) is det.

gather_c_module_labels(Procs, EntryLabels, InternalLabels) :-
    gather_labels_from_c_procs_acc(Procs,
        [], RevEntryLabels, [], RevInternalLabels),
    list.reverse(RevEntryLabels, EntryLabels),
    list.reverse(RevInternalLabels, InternalLabels).

%----------------------------------------------------------------------------%

:- pred gather_labels_from_c_modules_acc(list(comp_gen_c_module)::in,
    list(label)::in, list(label)::out,
    list(label)::in, list(label)::out) is det.

gather_labels_from_c_modules_acc([], !RevEntryLabels, !RevInternalLabels).
gather_labels_from_c_modules_acc([Module | Modules],
        !RevEntryLabels, !RevInternalLabels) :-
    gather_labels_from_c_module_acc(Module,
        !RevEntryLabels, !RevInternalLabels),
    gather_labels_from_c_modules_acc(Modules,
        !RevEntryLabels, !RevInternalLabels).

:- pred gather_labels_from_c_module_acc(comp_gen_c_module::in,
    list(label)::in, list(label)::out,
    list(label)::in, list(label)::out) is det.

gather_labels_from_c_module_acc(comp_gen_c_module(_, Procs),
        !RevEntryLabels, !RevInternalLabels) :-
    gather_labels_from_c_procs_acc(Procs,
        !RevEntryLabels, !RevInternalLabels).

:- pred gather_labels_from_c_procs_acc(list(c_procedure)::in,
    list(label)::in, list(label)::out,
    list(label)::in, list(label)::out) is det.

gather_labels_from_c_procs_acc([], !RevEntryLabels, !RevInternalLabels).
gather_labels_from_c_procs_acc([Proc | Procs],
        !RevEntryLabels, !RevInternalLabels) :-
    Instrs = Proc ^ cproc_code,
    gather_labels_from_instrs_acc(Instrs,
        !RevEntryLabels, !RevInternalLabels),
    gather_labels_from_c_procs_acc(Procs,
        !RevEntryLabels, !RevInternalLabels).

:- pred gather_labels_from_instrs_acc(list(instruction)::in,
    list(label)::in, list(label)::out,
    list(label)::in, list(label)::out) is det.

gather_labels_from_instrs_acc([], !RevEntryLabels, !RevInternalLabels).
gather_labels_from_instrs_acc([Instr | Instrs],
        !RevEntryLabels, !RevInternalLabels) :-
    ( Instr = llds_instr(label(Label), _) ->
        (
            Label = entry_label(_, _),
            !:RevEntryLabels = [Label | !.RevEntryLabels]
        ;
            Label = internal_label(_, _),
            !:RevInternalLabels = [Label | !.RevInternalLabels]
        )
    ;
        true
    ),
    gather_labels_from_instrs_acc(Instrs,
        !RevEntryLabels, !RevInternalLabels).

%----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "llds_out_file.m".

%---------------------------------------------------------------------------%
:- end_module llds_out_file.
%---------------------------------------------------------------------------%
