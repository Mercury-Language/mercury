%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 1996-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: llds_out.m.
% Main authors: conway, fjh, zs.
%
% This module defines the routines for printing out LLDS,
% the Low Level Data Structure.
%
%----------------------------------------------------------------------------%

:- module ll_backend.llds_out.
:- interface.

:- import_module backend_libs.rtti.
:- import_module hlds.hlds_llds.
:- import_module hlds.hlds_pred.
:- import_module libs.globals.
:- import_module libs.trace_params.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.

%----------------------------------------------------------------------------%

:- type llds_out_info
    --->    llds_out_info(
                lout_module_name                :: module_name,
                lout_mangled_module_name        :: string,
                lout_internal_label_to_layout   :: map(label,
                                                    layout_slot_name),
                lout_entry_label_to_layout      :: map(label, data_id),
                lout_table_io_decl_map          :: map(pred_proc_id,
                                                    layout_slot_name),
                lout_auto_comments              :: bool,
                lout_line_numbers               :: bool,
                lout_emit_c_loops               :: bool,
                lout_generate_bytecode          :: bool,
                lout_local_thread_engine_base   :: bool,
                lout_profile_calls              :: bool,
                lout_profile_time               :: bool,
                lout_profile_memory             :: bool,
                lout_profile_deep               :: bool,
                lout_unboxed_float              :: bool,
                lout_static_ground_floats       :: bool,
                lout_use_macro_for_redo_fail    :: bool,
                lout_trace_level                :: trace_level,
                lout_globals                    :: globals
            ).

%----------------------------------------------------------------------------%

    % Given a 'c_file' structure, output the LLDS code inside it
    % into a .c file. The second argument gives the set of labels that have
    % layout structures.
    %
:- pred output_llds(globals::in, c_file::in, io::di, io::uo) is det.

    % output_record_rval_decls(Info, Rval, !DeclSet) outputs the declarations
    % of any static constants, etc. that need to be declared before
    % output_rval(Rval) is called.
    %
:- pred output_record_rval_decls(llds_out_info::in, rval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % output an rval (not converted to any particular type,
    % but instead output as its "natural" type)
    %
:- pred output_rval(llds_out_info::in, rval::in, io::di, io::uo) is det.

    % output_record_code_addr_decls(Info, CodeAddr, ...) outputs the
    % declarations of any extern symbols, etc. that need to be declared
    % before output_code_addr(CodeAddr) is called.
    %
:- pred output_record_code_addr_decls(llds_out_info::in, code_addr::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_code_addr(code_addr::in, io::di, io::uo) is det.

    % output_record_data_addr_decls(Info, DataAddr, ...) outputs the
    % declarations of any static constants, etc. that need to be declared
    % before output_data_addr(DataAddr) is called.
    %
:- pred output_record_data_id_decls(llds_out_info::in, data_id::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.
:- pred output_record_data_id_decls_format(llds_out_info::in, data_id::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

:- pred output_record_data_ids_decls(llds_out_info::in, list(data_id)::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

:- pred output_data_id(llds_out_info::in, data_id::in, io::di, io::uo)
    is det.

:- pred output_data_id_addr(llds_out_info::in, data_id::in, io::di, io::uo)
    is det.

:- func proc_tabling_info_var_name(proc_label) = string.

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

    % Return the suffix after do_call_closure_ or do_call_class_method_
    % represented by the given variant.
    %
:- func ho_call_variant_to_string(ho_call_variant) = string.

    % Convert an lval to a string description of that lval.
    %
:- func lval_to_string(lval) = string is semidet.

    % Convert a register to a string description of that register.
    %
:- func reg_to_string(reg_type, int) = string.

    % Output an instruction and (if the third arg is yes) the comment.
    % This predicate is provided for debugging use only.
    %
:- pred output_debug_instruction_and_comment(llds_out_info::in,
    instr::in, string::in, io::di, io::uo) is det.

    % Output an instruction.
    % This predicate is provided for debugging use only.
    %
:- pred output_debug_instruction(llds_out_info::in, instr::in,
    io::di, io::uo) is det.

    % Output a label (used by garbage collection).
    %
:- pred output_label(label::in, io::di, io::uo) is det.

    % Output a label without the standard mercury__ prefix.
    %
:- pred output_label(label::in, bool::in, io::di, io::uo) is det.

    % Convert a label to a C string. The boolean controls whether
    % a prefix ("mercury__") is added to the string.
    %
:- func label_to_c_string(label, bool) = string.

    % The following are exported to rtti_out. It may be worthwhile
    % to put these in a new module (maybe llds_out_util).

:- type decl_id
    --->    decl_float_label(string)
    ;       decl_common_type(type_num)
    ;       decl_code_addr(code_addr)
    ;       decl_rtti_id(rtti_id)
    ;       decl_layout_id(layout_name)
    ;       decl_tabling_id(proc_label, proc_tabling_struct_id)
    ;       decl_foreign_proc_struct(string)
    ;       decl_c_global_var(c_global_var_ref)
    ;       decl_type_info_like_struct(int)
    ;       decl_typeclass_constraint_struct(int).

:- type decl_set.

    % Every time we emit a declaration for a symbol, we insert it into the
    % set of symbols we've already declared. That way, we avoid generating
    % the same symbol twice, which would cause an error in the C code.

:- pred decl_set_init(decl_set::out) is det.

:- pred decl_set_insert(decl_id::in, decl_set::in, decl_set::out) is det.

:- pred decl_set_is_member(decl_id::in, decl_set::in) is semidet.

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

:- func explain_stack_slots(stack_slots, prog_varset) = string.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.builtin_ops.
:- import_module backend_libs.c_util.
:- import_module backend_libs.export.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module libs.compiler_util.
:- import_module libs.options.
:- import_module ll_backend.layout_out.
:- import_module ll_backend.pragma_c_gen.
:- import_module ll_backend.rtti_out.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.modules.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module char.
:- import_module deconstruct.
:- import_module int.
:- import_module library.   % for the version number.
:- import_module maybe.
:- import_module multi_map.
:- import_module pair.
:- import_module set.
:- import_module set_tree234.
:- import_module string.
:- import_module svmulti_map.
:- import_module term.
:- import_module varset.

%----------------------------------------------------------------------------%

:- type decl_set == set_tree234(decl_id).

decl_set_init(DeclSet) :-
    DeclSet = set_tree234.init.

decl_set_insert(DeclId, DeclSet0, DeclSet) :-
    set_tree234.insert(DeclId, DeclSet0, DeclSet).

decl_set_is_member(DeclId, DeclSet) :-
    set_tree234.contains(DeclSet, DeclId).

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
        io.write_string("\n", !IO),
        io.write_string(ProgName, !IO),
        io.write_string(": can't open `", !IO),
        io.write_string(FileName, !IO),
        io.write_string("' for output:\n", !IO),
        io.write_string(io.error_message(Error), !IO),
        io.write_string("\n", !IO),
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
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes,
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
        ProcEventLayouts, ExecTraces, !DeclSet, !IO),

    list.foldl2(output_comp_gen_c_module(Info), Modules, !DeclSet, !IO),
    list.foldl(output_user_foreign_code(Info), UserForeignCode, !IO),
    list.foldl(io.write_string, Exports, !IO),
    io.write_string("\n", !IO),
    output_c_module_init_list(Info, ModuleName, Modules, RttiDatas,
        ProcLayoutDatas, ModuleLayoutDatas, ComplexityProcs,
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
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_c_module_init_list(Info, ModuleName, Modules, RttiDatas,
        ProcLayoutDatas, ModuleLayoutDatas, ComplexityProcs,
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

:- func complexity_arg_info_array_name(int) = string.

complexity_arg_info_array_name(ProcNum) =
    "MR_complexity_arg_info_" ++ int_to_string(ProcNum).

:- pred output_complexity_arg_info_arrays(list(complexity_proc_info)::in,
    io::di, io::uo) is det.

output_complexity_arg_info_arrays([], !IO).
output_complexity_arg_info_arrays([Info | Infos], !IO) :-
    Info = complexity_proc_info(ProcNum, _, Args),
    io.write_string("\nMR_ComplexityArgInfo ", !IO),
    io.write_string(complexity_arg_info_array_name(ProcNum), !IO),
    io.write_string("[", !IO),
    io.write_int(list.length(Args), !IO),
    io.write_string("] = {\n", !IO),
    output_complexity_arg_info_array(Args, !IO),
    io.write_string("};\n", !IO),
    output_complexity_arg_info_arrays(Infos, !IO).

:- pred output_complexity_arg_info_array(list(complexity_arg_info)::in,
    io::di, io::uo) is det.

output_complexity_arg_info_array([], !IO).
output_complexity_arg_info_array([Arg | Args], !IO) :-
    Arg = complexity_arg_info(MaybeName, Kind),
    io.write_string("{ ", !IO),
    (
        MaybeName = yes(Name),
        io.write_string("""", !IO),
        io.write_string(Name, !IO),
        io.write_string(""", ", !IO)
    ;
        MaybeName = no,
        io.write_string("NULL, ", !IO)
    ),
    (
        Kind = complexity_input_variable_size,
        io.write_string("MR_COMPLEXITY_INPUT_VAR_SIZE", !IO)
    ;
        Kind = complexity_input_fixed_size,
        io.write_string("MR_COMPLEXITY_INPUT_FIX_SIZE", !IO)
    ;
        Kind = complexity_output,
        io.write_string("MR_COMPLEXITY_OUTPUT", !IO)
    ),
    io.write_string(" },\n", !IO),
    output_complexity_arg_info_array(Args, !IO).

:- pred output_init_complexity_proc_list(list(complexity_proc_info)::in,
    io::di, io::uo) is det.

output_init_complexity_proc_list([], !IO).
output_init_complexity_proc_list([Info | Infos], !IO) :-
    Info = complexity_proc_info(ProcNum, FullProcName, ArgInfos),
    io.write_string("\tMR_init_complexity_proc(", !IO),
    io.write_int(ProcNum, !IO),
    io.write_string(", """, !IO),
    c_util.output_quoted_string(FullProcName, !IO),
    io.write_string(""", ", !IO),
    list.filter(complexity_arg_is_profiled, ArgInfos, ProfiledArgInfos),
    io.write_int(list.length(ProfiledArgInfos), !IO),
    io.write_string(", ", !IO),
    io.write_int(list.length(ArgInfos), !IO),
    io.write_string(", ", !IO),
    io.write_string(complexity_arg_info_array_name(ProcNum), !IO),
    io.write_string(");\n", !IO),
    output_init_complexity_proc_list(Infos, !IO).

:- pred complexity_arg_is_profiled(complexity_arg_info::in) is semidet.

complexity_arg_is_profiled(complexity_arg_info(_, Kind)) :-
    Kind = complexity_input_variable_size.

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

:- pred output_tabling_info_struct(llds_out_info::in, tabling_info_struct::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_tabling_info_struct(Info, TablingInfoStruct, !DeclSet, !IO) :-
    TablingInfoStruct = tabling_info_struct(ProcLabel, EvalMethod,
        NumInputs, NumOutputs, InputSteps, MaybeOutputSteps, PTIVectorRval,
        TypeParamsRval, MaybeSizeLimit, Stats),

    InfoDataId =
        proc_tabling_data_id(ProcLabel, tabling_info),
    InputStepsDataId =
        proc_tabling_data_id(ProcLabel, tabling_steps_desc(call_table)),
    OutputStepsDataId =
        proc_tabling_data_id(ProcLabel, tabling_steps_desc(answer_table)),
    TipsDataId =
        proc_tabling_data_id(ProcLabel, tabling_tips),

    CallStatsDataId =
        proc_tabling_data_id(ProcLabel,
            tabling_stat_steps(call_table, curr_table)),
    PrevCallStatsDataId =
        proc_tabling_data_id(ProcLabel,
            tabling_stat_steps(call_table, prev_table)),

    AnswerStatsDataId =
        proc_tabling_data_id(ProcLabel,
            tabling_stat_steps(answer_table, curr_table)),
    PrevAnswerStatsDataId =
        proc_tabling_data_id(ProcLabel,
            tabling_stat_steps(answer_table, prev_table)),

    output_table_steps_table(Info, InputStepsDataId, InputSteps, !IO),
    output_record_rval_decls(Info, PTIVectorRval, !DeclSet, !IO),

    (
        MaybeOutputSteps = no
    ;
        MaybeOutputSteps = yes(OutputStepsA),
        output_table_steps_table(Info, OutputStepsDataId, OutputStepsA, !IO),
        output_record_rval_decls(Info, PTIVectorRval, !DeclSet, !IO)
    ),

    (
        MaybeSizeLimit = no
    ;
        MaybeSizeLimit = yes(SizeLimit1),
        output_table_tips(Info, ProcLabel, SizeLimit1, !IO)
    ),

    (
        Stats = table_dont_gather_statistics
    ;
        Stats = table_gather_statistics,
        output_table_step_stats(Info, CallStatsDataId, InputSteps, !IO),
        output_table_step_stats(Info, PrevCallStatsDataId, InputSteps, !IO),
        (
            MaybeOutputSteps = no
        ;
            MaybeOutputSteps = yes(OutputStepsB),
            output_table_step_stats(Info, AnswerStatsDataId, OutputStepsB,
                !IO),
            output_table_step_stats(Info, PrevAnswerStatsDataId, OutputStepsB,
                !IO)
        )
    ),

    io.write_string("\nstatic MR_ProcTableInfo ", !IO),
    output_data_id(Info, InfoDataId, !IO),
    io.write_string(" = {\n", !IO),
    io.write_string(eval_method_to_table_type(EvalMethod), !IO),
    io.write_string(",\n", !IO),
    io.write_int(NumInputs, !IO),
    io.write_string(",\n", !IO),
    io.write_int(NumOutputs, !IO),
    io.write_string(",\n", !IO),
    (
        MaybeOutputSteps = no,
        io.write_string("0,\n", !IO)
    ;
        MaybeOutputSteps = yes(_),
        io.write_string("1,\n", !IO)
    ),
    io.write_string("(const MR_PseudoTypeInfo *) ", !IO),
    output_rval(Info, PTIVectorRval, !IO),
    io.write_string(",\n", !IO),
    io.write_string("(const MR_TypeParamLocns *) ", !IO),
    output_rval(Info, TypeParamsRval, !IO),
    io.write_string(",\n", !IO),
    io.write_string("{ 0 },\n", !IO),
    io.write_string("{\n", !IO),
    output_data_id_addr(Info, InputStepsDataId, !IO),
    io.write_string(",\n", !IO),
    (
        MaybeOutputSteps = no,
        io.write_string("NULL\n", !IO)
    ;
        MaybeOutputSteps = yes(_),
        output_data_id_addr(Info, OutputStepsDataId, !IO),
        io.write_string("\n", !IO)
    ),
    io.write_string("},\n", !IO),
    (
        Stats = table_dont_gather_statistics,
        io.write_string("{{{\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("NULL\n", !IO),
        io.write_string("},{\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("NULL\n", !IO),
        io.write_string("}},{{\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("NULL\n", !IO),
        io.write_string("},{\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("NULL\n", !IO),
        io.write_string("}}},\n", !IO)
    ;
        Stats = table_gather_statistics,
        io.write_string("{{{\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        output_data_id_addr(Info, CallStatsDataId, !IO),
        io.write_string("\n", !IO),
        io.write_string("},{\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        output_data_id_addr(Info, PrevCallStatsDataId, !IO),
        io.write_string("\n", !IO),
        io.write_string("}},{{\n", !IO),
        (
            MaybeOutputSteps = no,
            io.write_string("0,\n", !IO),
            io.write_string("0,\n", !IO),
            io.write_string("NULL\n", !IO),
            io.write_string("},{\n", !IO),
            io.write_string("0,\n", !IO),
            io.write_string("0,\n", !IO),
            io.write_string("NULL\n", !IO)
        ;
            MaybeOutputSteps = yes(_),
            io.write_string("0,\n", !IO),
            io.write_string("0,\n", !IO),
            output_data_id_addr(Info, AnswerStatsDataId, !IO),
            io.write_string("\n", !IO),
            io.write_string("},{\n", !IO),
            io.write_string("0,\n", !IO),
            io.write_string("0,\n", !IO),
            output_data_id_addr(Info, PrevAnswerStatsDataId, !IO),
            io.write_string("\n", !IO)
        ),
        io.write_string("}}},\n", !IO)
    ),
    (
        MaybeSizeLimit = no,
        io.write_string("-1,\n", !IO),
        io.write_string("NULL,\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0\n", !IO)
    ;
        MaybeSizeLimit = yes(SizeLimit2),
        io.write_int(SizeLimit2, !IO),
        io.write_string(",\n", !IO),
        output_data_id_addr(Info, TipsDataId, !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0\n", !IO)
    ),
    io.write_string("};\n", !IO),
    DeclId = decl_tabling_id(ProcLabel, tabling_info),
    decl_set_insert(DeclId, !DeclSet).

:- pred output_table_steps_table(llds_out_info::in, data_id::in,
    list(table_step_desc)::in, io::di, io::uo) is det.

output_table_steps_table(Info, DataId, StepDescs, !IO) :-
    io.write_string("\n", !IO),
    io.write_string("static const MR_TableStepDesc ", !IO),
    output_data_id(Info, DataId, !IO),
    io.write_string("[] = {\n", !IO),
    output_table_steps(StepDescs, !IO),
    io.write_string("};\n", !IO).

:- pred output_table_steps(list(table_step_desc)::in, io::di, io::uo) is det.

output_table_steps([], !IO).
output_table_steps([StepDesc | StepDescs], !IO) :-
    StepDesc = table_step_desc(VarName, Step),
    io.write_string("{ """, !IO),
    c_util.output_quoted_string(VarName, !IO),
    io.write_string(""", ", !IO),
    table_trie_step_to_c(Step, StepType, MaybeEnumRange),
    io.write_string(StepType, !IO),
    io.write_string(", ", !IO),
    (
        MaybeEnumRange = no,
        io.write_int(-1, !IO)
    ;
        MaybeEnumRange = yes(EnumRange),
        io.write_int(EnumRange, !IO)
    ),
    io.write_string(" },\n", !IO),
    output_table_steps(StepDescs, !IO).

:- pred output_table_tips(llds_out_info::in, proc_label::in, int::in,
    io::di, io::uo) is det.

output_table_tips(Info, ProcLabel, SizeLimit, !IO) :-
    % We don't need to initialize the elements of the array, since the
    % MR_pt_num_call_table_tips field explicitly says that none of the
    % array elements are meaningful.
    DataId = proc_tabling_data_id(ProcLabel, tabling_tips),
    io.write_string("\n", !IO),
    io.write_string("static MR_TrieNode ", !IO),
    output_data_id(Info, DataId, !IO),
    io.write_string("[", !IO),
    io.write_int(SizeLimit, !IO),
    io.write_string("];\n", !IO).

:- pred output_table_step_stats(llds_out_info::in,
    data_id::in, list(table_step_desc)::in, io::di, io::uo) is det.

output_table_step_stats(Info, DataId, Steps, !IO) :-
    % We don't need to initialize the elements of the array, because
    % we want to initialize all members of the array to structures
    % that contain all zeros, and C does that for us.
    io.write_string("\n", !IO),
    io.write_string("static MR_TableStepStats ", !IO),
    output_data_id(Info, DataId, !IO),
    io.write_string("[] = \n", !IO),
    io.write_string("{\n", !IO),
    output_table_step_stats_2(Steps, !IO),
    io.write_string("};\n", !IO).

:- pred output_table_step_stats_2(list(table_step_desc)::in, io::di, io::uo)
    is det.

output_table_step_stats_2([], !IO).
output_table_step_stats_2([StepDesc | StepDescs], !IO) :-
    StepDesc = table_step_desc(_VarName, Step),
    io.write_string("{ 0, 0, ", !IO),
    KindStr = table_step_stats_kind(Step),
    io.write_string(KindStr, !IO),
    io.write_string(",\n", !IO),
    % Initialize the fields about hash tables.
    io.write_string("0, 0, 0, 0, 0, 0, 0, 0, 0, ", !IO),
    % Initialize the fields about enums.
    io.write_string("0, 0, ", !IO),
    % Initialize the fields about du types.
    io.write_string("0, 0, 0, 0, ", !IO),
    % Initialize the fields about start tables.
    io.write_string("0, 0 },\n", !IO),
    output_table_step_stats_2(StepDescs, !IO).

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

:- pred output_common_type_defn(type_num::in, common_cell_type::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_type_defn(TypeNum, CellType, !DeclSet, !IO) :-
    TypeDeclId = decl_common_type(TypeNum),
    ( decl_set_is_member(TypeDeclId, !.DeclSet) ->
        true
    ;
        io.write_string("struct ", !IO),
        output_common_cell_type_name(TypeNum, !IO),
        io.write_string(" {\n", !IO),
        (
            CellType = plain_type(Types),
            output_cons_arg_types(Types, "\t", 1, !IO)
        ;
            CellType = grouped_args_type(ArgGroups),
            output_cons_arg_group_types(ArgGroups, "\t", 1, !IO)
        ),
        io.write_string("};\n", !IO),
        decl_set_insert(TypeDeclId, !DeclSet)
    ).

:- pred output_scalar_common_data_decl(scalar_common_data_array::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_scalar_common_data_decl(ScalarCommonDataArray, !DeclSet, !IO) :-
    ScalarCommonDataArray = scalar_common_data_array(CellType, TypeNum,
        _Values),
    io.write_string("\n", !IO),
    output_common_type_defn(TypeNum, CellType, !DeclSet, !IO),
    io.write_string("MR_STATIC_LINKAGE const struct ", !IO),
    output_common_cell_type_name(TypeNum, !IO),
    io.write_string(" ", !IO),
    output_common_scalar_cell_array_name(TypeNum, !IO),
    io.write_string("[];\n", !IO).

:- pred output_vector_common_data_decl(vector_common_data_array::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_vector_common_data_decl(VectorCommonDataArray, !DeclSet, !IO) :-
    VectorCommonDataArray = vector_common_data_array(CellType,
        TypeNum, CellNum, _Values),
    io.write_string("\n", !IO),
    output_common_type_defn(TypeNum, CellType, !DeclSet, !IO),
    io.write_string("MR_STATIC_LINKAGE const struct ", !IO),
    output_common_cell_type_name(TypeNum, !IO),
    io.write_string(" ", !IO),
    output_common_vector_cell_array_name(TypeNum, CellNum, !IO),
    io.write_string("[];\n", !IO).

%----------------------------------------------------------------------------%

:- pred output_scalar_common_data_defn(llds_out_info::in,
    scalar_common_data_array::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_scalar_common_data_defn(Info, ScalarCommonDataArray, !DeclSet, !IO) :-
    ScalarCommonDataArray = scalar_common_data_array(_CellType, TypeNum,
        Values),
    io.write_string("\n", !IO),
    ArgLists = list.map(common_cell_get_rvals, Values),
    list.condense(ArgLists, Args),
    output_record_rvals_decls(Info, Args, !DeclSet, !IO),

    io.write_string("static const struct ", !IO),
    output_common_cell_type_name(TypeNum, !IO),
    io.write_string(" ", !IO),
    output_common_scalar_cell_array_name(TypeNum, !IO),
    io.write_string("[", !IO),
    io.write_int(list.length(Values), !IO),
    io.write_string("] =\n{\n", !IO),
    list.foldl(output_common_cell_value(Info), Values, !IO),
    io.write_string("};\n", !IO).

:- pred output_vector_common_data_defn(llds_out_info::in,
    vector_common_data_array::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_vector_common_data_defn(Info, VectorCommonDataArray, !DeclSet, !IO) :-
    VectorCommonDataArray = vector_common_data_array(_CellType, TypeNum,
        CellNum, Values),
    io.write_string("\n", !IO),
    ArgLists = list.map(common_cell_get_rvals, Values),
    list.condense(ArgLists, Args),
    output_record_rvals_decls(Info, Args, !DeclSet, !IO),

    io.write_string("static const struct ", !IO),
    output_common_cell_type_name(TypeNum, !IO),
    io.write_string(" ", !IO),
    output_common_vector_cell_array_name(TypeNum, CellNum, !IO),
    io.write_string("[", !IO),
    io.write_int(list.length(Values), !IO),
    io.write_string("] =\n{\n", !IO),
    list.foldl(output_common_cell_value(Info), Values, !IO),
    io.write_string("};\n", !IO).

:- func common_cell_get_rvals(common_cell_value) = list(rval).

common_cell_get_rvals(Value) = Rvals :-
    (
        Value = plain_value(RvalsTypes),
        assoc_list.keys(RvalsTypes, Rvals)
    ;
        Value = grouped_args_value(Groups),
        RvalLists = list.map(common_group_get_rvals, Groups),
        list.condense(RvalLists, Rvals)
    ).

:- func common_group_get_rvals(common_cell_arg_group) = list(rval).

common_group_get_rvals(common_cell_grouped_args(_, _, Rvals)) = Rvals.
common_group_get_rvals(common_cell_ungrouped_arg(_, Rval)) = [Rval].

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
            set.insert(!.AlreadyDone, Code, !:AlreadyDone),
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
        svmulti_map.set(ProcLabel, LabelNum, !InternalLabelMap)
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
    output_label(Label, no, !IO),
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
                    svmulti_map.set(ProcLabel, Pair, !NoVarLayoutMap)
                ;
                    Vars = label_has_short_var_info,
                    svmulti_map.set(ProcLabel, Pair, !SVarLayoutMap)
                ;
                    Vars = label_has_long_var_info,
                    svmulti_map.set(ProcLabel, Pair, !LVarLayoutMap)
                )
            ;
                unexpected(this_file, "group_init_c_labels: bad slot type")
            )
        ;
            svmulti_map.set(ProcLabel, LabelNum, !NoLayoutMap)
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

:- pred output_record_instruction_decls(llds_out_info::in, instruction::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_instruction_decls(Info, Instr, !DeclSet, !IO) :-
    Instr = llds_instr(Uinstr, _),
    output_record_instr_decls(Info, Uinstr, !DeclSet, !IO).

:- pred output_record_instr_decls(llds_out_info::in, instr::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_instr_decls(Info, Instr, !DeclSet, !IO) :-
    (
        ( Instr = comment(_)
        ; Instr = livevals(_)
        ; Instr = arbitrary_c_code(_, _, _)
        ; Instr = label(_)
        ; Instr = push_region_frame(_, _)
        ; Instr = use_and_maybe_pop_region_frame(_, _)
        ; Instr = discard_ticket
        ; Instr = prune_ticket
        ; Instr = incr_sp(_, _, _)
        ; Instr = decr_sp(_)
        ; Instr = decr_sp_and_return(_)
        )
    ;
        Instr = block(_TempR, _TempF, Instrs),
        list.foldl2(output_record_instruction_decls(Info), Instrs,
            !DeclSet, !IO)
    ;
        Instr = assign(Lval, Rval),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO),
        output_record_rval_decls(Info, Rval, !DeclSet, !IO)
    ;
        Instr = keep_assign(Lval, Rval),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO),
        output_record_rval_decls(Info, Rval, !DeclSet, !IO)
    ;
        Instr = llcall(Target, ContLabel, _, _, _, _),
        output_record_code_addr_decls(Info, Target, !DeclSet, !IO),
        output_record_code_addr_decls(Info, ContLabel, !DeclSet, !IO)
    ;
        Instr = mkframe(FrameInfo, MaybeFailureContinuation),
        (
            FrameInfo = ordinary_frame(_, _, yes(Struct)),
            Struct = foreign_proc_struct(StructName, StructFields,
                MaybeStructFieldsContext)
        ->
            DeclId = decl_foreign_proc_struct(StructName),
            ( decl_set_is_member(DeclId, !.DeclSet) ->
                Msg = "struct " ++ StructName ++ " has been declared already",
                unexpected(this_file, Msg)
            ;
                true
            ),
            io.write_string("struct ", !IO),
            io.write_string(StructName, !IO),
            io.write_string(" {\n", !IO),
            (
                MaybeStructFieldsContext = yes(StructFieldsContext),
                output_set_line_num(Info, StructFieldsContext, !IO),
                io.write_string(StructFields, !IO),
                output_reset_line_num(Info, !IO)
            ;
                MaybeStructFieldsContext = no,
                io.write_string(StructFields, !IO)
            ),
            io.write_string("\n};\n", !IO),
            decl_set_insert(DeclId, !DeclSet)
        ;
            true
        ),
        (
            MaybeFailureContinuation = yes(FailureContinuation),
            output_record_code_addr_decls(Info, FailureContinuation,
                !DeclSet, !IO)
        ;
            MaybeFailureContinuation = no
        )
    ;
        Instr = goto(CodeAddr),
        output_record_code_addr_decls(Info, CodeAddr, !DeclSet, !IO)
    ;
        Instr = computed_goto(Rval, _MaybeLabels),
        output_record_rval_decls(Info, Rval, !DeclSet, !IO)
    ;
        Instr = if_val(Rval, Target),
        output_record_rval_decls(Info, Rval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, Target, !DeclSet, !IO)
    ;
        Instr = save_maxfr(Lval),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO)
    ;
        Instr = restore_maxfr(Lval),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO)
    ;
        Instr = incr_hp(Lval, _Tag, _, Rval, _, _, MaybeRegionRval,
            MaybeReuse),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO),
        output_record_rval_decls(Info, Rval, !DeclSet, !IO),
        (
            MaybeRegionRval = yes(RegionRval),
            output_record_rval_decls(Info, RegionRval, !DeclSet, !IO)
        ;
            MaybeRegionRval = no
        ),
        (
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            output_record_rval_decls(Info, ReuseRval, !DeclSet, !IO),
            (
                MaybeFlagLval = yes(FlagLval),
                output_record_lval_decls(Info, FlagLval, !DeclSet, !IO)
            ;
                MaybeFlagLval = no
            )
        ;
            MaybeReuse = no_llds_reuse
        )
    ;
        ( Instr = mark_hp(Lval)
        ; Instr = store_ticket(Lval)
        ; Instr = mark_ticket_stack(Lval)
        ; Instr = init_sync_term(Lval, _NumBranches)
        ),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO)
    ;
        ( Instr = restore_hp(Rval)
        ; Instr = free_heap(Rval)
        ; Instr = prune_tickets_to(Rval)
        ; Instr = reset_ticket(Rval, _Reason)
        ),
        output_record_rval_decls(Info, Rval, !DeclSet, !IO)
    ;
        Instr = region_fill_frame(_FillOp, _EmbeddedFrame, IdRval,
            NumLval, AddrLval),
        output_record_rval_decls(Info, IdRval, !DeclSet, !IO),
        output_record_lval_decls(Info, NumLval, !DeclSet, !IO),
        output_record_lval_decls(Info, AddrLval, !DeclSet, !IO)
    ;
        Instr = region_set_fixed_slot(_SetOp, _EmbeddedFrame, ValueRval),
        output_record_rval_decls(Info, ValueRval, !DeclSet, !IO)
    ;
        Instr = foreign_proc_code(_, Comps, _, _, _, _, _, _, _, _),
        list.foldl2(output_record_foreign_proc_component_decls(Info), Comps,
            !DeclSet, !IO)
    ;
        Instr = fork_new_child(Lval, Child),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, code_label(Child), !DeclSet, !IO)
    ;
        Instr = join_and_continue(Lval, Label),
        output_record_lval_decls(Info, Lval, !DeclSet, !IO),
        output_record_code_addr_decls(Info, code_label(Label), !DeclSet, !IO)
    ).

:- pred output_record_foreign_proc_component_decls(llds_out_info::in,
    foreign_proc_component::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_foreign_proc_component_decls(Info, Component, !DeclSet, !IO) :-
    (
        Component = foreign_proc_inputs(Inputs),
        output_record_foreign_proc_input_rval_decls(Info, Inputs,
            !DeclSet, !IO)
    ;
        Component = foreign_proc_outputs(Outputs),
        output_record_foreign_proc_output_lval_decls(Info, Outputs,
            !DeclSet, !IO)
    ;
        ( Component = foreign_proc_raw_code(_, _, _, _)
        ; Component = foreign_proc_user_code(_, _, _)
        ; Component = foreign_proc_fail_to(_)
        ; Component = foreign_proc_noop
        )
    ).

%----------------------------------------------------------------------------%

:- type after_layout_label
    --->    not_after_layout_label
    ;       after_layout_label.

:- pred output_instruction_list(llds_out_info::in, list(instruction)::in,
    pair(label, set_tree234(label))::in, set_tree234(label)::in,
    after_layout_label::in, io::di, io::uo) is det.

output_instruction_list(_, [], _, _, _, !IO).
output_instruction_list(Info, [Instr | Instrs], ProfInfo, WhileSet,
        AfterLayoutLabel0, !IO) :-
    Instr = llds_instr(Uinstr, Comment),
    ( Uinstr = label(Label) ->
        InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
        ( map.search(InternalLabelToLayoutMap, Label, _) ->
            AfterLayoutLabel = after_layout_label
        ;
            AfterLayoutLabel = not_after_layout_label
        ),
        (
            AfterLayoutLabel0 = after_layout_label,
            AfterLayoutLabel = after_layout_label
        ->
            % Make sure that the addresses of the two labels are distinct.
            io.write_string("\tMR_dummy_function_call();\n", !IO)
        ;
            true
        ),
        output_instruction_and_comment(Info, Uinstr, Comment, ProfInfo, !IO),
        ( set_tree234.contains(WhileSet, Label) ->
            io.write_string("\twhile (1) {\n", !IO),
            output_instruction_list_while(Info, Instrs, Label, ProfInfo,
                WhileSet, !IO)
            % The matching close brace is printed in output_instruction_list
            % when before the next label, before a goto that closes the loop,
            % or when we get to the end of Instrs.
        ;
            output_instruction_list(Info, Instrs, ProfInfo, WhileSet,
                AfterLayoutLabel, !IO)
        )
    ;
        output_instruction_and_comment(Info, Uinstr, Comment,
            ProfInfo, !IO),
        ( Uinstr = comment(_) ->
            AfterLayoutLabel = AfterLayoutLabel0
        ;
            AfterLayoutLabel = not_after_layout_label
        ),
        output_instruction_list(Info, Instrs, ProfInfo, WhileSet,
            AfterLayoutLabel, !IO)
    ).

:- pred output_instruction_list_while(llds_out_info::in,
    list(instruction)::in, label::in, pair(label,
    set_tree234(label))::in, set_tree234(label)::in, io::di, io::uo) is det.

output_instruction_list_while(_, [], _, _, _, !IO) :-
    io.write_string("\tbreak; } /* end while */\n", !IO).
output_instruction_list_while(Info, [Instr | Instrs], Label, ProfInfo,
        WhileSet, !IO) :-
    Instr = llds_instr(Uinstr, Comment),
    ( Uinstr = label(_) ->
        io.write_string("\tbreak; } /* end while */\n", !IO),
        output_instruction_list(Info, [Instr | Instrs],
            ProfInfo, WhileSet, not_after_layout_label, !IO)
    ; Uinstr = goto(code_label(Label)) ->
        io.write_string("\t/* continue */ } /* end while */\n", !IO),
        output_instruction_list(Info, Instrs, ProfInfo, WhileSet,
            not_after_layout_label, !IO)
    ; Uinstr = if_val(Rval, code_label(Label)) ->
        io.write_string("\tif (", !IO),
        output_test_rval(Info, Rval, !IO),
        io.write_string(")\n\t\tcontinue;\n", !IO),
        AutoComments = Info ^ lout_auto_comments,
        (
            AutoComments = yes,
            Comment \= ""
        ->
            io.write_string("\t\t/* ", !IO),
            io.write_string(Comment, !IO),
            io.write_string(" */\n", !IO)
        ;
            true
        ),
        output_instruction_list_while(Info, Instrs, Label, ProfInfo,
            WhileSet, !IO)
    ; Uinstr = block(TempR, TempF, BlockInstrs) ->
        output_block_start(TempR, TempF, !IO),
        output_instruction_list_while_block(Info, BlockInstrs, Label,
            ProfInfo, !IO),
        output_block_end(!IO),
        output_instruction_list_while(Info, Instrs, Label, ProfInfo,
            WhileSet, !IO)
    ;
        output_instruction_and_comment(Info, Uinstr, Comment, ProfInfo, !IO),
        output_instruction_list_while(Info, Instrs, Label, ProfInfo,
            WhileSet, !IO)
    ).

:- pred output_instruction_list_while_block(llds_out_info::in,
    list(instruction)::in, label::in, pair(label, set_tree234(label))::in,
    io::di, io::uo) is det.

output_instruction_list_while_block(_, [], _, _, !IO).
output_instruction_list_while_block(Info, [Instr | Instrs], Label, ProfInfo,
        !IO) :-
    Instr = llds_instr(Uinstr, Comment),
    ( Uinstr = label(_) ->
        unexpected(this_file, "label in block")
    ; Uinstr = goto(code_label(Label)) ->
        io.write_string("\tcontinue;\n", !IO),
        expect(unify(Instrs, []), this_file,
            "output_instruction_list_while_block: code after goto")
    ; Uinstr = if_val(Rval, code_label(Label)) ->
        io.write_string("\tif (", !IO),
        output_test_rval(Info, Rval, !IO),
        io.write_string(")\n\t\tcontinue;\n", !IO),
        AutoComments = Info ^ lout_auto_comments,
        (
            AutoComments = yes,
            Comment \= ""
        ->
            io.write_string("\t\t/* ", !IO),
            io.write_string(Comment, !IO),
            io.write_string(" */\n", !IO)
        ;
            true
        ),
        output_instruction_list_while_block(Info, Instrs, Label,
            ProfInfo, !IO)
    ; Uinstr = block(_, _, _) ->
        unexpected(this_file, "block in block")
    ;
        output_instruction_and_comment(Info, Uinstr, Comment, ProfInfo, !IO),
        output_instruction_list_while_block(Info, Instrs, Label,
            ProfInfo, !IO)
    ).

:- pred output_instruction_and_comment(llds_out_info::in, instr::in,
    string::in, pair(label, set_tree234(label))::in, io::di, io::uo) is det.

output_instruction_and_comment(Info, Instr, Comment, ProfInfo, !IO) :-
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = no,
        (
            ( Instr = comment(_)
            ; Instr = livevals(_)
            )
        ->
            true
        ;
            output_instruction(Info, Instr, ProfInfo, !IO)
        )
    ;
        AutoComments = yes,
        output_instruction(Info, Instr, ProfInfo, !IO),
        ( Comment = "" ->
            true
        ;
            io.write_string("\t\t/* ", !IO),
            io.write_string(Comment, !IO),
            io.write_string(" */\n", !IO)
        )
    ).

    % output_debug_instruction_and_comment/5 is only for debugging.
    % Normally we use output_instruction_and_comment/6.
    %
output_debug_instruction_and_comment(Info, Instr, Comment, !IO) :-
    ContLabelSet = set_tree234.init,
    DummyModule = unqualified("DEBUG"),
    DummyPredName = "DEBUG",
    proc_id_to_int(hlds_pred.initial_proc_id, InitialProcIdInt),
    ProcLabel = ordinary_proc_label(DummyModule, pf_predicate, DummyModule,
        DummyPredName, 0, InitialProcIdInt),
    ProfInfo = entry_label(entry_label_local, ProcLabel) - ContLabelSet,
    output_instruction_and_comment(Info, Instr, Comment, ProfInfo, !IO).

    % output_debug_instruction/3 is only for debugging.
    % Normally we use output_instruction/4.
    %
output_debug_instruction(Info, Instr, !IO) :-
    ContLabelSet = set_tree234.init,
    DummyModule = unqualified("DEBUG"),
    DummyPredName = "DEBUG",
    proc_id_to_int(hlds_pred.initial_proc_id, InitialProcIdInt),
    ProcLabel = ordinary_proc_label(DummyModule, pf_predicate, DummyModule,
        DummyPredName, 0, InitialProcIdInt),
    ProfInfo = entry_label(entry_label_local, ProcLabel) - ContLabelSet,
    output_instruction(Info, Instr, ProfInfo, !IO).

:- pred output_block_start(int::in, int::in, io::di, io::uo) is det.

output_block_start(TempR, TempF, !IO) :-
    io.write_string("\t{\n", !IO),
    ( TempR > 0 ->
        io.write_string("\tMR_Word ", !IO),
        output_temp_decls(TempR, "r", !IO),
        io.write_string(";\n", !IO)
    ;
        true
    ),
    ( TempF > 0 ->
        io.write_string("\tMR_Float ", !IO),
        output_temp_decls(TempF, "f", !IO),
        io.write_string(";\n", !IO)
    ;
        true
    ).

:- pred output_block_end(io::di, io::uo) is det.

output_block_end(!IO) :-
    io.write_string("\t}\n", !IO).

:- pred output_comment_chars(char::in, list(char)::in, io::di, io::uo) is det.

output_comment_chars(_PrevChar, [], !IO).
output_comment_chars(PrevChar, [Char | Chars], !IO) :-
    (
        PrevChar = ('/'),
        Char = ('*')
    ->
        io.write_string(" *", !IO)
    ;
        PrevChar = ('*'),
        Char = ('/')
    ->
        io.write_string(" /", !IO)
    ;
        io.write_char(Char, !IO)
    ),
    output_comment_chars(Char, Chars, !IO).

:- pred output_instruction(llds_out_info::in, instr::in,
    pair(label, set_tree234(label))::in, io::di, io::uo) is det.

output_instruction(Info, Instr, ProfInfo, !IO) :-
    (
        Instr = comment(Comment),
        % Ensure that any comments embedded inside Comment are made safe, i.e.
        % prevent the closing of embedded comments from closing the outer
        % comment. The fact that the code here is not very efficient doesn't
        % matter since we write out comments only with --auto-comments,
        % which we enable only when we want to debug the generated C code.
        io.write_string("/*", !IO),
        string.to_char_list(Comment, CommentChars),
        output_comment_chars('*', CommentChars, !IO),
        io.write_string("*/\n", !IO)
    ;
        Instr = livevals(LiveVals),
        io.write_string("/*\n* Live lvalues:\n", !IO),
        set.to_sorted_list(LiveVals, LiveValsList),
        output_livevals(Info, LiveValsList, !IO),
        io.write_string("*/\n", !IO)
    ;
        Instr = block(TempR, TempF, Instrs),
        output_block_start(TempR, TempF, !IO),
        output_instruction_list(Info, Instrs, ProfInfo, set_tree234.init,
            not_after_layout_label, !IO),
        output_block_end(!IO)
    ;
        Instr = assign(Lval, Rval),
        io.write_string("\t", !IO),
        output_lval_for_assign(Info, Lval, Type, !IO),
        io.write_string(" = ", !IO),
        output_rval_as_type(Info, Rval, Type, !IO),
        io.write_string(";\n", !IO)
    ;
        Instr = keep_assign(Lval, Rval),
        io.write_string("\t", !IO),
        output_lval_for_assign(Info, Lval, Type, !IO),
        io.write_string(" = ", !IO),
        output_rval_as_type(Info, Rval, Type, !IO),
        io.write_string(";\n", !IO)
    ;
        Instr = llcall(Target, ContLabel, LiveVals, _, _, _),
        ProfInfo = CallerLabel - _,
        output_call(Info, Target, ContLabel, CallerLabel, !IO),
        output_gc_livevals(Info, LiveVals, !IO)
    ;
        Instr = arbitrary_c_code(_, _, C_Code),
        io.write_string("\t", !IO),
        io.write_string(C_Code, !IO)
    ;
        Instr = mkframe(FrameInfo, MaybeFailCont),
        (
            FrameInfo = ordinary_frame(Msg, Num, MaybeStruct),
            (
                MaybeStruct = yes(foreign_proc_struct(StructName, _, _)),
                (
                    MaybeFailCont = yes(FailCont),
                    io.write_string("\tMR_mkpragmaframe(""", !IO),
                    c_util.output_quoted_string(Msg, !IO),
                    io.write_string(""", ", !IO),
                    io.write_int(Num, !IO),
                    io.write_string(",\n\t\t", !IO),
                    io.write_string(StructName, !IO),
                    io.write_string(", ", !IO),
                    output_code_addr(FailCont, !IO),
                    io.write_string(");\n", !IO)
                ;
                    MaybeFailCont = no,
                    io.write_string("\tMR_mkpragmaframe_no_redoip(""", !IO),
                    c_util.output_quoted_string(Msg, !IO),
                    io.write_string(""", ", !IO),
                    io.write_int(Num, !IO),
                    io.write_string(",\n\t\t", !IO),
                    io.write_string(StructName, !IO),
                    io.write_string(");\n", !IO)
                )
            ;
                MaybeStruct = no,
                (
                    MaybeFailCont = yes(FailCont),
                    io.write_string("\tMR_mkframe(""", !IO),
                    c_util.output_quoted_string(Msg, !IO),
                    io.write_string(""", ", !IO),
                    io.write_int(Num, !IO),
                    io.write_string(",\n\t\t", !IO),
                    output_code_addr(FailCont, !IO),
                    io.write_string(");\n", !IO)
                ;
                    MaybeFailCont = no,
                    io.write_string("\tMR_mkframe_no_redoip(""",
                        !IO),
                    c_util.output_quoted_string(Msg, !IO),
                    io.write_string(""", ", !IO),
                    io.write_int(Num, !IO),
                    io.write_string(");\n", !IO)
                )
            )
        ;
            FrameInfo = temp_frame(Kind),
            (
                Kind = det_stack_proc,
                io.write_string("\tMR_mkdettempframe(", !IO),
                (
                    MaybeFailCont = yes(FailCont),
                    output_code_addr(FailCont, !IO)
                ;
                    MaybeFailCont = no,
                    unexpected(this_file, "output_instruction: no failcont")
                ),
                io.write_string(");\n", !IO)
            ;
                Kind = nondet_stack_proc,
                io.write_string("\tMR_mktempframe(", !IO),
                (
                    MaybeFailCont = yes(FailCont),
                    output_code_addr(FailCont, !IO)
                ;
                    MaybeFailCont = no,
                    unexpected(this_file, "output_instruction: no failcont")
                ),
                io.write_string(");\n", !IO)
            )
        )
    ;
        Instr = label(Label),
        output_label_defn(Label, !IO),
        LocalThreadEngineBase = Info ^ lout_local_thread_engine_base,
        (
            LocalThreadEngineBase = yes,
            io.write_string("\tMR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE\n", !IO)
        ;
            LocalThreadEngineBase = no
        ),
        maybe_output_update_prof_counter(Info, Label, ProfInfo, !IO)
    ;
        Instr = goto(CodeAddr),
        ProfInfo = CallerLabel - _,
        io.write_string("\t", !IO),
        output_goto(Info, CodeAddr, CallerLabel, !IO)
    ;
        Instr = computed_goto(Rval, MaybeLabels),
        io.write_string("\tMR_COMPUTED_GOTO(", !IO),
        output_rval_as_type(Info, Rval, lt_unsigned, !IO),
        io.write_string(",\n\t\t", !IO),
        output_label_list_or_not_reached(MaybeLabels, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = if_val(Rval, Target),
        ProfInfo = CallerLabel - _,
        io.write_string("\tif (", !IO),
        output_test_rval(Info, Rval, !IO),
        io.write_string(") {\n\t\t", !IO),
        output_goto(Info, Target, CallerLabel, !IO),
        io.write_string("\t}\n", !IO)
    ;
        Instr = save_maxfr(Lval),
        io.write_string("\tMR_save_maxfr(", !IO),
        output_lval(Info, Lval, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = restore_maxfr(Lval),
        io.write_string("\tMR_restore_maxfr(", !IO),
        output_lval(Info, Lval, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = incr_hp(Lval, MaybeTag, MaybeOffset, SizeRval, TypeMsg,
            MayUseAtomicAlloc, MaybeRegionRval, MaybeReuse),
        io.write_string("\t", !IO),
        (
            MaybeReuse = no_llds_reuse,
            output_incr_hp_no_reuse(Info, Lval, MaybeTag, MaybeOffset,
                SizeRval, TypeMsg, MayUseAtomicAlloc, MaybeRegionRval,
                ProfInfo, !IO)
        ;
            MaybeReuse = llds_reuse(ReuseRval, MaybeFlagLval),
            (
                MaybeTag = no,
                (
                    MaybeFlagLval = yes(FlagLval),
                    io.write_string("MR_reuse_or_alloc_heap_flag(", !IO),
                    output_lval_as_word(Info, Lval, !IO),
                    io.write_string(", ", !IO),
                    output_lval_as_word(Info, FlagLval, !IO)
                ;
                    MaybeFlagLval = no,
                    io.write_string("MR_reuse_or_alloc_heap(", !IO),
                    output_lval_as_word(Info, Lval, !IO)
                )
            ;
                MaybeTag = yes(Tag),
                (
                    MaybeFlagLval = yes(FlagLval),
                    io.write_string("MR_tag_reuse_or_alloc_heap_flag(", !IO),
                    output_lval_as_word(Info, Lval, !IO),
                    io.write_string(", ", !IO),
                    output_tag(Tag, !IO),
                    io.write_string(", ", !IO),
                    output_lval_as_word(Info, FlagLval, !IO)
                ;
                    MaybeFlagLval = no,
                    io.write_string("MR_tag_reuse_or_alloc_heap(", !IO),
                    output_lval_as_word(Info, Lval, !IO),
                    io.write_string(", ", !IO),
                    output_tag(Tag, !IO)
                )
            ),
            io.write_string(", ", !IO),
            output_rval(Info, ReuseRval, !IO),
            io.write_string(", ", !IO),
            output_incr_hp_no_reuse(Info, Lval, MaybeTag, MaybeOffset,
                SizeRval, TypeMsg, MayUseAtomicAlloc, MaybeRegionRval,
                ProfInfo, !IO),
            io.write_string(")", !IO)
        ),
        io.write_string(";\n", !IO)
    ;
        Instr = mark_hp(Lval),
        io.write_string("\tMR_mark_hp(", !IO),
        output_lval_as_word(Info, Lval, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = restore_hp(Rval),
        io.write_string("\tMR_restore_hp(", !IO),
        output_rval_as_type(Info, Rval, lt_word, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = free_heap(Rval),
        io.write_string("\tMR_free_heap(", !IO),
        output_rval_as_type(Info, Rval, lt_data_ptr, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = push_region_frame(StackId, EmbeddedFrame),
        (
            StackId = region_stack_ite,
            io.write_string("\tMR_push_region_ite_frame", !IO)
        ;
            StackId = region_stack_disj,
            io.write_string("\tMR_push_region_disj_frame", !IO)
        ;
            StackId = region_stack_commit,
            io.write_string("\tMR_push_region_commit_frame", !IO)
        ),
        io.write_string("(", !IO),
        output_embedded_frame_addr(Info, EmbeddedFrame, !IO),
        io.write_string(");", !IO),

        % The comment is to make the code easier to debug;
        % we can stop printing it out once that has been done.
        EmbeddedFrame = embedded_stack_frame_id(_StackId,
            FirstSlot, LastSlot),
        Comment = " /* " ++ int_to_string(FirstSlot) ++ ".." ++
            int_to_string(LastSlot) ++ " */",
        io.write_string(Comment, !IO),

        io.write_string("\n", !IO)
    ;
        Instr = region_fill_frame(FillOp, EmbeddedFrame, IdRval,
            NumLval, AddrLval),
        (
            FillOp = region_fill_ite_protect,
            io.write_string("\tMR_region_fill_ite_protect", !IO)
        ;
            FillOp = region_fill_ite_snapshot(removed_at_start_of_else),
            io.write_string("\tMR_region_fill_ite_snapshot_removed", !IO)
        ;
            FillOp = region_fill_ite_snapshot(not_removed_at_start_of_else),
            io.write_string("\tMR_region_fill_ite_snapshot_not_removed", !IO)
        ;
            FillOp = region_fill_semi_disj_protect,
            io.write_string("\tMR_region_fill_semi_disj_protect", !IO)
        ;
            FillOp = region_fill_disj_snapshot,
            io.write_string("\tMR_region_fill_disj_snapshot", !IO)
        ;
            FillOp = region_fill_commit,
            io.write_string("\tMR_region_fill_commit", !IO)
        ),
        io.write_string("(", !IO),
        output_embedded_frame_addr(Info, EmbeddedFrame, !IO),
        io.write_string(", ", !IO),
        output_rval(Info, IdRval, !IO),
        io.write_string(", ", !IO),
        output_lval(Info, NumLval, !IO),
        io.write_string(", ", !IO),
        output_lval(Info, AddrLval, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = region_set_fixed_slot(SetOp, EmbeddedFrame, ValueRval),
        (
            SetOp = region_set_ite_num_protects,
            io.write_string("\tMR_region_set_ite_num_protects", !IO)
        ;
            SetOp = region_set_ite_num_snapshots,
            io.write_string("\tMR_region_set_ite_num_snapshots", !IO)
        ;
            SetOp = region_set_disj_num_protects,
            io.write_string("\tMR_region_set_disj_num_protects", !IO)
        ;
            SetOp = region_set_disj_num_snapshots,
            io.write_string("\tMR_region_set_disj_num_snapshots", !IO)
        ;
            SetOp = region_set_commit_num_entries,
            io.write_string("\tMR_region_set_commit_num_entries", !IO)
        ),
        io.write_string("(", !IO),
        output_embedded_frame_addr(Info, EmbeddedFrame, !IO),
        io.write_string(", ", !IO),
        output_rval(Info, ValueRval, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = use_and_maybe_pop_region_frame(UseOp, EmbeddedFrame),
        (
            UseOp = region_ite_then(region_ite_semidet_cond),
            io.write_string("\tMR_use_region_ite_then_semidet", !IO)
        ;
            UseOp = region_ite_then(region_ite_nondet_cond),
            io.write_string("\tMR_use_region_ite_then_nondet", !IO)
        ;
            UseOp = region_ite_else(region_ite_semidet_cond),
            io.write_string("\tMR_use_region_ite_else_semidet", !IO)
        ;
            UseOp = region_ite_else(region_ite_nondet_cond),
            io.write_string("\tMR_use_region_ite_else_nondet", !IO)
        ;
            UseOp = region_ite_nondet_cond_fail,
            io.write_string("\tMR_use_region_ite_nondet_cond_fail", !IO)
        ;
            UseOp = region_disj_later,
            io.write_string("\tMR_use_region_disj_later", !IO)
        ;
            UseOp = region_disj_last,
            io.write_string("\tMR_use_region_disj_last", !IO)
        ;
            UseOp = region_disj_nonlast_semi_commit,
            io.write_string("\tMR_use_region_disj_nonlast_semi_commit", !IO)
        ;
            UseOp = region_commit_success,
            io.write_string("\tMR_use_region_commit_success", !IO)
        ;
            UseOp = region_commit_failure,
            io.write_string("\tMR_use_region_commit_failure", !IO)
        ),
        io.write_string("(", !IO),
        output_embedded_frame_addr(Info, EmbeddedFrame, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = store_ticket(Lval),
        io.write_string("\tMR_store_ticket(", !IO),
        output_lval_as_word(Info, Lval, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = reset_ticket(Rval, Reason),
        io.write_string("\tMR_reset_ticket(", !IO),
        output_rval_as_type(Info, Rval, lt_word, !IO),
        io.write_string(", ", !IO),
        output_reset_trail_reason(Reason, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = discard_ticket,
        io.write_string("\tMR_discard_ticket();\n", !IO)
    ;
        Instr = prune_ticket,
        io.write_string("\tMR_prune_ticket();\n", !IO)
    ;
        Instr = mark_ticket_stack(Lval),
        io.write_string("\tMR_mark_ticket_stack(", !IO),
        output_lval_as_word(Info, Lval, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = prune_tickets_to(Rval),
        io.write_string("\tMR_prune_tickets_to(", !IO),
        output_rval_as_type(Info, Rval, lt_word, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = incr_sp(N, _Msg, Kind),
        (
            Kind = stack_incr_leaf,
            ( N < max_leaf_stack_frame_size ->
                io.write_string("\tMR_incr_sp_leaf(", !IO)
            ;
                io.write_string("\tMR_incr_sp(", !IO)
            )
        ;
            Kind = stack_incr_nonleaf,
            io.write_string("\tMR_incr_sp(", !IO)
        ),
        io.write_int(N, !IO),
        io.write_string(");\n", !IO)
        % Use the code below instead of the code above if you want to run
        % tools/framesize on the output of the compiler.
        % io.write_string("\tMR_incr_sp_push_msg(", !IO),
        % io.write_int(N, !IO),
        % io.write_string(", """, !IO),
        % c_util.output_quoted_string(Msg, !IO),
        % io.write_string(""");\n", !IO)
    ;
        Instr = decr_sp(N),
        io.write_string("\tMR_decr_sp(", !IO),
        io.write_int(N, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = decr_sp_and_return(N),
        io.write_string("\tMR_decr_sp_and_return(", !IO),
        io.write_int(N, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = foreign_proc_code(Decls, Components, _, _, _, _, _,
            MaybeDefLabel, _, _),
        io.write_string("\t{\n", !IO),
        output_foreign_proc_decls(Decls, !IO),
        (
            MaybeDefLabel = no,
            list.foldl(output_foreign_proc_component(Info), Components, !IO)
        ;
            MaybeDefLabel = yes(DefLabel),
            InternalLabelToLayoutMap = Info ^ lout_internal_label_to_layout,
            map.lookup(InternalLabelToLayoutMap, DefLabel, DefLabelLayout),
            io.write_string("#define MR_HASH_DEF_LABEL_LAYOUT ", !IO),
            MangledModuleName = Info ^ lout_mangled_module_name,
            output_layout_slot_addr(use_layout_macro, MangledModuleName,
                DefLabelLayout, !IO),
            io.nl(!IO),
            list.foldl(output_foreign_proc_component(Info), Components, !IO),
            io.write_string("#undef MR_HASH_DEF_LABEL_LAYOUT\n", !IO)
        ),
        io.write_string("\t}\n", !IO)
    ;
        Instr = init_sync_term(Lval, N),
        io.write_string("\tMR_init_sync_term(", !IO),
        output_lval_as_word(Info, Lval, !IO),
        io.write_string(", ", !IO),
        io.write_int(N, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = fork_new_child(Lval, Child),
        io.write_string("\tMR_fork_new_child(", !IO),
        output_lval_as_word(Info, Lval, !IO),
        io.write_string(", ", !IO),
        output_label_as_code_addr(Child, !IO),
        io.write_string(");\n", !IO)
    ;
        Instr = join_and_continue(Lval, Label),
        io.write_string("\tMR_join_and_continue(", !IO),
        output_lval(Info, Lval, !IO),
        io.write_string(", ", !IO),
        output_label_as_code_addr(Label, !IO),
        io.write_string(");\n", !IO)
    ).

:- pred output_incr_hp_no_reuse(llds_out_info::in, lval::in, maybe(tag)::in,
    maybe(int)::in, rval::in, string::in, may_use_atomic_alloc::in,
    maybe(rval)::in, pair(label, set_tree234(label))::in, io::di, io::uo)
    is det.

output_incr_hp_no_reuse(Info, Lval, MaybeTag, MaybeOffset, Rval, TypeMsg,
        MayUseAtomicAlloc, MaybeRegionRval, ProfInfo, !IO) :-
    (
        MaybeRegionRval = yes(RegionRval),
        (
            MaybeTag = no,
            io.write_string("MR_alloc_in_region(", !IO),
            output_lval_as_word(Info, Lval, !IO)
        ;
            MaybeTag = yes(Tag),
            io.write_string("MR_tag_alloc_in_region(", !IO),
            output_lval_as_word(Info, Lval, !IO),
            io.write_string(", ", !IO),
            output_tag(Tag, !IO)
        ),
        io.write_string(", ", !IO),
        output_rval(Info, RegionRval, !IO),
        io.write_string(", ", !IO),
        output_rval_as_type(Info, Rval, lt_word, !IO),
        io.write_string(")", !IO)
    ;
        MaybeRegionRval = no,
        ProfMem = Info ^ lout_profile_memory,
        (
            ProfMem = yes,
            (
                MaybeTag = no,
                (
                    MayUseAtomicAlloc = may_not_use_atomic_alloc,
                    io.write_string("MR_offset_incr_hp_msg(", !IO)
                ;
                    MayUseAtomicAlloc = may_use_atomic_alloc,
                    io.write_string("MR_offset_incr_hp_atomic_msg(", !IO)
                ),
                output_lval_as_word(Info, Lval, !IO)
            ;
                MaybeTag = yes(Tag),
                (
                    MayUseAtomicAlloc = may_not_use_atomic_alloc,
                    io.write_string("MR_tag_offset_incr_hp_msg(", !IO)
                ;
                    MayUseAtomicAlloc = may_use_atomic_alloc,
                    io.write_string(
                        "MR_tag_offset_incr_hp_atomic_msg(", !IO)
                ),
                output_lval_as_word(Info, Lval, !IO),
                io.write_string(", ", !IO),
                output_tag(Tag, !IO)
            ),
            io.write_string(", ", !IO),
            (
                MaybeOffset = no,
                io.write_string("0, ", !IO)
            ;
                MaybeOffset = yes(Offset),
                io.write_int(Offset, !IO),
                io.write_string(", ", !IO)
            ),
            output_rval_as_type(Info, Rval, lt_word, !IO),
            io.write_string(", ", !IO),
            ProfInfo = CallerLabel - _,
            output_label(CallerLabel, !IO),
            io.write_string(", """, !IO),
            c_util.output_quoted_string(TypeMsg, !IO),
            io.write_string(""")", !IO)
        ;
            ProfMem = no,
            (
                MaybeTag = no,
                (
                    MaybeOffset = yes(_),
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string("MR_offset_incr_hp(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string("MR_offset_incr_hp_atomic(", !IO)
                    )
                ;
                    MaybeOffset = no,
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string("MR_alloc_heap(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string("MR_alloc_heap_atomic(", !IO)
                    )
                ),
                output_lval_as_word(Info, Lval, !IO)
            ;
                MaybeTag = yes(Tag),
                (
                    MaybeOffset = yes(_),
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string("MR_tag_offset_incr_hp(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string(
                            "MR_tag_offset_incr_hp_atomic(", !IO)
                    ),
                    output_lval_as_word(Info, Lval, !IO),
                    io.write_string(", ", !IO),
                    output_tag(Tag, !IO)
                ;
                    MaybeOffset = no,
                    (
                        MayUseAtomicAlloc = may_not_use_atomic_alloc,
                        io.write_string("MR_tag_alloc_heap(", !IO)
                    ;
                        MayUseAtomicAlloc = may_use_atomic_alloc,
                        io.write_string("MR_tag_alloc_heap_atomic(", !IO)
                    ),
                    output_lval_as_word(Info, Lval, !IO),
                    io.write_string(", ", !IO),
                    io.write_int(Tag, !IO)
                )
            ),
            io.write_string(", ", !IO),
            (
                MaybeOffset = yes(Offset),
                io.write_int(Offset, !IO),
                io.write_string(", ", !IO)
            ;
                MaybeOffset = no
            ),
            output_rval_as_type(Info, Rval, lt_word, !IO),
            io.write_string(")", !IO)
        )
    ).

    % Our stacks grow upwards in that new stack frames have higher addresses
    % than old stack frames, but within in each stack frame, we compute the
    % address of stackvar N or framevar N by *subtracting* N from the address
    % of the top of (the non-fixed part of) the stack frame, so that e.g.
    % framevar N+1 is actually stored at a *lower* address than framevar N.
    %
    % The C code we interact with refers to embedded stack frames by the
    % starting (i.e. lowest) address.
    %
:- pred output_embedded_frame_addr(llds_out_info::in,
    embedded_stack_frame_id::in, io::di, io::uo) is det.

output_embedded_frame_addr(Info, EmbeddedFrame, !IO) :-
    EmbeddedFrame = embedded_stack_frame_id(MainStackId,
        _FirstSlot, LastSlot),
    FrameStartRval = stack_slot_num_to_lval_ref(MainStackId, LastSlot),
    output_rval_as_type(Info, FrameStartRval, lt_data_ptr, !IO).

:- func max_leaf_stack_frame_size = int.

% This should be kept in sync with the value of MR_stack_margin_size
% in runtime/mercury_wrapper.c. See the documentation there.
max_leaf_stack_frame_size = 128.

:- pred output_foreign_proc_component(llds_out_info::in,
    foreign_proc_component::in, io::di, io::uo) is det.

output_foreign_proc_component(Info, Component, !IO) :-
    (
        Component = foreign_proc_inputs(Inputs),
        output_foreign_proc_inputs(Info, Inputs, !IO)
    ;
        Component = foreign_proc_outputs(Outputs),
        output_foreign_proc_outputs(Info, Outputs, !IO)
    ;
        Component = foreign_proc_user_code(MaybeContext, _, C_Code),
        ( C_Code = "" ->
            true
        ;
                % We should start the C_Code on a new line,
                % just in case it starts with a proprocessor directive.
            (
                MaybeContext = yes(Context),
                io.write_string("{\n", !IO),
                output_set_line_num(Info, Context, !IO),
                io.write_string(C_Code, !IO),
                io.write_string(";}\n", !IO),
                output_reset_line_num(Info, !IO)
            ;
                MaybeContext = no,
                io.write_string("{\n", !IO),
                io.write_string(C_Code, !IO),
                io.write_string(";}\n", !IO)
            )
        )
    ;
        Component = foreign_proc_raw_code(_, _, _, C_Code),
        io.write_string(C_Code, !IO)
    ;
        Component = foreign_proc_fail_to(Label),
        io.write_string(
            "if (!" ++ foreign_proc_succ_ind_name ++ ") MR_GOTO_LAB(", !IO),
        output_label(Label, no, !IO),
        io.write_string(");\n", !IO)
    ;
        Component = foreign_proc_noop
    ).

    % Output the local variable declarations at the top of the
    % foreign_proc code for C.
    %
:- pred output_foreign_proc_decls(list(foreign_proc_decl)::in, io::di, io::uo)
    is det.

output_foreign_proc_decls([], !IO).
output_foreign_proc_decls([Decl | Decls], !IO) :-
    (
        % Apart from special cases, the local variables are MR_Words
        Decl = foreign_proc_arg_decl(_Type, TypeString, VarName),
        io.write_string("\t", !IO),
        io.write_string(TypeString, !IO),
        io.write_string("\t", !IO),
        io.write_string(VarName, !IO),
        io.write_string(";\n", !IO)
    ;
        Decl = foreign_proc_struct_ptr_decl(StructTag, VarName),
        io.write_string("\tstruct ", !IO),
        io.write_string(StructTag, !IO),
        io.write_string("\t*", !IO),
        io.write_string(VarName, !IO),
        io.write_string(";\n", !IO)
    ),
    output_foreign_proc_decls(Decls, !IO).

    % Output declarations for any rvals used to initialize the inputs.
    %
:- pred output_record_foreign_proc_input_rval_decls(llds_out_info::in,
    list(foreign_proc_input)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_foreign_proc_input_rval_decls(_, [], !DeclSet, !IO).
output_record_foreign_proc_input_rval_decls(Info, [Input | Inputs],
        !DeclSet, !IO) :-
    Input = foreign_proc_input(_VarName, _VarType, _IsDummy, _OrigType, Rval,
        _, _),
    output_record_rval_decls_format(Info, Rval, "", "\t",
        0, _N, !DeclSet, !IO),
    output_record_foreign_proc_input_rval_decls(Info, Inputs, !DeclSet, !IO).

    % Output the input variable assignments at the top of the foreign code
    % for C.
    %
:- pred output_foreign_proc_inputs(llds_out_info::in,
    list(foreign_proc_input)::in, io::di, io::uo) is det.

output_foreign_proc_inputs(_, [], !IO).
output_foreign_proc_inputs(Info, [Input | Inputs], !IO) :-
    Input = foreign_proc_input(VarName, VarType, IsDummy, _OrigType, _Rval,
        _MaybeForeignTypeInfo, _BoxPolicy),
    (
        IsDummy = is_dummy_type,
        (
            % Avoid outputting an assignment for builtin dummy types.
            % For other dummy types we must output an assignment because
            % code in the foreign_proc body may examine the value.
            type_to_ctor_and_args(VarType, VarTypeCtor, []),
            check_builtin_dummy_type_ctor(VarTypeCtor) =
                is_builtin_dummy_type_ctor
        ->
            true
        ;
            io.write_string("\t" ++ VarName ++ " = 0;\n", !IO)
        )
    ;
        IsDummy = is_not_dummy_type,
        output_foreign_proc_input(Info, Input, !IO)
    ),
    output_foreign_proc_inputs(Info, Inputs, !IO).

    % Output an input variable assignment at the top of the foreign code
    % for C.
    %
:- pred output_foreign_proc_input(llds_out_info::in, foreign_proc_input::in,
    io::di, io::uo) is det.

output_foreign_proc_input(Info, Input, !IO) :-
    Input = foreign_proc_input(VarName, _VarType, _IsDummy, OrigType, Rval,
        MaybeForeignTypeInfo, BoxPolicy),
    io.write_string("\t", !IO),
    (
        BoxPolicy = always_boxed,
        io.write_string(VarName, !IO),
        io.write_string(" = ", !IO),
        output_rval_as_type(Info, Rval, lt_word, !IO)
    ;
        BoxPolicy = native_if_possible,
        (
            MaybeForeignTypeInfo = yes(ForeignTypeInfo),
            ForeignTypeInfo = foreign_proc_type(ForeignType, Assertions),
            % For foreign types for which c_type_is_word_sized_int_or_ptr
            % succeeds, the code in the else branch is not only correct,
            % it also generates faster code than would be generated by
            % the then branch, because MR_MAYBE_UNBOX_FOREIGN_TYPE
            % invokes memcpy when given a word-sized type.
            (
                (
                    c_type_is_word_sized_int_or_ptr(ForeignType)
                ;
                    list.member(foreign_type_can_pass_as_mercury_type,
                        Assertions)
                )
            ->
                % Note that for this cast to be correct the foreign
                % type must be a word sized integer or pointer type.
                io.write_string(VarName, !IO),
                io.write_string(" = ", !IO),
                io.write_string("(" ++ ForeignType ++ ") ", !IO),
                output_rval_as_type(Info, Rval, lt_word, !IO)
            ;
                io.write_string("MR_MAYBE_UNBOX_FOREIGN_TYPE(", !IO),
                io.write_string(ForeignType, !IO),
                io.write_string(", ", !IO),
                output_rval_as_type(Info, Rval, lt_word, !IO),
                io.write_string(", ", !IO),
                io.write_string(VarName, !IO),
                io.write_string(")", !IO)
            )
        ;
            MaybeForeignTypeInfo = no,
            io.write_string(VarName, !IO),
            io.write_string(" = ", !IO),
            ( OrigType = builtin_type(builtin_type_string) ->
                output_llds_type_cast(lt_string, !IO),
                output_rval_as_type(Info, Rval, lt_word, !IO)
            ; OrigType = builtin_type(builtin_type_float) ->
                output_rval_as_type(Info, Rval, lt_float, !IO)
            ;
                output_rval_as_type(Info, Rval, lt_word, !IO)
            )
        )
    ),
    io.write_string(";\n", !IO).

    % Output declarations for any lvals used for the outputs.
    %
:- pred output_record_foreign_proc_output_lval_decls(llds_out_info::in,
    list(foreign_proc_output)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_foreign_proc_output_lval_decls(_, [], !DeclSet, !IO).
output_record_foreign_proc_output_lval_decls(Info, [Output | Outputs],
        !DeclSet, !IO) :-
    Output = foreign_proc_output(Lval, _VarType, _IsDummy, _OrigType,
        _VarName, _, _),
    output_record_lval_decls_format(Info, Lval, "\t", "\t",
        0, _N, !DeclSet, !IO),
    output_record_foreign_proc_output_lval_decls(Info, Outputs,
        !DeclSet, !IO).

    % Output the output variable assignments at the bottom of the foreign code
    % for C.
    %
:- pred output_foreign_proc_outputs(llds_out_info::in,
    list(foreign_proc_output)::in, io::di, io::uo) is det.

output_foreign_proc_outputs(_, [], !IO).
output_foreign_proc_outputs(Info, [Output | Outputs], !IO) :-
    Output = foreign_proc_output(_Lval, _VarType, IsDummy, _OrigType,
        _VarName, _MaybeForeignType, _BoxPolicy),
    (
        IsDummy = is_dummy_type
    ;
        IsDummy = is_not_dummy_type,
        output_foreign_proc_output(Info, Output, !IO)
    ),
    output_foreign_proc_outputs(Info, Outputs, !IO).

    % Output a output variable assignment at the bottom of the foreign code
    % for C.
    %
:- pred output_foreign_proc_output(llds_out_info::in, foreign_proc_output::in,
    io::di, io::uo) is det.

output_foreign_proc_output(Info, Output, !IO) :-
    Output = foreign_proc_output(Lval, _VarType, _IsDummy, OrigType, VarName,
        MaybeForeignType, BoxPolicy),
    io.write_string("\t", !IO),
    (
        BoxPolicy = always_boxed,
        output_lval_as_word(Info, Lval, !IO),
        io.write_string(" = ", !IO),
        io.write_string(VarName, !IO)
    ;
        BoxPolicy = native_if_possible,
        (
            MaybeForeignType = yes(ForeignTypeInfo),
            ForeignTypeInfo = foreign_proc_type(ForeignType, Assertions),
            (
                list.member(foreign_type_can_pass_as_mercury_type, Assertions)
            ->
                output_lval_as_word(Info, Lval, !IO),
                io.write_string(" = ", !IO),
                output_llds_type_cast(lt_word, !IO),
                io.write_string(VarName, !IO)
            ;
                io.write_string("MR_MAYBE_BOX_FOREIGN_TYPE(", !IO),
                io.write_string(ForeignType, !IO),
                io.write_string(", ", !IO),
                io.write_string(VarName, !IO),
                io.write_string(", ", !IO),
                output_lval_as_word(Info, Lval, !IO),
                io.write_string(")", !IO)
            )
        ;
            MaybeForeignType = no,
            output_lval_as_word(Info, Lval, !IO),
            io.write_string(" = ", !IO),
            (
                OrigType = builtin_type(builtin_type_string)
            ->
                output_llds_type_cast(lt_word, !IO),
                io.write_string(VarName, !IO)
            ;
                OrigType = builtin_type(builtin_type_float)
            ->
                io.write_string("MR_float_to_word(", !IO),
                io.write_string(VarName, !IO),
                io.write_string(")", !IO)
            ;
                io.write_string(VarName, !IO)
            )
        )
    ),
    io.write_string(";\n", !IO).

:- pred output_reset_trail_reason(reset_trail_reason::in, io::di, io::uo)
    is det.

output_reset_trail_reason(reset_reason_undo, !IO) :-
    io.write_string("MR_undo", !IO).
output_reset_trail_reason(reset_reason_commit, !IO) :-
    io.write_string("MR_commit", !IO).
output_reset_trail_reason(reset_reason_solve, !IO) :-
    io.write_string("MR_solve", !IO).
output_reset_trail_reason(reset_reason_exception, !IO) :-
    io.write_string("MR_exception", !IO).
output_reset_trail_reason(reset_reason_retry, !IO) :-
    io.write_string("MR_retry", !IO).
output_reset_trail_reason(reset_reason_gc, !IO) :-
    io.write_string("MR_gc", !IO).

:- pred output_livevals(llds_out_info::in, list(lval)::in,
    io::di, io::uo) is det.

output_livevals(_, [], !IO).
output_livevals(Info, [Lval | Lvals], !IO) :-
    io.write_string("*\t", !IO),
    output_lval(Info, Lval, !IO),
    io.write_string("\n", !IO),
    output_livevals(Info, Lvals, !IO).

:- pred output_gc_livevals(llds_out_info::in, list(liveinfo)::in,
    io::di, io::uo) is det.

output_gc_livevals(Info, LiveVals, !IO) :-
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = yes,
        io.write_string("/*\n", !IO),
        io.write_string("* Garbage collection livevals info\n", !IO),
        output_gc_livevals_2(Info, LiveVals, !IO),
        io.write_string("*/\n", !IO)
    ;
        AutoComments = no
    ).

:- pred output_gc_livevals_2(llds_out_info::in, list(liveinfo)::in,
    io::di, io::uo) is det.

output_gc_livevals_2(_, [], !IO).
output_gc_livevals_2(Info, [LiveInfo | LiveInfos], !IO) :-
    LiveInfo = live_lvalue(Locn, LiveValueType, TypeParams),
    io.write_string("*\t", !IO),
    output_layout_locn(Info, Locn, !IO),
    io.write_string("\t", !IO),
    output_live_value_type(LiveValueType, !IO),
    io.write_string("\t", !IO),
    map.to_assoc_list(TypeParams, TypeParamList),
    output_gc_livevals_params(Info, TypeParamList, !IO),
    io.write_string("\n", !IO),
    output_gc_livevals_2(Info, LiveInfos, !IO).

:- pred output_gc_livevals_params(llds_out_info::in,
    assoc_list(tvar, set(layout_locn))::in, io::di, io::uo) is det.

output_gc_livevals_params(_, [], !IO).
output_gc_livevals_params(Info, [Var - LocnSet | VarLocnSets], !IO) :-
    term.var_to_int(Var, VarInt),
    io.write_int(VarInt, !IO),
    io.write_string(" - ", !IO),
    set.to_sorted_list(LocnSet, Locns),
    output_layout_locns(Info, Locns, !IO),
    io.write_string("  ", !IO),
    output_gc_livevals_params(Info, VarLocnSets, !IO).

:- pred output_layout_locns(llds_out_info::in, list(layout_locn)::in,
    io::di, io::uo) is det.

output_layout_locns(_,[], !IO).
output_layout_locns(Info, [Locn | Locns], !IO) :-
    output_layout_locn(Info, Locn, !IO),
    (
        Locns = []
    ;
        Locns = [_ | _],
        io.write_string(" and ", !IO),
        output_layout_locns(Info, Locns, !IO)
    ).

:- pred output_layout_locn(llds_out_info::in, layout_locn::in,
    io::di, io::uo) is det.

output_layout_locn(Info, Locn, !IO) :-
    (
        Locn = locn_direct(Lval),
        output_lval(Info, Lval, !IO)
    ;
        Locn = locn_indirect(Lval, Offset),
        io.write_string("offset ", !IO),
        io.write_int(Offset, !IO),
        io.write_string(" from ", !IO),
        output_lval(Info, Lval, !IO)
    ).

:- pred output_live_value_type(live_value_type::in, io::di, io::uo) is det.

output_live_value_type(live_value_succip, !IO) :-
    io.write_string("type succip", !IO).
output_live_value_type(live_value_curfr, !IO) :-
    io.write_string("type curfr", !IO).
output_live_value_type(live_value_maxfr, !IO) :-
    io.write_string("type maxfr", !IO).
output_live_value_type(live_value_redofr, !IO) :-
    io.write_string("type redofr", !IO).
output_live_value_type(live_value_redoip, !IO) :-
    io.write_string("type redoip", !IO).
output_live_value_type(live_value_hp, !IO) :-
    io.write_string("type hp", !IO).
output_live_value_type(live_value_trail_ptr, !IO) :-
    io.write_string("type trail_ptr", !IO).
output_live_value_type(live_value_ticket, !IO) :-
    io.write_string("type ticket", !IO).
output_live_value_type(live_value_region_disj, !IO) :-
    io.write_string("type region disj", !IO).
output_live_value_type(live_value_region_commit, !IO) :-
    io.write_string("type region commit", !IO).
output_live_value_type(live_value_region_ite, !IO) :-
    io.write_string("type region ite", !IO).
output_live_value_type(live_value_unwanted, !IO) :-
    io.write_string("unwanted", !IO).
output_live_value_type(live_value_var(Var, Name, Type, LldsInst), !IO) :-
    io.write_string("var(", !IO),
    term.var_to_int(Var, VarInt),
    io.write_int(VarInt, !IO),
    io.write_string(", ", !IO),
    io.write_string(Name, !IO),
    io.write_string(", ", !IO),
    % XXX Fake type varset
    varset.init(NewTVarset),
    mercury_output_type(NewTVarset, no, Type, !IO),
    io.write_string(", ", !IO),
    (
        LldsInst = llds_inst_ground,
        io.write_string("ground", !IO)
    ;
        LldsInst = llds_inst_partial(Inst),
        % XXX Fake inst varset
        varset.init(NewIVarset),
        mercury_output_inst(Inst, NewIVarset, !IO)
    ),
    io.write_string(")", !IO).

:- pred output_temp_decls(int::in, string::in, io::di, io::uo) is det.

output_temp_decls(N, Type, !IO) :-
    output_temp_decls_2(1, N, Type, !IO).

:- pred output_temp_decls_2(int::in, int::in, string::in, io::di, io::uo)
    is det.

output_temp_decls_2(Next, Max, Type, !IO) :-
    ( Next =< Max ->
        ( Next > 1 ->
            io.write_string(", ", !IO)
        ;
            true
        ),
        io.write_string("MR_temp", !IO),
        io.write_string(Type, !IO),
        io.write_int(Next, !IO),
        output_temp_decls_2(Next + 1, Max, Type, !IO)
    ;
        true
    ).

output_record_rval_decls(Info, Rval, !DeclSet, !IO) :-
    output_record_rval_decls_format(Info, Rval, "", "", 0, _, !DeclSet, !IO).

    % output_record_rval_decls_format(Info, Rval, FirstIndent, LaterIndent,
    %   !N, !DeclSet, !IO)
    %
    % Outputs the declarations of any static constants, etc. that need to be
    % declared before output_rval(Rval) is called. FirstIndent is output
    % before the first declaration, while LaterIndent is output before
    % all later declaration; N0 and N give the number of declarations output
    % before and after this call.
    %
    % Every time we emit a declaration for a symbol, we insert it into the
    % set of symbols we've already declared. That way, we avoid generating
    % the same symbol twice, which would cause an error in the C code.
    %
:- pred output_record_rval_decls_format(llds_out_info::in, rval::in,
    string::in, string::in, int::in, int::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_rval_decls_format(Info, Rval, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        Rval = lval(Lval),
        output_record_lval_decls_format(Info, Lval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = var(_),
        unexpected(this_file, "output_record_rval_decls_format: var")
    ;
        Rval = mkword(_, SubRval),
        output_record_rval_decls_format(Info, SubRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = const(Const),
        ( Const = llconst_code_addr(CodeAddress) ->
            output_record_code_addr_decls_format(Info, CodeAddress,
                FirstIndent, LaterIndent, !N, !DeclSet, !IO)
        ; Const = llconst_data_addr(DataId, _) ->
            output_record_data_id_decls_format(Info, DataId,
                FirstIndent, LaterIndent, !N, !DeclSet, !IO)
        ; Const = llconst_float(FloatVal) ->
            % If floats are boxed, but are allocated statically, then for each
            % float constant which we might want to box we declare a static
            % const variable holding that constant.

            UnboxedFloat = Info ^ lout_unboxed_float,
            StaticGroundFloats = Info ^ lout_static_ground_floats,
            (
                UnboxedFloat = no,
                StaticGroundFloats = yes
            ->
                float_literal_name(FloatVal, FloatName),
                FloatLabel = decl_float_label(FloatName),
                ( decl_set_is_member(FloatLabel, !.DeclSet) ->
                    true
                ;
                    decl_set_insert(FloatLabel, !DeclSet),
                    FloatString = c_util.make_float_literal( FloatVal),
                    output_indent(FirstIndent, LaterIndent, !.N, !IO),
                    !:N = !.N + 1,
                    io.write_strings(["static const MR_Float ",
                        "mercury_float_const_", FloatName, " = ", FloatString,
                        ";\n"
                    ], !IO)
                )
            ;
                true
            )
        ;
            true
        )
    ;
        Rval = unop(_, SubRvalA),
        output_record_rval_decls_format(Info, SubRvalA,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        Rval = binop(Op, SubRvalA, SubRvalB),
        output_record_rval_decls_format(Info, SubRvalA,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, SubRvalB,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),

        % If floats are boxed, and the static ground terms option is enabled,
        % then for each float constant which we might want to box we declare
        % a static const variable holding that constant.

        c_util.binop_category_string(Op, Category, OpStr),
        ( Category = float_arith_binop ->
            UnboxFloat = Info ^ lout_unboxed_float,
            StaticGroundFloats = Info ^ lout_static_ground_floats,
            (
                UnboxFloat = no,
                StaticGroundFloats = yes,
                float_const_binop_expr_name(Op, SubRvalA, SubRvalB, FloatName)
            ->
                FloatLabel = decl_float_label(FloatName),
                ( decl_set_is_member(FloatLabel, !.DeclSet) ->
                    true
                ;
                    decl_set_insert(FloatLabel, !DeclSet),
                    output_indent(FirstIndent, LaterIndent, !.N, !IO),
                    !:N = !.N + 1,
                    io.write_string("static const ", !IO),
                    output_llds_type(lt_float, !IO),
                    io.write_string(" mercury_float_const_", !IO),
                    io.write_string(FloatName, !IO),
                    io.write_string(" = ", !IO),
                    % Note that we just output the expression here, and
                    % let the C compiler evaluate it, rather than evaluating
                    % it ourselves. This avoids having to deal with some nasty
                    % issues regarding floating point accuracy when doing
                    % cross-compilation.
                    output_rval_as_type(Info, SubRvalA, lt_float, !IO),
                    io.write_string(" ", !IO),
                    io.write_string(OpStr, !IO),
                    io.write_string(" ", !IO),
                    output_rval_as_type(Info, SubRvalB, lt_float, !IO),
                    io.write_string(";\n", !IO)
                )
            ;
                true
            )
        ;
            true
        )
    ;
        Rval = mem_addr(MemRef),
        output_record_mem_ref_decls_format(Info, MemRef,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ).

:- pred output_record_rvals_decls(llds_out_info::in, list(rval)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_rvals_decls(Info, Rvals, !DeclSet, !IO) :-
    output_record_rvals_decls_format(Info, Rvals, "", "", 0, _,
        !DeclSet, !IO).

:- pred output_record_rvals_decls_format(llds_out_info::in, list(rval)::in,
    string::in, string::in, int::in, int::out, decl_set::in,
    decl_set::out, io::di, io::uo) is det.

output_record_rvals_decls_format(_, [], _, _, !N, !DeclSet, !IO).
output_record_rvals_decls_format(Info, [Rval | Rvals],
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    output_record_rval_decls_format(Info, Rval,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO),
    output_record_rvals_decls_format(Info, Rvals,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

:- pred output_record_mem_ref_decls_format(llds_out_info::in, mem_ref::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_mem_ref_decls_format(Info, MemRef, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        ( MemRef = stackvar_ref(Rval)
        ; MemRef = framevar_ref(Rval)
        ),
        output_record_rval_decls_format(Info, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        MemRef = heap_ref(BaseRval, _, OffsetRval),
        output_record_rval_decls_format(Info, BaseRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, OffsetRval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ).

%----------------------------------------------------------------------------%
%
% The following predicates are used to compute the names used for
% floating point static constants.
%

    % Given an rval, succeed iff it is a floating point constant expression;
    % if so, return a name for that rval that is suitable for use in a C
    % identifier. Different rvals must be given different names.
    %
:- pred float_const_expr_name(rval::in, string::out) is semidet.

float_const_expr_name(Expr, Name) :-
    ( Expr = const(llconst_float(Float)) ->
        float_literal_name(Float, Name)
    ; Expr = binop(Op, Arg1, Arg2) ->
        float_const_binop_expr_name(Op, Arg1, Arg2, Name)
    ;
        fail
    ).

    % Given a binop rval, succeed iff that rval is a floating point constant
    % expression; if so, return a name for that rval that is suitable for use
    % in a C identifier. Different rvals must be given different names.
    %
:- pred float_const_binop_expr_name(binary_op::in, rval::in, rval::in,
    string::out) is semidet.

float_const_binop_expr_name(Op, Arg1, Arg2, Name) :-
    float_op_name(Op, OpName),
    float_const_expr_name(Arg1, Arg1Name),
    float_const_expr_name(Arg2, Arg2Name),
    % We use prefix notation (operator, argument, argument) rather than infix,
    % to ensure that different rvals get different names.
    Name = OpName ++ "_" ++ Arg1Name ++ "_" ++ Arg2Name.

    % Given an rval which is a floating point literal, return
    % a name for that rval that is suitable for use in a C identifier.
    % Different rvals must be given different names.
    %
:- pred float_literal_name(float::in, string::out) is det.

float_literal_name(Float, FloatName) :-
    % The name of the variable is based on the value of the float const, with
    % "pt" instead of ".", "plus" instead of "+", and "neg" instead of "-".
    FloatName0 = c_util.make_float_literal(Float),
    string.replace_all(FloatName0, ".", "pt", FloatName1),
    string.replace_all(FloatName1, "+", "plus", FloatName2),
    string.replace_all(FloatName2, "-", "neg", FloatName).

    % Succeed iff the binary operator is an operator whose return
    % type is float; bind the output string to a name for that operator
    % that is suitable for use in a C identifier
    %
:- pred float_op_name(binary_op::in, string::out) is semidet.

float_op_name(float_plus, "plus").
float_op_name(float_minus, "minus").
float_op_name(float_times, "times").
float_op_name(float_divide, "divide").

%----------------------------------------------------------------------------%

    % Return true if a data structure of the given type will eventually
    % have code addresses filled in inside it. Note that we can't just
    % test the data structure itself, since in the absence of static
    % code addresses the earlier passes will have replaced any code
    % addresses with dummy values that will have to be overridden with
    % the real code address at initialization time.
    %
:- func data_id_may_include_non_static_code_address(data_id) = bool.

data_id_may_include_non_static_code_address(DataId) = MayContain :-
    (
        DataId = rtti_data_id(RttiId),
        MayContain = rtti_id_would_include_code_addr(RttiId)
    ;
        % Common structures can include code addresses, but only in grades
        % with static code addresses.
        ( DataId = proc_tabling_data_id(_, _)
        ; DataId = scalar_common_data_id(_, _)
        ; DataId = vector_common_data_id(_, _)
        ; DataId = layout_slot_id(table_io_decl_id, _)
        ),
        MayContain = no
    ;
        DataId = layout_id(LayoutName),
        MayContain = layout_name_would_include_code_addr(LayoutName)
    ).

:- pred output_cons_arg_types(list(llds_type)::in, string::in, int::in,
    io::di, io::uo) is det.

output_cons_arg_types([], _, _, !IO).
output_cons_arg_types([Type | Types], Indent, ArgNum, !IO) :-
    io.write_string(Indent, !IO),
    output_llds_type(Type, !IO),
    io.write_string(" f", !IO),
    io.write_int(ArgNum, !IO),
    io.write_string(";\n", !IO),
    output_cons_arg_types(Types, Indent, ArgNum + 1, !IO).

:- pred output_cons_arg_group_types(assoc_list(llds_type, int)::in,
    string::in, int::in, io::di, io::uo) is det.

output_cons_arg_group_types([], _, _, !IO).
output_cons_arg_group_types([Group | Groups], Indent, ArgNum, !IO) :-
    io.write_string(Indent, !IO),
    Group = Type - ArraySize,
    ( ArraySize = 1 ->
        output_llds_type(Type, !IO),
        io.write_string(" f", !IO),
        io.write_int(ArgNum, !IO),
        io.write_string(";\n", !IO)
    ;
        output_llds_type(Type, !IO),
        io.write_string(" f", !IO),
        io.write_int(ArgNum, !IO),
        io.write_string("[", !IO),
        io.write_int(ArraySize, !IO),
        io.write_string("];\n", !IO)
    ),
    output_cons_arg_group_types(Groups, Indent, ArgNum + 1, !IO).

    % Same as output_llds_type, but will put parentheses around the llds_type.
    %
:- pred output_llds_type_cast(llds_type::in, io::di, io::uo) is det.

output_llds_type_cast(LLDSType, !IO) :-
    io.write_string("(", !IO),
    output_llds_type(LLDSType, !IO),
    io.write_string(") ", !IO).

:- pred output_llds_type(llds_type::in, io::di, io::uo) is det.

output_llds_type(lt_int_least8, !IO) :-
    io.write_string("MR_int_least8_t", !IO).
output_llds_type(lt_uint_least8, !IO) :-
    io.write_string("MR_uint_least8_t", !IO).
output_llds_type(lt_int_least16, !IO) :-
    io.write_string("MR_int_least16_t", !IO).
output_llds_type(lt_uint_least16, !IO) :-
    io.write_string("MR_uint_least16_t", !IO).
output_llds_type(lt_int_least32, !IO) :-
    io.write_string("MR_int_least32_t", !IO).
output_llds_type(lt_uint_least32, !IO) :-
    io.write_string("MR_uint_least32_t", !IO).
output_llds_type(lt_bool, !IO) :-
    io.write_string("MR_Integer", !IO).
output_llds_type(lt_integer, !IO) :-
    io.write_string("MR_Integer", !IO).
output_llds_type(lt_unsigned, !IO) :-
    io.write_string("MR_Unsigned", !IO).
output_llds_type(lt_float, !IO) :-
    io.write_string("MR_Float", !IO).
output_llds_type(lt_word, !IO) :-
    io.write_string("MR_Word", !IO).
output_llds_type(lt_string, !IO) :-
    io.write_string("MR_String", !IO).
output_llds_type(lt_data_ptr, !IO) :-
    io.write_string("MR_Word *", !IO).
output_llds_type(lt_code_ptr, !IO) :-
    io.write_string("MR_Code *", !IO).

:- pred output_common_cell_value(llds_out_info::in, common_cell_value::in,
    io::di, io::uo) is det.

output_common_cell_value(Info, CellValue, !IO) :-
    io.write_string("{\n", !IO),
    (
        CellValue = plain_value(ArgsTypes),
        output_cons_args(Info, ArgsTypes, !IO)
    ;
        CellValue = grouped_args_value(ArgGroups),
        output_cons_arg_groups(Info, ArgGroups, !IO)
    ),
    io.write_string("},\n", !IO).

    % Output the arguments, each on its own line, and with a cast appropriate
    % to its type if that is necessary.
    %
:- pred output_cons_args(llds_out_info::in, assoc_list(rval, llds_type)::in,
    io::di, io::uo) is det.

output_cons_args(_, [], !IO).
output_cons_args(Info, [Rval - Type | RvalsTypes], !IO) :-
    (
        direct_field_int_constant(Type) = yes,
        Rval = const(llconst_int(N))
    ->
        output_int_const(N, Type, !IO)
    ;
        output_rval_as_type(Info, Rval, Type, !IO)
    ),
    (
        RvalsTypes = [_ | _],
        io.write_string(",\n", !IO),
        output_cons_args(Info, RvalsTypes, !IO)
    ;
        RvalsTypes = [],
        io.write_string("\n", !IO)
    ).

:- pred output_cons_arg_groups(llds_out_info::in,
    list(common_cell_arg_group)::in, io::di, io::uo) is det.

output_cons_arg_groups(_, [], !IO).
output_cons_arg_groups(Info, [Group | Groups], !IO) :-
    (
        Group = common_cell_grouped_args(Type, _, Rvals),
        io.write_string("{\n", !IO),
        (
            direct_field_int_constant(Type) = yes,
            list.map(project_int_constant, Rvals, Ints)
        ->
            Check = check_int_const_sizes,
            (
                Check = no,
                output_cons_arg_group_ints(Ints, !IO)
            ;
                Check = yes,
                output_cons_arg_group_ints_check(Ints, Type, !IO)
            )
        ;
            output_cons_arg_group_elements(Info, Type, Rvals, !IO)
        ),
        io.write_string("}", !IO)
    ;
        Group = common_cell_ungrouped_arg(Type, Rval),
        (
            direct_field_int_constant(Type) = yes,
            project_int_constant(Rval, Int)
        ->
            output_int_const(Int, Type, !IO)
        ;
            output_rval_as_type(Info, Rval, Type, !IO)
        )
    ),
    (
        Groups = [_ | _],
        io.write_string(",\n", !IO),
        output_cons_arg_groups(Info, Groups, !IO)
    ;
        Groups = [],
        io.write_string("\n", !IO)
    ).

:- pred output_cons_arg_group_elements(llds_out_info::in, llds_type::in,
    list(rval)::in, io::di, io::uo) is det.

output_cons_arg_group_elements(_, _, [], !IO).
output_cons_arg_group_elements(Info, Type, [Rval | Rvals], !IO) :-
    output_rval_as_type(Info, Rval, Type, !IO),
    (
        Rvals = [_ | _],
        io.write_string(",\n", !IO),
        output_cons_arg_group_elements(Info, Type, Rvals, !IO)
    ;
        Rvals = [],
        io.write_string("\n", !IO)
    ).

:- pred output_cons_arg_group_ints(list(int)::in, io::di, io::uo) is det.

output_cons_arg_group_ints([], !IO).
output_cons_arg_group_ints([Int | Ints], !IO) :-
    io.write_int(Int, !IO),
    (
        Ints = [_ | _],
        io.write_string(",\n", !IO),
        output_cons_arg_group_ints(Ints, !IO)
    ;
        Ints = [],
        io.write_string("\n", !IO)
    ).

:- pred output_cons_arg_group_ints_check(list(int)::in, llds_type::in,
    io::di, io::uo) is det.

output_cons_arg_group_ints_check([], _, !IO).
output_cons_arg_group_ints_check([Int | Ints], Type, !IO) :-
    output_int_const(Int, Type, !IO),
    (
        Ints = [_ | _],
        io.write_string(",\n", !IO),
        output_cons_arg_group_ints_check(Ints, Type, !IO)
    ;
        Ints = [],
        io.write_string("\n", !IO)
    ).

:- pred project_int_constant(rval::in, int::out) is semidet.

project_int_constant(const(llconst_int(N)), N).

:- func check_int_const_sizes = bool.
:- pragma inline(check_int_const_sizes/0).

% If you this to `yes', we will test all integer constants placed into static
% data structures to see if they fit into the space allocated for them.

check_int_const_sizes = no.

:- pred output_int_const(int::in, llds_type::in, io::di, io::uo) is det.
:- pragma inline(output_int_const/4).

output_int_const(N, Type, !IO) :-
    Check = check_int_const_sizes,
    (
        Check = yes,
        ( ok_int_const(N, Type) ->
            io.write_int(N, !IO)
        ;
            unexpected(this_file,
                "output_int_const: constant does not fit in type")
        )
    ;
        Check = no,
        io.write_int(N, !IO)
    ).

:- pred ok_int_const(int::in, llds_type::in) is semidet.
:- pragma inline(ok_int_const/2).

ok_int_const(N, lt_int_least8) :-
    -128 =< N, N < 128.
ok_int_const(N, lt_uint_least8) :-
    0 =< N, N < 256.
ok_int_const(N, lt_int_least16) :-
    -32768 =< N, N < 32768.
ok_int_const(N, lt_uint_least16) :-
    0 =< N, N < 65536.
ok_int_const(_N, lt_int_least32).
ok_int_const(_N, lt_uint_least32).
ok_int_const(_N, lt_bool) :-
    unexpected(this_file, "ok_int_const: not integer constant").
ok_int_const(_N, lt_integer).
ok_int_const(_N, lt_unsigned).
ok_int_const(_, lt_float) :-
    unexpected(this_file, "ok_int_const: not integer constant").
ok_int_const(_, lt_word) :-
    unexpected(this_file, "ok_int_const: not integer constant").
ok_int_const(_, lt_string) :-
    unexpected(this_file, "ok_int_const: not integer constant").
ok_int_const(_, lt_data_ptr) :-
    unexpected(this_file, "ok_int_const: not integer constant").
ok_int_const(_, lt_code_ptr) :-
    unexpected(this_file, "ok_int_const: not integer constant").

%----------------------------------------------------------------------------%

    % output_lval_decls(Lval, ...) outputs the declarations of any
    % static constants, etc. that need to be declared before
    % output_lval(Lval) is called.
    %
:- pred output_record_lval_decls(llds_out_info::in, lval::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_record_lval_decls(Info, Lval, !DeclSet, !IO) :-
    output_record_lval_decls_format(Info, Lval, "", "", 0, _, !DeclSet, !IO).

:- pred output_record_lval_decls_format(llds_out_info::in, lval::in,
    string::in, string::in, int::in, int::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_record_lval_decls_format(Info, Lval, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        Lval = field(_, Rval, FieldNum),
        output_record_rval_decls_format(Info, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO),
        output_record_rval_decls_format(Info, FieldNum,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        ( Lval = succfr_slot(Rval)
        ; Lval = prevfr_slot(Rval)
        ; Lval = redofr_slot(Rval)
        ; Lval = redoip_slot(Rval)
        ; Lval = succip_slot(Rval)
        ; Lval = mem_ref(Rval)
        ),
        output_record_rval_decls_format(Info, Rval,
            FirstIndent, LaterIndent, !N, !DeclSet, !IO)
    ;
        ( Lval = reg(_, _)
        ; Lval = stackvar(_)
        ; Lval = parent_stackvar(_)
        ; Lval = framevar(_)
        ; Lval = succip
        ; Lval = maxfr
        ; Lval = curfr
        ; Lval = hp
        ; Lval = sp
        ; Lval = parent_sp
        ; Lval = lvar(_)
        ; Lval = temp(_, _)
        )
    ;
        Lval = global_var_ref(CGlobalVar),
        ( decl_set_is_member(decl_c_global_var(CGlobalVar), !.DeclSet) ->
            true
        ;
            % All env_var_ref global_var_refs should have been output by
            % output_c_procedure_decls already, and as of now there are no
            % other global_var_refs.
            unexpected(this_file,
                "output_record_lval_decls_format: global_var_ref")
        )
    ).

output_record_code_addr_decls(Info, CodeAddress, !DeclSet, !IO) :-
    output_record_code_addr_decls_format(Info, CodeAddress, "", "", 0, _,
        !DeclSet, !IO).

:- pred output_record_code_addr_decls_format(llds_out_info::in, code_addr::in,
    string::in, string::in, int::in, int::out, decl_set::in,
    decl_set::out, io::di, io::uo) is det.

output_record_code_addr_decls_format(Info, CodeAddress,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    ( decl_set_is_member(decl_code_addr(CodeAddress), !.DeclSet) ->
        true
    ;
        decl_set_insert(decl_code_addr(CodeAddress), !DeclSet),
        need_code_addr_decls(Info, CodeAddress, NeedDecl),
        (
            NeedDecl = yes,
            output_indent(FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_code_addr_decls(Info, CodeAddress, !IO)
        ;
            NeedDecl = no
        )
    ).

:- pred need_code_addr_decls(llds_out_info::in, code_addr::in, bool::out)
    is det.

need_code_addr_decls(Info, CodeAddr, Need) :-
    (
        CodeAddr = code_label(Label),
        (
            Label = entry_label(entry_label_exported, _),
            Need = yes
        ;
            Label = entry_label(entry_label_local, _),
            Need = yes
        ;
            Label = entry_label(entry_label_c_local, _),
            Need = no
        ;
            Label = internal_label(_, _),
            Need = no
        )
    ;
        ( CodeAddr = code_imported_proc(_)
        ; CodeAddr = do_trace_redo_fail_shallow
        ; CodeAddr = do_trace_redo_fail_deep
        ; CodeAddr = do_call_closure(_)
        ; CodeAddr = do_call_class_method(_)
        ; CodeAddr = do_not_reached
        ),
        Need = yes
    ;
        ( CodeAddr = code_succip
        ; CodeAddr = do_succeed(_)
        ),
        Need = no
    ;
        ( CodeAddr = do_redo
        ; CodeAddr = do_fail
        ),
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes,
            Need = no
        ;
            UseMacro = no,
            Need = yes
        )
    ).

:- pred output_code_addr_decls(llds_out_info::in, code_addr::in,
    io::di, io::uo) is det.

output_code_addr_decls(Info, CodeAddr, !IO) :-
    (
        CodeAddr = code_label(Label),
        output_label_as_code_addr_decls(Label, !IO)
    ;
        CodeAddr = code_imported_proc(ProcLabel),
        io.write_string("MR_decl_entry(", !IO),
        output_proc_label_no_prefix(ProcLabel, !IO),
        io.write_string(");\n", !IO)
    ;
        ( CodeAddr = code_succip
        ; CodeAddr = do_succeed(_)
        )
    ;
        CodeAddr = do_redo,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes
        ;
            UseMacro = no,
            io.write_string("MR_declare_entry(", !IO),
            io.write_string("MR_do_redo", !IO),
            io.write_string(");\n", !IO)
        )
    ;
        CodeAddr = do_fail,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes
        ;
            UseMacro = no,
            io.write_string("MR_declare_entry(", !IO),
            io.write_string("MR_do_fail", !IO),
            io.write_string(");\n", !IO)
        )
    ;
        CodeAddr = do_trace_redo_fail_shallow,
        io.write_string("MR_declare_entry(MR_do_trace_redo_fail_shallow);\n",
            !IO)
    ;
        CodeAddr = do_trace_redo_fail_deep,
        io.write_string("MR_declare_entry(MR_do_trace_redo_fail_deep);\n",
            !IO)
    ;
        CodeAddr = do_call_closure(Variant),
        io.write_string("MR_declare_entry(mercury__do_call_closure_", !IO),
        io.write_string(ho_call_variant_to_string(Variant), !IO),
        io.write_string(");\n", !IO)
    ;
        CodeAddr = do_call_class_method(Variant),
        io.write_string("MR_declare_entry(mercury__do_call_class_method_",
            !IO),
        io.write_string(ho_call_variant_to_string(Variant), !IO),
        io.write_string(");\n", !IO)
    ;
        CodeAddr = do_not_reached,
        io.write_string("MR_declare_entry(MR_do_not_reached);\n", !IO)
    ).

:- pred output_label_as_code_addr_decls(label::in, io::di, io::uo) is det.

output_label_as_code_addr_decls(Label, !IO) :-
    (
        Label = entry_label(entry_label_exported, ProcLabel),
        io.write_string("MR_decl_entry(", !IO),
        output_label(entry_label(entry_label_exported, ProcLabel), no, !IO),
        io.write_string(");\n", !IO)
    ;
        Label = entry_label(entry_label_local, _ProcLabel)
    ;
        Label = entry_label(entry_label_c_local, _ProcLabel)
    ;
        Label = internal_label(_, _)
    ).

output_record_data_id_decls(Info, DataId, !DeclSet, !IO) :-
    output_record_data_id_decls_format(Info, DataId, "", "",
        0, _, !DeclSet, !IO).

output_record_data_id_decls_format(Info, DataId, FirstIndent, LaterIndent,
        !N, !DeclSet, !IO) :-
    (
        ( DataId = scalar_common_data_id(_, _)
        ; DataId = vector_common_data_id(_, _)
        ; DataId = layout_slot_id(_, _)
        )
        % These are always declared at the top of the generated C source file.
    ;
        DataId = proc_tabling_data_id(_, _)
        % These are always defined (and therefore declared) before being used.
    ;
        DataId = rtti_data_id(RttiId),
        DeclId = decl_rtti_id(RttiId),
        ( decl_set_is_member(DeclId, !.DeclSet) ->
            true
        ;
            decl_set_insert(DeclId, !DeclSet),
            output_indent(FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_rtti_id_storage_type_name_no_decl(Info, RttiId, no, !IO),
            io.write_string(";\n", !IO)
        )
    ;
        DataId = layout_id(LayoutName),
        DeclId = decl_layout_id(LayoutName),
        ( decl_set_is_member(DeclId, !.DeclSet) ->
            true
        ;
            decl_set_insert(DeclId, !DeclSet),
            output_indent(FirstIndent, LaterIndent, !.N, !IO),
            !:N = !.N + 1,
            output_layout_name_storage_type_name(LayoutName,
                not_being_defined, !IO),
            io.write_string(";\n", !IO)
        )
    ).

output_record_data_ids_decls(_, [], _, _, !N, !DeclSet, !IO).
output_record_data_ids_decls(Info, [DataId | DataIds],
        FirstIndent, LaterIndent, !N, !DeclSet, !IO) :-
    output_record_data_id_decls_format(Info, DataId,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO),
    output_record_data_ids_decls(Info, DataIds,
        FirstIndent, LaterIndent, !N, !DeclSet, !IO).

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

:- pred output_indent(string::in, string::in, int::in, io::di, io::uo) is det.

output_indent(FirstIndent, LaterIndent, N0, !IO) :-
    ( N0 > 0 ->
        io.write_string(LaterIndent, !IO)
    ;
        io.write_string(FirstIndent, !IO)
    ).

%----------------------------------------------------------------------------%

:- pred maybe_output_update_prof_counter(llds_out_info::in, label::in,
    pair(label, set_tree234(label))::in, io::di, io::uo) is det.

maybe_output_update_prof_counter(Info, Label, CallerLabel - ContLabelSet,
        !IO) :-
    % If ProfileTime is no, the definition of MR_update_prof_current_proc
    % is empty anyway.
    ProfileTime = Info ^ lout_profile_time,
    (
        set_tree234.member(ContLabelSet, Label),
        ProfileTime = yes
    ->
        io.write_string("\tMR_update_prof_current_proc(MR_LABEL_AP(", !IO),
        output_label(CallerLabel, no, !IO),
        io.write_string("));\n", !IO)
    ;
        true
    ).

%----------------------------------------------------------------------------%

:- pred output_goto(llds_out_info::in, code_addr::in, label::in,
    io::di, io::uo) is det.

output_goto(Info, Target, CallerLabel, !IO) :-
    (
        Target = code_label(Label),
        % Note that we do some optimization here: instead of always outputting
        % `MR_GOTO(<label>)', we output different things for each different
        % kind of label.
        ProfileCalls = Info ^ lout_profile_calls,
        (
            Label = entry_label(entry_label_exported, _),
            (
                ProfileCalls = yes,
                io.write_string("MR_tailcall(", !IO),
                output_label_as_code_addr(Label, !IO),
                io.write_string(",\n\t\t", !IO),
                output_label_as_code_addr(CallerLabel, !IO),
                io.write_string(");\n", !IO)
            ;
                ProfileCalls = no,
                io.write_string("MR_np_tailcall_ent(", !IO),
                output_label(Label, no, !IO),
                io.write_string(");\n", !IO)
            )
        ;
            Label = entry_label(entry_label_local, _),
            (
                ProfileCalls = yes,
                io.write_string("MR_tailcall(", !IO),
                output_label_as_code_addr(Label, !IO),
                io.write_string(",\n\t\t", !IO),
                output_label_as_code_addr(CallerLabel, !IO),
                io.write_string(");\n", !IO)
            ;
                ProfileCalls = no,
                io.write_string("MR_np_tailcall_ent(", !IO),
                output_label(Label, no, !IO),
                io.write_string(");\n", !IO)
            )
        ;
            Label = entry_label(entry_label_c_local, _),
            (
                ProfileCalls = yes,
                io.write_string("MR_localtailcall(", !IO),
                output_label(Label, !IO),
                io.write_string(",\n\t\t", !IO),
                output_label_as_code_addr(CallerLabel, !IO),
                io.write_string(");\n", !IO)
            ;
                ProfileCalls = no,
                io.write_string("MR_np_localtailcall(", !IO),
                output_label(Label, no, !IO),
                io.write_string(");\n", !IO)
            )
        ;
            Label = internal_label(_, _),
            io.write_string("MR_GOTO_LAB(", !IO),
            output_label(Label, no, !IO),
            io.write_string(");\n", !IO)
        )
    ;
        Target = code_imported_proc(ProcLabel),
        ProfileCalls = Info ^ lout_profile_calls,
        (
            ProfileCalls = yes,
            io.write_string("MR_tailcall(MR_ENTRY(", !IO),
            output_proc_label(ProcLabel, !IO),
            io.write_string("),\n\t\t", !IO),
            output_label_as_code_addr(CallerLabel, !IO),
            io.write_string(");\n", !IO)
        ;
            ProfileCalls = no,
            io.write_string("MR_np_tailcall_ent(", !IO),
            output_proc_label_no_prefix(ProcLabel, !IO),
            io.write_string(");\n", !IO)
        )
    ;
        Target = code_succip,
        io.write_string("MR_proceed();\n", !IO)
    ;
        Target = do_succeed(Last),
        (
            Last = no,
            io.write_string("MR_succeed();\n", !IO)
        ;
            Last = yes,
            io.write_string("MR_succeed_discard();\n", !IO)
        )
    ;
        Target = do_redo,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes,
            io.write_string("MR_redo();\n", !IO)
        ;
            UseMacro = no,
            io.write_string("MR_GOTO(MR_ENTRY(MR_do_redo));\n", !IO)
        )
    ;
        Target = do_fail,
        UseMacro = Info ^ lout_use_macro_for_redo_fail,
        (
            UseMacro = yes,
            io.write_string("MR_fail();\n", !IO)
        ;
            UseMacro = no,
            io.write_string("MR_GOTO(MR_ENTRY(MR_do_fail));\n", !IO)
        )
    ;
        Target = do_trace_redo_fail_shallow,
        io.write_string("MR_GOTO(MR_ENTRY(MR_do_trace_redo_fail_shallow));\n",
            !IO)
    ;
        Target = do_trace_redo_fail_deep,
        io.write_string("MR_GOTO(MR_ENTRY(MR_do_trace_redo_fail_deep));\n",
            !IO)
    ;
        Target = do_call_closure(Variant),
        % see comment in output_call for why we use `noprof_' etc. here
        io.write_string("MR_set_prof_ho_caller_proc(", !IO),
        output_label_as_code_addr(CallerLabel, !IO),
        io.write_string(");\n\t", !IO),
        io.write_string("MR_np_tailcall_ent(do_call_closure_", !IO),
        io.write_string(ho_call_variant_to_string(Variant), !IO),
        io.write_string(");\n", !IO)
    ;
        Target = do_call_class_method(Variant),
        % see comment in output_call for why we use `noprof_' etc. here
        io.write_string("MR_set_prof_ho_caller_proc(", !IO),
        output_label_as_code_addr(CallerLabel, !IO),
        io.write_string(");\n\t", !IO),
        io.write_string("MR_np_tailcall_ent(do_call_class_method_", !IO),
        io.write_string(ho_call_variant_to_string(Variant), !IO),
        io.write_string(");\n", !IO)
    ;
        Target = do_not_reached,
        io.write_string("MR_tailcall(MR_ENTRY(MR_do_not_reached),\n\t\t",
            !IO),
        output_label_as_code_addr(CallerLabel, !IO),
        io.write_string(");\n", !IO)
    ).

    % Note that we also do some optimization here by outputting `localcall'
    % rather than `call' for calls to local labels, or `call_localret' for
    % calls which return to local labels (i.e. most of them).
    %
    % We also reduce the size of the output by emitting shorthand forms of
    % the relevant macros when possible, allowing those shorthand macros
    % to apply mercury__ prefixes and possible MR_ENTRY() wrappers.
    %
:- pred output_call(llds_out_info::in, code_addr::in, code_addr::in,
    label::in, io::di, io::uo) is det.

output_call(Info, Target, Continuation, CallerLabel, !IO) :-
    io.write_string("\t", !IO),
    % For profiling, we ignore calls to do_call_closure and
    % do_call_class_method, because in general they lead to cycles in the call
    % graph that screw up the profile. By generating a `noprof_call' rather
    % than a `call', we ensure that time spent inside those routines
    % is credited to the caller, rather than to do_call_closure or
    % do_call_class_method itself. But if we do use a noprof_call,
    % we need to set MR_prof_ho_caller_proc, so that the callee knows
    % which proc it has been called from.
    (
        ( Target = do_call_closure(_)
        ; Target = do_call_class_method(_)
        )
    ->
        ProfileCall = no,
        io.write_string("MR_set_prof_ho_caller_proc(", !IO),
        output_label_as_code_addr(CallerLabel, !IO),
        io.write_string(");\n\t", !IO)
    ;
        ProfileCall = Info ^ lout_profile_calls
    ),
    (
        Target = code_label(Label),
        % We really shouldn't be calling internal labels ...
        label_is_external_to_c_module(Label) = no
    ->
        (
            ProfileCall = yes,
            io.write_string("MR_localcall(", !IO),
            output_label(Label, !IO),
            io.write_string(",\n\t\t", !IO),
            output_code_addr(Continuation, !IO)
        ;
            ProfileCall = no,
            code_addr_to_string_base(Continuation, BaseStr,
                NeedsPrefix, Wrapper),
            (
                NeedsPrefix = no,
                io.write_string("MR_noprof_localcall(", !IO),
                output_label(Label, no, !IO),
                io.write_string(",\n\t\t", !IO),
                io.write_string(BaseStr, !IO),
                output_code_addr_from_pieces(BaseStr,
                    NeedsPrefix, Wrapper, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_entry,
                io.write_string("MR_np_localcall_ent(", !IO),
                output_label(Label, no, !IO),
                io.write_string(",\n\t\t", !IO),
                io.write_string(BaseStr, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_label,
                io.write_string("MR_np_localcall_lab(", !IO),
                output_label(Label, no, !IO),
                io.write_string(",\n\t\t", !IO),
                io.write_string(BaseStr, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_none,
                io.write_string("MR_np_localcall(", !IO),
                output_label(Label, no, !IO),
                io.write_string(",\n\t\t", !IO),
                output_code_addr_from_pieces(BaseStr,
                    NeedsPrefix, Wrapper, !IO)
            )
        )
    ;
        Continuation = code_label(ContLabel),
        label_is_external_to_c_module(ContLabel) = no
    ->
        (
            ProfileCall = yes,
            io.write_string("MR_call_localret(", !IO),
            output_code_addr(Target, !IO),
            io.write_string(",\n\t\t", !IO),
            output_label(ContLabel, !IO)
        ;
            ProfileCall = no,
            code_addr_to_string_base(Target, BaseStr, NeedsPrefix, Wrapper),
            (
                NeedsPrefix = no,
                io.write_string("MR_noprof_call_localret(", !IO),
                output_code_addr_from_pieces(BaseStr,
                    NeedsPrefix, Wrapper, !IO),
                io.write_string(",\n\t\t", !IO),
                output_label(ContLabel, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_entry,
                io.write_string("MR_np_call_localret_ent(", !IO),
                io.write_string(BaseStr, !IO),
                io.write_string(",\n\t\t", !IO),
                output_label(ContLabel, no, !IO)
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_label,
                % We should never get here; the conditions that lead here
                % in this switch should have been caught by the first
                % if-then-else condition that tests Target.
                unexpected(this_file, "output_call: calling label")
            ;
                NeedsPrefix = yes,
                Wrapper = wrapper_none,
                io.write_string("MR_np_call_localret(", !IO),
                output_code_addr_from_pieces(BaseStr,
                    NeedsPrefix, Wrapper, !IO),
                io.write_string(",\n\t\t", !IO),
                output_label(ContLabel, no, !IO)
            )
        )
    ;
        (
            ProfileCall = yes,
            io.write_string("MR_call(", !IO)
        ;
            ProfileCall = no,
            io.write_string("MR_noprof_call(", !IO)
        ),
        output_code_addr(Target, !IO),
        io.write_string(",\n\t\t", !IO),
        output_code_addr(Continuation, !IO)
    ),
    (
        ProfileCall = yes,
        io.write_string(",\n\t\t", !IO),
        output_label_as_code_addr(CallerLabel, !IO)
    ;
        ProfileCall = no
    ),
    io.write_string(");\n", !IO).

output_code_addr(CodeAddr, !IO) :-
    code_addr_to_string_base(CodeAddr, BaseStr, NeedsPrefix, Wrapper),
    output_code_addr_from_pieces(BaseStr, NeedsPrefix, Wrapper, !IO).

:- type wrapper
    --->    wrapper_entry
    ;       wrapper_label
    ;       wrapper_none.

:- pred output_code_addr_from_pieces(string::in, bool::in, wrapper::in,
    io::di, io::uo) is det.

output_code_addr_from_pieces(BaseStr, NeedsPrefix, Wrapper, !IO) :-
    (
        Wrapper = wrapper_none,
        (
            NeedsPrefix = yes,
            io.write_string(mercury_label_prefix, !IO)
        ;
            NeedsPrefix = no
        ),
        io.write_string(BaseStr, !IO)
    ;
        Wrapper = wrapper_entry,
        (
            NeedsPrefix = yes,
            % The _AP version of the macro adds the prefix.
            io.write_string("MR_ENTRY_AP(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        ;
            NeedsPrefix = no,
            io.write_string("MR_ENTRY(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        )
    ;
        Wrapper = wrapper_label,
        (
            NeedsPrefix = yes,
            % The _AP version of the macro adds the prefix.
            io.write_string("MR_LABEL_AP(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        ;
            NeedsPrefix = no,
            io.write_string("MR_LABEL(", !IO),
            io.write_string(BaseStr, !IO),
            io.write_string(")", !IO)
        )
    ).

:- pred code_addr_to_string_base(code_addr::in, string::out,
    bool::out, wrapper::out) is det.

code_addr_to_string_base(CodeAddr, BaseStr, NeedsPrefix, Wrapper) :-
    (
        CodeAddr = code_label(Label),
        BaseStr = label_to_c_string(Label, no),
        NeedsPrefix = yes,
        IsExternal = label_is_external_to_c_module(Label),
        (
            IsExternal = yes,
            Wrapper = wrapper_entry
        ;
            IsExternal = no,
            Wrapper = wrapper_label
        )
    ;
        CodeAddr = code_imported_proc(ProcLabel),
        BaseStr = proc_label_to_c_string(ProcLabel, no),
        NeedsPrefix = yes,
        Wrapper = wrapper_entry
    ;
        CodeAddr = code_succip,
        BaseStr = "MR_succip",
        NeedsPrefix = no,
        Wrapper = wrapper_none
    ;
        CodeAddr = do_succeed(Last),
        (
            Last = no,
            BaseStr = "MR_do_succeed"
        ;
            Last = yes,
            BaseStr = "MR_do_last_succeed"
        ),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ;
        (
            CodeAddr = do_redo,
            BaseStr = "MR_do_redo"
        ;
            CodeAddr = do_fail,
            BaseStr = "MR_do_fail"
        ;
            CodeAddr = do_trace_redo_fail_shallow,
            BaseStr = "MR_do_trace_redo_fail_shallow"
        ;
            CodeAddr = do_trace_redo_fail_deep,
            BaseStr = "MR_do_trace_redo_fail_deep"
        ;
            CodeAddr = do_not_reached,
            BaseStr = "MR_do_not_reached"
        ),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ;
        CodeAddr = do_call_closure(Variant),
        BaseStr = "mercury__do_call_closure_" ++
            ho_call_variant_to_string(Variant),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ;
        CodeAddr = do_call_class_method(Variant),
        BaseStr = "mercury__do_call_class_method_" ++
            ho_call_variant_to_string(Variant),
        NeedsPrefix = no,
        Wrapper = wrapper_entry
    ).

ho_call_variant_to_string(Variant) = Str :-
    (
        Variant = generic,
        Str = "compact"
    ;
        Variant = specialized_known(Num),
        Str = int_to_string(Num)
    ).

    % Output a list of data ids.
    %
:- pred output_data_ids(llds_out_info::in, list(data_id)::in,
    io::di, io::uo) is det.

output_data_ids(_, [], !IO).
output_data_ids(Info, [DataId | DataIds], !IO) :-
    io.write_string("\t", !IO),
    io.write_list([DataId | DataIds], ",\n\t", output_data_id(Info), !IO),
    io.write_string("\n", !IO).

output_data_id_addr(Info, DataId, !IO) :-
    io.write_string("&", !IO),
    output_data_id(Info, DataId, !IO).

    % Output a data id.
    %
output_data_id(Info, DataId, !IO) :-
    (
        DataId = rtti_data_id(RttiId),
        output_rtti_id(RttiId, !IO)
    ;
        DataId = proc_tabling_data_id(ProcLabel, TablingId),
        io.write_string(tabling_struct_data_addr_string(ProcLabel, TablingId),
            !IO)
    ;
        DataId = scalar_common_data_id(TypeNum, CellNum),
        output_common_scalar_cell_array_name(TypeNum, !IO),
        io.write_string("[", !IO),
        io.write_int(CellNum, !IO),
        io.write_string("]", !IO)
    ;
        DataId = vector_common_data_id(TypeNum, CellNum),
        output_common_vector_cell_array_name(TypeNum, CellNum, !IO)
    ;
        DataId = layout_id(LayoutName),
        output_layout_name(LayoutName, !IO)
    ;
        DataId = layout_slot_id(Kind, PredProcId),
        Kind = table_io_decl_id,
        TableIoDeclMap = Info ^ lout_table_io_decl_map,
        map.lookup(TableIoDeclMap, PredProcId, LayoutSlotName),
        MangledModuleName = Info ^ lout_mangled_module_name,
        output_layout_slot_id(use_layout_macro, MangledModuleName,
            LayoutSlotName, !IO)
    ).

proc_tabling_info_var_name(ProcLabel) =
    tabling_struct_data_addr_string(ProcLabel, tabling_info).

:- func tabling_struct_data_addr_string(proc_label, proc_tabling_struct_id)
    = string.

tabling_struct_data_addr_string(ProcLabel, Id) =
    mercury_var_prefix ++ "_proc" ++ tabling_info_id_str(Id) ++ "__" ++
        proc_label_to_c_string(ProcLabel, no).

:- pred output_common_cell_type_name(type_num::in, io::di, io::uo) is det.

output_common_cell_type_name(type_num(TypeNum), !IO) :-
    io.write_string(mercury_common_type_prefix, !IO),
    io.write_int(TypeNum, !IO).

:- pred output_common_scalar_cell_array_name(type_num::in, io::di, io::uo)
    is det.

output_common_scalar_cell_array_name(type_num(TypeNum), !IO) :-
    io.write_string(mercury_scalar_common_array_prefix, !IO),
    io.write_int(TypeNum, !IO).

:- pred output_common_vector_cell_array_name(type_num::in, int::in,
    io::di, io::uo) is det.

output_common_vector_cell_array_name(type_num(TypeNum), CellNum, !IO) :-
    io.write_string(mercury_vector_common_array_prefix, !IO),
    io.write_int(TypeNum, !IO),
    io.write_string("_", !IO),
    io.write_int(CellNum, !IO).

:- pred output_label_as_code_addr(label::in, io::di, io::uo) is det.

output_label_as_code_addr(Label, !IO) :-
    label_as_code_addr_to_string(Label, Str),
    io.write_string(Str, !IO).

:- func label_is_external_to_c_module(label) = bool.

label_is_external_to_c_module(entry_label(entry_label_exported, _)) = yes.
label_is_external_to_c_module(entry_label(entry_label_local, _)) = yes.
label_is_external_to_c_module(entry_label(entry_label_c_local, _)) = no.
label_is_external_to_c_module(internal_label(_, _)) = no.

:- pred label_as_code_addr_to_string(label::in, string::out) is det.

label_as_code_addr_to_string(Label, Str) :-
    LabelStr = label_to_c_string(Label, no),
    IsEntry = label_is_external_to_c_module(Label),
    (
        IsEntry = yes,
        Str = "MR_ENTRY_AP(" ++ LabelStr ++ ")"
    ;
        IsEntry = no,
        Str = "MR_LABEL_AP(" ++ LabelStr ++ ")"
    ).

:- pred output_label_list_or_not_reached(list(maybe(label))::in,
    io::di, io::uo) is det.

output_label_list_or_not_reached([], !IO).
output_label_list_or_not_reached([MaybeLabel | MaybeLabels], !IO) :-
    output_label_or_not_reached(MaybeLabel, !IO),
    output_label_list_or_not_reached_2(MaybeLabels, !IO).

:- pred output_label_list_or_not_reached_2(list(maybe(label))::in,
    io::di, io::uo) is det.

output_label_list_or_not_reached_2([], !IO).
output_label_list_or_not_reached_2([MaybeLabel | MaybeLabels], !IO) :-
    io.write_string(" MR_AND\n\t\t", !IO),
    output_label_or_not_reached(MaybeLabel, !IO),
    output_label_list_or_not_reached_2(MaybeLabels, !IO).

:- pred output_label_or_not_reached(maybe(label)::in, io::di, io::uo) is det.

output_label_or_not_reached(MaybeLabel, !IO) :-
    (
        MaybeLabel = yes(Label),
        io.write_string("MR_LABEL_AP(", !IO),
        output_label(Label, no, !IO),
        io.write_string(")", !IO)
    ;
        MaybeLabel = no,
        io.write_string("MR_ENTRY(MR_do_not_reached)", !IO)
    ).

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

% Entry labels should generate the same code, regardless of the entry label
% type, because we may refer to an entry label via different entry label types
% in different circumstances.
% For example, the entry label of a recursive unification predicate
% is referred to as local in type_info structures and as c_local
% in the recursive call, since the c_local is special cased in some
% circumstances, leading to better code.

output_label(Label, !IO) :-
    LabelStr = label_to_c_string(Label, yes),
    io.write_string(LabelStr, !IO).

output_label(Label, AddPrefix, !IO) :-
    LabelStr = label_to_c_string(Label, AddPrefix),
    io.write_string(LabelStr, !IO).

label_to_c_string(entry_label(_, ProcLabel), AddPrefix) =
    proc_label_to_c_string(ProcLabel, AddPrefix).
label_to_c_string(internal_label(Num, ProcLabel), AddPrefix) = LabelStr :-
    ProcLabelStr = proc_label_to_c_string(ProcLabel, AddPrefix),
    string.int_to_string(Num, NumStr),
    string.append("_i", NumStr, NumSuffix),
    string.append(ProcLabelStr, NumSuffix, LabelStr).

:- pred output_reg(reg_type::in, int::in, io::di, io::uo) is det.

output_reg(reg_r, N, !IO) :-
    io.write_string(reg_to_string(reg_r, N), !IO).
output_reg(reg_f, _, !IO) :-
    sorry(this_file, "Floating point registers not implemented").

:- pred output_tag(tag::in, io::di, io::uo) is det.

output_tag(Tag, !IO) :-
    io.write_string("MR_mktag(", !IO),
    io.write_int(Tag, !IO),
    io.write_string(")", !IO).

    % Output an rval, converted to the specified type
    %
:- pred output_rval_as_type(llds_out_info::in, rval::in, llds_type::in,
    io::di, io::uo) is det.

output_rval_as_type(Info, Rval, DesiredType, !IO) :-
    llds.rval_type(Rval, ActualType),
    ( types_match(DesiredType, ActualType) ->
        % No casting needed.
        output_rval(Info, Rval, !IO)
    ;
        % We need to convert to the right type first.
        % Convertions to/from float must be treated specially;
        % for the others, we can just use a cast.
        ( DesiredType = lt_float ->
            io.write_string("MR_word_to_float(", !IO),
            output_rval(Info, Rval, !IO),
            io.write_string(")", !IO)
        ; ActualType = lt_float ->
            ( DesiredType = lt_word ->
                output_float_rval_as_word(Info, Rval, !IO)
            ; DesiredType = lt_data_ptr ->
                output_float_rval_as_data_ptr(Info, Rval, !IO)
            ;
                unexpected(this_file, "output_rval_as_type: type error")
            )
        ;
            (
                Rval = const(llconst_int(N)),
                direct_field_int_constant(DesiredType) = yes
            ->
                % The condition above increases the runtime of
                % the compiler very slightly. The elimination
                % of the unnecessary casts reduces the size
                % of the generated C source file, which has
                % a considerably longer lifetime. In debugging
                % grades, the file size difference can be
                % very substantial (in the range of megabytes).
                output_int_const(N, DesiredType, !IO)
            ;
                % Cast value to desired type.
                output_llds_type_cast(DesiredType, !IO),
                output_rval(Info, Rval, !IO)
            )
        )
    ).

    % types_match(DesiredType, ActualType) is true iff
    % a value of type ActualType can be used as a value of
    % type DesiredType without casting.
    %
:- pred types_match(llds_type::in, llds_type::in) is semidet.

types_match(Type, Type).
types_match(lt_word, lt_unsigned).
types_match(lt_word, lt_integer).
types_match(lt_word, lt_bool).
types_match(lt_bool, lt_integer).
types_match(lt_bool, lt_unsigned).
types_match(lt_bool, lt_word).
types_match(lt_integer, lt_bool).

    % Return true iff an integer constant can be used directly as a value
    % in a structure field of the given type, instead of being cast to
    % MR_Integer first and then to the type. The answer can be
    % conservative: it is always ok to return `no'.
    %
    % Only the compiler generates values of the uint_leastN types,
    % and for these the constant will never be negative.
    %
:- func direct_field_int_constant(llds_type) = bool.

direct_field_int_constant(lt_bool) = no.
direct_field_int_constant(lt_int_least8) = yes.
direct_field_int_constant(lt_uint_least8) = yes.
direct_field_int_constant(lt_int_least16) = yes.
direct_field_int_constant(lt_uint_least16) = yes.
direct_field_int_constant(lt_int_least32) = yes.
direct_field_int_constant(lt_uint_least32) = yes.
direct_field_int_constant(lt_integer) = yes.
direct_field_int_constant(lt_unsigned) = yes.
direct_field_int_constant(lt_float) = no.
direct_field_int_constant(lt_string) = no.
direct_field_int_constant(lt_data_ptr) = no.
direct_field_int_constant(lt_code_ptr) = no.
direct_field_int_constant(lt_word) = no.

    % Output a float rval, converted to type `MR_Word *'
    %
:- pred output_float_rval_as_data_ptr(llds_out_info::in, rval::in,
    io::di, io::uo) is det.

output_float_rval_as_data_ptr(Info, Rval, !IO) :-
    output_float_rval(Info, Rval, yes, !IO).

    % Output a float rval, converted to type `MR_Word'
    %
:- pred output_float_rval_as_word(llds_out_info::in, rval::in,
    io::di, io::uo) is det.

output_float_rval_as_word(Info, Rval, !IO) :-
    output_float_rval(Info, Rval, no, !IO).

    % Output a float rval, converted to type `MR_Word' or `MR_Word *'
    %
:- pred output_float_rval(llds_out_info::in, rval::in, bool::in,
    io::di, io::uo) is det.

output_float_rval(Info, Rval, IsPtr, !IO) :-
    % For float constant expressions, if we're using boxed floats
    % and --static-ground-floats is enabled, we just refer to the static const
    % which we declared earlier.
    UnboxFloat = Info ^ lout_unboxed_float,
    StaticGroundFloats = Info ^ lout_static_ground_floats,
    (
        UnboxFloat = no,
        StaticGroundFloats = yes,
        float_const_expr_name(Rval, FloatName)
    ->
        (
            IsPtr = yes,
            Cast = lt_data_ptr
        ;
            IsPtr = no,
            Cast = lt_word
        ),
        output_llds_type_cast(Cast, !IO),
        io.write_string("&mercury_float_const_", !IO),
        io.write_string(FloatName, !IO)
    ;
        (
            IsPtr = yes,
            output_llds_type_cast(lt_data_ptr, !IO)
        ;
            IsPtr = no
        ),
        io.write_string("MR_float_to_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_test_rval(llds_out_info::in, rval::in, io::di, io::uo) is det.

output_test_rval(Info, Test, !IO) :-
    (
        is_int_cmp(Test, Left, RightConst, OpStr, _)
    ->
        io.write_string(OpStr, !IO),
        io.write_string("(", !IO),
        output_rval(Info, Left, !IO),
        io.write_string(",", !IO),
        io.write_int(RightConst, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        is_int_cmp(InnerTest, Left, RightConst, _, NegOpStr)
    ->
        io.write_string(NegOpStr, !IO),
        io.write_string("(", !IO),
        output_rval(Info, Left, !IO),
        io.write_string(",", !IO),
        io.write_int(RightConst, !IO),
        io.write_string(")", !IO)
    ;
        is_ptag_test(Test, Rval, Ptag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_PTAG_TEST(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_PTAG_TESTR(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        is_ptag_test(InnerTest, Rval, Ptag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_PTAG_TESTR(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_PTAG_TEST(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(")", !IO)
    ;
        Test = binop(logical_and, Left, Right),
        is_ptag_test(Left, Rval, Ptag, no),
        is_remote_stag_test(Right, Rval, Ptag, Stag)
    ->
        io.write_string("MR_RTAGS_TEST(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        InnerTest = binop(logical_and, Left, Right),
        is_ptag_test(Left, Rval, Ptag, no),
        is_remote_stag_test(Right, Rval, Ptag, Stag)
    ->
        io.write_string("MR_RTAGS_TESTR(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        is_local_stag_test(Test, Rval, Ptag, Stag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_LTAGS_TEST(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_LTAGS_TESTR(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        Test = unop(logical_not, InnerTest),
        is_local_stag_test(InnerTest, Rval, Ptag, Stag, Negated)
    ->
        (
            Negated = no,
            io.write_string("MR_LTAGS_TESTR(", !IO)
        ;
            Negated = yes,
            io.write_string("MR_LTAGS_TEST(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(",", !IO),
        io.write_int(Ptag, !IO),
        io.write_string(",", !IO),
        io.write_int(Stag, !IO),
        io.write_string(")", !IO)
    ;
        output_rval_as_type(Info, Test, lt_bool, !IO)
    ).

:- pred is_int_cmp(rval::in, rval::out, int::out, string::out, string::out)
    is semidet.

is_int_cmp(Test, Left, RightConst, OpStr, NegOpStr) :-
    Test = binop(Op, Left, Right),
    Right = const(llconst_int(RightConst)),
    (
        Op = eq,
        OpStr = "MR_INT_EQ",
        NegOpStr = "MR_INT_NE"
    ;
        Op = ne,
        OpStr = "MR_INT_NE",
        NegOpStr = "MR_INT_EQ"
    ;
        Op = int_lt,
        OpStr = "MR_INT_LT",
        NegOpStr = "MR_INT_GE"
    ;
        Op = int_gt,
        OpStr = "MR_INT_GT",
        NegOpStr = "MR_INT_LT"
    ;
        Op = int_le,
        OpStr = "MR_INT_LE",
        NegOpStr = "MR_INT_GT"
    ;
        Op = int_ge,
        OpStr = "MR_INT_GE",
        NegOpStr = "MR_INT_LT"
    ).

:- pred is_ptag_test(rval::in, rval::out, int::out, bool::out) is semidet.

is_ptag_test(Test, Rval, Ptag, Negated) :-
    Test = binop(Op, Left, Right),
    Left = unop(tag, Rval),
    Right = unop(mktag, const(llconst_int(Ptag))),
    (
        Op = eq,
        Negated = no
    ;
        Op = ne,
        Negated = yes
    ).

:- pred is_remote_stag_test(rval::in, rval::in, int::in, int::out) is semidet.

is_remote_stag_test(Test, Rval, Ptag, Stag) :-
    Test = binop(eq, Left, Right),
    Left = lval(field(yes(Ptag), Rval, Zero)),
    Zero = const(llconst_int(0)),
    Right = const(llconst_int(Stag)).

:- pred is_local_stag_test(rval::in, rval::out, int::out, int::out, bool::out)
    is semidet.

is_local_stag_test(Test, Rval, Ptag, Stag, Negated) :-
    Test = binop(Op, Rval, Right),
    Right = mkword(Ptag, unop(mkbody, const(llconst_int(Stag)))),
    (
        Op = eq,
        Negated = no
    ;
        Op = ne,
        Negated = yes
    ).

output_rval(Info, Rval, !IO) :-
    (
        Rval = const(Const),
        output_rval_const(Info, Const, !IO)
    ;
        Rval = unop(UnaryOp, SubRvalA),
        c_util.unary_prefix_op(UnaryOp, OpString),
        io.write_string(OpString, !IO),
        io.write_string("(", !IO),
        llds.unop_arg_type(UnaryOp, ArgType),
        output_rval_as_type(Info, SubRvalA, ArgType, !IO),
        io.write_string(")", !IO)
    ;
        Rval = binop(Op, SubRvalA, SubRvalB),
        binop_category_string(Op, Category, OpStr),
        (
            Category = array_index_binop,
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_data_ptr, !IO),
            io.write_string(")[", !IO),
            output_rval_as_type(Info, SubRvalB, lt_integer, !IO),
            io.write_string("]", !IO)
        ;
            Category = compound_compare_binop,
            % These operators are intended to be generated only when using
            % the Erlang backend.
            unexpected(this_file, "output_rval: compound_compare_binop")
        ;
            Category = string_compare_binop,
            io.write_string("(strcmp(", !IO),
            ( SubRvalA = const(llconst_string(SubRvalAConst)) ->
                output_rval_const(Info, llconst_string(SubRvalAConst), !IO)
            ;
                io.write_string("(char *) ", !IO),
                output_rval_as_type(Info, SubRvalA, lt_data_ptr, !IO)
            ),
            io.write_string(", ", !IO),
            ( SubRvalB = const(llconst_string(SubRvalBConst)) ->
                output_rval_const(Info, llconst_string(SubRvalBConst), !IO)
            ;
                io.write_string("(char *) ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_data_ptr, !IO)
            ),
            io.write_string(")", !IO),
            io.write_string(" ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" ", !IO),
            io.write_string("0)", !IO)
        ;
            ( Category = float_compare_binop
            ; Category = float_arith_binop
            ),
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_float, !IO),
            io.write_string(" ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_float, !IO),
            io.write_string(")", !IO)
        ;
            Category = unsigned_compare_binop,
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_unsigned, !IO),
            io.write_string(" ", !IO),
            io.write_string(OpStr, !IO),
            io.write_string(" ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_unsigned, !IO),
            io.write_string(")", !IO)
        ;
            Category = int_or_bool_binary_infix_binop,
            (
                % Special-case equality ops to avoid some unnecessary casts --
                % there's no difference between signed and unsigned equality,
                % so if both args are unsigned, we don't need to cast them to
                % MR_Integer.
                ( Op = eq ; Op = ne ),
                llds.rval_type(SubRvalA, SubRvalAType),
                ( SubRvalAType = lt_word ; SubRvalAType = lt_unsigned ),
                llds.rval_type(SubRvalB, SubRvalBType),
                ( SubRvalBType = lt_word ; SubRvalBType = lt_unsigned )
            ->
                io.write_string("(", !IO),
                output_rval(Info, SubRvalA, !IO),
                io.write_string(" ", !IO),
                io.write_string(OpStr, !IO),
                io.write_string(" ", !IO),
                output_rval(Info, SubRvalB, !IO),
                io.write_string(")", !IO)
        %   ;
        %       XXX broken for C == minint
        %       (since `NewC = 0 - C' overflows)
        %       Op = (+),
        %       SubRvalB = const(llconst_int(C)),
        %       C < 0
        %   ->
        %       NewOp = (-),
        %       NewC = 0 - C,
        %       NewSubRvalB = const(llconst_int(NewC)),
        %       io.write_string("("),
        %       output_rval(SubRvalA),
        %       io.write_string(" "),
        %       io.write_string(NewOpStr),
        %       io.write_string(" "),
        %       output_rval(NewSubRvalB),
        %       io.write_string(")")
            ;
                io.write_string("(", !IO),
                output_rval_as_type(Info, SubRvalA, lt_integer, !IO),
                io.write_string(" ", !IO),
                io.write_string(OpStr, !IO),
                io.write_string(" ", !IO),
                output_rval_as_type(Info, SubRvalB, lt_integer, !IO),
                io.write_string(")", !IO)
            )
        ;
            Category = macro_binop,
            io.write_string(OpStr, !IO),
            io.write_string("(", !IO),
            output_rval_as_type(Info, SubRvalA, lt_integer, !IO),
            io.write_string(", ", !IO),
            output_rval_as_type(Info, SubRvalB, lt_integer, !IO),
            io.write_string(")", !IO)
        )
    ;
        Rval = mkword(Tag, SubRval),
        (
            SubRval = const(llconst_data_addr(DataId, no)),
            DataId = scalar_common_data_id(type_num(TypeNum), CellNum)
        ->
            io.write_string("MR_TAG_COMMON(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(",", !IO),
            io.write_int(TypeNum, !IO),
            io.write_string(",", !IO),
            io.write_int(CellNum, !IO),
            io.write_string(")", !IO)
        ;
            SubRval = unop(mkbody, const(llconst_int(Body)))
        ->
            io.write_string("MR_tbmkword(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO),
            io.write_int(Body, !IO),
            io.write_string(")", !IO)
        ;
            io.write_string("MR_tmkword(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO),
            output_rval_as_type(Info, SubRval, lt_data_ptr, !IO),
            io.write_string(")", !IO)
        )
    ;
        Rval = lval(Lval),
        % If a field is used as an rval, then we need to use the
        % MR_const_field() macro or its variants, not the MR_field() macro
        % or its variants, to avoid warnings about discarding const.
        ( Lval = field(MaybeTag, Rval, FieldNumRval) ->
            (
                MaybeTag = yes(Tag),
                io.write_string("MR_ctfield(", !IO),
                io.write_int(Tag, !IO),
                io.write_string(", ", !IO)
            ;
                MaybeTag = no,
                io.write_string("MR_const_mask_field(", !IO)
            ),
            output_rval(Info, Rval, !IO),
            io.write_string(", ", !IO),
            ( FieldNumRval = const(llconst_int(FieldNum)) ->
                % Avoid emitting the (MR_Integer) cast.
                io.write_int(FieldNum, !IO)
            ;
                output_rval(Info, FieldNumRval, !IO)
            ),
            io.write_string(")", !IO)
        ;
            output_lval(Info, Lval, !IO)
        )
    ;
        Rval = var(_),
        unexpected(this_file, "Cannot output a var(_) expression in code")
    ;
        Rval = mem_addr(MemRef),
        (
            MemRef = stackvar_ref(SubRval),
            io.write_string("&MR_sv(", !IO),
            % Don't clutter the output with unnecessary casts.
            ( SubRval = const(llconst_int(SlotNum)) ->
                io.write_int(SlotNum, !IO)
            ;
                output_rval_as_type(Info, SubRval, lt_integer, !IO)
            ),
            io.write_string(")", !IO)
        ;
            MemRef = framevar_ref(SubRval),
            io.write_string("&MR_fv(", !IO),
            % Don't clutter the output with unnecessary casts.
            ( SubRval = const(llconst_int(SlotNum)) ->
                io.write_int(SlotNum, !IO)
            ;
                output_rval_as_type(Info, SubRval, lt_integer, !IO)
            ),
            io.write_string(")", !IO)
        ;
            MemRef = heap_ref(BaseRval, Tag, FieldNumRval),
            io.write_string("&MR_tfield(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO),
            output_rval(Info, BaseRval, !IO),
            io.write_string(", ", !IO),
            % Don't clutter the output with unnecessary casts.
            ( FieldNumRval = const(llconst_int(FieldNum)) ->
                io.write_int(FieldNum, !IO)
            ;
                output_rval_as_type(Info, FieldNumRval, lt_integer, !IO)
            ),
            io.write_string(")", !IO)
        )
    ).

:- pred output_rval_const(llds_out_info::in, rval_const::in,
    io::di, io::uo) is det.

output_rval_const(Info, Const, !IO) :-
    (
        Const = llconst_true,
        io.write_string("MR_TRUE", !IO)
    ;
        Const = llconst_false,
        io.write_string("MR_FALSE", !IO)
    ;
        Const = llconst_int(N),
        % We need to cast to (MR_Integer) to ensure things like 1 << 32 work
        % when `MR_Integer' is 64 bits but `int' is 32 bits.
        output_llds_type_cast(lt_integer, !IO),
        io.write_int(N, !IO)
    ;
        Const = llconst_foreign(Value, Type),
        io.write_char('(', !IO),
        output_llds_type_cast(Type, !IO),
        io.write_string(Value, !IO),
        io.write_char(')', !IO)
    ;
        Const = llconst_float(FloatVal),
        % The cast to (MR_Float) here lets the C compiler do arithmetic in
        % `float' rather than `double' if `MR_Float' is `float' not `double'.
        output_llds_type_cast(lt_float, !IO),
        c_util.output_float_literal(FloatVal, !IO)
    ;
        Const = llconst_string(String),
        io.write_string("MR_string_const(""", !IO),
        c_util.output_quoted_string(String, !IO),
        string.length(String, StringLength),
        io.write_string(""", ", !IO),
        io.write_int(StringLength, !IO),
        io.write_string(")", !IO)
    ;
        Const = llconst_multi_string(String),
        io.write_string("MR_string_const(""", !IO),
        c_util.output_quoted_multi_string(String, !IO),
        io.write_string(""", ", !IO),

        % The "+1" is for the NULL character.
        Length = list.foldl((func(S, L0) = L0 + length(S) + 1), String, 0),
        io.write_int(Length, !IO),
        io.write_string(")", !IO)
    ;
        Const = llconst_code_addr(CodeAddress),
        output_code_addr(CodeAddress, !IO)
    ;
        Const = llconst_data_addr(DataId, MaybeOffset),
        % Data addresses are all assumed to be of type `MR_Word *'; we need to
        % cast them here to avoid type errors. The offset is also in MR_Words.
        (
            MaybeOffset = no,
            % The tests for special cases below increase the runtime of the
            % compiler very slightly, but the use of shorter names reduces
            % the size of the generated C source file, which has a
            % considerably longer lifetime. In debugging grades, the
            % file size difference can be very substantial.
            (
                DataId = scalar_common_data_id(type_num(TypeNum), CellNum)
            ->
                io.write_string("MR_COMMON(", !IO),
                io.write_int(TypeNum, !IO),
                io.write_string(",", !IO),
                io.write_int(CellNum, !IO),
                io.write_string(")", !IO)
            ;
                DataId = rtti_data_id(RttiId),
                rtti_id_emits_type_ctor_info(RttiId, Ctor),
                Ctor = rtti_type_ctor(Module, Name, Arity),
                sym_name_doesnt_need_mangling(Module),
                name_doesnt_need_mangling(Name)
            ->
                output_type_ctor_addr(Module, Name, Arity, !IO)
            ;
                output_llds_type_cast(lt_data_ptr, !IO),
                output_data_id_addr(Info, DataId, !IO)
            )
        ;
            MaybeOffset = yes(Offset),
            io.write_string("((", !IO),
            output_llds_type_cast(lt_data_ptr, !IO),
            output_data_id_addr(Info, DataId, !IO),
            io.write_string(") + ", !IO),
            io.write_int(Offset, !IO),
            io.write_string(")", !IO)
        )
    ).

:- pred output_type_ctor_addr(module_name::in, string::in, int::in,
    io::di, io::uo) is det.

output_type_ctor_addr(Module0, Name, Arity, !IO) :-
    ( Module0 = unqualified("") ->
        Module = mercury_public_builtin_module
    ;
        Module = Module0
    ),
    % We don't need to mangle the module name, but we do need to convert it
    % to a C identifier in the standard fashion.
    ModuleStr = sym_name_mangle(Module),
    ( Arity = 0 ->
        (
            ModuleStr = "builtin",
            ( Name = "int" ->
                Macro = "MR_INT_CTOR_ADDR"
            ; Name = "float" ->
                Macro = "MR_FLOAT_CTOR_ADDR"
            ; Name = "string" ->
                Macro = "MR_STRING_CTOR_ADDR"
            ; Name = "character" ->
                Macro = "MR_CHAR_CTOR_ADDR"
            ;
                fail
            )
        ->
            io.write_string(Macro, !IO)
        ;
            ModuleStr = "io",
            Name = "state"
        ->
            io.write_string("MR_IO_CTOR_ADDR", !IO)
        ;
            ModuleStr = "bool",
            Name = "bool"
        ->
            io.write_string("MR_BOOL_CTOR_ADDR", !IO)
        ;
            io.format("MR_CTOR0_ADDR(%s, %s)", [s(ModuleStr), s(Name)], !IO)
        )
    ; Arity = 1 ->
        (
            Name = "list",
            ModuleStr = "list"
        ->
            io.write_string("MR_LIST_CTOR_ADDR", !IO)
        ;
            Name = "private_builtin",
            ModuleStr = "type_info"
        ->
            io.write_string("MR_TYPE_INFO_CTOR_ADDR", !IO)
        ;
            io.format("MR_CTOR1_ADDR(%s, %s)", [s(ModuleStr), s(Name)], !IO)
        )
    ;
        io.format("MR_CTOR_ADDR(%s, %s, %d)",
            [s(ModuleStr), s(Name), i(Arity)], !IO)
    ).

:- pred output_lval_as_word(llds_out_info::in, lval::in,
    io::di, io::uo) is det.

output_lval_as_word(Info, Lval, !IO) :-
    llds.lval_type(Lval, ActualType),
    ( types_match(lt_word, ActualType) ->
        output_lval(Info, Lval, !IO)
    ; ActualType = lt_float ->
        % Sanity check -- if this happens, the LLDS is ill-typed.
        unexpected(this_file, "output_lval_as_word: got float")
    ;
        io.write_string("MR_LVALUE_CAST(MR_Word,", !IO),
        output_lval(Info, Lval, !IO),
        io.write_string(")", !IO)
    ).

:- pred output_lval(llds_out_info::in, lval::in, io::di, io::uo) is det.

output_lval(Info, Lval, !IO) :-
    (
        Lval = reg(Type, Num),
        output_reg(Type, Num, !IO)
    ;
        Lval = stackvar(N),
        ( N =< 0 ->
            unexpected(this_file, "stack var out of range")
        ;
            true
        ),
        io.write_string("MR_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = parent_stackvar(N),
        ( N =< 0 ->
            unexpected(this_file, "parent stack var out of range")
        ;
            true
        ),
        io.write_string("MR_parent_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = framevar(N),
        ( N =< 0 ->
            unexpected(this_file, "frame var out of range")
        ;
            true
        ),
        io.write_string("MR_fv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = succip,
        io.write_string("MR_succip", !IO)
    ;
        Lval = sp,
        io.write_string("MR_sp", !IO)
    ;
        Lval = parent_sp,
        io.write_string("MR_parent_sp", !IO)
    ;
        Lval = hp,
        io.write_string("MR_hp", !IO)
    ;
        Lval = maxfr,
        io.write_string("MR_maxfr", !IO)
    ;
        Lval = curfr,
        io.write_string("MR_curfr", !IO)
    ;
        Lval = succfr_slot(Rval),
        io.write_string("MR_succfr_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = prevfr_slot(Rval),
        io.write_string("MR_prevfr_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redofr_slot(Rval),
        io.write_string("MR_redofr_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redoip_slot(Rval),
        io.write_string("MR_redoip_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = succip_slot(Rval),
        io.write_string("MR_succip_slot(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = field(MaybeTag, Rval, FieldNumRval),
        (
            MaybeTag = yes(Tag),
            io.write_string("MR_tfield(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO)
        ;
            MaybeTag = no,
            io.write_string("MR_mask_field(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(", ", !IO),
        ( FieldNumRval = const(llconst_int(FieldNum)) ->
            % Avoid emitting the (MR_Integer) cast.
            io.write_int(FieldNum, !IO)
        ;
            output_rval(Info, FieldNumRval, !IO)
        ),
        io.write_string(")", !IO)
    ;
        Lval = lvar(_),
        unexpected(this_file, "output_lval/3: illegal to output an lvar.")
    ;
        Lval = temp(Type, Num),
        (
            Type = reg_r,
            io.write_string("MR_tempr", !IO),
            io.write_int(Num, !IO)
        ;
            Type = reg_f,
            io.write_string("MR_tempf", !IO),
            io.write_int(Num, !IO)
        )
    ;
        Lval = mem_ref(Rval),
        io.write_string("* (MR_Word *) (", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = global_var_ref(GlobalVar),
        io.write_string(c_global_var_name(GlobalVar), !IO)
    ).

:- pred output_lval_for_assign(llds_out_info::in, lval::in, llds_type::out,
    io::di, io::uo) is det.

output_lval_for_assign(Info, Lval, Type, !IO) :-
    (
        Lval = reg(RegType, Num),
        Type = lt_word,
        expect(unify(RegType, reg_r), this_file,
            "output_lval_for_assign: float reg"),
        output_reg(RegType, Num, !IO)
    ;
        Lval = stackvar(N),
        Type = lt_word,
        ( N < 0 ->
            unexpected(this_file, "stack var out of range")
        ;
            true
        ),
        io.write_string("MR_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = parent_stackvar(N),
        Type = lt_word,
        ( N < 0 ->
            unexpected(this_file, "parent stack var out of range")
        ;
            true
        ),
        io.write_string("MR_parent_sv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = framevar(N),
        Type = lt_word,
        ( N =< 0 ->
            unexpected(this_file, "frame var out of range")
        ;
            true
        ),
        io.write_string("MR_fv(", !IO),
        io.write_int(N, !IO),
        io.write_string(")", !IO)
    ;
        Lval = succip,
        Type = lt_word,
        io.write_string("MR_succip_word", !IO)
    ;
        Lval = sp,
        Type = lt_word,
        io.write_string("MR_sp_word", !IO)
    ;
        Lval = parent_sp,
        Type = lt_data_ptr,
        io.write_string("MR_parent_sp", !IO)
    ;
        Lval = hp,
        Type = lt_word,
        io.write_string("MR_hp_word", !IO)
    ;
        Lval = maxfr,
        Type = lt_word,
        io.write_string("MR_maxfr_word", !IO)
    ;
        Lval = curfr,
        Type = lt_word,
        io.write_string("MR_curfr_word", !IO)
    ;
        Lval = succfr_slot(Rval),
        Type = lt_word,
        io.write_string("MR_succfr_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = prevfr_slot(Rval),
        Type = lt_word,
        io.write_string("MR_prevfr_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redofr_slot(Rval),
        Type = lt_word,
        io.write_string("MR_redofr_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = redoip_slot(Rval),
        Type = lt_word,
        io.write_string("MR_redoip_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = succip_slot(Rval),
        Type = lt_word,
        io.write_string("MR_succip_slot_word(", !IO),
        output_rval(Info, Rval, !IO),
        io.write_string(")", !IO)
    ;
        Lval = field(MaybeTag, Rval, FieldNumRval),
        Type = lt_word,
        (
            MaybeTag = yes(Tag),
            io.write_string("MR_tfield(", !IO),
            io.write_int(Tag, !IO),
            io.write_string(", ", !IO)
        ;
            MaybeTag = no,
            io.write_string("MR_mask_field(", !IO)
        ),
        output_rval(Info, Rval, !IO),
        io.write_string(", ", !IO),
        ( FieldNumRval = const(llconst_int(FieldNum)) ->
            % Avoid emitting the (MR_Integer) cast.
            io.write_int(FieldNum, !IO)
        ;
            output_rval(Info, FieldNumRval, !IO)
        ),
        io.write_string(")", !IO)
    ;
        Lval = lvar(_),
        unexpected(this_file, "output_lval_for_assign: lvar")
    ;
        Lval = temp(RegType, Num),
        (
            RegType = reg_r,
            Type = lt_word,
            io.write_string("MR_tempr", !IO),
            io.write_int(Num, !IO)
        ;
            RegType = reg_f,
            Type = lt_float,
            io.write_string("MR_tempf", !IO),
            io.write_int(Num, !IO)
        )
    ;
        Lval = mem_ref(_MemRef),
        Type = lt_word,
        output_lval(Info, Lval, !IO)
    ;
        Lval = global_var_ref(GlobalVar),
        Type = lt_word,
        io.write_string(c_global_var_name(GlobalVar), !IO)
    ).

:- func c_global_var_name(c_global_var_ref) = string.

% The calls to env_var_is_acceptable_char in prog_io_goal.m ensure that
% EnvVarName is acceptable as part of a C identifier.
% The prefix must be identical to envvar_prefix in util/mkinit.c and
% global_var_name in mlds_to_c.m.
c_global_var_name(env_var_ref(EnvVarName)) = "mercury_envvar_" ++ EnvVarName.

%----------------------------------------------------------------------------%

:- pred output_set_line_num(llds_out_info::in, prog_context::in,
    io::di, io::uo) is det.

output_set_line_num(Info, Context, !IO) :-
    LineNumbers = Info ^ lout_line_numbers,
    (
        LineNumbers = yes,
        term.context_file(Context, File),
        term.context_line(Context, Line),
        c_util.always_set_line_num(File, Line, !IO)
    ;
        LineNumbers = no
    ).

:- pred output_reset_line_num(llds_out_info::in, io::di, io::uo) is det.

output_reset_line_num(Info, !IO) :-
    LineNumbers = Info ^ lout_line_numbers,
    (
        LineNumbers = yes,
        c_util.always_reset_line_num(!IO)
    ;
        LineNumbers= no
    ).

%----------------------------------------------------------------------------%

lval_to_string(framevar(N)) =
    "MR_fv(" ++ int_to_string(N) ++ ")".
lval_to_string(stackvar(N)) =
    "MR_sv(" ++ int_to_string(N) ++ ")".
lval_to_string(parent_stackvar(N)) =
    "MR_parent_sv(" ++ int_to_string(N) ++ ")".
lval_to_string(reg(RegType, RegNum)) =
    "reg(" ++ reg_to_string(RegType, RegNum) ++ ")".

reg_to_string(reg_r, N) =
    ( N =< max_real_r_reg ->
        "MR_r" ++ int_to_string(N)
    ; N =< max_virtual_r_reg ->
        "MR_r(" ++ int_to_string(N) ++ ")"
    ;
        unexpected(this_file, "reg_to_string: register number too large")
    ).
reg_to_string(reg_f, N) =
    "MR_f(" ++ int_to_string(N) ++ ")".

:- func max_real_r_reg = int.
:- func max_virtual_r_reg = int.

max_real_r_reg = 32.
max_virtual_r_reg = 1024.

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

explain_stack_slots(StackSlots, VarSet) = Explanation :-
    map.to_assoc_list(StackSlots, StackSlotsList),
    explain_stack_slots_2(StackSlotsList, VarSet, "", Explanation1),
    Explanation = "\nStack slot assignments (if any):\n" ++ Explanation1.

:- pred explain_stack_slots_2(assoc_list(prog_var, stack_slot)::in,
    prog_varset::in, string::in, string::out) is det.

explain_stack_slots_2([], _, !Explanation).
explain_stack_slots_2([Var - Slot | Rest], VarSet, !Explanation) :-
    explain_stack_slots_2(Rest, VarSet, !Explanation),
    (
        Slot = det_slot(SlotNum),
        StackStr = "sv"
    ;
        Slot = parent_det_slot(SlotNum),
        StackStr = "parent_sv"
    ;
        Slot = nondet_slot(SlotNum),
        StackStr = "fv"
    ),
    int_to_string(SlotNum, SlotStr),
    varset.lookup_name(VarSet, Var, VarName),
    string.append_list([VarName, "\t ->\t", StackStr, SlotStr, "\n",
        !.Explanation], !:Explanation).

%---------------------------------------------------------------------------%

:- func init_llds_out_info(module_name, globals,
    map(label, layout_slot_name), map(label, data_id),
    map(pred_proc_id, layout_slot_name)) = llds_out_info.

init_llds_out_info(ModuleName, Globals,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap, TableIoDeclMap)
        = Info :-
    MangledModuleName = sym_name_mangle(ModuleName),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers, LineNumbers),
    globals.lookup_bool_option(Globals, emit_c_loops, EmitCLoops),
    globals.lookup_bool_option(Globals, generate_bytecode, GenerateBytecode),
    globals.lookup_bool_option(Globals, local_thread_engine_base,
        LocalThreadEngineBase),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    globals.lookup_bool_option(Globals, profile_memory, ProfileMemory),
    globals.lookup_bool_option(Globals, profile_deep, ProfileDeep),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
    globals.lookup_bool_option(Globals, static_ground_floats,
        StaticGroundFloats),
    globals.lookup_bool_option(Globals, use_macro_for_redo_fail,
        UseMacroForRedoFail),
    globals.get_trace_level(Globals, TraceLevel),
    Info = llds_out_info(ModuleName, MangledModuleName,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap, TableIoDeclMap,
        AutoComments, LineNumbers,
        EmitCLoops, GenerateBytecode, LocalThreadEngineBase,
        ProfileCalls, ProfileTime, ProfileMemory, ProfileDeep,
        UnboxedFloat, StaticGroundFloats, UseMacroForRedoFail,
        TraceLevel, Globals).

%---------------------------------------------------------------------------%

:- func this_file = string.

this_file = "llds_out.m".

%---------------------------------------------------------------------------%
:- end_module llds_out.
%---------------------------------------------------------------------------%
