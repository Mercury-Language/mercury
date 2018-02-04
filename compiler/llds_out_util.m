%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009, 2011 The University of Melbourne.
% Copyright (C) 2013-2016, 2018 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: llds_out_util.m.
%
% This module defines utility routines for printing out LLDS code and data.
%
%----------------------------------------------------------------------------%

:- module ll_backend.llds_out.llds_out_util.
:- interface.

:- import_module backend_libs.
:- import_module backend_libs.rtti.
:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.trace_params.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module io.
:- import_module map.

%----------------------------------------------------------------------------%

:- type llds_out_info
    --->    llds_out_info(
                lout_module_name                :: module_name,
                lout_mangled_module_name        :: string,
                lout_source_file_name           :: string,
                lout_internal_label_to_layout   :: map(label,
                                                    layout_slot_name),
                lout_entry_label_to_layout      :: map(label, data_id),
                lout_table_io_entry_map         :: map(pred_proc_id,
                                                    layout_slot_name),
                lout_alloc_site_map             :: map(alloc_site_id,
                                                    layout_slot_name),
                lout_auto_comments              :: bool,
                lout_foreign_line_numbers       :: bool,
                lout_emit_c_loops               :: bool,
                lout_generate_bytecode          :: bool,
                lout_local_thread_engine_base   :: bool,
                lout_profile_calls              :: bool,
                lout_profile_time               :: bool,
                lout_profile_memory             :: bool,
                lout_profile_deep               :: bool,
                lout_unboxed_float              :: bool,
                lout_det_stack_dword_alignment  :: bool,
                lout_static_ground_floats       :: bool,
                lout_unboxed_int64s             :: bool,
                lout_static_ground_int64s       :: bool,
                lout_use_macro_for_redo_fail    :: bool,
                lout_trace_level                :: trace_level,
                lout_globals                    :: globals
            ).

:- func init_llds_out_info(module_name, string, globals,
    map(label, layout_slot_name), map(label, data_id),
    map(pred_proc_id, layout_slot_name),
    map(alloc_site_id, layout_slot_name)) = llds_out_info.

:- pred output_set_line_num(bool::in, prog_context::in,
    io::di, io::uo) is det.

:- pred output_reset_line_num(bool::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%

:- type decl_id
    --->    decl_float_label(string)
    ;       decl_int64_label(int64)
    ;       decl_uint64_label(uint64)
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

:- pred output_indent(string::in, string::in, int::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.c_util.
:- import_module libs.options.
:- import_module parse_tree.prog_foreign.

:- import_module int.
:- import_module maybe.
:- import_module set_tree234.
:- import_module term.

%----------------------------------------------------------------------------%

init_llds_out_info(ModuleName, SourceFileName, Globals,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap, TableIoEntryMap,
        AllocSiteMap) = Info :-
    MangledModuleName = sym_name_mangle(ModuleName),
    globals.lookup_bool_option(Globals, auto_comments, AutoComments),
    globals.lookup_bool_option(Globals, line_numbers_around_foreign_code,
        ForeignLineNumbers),
    globals.lookup_bool_option(Globals, emit_c_loops, EmitCLoops),
    globals.lookup_bool_option(Globals, generate_bytecode, GenerateBytecode),
    globals.lookup_bool_option(Globals, local_thread_engine_base,
        LocalThreadEngineBase),
    globals.lookup_bool_option(Globals, profile_calls, ProfileCalls),
    globals.lookup_bool_option(Globals, profile_time, ProfileTime),
    globals.lookup_bool_option(Globals, profile_memory, ProfileMemory),
    globals.lookup_bool_option(Globals, profile_deep, ProfileDeep),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloat),
    double_width_floats_on_det_stack(Globals, DetStackDwordAligment),
    globals.lookup_bool_option(Globals, static_ground_floats,
        StaticGroundFloats),
    globals.lookup_bool_option(Globals, unboxed_int64s, UnboxedInt64s),
    globals.lookup_bool_option(Globals, static_ground_int64s,
        StaticGroundInt64s),
    globals.lookup_bool_option(Globals, use_macro_for_redo_fail,
        UseMacroForRedoFail),
    globals.get_trace_level(Globals, TraceLevel),
    Info = llds_out_info(ModuleName, MangledModuleName, SourceFileName,
        InternalLabelToLayoutMap, EntryLabelToLayoutMap, TableIoEntryMap,
        AllocSiteMap,
        AutoComments, ForeignLineNumbers,
        EmitCLoops, GenerateBytecode, LocalThreadEngineBase,
        ProfileCalls, ProfileTime, ProfileMemory, ProfileDeep,
        UnboxedFloat, DetStackDwordAligment, StaticGroundFloats,
        UnboxedInt64s, StaticGroundInt64s, UseMacroForRedoFail,
        TraceLevel, Globals).

output_set_line_num(OutputLineNumbers, Context, !IO) :-
    (
        OutputLineNumbers = yes,
        term.context_file(Context, File),
        term.context_line(Context, Line),
        c_util.always_set_line_num_cur_stream(File, Line, !IO)
    ;
        OutputLineNumbers = no
    ).

output_reset_line_num(OutputLineNumbers, !IO) :-
    (
        OutputLineNumbers = yes,
        c_util.always_reset_line_num_cur_stream(no, !IO)
    ;
        OutputLineNumbers= no
    ).

%----------------------------------------------------------------------------%

:- type decl_set == set_tree234(decl_id).

decl_set_init(DeclSet) :-
    DeclSet = set_tree234.init.

decl_set_insert(DeclId, DeclSet0, DeclSet) :-
    set_tree234.insert(DeclId, DeclSet0, DeclSet).

decl_set_is_member(DeclId, DeclSet) :-
    set_tree234.contains(DeclSet, DeclId).

%----------------------------------------------------------------------------%

output_indent(FirstIndent, LaterIndent, N0, !IO) :-
    ( if N0 > 0 then
        io.write_string(LaterIndent, !IO)
    else
        io.write_string(FirstIndent, !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.llds_out.llds_out_util.
%---------------------------------------------------------------------------%
