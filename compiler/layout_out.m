%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: layout_out.m.
% Author: zs.
%
% This structure converts layout structures from the representation used
% within the compiler to the representation used by the runtime system.
% The types of the inputs are defined in layout.m. The types of the outputs
% are defined in runtime/mercury_stack_layout.h, where the documentation
% of the semantics of the various kinds of layout structures can also be found.
%
% This module should be, but as yet isn't, independent of whether we are
% compiling to LLDS or MLDS.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.layout_out.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.
:- import_module ll_backend.llds_out.llds_out_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.

:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred output_layout_array_decls(llds_out_info::in,
    io.text_output_stream::in,
    list(rval)::in, list(int)::in, list(int)::in, list(int)::in,
    list(maybe(int))::in, list(user_event_data)::in,
    list(label_layout_no_vars)::in,
    list(label_layout_short_vars)::in, list(label_layout_long_vars)::in,
    list(call_site_static_data)::in, list(coverage_point_info)::in,
    list(proc_layout_proc_static)::in,
    list(int)::in, list(int)::in, list(int)::in, list(table_io_entry_data)::in,
    list(layout_slot_name)::in, list(proc_layout_exec_trace)::in,
    list(alloc_site_info)::in, io::di, io::uo) is det.

:- pred output_layout_array_defns(llds_out_info::in,
    io.text_output_stream::in,
    list(rval)::in, list(int)::in, list(int)::in, list(int)::in,
    list(maybe(int))::in, list(user_event_data)::in,
    list(label_layout_no_vars)::in,
    list(label_layout_short_vars)::in, list(label_layout_long_vars)::in,
    list(call_site_static_data)::in, list(coverage_point_info)::in,
    list(proc_layout_proc_static)::in,
    list(int)::in, list(int)::in, list(int)::in, list(table_io_entry_data)::in,
    list(layout_slot_name)::in, list(proc_layout_exec_trace)::in,
    list(string)::in, list(alloc_site_info)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Given the name of a layout structure, output the declaration
    % of the C global variable which will hold it.
    %
:- pred output_layout_name_decl(io.text_output_stream::in, layout_name::in,
    io::di, io::uo) is det.

    % Given the name of a layout structure, output the declaration of the C
    % global variable which will hold it, if it has not already been declared.
    %
:- pred output_maybe_layout_name_decl(io.text_output_stream::in,
    layout_name::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- type use_layout_macro
    --->    do_not_use_layout_macro
    ;       use_layout_macro.

    % Given the mangled name of the module and a reference to a layout array,
    % output the name of the global variable that will hold it. The first arg
    % says whether we should use a macro to refer to the global. Using the
    % macro generates shorter code, but is not valid in all contexts.
    %
:- pred output_layout_array_name(io.text_output_stream::in,
    use_layout_macro::in, string::in, layout_array_name::in,
    io::di, io::uo) is det.

    % Given the mangled name of the module, output the id of the given layout
    % array slot.
    %
:- pred output_layout_slot_id(io.text_output_stream::in, use_layout_macro::in,
    string::in, layout_slot_name::in, io::di, io::uo) is det.

    % Given the mangled name of the module, output a reference to the address
    % of the given layout array slot.
    %
:- pred output_layout_slot_addr(io.text_output_stream::in,
    use_layout_macro::in, string::in, layout_slot_name::in,
    io::di, io::uo) is det.

    % Given a reference to a layout structure, output the name of the
    % global variable that will hold it.
    %
:- pred output_layout_name(io.text_output_stream::in, layout_name::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type being_defined
    --->    not_being_defined
    ;       being_defined.

    % Given a reference to a layout structure, output the storage class
    % (e.g. static), type and name of the global variable that will hold it.
    % The second arg says whether the output is part of the definition of that
    % variable; this influences e.g. whether we output "extern" or not.
    %
:- pred output_layout_name_storage_type_name(io.text_output_stream::in,
    layout_name::in, being_defined::in, io::di, io::uo) is det.

    % Given a mangled module name and a reference to a layout array, output
    % the storage class (e.g. static), type and name of the global variable
    % that will hold it. The bool says whether the output is part of the
    % definition of that variable; this influences e.g. whether we output
    % "extern" or not.
    %
:- pred output_layout_array_name_storage_type_name(io.text_output_stream::in,
    string::in, layout_array_name::in, being_defined::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Given a Mercury representation of a proc layout structure, output its
    % definition in the appropriate C global variable.
    %
:- pred output_proc_layout_data_defn(llds_out_info::in,
    io.text_output_stream::in, proc_layout_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Given a Mercury representation of a closure layout structure, output its
    % definition in the appropriate C global variable.
    %
:- pred output_closure_layout_data_defn(llds_out_info::in,
    io.text_output_stream::in, closure_proc_id_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Given a Mercury representation of a module layout structure, output its
    % definition in the appropriate C global variable.
    %
:- pred output_module_layout_data_defn(llds_out_info::in,
    io.text_output_stream::in, module_layout_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

    % Given a reference to a layout structure, return a bool that is true
    % iff the layout structure contains code addresses.
    %
:- func layout_name_would_include_code_addr(layout_name) = bool.

    % For a given procedure label, return whether the procedure is
    % user-defined or part of a compiler-generated unify, compare or index
    % predicate.
    %
:- func proc_label_user_or_uci(proc_label) = proc_layout_user_or_uci.

    % Return a value of the C type MR_PredFunc corrresponding to the argument.
    %
:- func mr_pred_or_func_to_string(pred_or_func) = string.

    % Return the name of the given port, as in the enum MR_Trace_Port
    % in runtime/mercury_stack_layout.h.
    %
:- func trace_port_to_string(trace_port) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module hlds.hlds_rtti.
:- import_module hlds.pred_name.
:- import_module libs.
:- import_module libs.trace_params.
:- import_module ll_backend.llds_out.llds_out_code_addr.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.

:- import_module assoc_list.
:- import_module char.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

output_layout_array_decls(Info, Stream, PseudoTypeInfos, HLDSVarNums,
        ShortLocns, LongLocns, UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TableIoEntries,
        ProcEventLayouts, ExecTraces, AllocSites, !IO) :-
    MangledModuleName = Info ^ lout_mangled_module_name,
    (
        PseudoTypeInfos = []
    ;
        PseudoTypeInfos = [_ | _],
        PseudoTypeInfoArrayName = pseudo_type_info_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            PseudoTypeInfoArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        HLDSVarNums = []
    ;
        HLDSVarNums = [_ | _],
        HLDSVarNumArrayName = hlds_var_nums_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            HLDSVarNumArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        ShortLocns = []
    ;
        ShortLocns = [_ | _],
        ShortLocnArrayName = short_locns_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            ShortLocnArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        LongLocns = []
    ;
        LongLocns = [_ | _],
        LongLocnArrayName = long_locns_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            LongLocnArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        UserEventVarNums = []
    ;
        UserEventVarNums = [_ | _],
        UserEventVarNumArrayName = user_event_var_nums_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            UserEventVarNumArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        UserEvents = []
    ;
        UserEvents = [_ | _],
        UserEventsArrayName = user_event_layout_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            UserEventsArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        NoVarLabelLayouts = []
    ;
        NoVarLabelLayouts = [_ | _],
        NoVarLabelLayoutArrayName = label_layout_array(label_has_no_var_info),
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            NoVarLabelLayoutArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        SVarLabelLayouts = []
    ;
        SVarLabelLayouts = [_ | _],
        SVarLabelLayoutArrayName =
            label_layout_array(label_has_short_var_info),
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            SVarLabelLayoutArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        LVarLabelLayouts = []
    ;
        LVarLabelLayouts = [_ | _],
        LVarLabelLayoutArrayName =
            label_layout_array(label_has_long_var_info),
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            LVarLabelLayoutArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        CallSiteStatics = []
    ;
        CallSiteStatics = [_ | _],
        CallSiteStaticsArrayName = proc_static_call_sites_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            CallSiteStaticsArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        CoveragePoints = []
    ;
        CoveragePoints = [_ | _],
        CoveragePointsStaticArrayName = proc_static_cp_static_array,
        CoveragePointsDynamicArrayName = proc_static_cp_dynamic_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            CoveragePointsStaticArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO),
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            CoveragePointsDynamicArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        ProcStatics = []
    ;
        ProcStatics = [_ | _],
        ProcStaticArrayName = proc_static_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            ProcStaticArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        ProcHeadVarNums = []
    ;
        ProcHeadVarNums = [_ | _],
        ProcHeadVarNumArrayName = proc_head_var_nums_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            ProcHeadVarNumArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        ProcVarNames = []
    ;
        ProcVarNames = [_ | _],
        ProcVarNameArrayName = proc_var_names_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            ProcVarNameArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        ProcBodyBytecodes = []
    ;
        ProcBodyBytecodes = [_ | _],
        ProcBodyBytecodeArrayName = proc_body_bytecodes_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            ProcBodyBytecodeArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        TableIoEntries = []
    ;
        TableIoEntries = [_ | _],
        TableIoEntrieArrayName = proc_table_io_entry_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            TableIoEntrieArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        ExecTraces = []
    ;
        ExecTraces = [_ | _],
        ExecTraceArrayName = proc_exec_trace_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            ExecTraceArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        ProcEventLayouts = []
    ;
        ProcEventLayouts = [_ | _],
        ProcEventLayoutArrayName = proc_event_layouts_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            ProcEventLayoutArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ),
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        AllocSiteArrayName = alloc_site_array,
        output_layout_array_name_storage_type_name(Stream, MangledModuleName,
            AllocSiteArrayName, not_being_defined, !IO),
        io.write_string(Stream, "[];\n", !IO)
    ).

output_layout_array_defns(Info, Stream, PseudoTypeInfos, HLDSVarNums,
        ShortLocns, LongLocns, UserEventVarNums, UserEvents,
        NoVarLabelLayouts, SVarLabelLayouts, LVarLabelLayouts,
        CallSiteStatics, CoveragePoints, ProcStatics,
        ProcHeadVarNums, ProcVarNames, ProcBodyBytecodes, TableIoEntries,
        ProcEventLayouts, ExecTraces, TSStringTable, AllocSites,
        !DeclSet, !IO) :-
    (
        PseudoTypeInfos = []
    ;
        PseudoTypeInfos = [_ | _],
        io.nl(Stream, !IO),
        output_pseudo_type_info_array_defn(Info, Stream, PseudoTypeInfos, !IO)
    ),
    (
        HLDSVarNums = []
    ;
        HLDSVarNums = [_ | _],
        io.nl(Stream, !IO),
        output_hlds_var_nums_array_defn(Info, Stream, HLDSVarNums, !IO)
    ),
    (
        ShortLocns = []
    ;
        ShortLocns = [_ | _],
        io.nl(Stream, !IO),
        output_short_locns_array_defn(Info, Stream, ShortLocns, !IO)
    ),
    (
        LongLocns = []
    ;
        LongLocns = [_ | _],
        io.nl(Stream, !IO),
        output_long_locns_array_defn(Info, Stream, LongLocns, !IO)
    ),
    (
        UserEventVarNums = []
    ;
        UserEventVarNums = [_ | _],
        io.nl(Stream, !IO),
        output_user_event_var_nums_array_defn(Info, Stream,
            UserEventVarNums, !IO)
    ),
    (
        UserEvents = []
    ;
        UserEvents = [_ | _],
        io.nl(Stream, !IO),
        output_user_events_array_defn(Info, Stream, UserEvents, !IO)
    ),
    (
        NoVarLabelLayouts = []
    ;
        NoVarLabelLayouts = [_ | _],
        io.nl(Stream, !IO),
        output_no_var_label_layouts_array_defn(Info, Stream,
            NoVarLabelLayouts, !IO)
    ),
    (
        SVarLabelLayouts = []
    ;
        SVarLabelLayouts = [_ | _],
        io.nl(Stream, !IO),
        output_short_var_label_layouts_array_defn(Info, Stream,
            SVarLabelLayouts, !IO)
    ),
    (
        LVarLabelLayouts = []
    ;
        LVarLabelLayouts = [_ | _],
        io.nl(Stream, !IO),
        output_long_var_label_layouts_array_defn(Info, Stream,
            LVarLabelLayouts, !IO)
    ),
    (
        CallSiteStatics = []
    ;
        CallSiteStatics = [_ | _],
        io.nl(Stream, !IO),
        output_call_site_static_array(Info, Stream, CallSiteStatics,
            !DeclSet, !IO)
    ),
    (
        CoveragePoints = []
    ;
        CoveragePoints = [_ | _],
        io.nl(Stream, !IO),
        list.length(CoveragePoints, NumCoveragePoints),
        output_proc_static_cp_static_array(Info, Stream, CoveragePoints,
            NumCoveragePoints, !IO),
        output_proc_static_cp_dynamic_array(Info, Stream,
            NumCoveragePoints, !IO)
    ),
    (
        ProcStatics = []
    ;
        ProcStatics = [_ | _],
        io.nl(Stream, !IO),
        output_proc_statics_array_defn(Info, Stream, ProcStatics, !IO)
    ),
    (
        ProcHeadVarNums = []
    ;
        ProcHeadVarNums = [_ | _],
        io.nl(Stream, !IO),
        output_proc_head_var_nums_array(Info, Stream, ProcHeadVarNums, !IO)
    ),
    (
        ProcVarNames = []
    ;
        ProcVarNames = [_ | _],
        io.nl(Stream, !IO),
        output_proc_var_names_array(Info, Stream, ProcVarNames, !IO)
    ),
    (
        ProcBodyBytecodes = []
    ;
        ProcBodyBytecodes = [_ | _],
        io.nl(Stream, !IO),
        output_proc_body_bytecodes_array(Info, Stream, ProcBodyBytecodes, !IO)
    ),
    (
        TableIoEntries = []
    ;
        TableIoEntries = [_ | _],
        io.nl(Stream, !IO),
        output_table_io_entry_array(Info, Stream, TableIoEntries, !IO)
    ),
    (
        ProcEventLayouts = []
    ;
        ProcEventLayouts = [_ | _],
        io.nl(Stream, !IO),
        output_proc_event_layout_array(Info, Stream, ProcEventLayouts, !IO)
    ),
    (
        ExecTraces = []
    ;
        ExecTraces = [_ | _],
        io.nl(Stream, !IO),
        output_exec_traces_array(Info, Stream, ExecTraces, !IO)
    ),
    (
        TSStringTable = []
    ;
        TSStringTable = [_ | _],
        io.nl(Stream, !IO),
        output_threadscope_string_table_array(Info, Stream, TSStringTable, !IO)
    ),
    (
        AllocSites = []
    ;
        AllocSites = [_ | _],
        io.nl(Stream, !IO),
        output_alloc_sites_array(Info, Stream, AllocSites, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Definition of array #1: pseudo_typeinfos.
%

:- pred output_pseudo_type_info_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(rval)::in, io::di, io::uo) is det.

output_pseudo_type_info_array_defn(Info, Stream, PTIs, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    long_length(PTIs, NumPTIs),
    Name = pseudo_type_info_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {", [i(NumPTIs)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        output_ptis_outer_loop_ac(Info, Stream, PTIs, 0, _, !IO)
    ;
        AutoComments = no_auto_comments,
        output_ptis_outer_loop_noac(Info, Stream, PTIs, 0, _, !IO)
    ),
    io.write_string(Stream, "\n};\n\n", !IO).

:- pred output_ptis_outer_loop_ac(llds_out_info::in, io.text_output_stream::in,
    list(rval)::in, int::in, int::out, io::di, io::uo) is det.

output_ptis_outer_loop_ac(_Info, _Stream, [], !Slot, !IO).
output_ptis_outer_loop_ac(Info, Stream, PTIs @ [_ | _], !Slot, !IO) :-
    list.split_upto(1000, PTIs, StartPTIs, LaterPTIs),
    list.chunk(StartPTIs, 10, PTIChunks),
    list.foldl2(output_pti_chunk_ac(Info, Stream), PTIChunks, !Slot, !IO),
    output_ptis_outer_loop_ac(Info, Stream, LaterPTIs, !Slot, !IO).

:- pred output_ptis_outer_loop_noac(llds_out_info::in,
    io.text_output_stream::in, list(rval)::in, int::in, int::out,
    io::di, io::uo) is det.

output_ptis_outer_loop_noac(_Info, _Stream, [], !Slot, !IO).
output_ptis_outer_loop_noac(Info, Stream, PTIs @ [_ | _], !Slot, !IO) :-
    list.split_upto(1000, PTIs, StartPTIs, LaterPTIs),
    list.chunk(StartPTIs, 10, PTIChunks),
    list.foldl2(output_pti_chunk_noac(Info, Stream), PTIChunks, !Slot, !IO),
    output_ptis_outer_loop_noac(Info, Stream, LaterPTIs, !Slot, !IO).

:- pred output_pti_chunk_ac(llds_out_info::in,
    io.text_output_stream::in, list(rval)::in, int::in, int::out,
    io::di, io::uo) is det.

output_pti_chunk_ac(Info, Stream, ChunkPTIs, !Slot, !IO) :-
    list.length(ChunkPTIs, NumChunkPTIs),
    io.format(Stream, "\n/* slots %d+ */ MR_cast_to_pti%d(\n\t",
        [i(!.Slot), i(NumChunkPTIs)], !IO),
    write_out_list(output_rval(Info), ",\n\t", ChunkPTIs, Stream, !IO),
    io.write_string(Stream, ")", !IO),
    !:Slot = !.Slot + NumChunkPTIs.

:- pred output_pti_chunk_noac(llds_out_info::in,
    io.text_output_stream::in, list(rval)::in, int::in, int::out,
    io::di, io::uo) is det.

output_pti_chunk_noac(Info, Stream, ChunkPTIs, !Slot, !IO) :-
    list.length(ChunkPTIs, NumChunkPTIs),
    io.format(Stream, "\nMR_cast_to_pti%d(", [i(NumChunkPTIs)], !IO),
    write_out_list(output_rval(Info), ",\n\t", ChunkPTIs, Stream, !IO),
    io.write_string(Stream, ")", !IO),
    !:Slot = !.Slot + NumChunkPTIs.

%-----------------------------------------------------------------------------%
%
% Definition of array #2: HLDS var numbers.
%

:- pred output_hlds_var_nums_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(int)::in, io::di, io::uo) is det.

output_hlds_var_nums_array_defn(Info, Stream, VarNums, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    long_length(VarNums, NumVarNums),
    Name = hlds_var_nums_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {", [i(NumVarNums)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        output_numbers_in_vector_ac(Stream, VarNums, 0, !IO)
    ;
        AutoComments = no_auto_comments,
        output_numbers_in_vector_noac(Stream, VarNums, 0, !IO)
    ),
    io.write_string(Stream, "\n};\n\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #3: MR_ShortLvals.
%

:- pred output_short_locns_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(int)::in, io::di, io::uo) is det.

output_short_locns_array_defn(Info, Stream, ShortLocns, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(ShortLocns, NumShortLocns),
    Name = short_locns_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {", [i(NumShortLocns)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        output_numbers_in_vector_ac(Stream, ShortLocns, 0, !IO)
    ;
        AutoComments = no_auto_comments,
        output_numbers_in_vector_noac(Stream, ShortLocns, 0, !IO)
    ),
    io.write_string(Stream, "\n};\n\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #4: MR_LongLvals.
%

:- pred output_long_locns_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(int)::in, io::di, io::uo) is det.

output_long_locns_array_defn(Info, Stream, LongLocns, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(LongLocns, NumLongLocns),
    Name = long_locns_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {", [i(NumLongLocns)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        output_numbers_in_vector_ac(Stream, LongLocns, 0, !IO)
    ;
        AutoComments = no_auto_comments,
        output_numbers_in_vector_noac(Stream, LongLocns, 0, !IO)
    ),
    io.write_string(Stream, "\n};\n\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #5: user event variable numbers.
%

:- pred output_user_event_var_nums_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(maybe(int))::in, io::di, io::uo) is det.

output_user_event_var_nums_array_defn(Info, Stream, MaybeVarNums, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(MaybeVarNums, NumMaybeVarNums),
    Name = user_event_var_nums_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {", [i(NumMaybeVarNums)], !IO),
    list.foldl2(output_maybe_var_num_slot(Stream), MaybeVarNums, 0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_maybe_var_num_slot(io.text_output_stream::in, maybe(int)::in,
    int::in, int::out, io::di, io::uo) is det.

output_maybe_var_num_slot(Stream, MaybeVarNum, !Slot, !IO) :-
    (
        MaybeVarNum = no,
        % Zero means not a variable, which is what we want.
        VarNum = 0
    ;
        MaybeVarNum = yes(VarNum)
    ),
    ( if !.Slot mod 10 = 0 then
        io.format(Stream, "\n/* slot %d */ ", [i(!.Slot)], !IO)
    else
        io.write_string(Stream, " ", !IO)
    ),
    io.format(Stream, "%d,", [i(VarNum)], !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #6: user events.
%

:- pred output_user_events_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(user_event_data)::in,
    io::di, io::uo) is det.

output_user_events_array_defn(Info, Stream, UserEvents, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(UserEvents, NumUserEvents),
    Name = user_event_layout_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumUserEvents)], !IO),
    list.foldl2(output_user_event_slot(Info, Stream), UserEvents, 0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_user_event_slot(llds_out_info::in, io.text_output_stream::in,
    user_event_data::in, int::in, int::out, io::di, io::uo) is det.

output_user_event_slot(Info, Stream, UserEvent, !Slot, !IO) :-
    UserEvent = user_event_data(UserEventNumber, UserLocnsRval,
        MaybeVarNumsSlot),
    io.write_string(Stream, "{ ", !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* slot %d */ ", [i(!.Slot)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    io.write_int(Stream, UserEventNumber, !IO),
    io.write_string(Stream, ", (MR_LongLval *) ", !IO),
    output_rval_as_addr(Info, Stream, UserLocnsRval, !IO),
    io.write_string(Stream, ",\n  ", !IO),
    ModuleName = Info ^ lout_mangled_module_name,
    output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
        MaybeVarNumsSlot, !IO),
    io.write_string(Stream, " },\n", !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #7: label layout structures for labels with
% no variable information.
%

:- pred output_no_var_label_layouts_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(label_layout_no_vars)::in,
    io::di, io::uo) is det.

output_no_var_label_layouts_array_defn(Info, Stream, LabelLayouts, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(LabelLayouts, NumLabelLayouts),
    Name = label_layout_array(label_has_no_var_info),
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumLabelLayouts)], !IO),
    list.foldl2(output_no_var_label_layout_slot(Info, Stream), LabelLayouts,
        0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_no_var_label_layout_slot(llds_out_info::in,
    io.text_output_stream::in, label_layout_no_vars::in,
    int::in, int::out, io::di, io::uo) is det.

output_no_var_label_layout_slot(Info, Stream, LabelLayout, !Slot, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    LabelLayout = label_layout_no_vars(BasicLabelLayout),
    BasicLabelLayout = basic_label_layout(_ProcLabel, LabelNum,
        _, _, _, _, _, _),
    % The procedure is given by the proc_label printed from the basic layout.
    io.write_string(Stream, "{ ", !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d, %d */\n  ", [i(!.Slot), i(LabelNum)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    output_basic_label_layout_slot(Info, Stream, ModuleName,
        BasicLabelLayout, !IO),
    io.write_string(Stream, " },\n", !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #8: label layout structures for labels with
% only short-descriptor variable information.
%

:- pred output_short_var_label_layouts_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(label_layout_short_vars)::in,
    io::di, io::uo) is det.

output_short_var_label_layouts_array_defn(Info, Stream, LabelLayouts, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(LabelLayouts, NumLabelLayouts),
    Name = label_layout_array(label_has_short_var_info),
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumLabelLayouts)], !IO),
    list.foldl2(output_short_var_label_layout_slot(Info, Stream), LabelLayouts,
        0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_short_var_label_layout_slot(llds_out_info::in,
    io.text_output_stream::in, label_layout_short_vars::in,
    int::in, int::out, io::di, io::uo) is det.

output_short_var_label_layout_slot(Info, Stream, LabelLayout, !Slot, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    LabelLayout = label_layout_short_vars(BasicLabelLayout, LabelVarInfo),
    BasicLabelLayout = basic_label_layout(_ProcLabel, LabelNum,
        _, _, _, _, _, _),
    % The procedure is given by the proc_label printed from the basic layout.
    io.write_string(Stream, "{ ", !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d, %d */\n  ", [i(!.Slot), i(LabelNum)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    output_basic_label_layout_slot(Info, Stream, ModuleName,
        BasicLabelLayout, !IO),
    io.write_string(Stream, ",\n  ", !IO),

    LabelVarInfo = label_short_var_info(EncodedVarCount, TypeParams, PTIsSlot,
        HLDSVarNumsSlot, ShortLocnsSlot),
    io.write_int(Stream, EncodedVarCount, !IO),
    io.write_string(Stream, ",", !IO),
    ( if
        PTIsSlot >= 0,
        HLDSVarNumsSlot >= 0,
        ShortLocnsSlot >= 0
    then
        ( if
            TypeParams = const(llconst_int(0))
        then
            io.format(Stream, "MR_LLVS0(%s,%d,%d,%d)",
                [s(ModuleName),
                i(PTIsSlot), i(HLDSVarNumsSlot), i(ShortLocnsSlot)], !IO)
        else if
            TypeParams = const(llconst_data_addr(TPDataId, no)),
            TPDataId = scalar_common_data_id(type_num(TPTypeNum), TPCellNum)
        then
            io.format(Stream, "MR_LLVSC(%s,%d,%d,%d,%d,%d)",
                [s(ModuleName), i(TPTypeNum), i(TPCellNum),
                i(PTIsSlot), i(HLDSVarNumsSlot), i(ShortLocnsSlot)], !IO)
        else
            output_rval_as_addr(Info, Stream, TypeParams, !IO),
            io.format(Stream, ",MR_LLVS(%s,%d,%d,%d)",
                [s(ModuleName),
                i(PTIsSlot), i(HLDSVarNumsSlot), i(ShortLocnsSlot)], !IO)
        )
    else
        io.write_string(Stream, "(const MR_TypeParamLocns *) ", !IO),
        output_rval_as_addr(Info, Stream, TypeParams, !IO),
        io.write_string(Stream, ",", !IO),
        ( if PTIsSlot >= 0 then
            output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
                layout_slot(pseudo_type_info_array, PTIsSlot), !IO),
            io.write_string(Stream, ",", !IO)
        else
            io.write_string(Stream, "0,", !IO)
        ),
        ( if HLDSVarNumsSlot >= 0 then
            output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
                layout_slot(hlds_var_nums_array, HLDSVarNumsSlot), !IO),
            io.write_string(Stream, ",", !IO)
        else
            io.write_string(Stream, "0,", !IO)
        ),
        ( if ShortLocnsSlot >= 0 then
            output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
                layout_slot(short_locns_array, ShortLocnsSlot), !IO)
        else
            io.write_string(Stream, "0", !IO)
        )
    ),
    io.write_string(Stream, " },\n", !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #9: label layout structures for labels with
% both short and long variable descriptors.
%

:- pred output_long_var_label_layouts_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(label_layout_long_vars)::in,
    io::di, io::uo) is det.

output_long_var_label_layouts_array_defn(Info, Stream, LabelLayouts, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(LabelLayouts, NumLabelLayouts),
    Name = label_layout_array(label_has_long_var_info),
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumLabelLayouts)], !IO),
    list.foldl2(output_long_var_label_layout_slot(Info, Stream), LabelLayouts,
        0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_long_var_label_layout_slot(llds_out_info::in,
    io.text_output_stream::in, label_layout_long_vars::in,
    int::in, int::out, io::di, io::uo) is det.

output_long_var_label_layout_slot(Info, Stream, LabelLayout, !Slot, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    LabelLayout = label_layout_long_vars(BasicLabelLayout, LabelVarInfo),
    BasicLabelLayout = basic_label_layout(_ProcLabel, LabelNum,
        _, _, _, _, _, _),
    % The procedure is given by the proc_label printed from the basic layout.
    io.write_string(Stream, "{ ", !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d, %d */\n  ", [i(!.Slot), i(LabelNum)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    output_basic_label_layout_slot(Info, Stream, ModuleName,
        BasicLabelLayout, !IO),
    io.write_string(Stream, ",\n  ", !IO),

    LabelVarInfo = label_long_var_info(EncodedVarCount, TypeParams, PTIsSlot,
        HLDSVarNumsSlot, ShortLocnsSlot, LongLocnsSlot),
    ( if LongLocnsSlot >= 0 then
        true
    else
        unexpected($pred, "no long locn")
    ),

    io.write_int(Stream, EncodedVarCount, !IO),
    io.write_string(Stream, ",", !IO),
    ( if
        PTIsSlot >= 0,
        ShortLocnsSlot >= 0,
        HLDSVarNumsSlot >= 0
    then
        ( if
            TypeParams = const(llconst_int(0))
        then
            io.format(Stream, "MR_LLVL0(%s,%d,%d,%d,%d)",
                [s(ModuleName), i(PTIsSlot), i(HLDSVarNumsSlot),
                i(ShortLocnsSlot), i(LongLocnsSlot)], !IO)
        else if
            TypeParams = const(llconst_data_addr(TPDataId, no)),
            TPDataId = scalar_common_data_id(type_num(TPTypeNum), TPCellNum)
        then
            io.format(Stream, "MR_LLVLC(%s,%d,%d,%d,%d,%d,%d)",
                [s(ModuleName), i(TPTypeNum), i(TPCellNum),
                i(PTIsSlot), i(HLDSVarNumsSlot),
                i(ShortLocnsSlot), i(LongLocnsSlot)], !IO)
        else
            output_rval_as_addr(Info, Stream, TypeParams, !IO),
            io.format(Stream, ",MR_LLVL(%s,%d,%d,%d,%d)",
                [s(ModuleName), i(PTIsSlot), i(HLDSVarNumsSlot),
                i(ShortLocnsSlot), i(LongLocnsSlot)], !IO)
        )
    else
        io.write_string(Stream, "(const MR_TypeParamLocns *) ", !IO),
        output_rval_as_addr(Info, Stream, TypeParams, !IO),
        io.write_string(Stream, ",", !IO),
        ( if PTIsSlot >= 0 then
            output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
                layout_slot(pseudo_type_info_array, PTIsSlot), !IO),
            io.write_string(Stream, ",", !IO)
        else
            io.write_string(Stream, "0,", !IO)
        ),
        ( if HLDSVarNumsSlot >= 0 then
            output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
                layout_slot(hlds_var_nums_array, HLDSVarNumsSlot), !IO),
            io.write_string(Stream, ",", !IO)
        else
            io.write_string(Stream, "0,", !IO)
        ),
        ( if ShortLocnsSlot >= 0 then
            output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
                layout_slot(short_locns_array, ShortLocnsSlot), !IO),
            io.write_string(Stream, ",", !IO)
        else
            io.write_string(Stream, "0,", !IO)
        ),
        output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
            layout_slot(long_locns_array, LongLocnsSlot), !IO)
    ),
    io.write_string(Stream, " },\n", !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Common code shared by arrays 7, 8 and 9.
%

:- pred output_basic_label_layout_slot(llds_out_info::in,
    io.text_output_stream::in, string::in, basic_label_layout::in,
    io::di, io::uo) is det.

output_basic_label_layout_slot(_Info, Stream, ModuleName,
        BasicLabelLayout, !IO) :-
    BasicLabelLayout = basic_label_layout(ProcLabel, _LabelNum,
        _ProcLayoutName, MaybePort, MaybeIsHidden, LabelNumberInModule,
        MaybeGoalPath, MaybeUserSlotName),
    some [!MacroName] (
        !:MacroName = "MR_LL",
        % MaybeIsHidden = no means that the value of the hidden field shouldn't
        % matter; we arbitrarily make this mean `not hidden'.
        (
            ( MaybeIsHidden = no
            ; MaybeIsHidden = yes(no)
            )
        ;
            MaybeIsHidden = yes(yes),
            !:MacroName = !.MacroName ++ "_H"
        ),
        (
            MaybeUserSlotName = no
        ;
            MaybeUserSlotName = yes(_),
            !:MacroName = !.MacroName ++ "_U"
        ),
        MacroName = !.MacroName
    ),

    (
        MaybePort = yes(Port),
        PortStr = trace_port_to_string(Port)
    ;
        MaybePort = no,
        PortStr = "NONE"
    ),
    (
        MaybeGoalPath = yes(GoalPath)
    ;
        MaybeGoalPath = no,
        GoalPath = 0
    ),
    ProcLabelStr = proc_label_to_c_string(do_not_add_label_prefix, ProcLabel),
    io.format(Stream, "%s(%s, %s,%d,%d",
        [s(MacroName), s(ProcLabelStr), s(PortStr),
        i(LabelNumberInModule), i(GoalPath)], !IO),
    (
        MaybeUserSlotName = no
    ;
        MaybeUserSlotName = yes(UserSlotName),
        io.write_string(Stream, ",", !IO),
        output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
            UserSlotName, !IO)
    ),
    io.write_string(Stream, ")", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #10: proc static call sites.
%

:- pred output_call_site_static_array(llds_out_info::in,
    io.text_output_stream::in, list(call_site_static_data)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_call_site_static_array(Info, Stream, CallSiteStatics, !DeclSet, !IO) :-
    % At normal call sites, the call_site_static contains a pointer to
    % the proc layout of the callee procedure. Regardless of whether
    % the callee is in this module or not, we won't have declared its
    % proc layout structure yet.
    list.foldl2(output_call_site_static_slot_decls(Info, Stream),
        CallSiteStatics, !DeclSet, !IO),
    io.nl(Stream, !IO),

    ModuleName = Info ^ lout_mangled_module_name,
    Name = proc_static_call_sites_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    list.length(CallSiteStatics, NumCallSiteStatics),
    io.format(Stream, "[%d] = {\n", [i(NumCallSiteStatics)], !IO),
    list.foldl2(output_call_site_static_slot(Info, Stream), CallSiteStatics,
        0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_call_site_static_slot_decls(llds_out_info::in,
    io.text_output_stream::in, call_site_static_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_call_site_static_slot_decls(Info, Stream, CallSiteStatic,
        !DeclSet, !IO) :-
    (
        CallSiteStatic = normal_call(Callee, _, _, _, _),
        CalleeProcLabel = make_proc_label_from_rtti(Callee),
        CalleeUserOrUci = proc_label_user_or_uci(CalleeProcLabel),
        CalleeProcLayoutName =
            proc_layout(Callee, proc_layout_proc_id(CalleeUserOrUci)),
        CalleProcLayoutDataId = layout_id(CalleeProcLayoutName),
        output_record_data_id_decls(Info, Stream, CalleProcLayoutDataId,
            !DeclSet, !IO)
    ;
        ( CallSiteStatic = special_call(_, _, _)
        ; CallSiteStatic = higher_order_call(_, _, _)
        ; CallSiteStatic = method_call(_, _, _)
        ; CallSiteStatic = callback(_, _, _)
        )
    ).

:- pred output_call_site_static_slot(llds_out_info::in,
    io.text_output_stream::in, call_site_static_data::in,
    int::in, int::out, io::di, io::uo) is det.

output_call_site_static_slot(Info, Stream, CallSiteStatic, !Slot, !IO) :-
    io.write_string(Stream, "{ ", !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d */ ", [i(!.Slot)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    (
        CallSiteStatic = normal_call(Callee, TypeSubst, FileName, LineNumber,
            GoalPath),
        io.write_string(Stream,
            "MR_callsite_normal_call, (MR_ProcLayout *)\n&", !IO),
        CalleeProcLabel = make_proc_label_from_rtti(Callee),
        CalleeUserOrUci = proc_label_user_or_uci(CalleeProcLabel),
        CalleeProcLayoutName =
            proc_layout(Callee, proc_layout_proc_id(CalleeUserOrUci)),
        output_layout_name(Stream, CalleeProcLayoutName, !IO),
        io.write_string(Stream, ",\n", !IO),
        ( if TypeSubst = "" then
            io.write_string(Stream, "NULL, ", !IO)
        else
            io.write_string(Stream, """", !IO),
            io.write_string(Stream, TypeSubst, !IO),
            io.write_string(Stream, """, ", !IO)
        )
    ;
        CallSiteStatic = special_call(FileName, LineNumber, GoalPath),
        io.write_string(Stream,
            "MR_callsite_special_call, NULL, NULL, ", !IO)
    ;
        CallSiteStatic = higher_order_call(FileName, LineNumber, GoalPath),
        io.write_string(Stream,
            "MR_callsite_higher_order_call, NULL, NULL, ", !IO)
    ;
        CallSiteStatic = method_call(FileName, LineNumber, GoalPath),
        io.write_string(Stream,
            "MR_callsite_method_call, NULL, NULL, ", !IO)
    ;
        CallSiteStatic = callback(FileName, LineNumber, GoalPath),
        io.write_string(Stream,
            "MR_callsite_callback, NULL, NULL, ", !IO)
    ),
    io.format(Stream, "\"%s\", %d, \"%s\" },\n",
        [s(FileName), i(LineNumber), s(goal_path_to_string(GoalPath))], !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #11: the static parts of coverage points
% (information about the coverage point).
%

:- pred output_proc_static_cp_static_array(llds_out_info::in,
    io.text_output_stream::in, list(coverage_point_info)::in,
    int::in, io::di, io::uo) is det.

output_proc_static_cp_static_array(Info, Stream, CoveragePoints,
        NumCoveragePoints, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    Name = proc_static_cp_static_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumCoveragePoints)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    list.foldl2(output_proc_static_cp_static_slot(Stream, AutoComments),
        CoveragePoints, 0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_proc_static_cp_static_slot(io.text_output_stream::in,
    maybe_auto_comments::in, coverage_point_info::in,
    int::in, int::out, io::di, io::uo) is det.

output_proc_static_cp_static_slot(Stream, AutoComments, CoveragePoint,
        !Slot, !IO) :-
    CoveragePoint = coverage_point_info(RevGoalPath, CPType),
    io.write_string(Stream, "{ ", !IO),
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d */ ", [i(!.Slot)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    GoalPathString = rev_goal_path_to_string(RevGoalPath),
    coverage_point_type_c_value(CPType, CPTypeCValue),
    io.format(Stream, "\"%s\", %s },\n",
        [s(GoalPathString), s(CPTypeCValue)], !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #12: the dynamic parts of coverage points (the counts).
%

:- pred output_proc_static_cp_dynamic_array(llds_out_info::in,
    io.text_output_stream::in, int::in, io::di, io::uo) is det.

output_proc_static_cp_dynamic_array(Info, Stream, NumCoveragePoints, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    Name = proc_static_cp_dynamic_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    % The C compiler will arrange for the array to be initialized to all zeros.
    io.format(Stream, "[%d];\n", [i(NumCoveragePoints)], !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #13: proc static structures.
%

:- pred output_proc_statics_array_defn(llds_out_info::in,
    io.text_output_stream::in, list(proc_layout_proc_static)::in,
    io::di, io::uo) is det.

output_proc_statics_array_defn(Info, Stream, ProcStatics, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(ProcStatics, NumProcStatics),
    Name = proc_static_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumProcStatics)], !IO),
    list.foldl2(output_proc_static_slot(Info, Stream), ProcStatics, 0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_proc_static_slot(llds_out_info::in, io.text_output_stream::in,
    proc_layout_proc_static::in, int::in, int::out, io::di, io::uo) is det.

output_proc_static_slot(Info, Stream, ProcStatic, !Slot, !IO) :-
    ProcStatic = proc_layout_proc_static(FileName, LineNumber,
        IsInInterface, DeepExcpVars, MaybeCallSites, MaybeCoveragePoints),
    io.write_string(Stream, "{ ", !IO),
    AutoComments = Info ^ lout_auto_comments,
    MangledModuleName = Info ^ lout_mangled_module_name,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d */ ", [i(!.Slot)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    quote_and_write_string(Stream, FileName, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, LineNumber, !IO),
    io.write_string(Stream, ",", !IO),
    (
        IsInInterface = yes,
        io.write_string(Stream, "MR_TRUE", !IO)
    ;
        IsInInterface = no,
        io.write_string(Stream, "MR_FALSE", !IO)
    ),
    io.write_string(Stream, ",\n  ", !IO),
    (
        MaybeCallSites = yes({CallSitesSlot, NumCallSites}),
        io.write_int(Stream, NumCallSites, !IO),
        io.write_string(Stream, ",", !IO),
        CallSitesSlotName =
            layout_slot(proc_static_call_sites_array, CallSitesSlot),
        output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
            CallSitesSlotName, !IO),
        io.write_string(Stream, ",\n", !IO)
    ;
        MaybeCallSites = no,
        io.write_string(Stream, "0,NULL,\n", !IO)
    ),
    io.write_string(Stream, "#ifdef MR_USE_ACTIVATION_COUNTS\n", !IO),
    io.write_string(Stream, "0,\n", !IO),
    io.write_string(Stream, "#endif\n", !IO),
    io.write_string(Stream, "NULL,", !IO),
    DeepExcpVars = deep_excp_slots(TopCSDSlot, MiddleCSDSlot,
        OldOutermostSlot),
    io.write_int(Stream, TopCSDSlot, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, MiddleCSDSlot, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, OldOutermostSlot, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_string(Stream, "\n#ifdef MR_DEEP_PROFILING_COVERAGE\n", !IO),
    (
        MaybeCoveragePoints = yes({CoveragePointsSlot, NumCoveragePoints}),
        % If MR_DEEP_PROFILING_COVERAGE is not defined but
        % --deep-profiling-coverage is, this generated code will not compile,
        % as these fields in this structure will not be present.
        io.write_int(Stream, NumCoveragePoints, !IO),
        io.write_string(Stream, ",\n", !IO),
        CoveragePointsStaticSlotName =
            layout_slot(proc_static_cp_static_array, CoveragePointsSlot),
        output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
            CoveragePointsStaticSlotName, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_string(Stream,
            "#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC\n", !IO),
        CoveragePointsDynamicSlotName =
            layout_slot(proc_static_cp_dynamic_array, CoveragePointsSlot),
        output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
            CoveragePointsDynamicSlotName, !IO)
    ;
        MaybeCoveragePoints = no,
        io.write_string(Stream, "0,NULL,\n", !IO),
        io.write_string(Stream,
            "#ifdef MR_DEEP_PROFILING_COVERAGE_STATIC\n", !IO),
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, "\n#endif\n", !IO),
    io.write_string(Stream, "#endif\n", !IO),
    io.write_string(Stream, " },\n", !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #14: proc head variable numbers.
%

:- pred output_proc_head_var_nums_array(llds_out_info::in,
    io.text_output_stream::in, list(int)::in, io::di, io::uo) is det.

output_proc_head_var_nums_array(Info, Stream, HeadVarNums, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(HeadVarNums, NumHeadVarNums),
    Name = proc_head_var_nums_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {", [i(NumHeadVarNums)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        output_numbers_in_vector_ac(Stream, HeadVarNums, 0, !IO)
    ;
        AutoComments = no_auto_comments,
        output_numbers_in_vector_noac(Stream, HeadVarNums, 0, !IO)
    ),
    io.write_string(Stream, "\n};\n\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #15: proc variable names.
%

:- pred output_proc_var_names_array(llds_out_info::in,
    io.text_output_stream::in,  list(int)::in, io::di, io::uo) is det.

output_proc_var_names_array(Info, Stream, VarNames, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(VarNames, NumVarNames),
    Name = proc_var_names_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {", [i(NumVarNames)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        output_numbers_in_vector_ac(Stream, VarNames, 0, !IO)
    ;
        AutoComments = no_auto_comments,
        output_numbers_in_vector_noac(Stream, VarNames, 0, !IO)
    ),
    io.write_string(Stream, "\n};\n\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #16: proc body bytecodes names.
%

:- pred output_proc_body_bytecodes_array(llds_out_info::in,
    io.text_output_stream::in, list(int)::in, io::di, io::uo) is det.

output_proc_body_bytecodes_array(Info, Stream, Bytecodes, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(Bytecodes, NumBytecodes),
    Name = proc_body_bytecodes_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumBytecodes)], !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        output_numbers_in_vector_ac(Stream, Bytecodes, 0, !IO)
    ;
        AutoComments = no_auto_comments,
        output_numbers_in_vector_noac(Stream, Bytecodes, 0, !IO)
    ),
    io.write_string(Stream, "};\n\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #17: table_io structures.
%

:- pred output_table_io_entry_array(llds_out_info::in,
    io.text_output_stream::in, list(table_io_entry_data)::in,
    io::di, io::uo) is det.

output_table_io_entry_array(Info, Stream, TableIoEntries, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(TableIoEntries, NumTableIoEntries),
    Name = proc_table_io_entry_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumTableIoEntries)], !IO),
    list.foldl2(output_table_io_entry_slot(Info, Stream),
        TableIoEntries, 0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_table_io_entry_slot(llds_out_info::in,
    io.text_output_stream::in, table_io_entry_data::in,
    int::in, int::out, io::di, io::uo) is det.

output_table_io_entry_slot(Info, Stream, TableIoEntry, !Slot, !IO) :-
    TableIoEntry = table_io_entry_data(ProcLayoutName, MaybeArgInfos),
    io.write_string(Stream, "{ ", !IO),
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d */\n  ", [i(!.Slot)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    io.write_string(Stream, "(const MR_ProcLayout *) &", !IO),
    output_layout_name(Stream, ProcLayoutName, !IO),
    io.write_string(Stream, ",\n  ", !IO),
    (
        MaybeArgInfos = no,
        io.write_string(Stream, "MR_FALSE, 0, NULL, NULL", !IO)
    ;
        MaybeArgInfos = yes(ArgInfos),
        ArgInfos = table_io_args_data(NumPTIs, PTIVectorRval, TypeParamsRval),
        io.write_string(Stream, "MR_TRUE,\n  ", !IO),
        io.write_int(Stream, NumPTIs, !IO),
        io.write_string(Stream, ",\n  (const MR_PseudoTypeInfo *) ", !IO),
        output_rval(Info, PTIVectorRval, Stream, !IO),
        io.write_string(Stream, ", (const MR_TypeParamLocns *) ", !IO),
        output_rval(Info, TypeParamsRval, Stream, !IO)
    ),
    io.write_string(Stream, " },\n", !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Definition of array #18: proc event layouts.
%

:- pred output_proc_event_layout_array(llds_out_info::in,
    io.text_output_stream::in, list(layout_slot_name)::in,
    io::di, io::uo) is det.

output_proc_event_layout_array(Info, Stream, ProcEventLayoutSlotNames, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(ProcEventLayoutSlotNames, NumProcEventLayoutSlotNames),
    Name = proc_event_layouts_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumProcEventLayoutSlotNames)], !IO),
    output_layout_slots_in_vector(Stream, ModuleName,
        ProcEventLayoutSlotNames, !IO),
    io.write_string(Stream, "};\n\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #19: execution tracing structures.
%

:- pred output_exec_traces_array(llds_out_info::in,
    io.text_output_stream::in, list(proc_layout_exec_trace)::in,
    io::di, io::uo) is det.

output_exec_traces_array(Info, Stream, ExecTraces, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(ExecTraces, NumExecTraces),
    Name = proc_exec_trace_array,
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumExecTraces)], !IO),
    list.foldl2(output_exec_trace_slot(Info, Stream), ExecTraces, 0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_exec_trace_slot(llds_out_info::in,
    io.text_output_stream::in, proc_layout_exec_trace::in,
    int::in, int::out, io::di, io::uo) is det.

output_exec_trace_slot(Info, Stream, ExecTrace, !Slot, !IO) :-
    ExecTrace = proc_layout_exec_trace(MaybeCallLabelSlotName,
        EventLayoutsSlotName, NumEventLayouts, MaybeTableInfo,
        MaybeHeadVarsSlotName, NumHeadVarNums, MaybeVarNamesSlotName,
        MaxVarNum, MaxRegR, MaxRegF, MaybeFromFullSlot, MaybeIoSeqSlot,
        MaybeTrailSlot, MaybeMaxfrSlot, EvalMethod, MaybeCallTableSlot,
        MaybeTailRecSlot, EffTraceLevel, Flags),
    AutoComments = Info ^ lout_auto_comments,
    io.write_string(Stream, "{ ", !IO),
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d */ ", [i(!.Slot)], !IO)
    ;
        AutoComments = no_auto_comments
    ),

    MangledModuleName = Info ^ lout_mangled_module_name,
    (
        MaybeCallLabelSlotName = yes(CallLabelSlotName),
        io.write_string(Stream, "(MR_LabelLayout *) ", !IO),
        output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
            CallLabelSlotName, !IO),
        io.write_string(Stream, ",\n  ", !IO)
    ;
        MaybeCallLabelSlotName = no,
        io.write_string(Stream, "NULL,\n  ", !IO)
    ),
    io.write_string(Stream, "(const MR_ModuleLayout *) &", !IO),
    ModuleName = Info ^ lout_module_name,
    output_layout_name(Stream, module_layout(ModuleName), !IO),
    io.write_string(Stream, ",\n  ", !IO),
    output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
        EventLayoutsSlotName, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, NumEventLayouts, !IO),
    io.write_string(Stream, ",\n  ", !IO),
    io.write_string(Stream, "{ ", !IO),
    (
        MaybeTableInfo = yes(TableInfo),
        (
            TableInfo = data_or_slot_is_slot(TableSlotName),
            io.write_string(Stream, "(const void *) ", !IO),
            output_layout_slot_addr(Stream, use_layout_macro,
                MangledModuleName, TableSlotName, !IO)
        ;
            TableInfo = data_or_slot_is_data(TableDataId),
            io.write_string(Stream, "(const void *) &", !IO),
            output_data_id(Info, Stream, TableDataId, !IO)
        )
    ;
        MaybeTableInfo = no,
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, " },\n  ", !IO),
    (
        MaybeHeadVarsSlotName = yes(HeadVarNumsSlotName),
        output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
            HeadVarNumsSlotName, !IO)
    ;
        MaybeHeadVarsSlotName = no,
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ",", !IO),
    (
        MaybeVarNamesSlotName = yes(VarNamesSlotName),
        output_layout_slot_addr(Stream, use_layout_macro, MangledModuleName,
            VarNamesSlotName, !IO)
    ;
        MaybeVarNamesSlotName = no,
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ",\n  ", !IO),
    io.write_int(Stream, NumHeadVarNums, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, MaxVarNum, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, MaxRegR, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_int(Stream, MaxRegF, !IO),
    io.write_string(Stream, ",", !IO),
    write_maybe_slot_num(Stream, MaybeFromFullSlot, !IO),
    io.write_string(Stream, ",", !IO),
    write_maybe_slot_num(Stream, MaybeIoSeqSlot, !IO),
    io.write_string(Stream, ",", !IO),
    write_maybe_slot_num(Stream, MaybeTrailSlot, !IO),
    io.write_string(Stream, ",", !IO),
    write_maybe_slot_num(Stream, MaybeMaxfrSlot, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_string(Stream, eval_method_to_c_string(EvalMethod), !IO),
    io.write_string(Stream, ",", !IO),
    write_maybe_slot_num(Stream, MaybeCallTableSlot, !IO),
    io.write_string(Stream, ",", !IO),
    io.write_string(Stream, eff_trace_level_rep(EffTraceLevel), !IO),
    io.write_string(Stream, ",\n  ", !IO),
    io.write_int(Stream, Flags, !IO),
    io.write_string(Stream, ",", !IO),
    write_maybe_slot_num(Stream, MaybeTailRecSlot, !IO),
    io.write_string(Stream, " },\n", !IO).

:- pred write_maybe_slot_num(io.text_output_stream::in, maybe(int)::in,
    io::di, io::uo) is det.

write_maybe_slot_num(Stream, yes(SlotNum), !IO) :-
    io.write_int(Stream, SlotNum, !IO).
write_maybe_slot_num(Stream, no, !IO) :-
    io.write_int(Stream, -1, !IO).

:- func eval_method_to_c_string(eval_method) = string.

eval_method_to_c_string(EvalMethod) = Str :-
    (
        EvalMethod = eval_normal,
        Str = "MR_EVAL_METHOD_NORMAL"
    ;
        EvalMethod = eval_tabled(TabledMethod),
        (
            TabledMethod = tabled_loop_check,
            Str = "MR_EVAL_METHOD_LOOP_CHECK"
        ;
            TabledMethod = tabled_memo(_),
            Str = "MR_EVAL_METHOD_MEMO"
        ;
            TabledMethod = tabled_minimal(MinimalMethod),
            (
                MinimalMethod = stack_copy,
                Str = "MR_EVAL_METHOD_MINIMAL_STACK_COPY"
            ;
                MinimalMethod = own_stacks_consumer,
                Str = "MR_EVAL_METHOD_MINIMAL_OWN_STACKS_CONSUMER"
            ;
                MinimalMethod = own_stacks_generator,
                Str = "MR_EVAL_METHOD_MINIMAL_OWN_STACKS_GENERATOR"
            )
        ;
            TabledMethod = tabled_io(EntryKind, Unitize),
            (
                ( EntryKind = entry_stores_outputs
                ; EntryKind = entry_stores_procid_outputs
                ),
                Unitize = table_io_alone,
                Str = "MR_EVAL_METHOD_TABLE_IO"
            ;
                ( EntryKind = entry_stores_outputs
                ; EntryKind = entry_stores_procid_outputs
                ),
                Unitize = table_io_unitize,
                Str = "MR_EVAL_METHOD_TABLE_IO_UNITIZE"
            ;
                EntryKind = entry_stores_procid_inputs_outputs,
                Unitize = table_io_alone,
                Str = "MR_EVAL_METHOD_TABLE_IO_DECL"
            ;
                EntryKind = entry_stores_procid_inputs_outputs,
                Unitize = table_io_unitize,
                Str = "MR_EVAL_METHOD_TABLE_IO_UNITIZE_DECL"
            )
        )
    ).

%-----------------------------------------------------------------------------%
%
% Definition of array #20: threadscope string table.
%

:- pred output_threadscope_string_table_array(llds_out_info::in,
    io.text_output_stream::in, list(string)::in, io::di, io::uo) is det.

output_threadscope_string_table_array(Info, Stream, TSStringTable, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    list.length(TSStringTable, NumStrings),
    Name = threadscope_string_table_array,
    io.write_string(Stream, "#ifdef MR_THREADSCOPE\n", !IO),
    output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        being_defined, !IO),
    io.format(Stream, "[%d] = {\n", [i(NumStrings)], !IO),
    list.foldl2(output_threadscope_string_table_slot(Info, Stream),
        TSStringTable, 0, _, !IO),
    io.write_string(Stream, "};\n#endif\n\n", !IO).

:- pred output_threadscope_string_table_slot(llds_out_info::in,
    io.text_output_stream::in, string::in,
    int::in, int::out, io::di, io::uo) is det.

output_threadscope_string_table_slot(Info, Stream, String, !Slot, !IO) :-
    AutoComments = Info ^ lout_auto_comments,
    (
        AutoComments = auto_comments,
        io.format(Stream, "/* %d */ ", [i(!.Slot)], !IO)
    ;
        AutoComments = no_auto_comments
    ),
    io.write_string(Stream, "{ ", !IO),
    quote_and_write_string(Stream, String, !IO),
    io.write_string(Stream, ", 0},\n", !IO).

%-----------------------------------------------------------------------------%
%
% Definition of array #21: allocation site structures.
%

:- pred output_alloc_sites_array(llds_out_info::in, io.text_output_stream::in,
    list(alloc_site_info)::in, io::di, io::uo) is det.

output_alloc_sites_array(Info, Stream, AllocSites, !IO) :-
    ModuleName = Info ^ lout_mangled_module_name,
    output_layout_array_name_storage_type_name(Stream, ModuleName,
        alloc_site_array, being_defined, !IO),
    list.length(AllocSites, NumAllocSitess),
    io.format(Stream, "[%d] = {\n", [i(NumAllocSitess)], !IO),
    list.foldl2(output_alloc_site_slot(Info, Stream), AllocSites, 0, _, !IO),
    io.write_string(Stream, "};\n\n", !IO).

:- pred output_alloc_site_slot(llds_out_info::in, io.text_output_stream::in,
    alloc_site_info::in, int::in, int::out, io::di, io::uo) is det.

output_alloc_site_slot(_Info, Stream, AllocSite, !Slot, !IO) :-
    AllocSite = alloc_site_info(ProcLabel, Context, TypeMsg, Words),
    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    io.write_string(Stream, "\t{ ", !IO),
    io.write_string(Stream,
        proc_label_to_c_string(add_label_prefix, ProcLabel), !IO),
    io.write_string(Stream, ", ", !IO),
    quote_and_write_string(Stream, FileName, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, LineNumber, !IO),
    io.write_string(Stream, ", ", !IO),
    quote_and_write_string(Stream, TypeMsg, !IO),
    io.write_string(Stream, ", ", !IO),
    io.write_int(Stream, Words, !IO),
    io.write_string(Stream, "},\n", !IO).

%-----------------------------------------------------------------------------%

output_layout_name_decl(Stream, LayoutName, !IO) :-
    output_layout_name_storage_type_name(Stream, LayoutName,
        not_being_defined, !IO),
    io.write_string(Stream, ";\n", !IO).

output_maybe_layout_name_decl(Stream, LayoutName, !DeclSet, !IO) :-
    ( if decl_set_is_member(decl_layout_id(LayoutName), !.DeclSet) then
        true
    else
        output_layout_name_decl(Stream, LayoutName, !IO),
        decl_set_insert(decl_layout_id(LayoutName), !DeclSet)
    ).

:- pred output_layout_decl(io.text_output_stream::in, layout_name::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_layout_decl(Stream, LayoutName, !DeclSet, !IO) :-
    ( if decl_set_is_member(decl_layout_id(LayoutName), !.DeclSet) then
        true
    else
        output_layout_name_storage_type_name(Stream, LayoutName,
            not_being_defined, !IO),
        io.write_string(Stream, ";\n", !IO),
        decl_set_insert(decl_layout_id(LayoutName), !DeclSet)
    ).

output_layout_array_name(Stream, UseMacro, ModuleName, ArrayName, !IO) :-
    (
        UseMacro = use_layout_macro,
        (
            ArrayName = label_layout_array(label_has_no_var_info),
            Macro = "MR_no_var_label_layouts"
        ;
            ArrayName = label_layout_array(label_has_short_var_info),
            Macro = "MR_svar_label_layouts"
        ;
            ArrayName = label_layout_array(label_has_long_var_info),
            Macro = "MR_lvar_label_layouts"
        ;
            ArrayName = pseudo_type_info_array,
            Macro = "MR_pseudo_type_infos"
        ;
            ArrayName = long_locns_array,
            Macro = "MR_long_locns"
        ;
            ArrayName = short_locns_array,
            Macro = "MR_short_locns"
        ;
            ArrayName = hlds_var_nums_array,
            Macro = "MR_hlds_var_nums"
        ;
            ArrayName = user_event_var_nums_array,
            Macro = "MR_user_event_var_nums"
        ;
            ArrayName = user_event_layout_array,
            Macro = "MR_user_event_layouts"
        ;
            ArrayName = proc_static_call_sites_array,
            Macro = "MR_proc_call_sites"
        ;
            ArrayName = proc_static_cp_static_array,
            Macro = "MR_proc_cp_statics"
        ;
            ArrayName = proc_static_cp_dynamic_array,
            Macro = "MR_proc_cp_dynamics"
        ;
            ArrayName = proc_static_array,
            Macro = "MR_proc_statics"
        ;
            ArrayName = proc_head_var_nums_array,
            Macro = "MR_proc_head_var_nums"
        ;
            ArrayName = proc_var_names_array,
            Macro = "MR_proc_var_names"
        ;
            ArrayName = proc_body_bytecodes_array,
            Macro = "MR_proc_body_bytecodes"
        ;
            ArrayName = proc_table_io_entry_array,
            Macro = "MR_proc_table_io_entries"
        ;
            ArrayName = proc_event_layouts_array,
            Macro = "MR_proc_event_layouts"
        ;
            ArrayName = proc_exec_trace_array,
            Macro = "MR_proc_exec_traces"
        ;
            ArrayName = threadscope_string_table_array,
            Macro = "MR_threadscope_strings"
        ;
            ArrayName = alloc_site_array,
            Macro = "MR_alloc_sites"
        ),
        io.format(Stream, "%s(%s)", [s(Macro), s(ModuleName)], !IO)
    ;
        UseMacro = do_not_use_layout_macro,
        (
            ArrayName = label_layout_array(label_has_no_var_info),
            Prefix = "mercury_data__no_var_label_layout_array__"
        ;
            ArrayName = label_layout_array(label_has_short_var_info),
            Prefix = "mercury_data__svar_label_layout_array__"
        ;
            ArrayName = label_layout_array(label_has_long_var_info),
            Prefix = "mercury_data__lvar_label_layout_array__"
        ;
            ArrayName = pseudo_type_info_array,
            Prefix = "mercury_data__pseudo_type_info_array__"
        ;
            ArrayName = long_locns_array,
            Prefix = "mercury_data__long_locns_array__"
        ;
            ArrayName = short_locns_array,
            Prefix = "mercury_data__short_locns_array__"
        ;
            ArrayName = hlds_var_nums_array,
            Prefix = "mercury_data__hlds_var_nums_array__"
        ;
            ArrayName = user_event_var_nums_array,
            Prefix = "mercury_data__user_event_var_nums_array__"
        ;
            ArrayName = user_event_layout_array,
            Prefix = "mercury_data__user_event_layouts_array__"
        ;
            ArrayName = proc_static_call_sites_array,
            Prefix = "mercury_data__proc_call_sites_array__"
        ;
            ArrayName = proc_static_cp_static_array,
            Prefix = "mercury_data__proc_cp_statics_array__"
        ;
            ArrayName = proc_static_cp_dynamic_array,
            Prefix = "mercury_data__proc_cp_dynamics_array__"
        ;
            ArrayName = proc_static_array,
            Prefix = "mercury_data__proc_statics_array__"
        ;
            ArrayName = proc_head_var_nums_array,
            Prefix = "mercury_data__proc_head_var_nums_array__"
        ;
            ArrayName = proc_var_names_array,
            Prefix = "mercury_data__proc_var_names_array__"
        ;
            ArrayName = proc_body_bytecodes_array,
            Prefix = "mercury_data__proc_body_bytecodes_array__"
        ;
            ArrayName = proc_table_io_entry_array,
            Prefix = "mercury_data__proc_table_io_entries_array__"
        ;
            ArrayName = proc_event_layouts_array,
            Prefix = "mercury_data__proc_event_layouts_array__"
        ;
            ArrayName = proc_exec_trace_array,
            Prefix = "mercury_data__proc_exec_traces_array__"
        ;
            ArrayName = threadscope_string_table_array,
            Prefix = "mercury_data__threadscope_string_table_array__"
        ;
            ArrayName = alloc_site_array,
            Prefix = "mercury_data__alloc_sites_array__"
        ),
        io.format(Stream, "%s%s", [s(Prefix), s(ModuleName)], !IO)
    ).

output_layout_slot_id(Stream, UseMacro, ModuleName, SlotName, !IO) :-
    SlotName = layout_slot(ArrayName, SlotNum),
    output_layout_array_name(Stream, UseMacro, ModuleName, ArrayName, !IO),
    io.format(Stream, "[%d]", [i(SlotNum)], !IO).

output_layout_slot_addr(Stream, UseMacro, ModuleName, SlotName, !IO) :-
    SlotName = layout_slot(ArrayName, SlotNum),
    io.write_string(Stream, "&", !IO),
    output_layout_array_name(Stream, UseMacro, ModuleName, ArrayName, !IO),
    io.format(Stream, "[%d]", [i(SlotNum)], !IO).

output_layout_name(Stream, Name, !IO) :-
    (
        Name = proc_layout(RttiProcLabel, _),
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        ProcLabelStr = proc_label_to_c_string(add_label_prefix, ProcLabel),
        % We can't omit the mercury_ prefix on ProcLabel, even though the
        % mercury_data_prefix duplicates it, because there is no simple way
        % to make the MR_init_entryl_sl macro delete that prefix from the
        % entry label's name to get the name of its layout structure.
        io.format(Stream, "%s_proc_layout__%s",
            [s(mercury_data_prefix), s(ProcLabelStr)], !IO)
    ;
        Name = closure_proc_id(CallerProcLabel, SeqNo, _),
        CallerProcLabelStr =
            proc_label_to_c_string(do_not_add_label_prefix, CallerProcLabel),
        io.format(Stream, "%s_closure_layout__%s_%d",
            [s(mercury_data_prefix), s(CallerProcLabelStr), i(SeqNo)], !IO)
    ;
        Name = file_layout(ModuleName, FileNum),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_file_layout__%s_%d",
            [s(mercury_data_prefix), s(ModuleNameStr), i(FileNum)], !IO)
    ;
        Name = file_layout_line_number_vector(ModuleName, FileNum),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_file_lines__%s_%d",
            [s(mercury_data_prefix), s(ModuleNameStr), i(FileNum)], !IO)
    ;
        Name = file_layout_label_layout_vector(ModuleName, FileNum),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_file_label_layouts__%s_%d",
            [s(mercury_data_prefix), s(ModuleNameStr), i(FileNum)], !IO)
    ;
        Name = module_layout_string_table(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_strings__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout_file_vector(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_files__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout_proc_vector(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_procs__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout_label_exec_count(ModuleName, _),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_label_exec_counts__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout_event_set_desc(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_event_set_desc__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout_event_arg_names(ModuleName, EventNumber),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_event_arg_names__%s_%d",
            [s(mercury_data_prefix), s(ModuleNameStr), i(EventNumber)], !IO)
    ;
        Name = module_layout_event_synth_attrs(ModuleName, EventNumber),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_event_synth_attrs__%s_%d",
            [s(mercury_data_prefix), s(ModuleNameStr), i(EventNumber)], !IO)
    ;
        Name = module_layout_event_synth_attr_args(ModuleName,
            EventNumber, SynthCallArgNumber),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_event_synth_attr_args__%s_%d_%d",
            [s(mercury_data_prefix), s(ModuleNameStr),
            i(EventNumber), i(SynthCallArgNumber)], !IO)
    ;
        Name = module_layout_event_synth_attr_order(ModuleName,
            EventNumber, SynthCallArgNumber),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_event_synth_attr_order__%s_%d_%d",
            [s(mercury_data_prefix), s(ModuleNameStr),
            i(EventNumber), i(SynthCallArgNumber)], !IO)
    ;
        Name = module_layout_event_synth_order(ModuleName, EventNumber),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_event_synth_order__%s_%d",
            [s(mercury_data_prefix), s(ModuleNameStr), i(EventNumber)], !IO)
    ;
        Name = module_layout_event_specs(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_event_specs__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout_oisu_bytes(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_oisu_bytes__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout_type_table_bytes(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout_type_table_bytes__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ;
        Name = module_layout(ModuleName),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.format(Stream, "%s_module_layout__%s",
            [s(mercury_data_prefix), s(ModuleNameStr)], !IO)
    ).

output_layout_name_storage_type_name(Stream, Name, BeingDefined, !IO) :-
    (
        Name = proc_layout(RttiProcLabel, Kind),
        ProcIsImported = RttiProcLabel ^ rpl_proc_is_imported,
        ProcIsExported = RttiProcLabel ^ rpl_proc_is_exported,
        ( if
            ProcIsImported = no,
            ProcIsExported = no
        then
            io.write_string(Stream, "static ", !IO)
        else
            (
                BeingDefined = being_defined
            ;
                BeingDefined = not_being_defined,
                io.write_string(Stream, "extern ", !IO)
            )
        ),
        io.write_string(Stream, "const ", !IO),
        io.write_string(Stream, proc_layout_kind_to_type(Kind), !IO),
        io.write_string(Stream, " ", !IO),
        output_layout_name(Stream, Name, !IO)
    ;
        Name = closure_proc_id(_CallerProcLabel, _SeqNo, ClosureProcLabel),
        io.write_string(Stream, "static const ", !IO),
        (
            ClosureProcLabel = ordinary_proc_label(_, _, _, _, _, _),
            io.write_string(Stream, "MR_UserClosureId\n", !IO)
        ;
            ClosureProcLabel = special_proc_label(_, _, _, _, _, _),
            io.write_string(Stream, "MR_UCIClosureId\n", !IO)
        ),
        output_layout_name(Stream, Name, !IO)
    ;
        Name = file_layout(_ModuleName, _FileNum),
        io.write_string(Stream, "static const MR_ModuleFileLayout ", !IO),
        output_layout_name(Stream, Name, !IO)
    ;
        Name = file_layout_line_number_vector(_ModuleName, _FileNum),
        io.write_string(Stream, "static const MR_int_least16_t ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = file_layout_label_layout_vector(_ModuleName, _FileNum),
        io.write_string(Stream, "static const MR_LabelLayout *", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_string_table(_ModuleName),
        io.write_string(Stream, "static const char ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_file_vector(_ModuleName),
        io.write_string(Stream, "static const MR_ModuleFileLayout *", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_label_exec_count(_ModuleName, NumElements),
        io.write_string(Stream, "static MR_Unsigned ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[", !IO),
        io.write_int(Stream, NumElements, !IO),
        io.write_string(Stream, "]", !IO)
    ;
        Name = module_layout_proc_vector(_ModuleName),
        io.write_string(Stream, "static const MR_ProcLayout *", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_event_set_desc(_ModuleName),
        io.write_string(Stream, "static const char ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_event_arg_names(_ModuleName, _EventNumber),
        io.write_string(Stream, "static const char * ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_event_synth_attrs(_ModuleName, _EventNumber),
        io.write_string(Stream, "static MR_SynthAttr ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_event_synth_attr_args(_ModuleName,
            _EventNumber, _SynthCallArgNumber),
        io.write_string(Stream, "static MR_uint_least16_t ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_event_synth_attr_order(_ModuleName,
            _EventNumber, _SynthCallArgNumber),
        io.write_string(Stream, "static MR_int_least16_t ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_event_synth_order(_ModuleName, _EventNumber),
        io.write_string(Stream, "static MR_int_least16_t ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_event_specs(_ModuleName),
        io.write_string(Stream, "static MR_UserEventSpec ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_oisu_bytes(_ModuleName),
        io.write_string(Stream, "static const MR_uint_least8_t ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout_type_table_bytes(_ModuleName),
        io.write_string(Stream, "static const MR_uint_least8_t ", !IO),
        output_layout_name(Stream, Name, !IO),
        io.write_string(Stream, "[]", !IO)
    ;
        Name = module_layout(_ModuleName),
        io.write_string(Stream, "static const MR_ModuleLayout ", !IO),
        output_layout_name(Stream, Name, !IO)
    ).

output_layout_array_name_storage_type_name(Stream, ModuleName, Name,
        BeingDefined, !IO) :-
    (
        BeingDefined = being_defined,
        io.write_string(Stream, "static ", !IO)
    ;
        % Avoid problems with MS Visual C.
        % See the comments in llds_out_file.output_static_linkage_define/2
        % for a further explanation.
        BeingDefined = not_being_defined,
        io.write_string(Stream, "MR_STATIC_LINKAGE ", !IO)
    ),
    (
        Name = label_layout_array(label_has_no_var_info),
        io.write_string(Stream, "const MR_LabelLayoutNoVarInfo ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = label_layout_array(label_has_short_var_info),
        io.write_string(Stream, "const MR_LabelLayoutShort ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = label_layout_array(label_has_long_var_info),
        io.write_string(Stream, "const MR_LabelLayout ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = pseudo_type_info_array,
        io.write_string(Stream, "const MR_PseudoTypeInfo ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = long_locns_array,
        io.write_string(Stream, "const MR_LongLval ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = short_locns_array,
        io.write_string(Stream, "const MR_ShortLval ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = hlds_var_nums_array,
        io.write_string(Stream, "const MR_HLDSVarNum ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = user_event_var_nums_array,
        io.write_string(Stream, "const MR_HLDSVarNum ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = user_event_layout_array,
        io.write_string(Stream, "const struct MR_UserEvent_Struct ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_static_call_sites_array,
        io.write_string(Stream, "const MR_CallSiteStatic ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_static_cp_static_array,
        io.write_string(Stream, "const MR_CoveragePointStatic ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_static_cp_dynamic_array,
        io.write_string(Stream, "MR_Unsigned ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_static_array,
        io.write_string(Stream, "MR_ProcStatic ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_head_var_nums_array,
        io.write_string(Stream, "const MR_uint_least16_t ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_var_names_array,
        io.write_string(Stream, "const MR_uint_least32_t ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_body_bytecodes_array,
        io.write_string(Stream, "const MR_uint_least8_t ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_table_io_entry_array,
        io.write_string(Stream, "const MR_TableIoEntry ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_event_layouts_array,
        io.write_string(Stream, "const MR_LabelLayout *", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = proc_exec_trace_array,
        io.write_string(Stream, "MR_STATIC_CODE_CONST MR_ExecTrace ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = threadscope_string_table_array,
        io.write_string(Stream, "MR_Threadscope_String ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ;
        Name = alloc_site_array,
        % The type field may be updated at runtime so this array is not const.
        io.write_string(Stream, "MR_AllocSiteInfo ", !IO),
        output_layout_array_name(Stream, do_not_use_layout_macro, ModuleName,
            Name, !IO)
    ).

%-----------------------------------------------------------------------------%

    % Output the rval in a context in which it is immediately cast to an
    % address.
    %
:- pred output_rval_as_addr(llds_out_info::in, io.text_output_stream::in,
    rval::in, io::di, io::uo) is det.

output_rval_as_addr(Info, Stream, Rval, !IO) :-
    ( if Rval = const(llconst_int(0)) then
        io.write_string(Stream, "0", !IO)
    else if Rval = const(llconst_data_addr(DataId, no)) then
        output_data_id_addr(Info, Stream, DataId, !IO)
    else
        io.write_string(Stream, "\n", !IO),
        output_rval(Info, Rval, Stream, !IO)
    ).

%-----------------------------------------------------------------------------%

output_proc_layout_data_defn(Info, Stream, ProcLayoutData, !DeclSet, !IO) :-
    ProcLayoutData = proc_layout_data(RttiProcLabel, Traversal, MaybeRest),
    ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
    Traversal = proc_layout_stack_traversal(MaybeEntryLabel, MaybeSuccipSlot,
        StackSlotCount, Detism),

    % Output the declarations needed by this definition.
    (
        MaybeEntryLabel = yes(EntryLabelDecl),
        output_record_code_addr_decls(Info, Stream, code_label(EntryLabelDecl),
            !DeclSet, !IO)
    ;
        MaybeEntryLabel = no
    ),
    (
        MaybeRest = no_proc_id_and_more,
        Kind = proc_layout_traversal
    ;
        MaybeRest = proc_id_and_more(_, _, _, ModuleLayoutDecl),
        Kind = proc_layout_proc_id(proc_label_user_or_uci(ProcLabel)),
        output_layout_decl(Stream, ModuleLayoutDecl, !DeclSet, !IO)
    ),

    io.write_string(Stream, "\n", !IO),
    ProcLayoutName = proc_layout(RttiProcLabel, Kind),
    output_layout_name_storage_type_name(Stream, ProcLayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),

    % Write out the traversal structure.
    io.write_string(Stream, "{\n", !IO),
    (
        MaybeEntryLabel = yes(EntryLabel),
        output_code_addr(Stream, code_label(EntryLabel), !IO)
    ;
        MaybeEntryLabel = no,
        % The actual code address will be put into the structure
        % by module initialization code.
        io.write_string(Stream, "NULL", !IO)
    ),
    io.write_string(Stream, ", ", !IO),
    (
        MaybeSuccipSlot = yes(SuccipSlot),
        io.write_int(Stream, SuccipSlot, !IO)
    ;
        MaybeSuccipSlot = no,
        io.write_int(Stream, -1, !IO)
    ),
    io.write_string(Stream, ",\n", !IO),
    io.write_int(Stream, StackSlotCount, !IO),
    io.write_string(Stream, ",\n", !IO),
    io.write_string(Stream, detism_to_c_detism(Detism), !IO),
    io.write_string(Stream, "\n},\n", !IO),

    (
        MaybeRest = no_proc_id_and_more,
        io.write_string(Stream, "-1\n", !IO)
    ;
        MaybeRest = proc_id_and_more(MaybeProcStatic, MaybeExecTrace,
            MaybeProcBodyBytes, ModuleLayout),

        % Output the proc_id structure.
        io.write_string(Stream, "{\n", !IO),
        Origin = RttiProcLabel ^ rpl_pred_info_origin,
        output_proc_id(Stream, ProcLabel, Origin, !IO),
        io.write_string(Stream, "},\n", !IO),

        MangledModuleName = Info ^ lout_mangled_module_name,
        (
            MaybeExecTrace = no,
            io.write_string(Stream, "NULL,\n", !IO)
        ;
            MaybeExecTrace = yes(ExecTraceSlotName),
            output_layout_slot_addr(Stream, use_layout_macro,
                MangledModuleName, ExecTraceSlotName, !IO),
            io.write_string(Stream, ",\n", !IO)
        ),
        (
            MaybeProcStatic = no,
            io.write_string(Stream, "NULL,\n", !IO)
        ;
            MaybeProcStatic = yes(ProcStaticSlotName),
            output_layout_slot_addr(Stream, use_layout_macro,
                MangledModuleName, ProcStaticSlotName, !IO),
            io.write_string(Stream, ",\n", !IO)
        ),
        (
            MaybeProcBodyBytes = no,
            io.write_string(Stream, "NULL,\n", !IO)
        ;
            MaybeProcBodyBytes = yes(ProcBodyBytesSlotName),
            output_layout_slot_addr(Stream, use_layout_macro,
                MangledModuleName, ProcBodyBytesSlotName, !IO),
            io.write_string(Stream, ",\n", !IO)
        ),
        io.write_string(Stream, "&", !IO),
        output_layout_name(Stream, ModuleLayout, !IO)
    ),
    io.write_string(Stream, "\n};\n", !IO),
    DeclId = decl_layout_id(ProcLayoutName),
    decl_set_insert(DeclId, !DeclSet).

%-----------------------------------------------------------------------------%

    % The job of this predicate is to minimize stack space consumption in
    % grades that do not allow output_bytecodes to be tail recursive.
    %
:- pred output_bytecodes_driver(io.text_output_stream::in, list(int)::in,
    io::di, io::uo) is det.

output_bytecodes_driver(Stream, Bytes, !IO) :-
    (
        Bytes = []
    ;
        Bytes = [_ | _],
        output_bytecodes(Stream, Bytes, BytesLeft, 0, 256, !IO),
        output_bytecodes_driver(Stream, BytesLeft, !IO)
    ).

:- pred output_bytecodes(io.text_output_stream::in,
    list(int)::in, list(int)::out, int::in, int::in, io::di, io::uo) is det.

output_bytecodes(Stream, Bytes, BytesLeft, !.Seq, MaxSeq, !IO) :-
    (
        Bytes = [],
        BytesLeft = []
    ;
        Bytes = [Head | Tail],
        ( if !.Seq < MaxSeq then
            io.write_int(Stream, Head, !IO),
            io.write_char(Stream, ',', !IO),
            !:Seq = !.Seq + 1,
            ( if unchecked_rem(!.Seq, 16) = 0 then
                io.write_char(Stream, '\n', !IO)
            else
                true
            ),
            output_bytecodes(Stream, Tail, BytesLeft, !.Seq, MaxSeq, !IO)
        else
            BytesLeft = Bytes
        )
    ).

%-----------------------------------------------------------------------------%

output_closure_layout_data_defn(_Info, Stream, ClosureData, !DeclSet, !IO) :-
    ClosureData = closure_proc_id_data(CallerProcLabel, SeqNo,
        ClosureProcLabel, ModuleName, FileName, LineNumber, PredOrigin,
        GoalPath),
    io.write_string(Stream, "\n", !IO),
    LayoutName = closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel),
    output_layout_name_storage_type_name(Stream, LayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n{\n", !IO),
    output_proc_id(Stream, ClosureProcLabel, PredOrigin, !IO),
    io.write_string(Stream, "},\n", !IO),
    quote_and_write_string(Stream, sym_name_to_string(ModuleName), !IO),
    io.write_string(Stream, ",\n", !IO),
    quote_and_write_string(Stream, FileName, !IO),
    io.write_string(Stream, ",\n", !IO),
    io.write_int(Stream, LineNumber, !IO),
    io.write_string(Stream, ",\n", !IO),
    quote_and_write_string(Stream, GoalPath, !IO),
    io.write_string(Stream, "\n};\n", !IO),
    decl_set_insert(decl_layout_id(LayoutName), !DeclSet).

:- pred output_proc_id(io.text_output_stream::in,
    proc_label::in, pred_origin::in, io::di, io::uo) is det.

output_proc_id(Stream, ProcLabel, Origin, !IO) :-
    (
        ProcLabel = ordinary_proc_label(DefiningModule, PredOrFunc,
            DeclaringModule, PredName0, Arity, ModeNum),
        PredName = layout_origin_name(Origin, PredName0),
        io.write_string(Stream, mr_pred_or_func_to_string(PredOrFunc), !IO),
        io.write_string(Stream, ",\n", !IO),
        quote_and_write_string(Stream,
            sym_name_to_string(DeclaringModule), !IO),
        io.write_string(Stream, ",\n", !IO),
        quote_and_write_string(Stream,
            sym_name_to_string(DefiningModule), !IO),
        io.write_string(Stream, ",\n", !IO),
        quote_and_write_string(Stream, PredName, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_int(Stream, Arity, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_int(Stream, ModeNum, !IO),
        io.write_string(Stream, "\n", !IO)
    ;
        ProcLabel = special_proc_label(DefiningModule, SpecialPredId,
            TypeModule, TypeName, TypeArity, ModeNum),
        TypeCtor = type_ctor(qualified(TypeModule, TypeName), TypeArity),
        PredName0 = uci_pred_name(SpecialPredId, TypeCtor),
        PredName = layout_origin_name(Origin, PredName0),
        quote_and_write_string(Stream, TypeName, !IO),
        io.write_string(Stream, ",\n", !IO),
        quote_and_write_string(Stream, sym_name_to_string(TypeModule), !IO),
        io.write_string(Stream, ",\n", !IO),
        quote_and_write_string(Stream,
            sym_name_to_string(DefiningModule), !IO),
        io.write_string(Stream, ",\n", !IO),
        quote_and_write_string(Stream, PredName, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_int(Stream, TypeArity, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_int(Stream, ModeNum, !IO),
        io.write_string(Stream, "\n", !IO)
    ).

%-----------------------------------------------------------------------------%

    % The version of the layout data structures -- useful for bootstrapping.
    % If you write runtime code that checks this version number and can
    % at least handle the previous version of the data structure,
    % it makes it easier to bootstrap changes to these data structures.
    %
    % This number should be kept in sync with MR_LAYOUT_VERSION in
    % runtime/mercury_stack_layout.h. This means you need to update
    % the code in the runtime (including the trace directory) that uses
    % layout structures to conform to whatever changes the new version
    % introduces.
    %
:- func layout_version_number = int.

layout_version_number = 5.

output_module_layout_data_defn(Info, Stream, Data, !DeclSet, !IO) :-
    Data = module_layout_data(ModuleName, StringTableSize, StringTable,
        MaybeDeepProfData, MaybeDebugData),
    output_module_string_table(Stream, ModuleName,
        StringTableSize, StringTable, !DeclSet, !IO),
    (
        MaybeDeepProfData = yes(DeepProfData),
        DeepProfData = module_layout_deep_prof(NumOISUTypesA, OISUBytes,
            NumTypesA, TypeTableBytes),
        ( if NumOISUTypesA = 0 then
            MaybeOISUBytesLayoutNameA = no
        else
            OISUBytesLayoutNameA = module_layout_oisu_bytes(ModuleName),
            io.write_string(Stream, "\n", !IO),
            output_layout_name_storage_type_name(Stream, OISUBytesLayoutNameA,
                being_defined, !IO),
            io.write_string(Stream, " = {", !IO),
            output_bytecodes_driver(Stream, OISUBytes, !IO),
            io.write_string(Stream, "};\n", !IO),
            decl_set_insert(decl_layout_id(OISUBytesLayoutNameA), !DeclSet),
            MaybeOISUBytesLayoutNameA = yes(OISUBytesLayoutNameA)
        ),
        (
            TypeTableBytes = [],
            MaybeTypeTableLayoutNameA = no
        ;
            TypeTableBytes = [_ | _],
            TypeTableLayoutNameA = module_layout_type_table_bytes(ModuleName),
            io.write_string(Stream, "\n", !IO),
            output_layout_name_storage_type_name(Stream, TypeTableLayoutNameA,
                being_defined, !IO),
            io.write_string(Stream, " = {", !IO),
            output_bytecodes_driver(Stream, TypeTableBytes, !IO),
            io.write_string(Stream, "};\n", !IO),
            decl_set_insert(decl_layout_id(TypeTableLayoutNameA), !DeclSet),
            MaybeTypeTableLayoutNameA = yes(TypeTableLayoutNameA)
        ),
        OISUInfo = {NumOISUTypesA, MaybeOISUBytesLayoutNameA,
            NumTypesA, MaybeTypeTableLayoutNameA}
    ;
        MaybeDeepProfData = no,
        OISUInfo = {0, no, 0, no}
    ),

    (
        MaybeDebugData = yes(DebugData),
        DebugData = module_layout_debug(ProcLayoutNames, FileLayouts,
            TraceLevelA, SuppressedEventsA, NumLabelsA, MaybeEventSetA),

        output_module_layout_proc_vector_defn(Stream, ModuleName,
            ProcLayoutNames, ProcLayoutVectorNameA, !DeclSet, !IO),
        list.length(ProcLayoutNames, ProcLayoutVectorLengthA),
        output_file_layout_data_defns(Info, Stream, ModuleName,
            0, FileLayouts, FileLayoutNames, !DeclSet, !IO),
        list.length(FileLayouts, FileLayoutVectorLengthA),
        output_file_layout_vector_data_defn(Stream, ModuleName, FileLayoutNames,
            FileLayoutVectorNameA, !DeclSet, !IO),

        io.write_string(Stream, "\n", !IO),
        LabelExecCountNameA = module_layout_label_exec_count(ModuleName,
            NumLabelsA),
        output_layout_name_storage_type_name(Stream, LabelExecCountNameA,
            being_defined, !IO),
        io.write_string(Stream, ";\n", !IO),
        decl_set_insert(decl_layout_id(LabelExecCountNameA), !DeclSet),

        (
            MaybeEventSetA = no,
            MaybeEventInfoA = no
        ;
            MaybeEventSetA = yes(EventSetDataLayout),
            EventSetDataLayout =
                event_set_layout_data(EventSetDataA, TypesRvalMap),
            EventSetDataA = event_set_data(EventSetNameA, EventSetDesc,
                EventSpecs, MaxNumAttrA),
            output_event_set_desc_defn(Stream, ModuleName, EventSetDesc,
                EventSetDescLayoutNameA, !DeclSet, !IO),
            output_event_specs_and_components(Info, Stream,
                EventSpecs, ModuleName, TypesRvalMap, EventSpecsLayoutNameA,
                !DeclSet, !IO),
            list.length(EventSpecs, NumEventSpecsA),
            MaybeEventInfoA = yes({EventSetNameA, MaxNumAttrA, NumEventSpecsA,
                EventSetDescLayoutNameA, EventSpecsLayoutNameA})
        ),

        MaybeDebugInfo = yes({ProcLayoutVectorLengthA, ProcLayoutVectorNameA,
            FileLayoutVectorLengthA, FileLayoutVectorNameA,
            TraceLevelA, SuppressedEventsA, NumLabelsA, LabelExecCountNameA,
            MaybeEventInfoA})
    ;
        MaybeDebugData = no,
        MaybeDebugInfo = no
    ),

    ModuleLayoutName = module_layout(ModuleName),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, ModuleLayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),
    io.write_int(Stream, layout_version_number, !IO),
    io.write_string(Stream, ",\n", !IO),
    quote_and_write_string(Stream, sym_name_to_string(ModuleName), !IO),
    io.write_string(Stream, ",\n", !IO),
    io.write_int(Stream, StringTableSize, !IO),
    io.write_string(Stream, ",\n", !IO),
    ModuleStringTableName = module_layout_string_table(ModuleName),
    output_layout_name(Stream, ModuleStringTableName, !IO),
    io.write_string(Stream, ",\n", !IO),

    OISUInfo = {NumOISUTypesB, MaybeOISUBytesLayoutNameB,
        NumTypesB, MaybeTypeTableLayoutNameB},
    io.write_int(Stream, NumOISUTypesB, !IO),
    io.write_string(Stream, ",\n", !IO),
    (
        MaybeOISUBytesLayoutNameB = yes(OISUBytesLayoutNameB),
        output_layout_name(Stream, OISUBytesLayoutNameB, !IO),
        io.write_string(Stream, ",\n", !IO)
    ;
        MaybeOISUBytesLayoutNameB = no,
        io.write_string(Stream, "NULL,\n", !IO)
    ),
    io.write_int(Stream, NumTypesB, !IO),
    io.write_string(Stream, ",\n", !IO),
    (
        MaybeTypeTableLayoutNameB = yes(TypeTableLayoutNameB),
        output_layout_name(Stream, TypeTableLayoutNameB, !IO),
        io.write_string(Stream, ",\n", !IO)
    ;
        MaybeTypeTableLayoutNameB = no,
        io.write_string(Stream, "NULL,\n", !IO)
    ),

    (
        MaybeDebugInfo = yes({ProcLayoutVectorLengthB, ProcLayoutVectorNameB,
            FileLayoutVectorLengthB, FileLayoutVectorNameB,
            TraceLevelB, SuppressedEventsB, NumLabelsB, LabelExecCountNameB,
            MaybeEventInfoB}),

        io.write_int(Stream, ProcLayoutVectorLengthB, !IO),
        io.write_string(Stream, ",\n", !IO),
        output_layout_name(Stream, ProcLayoutVectorNameB, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_int(Stream, FileLayoutVectorLengthB, !IO),
        io.write_string(Stream, ",\n", !IO),
        output_layout_name(Stream, FileLayoutVectorNameB, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_string(Stream, trace_level_rep(TraceLevelB), !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_int(Stream, SuppressedEventsB, !IO),
        io.write_string(Stream, ",\n", !IO),
        io.write_int(Stream, NumLabelsB, !IO),
        io.write_string(Stream, ",\n", !IO),
        output_layout_name(Stream, LabelExecCountNameB, !IO),
        io.write_string(Stream, ",\n", !IO),

        (
            MaybeEventInfoB = no,
            io.write_string(Stream, "NULL,\n", !IO),
            io.write_string(Stream, "NULL,\n", !IO),
            io.write_string(Stream, "0,\n", !IO),
            io.write_string(Stream, "0,\n", !IO),
            io.write_string(Stream, "NULL\n", !IO)
        ;
            MaybeEventInfoB = yes({EventSetNameB, MaxNumAttrB, NumEventSpecsB,
                EventSetDescLayoutNameB, EventSpecsLayoutNameB}),
            quote_and_write_string(Stream, EventSetNameB, !IO),
            io.write_string(Stream, ",\n", !IO),
            output_layout_name(Stream, EventSetDescLayoutNameB, !IO),
            io.write_string(Stream, ",\n", !IO),
            io.write_int(Stream, MaxNumAttrB, !IO),
            io.write_string(Stream, ",\n", !IO),
            io.write_int(Stream, NumEventSpecsB, !IO),
            io.write_string(Stream, ",\n", !IO),
            output_layout_name(Stream, EventSpecsLayoutNameB, !IO),
            io.write_string(Stream, "\n", !IO)
        )
    ;
        MaybeDebugInfo = no,

        io.write_string(Stream, "0,\n", !IO),
        io.write_string(Stream, "NULL,\n", !IO),
        io.write_string(Stream, "0,\n", !IO),
        io.write_string(Stream, "NULL,\n", !IO),
        io.write_string(Stream, "MR_TRACE_LEVEL_NONE,\n", !IO),
        io.write_string(Stream, "0,\n", !IO),
        io.write_string(Stream, "0,\n", !IO),
        io.write_string(Stream, "NULL,\n", !IO),

        io.write_string(Stream, "NULL,\n", !IO),
        io.write_string(Stream, "NULL,\n", !IO),
        io.write_string(Stream, "0,\n", !IO),
        io.write_string(Stream, "0,\n", !IO),
        io.write_string(Stream, "NULL\n", !IO)
    ),

    io.write_string(Stream, "};\n", !IO),
    decl_set_insert(decl_layout_id(ModuleLayoutName), !DeclSet).

:- pred output_event_specs_and_components(llds_out_info::in,
    io.text_output_stream::in, list(event_spec)::in, module_name::in,
    map(int, rval)::in, layout_name::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_event_specs_and_components(Info, Stream, EventSpecs, ModuleName,
        TypesRvalMap, LayoutName, !DeclSet, !IO) :-
    list.foldl2(output_event_spec_components(Stream, ModuleName), EventSpecs,
        !DeclSet, !IO),

    LayoutName = module_layout_event_specs(ModuleName),
    decl_set_insert(decl_layout_id(LayoutName), !DeclSet),
    output_layout_name_storage_type_name(Stream, LayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),
    write_out_list(output_event_spec(Info, ModuleName, TypesRvalMap),
        ",\n", EventSpecs, Stream, !IO),
    io.write_string(Stream, "\n};\n\n", !IO).

:- pred output_event_spec_components(io.text_output_stream::in,
    module_name::in, event_spec::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_event_spec_components(Stream, ModuleName, EventSpec, !DeclSet, !IO) :-
    EventSpec = event_spec(EventNumber, _EventName, _EventLineNumber,
        Attrs, SynthOrder),

    AttrNamesLayoutName =
        module_layout_event_arg_names(ModuleName, EventNumber),
    decl_set_insert(decl_layout_id(AttrNamesLayoutName), !DeclSet),
    output_layout_name_storage_type_name(Stream, AttrNamesLayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),
    write_out_list(output_attr_name, ", ", Attrs, Stream, !IO),
    io.write_string(Stream, "\n};\n\n", !IO),

    (
        SynthOrder = []
    ;
        SynthOrder = [_ | _],

        list.foldl2(output_synth_attr_args(Stream, ModuleName, EventNumber),
            Attrs, !DeclSet, !IO),

        SynthAttrsLayoutName =
            module_layout_event_synth_attrs(ModuleName, EventNumber),
        decl_set_insert(decl_layout_id(SynthAttrsLayoutName), !DeclSet),
        output_layout_name_storage_type_name(Stream, SynthAttrsLayoutName,
            being_defined, !IO),
        io.write_string(Stream, " = {\n", !IO),
        write_out_list(output_synth_attr(ModuleName, EventNumber),
            ",\n", Attrs, Stream, !IO),
        io.write_string(Stream, "\n};\n\n", !IO),

        SynthOrderLayoutName =
            module_layout_event_synth_order(ModuleName, EventNumber),
        decl_set_insert(decl_layout_id(SynthOrderLayoutName), !DeclSet),
        output_layout_name_storage_type_name(Stream, SynthOrderLayoutName,
            being_defined, !IO),
        io.write_string(Stream, " = {\n", !IO),
        % The -1 acts as sentinel.
        write_out_list(add_int, ", ", SynthOrder ++ [-1], Stream, !IO),
        io.write_string(Stream, "\n};\n\n", !IO)
    ).

:- pred output_attr_name(event_attribute::in, io.text_output_stream::in,
    io::di, io::uo) is det.

output_attr_name(Attr, Stream, !IO) :-
    io.write_string(Stream, """", !IO),
    io.write_string(Stream, Attr ^ attr_name, !IO),
    io.write_string(Stream, """", !IO).

:- pred output_synth_attr_args(io.text_output_stream::in, module_name::in,
    int::in, event_attribute::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_synth_attr_args(Stream, ModuleName, EventNumber, Attr, !DeclSet, !IO) :-
    MaybeSynthCall = Attr ^ attr_maybe_synth_call,
    (
        MaybeSynthCall = yes(SynthCall),
        SynthCall = event_attr_synth_call(_FuncAttrNameNum, ArgAttrNameNums,
            Order),
        assoc_list.values(ArgAttrNameNums, ArgAttrNums),
        AttrNumber = Attr ^ attr_num,

        ArgsLayoutName = module_layout_event_synth_attr_args(ModuleName,
            EventNumber, AttrNumber),
        decl_set_insert(decl_layout_id(ArgsLayoutName), !DeclSet),
        output_layout_name_storage_type_name(Stream, ArgsLayoutName,
            being_defined, !IO),
        io.write_string(Stream, " =\n{ ", !IO),
        write_out_list(add_int, ", ", ArgAttrNums, Stream, !IO),
        io.write_string(Stream, " };\n\n", !IO),

        OrderLayoutName = module_layout_event_synth_attr_order(ModuleName,
            EventNumber, AttrNumber),
        decl_set_insert(decl_layout_id(OrderLayoutName), !DeclSet),
        output_layout_name_storage_type_name(Stream, OrderLayoutName,
            being_defined, !IO),
        io.write_string(Stream, " =\n{ ", !IO),
        OrderSentinel = Order ++ [-1],
        write_out_list(add_int, ", ", OrderSentinel, Stream, !IO),
        io.write_string(Stream, " };\n\n", !IO)
    ;
        MaybeSynthCall = no
    ).

:- pred output_synth_attr(module_name::in, int::in, event_attribute::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_synth_attr(ModuleName, EventNumber, Attr, Stream, !IO) :-
    io.write_string(Stream, "{ ", !IO),
    MaybeSynthCall = Attr ^ attr_maybe_synth_call,
    (
        MaybeSynthCall = yes(SynthCall),
        SynthCall = event_attr_synth_call(_FuncAttrName - FuncAttrNum,
            ArgAttrNameNums, _EvalOrder),
        io.write_int(Stream, FuncAttrNum, !IO),
        io.write_string(Stream, ", ", !IO),
        io.write_int(Stream, list.length(ArgAttrNameNums), !IO),
        io.write_string(Stream, ",\n ", !IO),

        AttrNumber = Attr ^ attr_num,
        ArgsLayoutName = module_layout_event_synth_attr_args(ModuleName,
            EventNumber, AttrNumber),
        output_layout_name(Stream, ArgsLayoutName, !IO),
        io.write_string(Stream, ",\n ", !IO),
        OrderLayoutName = module_layout_event_synth_attr_order(ModuleName,
            EventNumber, AttrNumber),
        output_layout_name(Stream, OrderLayoutName, !IO)
    ;
        MaybeSynthCall = no,
        io.write_string(Stream, "-1, -1, NULL, NULL", !IO)
    ),
    io.write_string(Stream, " }", !IO).

:- pred output_event_spec(llds_out_info::in,
    module_name::in, map(int, rval)::in, event_spec::in,
    io.text_output_stream::in, io::di, io::uo) is det.

output_event_spec(Info, ModuleName, TypesRvalMap, EventSpec, Stream, !IO) :-
    EventSpec = event_spec(EventNumber, EventName, _EventLineNumber, Attrs,
        SynthOrder),
    map.lookup(TypesRvalMap, EventNumber, TypesRval),

    io.write_string(Stream, "{ """, !IO),
    io.write_string(Stream, EventName, !IO),
    io.write_string(Stream, """, ", !IO),
    io.write_int(Stream, list.length(Attrs), !IO),
    io.write_string(Stream, ",\n\t", !IO),

    AttrNamesLayoutName =
        module_layout_event_arg_names(ModuleName, EventNumber),
    output_layout_name(Stream, AttrNamesLayoutName, !IO),
    io.write_string(Stream, ",\n\t(MR_TypeInfo *) ", !IO),
    output_rval_as_addr(Info, Stream, TypesRval, !IO),
    io.write_string(Stream, ",\n\t", !IO),

    (
        SynthOrder = [],
        io.write_string(Stream, "NULL, NULL }", !IO)
    ;
        SynthOrder = [_ | _],
        SynthAttrsLayoutName =
            module_layout_event_synth_attrs(ModuleName, EventNumber),
        SynthOrderLayoutName =
            module_layout_event_synth_order(ModuleName, EventNumber),
        output_layout_name(Stream, SynthAttrsLayoutName, !IO),
        io.write_string(Stream, ",\n\t", !IO),
        output_layout_name(Stream, SynthOrderLayoutName, !IO),
        io.write_string(Stream, " }", !IO)
    ).

:- pred output_module_layout_proc_vector_defn(io.text_output_stream::in,
    module_name::in, list(layout_name)::in, layout_name::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_module_layout_proc_vector_defn(Stream, ModuleName, ProcLayoutNames,
        VectorName, !DeclSet, !IO) :-
    list.foldl2(output_layout_decl(Stream), ProcLayoutNames, !DeclSet, !IO),
    VectorName = module_layout_proc_vector(ModuleName),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, VectorName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),
    (
        ProcLayoutNames = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string(Stream, "NULL\n", !IO)
    ;
        ProcLayoutNames = [_ | _],
        list.foldl(output_proc_layout_name_in_vector(Stream),
            ProcLayoutNames, !IO)
    ),
    io.write_string(Stream, "};\n", !IO),
    decl_set_insert(decl_layout_id(VectorName), !DeclSet).

:- pred output_proc_layout_name_in_vector(io.text_output_stream::in,
    layout_name::in, io::di, io::uo) is det.

output_proc_layout_name_in_vector(Stream, LayoutName, !IO) :-
    ( if LayoutName = proc_layout(RttiProcLabel, _) then
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        io.write_string(Stream, "MR_PROC_LAYOUT1(", !IO),
        io.write_string(Stream,
            proc_label_to_c_string(do_not_add_label_prefix, ProcLabel), !IO),
        io.write_string(Stream, ")\n", !IO)
    else
        unexpected($pred, "not proc layout")
    ).

%-----------------------------------------------------------------------------%

:- pred output_event_set_desc_defn(io.text_output_stream::in, module_name::in,
    string::in, layout_name::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_event_set_desc_defn(Stream, ModuleName, EventSetDesc, LayoutName,
        !DeclSet, !IO) :-
    LayoutName = module_layout_event_set_desc(ModuleName),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, LayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {", !IO),
    output_module_string_table_strings(Stream, EventSetDesc, [], !IO),
    io.write_string(Stream, "};\n", !IO),
    decl_set_insert(decl_layout_id(LayoutName), !DeclSet).

:- pred output_module_string_table(io.text_output_stream::in, module_name::in,
    int::in, string_with_0s::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_module_string_table(Stream, ModuleName, _StringTableSize,
        string_with_0s(StringTable0), !DeclSet, !IO) :-
    TableName = module_layout_string_table(ModuleName),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, TableName, being_defined, !IO),
    io.write_string(Stream, " = {", !IO),

    % The string table cannot be zero size; it must contain at least an
    % empty string.
    ( StringTable0 = [], FirstString = "", Rest = []
    ; StringTable0 = [FirstString | Rest]
    ),
    output_module_string_table_strings(Stream, FirstString, Rest, !IO),
    io.write_string(Stream, "};\n", !IO),
    decl_set_insert(decl_layout_id(TableName), !DeclSet).

:- pred output_module_string_table_strings(io.text_output_stream::in,
    string::in, list(string)::in, io::di, io::uo) is det.

output_module_string_table_strings(Stream, String, [], !IO) :-
    output_module_string_table_chars(Stream, 0, 0, String, !IO).
output_module_string_table_strings(Stream, String, [Next | Rest], !IO) :-
    output_module_string_table_chars(Stream, 0, 0, String, !IO),
    io.write_string(Stream, ",\n", !IO),
    output_module_string_table_strings(Stream, Next, Rest, !IO).

:- pred output_module_string_table_chars(io.text_output_stream::in,
    int::in, int::in, string::in, io::di, io::uo) is det.

output_module_string_table_chars(Stream, CurIndex, Count, String, !IO) :-
    ( if string.unsafe_index_next(String, CurIndex, NextIndex, Char) then
        ( if
            char.to_int(Char, Int),
            Int =< 0x7f
        then
            io.write_char(Stream, '''', !IO),
            c_util.output_quoted_char(Stream, Char, !IO),
            io.write_char(Stream, '''', !IO),
            io.write_string(Stream, ", ", !IO)
        else if
            char.to_utf8(Char, Codes)
        then
            output_multi_byte_char_codes(Stream, Codes, !IO)
        else
            unexpected($pred, "invalid code point")
        ),
        ( if Count = 10 then
            io.nl(Stream, !IO),
            output_module_string_table_chars(Stream, NextIndex,
                0, String, !IO)
        else
            output_module_string_table_chars(Stream, NextIndex,
                Count + 1, String, !IO)
        )
    else
        io.write_char(Stream, '''', !IO),
        c_util.output_quoted_char(Stream, char.det_from_int(0), !IO),
        io.write_char(Stream, '''', !IO)
    ).

:- pred output_multi_byte_char_codes(io.text_output_stream::in, list(int)::in,
    io::di, io::uo) is det.

output_multi_byte_char_codes(_, [], !IO).
output_multi_byte_char_codes(Stream, [C | Cs], !IO) :-
    io.write_int(Stream, C, !IO),
    io.write_string(Stream, ", ", !IO),
    output_multi_byte_char_codes(Stream, Cs, !IO).

%-----------------------------------------------------------------------------%

:- pred output_file_layout_vector_data_defn(io.text_output_stream::in,
    module_name::in, list(layout_name)::in, layout_name::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_vector_data_defn(Stream, ModuleName, FileLayoutNames,
        VectorName, !DeclSet, !IO) :-
    list.foldl2(output_layout_decl(Stream), FileLayoutNames, !DeclSet, !IO),
    VectorName = module_layout_file_vector(ModuleName),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, VectorName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),
    (
        FileLayoutNames = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string(Stream, "NULL\n", !IO)
    ;
        FileLayoutNames = [_ | _],
        list.foldl(output_layout_name_in_vector(Stream, "&"),
            FileLayoutNames, !IO)
    ),
    io.write_string(Stream, "};\n", !IO),
    decl_set_insert(decl_layout_id(VectorName), !DeclSet).

:- pred output_file_layout_data_defns(llds_out_info::in,
    io.text_output_stream::in, module_name::in, int::in,
    list(file_layout_data)::in, list(layout_name)::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_data_defns(_, _, _, _, [], [], !DeclSet, !IO).
output_file_layout_data_defns(Info, Stream, ModuleName, FileNum,
        [FileLayout | FileLayouts], [FileLayoutName | FileLayoutNames],
        !DeclSet, !IO) :-
    output_file_layout_data_defn(Info, Stream, ModuleName,
        FileNum, FileLayout, FileLayoutName, !DeclSet, !IO),
    output_file_layout_data_defns(Info, Stream, ModuleName,
        FileNum + 1, FileLayouts, FileLayoutNames, !DeclSet, !IO).

:- pred output_file_layout_data_defn(llds_out_info::in,
    io.text_output_stream::in, module_name::in, int::in,
    file_layout_data::in, layout_name::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_data_defn(Info, Stream, ModuleName, FileNum, FileLayout,
        FileLayoutName, !DeclSet, !IO) :-
    MangledModuleName = Info ^ lout_mangled_module_name,
    FileLayout = file_layout_data(FileName, LineNoLabelList),
    assoc_list.keys_and_values(LineNoLabelList, LineNos, LabelLayoutSlots),

    list.length(LineNoLabelList, VectorLengths),
    output_file_layout_line_number_vector_defn(Info, Stream, ModuleName,
        FileNum, LineNos, LineNumberVectorName, !DeclSet, !IO),
    output_file_layout_label_layout_vector_defn(Stream, MangledModuleName,
        ModuleName, FileNum, LabelLayoutSlots, LabelVectorName, !DeclSet, !IO),

    FileLayoutName = file_layout(ModuleName, FileNum),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, FileLayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),
    quote_and_write_string(Stream, FileName, !IO),
    io.write_string(Stream, ",\n", !IO),
    io.write_int(Stream, VectorLengths, !IO),
    io.write_string(Stream, ",\n", !IO),
    output_layout_name(Stream, LineNumberVectorName, !IO),
    io.write_string(Stream, ",\n", !IO),
    output_layout_name(Stream, LabelVectorName, !IO),
    io.write_string(Stream, "\n};\n", !IO),
    decl_set_insert(decl_layout_id(FileLayoutName), !DeclSet).

:- pred output_file_layout_line_number_vector_defn(llds_out_info::in,
    io.text_output_stream::in, module_name::in, int::in, list(int)::in,
    layout_name::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_line_number_vector_defn(Info, Stream, ModuleName, FileNum,
        LineNumbers, LayoutName, !DeclSet, !IO) :-
    LayoutName = file_layout_line_number_vector(ModuleName, FileNum),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, LayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {", !IO),
    (
        LineNumbers = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string(Stream, "\n0", !IO)
    ;
        LineNumbers = [_ | _],
        AutoComments = Info ^ lout_auto_comments,
        (
            AutoComments = auto_comments,
            output_numbers_in_vector_ac(Stream, LineNumbers, 0, !IO)
        ;
            AutoComments = no_auto_comments,
            output_numbers_in_vector_noac(Stream, LineNumbers, 0, !IO)
        )
    ),
    io.write_string(Stream, "\n};\n", !IO),
    decl_set_insert(decl_layout_id(LayoutName), !DeclSet).

:- pred output_file_layout_label_layout_vector_defn(io.text_output_stream::in,
    string::in, module_name::in, int::in, list(layout_slot_name)::in,
    layout_name::out, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_label_layout_vector_defn(Stream, MangledModuleName,
        ModuleName, FileNum, LabelSlots, LayoutName, !DeclSet, !IO) :-
    LayoutName = file_layout_label_layout_vector(ModuleName, FileNum),
    io.write_string(Stream, "\n", !IO),
    output_layout_name_storage_type_name(Stream, LayoutName,
        being_defined, !IO),
    io.write_string(Stream, " = {\n", !IO),
    (
        LabelSlots = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string(Stream, "NULL\n", !IO)
    ;
        LabelSlots = [_ | _],
        output_layout_slots_in_vector(Stream, MangledModuleName,
            LabelSlots, !IO)
    ),
    io.write_string(Stream, "};\n", !IO),
    decl_set_insert(decl_layout_id(LayoutName), !DeclSet).

%-----------------------------------------------------------------------------%

:- pred output_layout_slots_in_vector(io.text_output_stream::in, string::in,
    list(layout_slot_name)::in, io::di, io::uo) is det.

output_layout_slots_in_vector(_, _, [], !IO).
output_layout_slots_in_vector(Stream, ModuleName,
        [SlotName | SlotNames], !IO) :-
    SlotName = layout_slot(ArrayName, SlotNum),
    (
        (
            ArrayName = label_layout_array(label_has_no_var_info),
            Macro = "MR_no_var_label_layout_refs"
        ;
            ArrayName = label_layout_array(label_has_short_var_info),
            Macro = "MR_svar_label_layout_refs"
        ;
            ArrayName = label_layout_array(label_has_long_var_info),
            Macro = "MR_lvar_label_layout_refs"
        ),
        find_slots_in_same_array(ArrayName, SlotNames, [], RevTailSlotNums,
            OtherArraySlotNames),
        list.reverse(RevTailSlotNums, TailSlotNums),
        SlotNums = [SlotNum | TailSlotNums],
        % There must be a macro of the form Macro<n> for all values of <n>
        % between 1 and ChunkSize.
        ChunkSize = 10,
        list.chunk(SlotNums, ChunkSize, SlotNumChunks),
        list.foldl(output_layout_slot_chunk(Stream, Macro, ModuleName),
            SlotNumChunks, !IO),
        output_layout_slots_in_vector(Stream, ModuleName,
            OtherArraySlotNames, !IO)
    ;
        ( ArrayName = pseudo_type_info_array
        ; ArrayName = long_locns_array
        ; ArrayName = short_locns_array
        ; ArrayName = hlds_var_nums_array
        ; ArrayName = user_event_var_nums_array
        ; ArrayName = user_event_layout_array
        ; ArrayName = proc_static_call_sites_array
        ; ArrayName = proc_static_cp_static_array
        ; ArrayName = proc_static_cp_dynamic_array
        ; ArrayName = proc_static_array
        ; ArrayName = proc_var_names_array
        ; ArrayName = proc_head_var_nums_array
        ; ArrayName = proc_body_bytecodes_array
        ; ArrayName = proc_table_io_entry_array
        ; ArrayName = proc_event_layouts_array
        ; ArrayName = proc_exec_trace_array
        ; ArrayName = threadscope_string_table_array
        ; ArrayName = alloc_site_array
        ),
        output_layout_slot_addr(Stream, use_layout_macro, ModuleName,
            SlotName, !IO),
        io.write_string(Stream, ",\n", !IO),
        output_layout_slots_in_vector(Stream, ModuleName, SlotNames, !IO)
    ).

:- pred find_slots_in_same_array(layout_array_name::in,
    list(layout_slot_name)::in, list(int)::in, list(int)::out,
    list(layout_slot_name)::out) is det.

find_slots_in_same_array(_ArrayName, [], !RevSlotNums, []).
find_slots_in_same_array(ArrayName, [SlotName | SlotNames], !RevSlotNums,
        OtherArraySlotNames) :-
    SlotName = layout_slot(SlotArrayName, SlotNum),
    ( if SlotArrayName = ArrayName then
        !:RevSlotNums = [SlotNum | !.RevSlotNums],
        find_slots_in_same_array(ArrayName, SlotNames, !RevSlotNums,
            OtherArraySlotNames)
    else
        OtherArraySlotNames = [SlotName | SlotNames]
    ).

:- pred output_layout_slot_chunk(io.text_output_stream::in,
    string::in, string::in, list(int)::in, io::di, io::uo) is det.

output_layout_slot_chunk(Stream, Macro, ModuleName, SlotNums, !IO) :-
    list.length(SlotNums, Length),
    io.write_string(Stream, Macro, !IO),
    io.write_int(Stream, Length, !IO),
    io.write_string(Stream, "(", !IO),
    io.write_string(Stream, ModuleName, !IO),
    io.write_string(Stream, ", ", !IO),
    write_out_list(add_int, ",", SlotNums, Stream, !IO),
    io.write_string(Stream, ")\n", !IO).

%-----------------------------------------------------------------------------%
%
% Print out vectors of numbers.
%
% We print ten numbers per line to make it easy to locate a particular element
% in a vector.
%
% We do a two level loop to limit the number of stack frames we consume
% in grades that do not permit tail recursion. The idea is that after printing
% 1000 numbers, we free up all the stack space that printing consumed.
% When printing N numbers, we use only about (N / 1000) + (N mod 1000) frames.
%
% Each predicate has two versions. The _ac version assumes auto-comment,
% the _noac version assumes no auto-comment.
%

:- pred output_numbers_in_vector_ac(io.text_output_stream::in,
    list(int)::in, int::in, io::di, io::uo) is det.

output_numbers_in_vector_ac(_, [], _, !IO).
output_numbers_in_vector_ac(Stream, VarNums @ [_ | _], !.Slot, !IO) :-
    output_upto_n_numbers_in_vector_ac(Stream, VarNums, 1000,
        LeftOverVarNums, !Slot, !IO),
    output_numbers_in_vector_ac(Stream, LeftOverVarNums, !.Slot, !IO).

:- pred output_numbers_in_vector_noac(io.text_output_stream::in,
    list(int)::in, int::in, io::di, io::uo) is det.

output_numbers_in_vector_noac(_, [], _, !IO).
output_numbers_in_vector_noac(Stream, VarNums @ [_ | _], !.Slot, !IO) :-
    output_upto_n_numbers_in_vector_noac(Stream, VarNums, 1000,
        LeftOverVarNums, !Slot, !IO),
    output_numbers_in_vector_noac(Stream, LeftOverVarNums, !.Slot, !IO).

:- pred output_upto_n_numbers_in_vector_ac(io.text_output_stream::in,
    list(int)::in, int::in, list(int)::out, int::in, int::out,
    io::di, io::uo) is det.

output_upto_n_numbers_in_vector_ac(_, [], _, [], !Slot, !IO).
output_upto_n_numbers_in_vector_ac(Stream, [VarNum | VarNums], N,
        LeftOverVarNums, !Slot, !IO) :-
    ( if N > 0 then
        output_number_in_vector_ac(Stream, VarNum, !Slot, !IO),
        output_upto_n_numbers_in_vector_ac(Stream, VarNums, N - 1,
            LeftOverVarNums, !Slot, !IO)
    else
        LeftOverVarNums = [VarNum | VarNums]
    ).

:- pred output_upto_n_numbers_in_vector_noac(io.text_output_stream::in,
    list(int)::in, int::in, list(int)::out, int::in, int::out,
    io::di, io::uo) is det.

output_upto_n_numbers_in_vector_noac(_, [], _, [], !Slot, !IO).
output_upto_n_numbers_in_vector_noac(Stream, [VarNum | VarNums], N,
        LeftOverVarNums, !Slot, !IO) :-
    ( if N > 0 then
        output_number_in_vector_noac(Stream, VarNum, !Slot, !IO),
        output_upto_n_numbers_in_vector_noac(Stream, VarNums, N - 1,
            LeftOverVarNums, !Slot, !IO)
    else
        LeftOverVarNums = [VarNum | VarNums]
    ).

:- pred output_number_in_vector_ac(io.text_output_stream::in,
    int::in, int::in, int::out, io::di, io::uo) is det.

output_number_in_vector_ac(Stream, VarNum, !Slot, !IO) :-
    ( if !.Slot mod 10 = 0 then
        io.format(Stream, "\n/* slots %d+ */ ", [i(!.Slot)], !IO)
    else
        io.write_string(Stream, " ", !IO)
    ),
    io.format(Stream, "%d,", [i(VarNum)], !IO),
    !:Slot = !.Slot + 1.

:- pred output_number_in_vector_noac(io.text_output_stream::in,
    int::in, int::in, int::out, io::di, io::uo) is det.

output_number_in_vector_noac(Stream, VarNum, !Slot, !IO) :-
    ( if !.Slot mod 10 = 0 then
        io.nl(Stream, !IO)
    else
        io.write_string(Stream, " ", !IO)
    ),
    io.format(Stream, "%d,", [i(VarNum)], !IO),
    !:Slot = !.Slot + 1.

%-----------------------------------------------------------------------------%
%
% Utility predicates.
%

:- pred output_layout_name_in_vector(io.text_output_stream::in, string::in,
    layout_name::in, io::di, io::uo) is det.

output_layout_name_in_vector(Stream, Prefix, Name, !IO) :-
    io.write_string(Stream, Prefix, !IO),
    output_layout_name(Stream, Name, !IO),
    io.write_string(Stream, ",\n", !IO).

:- pred quote_and_write_string(io.text_output_stream::in, string::in,
    io::di, io::uo) is det.

quote_and_write_string(Stream, String, !IO) :-
    io.write_string(Stream, """", !IO),
    c_util.output_quoted_string(Stream, String, !IO),
    io.write_string(Stream, """", !IO).

:- pred long_length(list(T)::in, int::out) is det.

long_length(List, Length) :-
    long_length_outer_loop(List, 0, Length).

:- pred long_length_outer_loop(list(T)::in, int::in, int::out) is det.

long_length_outer_loop([], !Length).
long_length_outer_loop(List @ [_ | _], !Length) :-
    long_length_inner_loop(List, 5000, LeftOver, !Length),
    long_length_outer_loop(LeftOver, !Length).

:- pred long_length_inner_loop(list(T)::in, int::in, list(T)::out,
    int::in, int::out) is det.

long_length_inner_loop([], _, [], !Length).
long_length_inner_loop([H | T], Count, LeftOver, !Length) :-
    ( if Count > 0 then
        !:Length = !.Length + 1,
        long_length_inner_loop(T, Count - 1, LeftOver, !Length)
    else
        LeftOver = [H | T]
    ).

%-----------------------------------------------------------------------------%

layout_name_would_include_code_addr(LayoutName) = InclCodeAddr :-
    (
        ( LayoutName = proc_layout(_, _)
        ; LayoutName = closure_proc_id(_, _, _)
        ; LayoutName = file_layout(_, _)
        ; LayoutName = file_layout_line_number_vector(_, _)
        ; LayoutName = file_layout_label_layout_vector(_, _)
        ; LayoutName = module_layout_string_table(_)
        ; LayoutName = module_layout_file_vector(_)
        ; LayoutName = module_layout_proc_vector(_)
        ; LayoutName = module_layout_label_exec_count(_, _)
        ; LayoutName = module_layout_event_set_desc(_)
        ; LayoutName = module_layout_event_arg_names(_, _)
        ; LayoutName = module_layout_event_synth_attrs(_, _)
        ; LayoutName = module_layout_event_synth_attr_args(_, _, _)
        ; LayoutName = module_layout_event_synth_attr_order(_, _, _)
        ; LayoutName = module_layout_event_synth_order(_, _)
        ; LayoutName = module_layout_event_specs(_)
        ; LayoutName = module_layout_oisu_bytes(_)
        ; LayoutName = module_layout_type_table_bytes(_)
        ; LayoutName = module_layout(_)
        ),
        InclCodeAddr = no
    ).

:- func proc_layout_kind_to_type(proc_layout_kind) = string.

proc_layout_kind_to_type(proc_layout_traversal) = "MR_ProcLayout_Traversal".
proc_layout_kind_to_type(proc_layout_proc_id(user)) = "MR_ProcLayoutUser".
proc_layout_kind_to_type(proc_layout_proc_id(uci)) = "MR_ProcLayoutUCI".

proc_label_user_or_uci(ordinary_proc_label(_, _, _, _, _, _)) = user.
proc_label_user_or_uci(special_proc_label(_, _, _, _, _, _)) = uci.

mr_pred_or_func_to_string(PredOrFunc) = Str :-
    (
        PredOrFunc = pf_predicate,
        Str = "MR_PREDICATE"
    ;
        PredOrFunc = pf_function,
        Str = "MR_FUNCTION"
    ).

:- func detism_to_c_detism(determinism) = string.

detism_to_c_detism(detism_det) =       "MR_DETISM_DET".
detism_to_c_detism(detism_semi) =      "MR_DETISM_SEMI".
detism_to_c_detism(detism_non) =       "MR_DETISM_NON".
detism_to_c_detism(detism_multi) =     "MR_DETISM_MULTI".
detism_to_c_detism(detism_erroneous) = "MR_DETISM_ERRONEOUS".
detism_to_c_detism(detism_failure) =   "MR_DETISM_FAILURE".
detism_to_c_detism(detism_cc_non) =    "MR_DETISM_CCNON".
detism_to_c_detism(detism_cc_multi) =  "MR_DETISM_CCMULTI".

trace_port_to_string(port_call) =            "CALL".
trace_port_to_string(port_exit) =            "EXIT".
trace_port_to_string(port_redo) =            "REDO".
trace_port_to_string(port_fail) =            "FAIL".
trace_port_to_string(port_tailrec_call) =    "TAILREC_CALL".
trace_port_to_string(port_exception) =       "EXCEPTION".
trace_port_to_string(port_ite_cond) =        "COND".
trace_port_to_string(port_ite_then) =        "THEN".
trace_port_to_string(port_ite_else) =        "ELSE".
trace_port_to_string(port_neg_enter) =       "NEG_ENTER".
trace_port_to_string(port_neg_success) =     "NEG_SUCCESS".
trace_port_to_string(port_neg_failure) =     "NEG_FAILURE".
trace_port_to_string(port_disj_first) =      "DISJ_FIRST".
trace_port_to_string(port_disj_later) =      "DISJ_LATER".
trace_port_to_string(port_switch) =          "SWITCH".
trace_port_to_string(port_user) =            "USER".

%-----------------------------------------------------------------------------%
:- end_module ll_backend.layout_out.
%-----------------------------------------------------------------------------%
