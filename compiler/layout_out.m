%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2007 The University of Melbourne.
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

:- import_module ll_backend.layout.
:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.
:- import_module mdbcomp.prim_data.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

    % Given a Mercury representation of a layout structure, output its
    % definition in the appropriate C global variable.
    %
:- pred output_layout_data_defn(layout_data::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

    % Given the name of a layout structure, output the declaration
    % of the C global variable which will hold it.
    %
:- pred output_layout_name_decl(layout_name::in, io::di, io::uo) is det.

    % Given the name of a layout structure, output the declaration of the C
    % global variable which will hold it, if it has not already been declared.
:- pred output_maybe_layout_name_decl(layout_name::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Given a Mercury representation of a layout structure, output the
    % declaration of the C global variable which will hold it, if it has
    % not already been declared.
    %
:- pred output_maybe_layout_data_decl(layout_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

    % Given a reference to a layout structure, output the storage class
    % (e.g. static), type and name of the global variable that will
    % hold it. The bool says whether the output is part of the definition of
    % that variable (this influences e.g. whether we output "extern" or not).
    %
:- pred output_layout_name_storage_type_name(layout_name::in, bool::in,
    io::di, io::uo) is det.

    % Given a reference to a layout structure, output the name of the
    % global variable that will hold it.
    %
:- pred output_layout_name(layout_name::in, io::di, io::uo) is det.

    % Given a reference to a layout structure, return a bool that is true
    % iff the layout structure contains code addresses.
    %
:- func layout_name_would_include_code_addr(layout_name) = bool.

    % Given a label, return a string giving the name of the global variable
    % containing the label layout structure that would be associated with it.
    % Make_label_layout_name does not guarantee that the label *has* an
    % associated label layout structure.
    %
:- func make_label_layout_name(label) = string.

    % For a given procedure label, return whether the procedure is
    % user-defined or part of a compiler-generated unify, compare or index
    % predicate.
    %
:- func proc_label_user_or_uci(proc_label) = proc_layout_user_or_uci.

    % Output a value of C type MR_PredFunc corrresponding to the argument.
    %
:- pred output_pred_or_func(pred_or_func::in, io::di, io::uo) is det.

    % Return the name of the given port, as in the enum MR_Trace_Port
    % in runtime/mercury_stack_layout.h.
    %
:- func trace_port_to_string(trace_port) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.proc_label.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.special_pred.
:- import_module libs.compiler_util.
:- import_module libs.trace_params.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.

:- import_module assoc_list.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.
:- import_module varset.

%-----------------------------------------------------------------------------%

output_layout_data_defn(Data, !DeclSet, !IO) :-
    (
        Data = label_layout_data(ProcLabel, LabelNum, ProcLayoutAddr,
            MaybePort, MaybeIsHidden, LabelNumber, MaybeGoalPath,
            MaybeUserData, MaybeVarInfo),
        output_label_layout_data_defn(ProcLabel, LabelNum, ProcLayoutAddr,
            MaybePort, MaybeIsHidden, LabelNumber, MaybeGoalPath,
            MaybeUserData, MaybeVarInfo, !DeclSet, !IO)
    ;
        Data = proc_layout_data(ProcLabel, Traversal, MaybeRest),
        output_proc_layout_data_defn(ProcLabel, Traversal, MaybeRest,
            !DeclSet, !IO)
    ;
        Data = closure_proc_id_data(CallerProcLabel, SeqNo, ProcLabel,
            ModuleName, FileName, LineNumber, PredOrigin, GoalPath),
        output_closure_proc_id_data_defn(CallerProcLabel, SeqNo, ProcLabel,
            ModuleName, FileName, LineNumber, PredOrigin, GoalPath,
            !DeclSet, !IO)
    ;
        Data = module_layout_data(ModuleName, StringTableSize,
            StringTable, ProcLayoutNames, FileLayouts, TraceLevel,
            SuppressedEvents, NumLabels, MaybeEventSet),
        output_module_layout_data_defn(ModuleName, StringTableSize,
            StringTable, ProcLayoutNames, FileLayouts, TraceLevel,
            SuppressedEvents, NumLabels, MaybeEventSet, !DeclSet, !IO)
    ;
        Data = table_io_decl_data(RttiProcLabel, Kind, NumPTIs,
            PTIVectorRval, TypeParamsRval),
        output_table_io_decl(RttiProcLabel, Kind, NumPTIs,
            PTIVectorRval, TypeParamsRval, !DeclSet, !IO)
    ).

%-----------------------------------------------------------------------------%

output_layout_name_decl(LayoutName, !IO) :-
    output_layout_name_storage_type_name(LayoutName, no, !IO),
    io.write_string(";\n", !IO).

output_maybe_layout_name_decl(LayoutName, !DeclSet, !IO) :-
    ( decl_set_is_member(decl_data_addr(layout_addr(LayoutName)), !.DeclSet) ->
        true
    ;
        output_layout_name_decl(LayoutName, !IO),
        decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet)
    ).

output_maybe_layout_data_decl(LayoutData, !DeclSet, !IO) :-
    extract_layout_name(LayoutData, LayoutName),
    output_maybe_layout_name_decl(LayoutName, !DeclSet, !IO).

:- pred extract_layout_name(layout_data::in, layout_name::out) is det.

extract_layout_name(Data, LayoutName) :-
    (
        Data = label_layout_data(ProcLabel, LabelNum, _, _, _, _, _, _,
            MaybeVarInfo),
        (
            MaybeVarInfo = yes(_),
            LabelVars = label_has_var_info
        ;
            MaybeVarInfo = no,
            LabelVars = label_has_no_var_info
        ),
        LayoutName = label_layout(ProcLabel, LabelNum, LabelVars)
    ;
        Data = proc_layout_data(RttiProcLabel, _, MaybeRest),
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        Kind = maybe_proc_layout_and_more_kind(MaybeRest, ProcLabel),
        LayoutName = proc_layout(RttiProcLabel, Kind)
    ;
        Data = closure_proc_id_data(CallerProcLabel, SeqNo, ClosureProcLabel,
            _, _, _, _, _),
        LayoutName = closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel)
    ;
        Data = module_layout_data(ModuleName, _, _, _, _, _, _, _, _),
        LayoutName = module_layout(ModuleName)
    ;
        Data = table_io_decl_data(RttiProcLabel, _, _, _, _),
        LayoutName = table_io_decl(RttiProcLabel)
    ).

:- pred output_layout_decl(layout_name::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_layout_decl(LayoutName, !DeclSet, !IO) :-
    ( decl_set_is_member(decl_data_addr(layout_addr(LayoutName)), !.DeclSet) ->
        true
    ;
        output_layout_name_storage_type_name(LayoutName, no, !IO),
        io.write_string(";\n", !IO),
        decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet)
    ).

    % This code should be kept in sync with output_layout_name/3 below.
make_label_layout_name(Label) = Name :-
    % We can't omit the mercury_ prefix on LabelName, even though the
    % mercury_data_prefix duplicates it, because there is no simple way
    % to make the MR_init_label_sl macro delete that prefix from the
    % label's name to get the name of its layout structure.
    LabelName = label_to_c_string(Label, yes),
    string.append_list([
        mercury_data_prefix,
        "_label_layout__",
        LabelName
    ], Name).

output_layout_name(Data, !IO) :-
    (
        Data = label_layout(ProcLabel, LabelNum, _),
        % This code should be kept in sync with make_label_layout_name/1 above.
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_label_layout__", !IO),
        io.write_string(
            label_to_c_string(internal_label(LabelNum, ProcLabel), yes), !IO)
    ;
        Data = user_event_layout(ProcLabel, LabelNum),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_user_event_layout__", !IO),
        io.write_string(
            label_to_c_string(internal_label(LabelNum, ProcLabel), yes), !IO)
    ;
        Data = user_event_attr_var_nums(ProcLabel, LabelNum),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_user_event_attr_var_nums__", !IO),
        io.write_string(
            label_to_c_string(internal_label(LabelNum, ProcLabel), yes), !IO)
    ;
        Data = proc_layout(RttiProcLabel, _),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_proc_layout__", !IO),
        % We can't omit the mercury_ prefix on ProcLabel, even though the
        % mercury_data_prefix duplicates it, because there is no simply way
        % to make the MR_init_entryl_sl macro delete that prefix from the
        % entry label's name to get the name of its layout structure.
        output_proc_label(make_proc_label_from_rtti(RttiProcLabel), !IO)
    ;
        Data = proc_layout_exec_trace(RttiProcLabel),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_proc_layout_exec_trace__", !IO),
        output_proc_label_no_prefix(make_proc_label_from_rtti(RttiProcLabel),
            !IO)
    ;
        Data = proc_layout_head_var_nums(RttiProcLabel),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_head_var_nums__", !IO),
        output_proc_label_no_prefix(make_proc_label_from_rtti(RttiProcLabel),
            !IO)
    ;
        Data = proc_layout_var_names(RttiProcLabel),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_var_names__", !IO),
        output_proc_label_no_prefix(make_proc_label_from_rtti(RttiProcLabel),
            !IO)
    ;
        Data = proc_layout_body_bytecode(RttiProcLabel),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_body_bytecode__", !IO),
        output_proc_label_no_prefix(make_proc_label_from_rtti(RttiProcLabel),
            !IO)
    ;
        Data = closure_proc_id(CallerProcLabel, SeqNo, _),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_closure_layout__", !IO),
        output_proc_label_no_prefix(CallerProcLabel, !IO),
        io.write_string("_", !IO),
        io.write_int(SeqNo, !IO)
    ;
        Data = file_layout(ModuleName, FileNum),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_file_layout__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(FileNum, !IO)
    ;
        Data = file_layout_line_number_vector(ModuleName, FileNum),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_file_lines__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(FileNum, !IO)
    ;
        Data = file_layout_label_layout_vector(ModuleName, FileNum),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_file_label_layouts__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(FileNum, !IO)
    ;
        Data = module_layout_string_table(ModuleName),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_strings__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO)
    ;
        Data = module_layout_file_vector(ModuleName),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_files__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO)
    ;
        Data = module_layout_proc_vector(ModuleName),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_procs__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO)
    ;
        Data = module_layout_label_exec_count(ModuleName, _),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_label_exec_counts__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO)
    ;
        Data = module_layout_event_set_desc(ModuleName),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout_event_set_desc__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO)
    ;
        Data = module_layout_event_arg_names(ModuleName, EventNumber),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout_event_arg_names__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(EventNumber, !IO)
    ;
        Data = module_layout_event_synth_attrs(ModuleName, EventNumber),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout_event_synth_attrs__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(EventNumber, !IO)
    ;
        Data = module_layout_event_synth_attr_args(ModuleName,
            EventNumber, SynthCallArgNumber),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout_event_synth_attr_args__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(EventNumber, !IO),
        io.write_string("_", !IO),
        io.write_int(SynthCallArgNumber, !IO)
    ;
        Data = module_layout_event_synth_attr_order(ModuleName,
            EventNumber, SynthCallArgNumber),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout_event_synth_attr_order__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(EventNumber, !IO),
        io.write_string("_", !IO),
        io.write_int(SynthCallArgNumber, !IO)
    ;
        Data = module_layout_event_synth_order(ModuleName, EventNumber),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout_event_synth_order__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO),
        io.write_string("_", !IO),
        io.write_int(EventNumber, !IO)
    ;
        Data = module_layout_event_specs(ModuleName),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout_event_specs__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO)
    ;
        Data = module_layout(ModuleName),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_module_layout__", !IO),
        ModuleNameStr = sym_name_mangle(ModuleName),
        io.write_string(ModuleNameStr, !IO)
    ;
        Data = proc_static(RttiProcLabel),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_proc_static__", !IO),
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        output_proc_label_no_prefix(ProcLabel, !IO)
    ;
        Data = proc_static_call_sites(RttiProcLabel),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_proc_static_call_sites__", !IO),
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        output_proc_label_no_prefix(ProcLabel, !IO)
    ;
        Data = table_io_decl(RttiProcLabel),
        io.write_string(mercury_data_prefix, !IO),
        io.write_string("_table_io_decl__", !IO),
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        output_proc_label_no_prefix(ProcLabel, !IO)
    ).

output_layout_name_storage_type_name(Name, BeingDefined, !IO) :-
    (
        Name = label_layout(_ProcLabel, _LabelNum, LabelVars),
        io.write_string("static const ", !IO),
        io.write_string(label_vars_to_type(LabelVars), !IO),
        io.write_string(" ", !IO),
        output_layout_name(Name, !IO)
    ;
        Name = user_event_layout(_ProcLabel, _LabelNum),
        io.write_string("static const struct MR_UserEvent_Struct ", !IO),
        output_layout_name(Name, !IO)
    ;
        Name = user_event_attr_var_nums(_ProcLabel, _LabelNum),
        io.write_string("static const MR_HLDSVarNum ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = proc_layout(ProcLabel, Kind),
        ProcIsImported = ProcLabel ^ proc_is_imported,
        ProcIsExported = ProcLabel ^ proc_is_exported,
        (
            ProcIsImported = no,
            ProcIsExported = no
        ->
            io.write_string("static ", !IO)
        ;
            (
                BeingDefined = yes
            ;
                BeingDefined = no,
                io.write_string("extern ", !IO)
            )
        ),
        io.write_string("const ", !IO),
        io.write_string(proc_layout_kind_to_type(Kind), !IO),
        io.write_string(" ", !IO),
        output_layout_name(Name, !IO)
    ;
        Name = proc_layout_exec_trace(_ProcLabel),
        io.write_string("static MR_STATIC_CODE_CONST MR_ExecTrace\n\t", !IO),
        output_layout_name(Name, !IO)
    ;
        Name = proc_layout_head_var_nums(_ProcLabel),
        io.write_string("static const ", !IO),
        io.write_string("MR_uint_least16_t ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = proc_layout_var_names(_ProcLabel),
        io.write_string("static const ", !IO),
        io.write_string("MR_uint_least32_t ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = proc_layout_body_bytecode(_ProcLabel),
        io.write_string("static const ", !IO),
        io.write_string("MR_uint_least8_t ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = closure_proc_id(_CallerProcLabel, _SeqNo, ClosureProcLabel),
        io.write_string("static const ", !IO),
        (
            ClosureProcLabel = ordinary_proc_label(_, _, _, _, _, _),
            io.write_string("MR_UserClosureId\n", !IO)
        ;
            ClosureProcLabel = special_proc_label(_, _, _, _, _, _),
            io.write_string("MR_UCIClosureId\n", !IO)
        ),
        output_layout_name(Name, !IO)
    ;
        Name = file_layout(_ModuleName, _FileNum),
        io.write_string("static const MR_ModuleFileLayout ", !IO),
        output_layout_name(Name, !IO)
    ;
        Name = file_layout_line_number_vector(_ModuleName, _FileNum),
        io.write_string("static const MR_int_least16_t ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = file_layout_label_layout_vector(_ModuleName, _FileNum),
        io.write_string("static const MR_LabelLayout *", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_string_table(_ModuleName),
        io.write_string("static const char ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_file_vector(_ModuleName),
        io.write_string("static const MR_ModuleFileLayout *", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_label_exec_count(_ModuleName, NumElements),
        io.write_string("static MR_Unsigned ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[", !IO),
        io.write_int(NumElements, !IO),
        io.write_string("]", !IO)
    ;
        Name = module_layout_proc_vector(_ModuleName),
        io.write_string("static const MR_ProcLayout *", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_event_set_desc(_ModuleName),
        io.write_string("static const char ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_event_arg_names(_ModuleName, _EventNumber),
        io.write_string("static const char * ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_event_synth_attrs(_ModuleName, _EventNumber),
        io.write_string("static MR_SynthAttr ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_event_synth_attr_args(_ModuleName,
            _EventNumber, _SynthCallArgNumber),
        io.write_string("static MR_uint_least16_t ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_event_synth_attr_order(_ModuleName,
            _EventNumber, _SynthCallArgNumber),
        io.write_string("static MR_uint_least16_t ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_event_synth_order(_ModuleName, _EventNumber),
        io.write_string("static MR_int_least16_t ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout_event_specs(_ModuleName),
        io.write_string("static MR_UserEventSpec ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = module_layout(_ModuleName),
        io.write_string("static const MR_ModuleLayout ", !IO),
        output_layout_name(Name, !IO)
    ;
        Name = proc_static(_RttiProcLabel),
        io.write_string("static MR_ProcStatic ", !IO),
        output_layout_name(Name, !IO)
    ;
        Name = proc_static_call_sites(_RttiProcLabel),
        io.write_string("static const MR_CallSiteStatic ", !IO),
        output_layout_name(Name, !IO),
        io.write_string("[]", !IO)
    ;
        Name = table_io_decl(_RttiProcLabel),
        io.write_string("static const MR_TableIoDecl ", !IO),
        output_layout_name(Name, !IO)
    ).

layout_name_would_include_code_addr(label_layout(_, _, _)) = no.
layout_name_would_include_code_addr(user_event_layout(_, _)) = no.
layout_name_would_include_code_addr(user_event_attr_var_nums(_, _)) = no.
layout_name_would_include_code_addr(proc_layout(_, _)) = no.
layout_name_would_include_code_addr(proc_layout_exec_trace(_)) = yes.
layout_name_would_include_code_addr(proc_layout_head_var_nums(_)) = no.
layout_name_would_include_code_addr(proc_layout_var_names(_)) = no.
layout_name_would_include_code_addr(proc_layout_body_bytecode(_)) = no.
layout_name_would_include_code_addr(closure_proc_id(_, _, _)) = no.
layout_name_would_include_code_addr(file_layout(_, _)) = no.
layout_name_would_include_code_addr(file_layout_line_number_vector(_, _)) = no.
layout_name_would_include_code_addr(file_layout_label_layout_vector(_, _))
    = no.
layout_name_would_include_code_addr(module_layout_string_table(_)) = no.
layout_name_would_include_code_addr(module_layout_file_vector(_)) = no.
layout_name_would_include_code_addr(module_layout_proc_vector(_)) = no.
layout_name_would_include_code_addr(module_layout_label_exec_count(_, _)) = no.
layout_name_would_include_code_addr(module_layout_event_set_desc(_)) = no.
layout_name_would_include_code_addr(module_layout_event_arg_names(_, _)) = no.
layout_name_would_include_code_addr(module_layout_event_synth_attrs(_, _))
    = no.
layout_name_would_include_code_addr(
    module_layout_event_synth_attr_args(_, _, _)) = no.
layout_name_would_include_code_addr(
    module_layout_event_synth_attr_order(_, _, _)) = no.
layout_name_would_include_code_addr(module_layout_event_synth_order(_, _))
    = no.
layout_name_would_include_code_addr(module_layout_event_specs(_)) = no.
layout_name_would_include_code_addr(module_layout(_)) = no.
layout_name_would_include_code_addr(proc_static(_)) = no.
layout_name_would_include_code_addr(proc_static_call_sites(_)) = no.
layout_name_would_include_code_addr(table_io_decl(_)) = no.

:- func label_vars_to_type(label_vars) = string.

label_vars_to_type(label_has_var_info) =    "MR_LabelLayout".
label_vars_to_type(label_has_no_var_info) = "MR_LabelLayoutNoVarInfo".

:- func proc_layout_kind_to_type(proc_layout_kind) = string.

proc_layout_kind_to_type(proc_layout_traversal) = "MR_ProcLayout_Traversal".
proc_layout_kind_to_type(proc_layout_proc_id(user)) = "MR_ProcLayoutUser".
proc_layout_kind_to_type(proc_layout_proc_id(uci)) = "MR_ProcLayoutUCI".

%-----------------------------------------------------------------------------%

:- type rval_or_numpair_or_none
    --->    kind_rval(rval)
    ;       kind_num_pair(type_num, int)
    ;       kind_none.

:- pred output_rval_or_numpair_or_none(rval_or_numpair_or_none::in,
    io::di, io::uo) is det.

output_rval_or_numpair_or_none(kind_rval(Rval), !IO) :-
    io.write_string(", ", !IO),
    output_rval_as_addr(Rval, !IO).
output_rval_or_numpair_or_none(kind_num_pair(type_num(Num1), Num2), !IO) :-
    io.write_string(", ", !IO),
    io.write_int(Num1, !IO),
    io.write_string(", ", !IO),
    io.write_int(Num2, !IO).
output_rval_or_numpair_or_none(kind_none, !IO).

:- pred output_label_layout_data_defn(proc_label::in, int::in, layout_name::in,
    maybe(trace_port)::in, maybe(bool)::in, int::in, maybe(int)::in,
    maybe(user_event_data)::in, maybe(label_var_info)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_label_layout_data_defn(ProcLabel, LabelNum, ProcLayoutAddr, MaybePort,
        MaybeIsHidden, LabelNumberInModule, MaybeGoalPath, MaybeUserData,
        MaybeVarInfo, !DeclSet, !IO) :-
    output_layout_decl(ProcLayoutAddr, !DeclSet, !IO),
    (
        MaybeUserData = no,
        UserChars = ""
    ;
        MaybeUserData = yes(UserData),
        UserChars = "_U",
        UserData = user_event_data(UserEventNumber, UserLocnsRval,
            UserAttrMaybeVarNums),

        AttrVarNumsLayoutName = user_event_attr_var_nums(ProcLabel, LabelNum),
        AttrVarNumsDataAddr = layout_addr(AttrVarNumsLayoutName),
        decl_set_insert(decl_data_addr(AttrVarNumsDataAddr), !DeclSet),
        output_layout_name_storage_type_name(AttrVarNumsLayoutName, no, !IO),
        io.write_string(" = {\n", !IO),
        io.write_list(UserAttrMaybeVarNums, ", ", output_maybe_var_num, !IO),
        io.write_string("\n};\n\n", !IO),

        UserLayoutName = user_event_layout(ProcLabel, LabelNum),
        UserDataAddr = layout_addr(UserLayoutName),
        decl_set_insert(decl_data_addr(UserDataAddr), !DeclSet),
        output_layout_name_storage_type_name(UserLayoutName, no, !IO),
        io.write_string(" = {\n", !IO),
        io.write_int(UserEventNumber, !IO),
        io.write_string(",\n(MR_LongLval *) ", !IO),
        output_rval_as_addr(UserLocnsRval, !IO),
        io.write_string(",\n", !IO),
        output_layout_name(AttrVarNumsLayoutName, !IO),
        io.write_string("\n};\n\n", !IO)
    ),
    (
        MaybeIsHidden = yes(yes),
        HiddenChars = "T"
    ;
        MaybeIsHidden = yes(no),
        HiddenChars = ""
    ;
        MaybeIsHidden = no,
        % The value of the hidden field shouldn't matter here.
        HiddenChars = ""
    ),
    (
        MaybeVarInfo = yes(VarInfo0),
        VarInfo0 = label_var_info(EncodedVarCount1, LocnsTypes0, VarNums0,
            TypeParams0),
        output_rval_decls(LocnsTypes0, !DeclSet, !IO),
        output_rval_decls(VarNums0, !DeclSet, !IO),
        output_rval_decls(TypeParams0, !DeclSet, !IO),
        LabelVars = label_has_var_info,
        (
            LocnsTypes0 = const(llconst_data_addr(LTDataAddr, no)),
            LTDataAddr = data_addr(_, scalar_common_ref(LTTypeNum, LTCellNum)),
            VarNums0 = const(llconst_data_addr(VNDataAddr, no)),
            VNDataAddr = data_addr(_, scalar_common_ref(VNTypeNum, VNCellNum))
        ->
            (
                TypeParams0 = const(llconst_data_addr(TPDataAddr, no)),
                TPDataAddr = data_addr(_,
                    scalar_common_ref(TPTypeNum, TPCellNum))
            ->
                CommonChars = "XCCC",
                LocnsTypes1 = kind_num_pair(LTTypeNum, LTCellNum),
                VarNums1 = kind_num_pair(VNTypeNum, VNCellNum),
                TypeParams1 = kind_num_pair(TPTypeNum, TPCellNum)
            ;
                TypeParams0 = const(llconst_int(0))
            ->
                CommonChars = "XCC0",
                LocnsTypes1 = kind_num_pair(LTTypeNum, LTCellNum),
                VarNums1 = kind_num_pair(VNTypeNum, VNCellNum),
                TypeParams1 = kind_none
            ;
                CommonChars = "",
                LocnsTypes1 = kind_rval(LocnsTypes0),
                VarNums1 = kind_rval(VarNums0),
                TypeParams1 = kind_rval(TypeParams0)
            )
        ;
            CommonChars = "",
            LocnsTypes1 = kind_rval(LocnsTypes0),
            VarNums1 = kind_rval(VarNums0),
            TypeParams1 = kind_rval(TypeParams0)
        ),
        Macro0 = "MR_DEF_LL" ++ HiddenChars ++ CommonChars,
        MaybeVarInfoTuple = yes({EncodedVarCount1, LocnsTypes1, VarNums1,
            TypeParams1})
    ;
        MaybeVarInfo = no,
        LabelVars = label_has_no_var_info,
        Macro0 = "MR_DEF_LLNVI" ++ HiddenChars,
        MaybeVarInfoTuple = no
    ),
    Macro = Macro0 ++ UserChars,
    LayoutName = label_layout(ProcLabel, LabelNum, LabelVars),
    io.write_string("\n", !IO),
    io.write_string(Macro, !IO),
    io.write_string("(", !IO),
    output_proc_label_no_prefix(ProcLabel, !IO),
    io.write_string(",\n", !IO),
    io.write_int(LabelNum, !IO),
    io.write_string(", ", !IO),
    (
        MaybePort = yes(Port),
        io.write_string(trace_port_to_string(Port), !IO)
    ;
        MaybePort = no,
        io.write_string("NONE", !IO)
    ),
    io.write_string(", ", !IO),
    io.write_int(LabelNumberInModule, !IO),
    io.write_string(", ", !IO),
    (
        MaybeGoalPath = yes(GoalPath),
        io.write_int(GoalPath, !IO)
    ;
        MaybeGoalPath = no,
        io.write_string("0", !IO)
    ),
    (
        MaybeVarInfoTuple = yes({EncodedVarCount,
            LocnsTypes, VarNums, TypeParams}),
        io.write_string(", ", !IO),
        io.write_int(EncodedVarCount, !IO),
        output_rval_or_numpair_or_none(LocnsTypes, !IO),
        output_rval_or_numpair_or_none(VarNums, !IO),
        output_rval_or_numpair_or_none(TypeParams, !IO)
    ;
        MaybeVarInfoTuple = no
    ),
    io.write_string(");\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_maybe_var_num(maybe(int)::in, io::di, io::uo) is det.

output_maybe_var_num(no, !IO) :-
    % Zero means not a variable, which is what we want.
    io.write_int(0, !IO).
output_maybe_var_num(yes(VarNum), !IO) :-
    io.write_int(VarNum, !IO).

    % Output the rval in a context in which it is immediately cast to an
    % address.
    %
:- pred output_rval_as_addr(rval::in, io::di, io::uo) is det.

output_rval_as_addr(Rval, !IO) :-
    ( Rval = const(llconst_int(0)) ->
        io.write_string(" 0", !IO)
    ; Rval = const(llconst_data_addr(DataAddr, no)) ->
        ( DataAddr = data_addr(_, scalar_common_ref(_TypeNum, _CellNum)) ->
            output_data_addr(DataAddr, !IO)
        ;
            io.write_string("&", !IO),
            output_data_addr(DataAddr, !IO)
        )
    ;
        io.write_string("\n", !IO),
        output_rval(Rval, !IO)
    ).

trace_port_to_string(port_call) =                "CALL".
trace_port_to_string(port_exit) =                "EXIT".
trace_port_to_string(port_redo) =                "REDO".
trace_port_to_string(port_fail) =                "FAIL".
trace_port_to_string(port_exception) =           "EXCEPTION".
trace_port_to_string(port_ite_cond) =            "COND".
trace_port_to_string(port_ite_then) =            "THEN".
trace_port_to_string(port_ite_else) =            "ELSE".
trace_port_to_string(port_neg_enter) =           "NEG_ENTER".
trace_port_to_string(port_neg_success) =         "NEG_SUCCESS".
trace_port_to_string(port_neg_failure) =         "NEG_FAILURE".
trace_port_to_string(port_disj) =                "DISJ".
trace_port_to_string(port_switch) =              "SWITCH".
trace_port_to_string(port_nondet_pragma_first) = "PRAGMA_FIRST".
trace_port_to_string(port_nondet_pragma_later) = "PRAGMA_LATER".
trace_port_to_string(port_user) =                "USER".

%-----------------------------------------------------------------------------%

:- pred output_proc_layout_data_defn(rtti_proc_label::in,
    proc_layout_stack_traversal::in, maybe_proc_id_and_more::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_proc_layout_data_defn(RttiProcLabel, Traversal, MaybeRest,
        !DeclSet, !IO) :-
    output_layout_traversal_decls(Traversal, !DeclSet, !IO),
    ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
    Kind = maybe_proc_layout_and_more_kind(MaybeRest, ProcLabel),
    (
        MaybeRest = no_proc_id,
        output_proc_layout_data_defn_start(RttiProcLabel, Kind, Traversal,
            !IO),
        output_layout_no_proc_id_group(!IO),
        output_proc_layout_data_defn_end(!IO)
    ;
        MaybeRest = proc_id(MaybeProcStatic, MaybeExecTrace),
        (
            MaybeProcStatic = yes(ProcStatic),
            output_proc_static_data_defn(RttiProcLabel, ProcStatic, !DeclSet,
                !IO)
        ;
            MaybeProcStatic = no
        ),
        (
            MaybeExecTrace = yes(ExecTrace),
            HeadVarNums = ExecTrace ^ head_var_nums,
            output_proc_layout_head_var_nums(RttiProcLabel, HeadVarNums,
                !DeclSet, !IO),
            VarNames = ExecTrace ^ var_names,
            MaxVarNum = ExecTrace ^ max_var_num,
            output_proc_layout_var_names(RttiProcLabel, VarNames, MaxVarNum,
                !DeclSet, !IO),
            output_layout_exec_trace_decls(RttiProcLabel, ExecTrace,
                !DeclSet, !IO),
            output_layout_exec_trace(RttiProcLabel, ExecTrace, !DeclSet, !IO)
        ;
            MaybeExecTrace = no
        ),

        output_proc_layout_data_defn_start(RttiProcLabel, Kind, Traversal,
            !IO),
        Origin = RttiProcLabel ^ pred_info_origin,
        output_layout_proc_id_group(ProcLabel, Origin, !IO),
        (
            MaybeExecTrace = no,
            io.write_string("NULL,\n", !IO)
        ;
            MaybeExecTrace = yes(_),
            io.write_string("&", !IO),
            output_layout_name(proc_layout_exec_trace(RttiProcLabel), !IO),
            io.write_string(",\n", !IO)
        ),
        (
            MaybeProcStatic = no,
            io.write_string("NULL\n", !IO)
        ;
            MaybeProcStatic = yes(_),
            io.write_string("&", !IO),
            output_layout_name(proc_static(RttiProcLabel), !IO),
            io.write_string("\n", !IO)
        ),
        output_proc_layout_data_defn_end(!IO)
    ),
    DeclId = decl_data_addr(layout_addr(proc_layout(RttiProcLabel, Kind))),
    decl_set_insert(DeclId, !DeclSet).

:- func maybe_proc_layout_and_more_kind(maybe_proc_id_and_more,
    proc_label) = proc_layout_kind.

maybe_proc_layout_and_more_kind(MaybeRest, ProcLabel) = Kind :-
    (
        MaybeRest = no_proc_id,
        Kind = proc_layout_traversal
    ;
        MaybeRest = proc_id(_, _),
        Kind = proc_layout_proc_id(proc_label_user_or_uci(ProcLabel))
    ).

proc_label_user_or_uci(ordinary_proc_label(_, _, _, _, _, _)) = user.
proc_label_user_or_uci(special_proc_label(_, _, _, _, _, _)) = uci.

:- pred output_proc_layout_data_defn_start(rtti_proc_label::in,
    proc_layout_kind::in, proc_layout_stack_traversal::in,
    io::di, io::uo) is det.

output_proc_layout_data_defn_start(RttiProcLabel, Kind, Traversal, !IO) :-
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(proc_layout(RttiProcLabel, Kind),
        yes, !IO),
    io.write_string(" = {\n", !IO),
    output_layout_traversal_group(Traversal, !IO).

:- pred output_proc_layout_data_defn_end(io::di, io::uo) is det.

output_proc_layout_data_defn_end(!IO) :-
    io.write_string("};\n", !IO).

:- pred output_layout_traversal_decls(proc_layout_stack_traversal::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_layout_traversal_decls(Traversal, !DeclSet, !IO) :-
    Traversal = proc_layout_stack_traversal(MaybeEntryLabel, _MaybeSuccipSlot,
        _StackSlotCount, _Detism),
    (
        MaybeEntryLabel = yes(EntryLabel),
        output_code_addr_decls(code_label(EntryLabel), !DeclSet, !IO)
    ;
        MaybeEntryLabel = no
    ).

:- pred output_layout_traversal_group(proc_layout_stack_traversal::in,
    io::di, io::uo) is det.

output_layout_traversal_group(Traversal, !IO) :-
    Traversal = proc_layout_stack_traversal(MaybeEntryLabel, MaybeSuccipSlot,
        StackSlotCount, Detism),
    io.write_string("{\n", !IO),
    (
        MaybeEntryLabel = yes(EntryLabel),
        output_code_addr(code_label(EntryLabel), !IO)
    ;
        MaybeEntryLabel = no,
        % The actual code address will be put into the structure
        % by module initialization code.
        io.write_string("NULL", !IO)
    ),
    io.write_string(",\n{ ", !IO),
    (
        MaybeSuccipSlot = yes(SuccipSlot),
        io.write_int(SuccipSlot, !IO)
    ;
        MaybeSuccipSlot = no,
        io.write_int(-1, !IO)
    ),
    io.write_string(" },\n", !IO),
    io.write_int(StackSlotCount, !IO),
    io.write_string(",\n", !IO),
    io.write_string(detism_to_c_detism(Detism), !IO),
    io.write_string("\n},\n", !IO).

:- func detism_to_c_detism(determinism) = string.

detism_to_c_detism(detism_det) =       "MR_DETISM_DET".
detism_to_c_detism(detism_semi) =      "MR_DETISM_SEMI".
detism_to_c_detism(detism_non) =       "MR_DETISM_NON".
detism_to_c_detism(detism_multi) =     "MR_DETISM_MULTI".
detism_to_c_detism(detism_erroneous) = "MR_DETISM_ERRONEOUS".
detism_to_c_detism(detism_failure) =   "MR_DETISM_FAILURE".
detism_to_c_detism(detism_cc_non) =    "MR_DETISM_CCNON".
detism_to_c_detism(detism_cc_multi) =  "MR_DETISM_CCMULTI".

:- pred output_layout_proc_id_group(proc_label::in, pred_origin::in,
    io::di, io::uo) is det.

output_layout_proc_id_group(ProcLabel, Origin, !IO) :-
    io.write_string("{\n", !IO),
    output_proc_id(ProcLabel, Origin, !IO),
    io.write_string("},\n", !IO).

:- pred output_layout_no_proc_id_group(io::di, io::uo) is det.

output_layout_no_proc_id_group(!IO) :-
    io.write_string("-1\n", !IO).

:- pred output_layout_exec_trace_decls(rtti_proc_label::in,
    proc_layout_exec_trace::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_layout_exec_trace_decls(RttiProcLabel, ExecTrace, !DeclSet, !IO) :-
    ExecTrace = proc_layout_exec_trace(MaybeCallLabelLayout, _ProcBodyBytes,
        MaybeTableInfo, _HeadVarNums, _VarNames, _MaxVarNum,
        _MaxRegNum, _MaybeFromFullSlot, _MaybeIoSeqSlot,
        _MaybeTrailSlot, _MaybeMaxfrSlot, _EvalMethod,
        _MaybeCallTableSlot, _EffTraceLevel, _Flags),
    ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
    ModuleName = get_defining_module_name(ProcLabel) ,
    (
        MaybeCallLabelLayout = yes(CallLabelDetails),
        CallLabelDetails = label_layout_details(CallProcLabel, LabelNum,
            LabelVars),
        CallLabelLayout = label_layout(CallProcLabel, LabelNum, LabelVars),
        output_layout_decl(CallLabelLayout, !DeclSet, !IO)
    ;
        MaybeCallLabelLayout = no
    ),
    output_layout_decl(module_layout(ModuleName), !DeclSet, !IO),
    (
        MaybeTableInfo = yes(TableInfo),
        output_data_addr_decls(TableInfo, !DeclSet, !IO)
    ;
        MaybeTableInfo = no
    ).

    % The job of this predicate is to minimize stack space consumption in
    % grades that do not allow output_bytecodes to be tail recursive.
    %
:- pred output_bytecodes_driver(list(int)::in, io::di, io::uo) is det.

output_bytecodes_driver(Bytes, !IO) :-
    (
        Bytes = []
    ;
        Bytes = [_ | _],
        output_bytecodes(Bytes, BytesLeft, 0, 256, !IO),
        output_bytecodes_driver(BytesLeft, !IO)
    ).

:- pred output_bytecodes(list(int)::in, list(int)::out, int::in, int::in,
    io::di, io::uo) is det.

output_bytecodes(Bytes, BytesLeft, !.Seq, MaxSeq, !IO) :-
    (
        Bytes = [],
        BytesLeft = []
    ;
        Bytes = [Head | Tail],
        ( !.Seq < MaxSeq ->
            io.write_int(Head, !IO),
            io.write_char(',', !IO),
            !:Seq = !.Seq + 1,
            ( unchecked_rem(!.Seq, 16) = 0 ->
                io.write_char('\n', !IO)
            ;
                true
            ),
            output_bytecodes(Tail, BytesLeft, !.Seq, MaxSeq, !IO)
        ;
            BytesLeft = Bytes
        )
    ).

:- pred output_layout_exec_trace(rtti_proc_label::in,
    proc_layout_exec_trace::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_layout_exec_trace(RttiProcLabel, ExecTrace, !DeclSet, !IO) :-
    ExecTrace = proc_layout_exec_trace(MaybeCallLabelDetails, ProcBodyBytes,
        MaybeTableInfo, HeadVarNums, _VarNames, MaxVarNum,
        MaxRegNum, MaybeFromFullSlot, MaybeIoSeqSlot, MaybeTrailSlot,
        MaybeMaxfrSlot, EvalMethod, MaybeCallTableSlot, EffTraceLevel, Flags),
    (
        ProcBodyBytes = []
    ;
        ProcBodyBytes = [_ | _],
        io.write_string("\n", !IO),
        output_layout_name_storage_type_name(
            proc_layout_body_bytecode(RttiProcLabel), yes, !IO),
        io.write_string(" = {\n", !IO),
        output_bytecodes_driver(ProcBodyBytes, !IO),
        io.write_string("};\n", !IO)
    ),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(
        proc_layout_exec_trace(RttiProcLabel), yes, !IO),
    io.write_string(" = {\n", !IO),
    (
        MaybeCallLabelDetails = yes(CallLabelDetails),
        io.write_string("MR_LABEL_LAYOUT_REF(", !IO),
        CallLabelDetails = label_layout_details(CallProcLabel, CallLabelNum,
            _),
        output_label(internal_label(CallLabelNum, CallProcLabel), no, !IO),
        io.write_string("),\n", !IO)
    ;
        MaybeCallLabelDetails = no,
        io.write_string("NULL,\n", !IO)
    ),
    io.write_string("(const MR_ModuleLayout *) &", !IO),
    ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
    ModuleName = get_defining_module_name(ProcLabel),
    output_layout_name(module_layout(ModuleName), !IO),
    io.write_string(",\n", !IO),
    (
        ProcBodyBytes = [],
        io.write_string("NULL", !IO)
    ;
        ProcBodyBytes = [_ | _],
        output_layout_name(proc_layout_body_bytecode(RttiProcLabel), !IO)
    ),
    io.write_string(",\n", !IO),
    io.write_string("0,\n{ ", !IO),
    (
        MaybeTableInfo = yes(TableInfo),
        io.write_string("(const void *) &", !IO),
        output_data_addr(TableInfo, !IO)
    ;
        MaybeTableInfo = no,
        io.write_string("NULL", !IO)
    ),
    io.write_string(" },\n", !IO),
    output_layout_name(proc_layout_head_var_nums(RttiProcLabel), !IO),
    io.write_string(",\n", !IO),
    output_layout_name(proc_layout_var_names(RttiProcLabel), !IO),
    io.write_string(",\n", !IO),
    io.write_int(list.length(HeadVarNums), !IO),
    io.write_string(",\n", !IO),
    io.write_int(MaxVarNum, !IO),
    io.write_string(",\n", !IO),
    io.write_int(MaxRegNum, !IO),
    io.write_string(",\n", !IO),
    write_maybe_slot_num(MaybeFromFullSlot, !IO),
    io.write_string(",\n", !IO),
    write_maybe_slot_num(MaybeIoSeqSlot, !IO),
    io.write_string(",\n", !IO),
    write_maybe_slot_num(MaybeTrailSlot, !IO),
    io.write_string(",\n", !IO),
    write_maybe_slot_num(MaybeMaxfrSlot, !IO),
    io.write_string(",\n", !IO),
    io.write_string(eval_method_to_c_string(EvalMethod), !IO),
    io.write_string(",\n", !IO),
    write_maybe_slot_num(MaybeCallTableSlot, !IO),
    io.write_string(",\n", !IO),
    io.write_string(trace_level_rep(EffTraceLevel), !IO),
    io.write_string(",\n", !IO),
    io.write_int(Flags, !IO),
    io.write_string("\n};\n", !IO).

:- pred write_maybe_slot_num(maybe(int)::in, io::di, io::uo) is det.

write_maybe_slot_num(yes(SlotNum), !IO) :-
    io.write_int(SlotNum, !IO).
write_maybe_slot_num(no, !IO) :-
    io.write_int(-1, !IO).

:- func eval_method_to_c_string(eval_method) = string.

eval_method_to_c_string(eval_normal) =     "MR_EVAL_METHOD_NORMAL".
eval_method_to_c_string(eval_loop_check) = "MR_EVAL_METHOD_LOOP_CHECK".
eval_method_to_c_string(eval_memo) =       "MR_EVAL_METHOD_MEMO".
eval_method_to_c_string(eval_minimal(MinimalMethod)) = Str :-
    (
        MinimalMethod = stack_copy,
        Str = "MR_EVAL_METHOD_MINIMAL_STACK_COPY"
    ;
        MinimalMethod = own_stacks_consumer,
        Str = "MR_EVAL_METHOD_MINIMAL_OWN_STACKS_CONSUMER"
    ;
        MinimalMethod = own_stacks_generator,
        Str = "MR_EVAL_METHOD_MINIMAL_OWN_STACKS_GENERATOR"
    ).
eval_method_to_c_string(eval_table_io(Decl, Unitize)) = Str :-
    (
        Decl = table_io_proc,
        Unitize = table_io_alone,
        Str = "MR_EVAL_METHOD_TABLE_IO"
    ;
        Decl = table_io_proc,
        Unitize = table_io_unitize,
        Str = "MR_EVAL_METHOD_TABLE_IO_UNITIZE"
    ;
        Decl = table_io_decl,
        Unitize = table_io_alone,
        Str = "MR_EVAL_METHOD_TABLE_IO_DECL"
    ;
        Decl = table_io_decl,
        Unitize = table_io_unitize,
        Str = "MR_EVAL_METHOD_TABLE_IO_UNITIZE_DECL"
    ).

:- pred output_proc_layout_head_var_nums(rtti_proc_label::in, list(int)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_proc_layout_head_var_nums(ProcLabel, HeadVarNums, !DeclSet, !IO) :-
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(proc_layout_head_var_nums(ProcLabel),
        yes, !IO),
    io.write_string(" = {\n", !IO),
    (
        HeadVarNums = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string("0\n", !IO)
    ;
        HeadVarNums = [_ | _],
        list.foldl(output_number_in_vector, HeadVarNums, !IO)
    ),
    io.write_string("};\n", !IO),
    DeclId = decl_data_addr(layout_addr(proc_layout_head_var_nums(ProcLabel))),
    decl_set_insert(DeclId, !DeclSet).

:- pred output_proc_layout_var_names(rtti_proc_label::in, list(int)::in,
    int::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_proc_layout_var_names(ProcLabel, VarNames, MaxVarNum, !DeclSet, !IO) :-
    list.length(VarNames, VarNameCount),
    expect(unify(VarNameCount, MaxVarNum), this_file,
        "output_proc_layout_var_names: VarNameCount != MaxVarNum"),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(proc_layout_var_names(ProcLabel), yes,
        !IO),
    io.write_string(" = {\n", !IO),
    (
        VarNames = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string("0\n", !IO)
    ;
        VarNames = [_ | _],
        list.foldl(output_number_in_vector, VarNames, !IO)
    ),
    io.write_string("};\n", !IO),
    DeclId = decl_data_addr(layout_addr(proc_layout_var_names(ProcLabel))),
    decl_set_insert(DeclId, !DeclSet).

%-----------------------------------------------------------------------------%

:- pred output_closure_proc_id_data_defn(proc_label::in, int::in,
    proc_label::in, module_name::in, string::in, int::in, pred_origin::in,
    string::in, decl_set::in, decl_set::out, io::di, io::uo) is det.

output_closure_proc_id_data_defn(CallerProcLabel, SeqNo, ClosureProcLabel,
        ModuleName, FileName, LineNumber, PredOrigin, GoalPath,
        !DeclSet, !IO) :-
    io.write_string("\n", !IO),
    LayoutName = closure_proc_id(CallerProcLabel, SeqNo, ClosureProcLabel),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {\n{\n", !IO),
    output_proc_id(ClosureProcLabel, PredOrigin, !IO),
    io.write_string("},\n", !IO),
    quote_and_write_string(sym_name_to_string(ModuleName), !IO),
    io.write_string(",\n", !IO),
    quote_and_write_string(FileName, !IO),
    io.write_string(",\n", !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(",\n", !IO),
    quote_and_write_string(GoalPath, !IO),
    io.write_string("\n};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_proc_id(proc_label::in, pred_origin::in, io::di, io::uo) is det.

output_proc_id(ProcLabel, Origin, !IO) :-
    (
        ProcLabel = ordinary_proc_label(DefiningModule, PredOrFunc,
            DeclaringModule, PredName0, Arity, Mode),
        PredName = origin_name(Origin, PredName0),
        output_pred_or_func(PredOrFunc, !IO),
        io.write_string(",\n", !IO),
        quote_and_write_string(sym_name_to_string(DeclaringModule), !IO),
        io.write_string(",\n", !IO),
        quote_and_write_string(sym_name_to_string(DefiningModule), !IO),
        io.write_string(",\n", !IO),
        quote_and_write_string(PredName, !IO),
        io.write_string(",\n", !IO),
        io.write_int(Arity, !IO),
        io.write_string(",\n", !IO),
        io.write_int(Mode, !IO),
        io.write_string("\n", !IO)
    ;
        ProcLabel = special_proc_label(DefiningModule, SpecialPredId,
            TypeModule, TypeName, TypeArity, Mode),
        TypeCtor = type_ctor(qualified(TypeModule, TypeName), TypeArity),
        PredName0 = special_pred_name(SpecialPredId, TypeCtor),
        PredName = origin_name(Origin, PredName0),
        quote_and_write_string(TypeName, !IO),
        io.write_string(",\n", !IO),
        quote_and_write_string(sym_name_to_string(TypeModule), !IO),
        io.write_string(",\n", !IO),
        quote_and_write_string(sym_name_to_string(DefiningModule), !IO),
        io.write_string(",\n", !IO),
        quote_and_write_string(PredName, !IO),
        io.write_string(",\n", !IO),
        io.write_int(TypeArity, !IO),
        io.write_string(",\n", !IO),
        io.write_int(Mode, !IO),
        io.write_string("\n", !IO)
    ).

:- func origin_name(pred_origin, string) = string.

origin_name(Origin, Name0) = Name :-
    (
        Origin = origin_lambda(FileName0, LineNum, SeqNo),
        ( string.append("IntroducedFrom", _, Name0) ->
            string.replace_all(FileName0, ".", "_", FileName),
            ( SeqNo > 1 ->
                string.format("lambda%d_%s_%d",
                    [i(SeqNo), s(FileName), i(LineNum)], Name)
            ;
                string.format("lambda_%s_%d", [s(FileName), i(LineNum)], Name)
            )
        ;
            % If the lambda pred has a meaningful name, use it.
            % This happens when the lambda is a partial application
            % that happens to supply zero arguments.
            Name = Name0
        )
    ;
        Origin = origin_special_pred(_SpecialPredId - _TypeCtor),
        Name = Name0
        % We can't use the following code until we have adapted the
        % code in the runtime and trace directories to handle the names
        % of special preds the same way as we do user-defined names.
%       (
%           SpecialPredId = unify,
%           SpecialName = "unify"
%       ;
%           SpecialPredId = compare,
%           SpecialName = "compare"
%       ;
%           SpecialPredId = index,
%           SpecialName = "index"
%       ;
%           SpecialPredId = initialise,
%           SpecialName = "init"
%       ),
%       TypeCtor = TypeSymName - TypeArity,
%       TypeName = sym_name_to_string(TypeSymName),
%       string.format("%s_for_%s_%d",
%           [s(SpecialName), s(TypeName), i(TypeArity)], Name)
    ;
        Origin = origin_transformed(Transform, OldOrigin, _),
        OldName = origin_name(OldOrigin, ""),
        ( OldName = "" ->
            Name = Name0
        ;
            Name = OldName ++ "_" ++ pred_transform_name(Transform)
        )
    ;
        ( Origin = origin_instance_method(_)
        ; Origin = origin_created(_)
        ; Origin = origin_assertion(_, _)
        ; Origin = origin_user(_)
        ),
        Name = Name0
    ).

:- func pred_transform_name(pred_transformation) = string.

pred_transform_name(transform_higher_order_specialization(Seq)) =
    "ho" ++ int_to_string(Seq).
pred_transform_name(transform_higher_order_type_specialization(Proc)) =
    "hoproc" ++ int_to_string(Proc).
pred_transform_name(transform_type_specialization(Substs)) =
    string.join_list("_", list.map(subst_to_name, Substs)).
pred_transform_name(transform_unused_argument_elimination(Posns)) =
    "ua_" ++ string.join_list("_", list.map(int_to_string, Posns)).
pred_transform_name(transform_accumulator(Posns)) = "acc_" ++
    string.join_list("_", list.map(int_to_string, Posns)).
pred_transform_name(transform_loop_invariant(Proc)) =
    "inv_" ++ int_to_string(Proc).
pred_transform_name(transform_tuple(Proc)) = "tup_" ++ int_to_string(Proc).
pred_transform_name(transform_untuple(Proc)) = "untup_" ++ int_to_string(Proc).
pred_transform_name(transform_dependent_parallel_conjunction) =
    "dep_par_conj_".
pred_transform_name(transform_return_via_ptr(ProcId, ArgPos)) =
    "retptr_" ++ int_to_string(proc_id_to_int(ProcId)) ++ "_args"
        ++ ints_to_string(ArgPos).
pred_transform_name(transform_table_generator) = "table_gen".
pred_transform_name(transform_dnf(N)) = "dnf_" ++ int_to_string(N).

:- func ints_to_string(list(int)) = string.

ints_to_string([]) = "".
ints_to_string([N | Ns]) = "_" ++ int_to_string(N) ++ ints_to_string(Ns).

:- func subst_to_name(pair(int, mer_type)) = string.

subst_to_name(TVar - Type) =
    string.format("%d/%s",
        [i(TVar), s(mercury_type_to_string(varset.init, no, Type))]).

%-----------------------------------------------------------------------------%

    % The version of the layout data structures -- useful for bootstrapping.
    % If you write runtime code that checks this version number and can
    % at least handle the previous version of the data structure,
    % it makes it easier to bootstrap changes to these data structures.
    %
    % This number should be kept in sync with MR_LAYOUT_VERSION in
    % runtime/mercury_stack_layout.h.  This means you need to update
    % the code in the runtime (including the trace directory) that uses
    % layout structures to conform to whatever changes the new version
    % introduces.
    %
:- func layout_version_number = int.

layout_version_number = 1.

:- pred output_module_layout_data_defn(module_name::in, int::in,
    string_with_0s::in, list(layout_name)::in, list(file_layout_data)::in,
    trace_level::in, int::in, int::in, maybe(event_set_layout_data)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_module_layout_data_defn(ModuleName, StringTableSize, StringTable,
        ProcLayoutNames, FileLayouts, TraceLevel, SuppressedEvents,
        NumLabels, MaybeEventSetLayout, !DeclSet, !IO) :-
    output_module_string_table(ModuleName, StringTableSize, StringTable,
        !DeclSet, !IO),
    output_module_layout_proc_vector_defn(ModuleName, ProcLayoutNames,
        ProcVectorName, !DeclSet, !IO),
    output_file_layout_data_defns(ModuleName, 0, FileLayouts,
        FileLayoutNames, !DeclSet, !IO),
    output_file_layout_vector_data_defn(ModuleName, FileLayoutNames,
        FileVectorName, !DeclSet, !IO),

    io.write_string("\n", !IO),
    LabelExecCountName = module_layout_label_exec_count(ModuleName,
        NumLabels),
    output_layout_name_storage_type_name(LabelExecCountName, yes, !IO),
    io.write_string(";\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LabelExecCountName)), !DeclSet),

    (
        MaybeEventSetLayout = no
    ;
        MaybeEventSetLayout = yes(EventSetDataLayout),
        EventSetDataLayout =
            event_set_layout_data(EventSetDataA, TypesRvalMap),
        EventSetDataA = event_set_data(_EventSetName, EventSetDesc,
            EventSpecsA, _MaxNumAttr),
        output_event_set_desc_defn(ModuleName, EventSetDesc, !DeclSet, !IO),
        output_event_specs_and_components(EventSpecsA, ModuleName,
            TypesRvalMap, !DeclSet, !IO)
    ),

    ModuleLayoutName = module_layout(ModuleName),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(ModuleLayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    io.write_int(layout_version_number, !IO),
    io.write_string(",\n", !IO),
    quote_and_write_string(sym_name_to_string(ModuleName), !IO),
    io.write_string(",\n", !IO),
    io.write_int(StringTableSize, !IO),
    io.write_string(",\n", !IO),
    ModuleStringTableName = module_layout_string_table(ModuleName),
    output_layout_name(ModuleStringTableName, !IO),
    io.write_string(",\n", !IO),
    list.length(ProcLayoutNames, ProcLayoutVectorLength),
    io.write_int(ProcLayoutVectorLength, !IO),
    io.write_string(",\n", !IO),
    output_layout_name(ProcVectorName, !IO),
    io.write_string(",\n", !IO),
    list.length(FileLayouts, FileLayoutVectorLength),
    io.write_int(FileLayoutVectorLength, !IO),
    io.write_string(",\n", !IO),
    output_layout_name(FileVectorName, !IO),
    io.write_string(",\n", !IO),
    io.write_string(trace_level_rep(TraceLevel), !IO),
    io.write_string(",\n", !IO),
    io.write_int(SuppressedEvents, !IO),
    io.write_string(",\n", !IO),
    io.write_int(NumLabels, !IO),
    io.write_string(",\n", !IO),
    output_layout_name(LabelExecCountName, !IO),
    io.write_string(",\n", !IO),
    (
        MaybeEventSetLayout = no,
        io.write_string("NULL,\n", !IO),
        io.write_string("NULL,\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("NULL", !IO)
    ;
        MaybeEventSetLayout = yes(EventSetDataLayoutB),
        EventSetDataLayoutB =
            event_set_layout_data(EventSetDataB, _TypesRvalMap),
        EventSetDataB = event_set_data(EventSetName, _EventSetDesc,
            EventSpecsB, MaxNumAttr),
        quote_and_write_string(EventSetName, !IO),
        io.write_string(",\n", !IO),
        EventSetDescLayoutName = module_layout_event_set_desc(ModuleName),
        output_layout_name(EventSetDescLayoutName, !IO),
        io.write_string(",\n", !IO),
        io.write_int(MaxNumAttr, !IO),
        io.write_string(",\n", !IO),
        io.write_int(list.length(EventSpecsB), !IO),
        io.write_string(",\n", !IO),
        EventSpecLayoutName = module_layout_event_specs(ModuleName),
        output_layout_name(EventSpecLayoutName, !IO)
    ),
    io.write_string("\n};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(ModuleLayoutName)), !DeclSet).

:- pred output_event_specs_and_components(list(event_spec)::in,
    module_name::in, map(int, rval)::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_event_specs_and_components(EventSpecs, ModuleName, TypesRvalMap,
        !DeclSet, !IO) :-
    list.foldl2(output_event_spec_components(ModuleName), EventSpecs,
        !DeclSet, !IO),

    LayoutName = module_layout_event_specs(ModuleName),
    DataAddr = layout_addr(LayoutName),
    decl_set_insert(decl_data_addr(DataAddr), !DeclSet),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    io.write_list(EventSpecs, ",\n",
        output_event_spec(ModuleName, TypesRvalMap), !IO),
    io.write_string("\n};\n\n", !IO).

:- pred output_event_spec_components(module_name::in, event_spec::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_event_spec_components(ModuleName, EventSpec, !DeclSet, !IO) :-
    EventSpec = event_spec(EventNumber, _EventName, _EventLineNumber,
        Attrs, SynthOrder),

    AttrNamesLayoutName =
        module_layout_event_arg_names(ModuleName, EventNumber),
    AttrNamesDataAddr = layout_addr(AttrNamesLayoutName),
    decl_set_insert(decl_data_addr(AttrNamesDataAddr), !DeclSet),
    output_layout_name_storage_type_name(AttrNamesLayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    io.write_list(Attrs, ", ", output_attr_name, !IO),
    io.write_string("\n};\n\n", !IO),

    (
        SynthOrder = []
    ;
        SynthOrder = [_ | _],

        list.foldl2(output_synth_attr_args(ModuleName, EventNumber),
            Attrs, !DeclSet, !IO),

        SynthAttrsLayoutName =
            module_layout_event_synth_attrs(ModuleName, EventNumber),
        SynthAttrsDataAddr = layout_addr(SynthAttrsLayoutName),
        decl_set_insert(decl_data_addr(SynthAttrsDataAddr), !DeclSet),
        output_layout_name_storage_type_name(SynthAttrsLayoutName, yes, !IO),
        io.write_string(" = {\n", !IO),
        io.write_list(Attrs, ",\n",
            output_synth_attr(ModuleName, EventNumber), !IO),
        io.write_string("\n};\n\n", !IO),

        SynthOrderLayoutName =
            module_layout_event_synth_order(ModuleName, EventNumber),
        SynthOrderDataAddr = layout_addr(SynthOrderLayoutName),
        decl_set_insert(decl_data_addr(SynthOrderDataAddr), !DeclSet),
        output_layout_name_storage_type_name(SynthOrderLayoutName, yes, !IO),
        io.write_string(" = {\n", !IO),
        % The -1 acts as sentinel.
        io.write_list(SynthOrder ++ [-1], ", ", io.write_int, !IO),
        io.write_string("\n};\n\n", !IO)
    ).

:- pred output_attr_name(event_attribute::in, io::di, io::uo) is det.

output_attr_name(Attr, !IO) :-
    io.write_string("""", !IO),
    io.write_string(Attr ^ attr_name, !IO),
    io.write_string("""", !IO).

:- pred output_synth_attr_args(module_name::in, int::in, event_attribute::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_synth_attr_args(ModuleName, EventNumber, Attr, !DeclSet, !IO) :-
    MaybeSynthCall = Attr ^ attr_maybe_synth_call,
    (
        MaybeSynthCall = yes(SynthCall),
        SynthCall = event_attr_synth_call(_FuncAttrNameNum, ArgAttrNameNums,
            Order),
        assoc_list.values(ArgAttrNameNums, ArgAttrNums),
        AttrNumber = Attr ^ attr_num,

        ArgsLayoutName = module_layout_event_synth_attr_args(ModuleName,
            EventNumber, AttrNumber),
        ArgsDataAddr = layout_addr(ArgsLayoutName),
        decl_set_insert(decl_data_addr(ArgsDataAddr), !DeclSet),
        output_layout_name_storage_type_name(ArgsLayoutName, yes, !IO),
        io.write_string(" =\n{ ", !IO),
        io.write_list(ArgAttrNums, ", ", io.write_int, !IO),
        io.write_string(" };\n\n", !IO),

        OrderLayoutName = module_layout_event_synth_attr_order(ModuleName,
            EventNumber, AttrNumber),
        OrderDataAddr = layout_addr(OrderLayoutName),
        decl_set_insert(decl_data_addr(OrderDataAddr), !DeclSet),
        output_layout_name_storage_type_name(OrderLayoutName, yes, !IO),
        io.write_string(" =\n{ ", !IO),
        OrderSentinel = Order ++ [-1],
        io.write_list(OrderSentinel, ", ", io.write_int, !IO),
        io.write_string(" };\n\n", !IO)
    ;
        MaybeSynthCall = no
    ).

:- pred output_synth_attr(module_name::in, int::in, event_attribute::in,
    io::di, io::uo) is det.

output_synth_attr(ModuleName, EventNumber, Attr, !IO) :-
    io.write_string("{ ", !IO),
    MaybeSynthCall = Attr ^ attr_maybe_synth_call,
    (
        MaybeSynthCall = yes(SynthCall),
        SynthCall = event_attr_synth_call(_FuncAttrName - FuncAttrNum,
            ArgAttrNameNums, _EvalOrder),
        io.write_int(FuncAttrNum, !IO),
        io.write_string(", ", !IO),
        io.write_int(list.length(ArgAttrNameNums), !IO),
        io.write_string(",\n ", !IO),

        AttrNumber = Attr ^ attr_num,
        ArgsLayoutName = module_layout_event_synth_attr_args(ModuleName,
            EventNumber, AttrNumber),
        output_layout_name(ArgsLayoutName, !IO),
        io.write_string(",\n ", !IO),
        OrderLayoutName = module_layout_event_synth_attr_order(ModuleName,
            EventNumber, AttrNumber),
        output_layout_name(OrderLayoutName, !IO)
    ;
        MaybeSynthCall = no,
        io.write_string("-1, -1, NULL, NULL", !IO)
    ),
    io.write_string(" }", !IO).

:- pred output_event_spec(module_name::in, map(int, rval)::in, event_spec::in,
    io::di, io::uo) is det.

output_event_spec(ModuleName, TypesRvalMap, EventSpec, !IO) :-
    EventSpec = event_spec(EventNumber, EventName, _EventLineNumber, Attrs,
        SynthOrder),
    map.lookup(TypesRvalMap, EventNumber, TypesRval),

    io.write_string("{ """, !IO),
    io.write_string(EventName, !IO),
    io.write_string(""", ", !IO),
    io.write_int(list.length(Attrs), !IO),
    io.write_string(",\n\t", !IO),

    AttrNamesLayoutName =
        module_layout_event_arg_names(ModuleName, EventNumber),
    output_layout_name(AttrNamesLayoutName, !IO),
    io.write_string(",\n\t(MR_TypeInfo *) ", !IO),
    output_rval_as_addr(TypesRval, !IO),
    io.write_string(",\n\t", !IO),

    (
        SynthOrder = [],
        io.write_string("NULL, NULL }", !IO)
    ;
        SynthOrder = [_ | _],
        SynthAttrsLayoutName =
            module_layout_event_synth_attrs(ModuleName, EventNumber),
        SynthOrderLayoutName =
            module_layout_event_synth_order(ModuleName, EventNumber),
        output_layout_name(SynthAttrsLayoutName, !IO),
        io.write_string(",\n\t", !IO),
        output_layout_name(SynthOrderLayoutName, !IO),
        io.write_string(" }", !IO)
    ).

:- pred output_module_layout_proc_vector_defn(module_name::in,
    list(layout_name)::in, layout_name::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_module_layout_proc_vector_defn(ModuleName, ProcLayoutNames,
        VectorName, !DeclSet, !IO) :-
    list.foldl2(output_layout_decl, ProcLayoutNames, !DeclSet, !IO),
    VectorName = module_layout_proc_vector(ModuleName),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(VectorName, yes, !IO),
    io.write_string(" = {\n", !IO),
    (
        ProcLayoutNames = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string("NULL\n", !IO)
    ;
        ProcLayoutNames = [_ | _],
        list.foldl(output_proc_layout_name_in_vector, ProcLayoutNames, !IO)
    ),
    io.write_string("};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(VectorName)), !DeclSet).

:- pred output_proc_layout_name_in_vector(layout_name::in, io::di, io::uo)
    is det.

output_proc_layout_name_in_vector(LayoutName, !IO) :-
    ( LayoutName = proc_layout(RttiProcLabel, _) ->
        ProcLabel = make_proc_label_from_rtti(RttiProcLabel),
        io.write_string("MR_PROC_LAYOUT1(", !IO),
        output_proc_label_no_prefix(ProcLabel, !IO),
        io.write_string(")\n", !IO)
    ;
        unexpected(this_file,
            "output_proc_layout_name_in_vector: not proc layout")
    ).

%-----------------------------------------------------------------------------%

:- pred output_event_set_desc_defn(module_name::in, string::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_event_set_desc_defn(ModuleName, EventSetDesc, !DeclSet, !IO) :-
    LayoutName = module_layout_event_set_desc(ModuleName),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {", !IO),
    string.length(EventSetDesc, EventSetDescSize),
    output_module_string_table_chars_driver(0, EventSetDescSize - 1,
        string_with_0s(EventSetDesc), !IO),
    io.write_string("};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

    % The string table cannot be zero size; it must contain at least an
    % empty string.
    %
:- pred output_module_string_table(module_name::in,
    int::in, string_with_0s::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_module_string_table(ModuleName, StringTableSize, StringTable,
        !DeclSet, !IO) :-
    TableName = module_layout_string_table(ModuleName),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(TableName, yes, !IO),
    io.write_string(" = {", !IO),
    output_module_string_table_chars_driver(0, StringTableSize - 1,
        StringTable, !IO),
    io.write_string("};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(TableName)), !DeclSet).

    % The job of this predicate is to minimize stack space consumption in
    % grades that do not allow output_module_string_table_chars to be tail
    % recursive. The maximum observed size of the module string so far has
    % been just short of 64 kilobytes; writing that out in 256 batches of
    % 256 characters minimizes maximum total stack requirements.
    %
:- pred output_module_string_table_chars_driver(int::in, int::in,
    string_with_0s::in, io::di, io::uo) is det.

output_module_string_table_chars_driver(CurIndex, MaxIndex, StringWithNulls,
        !IO) :-
    ( CurIndex < MaxIndex ->
        SubMaxIndex = int.min(MaxIndex, CurIndex + 255),
        output_module_string_table_chars(CurIndex, SubMaxIndex,
            StringWithNulls, !IO),
        output_module_string_table_chars_driver(SubMaxIndex + 1, MaxIndex,
            StringWithNulls, !IO)
    ;
        true
    ).

:- pred output_module_string_table_chars(int::in, int::in, string_with_0s::in,
    io::di, io::uo) is det.

output_module_string_table_chars(CurIndex, MaxIndex, StringWithNulls, !IO) :-
    ( CurIndex mod 10 = 0 ->
        io.write_string("\n", !IO)
    ;
        true
    ),
    StringWithNulls = string_with_0s(String),
    string.unsafe_index(String, CurIndex, Char),
    io.write_char('''', !IO),
    c_util.output_quoted_char(Char, !IO),
    io.write_char('''', !IO),
    io.write_string(", ", !IO),
    ( CurIndex < MaxIndex ->
        output_module_string_table_chars(CurIndex + 1, MaxIndex,
            StringWithNulls, !IO)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred output_file_layout_vector_data_defn(module_name::in,
    list(layout_name)::in, layout_name::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_file_layout_vector_data_defn(ModuleName, FileLayoutNames, VectorName,
        !DeclSet, !IO) :-
    list.foldl2(output_layout_decl, FileLayoutNames, !DeclSet, !IO),
    VectorName = module_layout_file_vector(ModuleName),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(VectorName, yes, !IO),
    io.write_string(" = {\n", !IO),
    (
        FileLayoutNames = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string("NULL\n", !IO)
    ;
        FileLayoutNames = [_ | _],
        list.foldl(output_layout_name_in_vector("&"), FileLayoutNames, !IO)
    ),
    io.write_string("};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(VectorName)), !DeclSet).

:- pred output_file_layout_data_defns(module_name::in, int::in,
    list(file_layout_data)::in, list(layout_name)::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_data_defns(_, _, [], [], !DeclSet, !IO).
output_file_layout_data_defns(ModuleName, FileNum, [FileLayout | FileLayouts],
        [FileLayoutName | FileLayoutNames], !DeclSet, !IO) :-
    output_file_layout_data_defn(ModuleName, FileNum, FileLayout,
        FileLayoutName, !DeclSet, !IO),
    output_file_layout_data_defns(ModuleName, FileNum + 1, FileLayouts,
        FileLayoutNames, !DeclSet, !IO).

:- pred output_file_layout_data_defn(module_name::in, int::in,
    file_layout_data::in, layout_name::out,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_file_layout_data_defn(ModuleName, FileNum, FileLayout, FileLayoutName,
        !DeclSet, !IO) :-
    FileLayout = file_layout_data(FileName, LineNoLabelList),
    list.map2(line_no_label_to_label_layout_addr, LineNoLabelList,
        LineNos, LabelLayoutAddrs),
    list.foldl2(output_data_addr_decls, LabelLayoutAddrs, !DeclSet, !IO),

    list.length(LineNoLabelList, VectorLengths),
    output_file_layout_line_number_vector_defn(ModuleName, FileNum,
        LineNos, LineNumberVectorName, !DeclSet, !IO),
    output_file_layout_label_layout_vector_defn(ModuleName, FileNum,
        LabelLayoutAddrs, LabelVectorName, !DeclSet, !IO),

    FileLayoutName = file_layout(ModuleName, FileNum),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(FileLayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    quote_and_write_string(FileName, !IO),
    io.write_string(",\n", !IO),
    io.write_int(VectorLengths, !IO),
    io.write_string(",\n", !IO),
    output_layout_name(LineNumberVectorName, !IO),
    io.write_string(",\n", !IO),
    output_layout_name(LabelVectorName, !IO),
    io.write_string("\n};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(FileLayoutName)), !DeclSet).

:- pred output_file_layout_line_number_vector_defn(module_name::in, int::in,
    list(int)::in, layout_name::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_file_layout_line_number_vector_defn(ModuleName, FileNum, LineNumbers,
        LayoutName, !DeclSet, !IO) :-
    LayoutName = file_layout_line_number_vector(ModuleName, FileNum),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    (
        LineNumbers = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string("0\n", !IO)
    ;
        LineNumbers = [_ | _],
        list.foldl(output_number_in_vector, LineNumbers, !IO)
    ),
    io.write_string("};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_file_layout_label_layout_vector_defn(module_name::in, int::in,
    list(data_addr)::in, layout_name::out, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_file_layout_label_layout_vector_defn(ModuleName, FileNum, LabelAddrs,
        LayoutName, !DeclSet, !IO) :-
    LayoutName = file_layout_label_layout_vector(ModuleName, FileNum),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    (
        LabelAddrs = [],
        % ANSI/ISO C doesn't allow empty arrays, so place a dummy value
        % in the array.
        io.write_string("NULL\n", !IO)
    ;
        LabelAddrs = [_ | _],
        list.map(project_label_layout, LabelAddrs, Labels),
        output_label_layout_addrs_in_vector(Labels, !IO)
    ),
    io.write_string("};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred project_label_layout(data_addr::in, label::out) is det.

project_label_layout(DataAddr, Label) :-
    (
        DataAddr = layout_addr(LayoutName),
        LayoutName = label_layout(ProcLabel, LabelNum, _)
    ->
        Label = internal_label(LabelNum, ProcLabel)
    ;
        unexpected(this_file, "project_label_layout: not label layout")
    ).

:- pred output_label_layout_addrs_in_vector(list(label)::in, io::di, io::uo)
    is det.

output_label_layout_addrs_in_vector([], !IO).
output_label_layout_addrs_in_vector([Label | Labels], !IO) :-
    (
        Label = internal_label(LabelNum, ProcLabel),
        groupable_labels(ProcLabel, 1, _N, [LabelNum], RevLabelNums,
            Labels, RemainingLabels)
    ->
        list.reverse(RevLabelNums, LabelNums),
        io.write_string("MR_LABEL_LAYOUT", !IO),
        io.write_int(list.length(LabelNums), !IO),
        io.write_string("(", !IO),
        output_proc_label_no_prefix(ProcLabel, !IO),
        io.write_string(",", !IO),
        io.write_list(LabelNums, ",", io.write_int, !IO),
        io.write_string(")\n", !IO),
        output_label_layout_addrs_in_vector(RemainingLabels, !IO)
    ;
        io.write_string("MR_LABEL_LAYOUT(", !IO),
        output_label(Label, !IO),
        io.write_string("),\n", !IO),
        output_label_layout_addrs_in_vector(Labels, !IO)
    ).

:- pred groupable_labels(proc_label::in, int::in, int::out,
    list(int)::in, list(int)::out, list(label)::in, list(label)::out) is det.

groupable_labels(ProcLabel, !Count, !RevLabelsNums, !Labels) :-
    (
        % There must be a macro of the form MR_LABEL_LAYOUT<n>
        % for every <n> up to MaxChunkSize.
        !.Labels = [Label | !:Labels],
        MaxChunkSize = 9,
        !.Count < MaxChunkSize, % leave room for the one we're adding
        Label = internal_label(LabelNum, ProcLabel)
    ->
        !:Count = !.Count + 1,
        !:RevLabelsNums = [LabelNum | !.RevLabelsNums],
        groupable_labels(ProcLabel, !Count, !RevLabelsNums, !Labels)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred line_no_label_to_label_layout_addr(pair(int, layout_name)::in,
    int::out, data_addr::out) is det.

line_no_label_to_label_layout_addr(LineNo - LabelLayout, LineNo, DataAddr) :-
    DataAddr = layout_addr(LabelLayout).

:- pred quote_and_write_string(string::in, io::di, io::uo) is det.

quote_and_write_string(String, !IO) :-
    io.write_string("""", !IO),
    c_util.output_quoted_string(String, !IO),
    io.write_string("""", !IO).

:- pred output_number_in_vector(int::in, io::di, io::uo) is det.

output_number_in_vector(Num, !IO) :-
    io.write_int(Num, !IO),
    io.write_string(",\n", !IO).

:- pred output_layout_name_in_vector(string::in, layout_name::in,
    io::di, io::uo) is det.

output_layout_name_in_vector(Prefix, Name, !IO) :-
    io.write_string(Prefix, !IO),
    output_layout_name(Name, !IO),
    io.write_string(",\n", !IO).

:- pred output_data_addr_in_vector(string::in, data_addr::in,
    io::di, io::uo) is det.

output_data_addr_in_vector(Prefix, DataAddr, !IO) :-
    io.write_string(Prefix, !IO),
    output_data_addr(DataAddr, !IO),
    io.write_string(",\n", !IO).

%-----------------------------------------------------------------------------%

:- pred output_proc_static_data_defn(rtti_proc_label::in,
    proc_layout_proc_static::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_proc_static_data_defn(RttiProcLabel, ProcLayoutProcStatic,
        !DeclSet, !IO) :-
    ProcLayoutProcStatic = proc_layout_proc_static(HLDSProcStatic,
        DeepExcpVars),
    HLDSProcStatic = hlds_proc_static(FileName, LineNumber, IsInInterface,
        CallSites),
    list.foldl2(output_call_site_static_decl, CallSites, !DeclSet, !IO),
    output_call_site_static_array(RttiProcLabel, CallSites, !DeclSet, !IO),
    LayoutName = proc_static(RttiProcLabel),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    quote_and_write_string(FileName, !IO),
    io.write_string(",\n", !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(",\n", !IO),
    (
        IsInInterface = yes,
        io.write_string("MR_TRUE", !IO)
    ;
        IsInInterface = no,
        io.write_string("MR_FALSE", !IO)
    ),
    io.write_string(",\n", !IO),
    io.write_int(list.length(CallSites), !IO),
    io.write_string(",\n", !IO),
    CallSitesLayoutName = proc_static_call_sites(RttiProcLabel),
    output_layout_name(CallSitesLayoutName, !IO),
    io.write_string(",\n#ifdef MR_USE_ACTIVATION_COUNTS\n", !IO),
    io.write_string("0,\n", !IO),
    io.write_string("#endif\n", !IO),
    io.write_string("NULL,\n", !IO),
    DeepExcpVars = deep_excp_slots(TopCSDSlot, MiddleCSDSlot,
        OldOutermostSlot),
    io.write_int(TopCSDSlot, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(MiddleCSDSlot, !IO),
    io.write_string(",\n\t", !IO),
    io.write_int(OldOutermostSlot, !IO),
    io.write_string("\n};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_call_site_static_array(rtti_proc_label::in,
    list(call_site_static_data)::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_call_site_static_array(RttiProcLabel, CallSites, !DeclSet, !IO) :-
    LayoutName = proc_static_call_sites(RttiProcLabel),
    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {\n", !IO),
    list.foldl2(output_call_site_static, CallSites, 0, _, !IO),
    io.write_string("};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

:- pred output_call_site_static(call_site_static_data::in, int::in, int::out,
    io::di, io::uo) is det.

output_call_site_static(CallSiteStatic, Index, Index + 1, !IO) :-
    io.write_string("{ /* ", !IO),
    io.write_int(Index, !IO),
    io.write_string(" */ ", !IO),
    (
        CallSiteStatic = normal_call(Callee, TypeSubst, FileName, LineNumber,
            GoalPath),
        io.write_string("MR_normal_call, (MR_ProcLayout *)\n&", !IO),
        CalleeProcLabel = make_proc_label_from_rtti(Callee),
        CalleeUserOrUci = proc_label_user_or_uci(CalleeProcLabel),
        output_layout_name(proc_layout(Callee,
            proc_layout_proc_id(CalleeUserOrUci)), !IO),
        ( TypeSubst = "" ->
            io.write_string(", NULL, ", !IO)
        ;
            io.write_string(",\n""", !IO),
            io.write_string(TypeSubst, !IO),
            io.write_string(""", ", !IO)
        )
    ;
        CallSiteStatic = special_call(FileName, LineNumber, GoalPath),
        io.write_string("MR_special_call, NULL, NULL, ", !IO)
    ;
        CallSiteStatic = higher_order_call(FileName, LineNumber, GoalPath),
        io.write_string("MR_higher_order_call, NULL, NULL, ", !IO)
    ;
        CallSiteStatic = method_call(FileName, LineNumber, GoalPath),
        io.write_string("MR_method_call, NULL, NULL, ", !IO)
    ;
        CallSiteStatic = callback(FileName, LineNumber, GoalPath),
        io.write_string("MR_callback, NULL, NULL, ", !IO)
    ),
    io.write_string("""", !IO),
    io.write_string(FileName, !IO),
    io.write_string(""", ", !IO),
    io.write_int(LineNumber, !IO),
    io.write_string(", """, !IO),
    GoalPathStr = goal_path_to_string(GoalPath),
    io.write_string(GoalPathStr, !IO),
    io.write_string(""" },\n", !IO).

:- pred output_call_site_static_decl(call_site_static_data::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_call_site_static_decl(CallSiteStatic, !DeclSet, !IO) :-
    (
        CallSiteStatic = normal_call(Callee, _, _, _, _),
        CalleeProcLabel = make_proc_label_from_rtti(Callee),
        CalleeUserOrUci = proc_label_user_or_uci(CalleeProcLabel),
        output_maybe_layout_name_decl(proc_layout(Callee,
            proc_layout_proc_id(CalleeUserOrUci)), !DeclSet, !IO)
    ;
        CallSiteStatic = special_call(_, _, _)
    ;
        CallSiteStatic = higher_order_call(_, _, _)
    ;
        CallSiteStatic = method_call(_, _, _)
    ;
        CallSiteStatic = callback(_, _, _)
    ).

%-----------------------------------------------------------------------------%

:- pred output_table_io_decl(rtti_proc_label::in, proc_layout_kind::in,
    int::in, rval::in, rval::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

output_table_io_decl(RttiProcLabel, ProcLayoutKind, NumPTIs,
        PTIVectorRval, TypeParamsRval, !DeclSet, !IO) :-
    output_rval_decls(PTIVectorRval, !DeclSet, !IO),
    LayoutName = table_io_decl(RttiProcLabel),
    ProcLayoutName = proc_layout(RttiProcLabel, ProcLayoutKind),
    output_layout_decl(ProcLayoutName, !DeclSet, !IO),

    io.write_string("\n", !IO),
    output_layout_name_storage_type_name(LayoutName, yes, !IO),
    io.write_string(" = {\n(const MR_ProcLayout *) &", !IO),
    output_layout_name(ProcLayoutName, !IO),
    io.write_string(",\n", !IO),
    io.write_int(NumPTIs, !IO),
    io.write_string(",\n(const MR_PseudoTypeInfo *) ", !IO),
    output_rval(PTIVectorRval, !IO),
    io.write_string(",\n(const MR_TypeParamLocns *) ", !IO),
    output_rval(TypeParamsRval, !IO),
    io.write_string("\n};\n", !IO),
    decl_set_insert(decl_data_addr(layout_addr(LayoutName)), !DeclSet).

%-----------------------------------------------------------------------------%

output_pred_or_func(PredOrFunc, !IO) :-
    (
        PredOrFunc = predicate,
        io.write_string("MR_PREDICATE", !IO)
    ;
        PredOrFunc = function,
        io.write_string("MR_FUNCTION", !IO)
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "layout_out.m".

%-----------------------------------------------------------------------------%
