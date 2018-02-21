%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: llds_out_global.m.
% Main author: zs.
%
% This module outputs static data as C global variables.
%
%----------------------------------------------------------------------------%

:- module ll_backend.llds_out.llds_out_global.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ll_backend.llds.
:- import_module ll_backend.llds_out.llds_out_util.

:- import_module io.
:- import_module list.

:- pred output_complexity_arg_info_arrays(list(complexity_proc_info)::in,
    io::di, io::uo) is det.

:- pred output_init_complexity_proc_list(list(complexity_proc_info)::in,
    io::di, io::uo) is det.

:- pred output_tabling_info_struct(llds_out_info::in, tabling_info_struct::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_scalar_common_data_decl(scalar_common_data_array::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_vector_common_data_decl(vector_common_data_array::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

:- pred output_scalar_common_data_defn(llds_out_info::in,
    scalar_common_data_array::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

:- pred output_vector_common_data_defn(llds_out_info::in,
    vector_common_data_array::in, decl_set::in, decl_set::out,
    io::di, io::uo) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.c_util.
:- import_module backend_libs.name_mangle.
:- import_module backend_libs.rtti.
:- import_module hlds.hlds_pred.
:- import_module ll_backend.llds_out.llds_out_data.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module int64.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%
%
% Complexity structures.
%

:- func complexity_arg_info_array_name(int) = string.

complexity_arg_info_array_name(ProcNum) =
    "MR_complexity_arg_info_" ++ int_to_string(ProcNum).

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

output_init_complexity_proc_list([], !IO).
output_init_complexity_proc_list([Info | Infos], !IO) :-
    Info = complexity_proc_info(ProcNum, FullProcName, ArgInfos),
    io.write_string("\tMR_init_complexity_proc(", !IO),
    io.write_int(ProcNum, !IO),
    io.write_string(", """, !IO),
    c_util.output_quoted_string_cur_stream(FullProcName, !IO),
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
%
% Tabling structures.
%

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
    output_data_id(Info, InputStepsDataId, !IO),
    io.write_string(",\n", !IO),
    (
        MaybeOutputSteps = no,
        io.write_string("NULL\n", !IO)
    ;
        MaybeOutputSteps = yes(_),
        output_data_id(Info, OutputStepsDataId, !IO),
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
        output_data_id(Info, CallStatsDataId, !IO),
        io.write_string("\n", !IO),
        io.write_string("},{\n", !IO),
        io.write_string("0,\n", !IO),
        io.write_string("0,\n", !IO),
        output_data_id(Info, PrevCallStatsDataId, !IO),
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
            output_data_id(Info, AnswerStatsDataId, !IO),
            io.write_string("\n", !IO),
            io.write_string("},{\n", !IO),
            io.write_string("0,\n", !IO),
            io.write_string("0,\n", !IO),
            output_data_id(Info, PrevAnswerStatsDataId, !IO),
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
    c_util.output_quoted_string_cur_stream(VarName, !IO),
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
%
% Common cell declarations.
%

:- pred output_common_cell_type_name(type_num::in, io::di, io::uo) is det.

output_common_cell_type_name(type_num(TypeNum), !IO) :-
    io.write_string(mercury_common_type_prefix, !IO),
    io.write_int(TypeNum, !IO).

:- pred output_common_type_defn(type_num::in, common_cell_type::in,
    decl_set::in, decl_set::out, io::di, io::uo) is det.

output_common_type_defn(TypeNum, CellType, !DeclSet, !IO) :-
    TypeDeclId = decl_common_type(TypeNum),
    ( if decl_set_is_member(TypeDeclId, !.DeclSet) then
        true
    else
        output_pragma_pack_push(!IO),
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
        output_pragma_pack_pop(!IO),
        decl_set_insert(TypeDeclId, !DeclSet)
    ).

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
%
% Common cell definitions.
%

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
        Value = plain_value(TypedRvals),
        Rvals = typed_rvals_project_rvals(TypedRvals)
    ;
        Value = grouped_args_value(Groups),
        RvalLists = list.map(common_group_get_rvals, Groups),
        list.condense(RvalLists, Rvals)
    ).

:- func common_group_get_rvals(common_cell_arg_group) = list(rval).

common_group_get_rvals(common_cell_grouped_args(_, _, Rvals)) = Rvals.
common_group_get_rvals(common_cell_ungrouped_arg(_, Rval)) = [Rval].

:- pred output_cons_arg_types(list(llds_type)::in, string::in, int::in,
    io::di, io::uo) is det.

output_cons_arg_types([], _, _, !IO).
output_cons_arg_types([Type | Types], Indent, ArgNum, !IO) :-
    io.write_string(Indent, !IO),
    ( if Type = lt_float then
        % Ensure float structure members are word-aligned.
        io.write_string("MR_Float_Aligned", !IO)
    else
        output_llds_type(Type, !IO)
    ),
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
    ( if ArraySize = 1 then
        output_llds_type(Type, !IO),
        io.write_string(" f", !IO),
        io.write_int(ArgNum, !IO),
        io.write_string(";\n", !IO)
    else
        output_llds_type(Type, !IO),
        io.write_string(" f", !IO),
        io.write_int(ArgNum, !IO),
        io.write_string("[", !IO),
        io.write_int(ArraySize, !IO),
        io.write_string("];\n", !IO)
    ),
    output_cons_arg_group_types(Groups, Indent, ArgNum + 1, !IO).

:- pred output_common_cell_value(llds_out_info::in, common_cell_value::in,
    io::di, io::uo) is det.

output_common_cell_value(Info, CellValue, !IO) :-
    io.write_string("{\n", !IO),
    (
        CellValue = plain_value(TypedArgs),
        output_cons_args(Info, TypedArgs, !IO)
    ;
        CellValue = grouped_args_value(ArgGroups),
        output_cons_arg_groups(Info, ArgGroups, !IO)
    ),
    io.write_string("},\n", !IO).

    % Output the arguments, each on its own line, and with a cast appropriate
    % to its type if that is necessary.
    %
:- pred output_cons_args(llds_out_info::in, list(typed_rval)::in,
    io::di, io::uo) is det.

output_cons_args(_, [], !IO).
output_cons_args(Info, [TypedRval | TypedRvals], !IO) :-
    TypedRval = typed_rval(Rval, Type),
    ( if
        Rval = const(llconst_int(N)),
        direct_field_int_constant(Type) = yes
    then
        output_int_const(N, Type, !IO)
    else
        output_rval_as_type(Info, Rval, Type, !IO)
    ),
    (
        TypedRvals = [_ | _],
        io.write_string(",\n", !IO),
        output_cons_args(Info, TypedRvals, !IO)
    ;
        TypedRvals = [],
        io.write_string("\n", !IO)
    ).

:- pred output_cons_arg_groups(llds_out_info::in,
    list(common_cell_arg_group)::in, io::di, io::uo) is det.

output_cons_arg_groups(_, [], !IO).
output_cons_arg_groups(Info, [Group | Groups], !IO) :-
    (
        Group = common_cell_grouped_args(Type, _, Rvals),
        io.write_string("{\n", !IO),
        ( if
            list.map(project_int_constant, Rvals, Ints),
            direct_field_int_constant(Type) = yes
        then
            Check = check_int_const_sizes,
            (
                Check = no,
                output_cons_arg_group_ints(Ints, !IO)
            ;
                Check = yes,
                output_cons_arg_group_ints_check(Ints, Type, !IO)
            )
        else
            output_cons_arg_group_elements(Info, Type, Rvals, !IO)
        ),
        io.write_string("}", !IO)
    ;
        Group = common_cell_ungrouped_arg(Type, Rval),
        ( if
            project_int_constant(Rval, Int),
            direct_field_int_constant(Type) = yes
        then
            output_int_const(Int, Type, !IO)
        else
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
        ( if ok_int_const(N, Type) then
            io.write_int(N, !IO)
        else
            unexpected($module, $pred, "constant does not fit in type")
        )
    ;
        Check = no,
        io.write_int(N, !IO)
    ).

:- pred ok_int_const(int::in, llds_type::in) is semidet.
:- pragma inline(ok_int_const/2).

ok_int_const(N, LLDSType) :-
    require_complete_switch [LLDSType]
    (
        ( LLDSType = lt_bool
        ; LLDSType = lt_float
        ; LLDSType = lt_string
        ; LLDSType = lt_data_ptr
        ; LLDSType = lt_code_ptr
        ; LLDSType = lt_word
        ),
        unexpected($pred, "not integer constant")
    ;
        LLDSType = lt_int_least(IntLeastType),
        require_complete_switch [IntLeastType]
        (
            IntLeastType = int_least8,
            -128 =< N, N =< 127
        ;
            IntLeastType = uint_least8,
            0 =< N, N =< 255
        ;
            IntLeastType = int_least16,
            -32768 =< N, N =< 32767
        ;
            IntLeastType = uint_least16,
            0 =< N, N =< 65535
        ;
            IntLeastType = int_least32,
            -2147483648 =< N, N =< 2147483647
        ;
            IntLeastType = uint_least32,
            0 =< N,
            % The test N =< 4294967295 won't work on 32 bit platforms.
            ( int.bits_per_int = 32
            ; int64.from_int(N) =< 4294967295_i64
            )
        )
    ;
        LLDSType = lt_int(IntType),
        require_complete_switch [IntType]
        (
            IntType = int_type_int8,
            -128 =< N, N =< 127
        ;
            IntType = int_type_uint8,
            0 =< N, N =< 255
        ;
            IntType = int_type_int16,
            -32768 =< N, N =< 32767
        ;
            IntType = int_type_uint16,
            0 =< N, N =< 65535
        ;
            IntType = int_type_int32,
            -2147483648 =< N, N =< 2147483647
        ;
            IntType = int_type_uint32,
            0 =< N,
            % The test N =< 4294967295 won't work on 32 bit platforms.
            ( int.bits_per_int = 32
            ; int64.from_int(N) =< 4294967295_i64
            )
        ;
            ( IntType = int_type_int64
            ; IntType = int_type_uint64
            ; IntType = int_type_int
            ; IntType = int_type_uint
            )
        )
    ).

%---------------------------------------------------------------------------%
:- end_module ll_backend.llds_out.llds_out_global.
%---------------------------------------------------------------------------%
