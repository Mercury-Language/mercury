%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_top_gen.m.
%

:- module ml_backend.ml_top_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Generate MLDS code for an entire module.
    %
:- pred ml_code_gen(io.text_output_stream::in, mlds_target_lang::in,
    mlds::out, module_info::in, module_info::out, 
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.      % XXX for handling foreign_procs
:- import_module backend_libs.rtti.
:- import_module hlds.hlds_pred.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_args_util.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_proc_gen.
:- import_module ml_backend.ml_type_gen.
:- import_module ml_backend.ml_unify_gen_construct.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_foreign.

:- import_module bool.
:- import_module cord.
:- import_module getopt.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

ml_code_gen(ProgressStream, Target, MLDS, !ModuleInfo, !Specs) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ml_gen_foreign_code(!.ModuleInfo, ForeignCode),
    ml_gen_imports(!.ModuleInfo, Imports),

    ml_gen_types(!.ModuleInfo, Target, TypeDefns),
    ml_gen_table_structs(!.ModuleInfo, TableStructDefns),
    ml_gen_init_global_data(!.ModuleInfo, Target, GlobalData0),
    ml_generate_const_structs(!.ModuleInfo, Target, ConstStructMap,
        GlobalData0, GlobalData1),
    ml_gen_exported_enums(!.ModuleInfo, ExportedEnums),
    module_info_user_init_pred_target_names(!.ModuleInfo, InitPreds),
    module_info_user_final_pred_target_names(!.ModuleInfo, FinalPreds),
    ml_gen_preds(ProgressStream, Target, ConstStructMap, PredDefns,
        GlobalData1, GlobalData, !ModuleInfo, !Specs),
    MLDS = mlds(ModuleName, Imports, GlobalData, TypeDefns,
        TableStructDefns, PredDefns, InitPreds, FinalPreds,
        ForeignCode, ExportedEnums).

:- pred ml_gen_foreign_code(module_info::in,
    map(foreign_language, mlds_foreign_code)::out) is det.

ml_gen_foreign_code(ModuleInfo, AllForeignCodeMap) :-
    module_info_get_foreign_decl_codes_user(ModuleInfo,
        ForeignDeclCodeUserCord),
    module_info_get_foreign_decl_codes_aux(ModuleInfo,
        ForeignDeclCodeAuxCord),
    module_info_get_foreign_body_codes(ModuleInfo, ForeignBodyCodeCord),
    module_info_get_c_j_cs_fims(ModuleInfo, CJCsEFIMs),
    module_info_get_pragma_exported_procs(ModuleInfo, ForeignExportsCord),
    ForeignDeclCodes =
        cord.list(ForeignDeclCodeUserCord ++ ForeignDeclCodeAuxCord),
    ForeignBodyCodes = cord.list(ForeignBodyCodeCord),
    ForeignExports = cord.list(ForeignExportsCord),

    module_info_get_globals(ModuleInfo, Globals),
    globals.get_backend_foreign_languages(Globals, BackendForeignLanguages),
    WantedForeignImports = set.to_sorted_list(set.union_list(
        list.map(get_lang_fim_specs(CJCsEFIMs), BackendForeignLanguages))),

    list.foldl(
        ml_gen_foreign_code_lang(ModuleInfo,
            ForeignDeclCodes, ForeignBodyCodes,
            WantedForeignImports, ForeignExports),
        BackendForeignLanguages, map.init, AllForeignCodeMap).

:- pred ml_gen_foreign_code_lang(module_info::in,
    list(foreign_decl_code)::in, list(foreign_body_code)::in,
    list(fim_spec)::in, list(pragma_exported_proc)::in,
    foreign_language::in,
    map(foreign_language, mlds_foreign_code)::in,
    map(foreign_language, mlds_foreign_code)::out) is det.

ml_gen_foreign_code_lang(ModuleInfo, ForeignDeclCodes, ForeignBodyCodes,
        WantedForeignImports, ForeignExports, Lang, !Map) :-
    foreign.filter_decls(Lang, ForeignDeclCodes, WantedForeignDeclCodes,
        _OtherForeignDeclCodes),
    foreign.filter_bodys(Lang, ForeignBodyCodes, WantedForeignBodyCodes,
        _OtherForeignBodyCodes),
    foreign.filter_exports(Lang, ForeignExports, WantedForeignExports,
        _OtherForeignExports),
    list.map(ml_gen_pragma_export_proc(ModuleInfo),
        WantedForeignExports, MLDSWantedForeignExports),
    MLDS_ForeignCode = mlds_foreign_code(
        WantedForeignDeclCodes, WantedForeignBodyCodes,
        WantedForeignImports, MLDSWantedForeignExports),
    map.det_insert(Lang, MLDS_ForeignCode, !Map).

:- pred ml_gen_imports(module_info::in, list(mlds_import)::out) is det.

ml_gen_imports(ModuleInfo, MLDS_ImportList) :-
    % Determine all the mercury imports.
    % XXX This is overly conservative, i.e. we import more than we really need.
    module_info_get_all_deps(ModuleInfo, AllImports0),
    % No module needs to import itself.
    module_info_get_name(ModuleInfo, ThisModule),
    AllImports = set.delete(AllImports0, ThisModule),
    ImportMLDS = (func(Name) = mlds_import(compiler_visible_interface, Name)),
    MLDS_ImportList = list.map(ImportMLDS, set.to_sorted_list(AllImports)).

:- pred ml_gen_init_global_data(module_info::in, mlds_target_lang::in,
    ml_global_data::out) is det.

ml_gen_init_global_data(ModuleInfo, Target, GlobalData) :-
    (
        ( Target = ml_target_c
        ; Target = ml_target_csharp
        ; Target = ml_target_java
        ),
        UseCommonCells = use_common_cells
    ),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloats),
    (
        UnboxedFloats = yes,
        HaveUnboxedFloats = have_unboxed_floats
    ;
        UnboxedFloats = no,
        HaveUnboxedFloats = do_not_have_unboxed_floats
    ),
    globals.lookup_bool_option(Globals, unboxed_int64s, UnboxedInt64s),
    (
        UnboxedInt64s = yes,
        HaveUnboxedInt64s = have_unboxed_int64s
    ;
        UnboxedInt64s = no,
        HaveUnboxedInt64s = do_not_have_unboxed_int64s
    ),
    GlobalData = ml_global_data_init(Target, UseCommonCells,
        HaveUnboxedFloats, HaveUnboxedInt64s).

%---------------------------------------------------------------------------%
%
% For each pragma foreign_export declaration we associate with it the
% information used to generate the function prototype for the MLDS entity.
%

:- pred ml_gen_pragma_export_proc(module_info::in, pragma_exported_proc::in,
    mlds_pragma_export::out) is det.

ml_gen_pragma_export_proc(ModuleInfo, PragmaExportedProc, Defn) :-
    PragmaExportedProc = pragma_exported_proc(Lang, PredId, ProcId,
        ExportName, Context),
    PredProcId = proc(PredId, ProcId),
    ml_gen_proc_label(ModuleInfo, PredProcId, ModuleName, PlainName),
    MLDS_Name = qual_function_name(ModuleName, mlds_function_name(PlainName)),
    ml_gen_export_proc_params(ModuleInfo, PredProcId, FuncParams),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_univ_quant_tvars(PredInfo, UnivQTVars),
    Defn = ml_pragma_export(Lang, ExportName, MLDS_Name, FuncParams,
        UnivQTVars, Context).

:- pred ml_gen_export_proc_params(module_info::in, pred_proc_id::in,
    mlds_func_params::out) is det.

ml_gen_export_proc_params(ModuleInfo, PredProcId, FuncParams) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    ( if
        ( Target = target_java
        ; Target = target_csharp
        ),
        globals.set_option(det_copy_out, bool(no), Globals, GlobalsByRef),
        module_info_set_globals(GlobalsByRef, ModuleInfo, ModuleInfoByRef),
        ml_gen_proc_params_no_gc_stmts(ModuleInfoByRef, PredProcId,
            _ArgTuples, FuncParamsByRef),
        FuncParamsByRef = mlds_func_params(Args, ReturnTypes),
        (
            ReturnTypes = [],
            % If there is only one output argument, then we should use the
            % return value.
            list.filter(has_ptr_type, Args, OutArgs),
            list.length(OutArgs) > 1
        ;
            ReturnTypes = [_ | _]
        )
    then
        FuncParams = FuncParamsByRef
    else
        ml_gen_proc_params_no_gc_stmts(ModuleInfo, PredProcId,
            _ArgTuples, FuncParams)
    ).

:- pred has_ptr_type(mlds_argument::in) is semidet.

has_ptr_type(mlds_argument(_, mlds_ptr_type(_), _)).

%---------------------------------------------------------------------------%
%
% Code for handling tabling structures.
%

:- pred ml_gen_table_structs(module_info::in, list(mlds_global_var_defn)::out)
    is det.

ml_gen_table_structs(ModuleInfo, DataDefns) :-
    module_info_get_table_struct_map(ModuleInfo, TableStructMap),
    map.to_assoc_list(TableStructMap, TableStructs),
    (
        TableStructs = [],
        DataDefns = []
    ;
        TableStructs = [_ | _],
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_gc_method(Globals, GC_Method),
        % XXX To handle accurate GC properly, the GC would need to trace
        % through the global variables that we generate for the tables.
        % Support for this is not yet implemented. Also, we would need to add
        % GC support (stack frame registration, and calls to MR_GC_check()) to
        % MR_make_long_lived() and MR_deep_copy() so that we do garbage
        % collection of the "global heap" which is used to store the tables.
        expect(isnt(unify(gc_accurate), GC_Method), $pred,
            "tabling and `--gc accurate'"),

        list.foldl(ml_gen_add_table_var(ModuleInfo), TableStructs,
            [], DataDefns)
    ).

:- pred ml_gen_add_table_var(module_info::in,
    pair(pred_proc_id, table_struct_info)::in,
    list(mlds_global_var_defn)::in, list(mlds_global_var_defn)::out) is det.

ml_gen_add_table_var(ModuleInfo, PredProcId - TableStructInfo, !DataDefns) :-
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    PredProcId = proc(_PredId, ProcId),

    TableStructInfo = table_struct_info(ProcTableStructInfo, _Attributes),
    ProcTableStructInfo = proc_table_struct_info(RttiProcLabel, _TVarSet,
        Context, NumInputs, NumOutputs, InputSteps, MaybeOutputSteps,
        _ArgInfos, TabledMethod),

    ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcLabel, PredLabel,
        _PredModule),
    MLDS_ProcLabel = mlds_proc_label(PredLabel, ProcId),
    TableTypeStr = tabled_eval_method_to_table_type(TabledMethod),
    % We will probably need to add actual prefixes when tabling is implemented
    % for Java and C#.
    TableTypeTargetPrefixes = target_prefixes("", ""),
    (
        InputSteps = [],
        % We don't want to generate arrays with zero elements.
        InputStepsRefInit = gen_init_null_pointer(
            mlds_tabling_type(tabling_steps_desc(call_table))),
        InputStepsDefns = []
    ;
        InputSteps = [_ | _],
        InputStepsRefInit = gen_init_tabling_name(MLDS_ModuleName,
            MLDS_ProcLabel, tabling_steps_desc(call_table)),
        InputStepsInit = init_array(
            list.map(init_step_desc(tabling_steps_desc(call_table)),
            InputSteps)),
        InputStepsDefn = tabling_name_and_init_to_defn(MLDS_ProcLabel,
            tabling_steps_desc(call_table),
            Context, const, InputStepsInit),
        InputStepsDefns = [InputStepsDefn]
    ),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, Context,
        call_table, curr_table, InputSteps,
        CallStatsInit, CallStatsDefns),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, Context,
        call_table, prev_table, InputSteps,
        PrevCallStatsInit, PrevCallStatsDefns),
    CallDefns = InputStepsDefns ++ CallStatsDefns ++ PrevCallStatsDefns,
    (
        MaybeOutputSteps = no,
        HasAnswerTable = 0,
        OutputStepsRefInit = gen_init_null_pointer(
            mlds_tabling_type(tabling_steps_desc(answer_table))),
        OutputStepsDefns = []
    ;
        MaybeOutputSteps = yes(OutputSteps),
        HasAnswerTable = 1,
        OutputStepsRefInit = gen_init_tabling_name(MLDS_ModuleName,
            MLDS_ProcLabel, tabling_steps_desc(answer_table)),
        OutputStepsInit = init_array(
            list.map(init_step_desc(tabling_steps_desc(answer_table)),
            OutputSteps)),
        OutputStepsDefn = tabling_name_and_init_to_defn(MLDS_ProcLabel,
            tabling_steps_desc(answer_table),
            Context, const, OutputStepsInit),
        OutputStepsDefns = [OutputStepsDefn]
    ),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, Context,
        answer_table, curr_table, InputSteps,
        AnswerStatsInit, AnswerStatsDefns),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, Context,
        answer_table, prev_table, InputSteps,
        PrevAnswerStatsInit, PrevAnswerStatsDefns),
    AnswerDefns = OutputStepsDefns ++ AnswerStatsDefns ++ PrevAnswerStatsDefns,

    PTIsRefInit = gen_init_null_pointer(mlds_tabling_type(tabling_ptis)),
    TypeParamLocnsRefInit = gen_init_null_pointer(
        mlds_tabling_type(tabling_type_param_locns)),
    RootNodeInit = init_struct(mlds_tabling_type(tabling_root_node),
        [gen_init_int(0)]),
    TipsRefInit = gen_init_null_pointer(mlds_tabling_type(tabling_tips)),

    ProcTableInfoInit = init_struct(mlds_tabling_type(tabling_info), [
        gen_init_builtin_const(TableTypeTargetPrefixes, TableTypeStr),
        gen_init_int(NumInputs),
        gen_init_int(NumOutputs),
        gen_init_int(HasAnswerTable),
        PTIsRefInit,
        TypeParamLocnsRefInit,
        RootNodeInit,
        init_array([InputStepsRefInit, OutputStepsRefInit]),
        init_array([
            init_array([CallStatsInit, PrevCallStatsInit]),
            init_array([AnswerStatsInit, PrevAnswerStatsInit])
        ]),
        gen_init_int(0),
        TipsRefInit,
        gen_init_int(0),
        gen_init_int(0)
    ]),
    ProcTableInfoDefn = tabling_name_and_init_to_defn(MLDS_ProcLabel,
        tabling_info, Context, modifiable, ProcTableInfoInit),

    !:DataDefns = CallDefns ++ AnswerDefns ++
        [ProcTableInfoDefn | !.DataDefns].

:- func init_step_desc(proc_tabling_struct_id, table_step_desc)
    = mlds_initializer.

init_step_desc(StructId, StepDesc) = init_struct(StructType, FieldInits) :-
    StepDesc = table_step_desc(VarName, Step),
    table_trie_step_to_c(Step, StepStr, MaybeEnumRange),
    VarNameInit = gen_init_string(VarName),
    StepInit = encode_enum_init(StepStr),
    (
        MaybeEnumRange = no,
        MaybeEnumRangeInit = gen_init_int(-1)
    ;
        MaybeEnumRange = yes(EnumRange),
        MaybeEnumRangeInit = gen_init_int(EnumRange)
    ),
    StructType = mlds_tabling_type(StructId),
    FieldInits = [VarNameInit, StepInit, MaybeEnumRangeInit].

:- pred init_stats(mlds_module_name::in, mlds_proc_label::in, prog_context::in,
    call_or_answer_table::in, curr_or_prev_table::in,
    list(table_step_desc)::in, mlds_initializer::out,
    list(mlds_global_var_defn)::out) is det.

init_stats(MLDS_ModuleName, MLDS_ProcLabel, Context,
        CallOrAnswer, CurrOrPrev, StepDescs, StatsInit, StatsStepDefns) :-
    StatsId = tabling_stats(CallOrAnswer, CurrOrPrev),
    StatsStepsId = tabling_stat_steps(CallOrAnswer, CurrOrPrev),
    StatsType = mlds_tabling_type(StatsId),
    StatsStepsType = mlds_tabling_type(StatsStepsId),
    (
        StepDescs = [],
        StatsStepDefns = [],
        StatsStepsArrayRefInit = gen_init_null_pointer(StatsStepsType)
    ;
        StepDescs = [_ | _],
        list.map(init_stats_step(StatsStepsId), StepDescs, StatsStepsInits),
        StatsStepsArrayInit = init_array(StatsStepsInits),
        StatsStepDefn = tabling_name_and_init_to_defn(MLDS_ProcLabel,
            StatsStepsId, Context, modifiable, StatsStepsArrayInit),
        StatsStepDefns = [StatsStepDefn],
        StatsStepsArrayRefInit = gen_init_tabling_name(MLDS_ModuleName,
            MLDS_ProcLabel, tabling_stat_steps(CallOrAnswer, CurrOrPrev))
    ),
    StatsInit = init_struct(StatsType, [
        gen_init_int(0),
        gen_init_int(0),
        StatsStepsArrayRefInit
    ]).

:- pred init_stats_step(proc_tabling_struct_id::in, table_step_desc::in,
    mlds_initializer::out) is det.

init_stats_step(StepId, StepDesc, Init) :-
    StepDesc = table_step_desc(_VarName, Step),
    KindStr = table_step_stats_kind(Step),
    Init = init_struct(mlds_tabling_type(StepId), [
        gen_init_int(0),
        gen_init_int(0),
        encode_enum_init(KindStr),

        % The fields about hash tables.
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),

        % The fields about enums.
        gen_init_int(0),
        gen_init_int(0),

        % The fields about du types.
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),
        gen_init_int(0),

        % The fields about start tables.
        gen_init_int(0),
        gen_init_int(0)
    ]).

:- func encode_enum_init(string) = mlds_initializer.

encode_enum_init(EnumConstName) = Initializer :-
    % We will probably need to add actual prefixes when tabling is implemented
    % for Java and C#.
    TargetPrefixes = target_prefixes("", ""),
    Const = mlconst_named_const(TargetPrefixes, EnumConstName),
    Initializer = init_obj(ml_const(Const)).

:- func gen_init_tabling_name(mlds_module_name, mlds_proc_label,
    proc_tabling_struct_id) = mlds_initializer.

gen_init_tabling_name(ModuleName, ProcLabel, TablingId) = Rval :-
    QualProcLabel = qual_proc_label(ModuleName, ProcLabel),
    Const = mlconst_data_addr_tabling(QualProcLabel, TablingId),
    Rval = init_obj(ml_const(Const)).

:- func tabling_name_and_init_to_defn(mlds_proc_label, proc_tabling_struct_id,
    prog_context, constness, mlds_initializer) = mlds_global_var_defn.

tabling_name_and_init_to_defn(ProcLabel, Id, Context, Constness, Initializer)
        = GlobalVarDefn :-
    GCStatement = gc_no_stmt,
    MLDS_Type = mlds_tabling_type(Id),
    Flags = mlds_global_var_decl_flags(gvar_acc_module_only, Constness),
    Name = gvn_tabling_var(ProcLabel, Id),
    GlobalVarDefn = mlds_global_var_defn(Name, Context, Flags,
        MLDS_Type, Initializer, GCStatement).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_top_gen.
%---------------------------------------------------------------------------%
