%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ml_proc_gen.m.
% Main author: fjh.
%

:- module ml_backend.ml_proc_gen.
:- interface.

:- import_module hlds.hlds_module.
:- import_module ml_backend.mlds.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Generate MLDS code for an entire module.
    %
:- pred ml_code_gen(module_info::in, module_info::out, mlds::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.foreign.      % XXX needed for foreign_proc
:- import_module backend_libs.rtti.
:- import_module check_hlds.mode_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.sym_name.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.ml_type_gen.
:- import_module ml_backend.ml_unify_gen.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module getopt_io.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

ml_code_gen(!ModuleInfo, MLDS) :-
    module_info_get_name(!.ModuleInfo, ModuleName),
    ml_gen_foreign_code(!.ModuleInfo, ForeignCode),
    ml_gen_imports(!.ModuleInfo, Imports),
    ml_gen_defns(!ModuleInfo, Defns, GlobalData),
    ml_gen_exported_enums(!.ModuleInfo, ExportedEnums),
    module_info_user_init_pred_c_names(!.ModuleInfo, InitPreds),
    module_info_user_final_pred_c_names(!.ModuleInfo, FinalPreds),
    MLDS = mlds(ModuleName, ForeignCode, Imports, GlobalData, Defns,
        InitPreds, FinalPreds, ExportedEnums).

:- pred ml_gen_foreign_code(module_info::in,
    map(foreign_language, mlds_foreign_code)::out) is det.

ml_gen_foreign_code(ModuleInfo, AllForeignCode) :-
    module_info_get_foreign_decl(ModuleInfo, ForeignDecls),
    module_info_get_foreign_import_module(ModuleInfo, ForeignImports),
    module_info_get_foreign_body_code(ModuleInfo, ForeignBodys),
    module_info_get_pragma_exported_procs(ModuleInfo, ForeignExports),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_backend_foreign_languages(Globals, BackendForeignLanguages),

    WantedForeignImports = list.condense(
        list.map((func(L) = Imports :-
            foreign.filter_imports(L, ForeignImports, Imports, _)
        ), BackendForeignLanguages)),

    list.foldl(ml_gen_foreign_code_lang(ModuleInfo, ForeignDecls,
        ForeignBodys, WantedForeignImports, ForeignExports),
        BackendForeignLanguages, map.init, AllForeignCode).

:- pred ml_gen_foreign_code_lang(module_info::in, foreign_decl_info::in,
    foreign_body_info::in, foreign_import_module_info_list::in,
    list(pragma_exported_proc)::in, foreign_language::in,
    map(foreign_language, mlds_foreign_code)::in,
    map(foreign_language, mlds_foreign_code)::out) is det.

ml_gen_foreign_code_lang(ModuleInfo, ForeignDecls, ForeignBodys,
        WantedForeignImports, ForeignExports, Lang, !Map) :-
    foreign.filter_decls(Lang, ForeignDecls, WantedForeignDecls,
        _OtherForeignDecls),
    foreign.filter_bodys(Lang, ForeignBodys, WantedForeignBodys,
        _OtherForeignBodys),
    foreign.filter_exports(Lang, ForeignExports, WantedForeignExports,
        _OtherForeignExports),
    ConvBody = (func(foreign_body_code(L, S, C)) =
        user_foreign_code(L, S, C)),
    MLDSWantedForeignBodys = list.map(ConvBody, WantedForeignBodys),
    list.map(ml_gen_pragma_export_proc(ModuleInfo),
        WantedForeignExports, MLDSWantedForeignExports),
    MLDS_ForeignCode = mlds_foreign_code(WantedForeignDecls,
        WantedForeignImports, MLDSWantedForeignBodys,
        MLDSWantedForeignExports),
    map.det_insert(Lang, MLDS_ForeignCode, !Map).

:- pred ml_gen_imports(module_info::in, mlds_imports::out) is det.

ml_gen_imports(ModuleInfo, MLDS_ImportList) :-
    % Determine all the mercury imports.
    % XXX This is overly conservative, i.e. we import more than we really need.
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    module_info_get_all_deps(ModuleInfo, AllImports0),
    % No module needs to import itself.
    module_info_get_name(ModuleInfo, ThisModule),
    AllImports = set.delete(AllImports0, ThisModule),
    P = (func(Name) = mercury_import(compiler_visible_interface,
        mercury_module_name_to_mlds(Name))),

    % For every foreign type determine the import needed to find
    % the declaration for that type.
    module_info_get_type_table(ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
    ForeignTypeImports = list.condense(
        list.map(foreign_type_required_imports(Target), TypeCtorsDefns)),

    MLDS_ImportList = ForeignTypeImports ++
        list.map(P, set.to_sorted_list(AllImports)).

:- func foreign_type_required_imports(compilation_target,
    pair(type_ctor, hlds_type_defn)) = list(mlds_import).

foreign_type_required_imports(Target, _TypeCtor - TypeDefn) = Imports :-
    (
        ( Target = target_c
        ; Target = target_java
        ; Target = target_csharp
        ),
        Imports = []
    ;
        Target = target_il,
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        (
            TypeBody = hlds_foreign_type(ForeignTypeBody),
            ForeignTypeBody = foreign_type_body(MaybeIL,
                _MaybeC, _MaybeJava, _MaybeCSharp, _MaybeErlang),
            (
                MaybeIL = yes(Data),
                Data = foreign_type_lang_data(il_type(_, Location, _), _, _)
            ->
                Name = il_assembly_name(mercury_module_name_to_mlds(
                    unqualified(Location))),
                Imports = [foreign_import(Name)]
            ;
                unexpected($module, $pred, "no IL type")
            )
        ;
            ( TypeBody = hlds_du_type(_, _, _,_,  _, _, _, _, _)
            ; TypeBody = hlds_eqv_type(_)
            ; TypeBody = hlds_solver_type(_, _)
            ; TypeBody = hlds_abstract_type(_)
            ),
            Imports = []
        )
    ;
        Target = target_erlang,
        unexpected($module, $pred, "target erlang")
    ).

:- pred ml_gen_defns(module_info::in, module_info::out,
    list(mlds_defn)::out, ml_global_data::out) is det.

ml_gen_defns(!ModuleInfo, Defns, !:GlobalData) :-
    ml_gen_types(!.ModuleInfo, TypeDefns),
    ml_gen_table_structs(!.ModuleInfo, TableStructDefns),
    ml_gen_init_common_data(!.ModuleInfo, !:GlobalData),
    ml_gen_const_structs(!.ModuleInfo, ConstStructMap, !GlobalData),
    ml_gen_preds(!ModuleInfo, ConstStructMap, PredDefns, !GlobalData),
    Defns = TypeDefns ++ TableStructDefns ++ PredDefns.

:- pred ml_gen_init_common_data(module_info::in, ml_global_data::out) is det.

ml_gen_init_common_data(ModuleInfo, GlobalData) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        ( Target = target_c
        ; Target = target_csharp
        ; Target = target_java
        ),
        UseCommonCells = use_common_cells
    ;
        ( Target = target_il
        ; Target = target_erlang
        ),
        UseCommonCells = do_not_use_common_cells
    ),
    globals.lookup_bool_option(Globals, unboxed_float, UnboxedFloats),
    (
        UnboxedFloats = yes,
        HaveUnboxedFloats = have_unboxed_floats
    ;
        UnboxedFloats = no,
        HaveUnboxedFloats = do_not_have_unboxed_floats
    ),
    GlobalData = ml_global_data_init(UseCommonCells, HaveUnboxedFloats).

%-----------------------------------------------------------------------------%
%
% For each pragma foreign_export declaration we associate with it the
% information used to generate the function prototype for the MLDS entity.
%

:- pred ml_gen_pragma_export_proc(module_info::in, pragma_exported_proc::in,
    mlds_pragma_export::out) is det.

ml_gen_pragma_export_proc(ModuleInfo, PragmaExportedProc, Defn) :-
    PragmaExportedProc = pragma_exported_proc(Lang, PredId, ProcId,
        ExportName, ProgContext),
    ml_gen_proc_label(ModuleInfo, PredId, ProcId, Name, ModuleName),
    MLDS_Name = qual(ModuleName, module_qual, Name),
    ml_gen_export_proc_params(ModuleInfo, PredId, ProcId, FuncParams),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_univ_quant_tvars(PredInfo, UnivQTVars),
    MLDS_Context = mlds_make_context(ProgContext),
    Defn = ml_pragma_export(Lang, ExportName, MLDS_Name, FuncParams,
        UnivQTVars, MLDS_Context).

:- pred ml_gen_export_proc_params(module_info::in, pred_id::in, proc_id::in,
    mlds_func_params::out) is det.

ml_gen_export_proc_params(ModuleInfo, PredId, ProcId, FuncParams) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_target(Globals, Target),
    (
        ( Target = target_java
        ; Target = target_csharp
        ),
        globals.set_option(det_copy_out, bool(no), Globals, GlobalsByRef),
        module_info_set_globals(GlobalsByRef, ModuleInfo, ModuleInfoByRef),
        FuncParamsByRef = ml_gen_proc_params(ModuleInfoByRef, PredId, ProcId),
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
    ->
        FuncParams = FuncParamsByRef
    ;
        FuncParams = ml_gen_proc_params(ModuleInfo, PredId, ProcId)
    ).

:- pred has_ptr_type(mlds_argument::in) is semidet.

has_ptr_type(mlds_argument(_, mlds_ptr_type(_), _)).

%-----------------------------------------------------------------------------%
%
% Stuff to generate MLDS code for HLDS predicates & functions.
%

    % Generate MLDS definitions for all the non-imported predicates
    % (and functions) in the HLDS.
    %
:- pred ml_gen_preds(module_info::in, module_info::out,
    ml_const_struct_map::in, list(mlds_defn)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_preds(!ModuleInfo, ConstStructMap, PredDefns, !GlobalData) :-
    module_info_get_preds(!.ModuleInfo, PredTable),
    map.keys(PredTable, PredIds),
    ml_gen_preds_acc(!ModuleInfo, ConstStructMap, PredIds, [], PredDefns,
        !GlobalData).

:- pred ml_gen_preds_acc(module_info::in, module_info::out,
    ml_const_struct_map::in, list(pred_id)::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_preds_acc(!ModuleInfo, ConstStructMap, PredIds0,
        !Defns, !GlobalDefns) :-
    (
        PredIds0 = [PredId | PredIds],
        module_info_get_preds(!.ModuleInfo, PredTable),
        map.lookup(PredTable, PredId, PredInfo),
        pred_info_get_import_status(PredInfo, ImportStatus),
        (
            (
                ImportStatus = status_imported(_)
            ;
                % We generate incorrect and unnecessary code for the external
                % special preds which are pseudo_imported, so just ignore them.
                is_unify_or_compare_pred(PredInfo),
                ImportStatus = status_external(status_pseudo_imported)
            )
        ->
            true
        ;
            ml_gen_pred(!ModuleInfo, ConstStructMap, PredId, PredInfo,
                ImportStatus, !Defns, !GlobalDefns)
        ),
        ml_gen_preds_acc(!ModuleInfo, ConstStructMap, PredIds,
            !Defns, !GlobalDefns)
    ;
        PredIds0 = []
    ).

    % Generate MLDS definitions for all the non-imported procedures
    % of a given predicate (or function).
    %
:- pred ml_gen_pred(module_info::in, module_info::out, ml_const_struct_map::in,
    pred_id::in, pred_info::in, import_status::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_pred(!ModuleInfo, ConstStructMap, PredId, PredInfo, ImportStatus,
        !Defns, !GlobalData) :-
    ( ImportStatus = status_external(_) ->
        ProcIds = pred_info_procids(PredInfo)
    ;
        ProcIds = pred_info_non_imported_procids(PredInfo)
    ),
    (
        ProcIds = []
    ;
        ProcIds = [_ | _],
        trace [io(!IO)] (
            write_pred_progress_message("% Generating MLDS code for ",
                PredId, !.ModuleInfo, !IO)
        ),
        ml_gen_procs(!ModuleInfo, ConstStructMap, PredId, ProcIds,
            !Defns, !GlobalData)
    ).

:- pred ml_gen_procs(module_info::in, module_info::out,
    ml_const_struct_map::in, pred_id::in, list(proc_id)::in,
    list(mlds_defn)::in, list(mlds_defn)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_procs(!ModuleInfo, _, _, [], !Defns, !GlobalData).
ml_gen_procs(!ModuleInfo, ConstStructMap, PredId, [ProcId | ProcIds],
        !Defns, !GlobalData) :-
    ml_gen_proc(!ModuleInfo, ConstStructMap, PredId, ProcId,
        !Defns, !GlobalData),
    ml_gen_procs(!ModuleInfo, ConstStructMap, PredId, ProcIds,
        !Defns, !GlobalData).

%-----------------------------------------------------------------------------%
%
% Code for handling individual procedures.
%

:- pred ml_gen_proc(module_info::in, module_info::out, ml_const_struct_map::in,
    pred_id::in, proc_id::in, list(mlds_defn)::in, list(mlds_defn)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_proc(!ModuleInfo, ConstStructMap, PredId, ProcId,
        !Defns, !GlobalData) :-
    % The specification of the HLDS allows goal_infos to overestimate
    % the set of non-locals. Such overestimates are bad for us for two reasons:
    %
    % - If the non-locals of the top-level goal contained any variables other
    %   than head vars, those variables would not be declared.
    %
    % - The code of goal_expr_find_subgoal_nonlocals depends on the nonlocals
    %   sets of goals being exactly correct, since this is the only way it can
    %   avoid traversing the entirety of the goals themselves. Such traversals
    %   can be very expensive on large goals, since it would have to be done
    %   repeatedly, once for each containing goal. Quantification does just one
    %   traversal.

    module_info_pred_proc_info(!.ModuleInfo, PredId, ProcId,
        PredInfo, ProcInfo0),
    requantify_proc_general(ordinary_nonlocals_no_lambda, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        !ModuleInfo),

    pred_info_get_import_status(PredInfo, ImportStatus),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_goal(ProcInfo, Goal),

    Goal = hlds_goal(_GoalExpr, GoalInfo),
    Context = goal_info_get_context(GoalInfo),

    some [!Info] (
        !:Info = ml_gen_info_init(!.ModuleInfo, ConstStructMap,
            PredId, ProcId, ProcInfo, !.GlobalData),

        ( ImportStatus = status_external(_) ->
            % For Mercury procedures declared `:- external', we generate an
            % MLDS definition for them with no function body. The MLDS ->
            % target code pass can treat this accordingly, e.g. for C
            % it outputs a function declaration with no corresponding
            % definition, making sure that the function is declared as `extern'
            % rather than `static'.
            %
            FunctionBody = body_external,
            ExtraDefns = [],
            ml_gen_proc_params(PredId, ProcId, MLDS_Params, !.Info, _Info)
        ;
            % Set up the initial success continuation, if any.
            % Also figure out which output variables are returned by value
            % (rather than being passed by reference) and remove them from
            % the byref_output_vars field in the ml_gen_info.
            (
                ( CodeModel = model_det
                ; CodeModel = model_semi
                ),
                ml_det_copy_out_vars(!.ModuleInfo, CopiedOutputVars, !Info)
            ;
                CodeModel = model_non,
                ml_set_up_initial_succ_cont(!.ModuleInfo, CopiedOutputVars,
                    !Info)
            ),

            % This would generate all the local variables at the top of
            % the function:
            %   ml_gen_all_local_var_decls(Goal,
            %       VarSet, VarTypes, HeadVars, MLDS_LocalVars, Info1, Info2)
            % But instead we now generate them locally for each goal.
            % We just declare the `succeeded' var here, plus locals
            % for any output arguments that are returned by value
            % (e.g. if --nondet-copy-out is enabled, or for det function
            % return values).
            (
                CopiedOutputVars = [],
                % Optimize common case.
                OutputVarLocals = []
            ;
                CopiedOutputVars = [_ | _],
                proc_info_get_varset(ProcInfo, VarSet),
                proc_info_get_vartypes(ProcInfo, VarTypes),
                % Note that for headvars we must use the types from
                % the procedure interface, not from the procedure body.
                vartypes_overlay_corresponding_lists(HeadVars, ArgTypes,
                    VarTypes, UpdatedVarTypes),
                ml_gen_local_var_decls(VarSet, UpdatedVarTypes,
                    Context, CopiedOutputVars, OutputVarLocals, !Info)
            ),
            MLDS_Context = mlds_make_context(Context),
            MLDS_LocalVars = [ml_gen_succeeded_var_decl(MLDS_Context) |
                OutputVarLocals],
            modes_to_arg_modes(!.ModuleInfo, Modes, ArgTypes, ArgModes),
            ml_gen_proc_body(CodeModel, HeadVars, ArgTypes, ArgModes,
                CopiedOutputVars, Goal, Defns0, Statements, !Info),
            ml_gen_proc_params(PredId, ProcId, MLDS_Params, !Info),
            ml_gen_info_get_closure_wrapper_defns(!.Info, ExtraDefns),
            ml_gen_info_get_global_data(!.Info, !:GlobalData),
            Defns = MLDS_LocalVars ++ Defns0,
            Statement = ml_gen_block(Defns, Statements, Context),
            FunctionBody = body_defined_here(Statement)

        ),
        % XXX Can env_var_names be affected by body_external?
        % If, as I (zs) suspect, it cannot, this should be inside the previous
        % scope.
        ml_gen_info_get_env_var_names(!.Info, EnvVarNames)
    ),

    proc_info_get_context(ProcInfo0, ProcContext),
    ml_gen_proc_label(!.ModuleInfo, PredId, ProcId, EntityName, _ModuleName),
    MLDS_ProcContext = mlds_make_context(ProcContext),
    DeclFlags = ml_gen_proc_decl_flags(!.ModuleInfo, PredId, ProcId),
    MaybePredProcId = yes(proc(PredId, ProcId)),
    pred_info_get_attributes(PredInfo, Attributes),
    attributes_to_attribute_list(Attributes, AttributeList),
    MLDS_Attributes =
        attributes_to_mlds_attributes(!.ModuleInfo, AttributeList),
    EntityBody = mlds_function(MaybePredProcId, MLDS_Params,
        FunctionBody, MLDS_Attributes, EnvVarNames),
    ProcDefn = mlds_defn(EntityName, MLDS_ProcContext, DeclFlags, EntityBody),
    !:Defns = ExtraDefns ++ [ProcDefn | !.Defns].

    % Return the declaration flags appropriate for a procedure definition.
    %
:- func ml_gen_proc_decl_flags(module_info, pred_id, proc_id)
    = mlds_decl_flags.

ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId) = DeclFlags :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ( procedure_is_exported(ModuleInfo, PredInfo, ProcId) ->
        Access = acc_public
    ;
        Access = acc_private
    ),
    PerInstance = one_copy,
    Virtuality = non_virtual,
    Overridability = overridable,
    Constness = modifiable,
    Abstractness = concrete,
    DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Overridability, Constness, Abstractness).

    % For model_det and model_semi procedures, figure out which output
    % variables are returned by value (rather than being passed by reference)
    % and remove them from the byref_output_vars field in the ml_gen_info.
    %
:- pred ml_det_copy_out_vars(module_info::in, list(prog_var)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_det_copy_out_vars(ModuleInfo, CopiedOutputVars, !Info) :-
    ml_gen_info_get_byref_output_vars(!.Info, OutputVars),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, det_copy_out, DetCopyOut),
    (
        % If --det-copy-out is enabled, all non-dummy output variables are
        % returned by value, rather than passing them by reference.
        DetCopyOut = yes,
        ByRefOutputVars = [],
        ml_gen_info_get_var_types(!.Info, VarTypes),
        list.filter(var_is_of_dummy_type(ModuleInfo, VarTypes), OutputVars,
            _, CopiedOutputVars)
    ;
        DetCopyOut = no,
        (
            % For det functions, the function result variable is returned by
            % value, and any remaining output variables are passed by
            % reference.
            ml_gen_info_get_pred_id(!.Info, PredId),
            ml_gen_info_get_proc_id(!.Info, ProcId),
            ml_is_output_det_function(ModuleInfo, PredId, ProcId, ResultVar)
        ->
            CopiedOutputVars = [ResultVar],
            list.delete_all(OutputVars, ResultVar, ByRefOutputVars)
        ;
            % Otherwise, all output vars are passed by reference.
            CopiedOutputVars = [],
            ByRefOutputVars = OutputVars
        )
    ),
    ml_gen_info_set_byref_output_vars(ByRefOutputVars, !Info),
    ml_gen_info_set_value_output_vars(CopiedOutputVars, !Info).

    % For model_non procedures, figure out which output variables are returned
    % by value (rather than being passed by reference) and remove them from
    % the byref_output_vars field in the ml_gen_info, and construct the
    % initial success continuation.
    %
:- pred ml_set_up_initial_succ_cont(module_info::in, list(prog_var)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_set_up_initial_succ_cont(ModuleInfo, NondetCopiedOutputVars, !Info) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, nondet_copy_out, NondetCopyOut),
    (
        NondetCopyOut = yes,
        % For --nondet-copy-out, we generate local variables for the output
        % variables and then pass them to the continuation, rather than
        % passing them by reference.
        ml_gen_info_get_byref_output_vars(!.Info, NondetCopiedOutputVars),
        ml_gen_info_set_byref_output_vars([], !Info)
    ;
        NondetCopyOut = no,
        NondetCopiedOutputVars = []
    ),
    ml_gen_info_set_value_output_vars(NondetCopiedOutputVars, !Info),
    ml_gen_var_list(!.Info, NondetCopiedOutputVars, OutputVarLvals),
    ml_variable_types(!.Info, NondetCopiedOutputVars, OutputVarTypes),
    ml_initial_cont(!.Info, OutputVarLvals, OutputVarTypes, InitialCont),
    ml_gen_info_push_success_cont(InitialCont, !Info).

    % Generate the code for a procedure body.
    %
:- pred ml_gen_proc_body(code_model::in, list(prog_var)::in,
    list(mer_type)::in, list(arg_mode)::in, list(prog_var)::in,
    hlds_goal::in, list(mlds_defn)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_proc_body(CodeModel, HeadVars, ArgTypes, ArgModes, CopiedOutputVars,
        Goal, Decls, Statements, !Info) :-
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),

    % First just generate the code for the procedure's goal.

    % In certain cases -- for example existentially typed procedures,
    % or unification/compare procedures for equivalence types --
    % the parameters types may not match the types of the head variables.
    % In such cases, we need to box/unbox/cast them to the right type.
    % We also grab the original (uncast) lvals for the copied output
    % variables (if any) here, since for the return statement that
    % we append below, we want the original vars, not their cast versions.

    ml_gen_var_list(!.Info, CopiedOutputVars, CopiedOutputVarOriginalLvals),
    ml_gen_convert_headvars(HeadVars, ArgTypes, ArgModes, CopiedOutputVars,
        Context, ConvDecls, ConvInputStatements, ConvOutputStatements, !Info),
    (
        ConvDecls = [],
        ConvInputStatements = [],
        ConvOutputStatements = []
    ->
        % No boxing/unboxing/casting required.
        ml_gen_goal(CodeModel, Goal, Decls, Statements1, !Info)
    ;
        DoGenGoal = ml_gen_goal(CodeModel, Goal),

        % Boxing/unboxing/casting required. We need to convert the input
        % arguments, generate the goal, convert the output arguments,
        % and then succeeed.
        DoConvOutputs = (pred(NewDecls::out, NewStatements::out,
                Info0::in, Info::out) is det :-
            ml_gen_success(CodeModel, Context, SuccStatements, Info0, Info),
            NewDecls = [],
            NewStatements = ConvOutputStatements ++ SuccStatements
        ),
        ml_combine_conj(CodeModel, Context, DoGenGoal, DoConvOutputs,
            Decls0, Statements0, !Info),
        Statements1 = ConvInputStatements ++ Statements0,
        Decls = ConvDecls ++ Decls0
    ),

    % Finally append an appropriate `return' statement, if needed.
    ml_append_return_statement(!.Info, CodeModel, CopiedOutputVarOriginalLvals,
        Context, Statements1, Statements).

    % In certain cases -- for example existentially typed procedures,
    % or unification/compare procedures for equivalence types --
    % the parameter types may not match the types of the head variables.
    % In such cases, we need to box/unbox/cast them to the right type.
    % This procedure handles that.
    %
:- pred ml_gen_convert_headvars(list(prog_var)::in, list(mer_type)::in,
    list(arg_mode)::in, list(prog_var)::in, prog_context::in,
    list(mlds_defn)::out, list(statement)::out, list(statement)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_convert_headvars(Vars, HeadTypes, ArgModes, CopiedOutputVars, Context,
        Decls, InputStatements, OutputStatements, !Info) :-
    (
        Vars = [],
        HeadTypes = [],
        ArgModes = []
    ->
        Decls = [],
        InputStatements = [],
        OutputStatements = []
    ;
        Vars = [Var | VarsTail],
        HeadTypes = [HeadType | HeadTypesTail],
        ArgModes = [ArgMode | ArgModesTail]
    ->
        ml_variable_type(!.Info, Var, BodyType),
        (
            % Arguments with mode `top_unused' do not need to be converted.
            ArgMode = top_unused
        ->
            ml_gen_convert_headvars(VarsTail, HeadTypesTail, ArgModesTail,
                CopiedOutputVars, Context, Decls,
                InputStatements, OutputStatements, !Info)
        ;
            % Check whether HeadType is the same as BodyType
            % (modulo the term.contexts). If so, no conversion is needed.
            map.init(Subst0),
            type_unify(HeadType, BodyType, [], Subst0, Subst),
            map.is_empty(Subst)
        ->
            ml_gen_convert_headvars(VarsTail, HeadTypesTail, ArgModesTail,
                CopiedOutputVars, Context, Decls,
                InputStatements, OutputStatements, !Info)
        ;
            % Generate the lval for the head variable.
            ml_gen_var_with_type(!.Info, Var, HeadType, HeadVarLval),

            % Generate code to box or unbox that head variable,
            % to convert its type from HeadType to BodyType.
            ml_gen_info_get_varset(!.Info, VarSet),
            VarName = ml_gen_var_name(VarSet, Var),
            ml_gen_box_or_unbox_lval(HeadType, BodyType, native_if_possible,
                HeadVarLval, VarName, Context, no, 0, BodyLval, ConvDecls,
                ConvInputStatements, ConvOutputStatements, !Info),

            % Ensure that for any uses of this variable in the procedure body,
            % we use the BodyLval (which has type BodyType) rather than the
            % HeadVarLval (which has type HeadType).
            ml_gen_info_set_var_lval(Var, BodyLval, !Info),

            ml_gen_convert_headvars(VarsTail, HeadTypesTail, ArgModesTail,
                CopiedOutputVars, Context, DeclsTail,
                InputStatementsTail, OutputStatementsTail, !Info),

            % Add the code to convert this input or output.
            ml_gen_info_get_byref_output_vars(!.Info, ByRefOutputVars),
            (
                ( list.member(Var, ByRefOutputVars)
                ; list.member(Var, CopiedOutputVars)
                )
            ->
                InputStatements = InputStatementsTail,
                OutputStatements = OutputStatementsTail ++ ConvOutputStatements
            ;
                InputStatements = ConvInputStatements ++ InputStatementsTail,
                OutputStatements = OutputStatementsTail
            ),
            Decls = ConvDecls ++ DeclsTail
        )
    ;
        unexpected($module, $pred, "length mismatch")
    ).

%-----------------------------------------------------------------------------%
%
% Code for handling tabling structures.
%

:- pred ml_gen_table_structs(module_info::in, list(mlds_defn)::out) is det.

ml_gen_table_structs(ModuleInfo, Defns) :-
    module_info_get_table_struct_map(ModuleInfo, TableStructMap),
    map.to_assoc_list(TableStructMap, TableStructs),
    (
        TableStructs = [],
        Defns = []
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
        expect(isnt(unify(gc_accurate), GC_Method), $module, $pred,
            "tabling and `--gc accurate'"),

        list.foldl(ml_gen_add_table_var(ModuleInfo), TableStructs, [], Defns)
    ).

:- pred ml_gen_add_table_var(module_info::in,
    pair(pred_proc_id, table_struct_info)::in,
    list(mlds_defn)::in, list(mlds_defn)::out) is det.

ml_gen_add_table_var(ModuleInfo, PredProcId - TableStructInfo, !Defns) :-
    module_info_get_name(ModuleInfo, ModuleName),
    MLDS_ModuleName = mercury_module_name_to_mlds(ModuleName),
    PredProcId = proc(_PredId, ProcId),

    TableStructInfo = table_struct_info(ProcTableStructInfo, _Attributes),
    ProcTableStructInfo = proc_table_struct_info(RttiProcLabel, _TVarSet,
        Context, NumInputs, NumOutputs, InputSteps, MaybeOutputSteps,
        _ArgInfos, EvalMethod),

    ml_gen_pred_label_from_rtti(ModuleInfo, RttiProcLabel, PredLabel,
        _PredModule),
    MLDS_ProcLabel = mlds_proc_label(PredLabel, ProcId),
    MLDS_Context = mlds_make_context(Context),
    TableTypeStr = eval_method_to_table_type(EvalMethod),
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
            MLDS_Context, const, tabling_steps_desc(call_table),
            InputStepsInit),
        InputStepsDefns = [InputStepsDefn]
    ),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, MLDS_Context,
        call_table, curr_table, InputSteps,
        CallStatsInit, CallStatsDefns),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, MLDS_Context,
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
            MLDS_Context, const, tabling_steps_desc(answer_table),
            OutputStepsInit),
        OutputStepsDefns = [OutputStepsDefn]
    ),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, MLDS_Context,
        answer_table, curr_table, InputSteps,
        AnswerStatsInit, AnswerStatsDefns),
    init_stats(MLDS_ModuleName, MLDS_ProcLabel, MLDS_Context,
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
        gen_init_builtin_const(TableTypeStr),
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
        MLDS_Context, modifiable, tabling_info, ProcTableInfoInit),

    !:Defns = CallDefns ++ AnswerDefns ++ [ProcTableInfoDefn | !.Defns].

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

:- pred init_stats(mlds_module_name::in, mlds_proc_label::in, mlds_context::in,
    call_or_answer_table::in, curr_or_prev_table::in,
    list(table_step_desc)::in, mlds_initializer::out, list(mlds_defn)::out)
    is det.

init_stats(MLDS_ModuleName, MLDS_ProcLabel, MLDS_Context,
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
            MLDS_Context, modifiable, StatsStepsId, StatsStepsArrayInit),
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

encode_enum_init(EnumConstName) =
    init_obj(ml_const(mlconst_named_const(EnumConstName))).

:- func gen_init_tabling_name(mlds_module_name, mlds_proc_label,
    proc_tabling_struct_id) = mlds_initializer.

gen_init_tabling_name(ModuleName, ProcLabel, TablingId) = Rval :-
    DataAddr = data_addr(ModuleName, mlds_tabling_ref(ProcLabel, TablingId)),
    Rval = init_obj(ml_const(mlconst_data_addr(DataAddr))).

:- func tabling_name_and_init_to_defn(mlds_proc_label, mlds_context, constness,
    proc_tabling_struct_id, mlds_initializer) = mlds_defn.

tabling_name_and_init_to_defn(ProcLabel, MLDS_Context, Constness, Id,
        Initializer) = Defn :-
    GCStatement = gc_no_stmt,
    MLDS_Type = mlds_tabling_type(Id),
    Flags = tabling_data_decl_flags(Constness),
    DefnBody = mlds_data(MLDS_Type, Initializer, GCStatement),
    Name = entity_data(mlds_tabling_ref(ProcLabel, Id)),
    Defn = mlds_defn(Name, MLDS_Context, Flags, DefnBody).

    % Return the declaration flags appropriate for a tabling data structure.
    %
:- func tabling_data_decl_flags(constness) = mlds_decl_flags.

tabling_data_decl_flags(Constness) = MLDS_DeclFlags :-
    Access = acc_private,
    PerInstance = one_copy,
    Virtuality = non_virtual,
    Overridability = sealed,
    Abstractness = concrete,
    MLDS_DeclFlags = init_decl_flags(Access, PerInstance,
        Virtuality, Overridability, Constness, Abstractness).

%-----------------------------------------------------------------------------%
%
% Code for handling attributes.
%

:- func attributes_to_mlds_attributes(module_info, list(hlds_pred.attribute))
    = list(mlds_attribute).

attributes_to_mlds_attributes(ModuleInfo, Attrs) =
    list.map(attribute_to_mlds_attribute(ModuleInfo), Attrs).

:- func attribute_to_mlds_attribute(module_info, hlds_pred.attribute)
    = mlds_attribute.

attribute_to_mlds_attribute(ModuleInfo, custom(Type)) =
    custom(mercury_type_to_mlds_type(ModuleInfo, Type)).

%-----------------------------------------------------------------------------%
:- end_module ml_backend.ml_proc_gen.
%-----------------------------------------------------------------------------%
