%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2021-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: fact_table_check.m.
%
% This module checks whether a predicate or function named in a
% fact_table pragma has argument types and modes that are suitable
% for our fact table implementation.
%
%---------------------------------------------------------------------------%

:- module ll_backend.fact_table_check.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_proc_id.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module list.
:- import_module map.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type fact_table_arg_check_result
    --->    fact_table_args_ok(fact_table_gen_info)
    ;       fact_table_args_not_ok(list(err_spec)).

%---------------------------------------------------------------------------%

    % This is the data structure we construct if the semantic checks
    % on the fact table predicate and its procedures all succeed.
:- type fact_table_gen_info
    --->    fact_table_gen_info(
                % Information about the arguments of the predicate.
                fgti_arg_infos          :: list(fact_arg_info),
                % Information about each procedure of the predicate.
                ftgi_proc_info_map      :: fact_table_proc_map,

                % We record the identities of kinds of procedures for use
                % by fact_table_compile_facts: the procedure (if any)
                % whose args are all inputs, and the procedures that have
                % both input and output arguments.
                ftgi_all_in_proc_id     :: maybe(proc_id),
                ftgi_in_out_proc_ids    :: list(proc_id)
            ).

%---------------------%

    % The most important information we record about each predicate argument
    % is its type, which must be one of the types supported in fact tables.
    % However, some parts of fact_table_compile_facts also want to know,
    % for each argument, whether it has a given in mode in *any* procedure.
:- type fact_arg_info
    --->    fact_arg_info(
                fact_arg_type,
                maybe_input_for_some_mode,
                maybe_in_or_output_for_some_mode
            ).

    % XXX UINT - handle uints here too when we support them in fact tables.
:- type fact_arg_type
    --->    fact_arg_type_int
    ;       fact_arg_type_float
    ;       fact_arg_type_string.

:- type maybe_input_for_some_mode
    --->    is_not_input_for_any_mode
    ;       is_input_for_some_mode.

    % XXX I think the bool that this bespoke type replaces was originally
    % meant to be set only for arguments that are output in some mode,
    % but then the code whose job is to fill it in set if the arg
    % is ever either input *or* output, which (since these are the only
    % two modes we support in fact table predicates) means it is *always*
    % set to is_in_or_output_for_some_mode.
:- type maybe_in_or_output_for_some_mode
    --->    is_not_in_or_output_for_any_mode
    ;       is_in_or_output_for_some_mode.

%---------------------%

    % We record, for each argument of each procedure,
    %
    % - the name of the C variable holding it,
    % - whether its mode is input or output,
    % - whether we need to make it unique when we copy it out of the table
    %   (this matters only for strings), and
    % - the pragma var that should represent this argument in the
    %   foreign proc.
    %
    % All this info is in the fact_table_var type.
    % The mode class effectively summarizes the arguments' modes, while
    % the prog_varset here contains the variables in the pragma_vars,
    %
:- type fact_table_proc_map == map(proc_id, fact_table_proc_info).
:- type fact_table_proc_info
    --->    fact_table_proc_info(
                list(fact_table_var),
                fact_table_mode_class,
                prog_varset
            ).

:- type fact_table_var
    --->    fact_table_var(
                ftv_name                :: string,
                ftv_mode                :: fact_table_mode,
                ftv_make_unique         :: maybe_make_unique,
                ftv_pragma_var          :: pragma_var
            ).

:- type fact_table_mode
    --->    fully_in
    ;       fully_out.

:- type maybe_make_unique
    --->    do_not_make_unique
    ;       make_unique.

:- type fact_table_mode_class
    --->    all_in
    ;       in_out
    ;       all_out.

    % Check whether the declaration of the given predicate and its mode(s)
    % are suitable for fact tables. If not, return a message for each error.
    % Otherwise, return an opaque data structure that the caller can give
    % to the other two exported predicates of this module to generate code
    % for the predicate.
    %
:- pred fact_table_check_args(module_info::in, prog_context::in,
    pred_id::in, pred_info::in, fact_table_arg_check_result::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_proc.
:- import_module hlds.inst_test.
:- import_module hlds.mode_test.
:- import_module hlds.mode_util.
:- import_module parse_tree.parse_tree_out_type.

:- import_module int.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

fact_table_check_args(ModuleInfo, PragmaContext, PredId, PredInfo, Result) :-
    pred_info_get_arg_types(PredInfo, Types),
    (
        Types = [],
        % We can say "predicate" because a function has at least one argument,
        % the result.
        Pieces = [words("Error:"), pragma_decl("fact_table"),
            words("declaration for a")] ++
            color_as_incorrect([words("predicate without arguments.")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_fact_table_check,
            PragmaContext, Pieces),
        % Since there are no arguments, they cannot have an unsupported
        % type or mode.
        Result = fact_table_args_not_ok([Spec])
    ;
        Types = [_ | _],
        init_fact_arg_infos(PredInfo, Types, FactArgInfos0, [], TypeSpecs),
        ProcIds = pred_info_all_proc_ids(PredInfo),
        (
            ProcIds = [],
            ModePieces = [words("Error:"), pragma_decl("fact_table"),
                words("declaration for a predicate with")] ++
                color_as_incorrect([words("no declared modes.")]) ++
                [nl],
            ModeSpec = spec($pred, severity_error, phase_fact_table_check,
                PragmaContext, ModePieces),
            ModeSpecs = [ModeSpec],
            FactArgInfos = FactArgInfos0,       % dummy; won't be used
            map.init(FactTableProcMap),         % dummy; won't be used
            MaybeAllInProcId = no,              % dummy; won't be used
            InOutProcIds = []                   % dummy; won't be used
        ;
            ProcIds = [_ | _],
            fact_table_check_proc_modes(ModuleInfo, PredId, PredInfo, ProcIds,
                FactArgInfos0, FactArgInfos, map.init, FactTableProcMap,
                [], RevAllInProcIds, [], RevInOutProcIds, [], ModeSpecs0),
            list.reverse(RevAllInProcIds, AllInProcIds),
            list.reverse(RevInOutProcIds, InOutProcIds),
            (
                AllInProcIds = [],
                MaybeAllInProcId = no,
                ModeSpecs = ModeSpecs0
            ;
                AllInProcIds = [AllInProcId],
                MaybeAllInProcId = yes(AllInProcId),
                ModeSpecs = ModeSpecs0
            ;
                AllInProcIds = [_, _ | _],
                AllInPieces = [words("Error:"), pragma_decl("fact_table"),
                    words("declaration for a predicate with")] ++
                    color_as_incorrect([words("more than one mode"),
                        words("in which all arguments are input.")]) ++
                    [nl],
                AllInSpec = spec($pred, severity_error,
                    phase_fact_table_check, PragmaContext, AllInPieces),
                ModeSpecs = [AllInSpec | ModeSpecs0],
                MaybeAllInProcId = no   % dummy; won't be used
            )
        ),
        Specs = TypeSpecs ++ ModeSpecs,
        (
            Specs = [],
            GenInfo = fact_table_gen_info(FactArgInfos, FactTableProcMap,
                MaybeAllInProcId, InOutProcIds),
            Result = fact_table_args_ok(GenInfo)
        ;
            Specs = [_ | _],
            Result = fact_table_args_not_ok(Specs)
        )
    ).

:- pred fact_table_check_proc_modes(module_info::in, pred_id::in,
    pred_info::in, list(proc_id)::in,
    list(fact_arg_info)::in, list(fact_arg_info)::out,
    fact_table_proc_map::in, fact_table_proc_map::out,
    list(proc_id)::in, list(proc_id)::out,
    list(proc_id)::in, list(proc_id)::out,
    list(err_spec)::in, list(err_spec)::out) is det.

fact_table_check_proc_modes(_, _, _, [],
        !FactArgInfos, !FactTableProcMap,
        !RevAllInProcIds, !RevInOutProcIds, !Specs).
fact_table_check_proc_modes(ModuleInfo, PredId, PredInfo, [ProcId | ProcIds],
        !FactArgInfos, !FactTableProcMap,
        !RevAllInProcIds, !RevInOutProcIds, !Specs) :-
    pred_info_get_arg_types(PredInfo, ArgTypes),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.lookup(ProcTable, ProcId, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    PredProcId = proc(PredId, ProcId),
    varset.init(VarSet0),
    check_proc_arg_modes(ModuleInfo, PredProcId, ProcInfo,
        1, ArgTypes, ArgModes, FactTableVars, VarSet0, VarSet,
        [], ArgModeSpecs),
    (
        ArgModeSpecs = [_ | _],
        !:Specs = ArgModeSpecs ++ !.Specs
        % There is no point in processing this procedure any further.
    ;
        ArgModeSpecs = [],
        FactTableModes = list.map((func(fact_table_var(_, M, _, _)) = M),
            FactTableVars),
        fill_in_fact_arg_infos(FactTableModes, !FactArgInfos),
        list.sort_and_remove_dups(FactTableModes, PresentModes),
        ( if PresentModes = [fully_in] then
            ModeClass = all_in,
            !:RevAllInProcIds = [ProcId | !.RevAllInProcIds]
        else if PresentModes = [fully_in, fully_out] then
            ModeClass = in_out,
            !:RevInOutProcIds = [ProcId | !.RevInOutProcIds]
        else if PresentModes = [fully_out] then
            ModeClass = all_out
        else
            unexpected($pred, "impossible mode class")
        ),
        FactTableProcInfo =
            fact_table_proc_info(FactTableVars, ModeClass, VarSet),
        map.det_insert(ProcId, FactTableProcInfo, !FactTableProcMap)
    ),
    fact_table_check_proc_modes(ModuleInfo, PredId, PredInfo, ProcIds,
        !FactArgInfos, !FactTableProcMap,
        !RevAllInProcIds, !RevInOutProcIds, !Specs).

%---------------------%

    % Initialise list of fact argument information. Input and output flags
    % are initialised to `no' and filled in correctly by
    % infer_determinism_pass_1.
    %
:- pred init_fact_arg_infos(pred_info::in, list(mer_type)::in,
    list(fact_arg_info)::out,
    list(err_spec)::in, list(err_spec)::out) is det.

init_fact_arg_infos(_, [], [], !Specs).
init_fact_arg_infos(PredInfo, [Type | Types], [Info | Infos], !Specs) :-
    ( if
        Type = builtin_type(BuiltinType),
        (
            BuiltinType = builtin_type_int(int_type_int),
            FactArgTypePrime = fact_arg_type_int
        ;
            BuiltinType = builtin_type_float,
            FactArgTypePrime = fact_arg_type_float
        ;
            BuiltinType = builtin_type_string,
            FactArgTypePrime = fact_arg_type_string
        )
    then
        FactArgType = FactArgTypePrime
    else
        pred_info_get_typevarset(PredInfo, TVarSet),
        TypeStr = mercury_type_to_string(TVarSet, print_name_only, Type),
        pred_info_get_context(PredInfo, Context),
        Pieces = [words("Error: type")] ++
            color_as_subject([quote(TypeStr)]) ++
            [words("is")] ++
            color_as_incorrect([words("not allowed")]) ++
            [words("in fact tables."),
            words("The only types allowed in fact tables are")] ++
            color_as_correct([quote("int"), suffix(","),
                quote("float"), suffix(",")]) ++
            [words("and")] ++
            color_as_correct([quote("string"), suffix(".")]) ++
            [nl],
        Spec = spec($pred, severity_error, phase_fact_table_check,
            Context, Pieces),
        !:Specs = [Spec | !.Specs],
        FactArgType = fact_arg_type_int % Dummy; won't be used.
    ),
    Info = fact_arg_info(FactArgType,
        is_not_input_for_any_mode, is_not_in_or_output_for_any_mode),
    init_fact_arg_infos(PredInfo, Types, Infos, !Specs).

:- pred check_proc_arg_modes(module_info::in, pred_proc_id::in, proc_info::in,
    int::in, list(mer_type)::in, list(mer_mode)::in, list(fact_table_var)::out,
    prog_varset::in, prog_varset::out,
    list(err_spec)::in, list(err_spec)::out) is det.

check_proc_arg_modes(_, _, _, _, [], [], [], !VarSet, !Specs).
check_proc_arg_modes(_, _, _, _, [], [_ | _], _, !VarSet, !Specs) :-
    unexpected($pred, "list length mismatch").
check_proc_arg_modes(_, _, _, _, [_ | _], [], _, !VarSet, !Specs) :-
    unexpected($pred, "list length mismatch").
check_proc_arg_modes(ModuleInfo, PredProcId, ProcInfo,
        ArgNum, [Type | Types], [Mode | Modes],
        [FactTableVar | FactTableVars], !VarSet, !Specs) :-
    ( if mode_get_insts_semidet(ModuleInfo, Mode, _, FinalInst) then
        ( if mode_is_fully_input(ModuleInfo, Type, Mode) then
            FactTableMode = fully_in
        else if mode_is_fully_output(ModuleInfo, Type, Mode) then
            FactTableMode = fully_out
        else
            ProcPieces = describe_one_proc_name(ModuleInfo, yes(color_subject),
                should_not_module_qualify, PredProcId),
            proc_info_get_context(ProcInfo, Context),
            Pieces = [words("Error: the"), pragma_decl("fact_table"),
                words("declaration requires all the arguments of")] ++
                ProcPieces ++
                [words("to be either fully input or fully output,"),
                words("but the"), nth_fixed(ArgNum), words("argument is")] ++
                color_as_incorrect([words("neither.")]) ++
                [nl],
            Spec = spec($pred, severity_error, phase_fact_table_check,
                Context, Pieces),
            !:Specs = [Spec | !.Specs],
            FactTableMode = fully_in        % dummy; won't be used.
        ),
        ( if inst_is_not_partly_unique(ModuleInfo, FinalInst) then
            MakeUnique = do_not_make_unique
        else
            MakeUnique = make_unique
        )
    else
        % Module qualification will catch and report this error,
        % so generating an error message here for the user would be redundant.
        % However, we *did* find an error that prevents us from generating
        % fact table code for this procedure, and we have to signal this
        % by an err_spec. So we generate and return an empty err_spec.
        proc_info_get_context(ProcInfo, Context),
        Spec = spec($pred, severity_error, phase_fact_table_check,
            Context, []),
        !:Specs = [Spec | !.Specs],
        FactTableMode = fully_in,       % dummy; won't be used.
        MakeUnique = do_not_make_unique % dummy; won't be used.
    ),
    % The name we set is the default name for an unnamed variable,
    % because old hand-written C code uses this name.
    % This works only because for variables in foreign_procs never have
    % their variable numbers appended to their name, precisely to avoid
    % mismatches with the foreign language code inside the foreign_proc.
    string.format("V_%d", [i(ArgNum)], VarName),
    varset.new_named_var(VarName, Var, !VarSet),
    PragmaVar = pragma_var(Var, VarName, Mode, bp_native_if_possible),
    FactTableVar =
        fact_table_var(VarName, FactTableMode, MakeUnique, PragmaVar),
    check_proc_arg_modes(ModuleInfo, PredProcId, ProcInfo,
        ArgNum + 1, Types, Modes, FactTableVars, !VarSet, !Specs).

:- pred fill_in_fact_arg_infos(list(fact_table_mode)::in,
    list(fact_arg_info)::in, list(fact_arg_info)::out) is det.

fill_in_fact_arg_infos([], [], []).
fill_in_fact_arg_infos([_ | _], [], _) :-
    unexpected($pred, "too many argmodes").
fill_in_fact_arg_infos([], [_ | _], _) :-
    unexpected($pred, "too many fact_arg_infos").
fill_in_fact_arg_infos([FactTableMode | FactTableModes],
        [Info0 | Infos0], [Info | Infos]) :-
    Info0 = fact_arg_info(Type, IsInput, _IsOutput),
    (
        FactTableMode = fully_in,
        % XXX Info = fact_arg_info(Type, yes, IsOutput)

        % XXX currently the first input mode requires _all_ arguments to be
        % written in the fact data table so it can do lookups on backtracking.
        % This may change if it is found to be less efficient than doing these
        % lookups via the hash table.
        Info = fact_arg_info(Type,
            is_input_for_some_mode, is_in_or_output_for_some_mode)
    ;
        FactTableMode = fully_out,
        Info = fact_arg_info(Type, IsInput, is_in_or_output_for_some_mode)
    ),
    fill_in_fact_arg_infos(FactTableModes, Infos0, Infos).

%---------------------------------------------------------------------------%
:- end_module ll_backend.fact_table_check.
%---------------------------------------------------------------------------%
