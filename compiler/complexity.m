%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% File: complexity.m.
% Author: zs.

% This module performs a program transformation that gathers information about
% the relationship between the sizes of a procedure's input arguments and the
% performance cost of the procedure in terms of memory and time.

%-----------------------------------------------------------------------------%

:- module transform_hlds__complexity.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module io.
:- import_module std_util.

%-----------------------------------------------------------------------------%

    % read_spec_file(FileName, MaybeNumLinesProcMap, !IO):
    % Try to read in a complexity proc map from FileName. If successful,
    % return the proc map and the number of entries in it. If not, return an
    % error message.
    %
:- pred read_spec_file(string::in,
    maybe_error(pair(int, complexity_proc_map))::out, io::di, io::uo) is det.

    % is_in_complexity_proc_map(ProcMap, ModuleInfo, PredId, ProcId):
    % If PredId/ProcId in ModuleInfo is in ProcMap, return its slot number
    % in the complexity table.
    %
:- func is_in_complexity_proc_map(complexity_proc_map, module_info,
    pred_id, proc_id) = maybe(int).

    % Return the name of the given procedure in the format required by the
    % complexity map file.
    %
:- func complexity_proc_name(module_info, pred_id, proc_id) = string.

    % Transform the given procedure if it is in the complexity map.
    %
:- pred process_proc_msg(int::in, complexity_proc_map::in,
    pred_id::in, proc_id::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_util.
:- import_module check_hlds.polymorphism.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.
:- import_module transform_hlds.term_norm.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

read_spec_file(FileName, MaybeNumLinesProcMap, !IO) :-
    io__open_input(FileName, ResStream, !IO),
    (
        ResStream = error(Error),
        MaybeNumLinesProcMap = error(io__error_message(Error))
    ;
        ResStream = ok(Stream),
        read_spec_file_lines(Stream, 0, NumLines, MaybeError,
            map__init, ProcMap, !IO),
        (
            MaybeError = yes(Msg),
            MaybeNumLinesProcMap = error(Msg)
        ;
            MaybeError = no,
            MaybeNumLinesProcMap = ok(NumLines - ProcMap)
        )
    ).

:- pred read_spec_file_lines(io__input_stream::in, int::in, int::out,
    maybe(string)::out, map(string, int)::in, map(string, int)::out,
    io::di, io::uo) is det.

read_spec_file_lines(Stream, CurLineNum, NumLines, MaybeError, !ProcMap,
        !IO) :-
    io__read_line(Stream, ResLine, !IO),
    (
        ResLine = eof,
        NumLines = CurLineNum,
        MaybeError = no
    ;
        ResLine = error(Error),
        NumLines = CurLineNum,
        MaybeError = yes(io__error_message(Error))
    ;
        ResLine = ok(Chars0),
        list__filter(unify('\n'), Chars0, _, Chars),
        string__from_char_list(Chars, ProcName),
        ( map__insert(!.ProcMap, ProcName, CurLineNum, !:ProcMap) ->
            read_spec_file_lines(Stream, CurLineNum + 1,
                NumLines, MaybeError, !ProcMap, !IO)
        ;
            NumLines = CurLineNum,
            MaybeError = yes("repeated line: " ++ ProcName)
        )
    ).

%-----------------------------------------------------------------------------%

complexity_proc_name(ModuleInfo, PredId, ProcId) = FullName :-
    module_info_get_name(ModuleInfo, ModuleSymName),
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredName = pred_info_name(PredInfo),
    QualifiedName = qualified(ModuleSymName, PredName),
    Arity = pred_info_orig_arity(PredInfo),
    NameAndArity = sym_name_and_arity_to_string(QualifiedName / Arity),
    proc_id_to_int(ProcId, ProcIdInt),
    FullName = NameAndArity ++ "-" ++ int_to_string(ProcIdInt).

is_in_complexity_proc_map(ProcMap, ModuleInfo, PredId, ProcId) = IsInMap :-
    FullName = complexity_proc_name(ModuleInfo, PredId, ProcId),
    ( map__search(ProcMap, FullName, ProcNum) ->
        IsInMap = yes(ProcNum)
    ;
        IsInMap = no
    ).

%-----------------------------------------------------------------------------%

process_proc_msg(NumProcs, ProcMap, PredId, ProcId, !ProcInfo, !ModuleInfo,
        !IO) :-
    IsInMap = is_in_complexity_proc_map(ProcMap, !.ModuleInfo,
        PredId, ProcId),
    (
        IsInMap = yes(ProcNum),
        FullName = complexity_proc_name(!.ModuleInfo, PredId, ProcId),
        globals__io_lookup_bool_option(verbose, Verbose, !IO),
        (
            Verbose = yes,
            pred_id_to_int(PredId, PredIdInt),
            proc_id_to_int(ProcId, ProcIdInt),
            Pieces = [words("% Applying complexity experiment " ++
                    "transformation to "),
                fixed(FullName ++ ":"),
                fixed(int_to_string(PredIdInt) ++ "/" ++
                    int_to_string(ProcIdInt))],
            write_error_pieces_plain(Pieces, !IO)
        ;
            Verbose = no
        ),
        process_proc(NumProcs, ProcNum, FullName, PredId,
            !ProcInfo, !ModuleInfo)
    ;
        IsInMap = no
    ).

% Example of transformation for model_det:
%
% p(In1, ..., InN, ...) :-
%   impure complexity_is_active(NumProcs, ProcNum, ProcName,
%       IsActive, Base),
%   (
%       IsActive = no,
%       impure complexity_call_proc(Slot, In1, ..., InN),
%       <original code>,
%       impure complexity_exit_proc(Slot)
%   ;
%       IsActive = yes,
%       <original code>
%   ).
%
% Example of transformation for model_semi:
%
% p(In1, ..., InN, ...) :-
%   impure complexity_is_active(NumProcs, ProcNum, ProcName,
%       IsActive, Base),
%   (
%       IsActive = no,
%       impure complexity_call_proc(Slot, In1, ..., InN),
%       (
%           <original code>,
%           impure complexity_exit_proc(Slot)
%       ;
%           impure complexity_fail_proc(Slot),
%           fail
%       )
%   ;
%       IsActive = yes,
%       <original code>
%   ).
%
% Example of transformation for model_non:
%
% p(In1, ..., InN, ...) :-
%   impure complexity_is_active(NumProcs, ProcNum, ProcName,
%       IsActive, Base),
%   (
%       IsActive = no,
%       impure complexity_call_proc(Slot, In1, ..., InN),
%       (
%           <original code>,
%           (
%               impure complexity_exit_proc(Slot)
%           ;
%               impure complexity_redo_proc(Slot),
%               fail
%           )
%       ;
%           impure complexity_fail_proc(Slot),
%           fail
%       )
%   ;
%       IsActive = yes,
%       <original code>
%   ).

:- func slot_var_name = string.

slot_var_name = "SlotVar".

:- pred process_proc(int::in, int::in, string::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out)
    is det.

process_proc(NumProcs, ProcNum, FullName, PredId, !ProcInfo, !ModuleInfo) :-
    proc_info_interface_determinism(!.ProcInfo, Detism),
    determinism_to_code_model(Detism, CodeModel),
    proc_info_headvars(!.ProcInfo, HeadVars),
    proc_info_argmodes(!.ProcInfo, ArgModes),
    proc_info_varset(!.ProcInfo, VarSet),
    proc_info_vartypes(!.ProcInfo, VarTypes),
    proc_info_goal(!.ProcInfo, OrigGoal),
    goal_info_get_context(OrigGoalInfo, Context),
    % Even if the original goal doesn't use all of the headvars, the code
    % generated by the transformation does, so we need to compute the
    % nonlocals from the headvars rather than getting it from the
    % nonlocals field in the original goal.
    set__list_to_set(HeadVars, OrigNonLocals),
    OrigGoal = _ - OrigGoalInfo,
    goal_info_get_instmap_delta(OrigGoalInfo, OrigInstMapDelta),
    add_goal_info_purity_feature(purity_impure,
        OrigGoalInfo, ImpureOrigGoalInfo),

    IsActiveVarName = "IsActive",
    generate_new_var(IsActiveVarName, is_active_type, !ProcInfo, IsActiveVar),

    classify_args(HeadVars, ArgModes, !.ModuleInfo, VarSet, VarTypes,
        VarInfos),
    allocate_slot_numbers_cl(VarInfos, 0, NumberedProfiledVars),
    list__length(NumberedProfiledVars, NumProfiledVars),
    generate_slot_goals(ProcNum, NumberedProfiledVars, NumProfiledVars,
        Context, PredId, !ProcInfo, !ModuleInfo, SlotVar, SlotVarName,
        SlotGoals),

    IsActiveOutputArg = foreign_arg(IsActiveVar,
        yes(IsActiveVarName - out_mode), is_active_type),
    SlotInputArg = foreign_arg(SlotVar,
        yes(SlotVarName - in_mode), int_type),

    ProcNumStr = int_to_string(ProcNum),

    IsActivePred = "complexity_is_active",
    IsActiveStr = "\tMR_" ++ IsActivePred ++ "(" ++
        int_to_string(NumProcs) ++ ", "
        ++ ProcNumStr ++ ", """ ++ FullName ++ """, " ++
        int_to_string(NumProfiledVars) ++ ", " ++
        IsActiveVarName ++ ");\n",

    complexity_generate_foreign_proc(IsActivePred, det,
        [IsActiveOutputArg], [], "", IsActiveStr, "", [IsActiveVar],
        !.ModuleInfo, Context, IsActiveGoal),

    ExitPred = "complexity_exit_proc",
    ExitStr = "\tMR_" ++ ExitPred ++ "(" ++
        ProcNumStr ++ ", " ++ slot_var_name ++ ");\n",
    complexity_generate_foreign_proc(ExitPred, det,
        [SlotInputArg], [], "", ExitStr, "", [],
        !.ModuleInfo, Context, ExitGoal),

    FailPred = "complexity_fail_proc",
    FailStr = "\tMR_" ++ FailPred ++ "(" ++
        ProcNumStr ++ ", " ++ slot_var_name ++ ");\n",
    complexity_generate_foreign_proc(FailPred, failure,
        [SlotInputArg], [], "", FailStr, "", [],
        !.ModuleInfo, Context, FailGoal),

    RedoPred = "complexity_redo_proc",
    RedoStr = "\tMR_" ++ RedoPred ++ "(" ++
        ProcNumStr ++ ", " ++ slot_var_name ++ ");\n",
    complexity_generate_foreign_proc(RedoPred, failure,
        [SlotInputArg], [], "", RedoStr, "", [],
        !.ModuleInfo, Context, RedoGoal0),

    (
        CodeModel = model_det,
        TransformedGoalExpr = conj(SlotGoals ++ [OrigGoal, ExitGoal]),
        TransformedGoal = TransformedGoalExpr - ImpureOrigGoalInfo
    ;
        CodeModel = model_semi,
        OrigAfterGoal = conj([OrigGoal, ExitGoal]) - ImpureOrigGoalInfo,
        DisjGoal = disj([OrigAfterGoal, FailGoal]) - ImpureOrigGoalInfo,
        TransformedGoal = conj(SlotGoals ++ [DisjGoal]) - ImpureOrigGoalInfo
    ;
        CodeModel = model_non,
        RedoGoal0 = RedoGoalExpr - RedoGoalInfo0,
        goal_info_add_feature(preserve_backtrack_into,
            RedoGoalInfo0, RedoGoalInfo),
        RedoGoal = RedoGoalExpr - RedoGoalInfo,

        instmap_delta_init_reachable(AfterInstMapDelta),
        goal_info_init(list_to_set([SlotVar]), AfterInstMapDelta,
            multidet, purity_impure, Context, AfterGoalInfo),
        AfterGoal = disj([ExitGoal, RedoGoal]) - AfterGoalInfo,

        OrigAfterGoal = conj([OrigGoal, AfterGoal]) - ImpureOrigGoalInfo,
        DisjGoal = disj([OrigAfterGoal, FailGoal]) - ImpureOrigGoalInfo,
        TransformedGoal = conj(SlotGoals ++ [DisjGoal]) - ImpureOrigGoalInfo
    ),

    mercury_term_size_prof_builtin_module(TSPB),
    SwitchArms = [
        case(cons(qualified(TSPB, "is_inactive"), 0), TransformedGoal),
        case(cons(qualified(TSPB, "is_active"), 0), OrigGoal)
    ],

    SwitchExpr = switch(IsActiveVar, cannot_fail, SwitchArms),
    goal_info_init(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, SwitchGoalInfo),
    SwitchGoal = SwitchExpr - SwitchGoalInfo,

    GoalExpr = conj([IsActiveGoal, SwitchGoal]),
    goal_info_init(OrigNonLocals, OrigInstMapDelta, Detism, purity_impure,
        Context, GoalInfo),
    Goal = GoalExpr - GoalInfo,

    proc_info_set_goal(Goal, !ProcInfo),

    assoc_list__values(VarInfos, Infos),
    ComplexityInfo = complexity_proc_info(ProcNum, FullName, Infos),
    module_info_get_complexity_proc_infos(!.ModuleInfo, ComplexityInfos0),
    ComplexityInfos = [ComplexityInfo | ComplexityInfos0],
    module_info_set_complexity_proc_infos(ComplexityInfos, !ModuleInfo).

%-----------------------------------------------------------------------------%

    % Generate a foreign_proc goal of the form:
    %
    %   MR_ComplexityProc   *proc;
    %
    %   MR_complexity_call_proc(proc_num, slot);
    %   proc = &MR_complexity_procs[proc_num];
    %   MR_complexity_fill_size_slot(proc, slot, num_inputs, 1, size1);
    %   ...
    %   MR_complexity_fill_size_slot(proc, slot, num_inputs, N, sizeN);
    %
    % prefixed by the goals required to generate the typeinfos we need
    % to compute the sizes.
    %
:- pred generate_slot_goals(int::in, assoc_list(prog_var, int)::in,
    int::in, term__context::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    prog_var::out, string::out, list(hlds_goal)::out) is det.

generate_slot_goals(ProcNum, NumberedVars, NumProfiledVars, Context, PredId,
        !ProcInfo, !ModuleInfo, SlotVar, SlotVarName, Goals) :-
    SlotVarName = slot_var_name,
    generate_new_var(SlotVarName, int_type, !ProcInfo, SlotVar),
    ProcVarName = "proc",
    generate_size_goals(NumberedVars, Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        PrefixGoals, ForeignArgs, FillCodeStr),
    SlotVarArg = foreign_arg(SlotVar,
        yes(SlotVarName - out_mode), int_type),
    PredName = "complexity_call_proc",
    DeclCodeStr = "\tMR_ComplexityProc *" ++ ProcVarName ++ ";\n",
    PredCodeStr = "\tMR_" ++ PredName ++ "(" ++
        int_to_string(ProcNum) ++ ", " ++ SlotVarName ++ ");\n",
    ProcStr = "\t" ++ ProcVarName ++ " = &MR_complexity_procs[" ++
        int_to_string(ProcNum) ++ "];\n",
    complexity_generate_foreign_proc(PredName, det, [SlotVarArg],
        ForeignArgs, DeclCodeStr, PredCodeStr, ProcStr ++ FillCodeStr,
        [SlotVar], !.ModuleInfo, Context, CallGoal),
    list__append(PrefixGoals, [CallGoal], Goals).

:- pred generate_size_goals(assoc_list(prog_var, int)::in,
    term__context::in, int::in, string::in, string::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    list(hlds_goal)::out, list(foreign_arg)::out, string::out) is det.

generate_size_goals([], _, _, _, _, _, !ProcInfo, !ModuleInfo, [], [], "").
generate_size_goals([Var - VarSeqNum | NumberedVars], Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        Goals ++ RestGoals, ForeignArgs ++ RestForeignArgs,
        CodeStr ++ RestCodeStr) :-
    generate_size_goal(Var, VarSeqNum, Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        Goals, ForeignArgs, CodeStr),
    generate_size_goals(NumberedVars, Context, NumProfiledVars,
        ProcVarName, SlotVarName, PredId, !ProcInfo, !ModuleInfo,
        RestGoals, RestForeignArgs, RestCodeStr).

:- pred generate_size_goal(prog_var::in, int::in, term__context::in,
    int::in, string::in, string::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    list(hlds_goal)::out, list(foreign_arg)::out, string::out) is det.

generate_size_goal(ArgVar, VarSeqNum, Context, NumProfiledVars, ProcVarName,
        SlotVarName, PredId, !ProcInfo, !ModuleInfo, Goals,
        ForeignArgs, CodeStr) :-
    proc_info_vartypes(!.ProcInfo, VarTypes1),
    map__lookup(VarTypes1, ArgVar, VarType),
    MacroName = "MR_complexity_fill_size_slot",
    make_type_info_var(VarType, Context, PredId, !ProcInfo, !ModuleInfo,
        TypeInfoVar, Goals),
    % Since we just created TypeInfoVar, it isn't in VarTypes1.
    proc_info_vartypes(!.ProcInfo, VarTypes2),
    map__lookup(VarTypes2, TypeInfoVar, TypeInfoType),
    ArgName = "arg" ++ int_to_string(VarSeqNum),
    TypeInfoArgName = "input_typeinfo" ++ int_to_string(VarSeqNum),
    ForeignArg = foreign_arg(ArgVar,
        yes(ArgName - in_mode), VarType),
    ForeignTypeInfoArg = foreign_arg(TypeInfoVar,
        yes(TypeInfoArgName - in_mode), TypeInfoType),
    ForeignArgs = [ForeignTypeInfoArg, ForeignArg],
    CodeStr = "\t" ++ MacroName ++ "(" ++
        ProcVarName ++ ", " ++
        SlotVarName ++ ", " ++
        int_to_string(NumProfiledVars) ++ ", " ++
        int_to_string(VarSeqNum) ++ ",\n\t\t" ++
        "MR_term_size((MR_TypeInfo) " ++
            TypeInfoArgName ++ ", " ++ ArgName ++ "));\n".

%-----------------------------------------------------------------------------%

:- pred generate_new_var(string::in, mer_type::in,
    proc_info::in, proc_info::out, prog_var::out) is det.

generate_new_var(Name, Type, !ProcInfo, Var) :-
    proc_info_varset(!.ProcInfo, VarSet0),
    proc_info_vartypes(!.ProcInfo, VarTypes0),
    varset__new_named_var(VarSet0, Name, Var, VarSet),
    map__set(VarTypes0, Var, Type, VarTypes),
    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo).

:- pred complexity_generate_foreign_proc(string::in, determinism::in,
    list(foreign_arg)::in, list(foreign_arg)::in, string::in, string::in,
    string::in, list(prog_var)::in, module_info::in, term__context::in,
    hlds_goal::out) is det.

complexity_generate_foreign_proc(PredName, Detism, Args, ExtraArgs,
        PrefixCode, Code, SuffixCode, BoundVars, ModuleInfo, Context, Goal) :-
    mercury_term_size_prof_builtin_module(BuiltinModule),
    Attrs0 = default_attributes(c),
    set_may_call_mercury(will_not_call_mercury, Attrs0, Attrs),
    goal_util__generate_foreign_proc(BuiltinModule, PredName, predicate,
        only_mode, Detism, Attrs, Args, ExtraArgs,
        PrefixCode, Code, SuffixCode, [impure_goal],
        ground_vars(BoundVars), ModuleInfo, Context, Goal).

%-----------------------------------------------------------------------------%

:- pred classify_args(list(prog_var)::in, list(mer_mode)::in, module_info::in,
    prog_varset::in, vartypes::in,
    assoc_list(prog_var, complexity_arg_info)::out) is det.

classify_args([], [], _, _, _, []).
classify_args([_ | _], [], _, _, _, _) :-
    unexpected(this_file, "classify_args: lists not same length").
classify_args([], [_ | _], _, _, _, _) :-
    unexpected(this_file, "classify_args: lists not same length").
classify_args([Var | Vars], [Mode | Modes], ModuleInfo, VarSet, VarTypes,
        [Var - complexity_arg_info(MaybeName, Kind) | VarInfos]) :-
    classify_args(Vars, Modes, ModuleInfo, VarSet, VarTypes, VarInfos),
    ( varset__search_name(VarSet, Var, Name) ->
        MaybeName = yes(Name)
    ;
        MaybeName = no
    ),
    ( mode_is_fully_input(ModuleInfo, Mode) ->
        map__lookup(VarTypes, Var, VarType),
        ( zero_size_type(VarType, ModuleInfo) ->
            Kind = complexity_input_fixed_size
        ;
            Kind = complexity_input_variable_size
        )
    ;
        Kind = complexity_output
    ).

%-----------------------------------------------------------------------------%

:- pred allocate_slot_numbers_cl(assoc_list(prog_var, complexity_arg_info)::in,
    int::in, assoc_list(prog_var, int)::out) is det.

allocate_slot_numbers_cl([], _, []).
allocate_slot_numbers_cl([Var - Info | VarInfos], Offset,
        NumberedProfiledVars) :-
    Info = complexity_arg_info(_, Kind),
    ( Kind = complexity_input_variable_size ->
        allocate_slot_numbers_cl(VarInfos, Offset + 1,
            NumberedProfiledVarsTail),
        NumberedProfiledVars = [Var - Offset | NumberedProfiledVarsTail]
    ;
        allocate_slot_numbers_cl(VarInfos, Offset, NumberedProfiledVars)
    ).

:- func ground_vars(list(prog_var)) = assoc_list(prog_var, mer_inst).

ground_vars(Vars) = VarsAndGround :-
    VarsAndGround = list__map(pair_with_ground, Vars).

:- func pair_with_ground(prog_var) = pair(prog_var, mer_inst).

pair_with_ground(Var) = Var - ground(shared, none).

%-----------------------------------------------------------------------------%

:- func is_active_type = mer_type.

is_active_type = Type :-
    mercury_term_size_prof_builtin_module(M),
    construct_type(qualified(M, "complexity_is_active") - 0, [], Type).

%-----------------------------------------------------------------------------%

:- pred make_type_info_var(mer_type::in, term__context::in, pred_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    prog_var::out, list(hlds_goal)::out) is det.

make_type_info_var(Type, Context, PredId, !ProcInfo, !ModuleInfo,
        TypeInfoVar, TypeInfoGoals) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    create_poly_info(!.ModuleInfo, PredInfo0, !.ProcInfo, PolyInfo0),
    polymorphism__make_type_info_var(Type, Context, TypeInfoVar,
        TypeInfoGoals, PolyInfo0, PolyInfo),
    poly_info_extract(PolyInfo, PredInfo0, PredInfo,
        !ProcInfo, !:ModuleInfo),
    require(unify(PredInfo0, PredInfo),
        "complexity__make_type_info_var: modified pred_info").

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "complexity.m".

%-----------------------------------------------------------------------------%
:- end_module complexity.
%-----------------------------------------------------------------------------%
