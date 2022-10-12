%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2022 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% This module checks the validity of format_call pragmas for predicates
% in the module being compiled.
%
% Each format_call pragma specifies one or more pairs of argument positions,
% with each pair specifying the position of a format string argument
% and a values list argument. We check whether the arguments in the
% nominated positions
%
% - actually exist,
% - have the expected types, string and list(poly_type) respectively,
% - and are inputs.
%
% In the presence of type and/or mode inference, the second and third checks
% can be done only after type and mode analysis. And the checks must be done
% before the simplification pass at the end of semantic analysis, because
% that is when format_call.m checks calls to predicates with these pragmas,
% using code that relies on them being correct. The code of this module
% is therefore invoked in the interval in between (provided that there are
% any pragmas to check, which usually there won't be).
%
% If any argument fails the checks above, then, besides generating an error
% message for each failure, we delete the argument pair it is a part of.
% In the usual case of a pragma that consists of one argument pair, this
% effectively deletes the pragma. The point is that any argument pairs
% in any pragmas left after this pass will have passed the above checks.
%
%---------------------------------------------------------------------------%

:- module check_hlds.check_pragma_format_call.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.
:- import_module set.

%---------------------------------------------------------------------------%

:- pred check_pragma_format_call_preds(set(pred_id)::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.mode_test.
:- import_module hlds.hlds_error_util.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module one_or_more.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

check_pragma_format_call_preds(FormatCallPredIds, !ModuleInfo, !Specs) :-
    list.foldl2(check_pragma_format_call_pred,
        set.to_sorted_list(FormatCallPredIds), !ModuleInfo, !Specs).

:- pred check_pragma_format_call_pred(pred_id::in,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_pragma_format_call_pred(PredId, !ModuleInfo, !Specs) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_format_call(PredInfo0, MaybeFormatCall0),
    (
        MaybeFormatCall0 = no,
        % add_pragma.m should add a pred_id to the set of predicates
        % that have a format_call pragma if and only if it also sets
        % this field to yes(...).
        unexpected($pred, "MaybeFormatCall0 = no")
    ;
        MaybeFormatCall0 = yes(FormatCall0)
    ),

    FormatCall0 = format_call(Context, OoMFormatArgs0),
    FormatArgs0 = one_or_more_to_list(OoMFormatArgs0),
    list.length(FormatArgs0, NumFormatArgs0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.to_sorted_assoc_list(ProcTable0, ProcIdsInfos0),
    list.length(ProcIdsInfos0, NumProcs),
    % Setting the maximum argument number that a format_call pragma
    % may refer to be from the *user* arity means that format_call
    % pragmas cannot refer to the return values of functions.
    % The reason for this is that a function's return value
    % is supposed to be output, while the format string and
    % values list arguments are supposed to be inputs.
    user_arity(MaxArgNum) = pred_info_user_arity(PredInfo0),
    pred_info_get_arg_types(PredInfo0, ArgTypes),
    check_format_args(!.ModuleInfo, PredInfo0, Context, MaxArgNum, ArgTypes,
        NumProcs, ProcIdsInfos0, NumFormatArgs0,
        1, FormatArgs0, FormatArgs, !Specs),
    (
        FormatArgs = [HeadFormatArgs | TailFormatArgs],
        OoMFormatArgs = one_or_more(HeadFormatArgs, TailFormatArgs),
        FormatCall = format_call(Context, OoMFormatArgs),
        MaybeFormatCall = yes(FormatCall)
    ;
        FormatArgs = [],
        MaybeFormatCall = no
    ),
    pred_info_set_format_call(MaybeFormatCall, PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred check_format_args(module_info::in, pred_info::in, prog_context::in,
    int::in, list(mer_type)::in, int::in, list(pair(proc_id, proc_info))::in,
    int::in, int::in,
    list(format_string_values)::in, list(format_string_values)::out,
    list(error_spec)::in, list(error_spec)::out) is det.

check_format_args(_, _, _, _, _, _, _, _, _, [], [], !Specs).
check_format_args(ModuleInfo, PredInfo, Context, MaxArgNum, ArgTypes,
        NumProcs, ProcIdsInfos, NumFormatArgs, FormatArgNum,
        [HeadFmtStrVals | TailFmtStrVals], OKFmtStrVals, !Specs) :-
    check_format_args(ModuleInfo, PredInfo, Context, MaxArgNum, ArgTypes,
        NumProcs, ProcIdsInfos, NumFormatArgs, FormatArgNum + 1,
        TailFmtStrVals, TailOKFmtStrVals, !Specs),
    HeadFmtStrVals = format_string_values(OrigArgNumFS, OrigArgNumVL,
        CurArgNumFS, CurArgNumVL),
    some [!CurSpecs] (
        !:CurSpecs = [],
        ( if OrigArgNumFS > MaxArgNum then
            SpecTooLargeFS = format_call_error_too_large_arg_num(PredInfo,
                Context, NumFormatArgs, FormatArgNum, MaxArgNum,
                OrigArgNumFS, "first"),
            !:CurSpecs = [SpecTooLargeFS | !.CurSpecs]
        else if OrigArgNumFS < 1 then
            SpecTooSmallFS = format_call_error_too_small_arg_num(PredInfo,
                Context, NumFormatArgs, FormatArgNum, OrigArgNumFS, "first"),
            !:CurSpecs = [SpecTooSmallFS | !.CurSpecs]
        else
            list.det_index1(ArgTypes, CurArgNumFS, ArgTypeFS),
            ( if ArgTypeFS = string_type then
                true
            else
                SpecWrongTypeFS = format_call_error_wrong_type(PredInfo,
                    Context, NumFormatArgs, FormatArgNum, OrigArgNumFS,
                    "first", "a format string", "string"),
                !:CurSpecs = [SpecWrongTypeFS | !.CurSpecs]
            ),
            list.foldl2(
                check_for_non_input_arg_nums_in_proc(ModuleInfo, CurArgNumFS),
                ProcIdsInfos, 1, _, [], BadModeNumsFS),
            (
                BadModeNumsFS = []
            ;
                BadModeNumsFS = [_ | _],
                SpecWrongModeFS = format_call_error_wrong_mode(PredInfo,
                    Context, NumFormatArgs, FormatArgNum, OrigArgNumFS,
                    "first", "a format string", NumProcs, BadModeNumsFS),
                !:CurSpecs = [SpecWrongModeFS | !.CurSpecs]
            )
        ),
        ( if OrigArgNumVL > MaxArgNum then
            SpecTooLargeVL = format_call_error_too_large_arg_num(PredInfo,
                Context, NumFormatArgs, FormatArgNum, MaxArgNum,
                OrigArgNumVL, "second"),
            !:CurSpecs = [SpecTooLargeVL | !.CurSpecs]
        else if OrigArgNumVL < 1 then
            SpecTooSmallVL = format_call_error_too_small_arg_num(PredInfo,
                Context, NumFormatArgs, FormatArgNum, OrigArgNumVL, "second"),
            !:CurSpecs = [SpecTooSmallVL | !.CurSpecs]
        else
            list.det_index1(ArgTypes, CurArgNumVL, ArgTypeVL),
            ( if
                ArgTypeVL = defined_type(ListTypeCtorSymName,
                    [ListElementType], kind_star),
                ListTypeCtorSymName =
                    qualified(ListTypeCtorModuleName, ListTypeCtorName),
                is_std_lib_module_name(ListTypeCtorModuleName, "list"),
                ListTypeCtorName = "list",
                ListElementType = defined_type(PolyTypeCtorSymName,
                    [], kind_star),
                PolyTypeCtorSymName =
                    qualified(PolyTypeCtorModuleName, PolyTypeCtorName),
                is_std_lib_module_name(PolyTypeCtorModuleName, "string"),
                PolyTypeCtorName = "poly_type"
            then
                true
            else
                SpecWrongTypeVL = format_call_error_wrong_type(PredInfo,
                    Context, NumFormatArgs, FormatArgNum, OrigArgNumVL,
                    "second", "a values list", "list(poly_type)"),
                !:CurSpecs = [SpecWrongTypeVL | !.CurSpecs]
            ),
            list.foldl2(
                check_for_non_input_arg_nums_in_proc(ModuleInfo, CurArgNumVL),
                ProcIdsInfos, 1, _, [], BadModeNumsVL),
            (
                BadModeNumsVL = []
            ;
                BadModeNumsVL = [_ | _],
                SpecWrongModeVL = format_call_error_wrong_mode(PredInfo,
                    Context, NumFormatArgs, FormatArgNum, OrigArgNumVL,
                    "second", "a values list", NumProcs, BadModeNumsVL),
                !:CurSpecs = [SpecWrongModeVL | !.CurSpecs]
            )
        ),
        (
            !.CurSpecs = [],
            OKFmtStrVals = [HeadFmtStrVals | TailOKFmtStrVals]
        ;
            !.CurSpecs = [_ | _],
            pred_info_get_status(PredInfo, PredStatus),
            PredIsLocal = pred_status_defined_in_this_module(PredStatus),
            (
                PredIsLocal = yes,
                !:Specs = !.CurSpecs ++ !.Specs
            ;
                PredIsLocal = no
                % We *could* report !.CurSpecs, but probably it would add
                % only "noise" for the programmer. The problem *will* be
                % reported when the module that we imported PredInfo from
                % is compiled, and until that module is fixed, the only thing
                % the programmer is missing out on is an error message about
                % a module that he/she may not be actively working on
                % at that moment.
            ),
            OKFmtStrVals = TailOKFmtStrVals
        )
    ).

:- pred check_for_non_input_arg_nums_in_proc(module_info::in,
    int::in, pair(proc_id, proc_info)::in, int::in, int::out,
    list(int)::in, list(int)::out) is det.

check_for_non_input_arg_nums_in_proc(ModuleInfo, ArgNum, _ProcId - ProcInfo,
        !CurModeNum, !BadModeNums) :-
    proc_info_get_argmodes(ProcInfo, ArgModes),
    list.det_index1(ArgModes, ArgNum, ArgMode),
    ( if mode_is_input(ModuleInfo, ArgMode) then
        true
    else
        !:BadModeNums = [!.CurModeNum | !.BadModeNums]
    ),
    !:CurModeNum = !.CurModeNum + 1.

%---------------------%

:- func format_call_error_too_large_arg_num(pred_info, prog_context,
    int, int, int, int, string) = error_spec.

format_call_error_too_large_arg_num(PredInfo, Context, NumFormatArgs,
        FormatArgNum, MaxArgNum, ArgNum, FirstOrSecond) = Spec :-
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    Pieces =
        format_call_error_prelude(PredInfo, NumFormatArgs, FormatArgNum) ++
        [words("the"), words(FirstOrSecond), words("argument of"),
        quote("format_string_values"), words("specifies argument number"),
        int_fixed(ArgNum), suffix(","), words("but the"), p_or_f(PredOrFunc),
        words("has only"), int_fixed(MaxArgNum), words("arguments."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- func format_call_error_too_small_arg_num(pred_info, prog_context,
    int, int, int, string) = error_spec.

format_call_error_too_small_arg_num(PredInfo, Context, NumFormatArgs,
        FormatArgNum, ArgNum, FirstOrSecond) = Spec :-
    Pieces =
        format_call_error_prelude(PredInfo, NumFormatArgs, FormatArgNum) ++
        [words("the"), words(FirstOrSecond), words("argument of"),
        quote("format_string_values"), words("specifies argument number"),
        int_fixed(ArgNum), suffix(","),
        words("but argument numbers start at 1."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- func format_call_error_wrong_type(pred_info, prog_context,
        int, int, int, string, string, string) = error_spec.

format_call_error_wrong_type(PredInfo, Context, NumFormatArgs, FormatArgNum,
        ArgNum, FirstOrSecond, Role, ExpectedType) = Spec :-
    Pieces =
        format_call_error_prelude(PredInfo, NumFormatArgs, FormatArgNum) ++
        [words("the"), words(FirstOrSecond), words("argument of"),
        quote("format_string_values"), words("specifies argument number"),
        int_fixed(ArgNum), words("as holding"), words(Role), suffix(","),
        words("but the type of that argument is not"),
        quote(ExpectedType), suffix("."), nl],
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

:- func format_call_error_wrong_mode(pred_info, prog_context,
        int, int, int, string, string, int, list(int)) = error_spec.

format_call_error_wrong_mode(PredInfo, Context, NumFormatArgs, FormatArgNum,
        ArgNum, FirstOrSecond, Role, NumProcs, BadModeNums) = Spec :-
    Pieces0 =
        format_call_error_prelude(PredInfo, NumFormatArgs, FormatArgNum) ++
        [words("the"), words(FirstOrSecond), words("argument of"),
        quote("format_string_values"), words("specifies argument number"),
        int_fixed(ArgNum), words("as holding"), words(Role), suffix(",")],
    ( if NumProcs = 1, BadModeNums = [_] then
        Pieces = Pieces0 ++ [words("but that argument is not input."), nl]
    else
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        list.sort(BadModeNums, SortedBadModeNums),
        BadModeNumPieceLists =
            list.map((func(N) = [nth_fixed(N)]), SortedBadModeNums),
        BadModeNumPieces =
            component_lists_to_pieces("and", BadModeNumPieceLists),
        Pieces = Pieces0 ++ [words("but that argument is not input"),
            words("in the")] ++ BadModeNumPieces ++
            [words("modes of the"), p_or_f(PredOrFunc), suffix("."), nl]
    ),
    Spec = simplest_spec($pred, severity_error, phase_parse_tree_to_hlds,
        Context, Pieces).

%---------------------%

:- func format_call_error_prelude(pred_info, int, int)
    = list(format_piece).

format_call_error_prelude(PredInfo, NumFormatArgs, FormatArgNum) = Pieces :-
    Pieces0 = [words("Error: in the second argument of"),
        pragma_decl("format_call"), words("declaration for")] ++
        describe_one_pred_info_name(should_not_module_qualify, PredInfo) ++
        [suffix(":"), nl],
    ( if FormatArgNum = 1, NumFormatArgs = 1 then
        Pieces = Pieces0
    else
        Pieces = Pieces0 ++ [words("in the"), nth_fixed(FormatArgNum),
            words("element of the list:"), nl]
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.check_pragma_format_call.
%---------------------------------------------------------------------------%
