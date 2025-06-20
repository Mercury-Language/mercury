%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2021, 2024-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_msgs.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.
:- import_module set_tree234.

:- func typecheck_report_max_iterations_exceeded(int) = error_spec.

    % Construct the inferred `pred' or `func' declarations for a list of
    % predicates. Don't construct declarations for the inferred types
    % of predicates that represent assertions.
    %
:- pred construct_type_inference_messages(module_info::in,
    set_tree234(pred_id)::in, list(pred_id)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_markers.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_pred_decl.
:- import_module parse_tree.parse_tree_out_type.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_util.

:- import_module bool.
:- import_module edit_seq.
:- import_module maybe.
:- import_module string.
:- import_module varset.

%---------------------------------------------------------------------------%

typecheck_report_max_iterations_exceeded(MaxIterations) = Spec :-
    Pieces = [words("Type inference iteration limit exceeded."),
        words("This probably indicates that your program has a type error."),
        words("You should declare the types explicitly."),
        words("(The current limit is"), int_fixed(MaxIterations),
        words("iterations."),
        words("You can use the"), quote("--type-inference-iteration-limit"),
        words("option to increase the limit)."), nl],
    Spec = no_ctxt_spec($pred, severity_error, phase_type_check,
        Pieces).

%---------------------------------------------------------------------------%

construct_type_inference_messages(_, _, [], !Specs).
construct_type_inference_messages(ModuleInfo, ValidPredIdSet,
        [PredId | PredIds], !Specs) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_markers(PredInfo, Markers),
    ( if
        marker_is_present(Markers, marker_infer_type),
        set_tree234.contains(ValidPredIdSet, PredId),
        not pred_info_is_promise(PredInfo, _)
    then
        Spec = construct_type_inference_message(ModuleInfo, PredId, PredInfo),
        !:Specs = [Spec | !.Specs]
    else
        true
    ),
    construct_type_inference_messages(ModuleInfo, ValidPredIdSet,
        PredIds, !Specs).

    % Construct a message containing the inferred `pred' or `func' declaration
    % for a single predicate.
    %
:- func construct_type_inference_message(module_info, pred_id, pred_info)
    = error_spec.

construct_type_inference_message(ModuleInfo, PredId, PredInfo) = Spec :-
    PredName = pred_info_name(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    UnqualPredSymName = unqualified(PredName),
    pred_info_get_context(PredInfo, Context),
    pred_info_get_arg_types(PredInfo, VarSet, ExistQVars, Types0),
    strip_module_names_from_type_list(strip_builtin_module_name,
        set_default_func, Types0, Types),
    pred_info_get_class_context(PredInfo, ClassContext),
    pred_info_get_purity(PredInfo, Purity),
    MaybeDet = no,
    VarNamePrint = print_name_only,
    (
        PredOrFunc = pf_predicate,
        ArgTypes = Types,
        MaybeReturnType = no,
        TypeStr = mercury_pred_type_to_string(VarSet, VarNamePrint, ExistQVars,
            UnqualPredSymName, Types, MaybeDet, Purity, ClassContext)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(Types, ArgTypes, ReturnType),
        MaybeReturnType = yes(ReturnType),
        TypeStr = mercury_func_type_to_string(VarSet, VarNamePrint, ExistQVars,
            UnqualPredSymName, ArgTypes, ReturnType, MaybeDet, Purity,
            ClassContext)
    ),
    InferredPieces = [invis_order_default_start(2, ""),
        words("Inferred"), words(TypeStr), nl],

    module_info_get_predicate_table(ModuleInfo, PredicateTable),
    ModuleName = pred_info_module(PredInfo),
    QualPredSymName = qualified(ModuleName, PredName),
    predicate_table_lookup_pf_sym(PredicateTable, is_fully_qualified,
        PredOrFunc, QualPredSymName, AllPredIds),
    list.delete_all(AllPredIds, PredId, AllOtherPredIds),
    PredIsDeclared =
        ( pred(OtherPredId::in) is semidet :-
            module_info_pred_info(ModuleInfo, OtherPredId, OtherPredInfo),
            pred_info_get_markers(OtherPredInfo, OtherPredMarkers),
            not marker_is_present(OtherPredMarkers, marker_infer_type)
        ),
    list.filter(PredIsDeclared, AllOtherPredIds, AllOtherDeclaredPredIds),
    (
        AllOtherDeclaredPredIds = [],
        Spec = conditional_spec($pred, inform_inferred_types, yes,
            severity_informational, phase_type_check,
            [msg(Context, InferredPieces)])
    ;
        AllOtherDeclaredPredIds = [_ | _],
        list.map(
            construct_pred_decl_diff(ModuleInfo, ArgTypes, MaybeReturnType),
            AllOtherDeclaredPredIds, DiffPieceLists),
        Pieces = [invis_order_default_start(2, "")] ++ InferredPieces ++
            list.condense(DiffPieceLists),
        Spec = spec($pred, severity_informational, phase_type_check,
            Context, Pieces)
    ).

:- pred construct_pred_decl_diff(module_info::in,
    list(mer_type)::in, maybe(mer_type)::in, pred_id::in,
    list(format_piece)::out) is det.

construct_pred_decl_diff(ModuleInfo, ActualArgTypes, MaybeActualReturnType,
        OtherPredId, Pieces) :-
    module_info_pred_info(ModuleInfo, OtherPredId, OtherPredInfo),
    pred_info_get_arg_types(OtherPredInfo, OtherTypes),
    % It would be nice if we could print the names of type variables,
    % but there are two problems. The lesser problem is that the names
    % may be different in the varsets of the two predicates, which may be
    % confusing. The more deadly problem is that a type variable that
    % exists in one of the varsets may not exist in the other.
    % Therefore printing variable numbers is the best thing we can do,
    % as a general case. (We *could* do better in the special case
    % where neither problem arises.)
    varset.init(TVarSet),
    (
        MaybeActualReturnType = no,
        list.length(OtherTypes, OtherArity),
        pred_decl_lines(TVarSet, ActualArgTypes, ActualLines),
        pred_decl_lines(TVarSet, OtherTypes, OtherLines)
    ;
        MaybeActualReturnType = yes(ActualReturnType),
        pred_args_to_func_args(OtherTypes, OtherArgTypes, OtherReturnType),
        list.length(OtherArgTypes, OtherArity),
        func_decl_lines(TVarSet, ActualArgTypes, ActualReturnType,
            ActualLines),
        func_decl_lines(TVarSet, OtherArgTypes, OtherReturnType,
            OtherLines)
    ),
    EditParams = edit_params(1, 1, 2), % cost(replace) = cost(delete + insert).
    find_shortest_edit_seq(EditParams, OtherLines, ActualLines, EditSeq),
    find_diff_seq(OtherLines, EditSeq, DiffSeq),
    DiffPieceLists = list.map(diff_to_pieces, DiffSeq),
    list.condense(DiffPieceLists, DiffPieces),
    Pieces = [words("The argument list difference from the arity"),
        int_fixed(OtherArity), words("version is"), nl] ++ DiffPieces.

:- func diff_to_pieces(diff(string)) = list(format_piece).

diff_to_pieces(Diff) = Pieces :-
    (
        Diff = unchanged(Line),
        Pieces = [fixed("  " ++ Line), nl]
    ;
        Diff = deleted(Line),
        Pieces0 = [fixed("- " ++ Line), nl],
        Pieces = color_as_correct(Pieces0)
    ;
        Diff = inserted(Line),
        Pieces0 = [fixed("+ " ++ Line), nl],
        Pieces = color_as_incorrect(Pieces0)
    ).

:- pred pred_decl_lines(tvarset::in, list(mer_type)::in,
    list(string)::out) is det.

pred_decl_lines(TVarSet, ArgTypes, Lines) :-
    ( if list.split_last(ArgTypes, NonLastArgTypes, LastArgType) then
        arg_decl_lines("pred", TVarSet, NonLastArgTypes, LastArgType, "",
            Lines)
    else
        Lines = ["pred"]
    ).

:- pred func_decl_lines(tvarset::in, list(mer_type)::in, mer_type::in,
    list(string)::out) is det.

func_decl_lines(TVarSet, ArgTypes, ReturnType, Lines) :-
    ReturnTypeStr =
        mercury_type_to_string(TVarSet, print_num_only, ReturnType),
    ReturnTypeSuffix = " = " ++ ReturnTypeStr,
    ( if list.split_last(ArgTypes, NonLastArgTypes, LastArgType) then
        arg_decl_lines("func", TVarSet, NonLastArgTypes, LastArgType,
            ReturnTypeSuffix, Lines)
    else
        Lines = ["func" ++ ReturnTypeSuffix]
    ).

:- pred arg_decl_lines(string::in, tvarset::in,
    list(mer_type)::in, mer_type::in, string::in, list(string)::out) is det.

arg_decl_lines(PredOrFuncStr, TVarSet, NonLastArgTypes, LastArgType, Suffix,
        Lines) :-
    NonLastArgTypeStrs = list.map(
        mercury_type_to_string(TVarSet, print_num_only), NonLastArgTypes),
    AddComma =
        (func(NonLastArgTypeStr) = one_indent ++ NonLastArgTypeStr ++ ","),
    NonLastArgTypeLines = list.map(AddComma, NonLastArgTypeStrs),
    LastArgTypeStr =
        mercury_type_to_string(TVarSet, print_num_only, LastArgType),
    Lines = [PredOrFuncStr ++ "("] ++
        NonLastArgTypeLines ++
        [one_indent ++ LastArgTypeStr,
        ")" ++ Suffix].

:- func one_indent = string.

one_indent = "    ".

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_msgs.
%---------------------------------------------------------------------------%
