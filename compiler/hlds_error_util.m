%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_error_util.m.
% Main author: zs.
%
% This module contains code that can be helpful in the formatting of
% error messages. It builds upon parse_tree.error_util, and extends it
% with predicates that access HLDS data structures.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_error_util.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.error_util.

:- import_module assoc_list.
:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%
%
% Predicates to convert predicate and procedure names to strings.
%

:- type should_module_qualify
    --->    should_module_qualify
    ;       should_not_module_qualify.

:- func describe_one_pred_name(module_info, should_module_qualify, pred_id)
    = list(format_component).

:- func describe_one_pred_info_name(should_module_qualify, pred_info)
    = list(format_component).

:- func describe_one_pred_name_mode(module_info, should_module_qualify,
    pred_id, inst_varset, list(mer_mode)) = list(format_component).

:- func describe_several_pred_names(module_info, should_module_qualify,
    list(pred_id)) = list(format_component).

:- func describe_one_proc_name(module_info, should_module_qualify,
    pred_proc_id) = list(format_component).

:- func describe_one_proc_name_mode(module_info, should_module_qualify,
    pred_proc_id) = list(format_component).

:- func describe_several_proc_names(module_info, should_module_qualify,
    list(pred_proc_id)) = list(format_component).

:- func describe_one_call_site(module_info, should_module_qualify,
    pair(pred_proc_id, prog_context)) = list(format_component).

:- func describe_several_call_sites(module_info, should_module_qualify,
    assoc_list(pred_proc_id, prog_context)) = list(format_component).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.special_pred.
:- import_module mdbcomp.prim_data.
:- import_module libs.compiler_util.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module string.
:- import_module list.
:- import_module term.

%-----------------------------------------------------------------------------%

    % NOTE: the code of this predicate duplicates the functionality of
    % hlds_out.write_pred_id. Changes here should be made there as well.
    %
describe_one_pred_name(Module, ShouldModuleQualify, PredId) = Pieces :-
    module_info_pred_info(Module, PredId, PredInfo),
    Pieces = describe_one_pred_info_name(ShouldModuleQualify, PredInfo).

describe_one_pred_info_name(ShouldModuleQualify, PredInfo) = Pieces :-
    PredName = pred_info_name(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_origin(PredInfo, Origin),
    ( Origin = origin_special_pred(SpecialId - TypeCtor) ->
        special_pred_description(SpecialId, Descr),
        TypeCtor = type_ctor(TypeSymName0, TypeArity),
        (
            ShouldModuleQualify = should_module_qualify,
            TypeSymName = TypeSymName0
        ;
            ShouldModuleQualify = should_not_module_qualify,
            TypeSymName = unqualified(unqualify_name(TypeSymName0))
        ),
        ( TypeArity = 0 ->
            Pieces = [words(Descr), words("for type"),
                sym_name(TypeSymName)]
        ;
            Pieces = [words(Descr), words("for type constructor"),
                sym_name(TypeSymName)]
        )
    ; check_marker(Markers, marker_class_instance_method) ->
        Pieces = [words("type class method implementation")]
    ; pred_info_get_goal_type(PredInfo, goal_type_promise(PromiseType)) ->
        Pieces = [words("`" ++ promise_to_string(PromiseType) ++ "'"),
            words("declaration")]
    ;
        ( check_marker(Markers, marker_class_method) ->
            Prefix = [words("type class"), p_or_f(PredOrFunc), words("method")]
        ;
            Prefix = [p_or_f(PredOrFunc)]
        ),
        (
            ShouldModuleQualify = should_module_qualify,
            PredSymName = qualified(ModuleName, PredName)
        ;
            ShouldModuleQualify = should_not_module_qualify,
            PredSymName = unqualified(PredName)
        ),
        Pieces = Prefix ++ [sym_name_and_arity(PredSymName / OrigArity)]
    ).

describe_one_pred_name_mode(Module, ShouldModuleQualify, PredId, InstVarSet,
        ArgModes0) = Pieces :-
    module_info_pred_info(Module, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    list.length(ArgModes0, NumArgModes),
    % We need to strip off the extra type_info arguments inserted at the
    % front by polymorphism.m - we only want the last `Arity' of them.
    ( list.drop(NumArgModes - Arity, ArgModes0, ArgModes) ->
        strip_builtin_qualifiers_from_mode_list(ArgModes, StrippedArgModes)
    ;
        unexpected(this_file, "describe_one_pred_name_mode: bad argument list")
    ),
    (
        PredOrFunc = pf_predicate,
        ArgModesPart = arg_modes_to_string(InstVarSet, StrippedArgModes)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(StrippedArgModes, FuncArgModes, FuncRetMode),
        ArgModesPart = arg_modes_to_string(InstVarSet, FuncArgModes)
            ++ " = " ++ mercury_mode_to_string(FuncRetMode, InstVarSet)
    ),
    string.append_list([
        "`",
        module_qualification(ModuleName, ShouldModuleQualify),
        PredName,
        "'",
        ArgModesPart], Descr),
    Pieces = [words(Descr)].

describe_several_pred_names(Module, ShouldModuleQualify, PredIds) = Pieces :-
    PiecesList = list.map(describe_one_pred_name(Module, ShouldModuleQualify),
        PredIds),
    Pieces = component_lists_to_pieces(PiecesList).

describe_one_proc_name(Module, ShouldModuleQualify, proc(PredId, ProcId))
        = Pieces :-
    PredPieces = describe_one_pred_name(Module, ShouldModuleQualify, PredId),
    proc_id_to_int(ProcId, ProcIdInt),
    string.int_to_string(ProcIdInt, ProcIdStr),
    Pieces = PredPieces ++ [words("mode"), words(ProcIdStr)].

describe_one_proc_name_mode(Module, ShouldModuleQualify, proc(PredId, ProcId))
        = Pieces :-
    module_info_pred_proc_info(Module, PredId, ProcId, _, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    Pieces = describe_one_pred_name_mode(Module, ShouldModuleQualify,
        PredId, InstVarSet, ArgModes).

describe_several_proc_names(Module, ShouldModuleQualify, PPIds) = Pieces :-
    PiecesList = list.map(describe_one_proc_name(Module, ShouldModuleQualify),
        PPIds),
    Pieces = component_lists_to_pieces(PiecesList).

describe_one_call_site(Module, ShouldModuleQualify, PPId - Context) = Pieces :-
    ProcNamePieces = describe_one_proc_name(Module, ShouldModuleQualify,
        PPId),
    term.context_file(Context, FileName),
    term.context_line(Context, LineNumber),
    string.int_to_string(LineNumber, LineNumberStr),
    Pieces = ProcNamePieces ++
        [words("at"), fixed(FileName ++ ":" ++ LineNumberStr)].

describe_several_call_sites(Module, ShouldModuleQualify, Sites) = Pieces :-
    PiecesList = list.map(describe_one_call_site(Module, ShouldModuleQualify),
        Sites),
    Pieces = component_lists_to_pieces(PiecesList).

:- func module_qualification(module_name, should_module_qualify) = string.

module_qualification(ModuleName, ShouldModuleQualify) = ModuleQualification :-
    (
        ShouldModuleQualify = should_module_qualify,
        ModuleQualification = sym_name_to_string(ModuleName) ++ "."
    ;
        ShouldModuleQualify = should_not_module_qualify,
        ModuleQualification = ""
    ).

:- func arg_modes_to_string(inst_varset, list(mer_mode)) = string.

arg_modes_to_string(InstVarSet, ArgModes) = Str :-
    (
        ArgModes = [],
        Str = ""
    ;
        ArgModes = [_ | _],
        ArgsStr = mercury_mode_list_to_string(ArgModes, InstVarSet),
        Str = "(" ++ ArgsStr ++ ")"
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "hlds_error_util.m".

%-----------------------------------------------------------------------------%
:- end_module hlds_error_util.
%-----------------------------------------------------------------------------%
