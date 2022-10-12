%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2007, 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_error_util.m.
% Main author: zs.
%
% This module contains code that can be helpful in the generation or
% formatting of error messages. It builds upon parse_tree.error_spec,
% and extends it with predicates that access HLDS data structures.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_error_util.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
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
    = list(format_piece).

:- func describe_one_pred_info_name(should_module_qualify, pred_info)
    = list(format_piece).

:- func describe_one_pred_name_mode(module_info, output_lang,
    should_module_qualify, pred_id, inst_varset, list(mer_mode))
    = list(format_piece).

:- func describe_several_pred_names(module_info, should_module_qualify,
    list(pred_id)) = list(format_piece).

:- func describe_one_proc_name(module_info, should_module_qualify,
    pred_proc_id) = list(format_piece).

:- func describe_one_proc_name_mode(module_info, output_lang,
    should_module_qualify, pred_proc_id) = list(format_piece).

:- func describe_several_proc_names(module_info, should_module_qualify,
    list(pred_proc_id)) = list(format_piece).

:- func describe_one_call_site(module_info, should_module_qualify,
    pair(pred_proc_id, prog_context)) = list(format_piece).

:- func describe_several_call_sites(module_info, should_module_qualify,
    assoc_list(pred_proc_id, prog_context)) = list(format_piece).

%-----------------------------------------------------------------------------%

    % Return the arities that the given pred_ids have.
    %
:- pred find_pred_arities(pred_id_table::in, list(pred_id)::in,
    list(pred_form_arity)::out) is det.
:- pred find_user_arities(pred_id_table::in, list(pred_id)::in,
    list(user_arity)::out) is det.

    % Return the set of arities that the given pred_ids have,
    % other than the given arity.
    %
:- pred find_pred_arities_other_than(pred_id_table::in, list(pred_id)::in,
    pred_form_arity::in, list(pred_form_arity)::out) is det.
:- pred find_user_arities_other_than(pred_id_table::in, list(pred_id)::in,
    user_arity::in, list(user_arity)::out) is det.

:- func project_user_arity_int(user_arity) = int.

:- func project_pred_form_arity_int(pred_form_arity) = int.

%-----------------------------------------------------------------------------%
%
% Every possible path of execution in mercury_compile.m should call
% definitely_write_out_errors exactly once, just after the compiler
% has finished doing all the things that can generate error reports.
%
% If Verbose = no, then this call is intended to write out all at once
% all the error specifications accumulated until then. They are written out
% all at once so that write_error_specs can sort them by context.
%
% If Verbose = yes, then keeping all the error messages until the end would
% be confusing, since we would be reporting that e.g. the program had type
% errors *before* printing the type error messages. In that case, we want to
% print (using maybe_write_out_errors or its pre-HLDS twin) all the
% accumulated errors before each message to the user.
%
% This applies to *all* messages.
%
% - The calls to maybe_write_out_errors before a message that announces
%   the completion (and success or failure) of a phase obviously report
%   the errors (if any) discovered by the phase.
%
% - The calls to maybe_write_out_errors before a message that announces
%   the phase the compiler is about to enter serve to write out any messages
%   from previous phases that have not yet been written out.
%
%   We could require each phase to write out the errors it discovers when it
%   finishes (if Verbose = yes, that is), but that would eliminate any
%   opportunity to group and sort together the error messages of two or more
%   adjacent phases that are *not* separated by a message to the user even with
%   Verbose = yes. Since the cost of calling maybe_write_out_errors
%   when there is nothing to print is so low (a few dozen instructions),
%   we can easily afford to incur it unnecessarily once per compiler phase.

:- pred definitely_write_out_errors(globals::in,
    list(error_spec)::in, io::di, io::uo) is det.
:- pragma obsolete(pred(definitely_write_out_errors/4),
    [definitely_write_out_errors/5]).
:- pred definitely_write_out_errors(io.text_output_stream::in, globals::in,
    list(error_spec)::in, io::di, io::uo) is det.

:- pred maybe_write_out_errors(bool::in,
    globals::in, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.
:- pragma obsolete(pred(maybe_write_out_errors/6), [maybe_write_out_errors/7]).
:- pred maybe_write_out_errors(io.text_output_stream::in, bool::in,
    globals::in, list(error_spec)::in, list(error_spec)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_name.
:- import_module hlds.special_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_out.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.write_error_spec.

:- import_module int.
:- import_module map.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term_context.

%-----------------------------------------------------------------------------%

describe_one_pred_name(ModuleInfo, ShouldModuleQualify, PredId) = Pieces :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    Pieces = describe_one_pred_info_name(ShouldModuleQualify, PredInfo).

describe_one_pred_info_name(ShouldModuleQualify, PredInfo) = Pieces :-
    % NOTE The code of this predicate duplicates the functionality of
    % hlds_out.write_pred_id. Changes here should be made there as well.
    %
    % XXX This predicate should subcontract its work to pred_name.m.
    PredName = pred_info_name(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    adjust_func_arity(PredOrFunc, OrigArity, Arity),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_origin(PredInfo, Origin),
    ( if Origin = origin_compiler(made_for_uci(SpecialId, TypeCtor)) then
        special_pred_description(SpecialId, Descr),
        TypeCtor = type_ctor(TypeSymName0, TypeArity),
        (
            ShouldModuleQualify = should_module_qualify,
            TypeSymName = TypeSymName0
        ;
            ShouldModuleQualify = should_not_module_qualify,
            TypeSymName = unqualified(unqualify_name(TypeSymName0))
        ),
        ( if TypeArity = 0 then
            Pieces = [words(Descr), words("for type"),
                qual_sym_name(TypeSymName)]
        else
            Pieces = [words(Descr), words("for type constructor"),
                qual_sym_name(TypeSymName)]
        )
    else if check_marker(Markers, marker_class_instance_method) then
        Pieces = [words("type class method implementation")]
    else if pred_info_is_promise(PredInfo, PromiseType) then
        Pieces = [quote(promise_to_string(PromiseType)),
            words("declaration")]
    else
        ( if check_marker(Markers, marker_class_method) then
            Prefix = [words("type class"), p_or_f(PredOrFunc), words("method")]
        else
            Prefix = [p_or_f(PredOrFunc)]
        ),
        PredSymName = qualified(ModuleName, PredName),
        PredSymNameAndArity = sym_name_arity(PredSymName, OrigArity),
        (
            ShouldModuleQualify = should_module_qualify,
            PredSymNamePiece = qual_sym_name_arity(PredSymNameAndArity)
        ;
            ShouldModuleQualify = should_not_module_qualify,
            PredSymNamePiece = unqual_sym_name_arity(PredSymNameAndArity)
        ),
        Pieces = Prefix ++ [PredSymNamePiece]
    ).

describe_one_pred_name_mode(ModuleInfo, Lang, ShouldModuleQualify, PredId,
        InstVarSet, ArgModes0) = Pieces :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    Arity = pred_info_orig_arity(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    list.length(ArgModes0, NumArgModes),
    % We need to strip off the extra type_info arguments inserted at the
    % front by polymorphism.m - we only want the last `Arity' of them.
    ( if list.drop(NumArgModes - Arity, ArgModes0, ArgModes) then
        strip_builtin_qualifiers_from_mode_list(ArgModes, StrippedArgModes)
    else
        unexpected($pred, "bad argument list")
    ),
    (
        PredOrFunc = pf_predicate,
        ArgModesPart = arg_modes_to_string(Lang, InstVarSet, StrippedArgModes)
    ;
        PredOrFunc = pf_function,
        pred_args_to_func_args(StrippedArgModes, FuncArgModes, FuncRetMode),
        ArgModesPart =
            arg_modes_to_string(Lang, InstVarSet, FuncArgModes) ++ " = " ++
            mercury_mode_to_string(Lang, InstVarSet, FuncRetMode)
    ),
    string.append_list([
        "`",
        module_qualification(ModuleName, ShouldModuleQualify),
        PredName,
        "'",
        ArgModesPart], Descr),
    Pieces = [words(Descr)].

describe_several_pred_names(ModuleInfo, ShouldModuleQualify, PredIds)
        = Pieces :-
    PiecesList = list.map(
        describe_one_pred_name(ModuleInfo, ShouldModuleQualify),
        PredIds),
    Pieces = component_lists_to_pieces("and", PiecesList).

describe_one_proc_name(ModuleInfo, ShouldModuleQualify, PredProcId) = Pieces :-
    PredProcId = proc(PredId, ProcId),
    PredPieces = describe_one_pred_name(ModuleInfo, ShouldModuleQualify,
        PredId),
    proc_id_to_int(ProcId, ProcIdInt),
    string.int_to_string(ProcIdInt, ProcIdStr),
    Pieces = PredPieces ++ [words("mode"), words(ProcIdStr)].

describe_one_proc_name_mode(ModuleInfo, Lang, ShouldModuleQualify, PredProcId)
        = Pieces :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, _, ProcInfo),
    proc_info_get_argmodes(ProcInfo, ArgModes),
    proc_info_get_inst_varset(ProcInfo, InstVarSet),
    PredProcId = proc(PredId, _),
    Pieces = describe_one_pred_name_mode(ModuleInfo, Lang, ShouldModuleQualify,
        PredId, InstVarSet, ArgModes).

describe_several_proc_names(ModuleInfo, ShouldModuleQualify, PPIds) = Pieces :-
    PiecesList = list.map(
        describe_one_proc_name(ModuleInfo, ShouldModuleQualify),
        PPIds),
    Pieces = component_lists_to_pieces("and", PiecesList).

describe_one_call_site(ModuleInfo, ShouldModuleQualify, PPId - Context)
        = Pieces :-
    ProcNamePieces = describe_one_proc_name(ModuleInfo, ShouldModuleQualify,
        PPId),
    FileName = term_context.context_file(Context),
    LineNumber = term_context.context_line(Context),
    string.int_to_string(LineNumber, LineNumberStr),
    Pieces = ProcNamePieces ++
        [words("at"), fixed(FileName ++ ":" ++ LineNumberStr)].

describe_several_call_sites(ModuleInfo, ShouldModuleQualify, Sites) = Pieces :-
    PiecesList = list.map(
        describe_one_call_site(ModuleInfo, ShouldModuleQualify),
        Sites),
    Pieces = component_lists_to_pieces("and", PiecesList).

:- func module_qualification(module_name, should_module_qualify) = string.

module_qualification(ModuleName, ShouldModuleQualify) = ModuleQualification :-
    (
        ShouldModuleQualify = should_module_qualify,
        ModuleQualification = sym_name_to_string(ModuleName) ++ "."
    ;
        ShouldModuleQualify = should_not_module_qualify,
        ModuleQualification = ""
    ).

:- func arg_modes_to_string(output_lang, inst_varset, list(mer_mode)) = string.

arg_modes_to_string(Lang, InstVarSet, ArgModes) = Str :-
    (
        ArgModes = [],
        Str = ""
    ;
        ArgModes = [_ | _],
        ArgsStr = mercury_mode_list_to_string(Lang, InstVarSet,
            ArgModes),
        Str = "(" ++ ArgsStr ++ ")"
    ).

%-----------------------------------------------------------------------------%

find_pred_arities(PredTable, PredIds, Arities) :-
    find_pred_arities_set(PredTable, PredIds, AritiesSet),
    set.to_sorted_list(AritiesSet, Arities).

find_user_arities(PredTable, PredIds, Arities) :-
    find_user_arities_set(PredTable, PredIds, AritiesSet),
    set.to_sorted_list(AritiesSet, Arities).

find_pred_arities_other_than(PredTable, PredIds, Arity, OtherArities) :-
    find_pred_arities_set(PredTable, PredIds, AritiesSet),
    set.delete(Arity, AritiesSet, OtherAritiesSet),
    set.to_sorted_list(OtherAritiesSet, OtherArities).

find_user_arities_other_than(PredTable, PredIds, Arity, OtherArities) :-
    find_user_arities_set(PredTable, PredIds, AritiesSet),
    set.delete(Arity, AritiesSet, OtherAritiesSet),
    set.to_sorted_list(OtherAritiesSet, OtherArities).

:- pred find_pred_arities_set(pred_id_table::in, list(pred_id)::in,
    set(pred_form_arity)::out) is det.

find_pred_arities_set(_, [], set.init).
find_pred_arities_set(PredTable, [PredId | PredIds], AritiesSet) :-
    find_pred_arities_set(PredTable, PredIds, AritiesSet0),
    map.lookup(PredTable, PredId, PredInfo),
    PredFormArityInt = pred_info_orig_arity(PredInfo),
    PredFormArity = pred_form_arity(PredFormArityInt),
    set.insert(PredFormArity, AritiesSet0, AritiesSet).

:- pred find_user_arities_set(pred_id_table::in, list(pred_id)::in,
    set(user_arity)::out) is det.

find_user_arities_set(_, [], set.init).
find_user_arities_set(PredTable, [PredId | PredIds], AritiesSet) :-
    find_user_arities_set(PredTable, PredIds, AritiesSet0),
    map.lookup(PredTable, PredId, PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredFormArityInt = pred_info_orig_arity(PredInfo),
    PredFormArity = pred_form_arity(PredFormArityInt),
    user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
    set.insert(UserArity, AritiesSet0, AritiesSet).

%-----------------------------------------------------------------------------%

project_user_arity_int(user_arity(A)) = A.

project_pred_form_arity_int(pred_form_arity(A)) = A.

%-----------------------------------------------------------------------------%

definitely_write_out_errors(Globals, Specs, !IO) :-
    io.output_stream(Stream, !IO),
    definitely_write_out_errors(Stream, Globals, Specs, !IO).

definitely_write_out_errors(Stream, Globals, Specs, !IO) :-
    write_error_specs(Stream, Globals, Specs, !IO).

maybe_write_out_errors(Verbose, Globals, !Specs, !IO) :-
    io.output_stream(Stream, !IO),
    maybe_write_out_errors(Stream, Verbose, Globals, !Specs, !IO).

maybe_write_out_errors(Stream, Verbose, Globals, !Specs, !IO) :-
    % pre_hlds_maybe_write_out_errors in error_spec.m is a pre-HLDS version
    % of this predicate.
    (
        Verbose = no
    ;
        Verbose = yes,
        write_error_specs(Stream, Globals, !.Specs, !IO),
        !:Specs = []
    ).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_error_util.
%-----------------------------------------------------------------------------%
