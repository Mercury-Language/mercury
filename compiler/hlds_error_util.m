%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2017, 2019-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_error_util.m.
% Main author: zs.
%
% This module contains code that can be helpful in the generation or
% formatting of error messages. It builds upon parse_tree.error_spec,
% and extends it with predicates that access HLDS data structures.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module hlds.hlds_error_util.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_table.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.parse_tree_out_info.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module list.
:- import_module maybe.
:- import_module pair.

%---------------------------------------------------------------------------%
%
% Predicates to convert predicate and procedure names to strings.
%

:- type should_module_qualify
    --->    should_module_qualify
    ;       should_not_module_qualify.

    % describe_qual_pred_name(ModuleInfo, PredId) = Spec:
    % describe_unqual_pred_name(ModuleInfo, PredId) = Spec:
    % describe_one_pred_name(ModuleInfo, MaybeColor, Qual, SuffixPieces,
    %   PredId) = Spec:
    %
    % Return a description of the given predicate or function. This
    % description will have one of the forms
    %
    %   predicate `symname'/arity
    %   function `symname'/arity
    %
    % in both cases followed by SuffixPieces.
    %
    % The Qual parameter governs whether the sym_name will contain
    % its full module qualification, or none.
    %
    % If MaybeColor is yes(Color), then the initial "predicate" or "function"
    % will not be in that color, but the`symname'/arity part, *and*
    % the SuffixPieces, will be. (The reason for taking SuffixPieces
    % as an argument is specifically intended to make this possible
    % without redundant switches between colors.)
    %
    % For describe_qual_pred_name/describe_unqual_pred_name, MaybeColor
    % is implicitly "no", SuffixPieces is [], and the value of Qual is
    % given in the predicate name.
    %
:- func describe_qual_pred_name(module_info, pred_id) = list(format_piece).
:- func describe_unqual_pred_name(module_info, pred_id) = list(format_piece).
:- func describe_one_pred_name(module_info, maybe(color_name),
    should_module_qualify, list(format_piece), pred_id) = list(format_piece).

    % describe_one_pred_info_name(MaybeColor, Qual, SuffixPieces, PredInfo)
    %   = Spec:
    %
    % Does the same job as describe_one_pred_name, except for letting the
    % caller do the lookup of the pred_info.
    %
:- func describe_one_pred_info_name(maybe(color_name), should_module_qualify,
    list(format_piece), pred_info) = list(format_piece).

    % describe_several_pred_names(ModuleInfo, MaybeColor, Qual, PredIds)
    %   = Spec:
    %
    % Invoke describe_one_pred_name on each of the given PredIds,
    % and return the results of those invocations joined togther in a list
    % with the final pair of predicate descriptions separated by "and".
    % We pass the given MaybeColor and Qual, and [] as SuffixPieces,
    % to each call to describe_one_pred_name. This is because it does not
    % make sense to add the same suffix to the description of every pred.
    % (Though it may make sense to add it to the last one.)
    %
:- func describe_several_pred_names(module_info, maybe(color_name),
    should_module_qualify, list(pred_id)) = list(format_piece).

%---------------------------------------------------------------------------%

    % describe_one_proc_name_maybe_argmodes(ModuleInfo, Lang, MaybeColor,
    %   Qual, SuffixPieces, PredProcId) = Spec:
    %
    % Return a description of the given procedure.
    %
    % If the procedure is the only procedure in its predicate or function,
    % the description will just a description of the predicate or function
    % as constructed by describe_one_pred_name.
    %
    % If the procedure is NOT the only procedure in its predicate or function,
    % then description will consist of the name of the predicate or function
    % (module qualified if Qual is should_module_qualify), followed by
    % the modes of its arguments. The result will look like one of these:
    %
    %   `name(in, in, out)'
    %   `name(in, in) = out'
    %
:- func describe_one_proc_name_maybe_argmodes(module_info, output_lang,
    maybe(color_name), should_module_qualify, list(format_piece), pred_proc_id)
    = list(format_piece).

%---------------------------------------------------------------------------%

    % describe_qual_proc_name(ModuleInfo, PredProcId) = Spec:
    % describe_unqual_proc_name(ModuleInfo, PredProcId) = Spec:
    % describe_one_proc_name(ModuleInfo, MaybeColor, Qual, SuffixPieces,
    %   PredProcId) = Spec:
    %
    % Return a description of the given procedure. These descriptions
    % will consist of a description of the predicate or function to which
    % the procedure belongs (as constructed by describe_one_pred_name),
    % followed by a suffix of the form "mode N".
    %
:- func describe_qual_proc_name(module_info, pred_proc_id)
    = list(format_piece).
:- func describe_unqual_proc_name(module_info, pred_proc_id)
    = list(format_piece).
:- func describe_one_proc_name(module_info, maybe(color_name),
    should_module_qualify, pred_proc_id) = list(format_piece).

    % describe_several_proc_names(ModuleInfo, MaybeColor, Qual, PredProcIds)
    %   = Spec:
    %
    % Do the same job for procedures as describe_several_pred_names does
    % for predicates and functions.
    %
:- func describe_several_proc_names(module_info, maybe(color_name),
    should_module_qualify, list(pred_proc_id)) = list(format_piece).

%---------------------------------------------------------------------------%

    % describe_one_call_site(ModuleInfo, MaybeColor, Qual, Site) = Pieces:
    %
    % Return a description of a call site, which a pair consisting of the
    % id of the callee, and the context of the call. The description
    % will consist of the description of the callee as returned by
    % describe_one_proc_name, followed by test of the form
    % "at filename:linenumber".
    %
:- func describe_one_call_site(module_info, maybe(color_name),
    should_module_qualify, pair(pred_proc_id, prog_context))
    = list(format_piece).

    % describe_several_call_sites(ModuleInfo, MaybeColor, Qual, Sites)
    %   = Pieces:
    %
    % Invoke describe_one_call_site on each call site, and join the
    % resulting descriptions together with commas and a final "and".
    %
:- func describe_several_call_sites(module_info, maybe(color_name),
    should_module_qualify, assoc_list(pred_proc_id, prog_context))
    = list(format_piece).

%---------------------------------------------------------------------------%

    % Return the arities that the given pred_ids have.
    %
:- pred find_pred_arities(pred_id_table::in, list(pred_id)::in,
    list(pred_form_arity)::out) is det.
:- pred find_user_arities(pred_id_table::in, list(pred_id)::in,
    list(user_arity)::out) is det.

    % Return the arities that the given pred_ids have,
    % other than the given arity.
    %
:- pred find_pred_arities_other_than(pred_id_table::in, list(pred_id)::in,
    pred_form_arity::in, list(pred_form_arity)::out) is det.
:- pred find_user_arities_other_than(pred_id_table::in, list(pred_id)::in,
    user_arity::in, list(user_arity)::out) is det.

:- func project_user_arity_int(user_arity) = int.

:- func project_pred_form_arity_int(pred_form_arity) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_markers.
:- import_module hlds.pred_name.
:- import_module hlds.special_pred.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.parse_tree_out_inst.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module map.
:- import_module string.
:- import_module term_context.

%---------------------------------------------------------------------------%

describe_qual_pred_name(ModuleInfo, PredId) =
    describe_one_pred_name(ModuleInfo, no, should_module_qualify,
        [], PredId).

describe_unqual_pred_name(ModuleInfo, PredId) =
    describe_one_pred_name(ModuleInfo, no, should_not_module_qualify,
        [], PredId).

describe_one_pred_name(ModuleInfo, MaybeColor, ShouldModuleQualify,
        SuffixPieces, PredId) = Pieces :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    Pieces = describe_one_pred_info_name(MaybeColor, ShouldModuleQualify,
        SuffixPieces, PredInfo).

describe_one_pred_info_name(MaybeColor, ShouldModuleQualify, SuffixPieces,
        PredInfo) = Pieces :-
    % NOTE The code of this predicate duplicates the functionality of
    % hlds_out.write_pred_id. Changes here should be made there as well.
    %
    % XXX This predicate should subcontract its work to pred_name.m.
    PredName = pred_info_name(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    pred_info_get_markers(PredInfo, Markers),
    pred_info_get_origin(PredInfo, Origin),
    ( if Origin = origin_compiler(made_for_uci(SpecialId, TypeCtor)) then
        special_pred_description(SpecialId, Descr),
        TypeCtor = type_ctor(TypeSymName, TypeArity),
        (
            ShouldModuleQualify = should_module_qualify,
            TypeSymNamePiece = qual_sym_name(TypeSymName)
        ;
            ShouldModuleQualify = should_not_module_qualify,
            TypeSymNamePiece = unqual_sym_name(TypeSymName)
        ),
        ( if TypeArity = 0 then
            Pieces0 = [words(Descr), words("for type"), TypeSymNamePiece]
        else
            Pieces0 = [words(Descr), words("for type constructor"),
                TypeSymNamePiece]
        ),
        Pieces = maybe_color_pieces(MaybeColor, Pieces0 ++ SuffixPieces)
    else if Origin = origin_user(user_made_class_method(_, PFNA)) then
        PFNA = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
        UserArity = user_arity(UserArityInt),
        SNA = sym_name_arity(SymName, UserArityInt),
        (
            ShouldModuleQualify = should_module_qualify,
            SNAPiece = qual_sym_name_arity(SNA)
        ;
            ShouldModuleQualify = should_not_module_qualify,
            SNAPiece = unqual_sym_name_arity(SNA)
        ),
        Pieces = [words("typeclass method"), p_or_f(PredOrFunc)] ++
            maybe_color_pieces(MaybeColor, [SNAPiece] ++ SuffixPieces)
    else if Origin = origin_user(user_made_instance_method(PFNA, _)) then
        PFNA = pred_pf_name_arity(PredOrFunc, SymName, UserArity),
        UserArity = user_arity(UserArityInt),
        SNA = sym_name_arity(SymName, UserArityInt),
        (
            ShouldModuleQualify = should_module_qualify,
            SNAPiece = qual_sym_name_arity(SNA)
        ;
            ShouldModuleQualify = should_not_module_qualify,
            SNAPiece = unqual_sym_name_arity(SNA)
        ),
        Pieces = [words("instance method"), p_or_f(PredOrFunc)] ++
            maybe_color_pieces(MaybeColor, [SNAPiece] ++ SuffixPieces)
    else if marker_is_present(Markers, marker_class_instance_method) then
        Pieces0 = [words("type class method implementation")] ++ SuffixPieces,
        Pieces = maybe_color_pieces(MaybeColor, Pieces0)
    else if pred_info_is_promise(PredInfo, PromiseType) then
        Pieces0 = [quote(promise_to_string(PromiseType)),
            words("declaration")] ++ SuffixPieces,
        Pieces = maybe_color_pieces(MaybeColor, Pieces0)
    else
        PredOrFunc = pred_info_is_pred_or_func(PredInfo),
        ( if marker_is_present(Markers, marker_class_method) then
            PrefixPieces =
                [words("type class"), p_or_f(PredOrFunc), words("method")]
        else
            PrefixPieces = [p_or_f(PredOrFunc)]
        ),
        SymName = qualified(ModuleName, PredName),
        user_arity_pred_form_arity(PredOrFunc,
            user_arity(UserArityInt), PredFormArity),
        SNA = sym_name_arity(SymName, UserArityInt),
        (
            ShouldModuleQualify = should_module_qualify,
            SNAPiece = qual_sym_name_arity(SNA)
        ;
            ShouldModuleQualify = should_not_module_qualify,
            SNAPiece = unqual_sym_name_arity(SNA)
        ),
        Pieces = PrefixPieces ++
            maybe_color_pieces(MaybeColor, [SNAPiece] ++ SuffixPieces)
    ).

describe_several_pred_names(ModuleInfo, MaybeColor, ShouldModuleQualify,
        PredIds) = Pieces :-
    SuffixPieces = [],
    PiecesList = list.map(
        describe_one_pred_name(ModuleInfo, MaybeColor, ShouldModuleQualify,
            SuffixPieces),
        PredIds),
    Pieces = pieces_list_to_pieces("and", PiecesList).

%---------------------------------------------------------------------------%

describe_one_proc_name_maybe_argmodes(ModuleInfo, Lang, MaybeColor,
        ShouldModuleQualify, SuffixPieces, PredProcId) = Pieces :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    pred_info_get_proc_table(PredInfo, ProcTable),
    map.count(ProcTable, NumProcs),
    ( if NumProcs > 1 then
        pred_info_get_orig_arity(PredInfo, PredFormArity),
        proc_info_get_argmodes(ProcInfo, ArgModes0),
        NumExtraArgs = num_extra_args(PredFormArity, ArgModes0),
        % We need to strip off the extra type_info arguments inserted at the
        % front by polymorphism.m - we only want the last `PredFormArity'
        % of them.
        list.det_drop(NumExtraArgs, ArgModes0, ArgModes),
        strip_module_names_from_mode_list(strip_builtin_module_name,
            set_default_func, ArgModes, StrippedArgModes),
        proc_info_get_inst_varset(ProcInfo, InstVarSet),
        (
            PredOrFunc = pf_predicate,
            ArgModesStr = arg_modes_to_string(Lang, InstVarSet,
                StrippedArgModes)
        ;
            PredOrFunc = pf_function,
            pred_args_to_func_args(StrippedArgModes,
                FuncArgModes, FuncRetMode),
            ArgModesStr =
                arg_modes_to_string(Lang, InstVarSet, FuncArgModes) ++ " = " ++
                mercury_mode_to_string(Lang, InstVarSet, FuncRetMode)
        ),
        MaybeModuleNameDotStr =
            maybe_module_qualification(ModuleName, ShouldModuleQualify),
        % The absence or presence of a distinguished return value argument
        % tells the reader whether we are reporting the name of a predicate
        % or a function; no need to specify that same info in a word as well.
        string.append_list([
            "`",
            MaybeModuleNameDotStr,
            PredName,
            ArgModesStr,
            "'"], Descr),
        Pieces = maybe_color_pieces(MaybeColor, [words(Descr)] ++ SuffixPieces)
    else
        % The Pieces we now return identifies a predicate or a function,
        % not a procedure per se. We *could* add a prefix such as
        % "the only mode of" in front of Pieces to fix this category error, but
        %
        % - even for novices, such a prefix would help only the first few
        %   times they see such a diagnostic, after which it will become
        %   superfluous, and
        %
        % - for non-novices and ex-novices, such a prefix will be clutter
        %   that hurts more than it helps.
        Pieces = describe_one_pred_info_name(MaybeColor, ShouldModuleQualify,
            SuffixPieces, PredInfo)
    ).

:- func maybe_module_qualification(module_name, should_module_qualify)
    = string.

maybe_module_qualification(ModuleName, ShouldModuleQualify)
        = MaybeModuleNameDotStr :-
    (
        ShouldModuleQualify = should_module_qualify,
        MaybeModuleNameDotStr = sym_name_to_string(ModuleName) ++ "."
    ;
        ShouldModuleQualify = should_not_module_qualify,
        MaybeModuleNameDotStr = ""
    ).

:- func arg_modes_to_string(output_lang, inst_varset, list(mer_mode)) = string.

arg_modes_to_string(Lang, InstVarSet, ArgModes0) = Str :-
    (
        ArgModes0 = [],
        Str = ""
    ;
        ArgModes0 = [_ | _],
        ArgModes = list.map(simplify_std_from_to_mode, ArgModes0),
        ArgsStr = mercury_mode_list_to_string(Lang, InstVarSet, ArgModes),
        Str = "(" ++ ArgsStr ++ ")"
    ).

%---------------------------------------------------------------------------%

describe_qual_proc_name(ModuleInfo, PredProcId) =
    describe_one_proc_name(ModuleInfo, no, should_module_qualify,
        PredProcId).

describe_unqual_proc_name(ModuleInfo, PredProcId) =
    describe_one_proc_name(ModuleInfo, no, should_not_module_qualify,
        PredProcId).

describe_one_proc_name(ModuleInfo, MaybeColor, ShouldModuleQualify,
        PredProcId) = Pieces :-
    SuffixPieces = [],
    PredProcId = proc(PredId, ProcId),
    PredPieces = describe_one_pred_name(ModuleInfo, MaybeColor,
        ShouldModuleQualify, SuffixPieces, PredId),
    proc_id_to_int(ProcId, ProcIdInt),
    Pieces = PredPieces ++ [words("mode"), int_fixed(ProcIdInt)].

describe_several_proc_names(ModuleInfo, MaybeColor, ShouldModuleQualify,
        PPIds) = Pieces :-
    PiecesList = list.map(
        describe_one_proc_name(ModuleInfo, MaybeColor, ShouldModuleQualify),
        PPIds),
    Pieces = pieces_list_to_pieces("and", PiecesList).

%---------------------------------------------------------------------------%

describe_one_call_site(ModuleInfo, MaybeColor, ShouldModuleQualify,
        PPId - Context) = Pieces :-
    ProcNamePieces = describe_one_proc_name(ModuleInfo, MaybeColor,
        ShouldModuleQualify, PPId),
    Context = context(FileName, LineNumber),
    string.int_to_string(LineNumber, LineNumberStr),
    Pieces = ProcNamePieces ++
        [words("at"), fixed(FileName ++ ":" ++ LineNumberStr)].

describe_several_call_sites(ModuleInfo, MaybeColor, ShouldModuleQualify,
        Sites) = Pieces :-
    PiecesList = list.map(
        describe_one_call_site(ModuleInfo, MaybeColor, ShouldModuleQualify),
        Sites),
    Pieces = pieces_list_to_pieces("and", PiecesList).

%---------------------------------------------------------------------------%

find_pred_arities(PredTable, PredIds, PredFormArities) :-
    gather_pred_form_arities(PredTable, PredIds, [], PredFormArities0),
    list.sort_and_remove_dups(PredFormArities0, PredFormArities).

find_user_arities(PredTable, PredIds, UserArities) :-
    gather_user_arities(PredTable, PredIds, [], UserArities0),
    list.sort_and_remove_dups(UserArities0, UserArities).

find_pred_arities_other_than(PredTable, PredIds, Arity, OtherArities) :-
    find_pred_arities(PredTable, PredIds, AllArities),
    list.delete_all(AllArities, Arity, OtherArities).

find_user_arities_other_than(PredTable, PredIds, Arity, OtherArities) :-
    find_user_arities(PredTable, PredIds, AllArities),
    list.delete_all(AllArities, Arity, OtherArities).

%---------------------%

:- pred gather_pred_form_arities(pred_id_table::in, list(pred_id)::in,
    list(pred_form_arity)::in, list(pred_form_arity)::out) is det.

gather_pred_form_arities(_, [], !PredFormArities).
gather_pred_form_arities(PredTable, [PredId | PredIds], !PredFormArities) :-
    map.lookup(PredTable, PredId, PredInfo),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    !:PredFormArities = [PredFormArity | !.PredFormArities],
    gather_pred_form_arities(PredTable, PredIds, !PredFormArities).

:- pred gather_user_arities(pred_id_table::in, list(pred_id)::in,
    list(user_arity)::in, list(user_arity)::out) is det.

gather_user_arities(_, [], !UserArities).
gather_user_arities(PredTable, [PredId | PredIds], !UserArities) :-
    map.lookup(PredTable, PredId, PredInfo),
    UserArity = pred_info_user_arity(PredInfo),
    !:UserArities = [UserArity | !.UserArities],
    gather_user_arities(PredTable, PredIds, !UserArities).

%---------------------------------------------------------------------------%

project_user_arity_int(user_arity(A)) = A.

project_pred_form_arity_int(pred_form_arity(A)) = A.

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_error_util.
%---------------------------------------------------------------------------%
