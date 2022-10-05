%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2003-2008, 2010-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: proc_label.m.
% Main author: zs.
%
% This file defines backend-independent identifiers for procedures that a
% backend can use as the basis for the names of the labels or functions
% implementing those procedures. It also has functions for creating these
% identifiers.

%-----------------------------------------------------------------------------%

:- module backend_libs.proc_label.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

    % Return the id of the procedure specified by the rtti_proc_label.
    %
:- func make_proc_label_from_rtti(rtti_proc_label) = proc_label.

    % Return the id of the specified procedure.
    %
:- func make_proc_label(module_info, pred_id, proc_id) = proc_label.

    % Return the id of the specified mode of the unification procedure
    % for the given type.

:- func make_uni_label(module_info, type_ctor, proc_id) = proc_label.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.pred_name.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module require.
:- import_module string.

make_proc_label_from_rtti(RttiProcLabel) = ProcLabel :-
    RttiProcLabel = rtti_proc_label(PredOrFunc, ThisModule,
        PredModule, PredName, PredFormArity, _ArgTypes, _PredId, ProcId,
        _ProcHeadVarsWithNames, _ArgModes, _CodeModel,
        PredIsImported, _PredIsPseudoImported, Origin,
        _ProcIsExported, _ProcIsImported),
    ProcLabel = do_make_proc_label(PredOrFunc, ThisModule, PredModule,
        PredName, PredFormArity, ProcId, PredIsImported, Origin).

make_proc_label(ModuleInfo, PredId, ProcId) = ProcLabel :-
    module_info_get_name(ModuleInfo, ThisModule),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo,
        _ProcInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    PredIsImported = (if pred_info_is_imported(PredInfo) then yes else no),
    pred_info_get_origin(PredInfo, Origin),

    ProcLabel = do_make_proc_label(PredOrFunc, ThisModule, PredModule,
        PredName, PredFormArity, ProcId, PredIsImported, Origin).

:- func do_make_proc_label(pred_or_func, module_name, module_name,
    string, pred_form_arity, proc_id, bool, pred_origin) = proc_label.

do_make_proc_label(PredOrFunc, ThisModule, PredModule, PredName, PredFormArity,
        ProcId, PredIsImported, Origin) = ProcLabel :-
    ( if Origin = origin_compiler(made_for_uci(SpecialPred, TypeCtor)) then
        ( if
            % All type_ctors other than tuples here should be module qualified,
            % since builtin types are handled separately in polymorphism.m.
            TypeCtor = type_ctor(TypeCtorSymName, TypeArity),
            (
                TypeCtorSymName = unqualified(TypeName),
                type_ctor_is_tuple(TypeCtor),
                TypeModule = mercury_public_builtin_module
            ;
                TypeCtorSymName = qualified(TypeModule, TypeName)
            )
        then
            ( if
                ThisModule \= TypeModule,
                SpecialPred = spec_pred_unify,
                not hlds_pred.in_in_unification_proc_id(ProcId)
            then
                DefiningModule = ThisModule
            else
                DefiningModule = TypeModule
            ),
            proc_id_to_int(ProcId, ProcIdInt),
            ProcLabel = special_proc_label(DefiningModule, SpecialPred,
                TypeModule, TypeName, TypeArity, ProcIdInt)
        else
            unexpected($pred,
                "cannot make label for special pred `" ++ PredName ++ "'")
        )
    else
        ProcLabel = make_user_proc_label(ThisModule, PredIsImported,
            PredOrFunc, PredModule, PredName, PredFormArity, ProcId)
    ).

    % make_user_proc_label(ModuleName, PredIsImported,
    %   PredOrFunc, ModuleName, PredName, Arity, ProcId) = Label:
    %
    % Make a proc_label for a user-defined procedure.
    %
    % The PredIsImported argument should be the result of
    % calling pred_info_is_imported.
    %
:- func make_user_proc_label(module_name, bool, pred_or_func,
    module_name, string, pred_form_arity, proc_id) = proc_label.

make_user_proc_label(ThisModule, PredIsImported, PredOrFunc, PredModule,
        PredName, PredFormArity, ProcId) = ProcLabel :-
    ( if
        % Work out which module supplies the code for the predicate.
        ThisModule \= PredModule,
        PredIsImported = no
    then
        % This predicate is a specialized version of a pred from a `.opt' file.
        DefiningModule = ThisModule
    else
        DefiningModule = PredModule
    ),
    PredFormArity = pred_form_arity(PredFormArityInt),
    proc_id_to_int(ProcId, ProcIdInt),
    ProcLabel = ordinary_proc_label(DefiningModule, PredOrFunc,
        PredModule, PredName, PredFormArityInt, ProcIdInt).

make_uni_label(ModuleInfo, TypeCtor, UniModeNum) = ProcLabel :-
    module_info_get_name(ModuleInfo, ModuleName),
    ( if TypeCtor = type_ctor(qualified(TypeModule, TypeName), Arity) then
        ( if hlds_pred.in_in_unification_proc_id(UniModeNum) then
            Module = TypeModule
        else
            Module = ModuleName
        ),
        proc_id_to_int(UniModeNum, UniModeNumInt),
        ProcLabel = special_proc_label(Module, spec_pred_unify, TypeModule,
            TypeName, Arity, UniModeNumInt)
    else
        unexpected($pred, "unqualified type_ctor")
    ).

%-----------------------------------------------------------------------------%
:- end_module backend_libs.proc_label.
%-----------------------------------------------------------------------------%
