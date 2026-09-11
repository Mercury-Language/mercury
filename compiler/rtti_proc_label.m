%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2016, 2018, 2021-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: rtti_proc_label.m.
%
% This module defines the rtti_proc_label type, and some operations on it.
%
%---------------------------------------------------------------------------%

:- module hlds.rtti_proc_label.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.pred_name.
:- import_module hlds.pred_proc_id.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module assoc_list.
:- import_module bool.
:- import_module list.

%---------------------------------------------------------------------------%

:- type prog_var_name == string.

    % The rtti_proc_label type holds all the information about a procedure
    % that we need to compute the entry label for that procedure
    % in the target language (the llds.code_addr or mlds.code_addr).

:- type rtti_proc_label
    --->    rtti_proc_label(
                rpl_pred_or_func            ::  pred_or_func,
                rpl_this_module             ::  module_name,
                rpl_proc_module             ::  module_name,
                rpl_proc_name               ::  string,
                rpl_proc_arity              ::  pred_form_arity,
                rpl_proc_arg_types          ::  list(mer_type),
                rpl_pred_id                 ::  pred_id,
                rpl_proc_id                 ::  proc_id,
                rpl_proc_headvars           ::  assoc_list(prog_var,
                                                prog_var_name),
                rpl_proc_top_modes          ::  list(top_functor_mode),
                rpl_proc_interface_detism   ::  determinism,

                % The following booleans hold values computed from the
                % pred_info, using procedures
                %   pred_info_is_imported/1,
                %   pred_info_is_pseudo_imported/1,
                %   pred_info_get_origin/1
                % respectively.
                % We store booleans here, rather than storing the
                % pred_info, to avoid retaining a reference to the
                % parts of the pred_info that we aren't interested in,
                % so that those parts can be garbage collected.
                % We use booleans rather than an import_status
                % so that we can continue to use the above-mentioned
                % abstract interfaces rather than hard-coding tests
                % on the import_status.

                rpl_pred_is_imported        ::  bool,
                rpl_pred_is_pseudo_imported ::  bool,
                rpl_pred_info_origin        ::  pred_origin,

                % The following boolean holds a value computed from the
                % proc_info, using procedure_is_exported/2

                rpl_proc_is_exported        ::  bool,

                % The following bool is true if the procedure was
                % imported, either because the containing predicate
                % was imported, or because it was pseudoimported
                % and the procedure is an in-in unify procedure.

                rpl_proc_is_imported        ::  bool
            ).

    % Construct an rtti_proc_label for a given procedure.
    %
:- func make_rtti_proc_label(module_info, pred_id, proc_id) = rtti_proc_label.

    % The inverse of make_rtti_proc_label.
    %
:- pred proc_label_pred_proc_id(rtti_proc_label::in,
    pred_id::out, proc_id::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_proc.
:- import_module hlds.hlds_proc_util.
:- import_module hlds.mode_top_functor.
:- import_module parse_tree.var_table.

:- import_module pair.
:- import_module term.

%---------------------------------------------------------------------------%

make_rtti_proc_label(ModuleInfo, PredId, ProcId) = ProcLabel :-
    module_info_get_name(ModuleInfo, ThisModule),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, PredInfo, ProcInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    PredModule = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    proc_info_get_var_table(ProcInfo, ProcVarTable),
    proc_info_get_headvars(ProcInfo, ProcHeadVars),
    proc_info_get_argmodes(ProcInfo, ProcModes),
    proc_info_interface_determinism(ProcInfo, ProcDetism),
    modes_to_top_functor_modes(ModuleInfo, ProcModes, ArgTypes, ProcTopModes),
    PredIsImported =
        (if pred_info_is_imported(PredInfo) then yes else no),
    PredIsPseudoImp =
        (if pred_info_is_pseudo_imported(PredInfo) then yes else no),
    ProcIsExported =
        (if procedure_is_exported(ModuleInfo, PredInfo, ProcId)
            then yes else no),
    pred_info_get_origin(PredInfo, Origin),
    ProcHeadVarsWithNames = list.map(
        ( func(Var) = Var - Name :-
            Name = var_table_entry_name(ProcVarTable, Var)
        ), ProcHeadVars),
    ( if
        (
            PredIsImported = yes
        ;
            PredIsPseudoImp = yes,
            in_in_unification_proc_id(ProcId)
        )
    then
        ProcIsImported = yes
    else
        ProcIsImported = no
    ),
    ProcLabel = rtti_proc_label(PredOrFunc, ThisModule, PredModule,
        PredName, PredFormArity, ArgTypes, PredId, ProcId,
        ProcHeadVarsWithNames, ProcTopModes, ProcDetism,
        PredIsImported, PredIsPseudoImp, Origin,
        ProcIsExported, ProcIsImported).

proc_label_pred_proc_id(RttiProcLabel, PredId, ProcId) :-
    PredId = RttiProcLabel ^ rpl_pred_id,
    ProcId = RttiProcLabel ^ rpl_proc_id.

%---------------------------------------------------------------------------%
:- end_module hlds.rtti_proc_label.
%---------------------------------------------------------------------------%
