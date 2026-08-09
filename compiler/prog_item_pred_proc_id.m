%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2011 The University of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: prog_item_pred_proc_id.m.
% Author: zs.

% This module defines types that identify predicates, functions
% and/or procedures, in pragmas and in other kinds of items.
%
%---------------------------------------------------------------------------%

:- module parse_tree.prog_item_pred_proc_id.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type pred_pfu_name_arity
    --->    pred_pfu_name_arity(
                ppfuna_pfu              :: pred_func_or_unknown,
                ppfuna_pred_name        :: sym_name,
                ppfuna_arity            :: user_arity
            ).

:- type pred_pfu_name_arity_pf =< pred_pfu_name_arity
    --->    pred_pfu_name_arity(
                ppfuna_pfu              :: pred_func_or_unknown_pf,
                ppfuna_pred_name        :: sym_name,
                ppfuna_arity            :: user_arity
            ).

:- type proc_pf_name_arity_mn
    --->    proc_pf_name_arity_mn(
                ppfnamn_pf              :: pred_or_func,
                ppfnamn_pred_name       :: sym_name,
                ppfnamn_arity           :: user_arity,
                ppfnamn_mode_num        :: mode_num
            ).

:- type proc_pf_name_modes
    --->    proc_pf_name_modes(
                ppfnm_pf                :: pred_or_func,
                ppfnm_pred_name         :: sym_name,
                ppfnm_arity             :: list(mer_mode)
            ).

:- type pred_or_proc_pfumm_name
    --->    pred_or_proc_pfumm_name(
                ppfummn_pfumm           :: pred_func_or_unknown_maybe_modes,
                ppfummn_pred_name       :: sym_name
            ).

:- type pred_func_or_unknown
    --->    pfu_predicate
    ;       pfu_function
    ;       pfu_unknown.

:- type pred_func_or_unknown_pf =< pred_func_or_unknown
    --->    pfu_predicate
    ;       pfu_function.

:- type pred_func_or_unknown_maybe_modes
    --->    pfumm_predicate(modes_or_arity)
    ;       pfumm_function(modes_or_arity)
    ;       pfumm_unknown(user_arity).

:- type modes_or_arity
    --->    moa_modes(list(mer_mode))
    ;       moa_arity(user_arity).

%---------------------------------------------------------------------------%

:- func pfu_to_maybe_pred_or_func(pred_func_or_unknown) = maybe(pred_or_func).
:- func maybe_pred_or_func_to_pfu(maybe(pred_or_func)) = pred_func_or_unknown.

:- pred pfumm_to_maybe_pf_arity_maybe_modes(
    pred_func_or_unknown_maybe_modes::in, maybe(pred_or_func)::out,
    user_arity::out, maybe(list(mer_mode))::out) is det.

%---------------------------------------------------------------------------%

    % Did an item originate in user code or was it added by the compiler
    % as part of a source-to-source transformation, e.g. the initialise
    % declarations? If the latter, specify the information that the
    % make_hlds pass may need to answer questions about the item.
    %
:- type item_maybe_attrs
    --->    item_origin_user
    ;       item_origin_compiler(item_compiler_attributes).

:- type item_compiler_attributes
    --->    item_compiler_attributes(
                compiler_origin
            ).

:- type compiler_origin
    --->    compiler_origin_initialise
    ;       compiler_origin_finalise
    ;       compiler_origin_class_method(
                cm_class_id                     :: class_id,
                cm_method                       :: pf_sym_name_user_arity
            )
    ;       compiler_origin_solver_repn(
                cosr_type_ctor                  :: type_ctor,
                cosr_aux_pred_kind              :: solver_type_pred_kind
            )
    ;       compiler_origin_mutable(
                com_module_name                 :: module_name,
                com_mutable_name                :: string,
                com_aux_pred_kind               :: mutable_pred_kind
            )
    ;       compiler_origin_tabling(
                cot_pred_spec                   :: pf_sym_name_user_arity,
                cot_aux_pred_kind               :: tabling_aux_pred_kind
            ).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.prog_util.

%---------------------------------------------------------------------------%

pfu_to_maybe_pred_or_func(pfu_predicate) = yes(pf_predicate).
pfu_to_maybe_pred_or_func(pfu_function) = yes(pf_function).
pfu_to_maybe_pred_or_func(pfu_unknown) = no.

maybe_pred_or_func_to_pfu(yes(pf_predicate)) = pfu_predicate.
maybe_pred_or_func_to_pfu(yes(pf_function)) = pfu_function.
maybe_pred_or_func_to_pfu(no) = pfu_unknown.

pfumm_to_maybe_pf_arity_maybe_modes(PFUMM, MaybePredOrFunc, UserArity,
        MaybeModes) :-
    (
        (
            PFUMM = pfumm_predicate(ModesOrArity),
            PredOrFunc = pf_predicate
        ;
            PFUMM = pfumm_function(ModesOrArity),
            PredOrFunc = pf_function
        ),
        MaybePredOrFunc = yes(PredOrFunc),
        (
            ModesOrArity = moa_modes(Modes),
            list.length(Modes, NumModes),
            PredFormArity = pred_form_arity(NumModes),
            user_arity_pred_form_arity(PredOrFunc, UserArity, PredFormArity),
            MaybeModes = yes(Modes)
        ;
            ModesOrArity = moa_arity(UserArity),
            MaybeModes = no
        )
    ;
        PFUMM = pfumm_unknown(UserArity),
        MaybePredOrFunc = no,
        MaybeModes = no
    ).

%---------------------------------------------------------------------------%
:- end_module parse_tree.prog_item_pred_proc_id.
%---------------------------------------------------------------------------%
