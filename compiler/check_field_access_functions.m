%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2012 The University of Melbourne.
% Copyright (C) 2014-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: check_field_access_functions.m.
%
% Check that the declarations for field extraction and update functions
% are sensible, and generate error messages for the ones that aren't.
% We can do this only after we have processed every predicate declaration,
% as well as everything that affects either the type table or the
% constructor table.
%
%---------------------------------------------------------------------------%

:- module hlds.make_hlds.check_field_access_functions.
:- interface.

:- import_module hlds.hlds_module.
:- import_module hlds.make_hlds.make_hlds_types.
:- import_module hlds.status.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.

:- import_module list.

:- pred check_preds_if_field_access_function(module_info::in,
    sec_list(item_pred_decl_info)::in,
    list(error_spec)::in, list(error_spec)::out) is det.

:- pred maybe_check_field_access_function(module_info::in,
    sym_name::in, user_arity::in, pred_status::in, prog_context::in,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_pred.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_util.

:- import_module map.
:- import_module maybe.
:- import_module term_context.
:- import_module varset.

%---------------------------------------------------------------------------%

check_preds_if_field_access_function(_ModuleInfo, [], !Specs).
check_preds_if_field_access_function(ModuleInfo, [SecList | SecLists],
        !Specs) :-
    SecList = sec_sub_list(SectionInfo, ItemPredSecls),
    SectionInfo = sec_info(ItemMercuryStatus, _NeedQual),
    item_mercury_status_to_pred_status(ItemMercuryStatus, PredStatus),
    list.foldl(check_pred_if_field_access_function(ModuleInfo, PredStatus),
        ItemPredSecls, !Specs),
    check_preds_if_field_access_function(ModuleInfo, SecLists, !Specs).

:- pred check_pred_if_field_access_function(module_info::in, pred_status::in,
    item_pred_decl_info::in,
    list(error_spec)::in, list(error_spec)::out) is det.

check_pred_if_field_access_function(ModuleInfo, PredStatus, ItemPredDecl,
        !Specs) :-
    ItemPredDecl = item_pred_decl_info(SymName, PredOrFunc, TypesAndMaybeModes,
        _, _, _, _, _, _, _, _, _, Context, _SeqNum),
    (
        PredOrFunc = pf_predicate
    ;
        PredOrFunc = pf_function,
        PredFormArity = types_and_maybe_modes_arity(TypesAndMaybeModes),
        user_arity_pred_form_arity(pf_function, UserArity, PredFormArity),
        maybe_check_field_access_function(ModuleInfo, SymName, UserArity,
            PredStatus, Context, !Specs)
    ).

maybe_check_field_access_function(ModuleInfo, FuncSymName, UserArity,
        FuncStatus, Context, !Specs) :-
    UserArity = user_arity(UserArityInt),
    ( if
        % XXX ARITY Make this take UserArity, not UserArityInt.
        is_field_access_function_name(ModuleInfo, FuncSymName, UserArityInt,
            AccessType, FieldName)
    then
        check_field_access_function(ModuleInfo, AccessType, FieldName,
            FuncSymName, UserArity, FuncStatus, Context, !Specs)
    else
        true
    ).

:- pred check_field_access_function(module_info::in, field_access_type::in,
    sym_name::in, sym_name::in, user_arity::in, pred_status::in,
    prog_context::in, list(error_spec)::in, list(error_spec)::out) is det.

check_field_access_function(ModuleInfo, _AccessType, FieldName, FuncSymName,
        UserArity, FuncStatus, Context, !Specs) :-
    % Check that a function applied to an exported type is also exported.
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    ( if
        % Abstract types have status `abstract_exported', so errors won't be
        % reported for local field access functions for them.
        map.search(CtorFieldTable, FieldName, [FieldDefn]),
        FieldDefn = hlds_ctor_field_defn(_, DefnStatus, _, _, _),
        DefnStatus = type_status(status_exported),
        FuncStatus \= pred_status(status_exported)
    then
        user_arity_pred_form_arity(pf_function, UserArity, PredFormArity),
        PFSymNameArity =
            pf_sym_name_arity(pf_function, FuncSymName, PredFormArity),
        report_field_status_mismatch(Context, PFSymNameArity, !Specs)
    else
        true
    ).

:- pred report_field_status_mismatch(prog_context::in, pf_sym_name_arity::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_field_status_mismatch(Context, PFSymNameArity, !Specs) :-
    Pieces = [words("In declaration of"),
        unqual_pf_sym_name_pred_form_arity(PFSymNameArity), suffix(":"), nl,
        words("error:")] ++
        color_as_subject(
            [words("a field access function for an exported field")]) ++
        color_as_incorrect([words("must also be exported.")]) ++ [nl],
        % XXX Should we add "to ensure consistency"?
    Spec = spec($pred, severity_error, phase_pt2h, Context, Pieces),
    !:Specs = [Spec | !.Specs].

%---------------------------------------------------------------------------%
:- end_module hlds.make_hlds.check_field_access_functions.
%---------------------------------------------------------------------------%
