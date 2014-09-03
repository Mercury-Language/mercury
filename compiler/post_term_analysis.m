%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2005-2007,2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: post_term_analysis.m.
% Main author: juliensf.
%
% This module contains various checks that rely on the information produced by
% termination analysis.
%
% Currently, the only thing implemented in this module is a check to see if
% user-defined special predicates terminate.  A warning is emitted for all
% those that do not.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.post_term_analysis.
:- interface.

:- import_module hlds.hlds_module.

:- import_module io.

:- pred run_post_term_analysis(module_info::in, io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.special_pred.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

run_post_term_analysis(ModuleInfo, !IO) :-
    warn_non_term_user_special_preds(ModuleInfo, !IO).

%----------------------------------------------------------------------------%
%
% Warn about user-defined special predicates that do not terminate.
%

    % We check the termination status of user-defined special predicates
    % by taking the body goal of the compiler generated wrapper predicate
    % and checking if that terminates. We cannot check the termination status
    % of the compiler generated wrappers directly, because termination analysis
    % always assumes that they terminate.
    %
    % Since all of the special predicates of interest here have to be defined
    % in the same module as the type that uses them, we only check locally
    % defined types. The ones for imported types will be checked when
    % the relevant module is compiled and analysed.
    %
:- pred warn_non_term_user_special_preds(module_info::in, io::di, io::uo)
    is det.

warn_non_term_user_special_preds(ModuleInfo, !IO) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, termination, Termination),
    globals.lookup_bool_option(Globals, warn_non_term_special_preds,
        WarnSpecialPreds),
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    globals.lookup_bool_option(Globals, transitive_optimization,
        TransIntermodOpt),
    (
        Termination = yes, WarnSpecialPreds = yes,
        %
        % Don't run this pass if we are only building the optimization
        % interface and we are compiling with
        % `--transitive-intermodule-optimization' enabled because we'll get
        % more accurate results when we build the .trans_opt files.  Any
        % warnings this time around may be spurious.
        %
        not (MakeOptInt = yes, TransIntermodOpt = yes)
    ->
        module_info_get_special_pred_map(ModuleInfo, SpecialPredMap),
        module_info_get_type_table(ModuleInfo, TypeTable),
        map.foldl(warn_non_term_user_special_pred(ModuleInfo, TypeTable),
            SpecialPredMap, !IO)
    ;
        true
    ).

:- pred warn_non_term_user_special_pred(module_info::in, type_table::in,
    special_pred::in, pred_id::in, io::di, io::uo) is det.

warn_non_term_user_special_pred(ModuleInfo, TypeTable,
        SpecialPredId - TypeCtor, PredId, !IO) :-
    % Index predicates cannot be defined by the user and should always
    % terminate by design. Do not perform this check for builtin types
    % that don't have hlds_type_defns.

    BuiltinTypeCtors = builtin_type_ctors_with_no_hlds_type_defn,
    (
        SpecialPredId \= spec_pred_index,
        not list.member(TypeCtor, BuiltinTypeCtors)
    ->
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_status(TypeDefn, ImportStatus),
        DefinedThisModule = status_defined_in_this_module(ImportStatus),
        (
            DefinedThisModule = yes,
            process_special_pred_for_type(ModuleInfo,
                SpecialPredId, TypeCtor, PredId, TypeDefn, !IO)
        ;
            DefinedThisModule = no
        )
    ;
        true
    ).

    % If the specified special predicate for the given type is user-defined
    % then check that it terminates.  Emit a warning if it does not.
    %
:- pred process_special_pred_for_type(module_info::in,
    special_pred_id::in, type_ctor::in, pred_id::in,
    hlds_type_defn::in, io::di, io::uo) is det.

process_special_pred_for_type(ModuleInfo, SpecialPredId, TypeCtor,
        PredId, TypeDefn, !IO) :-
    ( special_pred_needs_term_check(ModuleInfo, SpecialPredId, TypeDefn) ->
        % Compiler generated special preds are always mode 0.
        proc_id_to_int(ProcId, 0),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
        proc_info_get_goal(ProcInfo, BodyGoal0),
        % The pretest code we add for compiler-generated unification
        % and comparison predicates uses type casts. It uses them in a way
        % that is guaranteed to terminate, but our analysis is not (yet) able
        % to find this out for itself. We therefore analyse only the
        % non-pretest parts of such goals.
        BodyGoal = maybe_strip_equality_pretest(BodyGoal0),

        % We cannot just look up the the termination_info because the
        % termination status of compiler generated wrapper predicates for
        % special preds is always set to terminates.  Instead, we check if the
        % body of the generated wrapper predicate terminates.

        ( not goal_cannot_loop(ModuleInfo, BodyGoal) ->
            get_type_defn_context(TypeDefn, Context),
            module_info_get_globals(ModuleInfo, Globals),
            emit_non_term_user_special_warning(Globals, Context, SpecialPredId,
                TypeCtor, !IO)
        ;
            true
        )
    ;
        true
    ).

    % Succeeds if the specified type of special_pred for this type needs to
    % have its termination status checked.
    %
:- pred special_pred_needs_term_check(module_info::in,
    special_pred_id::in, hlds_type_defn::in) is semidet.

special_pred_needs_term_check(ModuleInfo, SpecialPredId, TypeDefn) :-
    get_type_defn_body(TypeDefn, TypeBody),
    (
        % Always check solver type initialisation
        % predicates since they are always user-defined.
        SpecialPredId = spec_pred_init
    ;
        get_user_unify_compare(ModuleInfo, TypeBody, UnifyCompare),
        (
            UnifyCompare = unify_compare(MaybeUnify, MaybeCompare),
            (
                MaybeUnify = yes(_),
                SpecialPredId = spec_pred_unify
            ;
                MaybeCompare = yes(_),
                SpecialPredId = spec_pred_compare
            )
        ;
            UnifyCompare = abstract_noncanonical_type(_),
            unexpected($module, $pred,
                "type is local and abstract_noncanonical")
        )
    ).

    % Succeeds if the given type has user-defined equality and/or comparison
    % and returns the relevant information about which predicates implement it.
    %
:- pred get_user_unify_compare(module_info::in, hlds_type_body::in,
    unify_compare::out) is semidet.

get_user_unify_compare(_ModuleInfo, TypeBody, UnifyCompare) :-
    TypeBody = hlds_du_type(_, _, _, _, yes(UnifyCompare), _, _, _, _).
get_user_unify_compare(ModuleInfo, TypeBody, UnifyCompare) :-
    TypeBody = hlds_foreign_type(ForeignTypeBody),
    foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo,
        ForeignTypeBody, UnifyCompare).
get_user_unify_compare(_ModuleInfo, TypeBody, UnifyCompare) :-
    TypeBody = hlds_solver_type(_, yes(UnifyCompare)).

:- pred emit_non_term_user_special_warning(globals::in, prog_context::in,
    special_pred_id::in, type_ctor::in, io::di, io::uo) is det.

emit_non_term_user_special_warning(Globals, Context, SpecialPred, TypeCtor,
        !IO) :-
    type_ctor_module_name_arity(TypeCtor, TypeModule, TypeName, TypeArity),
    (
        SpecialPred = spec_pred_unify,
        SpecialPredStr = "equality"
    ;
        SpecialPred = spec_pred_compare,
        SpecialPredStr = "comparison"
    ;
        SpecialPred = spec_pred_init,
        SpecialPredStr = "initialisation"
    ;
        SpecialPred = spec_pred_index,
        unexpected($module, $pred, "index predicate.")
    ),
    Pieces = [words("Warning: the user-defined "),
        fixed(SpecialPredStr ++ " predicate"),
        words("for the type "),
        sym_name_and_arity(qualified(TypeModule, TypeName) / TypeArity),
        words("cannot be proven to terminate.")],
    report_warning(Globals, Context, 0, Pieces, !IO).

%----------------------------------------------------------------------------%
:- end_module transform_hlds.post_term_analysis.
%----------------------------------------------------------------------------%
