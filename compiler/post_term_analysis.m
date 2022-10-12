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
% user-defined special predicates terminate. We emit a warning for all those
% that do not.
%
%----------------------------------------------------------------------------%

:- module transform_hlds.post_term_analysis.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.

:- import_module list.

:- pred run_post_term_analysis(module_info::in, list(error_spec)::out) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.foreign.
:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_pred.
:- import_module hlds.special_pred.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module string.

%----------------------------------------------------------------------------%

run_post_term_analysis(ModuleInfo, Specs) :-
    warn_non_term_user_special_preds(ModuleInfo, Specs).

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
:- pred warn_non_term_user_special_preds(module_info::in,
    list(error_spec)::out) is det.

warn_non_term_user_special_preds(ModuleInfo, !:Specs) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_op_mode(Globals, OpMode),
    globals.lookup_bool_option(Globals, termination, Termination),
    globals.lookup_bool_option(Globals, warn_non_term_special_preds,
        WarnSpecialPreds),
    globals.lookup_bool_option(Globals, transitive_optimization,
        TransIntermodOpt),
    !:Specs = [],
    ( if
        Termination = yes,
        WarnSpecialPreds = yes,

        % Don't run this pass if we are only building the optimization
        % interface and we are compiling with
        % `--transitive-intermodule-optimization' enabled, because we will get
        % more accurate results when we build the .trans_opt files.
        % Any warnings this time around may be spurious.
        not (
            OpMode = opm_top_args(opma_augment(opmau_make_plain_opt)),
            TransIntermodOpt = yes
        )
    then
        module_info_get_type_table(ModuleInfo, TypeTable),
        module_info_get_special_pred_maps(ModuleInfo, SpecialPredMaps),
        % Index predicates cannot be defined by the user and should always
        % terminate by design.
        SpecialPredMaps = special_pred_maps(UnifyMap, _IndexMap, CompareMap),
        map.foldl(
            warn_non_term_user_special_pred_kind(ModuleInfo, TypeTable,
                spec_pred_unify),
            UnifyMap, !Specs),
        map.foldl(
            warn_non_term_user_special_pred_kind(ModuleInfo, TypeTable,
                spec_pred_compare),
            CompareMap, !Specs)
    else
        true
    ).

:- pred warn_non_term_user_special_pred_kind(module_info::in, type_table::in,
    special_pred_id::in, type_ctor::in, pred_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

warn_non_term_user_special_pred_kind(ModuleInfo, TypeTable, SpecialPredId,
        TypeCtor, PredId, !Specs) :-
    % Do not perform this check for builtin types that don't have
    % hlds_type_defns.
    BuiltinTypeCtors = builtin_type_ctors_with_no_hlds_type_defn,
    ( if list.member(TypeCtor, BuiltinTypeCtors) then
        true
    else
        lookup_type_ctor_defn(TypeTable, TypeCtor, TypeDefn),
        get_type_defn_status(TypeDefn, TypeStatus),
        DefinedThisModule = type_status_defined_in_this_module(TypeStatus),
        (
            DefinedThisModule = yes,
            process_special_pred_for_type(ModuleInfo, SpecialPredId,
                TypeCtor, TypeDefn, PredId, !Specs)
        ;
            DefinedThisModule = no
        )
    ).

    % If the specified special predicate for the given type is user-defined
    % then check that it terminates. Emit a warning if it does not.
    %
:- pred process_special_pred_for_type(module_info::in,
    special_pred_id::in, type_ctor::in, hlds_type_defn::in, pred_id::in,
    list(error_spec)::in, list(error_spec)::out) is det.

process_special_pred_for_type(ModuleInfo, SpecialPredId, TypeCtor, TypeDefn,
        PredId, !Specs) :-
    NeedsTermCheck =
        special_pred_needs_term_check(ModuleInfo, SpecialPredId, TypeDefn),
    (
        NeedsTermCheck = no
    ;
        NeedsTermCheck = yes,

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

        % We cannot just look up the termination_info because the
        % termination status of compiler generated wrapper predicates for
        % special preds is always set to terminates. Instead, we check if the
        % body of the generated wrapper predicate terminates.

        ( if goal_cannot_loop_term_info(ModuleInfo, BodyGoal) then
            true
        else
            get_type_defn_context(TypeDefn, Context),
            generate_non_term_user_special_warning(Context, SpecialPredId,
                TypeCtor, !Specs)
        )
    ).

    % Return `yes' iff the specified kind of special_pred for this type
    % needs to have its termination status checked.
    %
:- func special_pred_needs_term_check(module_info, special_pred_id,
    hlds_type_defn) = bool.

special_pred_needs_term_check(ModuleInfo, SpecialPredId, TypeDefn)
        = NeedsTermCheck :-
    get_type_defn_body(TypeDefn, TypeBody),
    get_user_unify_compare(ModuleInfo, TypeBody, MaybeCanonical),
    (
        MaybeCanonical = noncanon(NonCanonical),
        (
            ( NonCanonical = noncanon_uni_cmp(_, _)
            ; NonCanonical = noncanon_uni_only(_)
            ; NonCanonical = noncanon_cmp_only(_)
            ),
            (
                SpecialPredId = spec_pred_unify,
                (
                    ( NonCanonical = noncanon_uni_cmp(_, _)
                    ; NonCanonical = noncanon_uni_only(_)
                    ),
                    NeedsTermCheck = yes
                ;
                    NonCanonical = noncanon_cmp_only(_),
                    NeedsTermCheck = no
                )
            ;
                SpecialPredId = spec_pred_compare,
                (
                    ( NonCanonical = noncanon_uni_cmp(_, _)
                    ; NonCanonical = noncanon_cmp_only(_)
                    ),
                    NeedsTermCheck = yes
                ;
                    NonCanonical = noncanon_uni_only(_),
                    NeedsTermCheck = no
                )
            ;
                SpecialPredId = spec_pred_index,
                % There is no way for users to specify their own index
                % predicates. All index predicates are compiler generated,
                % and all of them are designed to terminate.
                NeedsTermCheck = no
            )
        ;
            NonCanonical = noncanon_abstract(_),
            unexpected($pred, "type is local yet it is noncanon_abstract")
        ;
            NonCanonical = noncanon_subtype,
            NeedsTermCheck = no
        )
    ;
        MaybeCanonical = canon,
        NeedsTermCheck = no
    ).

    % Succeeds if the given type has user-defined equality and/or comparison
    % and returns the relevant information about which predicates implement it.
    %
:- pred get_user_unify_compare(module_info::in, hlds_type_body::in,
    maybe_canonical::out) is det.

get_user_unify_compare(ModuleInfo, TypeBody, MaybeCanonical) :-
    (
        TypeBody = hlds_du_type(type_body_du(_, _, MaybeCanonical, _, _))
    ;
        TypeBody = hlds_solver_type(DetailsSolver),
        DetailsSolver = type_details_solver(_, MaybeCanonical)
    ;
        TypeBody = hlds_foreign_type(ForeignTypeBody),
        ( if
            foreign_type_body_has_user_defined_eq_comp_pred(ModuleInfo,
                ForeignTypeBody, NonCanonical)
        then
            MaybeCanonical = noncanon(NonCanonical)
        else
            MaybeCanonical = canon
        )
    ;
        ( TypeBody = hlds_abstract_type(_)
        ; TypeBody = hlds_eqv_type(_)
        ),
        MaybeCanonical = canon
    ).

:- pred generate_non_term_user_special_warning(prog_context::in,
    special_pred_id::in, type_ctor::in,
    list(error_spec)::in, list(error_spec)::out) is det.

generate_non_term_user_special_warning(Context, SpecialPred, TypeCtor,
        !Specs) :-
    type_ctor_module_name_arity(TypeCtor, TypeModule, TypeName, TypeArity),
    (
        SpecialPred = spec_pred_unify,
        SpecialPredStr = "equality"
    ;
        SpecialPred = spec_pred_index,
        unexpected($pred, "index predicate.")
    ;
        SpecialPred = spec_pred_compare,
        SpecialPredStr = "comparison"
    ),
    SNA = sym_name_arity(qualified(TypeModule, TypeName), TypeArity),
    Pieces = [words("Warning: the user-defined"),
        fixed(SpecialPredStr ++ " predicate"), words("for the type"),
        qual_sym_name_arity(SNA),
        words("cannot be proven to terminate."), nl],
    Spec = simplest_spec($pred, severity_warning, phase_termination_analysis,
        Context, Pieces),
    !:Specs = [Spec | !.Specs].

%----------------------------------------------------------------------------%
:- end_module transform_hlds.post_term_analysis.
%----------------------------------------------------------------------------%
