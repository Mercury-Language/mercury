%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: typecheck_error_util.m.
%
% This file defines types, predicates and functions that are useful
% when generating error messages for more than one kind of error.
%
%---------------------------------------------------------------------------%

:- module check_hlds.typecheck_error_util.
:- interface.

:- import_module check_hlds.type_assign.
:- import_module check_hlds.typecheck_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

:- type arg_vector_kind
    --->    arg_vector_clause_head
    ;       arg_vector_plain_call_pred_id(pred_id)
    ;       arg_vector_plain_pred_call(sym_name_pred_form_arity)
    ;       arg_vector_generic_call(generic_call_id)
    ;       arg_vector_foreign_proc_call(pred_id)
    ;       arg_vector_event(string).

:- type var_vector_kind
    --->    var_vector_args(arg_vector_kind)
    ;       var_vector_cond_quant
    ;       var_vector_exist_quant
    ;       var_vector_promise_solutions
    ;       var_vector_switch_complete
    ;       var_vector_switch_arm_detism
    ;       var_vector_loop_control
    ;       var_vector_try_io
    ;       var_vector_atomic_output
    ;       var_vector_atomic_outer.

:- type type_error_goal_context
    --->    type_error_in_var_vector(
                % What kind of variable vector is it?
                teiav_kind                      :: var_vector_kind,

                % The argument number within that vector of variables.
                teiav_arg_num                   :: int
            )
    ;       type_error_in_unify(
                % The original source of the unification we are checking.
                teiu_unify_context              :: unify_context
            )
    ;       type_error_in_atomic_inner.

%---------------------------------------------------------------------------%

:- func describe_cons_type_info_source(module_info, cons_type_info_source)
    = list(format_piece).

:- func describe_args_type_assign_source(module_info, args_type_assign_source)
    = list(format_piece).

%---------------------------------------------------------------------------%

:- func goal_context_to_pieces(type_error_clause_context,
    type_error_goal_context) = list(format_piece).

:- func arg_vector_kind_to_pieces(type_error_clause_context, arg_vector_kind)
    = list(format_piece).

%---------------------------------------------------------------------------%

    % This function generates the preamble (initial part of) all type error
    % messages, giving the name of the predicate or function in which the error
    % occurred.
    %
:- func in_clause_for_pieces(type_error_clause_context) =
    list(format_piece).

%---------------------------------------------------------------------------%

:- pred get_inst_varset(type_error_clause_context::in, inst_varset::out)
    is det.

%---------------------------------------------------------------------------%

    % Check whether two types are identical, i.e. whether they can be unified
    % without binding any type parameters.
    %
:- pred identical_types(mer_type::in, mer_type::in) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.prog_type_unify.

:- import_module map.
:- import_module require.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

describe_cons_type_info_source(ModuleInfo, Source) = Pieces :-
    (
        Source = source_type(TypeCtor, _ConsId),
        Pieces = [words("the type constructor"), qual_type_ctor(TypeCtor)]
    ;
        Source = source_builtin_type(TypeCtorName),
        Pieces = [words("the builtin type constructor"), quote(TypeCtorName)]
    ;
        Source = source_field_access(GetOrSet, TypeCtor,
            _ConsId, _FieldName),
        ( GetOrSet = get, GetOrSetStr = "get"
        ; GetOrSet = set, GetOrSetStr = "set"
        ),
        Pieces = [words("a"), quote(GetOrSetStr),
            words("field access function"),
            words("for the type constructor"), qual_type_ctor(TypeCtor)]
    ;
        Source = source_pred(PredId),
        Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)
    ;
        Source = source_apply(ApplyOp),
        Pieces = [words("the builtin operator constructor"), quote(ApplyOp)]
    ).

describe_args_type_assign_source(ModuleInfo, Source) = Pieces :-
    (
        Source = atas_pred(PredId),
        Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
            PredId)
    ;
        Source = atas_cons(ConsSource),
        (
            ConsSource = source_type(TypeCtor, ConsId),
            Pieces = [words("the functor"),
                unqual_cons_id_and_maybe_arity(ConsId),
                words("of the type constructor"), qual_type_ctor(TypeCtor)]
        ;
            ConsSource = source_builtin_type(TypeCtorName),
            Pieces = [words("the builtin type constructor"),
                quote(TypeCtorName)]
        ;
            ConsSource = source_field_access(GetOrSet, TypeCtor, ConsId,
                FieldName),
            ( GetOrSet = get, GetOrSetStr = "get"
            ; GetOrSet = set, GetOrSetStr = "set"
            ),
            Pieces = [words("the"), quote(GetOrSetStr),
                words("access function for the"), fixed(FieldName),
                words("field of the"), unqual_cons_id_and_maybe_arity(ConsId),
                words("function symbol of the type constructor"),
                qual_type_ctor(TypeCtor)]
        ;
            ConsSource = source_pred(PredId),
            Pieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
                PredId)
        ;
            ConsSource = source_apply(ApplyOp),
            Pieces = [words("the builtin operator constructor"),
                quote(ApplyOp)]
        )
    ;
        Source = atas_higher_order_call(_PredVar),
        % We can't print _PredVar without a varset.
        Pieces = []
    ;
        Source = atas_ensure_have_a_type,
        % These should not occur in errors at all.
        Pieces = []
    ).

%---------------------------------------------------------------------------%

goal_context_to_pieces(ClauseContext, GoalContext) = Pieces :-
    (
        GoalContext = type_error_in_var_vector(VarVectorKind, ArgNum),
        (
            VarVectorKind = var_vector_args(ArgVectorKind),
            (
                ArgVectorKind = arg_vector_clause_head,
                Pieces = [words("in argument"),
                    invis_order_default_end(ArgNum),
                    int_fixed(ArgNum), words("of the clause head:"), nl]
            ;
                (
                    ArgVectorKind =
                        arg_vector_plain_pred_call(SymNamePredFormArity),
                    SymNamePredFormArity =
                        sym_name_pred_form_arity(SymName, PredFormArity),
                    PFSymNameArity = pf_sym_name_arity(pf_predicate, SymName,
                        PredFormArity),
                    CallId = plain_call_id(PFSymNameArity)
                ;
                    ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
                    ModuleInfo = ClauseContext ^ tecc_module_info,
                    module_info_pred_info(ModuleInfo, PredId, PredInfo),
                    pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity),
                    CallId = plain_call_id(PFSymNameArity)
                ;
                    ArgVectorKind = arg_vector_generic_call(GenericId),
                    CallId = generic_call_id(GenericId)
                ),
                PredMarkers = ClauseContext ^ tecc_pred_markers,
                Pieces = [words("in"),
                    words(call_arg_id_to_string(CallId, ArgNum, PredMarkers)),
                    suffix(":"), nl]
            ;
                ArgVectorKind = arg_vector_foreign_proc_call(_PredId),
                % During typechecking, call_foreign_proc goals can occur
                % only in foreign clauses derived from foreign_proc
                % "clauses". Any error we report for them is for the
                % foreign_proc as a whole, not for a "call" to the
                % foreign_proc.
                %
                % Our caller will prefix the Pieces we return here
                % with the identification of the predicate or function
                % whose foreign_proc the error is for, which is all
                % the context that the error needs. So there is nothing
                % useful we can add here.
                Pieces = []
            ;
                ArgVectorKind = arg_vector_event(EventName),
                Pieces = [words("in argument"),
                    invis_order_default_end(ArgNum), int_fixed(ArgNum),
                    words("of event"), fixed(EventName), suffix(":"), nl]
            )
        ;
            VarVectorKind = var_vector_cond_quant,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("quantified variable in if-then-else condition:"), nl]
        ;
            VarVectorKind = var_vector_exist_quant,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("variable of quantification scope:"), nl]
        ;
            VarVectorKind = var_vector_promise_solutions,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("variable of promise_solutions scope:"), nl]
        ;
            VarVectorKind = var_vector_switch_complete,
            Pieces = [words("in the"),
                words("variable of require_switch_complete scope:"), nl]
        ;
            VarVectorKind = var_vector_switch_arm_detism,
            Pieces = [words("in the"),
                words("variable of require_switch_arm_detism scope:"), nl]
        ;
            VarVectorKind = var_vector_loop_control,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum),
                words("variable of loop control scope:"), nl]
        ;
            VarVectorKind = var_vector_try_io,
            ( if ArgNum = 1 then
                Pieces = [invis_order_default_end(1),
                    words("in initial I/O state variable of try goal:"), nl]
            else if ArgNum = 2 then
                Pieces = [invis_order_default_end(2),
                    words("in final I/O state variable of try goal:"), nl]
            else
                unexpected($pred, "try io variable not arg 1 or 2")
            )
        ;
            VarVectorKind = var_vector_atomic_output,
            Pieces = [words("in the"), invis_order_default_end(ArgNum),
                nth_fixed(ArgNum), words("output variable of atomic goal:"),
                nl]
        ;
            VarVectorKind = var_vector_atomic_outer,
            ( if ArgNum = 1 then
                Pieces = [invis_order_default_end(1),
                    words("in the first outer variable"),
                    words("of atomic goal:"), nl]
            else if ArgNum = 2 then
                Pieces = [invis_order_default_end(2),
                    words("in the second outer variable"),
                    words("of atomic goal:"), nl]
            else
                unexpected($pred, "outer variable not arg 1 or 2")
            )
        )
    ;
        GoalContext = type_error_in_unify(UnifyContext),
        unify_context_to_pieces(UnifyContext, [], Pieces)
    ;
        GoalContext = type_error_in_atomic_inner,
        Pieces = [words("in inner variable of atomic goal:"), nl]
    ).

arg_vector_kind_to_pieces(ClauseContext, ArgVectorKind) = Pieces :-
    (
        ArgVectorKind = arg_vector_clause_head,
        Pieces = [words("in arguments of the clause head:"), nl]
    ;
        (
            ArgVectorKind = arg_vector_plain_pred_call(SymNamePredFormArity),
            SymNamePredFormArity =
                sym_name_pred_form_arity(SymName, PredFormArity),
            PFSymNameArity =
                pf_sym_name_arity(pf_predicate, SymName, PredFormArity),
            CallId = plain_call_id(PFSymNameArity)
        ;
            ArgVectorKind = arg_vector_plain_call_pred_id(PredId),
            ModuleInfo = ClauseContext ^ tecc_module_info,
            module_info_pred_info(ModuleInfo, PredId, PredInfo),
            pred_info_get_pf_sym_name_arity(PredInfo, PFSymNameArity),
            CallId = plain_call_id(PFSymNameArity)
        ;
            ArgVectorKind = arg_vector_generic_call(GenericId),
            CallId = generic_call_id(GenericId)
        ),
        PredMarkers = ClauseContext ^ tecc_pred_markers,
        Pieces = [words("in"),
            words(call_arg_id_to_string(CallId, -1, PredMarkers)),
            suffix(":"), nl]
    ;
        ArgVectorKind = arg_vector_foreign_proc_call(_PredId),
        unexpected($pred, "arg_vector_foreign_proc_call")
    ;
        ArgVectorKind = arg_vector_event(EventName),
        Pieces = [words("in arguments of event"), fixed(EventName),
            suffix(":"), nl]
    ).

in_clause_for_pieces(ClauseContext) = Pieces :-
    ModuleInfo = ClauseContext ^ tecc_module_info,
    PredId = ClauseContext ^ tecc_pred_id,
    PredIdPieces = describe_one_pred_name(ModuleInfo,
        should_not_module_qualify, PredId),
    Pieces = [words("In clause for") | PredIdPieces] ++ [suffix(":"), nl].

%---------------------------------------------------------------------------%

get_inst_varset(ClauseContext, InstVarSet) :-
    % XXX Typechecking works on pred_infos, which do NOT have an inst_varset.
    % I (zs) don't know where the inst variables in any ho_inst_infos
    % in higher-order types come from, but I am *guessing* that it is
    % from the varset of the clause itself. I am not even sure whether
    % this matters, since I don't know whether ho_inst_infos can ever
    % be filled in before the end of typechecking.
    %
    % XXX Note that replacing the code below with "varset.init(InstVarSet)"
    % has no effect on a bootcheck, so it seems that the answer to the
    % question above is "no".
    ProgVarSet = ClauseContext ^ tecc_varset,
    varset.coerce(ProgVarSet, InstVarSet).

%---------------------------------------------------------------------------%

identical_types(Type1, Type2) :-
    map.init(TypeSubst0),
    type_unify(Type1, Type2, [], TypeSubst0, TypeSubst),
    TypeSubst = TypeSubst0.

%---------------------------------------------------------------------------%
:- end_module check_hlds.typecheck_error_util.
%---------------------------------------------------------------------------%
