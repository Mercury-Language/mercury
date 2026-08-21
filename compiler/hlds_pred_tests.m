%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1996-2012 The University of Melbourne.
% Copyright (C) 2013-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: hlds_pred_tests.m.
%
% Tests on pred_infos, or proxies for pred_infos.
%
%---------------------------------------------------------------------------%

:- module hlds.hlds_pred_tests.
:- interface.

:- import_module hlds.hlds_cons.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.pred_proc_id.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_rare.

:- import_module one_or_more.

%---------------------------------------------------------------------------%

    % Predicates to deal with record syntax.

    % construct_field_access_function_name(AccessType, FieldName, FuncName):
    %
    % From the access type and the name of the field,
    % construct a function name.
    %
:- pred construct_field_access_function_name(field_access_type::in,
    sym_name::in, sym_name::out) is det.

    % is_field_access_function_name(ModuleInfo, FuncName, Arity,
    %   AccessType, FieldName, OoMFieldDefns):
    %
    % Inverse of the above.
    %
    % XXX ARITY The third argument should be either pred_form_arity or
    % user_arity.
    %
:- pred is_field_access_function_name(module_info::in, sym_name::in,
    arity::out, field_access_type::out, sym_name::out,
    one_or_more(hlds_ctor_field_defn)::out) is semidet.

:- pred pred_info_is_field_access_function(module_info::in, pred_info::in,
    field_access_type::out, sym_name::out,
    one_or_more(hlds_ctor_field_defn)::out) is semidet.

%---------------------------------------------------------------------------%

    % Predicates to deal with builtins.

    % is_unify_pred(PredInfo) succeeds iff the PredInfo is for a
    % compiler-generated instance of a type-specific unify predicate.
    %
:- pred is_unify_pred(pred_info::in) is semidet.

    % is_unify_index_or_compare_pred(PredInfo) succeeds iff the PredInfo
    % is for a compiler generated instance of a type-specific special_pred
    % (i.e. one of the unify, compare, or index predicates generated as
    % a type-specific instance of unify/2, index/2, or compare/3).
    %
:- pred is_unify_index_or_compare_pred(pred_info::in) is semidet.

    % Is the argument the pred_info for a builtin that can be generated inline?
    %
:- pred pred_info_is_builtin(pred_info::in) is semidet.

    % builtin_state(ModuleInfo, CallerPredId, PredId, ProcId, BuiltinState)
    %
    % Is the given procedure a builtin that should be generated inline
    % in the given caller?
    %
:- func builtin_state(module_info, pred_id, pred_id, proc_id) = builtin_state.

    % Succeeds iff PredInfo represents a promise of the given type.
    %
:- pred pred_info_is_promise(pred_info::in, promise_type::out) is semidet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.
:- import_module backend_libs.builtin_ops.
:- import_module hlds.pred_info_types.
:- import_module hlds.pred_name.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_util.

:- import_module int.
:- import_module map.

%---------------------------------------------------------------------------%

construct_field_access_function_name(get, FieldName, FieldName).
construct_field_access_function_name(set, FieldName, FuncName) :-
    add_sym_name_suffix(FieldName, " :=", FuncName).

is_field_access_function_name(ModuleInfo, FuncName,
        Arity, AccessType, FieldName, OoMFieldDefns) :-
    ( if remove_sym_name_suffix(FuncName, " :=", FieldName0) then
        Arity = 2,
        AccessType = set,
        FieldName = FieldName0
    else
        Arity = 1,
        AccessType = get,
        FieldName = FuncName
    ),
    module_info_get_ctor_field_table(ModuleInfo, CtorFieldTable),
    map.search(CtorFieldTable, FieldName, OoMFieldDefns).

pred_info_is_field_access_function(ModuleInfo, PredInfo,
        Accesstype, FieldName, OoMFieldDefns) :-
    pred_info_is_pred_or_func(PredInfo) = pf_function,
    Module = pred_info_module(PredInfo),
    Name = pred_info_name(PredInfo),
    pred_info_get_orig_arity(PredInfo, PredFormArity),
    user_arity_pred_form_arity(pf_function, user_arity(FuncArityInt),
        PredFormArity),
    is_field_access_function_name(ModuleInfo, qualified(Module, Name),
        FuncArityInt, Accesstype, FieldName, OoMFieldDefns).

%---------------------------------------------------------------------------%

is_unify_pred(PredInfo) :-
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_compiler(made_for_uci(spec_pred_unify, _TypeCtor)).

is_unify_index_or_compare_pred(PredInfo) :-
    pred_info_get_origin(PredInfo, Origin),
    Origin = origin_compiler(made_for_uci(_SpecialPredId, _TypeCtor)).

pred_info_is_builtin(PredInfo) :-
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    is_inline_builtin(ModuleName, PredName, PredFormArity).

builtin_state(ModuleInfo, CallerPredId, PredId, _ProcId) = BuiltinState :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ModuleName = pred_info_module(PredInfo),
    PredName = pred_info_name(PredInfo),
    PredFormArity = pred_info_pred_form_arity(PredInfo),
    ( if
        % XXX This should ask: is this an inline builtin FOR THIS BACKEND?
        is_inline_builtin(ModuleName, PredName, PredFormArity),
        (
            module_info_get_globals(ModuleInfo, Globals),
            globals.get_opt_tuple(Globals, OptTuple),
            OptTuple ^ ot_allow_inlining = allow_inlining,
            (
                OptTuple ^ ot_inline_builtins = inline_builtins
            ;
                PredName = "store_at_ref_impure",
                ModuleName = mercury_private_builtin_module
            )
        ;
            % The "recursive" call in the automatically generated body
            % of each builtin predicate MUST be generated inline.
            % If it isn't generated inline, then any call to the predicate
            % form of the builtin would fall into an infinite loop.
            CallerPredId = PredId
        )
    then
        BuiltinState = inline_builtin
    else
        BuiltinState = not_builtin
    ).

:- pred is_inline_builtin(module_name::in, string::in, pred_form_arity::in)
    is semidet.

is_inline_builtin(ModuleName, PredName, PredFormArity) :-
    PredFormArity = pred_form_arity(Arity),
    % None of our inline builtins has an arity greater than three.
    % Fail for predicates with arities of four or more *without*
    % doing a switch on ModuleName or PredName.
    Arity =< 3,
    builtin_ops.test_if_builtin(ModuleName, PredName, Arity).

pred_info_is_promise(PredInfo, PromiseType) :-
    pred_info_get_goal_type(PredInfo, goal_for_promise(PromiseType)).

%---------------------------------------------------------------------------%
:- end_module hlds.hlds_pred_tests.
%---------------------------------------------------------------------------%

