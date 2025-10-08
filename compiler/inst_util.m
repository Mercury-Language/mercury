%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2014-2015, 2017-2021, 2023-2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: inst_util.m.
% Author: fjh.
%
% This module defines some utility routines for manipulating insts.
%
%---------------------------------------------------------------------------%

:- module hlds.inst_util.
:- interface.

:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Return the default mode for a function of the given arity.
    %
:- func pred_inst_info_default_func_mode(arity) = pred_inst_info.

%---------------------------------------------------------------------------%
%
% These utility predicates on types are in this module because they are needed
% *only* by code that manipulates insts.
%

    % If possible, get the argument types for the cons_id. We need to pass in
    % the arity rather than using the arity from the cons_id because the arity
    % in the cons_id will not include any extra type_info arguments for
    % existentially quantified types.
    %
    % This utility predicate on types is in this module because it is needed
    % *only* by code that manipulates insts.
    %
    % XXX This predicate returns the types of the arguments, but
    % loses any ho_inst_info for the arguments.
    %
    % XXX We should consider returning not a list of types, but a list of
    % type/inst pairs. That would incur the cost of the construction of
    % the pairs, but it would simplify (and thereby probably speed up)
    % the code that would later traverse both those lists, and maybe some
    % other list of insts as well.
    %
:- pred get_cons_id_arg_types_for_bound_functor(module_info::in, mer_type::in,
    bound_functor::in, list(mer_type)::out) is det.
:- pred get_cons_id_arg_types_for_inst(module_info::in, mer_type::in,
    cons_id::in, list(mer_inst)::in, list(mer_type)::out) is det.

    % XXX This predicate returns the types of the arguments, but
    % loses any ho_inst_info for the arguments.
    %
:- pred get_higher_order_arg_types(mer_type::in, arity::in,
    list(mer_type)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.type_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_test.

:- import_module int.
:- import_module varset.

%---------------------------------------------------------------------------%

pred_inst_info_default_func_mode(Arity) = PredInstInfo :-
    in_mode(InMode),
    out_mode(OutMode),
    ArgModes = list.duplicate(Arity - 1, InMode) ++ [OutMode],
    PredInstInfo = pred_inst_info(pf_function, ArgModes, arg_reg_types_unset,
        detism_det).

%---------------------------------------------------------------------------%

get_cons_id_arg_types_for_bound_functor(ModuleInfo, Type, BoundFunctor,
        ArgTypes) :-
    BoundFunctor = bound_functor(ConsId, ArgInsts),
    get_cons_id_arg_types_for_inst(ModuleInfo, Type, ConsId,
        ArgInsts, ArgTypes).

get_cons_id_arg_types_for_inst(ModuleInfo, Type, ConsId, ArgInsts, ArgTypes) :-
    list.length(ArgInsts, Arity),
    ( if ConsId = du_data_ctor(DuCtor) then
        ( if
            % XXX get_cons_id_non_existential_arg_types will fail
            % for ConsIds with existentially typed arguments.
            %
            % XXX It will also fail if Type is a builtin type for which
            % the compiler creates what are effectively builtin data
            % constructors, such as "type_info" and "typeclass_info"
            % (to which cell_inst_cons_id constructs references).
            % In that case, Type won't be in the type table at all.
            % (XXX Question: why does cell_inst_cons_id have to return
            % a du_data_ctor cons_id?)
            %
            % Type also won't be in the type table if it is a type variable.
            % Usually, we don't allow insts to apply to a type var, but this
            % can nevertheless happen for e.g. inst_preserving_append.
            % Exactly how, I (zs) don't yet know, but almost certainly
            % it has something to do with the compiler handling constrained
            % insts incorrectly (since such insts are the one difference
            % between inst preserving and regular append).
            get_du_ctor_non_existential_arg_types(ModuleInfo, Type,
                DuCtor, ArgTypes0),
            list.length(ArgTypes0, Arity)
        then
            ArgTypes = ArgTypes0
        else if
            % For tuple types, the cons_id is sometimes not tuple_cons/1,
            % but cons/1, with unqualified("{}")/2 as the data constructor.
            type_to_ctor_and_args(Type, TypeCtor, ArgTypes0),
            type_ctor_is_tuple(TypeCtor)
        then
            ArgTypes = ArgTypes0
        else
            list.duplicate(Arity, no_type_available, ArgTypes)
        )
    else if ConsId = tuple_cons(_) then
        ( if type_to_ctor_and_args(Type, _TypeCtor, ArgTypes0) then
            ArgTypes = ArgTypes0
        else
            list.duplicate(Arity, no_type_available, ArgTypes)
        )
    else
        ArgTypes = []
    ).

get_higher_order_arg_types(Type, Arity, Types) :-
    ( if type_is_higher_order_details(Type, _, _, ArgTypes) then
        Types = ArgTypes
    else
        % NOTE Replacing the else case code with an abort does not prevent
        % a standard bootcheck in the hlc.gc grade, and leads to the failure
        % of only three test cases: hard_coded/exist_cons_ho_arg,
        % hard_coded/hash_table_delete, and invalid/type_prop_into_inst.
        % Eliminating this call to no_type_available should start by
        % looking at those failures.
        list.duplicate(Arity, no_type_available, Types)
    ).

%---------------------------------------------------------------------------%

    % Return a type that represents the *absence* of type information.
    % This type is a type variable. Since the predicates and functions
    % working on insts and modes use type information *only* to look up
    % the constructors of a type, this conveys exactly the right info.
    %
    % Note that we always return the *same* type variable. Besides matching
    % the semantics expected of functions, it also means our callers will not
    % construct non-canonical inst_match_inputs structures. The fact that
    % we return the same type variable as the "types" of e.g. different
    % existentially typed arguments of a data constructor is not a problem,
    % even though those arguments may contain values of different types at
    % runtime, because the predicates working insts and modes never compare
    % the types associated with those insts and modes for equality.
    %
:- func no_type_available = mer_type.

no_type_available = Type :-
    varset.init(VarSet0),
    new_var(Var, VarSet0, _VarSet),
    Type = type_variable(Var, kind_star).

%---------------------------------------------------------------------------%
:- end_module hlds.inst_util.
%---------------------------------------------------------------------------%
