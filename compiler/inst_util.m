%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2012 The University of Melbourne.
% Copyright (C) 2015, 2024 The Mercury team.
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

:- module check_hlds.inst_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%---------------------------------------------------------------------------%

    % Given an inst, return a new inst which is the same as the original inst
    % but with all occurrences of `unique' replaced with `mostly_unique'.
    %
:- pred make_mostly_uniq_inst(mer_type::in, mer_inst::in, mer_inst::out,
    module_info::in, module_info::out) is det.

    % Given a list of insts, return a new list of insts which is the same
    % as the original list of insts, but with all occurrences of `unique'
    % replaced with `shared'. It is an error if any part of the inst list
    % is free.
    %
:- pred make_shared_inst_list(list(mer_type)::in, list(mer_inst)::in,
    list(mer_inst)::out, module_info::in, module_info::out) is det.

    % Make an inst shared; replace all occurrences of `unique' or
    % `mostly_unique' in the inst with `shared'.
    %
:- pred make_shared_inst(mer_type::in, mer_inst::in, mer_inst::out,
    module_info::in, module_info::out) is det.

:- pred make_shared_bound_functors(mer_type::in, list(bound_functor)::in,
    list(bound_functor)::out, module_info::in, module_info::out) is det.

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

:- import_module check_hlds.inst_lookup.
:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_inst_mode.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_test.

:- import_module int.
:- import_module maybe.
:- import_module require.
:- import_module varset.

%---------------------------------------------------------------------------%

make_mostly_uniq_inst(Type, Inst0, Inst, !ModuleInfo) :-
    (
        ( Inst0 = not_reached
        ; Inst0 = free
        ),
        Inst = Inst0
    ;
        Inst0 = any(Uniq0, HOInstInfo),
        make_mostly_uniq(Uniq0, Uniq),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq0, _InstResults0, BoundFunctors0),
        % XXX could improve efficiency by avoiding recursion here
        make_mostly_uniq(Uniq0, Uniq),
        make_mostly_uniq_bound_functors(Type, BoundFunctors0, BoundFunctors,
            !ModuleInfo),
        % XXX A better approximation of InstResults is probably possible.
        Inst = bound(Uniq, inst_test_no_results, BoundFunctors)
    ;
        Inst0 = ground(Uniq0, PredInst),
        make_mostly_uniq(Uniq0, Uniq),
        Inst = ground(Uniq, PredInst)
    ;
        Inst0 = inst_var(_),
        unexpected($pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        make_mostly_uniq_inst(Type, SubInst0, SubInst, !ModuleInfo),
        ( if inst_matches_final(!.ModuleInfo, Type, SubInst, SubInst0) then
            Inst = constrained_inst_vars(InstVars, SubInst)
        else
            Inst = SubInst
        )
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the mostly_uniq_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_mostly_uniq_insts(InstTable0, MostlyUniqInstTable0),
        search_insert_unknown_mostly_uniq_inst(InstName, MaybeOldMaybeInst,
            MostlyUniqInstTable0, MostlyUniqInstTable1),
        (
            MaybeOldMaybeInst = yes(OldMaybeInst),
            (
                OldMaybeInst = inst_known(MostlyUniqInst)
            ;
                OldMaybeInst = inst_unknown,
                MostlyUniqInst = defined_inst(InstName)
            )
        ;
            MaybeOldMaybeInst = no,
            % We have inserted InstName into the table with value
            % `inst_unknown'.
            inst_table_set_mostly_uniq_insts(MostlyUniqInstTable1,
                InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_mostly_uniq_inst(Type, SubInst1, MostlyUniqInst, !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(MostlyUniqInst)' in the
            % mostly_uniq_inst table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_mostly_uniq_insts(InstTable2, MostlyUniqInstTable2),
            det_update_mostly_uniq_inst(InstName, inst_known(MostlyUniqInst),
                MostlyUniqInstTable2, MostlyUniqInstTable),
            inst_table_set_mostly_uniq_insts(MostlyUniqInstTable,
                InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( if
            inst_contains_inst_name(!.ModuleInfo, InstName, MostlyUniqInst)
        then
            Inst = defined_inst(InstName)
        else
            Inst = MostlyUniqInst
        )
    ).

:- pred make_mostly_uniq(uniqueness::in, uniqueness::out) is det.

make_mostly_uniq(unique, mostly_unique).
make_mostly_uniq(mostly_unique, mostly_unique).
make_mostly_uniq(shared, shared).
make_mostly_uniq(mostly_clobbered, mostly_clobbered).
make_mostly_uniq(clobbered, clobbered).

:- pred make_mostly_uniq_bound_functors(mer_type::in,
    list(bound_functor)::in, list(bound_functor)::out,
    module_info::in, module_info::out) is det.

make_mostly_uniq_bound_functors(_, [], [], !ModuleInfo).
make_mostly_uniq_bound_functors(Type,
        [BoundFunctor0 | BoundFunctors0], [BoundFunctor | BoundFunctors],
        !ModuleInfo) :-
    BoundFunctor0 = bound_functor(ConsId, ArgInsts0),
    get_cons_id_arg_types_for_inst(!.ModuleInfo, Type, ConsId, ArgInsts0,
        ArgTypes),
    make_mostly_uniq_inst_list(ArgTypes, ArgInsts0, ArgInsts, !ModuleInfo),
    BoundFunctor = bound_functor(ConsId, ArgInsts),
    make_mostly_uniq_bound_functors(Type,
        BoundFunctors0, BoundFunctors, !ModuleInfo).

:- pred make_mostly_uniq_inst_list(list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::out,
    module_info::in, module_info::out) is det.

make_mostly_uniq_inst_list([], [], [], !ModuleInfo).
make_mostly_uniq_inst_list([], [_ | _], _, !ModuleInfo) :-
    unexpected($pred, "list length mismatch").
make_mostly_uniq_inst_list([_ | _], [], _, !ModuleInfo) :-
    unexpected($pred, "list length mismatch").
make_mostly_uniq_inst_list([Type | Types], [Inst0 | Insts0], [Inst | Insts],
        !ModuleInfo) :-
    make_mostly_uniq_inst(Type, Inst0, Inst, !ModuleInfo),
    make_mostly_uniq_inst_list(Types, Insts0, Insts, !ModuleInfo).

%---------------------------------------------------------------------------%

make_shared_inst_list([], [], [], !ModuleInfo).
make_shared_inst_list([], [_ | _], _, !ModuleInfo) :-
    unexpected($pred, "list length mismatch").
make_shared_inst_list([_ | _], [], _, !ModuleInfo) :-
    unexpected($pred, "list length mismatch").
make_shared_inst_list([Type | Types], [Inst0 | Insts0], [Inst | Insts],
        !ModuleInfo) :-
    make_shared_inst(Type, Inst0, Inst, !ModuleInfo),
    make_shared_inst_list(Types, Insts0, Insts, !ModuleInfo).

make_shared_inst(Type, Inst0, Inst, !ModuleInfo) :-
    (
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = free,
        % The caller should ensure that this never happens.
        unexpected($pred, "cannot make shared version of `free'")
    ;
        Inst0 = any(Uniq0, HOInstInfo),
        make_shared(Uniq0, Uniq),
        Inst = any(Uniq, HOInstInfo)
    ;
        Inst0 = bound(Uniq0, InstResults0, BoundFunctors0),
        % XXX This code has a performance problem.
        %
        % The problem is that e.g. in a list of length N, you will have
        % N variables for the skeletons whose insts contain an average of
        % N/2 occurences of `bound' each, so the complexity of running
        % make_shared_inst on all their insts is quadratic in N.
        %
        % One potential way to fix this would be to introduce a new function
        % symbol for insts, make_shared(mer_inst), which would have the meaning
        % of requiring any compiler component that finds it to run
        % make_shared_inst on its argument before using it. That would require
        % parameterizing make_shared_inst to say whether it is being used
        % in such a manner.
        %
        % Another similar fix would be to add an extra argument to bound/2
        % to say whether the insts in its last argument should implicitly be
        % made shared.
        %
        % If Uniq0 = shared, then all the other cells below it should also be
        % shared as well, which means we should be able to avoid the call to
        % make_shared_bound_functors below. However, for the kinds of goals
        % for which the call is a bottleneck, the goals resulting from the
        % construction of large ground terms, Uniq0 will in fact be `unique'.

        make_shared(Uniq0, Uniq),
        make_shared_bound_functors(Type, BoundFunctors0, BoundFunctors,
            !ModuleInfo),
        Inst = bound(Uniq, InstResults0, BoundFunctors)
    ;
        Inst0 = ground(Uniq0, PredInst),
        make_shared(Uniq0, Uniq),
        Inst = ground(Uniq, PredInst)
    ;
        Inst0 = inst_var(_),
        unexpected($pred, "free inst var")
    ;
        Inst0 = constrained_inst_vars(InstVars, SubInst0),
        make_shared_inst(Type, SubInst0, SubInst1, !ModuleInfo),
        ( if inst_matches_final(!.ModuleInfo, Type, SubInst1, SubInst0) then
            Inst = constrained_inst_vars(InstVars, SubInst1)
        else
            Inst = SubInst1
        )
    ;
        Inst0 = defined_inst(InstName),
        % Check whether the inst name is already in the shared_inst table.
        module_info_get_inst_table(!.ModuleInfo, InstTable0),
        inst_table_get_shared_insts(InstTable0, SharedInstTable0),
        search_insert_unknown_shared_inst(InstName, MaybeOldMaybeInst,
            SharedInstTable0, SharedInstTable1),
        (
            MaybeOldMaybeInst = yes(OldMaybeInst),
            (
                OldMaybeInst = inst_known(SharedInst)
            ;
                OldMaybeInst = inst_unknown,
                SharedInst = Inst0
            )
        ;
            MaybeOldMaybeInst = no,
            % We have inserted SharedInstKey into the table with value
            % `inst_unknown'.
            inst_table_set_shared_insts(SharedInstTable1,
                InstTable0, InstTable1),
            module_info_set_inst_table(InstTable1, !ModuleInfo),

            % Expand the inst name, and invoke ourself recursively on its
            % expansion.
            inst_lookup(!.ModuleInfo, InstName, SubInst0),
            inst_expand(!.ModuleInfo, SubInst0, SubInst1),
            make_shared_inst(Type, SubInst1, SharedInst, !ModuleInfo),

            % Now that we have determined the resulting Inst, store the
            % appropriate value `known(SharedInst)' in the shared_inst table.
            module_info_get_inst_table(!.ModuleInfo, InstTable2),
            inst_table_get_shared_insts(InstTable2, SharedInstTable2),
            det_update_shared_inst(InstName, inst_known(SharedInst),
                SharedInstTable2, SharedInstTable),
            inst_table_set_shared_insts(SharedInstTable,
                InstTable2, InstTable),
            module_info_set_inst_table(InstTable, !ModuleInfo)
        ),
        % Avoid expanding recursive insts.
        ( if inst_contains_inst_name(!.ModuleInfo, InstName, SharedInst) then
            Inst = defined_inst(InstName)
        else
            Inst = SharedInst
        )
    ).

:- pred make_shared(uniqueness::in, uniqueness::out) is det.

make_shared(unique, shared).
make_shared(mostly_unique, shared).
make_shared(shared, shared).
make_shared(mostly_clobbered, mostly_clobbered).
make_shared(clobbered, clobbered).

make_shared_bound_functors(_, [], [], !ModuleInfo).
make_shared_bound_functors(Type, [BoundFunctor0 | BoundFunctors0],
        [BoundFunctor | BoundFunctors], !ModuleInfo) :-
    BoundFunctor0 = bound_functor(ConsId, ArgInsts0),
    get_cons_id_arg_types_for_inst(!.ModuleInfo, Type, ConsId, ArgInsts0,
        ArgTypes),
    make_shared_inst_list(ArgTypes, ArgInsts0, ArgInsts, !ModuleInfo),
    BoundFunctor = bound_functor(ConsId, ArgInsts),
    make_shared_bound_functors(Type,
        BoundFunctors0, BoundFunctors, !ModuleInfo).

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
:- end_module check_hlds.inst_util.
%---------------------------------------------------------------------------%
