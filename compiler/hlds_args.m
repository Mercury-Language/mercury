%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007, 2010 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_args.m.
% Main authors: juliensf.
%
% This module defines the part of the HLDS that deals with procedure and call
% site arguments. (See comments at the head of polymorphism.m for further
% details.)
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_args.
:- interface.

:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.

:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

    % This type represents the arguments to a predicate symbol, or
    % the arguments and result of a function symbol. The arguments may be
    % variables, types, modes, etc, depending on the context.
    %
    % Rather than keep all arguments in a single list, we retain information
    % about the origin of each argument (such as whether it was introduced
    % by polymorphism.m to hold a type_info). This simplifies the processing
    % in polymorphism.m, and also abstracts away the specific calling
    % convention that we use.
    %
:- type proc_arg_vector(T).

%-----------------------------------------------------------------------------%

    % Create a new proc_arg_vector from the given list.
    % If the first argument is `function' then the last element in the
    % list is used to set the return value field of the proc_arg_vector.
    %
:- func proc_arg_vector_init(pred_or_func, list(T)) = proc_arg_vector(T).

%-----------------------------------------------------------------------------%
%
% Access predicates for the proc_arg_vector structure
%

:- func proc_arg_vector_get_instance_type_infos(proc_arg_vector(T))
    = list(T).
:- func proc_arg_vector_get_instance_typeclass_infos(proc_arg_vector(T))
    = list(T).
:- func proc_arg_vector_get_univ_type_infos(proc_arg_vector(T)) = list(T).
:- func proc_arg_vector_get_exist_type_infos(proc_arg_vector(T)) = list(T).
:- func proc_arg_vector_get_univ_typeclass_infos(proc_arg_vector(T)) = list(T).
:- func proc_arg_vector_get_exist_typeclass_infos(proc_arg_vector(T))
    = list(T).
:- func proc_arg_vector_get_user_args(proc_arg_vector(T)) = list(T).
:- func proc_arg_vector_get_maybe_ret_value(proc_arg_vector(T)) = maybe(T).

:- pred proc_arg_vector_set_instance_type_infos(list(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.
:- pred proc_arg_vector_set_instance_typeclass_infos(list(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.
:- pred proc_arg_vector_set_univ_type_infos(list(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.
:- pred proc_arg_vector_set_exist_type_infos(list(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.
:- pred proc_arg_vector_set_univ_typeclass_infos(list(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.
:- pred proc_arg_vector_set_exist_typeclass_infos(list(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.
:- pred proc_arg_vector_set_user_args(list(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.
:- pred proc_arg_vector_set_maybe_ret_value(maybe(T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.

%-----------------------------------------------------------------------------%
%
% Utility predicates that operate on proc_arg_vectors.
%

    % Return a list of the items in a arg_vector. The order of the list
    % corresponds to that required by the calling conventions.
    % See comments at the head of polymorphism.m for details.
    % If the arg_vector is for a function, then the last element in the list
    % will correspond to the function return value.
    %
:- func proc_arg_vector_to_list(proc_arg_vector(T)) = list(T).

    % Return the set of items in an arg_vector.
    %
:- func proc_arg_vector_to_set(proc_arg_vector(T)) = set(T).

    % Apply a renaming to an arg_vector.
    % Useful for renaming variables etc.
    %
:- pred apply_renaming_to_proc_arg_vector(map(T, T)::in,
    proc_arg_vector(T)::in, proc_arg_vector(T)::out) is det.

    % proc_arg_vector_partition_poly_args(Vec, PolyArgs, NonPolyArgs):
    %
    % Partition the argument vector into two lists depending on whether
    % something was introduced by the polymorphism transformation or not.
    %
:- pred proc_arg_vector_partition_poly_args(proc_arg_vector(T)::in,
    list(T)::out, list(T)::out) is det.

    % proc_arg_vector_member(Vector, V):
    %
    % Succeeds iff V is a member of the argument vector `Vector'.
    %
:- pred proc_arg_vector_member(proc_arg_vector(T)::in, T::in) is semidet.

    % Partition the given arg_vector into a list of arguments and
    % a function return value. Throws an exception if the arg_vector does
    % not correspond to a function.
    %
:- pred proc_arg_vector_to_func_args(proc_arg_vector(T)::in,
    list(T)::out, T::out) is det.

%-----------------------------------------------------------------------------%
%
% Higher-order operations on proc_arg_vectors.
%

%
% NOTE these higher-order operations all work in a similar fashion
% to their counterparts in library/list.m.

:- func proc_arg_vector_map(func(T) = U, proc_arg_vector(T)) =
    proc_arg_vector(U).

:- pred proc_arg_vector_map(pred(T, U)::in(pred(in, out) is det),
    proc_arg_vector(T)::in, proc_arg_vector(U)::out) is det.

:- pred proc_arg_vector_map_corresponding(
    pred(T, U, V)::in(pred(in, in, out) is det),
    proc_arg_vector(T)::in, proc_arg_vector(U)::in, proc_arg_vector(V)::out)
    is det.

:- pred proc_arg_vector_all_true(pred(T)::in(pred(in) is semidet),
    proc_arg_vector(T)::in) is semidet.

:- pred proc_arg_vector_map_corresponding_foldl2(
    pred(A, B, C, D, D, E, E)::in(pred(in, in, out, in, out, in, out) is det),
    proc_arg_vector(A)::in, proc_arg_vector(B)::in, proc_arg_vector(C)::out,
    D::in, D::out, E::in, E::out) is det.

:- pred proc_arg_vector_foldl3_corresponding(
    pred(A, B, C, C, D, D, E, E)
    ::in(pred(in, in, in, out, in, out, in, out) is det),
    proc_arg_vector(A)::in, proc_arg_vector(B)::in,
    C::in, C::out, D::in, D::out, E::in, E::out) is det.

:- pred proc_arg_vector_foldl2_corresponding3(
    pred(A, B, C, D, D, E, E)
    ::in(pred(in, in, in, in, out, in, out) is det),
    proc_arg_vector(A)::in, proc_arg_vector(B)::in, proc_arg_vector(C)::in,
    D::in, D::out, E::in, E::out) is det.

:- pred proc_arg_vector_foldl3_corresponding3(
    pred(A, B, C, D, D, E, E, F, F)
    ::in(pred(in, in, in, in, out, in, out, in, out) is det),
    proc_arg_vector(A)::in, proc_arg_vector(B)::in, proc_arg_vector(C)::in,
    D::in, D::out, E::in, E::out, F::in, F::out) is det.

:- pred proc_arg_vector_foldl4_corresponding3(
    pred(A, B, C, D, D, E, E, F, F, G, G)
    ::in(pred(in, in, in, in, out, in, out, in, out, in, out) is det),
    proc_arg_vector(A)::in, proc_arg_vector(B)::in, proc_arg_vector(C)::in,
    D::in, D::out, E::in, E::out, F::in, F::out, G::in, G::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff related to the polymorphism pass.
%

    % This type represents those arguments of a predicate or function
    % symbol that are introduced by the polymorphism transformation.
    % The arguments may be variables, types, modes, etc, depending on the
    % context.
    %
    % Values of this type are used to pass around intermediate values
    % during the polymorphism transformation.
    %
:- type poly_arg_vector(T).

:- func poly_arg_vector_init = poly_arg_vector(T).

:- pred poly_arg_vector_set_instance_type_infos(list(T)::in,
    poly_arg_vector(T)::in, poly_arg_vector(T)::out) is det.
:- pred poly_arg_vector_set_instance_typeclass_infos(list(T)::in,
    poly_arg_vector(T)::in, poly_arg_vector(T)::out) is det.
:- pred poly_arg_vector_set_univ_type_infos(list(T)::in,
    poly_arg_vector(T)::in, poly_arg_vector(T)::out) is det.
:- pred poly_arg_vector_set_exist_type_infos(list(T)::in,
    poly_arg_vector(T)::in, poly_arg_vector(T)::out) is det.
:- pred poly_arg_vector_set_univ_typeclass_infos(list(T)::in,
    poly_arg_vector(T)::in, poly_arg_vector(T)::out) is det.
:- pred poly_arg_vector_set_exist_typeclass_infos(list(T)::in,
    poly_arg_vector(T)::in, poly_arg_vector(T)::out) is det.

    % Convert a poly_arg_vector into a list.
    % XXX ARGVEC - this is only temporary until the proc_info structure use
    % proc_arg_vectors. We should then provide a predicate that merges a
    % poly_arg_vector with a proc_arg_vector.
    %
:- func poly_arg_vector_to_list(poly_arg_vector(T)) = list(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.
:- import_module parse_tree.prog_type.
    % Required for apply_partial_map_to_list.
    % XXX That should really live in a different module.

:- import_module require.

%-----------------------------------------------------------------------------%

    % The first six fields are set by the polymorphism pass.
    %
:- type proc_arg_vector(T)
    --->    proc_arg_vector(
                % Type_infos for the unconstrained type variables in an
                % instance declaration in the order which they first appear
                % in the instance arguments. For procedures that are not
                % class methods, this will always be the empty list.
                pav_instance_type_infos         :: list(T),

                % Typeclass_infos for the class constraints on an instance
                % declaration, in the order in which they appear in the
                % declaration. For procedures that are not class methods,
                % this will always be the empty list.
                pav_instance_typeclass_infos    :: list(T),

                % Type_infos for unconstrained universally quantified type
                % variables, in the order in which they first appear in the
                % argument types.
                pav_univ_type_infos             :: list(T),

                % Type_infos for unconstrained existentially quantified type
                % variables, in the order that the type variables first
                % appear in the argument types.
                pav_exist_type_infos            :: list(T),

                % Typeclass_infos for universally quantified constraints
                % in the order in which the constraints appear in the class
                % context.
                pav_univ_typeclass_infos        :: list(T),

                % Typeclass_infos for existentially quantified constraints
                % in the order in which the constraints appear in the class
                % context.
                pav_exist_typeclass_infos       :: list(T),

                % The original procedure arguments.
                % XXX Plus at the moment any arguments that may be
                % introduced by transformations performed by the compiler.
                % Eventually these should be separated out.
                pav_user_args                   :: list(T),

                % For predicates this field is no; for functions
                % it corresponds to the function's return value.
                pav_maybe_ret_value             :: maybe(T)
            ).

%-----------------------------------------------------------------------------%

proc_arg_vector_init(PredOrFunc, Args0) = ArgVec :-
    (
        PredOrFunc = pf_predicate,
        Args = Args0,
        MaybeRetVal = no
    ;
        PredOrFunc = pf_function,
        list.det_split_last(Args0, Args, RetVal),
        MaybeRetVal = yes(RetVal)
    ),
    ArgVec = proc_arg_vector([], [], [], [], [], [], Args, MaybeRetVal).

%-----------------------------------------------------------------------------%

proc_arg_vector_get_instance_type_infos(V)
    = V ^ pav_instance_type_infos.
proc_arg_vector_get_instance_typeclass_infos(V)
    = V ^ pav_instance_typeclass_infos.
proc_arg_vector_get_univ_type_infos(V)
    = V ^ pav_univ_type_infos.
proc_arg_vector_get_exist_type_infos(V)
    = V ^ pav_exist_type_infos.
proc_arg_vector_get_univ_typeclass_infos(V)
    = V ^ pav_univ_typeclass_infos.
proc_arg_vector_get_exist_typeclass_infos(V)
    = V ^ pav_exist_typeclass_infos.
proc_arg_vector_get_user_args(V)
    = V ^ pav_user_args.
proc_arg_vector_get_maybe_ret_value(V)
    = V ^ pav_maybe_ret_value.

proc_arg_vector_set_instance_type_infos(ITI, !V) :-
    !V ^ pav_instance_type_infos := ITI.
proc_arg_vector_set_instance_typeclass_infos(ITCI, !V) :-
    !V ^ pav_instance_typeclass_infos := ITCI.
proc_arg_vector_set_univ_type_infos(UTI, !V) :-
    !V ^ pav_univ_type_infos := UTI.
proc_arg_vector_set_exist_type_infos(ETI, !V) :-
    !V ^ pav_exist_type_infos := ETI.
proc_arg_vector_set_univ_typeclass_infos(UTCI, !V) :-
    !V ^ pav_univ_typeclass_infos := UTCI.
proc_arg_vector_set_exist_typeclass_infos(ETCI, !V) :-
    !V ^ pav_exist_typeclass_infos := ETCI.
proc_arg_vector_set_user_args(UA, !V) :-
    !V ^ pav_user_args := UA.
proc_arg_vector_set_maybe_ret_value(RV, !V) :-
    !V ^ pav_maybe_ret_value := RV.

%-----------------------------------------------------------------------------%

proc_arg_vector_to_list(ArgVec) = List :-
    ArgVec = proc_arg_vector(InstanceTypeInfos, InstanceTypeClassInfos,
        UnivTypeInfos, ExistTypeInfos, UnivTypeClassInfos,
        ExistTypeClassInfos, OrigArgs, MaybeRetValue),
    (
        MaybeRetValue = yes(Value),
        RetValue = [Value]
    ;
        MaybeRetValue = no,
        RetValue = []
    ),
    list.condense([InstanceTypeInfos,
           InstanceTypeClassInfos,
           UnivTypeInfos,
           ExistTypeInfos,
           UnivTypeClassInfos,
           ExistTypeClassInfos,
           OrigArgs,
           RetValue], List).

proc_arg_vector_to_set(ArgVec) = Set :-
    List = proc_arg_vector_to_list(ArgVec),
    Set  = set.from_list(List).

apply_renaming_to_proc_arg_vector(Renaming, ArgVec0, ArgVec) :-
    ArgVec0 = proc_arg_vector(InstanceTypeInfos0, InstanceTypeClassInfos0,
        UnivTypeInfos0, ExistTypeInfos0, UnivTypeClassInfos0,
        ExistTypeClassInfos0, OrigArgs0, MaybeRetValue0),
    apply_partial_map_to_list(Renaming, InstanceTypeInfos0, InstanceTypeInfos),
    apply_partial_map_to_list(Renaming, InstanceTypeClassInfos0,
        InstanceTypeClassInfos),
    apply_partial_map_to_list(Renaming, UnivTypeInfos0, UnivTypeInfos),
    apply_partial_map_to_list(Renaming, ExistTypeInfos0, ExistTypeInfos),
    apply_partial_map_to_list(Renaming, UnivTypeClassInfos0,
        UnivTypeClassInfos),
    apply_partial_map_to_list(Renaming, ExistTypeClassInfos0,
        ExistTypeClassInfos),
    apply_partial_map_to_list(Renaming, OrigArgs0, OrigArgs),
    (
        MaybeRetValue0 = yes(Value0),
        ( if map.search(Renaming, Value0, Value) then
            MaybeRetValue = yes(Value)
        else
            MaybeRetValue = yes(Value0)
        )
    ;
        MaybeRetValue0 = no,
        MaybeRetValue  = no
    ),
    ArgVec = proc_arg_vector(InstanceTypeInfos, InstanceTypeClassInfos,
        UnivTypeInfos, ExistTypeInfos, UnivTypeClassInfos,
        ExistTypeClassInfos, OrigArgs, MaybeRetValue).

proc_arg_vector_partition_poly_args(ArgVec, PolyArgs, NonPolyArgs) :-
    ArgVec = proc_arg_vector(InstanceTypeInfos, InstanceTypeClassInfos,
        UnivTypeInfos, ExistTypeInfos, UnivTypeClassInfos,
        ExistTypeClassInfos, OrigArgs, MaybeRetValue),
    list.condense([InstanceTypeInfos, InstanceTypeClassInfos,
        UnivTypeInfos, ExistTypeInfos, UnivTypeClassInfos,
        ExistTypeClassInfos], PolyArgs),
    (
        MaybeRetValue = yes(RetValue),
        NonPolyArgs = OrigArgs ++ [RetValue]
    ;
        MaybeRetValue = no,
        NonPolyArgs = OrigArgs
    ).

proc_arg_vector_member(ArgVec, Var) :-
    ArgVec = proc_arg_vector(InstanceTypeInfos, InstanceTypeClassInfos,
        UnivTypeInfos, ExistTypeInfos, UnivTypeClassInfos,
        ExistTypeClassInfos, OrigArgs, MaybeRetValue),
    ( list.member(Var, OrigArgs)
    ; MaybeRetValue = yes(Var)
    ; list.member(Var, InstanceTypeInfos)
    ; list.member(Var, InstanceTypeClassInfos)
    ; list.member(Var, UnivTypeInfos)
    ; list.member(Var, ExistTypeInfos)
    ; list.member(Var, UnivTypeClassInfos)
    ; list.member(Var, ExistTypeClassInfos)
    ).

proc_arg_vector_to_func_args(Vector, FuncArgs, FuncRetVal) :-
    Vector = proc_arg_vector(InstanceTypeInfos, InstanceTypeClassInfos,
        UnivTypeInfos, ExistTypeInfos, UnivTypeClassInfos,
        ExistTypeClassInfos, OrigArgs, MaybeRetValue),
    FuncArgs = list.condense([InstanceTypeInfos, InstanceTypeClassInfos,
        UnivTypeInfos, ExistTypeInfos, UnivTypeClassInfos,
        ExistTypeClassInfos, OrigArgs]),
    (
        MaybeRetValue = yes(FuncRetVal)
    ;
        MaybeRetValue = no,
        unexpected($pred, "predicate")
    ).

%-----------------------------------------------------------------------------%

proc_arg_vector_map(Func, V0) = V :-
    V0 = proc_arg_vector(ITI0, ITCI0, UTI0, ETI0, UTCI0, ETCI0, Args0,
        MaybeRetVal0),
    ITI  = list.map(Func, ITI0),
    ITCI = list.map(Func, ITCI0),
    UTI  = list.map(Func, UTI0),
    ETI  = list.map(Func, ETI0),
    UTCI = list.map(Func, UTCI0),
    ETCI = list.map(Func, ETCI0),
    Args = list.map(Func, Args0),
    (
        MaybeRetVal0 = yes(RetVal0),
        RetVal = Func(RetVal0),
        MaybeRetVal = yes(RetVal)
    ;
        MaybeRetVal0 = no,
        MaybeRetVal  = no
    ),
    V = proc_arg_vector(ITI, ITCI, UTI, ETI, UTCI, ETCI, Args,
        MaybeRetVal).

proc_arg_vector_map(Pred, V0, V) :-
    V0 = proc_arg_vector(ITI0, ITCI0, UTI0, ETI0, UTCI0, ETCI0, Args0,
        MaybeRetVal0),
    list.map(Pred, ITI0,  ITI),
    list.map(Pred, ITCI0, ITCI),
    list.map(Pred, UTI0,  UTI),
    list.map(Pred, ETI0,  ETI),
    list.map(Pred, UTCI0, UTCI),
    list.map(Pred, ETCI0, ETCI),
    list.map(Pred, Args0, Args),
    (
        MaybeRetVal0 = yes(RetVal0),
        Pred(RetVal0, RetVal),
        MaybeRetVal = yes(RetVal)
    ;
        MaybeRetVal0 = no,
        MaybeRetVal  = no
    ),
    V = proc_arg_vector(ITI, ITCI, UTI, ETI, UTCI, ETCI, Args,
        MaybeRetVal).

proc_arg_vector_map_corresponding(P, A, B, C) :-
    A = proc_arg_vector(ITIA, ITCIA, UTIA, ETIA, UTCIA, ETCIA, ArgsA,
        MaybeRetValA),
    B = proc_arg_vector(ITIB, ITCIB, UTIB, ETIB, UTCIB, ETCIB, ArgsB,
        MaybeRetValB),
    list.map_corresponding(P, ITIA, ITIB, ITIC),
    list.map_corresponding(P, ITCIA, ITCIB, ITCIC),
    list.map_corresponding(P, UTIA, UTIB, UTIC),
    list.map_corresponding(P, ETIA, ETIB, ETIC),
    list.map_corresponding(P, UTCIA, UTCIB, UTCIC),
    list.map_corresponding(P, ETCIA, ETCIB, ETCIC),
    list.map_corresponding(P, ArgsA, ArgsB, ArgsC),
    (
        MaybeRetValA = yes(RetValA),
        MaybeRetValB = yes(RetValB),
        P(RetValA, RetValB, RetValC),
        MaybeRetValC = yes(RetValC)
    ;
        MaybeRetValA = yes(_),
        MaybeRetValB = no,
        unexpected($pred, "mismatched proc_arg_vectors")
    ;
        MaybeRetValA = no,
        MaybeRetValB = yes(_),
        unexpected($pred, "mismatched proc_arg_vectors")
    ;
        MaybeRetValA = no,
        MaybeRetValB = no,
        MaybeRetValC = no
    ),
    C = proc_arg_vector(ITIC, ITCIC, UTIC, ETIC, UTCIC, ETCIC, ArgsC,
        MaybeRetValC).

proc_arg_vector_all_true(P, V) :-
    V = proc_arg_vector(ITI, ITCI, UTI, ETI, UTCI, ETCI, Args, MaybeRetVal),
    list.all_true(P, ITI),
    list.all_true(P, ITCI),
    list.all_true(P, UTI),
    list.all_true(P, ETI),
    list.all_true(P, UTCI),
    list.all_true(P, ETCI),
    list.all_true(P, Args),
    (
        MaybeRetVal = yes(RetVal),
        P(RetVal)
    ;
        MaybeRetVal = no
    ).

proc_arg_vector_map_corresponding_foldl2(P, A, B, C, !Acc1, !Acc2) :-
    A = proc_arg_vector(ITIA, ITCIA, UTIA, ETIA, UTCIA, ETCIA, ArgsA,
        MaybeRetValA),
    B = proc_arg_vector(ITIB, ITCIB, UTIB, ETIB, UTCIB, ETCIB, ArgsB,
        MaybeRetValB),
    list.map_corresponding_foldl2(P, ITIA,  ITIB,  ITIC,  !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, ITCIA, ITCIB, ITCIC, !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, UTIA,  UTIB,  UTIC,  !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, ETIA,  ETIB,  ETIC,  !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, UTCIA, UTCIB, UTCIC, !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, ETCIA, ETCIB, ETCIC, !Acc1, !Acc2),
    list.map_corresponding_foldl2(P, ArgsA, ArgsB, ArgsC, !Acc1, !Acc2),
    (
        MaybeRetValA = yes(RetValA),
        MaybeRetValB = yes(RetValB),
        P(RetValA, RetValB, RetValC, !Acc1, !Acc2),
        MaybeRetValC = yes(RetValC)
    ;
        MaybeRetValA = yes(_),
        MaybeRetValB = no,
        unexpected($pred, "mismatched proc_arg_vectors")
    ;
        MaybeRetValA = no,
        MaybeRetValB = yes(_),
        unexpected($pred, "mismatched proc_arg_vectors")
    ;
        MaybeRetValA = no,
        MaybeRetValB = no,
        MaybeRetValC = no
    ),
    C = proc_arg_vector(ITIC, ITCIC, UTIC, ETIC, UTCIC, ETCIC, ArgsC,
        MaybeRetValC).

proc_arg_vector_foldl3_corresponding(P, A, B, !Acc1, !Acc2, !Acc3) :-
    A = proc_arg_vector(ITIA, ITCIA, UTIA, ETIA, UTCIA, ETCIA, ArgsA,
        MaybeRetValA),
    B = proc_arg_vector(ITIB, ITCIB, UTIB, ETIB, UTCIB, ETCIB, ArgsB,
        MaybeRetValB),
    list.foldl3_corresponding(P, ITIA,  ITIB,  !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, ITCIA, ITCIB, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, UTIA,  UTIB,  !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, ETIA,  ETIB,  !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, UTCIA, UTCIB, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, ETCIA, ETCIB, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding(P, ArgsA, ArgsB, !Acc1, !Acc2, !Acc3),
    (
        MaybeRetValA = yes(RetValA),
        MaybeRetValB = yes(RetValB),
        P(RetValA, RetValB, !Acc1, !Acc2, !Acc3)
    ;
        MaybeRetValA = yes(_),
        MaybeRetValB = no,
        unexpected($pred, "mismatched proc_arg_vectors")
    ;
        MaybeRetValA = no,
        MaybeRetValB = yes(_),
        unexpected($pred, "mismatched proc_arg_vectors")
    ;
        MaybeRetValA = no,
        MaybeRetValB = no
    ).

proc_arg_vector_foldl2_corresponding3(P, A, B, C, !Acc1, !Acc2) :-
    A = proc_arg_vector(ITIA, ITCIA, UTIA, ETIA, UTCIA, ETCIA, ArgsA,
        MaybeRetValA),
    B = proc_arg_vector(ITIB, ITCIB, UTIB, ETIB, UTCIB, ETCIB, ArgsB,
        MaybeRetValB),
    C = proc_arg_vector(ITIC, ITCIC, UTIC, ETIC, UTCIC, ETCIC, ArgsC,
        MaybeRetValC),
    list.foldl2_corresponding3(P, ITIA,  ITIB,  ITIC,  !Acc1, !Acc2),
    list.foldl2_corresponding3(P, ITCIA, ITCIB, ITCIC, !Acc1, !Acc2),
    list.foldl2_corresponding3(P, UTIA,  UTIB,  UTIC,  !Acc1, !Acc2),
    list.foldl2_corresponding3(P, ETIA,  ETIB,  ETIC,  !Acc1, !Acc2),
    list.foldl2_corresponding3(P, UTCIA, UTCIB, UTCIC, !Acc1, !Acc2),
    list.foldl2_corresponding3(P, ETCIA, ETCIB, ETCIC, !Acc1, !Acc2),
    list.foldl2_corresponding3(P, ArgsA, ArgsB, ArgsC, !Acc1, !Acc2),
    ( if
        MaybeRetValA = yes(RetValA),
        MaybeRetValB = yes(RetValB),
        MaybeRetValC = yes(RetValC)
    then
        P(RetValA, RetValB, RetValC, !Acc1, !Acc2)
    else if
        MaybeRetValA = no,
        MaybeRetValB = no,
        MaybeRetValC = no
    then
        true
    else
        unexpected($pred, "mismatched proc_arg_vectors")
    ).

proc_arg_vector_foldl3_corresponding3(P, A, B, C, !Acc1, !Acc2, !Acc3) :-
    A = proc_arg_vector(ITIA, ITCIA, UTIA, ETIA, UTCIA, ETCIA, ArgsA,
        MaybeRetValA),
    B = proc_arg_vector(ITIB, ITCIB, UTIB, ETIB, UTCIB, ETCIB, ArgsB,
        MaybeRetValB),
    C = proc_arg_vector(ITIC, ITCIC, UTIC, ETIC, UTCIC, ETCIC, ArgsC,
        MaybeRetValC),
    list.foldl3_corresponding3(P, ITIA,  ITIB,  ITIC,  !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, ITCIA, ITCIB, ITCIC, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, UTIA,  UTIB,  UTIC,  !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, ETIA,  ETIB,  ETIC,  !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, UTCIA, UTCIB, UTCIC, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, ETCIA, ETCIB, ETCIC, !Acc1, !Acc2, !Acc3),
    list.foldl3_corresponding3(P, ArgsA, ArgsB, ArgsC, !Acc1, !Acc2, !Acc3),
    ( if
        MaybeRetValA = yes(RetValA),
        MaybeRetValB = yes(RetValB),
        MaybeRetValC = yes(RetValC)
    then
        P(RetValA, RetValB, RetValC, !Acc1, !Acc2, !Acc3)
    else if
        MaybeRetValA = no,
        MaybeRetValB = no,
        MaybeRetValC = no
    then
        true
    else
        unexpected($pred, "mismatched proc_arg_vectors")
    ).

proc_arg_vector_foldl4_corresponding3(P, A, B, C, !Acc1, !Acc2, !Acc3,
        !Acc4) :-
    A = proc_arg_vector(ITIA, ITCIA, UTIA, ETIA, UTCIA, ETCIA, ArgsA,
        MaybeRetValA),
    B = proc_arg_vector(ITIB, ITCIB, UTIB, ETIB, UTCIB, ETCIB, ArgsB,
        MaybeRetValB),
    C = proc_arg_vector(ITIC, ITCIC, UTIC, ETIC, UTCIC, ETCIC, ArgsC,
        MaybeRetValC),
    list.foldl4_corresponding3(P, ITIA,  ITIB,  ITIC,  !Acc1, !Acc2, !Acc3,
        !Acc4),
    list.foldl4_corresponding3(P, ITCIA, ITCIB, ITCIC, !Acc1, !Acc2, !Acc3,
        !Acc4),
    list.foldl4_corresponding3(P, UTIA,  UTIB,  UTIC,  !Acc1, !Acc2, !Acc3,
        !Acc4),
    list.foldl4_corresponding3(P, ETIA,  ETIB,  ETIC,  !Acc1, !Acc2, !Acc3,
        !Acc4),
    list.foldl4_corresponding3(P, UTCIA, UTCIB, UTCIC, !Acc1, !Acc2, !Acc3,
        !Acc4),
    list.foldl4_corresponding3(P, ETCIA, ETCIB, ETCIC, !Acc1, !Acc2, !Acc3,
        !Acc4),
    list.foldl4_corresponding3(P, ArgsA, ArgsB, ArgsC, !Acc1, !Acc2, !Acc3,
        !Acc4),
    ( if
        MaybeRetValA = yes(RetValA),
        MaybeRetValB = yes(RetValB),
        MaybeRetValC = yes(RetValC)
    then
        P(RetValA, RetValB, RetValC, !Acc1, !Acc2, !Acc3, !Acc4)
    else if
        MaybeRetValA = no,
        MaybeRetValB = no,
        MaybeRetValC = no
    then
        true
    else
        unexpected($pred, "mismatched proc_arg_vectors")
    ).

%-----------------------------------------------------------------------------%
%
% Stuff related to the polymorphism transformation.
%

    % Internally we represent a poly_arg_vector as a proc_arg_vector.
    % This ensures that poly_arg_vectors obey the same calling convention
    % w.r.t introduced type_info and typeclass_info arguments that
    % proc_arg_vectors do. For the proc_arg_vectors that are used to represent
    % poly_arg_vectors, we insist that the last two fields are
    % the empty list and `no' respectively.
    %
:- type poly_arg_vector(T) == proc_arg_vector(T).

poly_arg_vector_init = proc_arg_vector_init(pf_predicate, []).

poly_arg_vector_set_instance_type_infos(ITI, !A) :-
    proc_arg_vector_set_instance_type_infos(ITI, !A).
poly_arg_vector_set_instance_typeclass_infos(ITCI, !A) :-
    proc_arg_vector_set_instance_typeclass_infos(ITCI, !A).
poly_arg_vector_set_univ_type_infos(UTI, !A) :-
    proc_arg_vector_set_univ_type_infos(UTI, !A).
poly_arg_vector_set_exist_type_infos(ETI, !A) :-
    proc_arg_vector_set_exist_type_infos(ETI, !A).
poly_arg_vector_set_univ_typeclass_infos(UTCI, !A) :-
    proc_arg_vector_set_univ_typeclass_infos(UTCI, !A).
poly_arg_vector_set_exist_typeclass_infos(ETCI, !A) :-
    proc_arg_vector_set_exist_typeclass_infos(ETCI, !A).

poly_arg_vector_to_list(V) =
    proc_arg_vector_to_list(V).

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_args.
%-----------------------------------------------------------------------------%
