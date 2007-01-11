%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: hlds_args.m.
% Main authors: juliensf.
%
% This module defines the part of the HLDS that deals with procedure and call
% site arguments.  (See comments at the head of polymorphism.m for further
% details.)
%
%-----------------------------------------------------------------------------%

:- module hlds.hlds_args.
:- interface.
  
:- import_module mdbcomp.prim_data.
  
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module set.

%-----------------------------------------------------------------------------%

	% This type represents the arguments to a predicate symbol, or the
	% arguments and result of a function symbol.  The arguments may be
	% variables, types, modes, etc, depending on the context.
	%
	% Rather than keep all arguments in a single list, we retain
	% information about the origin of each argument (such as whether
	% it was introduced by polymorphism.m to hold a type_info).  This
	% simplifies the processing in polymorphism.m, and also abstracts
	% away the specific calling convention that we use.
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
% Utility predicates that operate on proc_arg_vectors
%

    % Return a list of the items in a arg_vector.  The order of the
    % list corresponds to that required by the calling conventions.
    % See comments at the head of polymorphism.m. for details.  If the
    % arg_vector is for a function then the last element in the list
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
    % a function return value.  Throws an exception if the arg_vector does
    % not correspond to a function.
    %
:- pred proc_arg_vector_to_func_args(proc_arg_vector(T)::in,
    list(T)::out, T::out) is det.

%-----------------------------------------------------------------------------%
%
% Stuff related to the polymorphism pass
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
    % proc_arg_vectors.  We should then provide a predicate that merges a
    % poly_arg_vector with a proc_arg_vector.
    % 
:- func poly_arg_vector_to_list(poly_arg_vector(T)) = list(T).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module libs.compiler_util.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.prog_type.  
    % Require for apply_partial_map_to_list
    % XXX that should really live in a different module.

:- import_module string.

%-----------------------------------------------------------------------------%

    % The first six fields are set by the polymorphism pass.
    %
:- type proc_arg_vector(T)
    --->    proc_arg_vector(
                pav_instance_type_infos :: list(T),
                % Type_infos for the unconstrained type variables in an
                % instance declaration in the order which they first appear
                % in the instance arguments.  For procedures that are not
                % class methods this will always be the empty list.
               
                pav_instance_typeclass_infos :: list(T),
                % Typeclass_infos for the class constraints on an instance
                % declaration, in the order in which they appear in the
                % declaration.  For procedures that are not class methods
                % this will always be the empty list.
                
                pav_univ_type_infos :: list(T),
                % Type_infos for unconstrained universally quantified type
                % variables, in the order in which they first appear in the
                % argument types.
                
                pav_exist_type_infos :: list(T),
                % Type_infos for unconstrained existentially quantified type
                % variables, in the order that the type variables first
                % appear in the argument types.
                
                pav_univ_typeclass_infos :: list(T),
                % Typeclass_infos for universally quantified constraints
                % in the order in which the constraints appear in the class
                % context.
                
                pav_exist_typeclass_infos :: list(T),
                % Typeclass_infos for existentially quantified constraints
                % in the order in which the constraints appear in the class
                % context.
                
                pav_user_args :: list(T),
                % The original procedure arguments.
                % XXX Plus at the moment any arguments that may be
                % introduced by transformations performed by the compiler.
                % Eventually these should be separated out.
                
                pav_maybe_ret_value :: maybe(T)
                % For predicates this field is no; for functions it
                % corresponds to the function's return value.
            ).

%-----------------------------------------------------------------------------%

proc_arg_vector_init(PredOrFunc, Args0) = ArgVec :-
    (
        PredOrFunc = predicate,
        Args = Args0,
        MaybeRetVal = no
    ;
        PredOrFunc = function,
        list.det_split_last(Args0, Args, RetVal),
        MaybeRetVal = yes(RetVal)
    ),
    ArgVec = proc_arg_vector([], [], [], [], [], [], Args, MaybeRetVal).

%-----------------------------------------------------------------------------%

proc_arg_vector_get_instance_type_infos(V)      = V ^ pav_instance_type_infos.
proc_arg_vector_get_instance_typeclass_infos(V) =
    V ^ pav_instance_typeclass_infos.
proc_arg_vector_get_univ_type_infos(V)          = V ^ pav_univ_type_infos.
proc_arg_vector_get_exist_type_infos(V)         = V ^ pav_exist_type_infos.
proc_arg_vector_get_univ_typeclass_infos(V)     = V ^ pav_univ_typeclass_infos.
proc_arg_vector_get_exist_typeclass_infos(V)    = V ^ pav_exist_typeclass_infos.
proc_arg_vector_get_user_args(V)                = V ^ pav_user_args.
proc_arg_vector_get_maybe_ret_value(V)          = V ^ pav_maybe_ret_value.

proc_arg_vector_set_instance_type_infos(ITI, !V) :-
    !:V = !.V ^ pav_instance_type_infos := ITI.
proc_arg_vector_set_instance_typeclass_infos(ITCI, !V) :-
    !:V = !.V ^ pav_instance_typeclass_infos := ITCI.
proc_arg_vector_set_univ_type_infos(UTI, !V) :-
    !:V = !.V ^ pav_univ_type_infos := UTI.
proc_arg_vector_set_exist_type_infos(ETI, !V) :-
    !:V = !.V ^ pav_exist_type_infos := ETI.
proc_arg_vector_set_univ_typeclass_infos(UTCI, !V) :-
    !:V = !.V ^ pav_univ_typeclass_infos := UTCI.
proc_arg_vector_set_exist_typeclass_infos(ETCI, !V) :-
    !:V = !.V ^ pav_exist_typeclass_infos := ETCI.
proc_arg_vector_set_user_args(UA, !V) :-
    !:V = !.V ^ pav_user_args := UA.
proc_arg_vector_set_maybe_ret_value(RV, !V) :-
    !:V = !.V ^ pav_maybe_ret_value := RV.

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
        ( map.search(Renaming, Value0, Value) ->
            MaybeRetValue = yes(Value)
        ;
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
        MaybeRetValue = yes(Value),
        RetValue = [Value]
    ;
        MaybeRetValue = no,
        RetValue = []
    ),
    NonPolyArgs = OrigArgs ++ RetValue.

proc_arg_vector_member(Vec, V) :-
    List = proc_arg_vector_to_list(Vec),
    list.member(V, List).

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
        unexpected(this_file, "Call to arg_vector_to_func_args/3 " ++
            "with predicate.")
    ).

%-----------------------------------------------------------------------------%
%
% Stuff related to the polymorphism transformation
%

    % Internally we represent a poly_arg_vector as a proc_arg_vector.
    % This ensures that poly_arg_vectors obey the same calling convention
    % w.r.t introduced type_info and typeclass_info arguments that 
    % proc_arg_vectors do.  For the proc_arg_vectors that are
    % used to represent poly_arg_vectors we insist that the the last
    % two fields are the empty list and `no' respectively.
    %
:- type poly_arg_vector(T) == proc_arg_vector(T).

poly_arg_vector_init = proc_arg_vector_init(predicate, []).

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

:- func this_file = string.

this_file = "hlds_args.m".

%-----------------------------------------------------------------------------%
:- end_module hlds.hlds_args.
%-----------------------------------------------------------------------------%
