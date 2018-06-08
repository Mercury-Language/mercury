%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: term_norm.m.
% Main author: crs.
%
% This modules defines predicates for computing functor norms.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.term_norm.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module libs.
:- import_module libs.globals.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

%-----------------------------------------------------------------------------%

    % The functor_info type contains information about how the weight of a
    % term is calculated.
    %
:- type functor_info.

    % This predicate sets the functor_info depending on the value of the
    % termination_norm or termination2_norm option.
    %
:- func set_functor_info(module_info, globals.termination_norm) = functor_info.

    % This predicate computes the weight of a functor and the set of arguments
    % of that functor whose sizes should be counted towards the size of the
    % whole term.
    %
    % NOTE: the list of arguments and the list of modes must be the same
    % length. They must also *not* contain any typeinfo related arguments as
    % this may cause an exception to be thrown when using the
    % `--num-data-elems' norm. (This is because the weight table does not
    % keep track of typeinfo related variables - it used to, but intervening
    % compiler passes tend to do things to the code in the mean time, so the
    % whole lot becomes inconsistent - in the end it is just easier to ignore
    % them).
    %
:- pred functor_norm(module_info::in, functor_info::in, type_ctor::in,
    cons_id::in, int::out, list(prog_var)::in, list(prog_var)::out,
    list(unify_mode)::in, list(unify_mode)::out) is det.

    % This function computes a lower bound on the weight of a functor.
    % If the lower bound is zero then the weight of that functor is also zero.
    % If the lower bound is non-zero then there may be no upper bound
    % on the size of the functor. (And if there were, this function
    % would not tell you about it anyhow).
    %
:- func functor_lower_bound(module_info, functor_info, type_ctor, cons_id)
    = int.

    % Succeeds if all values of the given type are zero size (for all norms).
    %
:- pred zero_size_type(module_info::in, mer_type::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.type_util.
:- import_module hlds.const_struct.
:- import_module hlds.hlds_data.
:- import_module parse_tree.prog_type.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.

%-----------------------------------------------------------------------------%

% We use semilinear norms (denoted by ||) to compute the sizes of terms.
% These have the form
%
% | f(t1, ... tn) | = weight(f) + sum of | ti |
% where i is an element of a set I, and I is a subset of {1, ... n}
%
% We currently support four kinds of semilinear norms.
% XXX Actually we currently only use three of them. `use_map/1' is unused.

:- type functor_info
    --->    simple
            % All non-constant functors have weight 1, while constants
            % have weight 0. Use the size of all subterms (I = {1, ..., n}.

    ;       total
            % All functors have weight = arity of the functor.
            % Use the size of all subterms (I = {1, ..., n}.

    ;       use_map(weight_table)
            % The weight of each functor is given by the table.
            % Use the size of all subterms (I = {1, ..., n}.

    ;       use_map_and_args(weight_table).
            % The weight of each functor is given by the table, and so is
            % the set of arguments of the functor whose size should be counted
            % (I is given by the table entry of the functor).

%-----------------------------------------------------------------------------%

% Calculate the weight to be assigned to each function symbol for the
% use_map and use_map_and_args semilinear norms.
%
% Given a type definition such as
%
% :- type t(Tk) --->    f1(a11, ... a1n1)   where n1 is the arity of f1
%       ;   ...
%       ;   fm(am1, ... amnm)   where nm is the arity of fm
%
% we check, for each aij, whether its type is recursive (i.e. it is t with
% type variable arguments that are a permutation of Tk). The weight info
% we compute for each functor will have a boolean list that has a `yes'
% for each recursive argument and a `no' for each nonrecursive argument.
% The weight to be assigned to the functor is the number of nonrecursive
% arguments, except that we assign a weight of at least 1 to all functors
% which are not constants.

:- type weight_table == map(pair(type_ctor, cons_id), weight_info).

:- type weight_info ---> weight(int, list(bool)).

:- pred find_weights(module_info::in, weight_table::out) is det.

find_weights(ModuleInfo, Weights) :-
    module_info_get_type_table(ModuleInfo, TypeTable),
    get_all_type_ctor_defns(TypeTable, TypeCtorsDefns),
    map.init(Weights0),
    list.foldl(find_weights_for_type, TypeCtorsDefns, Weights0, Weights).

:- pred find_weights_for_type(pair(type_ctor, hlds_type_defn)::in,
    weight_table::in, weight_table::out) is det.

find_weights_for_type(TypeCtor - TypeDefn, !Weights) :-
    hlds_data.get_type_defn_body(TypeDefn, TypeBody),
    (
        TypeBody = hlds_du_type(Constructors, _, _, _),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        list.foldl(find_weights_for_cons(TypeCtor, TypeParams),
            Constructors, !Weights)
    ;
        % This type does not introduce any functors.
        TypeBody = hlds_eqv_type(_)
    ;
        % This type may introduce some functors,
        % but we will never see them in this analysis.
        TypeBody = hlds_abstract_type(_)
    ;
        % This type does not introduce any functors.
        TypeBody = hlds_foreign_type(_)
    ;
        % This type does not introduce any functors.
        TypeBody = hlds_solver_type(_)
    ).

:- pred find_weights_for_cons(type_ctor::in, list(type_param)::in,
    constructor::in, weight_table::in, weight_table::out) is det.

find_weights_for_cons(TypeCtor, Params, Ctor, !Weights) :-
    % For existentially typed data items the compiler may insert some
    % typeinfo related arguments into the functor. We ignore these arguments
    % when calculating the weight of a functor and we do not include them
    % in the list of counted arguments.
    Ctor = ctor(_Ordinal, _MaybeExistConstraints, SymName, Args, Arity,
        _Context),
    ( if Arity > 0 then
        find_and_count_nonrec_args(Args, TypeCtor, Params,
            NumNonRec, ArgInfos0),
        ( if NumNonRec = 0 then
            Weight = 1,
            list.duplicate(Arity, yes, ArgInfos)
        else
            Weight = NumNonRec,
            ArgInfos = ArgInfos0
        ),
        WeightInfo = weight(Weight, ArgInfos)
    else
        WeightInfo = weight(0, [])
    ),
    ConsId = cons(SymName, Arity, TypeCtor),
    map.det_insert(TypeCtor - ConsId, WeightInfo, !Weights).

:- pred find_weights_for_tuple(arity::in, weight_info::out) is det.

find_weights_for_tuple(Arity, weight(Weight, ArgInfos)) :-
    % None of the tuple arguments are recursive.
    Weight = Arity,
    list.duplicate(Arity, yes, ArgInfos).

:- pred find_and_count_nonrec_args(list(constructor_arg)::in,
    type_ctor::in, list(type_param)::in,
    int::out, list(bool)::out) is det.

find_and_count_nonrec_args([], _, _, 0, []).
find_and_count_nonrec_args([Arg | Args], Id, Params, NonRecArgs, ArgInfo) :-
    find_and_count_nonrec_args(Args, Id, Params, NonRecArgs0, ArgInfo0),
    ( if is_arg_recursive(Arg, Id, Params) then
        NonRecArgs = NonRecArgs0,
        ArgInfo = [yes | ArgInfo0]
    else
        NonRecArgs = NonRecArgs0 + 1,
        ArgInfo = [no | ArgInfo0]
    ).

:- pred is_arg_recursive(constructor_arg::in, type_ctor::in,
    list(type_param)::in) is semidet.

is_arg_recursive(Arg, TypeCtor, Params) :-
    ArgType = Arg ^ arg_type,
    type_to_ctor_and_args(ArgType, ArgTypeCtor, ArgTypeArgs),
    TypeCtor = ArgTypeCtor,
    prog_type.type_list_to_var_list(ArgTypeArgs, ArgTypeParams),
    list.perm(Params, ArgTypeParams).

:- pred search_weight_table(weight_table::in, type_ctor::in, cons_id::in,
    weight_info::out) is semidet.

search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) :-
    ( if map.search(WeightMap, TypeCtor - ConsId, WeightInfo0) then
        WeightInfo = WeightInfo0
    else if type_ctor_is_tuple(TypeCtor) then
        TypeCtor = type_ctor(_, Arity),
        find_weights_for_tuple(Arity, WeightInfo)
    else
        fail
    ).

%-----------------------------------------------------------------------------%

set_functor_info(_ModuleInfo, norm_total) = total.
set_functor_info(_ModuleInfo, norm_simple) = simple.
set_functor_info(ModuleInfo, norm_num_data_elems) = FunctorInfo :-
    find_weights(ModuleInfo, WeightMap),
    FunctorInfo = use_map_and_args(WeightMap).
set_functor_info(ModuleInfo, norm_size_data_elems) = FunctorInfo :-
    find_weights(ModuleInfo, WeightMap),
    FunctorInfo = use_map(WeightMap).

%-----------------------------------------------------------------------------%

functor_norm(ModuleInfo, FunctorInfo, TypeCtor, ConsId, Gamma,
        !Args, !Modes) :-
    (
        FunctorInfo = simple,
        ( if
            ConsId = cons(_, Arity, _),
            Arity \= 0
        then
            Gamma = 1
        else if
            ConsId = ground_term_const(ConstNum, _)
        then
            module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
            const_struct_count_cells(ConstStructDb, ConstNum, 0, Gamma)
        else
            Gamma = 0
        )
    ;
        FunctorInfo = total,
        ( if ConsId = cons(_, Arity, _) then
            Gamma = Arity
        else if ConsId = ground_term_const(ConstNum, _) then
            module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
            const_struct_count_cell_arities(ConstStructDb, ConstNum, 0, Gamma)
        else
            Gamma = 0
        )
    ;
        FunctorInfo = use_map(WeightMap),
        ( if search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) then
            WeightInfo = weight(Gamma, _)
        else if ConsId = ground_term_const(ConstNum, _) then
            module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
            const_struct_count_cell_weights(ConstStructDb, WeightMap,
                ConstNum, 0, Gamma)
        else
            Gamma = 0
        )
    ;
        FunctorInfo = use_map_and_args(WeightMap),
        ( if search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) then
            WeightInfo = weight(Gamma, UseArgList),
            ( if functor_norm_filter_args(UseArgList, !Args, !Modes) then
                true
            else
                unexpected($pred, "unmatched lists")
            )
        else if ConsId = ground_term_const(ConstNum, _) then
            % XXX Since ground_term_consts have no argument variables,
            % we cannot filter those argument variables. I (zs) *think* that
            % returning the !.Args and !.Modes (which should both be empty
            % to begin with) does the right thing, but I am not sure.
            module_info_get_const_struct_db(ModuleInfo, ConstStructDb),
            const_struct_count_cell_filtered_weights(ConstStructDb, WeightMap,
                ConstNum, 0, Gamma)
        else
            Gamma = 0
        )
    ).

    % This predicate will fail if the length of the input lists are not
    % matched.
    %
:- pred functor_norm_filter_args(list(bool)::in, list(prog_var)::in,
    list(prog_var)::out, list(unify_mode)::in, list(unify_mode)::out)
    is semidet.

functor_norm_filter_args([], [], [], [], []).
functor_norm_filter_args([yes | Bools], [Arg0 | Args0], [Arg0 | Args],
        [Mode0 | Modes0], [Mode0 | Modes]) :-
    functor_norm_filter_args(Bools, Args0, Args, Modes0, Modes).
functor_norm_filter_args([no | Bools], [_Arg0 | Args0], Args,
        [_Mode0 | Modes0], Modes) :-
    functor_norm_filter_args(Bools, Args0, Args, Modes0, Modes).

%-----------------------------------------------------------------------------%

:- pred const_struct_count_cells(const_struct_db::in, int::in,
    int::in, int::out) is det.

const_struct_count_cells(ConstStructDb, ConstNum, !Gamma) :-
    lookup_const_struct_num(ConstStructDb, ConstNum, ConstStruct),
    ConstStruct = const_struct(_ConsId, Args, _, _),
    !:Gamma = !.Gamma + 1,
    const_struct_count_cells_args(ConstStructDb, Args, !Gamma).

:- pred const_struct_count_cells_args(const_struct_db::in,
    list(const_struct_arg)::in, int::in, int::out) is det.

const_struct_count_cells_args(_ConstStructDb, [], !Gamma).
const_struct_count_cells_args(ConstStructDb, [Arg | Args], !Gamma) :-
    (
        Arg = csa_constant(_, _)
    ;
        Arg = csa_const_struct(ArgConstNum),
        const_struct_count_cells(ConstStructDb, ArgConstNum, !Gamma)
    ),
    const_struct_count_cells_args(ConstStructDb, Args, !Gamma).

:- pred const_struct_count_cell_arities(const_struct_db::in, int::in,
    int::in, int::out) is det.

const_struct_count_cell_arities(ConstStructDb, ConstNum, !Gamma) :-
    lookup_const_struct_num(ConstStructDb, ConstNum, ConstStruct),
    ConstStruct = const_struct(_ConsId, Args, _, _),
    !:Gamma = !.Gamma + list.length(Args),
    const_struct_count_cell_arities_args(ConstStructDb, Args, !Gamma).

:- pred const_struct_count_cell_arities_args(const_struct_db::in,
    list(const_struct_arg)::in, int::in, int::out) is det.

const_struct_count_cell_arities_args(_ConstStructDb, [], !Gamma).
const_struct_count_cell_arities_args(ConstStructDb, [Arg | Args], !Gamma) :-
    (
        Arg = csa_constant(_, _)
    ;
        Arg = csa_const_struct(ArgConstNum),
        const_struct_count_cell_arities(ConstStructDb, ArgConstNum, !Gamma)
    ),
    const_struct_count_cell_arities_args(ConstStructDb, Args, !Gamma).

:- pred const_struct_count_cell_weights(const_struct_db::in,
    weight_table::in, int::in, int::in, int::out) is det.

const_struct_count_cell_weights(ConstStructDb, WeightMap, ConstNum, !Gamma) :-
    lookup_const_struct_num(ConstStructDb, ConstNum, ConstStruct),
    ConstStruct = const_struct(ConsId, Args, Type, _),
    type_to_ctor_det(Type, TypeCtor),
    ( if search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) then
        WeightInfo = weight(ConsIdGamma, _),
        !:Gamma = !.Gamma + ConsIdGamma,
        const_struct_count_cell_weights_args(ConstStructDb, WeightMap,
            Args, !Gamma)
    else
        true
    ).

:- pred const_struct_count_cell_weights_args(const_struct_db::in,
    weight_table::in, list(const_struct_arg)::in, int::in, int::out) is det.

const_struct_count_cell_weights_args(_ConstStructDb, _WeightMap, [], !Gamma).
const_struct_count_cell_weights_args(ConstStructDb, WeightMap,
        [Arg | Args], !Gamma) :-
    (
        Arg = csa_constant(ConsId, Type),
        type_to_ctor_det(Type, TypeCtor),
        ( if search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) then
            WeightInfo = weight(ConsIdGamma, _),
            !:Gamma = !.Gamma + ConsIdGamma
        else
            true
        )
    ;
        Arg = csa_const_struct(ArgConstNum),
        const_struct_count_cell_weights(ConstStructDb, WeightMap,
            ArgConstNum, !Gamma)
    ),
    const_struct_count_cell_weights_args(ConstStructDb, WeightMap,
        Args, !Gamma).

:- pred const_struct_count_cell_filtered_weights(const_struct_db::in,
    weight_table::in, int::in, int::in, int::out) is det.

const_struct_count_cell_filtered_weights(ConstStructDb, WeightMap,
        ConstNum, !Gamma) :-
    lookup_const_struct_num(ConstStructDb, ConstNum, ConstStruct),
    ConstStruct = const_struct(ConsId, Args, Type, _),
    type_to_ctor_det(Type, TypeCtor),
    ( if search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) then
        WeightInfo = weight(ConsIdGamma, UseArgs),
        !:Gamma = !.Gamma + ConsIdGamma,
        const_struct_count_cell_filtered_weights_args(ConstStructDb, WeightMap,
            Args, UseArgs, !Gamma)
    else
        true
    ).

:- pred const_struct_count_cell_filtered_weights_args(const_struct_db::in,
    weight_table::in, list(const_struct_arg)::in, list(bool)::in,
    int::in, int::out) is det.

const_struct_count_cell_filtered_weights_args(_ConstStructDb, _WeightMap,
        [], [], !Gamma).
const_struct_count_cell_filtered_weights_args(_ConstStructDb, _WeightMap,
        [], [_ | _], !Gamma) :-
    unexpected($pred, "mismatched lists").
const_struct_count_cell_filtered_weights_args(_ConstStructDb, _WeightMap,
        [_ | _], [], !Gamma) :-
    unexpected($pred, "mismatched lists").
const_struct_count_cell_filtered_weights_args(ConstStructDb, WeightMap,
        [Arg | Args], [UseArg | UseArgs], !Gamma) :-
    (
        UseArg = no
    ;
        UseArg = yes,
        (
            Arg = csa_constant(ConsId, Type),
            type_to_ctor_det(Type, TypeCtor),
            ( if
                search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo)
            then
                WeightInfo = weight(ConsIdGamma, _),
                !:Gamma = !.Gamma + ConsIdGamma
            else
                true
            )
        ;
            Arg = csa_const_struct(ArgConstNum),
            const_struct_count_cell_filtered_weights(ConstStructDb, WeightMap,
                ArgConstNum, !Gamma)
        )
    ),
    const_struct_count_cell_filtered_weights_args(ConstStructDb, WeightMap,
        Args, UseArgs, !Gamma).

%-----------------------------------------------------------------------------%

functor_lower_bound(_ModuleInfo, FunctorInfo, TypeCtor, ConsId) = Weight :-
    (
        FunctorInfo = simple,
        Weight = ( if ConsId = cons(_, Arity, _), Arity \= 0 then 1 else 0 )
    ;
        FunctorInfo = total,
        Weight = ( if ConsId = cons(_, Arity, _) then Arity else 0 )
    ;
        FunctorInfo = use_map(WeightMap),
        ( if search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) then
            WeightInfo = weight(Weight, _)
        else
            Weight = 0
        )
    ;
        FunctorInfo = use_map_and_args(WeightMap),
        ( if search_weight_table(WeightMap, TypeCtor, ConsId, WeightInfo) then
            WeightInfo = weight(Weight, _)
        else
            Weight = 0
        )
    ).

%-----------------------------------------------------------------------------%

zero_size_type(ModuleInfo, Type) :-
    CtorCat = classify_type(ModuleInfo, Type),
    zero_size_type_category(CtorCat, yes).

:- pred zero_size_type_category(type_ctor_category::in, bool::out) is det.

zero_size_type_category(CtorCat, ZeroSize) :-
    (
        ( CtorCat = ctor_cat_builtin(_)
        ; CtorCat = ctor_cat_user(cat_user_direct_dummy)
        ; CtorCat = ctor_cat_user(cat_user_abstract_dummy)
        ; CtorCat = ctor_cat_void
        ; CtorCat = ctor_cat_system(_)
        ; CtorCat = ctor_cat_higher_order
        ; CtorCat = ctor_cat_enum(_)
        ; CtorCat = ctor_cat_builtin_dummy
        ),
        ZeroSize = yes
    ;
        ( CtorCat = ctor_cat_user(cat_user_notag)
        ; CtorCat = ctor_cat_user(cat_user_abstract_notag)
        ; CtorCat = ctor_cat_user(cat_user_general)
        ; CtorCat = ctor_cat_tuple
        ; CtorCat = ctor_cat_variable
        ),
        ZeroSize = no
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.term_norm.
%-----------------------------------------------------------------------------%
