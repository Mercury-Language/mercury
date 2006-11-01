%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1994-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: mode_util.m.
% Main author: fjh.
%
% This module contains utility predicates for dealing with modes and insts.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.mode_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.

%-----------------------------------------------------------------------------%

    % mode_get_insts returns the initial instantiatedness and the final
    % instantiatedness for a given mode.
    % Throw an exception if the mode is undefined.
    %
:- pred mode_get_insts(module_info::in, mer_mode::in,
    mer_inst::out, mer_inst::out) is det.

    % A version of mode_get_insts that fails if the mode is undefined.
    %
:- pred mode_get_insts_semidet(module_info::in, mer_mode::in,
    mer_inst::out, mer_inst::out) is semidet.

    % A mode is considered input if the initial inst is bound.
    % Throws an exception if the mode is undefined.
    %
:- pred mode_is_input(module_info::in, mer_mode::in) is semidet.

    % A mode is considered fully input if the initial inst is ground.
    % Throws an exception if the mode is undefined.
    %
:- pred mode_is_fully_input(module_info::in, mer_mode::in) is semidet.

    % A mode is considered output if the initial inst is free and the
    % final inst is bound.
    % Throws an exception if the mode is undefined.
    %
:- pred mode_is_output(module_info::in, mer_mode::in) is semidet.

    % A mode is considered fully output if the initial inst is free and
    % the final inst is ground.
    % Throws an exception if the mode is undefined.
    %
:- pred mode_is_fully_output(module_info::in, mer_mode::in) is semidet.

    % A mode is considered unused if both initial and final insts are free.
    % Throws an exception if the mode is undefined.
    %
:- pred mode_is_unused(module_info::in, mer_mode::in) is semidet.

    % Succeeds iff the given mode is undefined.
    %
:- pred mode_is_undefined(module_info::in, mer_mode::in) is semidet.

    % mode_to_arg_mode converts a mode (and corresponding type) to
    % an arg_mode.  A mode is a high-level notion, the normal
    % Mercury language mode.  An `arg_mode' is a low-level notion
    % used for code generation, which indicates the argument
    % passing convention (top_in, top_out, or top_unused) that
    % corresponds to that mode.  We need to know the type, not just
    % the mode, because the argument passing convention can depend
    % on the type's representation.
    %
:- pred mode_to_arg_mode(module_info::in, mer_mode::in, mer_type::in,
    arg_mode::out) is det.

:- pred modes_to_arg_modes(module_info::in, list(mer_mode)::in,
    list(mer_type)::in, list(arg_mode)::out) is det.

:- func mode_get_initial_inst(module_info, mer_mode) = mer_inst.

:- func mode_get_final_inst(module_info, mer_mode) = mer_inst.

:- pred mode_list_get_initial_insts(module_info::in,
    list(mer_mode)::in, list(mer_inst)::out) is det.

:- pred mode_list_get_final_insts(module_info::in,
    list(mer_mode)::in, list(mer_inst)::out) is det.

:- pred modes_to_uni_modes(module_info::in, list(mer_mode)::in,
    list(mer_mode)::in, list(uni_mode)::out) is det.

    % Given a user-defined or compiler-defined inst name, lookup the
    % corresponding inst in the inst table.
    %
:- pred inst_lookup(module_info::in, inst_name::in, mer_inst::out) is det.

    % Use the instmap deltas for all the atomic sub-goals to recompute
    % the instmap deltas for all the non-atomic sub-goals of a goal.
    % Used to ensure that the instmap deltas remain valid after code has
    % been re-arranged, e.g. by followcode.  This also takes the
    % module_info as input and output since it may need to insert new
    % merge_insts into the merge_inst table.  If the first argument is
    % yes, the instmap_deltas for calls and deconstruction unifications
    % are also recomputed.
    %
:- pred recompute_instmap_delta_proc(bool::in, proc_info::in, proc_info::out,
    module_info::in, module_info::out) is det.

:- pred recompute_instmap_delta(bool::in, hlds_goal::in, hlds_goal::out,
    vartypes::in, inst_varset::in, instmap::in, module_info::in,
    module_info::out) is det.

    % Given corresponding lists of types and modes, produce a new list
    % of modes which includes the information provided by the
    % corresponding types.
    %
:- pred propagate_types_into_mode_list(module_info::in, list(mer_type)::in,
    list(mer_mode)::in, list(mer_mode)::out) is det.

    % Given corresponding lists of types and insts and a substitution
    % for the type variables in the type, produce a new list of insts
    % which includes the information provided by the corresponding types.
    %
:- pred propagate_types_into_inst_list(module_info::in, tsubst::in,
    list(mer_type)::in, list(mer_inst)::in, list(mer_inst)::out) is det.

    % Convert a list of constructors to a list of bound_insts where the
    % arguments are `ground'.
    %
    % NOTE: the list(bound_inst) is not sorted and may contain
    %       duplicates.
    %
:- pred constructors_to_bound_insts(module_info::in, uniqueness::in,
    list(constructor)::in, list(bound_inst)::out) is det.

    % Convert a list of constructors to a list of bound_insts where the
    % arguments are `any'.
    %
    % NOTE: the list(bound_inst) is not sorted and may contain
    %       duplicates.
    %
:- pred constructors_to_bound_any_insts(module_info::in, uniqueness::in,
    list(constructor)::in, list(bound_inst)::out) is det.

    % Given the mode of a predicate, work out which arguments are live
    % (might be used again by the caller of that predicate) and which
    % are dead.
    %
:- pred get_arg_lives(module_info::in, list(mer_mode)::in, list(is_live)::out)
    is det.

    % Given the switched on variable and the instmaps before the switch
    % and after a branch make sure that any information added by the
    % functor test gets added to the instmap for the case.
    %
:- pred fixup_switch_var(prog_var::in, instmap::in, instmap::in,
    hlds_goal::in, hlds_goal::out) is det.

%-----------------------------------------------------------------------------%

:- pred normalise_insts(module_info::in, list(mer_type)::in,
    list(mer_inst)::in, list(mer_inst)::out) is det.

:- pred normalise_inst(module_info::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

%-----------------------------------------------------------------------------%

    % Partition a list of arguments into inputs and others.
    % Throws an exception if one of the modes is undefined.
    %
:- pred partition_args(module_info::in, list(mer_mode)::in, list(T)::in,
    list(T)::out, list(T)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_util.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_data.
:- import_module libs.
:- import_module libs.compiler_util.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.prog_type_subst.

:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module set.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

mode_get_initial_inst(ModuleInfo, Mode) = Inst :-
    mode_get_insts(ModuleInfo, Mode, Inst, _).

mode_get_final_inst(ModuleInfo, Mode) = Inst :-
    mode_get_insts(ModuleInfo, Mode, _, Inst).

mode_list_get_initial_insts(_ModuleInfo, [], []).
mode_list_get_initial_insts(ModuleInfo, [Mode | Modes], [Inst | Insts]) :-
    mode_get_insts(ModuleInfo, Mode, Inst, _),
    mode_list_get_initial_insts(ModuleInfo, Modes, Insts).

mode_list_get_final_insts(_ModuleInfo, [], []).
mode_list_get_final_insts(ModuleInfo, [Mode | Modes], [Inst | Insts]) :-
    mode_get_insts(ModuleInfo, Mode, _, Inst),
    mode_list_get_final_insts(ModuleInfo, Modes, Insts).

%-----------------------------------------------------------------------------%

    % A mode is considered an input mode if the top-level node is input.
    %
mode_is_input(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_bound(ModuleInfo, InitialInst).

    % A mode is considered fully input if its initial inst is ground.
    %
mode_is_fully_input(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, _FinalInst),
    inst_is_ground(ModuleInfo, InitialInst).

    % A mode is considered an output mode if the top-level node is output.
    %
mode_is_output(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_bound(ModuleInfo, FinalInst).

    % A mode is considered fully output if its initial inst is free
    % and its final insts is ground.
    %
mode_is_fully_output(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_ground(ModuleInfo, FinalInst).

    % A mode is considered a unused mode if it is equivalent to free >> free.
    %
mode_is_unused(ModuleInfo, Mode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    inst_is_free(ModuleInfo, InitialInst),
    inst_is_free(ModuleInfo, FinalInst).

mode_is_undefined(ModuleInfo, Mode) :-
    not mode_get_insts_semidet(ModuleInfo, Mode, _, _).

%-----------------------------------------------------------------------------%

modes_to_arg_modes(_ModuleInfo, [], [], []).
modes_to_arg_modes(_ModuleInfo, [], [_ | _], _) :-
        unexpected(this_file, "modes_to_arg_modes: length mismatch").
modes_to_arg_modes(_ModuleInfo, [_ | _], [], _) :-
        unexpected(this_file, "modes_to_arg_modes: length mismatch").
modes_to_arg_modes(ModuleInfo, [Mode | Modes], [Type | Types],
        [ArgMode | ArgModes]) :-
    mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
    modes_to_arg_modes(ModuleInfo, Modes, Types, ArgModes).

mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode) :-
    mode_to_arg_mode_2(ModuleInfo, Mode, Type, [], ArgMode).

:- pred mode_to_arg_mode_2(module_info::in, mer_mode::in, mer_type::in,
    list(type_ctor)::in, arg_mode::out) is det.

mode_to_arg_mode_2(ModuleInfo, Mode, Type, ContainingTypes, ArgMode) :-
    %
    % We need to handle no_tag types (types which have exactly one constructor,
    % and whose one constructor has exactly one argument) specially here,
    % since for them an inst of bound(f(free)) is not really bound as far as
    % code generation is concerned, since the f/1 will get optimized away.
    %
    (
        % Is this a no_tag type?
        type_is_no_tag_type(ModuleInfo, Type, FunctorName, ArgType),
        % Avoid infinite recursion.
        type_to_ctor_and_args(Type, TypeCtor, _TypeArgs),
        \+ list.member(TypeCtor, ContainingTypes)
    ->
        % The arg_mode will be determined by the mode and type of the
        % functor's argument, so we figure out the mode and type of the
        % argument, and then recurse.
        %
        mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
        ConsId = cons(FunctorName, 1),
        get_single_arg_inst(InitialInst, ModuleInfo, ConsId, InitialArgInst),
        get_single_arg_inst(FinalInst, ModuleInfo, ConsId, FinalArgInst),
        ModeOfArg = (InitialArgInst -> FinalArgInst),
        mode_to_arg_mode_2(ModuleInfo, ModeOfArg, ArgType,
            [TypeCtor | ContainingTypes], ArgMode)
    ;
        base_mode_to_arg_mode(ModuleInfo, Mode, ArgMode)
    ).

:- pred base_mode_to_arg_mode(module_info::in, mer_mode::in, arg_mode::out)
    is det.

base_mode_to_arg_mode(ModuleInfo, Mode, ArgMode) :-
    mode_get_insts(ModuleInfo, Mode, InitialInst, FinalInst),
    ( inst_is_bound(ModuleInfo, InitialInst) ->
        ArgMode = top_in
    ; inst_is_bound(ModuleInfo, FinalInst) ->
        ArgMode = top_out
    ;
        ArgMode = top_unused
    ).

%-----------------------------------------------------------------------------%

    % get_single_arg_inst(Inst, ConsId, Arity, ArgInsts):
    % Given an inst `Inst', figure out what the inst of the argument would be,
    % assuming that the functor is the one given by the specified ConsId,
    % whose arity is 1.
    %
:- pred get_single_arg_inst(mer_inst::in, module_info::in, cons_id::in,
    mer_inst::out) is det.

get_single_arg_inst(defined_inst(InstName), ModuleInfo, ConsId, ArgInst) :-
    inst_lookup(ModuleInfo, InstName, Inst),
    get_single_arg_inst(Inst, ModuleInfo, ConsId, ArgInst).
get_single_arg_inst(not_reached, _, _, not_reached).
get_single_arg_inst(ground(Uniq, _PredInst), _, _, ground(Uniq, none)).
get_single_arg_inst(bound(_Uniq, List), _, ConsId, ArgInst) :-
    ( get_single_arg_inst_2(List, ConsId, ArgInst0) ->
        ArgInst = ArgInst0
    ;
        % The code is unreachable.
        ArgInst = not_reached
    ).
get_single_arg_inst(free, _, _, free).
get_single_arg_inst(free(_Type), _, _, free).   % XXX loses type info
get_single_arg_inst(any(Uniq), _, _, any(Uniq)).
get_single_arg_inst(abstract_inst(_, _), _, _, _) :-
    unexpected(this_file,
        "get_single_arg_inst: abstract insts not supported").
get_single_arg_inst(inst_var(_), _, _, _) :-
    unexpected(this_file, "get_single_arg_inst: inst_var").
get_single_arg_inst(constrained_inst_vars(_, Inst), ModuleInfo, ConsId,
        ArgInst) :-
    get_single_arg_inst(Inst, ModuleInfo, ConsId, ArgInst).

:- pred get_single_arg_inst_2(list(bound_inst)::in, cons_id::in, mer_inst::out)
    is semidet.

get_single_arg_inst_2([BoundInst | BoundInsts], ConsId, ArgInst) :-
    ( BoundInst = bound_functor(ConsId, [ArgInst0]) ->
        ArgInst = ArgInst0
    ;
        get_single_arg_inst_2(BoundInsts, ConsId, ArgInst)
    ).

%-----------------------------------------------------------------------------%

    % Given two lists of modes (inst mappings) of equal length, convert
    % them into a single list of inst pair mappings.
    %
modes_to_uni_modes(_ModuleInfo, [], [], []).
modes_to_uni_modes(_ModuleInfo, [], [_ | _], _) :-
    unexpected(this_file, "modes_to_uni_modes: length mismatch").
modes_to_uni_modes(_ModuleInfo, [_ | _], [], _) :-
    unexpected(this_file, "modes_to_uni_modes: length mismatch").
modes_to_uni_modes(ModuleInfo, [X | Xs], [Y | Ys], [A | As]) :-
    mode_get_insts(ModuleInfo, X, InitialX, FinalX),
    mode_get_insts(ModuleInfo, Y, InitialY, FinalY),
    A = ((InitialX - InitialY) -> (FinalX - FinalY)),
    modes_to_uni_modes(ModuleInfo, Xs, Ys, As).

%-----------------------------------------------------------------------------%

inst_lookup(ModuleInfo, InstName, Inst) :-
    (
        InstName = unify_inst(_, _, _, _),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_unify_insts(InstTable, UnifyInstTable),
        map.lookup(UnifyInstTable, InstName, MaybeInst),
        ( MaybeInst = inst_det_known(Inst0, _) ->
            Inst = Inst0
        ;
            Inst = defined_inst(InstName)
        )
    ;
        InstName = merge_inst(A, B),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_merge_insts(InstTable, MergeInstTable),
        map.lookup(MergeInstTable, A - B, MaybeInst),
        ( MaybeInst = inst_known(Inst0) ->
            Inst = Inst0
        ;
            Inst = defined_inst(InstName)
        )
    ;
        InstName = ground_inst(_, _, _, _),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_ground_insts(InstTable, GroundInstTable),
        map.lookup(GroundInstTable, InstName, MaybeInst),
        ( MaybeInst = inst_det_known(Inst0, _) ->
            Inst = Inst0
        ;
            Inst = defined_inst(InstName)
        )
    ;
        InstName = any_inst(_, _, _, _),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_any_insts(InstTable, AnyInstTable),
        map.lookup(AnyInstTable, InstName, MaybeInst),
        ( MaybeInst = inst_det_known(Inst0, _) ->
            Inst = Inst0
        ;
            Inst = defined_inst(InstName)
        )
    ;
        InstName = shared_inst(SharedInstName),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_shared_insts(InstTable, SharedInstTable),
        map.lookup(SharedInstTable, SharedInstName, MaybeInst),
        ( MaybeInst = inst_known(Inst0) ->
            Inst = Inst0
        ;
            Inst = defined_inst(InstName)
        )
    ;
        InstName = mostly_uniq_inst(NondetLiveInstName),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_mostly_uniq_insts(InstTable,
            NondetLiveInstTable),
        map.lookup(NondetLiveInstTable, NondetLiveInstName, MaybeInst),
        ( MaybeInst = inst_known(Inst0) ->
            Inst = Inst0
        ;
            Inst = defined_inst(InstName)
        )
    ;
        InstName = user_inst(Name, Args),
        module_info_get_inst_table(ModuleInfo, InstTable),
        inst_table_get_user_insts(InstTable, UserInstTable),
        user_inst_table_get_inst_defns(UserInstTable, InstDefns),
        list.length(Args, Arity),
        ( map.search(InstDefns, inst_id(Name, Arity), InstDefn) ->
            InstDefn = hlds_inst_defn(_VarSet, Params, InstBody, _C, _),
            inst_lookup_subst_args(InstBody, Params, Name, Args, Inst)
        ;
            Inst = abstract_inst(Name, Args)
        )
    ;
        InstName = typed_ground(Uniq, Type),
        map.init(Subst),
        propagate_type_into_inst(ModuleInfo, Subst, Type,
            ground(Uniq, none), Inst)
    ;
        InstName = typed_inst(Type, TypedInstName),
        inst_lookup(ModuleInfo, TypedInstName, Inst0),
        map.init(Subst),
        propagate_type_into_inst(ModuleInfo, Subst, Type, Inst0, Inst)
    ).

%-----------------------------------------------------------------------------%

    % Given corresponding lists of types and modes, produce a new
    % list of modes which includes the information provided by the
    % corresponding types.
    %
propagate_types_into_mode_list(_, [], [], []).
propagate_types_into_mode_list(ModuleInfo, [Type | Types],
        [Mode0 | Modes0], [Mode | Modes]) :-
    propagate_type_into_mode(ModuleInfo, Type, Mode0, Mode),
    propagate_types_into_mode_list(ModuleInfo, Types, Modes0, Modes).
propagate_types_into_mode_list(_, [], [_ | _], []) :-
    unexpected(this_file, "propagate_types_into_mode_list: length mismatch").
propagate_types_into_mode_list(_, [_ | _], [], []) :-
    unexpected(this_file, "propagate_types_into_mode_list: length mismatch").

propagate_types_into_inst_list(_, _, [], [], []).
propagate_types_into_inst_list(ModuleInfo, Subst, [Type | Types],
        [Inst0 | Insts0], [Inst | Insts]) :-
    propagate_type_into_inst(ModuleInfo, Subst, Type, Inst0, Inst),
    propagate_types_into_inst_list(ModuleInfo, Subst, Types, Insts0, Insts).
propagate_types_into_inst_list(_, _, [], [_ | _], []) :-
    unexpected(this_file, "propagate_types_into_inst_list: length mismatch").
propagate_types_into_inst_list(_, _, [_ | _], [], []) :-
    unexpected(this_file, "propagate_types_into_inst_list: length mismatch").

    % Given a type and a mode, produce a new mode that includes the
    % information provided by the type.
    %
:- pred propagate_type_into_mode(module_info::in, mer_type::in,
    mer_mode::in, mer_mode::out) is det.

propagate_type_into_mode(ModuleInfo, Type, Mode0, Mode) :-
    mode_get_insts(ModuleInfo, Mode0, InitialInst0, FinalInst0),
    map.init(Subst),
    propagate_type_into_inst_lazily(ModuleInfo, Subst, Type,
        InitialInst0, InitialInst),
    propagate_type_into_inst_lazily(ModuleInfo, Subst, Type,
        FinalInst0, FinalInst),
    Mode = (InitialInst -> FinalInst).

    % Given a type, an inst and a substitution for the type variables in
    % the type, produce a new inst that includes the information
    % provided by the type.
    %
    % There are three sorts of information added:
    %   1.  Module qualifiers.
    %   2.  The set of constructors in the type.
    %   3.  For higher-order function types
    %       (but not higher-order predicate types),
    %       the higher-order inst, i.e. the argument modes
    %       and the determinism.
    %
    % Currently #2 is not yet implemented, due to unsolved
    % efficiency problems.  (See the XXX's below.)
    %
    % There are two versions, an "eager" one and a "lazy" one.  In
    % general eager expansion is to be preferred, because the expansion
    % is done just once, whereas with lazy expansion the work will be
    % done N times.
    % However, for recursive insts we must use lazy expansion (otherwise
    % we would get infinite regress).  Also, usually many of the
    % imported procedures will not be called, so for the insts in
    % imported mode declarations N is often zero.
    %
:- pred propagate_type_into_inst(module_info::in, tsubst::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

:- pred propagate_type_into_inst_lazily(module_info::in, tsubst::in,
    mer_type::in, mer_inst::in, mer_inst::out) is det.

%   % XXX We ought to expand things eagerly here, using the commented
%   % out code below.  However, that causes efficiency problems,
%   % so for the moment it is disabled.
% propagate_type_into_inst(Type, Subst, ModuleInfo, Inst0, Inst) :-
%   apply_type_subst(Type0, Subst, Type),
%   (
%       type_constructors(Type, ModuleInfo, Constructors)
%   ->
%       propagate_ctor_info(Inst0, Type, Constructors, ModuleInfo, Inst)
%   ;
%       Inst = Inst0
%   ).

propagate_type_into_inst(ModuleInfo, Subst, Type, Inst0, Inst) :-
    propagate_ctor_info_lazily(ModuleInfo, Subst, Type, Inst0, Inst).

propagate_type_into_inst_lazily(ModuleInfo, Subst, Type, Inst0, Inst) :-
    propagate_ctor_info_lazily(ModuleInfo, Subst, Type, Inst0, Inst).

%-----------------------------------------------------------------------------%

:- pred propagate_ctor_info(module_info::in, mer_type::in,
    list(constructor)::in, mer_inst::in, mer_inst::out) is det.

propagate_ctor_info(ModuleInfo, Type, Constructors, Inst0, Inst) :-
    (
        Inst0 = any(_Uniq),
        Inst = Inst0            % XXX loses type info!
    ;
        Inst0 = free,
        % Inst = free(Type)
        Inst = free             % XXX temporary hack
    ;
        Inst0 = free(_),
        unexpected(this_file, "propagate_ctor_info: type info already present")
    ;
        Inst0 = bound(Uniq, BoundInsts0),
        propagate_ctor_info_2(ModuleInfo, Type, BoundInsts0, BoundInsts),
        (
            BoundInsts = [],
            Inst = not_reached
        ;
            BoundInsts = [_ | _],
            % XXX do we need to sort the BoundInsts?
            Inst = bound(Uniq, BoundInsts)
        )
    ;
        Inst0 = ground(Uniq, none),
        ( type_is_higher_order_details(Type, _Purity, function, _, ArgTypes) ->
            default_higher_order_func_inst(ModuleInfo, ArgTypes,
                HigherOrderInstInfo),
            Inst = ground(Uniq, higher_order(HigherOrderInstInfo))
        ;
            constructors_to_bound_insts(ModuleInfo, Uniq,
                Constructors, BoundInsts0),
            list.sort_and_remove_dups(BoundInsts0, BoundInsts),
            Inst = bound(Uniq, BoundInsts)
        )
    ;
        Inst0 = ground(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, Det),
        (
            type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
            list.same_length(ArgTypes, Modes0)
        ->
            propagate_types_into_mode_list(ModuleInfo, ArgTypes, Modes0, Modes)
        ;
            % The inst is not a valid inst for the type, so leave it alone.
            % This can only happen if the user has made a mistake.  A mode
            % error should hopefully be reported if anything tries to match
            % with the inst.
            Modes = Modes0
        ),
        PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
        Inst = ground(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(V, SubInst0),
        propagate_ctor_info(ModuleInfo, Type, Constructors, SubInst0, SubInst),
        Inst = constrained_inst_vars(V, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName),
        inst_lookup(ModuleInfo, InstName, NamedInst),
        propagate_ctor_info(ModuleInfo, Type, Constructors, NamedInst, Inst)
    ).

:- pred propagate_ctor_info_lazily(module_info::in, tsubst::in, mer_type::in,
    mer_inst::in, mer_inst::out) is det.

propagate_ctor_info_lazily(ModuleInfo, Subst, Type0, Inst0, Inst) :-
    (
        Inst0 = any(_Uniq),
        Inst = Inst0            % XXX loses type info!
    ;
        Inst0 = free,
        % Inst = free(Type0)
        Inst = free             % XXX temporary hack
    ;
        Inst0 = free(_),
        unexpected(this_file,
            "propagate_ctor_info_lazily: typeinfo already present")
    ;
        Inst0 = bound(Uniq, BoundInsts0),
        apply_type_subst(Type0, Subst, Type),
        propagate_ctor_info_2(ModuleInfo, Type, BoundInsts0, BoundInsts),
        (
            BoundInsts = [],
            Inst = not_reached
        ;
            BoundInsts = [_ | _],
            % XXX Do we need to sort the BoundInsts?
            Inst = bound(Uniq, BoundInsts)
        )
    ;
        Inst0 = ground(Uniq, none),
        apply_type_subst(Type0, Subst, Type),
        ( type_is_higher_order_details(Type, _, function, _, ArgTypes) ->
            default_higher_order_func_inst(ModuleInfo, ArgTypes,
                HigherOrderInstInfo),
            Inst = ground(Uniq, higher_order(HigherOrderInstInfo))
        ;
            % XXX The information added by this is not yet used, so it's
            % disabled since it unnecessarily complicates the insts.
            %
            % Inst = defined_inst(typed_ground(Uniq, Type))
            Inst = ground(Uniq, none)
        )
    ;
        Inst0 = ground(Uniq, higher_order(PredInstInfo0)),
        PredInstInfo0 = pred_inst_info(PredOrFunc, Modes0, Det),
        apply_type_subst(Type0, Subst, Type),
        (
            type_is_higher_order_details(Type, _, PredOrFunc, _, ArgTypes),
            list.same_length(ArgTypes, Modes0)
        ->
            propagate_types_into_mode_list(ModuleInfo, ArgTypes, Modes0, Modes)
        ;
            % The inst is not a valid inst for the type, so leave it alone.
            % This can only happen if the user has made a mistake. A mode error
            % should hopefully be reported if anything tries to match with the
            % inst.
            Modes = Modes0
        ),
        PredInstInfo = pred_inst_info(PredOrFunc, Modes, Det),
        Inst = ground(Uniq, higher_order(PredInstInfo))
    ;
        Inst0 = not_reached,
        Inst = Inst0
    ;
        Inst0 = inst_var(_),
        Inst = Inst0
    ;
        Inst0 = constrained_inst_vars(V, SubInst0),
        propagate_ctor_info_lazily(ModuleInfo, Subst, Type0,
            SubInst0, SubInst),
        Inst = constrained_inst_vars(V, SubInst)
    ;
        Inst0 = abstract_inst(_Name, _Args),
        Inst = Inst0                        % XXX loses info
    ;
        Inst0 = defined_inst(InstName0),
        apply_type_subst(Type0, Subst, Type),
        ( InstName0 = typed_inst(_, _) ->
            % If this happens, it means that we have already lazily propagated
            % type info into this inst. We want to avoid creating insts of
            % the form typed_inst(_, typed_inst(...)), because that would be
            % unnecessary, and could cause efficiency problems or perhaps
            % even infinite loops.
            InstName = InstName0
        ;
            InstName = typed_inst(Type, InstName0)
        ),
        Inst = defined_inst(InstName)
    ).

    %
    % If the user does not explicitly specify a higher-order inst
    % for a higher-order function type, it defaults to
    % `func(in, in, ..., in) = out is det',
    % i.e. all args input, return value output, and det.
    % This applies recursively to the arguments and return
    % value too.
    %
:- pred default_higher_order_func_inst(module_info::in, list(mer_type)::in,
    pred_inst_info::out) is det.

default_higher_order_func_inst(ModuleInfo, PredArgTypes, PredInstInfo) :-
    In = (ground(shared, none) -> ground(shared, none)),
    Out = (free -> ground(shared, none)),
    list.length(PredArgTypes, NumPredArgs),
    NumFuncArgs = NumPredArgs - 1,
    list.duplicate(NumFuncArgs, In, FuncArgModes),
    FuncRetMode = Out,
    list.append(FuncArgModes, [FuncRetMode], PredArgModes0),
    propagate_types_into_mode_list(ModuleInfo, PredArgTypes,
        PredArgModes0, PredArgModes),
    PredInstInfo = pred_inst_info(function, PredArgModes, detism_det).

constructors_to_bound_insts(ModuleInfo, Uniq, Constructors, BoundInsts) :-
    constructors_to_bound_insts_2(ModuleInfo, Uniq,
        Constructors, ground(Uniq, none), BoundInsts).

constructors_to_bound_any_insts(ModuleInfo, Uniq, Constructors, BoundInsts) :-
    constructors_to_bound_insts_2(ModuleInfo, Uniq,
        Constructors, any(Uniq), BoundInsts).

:- pred constructors_to_bound_insts_2(module_info::in, uniqueness::in,
    list(constructor)::in, mer_inst::in, list(bound_inst)::out) is det.

constructors_to_bound_insts_2(_, _, [], _, []).
constructors_to_bound_insts_2(ModuleInfo, Uniq, [Ctor | Ctors], ArgInst,
        [BoundInst | BoundInsts]) :-
    Ctor = ctor(_ExistQVars, _Constraints, Name, Args, _Ctxt),
    ctor_arg_list_to_inst_list(Args, ArgInst, Insts),
    list.length(Insts, Arity),
    BoundInst = bound_functor(cons(Name, Arity), Insts),
    constructors_to_bound_insts_2(ModuleInfo, Uniq, Ctors,
        ArgInst, BoundInsts).

:- pred ctor_arg_list_to_inst_list(list(constructor_arg)::in, mer_inst::in,
    list(mer_inst)::out) is det.

ctor_arg_list_to_inst_list([], _, []).
ctor_arg_list_to_inst_list([_ | Args], Inst, [Inst | Insts]) :-
    ctor_arg_list_to_inst_list(Args, Inst, Insts).

:- pred propagate_ctor_info_2(module_info::in, mer_type::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

propagate_ctor_info_2(ModuleInfo, Type, BoundInsts0, BoundInsts) :-
    (
        type_is_tuple(Type, TupleArgTypes)
    ->
        list.map(propagate_ctor_info_tuple(ModuleInfo, TupleArgTypes),
            BoundInsts0, BoundInsts)
    ;
        type_to_ctor_and_args(Type, TypeCtor, TypeArgs),
        TypeCtor = type_ctor(qualified(TypeModule, _), _),
        module_info_get_type_table(ModuleInfo, TypeTable),
        map.search(TypeTable, TypeCtor, TypeDefn),
        hlds_data.get_type_defn_tparams(TypeDefn, TypeParams),
        hlds_data.get_type_defn_body(TypeDefn, TypeBody),
        Constructors = TypeBody ^ du_type_ctors
    ->
        map.from_corresponding_lists(TypeParams, TypeArgs, ArgSubst),
        propagate_ctor_info_3(ModuleInfo, ArgSubst, TypeModule, Constructors,
            BoundInsts0, BoundInsts1),
        list.sort(BoundInsts1, BoundInsts)
    ;
        % Builtin types don't need processing.
        BoundInsts = BoundInsts0
    ).

:- pred propagate_ctor_info_tuple(module_info::in, list(mer_type)::in,
    bound_inst::in, bound_inst::out) is det.

propagate_ctor_info_tuple(ModuleInfo, TupleArgTypes, BoundInst0, BoundInst) :-
    BoundInst0 = bound_functor(Functor, ArgInsts0),
    (
        Functor = cons(unqualified("{}"), _),
        list.length(ArgInsts0, ArgInstsLen),
        list.length(TupleArgTypes, TupleArgTypesLen),
        ArgInstsLen = TupleArgTypesLen
    ->
        map.init(Subst),
        propagate_types_into_inst_list(ModuleInfo, Subst, TupleArgTypes,
            ArgInsts0, ArgInsts)
    ;
        % The bound_inst's arity does not match the
        % tuple's arity, so leave it alone. This can
        % only happen in a user defined bound_inst.
        % A mode error should be reported if anything
        % tries to match with the inst.
        ArgInsts = ArgInsts0
    ),
    BoundInst = bound_functor(Functor, ArgInsts).

:- pred propagate_ctor_info_3(module_info::in, tsubst::in,
    module_name::in, list(constructor)::in,
    list(bound_inst)::in, list(bound_inst)::out) is det.

propagate_ctor_info_3(_, _, _, _, [], []).
propagate_ctor_info_3(ModuleInfo, Subst, TypeModule, Constructors,
        [BoundInst0 | BoundInsts0], [BoundInst | BoundInsts]) :-
    BoundInst0 = bound_functor(ConsId0, ArgInsts0),
    ( ConsId0 = cons(unqualified(Name), Ar) ->
        ConsId = cons(qualified(TypeModule, Name), Ar)
    ;
        ConsId = ConsId0
    ),
    (
        ConsId = cons(ConsName, Arity),
        GetCons = (pred(Ctor::in) is semidet :-
                Ctor = ctor(_, _, ConsName, CtorArgs, _),
                list.length(CtorArgs, Arity)
            ),
        list.filter(GetCons, Constructors, [Constructor])
    ->
        Constructor = ctor(_ExistQVars, _Constraints, _Name, Args, _Ctxt),
        GetArgTypes = (pred(CtorArg::in, ArgType::out) is det :-
                ArgType = CtorArg ^ arg_type
            ),
        list.map(GetArgTypes, Args, ArgTypes),
        propagate_types_into_inst_list(ModuleInfo, Subst, ArgTypes,
            ArgInsts0, ArgInsts),
        BoundInst = bound_functor(ConsId, ArgInsts)
    ;
        % The cons_id is not a valid constructor for the type,
        % so leave it alone. This can only happen in a user defined
        % bound_inst. A mode error should be reported if anything
        % tries to match with the inst.
        BoundInst = bound_functor(ConsId, ArgInsts0)
    ),
    propagate_ctor_info_3(ModuleInfo, Subst, TypeModule,
        Constructors, BoundInsts0, BoundInsts).

:- pred apply_type_subst(mer_type::in, tsubst::in, mer_type::out) is det.

apply_type_subst(Type0, Subst, Type) :-
    % optimize common case
    ( map.is_empty(Subst) ->
        Type = Type0
    ;
        apply_subst_to_type(Subst, Type0, Type)
    ).

%-----------------------------------------------------------------------------%

:- pred inst_lookup_subst_args(hlds_inst_body::in, list(inst_var)::in,
    sym_name::in, list(mer_inst)::in, mer_inst::out) is det.

inst_lookup_subst_args(eqv_inst(Inst0), Params, _Name, Args, Inst) :-
    inst_substitute_arg_list(Params, Args, Inst0, Inst).
inst_lookup_subst_args(abstract_inst, _Params, Name, Args,
        abstract_inst(Name, Args)).

%-----------------------------------------------------------------------------%

    % mode_get_insts returns the initial instantiatedness and
    % the final instantiatedness for a given mode.
    %
mode_get_insts_semidet(_ModuleInfo, (InitialInst -> FinalInst),
        InitialInst, FinalInst).
mode_get_insts_semidet(ModuleInfo, user_defined_mode(Name, Args),
        Initial, Final) :-
    list.length(Args, Arity),
    module_info_get_mode_table(ModuleInfo, Modes),
    mode_table_get_mode_defns(Modes, ModeDefns),
    map.search(ModeDefns, mode_id(Name, Arity), HLDS_Mode),
    HLDS_Mode = hlds_mode_defn(_VarSet, Params, ModeDefn, _Context, _Status),
    ModeDefn = eqv_mode(Mode0),
    mode_substitute_arg_list(Mode0, Params, Args, Mode),
    mode_get_insts_semidet(ModuleInfo, Mode, Initial, Final).

mode_get_insts(ModuleInfo, Mode, InstA, InstB) :-
    ( mode_get_insts_semidet(ModuleInfo, Mode, InstA0, InstB0) ->
        InstA = InstA0,
        InstB = InstB0
    ;
        unexpected(this_file, "mode_get_insts_semidet failed")
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

    % Use the instmap deltas for all the atomic sub-goals to recompute
    % the instmap deltas for all the non-atomic sub-goals of a goal.
    % Used to ensure that the instmap deltas remain valid after
    % code has been re-arranged, e.g. by followcode.
    % After common.m has been run, it may be necessary to recompute
    % instmap deltas for atomic goals, since more outputs of calls
    % and deconstructions may become non-local (XXX does this require
    % rerunning mode analysis rather than just recompute_instmap_delta?).
    %
recompute_instmap_delta_proc(RecomputeAtomic, !ProcInfo, !ModuleInfo) :-
    proc_info_get_initial_instmap(!.ProcInfo, !.ModuleInfo, InstMap0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes),
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
    recompute_instmap_delta(RecomputeAtomic, Goal0, Goal,
        VarTypes, InstVarSet, InstMap0, !ModuleInfo),
    proc_info_set_goal(Goal, !ProcInfo).

recompute_instmap_delta(RecomputeAtomic, Goal0, Goal, VarTypes, InstVarSet,
        InstMap0, ModuleInfo0, ModuleInfo) :-
    RI0 = recompute_info(ModuleInfo0, InstVarSet),
    recompute_instmap_delta_1(RecomputeAtomic, Goal0, Goal, VarTypes,
        InstMap0, _, RI0, RI),
    ModuleInfo = RI ^ module_info.

:- pred recompute_instmap_delta_1(bool::in, hlds_goal::in, hlds_goal::out,
    vartypes::in, instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_1(RecomputeAtomic, Goal0 - GoalInfo0, Goal - GoalInfo,
        VarTypes, InstMap0, InstMapDelta, !RI) :-
    (
        RecomputeAtomic = no,
        goal_is_atomic(Goal0),
        Goal0 \= unify(_, rhs_lambda_goal(_, _, _, _, _, _, _, _), _, _, _)
        % Lambda expressions always need to be processed.
    ->
        Goal = Goal0,
        GoalInfo1 = GoalInfo0
    ;
        recompute_instmap_delta_2(RecomputeAtomic, Goal0, GoalInfo0,
            Goal, VarTypes, InstMap0, InstMapDelta0, !RI),
        goal_info_get_nonlocals(GoalInfo0, NonLocals),
        instmap_delta_restrict(NonLocals, InstMapDelta0, InstMapDelta1),
        goal_info_set_instmap_delta(InstMapDelta1, GoalInfo0, GoalInfo1)
    ),

    % If the initial instmap is unreachable so is the final instmap.
    ( instmap.is_unreachable(InstMap0) ->
        instmap_delta_init_unreachable(UnreachableInstMapDelta),
        goal_info_set_instmap_delta(UnreachableInstMapDelta,
            GoalInfo1, GoalInfo)
    ;
        GoalInfo = GoalInfo1
    ),
    goal_info_get_instmap_delta(GoalInfo, InstMapDelta).

:- type recompute_info
    --->    recompute_info(
                module_info :: module_info,
                inst_varset :: inst_varset
            ).

    % update_module_info(P, R, RI0, RI) will call predicate P, passing it
    % the module_info from RI0 and placing the output module_info in RI.
    % The output of P's first argument is returned in R.
    %
:- pred update_module_info(
    pred(T, module_info, module_info)::in(pred(out, in, out) is det),
    T::out, recompute_info::in, recompute_info::out) is det.

update_module_info(P, R, !RI) :-
    ModuleInfo0 = !.RI ^ module_info,
    P(R, ModuleInfo0, ModuleInfo),
    !:RI = !.RI ^ module_info := ModuleInfo.

:- pred recompute_instmap_delta_2(bool::in, hlds_goal_expr::in,
    hlds_goal_info::in, hlds_goal_expr::out, vartypes::in, instmap::in,
    instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_2(Atomic, switch(Var, Det, Cases0), GoalInfo,
        switch(Var, Det, Cases), VarTypes, InstMap, InstMapDelta, !RI) :-
    ( goal_info_has_feature(GoalInfo, feature_mode_check_clauses_goal) ->
        Cases = Cases0,
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta)
    ;
        goal_info_get_nonlocals(GoalInfo, NonLocals),
        recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases,
            VarTypes, InstMap, NonLocals, InstMapDelta, !RI)
    ).

recompute_instmap_delta_2(Atomic, conj(ConjType, Goals0), _GoalInfo,
        conj(ConjType, Goals), VarTypes, InstMap, InstMapDelta, !RI) :-
    recompute_instmap_delta_conj(Atomic, Goals0, Goals,
        VarTypes, InstMap, InstMapDelta, !RI).

recompute_instmap_delta_2(Atomic, disj(Goals0), GoalInfo, disj(Goals),
        VarTypes, InstMap, InstMapDelta, !RI) :-
    ( goal_info_has_feature(GoalInfo, feature_mode_check_clauses_goal) ->
        Goals = Goals0,
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta)
    ;
        goal_info_get_nonlocals(GoalInfo, NonLocals),
        recompute_instmap_delta_disj(Atomic, Goals0, Goals,
            VarTypes, InstMap, NonLocals, InstMapDelta, !RI)
    ).

recompute_instmap_delta_2(Atomic, negation(Goal0), _, negation(Goal),
        VarTypes, InstMap, InstMapDelta, !RI) :-
    instmap_delta_init_reachable(InstMapDelta),
    recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap, _,
        !RI).

recompute_instmap_delta_2(Atomic, if_then_else(Vars, Cond0, Then0, Else0),
        GoalInfo, if_then_else(Vars, Cond, Then, Else), VarTypes,
        InstMap0, InstMapDelta, !RI) :-
    recompute_instmap_delta_1(Atomic, Cond0, Cond, VarTypes, InstMap0,
        InstMapDeltaCond, !RI),
    instmap.apply_instmap_delta(InstMap0, InstMapDeltaCond, InstMapCond),
    recompute_instmap_delta_1(Atomic, Then0, Then, VarTypes, InstMapCond,
        InstMapDeltaThen, !RI),
    recompute_instmap_delta_1(Atomic, Else0, Else, VarTypes, InstMap0,
        InstMapDeltaElse, !RI),
    instmap_delta_apply_instmap_delta(InstMapDeltaCond, InstMapDeltaThen,
        test_size, InstMapDeltaCondThen),
    goal_info_get_nonlocals(GoalInfo, NonLocals),
    update_module_info(
        merge_instmap_delta(InstMap0, NonLocals,
            VarTypes, InstMapDeltaElse, InstMapDeltaCondThen),
        InstMapDelta, !RI).

recompute_instmap_delta_2(Atomic, scope(Reason, Goal0), _,
        scope(Reason, Goal), VarTypes, InstMap, InstMapDelta, !RI) :-
    recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap,
        InstMapDelta, !RI).

recompute_instmap_delta_2(_, generic_call(Details, Vars, Modes, Detism), _,
        generic_call(Details, Vars, Modes, Detism),
        _VarTypes, _InstMap, InstMapDelta, !RI) :-
    ModuleInfo = !.RI ^ module_info,
    instmap_delta_from_mode_list(Vars, Modes, ModuleInfo, InstMapDelta).

recompute_instmap_delta_2(_, plain_call(PredId, ProcId, Args, BI, UC, Name), _,
        plain_call(PredId, ProcId, Args, BI, UC, Name), VarTypes,
        InstMap, InstMapDelta, !RI) :-
    recompute_instmap_delta_call(PredId, ProcId,
        Args, VarTypes, InstMap, InstMapDelta, !RI).

recompute_instmap_delta_2(Atomic, unify(LHS, RHS0, UniMode0, Uni, Context),
        GoalInfo, unify(LHS, RHS, UniMode, Uni, Context), VarTypes,
        InstMap0, InstMapDelta, !RI) :-
    (
        RHS0 = rhs_lambda_goal(Purity, PorF, EvalMethod, NonLocals,
            LambdaVars, Modes, Det, Goal0)
    ->
        ModuleInfo0 = !.RI ^ module_info,
        instmap.pre_lambda_update(ModuleInfo0, LambdaVars, Modes,
            InstMap0, InstMap),
        recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes,
            InstMap, _, !RI),
        RHS = rhs_lambda_goal(Purity, PorF, EvalMethod, NonLocals,
            LambdaVars, Modes, Det, Goal)
    ;
        RHS = RHS0
    ),
    (
        Atomic = yes,
        recompute_instmap_delta_unify(Uni, UniMode0, UniMode,
            GoalInfo, InstMap0, InstMapDelta, !.RI)
    ;
        Atomic = no,
        UniMode = UniMode0,
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta)
    ).

recompute_instmap_delta_2(_,
        call_foreign_proc(Attr, PredId, ProcId, Args, ExtraArgs, MTRC, Impl),
        GoalInfo,
        call_foreign_proc(Attr, PredId, ProcId, Args, ExtraArgs, MTRC, Impl),
        VarTypes, InstMap, InstMapDelta, !RI) :-
    ArgVars = list.map(foreign_arg_var, Args),
    recompute_instmap_delta_call(PredId, ProcId,
        ArgVars, VarTypes, InstMap, InstMapDelta0, !RI),
    (
        ExtraArgs = [],
        InstMapDelta = InstMapDelta0
    ;
        ExtraArgs = [_ | _],
        goal_info_get_instmap_delta(GoalInfo, OldInstMapDelta),
        ExtraArgVars = list.map(foreign_arg_var, ExtraArgs),
        instmap_delta_restrict(set.list_to_set(ExtraArgVars),
            OldInstMapDelta, ExtraArgsInstMapDelta),
        instmap_delta_apply_instmap_delta(InstMapDelta0,
            ExtraArgsInstMapDelta, large_base, InstMapDelta)
    ).

recompute_instmap_delta_2(_, shorthand(_), _, _, _, _, _, !RI) :-
    % these should have been expanded out by now
    unexpected(this_file,
        "recompute_instmap_delta_2: unexpected shorthand").

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_conj(bool::in, list(hlds_goal)::in,
    list(hlds_goal)::out, vartypes::in, instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_conj(_, [], [], _, _, InstMapDelta, !RI) :-
    instmap_delta_init_reachable(InstMapDelta).
recompute_instmap_delta_conj(Atomic, [Goal0 | Goals0], [Goal | Goals],
        VarTypes, InstMap0, InstMapDelta, !RI) :-
    recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap0,
        InstMapDelta0, !RI),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta0, InstMap1),
    recompute_instmap_delta_conj(Atomic, Goals0, Goals, VarTypes, InstMap1,
        InstMapDelta1, !RI),
    instmap_delta_apply_instmap_delta(InstMapDelta0, InstMapDelta1,
        large_overlay, InstMapDelta).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_disj(bool::in, list(hlds_goal)::in,
    list(hlds_goal)::out, vartypes::in, instmap::in, set(prog_var)::in,
    instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disj(Atomic, Goals0, Goals, VarTypes, InstMap,
        NonLocals, InstMapDelta, !RI) :-
    recompute_instmap_delta_disj_2(Atomic, Goals0, Goals, VarTypes, InstMap,
        NonLocals, InstMapDeltas, !RI),
    (
        InstMapDeltas = [],
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        InstMapDeltas = [_ | _],
        update_module_info(
            merge_instmap_deltas(InstMap, NonLocals, VarTypes, InstMapDeltas),
            InstMapDelta, !RI)
    ).

:- pred recompute_instmap_delta_disj_2(bool::in, list(hlds_goal)::in,
    list(hlds_goal)::out, vartypes::in, instmap::in, set(prog_var)::in,
    list(instmap_delta)::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_disj_2(_Atomic, [], [],
        _VarTypes, _InstMap, _NonLocals, [], !RI).
recompute_instmap_delta_disj_2(Atomic, [Goal0 | Goals0], [Goal | Goals],
        VarTypes, InstMap, NonLocals, [InstMapDelta | InstMapDeltas], !RI) :-
    recompute_instmap_delta_1(Atomic, Goal0, Goal,
        VarTypes, InstMap, InstMapDelta, !RI),
    recompute_instmap_delta_disj_2(Atomic, Goals0, Goals,
        VarTypes, InstMap, NonLocals, InstMapDeltas, !RI).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_cases(bool::in, prog_var::in, list(case)::in,
    list(case)::out, vartypes::in, instmap::in, set(prog_var)::in,
    instmap_delta::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_cases(Atomic, Var, Cases0, Cases, VarTypes,
        InstMap0, NonLocals, InstMapDelta, !RI) :-
    recompute_instmap_delta_cases_2(Atomic, Var, Cases0, Cases, VarTypes,
        InstMap0, NonLocals, InstMapDeltas, !RI),
    (
        InstMapDeltas = [],
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        InstMapDeltas = [_ | _],
        update_module_info(
            merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMapDeltas),
            InstMapDelta, !RI)
    ).

:- pred recompute_instmap_delta_cases_2(bool::in, prog_var::in, list(case)::in,
    list(case)::out, vartypes::in, instmap::in, set(prog_var)::in,
    list(instmap_delta)::out, recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_cases_2(_Atomic, _Var, [], [],
        _VarTypes, _InstMap, _NonLocals, [], !RI).
recompute_instmap_delta_cases_2(Atomic, Var, [Case0 | Cases0], [Case | Cases],
        VarTypes, InstMap0, NonLocals, [InstMapDelta | InstMapDeltas], !RI) :-
    Case0 = case(Functor, Goal0),
    map.lookup(VarTypes, Var, Type),
    update_module_info(instmap.bind_var_to_functor(Var, Type, Functor,
        InstMap0), InstMap1, !RI),
    recompute_instmap_delta_1(Atomic, Goal0, Goal, VarTypes, InstMap1,
        InstMapDelta0, !RI),
    update_module_info(instmap_delta_bind_var_to_functor(Var, Type,
        Functor, InstMap0, InstMapDelta0), InstMapDelta, !RI),
    Case = case(Functor, Goal),
    recompute_instmap_delta_cases_2(Atomic, Var, Cases0, Cases,
        VarTypes, InstMap0, NonLocals, InstMapDeltas, !RI).

%-----------------------------------------------------------------------------%

:- pred recompute_instmap_delta_call(pred_id::in, proc_id::in,
    list(prog_var)::in, vartypes::in, instmap::in, instmap_delta::out,
    recompute_info::in, recompute_info::out) is det.

recompute_instmap_delta_call(PredId, ProcId, Args, VarTypes, InstMap,
        InstMapDelta, !RI) :-
    ModuleInfo = !.RI ^ module_info,
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _, ProcInfo),
    proc_info_interface_determinism(ProcInfo, Detism),
    ( determinism_components(Detism, _, at_most_zero) ->
        instmap_delta_init_unreachable(InstMapDelta)
    ;
        proc_info_get_argmodes(ProcInfo, ArgModes0),
        proc_info_get_inst_varset(ProcInfo, ProcInstVarSet),
        InstVarSet = !.RI ^ inst_varset,
        rename_apart_inst_vars(InstVarSet, ProcInstVarSet,
            ArgModes0, ArgModes1),
        mode_list_get_initial_insts(ModuleInfo, ArgModes1, InitialInsts),

        % Compute the inst_var substitution from the initial insts
        % of the called procedure and the insts of the argument variables.
        map.init(InstVarSub0),
        update_module_info(compute_inst_var_sub(Args, VarTypes, InstMap,
            InitialInsts, InstVarSub0), InstVarSub, !RI),

        % Apply the inst_var substitution to the argument modes.
        mode_list_apply_substitution(InstVarSub, ArgModes1, ArgModes2),

        % Calculate the final insts of the argument variables from their
        % initial insts and the final insts of the called procedure
        % (with inst_var substitutions applied).
        update_module_info(
            recompute_instmap_delta_call_2(Args, InstMap, ArgModes2),
            ArgModes, !RI),
        instmap_delta_from_mode_list(Args, ArgModes, ModuleInfo, InstMapDelta)
    ).

:- pred compute_inst_var_sub(list(prog_var)::in, vartypes::in, instmap::in,
    list(mer_inst)::in, inst_var_sub::in, inst_var_sub::out,
    module_info::in, module_info::out) is det.

compute_inst_var_sub([], _, _, [], !Sub, !ModuleInfo).
compute_inst_var_sub([_ | _], _, _, [], !Sub, !ModuleInfo) :-
    unexpected(this_file, "compute_inst_var_sub").
compute_inst_var_sub([], _, _, [_ | _], !Sub, !ModuleInfo) :-
    unexpected(this_file, "compute_inst_var_sub").
compute_inst_var_sub([Arg | Args], VarTypes, InstMap, [Inst | Insts],
        !Sub, !ModuleInfo) :-
    % This is similar to modecheck_var_has_inst.
    SaveModuleInfo = !.ModuleInfo,
    SaveSub = !.Sub,
    ( instmap.is_reachable(InstMap) ->
        instmap.lookup_var(InstMap, Arg, ArgInst),
        map.lookup(VarTypes, Arg, Type),
        ( inst_matches_initial(ArgInst, Inst, Type, !ModuleInfo, !Sub) ->
            true
        ;
            % error("compute_inst_var_sub: " ++
            %   ++ "inst_matches_initial failed")
            % XXX  We shouldn't ever get here, but unfortunately
            % the mode system currently has several problems (most
            % noticeably lack of alias tracking for unique modes)
            % which mean inst_matches_initial can sometimes fail
            % here.
            !:ModuleInfo = SaveModuleInfo,
            !:Sub = SaveSub
        )
    ;
        true
    ),
    compute_inst_var_sub(Args, VarTypes, InstMap, Insts, !Sub, !ModuleInfo).

:- pred recompute_instmap_delta_call_2(list(prog_var)::in, instmap::in,
    list(mer_mode)::in, list(mer_mode)::out, module_info::in, module_info::out)
    is det.

recompute_instmap_delta_call_2([], _, [], [], !ModuleInfo).
recompute_instmap_delta_call_2([_ | _], _, [], _, !ModuleInfo) :-
    unexpected(this_file, "recompute_instmap_delta_call_2").
recompute_instmap_delta_call_2([], _, [_ | _], _, !ModuleInfo) :-
    unexpected(this_file, "recompute_instmap_delta_call_2").
recompute_instmap_delta_call_2([Arg | Args], InstMap, [Mode0 | Modes0],
        [Mode | Modes], !ModuleInfo) :-
    % This is similar to modecheck_set_var_inst.
    ( instmap.is_reachable(InstMap) ->
        instmap.lookup_var(InstMap, Arg, ArgInst0),
        mode_get_insts(!.ModuleInfo, Mode0, _, FinalInst),
        (
            abstractly_unify_inst(dead, ArgInst0, FinalInst,
                fake_unify, UnifyInst, _, !ModuleInfo)
        ->
            Mode = (ArgInst0 -> UnifyInst)
        ;
            unexpected(this_file,
                "recompute_instmap_delta_call_2: unify_inst failed")
        )
    ;
        Mode = (not_reached -> not_reached)
    ),
    recompute_instmap_delta_call_2(Args, InstMap, Modes0, Modes,
        !ModuleInfo).

:- pred recompute_instmap_delta_unify(unification::in, unify_mode::in,
    unify_mode::out, hlds_goal_info::in, instmap::in, instmap_delta::out,
    recompute_info::in) is det.

recompute_instmap_delta_unify(Uni, UniMode0, UniMode, GoalInfo,
        InstMap, InstMapDelta, RI) :-
    % Deconstructions are the only types of unifications that can require
    % updating of the instmap_delta after simplify.m has been run.
    ModuleInfo = RI ^ module_info,
    (
        Uni = deconstruct(Var, _ConsId, Vars, UniModes, _, _CanCGC)
    ->
        % Get the final inst of the deconstructed var, which will be the same
        % as in the old instmap.
        %
        goal_info_get_instmap_delta(GoalInfo, OldInstMapDelta),
        instmap.lookup_var(InstMap, Var, InitialInst),
        ( instmap_delta_search_var(OldInstMapDelta, Var, FinalInst1) ->
            % XXX we need to merge the information in InitialInst
            % and FinalInst1. In puzzle_detism_bug, InitialInst
            % has a var bound to one function symbol (james), while
            % FinalInst1 has it bound to another (katherine).
            % The correct final inst is thus `unreachable', but
            % we don't return that.
            FinalInst = FinalInst1
        ;
            % It wasn't in the instmap_delta, so the inst didn't
            % change.
            FinalInst = InitialInst
        ),
        UniModeToRhsMode = (pred(UMode::in, Mode::out) is det :-
                UMode = ((_ - Inst0) -> (_ - Inst)),
                Mode = (Inst0 -> Inst)
            ),
        list.map(UniModeToRhsMode, UniModes, Modes),
        instmap_delta_from_mode_list([Var | Vars],
            [(InitialInst -> FinalInst) |  Modes],
            ModuleInfo, InstMapDelta),
        UniMode = UniMode0
    ;
        goal_info_get_instmap_delta(GoalInfo, InstMapDelta),
        UniMode = UniMode0
    ).

%-----------------------------------------------------------------------------%

    % Arguments with final inst `clobbered' are dead, any
    % others are assumed to be live.
    %
get_arg_lives(_, [], []).
get_arg_lives(ModuleInfo, [Mode | Modes], [IsLive | IsLives]) :-
    mode_get_insts(ModuleInfo, Mode, _InitialInst, FinalInst),
    ( inst_is_clobbered(ModuleInfo, FinalInst) ->
        IsLive = dead
    ;
        IsLive = live
    ),
    get_arg_lives(ModuleInfo, Modes, IsLives).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

normalise_insts(_, [], [], []).
normalise_insts(_, [], [_ | _], _) :-
    unexpected(this_file, "normalise_insts: length mismatch").
normalise_insts(_, [_ | _], [], _) :-
    unexpected(this_file, "normalise_insts: length mismatch").
normalise_insts(ModuleInfo, [Type | Types],
        [Inst0 | Insts0], [Inst | Insts]) :-
    normalise_inst(ModuleInfo, Type, Inst0, Inst),
    normalise_insts(ModuleInfo, Types, Insts0, Insts).

    % This is a bit of a hack.
    % The aim is to avoid non-termination due to the creation
    % of ever-expanding insts.
    % XXX should also normalise partially instantiated insts.
    %
normalise_inst(ModuleInfo, Type, Inst0, NormalisedInst) :-
    inst_expand(ModuleInfo, Inst0, Inst),
    ( Inst = bound(_, _) ->
        (
            inst_is_ground(ModuleInfo, Inst),
            inst_is_unique(ModuleInfo, Inst),
            % don't infer unique modes for introduced type_infos
            % arguments, because that leads to an increase
            % in the number of inferred modes without any benefit
            \+ is_introduced_type_info_type(Type),
            \+ inst_contains_nonstandard_func_mode(ModuleInfo, Inst)
        ->
            NormalisedInst = ground(unique, none)
        ;
            inst_is_ground(ModuleInfo, Inst),
            inst_is_mostly_unique(ModuleInfo, Inst),
            % don't infer unique modes for introduced type_infos
            % arguments, because that leads to an increase
            % in the number of inferred modes without any benefit
            \+ is_introduced_type_info_type(Type),
            \+ inst_contains_nonstandard_func_mode(ModuleInfo, Inst)
        ->
            NormalisedInst = ground(mostly_unique, none)
        ;
            inst_is_ground(ModuleInfo, Inst),
            \+ inst_is_clobbered(ModuleInfo, Inst),
            \+ inst_contains_nonstandard_func_mode(ModuleInfo, Inst)
        ->
            NormalisedInst = ground(shared, none)
        ;
            % XXX need to limit the potential size of insts
            % here in order to avoid infinite loops in
            % mode inference
            NormalisedInst = Inst
        )
    ;
        NormalisedInst = Inst
    ).

%-----------------------------------------------------------------------------%

fixup_switch_var(Var, InstMap0, InstMap, Goal0, Goal) :-
    Goal0 = GoalExpr - GoalInfo0,
    goal_info_get_instmap_delta(GoalInfo0, InstMapDelta0),
    instmap.lookup_var(InstMap0, Var, Inst0),
    instmap.lookup_var(InstMap, Var, Inst),
    ( Inst = Inst0 ->
        GoalInfo = GoalInfo0
    ;
        instmap_delta_set(Var, Inst, InstMapDelta0, InstMapDelta),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo)
    ),
    Goal = GoalExpr - GoalInfo.

%-----------------------------------------------------------------------------%

partition_args(_, [], [_ | _], _, _) :-
    unexpected(this_file, "partition_args").
partition_args(_, [_ | _], [], _, _) :-
    unexpected(this_file, "partition_args").
partition_args(_, [], [], [], []).
partition_args(ModuleInfo, [ArgMode | ArgModes], [Arg | Args],
        !:InputArgs, !:OutputArgs) :-
    partition_args(ModuleInfo, ArgModes, Args, !:InputArgs, !:OutputArgs),
    ( mode_is_input(ModuleInfo, ArgMode) ->
        !:InputArgs = [Arg | !.InputArgs]
    ;
        !:OutputArgs = [Arg | !.OutputArgs]
    ).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "mode_util.m".

%-----------------------------------------------------------------------------%
