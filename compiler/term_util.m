%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1997-2007, 2010-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: term_util.m.
% Main author: crs.
%
% This module:
%
% - defines the types used by termination analysis
% - defines some utility predicates
%
%---------------------------------------------------------------------------%

:- module transform_hlds.term_util.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module transform_hlds.term_errors.
:- import_module transform_hlds.term_norm.

:- import_module bag.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module unit.

%---------------------------------------------------------------------------%
%
% The `arg_size_info' and `termination_info' structures.
%

% The types `arg_size_info' and `termination_info' hold information about
% procedures which is used for termination analysis. These types are stored
% as fields in the HLDS proc_info. For intermodule analysis, the information
% is written out as `pragma termination_info(...)' declarations in the `.opt'
% and `.trans_opt' files. The module prog_data.m defines types similar to
% these two (but without the `list(term_error)') which are used when parsing
% `termination_info' pragmas.

    % The arg size info defines an upper bound on the difference between the
    % sizes of the output arguments of a procedure and the sizes of the input
    % arguments:
    %
    % |input arguments| + constant >= |output arguments|
    %
    % where | | represents a semilinear norm.
    %
:- type arg_size_info == generic_arg_size_info(list(term_error)).

:- type termination_info ==
    generic_termination_info(unit, list(term_error)).

    % The type `used_args' holds a mapping which specifies for each procedure
    % which of its arguments are used.
    %
:- type used_args == map(pred_proc_id, list(bool)).

:- type pass_info
    --->    pass_info(
                functor_info,
                int,        % Max number of errors to gather.
                int         % Max number of paths to analyze.
            ).

%---------------------------------------------------------------------------%

    % This predicate partitions the arguments of a call into a list of input
    % variables and a list of output variables.
    %
:- pred partition_call_args(module_info::in, list(mer_mode)::in,
    list(prog_var)::in, bag(prog_var)::out, bag(prog_var)::out) is det.

    % Given a list of variables from a unification, this predicate divides the
    % list into a bag of input variables, and a bag of output variables.
    %
:- pred split_unification_vars(module_info::in,
    list(prog_var)::in, list(unify_mode)::in,
    bag(prog_var)::out, bag(prog_var)::out) is det.

%---------------------------------------------------------------------------%

    % Used to create lists of boolean values, which are used for used_args.
    % make_bool_list(HeadVars, BoolIn, BoolOut) creates a bool list which is
    % (length(HeadVars) - length(BoolIn)) `no' followed by BoolIn. This is
    % used to set the used args for compiler generated predicates. The no's
    % at the start are because the Type infos are not used. length(BoolIn)
    % should equal the arity of the predicate, and the difference in length
    % between the arity of the procedure and the arity of the predicate is
    % the number of typeinfos.
    %
:- pred make_bool_list(list(_T)::in, list(bool)::in, list(bool)::out) is det.

%---------------------%

    % Removes variables from the InVarBag that are not used in the call.
    % remove_unused_args(InVarBag0, VarList, BoolList, InVarBag) VarList and
    % BoolList are corresponding lists. Any variable in VarList that has a
    % `no' in the corresponding place in the BoolList is removed from
    % InVarBag.
    %
:- pred remove_unused_args(bag(prog_var)::in, list(prog_var)::in,
    list(bool)::in, bag(prog_var)::out) is det.

%---------------------%

    % Succeeds if one or more variables in the list are higher order.
    %
:- pred horder_vars(list(prog_var)::in, vartypes::in) is semidet.

%---------------------------------------------------------------------------%

:- pred lookup_proc_termination_info(module_info::in, pred_proc_id::in,
    maybe(termination_info)::out) is det.

:- pred lookup_proc_arg_size_info(module_info::in, pred_proc_id::in,
    maybe(arg_size_info)::out) is det.

%---------------------%

    % Succeeds if the termination status of a procedure is known.
    %
:- pred is_termination_known(module_info::in, pred_proc_id::in) is semidet.

    % pred_proc_id_terminates(ModuleInfo, PPId):
    %
    % Succeeds iff the procedure given by 'PPId' has been proven to terminate.
    %
:- pred pred_proc_id_terminates(module_info::in, pred_proc_id::in) is semidet.

%---------------------%

    % Succeed if all arguments of the given procedure of the given predicate
    % are either input or zero size.
    %
:- pred all_args_input_or_zero_size(module_info::in, pred_info::in,
    proc_info::in) is semidet.

%---------------------%

    % This predicate sets the argument size info of a given a list of
    % procedures.
    %
:- pred set_pred_proc_ids_arg_size_info(list(pred_proc_id)::in,
    arg_size_info::in, module_info::in, module_info::out) is det.

    % This predicate sets the termination info of a given a list of
    % procedures.
    %
:- pred set_pred_proc_ids_termination_info(list(pred_proc_id)::in,
    termination_info::in, module_info::in, module_info::out) is det.

%---------------------%

    % Convert a pragma_termination_info into a termination_info, by adding the
    % appropriate context.
    %
:- pred add_context_to_termination_info(maybe(pragma_termination_info)::in,
    prog_context::in, maybe(termination_info)::out) is det.

    % Convert a pragma_arg_size_info into a arg_size_info, by adding the
    % appropriate context.
    %
:- pred add_context_to_arg_size_info(maybe(pragma_arg_size_info)::in,
    prog_context::in, maybe(arg_size_info)::out) is det.

%---------------------------------------------------------------------------%

:- pred get_context_from_scc(module_info::in, scc::in, prog_context::out)
    is det.

%---------------------------------------------------------------------------%

    % Succeeds if the foreign proc attributes imply that a procedure is
    % terminating.
    %
:- pred attributes_imply_termination(pragma_foreign_proc_attributes::in)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.mode_util.
:- import_module parse_tree.prog_type.

:- import_module require.
:- import_module set.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

partition_call_args(ModuleInfo, ArgModes, Args, InVarsBag, OutVarsBag) :-
    partition_call_args_2(ModuleInfo, ArgModes, Args, InVars, OutVars),
    bag.from_list(InVars, InVarsBag),
    bag.from_list(OutVars, OutVarsBag).

:- pred partition_call_args_2(module_info::in, list(mer_mode)::in,
    list(prog_var)::in, list(prog_var)::out, list(prog_var)::out) is det.

partition_call_args_2(_, [], [], [], []).
partition_call_args_2(_, [], [_ | _], _, _) :-
    unexpected($pred, "unmatched variables").
partition_call_args_2(_, [_ | _], [], _, _) :-
    unexpected($pred, "unmatched variables").
partition_call_args_2(ModuleInfo, [ArgMode | ArgModes], [Arg | Args],
        InputArgs, OutputArgs) :-
    partition_call_args_2(ModuleInfo, ArgModes, Args,
        InputArgs1, OutputArgs1),
    ( if mode_is_input(ModuleInfo, ArgMode) then
        InputArgs = [Arg | InputArgs1],
        OutputArgs = OutputArgs1
    else if mode_is_output(ModuleInfo, ArgMode) then
        InputArgs = InputArgs1,
        OutputArgs = [Arg | OutputArgs1]
    else
        InputArgs = InputArgs1,
        OutputArgs = OutputArgs1
    ).

    % For these next two predicates (split_unification_vars and
    % partition_call_args) there is a problem of what needs to be done for
    % partially instantiated data structures. The correct answer is that the
    % system should use a norm such that the size of the uninstantiated parts
    % of a partially instantiated structure have no effect on the size of the
    % data structure according to the norm. For example when finding the size
    % of a list-skeleton, list-length norm should be used. Therefore, the
    % size of any term must be given by:
    %
    % sizeof(term) = constant + sum of the size of each
    %           (possibly partly) instantiated subterm.
    %
    % It is probably easiest to implement this by modifying term_weights.
    % The current implementation does not correctly handle partially
    % instantiated data structures.
    %
split_unification_vars(_, [], [], Vars, Vars) :-
    bag.init(Vars).
split_unification_vars(_, [], [_ | _], _, _) :-
    unexpected($pred, "unmatched variables").
split_unification_vars(_, [_ | _], [], _, _) :-
    unexpected($pred, "unmatched variables").
split_unification_vars(ModuleInfo, [Arg | Args], [ArgMode | ArgModes],
        InVars, OutVars):-
    split_unification_vars(ModuleInfo, Args, ArgModes, InVars0, OutVars0),
    ArgMode = unify_modes_li_lf_ri_rf(_, _, ArgInit, ArgFinal),
    ( if
        inst_is_bound(ModuleInfo, ArgInit)
    then
        % Variable is an input variable
        bag.insert(Arg, InVars0, InVars),
        OutVars = OutVars0
    else if
        inst_is_free(ModuleInfo, ArgInit),
        inst_is_bound(ModuleInfo, ArgFinal)
    then
        % Variable is an output variable
        InVars = InVars0,
        bag.insert(Arg, OutVars0, OutVars)
    else
        InVars = InVars0,
        OutVars = OutVars0
    ).

%---------------------------------------------------------------------------%

make_bool_list(HeadVars0, Bools, Out) :-
    list.length(Bools, Arity),
    ( if list.drop(Arity, HeadVars0, HeadVars1) then
        HeadVars = HeadVars1
    else
        unexpected($pred, "unmatched variables")
    ),
    make_bool_list_2(HeadVars, Bools, Out).

:- pred make_bool_list_2(list(_T)::in, list(bool)::in, list(bool)::out) is det.

make_bool_list_2([], Bools, Bools).
make_bool_list_2([ _ | Vars ], Bools, [no | Out]) :-
    make_bool_list_2(Vars, Bools, Out).

%---------------------------------------------------------------------------%

remove_unused_args(Vars, [], [], Vars).
remove_unused_args(Vars, [], [_X | _Xs], Vars) :-
    unexpected($pred, "unmatched variables").
remove_unused_args(Vars, [_X | _Xs], [], Vars) :-
    unexpected($pred, "unmatched variables").
remove_unused_args(Vars0, [ Arg | Args ], [ UsedVar | UsedVars ], Vars) :-
    (
        % The variable is used, so leave it.
        UsedVar = yes,
        remove_unused_args(Vars0, Args, UsedVars, Vars)
    ;
        % The variable is not used in producing output vars, so don't include
        % it as an input variable.
        UsedVar = no,
        bag.delete(Arg, Vars0, Vars1),
        remove_unused_args(Vars1, Args, UsedVars, Vars)
    ).

%---------------------------------------------------------------------------%

horder_vars([Arg | Args], VarType) :-
    (
        lookup_var_type(VarType, Arg, Type),
        type_is_higher_order(Type)
    ;
        horder_vars(Args, VarType)
    ).

%---------------------------------------------------------------------------%

lookup_proc_termination_info(ModuleInfo, PPId, MaybeTermination) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_maybe_termination_info(ProcInfo, MaybeTermination).

lookup_proc_arg_size_info(ModuleInfo, PPId, MaybeArgSize) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_maybe_arg_size_info(ProcInfo, MaybeArgSize).

is_termination_known(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_maybe_termination_info(ProcInfo, yes(_)).

pred_proc_id_terminates(ModuleInfo, PPId) :-
    module_info_pred_proc_info(ModuleInfo, PPId, _, ProcInfo),
    proc_info_get_maybe_termination_info(ProcInfo, TerminationInfo),
    TerminationInfo = yes(cannot_loop(_)).

%---------------------------------------------------------------------------%

all_args_input_or_zero_size(ModuleInfo, PredInfo, ProcInfo) :-
    pred_info_get_arg_types(PredInfo, TypeList),
    proc_info_get_argmodes(ProcInfo, ModeList),
    all_args_input_or_zero_size_2(TypeList, ModeList, ModuleInfo).

:- pred all_args_input_or_zero_size_2(list(mer_type)::in, list(mer_mode)::in,
    module_info::in) is semidet.

all_args_input_or_zero_size_2([], [], _).
all_args_input_or_zero_size_2([], [_|_], _) :-
    unexpected($pred, "unmatched lists").
all_args_input_or_zero_size_2([_|_], [], _) :-
    unexpected($pred, "unmatched lists").
all_args_input_or_zero_size_2([Type | Types], [Mode | Modes], ModuleInfo) :-
    ( if mode_is_input(ModuleInfo, Mode) then
        % The variable is an input variables, so its size is irrelevant.
        all_args_input_or_zero_size_2(Types, Modes, ModuleInfo)
    else
        term_norm.zero_size_type(ModuleInfo, Type),
        all_args_input_or_zero_size_2(Types, Modes, ModuleInfo)
    ).

%---------------------------------------------------------------------------%

set_pred_proc_ids_arg_size_info([], _ArgSize, !ModuleInfo).
set_pred_proc_ids_arg_size_info([PPId | PPIds], ArgSize, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    proc_info_set_maybe_arg_size_info(yes(ArgSize), ProcInfo0, ProcInfo),

    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    set_pred_proc_ids_arg_size_info(PPIds, ArgSize, !ModuleInfo).

set_pred_proc_ids_termination_info([], _Termination, !ModuleInfo).
set_pred_proc_ids_termination_info([PPId | PPIds], Termination, !ModuleInfo) :-
    PPId = proc(PredId, ProcId),
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    proc_info_set_maybe_termination_info(yes(Termination),
        ProcInfo0, ProcInfo),

    map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, PredTable0, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),
    set_pred_proc_ids_termination_info(PPIds, Termination, !ModuleInfo).

%---------------------------------------------------------------------------%

add_context_to_termination_info(no, _, no).
add_context_to_termination_info(yes(cannot_loop(_)), _,
    yes(cannot_loop(unit))).
add_context_to_termination_info(yes(can_loop(_)), Context,
    yes(can_loop([term_error(Context, imported_pred)]))).

add_context_to_arg_size_info(no, _, no).
add_context_to_arg_size_info(yes(finite(A, B)), _, yes(finite(A, B))).
add_context_to_arg_size_info(yes(infinite(_)), Context,
    yes(infinite([term_error(Context, imported_pred)]))).

%---------------------------------------------------------------------------%

get_context_from_scc(ModuleInfo, SCC, Context) :-
    set.to_sorted_list(SCC, SCCProcs),
    (
        SCCProcs = [proc(PredId, _) | _],
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        pred_info_get_context(PredInfo, Context)
    ;
        SCCProcs = [],
        unexpected($pred, "empty SCC")
    ).

%---------------------------------------------------------------------------%

attributes_imply_termination(Attributes) :-
    (
        get_terminates(Attributes) = proc_terminates
    ;
        get_terminates(Attributes) = depends_on_mercury_calls,
        get_may_call_mercury(Attributes) = proc_will_not_call_mercury
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.term_util.
%---------------------------------------------------------------------------%
