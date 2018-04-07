%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1998-2001, 2003-2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: pd_term.m.
% Main author: stayl.
%
% Termination checking for the deforestation process.
% There are two places where care must be taken to ensure
% termination of the process:
%
% - when unfolding a call to a recursive procedure
% - when creating a new version, to avoid creating an infinite sequence of
%   new versions for which folding never occurs.
%
% For conjunctions, count up the length of the conjunction.
% For each pair of calls on the end of the conjunction,
% this length must decrease for the check to succeed.
%
% For single calls, the first call records the sizes of the insts
% of all the arguments. If the total size of a later call increases,
% the increasing arguments are removed from the record. If there are
% no decreasing arguments, the termination check fails. Otherwise
% the check succeeds and the new argument sizes are recorded.
%
% There are many possible improvements to this:
%
% - Partition on subterms of arguments rather than whole arguments - useful
%   when partially instantiated structures are present.
% - Use homeomorphic embedding instead of term sizes as suggested in
%   the papers on partial deduction from K.U. Leuven. This will be
%   useful (necessary?) if we start propagating equality constraints.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.pd_term.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module transform_hlds.pd_info.

:- import_module list.
:- import_module maybe.
:- import_module pair.

%-----------------------------------------------------------------------------%

    % global_check(Module, CallGoal1, BetweenGoals, CallGoal2,
    %   InstMap, Versions, Info0, Info, Result):
    %
    % Check whether a new version can be created for the conjunction
    % (CallGoal1, BetweenGoals, CallGoal2) without the deforestation
    % process looping.
    %
:- pred global_check(module_info::in, hlds_goal::in, list(hlds_goal)::in,
    maybe(hlds_goal)::in, instmap::in, version_index::in, global_term_info::in,
    global_term_info::out, global_check_result::out) is det.

    % A proc_pair holds the pred_proc_ids of the procedures called at
    % the ends of a conjunction to be deforested.
    % The maybe(pred_proc_id) is `no' in the case of a predicate
    % created for constraint propagation.
:- type proc_pair == pair(pred_proc_id, maybe(pred_proc_id)).

:- type global_check_result
    --->    ok(proc_pair, int)
    ;       possible_loop(proc_pair, int, pred_proc_id)
    ;       loop.

    % Check whether a call can be unfolded without the
    % unfolding process looping.
    %
:- pred local_check(module_info::in, hlds_goal::in, instmap::in,
    local_term_info::in, local_term_info::out) is semidet.

:- pred global_term_info_init(global_term_info::out) is det.

:- pred local_term_info_init(local_term_info::out) is det.

:- pred get_proc_term_info(local_term_info::in, pred_proc_id::in,
    pd_proc_term_info::out) is semidet.

    % Update the global termination information when we find out the
    % pred_proc_id that has been assigned to a version.
    %
:- pred update_global_term_info(proc_pair::in, pred_proc_id::in,
    int::in, global_term_info::in,global_term_info::out) is det.

:- type global_term_info.
:- type local_term_info.
:- type pd_proc_term_info.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module transform_hlds.pd_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type global_term_info
    --->    global_term_info(
                single_covering_goals,
                multiple_covering_goals
            ).

    % We only deal with single atoms while unfolding.
:- type local_term_info == single_covering_goals.

    % For single goals, use the argument partition method.
:- type single_covering_goals == map(pred_proc_id, pd_proc_term_info).

    % Map from a pair of procedures at the end of the conjunction
    % to be deforested and the most recent ancestor with this pair
    % of goals.
:- type multiple_covering_goals ==
        map(proc_pair, pair(int, maybe(pred_proc_id))).

    % Mapping from argument to size.
:- type pd_proc_term_info   ==  assoc_list(int, int).

%-----------------------------------------------------------------------------%

global_term_info_init(TermInfo) :-
    map.init(SingleGoals),
    map.init(MultipleGoals),
    TermInfo = global_term_info(SingleGoals, MultipleGoals).

local_term_info_init(TermInfo) :-
    map.init(TermInfo).

get_proc_term_info(TermInfo, PredProcId, ProcTermInfo) :-
    map.search(TermInfo, PredProcId, ProcTermInfo).

%-----------------------------------------------------------------------------%

global_check(_ModuleInfo, EarlierGoal, BetweenGoals, MaybeLaterGoal,
        _InstMap, Versions, !Info, Result) :-
    !.Info = global_term_info(SingleGoalCover0, MultipleGoalCover0),
    ( if
        EarlierGoal = hlds_goal(plain_call(PredId1, ProcId1, _, _, _, _), _),
        Hd =
            ( pred(List::in, Head::out) is semidet :-
                List = [Head | _]
            ),
        expand_calls(Hd, Versions, proc(PredId1, ProcId1), FirstPredProcId),
        (
            MaybeLaterGoal = yes(
                hlds_goal(plain_call(PredId2, ProcId2, _, _, _, _), _)),
            expand_calls(list.last, Versions, proc(PredId2, ProcId2),
                LastPredProcId),
            MaybeLastPredProcId = yes(LastPredProcId)
        ;
            MaybeLaterGoal = no,
            MaybeLastPredProcId = no
        )
    then
        ProcPair = FirstPredProcId - MaybeLastPredProcId,
        list.length(BetweenGoals, Length),
        ( if
            map.search(MultipleGoalCover0, ProcPair,
                MaxLength - MaybeCoveringPredProcId)
        then
            ( if
                Length < MaxLength
            then
                Result = ok(ProcPair, Length),
                % Set the maybe(pred_proc_id) when we create the new predicate.
                map.set(ProcPair, Length - no,
                    MultipleGoalCover0, MultipleGoalCover)
            else if
                Length = MaxLength,
                MaybeCoveringPredProcId = yes(CoveringPredProcId)
            then
                % If the goals match, check that the argument insts decrease.
                % If not, we may need to do a generalisation step.
                Result = possible_loop(ProcPair, Length, CoveringPredProcId),
                MultipleGoalCover = MultipleGoalCover0
            else
                Result = loop,
                MultipleGoalCover = MultipleGoalCover0
            )
        else
            % We haven't seen this pair before, so it must be okay
            % to specialise.
            Result = ok(ProcPair, Length),

            % Set the maybe(pred_proc_id) when we create the new predicate.
            map.set(ProcPair, Length - no,
                MultipleGoalCover0, MultipleGoalCover)
        ),
        SingleGoalCover = SingleGoalCover0
    else
        unexpected($pred, "global_check")
    ),
    !:Info = global_term_info(SingleGoalCover, MultipleGoalCover).

    % We don't want to use folded calls to parent versions
    % when doing the global termination check, since that
    % could give a sequence:
    %   old ....pred1
    %   new1 .... pred1
    %   new2 ....... pred1
    %   new3 ......... pred1
    % Instead, we expand to predicates from the original program,
    % which must contain a finite number of pairs of pred_proc_ids.
    %
:- pred expand_calls(pred(list(pred_proc_id), pred_proc_id)::
    in(pred(in, out) is semidet), version_index::in,
    pred_proc_id::in, pred_proc_id::out) is semidet.

expand_calls(GetEnd, Versions, PredProcId0, PredProcId) :-
    ( if map.search(Versions, PredProcId0, VersionInfo) then
        Calls = VersionInfo ^ version_deforest_calls,
        GetEnd(Calls, PredProcId1),
        expand_calls(GetEnd, Versions, PredProcId1, PredProcId)
    else
        PredProcId = PredProcId0
    ).

%-----------------------------------------------------------------------------%

local_check(ModuleInfo, Goal1, InstMap, !Cover) :-
    Goal1 = hlds_goal(plain_call(PredId, ProcId, Args, _, _, _), _),
    ( if map.search(!.Cover, proc(PredId, ProcId), CoveringInstSizes0) then
        do_local_check(ModuleInfo, InstMap, Args,
            CoveringInstSizes0, CoveringInstSizes),
        map.set(proc(PredId, ProcId), CoveringInstSizes, !Cover)
    else
        initial_sizes(ModuleInfo, InstMap, Args, 1, ArgInstSizes),
        map.set(proc(PredId, ProcId), ArgInstSizes, !Cover)
    ).

:- pred do_local_check(module_info::in, instmap::in, list(prog_var)::in,
    assoc_list(int, int)::in, assoc_list(int, int)::out) is semidet.

do_local_check(ModuleInfo, InstMap, Args, OldSizes, NewSizes) :-
    get_matching_sizes(ModuleInfo, InstMap, Args, OldSizes, NewSizes1,
        OldTotal, NewTotal),
    ( if NewTotal < OldTotal then
        NewSizes = NewSizes1
    else
        split_out_non_increasing(OldSizes, NewSizes1, yes, NewSizes)
    ).

%-----------------------------------------------------------------------------%

update_global_term_info(ProcPair, PredProcId, Size, !TermInfo) :-
    !.TermInfo = global_term_info(Single, Multiple0),
    map.set(ProcPair, Size - yes(PredProcId), Multiple0, Multiple),
    !:TermInfo = global_term_info(Single, Multiple).

%-----------------------------------------------------------------------------%

:- pred initial_sizes(module_info::in, instmap::in,
    list(prog_var)::in, int::in, assoc_list(int, int)::out) is det.

initial_sizes(_, _, [], _, []).
initial_sizes(ModuleInfo, InstMap, [Arg | Args], ArgNo,
        [ArgNo - Size | Sizes]) :-
    NextArgNo = ArgNo + 1,
    initial_sizes(ModuleInfo, InstMap, Args, NextArgNo, Sizes),
    instmap_lookup_var(InstMap, Arg, ArgInst),
    pd_util.inst_size(ModuleInfo, ArgInst, Size).

%-----------------------------------------------------------------------------%

:- pred get_matching_sizes(module_info::in, instmap::in,
    list(prog_var)::in, assoc_list(int, int)::in,
    assoc_list(int, int)::out, int::out, int::out) is det.

get_matching_sizes(_, _, _, [], [], 0, 0).
get_matching_sizes(ModuleInfo, InstMap, Args,
        [ArgNo - OldSize | OldSizes], [ArgNo - NewSize | NewSizes],
        OldTotal, NewTotal) :-
    get_matching_sizes(ModuleInfo, InstMap, Args, OldSizes, NewSizes,
        OldTotal1, NewTotal1),
    list.det_index1(Args, ArgNo, Arg),
    instmap_lookup_var(InstMap, Arg, ArgInst),
    pd_util.inst_size(ModuleInfo, ArgInst, NewSize),
    OldTotal = OldTotal1 + OldSize,
    NewTotal = NewTotal1 + NewSize.

%-----------------------------------------------------------------------------%

:- pred split_out_non_increasing(assoc_list(int, int)::in,
    assoc_list(int, int)::in, bool::out, assoc_list(int, int)::out) is semidet.

split_out_non_increasing([], [], no, []).
split_out_non_increasing([_|_], [], _, _) :-
    unexpected($pred, "list length mismatch").
split_out_non_increasing([], [_|_], _, _) :-
    unexpected($pred, "list length mismatch").
split_out_non_increasing([Arg - OldSize | Args0],
        [_ - NewSize | Args], FoundDecreasing, NonIncreasing) :-
    split_out_non_increasing(Args0, Args, FoundDecreasing1, NonIncreasing1),
    ( if NewSize =< OldSize then
        NonIncreasing = [Arg - NewSize | NonIncreasing1],
        ( if NewSize = OldSize then
            FoundDecreasing = no
        else
            FoundDecreasing = yes
        )
    else
        NonIncreasing = NonIncreasing1,
        FoundDecreasing = FoundDecreasing1
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.pd_term.
%-----------------------------------------------------------------------------%
