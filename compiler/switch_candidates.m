%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2016-2026 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: switch_candidates.m.
% Author: zs.
%
% Sometimes, a disjunction can be converted into a switch
% on more than one variable. This module contains the logic we use
% to decide which variable we should choose.
%
%---------------------------------------------------------------------------%

:- module check_hlds.switch_candidates.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.

    % A disjunction is a candidate for conversion to a switch on cs_var
    % if the conditions that is_candidate_switch tests for are satisfied.
    %
    % The disjuncts of the original disjunction will each end up in one
    % the next three fields: cs_cases, cs_unreachable_case_goals, and
    % cs_left_over_disjuncts.
    %
    % The left over disjuncts are the disjuncts that do not unify cs_var
    % with any function symbol, at least in a way that is visible to
    % switch detection. The disjuncts that *do* unify cs_var with a function
    % symbol will be converted into cases. If two (or more) disjuncts
    % unify cs_var with the same function symbol, those disjuncts will be put
    % into the same case (with the case's goal being a disjunction containing
    % just these disjuncts). Some cases may turn out to be unselectable,
    % because cs_var's initial inst guarantees that it won't be unifiable
    % with the case's function symbol; cs_unreachable_case_goals contains
    % the bodies of such cases.
    %
    % Since we try to convert only non-empty disjunctions to switches,
    % the above guarantees that at least one of cs_cases,
    % cs_unreachable_case_goals and cs_left_over_disjuncts will be non-empty.
    % If both cs_cases and cs_unreachable_case_goals would be empty for a given
    % variable, then we won't consider converting the disjunction into
    % a switch on that variable, so for any candidate_switch we *do* construct,
    % at least one of cs_cases and cs_unreachable_case_goals will be nonempty.
    % However, it is possible for either one of those fields to be empty
    % if the other contains at least one entry.
    %
    % Some disjunctions can be converted into switches on more than one
    % variable. We prefer to pick the variable that will allow determinism
    % analysis to find the tightest possible bounds on the number of solutions.
    % As a heuristic to help us choose well, we associate a rank with each
    % candidate conversion scheme. If there is more than candidate switch
    % we can turn the disjunction into, we choose the candidate with
    % the highest rank; we break any ties by picking the candidate
    % with the smallest variable number.
    %
    % When we convert the disjunction into a switch based on the chosen
    % candidate, we need to fill in the can_fail field of the switch
    % we create. We get the value we need from the cs_can_fail field.
:- type candidate_switch
    --->    candidate_switch(
                cs_var                      :: prog_var,
                cs_cases                    :: list(case),
                cs_unreachable_case_goals   :: list(hlds_goal),
                cs_left_over_disjuncts      :: list(hlds_goal),
                cs_rank                     :: candidate_switch_rank,
                cs_can_fail                 :: can_fail
            ).

    % The initial version of switch detection, which we used for a *long* time,
    % looked at nonlocal variables in variable number order and stopped looking
    % when it found a viable candidate switch.
    %
    % This could lead to an suboptimal outcome for two separate reasons.
    %
    % - First, when the user specifies which variable the switch should be on
    %   (via a require_switch_* scope), the committed-to variable is
    %   not necessarily the specified variable.
    %
    % - Second, switching on a variable later in the order can lead to a
    %   tighter determinism (because it leads to a cannot_fail switch, when
    %   the switch on the earlier, committed-to variable is can_fail).
    %
    % We fixed the first problem by putting RequiredVar at the start of
    % VarsToTry in cases where MaybeRequiredVar is yes(RequiredVar), and
    % we fixed the second by evaluating all candidate switches, without
    % stopping when we found a viable one. The second fix obsoletes the first;
    % if we look at all candidates and select the one with the best rank,
    % then for correctness, it *doesn't matter* in what order we look at
    % the nonlocals.
    %
    % It might matter for performance. When MaybeRequiredVar is
    % yes(RequiredVar), we *could* arrange to look at RequiredVar first,
    % and if it does yield a candidate switch with the best possible rank,
    % stop looking at the other variables. However, this would require
    % detect_switch_candidates_in_disj testing that condition after finding
    % each candidate switch. Since require_switch_* scopes are relatively rare,
    % the cost of the test in the common case where MaybeRequiredVar is "no"
    % would probably cost us more overall than we could save in cases where
    % MaybeRequiredVar is "yes".

    % The order of preference that we use to decide which candidate switch
    % to turn a disjunction into, for disjunctions in which we actually
    % have a choice. The ranks are in order from the least attractive
    % to the most attractive choice.
    %
    % In general, we prefer to have no disjuncts "left over" after we convert
    % disjuncts to case arms. All else being equal, we also prefer switches
    % in which the resulting switch arms cover all the function symbols
    % in the type of the switched-on variable that are allowed by the instmap
    % at entry to the switch.
    %
:- type candidate_switch_rank
    --->    some_leftover_can_fail(
                % Some of the disjuncts will have to remain outside the switch,
                % and the switch will be can_fail. This is the least useful
                % kind of switch that switch detection can create.
                int     % number of case arms
            )

    ;       some_leftover_cannot_fail(
                % Some of the disjuncts will have to remain outside the switch,
                % but at least the switch will be cannot_fail (though the
                % code inside the switch arms may fail).
                int     % number of case arms
            )

    ;       no_leftover_twoplus_cases_finite_can_fail
            % All disjuncts unify the switch variable with a function symbol,
            % but there is at least one function symbol that the switch
            % variable can be bound to at the start of the disjunction
            % that is not covered by any of the original disjuncts.
            % There are at least two cases, and at least one is reachable
            % (the rest may be unreachable).

    ;       no_leftover_one_case
            % With no_leftover_twoplus_cases_finite_can_fail, we *know*
            % that the resulting switch will be can_fail, and therefore
            % it can't be det. However, if all the disjuncts unify this
            % candidate var with the *same* function symbol, which is the
            % situation that no_leftover_one_case describes, then we know that
            % cse_detection.m will pull this deconstruction unification
            % out of all the disjuncts. Then, when cse_detection.m repeats
            % switch detection, there is at least a chance that we will be
            % able to transform this disjunction to a det switch.

    ;       no_leftover_twoplus_cases_infinite_can_fail
            % All disjuncts unify the switch variable with a function symbol,
            % but the domain of the switch variable is infinite, so it is
            % not possible for all the function symbols in the domain
            % to be covered by a case.
            %
            % I (zs) don't know of any strong argument for deciding
            % the relative order of no_leftover_twoplus_cases_infinite_can_fail
            % and no_leftover_one_case either way. The current order replicates
            % the relative order between these two cases that was effectively
            % imposed by old code.

    ;       no_leftover_twoplus_cases_finite_cannot_fail
            % The best switch we can hope for in normal circumstances;
            % all disjuncts unify the switch variable with a function symbol,
            % and every function symbol that the switch variable can be
            % bound to at the start of the disjunction is covered by at least
            % one disjunct. There are at least two cases, and at least one
            % is reachable (the rest may be unreachable).

    ;       all_disjuncts_are_unreachable
            % We can convert the entire disjunction to just `fail'.
            % This is possible only in the very rare case when all disjuncts
            % unify the switch variable with a function symbol that the switch
            % variable's initial inst rules out, but when it *is* possible,
            % the resulting goal will have the tightest possible determinism
            % we can hope for, namely failure.

    ;       no_leftover_twoplus_cases_explicitly_selected.
            % If the disjunction is what the programmer would consider
            % to be a switch on the variable that they explicitly said
            % that they expect the disjunction to switch on, i.e. if
            % all disjunct unify the specified variable with a function
            % symbol and there are at least two cases, then follow the
            % programmer's lead. The programmer may prefer an incomplete
            % switch on the specified variable to a complete switch on
            % another variable. An example from the compiler: when handling
            % special options in options.m, we would prefer the option handler
            % to switch on the option, not on the kind of data (none, bool,
            % int, string, maybe_string) given to it.

%---------------------------------------------------------------------------%

:- pred is_candidate_switch(list(case)::in, list(hlds_goal)::in) is semidet.

:- type maybe_required_switch_var
    --->    nrsv
            % "no required switch var": This goal is not inside a scope
            % that requires its top level goal to be a switch on a specific
            % variable.
    ;       rsv(prog_var).
            % "required switch var": This goal *is* inside a scope
            % that requires its top level goal to be a switch on *this*
            % variable.

:- pred categorize_candidate_switch(module_info::in,
    maybe_required_switch_var::in, prog_var::in, mer_type::in, mer_inst::in,
    list(case)::in, list(hlds_goal)::in, candidate_switch::out) is det.

:- pred select_best_candidate_switch(candidate_switch::in,
    list(candidate_switch)::in, candidate_switch::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.det_util.
:- import_module hlds.inst_test.
:- import_module hlds.type_util.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_type.

:- import_module int.
:- import_module set_tree234.
:- import_module term.

%---------------------------------------------------------------------------%

is_candidate_switch(Cases0, LeftOver) :-
    (
        % If every disjunct unifies Var with a function symbol, then
        % it is candidate switch on Var, *even if* all disjuncts unify Var
        % with the *same* function symbol. This is because the resulting
        % single-arm switch may turn out to contain sub-switches on the
        % *arguments* of that function symbol.
        LeftOver = []
    ;
        % If some disjunct does not unify Var with any function symbol,
        % then we insist on at least two cases (though one may unreachable,
        % see below). We do this because the presence of the LeftOver
        % disjunct(s) requires us to have an outer disjunction anyway;
        % having one of its arms be a single-arm switch would be
        % indistinguishable from the original disjunction in almost all cases.
        % The only exception I (zs) can think of would happen if the same
        % X = f(...) goal occurred inside all the disjuncts that would end up
        % in an inner disjunction inside the single-arm switch's single arm,
        % but not in the other disjuncts. In that case, acting on the
        % candidate we would create here may allow cse_detection.m to make
        % a change could enable later follow-on changes by switch detection
        % itself. However, I have never seen any real-life code that could
        % benefit from this theoretical possibility, and until we do see
        % such code, so the gain from deleting this test would be minimal
        % at best, while the cost of deleting it would be to greatly increase
        % the number of candidates and thus the time taken by switch detection.
        Cases0 = [_, _ | _]
    ).

%---------------------------------------------------------------------------%

categorize_candidate_switch(ModuleInfo, MaybeRequiredVar, Var, VarType,
        VarInst0, Cases0, LeftOver, Candidate) :-
    can_candidate_switch_fail(ModuleInfo, VarType, VarInst0, Cases0,
        CanFail, CasesMissing, Cases, UnreachableCaseGoals),
    (
        LeftOver = [],
        (
            Cases = [],
            Rank = all_disjuncts_are_unreachable
        ;
            Cases = [_FirstCase | LaterCases],
            ( if
                LaterCases = [],
                UnreachableCaseGoals = []
            then
                Rank = no_leftover_one_case
            else
                % FirstCase is one case, and whichever of LaterCases and
                % UnreachableCaseGoals is nonempty is the second case.
                ( if
                    MaybeRequiredVar = rsv(RequiredVar),
                    RequiredVar = Var
                then
                    Rank = no_leftover_twoplus_cases_explicitly_selected
                else
                    (
                        CasesMissing = some_cases_missing,
                        Rank = no_leftover_twoplus_cases_finite_can_fail
                    ;
                        CasesMissing = no_cases_missing,
                        Rank = no_leftover_twoplus_cases_finite_cannot_fail
                    ;
                        CasesMissing = unbounded_cases,
                        Rank = no_leftover_twoplus_cases_infinite_can_fail
                    )
                )
            )
        )
    ;
        LeftOver = [_ | _],
        list.length(Cases, NumCases),
        (
            CanFail = cannot_fail,
            Rank = some_leftover_cannot_fail(NumCases)
        ;
            CanFail = can_fail,
            Rank = some_leftover_can_fail(NumCases)
        )
    ),
    Candidate = candidate_switch(Var, Cases, UnreachableCaseGoals,
        LeftOver, Rank, CanFail).

:- type cases_missing
    --->    no_cases_missing
    ;       some_cases_missing
    ;       unbounded_cases.

    % Find out whether a switch on a given variable with a given set
    % of cases can fail.
    %
:- pred can_candidate_switch_fail(module_info::in, mer_type::in, mer_inst::in,
    list(case)::in, can_fail::out, cases_missing::out, list(case)::out,
    list(hlds_goal)::out) is det.

can_candidate_switch_fail(ModuleInfo, VarType, VarInst0, Cases0,
        SwitchCanFail, CasesMissing, Cases, UnreachableCaseGoals) :-
    ( if inst_is_bound_to_functors(ModuleInfo, VarInst0, BoundFunctors) then
        type_to_ctor_det(VarType, TypeCtor),
        bound_functors_to_cons_ids(TypeCtor, BoundFunctors, InstConsIds),
        set_tree234.list_to_set(InstConsIds, InstConsIdSet),
        delete_unreachable_cases(Cases0, InstConsIdSet,
            Cases, UnreachableCaseGoals),
        switch_can_fail_with_bound_functors(InstConsIdSet, Cases,
            SwitchCanFail, CasesMissing)
    else
        % We do not have any inst information that would allow us to decide
        % that any case is unreachable.
        Cases = Cases0,
        UnreachableCaseGoals = [],
        ( if switch_type_num_functors(ModuleInfo, VarType, NumFunctors) then
            % We could check for each cons_id of the type whether a case covers
            % it, but given that type checking ensures that the set of covered
            % cons_ids is a subset of the set of cons_ids of the type, checking
            % whether the cardinalities of the two sets match is *equivalent*
            % to checking whether they are the same set.
            does_switch_cover_n_cases(NumFunctors, Cases,
                SwitchCanFail, CasesMissing)
        else
            % switch_type_num_functors fails only for types on which
            % you cannot have a complete switch, e.g. integers and strings.
            SwitchCanFail = can_fail,
            CasesMissing = unbounded_cases
        )
    ).

:- pred switch_can_fail_with_bound_functors(set_tree234(cons_id)::in,
    list(case)::in, can_fail::out, cases_missing::out) is det.

switch_can_fail_with_bound_functors(InstConsIds, Cases,
        SwitchCanFail, CasesMissing) :-
    acc_covered_functors(Cases, set_tree234.init, CoveredConsIds),
    set_tree234.difference(InstConsIds, CoveredConsIds, UncoveredConsIds),
    ( if set_tree234.is_empty(UncoveredConsIds) then
        SwitchCanFail = cannot_fail,
        CasesMissing = no_cases_missing
    else
        SwitchCanFail = can_fail,
        CasesMissing = some_cases_missing
    ).

    % Delete from !UncoveredConsIds all cons_ids mentioned in any of the cases.
    %
:- pred acc_covered_functors(list(case)::in,
    set_tree234(cons_id)::in, set_tree234(cons_id)::out) is det.

acc_covered_functors([], !CoveredConsIds).
acc_covered_functors([Case | Cases], !CoveredConsIds) :-
    Case = case(MainConsId, OtherConsIds, _Goal),
    set_tree234.insert(MainConsId, !CoveredConsIds),
    set_tree234.insert_list(OtherConsIds, !CoveredConsIds),
    acc_covered_functors(Cases, !CoveredConsIds).

    % Check whether a switch handles the given number of cons_ids.
    %
:- pred does_switch_cover_n_cases(int::in, list(case)::in,
    can_fail::out, cases_missing::out) is det.

does_switch_cover_n_cases(NumFunctors, Cases, SwitchCanFail, CasesMissing) :-
    count_covered_cons_ids(Cases, 0, NumCoveredConsIds),
    ( if NumCoveredConsIds = NumFunctors then
        SwitchCanFail = cannot_fail,
        CasesMissing = no_cases_missing
    else
        SwitchCanFail = can_fail,
        CasesMissing = some_cases_missing
    ).

:- pred count_covered_cons_ids(list(case)::in, int::in, int::out) is det.

count_covered_cons_ids([], !NumCoveredConsIds).
count_covered_cons_ids([Case | Cases], !NumCoveredConsIds) :-
    Case = case(_MainConsId, OtherConsIds, _Goal),
    !:NumCoveredConsIds = !.NumCoveredConsIds + 1 + list.length(OtherConsIds),
    count_covered_cons_ids(Cases, !NumCoveredConsIds).

%---------------------------------------------------------------------------%

select_best_candidate_switch(FirstCandidate, LaterCandidates, BestCandidate) :-
    (
        LaterCandidates = [],
        BestCandidate = FirstCandidate
    ;
        LaterCandidates = [_ | _],
        BestCandidate0 = FirstCandidate,
        select_best_candidate_switch_loop(LaterCandidates,
            BestCandidate0, BestCandidate)
    ).

:- pred select_best_candidate_switch_loop(list(candidate_switch)::in,
    candidate_switch::in, candidate_switch::out) is det.

select_best_candidate_switch_loop([], !BestCandidate).
select_best_candidate_switch_loop([Candidate | Candidates], !BestCandidate) :-
    compare(Result, Candidate ^ cs_rank, !.BestCandidate ^ cs_rank),
    (
        ( Result = (<)
        ; Result = (=)
        )
    ;
        Result = (>),
        !:BestCandidate = Candidate
    ),
    select_best_candidate_switch_loop(Candidates, !BestCandidate).

%---------------------------------------------------------------------------%
:- end_module check_hlds.switch_candidates.
%---------------------------------------------------------------------------%
