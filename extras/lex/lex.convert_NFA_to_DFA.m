%----------------------------------------------------------------------------
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix ft=mercury
%----------------------------------------------------------------------------
%
% lex.convert_NFA_to_DFA.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Copyright (C) 2002, 2010 The University of Melbourne
% Copyright (C) 2014, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%
% Fri Aug 18 12:30:25 BST 2000
%
% Powerset construction used to transform NFAs into DFAs.
%
%-----------------------------------------------------------------------------%

:- module lex.convert_NFA_to_DFA.
:- interface.

:- import_module lex.automata.

:- func convert_NFA_to_DFA(state_mc) = state_mc.
:- mode convert_NFA_to_DFA(in(null_transition_free_state_mc)) =
    out(null_transition_free_state_mc) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module sparse_bitset.

%-----------------------------------------------------------------------------%

:- type state_sets
    ==      set(state_set).

:- type state_set
    ==      set(state_no).

:- type state_set_transitions
    ==      list(state_set_transition).

:- type state_set_transition
    --->    trans(state_set, charset, state_set).

:- type state_set_no_map
    ==      map(state_set, int).

%-----------------------------------------------------------------------------%

convert_NFA_to_DFA(NFA) = DFA :-
    (
        NFA ^ smc_state_transitions = [],
        % An NFA with no transitions is probably a bug...
        DFA = NFA
    ;
        NFA ^ smc_state_transitions = [_ | _],

        % Do some unpacking of the NFA.
        NFAStopStates    = NFA ^ smc_stop_states,
        NFATransitions   = NFA ^ smc_state_transitions,
        DFAStartStateSet = set.make_singleton_set(NFA ^ smc_start_state),
        DFAStartStateSets = set.make_singleton_set(DFAStartStateSet),

        % Calculate the powerset version of the DFA from the NFA.
        compute_DFA_state_sets_and_transitions(
            NFATransitions, DFAStartStateSets,
            DFAStartStateSets, DFAStateSets, [], DFAStateSetTransitions),
        DFAStopStateSets =
            compute_DFA_stop_state_sets(NFAStopStates, DFAStateSets),

        % Replace the powerset state_no identifiers with numbers.
        DFAStateNos = number_state_sets(DFAStateSets),
        map.lookup(DFAStateNos, DFAStartStateSet, DFAStartState),
        DFAStopStates = set.map(map.lookup(DFAStateNos), DFAStopStateSets),
        DFATransitions = map_state_set_transitions_to_numbers(
            DFAStateNos, DFAStateSetTransitions),

        % Pack up the result.
        DFA = state_mc(DFAStartState, DFAStopStates, DFATransitions)
    ).

%-----------------------------------------------------------------------------%

    % If S is a state_no set, then S -c-> S' where
    % S' = {y | x in S, x -c-> y}
    %
    % We iterate to the least fixed point starting with the start
    % state_no set.
    %
:- pred compute_DFA_state_sets_and_transitions(transitions::in, state_sets::in,
    state_sets::in, state_sets::out,
    state_set_transitions::in, state_set_transitions::out) is det.

compute_DFA_state_sets_and_transitions(Ts, NewSs0, Ss0, Ss, STs0, STs) :-
    ( if set.is_empty(NewSs0) then
        Ss   = Ss0,
        STs0 = STs
      else
        NewSTs =
            list.condense(
                list.map(state_set_transitions(Ts),set.to_sorted_list(NewSs0))
            ),
        STs1 = list.append(NewSTs, STs0),
        TargetSs =
            set.list_to_set(
                list.map(( func(trans(_, _, S)) = S ), NewSTs)
            ),
        NewSs = TargetSs `set.difference` Ss0,
        Ss1   = NewSs `set.union` Ss0,
        compute_DFA_state_sets_and_transitions(Ts, NewSs, Ss1, Ss, STs1, STs)
    ).

%-----------------------------------------------------------------------------%

    % Given a state_no set and a set of transition chars for that
    % state_no set, find the set of state_no set transitions (said
    % Peter Piper):
    %
    % state_set_transitions(S) = {S -c-> S' | x in S, S' = {y | x -c-> y}}
    %
:- func state_set_transitions(transitions, state_set) = state_set_transitions.

state_set_transitions(Ts, S) = STs :-
    TCs = to_sorted_list(transition_chars(Ts, S)),
    STs = list.map(state_set_transition(Ts, S), TCs).

%-----------------------------------------------------------------------------%

    % Given a state_no set, find all the transition chars:
    %
    % transition_chars(S) = {c | x in S, some [y] x -c-> y}
    %
:- func transition_chars(transitions, state_set) = charset.

transition_chars(Ts, S) = Charset :-
    Sets = list.map(transition_chars_for_state(Ts), set.to_sorted_list(S)),
    Charset = union_list(Sets).

%-----------------------------------------------------------------------------%

:- func transition_chars_for_state(transitions, state_no) = charset.

transition_chars_for_state(Ts, X) =
    union_list(list.filter_map(transition_char_for_state(X), Ts)).

%-----------------------------------------------------------------------------%

:- func transition_char_for_state(state_no, transition) = charset.
:- mode transition_char_for_state(in, in) = out is semidet.

transition_char_for_state(X, trans(X, C, _Y)) = C.

%-----------------------------------------------------------------------------%

    % Given a state_no set and a char, find the state_no set transition:
    %
    % state_set_transition(S, c) = S -c-> target_state_set(S, c)
    %
:- func state_set_transition(transitions, state_set, char) =
    state_set_transition.

state_set_transition(Ts, FromStateSet, C) = Transition :-
    Charset = sparse_bitset.make_singleton_set(C),
    TargetStateSet = target_state_set(Ts, FromStateSet, C),
    Transition = trans(FromStateSet, Charset, TargetStateSet).

%-----------------------------------------------------------------------------%

    % Given a state_no set and a char, find the target state_no set:
    %
    % target_state_set(S, c) = {y | x in S, x -c-> y}
    %
:- func target_state_set(transitions, state_set, char) = state_set.

target_state_set(Ts, S, C) =
    set.power_union(set.map(target_state_set_0(Ts, C), S)).

%-----------------------------------------------------------------------------%

:- func target_state_set_0(transitions, char, state_no) = state_set.

target_state_set_0(Ts, C, X) =
    set.list_to_set(list.filter_map(target_state(X, C), Ts)).

%-----------------------------------------------------------------------------%

:- func target_state(state_no, char, transition) = state_no.
:- mode target_state(in, in, in) = out is semidet.

target_state(X, C, trans(X, Charset, Y)) = Y :-
    contains(Charset, C).

%-----------------------------------------------------------------------------%

:- func compute_DFA_stop_state_sets(state_set, state_sets) = state_sets.

compute_DFA_stop_state_sets(StopStates, StateSets) =
    set.filter_map(stop_state_set(StopStates), StateSets).

%-----------------------------------------------------------------------------%

:- func stop_state_set(state_set, state_set) = state_set.
:- mode stop_state_set(in, in) = out is semidet.

stop_state_set(StopStates, StateSet) = StateSet :-
    set.is_non_empty(StopStates `set.intersect` StateSet).

%-----------------------------------------------------------------------------%

:- func number_state_sets(state_sets) = state_set_no_map.

number_state_sets(Ss) = StateNos :-
    list.foldl2(
        ( pred(S::in, N::in, (N + 1)::out, Map0::in, Map::out) is det :-
            map.set(S, N, Map0, Map)
        ),
        set.to_sorted_list(Ss),
        0, _, map.init, StateNos).

%-----------------------------------------------------------------------------%

:- func map_state_set_transitions_to_numbers(state_set_no_map::in,
    state_set_transitions::in) = (transitions::out(atom_transitions)).

map_state_set_transitions_to_numbers(_Map, []) = [].
map_state_set_transitions_to_numbers(Map, [ST | STs]) = [T | Ts] :-
    Ts = map_state_set_transitions_to_numbers(Map, STs),
    ST = trans(SX, C, SY),
    X = map.lookup(Map, SX),
    Y = map.lookup(Map, SY),
    T = trans(X, C ,Y).

%-----------------------------------------------------------------------------%
:- end_module lex.convert_NFA_to_DFA.
%-----------------------------------------------------------------------------%
