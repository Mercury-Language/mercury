%-----------------------------------------------------------------------------
%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%
% lex.convert_NFA_to_DFA.m
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Fri Aug 18 12:30:25 BST 2000
%
% Powerset construction used to transform NFAs into DFAs.
%
%   THIS FILE IS HEREBY CONTRIBUTED TO THE MERCURY PROJECT TO
%   BE RELEASED UNDER WHATEVER LICENCE IS DEEMED APPROPRIATE
%   BY THE ADMINISTRATORS OF THE MERCURY PROJECT.
%
%------------------------------------------------------------------------------%

:- module lex__convert_NFA_to_DFA.

:- interface.

:- import_module lex__automata.



:- func convert_NFA_to_DFA(state_mc) = state_mc.
:- mode convert_NFA_to_DFA(in(null_transition_free_state_mc)) =
            out(null_transition_free_state_mc) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module counter, list, set, map, char, int.



:- type state_sets
    ==      set(state_set).

:- type state_set
    ==      set(state_no).

:- type state_set_transitions
    ==      list(state_set_transition).

:- type state_set_transition
    --->    trans(state_set, char, state_set).

:- type state_set_no_map
    ==      map(state_set, int).

%------------------------------------------------------------------------------%

convert_NFA_to_DFA(NFA) = NFA :-

        % An NFA with no transitions is probably a bug...

    NFA ^ smc_state_transitions = [].

convert_NFA_to_DFA(NFA) = DFA :-

    NFA ^ smc_state_transitions = [_ | _],

        % Do some unpacking of the NFA.
        %
    NFAStopStates    = NFA ^ smc_stop_states,
    NFATransitions   = NFA ^ smc_state_transitions,
    DFAStartStateSet = set__make_singleton_set(NFA ^ smc_start_state),
    DFAStartStateSets = set__make_singleton_set(DFAStartStateSet),

        % Calculate the powerset version of the DFA from the NFA.
        %
    compute_DFA_state_sets_and_transitions(
        NFATransitions,     DFAStartStateSets,
        DFAStartStateSets,  DFAStateSets,
        [],                 DFAStateSetTransitions
    ),
    DFAStopStateSets = compute_DFA_stop_state_sets(NFAStopStates, DFAStateSets),

        % Replace the powerset state_no identifiers with numbers.
        %
    DFAStateNos      = number_state_sets(DFAStateSets),
    map__lookup(DFAStateNos, DFAStartStateSet, DFAStartState),
    DFAStopStates0   = list__map(
                            map__lookup(DFAStateNos),
                            set__to_sorted_list(DFAStopStateSets)
                       ),
    DFAStopStates    = set__list_to_set(DFAStopStates0),
    DFATransitions   = map_state_set_transitions_to_numbers(
                            DFAStateNos,
                            DFAStateSetTransitions
                       ),

        % Pack up the result.
        %
    DFA = state_mc(DFAStartState, DFAStopStates, DFATransitions).

%------------------------------------------------------------------------------%

    % If S is a state_no set, then S -c-> S' where
    % S' = {y | x in S, x -c-> y}
    %
    % We iterate to the least fixed point starting with the start
    % state_no set.
    %
:- pred compute_DFA_state_sets_and_transitions(
            transitions, state_sets,
            state_sets, state_sets,
            state_set_transitions, state_set_transitions).
:- mode compute_DFA_state_sets_and_transitions(in, in, in, out, in, out) is det.

compute_DFA_state_sets_and_transitions(Ts, NewSs0, Ss0, Ss, STs0, STs) :-

    ( if set__empty(NewSs0) then

        Ss   = Ss0,
        STs0 = STs

      else

        NewSTs =
            list__condense(
                list__map(state_set_transitions(Ts),set__to_sorted_list(NewSs0))
            ),
        STs1 = list__append(NewSTs, STs0),

        TargetSs =
            set__list_to_set(
                list__map(( func(trans(_, _, S)) = S ), NewSTs)
            ),
        NewSs = TargetSs `set__difference` Ss0,
        Ss1   = NewSs `set__union` Ss0,

        compute_DFA_state_sets_and_transitions(Ts, NewSs, Ss1, Ss, STs1, STs)
    ).

%------------------------------------------------------------------------------%

    % Given a state_no set and a set of transition chars for that
    % state_no set, find the set of state_no set transitions (said
    % Peter Piper):
    %
    % state_set_transitions(S) = {S -c-> S' | x in S, S' = {y | x -c-> y}}
    %
:- func state_set_transitions(transitions, state_set) = state_set_transitions.

state_set_transitions(Ts, S) = STs :-
    TCs = transition_chars(Ts, S),
    STs = list__map(state_set_transition(Ts, S), TCs).

%------------------------------------------------------------------------------%

    % Given a state_no set, find all the transition chars:
    %
    % transition_chars(S) = {c | x in S, some [y] x -c-> y}
    %
:- func transition_chars(transitions, state_set) = list(char).

transition_chars(Ts, S) =
    list__sort_and_remove_dups(
        list__condense(
            list__map(transition_chars_for_state(Ts), set__to_sorted_list(S))
        )
    ).

%------------------------------------------------------------------------------%

:- func transition_chars_for_state(transitions, state_no) = list(char).
:- mode transition_chars_for_state(in, in) = out is det.

transition_chars_for_state(Ts, X) =
    list__filter_map(transition_char_for_state(X), Ts).

%------------------------------------------------------------------------------%

:- func transition_char_for_state(state_no, transition) = char.
:- mode transition_char_for_state(in, in) = out is semidet.

transition_char_for_state(X, trans(X, C, _Y)) = C.

%------------------------------------------------------------------------------%

    % Given a state_no set and a char, find the state_no set transition:
    %
    % state_set_transition(S, c) = S -c-> target_state_set(S, c)
    %
:- func state_set_transition(transitions, state_set, char) =
            state_set_transition.

state_set_transition(Ts, S, C) = trans(S, C, target_state_set(Ts, S, C)).

%------------------------------------------------------------------------------%

    % Given a state_no set and a char, find the target state_no set:
    %
    % target_state_set(S, c) = {y | x in S, x -c-> y}
    %
:- func target_state_set(transitions, state_set, char) = state_set.

target_state_set(Ts, S, C) =
    set__power_union(set__map(target_state_set_0(Ts, C), S)).

%------------------------------------------------------------------------------%

:- func target_state_set_0(transitions, char, state_no) = state_set.

target_state_set_0(Ts, C, X) =
    set__list_to_set(list__filter_map(target_state(X, C), Ts)).

%------------------------------------------------------------------------------%

:- func target_state(state_no, char, transition) = state_no.
:- mode target_state(in, in, in) = out is semidet.

target_state(X, C, trans(X, C, Y)) = Y.

%------------------------------------------------------------------------------%

:- func compute_DFA_stop_state_sets(state_set, state_sets) = state_sets.

compute_DFA_stop_state_sets(StopStates, StateSets) =
    set__filter_map(stop_state_set(StopStates), StateSets).

%------------------------------------------------------------------------------%

:- func stop_state_set(state_set, state_set) = state_set.
:- mode stop_state_set(in, in) = out is semidet.

stop_state_set(StopStates, StateSet) = StateSet :-
    not set__empty(StopStates `set__intersect` StateSet).

%------------------------------------------------------------------------------%

:- func number_state_sets(state_sets) = state_set_no_map.

number_state_sets(Ss) = StateNos :-
    list__foldl2(
        ( pred(S::in, N::in, (N + 1)::out, Map0::in, Map::out) is det :-
            Map = map__set(Map0, S, N)
        ),
        set__to_sorted_list(Ss),
        0,          _,
        map__init,  StateNos
    ).

%------------------------------------------------------------------------------%

:- func map_state_set_transitions_to_numbers(state_set_no_map,
            state_set_transitions
        ) = transitions.
:- mode map_state_set_transitions_to_numbers(in, in) =
            out(atom_transitions).

map_state_set_transitions_to_numbers(Map, STs) =
    list__map(
        ( func(trans(SX, C, SY)) = trans(X, C, Y) :-
            X = map__lookup(Map, SX),
            Y = map__lookup(Map, SY)
        ),
        STs
    ).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
