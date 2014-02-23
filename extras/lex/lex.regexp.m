%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et tw=0 wm=0 ff=unix
%-----------------------------------------------------------------------------%
%
% lex.regexp.m
% Fri Aug 18 06:43:09 BST 2000
% Copyright (C) 2001 Ralph Becket <rbeck@microsoft.com>
% Copyright (C) 2001 The Rationalizer Intelligent Software AG
%   The changes made by Rationalizer are contributed under the terms 
%   of the GNU Lesser General Public License, see the file COPYING.LGPL
%   in this directory.
% Copyright (C) 2002, 2010 The University of Melbourne
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%
% Thu Jul 26 07:45:47 UTC 2001
%
% Converts basic regular expressions into non-deterministic finite
% automata (NFAs).
%
%-----------------------------------------------------------------------------%

:- module lex.regexp.
:- interface.

:- import_module lex.automata.

    % Turn a regexp into an NFA.
    %
:- func regexp_to_NFA(regexp) = state_mc.

    % Turn an NFA into a null transition-free NFA.
    %
:- func remove_null_transitions(state_mc) = state_mc.
:- mode remove_null_transitions(in) = out(null_transition_free_state_mc) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module counter.
:- import_module list.
:- import_module map.
:- import_module set.
:- import_module std_util.
:- import_module string.
:- import_module sparse_bitset.

%-----------------------------------------------------------------------------%

regexp_to_NFA(R) = NFA :-
    C0 = counter.init(0),
    counter.allocate(Start, C0, C1),
    counter.allocate(Stop,  C1, C),
    compile(Start, R, Stop, Transitions, C, _),
    NFA = state_mc(Start, set.make_singleton_set(Stop), Transitions).

%-----------------------------------------------------------------------------%

:- pred compile(state_no, regexp, state_no, transitions, counter, counter).
:- mode compile(in, in, in, out, in, out) is det.

    % The primitive regexps.

compile(X, eps,        Y, [null(X, Y)]) --> [].

compile(X, atom(C),    Y, [trans(X, make_singleton_set(C), Y)]) --> [].

compile(X, charset(Charset), Y, [trans(X, Charset, Y)]) --> [].

compile(X, conc(RA,RB), Y, TsA ++ TsB) -->
    counter.allocate(Z),
    compile(X, RA, Z, TsA),
    compile(Z, RB, Y, TsB).

compile(X, alt(RA, RB), Y, TsA ++ TsB) -->
    compile(X, RA, Y, TsA),
    compile(X, RB, Y, TsB).

compile(X, star(R),    Y, TsA ++ TsB) -->
    compile(X, null, Y, TsA),
    compile(X, R,    X, TsB).

%-----------------------------------------------------------------------------%

    % If we have a non-looping null transition from X to Y then
    % we need to add all the transitions from Y to X.
    %
    % We do this by first finding the transitive closure of the
    % null transition graph and then, for each edge X -> Y in that
    % graph, adding X -C-> Z for all C and Z s.t. Y -C-> Z.
    %
remove_null_transitions(NFA0) = NFA :-

    Ts = NFA0 ^ smc_state_transitions,
    split_transitions(Ts, NullTs, CharTs),
    trans_closure(NullTs, map.init, _Ins, map.init, Outs),
    NullFreeTs = add_atom_transitions(Outs, CharTs),

    StopStates0 = NFA0 ^ smc_stop_states,
    StopStates1 =
        set.list_to_set(
            list.filter_map(
                nulls_to_stop_state(Outs, NFA0 ^ smc_stop_states),
                NullTs
            )
        ),
    StopStates  = StopStates0 `set.union` StopStates1,

    NFA = (( NFA0
                ^ smc_state_transitions := NullFreeTs )
                ^ smc_stop_states       := StopStates).

%-----------------------------------------------------------------------------%

:- pred split_transitions(transitions, transitions, transitions).
:- mode split_transitions(in, out(null_transitions), out(atom_transitions)).

split_transitions([], [], []).

split_transitions([null(X, Y) | Ts], [null(X, Y) | NTs], CTs) :-
    split_transitions(Ts, NTs, CTs).

split_transitions([trans(X, C, Y) | Ts], NTs, [trans(X, C, Y) | CTs]) :-
    split_transitions(Ts, NTs, CTs).

%-----------------------------------------------------------------------------%

:- type null_map == map(state_no, set(state_no)).

:- pred trans_closure(transitions, null_map, null_map, null_map, null_map).
:- mode trans_closure(in(null_transitions), in, out, in, out) is det.

trans_closure(Ts, Ins0, Ins, Outs0, Outs) :-
    list.foldl2(add_edge, Ts, Ins0, Ins, Outs0, Outs).

%-----------------------------------------------------------------------------%

:- pred add_edge(transition, null_map, null_map, null_map, null_map).
:- mode add_edge(in(null_transition), in, out, in, out) is det.

add_edge(null(X, Y), Ins0, Ins, Outs0, Outs) :-
    XInAndX  = set.insert(null_map_lookup(X, Ins0), X),
    YOutAndY = set.insert(null_map_lookup(Y, Outs0), Y),
    Xs = set.to_sorted_list(XInAndX),
    Ys = set.to_sorted_list(YOutAndY),
    Outs = list.foldl(add_to_null_mapping(YOutAndY), Xs, Outs0),
    Ins  = list.foldl(add_to_null_mapping(XInAndX),  Ys, Ins0).

%-----------------------------------------------------------------------------%

:- func null_map_lookup(state_no, null_map) = set(state_no).

null_map_lookup(X, Map) =
    ( if map.search(Map, X, Ys) then Ys
                                 else set.init
    ).

%-----------------------------------------------------------------------------%

:- func add_to_null_mapping(set(state_no), state_no, null_map) = null_map.

add_to_null_mapping(Xs, Y, Map) =
    map.set(Map, Y, Xs `set.union` null_map_lookup(Y, Map)).

%-----------------------------------------------------------------------------%

:- func add_atom_transitions(null_map, transitions) = transitions.
:- mode add_atom_transitions(in, in(atom_transitions)) =
            out(atom_transitions) is det.

add_atom_transitions(Outs, CTs) =
    list.sort_and_remove_dups(
        list.condense(
            [ CTs
            | list.map(
                add_atom_transitions_0(CTs),
                map.to_assoc_list(Outs)
              )
            ]
        )
    ).

%-----------------------------------------------------------------------------%

:- func add_atom_transitions_0(transitions, pair(state_no, set(state_no))) =
            transitions.
:- mode add_atom_transitions_0(in(atom_transitions), in) =
            out(atom_transitions) is det.

add_atom_transitions_0(CTs, X - Ys) =
    list.condense(
        list.map(add_atom_transitions_1(CTs, X), set.to_sorted_list(Ys))
    ).

%-----------------------------------------------------------------------------%

:- func add_atom_transitions_1(transitions, state_no, state_no) = transitions.
:- mode add_atom_transitions_1(in(atom_transitions), in, in) =
            out(atom_transitions) is det.

add_atom_transitions_1(CTs0, X, Y) = CTs :-
    list.filter_map(maybe_copy_transition(X, Y), CTs0, CTs).

%-----------------------------------------------------------------------------%

:- pred maybe_copy_transition(state_no, state_no, transition, transition).
:- mode maybe_copy_transition(in,in,in(atom_transition),out(atom_transition))
            is semidet.

maybe_copy_transition(X, Y, trans(Y, C, Z), trans(X, C, Z)).

%-----------------------------------------------------------------------------%

:- func nulls_to_stop_state(null_map, set(state_no), transition) = state_no.
:- mode nulls_to_stop_state(in, in, in) = out is semidet.

nulls_to_stop_state(Outs, StopStates, null(X, _Y)) = X :-
    some [Z] (
        set.member(Z, map.lookup(Outs, X)),
        set.member(Z, StopStates)
    ).

%-----------------------------------------------------------------------------%
:- end_module lex.regexp.
%-----------------------------------------------------------------------------%
