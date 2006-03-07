%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 1999, 2003, 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file: group.m.
% main author: conway.
% stability: low (obsolete).
%
% This module is probably not terribly useful, and it may not be supported
% in future releases.
%
% The `group' module provides a facility for handling a partitioned set.
% A group is a set of sets of elements, where each element is unique within
% the scope of the group. The module provides moderately efficient ways for
% manipulating groups and elements.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module group.

:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module set.

:- type group(T).

:- type group.key.

    % Create an empty group.
    %
:- pred group.init(group(T)::out) is det.
:- func group.init = group(T).

    % Insert a set of elements into the group.
    %
:- pred group.insert(group(T)::in, set(T)::in, group(T)::out) is det.
:- func group.insert(group(T), set(T)) = group(T).

    % Given an element, get the set containing that element.
    %
:- pred group.group(group(T)::in, T::in, set(T)::out) is det.
:- func group.group(group(T), T) = set(T).

    % Convert the group to a set of sets.
    %
:- pred group.to_set(group(T)::in, set(set(T))::out) is det.
:- func group.to_set(group(T)) = set(set(T)).

:- pred group.sets_and_keys(group(T)::in,
    assoc_list(set(T), group.key)::out) is det.
:- func group.sets_and_keys(group(T)) = assoc_list(set(T), group.key).

    % Given an element, get the key for the group containing that element.
    %
:- pred group.group_key(group(T)::in, T::in, group.key::out) is det.
:- func group.group_key(group(T), T) = group.key.

    % Given a group key, get the corresponding set of elements.
    %
:- pred group.key_group(group(T)::in, group.key::in, set(T)::out) is det.
:- func group.key_group(group(T), group.key) = set(T).

    % Remove a set from the group, and return the set.
    %
:- pred group.remove_group(group(T)::in, group.key::in, set(T)::out,
    group(T)::out) is det.

    % Test to see if two elements are in the same set.
    %
:- pred group.same_group(group(T)::in, T::in, T::in) is semidet.

:- pred group.largest_group_key(group(T)::in, group.key::out) is det.
:- func group.largest_group_key(group(T)) = group.key.

:- pred group.group_keys(group(T)::in, list(group.key)::out) is det.
:- func group.group_keys(group(T)) = list(group.key).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module std_util.

:- type group(T)
    --->    group(
                key_supply      :: counter,
                sets            :: map(group.key, set(T)),
                elements        :: map(T, group.key)
            ).

:- type group.key  ==  int.

group.init(G) :-
    map.init(Es),
    map.init(Ss),
    G = group(counter.init(0), Es, Ss).

group.insert(!.G, S, !:G) :-
    !.G = group(KS0, Ss0, Es0),
    counter.allocate(C, KS0, KS),
    map.set(Ss0, C, S, Ss),
    set.to_sorted_list(S, SL),
    group.insert_elements(SL, C, Es0, Es),
    !:G = group(KS, Ss, Es).

:- pred group.insert_elements(list(T)::in, group.key::in,
    map(T, group.key)::in, map(T, group.key)::out) is det.

group.insert_elements([], _GK, Es, Es).
group.insert_elements([I | Is], GK, Es0, Es) :-
    map.set(Es0, I, GK, Es1),
    group.insert_elements(Is, GK, Es1, Es).

group.group(G, E, S) :-
    map.lookup(G ^ elements, E, GK),
    map.lookup(G ^ sets, GK, S).

group.to_set(G, S) :-
    map.values(G ^ sets, S0),
    set.list_to_set(S0, S).

group.sets_and_keys(G, SKs) :-
    map.to_assoc_list(G ^ sets, SKs0),
    assoc_list.reverse_members(SKs0, SKs).

group.group_key(G, E, GK) :-
    map.lookup(G ^ elements, E, GK).

group.key_group(G, GK, S) :-
    map.lookup(G ^ sets, GK, S).

group.remove_group(!.G, GK, S, !:G) :-
    Ss0 = !.G ^ sets,
    Es0 = !.G ^ elements,
    ( map.remove(Ss0, GK, SPrime, SsPrime) ->
        S = SPrime,
        Ss = SsPrime
    ;
        error("map.remove unexpectedly failed.")
    ),
    set.to_sorted_list(S, SL),
    group.remove_elements(SL, Es0, Es),
    !:G = !.G ^ sets := Ss,
    !:G = !.G ^ elements := Es.

:- pred group.remove_elements(list(T)::in,
    map(T, group.key)::in, map(T, group.key)::out) is det.

group.remove_elements([], Es, Es).
group.remove_elements([I | Is], Es0, Es) :-
    map.delete(Es0, I, Es1),
    group.remove_elements(Is, Es1, Es).

group.same_group(G, E0, E1) :-
    Es = G ^ elements,
    map.lookup(Es, E0, GK),
    map.lookup(Es, E1, GK).

group.largest_group_key(G, GK) :-
    Ss = G ^ sets,
    map.to_assoc_list(Ss, SL),
    group.largest_group_key_2(SL, 0, 0, GK).

:- pred group.largest_group_key_2(assoc_list(group.key, set(T))::in, int::in,
    group.key::in, group.key::out) is det.

group.largest_group_key_2([], _, GK, GK).
group.largest_group_key_2([GK0 - S0 | Ss], Sz0, GK1, GK) :-
    set.to_sorted_list(S0, S1),
    list.length(S1, Sz1),
    compare(R, Sz1, Sz0),
    ( R = (>) ->
        Sz = Sz1,
        GK2 = GK0
    ;
        Sz = Sz0,
        GK2 = GK1
    ),
    group.largest_group_key_2(Ss, Sz, GK2, GK).

%---------------------------------------------------------------------------%

group.group_keys(G, Ks) :-
    map.keys(G ^ sets, Ks).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

group.init = G :-
    group.init(G).

group.insert(G1, S) = G2 :-
    group.insert(G1, S, G2).

group.group(G, T) = S :-
    group.group(G, T, S).

group.to_set(G) = SS :-
    group.to_set(G, SS).

group.sets_and_keys(G) = AL :-
    group.sets_and_keys(G, AL).

group.group_key(G, T) = K :-
    group.group_key(G, T, K).

group.key_group(G, K) = S :-
    group.key_group(G, K, S).

group.largest_group_key(G) = K :-
    group.largest_group_key(G, K).

group.group_keys(G) = Ks :-
    group.group_keys(G, Ks).

