%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997, 1999, 2003, 2005 The University of Melbourne.
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

:- type group__key.

    % Create an empty group.
    %
:- pred group__init(group(T)::out) is det.
:- func group__init = group(T).

    % Insert a set of elements into the group.
    %
:- pred group__insert(group(T)::in, set(T)::in, group(T)::out) is det.
:- func group__insert(group(T), set(T)) = group(T).

    % Given an element, get the set containing that element.
    %
:- pred group__group(group(T)::in, T::in, set(T)::out) is det.
:- func group__group(group(T), T) = set(T).

    % Convert the group to a set of sets.
    %
:- pred group__to_set(group(T)::in, set(set(T))::out) is det.
:- func group__to_set(group(T)) = set(set(T)).

:- pred group__sets_and_keys(group(T)::in,
    assoc_list(set(T), group__key)::out) is det.
:- func group__sets_and_keys(group(T)) = assoc_list(set(T), group__key).

    % Given an element, get the key for the group containing that element.
    %
:- pred group__group_key(group(T)::in, T::in, group__key::out) is det.
:- func group__group_key(group(T), T) = group__key.

    % Given a group key, get the corresponding set of elements.
    %
:- pred group__key_group(group(T)::in, group__key::in, set(T)::out) is det.
:- func group__key_group(group(T), group__key) = set(T).

    % Remove a set from the group, and return the set.
    %
:- pred group__remove_group(group(T)::in, group__key::in, set(T)::out,
    group(T)::out) is det.

    % Test to see if two elements are in the same set.
    %
:- pred group__same_group(group(T)::in, T::in, T::in) is semidet.

:- pred group__largest_group_key(group(T)::in, group__key::out) is det.
:- func group__largest_group_key(group(T)) = group__key.

:- pred group__group_keys(group(T)::in, list(group__key)::out) is det.
:- func group__group_keys(group(T)) = list(group__key).

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
                sets            :: map(group__key, set(T)),
                elements        :: map(T, group__key)
            ).

:- type group__key  ==  int.

group__init(G) :-
    map__init(Es),
    map__init(Ss),
    G = group(counter__init(0), Es, Ss).

group__insert(!.G, S, !:G) :-
    !.G = group(KS0, Ss0, Es0),
    counter__allocate(C, KS0, KS),
    map__set(Ss0, C, S, Ss),
    set__to_sorted_list(S, SL),
    group__insert_elements(SL, C, Es0, Es),
    !:G = group(KS, Ss, Es).

:- pred group__insert_elements(list(T)::in, group__key::in,
    map(T, group__key)::in, map(T, group__key)::out) is det.

group__insert_elements([], _GK, Es, Es).
group__insert_elements([I | Is], GK, Es0, Es) :-
    map__set(Es0, I, GK, Es1),
    group__insert_elements(Is, GK, Es1, Es).

group__group(G, E, S) :-
    map__lookup(G ^ elements, E, GK),
    map__lookup(G ^ sets, GK, S).

group__to_set(G, S) :-
    map__values(G ^ sets, S0),
    set__list_to_set(S0, S).

group__sets_and_keys(G, SKs) :-
    map__to_assoc_list(G ^ sets, SKs0),
    assoc_list__reverse_members(SKs0, SKs).

group__group_key(G, E, GK) :-
    map__lookup(G ^ elements, E, GK).

group__key_group(G, GK, S) :-
    map__lookup(G ^ sets, GK, S).

group__remove_group(!.G, GK, S, !:G) :-
    Ss0 = !.G ^ sets,
    Es0 = !.G ^ elements,
    ( map__remove(Ss0, GK, SPrime, SsPrime) ->
        S = SPrime,
        Ss = SsPrime
    ;
        error("map__remove unexpectedly failed.")
    ),
    set__to_sorted_list(S, SL),
    group__remove_elements(SL, Es0, Es),
    !:G = !.G ^ sets := Ss,
    !:G = !.G ^ elements := Es.

:- pred group__remove_elements(list(T)::in,
    map(T, group__key)::in, map(T, group__key)::out) is det.

group__remove_elements([], Es, Es).
group__remove_elements([I | Is], Es0, Es) :-
    map__delete(Es0, I, Es1),
    group__remove_elements(Is, Es1, Es).

group__same_group(G, E0, E1) :-
    Es = G ^ elements,
    map__lookup(Es, E0, GK),
    map__lookup(Es, E1, GK).

group__largest_group_key(G, GK) :-
    Ss = G ^ sets,
    map__to_assoc_list(Ss, SL),
    group__largest_group_key_2(SL, 0, 0, GK).

:- pred group__largest_group_key_2(assoc_list(group__key, set(T))::in, int::in,
    group__key::in, group__key::out) is det.

group__largest_group_key_2([], _, GK, GK).
group__largest_group_key_2([GK0 - S0 | Ss], Sz0, GK1, GK) :-
    set__to_sorted_list(S0, S1),
    list__length(S1, Sz1),
    compare(R, Sz1, Sz0),
    ( R = (>) ->
        Sz = Sz1,
        GK2 = GK0
    ;
        Sz = Sz0,
        GK2 = GK1
    ),
    group__largest_group_key_2(Ss, Sz, GK2, GK).

%---------------------------------------------------------------------------%

group__group_keys(G, Ks) :-
    map__keys(G ^ sets, Ks).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
%   Function forms added.

group__init = G :-
    group__init(G).

group__insert(G1, S) = G2 :-
    group__insert(G1, S, G2).

group__group(G, T) = S :-
    group__group(G, T, S).

group__to_set(G) = SS :-
    group__to_set(G, SS).

group__sets_and_keys(G) = AL :-
    group__sets_and_keys(G, AL).

group__group_key(G, T) = K :-
    group__group_key(G, T, K).

group__key_group(G, K) = S :-
    group__key_group(G, K, S).

group__largest_group_key(G) = K :-
    group__largest_group_key(G, K).

group__group_keys(G) = Ks :-
    group__group_keys(G, Ks).

