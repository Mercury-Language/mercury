%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: ctgc.fixpoint_table.m.
% Main author: nancy.
%
% This module defines a generic table to be used for fixpoint computations.
% The purpose of this table is mainly to map pred_proc_ids onto abstract
% substitutions representing either structure sharing or structure reuse.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.fixpoint_table.
:- interface.

:- import_module list.

:- type fixpoint_table(K, E).

    % Initialise the table.
    % The first parameter is a function that produces the initial value for
    % each of the keys that are to be inserted into the table.
    %
:- func init_fixpoint_table(func(K) = E, list(K)) = fixpoint_table(K, E).

    % Inform the table that a new run has begun.
    %
:- pred new_run(fixpoint_table(K, E)::in, fixpoint_table(K, E)::out) is det.

    % Which run of the fix point are we up to?
    %
:- func which_run(fixpoint_table(K, E)) = int.

    % Check whether a fixpoint has been reached.
    %
:- pred fixpoint_reached(fixpoint_table(K, E)::in) is semidet.

    % Return a short description of the state of the fixpoint table.
    %
:- func description(fixpoint_table(K, E)) = string.

    % Check whether the entries are recursive.
    %
:- pred is_recursive(fixpoint_table(K, E)::in) is semidet.

    % add_to_fixpoint_table(EqualityTest, Key, Element, !Table):
    %
    % Add a new element (E) associated with key (K) to the table.
    %
    %   - If an element is already recorded with that key:
    %      * if the new value is subsumed by the existing value, then
    %        a fixpoint is obtained as far as this key is concerned;
    %      * if the values are different, fixpoint is not reached yet,
    %        and the new value is recorded instead of the old one.
    %
    %   - If the key has not yet any value associated to it, add it
    %     to the table (which does not change the stability of the table)
    %
:- pred add_to_fixpoint_table(pred(E, E)::in(pred(in, in) is semidet),
    K::in, E::in, fixpoint_table(K, E)::in, fixpoint_table(K, E)::out) is det.

    % Retrieve an element E associated with key K from the table.
    % This operation changes the state of the table if the
    % element _is_ present in the table. This means we are facing
    % a recursive calltree. If the key is not an element of the
    % allowed keys, then the procedure fails.
    %
:- pred get_from_fixpoint_table(K::in, E::out,
    fixpoint_table(K, E)::in, fixpoint_table(K, E)::out) is semidet.

    % Retrieve an element E associated with key K from the table.
    % The operation reports a software error when the element is not present.
    %
:- func get_from_fixpoint_table_final(K, fixpoint_table(K, E)) = E.

    % Same as get_final, but the predicate fails instead of aborting when
    % the element is not present.
    %
:- pred get_from_fixpoint_table_final_semidet(K::in, fixpoint_table(K, E)::in,
    E::out) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module map.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type fixpoint_table(K, E)
    --->    fixpoint_table(
                keys            :: list(K),              % list of allowed keys
                run             :: int,                  % number of runs
                recursive       :: is_recursive,
                mapping         :: map(K, fp_entry(E))
            ).

:- type fp_entry(E)
    --->    entry(
                entry_stable    :: is_stable,
                entry_elem      :: E
            ).

:- type is_recursive
    --->    is_recursive
    ;       is_not_recursive.

:- type is_stable
    --->    is_stable
    ;       is_unstable.

%-----------------------------------------------------------------------------%

:- func fp_entry_init(E) = fp_entry(E).
:- func fp_entry_init_with_stability(is_stable, E) = fp_entry(E).

fp_entry_init(Elem) = entry(is_unstable, Elem).
fp_entry_init_with_stability(IsStable, Elem) = entry(IsStable, Elem).

init_fixpoint_table(InitFunction, Ks) = FT :-
    InsertElement =
        ( pred(K::in, !.Map::in, !:Map::out) is det :-
            E = InitFunction(K),
            map.det_insert(K, fp_entry_init(E), !Map)
        ),
    list.foldl(InsertElement, Ks, map.init, Map),
    Run = 0,
    FT = fixpoint_table(Ks, Run, is_not_recursive, Map).

new_run(T0, T0 ^ run := T0 ^ run + 1).
which_run(T0) = T0 ^ run.

is_recursive(T) :- T ^ recursive = is_recursive.

fixpoint_reached(T) :-
    IsRecursive = T ^ recursive,
    (
        IsRecursive = is_recursive,
        map.foldl(accumulate_instability, T ^ mapping,
            is_stable, FinalStability),
        FinalStability = is_stable
    ;
        IsRecursive = is_not_recursive
    ).

:- pred accumulate_instability(K::in, fp_entry(E)::in,
    is_stable::in, is_stable::out) is det.

accumulate_instability(_Key, Entry, S0, S) :-
    (
        S0 = is_unstable,
        S  = is_unstable
    ;
        S0 = is_stable,
        S = Entry ^ entry_stable
    ).

description(T) =
    ( if fixpoint_reached(T) then
        "stable"
    else
        "unstable"
    ).

add_to_fixpoint_table(IsLessOrEqualTest, Index, Elem, !T) :-
    Map0 = !.T ^ mapping,
    map.lookup(Map0, Index, Entry),
    TabledElem = Entry ^ entry_elem,
    ( if IsLessOrEqualTest(Elem, TabledElem) then
        IsStable = is_stable
    else
        IsStable = is_unstable
    ),
    %
    % Whether or not the tabled element is equal to the new element, the final
    % tabled element will always be set to the new one. This is handy for
    % performing the following trick: equality can be checked on some partial
    % piece of the elements (for deciding stability), but some other part
    % might have changed, a change that should become visible in the table
    % too.  (in fact this is necessary for the reuse-fixpoint table where not
    % only the reuses are kept (the abstract substitution), but also the goal
    % that might have changed.
    %
    FinalTabledElem = fp_entry_init_with_stability(IsStable, Elem),
    map.det_update(Index, FinalTabledElem, Map0, Map),
    !T ^ mapping := Map.

get_from_fixpoint_table(Index, Elem, !T) :-
    List = !.T ^ keys,
    list.member(Index, List), % can fail
    Map = !.T ^ mapping,
    map.lookup(Map, Index, Entry),
    Elem = Entry ^ entry_elem,
    !T ^ recursive := is_recursive.

get_from_fixpoint_table_final(Index, T) = Elem :-
    ( if get_from_fixpoint_table_final_semidet(Index, T, TabledElem) then
        Elem = TabledElem
    else
        unexpected($pred, "key not in map.")
    ).

get_from_fixpoint_table_final_semidet(Index, T, Elem) :-
    map.search(T ^ mapping, Index, Entry),
    Elem = Entry ^ entry_elem.

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.fixpoint_table.
%-----------------------------------------------------------------------------%
