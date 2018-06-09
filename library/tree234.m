%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997,1999-2000,2002-2012 The University of Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: tree234.m.
% Main author: conway.
% Stability: medium.
%
% This module implements a map (dictionary) using 2-3-4 trees - see
% map.m for further documentation.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module tree234.
:- interface.

:- import_module assoc_list.
:- import_module list.
:- import_module pretty_printer.
:- import_module term.

%---------------------------------------------------------------------------%

:- type tree234(K, V).

%---------------------%

:- func init = tree234(K, V).
:- pred init(tree234(K, V)::uo) is det.

:- func singleton(K, V) = tree234(K, V).

:- pred is_empty(tree234(K, V)::in) is semidet.

    % True if both trees have the same set of key-value pairs, regardless of
    % how the trees were constructed.
    %
    % Unifying trees does not work as one might expect because the internal
    % structures of two trees that contain the same set of key-value pairs
    % may be different.
    %
:- pred equal(tree234(K, V)::in, tree234(K, V)::in) is semidet.

%---------------------%

:- pred member(tree234(K, V)::in, K::out, V::out) is nondet.

:- pred search(tree234(K, V)::in, K::in, V::out) is semidet.

:- func lookup(tree234(K, V), K) = V.
:- pred lookup(tree234(K, V)::in, K::in, V::out) is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Fails if there is no key with the given or lower value.
    %
:- pred lower_bound_search(tree234(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next lower key instead.
    % Throws an exception if there is no key with the given or lower value.
    %
:- pred lower_bound_lookup(tree234(K, V)::in, K::in, K::out, V::out)
    is det.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Fails if there is no key with the given or higher value.
    %
:- pred upper_bound_search(tree234(K, V)::in, K::in, K::out, V::out)
    is semidet.

    % Search for a key-value pair using the key. If there is no entry
    % for the given key, returns the pair for the next higher key instead.
    % Throws an exception if there is no key with the given or higher value.
    %
:- pred upper_bound_lookup(tree234(K, V)::in, K::in, K::out, V::out)
    is det.

:- func max_key(tree234(K, V)) = K is semidet.

:- func min_key(tree234(K, V)) = K is semidet.

%---------------------%

    % Insert the given key/value pair into the tree. If the key is already
    % in the tree, fail.
    %
:- pred insert(K::in, V::in, tree234(K, V)::in, tree234(K, V)::out)
    is semidet.

    % search_insert(K, V, MaybeOldV, !Tree):
    %
    % Search for the key K in the tree. If the key is already in the tree,
    % with corresponding value OldV, set MaybeOldV to yes(OldV). If it is
    % not in the tree, then insert it into the tree with value V.
    %
:- pred search_insert(K::in, V::in, maybe(V)::out,
    tree234(K, V)::in, tree234(K, V)::out) is det.

    % Update the value corresponding to the given key in the tree.
    % If the key is not already in the tree, fail.
    %
:- pred update(K::in, V::in, tree234(K, V)::in, tree234(K, V)::out)
    is semidet.

    % set(K, V, !Tree):
    %
    % Set the value corresponding to K to V, regardless of whether K is
    % already in the tree or not.
    %
:- func set(tree234(K, V), K, V) = tree234(K, V).
:- pred set(K::in, V::in, tree234(K, V)::in, tree234(K, V)::out) is det.

%---------------------%

    % Update the value at the given key by applying the supplied
    % transformation to it. This is faster than first searching for
    % the value and then updating it.
    %
:- pred transform_value(pred(V, V)::in(pred(in, out) is det), K::in,
    tree234(K, V)::in, tree234(K, V)::out) is semidet.

%---------------------%

    % Delete the given key from the tree if it is there.
    %
:- func delete(tree234(K, V), K) = tree234(K, V).
:- pred delete(K::in, tree234(K, V)::in, tree234(K, V)::out) is det.

    % If the given key exists in the tree, return it and then delete the pair.
    % Otherwise, fail.
    %
:- pred remove(K, V, tree234(K, V), tree234(K, V)).
:- mode remove(in, out, in, out) is semidet.

    % Remove the smallest key from the tree, and return both it and the value
    % corresponding to it. If the tree is empty, fail.
    %
:- pred remove_smallest(K, V, tree234(K, V), tree234(K, V)).
:- mode remove_smallest(out, out, in, out) is semidet.

%---------------------%

    % Given a tree234, return a list of all the keys in the tree.
    % The list that is returned is in sorted order (ascending on keys).
    %
:- func keys(tree234(K, V)) = list(K).
:- pred keys(tree234(K, V)::in, list(K)::out) is det.

    % Given a tree234, return a list of all the values in the tree.
    % The list that is returned is in sorted order (ascending on the original
    % keys, but not sorted on the values).
    %
:- func values(tree234(K, V)) = list(V).
:- pred values(tree234(K, V)::in, list(V)::out) is det.

    % Given a tree234, return lists of all the keys and values in the tree.
    % The key list is in sorted order (ascending on keys).
    % The values list is in sorted order (ascending on their keys,
    % but not on the values themselves).
    %
:- pred keys_and_values(tree234(K, V)::in, list(K)::out, list(V)::out) is det.

%---------------------%

    % Count the number of elements in a tree.
    %
:- func count(tree234(K, V)) = int.
:- pred count(tree234(K, V)::in, int::out) is det.

    % Given a tree234, return an association list of all the keys and values
    % in the tree. The association list that is returned is sorted on the keys.
    %
:- func tree234_to_assoc_list(tree234(K, V)) = assoc_list(K, V).
:- pred tree234_to_assoc_list(tree234(K, V)::in,
    assoc_list(K, V)::out) is det.

:- func assoc_list_to_tree234(assoc_list(K, V)) = tree234(K, V).
:- pred assoc_list_to_tree234(assoc_list(K, V)::in,
    tree234(K, V)::out) is det.

    % Given an assoc list of keys and values that are sorted on the keys
    % in ascending order (with no duplicate keys), convert it directly
    % to a tree.
    %
:- pred from_sorted_assoc_list(assoc_list(K, V)::in,
    tree234(K, V)::out) is det.

    % Given an assoc list of keys and values that are sorted on the keys
    % in descending order (with no duplicate keys), convert it directly
    % to a tree.
    %
:- pred from_rev_sorted_assoc_list(assoc_list(K, V)::in,
    tree234(K, V)::out) is det.

%---------------------%

:- func foldl(func(K, V, A) = A, tree234(K, V), A) = A.

:- pred foldl(pred(K, V, A, A), tree234(K, V), A, A).
:- mode foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldl(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldl(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode foldl(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldl(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldl(pred(in, in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldl(pred(in, in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode foldl(pred(in, in, mdi, muo) is cc_multi, in, mdi, muo) is cc_multi.

:- pred foldl2(pred(K, V, A, A, B, B), tree234(K, V), A, A, B, B).
:- mode foldl2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode foldl2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode foldl2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode foldl2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode foldl2(pred(in, in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.
:- mode foldl2(pred(in, in, in, out, in, out) is cc_multi,
    in, in, out, in, out) is cc_multi.
:- mode foldl2(pred(in, in, in, out, mdi, muo) is cc_multi,
    in, in, out, mdi, muo) is cc_multi.
:- mode foldl2(pred(in, in, in, out, di, uo) is cc_multi,
    in, in, out, di, uo) is cc_multi.
:- mode foldl2(pred(in, in, di, uo, di, uo) is cc_multi,
    in, di, uo, di, uo) is cc_multi.

:- pred foldl3(pred(K, V, A, A, B, B, C, C), tree234(K, V),
    A, A, B, B, C, C).
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldl3(pred(in, in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldl3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode foldl3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.
:- mode foldl3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3(pred(in, in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.

:- pred foldl4(pred(K, V, A, A, B, B, C, C, D, D), tree234(K, V),
    A, A, B, B, C, C, D, D).
:- mode foldl4(pred(in, in, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldl4(pred(in, in, in, out, in, out, di, uo, di, uo) is det,
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode foldl4(pred(in, in, in, out, di, uo, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode foldl4(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl4(pred(in, in, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl5(pred(K, V, A, A, B, B, C, C, D, D, E, E), tree234(K, V),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldl5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldl_values(pred(V, A, A), tree234(K, V), A, A).
:- mode foldl_values(pred(in, in, out) is det, in, in, out) is det.
:- mode foldl_values(pred(in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldl_values(pred(in, di, uo) is det, in, di, uo) is det.
:- mode foldl_values(pred(in, in, out) is semidet, in, in, out)
    is semidet.
:- mode foldl_values(pred(in, mdi, muo) is semidet, in, mdi, muo)
    is semidet.
:- mode foldl_values(pred(in, di, uo) is semidet, in, di, uo)
    is semidet.
:- mode foldl_values(pred(in, in, out) is cc_multi, in, in, out)
    is cc_multi.
:- mode foldl_values(pred(in, di, uo) is cc_multi, in, di, uo)
    is cc_multi.
:- mode foldl_values(pred(in, mdi, muo) is cc_multi, in, mdi, muo)
    is cc_multi.

:- pred foldl2_values(pred(V, A, A, B, B), tree234(K, V), A, A, B, B).
:- mode foldl2_values(pred(in, in, out, in, out) is det, in,
    in, out, in, out) is det.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is det, in,
    in, out, mdi, muo) is det.
:- mode foldl2_values(pred(in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode foldl2_values(pred(in, in, out, in, out) is semidet, in,
    in, out, in, out) is semidet.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is semidet, in,
    in, out, mdi, muo) is semidet.
:- mode foldl2_values(pred(in, in, out, di, uo) is semidet, in,
    in, out, di, uo) is semidet.
:- mode foldl2_values(pred(in, in, out, in, out) is cc_multi, in,
    in, out, in, out) is cc_multi.
:- mode foldl2_values(pred(in, in, out, mdi, muo) is cc_multi, in,
    in, out, mdi, muo) is cc_multi.
:- mode foldl2_values(pred(in, in, out, di, uo) is cc_multi, in,
    in, out, di, uo) is cc_multi.

:- pred foldl3_values(pred(V, A, A, B, B, C, C), tree234(K, V),
    A, A, B, B, C, C).
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.
:- mode foldl3_values(pred(in, in, out, in, out, in, out) is cc_multi,
    in, in, out, in, out, in, out) is cc_multi.
:- mode foldl3_values(pred(in, in, out, in, out, mdi, muo) is cc_multi,
    in, in, out, in, out, mdi, muo) is cc_multi.
:- mode foldl3_values(pred(in, in, out, in, out, di, uo) is cc_multi,
    in, in, out, in, out, di, uo) is cc_multi.

:- func foldr(func(K, V, A) = A, tree234(K, V), A) = A.

:- pred foldr(pred(K, V, A, A), tree234(K, V), A, A).
:- mode foldr(pred(in, in, in, out) is det, in, in, out) is det.
:- mode foldr(pred(in, in, mdi, muo) is det, in, mdi, muo) is det.
:- mode foldr(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode foldr(pred(in, in, in, out) is semidet, in, in, out) is semidet.
:- mode foldr(pred(in, in, mdi, muo) is semidet, in, mdi, muo) is semidet.
:- mode foldr(pred(in, in, di, uo) is semidet, in, di, uo) is semidet.
:- mode foldr(pred(in, in, in, out) is cc_multi, in, in, out) is cc_multi.
:- mode foldr(pred(in, in, di, uo) is cc_multi, in, di, uo) is cc_multi.
:- mode foldr(pred(in, in, mdi, muo) is cc_multi, in, mdi, muo) is cc_multi.

:- pred foldr2(pred(K, V, A, A, B, B), tree234(K, V), A, A, B, B).
:- mode foldr2(pred(in, in, in, out, in, out) is det,
    in, in, out, in, out) is det.
:- mode foldr2(pred(in, in, in, out, mdi, muo) is det,
    in, in, out, mdi, muo) is det.
:- mode foldr2(pred(in, in, in, out, di, uo) is det,
    in, in, out, di, uo) is det.
:- mode foldr2(pred(in, in, di, uo, di, uo) is det,
    in, di, uo, di, uo) is det.
:- mode foldr2(pred(in, in, in, out, in, out) is semidet,
    in, in, out, in, out) is semidet.
:- mode foldr2(pred(in, in, in, out, mdi, muo) is semidet,
    in, in, out, mdi, muo) is semidet.
:- mode foldr2(pred(in, in, in, out, di, uo) is semidet,
    in, in, out, di, uo) is semidet.

:- pred foldr3(pred(K, V, A, A, B, B, C, C), tree234(K, V),
    A, A, B, B, C, C).
:- mode foldr3(pred(in, in, in, out, in, out, in, out) is det,
    in, in, out, in, out, in, out) is det.
:- mode foldr3(pred(in, in, in, out, in, out, mdi, muo) is det,
    in, in, out, in, out, mdi, muo) is det.
:- mode foldr3(pred(in, in, in, out, in, out, di, uo) is det,
    in, in, out, in, out, di, uo) is det.
:- mode foldr3(pred(in, in, in, out, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo) is det.
:- mode foldr3(pred(in, in, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo) is det.
:- mode foldr3(pred(in, in, in, out, in, out, in, out) is semidet,
    in, in, out, in, out, in, out) is semidet.
:- mode foldr3(pred(in, in, in, out, in, out, mdi, muo) is semidet,
    in, in, out, in, out, mdi, muo) is semidet.
:- mode foldr3(pred(in, in, in, out, in, out, di, uo) is semidet,
    in, in, out, in, out, di, uo) is semidet.

:- pred foldr4(pred(K, V, A, A, B, B, C, C, D, D), tree234(K, V),
    A, A, B, B, C, C, D, D).
:- mode foldr4(pred(in, in, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out) is det.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, di, uo) is det,
    in, in, out, in, out, in, out, di, uo) is det.
:- mode foldr4(pred(in, in, in, out, in, out, di, uo, di, uo) is det,
    in, in, out, in, out, di, uo, di, uo) is det.
:- mode foldr4(pred(in, in, in, out, di, uo, di, uo, di, uo) is det,
    in, in, out, di, uo, di, uo, di, uo) is det.
:- mode foldr4(pred(in, in, di, uo, di, uo, di, uo, di, uo) is det,
    in, di, uo, di, uo, di, uo, di, uo) is det.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr4(pred(in, in, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, di, uo) is semidet.

:- pred foldr5(pred(K, V, A, A, B, B, C, C, D, D, E, E), tree234(K, V),
    A, A, B, B, C, C, D, D, E, E).
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is det,
    in, in, out, in, out, in, out, in, out, in, out) is det.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is det,
    in, in, out, in, out, in, out, in, out, mdi, muo) is det.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is det,
    in, in, out, in, out, in, out, in, out, di, uo) is det.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, in, out)
    is semidet,
    in, in, out, in, out, in, out, in, out, in, out) is semidet.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, in, out, in, out, in, out, in, out, mdi, muo) is semidet.
:- mode foldr5(pred(in, in, in, out, in, out, in, out, in, out, di, uo)
    is semidet,
    in, in, out, in, out, in, out, in, out, di, uo) is semidet.

%---------------------%

:- func map_values(func(K, V) = W, tree234(K, V)) = tree234(K, W).

:- pred map_values(pred(K, V, W), tree234(K, V), tree234(K, W)).
:- mode map_values(pred(in, in, out) is det, in, out) is det.
:- mode map_values(pred(in, in, out) is semidet, in, out) is semidet.

:- func map_values_only(func(V) = W, tree234(K, V)) = tree234(K, W).

:- pred map_values_only(pred(V, W), tree234(K, V), tree234(K, W)).
:- mode map_values_only(pred(in, out) is det, in, out) is det.
:- mode map_values_only(pred(in, out) is semidet, in, out) is semidet.

%---------------------%

:- pred map_foldl(pred(K, V, W, A, A), tree234(K, V), tree234(K, W),
    A, A).
:- mode map_foldl(pred(in, in, out, in, out) is det,
    in, out, in, out) is det.
:- mode map_foldl(pred(in, in, out, mdi, muo) is det,
    in, out, mdi, muo) is det.
:- mode map_foldl(pred(in, in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode map_foldl(pred(in, in, out, in, out) is semidet,
    in, out, in, out) is semidet.
:- mode map_foldl(pred(in, in, out, mdi, muo) is semidet,
    in, out, mdi, muo) is semidet.
:- mode map_foldl(pred(in, in, out, di, uo) is semidet,
    in, out, di, uo) is semidet.

:- pred map_foldl2(pred(K, V, W, A, A, B, B),
    tree234(K, V), tree234(K, W), A, A, B, B).
:- mode map_foldl2(pred(in, in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map_foldl2(pred(in, in, out, in, out, mdi, muo) is det,
    in, out, in, out, mdi, muo) is det.
:- mode map_foldl2(pred(in, in, out, di, uo, di, uo) is det,
    in, out, di, uo, di, uo) is det.
:- mode map_foldl2(pred(in, in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode map_foldl2(pred(in, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.
:- mode map_foldl2(pred(in, in, out, in, out, mdi, muo) is semidet,
    in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl2(pred(in, in, out, in, out, di, uo) is semidet,
    in, out, in, out, di, uo) is semidet.

:- pred map_foldl3(pred(K, V, W, A, A, B, B, C, C),
    tree234(K, V), tree234(K, W), A, A, B, B, C, C).
:- mode map_foldl3(pred(in, in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out) is det.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, mdi, muo) is det,
    in, out, in, out, in, out, mdi, muo) is det.
:- mode map_foldl3(pred(in, in, out, di, uo, di, uo, di, uo) is det,
    in, out, di, uo, di, uo, di, uo) is det.
:- mode map_foldl3(pred(in, in, out, in, out, di, uo, di, uo) is det,
    in, out, in, out, di, uo, di, uo) is det.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, di, uo) is det.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, in, out)
    is semidet,
    in, out, in, out, in, out, in, out) is semidet.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, mdi, muo)
    is semidet,
    in, out, in, out, in, out, mdi, muo) is semidet.
:- mode map_foldl3(pred(in, in, out, in, out, in, out, di, uo)
    is semidet,
    in, out, in, out, in, out, di, uo) is semidet.

:- pred map_values_foldl(pred(V, W, A, A),
    tree234(K, V), tree234(K, W), A, A).
:- mode map_values_foldl(pred(in, out, di, uo) is det,
    in, out, di, uo) is det.
:- mode map_values_foldl(pred(in, out, in, out) is det,
    in, out, in, out) is det.
:- mode map_values_foldl(pred(in, out, in, out) is semidet,
    in, out, in, out) is semidet.

:- pred map_values_foldl2(pred(V, W, A, A, B, B),
    tree234(K, V), tree234(K, W), A, A, B, B).
:- mode map_values_foldl2(pred(in, out, di, uo, di, uo) is det,
    in, out, di, uo, di, uo) is det.
:- mode map_values_foldl2(pred(in, out, in, out, di, uo) is det,
    in, out, in, out, di, uo) is det.
:- mode map_values_foldl2(pred(in, out, in, out, in, out) is det,
    in, out, in, out, in, out) is det.
:- mode map_values_foldl2(pred(in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out) is semidet.

:- pred map_values_foldl3(pred(V, W, A, A, B, B, C, C),
    tree234(K, V), tree234(K, W), A, A, B, B, C, C).
:- mode map_values_foldl3(
    pred(in, out, di, uo, di, uo, di, uo) is det,
    in, out, di, uo, di, uo, di, uo) is det.
:- mode map_values_foldl3(
    pred(in, out, in, out, di, uo, di, uo) is det,
    in, out, in, out, di, uo, di, uo) is det.
:- mode map_values_foldl3(
    pred(in, out, in, out, in, out, di, uo) is det,
    in, out, in, out, in, out, di, uo) is det.
:- mode map_values_foldl3(
    pred(in, out, in, out, in, out, in, out) is det,
    in, out, in, out, in, out, in, out) is det.
:- mode map_values_foldl3(
    pred(in, out, in, out, in, out, in, out) is semidet,
    in, out, in, out, in, out, in, out) is semidet.

%---------------------%

    % Convert a tree234 into a pretty_printer.doc. A tree mapping
    % K1 to V1, K2 to V2, ... is formatted as
    % "map([K1 -> V1, K2 -> V2, ...])". The functor "map" is used
    % because tree234 values are almost exclusively maps.
    %
:- func tree234_to_doc(tree234(K, V)) = pretty_printer.doc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.
% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.
:- interface.

:- import_module maybe.

:- pragma type_spec(tree234.search/3, K = var(_)).
:- pragma type_spec(tree234.search/3, K = int).

:- pragma type_spec(tree234.lookup/3, K = var(_)).

:- pragma type_spec(tree234.set(in, in, in, out), K = var(_)).

:- pragma type_spec(tree234.update(in, in, in, out), K = var(_)).
:- pragma type_spec(tree234.update(in, in, in, out), K = int).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This should be abstract, but needs to be exported for insts.
    %
:- type tree234(K, V)
    --->    empty
    ;       two(K, V, tree234(K, V), tree234(K, V))
    ;       three(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V))
    ;       four(K, V, K, V, K, V, tree234(K, V), tree234(K, V),
                tree234(K, V), tree234(K, V)).

:- inst uniq_tree234(K, V) for tree234/2 ==
    unique((
        empty
    ;   two(K, V, uniq_tree234(K, V), uniq_tree234(K, V))
    ;   three(K, V, K, V, uniq_tree234(K, V), uniq_tree234(K, V),
            uniq_tree234(K, V))
    ;   four(K, V, K, V, K, V, uniq_tree234(K, V), uniq_tree234(K, V),
            uniq_tree234(K, V), uniq_tree234(K, V))
    )).

:- inst uniq_tree234_gg for tree234/2 ==
    unique((
        empty
    ;   two(ground, ground, uniq_tree234_gg, uniq_tree234_gg)
    ;   three(ground, ground, ground, ground,
            uniq_tree234_gg, uniq_tree234_gg, uniq_tree234_gg)
    ;   four(ground, ground, ground, ground, ground, ground,
            uniq_tree234_gg, uniq_tree234_gg, uniq_tree234_gg, uniq_tree234_gg)
    )).

:- mode di_tree234(K, V) == uniq_tree234(K, V) >> dead.
:- mode di_tree234       == uniq_tree234(ground, ground) >> dead.
:- mode uo_tree234(K, V) == free >> uniq_tree234(K, V).
:- mode uo_tree234       == free >> uniq_tree234(ground, ground).

    % Return the minimum number of key/value pairs in the tree, given its
    % depth. This is obviously not as accurate as tree234.count, but it
    % is computed *much* faster, in time O(log Count), not O(Count).
    %
:- pred find_min_size_based_on_depth(tree234(K, V)::in, int::out) is det.

    % Check whether the given tree is well formed, i.e. all the empty nodes
    % are at the same depth. If yes, return that depth. Otherwise, return `no'.
    %
    % This predicate is a sanity check; it should *never* return `no'.
    %
:- pred well_formed(tree234(K, V)::in, maybe(int)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.

:- inst two(K, V, T) for tree234/2
    --->    two(K, V, T, T).
:- inst three(K, V, T) for tree234/2
    --->    three(K, V, K, V, T, T, T).
:- inst four(K, V, T) for tree234/2
    --->    four(K, V, K, V, K, V, T, T, T, T).

:- inst uniq_two(K, V, T) for tree234/2
    == unique(two(K, V, T, T)).
:- inst uniq_three(K, V, T) for tree234/2
    == unique(three(K, V, K, V, T, T, T)).
:- inst uniq_four(K, V, T) for tree234/2
    == unique(four(K, V, K, V, K, V, T, T, T, T)).

:- inst tree234_nonempty for tree234/2
    --->    two(ground, ground, ground, ground)
    ;       three(ground, ground, ground, ground,
                ground, ground, ground)
    ;       four(ground, ground, ground, ground, ground, ground,
                ground, ground, ground, ground).

:- mode uo_two  == out(uniq_two(unique, unique, unique)).
:- mode suo_two == out(uniq_two(ground, ground, uniq_tree234_gg)).
:- mode out_two == out(two(ground, ground, ground)).

:- mode di_two  == di(uniq_two(unique, unique, unique)).
:- mode sdi_two == di(uniq_two(ground, ground, uniq_tree234_gg)).
:- mode in_two  == in(two(ground, ground, ground)).

:- mode di_three  == di(uniq_three(unique, unique, unique)).
:- mode sdi_three == di(uniq_three(ground, ground, uniq_tree234_gg)).
:- mode in_three  == in(three(ground, ground, ground)).

:- mode di_four  == di(uniq_four(unique, unique, unique)).
:- mode sdi_four == di(uniq_four(ground, ground, uniq_tree234_gg)).
:- mode in_four  == in(four(ground, ground, ground)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

init = empty.

init(empty).

singleton(K, V) = two(K, V, empty, empty).

is_empty(Tree) :-
    Tree = empty.

equal(TreeA, TreeB) :-
    % We first try to see if the two trees are identical, since this can
    % save a large amount of work. If they aren't, then we must compare
    % their structures. To do this, we use a stack to store the
    % as-yet-unexplored parts of TreeB in their original order while the
    % match_tree_against_stack predicate recurses on TreeA.
    ( if private_builtin.pointer_equal(TreeA, TreeB) then
        true
    else
        require_complete_switch [TreeB]
        (
            TreeB = empty,
            TreeA = empty
        ;
            ( TreeB = two(_, _, _, _)
            ; TreeB = three(_, _, _, _, _, _, _)
            ; TreeB = four(_, _, _, _, _, _, _, _, _, _)
            ),
            StackB0 = [TreeB],
            match_tree_against_stack(TreeA, StackB0, StackB),
            % Note that StackB can never contain empty trees (see the
            % comment on match_tree_against_stack) therefore testing that
            % the whole of TreeB has been matched is trivial.
            StackB = []
        )
    ).

    % match_tree_against_stack(SubtreeA, StackB0, StackB),
    %
    % True if the key-value pairs of SubtreeA match the key-value pairs of
    % the subtrees in StackB0, StackB contains any subtrees from StackB0
    % that were not matched.
    %
    % StackB0 can never contain an empty node because:
    %  + match_tree_against_stack/3's caller will never put an empty node in
    %    StackB0.
    %  + match_tree_against_stack/3 and it's callees will never put an empty
    %    node on the stack.  Arbitrary nodes are checked before they're
    %    placed on the stack and other nodes are constructed deliberately.
    % The inst subtyping on these arguments enforces this.
    %
:- pred match_tree_against_stack(tree234(K, V)::in,
    list(tree234(K, V))::in(list(tree234_nonempty)),
    list(tree234(K, V))::out(list(tree234_nonempty))) is semidet.

match_tree_against_stack(SubtreeA, !StackB) :-
    ( if
        !.StackB = [LeftmostSubtreeB | !:StackB],
        private_builtin.pointer_equal(SubtreeA, LeftmostSubtreeB)
    then
        true
    else
        require_complete_switch [SubtreeA]
        (
            SubtreeA = empty
        ;
            SubtreeA = two(K, V, Left, Right),
            match_tree_against_stack(Left, !StackB),
            match_kv_against_stack(K, V, !StackB),
            match_tree_against_stack(Right, !StackB)
        ;
            SubtreeA = three(K1, V1, K2, V2, Left, Middle, Right),
            match_tree_against_stack(Left, !StackB),
            match_kv_against_stack(K1, V1, !StackB),
            match_tree_against_stack(Middle, !StackB),
            match_kv_against_stack(K2, V2, !StackB),
            match_tree_against_stack(Right, !StackB)
        ;
            SubtreeA = four(K1, V1, K2, V2, K3, V3,
                Left, MidLeft, MidRight, Right),
            match_tree_against_stack(Left, !StackB),
            match_kv_against_stack(K1, V1, !StackB),
            match_tree_against_stack(MidLeft, !StackB),
            match_kv_against_stack(K2, V2, !StackB),
            match_tree_against_stack(MidRight, !StackB),
            match_kv_against_stack(K3, V3, !StackB),
            match_tree_against_stack(Right, !StackB)
        )
    ).

    % match_kv_against_stack(KA, VA, !StackB)
    %
    % Match a key-value pair against the leftmost key-value pair in the
    % stack of subtrees.  The first item on the stack is the leftmost
    % subtree.  The matched key-value pair is removed from the stack.
    %
:- pred match_kv_against_stack(K::in, V::in,
    list(tree234(K, V))::in(list(tree234_nonempty)),
    list(tree234(K, V))::out(list(tree234_nonempty))) is semidet.

match_kv_against_stack(KA, VA, !StackB) :-
    !.StackB = [LeftmostB | !:StackB],
    match_kv_against_subtree_and_stack(KA, VA, LeftmostB, !StackB).

    % match_kv_against_subtree_and_stack(KA, VA, LeftmostB, !StackB)
    %
    % Like match_kv_against_stack/4 except that LeftmostB is the subtree to
    % the left of !.StackB.
    %
:- pred match_kv_against_subtree_and_stack(K::in, V::in,
    tree234(K, V)::in(tree234_nonempty),
    list(tree234(K, V))::in(list(tree234_nonempty)),
    list(tree234(K, V))::out(list(tree234_nonempty))) is semidet.

match_kv_against_subtree_and_stack(KA, VA, LeftmostB, !StackB) :-
    require_complete_switch [LeftmostB]
    (
        LeftmostB = two(KB1, VB1, Left, Right),
        require_complete_switch [Left]
        (
            Left = empty,
            % Try to match the items.
            KA = KB1,
            VA = VB1,
            % Update stack and return.
            (
                ( Right = two(_, _, _, _)
                ; Right = three(_, _, _, _, _, _, _)
                ; Right = four(_, _, _, _, _, _, _, _, _, _)
                ),
                !:StackB = [Right | !.StackB]
            ;
                Right = empty
            )
        ;
            ( Left = two(_, _, _, _)
            ; Left = three(_, _, _, _, _, _, _)
            ; Left = four(_, _, _, _, _, _, _, _, _, _)
            ),
            % Break up this node and recurse
            !:StackB = [two(KB1, VB1, empty, Right) | !.StackB],
            match_kv_against_subtree_and_stack(KA, VA, Left, !StackB)
        )
    ;
        LeftmostB = three(KB1, VB1, KB2, VB2, Left, Middle, Right),
        % This case has the same structure as the one above.

        require_complete_switch [Left]
        (
            Left = empty,
            KA = KB1,
            VA = VB1,

            % We've eliminated Left and VB1-VB2, so we can build a two node
            % from the rest of the three node.
            !:StackB = [two(KB2, VB2, Middle, Right) | !.StackB]
        ;
            ( Left = two(_, _, _, _)
            ; Left = three(_, _, _, _, _, _, _)
            ; Left = four(_, _, _, _, _, _, _, _, _, _)
            ),
            !:StackB = [three(KB1, VB1, KB2, VB2, empty, Middle, Right) |
                !.StackB],
            % Pull the leftmost node out and try to match on it.
            match_kv_against_subtree_and_stack(KA, VA, Left, !StackB)
        )
    ;
        LeftmostB = four(KB1, VB1, KB2, VB2, KB3, VB3,
            Left, MidLeft, MidRight, Right),
        % This case has the same structure as the previous two cases.

        require_complete_switch [Left]
        (
            Left = empty,
            KA = KB1,
            VA = VB1,
            !:StackB = [three(KB2, VB2, KB3, VB3, MidLeft, MidRight, Right) |
                !.StackB]
        ;
            ( Left = two(_, _, _, _)
            ; Left = three(_, _, _, _, _, _, _)
            ; Left = four(_, _, _, _, _, _, _, _, _, _)
            ),
            !:StackB = [four(KB1, VB1, KB2, VB2, KB3, VB3,
                    empty, MidLeft, MidRight, Right)
                | !.StackB],
            match_kv_against_subtree_and_stack(KA, VA, Left, !StackB)
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

member(empty, _K, _V) :- fail.
member(two(K0, V0, T0, T1), K, V) :-
    (
        K = K0,
        V = V0
    ;
        tree234.member(T0, K, V)
    ;
        tree234.member(T1, K, V)
    ).
member(three(K0, V0, K1, V1, T0, T1, T2), K, V) :-
    (
        K = K0,
        V = V0
    ;
        K = K1,
        V = V1
    ;
        tree234.member(T0, K, V)
    ;
        tree234.member(T1, K, V)
    ;
        tree234.member(T2, K, V)
    ).
member(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V) :-
    (
        K = K0,
        V = V0
    ;
        K = K1,
        V = V1
    ;
        K = K2,
        V = V2
    ;
        tree234.member(T0, K, V)
    ;
        tree234.member(T1, K, V)
    ;
        tree234.member(T2, K, V)
    ;
        tree234.member(T3, K, V)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

search(T, K, V) :-
    (
        T = empty,
        fail
    ;
        T = two(K0, V0, T0, T1),
        compare(Result, K, K0),
        (
            Result = (<),
            tree234.search(T0, K, V)
        ;
            Result = (=),
            V = V0
        ;
            Result = (>),
            tree234.search(T1, K, V)
        )
    ;
        T = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            tree234.search(T0, K, V)
        ;
            Result0 = (=),
            V = V0
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                tree234.search(T1, K, V)
            ;
                Result1 = (=),
                V = V1
            ;
                Result1 = (>),
                tree234.search(T2, K, V)
            )
        )
    ;
        T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                tree234.search(T0, K, V)
            ;
                Result0 = (=),
                V = V0
            ;
                Result0 = (>),
                tree234.search(T1, K, V)
            )
        ;
            Result1 = (=),
            V = V1
        ;
            Result1 = (>),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                tree234.search(T2, K, V)
            ;
                Result2 = (=),
                V = V2
            ;
                Result2 = (>),
                tree234.search(T3, K, V)
            )
        )
    ).


lookup(T, K) = V :-
    tree234.lookup(T, K, V).

lookup(T, K, V) :-
    ( if tree234.search(T, K, V0) then
        V = V0
    else
        report_lookup_error("tree234.lookup: key not found.", K, V)
    ).

%---------------------------------------------------------------------------%

lower_bound_search(T, SearchK, K, V) :-
    (
        T = empty,
        fail
    ;
        T = two(K0, V0, T0, T1),
        compare(Result, SearchK, K0),
        (
            Result = (<),
            tree234.lower_bound_search(T0, SearchK, K, V)
        ;
            Result = (=),
            K = SearchK,
            V = V0
        ;
            Result = (>),
            ( if tree234.lower_bound_search(T1, SearchK, Kp, Vp) then
                K = Kp,
                V = Vp
            else
                T = two(_, V0, _, _),
                K = K0,
                V = V0
            )
        )
    ;
        T = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, SearchK, K0),
        (
            Result0 = (<),
            tree234.lower_bound_search(T0, SearchK, K, V)
        ;
            Result0 = (=),
            K = SearchK,
            V = V0
        ;
            Result0 = (>),
            compare(Result1, SearchK, K1),
            (
                Result1 = (<),
                ( if tree234.lower_bound_search(T1, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    T = three(_, V0, _, _, _, _, _),
                    K = K0,
                    V = V0
                )
            ;
                Result1 = (=),
                K = SearchK,
                V = V1
            ;
                Result1 = (>),
                ( if tree234.lower_bound_search(T2, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K1,
                    V = V1
                )
            )
        )
    ;
        T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, SearchK, K1),
        (
            Result1 = (<),
            compare(Result0, SearchK, K0),
            (
                Result0 = (<),
                tree234.lower_bound_search(T0, SearchK, K, V)
            ;
                Result0 = (=),
                K = SearchK,
                V = V0
            ;
                Result0 = (>),
                ( if tree234.lower_bound_search(T1, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K0,
                    V = V0
                )
            )
        ;
            Result1 = (=),
            K = SearchK,
            V = V1
        ;
            Result1 = (>),
            compare(Result2, SearchK, K2),
            (
                Result2 = (<),
                ( if tree234.lower_bound_search(T2, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K1,
                    V = V1
                )
            ;
                Result2 = (=),
                K = SearchK,
                V = V2
            ;
                Result2 = (>),
                ( if tree234.lower_bound_search(T3, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K2,
                    V = V2
                )
            )
        )
    ).

lower_bound_lookup(T, SearchK, K, V) :-
    ( if tree234.lower_bound_search(T, SearchK, K0, V0) then
        K = K0,
        V = V0
    else
        report_lookup_error("tree234.lower_bound_lookup: key not found.",
            SearchK, V)
    ).

%---------------------------------------------------------------------------%

upper_bound_search(T, SearchK, K, V) :-
    (
        T = empty,
        fail
    ;
        T = two(K0, V0, T0, T1),
        compare(Result, SearchK, K0),
        (
            Result = (<),
            ( if tree234.upper_bound_search(T0, SearchK, Kp, Vp) then
                K = Kp,
                V = Vp
            else
                T = two(_, V0, _, _),
                K = K0,
                V = V0
            )
        ;
            Result = (=),
            K = SearchK,
            V = V0
        ;
            Result = (>),
            tree234.upper_bound_search(T1, SearchK, K, V)
        )
    ;
        T = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, SearchK, K0),
        (
            Result0 = (<),
            ( if tree234.upper_bound_search(T0, SearchK, Kp, Vp) then
                K = Kp,
                V = Vp
            else
                K = K0,
                V = V0
            )
        ;
            Result0 = (=),
            K = SearchK,
            V = V0
        ;
            Result0 = (>),
            compare(Result1, SearchK, K1),
            (
                Result1 = (<),
                ( if tree234.upper_bound_search(T1, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K1,
                    V = V1
                )
            ;
                Result1 = (=),
                K = SearchK,
                V = V1
            ;
                Result1 = (>),
                tree234.upper_bound_search(T2, SearchK, K, V)
            )
        )
    ;
        T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, SearchK, K1),
        (
            Result1 = (<),
            compare(Result0, SearchK, K0),
            (
                Result0 = (<),
                ( if tree234.upper_bound_search(T0, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K0,
                    V = V0
                )
            ;
                Result0 = (=),
                K = SearchK,
                V = V0
            ;
                Result0 = (>),
                ( if tree234.upper_bound_search(T1, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K1,
                    V = V1
                )
            )
        ;
            Result1 = (=),
            K = SearchK,
            V = V1
        ;
            Result1 = (>),
            compare(Result2, SearchK, K2),
            (
                Result2 = (<),
                ( if tree234.upper_bound_search(T2, SearchK, Kp, Vp) then
                    K = Kp,
                    V = Vp
                else
                    K = K2,
                    V = V2
                )
            ;
                Result2 = (=),
                K = SearchK,
                V = V2
            ;
                Result2 = (>),
                tree234.upper_bound_search(T3, SearchK, K, V)
            )
        )
    ).

upper_bound_lookup(T, SearchK, K, V) :-
    ( if tree234.upper_bound_search(T, SearchK, K0, V0) then
        K = K0,
        V = V0
    else
        report_lookup_error("tree234.upper_bound_lookup: key not found.",
            SearchK, V)
    ).

%---------------------------------------------------------------------------%

max_key(T0) = MaxKey :-
    ( T0 = two(NodeMaxKey, _, _, NodeMaxSubtree)
    ; T0 = three(_, _, NodeMaxKey, _, _, _, NodeMaxSubtree)
    ; T0 = four(_, _, _, _, NodeMaxKey, _, _, _, _, NodeMaxSubtree)
    ),
    ( if MaxSubtreeKey = tree234.max_key(NodeMaxSubtree) then
        MaxKey = MaxSubtreeKey
    else
        MaxKey = NodeMaxKey
    ).

min_key(T0) = MinKey :-
    ( T0 = two(NodeMinKey, _, NodeMinSubtree, _)
    ; T0 = three(NodeMinKey, _, _, _, NodeMinSubtree, _, _)
    ; T0 = four(NodeMinKey, _, _, _, _, _, NodeMinSubtree, _, _, _)
    ),
    ( if MinSubtreeKey = tree234.min_key(NodeMinSubtree) then
        MinKey = MinSubtreeKey
    else
        MinKey = NodeMinKey
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

% tree234.insert is implemented using the simple top-down approach described
% in Sedgewick that splits 4-nodes into two 2-nodes on the downward traversal
% of the tree as we search for the right place to insert the new key-value
% pair. We know we have the right place if the subtrees of the node are
% empty (in which case we expand the node - which will always work because
% we have already split 4-nodes into 2-nodes), or if the tree itself is
% empty. This algorithm is O(lgN).

insert(K, V, Tin, Tout) :-
    (
        Tin = empty,
        Tout = two(K, V, empty, empty)
    ;
        Tin = two(_, _, _, _),
        tree234.insert2(Tin, K, V, Tout)
    ;
        Tin = three(_, _, _, _, _, _, _),
        tree234.insert3(Tin, K, V, Tout)
    ;
        Tin = four(_, _, _, _, _, _, _, _, _, _),
        tree234.split_four(Tin, MidK, MidV, Sub0, Sub1),
        compare(Result1, K, MidK),
        (
            Result1 = (<),
            tree234.insert2(Sub0, K, V, NewSub0),
            Tout = two(MidK, MidV, NewSub0, Sub1)
        ;
            Result1 = (=),
            fail
        ;
            Result1 = (>),
            tree234.insert2(Sub1, K, V, NewSub1),
            Tout = two(MidK, MidV, Sub0, NewSub1)
        )
    ).

:- pred insert2(tree234(K, V), K, V, tree234(K, V)).
% :- mode insert2(di_two, di, di, uo) is semidet.
% :- mode insert2(sdi_two, in, in, uo_tree234) is semidet.
:- mode insert2(in_two, in, in, out) is semidet.

insert2(two(K0, V0, T0, T1), K, V, Tout) :-
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty.
    then
        compare(Result, K, K0),
        (
            Result = (<),
            Tout = three(K, V, K0, V0, empty, empty, empty)
        ;
            Result = (=),
            fail
        ;
            Result = (>),
            Tout = three(K0, V0, K, V, empty, empty, empty)
        )
    else
        compare(Result, K, K0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T0, MT0K, MT0V, T00, T01),
                compare(Result1, K, MT0K),
                (
                    Result1 = (<),
                    tree234.insert2(T00, K, V, NewT00),
                    Tout = three(MT0K, MT0V, K0, V0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    tree234.insert2(T01, K, V, NewT01),
                    Tout = three(MT0K, MT0V, K0, V0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _, _, _),
                tree234.insert3(T0, K, V, NewT0),
                Tout = two(K0, V0, NewT0, T1)
            ;
                T0 = two(_, _, _, _),
                tree234.insert2(T0, K, V, NewT0),
                Tout = two(K0, V0, NewT0, T1)
            ;
                T0 = empty,
                NewT0 = two(K, V, empty, empty),
                Tout = two(K0, V0, NewT0, T1)
            )
        ;
            Result = (=),
            fail
        ;
            Result = (>),
            (
                T1 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T1, MT1K, MT1V, T10, T11),
                compare(Result1, K, MT1K),
                (
                    Result1 = (<),
                    tree234.insert2(T10, K, V, NewT10),
                    Tout = three(K0, V0, MT1K, MT1V, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    fail
                ;
                    Result1 = (>),
                    tree234.insert2(T11, K, V, NewT11),
                    Tout = three(K0, V0, MT1K, MT1V, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _, _, _),
                tree234.insert3(T1, K, V, NewT1),
                Tout = two(K0, V0, T0, NewT1)
            ;
                T1 = two(_, _, _, _),
                tree234.insert2(T1, K, V, NewT1),
                Tout = two(K0, V0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(K, V, empty, empty),
                Tout = two(K0, V0, T0, NewT1)
            )
        )
    ).

:- pred insert3(tree234(K, V), K, V, tree234(K, V)).
% :- mode insert3(di_three, di, di, uo) is semidet.
% :- mode insert3(sdi_three, in, in, uo_tree234) is semidet.
:- mode insert3(in_three, in, in, out) is semidet.

insert3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty.
        % T2 = empty implied by T0 = empty.
    then
        compare(Result0, K, K0),
        (
            Result0 = (<),
            Tout = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty)
        ;
            Result0 = (=),
            fail
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                Tout = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty)
            ;
                Result1 = (=),
                fail
            ;
                Result1 = (>),
                Tout = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty)
            )
        )
    else
        compare(Result0, K, K0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T0, MT0K, MT0V, T00, T01),
                compare(ResultM, K, MT0K),
                (
                    ResultM = (<),
                    tree234.insert2(T00, K, V, NewT00),
                    Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                        NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    fail
                ;
                    ResultM = (>),
                    tree234.insert2(T01, K, V, NewT01),
                    Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                        T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _, _, _),
                tree234.insert3(T0, K, V, NewT0),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _, _),
                tree234.insert2(T0, K, V, NewT0),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            ;
                T0 = empty,
                NewT0 = two(K, V, empty, empty),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            )
        ;
            Result0 = (=),
            fail
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                (
                    T1 = four(_, _, _, _, _, _, _, _, _, _),
                    tree234.split_four(T1, MT1K, MT1V, T10, T11),
                    compare(ResultM, K, MT1K),
                    (
                        ResultM = (<),
                        tree234.insert2(T10, K, V, NewT10),
                        Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                            T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        fail
                    ;
                        ResultM = (>),
                        tree234.insert2(T11, K, V, NewT11),
                        Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                            T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _, _, _),
                    tree234.insert3(T1, K, V, NewT1),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _, _),
                    tree234.insert2(T1, K, V, NewT1),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                ;
                    T1 = empty,
                    NewT1 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                )
            ;
                Result1 = (=),
                fail
            ;
                Result1 = (>),
                (
                    T2 = four(_, _, _, _, _, _, _, _, _, _),
                    tree234.split_four(T2, MT2K, MT2V,
                        T20, T21),
                    compare(ResultM, K, MT2K),
                    (
                        ResultM = (<),
                        tree234.insert2(T20, K, V, NewT20),
                        Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                            T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        fail
                    ;
                        ResultM = (>),
                        tree234.insert2(T21, K, V, NewT21),
                        Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                            T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _, _, _),
                    tree234.insert3(T2, K, V, NewT2),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _, _),
                    tree234.insert2(T2, K, V, NewT2),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

search_insert(K, V, MaybeOldV, Tin, Tout) :-
    (
        Tin = empty,
        MaybeOldV = no,
        Tout = two(K, V, empty, empty)
    ;
        Tin = two(_, _, _, _),
        tree234.search_insert2(Tin, K, V, MaybeOldV, Tout)
    ;
        Tin = three(_, _, _, _, _, _, _),
        tree234.search_insert3(Tin, K, V, MaybeOldV, Tout)
    ;
        Tin = four(_, _, _, _, _, _, _, _, _, _),
        tree234.split_four(Tin, MidK, MidV, Sub0, Sub1),
        compare(Result1, K, MidK),
        (
            Result1 = (<),
            tree234.search_insert2(Sub0, K, V, MaybeOldV, NewSub0),
            (
                MaybeOldV = no,
                Tout = two(MidK, MidV, NewSub0, Sub1)
            ;
                MaybeOldV = yes(_),
                Tout = Tin
            )
        ;
            Result1 = (=),
            MaybeOldV = yes(MidV),
            Tout = Tin
        ;
            Result1 = (>),
            tree234.search_insert2(Sub1, K, V, MaybeOldV, NewSub1),
            (
                MaybeOldV = no,
                Tout = two(MidK, MidV, Sub0, NewSub1)
            ;
                MaybeOldV = yes(_),
                Tout = Tin
            )
        )
    ).

:- pred search_insert2(tree234(K, V), K, V, maybe(V), tree234(K, V)).
:- mode search_insert2(in_two, in, in, out, out) is det.

search_insert2(Tin, K, V, MaybeOldV, Tout) :-
    Tin = two(K0, V0, T0, T1),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty.
    then
        compare(Result, K, K0),
        (
            Result = (<),
            MaybeOldV = no,
            Tout = three(K, V, K0, V0, empty, empty, empty)
        ;
            Result = (=),
            MaybeOldV = yes(V0),
            Tout = Tin
        ;
            Result = (>),
            MaybeOldV = no,
            Tout = three(K0, V0, K, V, empty, empty, empty)
        )
    else
        compare(Result, K, K0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T0, MT0K, MT0V, T00, T01),
                compare(Result1, K, MT0K),
                (
                    Result1 = (<),
                    tree234.search_insert2(T00, K, V, MaybeOldV, NewT00),
                    (
                        MaybeOldV = no,
                        Tout = three(MT0K, MT0V, K0, V0, NewT00, T01, T1)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                ;
                    Result1 = (=),
                    MaybeOldV = yes(MT0V),
                    Tout = Tin
                ;
                    Result1 = (>),
                    tree234.search_insert2(T01, K, V, MaybeOldV, NewT01),
                    (
                        MaybeOldV = no,
                        Tout = three(MT0K, MT0V, K0, V0, T00, NewT01, T1)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                )
            ;
                (
                    T0 = three(_, _, _, _, _, _, _),
                    tree234.search_insert3(T0, K, V, MaybeOldV, NewT0)
                ;
                    T0 = two(_, _, _, _),
                    tree234.search_insert2(T0, K, V, MaybeOldV, NewT0)
                ),
                (
                    MaybeOldV = no,
                    Tout = two(K0, V0, NewT0, T1)
                ;
                    MaybeOldV = yes(_),
                    Tout = Tin
                )
            ;
                T0 = empty,
                MaybeOldV = no,
                NewT0 = two(K, V, empty, empty),
                Tout = two(K0, V0, NewT0, T1)
            )
        ;
            Result = (=),
            MaybeOldV = yes(V0),
            Tout = Tin
        ;
            Result = (>),
            (
                T1 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T1, MT1K, MT1V, T10, T11),
                compare(Result1, K, MT1K),
                (
                    Result1 = (<),
                    tree234.search_insert2(T10, K, V, MaybeOldV, NewT10),
                    (
                        MaybeOldV = no,
                        Tout = three(K0, V0, MT1K, MT1V, T0, NewT10, T11)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                ;
                    Result1 = (=),
                    MaybeOldV = yes(MT1V),
                    Tout = Tin
                ;
                    Result1 = (>),
                    tree234.search_insert2(T11, K, V, MaybeOldV, NewT11),
                    (
                        MaybeOldV = no,
                        Tout = three(K0, V0, MT1K, MT1V, T0, T10, NewT11)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                )
            ;
                (
                    T1 = three(_, _, _, _, _, _, _),
                    tree234.search_insert3(T1, K, V, MaybeOldV, NewT1)
                ;
                    T1 = two(_, _, _, _),
                    tree234.search_insert2(T1, K, V, MaybeOldV, NewT1)
                ),
                (
                    MaybeOldV = no,
                    Tout = two(K0, V0, T0, NewT1)
                ;
                    MaybeOldV = yes(_),
                    Tout = Tin
                )
            ;
                T1 = empty,
                MaybeOldV = no,
                NewT1 = two(K, V, empty, empty),
                Tout = two(K0, V0, T0, NewT1)
            )
        )
    ).

:- pred search_insert3(tree234(K, V), K, V, maybe(V), tree234(K, V)).
:- mode search_insert3(in_three, in, in, out, out) is det.

search_insert3(Tin, K, V, MaybeOldV, Tout) :-
    Tin = three(K0, V0, K1, V1, T0, T1, T2),
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty.
        % T2 = empty implied by T0 = empty.
    then
        compare(Result0, K, K0),
        (
            Result0 = (<),
            MaybeOldV = no,
            Tout = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty)
        ;
            Result0 = (=),
            MaybeOldV = yes(V0),
            Tout = Tin
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                MaybeOldV = no,
                Tout = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty)
            ;
                Result1 = (=),
                MaybeOldV = yes(V1),
                Tout = Tin
            ;
                Result1 = (>),
                MaybeOldV = no,
                Tout = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty)
            )
        )
    else
        compare(Result0, K, K0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T0, MT0K, MT0V, T00, T01),
                compare(ResultM, K, MT0K),
                (
                    ResultM = (<),
                    tree234.search_insert2(T00, K, V, MaybeOldV, NewT00),
                    (
                        MaybeOldV = no,
                        Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                            NewT00, T01, T1, T2)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                ;
                    ResultM = (=),
                    MaybeOldV = yes(MT0V),
                    Tout = Tin
                ;
                    ResultM = (>),
                    tree234.search_insert2(T01, K, V, MaybeOldV, NewT01),
                    (
                        MaybeOldV = no,
                        Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                            T00, NewT01, T1, T2)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                )
            ;
                (
                    T0 = three(_, _, _, _, _, _, _),
                    tree234.search_insert3(T0, K, V, MaybeOldV, NewT0)
                ;
                    T0 = two(_, _, _, _),
                    tree234.search_insert2(T0, K, V, MaybeOldV, NewT0)
                ),
                (
                    MaybeOldV = no,
                    Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
                ;
                    MaybeOldV = yes(_),
                    Tout = Tin
                )
            ;
                T0 = empty,
                MaybeOldV = no,
                NewT0 = two(K, V, empty, empty),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            )
        ;
            Result0 = (=),
            MaybeOldV = yes(V0),
            Tout = Tin
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                (
                    T1 = four(_, _, _, _, _, _, _, _, _, _),
                    tree234.split_four(T1, MT1K, MT1V, T10, T11),
                    compare(ResultM, K, MT1K),
                    (
                        ResultM = (<),
                        tree234.search_insert2(T10, K, V, MaybeOldV, NewT10),
                        (
                            MaybeOldV = no,
                            Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                                T0, NewT10, T11, T2)
                        ;
                            MaybeOldV = yes(_),
                            Tout = Tin
                        )
                    ;
                        ResultM = (=),
                        MaybeOldV = yes(MT1V),
                        Tout = Tin
                    ;
                        ResultM = (>),
                        tree234.search_insert2(T11, K, V, MaybeOldV, NewT11),
                        (
                            MaybeOldV = no,
                            Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                                T0, T10, NewT11, T2)
                        ;
                            MaybeOldV = yes(_),
                            Tout = Tin
                        )
                    )
                ;
                    (
                        T1 = three(_, _, _, _, _, _, _),
                        tree234.search_insert3(T1, K, V, MaybeOldV, NewT1)
                    ;
                        T1 = two(_, _, _, _),
                        tree234.search_insert2(T1, K, V, MaybeOldV, NewT1)
                    ),
                    (
                        MaybeOldV = no,
                        Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                ;
                    T1 = empty,
                    MaybeOldV = no,
                    NewT1 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                )
            ;
                Result1 = (=),
                MaybeOldV = yes(V1),
                Tout = Tin
            ;
                Result1 = (>),
                (
                    T2 = four(_, _, _, _, _, _, _, _, _, _),
                    tree234.split_four(T2, MT2K, MT2V, T20, T21),
                    compare(ResultM, K, MT2K),
                    (
                        ResultM = (<),
                        tree234.search_insert2(T20, K, V, MaybeOldV, NewT20),
                        (
                            MaybeOldV = no,
                            Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                                T0, T1, NewT20, T21)
                        ;
                            MaybeOldV = yes(_),
                            Tout = Tin
                        )
                    ;
                        ResultM = (=),
                        MaybeOldV = yes(MT2V),
                        Tout = Tin
                    ;
                        ResultM = (>),
                        tree234.search_insert2(T21, K, V, MaybeOldV, NewT21),
                        (
                            MaybeOldV = no,
                            Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                                T0, T1, T20, NewT21)
                        ;
                            MaybeOldV = yes(_),
                            Tout = Tin
                        )
                    )
                ;
                    (
                        T2 = three(_, _, _, _, _, _, _),
                        tree234.search_insert3(T2, K, V, MaybeOldV, NewT2)
                    ;
                        T2 = two(_, _, _, _),
                        tree234.search_insert2(T2, K, V, MaybeOldV, NewT2)
                    ),
                    (
                        MaybeOldV = no,
                        Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                    ;
                        MaybeOldV = yes(_),
                        Tout = Tin
                    )
                ;
                    T2 = empty,
                    MaybeOldV = no,
                    NewT2 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

update(K, V, Tin, Tout) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(K0, V0, T0, T1),
        compare(Result, K, K0),
        (
            Result = (<),
            tree234.update(K, V, T0, NewT0),
            Tout = two(K0, V0, NewT0, T1)
        ;
            Result = (=),
            Tout = two(K0, V, T0, T1)
        ;
            Result = (>),
            tree234.update(K, V, T1, NewT1),
            Tout = two(K0, V0, T0, NewT1)
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            tree234.update(K, V, T0, NewT0),
            Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
        ;
            Result0 = (=),
            Tout = three(K0, V, K1, V1, T0, T1, T2)
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                tree234.update(K, V, T1, NewT1),
                Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
            ;
                Result1 = (=),
                Tout = three(K0, V0, K1, V, T0, T1, T2)
            ;
                Result1 = (>),
                tree234.update(K, V, T2, NewT2),
                Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                tree234.update(K, V, T0, NewT0),
                Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3)
            ;
                Result0 = (=),
                Tout = four(K0, V, K1, V1, K2, V2, T0, T1, T2, T3)
            ;
                Result0 = (>),
                tree234.update(K, V, T1, NewT1),
                Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3)
            )
        ;
            Result1 = (=),
            Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
        ;
            Result1 = (>),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                tree234.update(K, V, T2, NewT2),
                Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3)
            ;
                Result2 = (=),
                Tout = four(K0, V0, K1, V1, K2, V, T0, T1, T2, T3)
            ;
                Result2 = (>),
                tree234.update(K, V, T3, NewT3),
                Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3)
            )
        )
    ).

%---------------------------------------------------------------------------%

set(!.T, K, V) = !:T :-
    tree234.set(K, V, !T).

set(K, V, Tin, Tout) :-
    % tree234.set uses the same algorithm as used for tree234.insert,
    % except that instead of failing for equal keys, we replace the value.
    (
        Tin = empty,
        Tout = two(K, V, empty, empty)
    ;
        Tin = two(_, _, _, _),
        tree234.set2(Tin, K, V, Tout)
    ;
        Tin = three(_, _, _, _, _, _, _),
        tree234.set3(Tin, K, V, Tout)
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            Sub0 = two(K0, V0, T0, T1),
            Sub1 = two(K2, V2, T2, T3),
            tree234.set2(Sub0, K, V, NewSub0),
            Tout = two(K1, V1, NewSub0, Sub1)
        ;
            Result1 = (=),
            Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
        ;
            Result1 = (>),
            Sub0 = two(K0, V0, T0, T1),
            Sub1 = two(K2, V2, T2, T3),
            tree234.set2(Sub1, K, V, NewSub1),
            Tout = two(K1, V1, Sub0, NewSub1)
        )
    ).

:- pred set2(tree234(K, V), K, V, tree234(K, V)).
:- mode set2(di_two, di, di, uo) is det.
% :- mode set2(sdi_two, in, in, uo_tree234) is det.
:- mode set2(in_two, in, in, out) is det.
:- pragma type_spec(set2(in_two, in, in, out), K = var(_)).

set2(two(K0, V0, T0, T1), K, V, Tout) :-
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty.
    then
        compare(Result, K, K0),
        (
            Result = (<),
            Tout = three(K, V, K0, V0, empty, empty, empty)
        ;
            Result = (=),
            Tout = two(K, V, T0, T1)
        ;
            Result = (>),
            Tout = three(K0, V0, K, V, empty, empty, empty)
        )
    else
        compare(Result, K, K0),
        (
            Result = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T0, MT0K, MT0V, T00, T01),
                compare(Result1, K, MT0K),
                (
                    Result1 = (<),
                    tree234.set2(T00, K, V, NewT00),
                    Tout = three(MT0K, MT0V, K0, V0, NewT00, T01, T1)
                ;
                    Result1 = (=),
                    Tout = three(MT0K, V, K0, V0, T00, T01, T1)
                ;
                    Result1 = (>),
                    tree234.set2(T01, K, V, NewT01),
                    Tout = three(MT0K, MT0V, K0, V0, T00, NewT01, T1)
                )
            ;
                T0 = three(_, _, _, _, _, _, _),
                tree234.set3(T0, K, V, NewT0),
                Tout = two(K0, V0, NewT0, T1)
            ;
                T0 = two(_, _, _, _),
                tree234.set2(T0, K, V, NewT0),
                Tout = two(K0, V0, NewT0, T1)
            ;
                T0 = empty,
                NewT0 = two(K, V, empty, empty),
                Tout = two(K0, V0, NewT0, T1)
            )
        ;
            Result = (=),
            Tout = two(K, V, T0, T1)
        ;
            Result = (>),
            (
                T1 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T1, MT1K, MT1V, T10, T11),
                compare(Result1, K, MT1K),
                (
                    Result1 = (<),
                    tree234.set2(T10, K, V, NewT10),
                    Tout = three(K0, V0, MT1K, MT1V, T0, NewT10, T11)
                ;
                    Result1 = (=),
                    Tout = three(K0, V0, MT1K, V, T0, T10, T11)
                ;
                    Result1 = (>),
                    tree234.set2(T11, K, V, NewT11),
                    Tout = three(K0, V0, MT1K, MT1V, T0, T10, NewT11)
                )
            ;
                T1 = three(_, _, _, _, _, _, _),
                tree234.set3(T1, K, V, NewT1),
                Tout = two(K0, V0, T0, NewT1)
            ;
                T1 = two(_, _, _, _),
                tree234.set2(T1, K, V, NewT1),
                Tout = two(K0, V0, T0, NewT1)
            ;
                T1 = empty,
                NewT1 = two(K, V, empty, empty),
                Tout = two(K0, V0, T0, NewT1)
            )
        )
    ).

:- pred set3(tree234(K, V), K, V, tree234(K, V)).
:- mode set3(di_three, di, di, uo) is det.
% :- mode set3(sdi_three, in, in, uo_tree234) is det.
:- mode set3(in_three, in, in, out) is det.
:- pragma type_spec(set3(in_three, in, in, out), K = var(_)).

set3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
    ( if
        T0 = empty
        % T1 = empty implied by T0 = empty
        % T2 = empty implied by T0 = empty
    then
        compare(Result0, K, K0),
        (
            Result0 = (<),
            Tout = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty)
        ;
            Result0 = (=),
            Tout = three(K0, V, K1, V1, empty, empty, empty)
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                Tout = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty)
            ;
                Result1 = (=),
                Tout = three(K0, V0, K1, V, empty, empty, empty)
            ;
                Result1 = (>),
                Tout = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty)
            )
        )
    else
        compare(Result0, K, K0),
        (
            Result0 = (<),
            (
                T0 = four(_, _, _, _, _, _, _, _, _, _),
                tree234.split_four(T0, MT0K, MT0V, T00, T01),
                compare(ResultM, K, MT0K),
                (
                    ResultM = (<),
                    tree234.set2(T00, K, V, NewT00),
                    Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                        NewT00, T01, T1, T2)
                ;
                    ResultM = (=),
                    Tout = four(MT0K, V, K0, V0, K1, V1, T00, T01, T1, T2)
                ;
                    ResultM = (>),
                    tree234.set2(T01, K, V, NewT01),
                    Tout = four(MT0K, MT0V, K0, V0, K1, V1,
                        T00, NewT01, T1, T2)
                )
            ;
                T0 = three(_, _, _, _, _, _, _),
                tree234.set3(T0, K, V, NewT0),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            ;
                T0 = two(_, _, _, _),
                tree234.set2(T0, K, V, NewT0),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            ;
                T0 = empty,
                NewT0 = two(K, V, empty, empty),
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
            )
        ;
            Result0 = (=),
            Tout = three(K0, V, K1, V1, T0, T1, T2)
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                (
                    T1 = four(_, _, _, _, _, _, _, _, _, _),
                    tree234.split_four(T1, MT1K, MT1V, T10, T11),
                    compare(ResultM, K, MT1K),
                    (
                        ResultM = (<),
                        tree234.set2(T10, K, V, NewT10),
                        Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                            T0, NewT10, T11, T2)
                    ;
                        ResultM = (=),
                        Tout = four(K0, V0, MT1K, V, K1, V1, T0, T10, T11, T2)
                    ;
                        ResultM = (>),
                        tree234.set2(T11, K, V, NewT11),
                        Tout = four(K0, V0, MT1K, MT1V, K1, V1,
                            T0, T10, NewT11, T2)
                    )
                ;
                    T1 = three(_, _, _, _, _, _, _),
                    tree234.set3(T1, K, V, NewT1),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                ;
                    T1 = two(_, _, _, _),
                    tree234.set2(T1, K, V, NewT1),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                ;
                    T1 = empty,
                    NewT1 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
                )
            ;
                Result1 = (=),
                Tout = three(K0, V0, K, V, T0, T1, T2)
            ;
                Result1 = (>),
                (
                    T2 = four(_, _, _, _, _, _, _, _, _, _),
                    tree234.split_four(T2, MT2K, MT2V, T20, T21),
                    compare(ResultM, K, MT2K),
                    (
                        ResultM = (<),
                        tree234.set2(T20, K, V, NewT20),
                        Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                            T0, T1, NewT20, T21)
                    ;
                        ResultM = (=),
                        Tout = four(K0, V0, K1, V1, MT2K, V, T0, T1, T20, T21)
                    ;
                        ResultM = (>),
                        tree234.set2(T21, K, V, NewT21),
                        Tout = four(K0, V0, K1, V1, MT2K, MT2V,
                            T0, T1, T20, NewT21)
                    )
                ;
                    T2 = three(_, _, _, _, _, _, _),
                    tree234.set3(T2, K, V, NewT2),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                ;
                    T2 = two(_, _, _, _),
                    tree234.set2(T2, K, V, NewT2),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                ;
                    T2 = empty,
                    NewT2 = two(K, V, empty, empty),
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
                )
            )
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

transform_value(P, K, Tin, Tout) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(K0, V0, T0, T1),
        compare(Result, K, K0),
        (
            Result = (<),
            tree234.transform_value(P, K, T0, NewT0),
            Tout = two(K0, V0, NewT0, T1)
        ;
            Result = (=),
            P(V0, VNew),
            Tout = two(K0, VNew, T0, T1)
        ;
            Result = (>),
            tree234.transform_value(P, K, T1, NewT1),
            Tout = two(K0, V0, T0, NewT1)
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            tree234.transform_value(P, K, T0, NewT0),
            Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
        ;
            Result0 = (=),
            P(V0, VNew),
            Tout = three(K0, VNew, K1, V1, T0, T1, T2)
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                tree234.transform_value(P, K, T1, NewT1),
                Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
            ;
                Result1 = (=),
                P(V1, VNew),
                Tout = three(K0, V0, K1, VNew, T0, T1, T2)
            ;
                Result1 = (>),
                tree234.transform_value(P, K, T2, NewT2),
                Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                tree234.transform_value(P, K, T0, NewT0),
                Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3)
            ;
                Result0 = (=),
                P(V0, VNew),
                Tout = four(K0, VNew, K1, V1, K2, V2, T0, T1, T2, T3)
            ;
                Result0 = (>),
                tree234.transform_value(P, K, T1, NewT1),
                Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3)
            )
        ;
            Result1 = (=),
            P(V1, VNew),
            Tout = four(K0, V0, K1, VNew, K2, V2, T0, T1, T2, T3)
        ;
            Result1 = (>),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                tree234.transform_value(P, K, T2, NewT2),
                Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3)
            ;
                Result2 = (=),
                P(V2, VNew),
                Tout = four(K0, V0, K1, V1, K2, VNew, T0, T1, T2, T3)
            ;
                Result2 = (>),
                tree234.transform_value(P, K, T3, NewT3),
                Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3)
            )
        )
    ).

%---------------------------------------------------------------------------%
%
% Utilities used by the insertion predicates.
%

:- pred split_four(tree234(K, V), K, V, tree234(K, V), tree234(K, V)).
:- mode split_four(di_four, uo, uo, uo_two, uo_two) is det.
% :- mode split_four(sdi_four, out, out, suo_two, suo_two) is det.
:- mode split_four(in_four, out, out, out_two, out_two) is det.

split_four(Tin, MidK, MidV, Sub0, Sub1) :-
    Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
    Sub0 = two(K0, V0, T0, T1),
    MidK = K1,
    MidV = V1,
    Sub1 = two(K2, V2, T2, T3).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

delete(!.T, K) = !:T :-
    tree234.delete(K, !T).

delete(K, Tin, Tout) :-
    tree234.delete_2(Tin, K, Tout, _).

    % When deleting an item from a tree, the height of the tree may be
    % reduced by one. The last argument says whether this has occurred.
    %
:- pred delete_2(tree234(K, V), K, tree234(K, V), bool).
%:- mode delete_2(di, in, uo, out) is det.
:- mode delete_2(in, in, out, out) is det.

delete_2(Tin, K, Tout, RH) :-
    (
        Tin = empty,
        Tout = empty,
        RH = no
    ;
        Tin = two(K0, V0, T0, T1),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            tree234.delete_2(T0, K, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(K0, V0, NewT0, T1),
                RH = no
            )
        ;
            Result0 = (=),
            ( if tree234.remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_2node_t1(ST1K, ST1V, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(ST1K, ST1V, T0, NewT1),
                    RH = no
                )
            else
                % T1 must be empty
                Tout = T0,
                RH = yes
            )
        ;
            Result0 = (>),
            tree234.delete_2(T1, K, NewT1, RHT1),
            (
                RHT1 = yes,
                fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
            ;
                RHT1 = no,
                Tout = two(K0, V0, T0, NewT1),
                RH = no
            )
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            tree234.delete_2(T0, K, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                RH = no
            )
        ;
            Result0 = (=),
            ( if tree234.remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_3node_t1(ST1K, ST1V, K1, V1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(ST1K, ST1V, K1, V1, T0, NewT1, T2),
                    RH = no
                )
            else
                % T1 must be empty
                Tout = two(K1, V1, T0, T2),
                RH = no
            )
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                tree234.delete_2(T1, K, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_3node_t1(K0, V0, K1, V1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2),
                    RH = no
                )
            ;
                Result1 = (=),
                ( if
                    tree234.remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2)
                then
                    (
                        RHT2 = yes,
                        fix_3node_t2(K0, V0, ST2K, ST2V, T0, T1, NewT2, Tout,
                            RH)
                    ;
                        RHT2 = no,
                        Tout = three(K0, V0, ST2K, ST2V, T0, T1, NewT2),
                        RH = no
                    )
                else
                    % T2 must be empty.
                    Tout = two(K0, V0, T0, T1),
                    RH = no
                )
            ;
                Result1 = (>),
                tree234.delete_2(T2, K, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_3node_t2(K0, V0, K1, V1, T0, T1, NewT2, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
                    RH = no
                )
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                tree234.delete_2(T0, K, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_4node_t0(K0, V0, K1, V1, K2, V2,
                        NewT0, T1, T2, T3, Tout, RH)
                ;
                    RHT0 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if
                    tree234.remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
                then
                    (
                        RHT1 = yes,
                        fix_4node_t1(ST1K, ST1V, K1, V1, K2, V2,
                            T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(ST1K, ST1V, K1, V1, K2, V2,
                            T0, NewT1, T2, T3),
                        RH = no
                    )
                else
                    % T1 must be empty.
                    Tout = three(K1, V1, K2, V2, T0, T2, T3),
                    RH = no
                )
            ;
                Result0 = (>),
                tree234.delete_2(T1, K, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_4node_t1(K0, V0, K1, V1, K2, V2,
                        T0, NewT1, T2, T3, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3),
                    RH = no
                )
            )
        ;
            Result1 = (=),
            ( if tree234.remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2) then
                (
                    RHT2 = yes,
                    fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2,
                        T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(K0, V0, ST2K, ST2V, K2, V2,
                        T0, T1, NewT2, T3),
                    RH = no
                )
            else
                % T2 must be empty
                Tout = three(K0, V0, K2, V2, T0, T1, T3),
                RH = no
            )
        ;
            Result1 = (>),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                tree234.delete_2(T2, K, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_4node_t2(K0, V0, K1, V1, K2, V2,
                        T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                Result2 = (=),
                ( if
                    tree234.remove_smallest_2(T3, ST3K, ST3V, NewT3, RHT3)
                then
                    (
                        RHT3 = yes,
                        fix_4node_t3(K0, V0, K1, V1, ST3K, ST3V,
                            T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(K0, V0, K1, V1, ST3K, ST3V,
                            T0, T1, T2, NewT3),
                        RH = no
                    )
                else
                    % T3 must be empty
                    Tout = three(K0, V0, K1, V1, T0, T1, T2),
                    RH = no
                )
            ;
                Result2 = (>),
                tree234.delete_2(T3, K, NewT3, RHT3),
                (
                    RHT3 = yes,
                    fix_4node_t3(K0, V0, K1, V1, K2, V2,
                        T0, T1, T2, NewT3, Tout, RH)
                ;
                    RHT3 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3),
                    RH = no
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

remove(K, V, Tin, Tout) :-
    % We use the same algorithm as tree234.delete.
    tree234.remove_2(Tin, K, V, Tout, _).

:- pred remove_2(tree234(K, V), K, V, tree234(K, V), bool).
%:- mode remove_2(di, in, uo, uo, out) is semidet.
:- mode remove_2(in, in, out, out, out) is semidet.

remove_2(Tin, K, V, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(K0, V0, T0, T1),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            tree234.remove_2(T0, K, V, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(K0, V0, NewT0, T1),
                RH = no
            )
        ;
            Result0 = (=),
            ( if tree234.remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_2node_t1(ST1K, ST1V, T0, NewT1, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = two(ST1K, ST1V, T0, NewT1),
                    RH = no
                )
            else
                % T1 must be empty.
                Tout = T0,
                RH = yes
            ),
            V = V0
        ;
            Result0 = (>),
            tree234.remove_2(T1, K, V, NewT1, RHT1),
            (
                RHT1 = yes,
                fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
            ;
                RHT1 = no,
                Tout = two(K0, V0, T0, NewT1),
                RH = no
            )
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        compare(Result0, K, K0),
        (
            Result0 = (<),
            tree234.remove_2(T0, K, V, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                RH = no
            )
        ;
            Result0 = (=),
            ( if tree234.remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1) then
                (
                    RHT1 = yes,
                    fix_3node_t1(ST1K, ST1V, K1, V1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(ST1K, ST1V, K1, V1, T0, NewT1, T2),
                    RH = no
                )
            else
                % T1 must be empty.
                Tout = two(K1, V1, T0, T2),
                RH = no
            ),
            V = V0
        ;
            Result0 = (>),
            compare(Result1, K, K1),
            (
                Result1 = (<),
                tree234.remove_2(T1, K, V, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_3node_t1(K0, V0, K1, V1, T0, NewT1, T2, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = three(K0, V0, K1, V1, T0, NewT1, T2),
                    RH = no
                )
            ;
                Result1 = (=),
                ( if
                    tree234.remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2)
                then
                    (
                        RHT2 = yes,
                        fix_3node_t2(K0, V0, ST2K, ST2V,
                            T0, T1, NewT2, Tout, RH)
                    ;
                        RHT2 = no,
                        Tout = three(K0, V0, ST2K, ST2V, T0, T1, NewT2),
                        RH = no
                    )
                else
                    % T2 must be empty.
                    Tout = two(K0, V0, T0, T1),
                    RH = no
                ),
                V = V1
            ;
                Result1 = (>),
                tree234.remove_2(T2, K, V, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_3node_t2(K0, V0, K1, V1, T0, T1, NewT2, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
                    RH = no
                )
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        compare(Result1, K, K1),
        (
            Result1 = (<),
            compare(Result0, K, K0),
            (
                Result0 = (<),
                tree234.remove_2(T0, K, V, NewT0, RHT0),
                (
                    RHT0 = yes,
                    fix_4node_t0(K0, V0, K1, V1, K2, V2,
                        NewT0, T1, T2, T3, Tout, RH)
                ;
                    RHT0 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3),
                    RH = no
                )
            ;
                Result0 = (=),
                ( if
                    tree234.remove_smallest_2(T1, ST1K, ST1V, NewT1, RHT1)
                then
                    (
                        RHT1 = yes,
                        fix_4node_t1(ST1K, ST1V, K1, V1, K2, V2,
                            T0, NewT1, T2, T3, Tout, RH)
                    ;
                        RHT1 = no,
                        Tout = four(ST1K, ST1V, K1, V1, K2, V2,
                            T0, NewT1, T2, T3),
                        RH = no
                    )
                else
                    % T1 must be empty.
                    Tout = three(K1, V1, K2, V2, T0, T2, T3),
                    RH = no
                ),
                V = V0
            ;
                Result0 = (>),
                tree234.remove_2(T1, K, V, NewT1, RHT1),
                (
                    RHT1 = yes,
                    fix_4node_t1(K0, V0, K1, V1, K2, V2,
                        T0, NewT1, T2, T3, Tout, RH)
                ;
                    RHT1 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3),
                    RH = no
                )
            )
        ;
            Result1 = (=),
            ( if tree234.remove_smallest_2(T2, ST2K, ST2V, NewT2, RHT2) then
                (
                    RHT2 = yes,
                    fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2,
                        T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(K0, V0, ST2K, ST2V, K2, V2, T0, T1, NewT2, T3),
                    RH = no
                )
            else
                % T2 must be empty.
                Tout = three(K0, V0, K2, V2, T0, T1, T3),
                RH = no
            ),
            V = V1
        ;
            Result1 = (>),
            compare(Result2, K, K2),
            (
                Result2 = (<),
                tree234.remove_2(T2, K, V, NewT2, RHT2),
                (
                    RHT2 = yes,
                    fix_4node_t2(K0, V0, K1, V1, K2, V2,
                        T0, T1, NewT2, T3, Tout, RH)
                ;
                    RHT2 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, NewT2, T3),
                    RH = no
                )
            ;
                Result2 = (=),
                ( if
                    tree234.remove_smallest_2(T3, ST3K, ST3V, NewT3, RHT3)
                then
                    (
                        RHT3 = yes,
                        fix_4node_t3(K0, V0, K1, V1, ST3K, ST3V,
                            T0, T1, T2, NewT3, Tout, RH)
                    ;
                        RHT3 = no,
                        Tout = four(K0, V0, K1, V1, ST3K, ST3V,
                            T0, T1, T2, NewT3),
                        RH = no
                    )
                else
                    % T3 must be empty.
                    Tout = three(K0, V0, K1, V1, T0, T1, T2),
                    RH = no
                ),
                V = V2
            ;
                Result2 = (>),
                tree234.remove_2(T3, K, V, NewT3, RHT3),
                (
                    RHT3 = yes,
                    fix_4node_t3(K0, V0, K1, V1, K2, V2,
                        T0, T1, T2, NewT3, Tout, RH)
                ;
                    RHT3 = no,
                    Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3),
                    RH = no
                )
            )
        )
    ).

%---------------------------------------------------------------------------%

    % The algorithm we use similar to tree234.delete, except that we
    % always go down the left subtree.

remove_smallest(K, V, Tin, Tout) :-
    tree234.remove_smallest_2(Tin, K, V, Tout, _).

:- pred remove_smallest_2(tree234(K, V), K, V, tree234(K, V), bool).
%:- mode remove_smallest_2(di, uo, uo, uo, out) is semidet.
:- mode remove_smallest_2(in, out, out, out, out) is semidet.

remove_smallest_2(Tin, K, V, Tout, RH) :-
    (
        Tin = empty,
        fail
    ;
        Tin = two(K0, V0, T0, T1),
        ( if T0 = empty then
            K = K0,
            V = V0,
            Tout = T1,
            RH = yes
        else
            tree234.remove_smallest_2(T0, K, V, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
            ;
                RHT0 = no,
                Tout = two(K0, V0, NewT0, T1),
                RH = no
            )
        )
    ;
        Tin = three(K0, V0, K1, V1, T0, T1, T2),
        ( if T0 = empty then
            K = K0,
            V = V0,
            Tout = two(K1, V1, T1, T2),
            RH = no
        else
            tree234.remove_smallest_2(T0, K, V, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2, Tout, RH)
            ;
                RHT0 = no,
                Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
                RH = no
            )
        )
    ;
        Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        ( if T0 = empty then
            K = K0,
            V = V0,
            Tout = three(K1, V1, K2, V2, T1, T2, T3),
            RH = no
        else
            tree234.remove_smallest_2(T0, K, V, NewT0, RHT0),
            (
                RHT0 = yes,
                fix_4node_t0(K0, V0, K1, V1, K2, V2,
                    NewT0, T1, T2, T3, Tout, RH)
            ;
                RHT0 = no,
                Tout = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3),
                RH = no
            )
        )
    ).

%---------------------------------------------------------------------------%
%
% Utilities used by the deletion predicates.
%
% The input to the following group of predicates are the components of a two-,
% three- or four-node in which the height of the indicated subtree is one less
% than it should be. If it is possible to increase the height of that subtree
% by moving into it elements from its neighboring subtrees, do so, and return
% the resulting tree with RH set to no. Otherwise, return a balanced tree
% whose height is reduced by one, with RH set to yes to indicate the
% reduced height.
%

:- pred fix_2node_t0(K, V, tree234(K, V), tree234(K, V), tree234(K, V), bool).
:- mode fix_2node_t0(di, di, di, di, uo, out) is det.
:- mode fix_2node_t0(in, in, in, in, out, out) is det.

fix_2node_t0(K0, V0, T0, T1, Tout, RH) :-
    (
        % Steal T1's leftmost subtree and combine it with T0.
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
        Node = two(K0, V0, T0, T10),
        Tout = two(K10, V10, Node, NewT1),
        RH = no
    ;
        % Steal T1's leftmost subtree and combine it with T0.
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K11, V11, T11, T12),
        Node = two(K0, V0, T0, T10),
        Tout = two(K10, V10, Node, NewT1),
        RH = no
    ;
        % Move T0 one level down and combine it with the subtrees of T1
        % this reduces the depth of the tree.
        T1 = two(K10, V10, T10, T11),
        Tout = three(K0, V0, K10, V10, T0, T10, T11),
        RH = yes
    ;
        T1 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = two(K0, V0, T0, T1),
        % RH = yes
    ).

:- pred fix_2node_t1(K, V, tree234(K, V), tree234(K, V), tree234(K, V), bool).
:- mode fix_2node_t1(di, di, di, di, uo, out) is det.
:- mode fix_2node_t1(in, in, in, in, out, out) is det.

fix_2node_t1(K0, V0, T0, T1, Tout, RH) :-
    (
        % Steal T0's leftmost subtree and combine it with T1.
        T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
        NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
        Node = two(K0, V0, T03, T1),
        Tout = two(K02, V02, NewT0, Node),
        RH = no
    ;
        % Steal T0's leftmost subtree and combine it with T1.
        T0 = three(K00, V00, K01, V01, T00, T01, T02),
        NewT0 = two(K00, V00, T00, T01),
        Node = two(K0, V0, T02, T1),
        Tout = two(K01, V01, NewT0, Node),
        RH = no
    ;
        % Move T1 one level down and combine it with the subtrees of T0
        % this reduces the depth of the tree.
        T0 = two(K00, V00, T00, T01),
        Tout = three(K00, V00, K0, V0, T00, T01, T1),
        RH = yes
    ;
        T0 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = two(K0, V0, T0, T1),
        % RH = yes
    ).

:- pred fix_3node_t0(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V),
    tree234(K, V), bool).
:- mode fix_3node_t0(di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_3node_t0(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t0(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
    (
        % Steal T1's leftmost subtree and combine it with T0.
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
        Node = two(K0, V0, T0, T10),
        Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
        RH = no
    ;
        % Steal T1's leftmost subtree and combine it with T0.
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K11, V11, T11, T12),
        Node = two(K0, V0, T0, T10),
        Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
        RH = no
    ;
        % Move T0 one level down to become the leftmost subtree of T1.
        T1 = two(K10, V10, T10, T11),
        NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
        Tout = two(K1, V1, NewT1, T2),
        RH = no
    ;
        T1 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = three(K0, V0, K1, V1, T0, T1, T2),
        % The heights of T1 and T2 are unchanged
        % RH = no
    ).

:- pred fix_3node_t1(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V),
    tree234(K, V), bool).
%:- mode fix_3node_t1(di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_3node_t1(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t1(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
    (
        % Steal T0's rightmost subtree and combine it with T1.
        T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
        NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
        Node = two(K0, V0, T03, T1),
        Tout = three(K02, V02, K1, V1, NewT0, Node, T2),
        RH = no
    ;
        % Steal T0's rightmost subtree and combine it with T1.
        T0 = three(K00, V00, K01, V01, T00, T01, T02),
        NewT0 = two(K00, V00, T00, T01),
        Node = two(K0, V0, T02, T1),
        Tout = three(K01, V01, K1, V1, NewT0, Node, T2),
        RH = no
    ;
        % Move T1 one level down to become the rightmost subtree of T0.
        T0 = two(K00, V00, T00, T01),
        NewT0 = three(K00, V00, K0, V0, T00, T01, T1),
        Tout = two(K1, V1, NewT0, T2),
        RH = no
    ;
        T0 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = three(K0, V0, K1, V1, T0, T1, T2),
        % The heights of T0 and T2 are unchanged
        % RH = no
    ).

:- pred fix_3node_t2(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V),
    tree234(K, V), bool).
%:- mode fix_3node_t2(di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_3node_t2(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t2(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
    (
        % Steal T1's rightmost subtree and combine it with T2.
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K10, V10, K11, V11, T10, T11, T12),
        Node = two(K1, V1, T13, T2),
        Tout = three(K0, V0, K12, V12, T0, NewT1, Node),
        RH = no
    ;
        % Steal T1's rightmost subtree and combine it with T2.
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K10, V10, T10, T11),
        Node = two(K1, V1, T12, T2),
        Tout = three(K0, V0, K11, V11, T0, NewT1, Node),
        RH = no
    ;
        % Move T2 one level down to become the rightmost subtree of T1.
        T1 = two(K10, V10, T10, T11),
        NewT1 = three(K10, V10, K1, V1, T10, T11, T2),
        Tout = two(K0, V0, T0, NewT1),
        RH = no
    ;
        T1 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = three(K0, V0, K1, V1, T0, T1, T2),
        % The heights of T0 and T1 are unchanged
        % RH = no
    ).

:- pred fix_4node_t0(K, V, K, V, K, V,
    tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
    tree234(K, V), bool).
%:- mode fix_4node_t0(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t0(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t0(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % Steal T1's leftmost subtree and combine it with T0.
        T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
        NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
        Node = two(K0, V0, T0, T10),
        Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
        RH = no
    ;
        % Steal T1's leftmost subtree and combine it with T0.
        T1 = three(K10, V10, K11, V11, T10, T11, T12),
        NewT1 = two(K11, V11, T11, T12),
        Node = two(K0, V0, T0, T10),
        Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
        RH = no
    ;
        % Move T0 one level down to become the leftmost subtree of T1.
        T1 = two(K10, V10, T10, T11),
        NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
        Tout = three(K1, V1, K2, V2, NewT1, T2, T3),
        RH = no
    ;
        T1 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T1, T2 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t1(K, V, K, V, K, V,
    tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
    tree234(K, V), bool).
%:- mode fix_4node_t1(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t1(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % Steal T2's leftmost subtree and combine it with T1.
        T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
        NewT2 = three(K21, V21, K22, V22, T21, T22, T23),
        Node = two(K1, V1, T1, T20),
        Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
        RH = no
    ;
        % Steal T2's leftmost subtree and combine it with T1.
        T2 = three(K20, V20, K21, V21, T20, T21, T22),
        NewT2 = two(K21, V21, T21, T22),
        Node = two(K1, V1, T1, T20),
        Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
        RH = no
    ;
        % Move T1 one level down to become the leftmost subtree of T2.
        T2 = two(K20, V20, T20, T21),
        NewT2 = three(K1, V1, K20, V20, T1, T20, T21),
        Tout = three(K0, V0, K2, V2, T0, NewT2, T3),
        RH = no
    ;
        T2 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T0, T2 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t2(K, V, K, V, K, V,
    tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
    tree234(K, V), bool).
%:- mode fix_4node_t2(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t2(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % Steal T3's leftmost subtree and combine it with T2.
        T3 = four(K30, V30, K31, V31, K32, V32, T30, T31, T32, T33),
        NewT3 = three(K31, V31, K32, V32, T31, T32, T33),
        Node = two(K2, V2, T2, T30),
        Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
        RH = no
    ;
        % Steal T3's leftmost subtree and combine it with T2.
        T3 = three(K30, V30, K31, V31, T30, T31, T32),
        NewT3 = two(K31, V31, T31, T32),
        Node = two(K2, V2, T2, T30),
        Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
        RH = no
    ;
        % Move T2 one level down to become the leftmost subtree of T3.
        T3 = two(K30, V30, T30, T31),
        NewT3 = three(K2, V2, K30, V30, T2, T30, T31),
        Tout = three(K0, V0, K1, V1, T0, T1, NewT3),
        RH = no
    ;
        T3 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T0, T1 and T3 are unchanged
        % RH = no
    ).

:- pred fix_4node_t3(K, V, K, V, K, V,
    tree234(K, V), tree234(K, V), tree234(K, V), tree234(K, V),
    tree234(K, V), bool).
%:- mode fix_4node_t3(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t3(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
    (
        % Steal T2's rightmost subtree and combine it with T3.
        T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
        NewT2 = three(K20, V20, K21, V21, T20, T21, T22),
        Node = two(K2, V2, T23, T3),
        Tout = four(K0, V0, K1, V1, K22, V22, T0, T1, NewT2, Node),
        RH = no
    ;
        % Steal T2's rightmost subtree and combine it with T3.
        T2 = three(K20, V20, K21, V21, T20, T21, T22),
        NewT2 = two(K20, V20, T20, T21),
        Node = two(K2, V2, T22, T3),
        Tout = four(K0, V0, K1, V1, K21, V21, T0, T1, NewT2, Node),
        RH = no
    ;
        % Move T3 one level down to become the rightmost subtree of T2.
        T2 = two(K20, V20, T20, T21),
        NewT2 = three(K20, V20, K2, V2, T20, T21, T3),
        Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
        RH = no
    ;
        T2 = empty,
        unexpected($module, $pred, "unbalanced 234 tree")
        % Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        % The heights of T0, T1 and T2 are unchanged
        % RH = no
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

keys(T) = Ks :-
    tree234.keys(T, Ks).

keys(Tree, Keys) :-
    tree234.keys_acc(Tree, [], Keys).

:- pred keys_acc(tree234(K, V), list(K), list(K)).
:- mode keys_acc(in, in, out) is det.

keys_acc(empty, List, List).
keys_acc(two(K0, _V0, T0, T1), L0, L) :-
    tree234.keys_acc(T1, L0, L1),
    tree234.keys_acc(T0, [K0 | L1], L).
keys_acc(three(K0, _V0, K1, _V1, T0, T1, T2), L0, L) :-
    tree234.keys_acc(T2, L0, L1),
    tree234.keys_acc(T1, [K1 | L1], L2),
    tree234.keys_acc(T0, [K0 | L2], L).
keys_acc(four(K0, _V0, K1, _V1, K2, _V2, T0, T1, T2, T3), L0, L) :-
    tree234.keys_acc(T3, L0, L1),
    tree234.keys_acc(T2, [K2 | L1], L2),
    tree234.keys_acc(T1, [K1 | L2], L3),
    tree234.keys_acc(T0, [K0 | L3], L).

%---------------------------------------------------------------------------%

values(T) = Vs :-
    tree234.values(T, Vs).

values(Tree, Values) :-
    tree234.values_acc(Tree, [], Values).

:- pred values_acc(tree234(K, V), list(V), list(V)).
:- mode values_acc(in, in, out) is det.

values_acc(empty, List, List).
values_acc(two(_K0, V0, T0, T1), L0, L) :-
    tree234.values_acc(T1, L0, L1),
    tree234.values_acc(T0, [V0 | L1], L).
values_acc(three(_K0, V0, _K1, V1, T0, T1, T2), L0, L) :-
    tree234.values_acc(T2, L0, L1),
    tree234.values_acc(T1, [V1 | L1], L2),
    tree234.values_acc(T0, [V0 | L2], L).
values_acc(four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3), L0, L) :-
    tree234.values_acc(T3, L0, L1),
    tree234.values_acc(T2, [V2 | L1], L2),
    tree234.values_acc(T1, [V1 | L2], L3),
    tree234.values_acc(T0, [V0 | L3], L).

%---------------------------------------------------------------------------%

keys_and_values(Tree, Keys, Values) :-
    tree234.keys_and_values_acc(Tree, [], Keys, [], Values).

:- pred keys_and_values_acc(tree234(K, V)::in,
    list(K)::in, list(K)::out, list(V)::in, list(V)::out) is det.

keys_and_values_acc(empty, !Keys, !Values).
keys_and_values_acc(two(K0, V0, T0, T1), !Keys, !Values) :-
    tree234.keys_and_values_acc(T1, !Keys, !Values),
    !:Keys = [K0 | !.Keys],
    !:Values = [V0 | !.Values],
    tree234.keys_and_values_acc(T0, !Keys, !Values).
keys_and_values_acc(three(K0, V0, K1, V1, T0, T1, T2),
        !Keys, !Values) :-
    tree234.keys_and_values_acc(T2, !Keys, !Values),
    !:Keys = [K1 | !.Keys],
    !:Values = [V1 | !.Values],
    tree234.keys_and_values_acc(T1, !Keys, !Values),
    !:Keys = [K0 | !.Keys],
    !:Values = [V0 | !.Values],
    tree234.keys_and_values_acc(T0, !Keys, !Values).
keys_and_values_acc(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !Keys, !Values) :-
    tree234.keys_and_values_acc(T3, !Keys, !Values),
    !:Keys = [K2 | !.Keys],
    !:Values = [V2 | !.Values],
    tree234.keys_and_values_acc(T2, !Keys, !Values),
    !:Keys = [K1 | !.Keys],
    !:Values = [V1 | !.Values],
    tree234.keys_and_values_acc(T1, !Keys, !Values),
    !:Keys = [K0 | !.Keys],
    !:Values = [V0 | !.Values],
    tree234.keys_and_values_acc(T0, !Keys, !Values).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

count(T) = N :-
    tree234.count(T, N).

count(empty, 0).
count(two(_, _, T0, T1), N) :-
    tree234.count(T0, N0),
    tree234.count(T1, N1),
    N = 1 + N0 + N1.
count(three(_, _, _, _, T0, T1, T2), N) :-
    tree234.count(T0, N0),
    tree234.count(T1, N1),
    tree234.count(T2, N2),
    N = 2 + N0 + N1 + N2.
count(four(_, _, _, _, _, _, T0, T1, T2, T3), N) :-
    tree234.count(T0, N0),
    tree234.count(T1, N1),
    tree234.count(T2, N2),
    tree234.count(T3, N3),
    N = 3 + N0 + N1 + N2 + N3.

%---------------------------------------------------------------------------%

tree234_to_assoc_list(T) = AL :-
    tree234.tree234_to_assoc_list(T, AL).

tree234_to_assoc_list(Tree, AssocList) :-
    tree234.tree234_to_assoc_list_acc(Tree, [], AssocList).

:- pred tree234_to_assoc_list_acc(tree234(K, V)::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out) is det.

tree234_to_assoc_list_acc(empty, L, L).
tree234_to_assoc_list_acc(two(K0, V0, T0, T1), L0, L) :-
    tree234.tree234_to_assoc_list_acc(T1, L0, L1),
    tree234.tree234_to_assoc_list_acc(T0, [K0 - V0 | L1], L).
tree234_to_assoc_list_acc(three(K0, V0, K1, V1, T0, T1, T2), L0, L) :-
    tree234.tree234_to_assoc_list_acc(T2, L0, L1),
    tree234.tree234_to_assoc_list_acc(T1, [K1 - V1 | L1], L2),
    tree234.tree234_to_assoc_list_acc(T0, [K0 - V0 | L2], L).
tree234_to_assoc_list_acc(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        L0, L) :-
    tree234.tree234_to_assoc_list_acc(T3, L0, L1),
    tree234.tree234_to_assoc_list_acc(T2, [K2 - V2 | L1], L2),
    tree234.tree234_to_assoc_list_acc(T1, [K1 - V1 | L2], L3),
    tree234.tree234_to_assoc_list_acc(T0, [K0 - V0 | L3], L).

%---------------------------------------------------------------------------%

assoc_list_to_tree234(AL) = T :-
    tree234.assoc_list_to_tree234(AL, T).

assoc_list_to_tree234(AssocList, Tree) :-
    tree234.assoc_list_to_tree234_acc(AssocList, empty, Tree).

:- pred assoc_list_to_tree234_acc(assoc_list(K, V)::in,
    tree234(K, V)::in, tree234(K, V)::out) is det.

assoc_list_to_tree234_acc([], Tree, Tree).
assoc_list_to_tree234_acc([K - V | Rest], !Tree) :-
    tree234.set(K, V, !Tree),
    tree234.assoc_list_to_tree234_acc(Rest, !Tree).

%---------------------------------------------------------------------------%

from_sorted_assoc_list(List, Tree) :-
    list.length(List, Len),
    ( if Len = 0 then
        % We can handle the Len = 0 case here just once, or we can handle it
        % lots of times in do_from_sorted_assoc_list. The former is more
        % efficient.
        Tree = empty
    else
        find_num_234_levels(Len, Level, AllThrees),
        do_from_sorted_assoc_list(Len, List, LeftOver, Level, AllThrees, Tree),
        trace [compiletime(flag("tree234_sanity_checks"))] (
            expect(unify(LeftOver, []), $module, $pred, "leftovers")
        )
    ).

:- pred do_from_sorted_assoc_list(int::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out,
    int::in, int::in, tree234(K, V)::out) is det.

do_from_sorted_assoc_list(Len, !List, Level0, AllThrees0, Tree) :-
    ( if Level0 = 1 then
        ( if Len = 1 then
            (
                !.List = [K1 - V1 | !:List],
                Tree = two(K1, V1, empty, empty)
            ;
                !.List = [],
                unexpected($module, $pred, "len 1 nil")
            )
        else if Len = 2 then
            trace [compiletime(flag("tree234_sanity_checks"))] (
                expect(unify(Level0, 1), $module, $pred,
                    "Len = 2 but Level != 1")
            ),
            (
                !.List = [K1 - V1, K2 - V2 | !:List],
                Tree = three(K1, V1, K2, V2, empty, empty, empty)
            ;
                !.List = [_],
                unexpected($module, $pred, "len 2 one")
            ;
                !.List = [],
                unexpected($module, $pred, "len 2 nil")
            )
        else
            unexpected($module, $pred, "level 1, but len not 1 or 2")
        )
    else
        Level = Level0 - 1,
        AllThrees = (AllThrees0 - 2) / 3,
        ( if Len > 2 * AllThrees then
            BaseSubLen = (Len / 3),
            Diff = Len - (BaseSubLen * 3),
            ( if Diff = 0 then
                % Len = BaseSubLen * 3:
                % (BaseSubLen) + 1 + (BaseSubLen - 1) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1,
                SubLen3 = BaseSubLen - 1
            else if Diff = 1 then
                % Len = BaseSubLen * 3 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen - 1
            else
                trace [compiletime(flag("tree234_sanity_checks"))] (
                    expect(unify(Diff, 2), $module, $pred, "Diff != 2")
                ),
                % Len = BaseSubLen * 3 + 2:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_sorted_assoc_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into three: %d, %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2), i(SubLen3)], !IO)
            ),

            do_from_sorted_assoc_list(SubLen1, !List, Level, AllThrees,
                SubTree1),
            (
                !.List = [K1 - V1 | !:List]
            ;
                !.List = [],
                unexpected($module, $pred, "tree K1 V1 nil")
            ),
            do_from_sorted_assoc_list(SubLen2, !List, Level, AllThrees,
                SubTree2),
            (
                !.List = [K2 - V2 | !:List]
            ;
                !.List = [],
                unexpected($module, $pred, "tree K2 V2 nil")
            ),
            do_from_sorted_assoc_list(SubLen3, !List, Level, AllThrees,
                SubTree3),
            Tree = three(K1, V1, K2, V2, SubTree1, SubTree2, SubTree3),
            trace [io(!IO), compile_time(flag("from_sorted_assoc_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        else
            BaseSubLen = (Len) / 2,
            Diff = Len - (BaseSubLen * 2),
            ( if Diff = 0 then
                % Len = BaseSubLen * 2:
                % (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1
            else
                trace [compiletime(flag("tree234_sanity_checks"))] (
                    expect(unify(Diff, 1), $module, $pred, "Diff != 1")
                ),
                % Len = BaseSubLen * 2 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_sorted_assoc_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into two: %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2)], !IO)
            ),

            do_from_sorted_assoc_list(SubLen1, !List, Level, AllThrees,
                SubTree1),
            (
                !.List = [K1 - V1 | !:List]
            ;
                !.List = [],
                unexpected($module, $pred, "two K1 V1 nil")
            ),
            do_from_sorted_assoc_list(SubLen2, !List, Level, AllThrees,
                SubTree2),
            Tree = two(K1, V1, SubTree1, SubTree2),
            trace [io(!IO), compile_time(flag("from_sorted_assoc_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        )
    ).

from_rev_sorted_assoc_list(List, Tree) :-
    list.length(List, Len),
    ( if Len = 0 then
        % We can handle the Len = 0 case here just once, or we can handle it
        % lots of times in do_from_rev_sorted_assoc_list. The former is more
        % efficient.
        Tree = empty
    else
        find_num_234_levels(Len, Level, AllThrees),
        do_from_rev_sorted_assoc_list(Len, List, LeftOver, Level, AllThrees,
            Tree),
        trace [compiletime(flag("tree234_sanity_checks"))] (
            expect(unify(LeftOver, []), $module, $pred, "leftovers")
        )
    ).

:- pred do_from_rev_sorted_assoc_list(int::in,
    assoc_list(K, V)::in, assoc_list(K, V)::out,
    int::in, int::in, tree234(K, V)::out) is det.

do_from_rev_sorted_assoc_list(Len, !List, Level0, AllThrees0, Tree) :-
    ( if Level0 = 1 then
        ( if Len = 1 then
            (
                !.List = [K1 - V1 | !:List],
                Tree = two(K1, V1, empty, empty)
            ;
                !.List = [],
                unexpected($module, $pred, "len 1 nil")
            )
        else if Len = 2 then
            trace [compiletime(flag("tree234_sanity_checks"))] (
                expect(unify(Level0, 1), $module, $pred,
                    "Len = 2 but Level != 1")
            ),
            (
                !.List = [K2 - V2, K1 - V1 | !:List],
                Tree = three(K1, V1, K2, V2, empty, empty, empty)
            ;
                !.List = [_],
                unexpected($module, $pred, "len 2 one")
            ;
                !.List = [],
                unexpected($module, $pred, "len 2 nil")
            )
        else
            unexpected($module, $pred, "level 1, but len not 1 or 2")
        )
    else
        Level = Level0 - 1,
        AllThrees = (AllThrees0 - 2) / 3,
        ( if Len > 2 * AllThrees then
            BaseSubLen = (Len / 3),
            Diff = Len - (BaseSubLen * 3),
            ( if Diff = 0 then
                % Len = BaseSubLen * 3:
                % (BaseSubLen) + 1 + (BaseSubLen - 1) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1,
                SubLen3 = BaseSubLen - 1
            else if Diff = 1 then
                % Len = BaseSubLen * 3 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen - 1
            else
                trace [compiletime(flag("tree234_sanity_checks"))] (
                    expect(unify(Diff, 2), $module, $pred, "Diff != 2")
                ),
                % Len = BaseSubLen * 3 + 2:
                % (BaseSubLen) + 1 + (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen,
                SubLen3 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_rev_sorted_assoc_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into three: %d, %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2), i(SubLen3)], !IO)
            ),

            do_from_rev_sorted_assoc_list(SubLen3, !List, Level, AllThrees,
                SubTree3),
            (
                !.List = [K2 - V2 | !:List]
            ;
                !.List = [],
                unexpected($module, $pred, "tree K2 V2 nil")
            ),
            do_from_rev_sorted_assoc_list(SubLen2, !List, Level, AllThrees,
                SubTree2),
            (
                !.List = [K1 - V1 | !:List]
            ;
                !.List = [],
                unexpected($module, $pred, "tree K1 V1 nil")
            ),
            do_from_rev_sorted_assoc_list(SubLen1, !List, Level, AllThrees,
                SubTree1),
            Tree = three(K1, V1, K2, V2, SubTree1, SubTree2, SubTree3),
            trace [io(!IO), compile_time(flag("from_rev_sorted_assoc_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        else
            BaseSubLen = (Len) / 2,
            Diff = Len - (BaseSubLen * 2),
            ( if Diff = 0 then
                % Len = BaseSubLen * 2:
                % (BaseSubLen) + 1 + (BaseSubLen - 1)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen - 1
            else
                trace [compiletime(flag("tree234_sanity_checks"))] (
                    expect(unify(Diff, 1), $module, $pred, "Diff != 1")
                ),
                % Len = BaseSubLen * 2 + 1:
                % (BaseSubLen) + 1 + (BaseSubLen)
                SubLen1 = BaseSubLen,
                SubLen2 = BaseSubLen
            ),

            trace [io(!IO), compile_time(flag("from_rev_sorted_assoc_list"))] (
                io.output_stream(SplitStream, !IO),
                io.format(SplitStream,
                    "splitting %d into two: %d, %d\n",
                    [i(Len), i(SubLen1), i(SubLen2)], !IO)
            ),

            do_from_rev_sorted_assoc_list(SubLen2, !List, Level, AllThrees,
                SubTree2),
            (
                !.List = [K1 - V1 | !:List]
            ;
                !.List = [],
                unexpected($module, $pred, "two K1 V1 nil")
            ),
            do_from_rev_sorted_assoc_list(SubLen1, !List, Level, AllThrees,
                SubTree1),
            Tree = two(K1, V1, SubTree1, SubTree2),
            trace [io(!IO), compile_time(flag("from_rev_sorted_assoc_list"))] (
                io.output_stream(TreeStream, !IO),
                io.format(TreeStream, "tree for %d\n", [i(Len)], !IO),
                io.write(TreeStream, Tree, !IO),
                io.nl(TreeStream, !IO)
            )
        )
    ).

:- pred find_num_234_levels(int::in, int::out, int::out) is det.

find_num_234_levels(Len, Level, AllThrees) :-
    find_num_234_levels_loop(Len, 0, Level, 0, AllThrees).

:- pred find_num_234_levels_loop(int::in,
    int::in, int::out, int::in, int::out) is det.

find_num_234_levels_loop(Len, !Level, !AllThrees) :-
    ( if Len =< !.AllThrees then
        true
    else
        !:Level = !.Level + 1,
        !:AllThrees = !.AllThrees * 3 + 2,
        find_num_234_levels_loop(Len, !Level, !AllThrees)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

foldl(F, T, A) = B :-
    P = (pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    tree234.foldl(P, T, A, B).

foldl(_Pred, empty, !A).
foldl(Pred, two(K, V, T0, T1), !A) :-
    tree234.foldl(Pred, T0, !A),
    Pred(K, V, !A),
    tree234.foldl(Pred, T1, !A).
foldl(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A) :-
    tree234.foldl(Pred, T0, !A),
    Pred(K0, V0, !A),
    tree234.foldl(Pred, T1, !A),
    Pred(K1, V1, !A),
    tree234.foldl(Pred, T2, !A).
foldl(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), !A) :-
    tree234.foldl(Pred, T0, !A),
    Pred(K0, V0, !A),
    tree234.foldl(Pred, T1, !A),
    Pred(K1, V1, !A),
    tree234.foldl(Pred, T2, !A),
    Pred(K2, V2, !A),
    tree234.foldl(Pred, T3, !A).

foldl2(_Pred, empty, !A, !B).
foldl2(Pred, two(K, V, T0, T1), !A, !B) :-
    tree234.foldl2(Pred, T0, !A, !B),
    Pred(K, V, !A, !B),
    tree234.foldl2(Pred, T1, !A, !B).
foldl2(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B) :-
    tree234.foldl2(Pred, T0, !A, !B),
    Pred(K0, V0, !A, !B),
    tree234.foldl2(Pred, T1, !A, !B),
    Pred(K1, V1, !A, !B),
    tree234.foldl2(Pred, T2, !A, !B).
foldl2(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), !A, !B) :-
    tree234.foldl2(Pred, T0, !A, !B),
    Pred(K0, V0, !A, !B),
    tree234.foldl2(Pred, T1, !A, !B),
    Pred(K1, V1, !A, !B),
    tree234.foldl2(Pred, T2, !A, !B),
    Pred(K2, V2, !A, !B),
    tree234.foldl2(Pred, T3, !A, !B).

foldl3(_Pred, empty, !A, !B, !C).
foldl3(Pred, two(K, V, T0, T1), !A, !B, !C) :-
    tree234.foldl3(Pred, T0, !A, !B, !C),
    Pred(K, V, !A, !B, !C),
    tree234.foldl3(Pred, T1, !A, !B, !C).
foldl3(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B, !C) :-
    tree234.foldl3(Pred, T0, !A, !B, !C),
    Pred(K0, V0, !A, !B, !C),
    tree234.foldl3(Pred, T1, !A, !B, !C),
    Pred(K1, V1, !A, !B, !C),
    tree234.foldl3(Pred, T2, !A, !B, !C).
foldl3(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B, !C) :-
    tree234.foldl3(Pred, T0, !A, !B, !C),
    Pred(K0, V0, !A, !B, !C),
    tree234.foldl3(Pred, T1, !A, !B, !C),
    Pred(K1, V1, !A, !B, !C),
    tree234.foldl3(Pred, T2, !A, !B, !C),
    Pred(K2, V2, !A, !B, !C),
    tree234.foldl3(Pred, T3, !A, !B, !C).

foldl4(_Pred, empty, !A, !B, !C, !D).
foldl4(Pred, two(K, V, T0, T1), !A, !B, !C, !D) :-
    tree234.foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(K, V, !A, !B, !C, !D),
    tree234.foldl4(Pred, T1, !A, !B, !C, !D).
foldl4(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B, !C, !D) :-
    tree234.foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(K0, V0, !A, !B, !C, !D),
    tree234.foldl4(Pred, T1, !A, !B, !C, !D),
    Pred(K1, V1, !A, !B, !C, !D),
    tree234.foldl4(Pred, T2, !A, !B, !C, !D).
foldl4(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B, !C, !D) :-
    tree234.foldl4(Pred, T0, !A, !B, !C, !D),
    Pred(K0, V0, !A, !B, !C, !D),
    tree234.foldl4(Pred, T1, !A, !B, !C, !D),
    Pred(K1, V1, !A, !B, !C, !D),
    tree234.foldl4(Pred, T2, !A, !B, !C, !D),
    Pred(K2, V2, !A, !B, !C, !D),
    tree234.foldl4(Pred, T3, !A, !B, !C, !D).

foldl5(_Pred, empty, !A, !B, !C, !D, !E).
foldl5(Pred, two(K, V, T0, T1), !A, !B, !C, !D, !E) :-
    tree234.foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(K, V, !A, !B, !C, !D, !E),
    tree234.foldl5(Pred, T1, !A, !B, !C, !D, !E).
foldl5(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B, !C, !D, !E) :-
    tree234.foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(K0, V0, !A, !B, !C, !D, !E),
    tree234.foldl5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(K1, V1, !A, !B, !C, !D, !E),
    tree234.foldl5(Pred, T2, !A, !B, !C, !D, !E).
foldl5(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B, !C, !D, !E) :-
    tree234.foldl5(Pred, T0, !A, !B, !C, !D, !E),
    Pred(K0, V0, !A, !B, !C, !D, !E),
    tree234.foldl5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(K1, V1, !A, !B, !C, !D, !E),
    tree234.foldl5(Pred, T2, !A, !B, !C, !D, !E),
    Pred(K2, V2, !A, !B, !C, !D, !E),
    tree234.foldl5(Pred, T3, !A, !B, !C, !D, !E).

foldl_values(_Pred, empty, !A).
foldl_values(Pred, two(_K, V, T0, T1), !A) :-
    tree234.foldl_values(Pred, T0, !A),
    Pred(V, !A),
    tree234.foldl_values(Pred, T1, !A).
foldl_values(Pred, three(_K0, V0, _K1, V1, T0, T1, T2), !A) :-
    tree234.foldl_values(Pred, T0, !A),
    Pred(V0, !A),
    tree234.foldl_values(Pred, T1, !A),
    Pred(V1, !A),
    tree234.foldl_values(Pred, T2, !A).
foldl_values(Pred, four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3),
        !A) :-
    tree234.foldl_values(Pred, T0, !A),
    Pred(V0, !A),
    tree234.foldl_values(Pred, T1, !A),
    Pred(V1, !A),
    tree234.foldl_values(Pred, T2, !A),
    Pred(V2, !A),
    tree234.foldl_values(Pred, T3, !A).

foldl2_values(_Pred, empty, !A, !B).
foldl2_values(Pred, two(_K, V, T0, T1), !A, !B) :-
    tree234.foldl2_values(Pred, T0, !A, !B),
    Pred(V, !A, !B),
    tree234.foldl2_values(Pred, T1, !A, !B).
foldl2_values(Pred, three(_K0, V0, _K1, V1, T0, T1, T2), !A, !B) :-
    tree234.foldl2_values(Pred, T0, !A, !B),
    Pred(V0, !A, !B),
    tree234.foldl2_values(Pred, T1, !A, !B),
    Pred(V1, !A, !B),
    tree234.foldl2_values(Pred, T2, !A, !B).
foldl2_values(Pred, four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3),
        !A, !B) :-
    tree234.foldl2_values(Pred, T0, !A, !B),
    Pred(V0, !A, !B),
    tree234.foldl2_values(Pred, T1, !A, !B),
    Pred(V1, !A, !B),
    tree234.foldl2_values(Pred, T2, !A, !B),
    Pred(V2, !A, !B),
    tree234.foldl2_values(Pred, T3, !A, !B).

foldl3_values(_Pred, empty, !A, !B, !C).
foldl3_values(Pred, two(_K, V, T0, T1), !A, !B, !C) :-
    tree234.foldl3_values(Pred, T0, !A, !B, !C),
    Pred(V, !A, !B, !C),
    tree234.foldl3_values(Pred, T1, !A, !B, !C).
foldl3_values(Pred, three(_K0, V0, _K1, V1, T0, T1, T2), !A, !B, !C) :-
    tree234.foldl3_values(Pred, T0, !A, !B, !C),
    Pred(V0, !A, !B, !C),
    tree234.foldl3_values(Pred, T1, !A, !B, !C),
    Pred(V1, !A, !B, !C),
    tree234.foldl3_values(Pred, T2, !A, !B, !C).
foldl3_values(Pred, four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3),
        !A, !B, !C) :-
    tree234.foldl3_values(Pred, T0, !A, !B, !C),
    Pred(V0, !A, !B, !C),
    tree234.foldl3_values(Pred, T1, !A, !B, !C),
    Pred(V1, !A, !B, !C),
    tree234.foldl3_values(Pred, T2, !A, !B, !C),
    Pred(V2, !A, !B, !C),
    tree234.foldl3_values(Pred, T3, !A, !B, !C).

%---------------------------------------------------------------------------%

foldr(F, T, A) = B :-
    P = (pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
    tree234.foldr(P, T, A, B).

foldr(_Pred, empty, !A).
foldr(Pred, two(K, V, T0, T1), !A) :-
    tree234.foldr(Pred, T1, !A),
    Pred(K, V, !A),
    tree234.foldr(Pred, T0, !A).
foldr(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A) :-
    tree234.foldr(Pred, T2, !A),
    Pred(K1, V1, !A),
    tree234.foldr(Pred, T1, !A),
    Pred(K0, V0, !A),
    tree234.foldr(Pred, T0, !A).
foldr(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), !A) :-
    tree234.foldr(Pred, T3, !A),
    Pred(K2, V2, !A),
    tree234.foldr(Pred, T2, !A),
    Pred(K1, V1, !A),
    tree234.foldr(Pred, T1, !A),
    Pred(K0, V0, !A),
    tree234.foldr(Pred, T0, !A).

foldr2(_Pred, empty, !A, !B).
foldr2(Pred, two(K, V, T0, T1), !A, !B) :-
    tree234.foldr2(Pred, T1, !A, !B),
    Pred(K, V, !A, !B),
    tree234.foldr2(Pred, T0, !A, !B).
foldr2(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B) :-
    tree234.foldr2(Pred, T2, !A, !B),
    Pred(K1, V1, !A, !B),
    tree234.foldr2(Pred, T1, !A, !B),
    Pred(K0, V0, !A, !B),
    tree234.foldr2(Pred, T0, !A, !B).
foldr2(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), !A, !B) :-
    tree234.foldr2(Pred, T3, !A, !B),
    Pred(K2, V2, !A, !B),
    tree234.foldr2(Pred, T2, !A, !B),
    Pred(K1, V1, !A, !B),
    tree234.foldr2(Pred, T1, !A, !B),
    Pred(K0, V0, !A, !B),
    tree234.foldr2(Pred, T0, !A, !B).

foldr3(_Pred, empty, !A, !B, !C).
foldr3(Pred, two(K, V, T0, T1), !A, !B, !C) :-
    tree234.foldr3(Pred, T1, !A, !B, !C),
    Pred(K, V, !A, !B, !C),
    tree234.foldr3(Pred, T0, !A, !B, !C).
foldr3(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B, !C) :-
    tree234.foldr3(Pred, T2, !A, !B, !C),
    Pred(K1, V1, !A, !B, !C),
    tree234.foldr3(Pred, T1, !A, !B, !C),
    Pred(K0, V0, !A, !B, !C),
    tree234.foldr3(Pred, T0, !A, !B, !C).
foldr3(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B, !C) :-
    tree234.foldr3(Pred, T3, !A, !B, !C),
    Pred(K2, V2, !A, !B, !C),
    tree234.foldr3(Pred, T2, !A, !B, !C),
    Pred(K1, V1, !A, !B, !C),
    tree234.foldr3(Pred, T1, !A, !B, !C),
    Pred(K0, V0, !A, !B, !C),
    tree234.foldr3(Pred, T0, !A, !B, !C).

foldr4(_Pred, empty, !A, !B, !C, !D).
foldr4(Pred, two(K, V, T0, T1), !A, !B, !C, !D) :-
    tree234.foldr4(Pred, T1, !A, !B, !C, !D),
    Pred(K, V, !A, !B, !C, !D),
    tree234.foldr4(Pred, T0, !A, !B, !C, !D).
foldr4(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B, !C, !D) :-
    tree234.foldr4(Pred, T2, !A, !B, !C, !D),
    Pred(K1, V1, !A, !B, !C, !D),
    tree234.foldr4(Pred, T1, !A, !B, !C, !D),
    Pred(K0, V0, !A, !B, !C, !D),
    tree234.foldr4(Pred, T0, !A, !B, !C, !D).
foldr4(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B, !C, !D) :-
    tree234.foldr4(Pred, T3, !A, !B, !C, !D),
    Pred(K2, V2, !A, !B, !C, !D),
    tree234.foldr4(Pred, T2, !A, !B, !C, !D),
    Pred(K1, V1, !A, !B, !C, !D),
    tree234.foldr4(Pred, T1, !A, !B, !C, !D),
    Pred(K0, V0, !A, !B, !C, !D),
    tree234.foldr4(Pred, T0, !A, !B, !C, !D).

foldr5(_Pred, empty, !A, !B, !C, !D, !E).
foldr5(Pred, two(K, V, T0, T1), !A, !B, !C, !D, !E) :-
    tree234.foldr5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(K, V, !A, !B, !C, !D, !E),
    tree234.foldr5(Pred, T0, !A, !B, !C, !D, !E).
foldr5(Pred, three(K0, V0, K1, V1, T0, T1, T2), !A, !B, !C, !D, !E) :-
    tree234.foldr5(Pred, T2, !A, !B, !C, !D, !E),
    Pred(K1, V1, !A, !B, !C, !D, !E),
    tree234.foldr5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(K0, V0, !A, !B, !C, !D, !E),
    tree234.foldr5(Pred, T0, !A, !B, !C, !D, !E).
foldr5(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
        !A, !B, !C, !D, !E) :-
    tree234.foldr5(Pred, T3, !A, !B, !C, !D, !E),
    Pred(K2, V2, !A, !B, !C, !D, !E),
    tree234.foldr5(Pred, T2, !A, !B, !C, !D, !E),
    Pred(K1, V1, !A, !B, !C, !D, !E),
    tree234.foldr5(Pred, T1, !A, !B, !C, !D, !E),
    Pred(K0, V0, !A, !B, !C, !D, !E),
    tree234.foldr5(Pred, T0, !A, !B, !C, !D, !E).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

map_values(F, T1) = T2 :-
    P = (pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
    tree234.map_values(P, T1, T2).

map_values(_Pred, empty, empty).
map_values(Pred, Tree0, Tree) :-
    Tree0 = two(K0, V0, Left0, Right0),
    Pred(K0, V0, W0),
    tree234.map_values(Pred, Left0, Left),
    tree234.map_values(Pred, Right0, Right),
    Tree = two(K0, W0, Left, Right).
map_values(Pred, Tree0, Tree) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    Pred(K0, V0, W0),
    Pred(K1, V1, W1),
    tree234.map_values(Pred, Left0, Left),
    tree234.map_values(Pred, Middle0, Middle),
    tree234.map_values(Pred, Right0, Right),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_values(Pred, Tree0, Tree) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    Pred(K0, V0, W0),
    Pred(K1, V1, W1),
    Pred(K2, V2, W2),
    tree234.map_values(Pred, Left0, Left),
    tree234.map_values(Pred, LMid0, LMid),
    tree234.map_values(Pred, RMid0, RMid),
    tree234.map_values(Pred, Right0, Right),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

map_values_only(F, T1) = T2 :-
    P = (pred(Y::in, Z::out) is det :- Z = F(Y) ),
    tree234.map_values_only(P, T1, T2).

map_values_only(_Pred, empty, empty).
map_values_only(Pred, Tree0, Tree) :-
    Tree0 = two(K0, V0, Left0, Right0),
    Pred(V0, W0),
    tree234.map_values_only(Pred, Left0, Left),
    tree234.map_values_only(Pred, Right0, Right),
    Tree = two(K0, W0, Left, Right).
map_values_only(Pred, Tree0, Tree) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    Pred(V0, W0),
    Pred(V1, W1),
    tree234.map_values_only(Pred, Left0, Left),
    tree234.map_values_only(Pred, Middle0, Middle),
    tree234.map_values_only(Pred, Right0, Right),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_values_only(Pred, Tree0, Tree) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    Pred(V0, W0),
    Pred(V1, W1),
    Pred(V2, W2),
    tree234.map_values_only(Pred, Left0, Left),
    tree234.map_values_only(Pred, LMid0, LMid),
    tree234.map_values_only(Pred, RMid0, RMid),
    tree234.map_values_only(Pred, Right0, Right),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

map_foldl(_Pred, empty, empty, !A).
map_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = two(K0, V0, Left0, Right0),
    tree234.map_foldl(Pred, Left0, Left, !A),
    Pred(K0, V0, W0, !A),
    tree234.map_foldl(Pred, Right0, Right, !A),
    Tree = two(K0, W0, Left, Right).
map_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    tree234.map_foldl(Pred, Left0, Left, !A),
    Pred(K0, V0, W0, !A),
    tree234.map_foldl(Pred, Middle0, Middle, !A),
    Pred(K1, V1, W1, !A),
    tree234.map_foldl(Pred, Right0, Right, !A),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    tree234.map_foldl(Pred, Left0, Left, !A),
    Pred(K0, V0, W0, !A),
    tree234.map_foldl(Pred, LMid0, LMid, !A),
    Pred(K1, V1, W1, !A),
    tree234.map_foldl(Pred, RMid0, RMid, !A),
    Pred(K2, V2, W2, !A),
    tree234.map_foldl(Pred, Right0, Right, !A),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

map_foldl2(_Pred, empty, empty, !A, !B).
map_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = two(K0, V0, Left0, Right0),
    tree234.map_foldl2(Pred, Left0, Left, !A, !B),
    Pred(K0, V0, W0, !A, !B),
    tree234.map_foldl2(Pred, Right0, Right, !A, !B),
    Tree = two(K0, W0, Left, Right).
map_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    tree234.map_foldl2(Pred, Left0, Left, !A, !B),
    Pred(K0, V0, W0, !A, !B),
    tree234.map_foldl2(Pred, Middle0, Middle, !A, !B),
    Pred(K1, V1, W1, !A, !B),
    tree234.map_foldl2(Pred, Right0, Right, !A, !B),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    tree234.map_foldl2(Pred, Left0, Left, !A, !B),
    Pred(K0, V0, W0, !A, !B),
    tree234.map_foldl2(Pred, LMid0, LMid, !A, !B),
    Pred(K1, V1, W1, !A, !B),
    tree234.map_foldl2(Pred, RMid0, RMid, !A, !B),
    Pred(K2, V2, W2, !A, !B),
    tree234.map_foldl2(Pred, Right0, Right, !A, !B),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

map_foldl3(_Pred, empty, empty, !A, !B, !C).
map_foldl3(Pred, Tree0, Tree, !A, !B, !C) :-
    Tree0 = two(K0, V0, Left0, Right0),
    tree234.map_foldl3(Pred, Left0, Left, !A, !B, !C),
    Pred(K0, V0, W0, !A, !B, !C),
    tree234.map_foldl3(Pred, Right0, Right, !A, !B, !C),
    Tree = two(K0, W0, Left, Right).
map_foldl3(Pred, Tree0, Tree, !A, !B, !C) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    tree234.map_foldl3(Pred, Left0, Left, !A, !B, !C),
    Pred(K0, V0, W0, !A, !B, !C),
    tree234.map_foldl3(Pred, Middle0, Middle, !A, !B, !C),
    Pred(K1, V1, W1, !A, !B, !C),
    tree234.map_foldl3(Pred, Right0, Right, !A, !B, !C),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_foldl3(Pred, Tree0, Tree, !A, !B, !C) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    tree234.map_foldl3(Pred, Left0, Left, !A, !B, !C),
    Pred(K0, V0, W0, !A, !B, !C),
    tree234.map_foldl3(Pred, LMid0, LMid, !A, !B, !C),
    Pred(K1, V1, W1, !A, !B, !C),
    tree234.map_foldl3(Pred, RMid0, RMid, !A, !B, !C),
    Pred(K2, V2, W2, !A, !B, !C),
    tree234.map_foldl3(Pred, Right0, Right, !A, !B, !C),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

map_values_foldl(_Pred, empty, empty, !A).
map_values_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = two(K0, V0, Left0, Right0),
    tree234.map_values_foldl(Pred, Left0, Left, !A),
    Pred(V0, W0, !A),
    tree234.map_values_foldl(Pred, Right0, Right, !A),
    Tree = two(K0, W0, Left, Right).
map_values_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    tree234.map_values_foldl(Pred, Left0, Left, !A),
    Pred(V0, W0, !A),
    tree234.map_values_foldl(Pred, Middle0, Middle, !A),
    Pred(V1, W1, !A),
    tree234.map_values_foldl(Pred, Right0, Right, !A),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_values_foldl(Pred, Tree0, Tree, !A) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    tree234.map_values_foldl(Pred, Left0, Left, !A),
    Pred(V0, W0, !A),
    tree234.map_values_foldl(Pred, LMid0, LMid, !A),
    Pred(V1, W1, !A),
    tree234.map_values_foldl(Pred, RMid0, RMid, !A),
    Pred(V2, W2, !A),
    tree234.map_values_foldl(Pred, Right0, Right, !A),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

map_values_foldl2(_Pred, empty, empty, !A, !B).
map_values_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = two(K0, V0, Left0, Right0),
    tree234.map_values_foldl2(Pred, Left0, Left, !A, !B),
    Pred(V0, W0, !A, !B),
    tree234.map_values_foldl2(Pred, Right0, Right, !A, !B),
    Tree = two(K0, W0, Left, Right).
map_values_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    tree234.map_values_foldl2(Pred, Left0, Left, !A, !B),
    Pred(V0, W0, !A, !B),
    tree234.map_values_foldl2(Pred, Middle0, Middle, !A, !B),
    Pred(V1, W1, !A, !B),
    tree234.map_values_foldl2(Pred, Right0, Right, !A, !B),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_values_foldl2(Pred, Tree0, Tree, !A, !B) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    tree234.map_values_foldl2(Pred, Left0, Left, !A, !B),
    Pred(V0, W0, !A, !B),
    tree234.map_values_foldl2(Pred, LMid0, LMid, !A, !B),
    Pred(V1, W1, !A, !B),
    tree234.map_values_foldl2(Pred, RMid0, RMid, !A, !B),
    Pred(V2, W2, !A, !B),
    tree234.map_values_foldl2(Pred, Right0, Right, !A, !B),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

map_values_foldl3(_Pred, empty, empty, !A, !B, !C).
map_values_foldl3(Pred, Tree0, Tree, !A, !B, !C) :-
    Tree0 = two(K0, V0, Left0, Right0),
    tree234.map_values_foldl3(Pred, Left0, Left, !A, !B, !C),
    Pred(V0, W0, !A, !B, !C),
    tree234.map_values_foldl3(Pred, Right0, Right, !A, !B, !C),
    Tree = two(K0, W0, Left, Right).
map_values_foldl3(Pred, Tree0, Tree, !A, !B, !C) :-
    Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
    tree234.map_values_foldl3(Pred, Left0, Left, !A, !B, !C),
    Pred(V0, W0, !A, !B, !C),
    tree234.map_values_foldl3(Pred, Middle0, Middle, !A, !B, !C),
    Pred(V1, W1, !A, !B, !C),
    tree234.map_values_foldl3(Pred, Right0, Right, !A, !B, !C),
    Tree = three(K0, W0, K1, W1, Left, Middle, Right).
map_values_foldl3(Pred, Tree0, Tree, !A, !B, !C) :-
    Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
    tree234.map_values_foldl3(Pred, Left0, Left, !A, !B, !C),
    Pred(V0, W0, !A, !B, !C),
    tree234.map_values_foldl3(Pred, LMid0, LMid, !A, !B, !C),
    Pred(V1, W1, !A, !B, !C),
    tree234.map_values_foldl3(Pred, RMid0, RMid, !A, !B, !C),
    Pred(V2, W2, !A, !B, !C),
    tree234.map_values_foldl3(Pred, Right0, Right, !A, !B, !C),
    Tree = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % The default pretty_printer formatting for key_value_pair will do what
    % we want.
    %
:- type key_value_pair(K, V)
    --->    (K -> V).

tree234_to_doc(T) =
    indent([
        str("map(["),
        tree234_to_doc_2(tree234_to_lazy_list(T, empty)),
        str("])")
    ]).

:- func tree234_to_doc_2(lazy_list(K, V)) = doc.

tree234_to_doc_2(empty) = str("").
tree234_to_doc_2(lazy_cons(K, V, Susp)) = Doc :-
    LL = apply(Susp),
    (
        LL = empty,
        Doc = group([nl, format_arg(format((K -> V)))])
    ;
        LL = lazy_cons(_, _, _),
        Doc = docs([
            group([nl, format_arg(format((K -> V))), str(", ")]),
            format_susp((func) = tree234_to_doc_2(LL))
        ])
    ).

%---------------------------------------------------------------------------%

:- type lazy_list(K, V)
    --->    empty
    ;       lazy_cons(K, V, (func) = lazy_list(K, V)).

:- func tree234_to_lazy_list(tree234(K, V), lazy_list(K, V)) = lazy_list(K, V).

tree234_to_lazy_list(empty, LL) = LL.
tree234_to_lazy_list(two(K1, V1, T1, T2), LL) =
    tree234_to_lazy_list(T1,
        lazy_cons(K1, V1,
            (func) = tree234_to_lazy_list(T2, LL))).
tree234_to_lazy_list(three(K1, V1, K2, V2, T1, T2, T3), LL) =
    tree234_to_lazy_list(T1,
        lazy_cons(K1, V1,
            (func) = tree234_to_lazy_list(two(K2, V2, T2, T3), LL))).
tree234_to_lazy_list(four(K1, V1, K2, V2, K3, V3, T1, T2, T3, T4), LL) =
    tree234_to_lazy_list(T1,
        lazy_cons(K1, V1,
            (func) = tree234_to_lazy_list(
                three(K2, V2, K3, V3, T2, T3, T4), LL))).

%---------------------------------------------------------------------------%

find_min_size_based_on_depth(T, MinSize) :-
    find_depth(T, Depth),
    min_size_based_on_depth(Depth, MinSize).

:- pred min_size_based_on_depth(int::in, int::out) is det.

min_size_based_on_depth(Depth, MinSize) :-
    ( if Depth = 0 then
        MinSize = 0
    else
        min_size_based_on_depth(Depth - 1, MinSizeBelow),
        MinSize = MinSizeBelow * 2 + 1
    ).

:- pred find_depth(tree234(K, V)::in, int::out) is det.

find_depth(empty, 0).
find_depth(two(_, _, T1, _), Depth + 1) :-
    find_depth(T1, Depth).
find_depth(three(_, _, _, _, T1, _, _), Depth + 1) :-
    find_depth(T1, Depth).
find_depth(four(_, _, _, _, _, _, T1, _, _, _), Depth + 1) :-
    find_depth(T1, Depth).

%---------------------------------------------------------------------------%

well_formed(Tree, WellFormed) :-
    depth_levels(Tree, 0, set.init, Depths),
    ( if set.is_singleton(Depths, Depth) then
        WellFormed = yes(Depth)
    else
        WellFormed = no
    ).

:- pred depth_levels(tree234(K, V)::in, int::in,
    set(int)::in, set(int)::out) is det.

depth_levels(empty, Depth, !Depths) :-
    set.insert(Depth, !Depths).
depth_levels(two(_, _, T1, T2), Depth, !Depths) :-
    NextDepth = Depth + 1,
    depth_levels(T1, NextDepth, !Depths),
    depth_levels(T2, NextDepth, !Depths).
depth_levels(three(_, _, _, _, T1, T2, T3), Depth, !Depths) :-
    NextDepth = Depth + 1,
    depth_levels(T1, NextDepth, !Depths),
    depth_levels(T2, NextDepth, !Depths),
    depth_levels(T3, NextDepth, !Depths).
depth_levels(four(_, _, _, _, _, _, T1, T2, T3, T4), Depth, !Depths) :-
    NextDepth = Depth + 1,
    depth_levels(T1, NextDepth, !Depths),
    depth_levels(T2, NextDepth, !Depths),
    depth_levels(T3, NextDepth, !Depths),
    depth_levels(T4, NextDepth, !Depths).

%---------------------------------------------------------------------------%
:- end_module tree234.
%---------------------------------------------------------------------------%
