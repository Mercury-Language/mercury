%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2004-2006, 2011 The University of Melbourne.
% Copyright (C) 2013-2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: version_store.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
% Stability: low.
%
% See the header comments in version_array.m for an explanation of version
% types.
%
% A version_store is similar to, albeit slightly slower than, an ordinary
% store, but does not depend upon uniqueness.
%
% Note that, unlike ordinary stores, liveness of data is via the version store
% rather than the mutvars. This means that dead data (i.e. data whose mutvar
% is out of scope) in a version_store may not be garbage collected.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module version_store.
:- interface.

%---------------------------------------------------------------------------%

:- type version_store(S).

:- type mutvar(T, S).

    % Construct a new version store. This is distinguished from other
    % version stores by its existentially quantified type. This means
    % the compiler can automatically detect any attempt to use a mutvar
    % with the wrong version store.
    %
:- some [S] func init = version_store(S).

    % new_mutvar(X, Mutvar, VS0, VS) adds a new mutvar with value reference X
    % to the version store.
    %
:- pred new_mutvar(T::in, mutvar(T, S)::out,
    version_store(S)::in, version_store(S)::out) is det.

    % new_cyclic_mutvar(F, Mutvar, VS0, VS) adds a new mutvar with value
    % reference F(Mutvar) to the version store. This can be used to
    % construct cyclic terms.
    %
:- pred new_cyclic_mutvar((func(mutvar(T, S)) = T)::in, mutvar(T, S)::out,
    version_store(S)::in, version_store(S)::out) is det.

    % copy_mutvar(Mutvar, NewMutvar, VS0, VS) constructs NewMutvar
    % with the same value reference as Mutvar.
    %
:- pred copy_mutvar(mutvar(T, S)::in, mutvar(T, S)::out,
    version_store(S)::in, version_store(S)::out) is det.

    % VS ^ elem(Mutvar) returns the element referenced by Mutvar in
    % the version store.
    %
:- func version_store(S) ^ elem(mutvar(T, S)) = T.

    % lookup(VS, Mutvar) = VS ^ elem(Mutvar).
    %
    % A predicate version is also provided.
    %
:- func lookup(version_store(S), mutvar(T, S)) = T.
:- pred get_mutvar(mutvar(T, S)::in, T::out,
    version_store(S)::in, version_store(S)::out) is det.

    % ( VS ^ elem(Mutvar) := X ) updates the version store so that
    % Mutvar now refers to value X.
    %
:- func ( version_store(S) ^ elem(mutvar(T, S)) := T ) = version_store(S).

    % set(VS, Mutvar, X) = ( VS ^ elem(Mutvar) := X ).
    %
    % A predicate version is also provided.
    %
:- func set(version_store(S), mutvar(T, S), T) = version_store(S).
:- pred set_mutvar(mutvar(T, S)::in, T::in,
    version_store(S)::in, version_store(S)::out) is det.

    % unsafe_rewind(VS) produces a version of VS for which all accesses
    % are O(1). Invoking this predicate renders undefined VS and all later
    % versions undefined that were derived by performing individual updates.
    % Only use this when you are absolutely certain there are no live
    % references to VS or later versions of VS.
    %
    % A predicate version is also provided.
    %
:- func unsafe_rewind(version_store(T)) = version_store(T).
:- pred unsafe_rewind(version_store(T)::in, version_store(T)::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module counter.
:- import_module int.
:- import_module unit.
:- import_module univ.
:- import_module version_array.

%---------------------------------------------------------------------------%

    % Index 0 of the version_store contains the counter used to assign
    % new version store mutvars. A mutvar is just an index into the
    % version_store.
    %
:- type version_store(S)        ---> version_store(version_array(univ)).
:- type mutvar(T, S)            ---> mutvar(int).
:- type some_version_store_type ---> some_version_store_type.

%---------------------------------------------------------------------------%

init = version_store(VA) `with_type` version_store(some_version_store_type) :-
    % 256 is just a magic number. The version_store is resized by doubling
    % if necessary when adding a new mutvar. Index 0 of the version_store
    % holds a counter for allocating new mutvars.
    VA = version_array.init(256, univ(counter.init(1) `with_type` counter)).

%---------------------------------------------------------------------------%

new_mutvar(X, Mutvar, VS0, VS) :-
    new_cyclic_mutvar(func(_) = X, Mutvar, VS0, VS).

%---------------------------------------------------------------------------%

new_cyclic_mutvar(F, Mutvar, VS0, VS) :-
    Counter0 = VS0 ^ elem(mutvar(0)),
    counter.allocate(I, Counter0, Counter),
    Mutvar = mutvar(I),
    Size0 = size(VS0),
    ( if I >= Size0 then
        VS1 = resize(VS0, Size0 + Size0)
    else
        VS1 = VS0
    ),
    VS  = (( VS1 ^ elem(mutvar(0)) := Counter   )
                 ^ elem(Mutvar   ) := F(Mutvar) ).

:- func size(version_store(S)) = int.

size(version_store(VA)) = size(VA).

:- func resize(version_store(S), int) = version_store(S).

resize(version_store(VA), N) = version_store(resize(VA, N, univ(unit))).

%---------------------------------------------------------------------------%

copy_mutvar(Mutvar0, Mutvar, VS0, VS) :-
    X = VS0 ^ elem(Mutvar0),
    new_mutvar(X, Mutvar, VS0, VS).

%---------------------------------------------------------------------------%

version_store(VA) ^ elem(mutvar(I)) = X :-
    UnivX = lookup(VA, I),
    det_univ_to_type(UnivX, X).

lookup(VS, Mutvar) = VS ^ elem(Mutvar).

get_mutvar(Mutvar, VS ^ elem(Mutvar), VS, VS).

%---------------------------------------------------------------------------%

( version_store(VA) ^ elem(mutvar(I)) := X ) =
    version_store(VA ^ elem(I) := univ(X)).

set(VS, Mutvar, X) = ( VS ^ elem(Mutvar) := X ).

set_mutvar(Mutvar, X, VS, VS ^ elem(Mutvar) := X).

%---------------------------------------------------------------------------%

unsafe_rewind(version_store(VA)) = version_store(unsafe_rewind(VA)).

unsafe_rewind(VS, unsafe_rewind(VS)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
