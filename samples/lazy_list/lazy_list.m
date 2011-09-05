%-----------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%-----------------------------------------------------------------------------%
%
% This is an example of how to use the `lazy' module to define
% a recursive lazy data type, in this case lazy lists.
% It also defines a small number of functions and predicates
% that operate on lazy lists.
%
% See also lazy_list_test.m, which is an example program using this module.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
% Modified by Paul Bone (2011) for compatibilty with the lazy module in
% Mercury's standard library.

:- module lazy_list.
:- interface.
:- import_module lazy, int, list.

%-----------------------------------------------------------------------------%

    % The definition of the type `lazy_list(T)':
    %    A lazy lazy_list is either an empty lazy_list, denoted `[]',
    %    or an element `Head' of type `T' followed by a lazily
    %    evaluated tail `Tail', of type `lazy(lazy_list(T))',
    %    denoted `[Head | Tail]'.

:- type lazy_list(T) ---> [] ; [T | lazy(lazy_list(T))].

%-----------------------------------------------------------------------------%

    % force evaluation of (the top level of) a lazy list
:- func force_list(lazy(lazy_list(T))) = lazy_list(T).

%-----------------------------------------------------------------------------%

    % Convert a lazy_list to an ordinary list.
:- func to_list(lazy_list(T)) = list(T).

    % Convert an ordinary list to a lazy_list.
:- func from_list(list(T)) = lazy_list(T).

%-----------------------------------------------------------------------------%

    % A lazy_list function version of the usual append predicate:
    % append(Start, End) = List is true iff
    % `List' is the result of concatenating `Start' and `End'.
    %
:- func append(lazy_list(T), lazy(lazy_list(T))) = lazy_list(T).

    % member(Elem, List) :
    %    True iff `List' contains `Elem'.
:- pred member(T, lazy_list(T)).
:- mode member(in, in) is semidet.
:- mode member(out, in) is nondet.

%-----------------------------------------------------------------------------%

    % iterate(F, X0) = [X0, F(X0), F(F(X0)), F(F(F(X0))), ...]
:- func iterate(func(T) = T, T) = lazy_list(T).
:- mode iterate(func(in) = out is det, in) = out is det.

    % take(N, L) returns the first N elements of L
:- func take(int, lazy_list(T)) = lazy_list(T).

    % map(F, [X0, X1, X2, ...]) = [F(X0), F(X1), F(X2), ...].
:- func map(func(X) = Y, lazy_list(X)) = lazy_list(Y).
:- mode map(func(in) = out is det, in) = out is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

force_list(Xs) = force(Xs).

%-----------------------------------------------------------------------------%

to_list([]) = [].
to_list([X | Xs]) = [X | to_list(force_list(Xs))].

from_list([]) = [].
from_list([X | Xs]) =
    [X | val(from_list(Xs))].

%-----------------------------------------------------------------------------%

append([], Ys) = force_list(Ys).
append([X | Xs], Ys) =
    [X | delay((func) = R :- R = append(force_list(Xs), Ys))].

member(X, [X | _]).
member(X, [_ | Xs]) :-
    member(X, force_list(Xs)).

%-----------------------------------------------------------------------------%

map(_, []) = [].
map(F, [H|T]) = [F(H) | delay((func) = R :- R = map(F, force_list(T)))].

iterate(F, X0) = [X0 | delay((func) = R :- R = iterate(F, F(X0)))].

take(_, []) = [].
take(N, [X|Xs]) =
    (if N > 0 then
        [X | delay((func) = R :- R = take(N-1, force_list(Xs)))]
    else
        []
    ).

%-----------------------------------------------------------------------------%
