%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the use of explicit type qualification using the : operator.

:- module type_qual.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module type_desc.

main(!IO) :-
    test1(!IO),
    test2([] : list(io.state), !IO),
    test3(!IO),
    test4(!IO),
    test5(yes, !IO),
    test5(no, !IO),
    test6(!IO),
    test7(!IO).

:- pred test1(io.state::di, io.state::uo) is det.

test1(!IO) :-
    io.write_string("test1\n", !IO),
    io.read(X : io.read_result(int), !IO),
    io.write_line(X, !IO).

:- pred test2(T::in, io.state::di, io.state::uo) is det.

test2(X, !IO) :-
    io.write_string("test2\n", !IO),
    io.write_line(type_of(X : T), !IO),
    io.write_line(type_of(_ : list(T)), !IO).

:- pred test3(io.state::di, io.state::uo) is det.

test3(!IO) :-
    io.write_string("test3\n", !IO),
    io.write_line(empty_list, !IO),
    io.write_line(type_of(empty_list), !IO),
    empty(X),
    io.write_line(X, !IO),
    io.write_line(type_of(X), !IO).

:- pred test4(io.state::di, io.state::uo) is det.

test4(!IO) :-
    io.write_string("test4\n", !IO),
    List = build_list : TypeOfList,
    io.write_line(type_of(List), !IO),
    io.write_line(List, !IO),
    EmptyList = [] : TypeOfList,
    io.write_line(type_of(EmptyList), !IO),
    io.write_line(EmptyList, !IO).

    % Test use of same type variable in different clauses.
    %
:- pred test5(bool::in, io.state::di, io.state::uo) is det.

test5(yes, !IO) :-
    io.write_string("test5 yes\n", !IO),
    _ = [1, 2, 3] : T,
    Y = [] : T,
    io.write_line(type_of(Y), !IO),
    io.write_line(Y, !IO).
test5(no, !IO) :-
    io.write_string("test5 no\n", !IO),
    _ = ["1", "2", "3"] : T,
    Y = [] : T,
    io.write_line(type_of(Y), !IO),
    io.write_line(Y, !IO).

:- pred test6(io.state::di, io.state::uo) is det.

test6(!IO) :-
    io.write_string("test6\n", !IO),
    ( if
        (
            X = type_of([] : list(int))
        <=>
            X = type_of([1, 2, 3])
        )
    then
        io.write_string("bi-implication succeeded\n", !IO)
    else
        io.write_string("bi-implication failed\n", !IO)
    ).

:- pred test7(io.state::di, io.state::uo) is det.

test7(!IO) :-
    io.write_string("test7\n", !IO),
    % Test the bi-implication in both directions, since for efficiency,
    % quantification handles the LHS and RHS differently.
    ( if
        (
            X = type_of([1, 2, 3])
        <=>
            X = type_of([] : list(int))
        )
    then
        io.write_string("bi-implication succeeded\n", !IO)
    else
        io.write_string("bi-implication failed\n", !IO)
    ).

% inferred
empty_list = [] : list(int).

% inferred
empty([] : list(int)).

:- some [T] func build_list = list(T).

build_list = ["a", "b", "c"].

:- type my_map(_K, V) == map(int, V).

:- pred map_search(my_map(K, V)::in, int::in, V::out) is semidet.

map_search(Map : map(int, V), Key : int, Value : V) :-
    map.search(Map, Key, Value).
