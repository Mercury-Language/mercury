%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module test_cord3.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module list.
:- import_module pair.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    list.foldl(test_is_empty, test_cords, !IO),
    io.nl(!IO),
    list.foldl(test_is_singleton, test_cords, !IO).

%---------------------------------------------------------------------------%

:- pred test_is_empty(pair(string, cord(int))::in, io::di, io::uo) is det.

test_is_empty(Name - Cord, !IO) :-
    Result = ( if cord.is_empty(Cord) then "empty" else "not empty" ),
    io.format("%s is %s.\n", [s(Name), s(Result)], !IO).

%---------------------------------------------------------------------------%

:- pred test_is_singleton(pair(string, cord(int))::in, io::di, io::uo) is det.

test_is_singleton(Name - Cord, !IO) :-
    ( if cord.is_singleton(Cord, Result) then
        io.format("%s is a singleton with element %d.\n",
            [s(Name), i(Result)], !IO)
    else
        io.format("%s is not a singleton.\n", [s(Name)], !IO)
    ).

%---------------------------------------------------------------------------%

:- func test_cords = list(pair(string, cord(int))).

test_cords = [
    "Empty" - cord.empty,
    "Singleton1" - cord.singleton(1),
    "Singleton2" - cord.from_list([2]),
    "NonSingleton1" - cord.from_list([3, 4]),
    "NonSingleton2" - (cord.singleton(5) ++ cord.singleton(6))
].

%---------------------------------------------------------------------------%
:- end_module test_cord3.
%---------------------------------------------------------------------------%
