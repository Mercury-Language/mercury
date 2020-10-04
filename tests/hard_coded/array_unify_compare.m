%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test unification and comparison for arrays.
%
%---------------------------------------------------------------------------%

:- module array_unify_compare.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
   test_unify(test_arrays, test_arrays, !IO),
   io.nl(!IO),
   test_compare(test_arrays, test_arrays, !IO).

:- pred test_unify(list(list(int))::in, list(list(int))::in,
    io::di, io::uo) is det.

test_unify([], _, !IO).
test_unify([A | As], Bs, !IO) :-
    do_test_unify(array(A), Bs, !IO),
    test_unify(As, Bs, !IO).

:- pred do_test_unify(array(int)::in, list(list(int))::in,
    io::di, io::uo) is det.

do_test_unify(_, [], !IO).
do_test_unify(ArrayA, [B | Bs], !IO) :-
    ArrayB = array(B),
    ( if ArrayA = ArrayB then
        Result  = "true"
    else
        Result = "false"
    ),
    io.format("unify(%s, %s) ==> %s\n",
        [s(string(ArrayA)), s(string(ArrayB)), s(Result)], !IO),
    do_test_unify(ArrayA, Bs, !IO).

:- pred test_compare(list(list(int))::in, list(list(int))::in,
    io::di, io::uo) is det.

test_compare([], _, !IO).
test_compare([A | As], Bs, !IO) :-
    do_test_compare(array(A), Bs, !IO),
    test_compare(As, Bs, !IO).

:- pred do_test_compare(array(int)::in, list(list(int))::in, io::di, io::uo)
    is det.

do_test_compare(_, [], !IO).
do_test_compare(ArrayA, [B | Bs], !IO) :-
    ArrayB = array(B),
    compare(Result, ArrayA, ArrayB),
    io.format("compare(%s, %s) ==> %s\n",
        [s(string(ArrayA)), s(string(ArrayB)), s(string(Result))], !IO),
    do_test_compare(ArrayA, Bs, !IO).

:- func test_arrays = list(list(int)).

test_arrays = [
    [],
    [1],
    [1, 2],
    [2, 1],
    [2]
].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
