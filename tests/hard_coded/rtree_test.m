%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module rtree_test.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module math.
:- import_module rtree.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

:- type irtree == rtree(interval, int).

%---------------------------------------------------------------------------%

main(!IO) :-
    some [!RTree] (
        !:RTree = rtree.init,
        add_integers(0, 100, !RTree),

        % Find all integers between 33.0 and 66.0.
        some [!Is] (
            !:Is = rtree.search_intersects(!.RTree, interval(33.0, 66.0)),
            list.sort(!Is),
            io.write_string("Integers from 33 to 66:\n", !IO),
            io.write(!.Is, !IO),
            io.nl(!IO),
            io.nl(!IO)
        ),

        % Find integer 22.
        some [!Is] (
            !:Is = rtree.search_intersects(!.RTree, interval(22.0, 22.0)),
            io.write_string("Integers from 22 to 22:\n", !IO),
            io.write(!.Is, !IO),
            io.nl(!IO),
            io.nl(!IO)
        ),

        % Find all prime numbers.
        some [!Is] (
            !:Is = rtree.search_general(any_is_prime, true1, !.RTree),
            list.sort(!Is),
            io.write_string("Primes from 0 to 100:\n", !IO),
            io.write(!.Is, !IO),
            io.nl(!IO),
            io.nl(!IO)
        ),

        % Find the first prime number.
        io.write_string("First prime from 0 to 100:\n", !IO),
        ( if rtree.search_first(any_is_prime, id, !.RTree, 100.0, L, _) then
            io.write(L, !IO),
            io.nl(!IO),
            io.nl(!IO)
        else
            io.write_string("search_first FAILED!\n\n", !IO)
        ),

        % Delete all odd numbers.
        io.write_string("All odds deleted, " ++
            "remaining integers from 33 to 66:\n", !IO),
        some [!Is] (
            ( if delete_odd(1, 100, !RTree) then
                !:Is = rtree.search_intersects(!.RTree, interval(33.0, 66.0)),
                list.sort(!Is),
                io.write(!.Is, !IO),
                io.nl(!IO),
                io.nl(!IO)
            else
                io.write_string("delete FAILED!\n\n", !IO)
            )
        )
    ).

%---------------------------------------------------------------------------%

:- pred true1(T::in) is semidet.

true1(_) :-
    semidet_succeed.

:- pred id(int::in, float::out) is semidet.

id(I, float(I)) :-
    semidet_succeed.

%---------------------------------------------------------------------------%

:- pred add_integers(int::in, int::in, irtree::in, irtree::out) is det.

add_integers(N, M, !RTree) :-
    ( if N >= M then
        true
    else
        NF = float(N),
        K = interval(NF, NF),
        insert(K, N, !RTree),
        add_integers(N + 1, M, !RTree)
    ).

%---------------------------------------------------------------------------%

:- pred test_range(bool::in, int::in, int::in, irtree::in,
    list(int)::out, bool::out) is det.

test_range(Cnts, Mn, Mx, RT, Is, P) :-
    K = interval(float(Mn), float(Mx)),
    (
        Cnts = yes,
        Is0 = search_contains(RT, K)
    ;
        Cnts = no,
        Is0 = search_intersects(RT, K)
    ),
    list.sort(Is0, Is),
    P = check_range(Mn, Mx, Is).

%---------------------------------------------------------------------------%

:- func check_range(int, int, list(int)) = bool.

check_range(N, M, Is) = P :-
    ( if N > M then
        (
            Is = [] ,
            P = yes
        ;
            Is = [_ | _],
            P = no
        )
    else
        ( if Is = [N | Is1] then
            P = check_range(N + 1, M, Is1)
        else
            P = no
        )
    ).

%---------------------------------------------------------------------------%

    % A very naive prime number test.
    %
:- pred any_is_prime(interval::in) is semidet.

any_is_prime(I) :-
    any_is_prime(I, _).

%---------------------------------------------------------------------------%

:- pred any_is_prime(interval::in, float::out) is semidet.

any_is_prime(interval(Min, Max), P) :-
    Min1 = ceiling(Min),
    Max1 = floor(Max),
    any_is_prime(Min1, Max1, P).

%---------------------------------------------------------------------------%

:- pred any_is_prime(float::in, float::in, float::out) is semidet.

any_is_prime(Min, Max, P) :-
    Min =< Max,
    ( if is_prime(Min) then
        P = Min
    else
        any_is_prime(Min + 1.0, Max, P)
    ).

%---------------------------------------------------------------------------%

:- pred is_prime(float::in) is semidet.

is_prime(N) :-
    NI = floor_to_int(N),
    NI > 1,
    MaxD = round_to_int(sqrt(N)),
    none_divides(2, MaxD, NI).

%---------------------------------------------------------------------------%

:- pred none_divides(int::in, int::in, int::in) is semidet.

none_divides(N, M, I) :-
    ( if N > M then
        true
    else if I mod N = 0 then
        false
    else
        none_divides(N + 1, M, I)
    ).

%---------------------------------------------------------------------------%

    % Assumption: N is odd.
    %
:- pred delete_odd(int::in, int::in, irtree::in, irtree::out) is semidet.

delete_odd(N, M, !RT) :-
    ( if N >= M then
        true
    else
        NF = float(N),
        I = interval(NF, NF),
        delete(I, N, !RT),
        delete_odd(N + 2, M, !RT)
    ).
