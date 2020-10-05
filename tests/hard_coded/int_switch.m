%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module int_switch.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.
:- import_module string.

main(!IO) :-
    Keys =
        [1, 2, 3,
        11, 12, 13,
        21, 22, 23,
        31, 32, 33],

    % Test jump tables.
    list.foldl(test_jump, Keys, !IO),

    % Test semidet and det lookup tables.
    list.foldl(test_one, Keys, !IO),
    list.foldl(test_one_known, Keys, !IO),

    % Test nondet and multi lookup tables.
    list.foldl(test_several, Keys, !IO),
    list.foldl(test_several_known, Keys, !IO),
    list.foldl(test_several_nested, Keys, !IO).

%---------------------------------------------------------------------------%

:- pred test_jump(int::in, io::di, io::uo) is det.

test_jump(S, !IO) :-
    ( if jump(S, 50, N) then
        io.format("jump %d -> %d\n", [i(S), i(N)], !IO)
    else
        io.format("jump %d failed\n", [i(S)], !IO)
    ).

:- pred test_one(int::in, io::di, io::uo) is det.

test_one(S, !IO) :-
    ( if one(S, N) then
        io.format("one %d -> %d\n", [i(S), i(N)], !IO)
    else
        io.format("one %d failed\n", [i(S)], !IO)
    ).

:- pred test_one_known(int::in, io::di, io::uo) is det.

test_one_known(S, !IO) :-
    ( if
        ( S = 11
        ; S = 22
        ),
        one(S, N)
    then
        io.format("one known %d -> %d\n", [i(S), i(N)], !IO)
    else
        io.format("one known %d failed\n", [i(S)], !IO)
    ).

:- pred test_several(int::in, io::di, io::uo) is det.

test_several(S, !IO) :-
    solutions(several_unknown(S), Solns),
    io.format("several %d -> ", [i(S)], !IO),
    io.write(Solns, !IO),
    io.nl(!IO).

:- pred test_several_known(int::in, io::di, io::uo) is det.

test_several_known(S, !IO) :-
    solutions(several_known(S), Solns),
    io.format("several known %d -> ", [i(S)], !IO),
    io.write(Solns, !IO),
    io.nl(!IO).

:- pred test_several_nested(int::in, io::di, io::uo) is det.

test_several_nested(S, !IO) :-
    solutions(several_nested(S), Solns),
    io.format("several nested %d -> ", [i(S)], !IO),
    io.write(Solns, !IO),
    io.nl(!IO).

%---------------------------------------------------------------------------%

:- pred jump(int::in, int::in, int::out) is semidet.

jump(S, N0, N) :-
    (
        S = 1,
        N = N0 + 1
    ;
        S = 2,
        N = N0 + 2
    ;
        ( S = 11
        ; S = 12
        ),
        N = 11
    ;
        ( S = 21
        ; S = 22
        ),
        N = N0 + 12
    ;
        ( S = 31
        ; S = 34
        ; S = 35
        ),
        N = 13
    ;
        S = 77,
        N = 21
    ;
        S = 78,
        N = 22
    ;
        S = 79,
        N = 23
    ;
        S = 87,
        N = 24
    ).

%---------------------------------------------------------------------------%

:- inst aa_bb
    --->    11 ; 22.

:- pred one(int, int).
:- mode one(in(aa_bb), out) is det.
:- mode one(in, out) is semidet.

one(S, N) :-
    (
        S = 1,
        N = 1
    ;
        S = 2,
        N = 2
    ;
        ( S = 11
        ; S = 12
        ),
        N = 11
    ;
        ( S = 21
        ; S = 22
        ),
        N = 12
    ;
        ( S = 31
        ; S = 34
        ; S = 35
        ),
        N = 13
    ;
        S = 77,
        N = 21
    ;
        S = 78,
        N = 22
    ;
        S = 79,
        N = 23
    ;
        S = 87,
        N = 24
    ).

%---------------------------------------------------------------------------%

:- pred several_unknown(int::in, int::out) is nondet.

several_unknown(S, N) :-
    several(S, N).

:- pred several_known(int::in, int::out) is nondet.

several_known(S, N) :-
    ( S = 11
    ; S = 22
    ),
    several(S, N).

:- pred several(int, int).
:- mode several(in(aa_bb), out) is multi.
:- mode several(in, out) is nondet.

several(S, N) :-
    (
        S = 1,
        N = 1
    ;
        S = 2,
        N = 2
    ;
        ( S = 11
        ; S = 12
        ),
        ( N = 11
        ; N = 12
        )
    ;
        ( S = 21
        ; S = 22
        ),
        ( N = 13
        ; N = 14
        ; N = 15
        )
    ;
        ( S = 31
        ; S = 32
        ; S = 34
        ; S = 35
        ),
        N = 16
    ;
        S = 77,
        N = 21
    ;
        S = 78,
        N = 22
    ;
        S = 79,
        N = 23
    ;
        S = 87,
        ( N = 24
        ; N = 25
        ; N = 26
        )
    ).

%---------------------------------------------------------------------------%

:- pred several_nested(int::in, int::out) is nondet.

several_nested(S0, R) :-
    (
        S = S0
    ;
        ( if S0 < 10 then
            S = 10 + S0
        else
            S = 100 + S0
        )
    ),
    (
        S = 1,
        N = 1
    ;
        S = 3,
        N = 2
    ;
        ( S = 11
        ; S = 12
        ),
        ( N = 11
        ; N = 12
        )
    ;
        ( S = 21
        ; S = 22
        ),
        ( N = 13
        ; N = 14
        ; N = 15
        )
    ;
        ( S = 31
        ; S = 32
        ; S = 34
        ; S = 35
        ),
        N = 16
    ;
        S = 77,
        N = 21
    ;
        S = 78,
        N = 22
    ;
        S = 79,
        N = 23
    ;
        S = 87,
        ( N = 24
        ; N = 25
        ; N = 26
        )
    ),
    (
        S0 = 1,
        M = 1
    ;
        S0 = 2,
        M = 2
    ;
        ( S0 = 11
        ; S0 = 12
        ),
        ( M = 11
        ; M = 12
        )
    ;
        ( S0 = 21
        ; S0 = 22
        ),
        ( M = 13
        ; M = 14
        ; M = 15
        )
    ;
        ( S0 = 31
        ; S0 = 32
        ; S0 = 34
        ; S0 = 35
        ),
        M = 16
    ;
        S0 = 77,
        M = 21
    ;
        S0 = 78,
        M = 22
    ;
        S0 = 79,
        M = 23
    ;
        S0 = 87,
        ( M = 24
        ; M = 25
        ; M = 26
        )
    ),
    R = 1000 * N + M.
