%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% The codes of the tests
%
%   string_switch.m
%   string_switch2.m
%   string_switch2.m
%   string_switch4.m
%
% are all identical, but we compile them with different compilation options.
%

:- module string_switch4.

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
    TestStrs = [
        "a", "b", "c",
        "aa", "ab", "ac",
        "ba", "bb", "bc",
        "ca", "cb", "cc", "cf",
        "xxx", "xxxxxxΓ", "xxxxxxx",
        "Γ", "Δ", "Θ", "Σ", "Σζ"
    ],

    % Test jump tables.
    list.foldl(test_jump, TestStrs, !IO),

    % Test semidet and det lookup tables.
    list.foldl(test_one, TestStrs, !IO),
    list.foldl(test_one_known, TestStrs, !IO),

    % Test nondet and multi lookup tables.
    list.foldl(test_several, TestStrs, !IO),
    list.foldl(test_several_known, TestStrs, !IO),
    list.foldl(test_several_nested, TestStrs, !IO).

%---------------------------------------------------------------------------%

:- pred test_jump(string::in, io::di, io::uo) is det.

test_jump(S, !IO) :-
    ( if jump(S, 50, N) then
        io.format("jump %s -> %d\n", [s(S), i(N)], !IO)
    else
        io.format("jump %s failed\n", [s(S)], !IO)
    ).

:- pred test_one(string::in, io::di, io::uo) is det.

test_one(S, !IO) :-
    ( if one(S, N) then
        io.format("one %s -> %d\n", [s(S), i(N)], !IO)
    else
        io.format("one %s failed\n", [s(S)], !IO)
    ).

:- pred test_one_known(string::in, io::di, io::uo) is det.

test_one_known(S, !IO) :-
    ( if
        ( S = "aa"
        ; S = "bb"
        ),
        one(S, N)
    then
        io.format("one known %s -> %d\n", [s(S), i(N)], !IO)
    else
        io.format("one known %s failed\n", [s(S)], !IO)
    ).

:- pred test_several(string::in, io::di, io::uo) is det.

test_several(S, !IO) :-
    solutions(several_unknown(S), Solns),
    io.format("several %s -> ", [s(S)], !IO),
    io.write_line(Solns, !IO).

:- pred test_several_known(string::in, io::di, io::uo) is det.

test_several_known(S, !IO) :-
    solutions(several_known(S), Solns),
    io.format("several known %s -> ", [s(S)], !IO),
    io.write_line(Solns, !IO).

:- pred test_several_nested(string::in, io::di, io::uo) is det.

test_several_nested(S, !IO) :-
    solutions(several_nested(S), Solns),
    io.format("several nested %s -> ", [s(S)], !IO),
    io.write_line(Solns, !IO).

%---------------------------------------------------------------------------%

:- pred jump(string::in, int::in, int::out) is semidet.

jump(S, N0, N) :-
    (
        S = "a",
        N = N0 + 1
    ;
        S = "b",
        N = N0 + 2
    ;
        ( S = "aa"
        ; S = "ab"
        ),
        N = 11
    ;
        ( S = "ba"
        ; S = "bb"
        ),
        N = N0 + 12
    ;
        ( S = "ca"
        ; S = "cd"
        ; S = "ce"
        ),
        N = 13
    ;
        ( S = "cb"
        ; S = "cg"
        ; S = "ch"
        ),
        N = 14
    ;
        ( S = "cf"
        ; S = "ci"
        ),
        N = 15
    ;
        S = "xxx",
        N = 21
    ;
        S = "xxy",
        N = 22
    ;
        S = "xxz",
        N = 23
    ;
        S = "xyx",
        N = 24
    ;
        % The next two arms test the treatment of sticks by trie switches.
        % The stick ends in the *middle* of the last code point, since
        %
        % - the last code point of each string has two UTF-8 code units, and
        % - the first of those code units is the same in the two arms.
        S = "xxxxxxΓ",
        N = 25
    ;
        S = "xxxxxxΔ",
        N = 26
    ;
        S = "Γ",    % greek capital gamma, UC+0393
        N = 27
    ;
        S = "Δ",    % greek capital delta, UC+0394
        N = 28
    ;
        ( S = "Θ"   % greek capital theta, UC+0398
        ; S = "Σ"   % greek capital sigma, UC+03A3
        ),
        N = 29
    ).

%---------------------------------------------------------------------------%

:- inst aa_bb
    --->    "aa"
    ;       "bb".

:- pred one(string, int).
:- mode one(in(aa_bb), out) is det.
:- mode one(in, out) is semidet.

one(S, N) :-
    (
        S = "a",
        N = 1
    ;
        S = "b",
        N = 2
    ;
        ( S = "aa"
        ; S = "ab"
        ),
        N = 11
    ;
        ( S = "ba"
        ; S = "bb"
        ),
        N = 12
    ;
        ( S = "ca"
        ; S = "cd"
        ; S = "ce"
        ),
        N = 13
    ;
        ( S = "cb"
        ; S = "cg"
        ; S = "ch"
        ),
        N = 14
    ;
        ( S = "cf"
        ; S = "ci"
        ),
        N = 15
    ;
        S = "xxx",
        N = 21
    ;
        S = "xxy",
        N = 22
    ;
        S = "xxz",
        N = 23
    ;
        S = "xyx",
        N = 24
    ;
        S = "xxxxxxΓ",
        N = 25
    ;
        S = "xxxxxxΔ",
        N = 26
    ;
        S = "Γ",    % greek capital gamma, UC+0393
        N = 27
    ;
        S = "Δ",    % greek capital delta, UC+0394
        N = 28
    ;
        ( S = "Θ"   % greek capital theta, UC+0398
        ; S = "Σ"   % greek capital sigma, UC+03A3
        ),
        N = 29
    ).

%---------------------------------------------------------------------------%

:- pred several_unknown(string::in, int::out) is nondet.

several_unknown(S, N) :-
    several(S, N).

:- pred several_known(string::in, int::out) is nondet.

several_known(S, N) :-
    ( S = "aa"
    ; S = "bb"
    ),
    several(S, N).

:- pred several(string, int).
:- mode several(in(aa_bb), out) is multi.
:- mode several(in, out) is nondet.

several(S, N) :-
    (
        S = "a",
        N = 1
    ;
        S = "b",
        N = 2
    ;
        ( S = "aa"
        ; S = "ab"
        ),
        ( N = 11
        ; N = 12
        )
    ;
        ( S = "ba"
        ; S = "bb"
        ),
        ( N = 13
        ; N = 14
        ; N = 15
        )
    ;
        ( S = "ca"
        ; S = "cb"
        ; S = "cd"
        ; S = "ce"
        ),
        N = 16
    ;
        ( S = "cg"
        ; S = "ch"
        ),
        N = 17
    ;
        ( S = "cf"
        ; S = "ci"
        ),
        ( N = 18
        ; N = 19
        )
    ;
        S = "xxx",
        N = 21
    ;
        S = "xxy",
        N = 22
    ;
        S = "xxz",
        N = 23
    ;
        S = "xyx",
        ( N = 24
        ; N = 25
        ; N = 26
        )
    ;
        S = "xxxxxxΓ",
        N = 25
    ;
        S = "xxxxxxΔ",
        ( N = 26
        ; N = 27
        )
    ;
        S = "Γ",    % greek capital gamma, UC+0393
        N = 27
    ;
        S = "Δ",    % greek capital delta, UC+0394
        N = 28
    ;
        ( S = "Θ"   % greek capital theta, UC+0398
        ; S = "Σ"   % greek capital sigma, UC+03A3
        ),
        ( N = 29
        ; N = 30
        )
    ).

%---------------------------------------------------------------------------%

:- pred several_nested(string::in, int::out) is nondet.

several_nested(S0, R) :-
    (
        S = S0
    ;
        S = "a" ++ S0
    ),
    (
        S = "a",
        N = 1
    ;
        S = "b",
        N = 2
    ;
        ( S = "aa"
        ; S = "ab"
        ),
        ( N = 11
        ; N = 12
        )
    ;
        ( S = "ba"
        ; S = "bb"
        ),
        ( N = 13
        ; N = 14
        ; N = 15
        )
    ;
        ( S = "ca"
        ; S = "cb"
        ; S = "cd"
        ; S = "ce"
        ),
        N = 16
    ;
        S = "xxx",
        N = 21
    ;
        S = "xxy",
        N = 22
    ;
        S = "xxz",
        N = 23
    ;
        S = "xyx",
        ( N = 24
        ; N = 25
        ; N = 26
        )
    ;
        S = "axxxxxxΓ",
        N = 25
    ;
        S = "xxxxxxΔ",
        N = 26
    ;
        S = "aΓ",   % greek capital gamma, UC+0393
        N = 27
    ;
        S = "Δ",    % greek capital delta, UC+0394
        N = 28
    ;
        ( S = "aΘ"  % greek capital theta, UC+0398
        ; S = "Σ"   % greek capital sigma, UC+03A3
        ),
        N = 29
    ),
    (
        S0 = "a",
        M = 1
    ;
        S0 = "b",
        M = 2
    ;
        ( S0 = "aa"
        ; S0 = "ab"
        ),
        ( M = 11
        ; M = 12
        )
    ;
        ( S0 = "ba"
        ; S0 = "bb"
        ),
        ( M = 13
        ; M = 14
        ; M = 15
        )
    ;
        ( S0 = "ca"
        ; S0 = "cb"
        ; S0 = "cd"
        ; S0 = "ce"
        ),
        M = 16
    ;
        S0 = "xxx",
        M = 21
    ;
        S0 = "xxy",
        M = 22
    ;
        S0 = "xxz",
        M = 23
    ;
        S0 = "xyx",
        ( M = 24
        ; M = 25
        ; M = 26
        )
    ;
        S0 = "axxxxxxΓ",
        M = 25
    ;
        S0 = "xxxxxxΔ",
        M = 26
    ;
        S0 = "Γ",    % greek capital gamma, UC+0393
        M = 27
    ;
        S0 = "Δ",    % greek capital delta, UC+0394
        M = 28
    ;
        ( S0 = "xΘ"  % greek capital theta, UC+0398
        ; S0 = "Σ"   % greek capital sigma, UC+03A3
        ),
        M = 29
    ),
    R = 1000 * N + M.
