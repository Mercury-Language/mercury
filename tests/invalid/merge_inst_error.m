%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module merge_inst_error.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

:- type t
    --->    ta
    ;       tb
    ;       tc
    ;       td
    ;       te(int)
    ;       tf(int).

main(!IO) :-
    test1(ta, !IO),
    test2(tb, !IO),
    test3(tf(10), !IO).

%---------------------------------------------------------------------------%

:- pred test1(t::in, io::di, io::uo) is det.

test1(T, !IO) :-
    switch1(T, S),
    io.write(T, !IO),
    io.format(": %s\n", [s(S)], !IO).

:- pred switch1(t::in, string::out) is det.

switch1(T, S) :-
    (
        T = ta,
        S = "ta"
    ;
        T = tb
        % S = "tb"
    ;
        T = tc,
        S = "tc"
    ;
        T = td
        % S = "td"
    ;
        T = te(N),
        S = "te" ++ string.int_to_string(N)
    ;
        T = tf(N),
        S = "tf" ++ string.int_to_string(N)
    ).

%---------------------------------------------------------------------------%

:- pred test2(t::in, io::di, io::uo) is det.

test2(T, !IO) :-
    switch2(T, S),
    io.write(T, !IO),
    io.format(": %s\n", [s(S)], !IO).

:- pred switch2(t::in, string::out) is det.

switch2(T, S) :-
    (
        ( T = ta, S = "ta"
        ; T = tb
        )
        % S = "tab"
    ;
        T = tc,
        S = "tc"
    ;
        (
            T = td,
            S = "td"
        ;
            T = te(N),
            S = "te" ++ string.int_to_string(N)
        )
    ;
        T = tf(N),
        S = "tf" ++ string.int_to_string(N)
    ).

%---------------------------------------------------------------------------%

:- pred test3(t::in, io::di, io::uo) is det.

test3(T, !IO) :-
    switch3(T, S),
    io.write(T, !IO),
    io.format(": %s\n", [s(S)], !IO).

:- pred switch3(t::in, string::out) is det.

switch3(T, S) :-
    (
        ( T = ta
        ; T = tb
        ),
        S = "tab"
    ;
        T = tc,
        S = "tc"
    ;
        (
            T = td
            % S = "td"
        ;
            T = te(_N)
            % S = "te" ++ string.int_to_string(N)
        ;
            T = tf(_N)
            % S = "tf" ++ string.int_to_string(N)
        )
    ).

%---------------------------------------------------------------------------%
