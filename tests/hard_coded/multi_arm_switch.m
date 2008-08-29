%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% This is a test to check the proper functioning of the code that looks for
% multi-cons_id switch arms in which one of the cons_ids also has its own
% switch arm. Some old versions of the compiler had an overzealous sanity check
% that would cause a compiler abort on this code.

:- module multi_arm_switch.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.
:- import_module require.
:- import_module solutions.

:- type t
    --->    cmd_quit
    ;       cmd_menu
    ;       cmd_clique
    ;       cmd_dump_clique
    ;       cmd_root.

main(!IO) :-
    read_t(MaybeX, !IO),
    (
        MaybeX = yes(X),
        solutions(p(X), Solns),
        io.write(Solns, !IO),
        io.nl(!IO),
        main(!IO)
    ;
        MaybeX = no
    ).

:- pred p(t::in, int::out) is multi.

p(X, N) :-
    (
        X = cmd_quit,
        N = 1
    ;
        X = cmd_menu,
        N = 2
    ;
        X = cmd_clique,
        N = 3
    ;
        ( X = cmd_clique
        ; X = cmd_dump_clique
        ; X = cmd_root
        ),
        N = 4
    ).

:- pred read_t(maybe(t)::out, io::di, io::uo) is det.

read_t(MaybeX, !IO) :-
    io.read(Res, !IO),
    (
        Res = ok(X),
        MaybeX = yes(X)
    ;
        Res = error(_, _),
        error("I/O error")
    ;
        Res = eof,
        MaybeX = no
    ).
