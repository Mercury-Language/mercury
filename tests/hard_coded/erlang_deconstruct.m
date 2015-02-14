%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Both alternatives of the type t are represented by tuples of size 3 on
% the erlang backend, make sure the compiler distinguishes between
% them correctly.

:- module erlang_deconstruct.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

:- type t
    --->    f(int, int)
    ;       some [T] f(T).

main(!IO) :-
    check_t(create(0), !IO),
    check_t(create(100), !IO).

:- pred check_t(t::in, io::di, io::uo) is det.

check_t(f(_, _), !IO) :-
    io.write_string("f/2\n", !IO).
check_t(f(_), !IO) :-
    io.write_string("f/1\n", !IO).

:- func create(int) = t.

create(N) =
    ( abs(N) > 10 ->
        f(1, 1)
    ;
        'new f'("string")
    ).
