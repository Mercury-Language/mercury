%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mostly_uniq1.
:- interface.
:- import_module io.

:- pred my_main(io::di, io::uo) is multi.

:- implementation.

:- pred p(int::out) is multi.

p(1).
p(2).

% This should be a unique mode error, since the io-state is only mostly_unique,
% because p/1 has multiple solutions and we didn't declare main as cc_multi.

my_main(!IO) :-
    p(X),
    io.write_int(X, !IO).
