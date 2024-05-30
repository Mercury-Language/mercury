%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module mostly_uniq2.
:- interface.
:- import_module io.

:- pred foo(io::di, io::uo) is multi.

:- implementation.

% This should be a unique mode error, since the I/O state is only
% mostly_unique, since we didn't declare foo as cc_multi.

foo(!IO) :-
    io.write_int(1, !IO).
foo(!IO) :-
    io.write_int(2, !IO).
