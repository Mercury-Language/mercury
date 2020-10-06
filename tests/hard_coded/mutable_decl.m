%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% mutable_decl.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Wed Aug 31 15:19:58 EST 2005
%
% Test the `:- mutable(...)' declaration.
%
%---------------------------------------------------------------------------%

:- module mutable_decl.

:- interface.

:- import_module io.

:- impure pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

:- mutable(x, int, 0, ground, []).
:- mutable(y, int, 0, ground, [untrailed]).
:- mutable(z, int, 0, ground, [untrailed, thread_local]).

main(!IO) :-
    semipure get_x(X0), impure set_x(X0 + 1),
    semipure get_x(X1), impure set_x(X1 + 1),
    semipure get_x(X2), impure set_x(X2 + 1),
    io.write_list([X0, X1, X2], ", ", io.write_int, !IO),
    io.nl(!IO),
    impure set_x(0),
    ( if impure my_member(1, [2, 3, 4, 5, 6]) then
        io.print("what the?!\n", !IO)
    else
        true
    ),
    semipure get_x(X), io.write_int(X, !IO), io.nl(!IO),
    semipure get_y(Y), io.write_int(Y, !IO), io.nl(!IO),
    semipure get_z(Z), io.write_int(Z, !IO), io.nl(!IO).

:- impure pred my_member(int::in, list(int)::in) is nondet.

my_member(A, [B | Bs]) :-
    semipure get_x(X), impure set_x(X + 1),
    semipure get_y(Y), impure set_y(Y + 1),
    semipure get_z(Z), impure set_z(Z + 1),
    (
        A = B
    ;
        impure my_member(A, Bs)
    ).

%---------------------------------------------------------------------------%
