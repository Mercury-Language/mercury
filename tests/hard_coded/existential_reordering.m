%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the use of existential types,
% including type inference,
% but not including type class constraints.

:- module existential_reordering.
:- interface.

:- some [T] func my_exist_t = T.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.
:- import_module univ.

main(!IO) :-
    % do something which requires knowing the type of L
    L = [],
    Univ = univ(L),
    io.write_line(Univ, !IO),

    % now do something which binds the type of L
    same_type(L, [my_exist_t]).

:- pred same_type(T::unused, T::unused) is det.
same_type(_, _).

my_exist_t = 42.
