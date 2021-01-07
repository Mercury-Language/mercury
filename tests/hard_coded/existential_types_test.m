%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This module tests the use of existential types,
% including type inference,
% but not including type class constraints.

:- module existential_types_test.
:- interface.

:- import_module io.
:- import_module univ.

    % my_univ_value(Univ):
    %   returns the value of the object stored in Univ.
:- some [T] func my_univ_value(univ) = T.

:- some [T] func call_my_univ_value(univ) = T.

:- some [T] func my_exist_t = T.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    foo(univ(42), !IO),
    foo(univ("blah"), !IO),
    foo(univ(my_exist_t), !IO),
    foo(univ(call_my_exist_t), !IO),
    io.write_line(my_exist_t, !IO),
    io.write_line(call_my_exist_t, !IO).

my_exist_t = 43.

call_my_exist_t = my_exist_t.

:- pred foo(univ::in, io::di, io::uo) is det.

foo(X, !IO) :-
    io.write_line(my_univ_value(X), !IO),
    io.write_line(call_my_univ_value(X), !IO).

call_my_univ_value(Univ) = my_univ_value(Univ).

my_univ_value(Univ) = univ_value(Univ).
