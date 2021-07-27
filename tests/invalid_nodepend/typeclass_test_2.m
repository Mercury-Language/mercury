%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_test_2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    % Type error here, due to the syntax error below.
    io.write_int(type_num(42), !IO),
    io.nl(!IO).

:- typeclass numbered_type(T) where [
    func type_num(T) = int
].

:- instance numbered_type(int) where [
    type_num/0 is foo_type_num  % syntax error here
].

foo_type_num(_) = 42.
