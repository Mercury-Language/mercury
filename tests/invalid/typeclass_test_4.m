%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module typeclass_test_4.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_int(type_num(43), !IO).

:- typeclass numbered_type(T) where [
    func type_num(T::in) = (int::out) is det
].

:- instance numbered_type(int) where [
    func(type_num/0) is foo_type_num
].

:- func foo_type_num(T::in) = (int::out) is det.

foo_type_num(_) = 42.
