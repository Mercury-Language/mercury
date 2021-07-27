%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    io.write_int(type_num(42), !IO).

:- typeclass numbered_type(T) where [
    func type_num(T) = int
].

:- instance foo(int) where [
    type_num is foo_type_num
].

foo_type_num _ = 42.
