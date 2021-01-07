%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module uniq_duplicate_call.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module array.

main(!IO) :-
    dup_call(1, 2, Array1, Array2),
    io.write_int(array.lookup(Array1, 0), !IO),
    io.nl(!IO),
    io.write_int(array.lookup(Array2, 0), !IO),
    io.nl(!IO).

:- pred dup_call(T::in, T::in, array(T)::out, array(T)::out) is det.

dup_call(T, T2,
    array.set(array.init(1, T), 0, T2),
    array.init(1, T)).
