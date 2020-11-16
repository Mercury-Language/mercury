%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module extract_typeinfo.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    p(1, !IO),
    io.nl(!IO).

:- import_module list.

:- typeclass foo(T) where [
    pred print_it(T::in, io::di, io::uo) is det
].

:- instance foo(int) where [
    pred(print_it/3) is io.write_int
].

:- pred p(T::in, io::di, io::uo) is det <= foo(T).

p(X, !IO) :-
    ( if
        % At this call, the type-info gets extracted from the typeclass-info.
        list.append([X], [X], [X, X])
    then
        print_it(X, !IO)
    else
        true
    ).
