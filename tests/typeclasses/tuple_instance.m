%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module tuple_instance.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
    Size = size({"ok", "failed"}),
    io.write_int(Size, !IO),
    io.nl(!IO).

:- typeclass size(T) where [
    func size(T) = int
].

:- instance size({T, U}) <= (size(T), size(U)) where [
    func(size/1) is tuple_size
].

:- instance size(string) where [
    func(size/1) is string__length
].

:- func tuple_size({T, U}) = int <= (size(T), size(U)).

tuple_size({T, U}) = 1 + size(T) + size(U).
