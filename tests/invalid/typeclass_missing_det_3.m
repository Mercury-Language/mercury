%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the case of a type class mode declaration without any determinism,
% and with an instance declaration in the same module.

:- module typeclass_missing_det_3.

:- interface.

:- implementation.

:- import_module io.

:- typeclass writeable(T) where [
    pred write(T::in, io::di, io::uo)
].

:- instance writeable(int) where [
    (write(V) -->
        write_string("a"),
        write_int(V))
].
