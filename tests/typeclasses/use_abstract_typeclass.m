%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module use_abstract_typeclass.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module abstract_typeclass.
:- import_module list.

main -->
    p(43),
    p("Forty-three"),
    p([43]),
    p([[[], [43]]]),
    { q(X) },
    p(X).
