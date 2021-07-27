%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module polymorphic_unification.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main -->
    [].

:- import_module list.

:- pred p(T, T).
:- mode p(in, list_skel >> dead).

p(X, X).
