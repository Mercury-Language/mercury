% Test case for bug #133.

:- module try_detism.
:- interface.

:- import_module io.

:- pred p(int::out, io::di, io::uo) is cc_multi.

:- implementation.

p(Res, !IO) :-
    q(X, !IO),
    (try [io(!IO)] (
        q(X, !IO)
    )
    then
        Res = X
    catch E ->
        Res = E
    ).

:- pred q(int::out, io::di, io::uo) is det.

q(2, !IO).

% vim: set sts=4 sw=4 et:
