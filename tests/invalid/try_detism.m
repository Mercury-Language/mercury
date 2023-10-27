%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The difference between the .err_exp{,2} files is caused by the different
% data representation schemes used by the C and Java backends on the one hand
% and the C# backend on the other hand. These affect the number of variables
% required to construct a typeinfo needed by the transformation that
% implements try goals.
%---------------------------------------------------------------------------%

:- module try_detism.
:- interface.

:- import_module io.

:- pred p(int::out, io::di, io::uo) is cc_multi.

:- implementation.

p(Res, !IO) :-
    q(X, !IO),
    ( try [io(!IO)] (
        q(X, !IO)
    )
    then
        Res = X
    catch E ->
        Res = E
    ).

:- pred q(int::out, io::di, io::uo) is det.

q(2, !IO).
