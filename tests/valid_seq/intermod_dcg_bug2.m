%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_dcg_bug2.

:- interface.

:- import_module io.

:- pred foo(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.

:- pragma inline(foo/2).

foo -->
    { list.foldl(
        (pred(_::in, di, uo) is det -->
            =(Var0),
            :=(Var0 + 1)
        ), [1, 2, 3], 0, Count) },
    io.write_int(Count),

    list.foldl(
        (pred(X::in, di, uo) is det -->
            io.write(X)
        ), [1, 2, 3]).
