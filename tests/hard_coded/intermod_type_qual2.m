%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
:- module intermod_type_qual2.

:- interface.

:- import_module io.
:- import_module list.

:- pred p(list(T)::in, io__state::di, io__state::uo) is det.

:- implementation.

:- pragma inline(p/3).
p(L) -->
    io__write(list__length(L) `with_type` int),
    io__nl.
