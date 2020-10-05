%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module intermod_type_qual2.

:- interface.

:- import_module io.
:- import_module list.

:- pred p(list(T)::in, io::di, io::uo) is det.

:- implementation.

:- pragma inline(p/3).
p(L, !IO) :-
    io.write_line(list.length(L) `with_type` int, !IO).
