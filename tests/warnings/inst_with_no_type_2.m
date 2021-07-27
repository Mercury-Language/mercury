%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module inst_with_no_type_2.

:- interface.

:- import_module io.

:- type t.

:- pred p(t::in, io::di, io::uo) is det.

:- implementation.

:- type t
    --->    ft.

:- pragma inline(p/3).
p(T, !IO) :-
    io.write(T, !IO).
