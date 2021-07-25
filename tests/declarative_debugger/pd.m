%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module pd.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO) :-
    io.write_line(rev([1, 2, 3]), !IO).

:- func rev(list(int)) = list(int).

rev(As) = rev_2(As, []).

:- func rev_2(list(int), list(int)) = list(int).

rev_2([], _) = [].  % oops
rev_2([A | As], Bs) = rev_2(As, [A | Bs]).
