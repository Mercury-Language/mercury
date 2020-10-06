%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module string_append_ooi.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module pair.
:- import_module solutions.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    unsorted_solutions(
        ( pred(L - R::out) is multi :-
            string.append(L, R, "cat")
        ), UnsortedSolutions),
    list.sort(UnsortedSolutions, Solutions),
    io.write(Solutions, !IO),
    io.nl(!IO).
