%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test the warnings for suspicious recursion.
%
%---------------------------------------------------------------------------%

:- module suspicious_recursion.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    funny_append([1, 2, 3], [4, 5, 6], Ns),
    sum(Ns, 0, Sum),
    io.write_line(Sum, !IO).

:- pred funny_append(list(T)::in, list(T)::in, list(T)::out) is det.

funny_append(As, Bs, Cs) :-
    (
        As = [], Bs = Cs
    ;
        As = [X | As1],
        funny_append(Bs, As1, Cs1),
        Cs = [X | Cs1]
    ).

:- pred sum(list(int)::in, int::in, int::out) is det.

sum(!.Ns, !Sum) :-
    !.Ns = [].
sum(!.Ns, !Sum) :-
    !.Ns = [N | !:Ns],
    !:Sum = !.Sum + N,
    sum(!.Ns, !Sum).
