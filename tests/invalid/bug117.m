%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Regression test for bug #117.
%---------------------------------------------------------------------------%

:- module bug117.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module list.

main(!IO):-
    extract(hold([])) = List,
    List = [Head | _],
    io.write_int(Head, !IO),
    io.nl(!IO).

:- type hold(T)
    --->    hold(T).

:- func extract(hold(list(int))) = list(int).
:- mode extract(in) = out(non_empty_list) is det.

extract(hold(X)) = X.
