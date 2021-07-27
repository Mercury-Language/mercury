%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module unbound_inst_var.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.

:- type all(X) ---> a(X).

:- inst all(X) ---> a(X).

:- pred test(all(char), all(char)).
:- mode test(in(all(I)), out(all(I))) is det.

:- pred try_test is det.

test(X, X).

% :- pred main(io, io).
% :- mode main(di, uo) is det.

main(IO, IO) :-
    true.

try_test :-
    ( C = a ; C = b ; C = c),
    test(a(C), a(D)),
    ( D = a ; D = b ; D = c).

:- end_module unbound_inst_var.
