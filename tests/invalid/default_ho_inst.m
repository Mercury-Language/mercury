%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module default_ho_inst.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo
    --->    foo(func(string) = string).

:- pred callfoo(foo::in, string::in, string::out) is det.

callfoo(foo(F), X, F(X)).

:- func semifoo(string::in) = (string::out) is semidet.

semifoo(X) = X :- semidet_true.

main(!IO) :-
    callfoo(foo(semifoo), "X", X),
    io.write_string(X, !IO),
    io.nl(!IO).
