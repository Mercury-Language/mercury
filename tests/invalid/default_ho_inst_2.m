:- module default_ho_inst_2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- type foo
    --->    foo(func(string) = string).

:- pred makefoo(foo::out) is det.

makefoo(foo(detfoo)).

:- pred callfoo(foo::in, string::in, string::out) is det.

callfoo(foo(F), X, F(X)).

:- func detfoo(string) = string.

detfoo(X) = X.

:- func semifoo(string::in) = (string::out) is semidet.

semifoo(X) = X :- semidet_true.

main(!IO) :-
    (
        F = foo(semifoo)
    ;
        makefoo(F)
    ),
    callfoo(F, "X", X),
    io.write_string(X, !IO),
    io.nl(!IO).

% vim: ft=mercury ts=4 sts=4 sw=4 et
