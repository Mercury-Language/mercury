%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test.
% The Java backend was generating code that didn't initialise a type_ctor_info
% before another RTTI structure referenced it. This resulted in a null pointer
% exception when this program is executed.

:- module java_rtti_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

main(!IO) :-
    X = foo(bar(baz(1))),
    io.write_line(X, !IO).

:- type foo
    --->    foo(bar(baz(int))).

:- type bar(T)
    --->    bar(T).

:- type baz(T)
    --->    baz(T).
