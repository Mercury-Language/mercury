%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% foo is declared semidet but is actually erroneous. Unused argument
% elimination used to generate a call to the unused version in a model_det
% context instead of the correct model_semi context.

:- module unused_arg_determinism.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- type t.
:- func foo(t) = t.
:- mode foo(in) = out is semidet.

:- implementation.

:- import_module exception.

:- type t
    --->    t
    ;       u.

main(!IO) :-
    try_io(main_2, Result, !IO),
    (
        Result = succeeded(_),
        io.write_string("No exception.\n", !IO)
    ;
        Result = exception(_),
        io.write_string("Exception.\n", !IO)
    ).

:- pred main_2(int::out, io::di, io::uo) is det.

main_2(0, !IO) :-
    ( if X = foo(t) then
        io.write(X, !IO)
    else
        io.write_string("failure", !IO)
    ),
    io.nl(!IO).

foo(_) = _ :-
    private_builtin.sorry("foo").
