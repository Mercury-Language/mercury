%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module nondet_lambda.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    % The compiler was incorrectly simplifying this to `F = bar' in the erlang
    % grade.
    F = (pred(X::out) is nondet :- bar(X)),
    ( if F(Y) then
        io.write(Y, !IO),
        io.write_string("\n", !IO)
    else
        true
    ).

:- pred bar(int).
:- mode bar(out) is det.

bar(42).
