:- module nondet_lambda.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

main(!IO) :-
    % The compiler was incorrectly simplifying this to `F = bar' in the erlang
    % grade.
    F = (pred(X::out) is nondet :- bar(X)),
    ( F(Y) ->
	io__write(Y, !IO),
	io__write_string("\n", !IO)
    ;
	true
    ).

:- pred bar(int).
:- mode bar(out) is det.
bar(42).
