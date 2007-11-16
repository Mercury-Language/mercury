%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Test the hidden argument tabling method options
%
% :- pragma memo(..., [specified(...), hidden_arg_value]).
% :- pragma memo(..., [specified(...), hidden_arg_addr]).

:- module specified_hidden_arg.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

%-----------------------------------------------------------------------------%

main(!IO) :-
    p(foo(123), F1),
    p(foo(123), F2),
    q(42, F3),
    io.write({F1, F2, F3}, !IO),
    io.nl(!IO).

:- typeclass tc(T) where [
    pred double(T::in, T::out) is det
].

:- type foo
    --->    foo(int).

:- instance tc(foo) where [
    double(foo(F), foo(F + F))
].

:- pred p(T::in, T::out) is det <= tc(T).

:- pragma memo(p/2,
    [specified([value, output], hidden_arg_addr)]).

p(F, G) :-
    double(F, G).

:- pred q(T::in, T::out) is det.

:- pragma memo(q/2,
    [specified([addr, output], hidden_arg_value)]).

q(X, X).
