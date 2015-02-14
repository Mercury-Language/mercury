%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The Erlang backend treats [] and [ | ] data constructors specially.
% This test checks we can deconstruct existential types that use
% the special syntax.

:- module existential_list.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo
    --->    []
    ;       some [T] [T | foo].

main(!IO) :-
    T = 'new [|]'(42, 'new [|]'("bar", [])) : foo,
    io.write(T, !IO),
    io.nl(!IO).
