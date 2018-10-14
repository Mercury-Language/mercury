%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Construction of a partially instantiated value involving a direct argument
% functor did not produce any code, causing incorrectly behaviour at run-time
% if the value was deconstructed.
%

:- module direct_arg_partial_inst2.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- type struct
    --->    struct(int, int, int, int, int).

:- type fruit
    --->    apple(struct)   % direct arg functor
    ;       orange(struct). % direct arg functor

:- inst fruit
    --->    apple(free)
    ;       orange(free).

:- pred mk_apple(fruit::out(fruit)) is det.
:- pragma no_inline(mk_apple/1).

mk_apple(apple(_)).

:- pred mk_orange(fruit::out(fruit)) is det.
:- pragma no_inline(mk_orange/1).

mk_orange(orange(_)).

:- pred fill(struct::in, fruit::in(fruit), fruit::out) is det.
:- pragma no_inline(fill/3).

fill(Struct, apple(_), apple(Struct)).
fill(Struct, orange(_), orange(Struct)).

main(!IO) :-
    mk_apple(X0),
    fill(struct(1, 2, 3, 4, 5), X0, X),
    io.write(X, !IO),
    io.nl(!IO),

    mk_orange(Y0),
    fill(struct(1, 2, 3, 4, 5), Y0, Y),
    io.write(Y, !IO),
    io.nl(!IO).
