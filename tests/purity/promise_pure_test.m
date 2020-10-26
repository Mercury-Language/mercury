%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Various checks that promise_pure goals are treated properly.

:- module promise_pure_test.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    test1(5, List1),
    io__write(List1, !IO),
    io__nl(!IO),
    test2(21, List2),
    io__write(List2, !IO),
    io__nl(!IO).

:- impure pred set_x(int::in) is det.
:- pragma no_inline(set_x/1).
:- pragma foreign_proc("C", set_x(X::in), [will_not_call_mercury], "x=X;" ).
:- pragma foreign_proc("C#", set_x(X::in), [will_not_call_mercury], "x=X;" ).
:- pragma foreign_proc("Java", set_x(X::in), [will_not_call_mercury], "x=X;" ).

:- semipure pred get_x(int::out) is det.
:- pragma no_inline(get_x/1).
:- pragma promise_semipure(get_x/1).
:- pragma foreign_proc("C", get_x(X::out), [will_not_call_mercury], "X=x;").
:- pragma foreign_proc("C#", get_x(X::out), [will_not_call_mercury], "X=x;").
:- pragma foreign_proc("Java", get_x(X::out), [will_not_call_mercury], "X=x;").

:- impure pred incr_x is det.
:- pragma no_inline(incr_x/0).
:- pragma foreign_proc("C", incr_x, [will_not_call_mercury], "++x;" ).
:- pragma foreign_proc("C#", incr_x, [will_not_call_mercury], "++x;" ).
:- pragma foreign_proc("Java", incr_x, [will_not_call_mercury], "++x;" ).

:- pragma foreign_decl("C", "extern int x;").
:- pragma foreign_code("C", "int x = 0;").
:- pragma foreign_code("C#", "static int x = 0;").
:- pragma foreign_code("Java", "static int x = 0;").

:- pred test1(int::in, list(int)::out) is det.

test1(A, [X, Y, Z]) :-
    % Tempt compiler to optimize away duplicate semipure goals.
    % The promise_pure is actually a lie, since the value of Y depends
    % on the initial value of the global. This is why we call test1 only once.
    X = A * 2,
    promise_pure (
        semipure get_x(Y),
        impure set_x(X),
        semipure get_x(Z)
    ).

:- pred test2(int::in, list(int)::out) is det.

test2(A, [X, Y, Z]) :-
    % Tempt compiler to optimize away duplicate impure goals,
    % or to compile away det goals with no outputs.
    X = A * 3,
    promise_pure (
        impure set_x(X),
        impure incr_x,
        impure incr_x,
        semipure get_x(Y)
    ),
    Z = Y + 10.
