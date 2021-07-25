%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Various checks that impurity declarations are treated properly.

:- module purity.
:- interface.
:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    impure test1(!IO),
    impure test2(!IO),
    impure test3(!IO),
    impure test4(!IO),
    impure test1_inline(!IO),
    impure test2_inline(!IO),
    impure test3_inline(!IO),
    impure test4_inline(!IO).

:- impure pred test1(io::di, io::uo) is det.
:- impure pred test2(io::di, io::uo) is det.
:- impure pred test3(io::di, io::uo) is det.
:- impure pred test4(io::di, io::uo) is det.

:- impure pred test1_inline(io::di, io::uo) is det.
:- impure pred test2_inline(io::di, io::uo) is det.
:- impure pred test3_inline(io::di, io::uo) is det.
:- impure pred test4_inline(io::di, io::uo) is det.

:- impure pred set_x(int::in) is det.
:- pragma no_inline(set_x/1).
:- pragma foreign_proc("C",
    set_x(X::in),
    [will_not_call_mercury],
"
    x = X;
").
:- pragma foreign_proc("C#",
    set_x(X::in),
    [will_not_call_mercury],
"
    x = X;
").
:- pragma foreign_proc("Java",
    set_x(X::in),
    [will_not_call_mercury],
"
    x = X;
").

:- impure pred incr_x is det.
:- pragma no_inline(incr_x/0).
:- pragma foreign_proc("C",
    incr_x,
    [will_not_call_mercury],
"
    ++x;
").
:- pragma foreign_proc("C#",
    incr_x,
    [will_not_call_mercury],
"
    ++x;
").
:- pragma foreign_proc("Java",
    incr_x,
    [will_not_call_mercury],
"
    ++x;
").

:- semipure pred get_x(int::out) is det.
:- pragma no_inline(get_x/1).
:- pragma foreign_proc("C",
    get_x(X::out),
    [will_not_call_mercury, promise_semipure],
"
    X = x;
").
:- pragma foreign_proc("C#",
    get_x(X::out),
    [will_not_call_mercury, promise_semipure],
"
    X = x;
").
:- pragma foreign_proc("Java",
    get_x(X::out),
    [will_not_call_mercury, promise_semipure],
"
    X = x;
").

:- impure pred set_x_inline(int::in) is det.
:- pragma inline(set_x_inline/1).
:- pragma foreign_proc("C",
    set_x_inline(X::in),
    [will_not_call_mercury],
"
    x = X;
").
:- pragma foreign_proc("C#",
    set_x_inline(X::in),
    [will_not_call_mercury],
"
    x = X;
").
:- pragma foreign_proc("Java",
    set_x_inline(X::in),
    [will_not_call_mercury],
"
    x = X;
").

:- impure pred incr_x_inline is det.
:- pragma inline(incr_x_inline/0).
:- pragma foreign_proc("C",
    incr_x_inline,
    [will_not_call_mercury],
"
    ++x;
").
:- pragma foreign_proc("C#",
    incr_x_inline,
    [will_not_call_mercury],
"
    ++x;
").
:- pragma foreign_proc("Java",
    incr_x_inline,
    [will_not_call_mercury],
"
    ++x;
").

:- semipure pred get_x_inline(int::out) is det.
:- pragma inline(get_x_inline/1).
:- pragma foreign_proc("C",
    get_x_inline(X::out),
    [will_not_call_mercury, promise_semipure],
"
    X=x;
").
:- pragma foreign_proc("C#",
    get_x_inline(X::out),
    [will_not_call_mercury, promise_semipure],
"
    X=x;
").
:- pragma foreign_proc("Java",
    get_x_inline(X::out),
    [will_not_call_mercury, promise_semipure],
"
    X=x;
").

:- pragma foreign_decl("C", "extern int x;").
:- pragma foreign_code("C", "int x = 0;").
:- pragma foreign_code("C#", "static int x = 0;").
:- pragma foreign_code("Java", "static int x = 0;").

% tempt compiler to optimize away duplicate semipure goals.
test1(!IO) :-
    semipure get_x(X),
    io.format("%d\n", [i(X)], !IO),
    impure set_x(X + 1),
    semipure get_x(Y),
    io.format("%d\n", [i(Y)], !IO).

% tempt compiler to optimize away duplicate impure goals, or to compile away
% det goals with no outputs.
test2(!IO) :-
    impure incr_x,
    impure incr_x,
    semipure get_x(Y),
    io.format("%d\n", [i(Y)], !IO).

% tempt compiler to optimize away impure goal in branch that cannot succeed.
test3(!IO) :-
    (
        impure incr_x,
        fail
    ;
        semipure get_x(Y)
    ),
    io.format("%d\n", [i(Y)], !IO).

/***
% This test used to be written as follows, but currently
% the unique mode analysis is not smart enough to realize
% that the disjuncts which update the I/O state won't
% backtrack over I/O if the code is written like that.

% tempt compiler to optimize away impure goal in branch that cannot succeed.
test3(!IO) :-
    (
        impure incr_x,
        fail
    ;
        semipure get_x(Y),
        io.format("%d\n", [i(Y)], !IO)
    ).
***/

% regression test for problem with calls to implied modes of impure/semipure
% preds reporting spurious warnings about impurity markers in the wrong place.
test4(!IO) :-
    semipure get_x(OldX),
    impure incr_x,
    ( if semipure get_x(OldX + 1) then
        io.write_string("test4 succeeds\n", !IO)
    else
        io.write_string("test4 fails\n", !IO)
    ),
    impure set_x(OldX).

%  Now do it all again with inlining requested

test1_inline(!IO) :-
    % Tempt compiler to optimize away duplicate semipure goals.
    semipure get_x_inline(X),
    io.format("%d\n", [i(X)], !IO),
    impure set_x_inline(X + 1),
    semipure get_x_inline(Y),
    io.format("%d\n", [i(Y)], !IO).

test2_inline(!IO) :-
    % Tempt compiler to optimize away duplicate impure goals,
    % or to compile away det goals with no outputs.
    impure incr_x_inline,
    impure incr_x_inline,
    semipure get_x_inline(Y),
    io.format("%d\n", [i(Y)], !IO).

test3_inline(!IO) :-
    % Tempt compiler to optimize away impure goal in branch that
    % cannot succeed.
    (
        impure incr_x_inline,
        fail
    ;
        semipure get_x_inline(Y)
    ),
    io.format("%d\n", [i(Y)], !IO).

/***
% This test used to be written as follows, but currently
% the unique mode analysis is not smart enough to realize
% that the disjuncts which update the I/O state won't
% backtrack over I/O if the code is written like that.

% tempt compiler to optimize away impure goal in branch that cannot succeed.
test3_inline(!IO) :-
    (
        impure incr_x_inline,
        fail
    ;
        semipure get_x_inline(Y),
        io.format("%d\n", [i(Y)], !IO)
    ).
***/

% regression test for problem with calls to implied modes of impure/semipure
% preds reporting spurious warnings about impurity markers in the wrong place.
test4_inline(!IO) :-
    semipure get_x_inline(OldX),
    impure incr_x_inline,
    ( if semipure get_x_inline(OldX + 1) then
        io.write_string("test4_inline succeeds\n", !IO)
    else
        io.write_string("test4_inline fails\n", !IO)
    ),
    impure set_x_inline(OldX).
