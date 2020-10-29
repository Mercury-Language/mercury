%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
%
% Test for error messages produced by syntax errors in 'foreign_proc' pragmas.
%

:- module bad_foreign_proc.
:- interface.

:- pred foo(int::in, int::out) is det.

:- implementation.

    % Too few arguments.
    %
:- pragma foreign_proc("C").

    % Too many arguments.
    %
:- pragma foreign_proc("C",
    foo(A::in, B::ouit),
    [promise_pure],
"
    B = A;
", "Extra argument").

    % Invalid foreign language.
    %
:- pragma foreign_proc("InvalidLanuage",
    foo(A::in, B::out),
    [promise_pure],
"
    B = A;
").

    % Pred argument is not an atom.
    %
:- pragma foreign_proc("C", "foo", [promise_pure], "B  = A;").

    % Invalid attributes argument
    %
:- pragma foreign_proc("C", foo(A::in, B::out), "promise_pure", "B = A;").

    % XXX the compiler apparently accepts this.
    %
:- pragma foreign_proc("C", foo(A::in, B::out), promise_pure, "B = A;").

    % Conflicting foreing_proc attributes.
    %
:- pragma foreign_proc("C",
     foo(A::in, A::out),
     [promise_pure, promise_semipure, thread_safe, not_thread_safe],
"
    B = A;
").

    % ForeignCode is not a string.
    %
:- pragma foreign_proc("C", foo(A::in, B::out), [promise_pure], 5555).

    % Check contexts on arguments.
    %
:- pragma foreign_proc(
    "InvalidLanguage",
    "foo",
    "promise_pure",
    6666
).

foo(X, X).

:- pragma foreign_proc("Erlang", foo(A::in, B::out), [promise_pure], "B = A").
