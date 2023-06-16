%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Ensure that the foreign_decl is placed into the .opt file for procedures
% which are defined by both a foreign proc and a mercury clause.
%

:- module intermod_pragma_clause_helper_1.

:- interface.

:- pred f(int::out) is det.
:- pred g(int::out) is det.

:- implementation.

:- pragma foreign_decl("C", "
    #define ML_NUMBER   5
").

f(5).
:- pragma foreign_proc("C",
    f(X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = ML_NUMBER;
").

g(5).
:- pragma foreign_proc("C",
    g(X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = ML_NUMBER;
").

