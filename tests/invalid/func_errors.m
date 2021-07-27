%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module func_errors.

:- interface.

% It is an error to only declare some of the modes.

:- func bar(int::in, int) = int is semidet.
:- func baz(int::in, int::in) = int is semidet.
:- func quux(int, int) = (int::out) is semidet.

:- func ok(int::in, int::in) = (int::out) is semidet.

:- pred p(int, int) is semidet.
:- mode p(in, in) is semidet.
:- pred q(int::in, int, int) is semidet.

:- implementation.
:- import_module int.

% foo(X, Y) = X + Y :- X > 0.
% bar(X, Y) = X + Y :- X > 0.
% baz(X, Y) = X + Y :- X > 0.
% quux(X, Y) = X + Y :- X > 0.

p(X, Y) :-
    X > Y.
% q(X, Y, _) :-
%   X > Y.

ok(X, Y) = X + Y :-
    X > 0.
