%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% The following test case was contributed by Peter Schachte.
% It was intended to show up a couple of confusing error messages.
% It also exposed a bug in the typechecker in version 0.6-alpha. -fjh.
%---------------------------------------------------------------------------%
% Ok, here it is. There are actually two errors here, two confusions
% between predicates and functions. Both messages are not too helpful.
%---------------------------------------------------------------------------%

:- module funcs_as_preds.

:- import_module(list).
:- import_module(bool).

% I would like to write:
null(X) = (X = []).
% but there is no (=)/2 function in Mercury.  This works:
%   null(X) = (X=[] -> yes ; no).

car([X | _]) = X.
car([]) = [].

cdr([_ | X]) = X.
cdr([]) = [].

cons(X, Y) = [X | Y].

% I would like to write:
ap(X, Y) = (if null(X) then Y else cons(car(X), ap(cdr(X), Y))).
% but one can't use a function in the condition of a functional
% if->then;else.  This works:
% ap(X, Y) = (null(X)=yes -> Y ; cons(car(X), ap(cdr(X), Y))).
