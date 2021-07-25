%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Procedures which just forward to the corresponding procedures
% in the library, to avoid differences in behaviour depending
% on whether the library was compiled with debugging enabled.

:- module library_forwarding.

:- interface.

:- func int + int = int.
:- func int * int = int.
:- func int - int = int.
:- func int // int = int.
:- func - int = int.

:- func int mod int = int.

:- pred int =< int.
:- mode in =< in is semidet.

:- pred int > int.
:- mode in > in is semidet.

:- pred semidet_succeed is semidet.
:- pred semidet_fail is semidet.

:- implementation.

:- import_module int.

X + Y = 'int__+'(X, Y).
X * Y = 'int__*'(X, Y).
X - Y = 'int__-'(X, Y).
X // Y = 'int__//'(X, Y).
- X = 'int__-'(X).

X mod Y = 'int__mod'(X, Y).

X =< Y :-
    'int__=<'(X, Y).

X > Y :-
    'int__>'(X, Y).

semidet_succeed :-
    builtin.semidet_succeed.

semidet_fail :-
    builtin.semidet_fail.
