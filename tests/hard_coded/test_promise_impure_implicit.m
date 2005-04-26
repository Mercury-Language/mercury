%-----------------------------------------------------------------------------%
% test_promise_impure_implicit.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Tue Apr 26 13:14:12 EST 2005
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
%-----------------------------------------------------------------------------%

:- module test_promise_impure_implicit.

:- interface.

:- import_module io.



:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

main(!IO) :-
    promise_pure_implicit (
        X = some_impure_string,
        io.print(X, !IO)
    ).

:- impure func some_impure_string = string.

some_impure_string = X :-
    promise_impure (
        X = "Hello, World!\n"
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
