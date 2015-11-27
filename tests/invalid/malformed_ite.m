%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check that we generate readable error messages for if-then-elses
% that *attempt* to use "if C then T else E" syntax, but fail, because
% they use the corresponding element of the "C -> T ; E" syntax.
%
% In the case of "if", the corresponding element is nothing.
% In the case of "then", the corresponding element is "->".
% In the case of "else", the corresponding element is ";".
%
% We test that the parser can generate a meaningful error message
% in cases where one or two of the three keywords is wrong.

:- module malformed_ite.
:- interface.

:- pred p(int::in, int::out) is det.

:- implementation.

:- import_module int.

p(A, B) :-
    % Missing "if".
    ( A < 10 then
        B = A
    else
        B = A + 1
    ).

p(A, B) :-
    % Wrong "then".
    ( if A < 10 ->
        B = A
    else
        B = A + 1
    ).

p(A, B) :-
    % Wrong "else".
    ( if A < 10 then
        B = A
    ;
        B = A + 1
    ).

p(A, B) :-
    % Missing "if" and wrong "then".
    ( A < 10 ->
        B = A
    else
        B = A + 1
    ).

p(A, B) :-
    % Missing "if" and wrong "else".
    ( A < 10 then
        B = A
    ;
        B = A + 1
    ).

p(A, B) :-
    % Wrong "then" and "else".
    ( if A < 10 ->
        B = A
    ;
        B = A + 1
    ).
