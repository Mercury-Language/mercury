%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of the mode syntax.
%
% The only syntax is:
%
%   :- mode foo == bar >> baz.
%
% You can also use `bar >> baz' inline after a mode qualifier.

:- module mode_syntax.
:- interface.
:- import_module list.

:- mode my_input_list_skel == list_skel >> list_skel.
:- mode my_output_list_skel == free >> list_skel.
:- mode my_list_skel_output == (list_skel >> ground).
:- mode another_mode == list_skel >> list_skel.

:- pred p is semidet.

:- pred p2(list(T)::my_output_list_skel) is nondet.

:- implementation.

:- pred q(list(T)::list_skel >> list_skel /* my_input_list_skel */).
:- pred q2(list(T)::my_input_list_skel).
:- pred r(list(T)::my_output_list_skel).

q(_X) :-
    q([]).
q2(_X) :-
    q2([]).
r(X) :-
    r(X).

p :-
    r(X),
    q(X).

p2(X) :-
    (
        r(X),
        q(X)
    ;
        r(X),
        q(X)
    ).
