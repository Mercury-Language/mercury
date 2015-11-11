% vim: ts=4 sw=4 et ft=mercury
%
% This test case ensures that lco.m handles from ground term scopes properly
% such as the one that is created for the construction of the terms in the
% base case of list_data_term.

:- module bug300.

:- interface.

:- import_module list.

:- type data_term == mer_term(literal).

:- type mer_term(T)
    --->    functor(qualified_name, list(mer_term(T))).

:- type qualified_name == list(unqualified_name).

:- type unqualified_name == string.

:- type literal
    --->    string(string)
    ;       int(int).

:- func list_data_term(list(data_term)) = data_term.

:- implementation.

list_data_term([]) = functor(["list", "[]"], []).
list_data_term([H | T]) = functor(["list", "[ | ]"], [H, list_data_term(T)]).
