% vim: ts=4 sw=4 ft=mercury

% This tests the warnings you should get for unifications of the form
% X = f(..., X, ...).

:- module unify_x_f_x.
:- interface.

:- import_module bool.
:- import_module char.
:- import_module list.

:- pred get_percent(list(char)::in, list(char)::out, bool::out) is det.

:- implementation.

get_percent(!Chars, HavePercent) :-
    ( !.Chars = ['%' | !.Chars] ->
        HavePercent = yes
    ;
        HavePercent = no
    ).
