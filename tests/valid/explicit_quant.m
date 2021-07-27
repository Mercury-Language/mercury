%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module explicit_quant.

:- interface.

:- import_module char.
:- import_module list.

:- pred only_space_chars(list(char) :: in) is semidet.

:- implementation.

only_space_chars(Cs) :-
    all [C] (
        list.member(C, Cs)
    =>
        C = ' '
    ).
