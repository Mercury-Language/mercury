:- module explict_quant.

:- interface.
:- import_module list, char.

:- pred only_space_chars(list(char) :: in) is semidet.

:- implementation.

only_space_chars(Cs) :-
	all [C] list__member(C, Cs) => C = ' '.
