:- module string.
	% Beware that char_to_string/2 won't work with NU-Prolog 1.5.33 because
	% of a NU-Prolog bug (fixed in 1.5.35).

:- interface.

:- pred string__append(string, string, string).

string__append(A, B, C) :-
	append(A, B, C).


:- pred char_to_string(char, string).
%	char_to_string(Char, String).
%		Converts a character (single-character atom) to a string
%		(or vice versa).

:- implementation.

:- type string == list(integer).

:- char_to_string(Char,String) when Char or String.
char_to_string(Char,String) :-
	length(String,1),
	name(Char,String).	

:- end_module string.
