:- module expr.

:- interface.

:- import_module char, int, list.

:- type token
	--->	('+')
	;	('-')
	;	('*')
	;	('/')
	;	num(int)
	;	('(')
	;	(')')
	;	('$')
	.

:- parse(exprn/1, ('$'), token, xx).

:- pred scan(list(char), list(token)).
:- mode scan(in, out) is semidet.

:- implementation.

:- import_module string.

:- rule exprn(int).
exprn(Num)	--->	exprn(A), [+], term(B), { Num = A + B }.
exprn(Num)	--->	exprn(A), [-], term(B), { Num = A - B }.
exprn(Num)	--->	term(Num).

:- rule term(int).
term(Num)	--->	term(A), [*], factor(B), { Num = A * B }.
term(Num)	--->	term(A), [/], factor(B), { Num = A // B }.
term(Num)	--->	factor(Num).

:- rule factor(int).
factor(Num)	--->	['('], exprn(Num), [')'].
factor(Num)	--->	[num(Num)].

scan(Chars, Toks) :-
	scan(Chars, [], Toks0),
	list__reverse(Toks0, Toks).

:- pred scan(list(char), list(token), list(token)).
:- mode scan(in, in, out) is semidet.

scan([], Toks, ['$'|Toks]).
scan([C|Cs], Toks0, Toks) :-
	( char__is_whitespace(C) ->
		scan(Cs, Toks0, Toks)
	; char__is_digit(C) ->
		takewhile(char__is_digit, [C|Cs], Digits, Rest),
		string__from_char_list(Digits, NumStr),
		string__to_int(NumStr, Num),
		scan(Rest, [num(Num)|Toks0], Toks)
	; C = ('+') ->
		scan(Cs, ['+'|Toks0], Toks)
	; C = ('-') ->
		scan(Cs, ['-'|Toks0], Toks)
	; C = ('*') ->
		scan(Cs, ['*'|Toks0], Toks)
	; C = ('/') ->
		scan(Cs, ['/'|Toks0], Toks)
	; C = ('(') ->
		scan(Cs, ['('|Toks0], Toks)
	; C = (')') ->
		scan(Cs, [')'|Toks0], Toks)
	;
		fail
	).

