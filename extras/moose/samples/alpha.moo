:- module alpha.
:- interface.

:- import_module char.
:- import_module int.
:- import_module list.

:- type token
        --->    ('+')
        ;       num(int)
        ;       ('(')
        ;       (')')
        ;       eof
        .

:- parse(exprn/1, token, eof, xx, in, out).

:- pred scan(list(char)::in, list(token)::out) is det.

:- implementation.

:- import_module string.
:- import_module require.

:- rule exprn(int).
exprn(Num)      --->    exprn(A), [+], term(B), { Num = A + B }.
exprn(Term)     --->    term(Term).

:- rule term(int).
term(Num)       --->    factor(Num).

:- rule factor(int).
factor(Num)     --->    ['('], exprn(Num), [')'].
factor(Num)     --->    [num(Num)].

scan(Chars, Toks) :-
    scan(Chars, [], Toks0),
    list.reverse(Toks0, Toks).

:- pred scan(list(char), list(token), list(token)).
:- mode scan(in, in, out) is det.

scan([], Toks, [eof|Toks]).
scan([C | Cs], Toks0, Toks) :-
    ( if
        char.is_whitespace(C)
    then
        scan(Cs, Toks0, Toks)
    else if
        char.is_digit(C)
    then
        take_while(char.is_digit, [C | Cs], Digits, Rest),
        string.from_char_list(Digits, NumStr),
        Num = string.det_to_int(NumStr),
        scan(Rest, [num(Num) | Toks0], Toks)
    else if
        C = ('+')
    then
        scan(Cs, ['+' | Toks0], Toks)
    else if
        C = ('(')
    then
        scan(Cs, ['(' | Toks0], Toks)
    else if
        C = (')')
    then
        scan(Cs, [')' | Toks0], Toks)
    else
        error("expr: syntax error in input")
    ).
