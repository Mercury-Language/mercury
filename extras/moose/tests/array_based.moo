% This is a regression test.  It tests that moose supports parsers that do not
% use lists.

:- module array_based.
:- interface.

:- import_module char, int, array.

:- type token
    --->    ('+')
    ;   num(int)
    ;   ('(')
    ;   (')')
    ;   eof
    .

:- parse(exprn/1, token, eof, xx, in, out).

:- pred scan(array(char)::in, array(token)::out) is det.

:- implementation.

:- import_module require.
:- import_module string.

:- rule exprn(int).
exprn(Num)  --->  exprn(A), [+], term(B), { Num = A + B }.
exprn(Term) --->  term(Term).

:- rule term(int).
term(Num) ---> factor(Num).

:- rule factor(int).
factor(Num) ---> ['('], exprn(Num), [')'].
factor(Num) ---> [num(Num)].

scan(Chars, Toks) :-
    scan(Chars, array.make_empty_array, Toks0),
    Toks = array_reverse(Toks0).

:- pred scan(array(char)::in, array(token)::in, array(token)::out) is det.

scan(Cs0, Toks0, Toks) :-
    ( if array.is_empty(Cs0) then
        Toks = array_cons(eof, Toks0)
    else
        C = Cs0 ^ elem(0),
        Cs = array_tail(Cs0),
        ( if
            char.is_whitespace(C)
        then
            scan(Cs, Toks0, Toks)
        else if
            char.decimal_digit_to_int(C, Num)
        then
            scan(Cs, array_cons(num(Num), Toks0), Toks)
        else if
            C = ('+')
        then
            scan(Cs, array_cons('+', Toks0), Toks)
        else if
            C = ('(')
        then
            scan(Cs, array_cons('(', Toks0), Toks)
        else if
            C = (')')
        then
            scan(Cs, array_cons(')', Toks0), Toks)
        else
            error("expr: syntax error in input")
        )
    ).

:- func array_cons(T, array(T)) = array(T).
:- pragma external_func(array_cons/2).
:- func array_reverse(array(T)) = array(T).
:- pragma external_func(array_reverse/1).
:- func array_tail(array(T)) = array(T).
:- pragma external_func(array_tail/1).
