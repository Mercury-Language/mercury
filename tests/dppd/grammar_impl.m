%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grammar_impl.

:- interface.

:- type variable
    --->    a
    ;       n
    ;       f
    ;       v
    ;       x
    ;       i
    ;       shp
    ;       arg
    ;       res
    ;       forall
    ;       var_for
    ;       var_all
    ;       such
    ;       that
    ;       suchthat
    ;       var_is
    ;       subscripted(variable, variable)
    ;       var_where
    ;       var_when
    ;       '('
    ;       ')'
    ;       '['
    ;       ']'
    ;       '{'
    ;       '}'
    ;       (', ')
    ;       (':')
    ;       (';')
    ;       ('..')
    ;       ('=').

:- pred expression_query(list(variable)::in) is semidet.

:- import_module list.

:- implementation.

:- type qualifier
    --->    such_that(expression, expression)
    ;       for_all(term)
    ;       qual_when(term)
    ;       qual_where(term)
    ;       merge_qualifiers(qualifier, qualifier)
    ;       none.

:- type expression == variable.
:- type term == variable.

expression_query(String) :-
    expression(n, none, String, []).

:- pred expression(expression::in, qualifier::in,
    list(variable)::in, list(variable)::out) is nondet.

:- implementation.

expression(Term, Qualifiers) -->
    value(Term), qualification(Qualifiers).
expression(Term, none) -->
    value(Term).

:- pred value(variable::in, list(variable)::in, list(variable)::out) is nondet.

value(Term) -->
    identifier(Term).
value(Term) -->
    numeric(Term).
value(Term) -->
    built_in(Term).
value(Term) -->
    bracketed(Term).
value(subscripted(Term, Subscript)) -->
    identifier(Term), start_subscript, value(Subscript), end_subscript.
value(Term) -->
    leftparen, value(Term), rightparen.

:- pred qualification(qualifier::in, list(variable)::in,
    list(variable)::out) is nondet.

qualification(merge_qualifiers(Qualifier, Qualifiers)) -->
    qualifier(Qualifier),
    qualification(Qualifiers).
qualification(Qualifier) -->
    qualifier(Qualifier).

:- pred qualifier(qualifier::in, list(variable)::in, list(variable)::out)
    is nondet.

qualifier(such_that(Left, Right)) -->
    such_that, value(Left), equals, value(Right).
qualifier(for_all(Term)) -->
    for_all, subrange(Term).
qualifier(qual_when(Term)) -->
    (when), value(Term).
qualifier(qual_where(Term)) -->
    (where), is_a(Term).

:- pred leftparen(list(variable)::in, list(variable)::out) is semidet.
leftparen --> ['('].

:- pred rightparen(list(variable)::in, list(variable)::out) is semidet.
rightparen --> [')'].

:- pred leftbracket(list(variable)::in, list(variable)::out) is semidet.
leftbracket --> ['['].

:- pred rightbracket(list(variable)::in, list(variable)::out) is semidet.
rightbracket --> [']'].

:- pred colon(list(variable)::in, list(variable)::out) is semidet.
colon --> [':'].

:- pred semicolon(list(variable)::in, list(variable)::out) is semidet.
semicolon --> [';'].

:- pred dotdot(list(variable)::in, list(variable)::out) is semidet.
dotdot --> ['..'].

:- pred comma(list(variable)::in, list(variable)::out) is semidet.
comma --> [', '].

:- pred equals(list(variable)::in, list(variable)::out) is semidet.
equals --> ['='].

:- pred where(list(variable)::in, list(variable)::out) is semidet.
where --> [var_where].

:- pred when(list(variable)::in, list(variable)::out) is semidet.
when --> [var_when].

:- pred is_a(list(variable)::in, list(variable)::out) is semidet.
is_a --> [var_is].

:- pred such_that(list(variable)::in, list(variable)::out) is semidet.
such_that --> [suchthat].
such_that --> [such], [that].

:- pred for_all(list(variable)::in, list(variable)::out) is semidet.
for_all --> [forall].
for_all --> [var_for], [var_all].

:- pred start_subscript(list(variable)::in, list(variable)::out) is semidet.
start_subscript --> ['{'].

:- pred end_subscript(list(variable)::in, list(variable)::out) is semidet.
end_subscript --> ['}'].

:- pred identifier(variable::in, list(variable)::in, list(variable)::out)
    is semidet.

identifier(Identifier) --> common_function(Identifier).
identifier(Identifier) --> common_variable(Identifier).

:- pred common_variable(variable::in,
    list(variable)::in, list(variable)::out) is semidet.

common_variable(a) --> [a].
common_variable(n) --> [n].
common_variable(f) --> [f].
common_variable(v) --> [v].
common_variable(x) --> [x].
common_variable(i) --> [i].
common_variable(shp) --> [shp].
common_variable(arg) --> [arg].
common_variable(res) --> [res].

:- pred numeric(variable::in, list(variable)::in,
    list(variable)::out) is failure.
numeric(_) -->
    fail.

:- pred built_in(variable::in, list(variable)::in, list(variable)::out)
    is failure.
built_in(_) -->
    fail.

:- pred bracketed(variable::in, list(variable)::in, list(variable)::out)
    is failure.
bracketed(_) --> fail.

:- pred common_function(variable::in, list(variable)::in, list(variable)::out)
    is failure.
common_function(_) --> fail.

:- pred is_a(variable::in, list(variable)::in, list(variable)::out) is failure.
is_a(_) --> fail.

:- pred subrange(variable::in, list(variable)::in, list(variable)::out)
    is failure.
subrange(_) --> fail.

:- pred fail(T::in, T::out) is failure.
fail -->
    { fail }.
