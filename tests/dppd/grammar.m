%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The "grammar" Benchmark.
% Part of the DPPD Library.
% 
% This is the grammar example from the Lam & Kusalik benchmarks.
% It is written in DCG form (when transformed into ordinary clauses the
% built-in =/2 appears.

:- module grammar.

:- interface.

:- pred grammar is semidet.

:- implementation.

:- import_module grammar_impl.
:- import_module list.

grammar :-
    expression_query(['(', '(', '(', '(', n, ')', ')', ')', ')']).

% The benchmark program (in ordinary clause format)
% 
% expression(X1, X2, X3, X4) :-
%     value(X1, X3, X5), qualification(X2, X5, X4).
% expression(X1, [], X2, X3) :- value(X1, X2, X3).
% 
% value(X1, X2, X3) :- identifier(X1, X2, X3).
% value(X1, X2, X3) :- numeric(X1, X2, X3).
% value(X1, X2, X3) :- built_in(X1, X2, X3).
% value(X1, X2, X3) :- bracketted(X1, X2, X3).
% value(subscripted(X1, X2), X3, X4) :-
%     identifier(X1, X3, X5),
%     start_subscript(X5, X6),
%     value(X2, X6, X7),
%     end_subscript(X7, X4).
% value(X1, X2, X3) :-
%     leftparen(X2, X4),
%     value(X1, X4, X5),
%     rightparen(X5, X3).
% 
% qualification(merge_qualifiers(X1, X2), X3, X4) :-
%     qualifier(X1, X3, X5), qualification(X2, X5, X4).
% qualification(X1, X2, X3) :-
%     qualifier(X1, X2, X3).
% qualifier(such_that(X1, X2), X3, X4) :-
%     such_that(X3, X5), value(X1, X5, X6), equals(X6, X7), value(X2, X7, X4).
% qualifier(for_all(X1), X2, X3) :-
%     for_all(X2, X4), subrange(X1, X4, X3).
% qualifier(when(X1), X2, X3) :-
%     when(X2, X4), value(X1, X4, X3).
% qualifier(where(X1), X2, X3) :-
%     where(X2, X4), is_a(X1, X4, X3).
% 
% leftparen(X1, X2) :- '='(X1, ['(' | X2]).
% rightparen(X1, X2) :- '='(X1, [')' | X2]).
% leftbracket(X1, X2) :- '='(X1, ['[' | X2]).
% rightbracket(X1, X2) :- '='(X1, [']' | X2]).
% colon(X1, X2) :- '='(X1, [':' | X2]).
% semicolon(X1, X2) :- '='(X1, [';' | X2]).
% dotdot(X1, X2) :- '='(X1, ['..' | X2]).
% comma(X1, X2) :- '='(X1, [', ' | X2]).
% equals(X1, X2) :- '='(X1, ['=' | X2]).
% where(X1, X2) :- '='(X1, [where | X2]).
% when(X1, X2) :- '='(X1, [when | X2]).
% is_a(X1, X2) :- '='(X1, [is | X2]).
% such_that(X1, X2) :- '='(X1, [suchthat | X2]).
% such_that(X1, X2) :- '='(X1, [such | X3]), '='(X3, [that | X2]).
% for_all(X1, X2) :- '='(X1, [forall | X2]).
% for_all(X1, X2) :- '='(X1, [for | X3]), '='(X3, [all | X2]).
% start_subscript(X1, X2) :- '='(X1, ['{' | X2]).
% end_subscript(X1, X2) :- '='(X1, ['}' | X2]).
% identifier(X1, X2, X3) :-  common_function(X1, X2, X3).
% identifier(X1, X2, X3) :-  common_variable(X1, X2, X3).
% common_variable(a, X1, X2) :- '='(X1, [a | X2]).
% common_variable(n, X1, X2) :- '='(X1, [n | X2]).
% common_variable(f, X1, X2) :- '='(X1, [f | X2]).
% common_variable(v, X1, X2) :- '='(X1, [v | X2]).
% common_variable(x, X1, X2) :- '='(X1, [x | X2]).
% common_variable(i, X1, X2) :- '='(X1, [i | X2]).
% common_variable(shp, X1, X2) :- '='(X1, [shp | X2]).
% common_variable(arg, X1, X2) :- '='(X1, [arg | X2]).
% common_variable(res, X1, X2) :- '='(X1, [res | X2]).
% numeric(X1, nomatch, X2).
% built_in(X1, nomatch, X2).
% bracketted(X1, nomatch, X2).
% common_function(X1, nomatch, X2).
% 
% The partial deduction query
% 
% :- expression(n, [], String, []).
% 
% The run-time queries
% 
% :- expression( n, [], ['(', '(', '(', '(', n, ')', ')', ')', ')'], [] ).
% 
% Example solution
% 
% The following specialised program can be obtained by the ECCE partial
% deduction system. It runs more than 7 times faster than the original.
% 
% expression__1(X1) :- value__3(X1, []).
% 
% value__3(nomatch, X1).
% value__3([n | X1], X1).
% value__3(['(' | X1], X2) :- value__3(X1, [')' | X2]).
% 
% Michael Leuschel / K.U. Leuven / michael@cs.kuleuven.ac.be
