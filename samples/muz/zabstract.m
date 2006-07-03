%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: zabstract.m
% main author: philip

:- module zabstract.

:- interface.
:- import_module maybe, pair, word, list, string, assoc_list, io.

:- type quantifier ---> universal ; exists ; unique.

:- type lconn ---> disjunction ; conjunction ; implication ; equivalence.

%%%
% A.1	SPECIFICATION

:- type spec == list(par).

%%%
% A.2	PARAGRAPH

:- type par == pair(par1, zcontext).

:- type par1
	--->	given(list(ident))
%	;	let(schema)
	;	sdef(ident, formals, sexpr)
	;	eqeq(ident, formals, expr)
	;	data(ref, ident, list(branch))
	;	zpred(zpred)
	;	define(formals, sexpr)
	.

:- type formals == list(ident).

:- type branch ---> branch(ref, ident, maybe(expr)).

%%%
% A.3	SCHEMA
% A.4	SCHEMA TEXT

% :- type schema ---> schema(list(decl), list(zpred)).

:- type sexpr ---> sexpr(ref, sexpr1, zcontext).

:- type sexpr1
	--->	ref(ident, maybe(list(expr)))
	;	text(list(decl), list(zpred))
	;	negation(sexpr)
	;	lbpred(lconn, sexpr, sexpr)
	;	projection(ref, sexpr, sexpr)
	;	hide(ref, sexpr, list(ident))
	;	quantification(quantifier, sexpr, sexpr)
	;	renaming(sexpr, renaming)
	;	bsexpr(ref, sconn, sexpr, sexpr)
	;	decoration(sexpr, decoration)
	;	pre(ref, sexpr)
	.

:- type sconn ---> composition ; piping.

%%%
% A.5	DECLARATION

:- type decl
	--->	decl(list(ident), expr)
	;	include(sexpr).

%%%
% A.6	PREDICATE

:- type zpred == pair(zpred1, zcontext).

:- type zpred1
	--->	equality(expr, expr)
	;	membership(expr, expr)
	;	truth
	;	falsehood
	;	negation(zpred)
	;	lbpred(lconn, zpred, zpred)
	;	quantification(quantifier, sexpr, zpred)
	;	sexpr(sexpr)
	;	let(assoc_list(ident, expr), zpred)
	.

%%%
% A.7	EXPRESSION

:- type number == string.

:- type display ---> set ; seq ; bag .

:- type expr == pair(expr1, zcontext).

:- type expr1
	--->	ref(ref, ident, maybe(list(expr)))
	;	number(number)
 	;	stringl(string)
	;	display(ref, display, list(expr)) % ref to implicit empty-set
	;	setcomp(sexpr, maybe(expr))
	;	powerset(expr)
	;	tuple(list(expr))
	;	product(list(expr))
	;	tupleselection(expr, number)
%	;	bindingextn(assoc_list(ident, expr))
	;	let(assoc_list(ident, expr), expr)
	;	theta(ref, sexpr, decoration)
	;	sexp(sexpr)
	;	select(ref, expr, ident)
	;	zapply(ref, expr, expr)	% `apply' is a reserved word in Mercury
	;	lambda(sexpr, expr)
	;	mu(sexpr, maybe(expr))
	;	if(zpred, expr, expr)
%	;	expsubstitution(exp, exp)
	.

%%%
% A.8	IDENTIFIER

% See the file: word.m

%%%%%

:- func exprPortray(operators, expr) = string.

:- func expr1Portray(operators, expr1) = string.

:- pred writeSpec(spec, io__state, io__state).
:- mode writeSpec(in, di, uo) is det.

:- func display_to_string(display) = string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.
:- import_module int, higher_order, map. %, term, term_io, varset.

display_to_string(set) = "set".
display_to_string(seq) = "seq".
display_to_string(bag) = "bag".

exprPortray(O, X-_) = expr1Portray(O, X).

expr1Portray(O, lambda(_, X-_)) = S :-
	string__append_list(["(lambda .. @ ", expr1Portray(O, X), ")"], S).
expr1Portray(O, mu(_, M)) = S :-
	( M = no, S = "(mu .. )"
	; M = yes(X-_),
		string__append_list(["(mu .. @ ", expr1Portray(O, X), ")"], S)
	).
expr1Portray(O, let(_, X-_)) = S :-
	string__append_list(["(let .. @ ", expr1Portray(O, X), ")"], S).
expr1Portray(O, if(_, X0-_, X1-_)) = S :-
	string__append_list(["if .. then ",
			expr1Portray(O, X0), " else ", expr1Portray(O, X1)], S).
expr1Portray(O, zapply(_, X0-_, X1-_)) = S :-
	S0 = expr1Portray(O, X0),
	% ( X0 = ref(_, I, _), op(infun(_), I), X1 = tuple([X11, X12]) ->
	( X0 = ref(_, I, _), X1 = tuple([X11, X12]),
						map__search(O, I, infun(_)) ->
		string__append_list([
			"(", exprPortray(O, X11), " ", S0,
			" ", exprPortray(O, X12), ")"], S)
	;	S1 = expr1Portray(O, X1),
		% ( X0 = ref(_, I, _), op(postfun, I) ->
		( X0 = ref(_, I, _), map__search(O, I, postfun) ->
			string__append_list(["(", S1, " ", S0, ")"], S)
		;	string__append_list([S0, " ", S1], S)
		)
	).
expr1Portray(O, select(_, X0-_, I)) = S :-
	string__append_list(["(", expr1Portray(O, X0), ").", identPortray(I)], S).
expr1Portray(O, product(L)) = exprLPortray(O,  "", " x ", "",   L).
expr1Portray(O, tupleselection(X0-_, N)) = S :-
	string__append_list(["(", expr1Portray(O, X0), ").", N], S).
expr1Portray(O, tuple(L)) = exprLPortray(O, "(", ", ",  ")",  L).
expr1Portray(O, display(_, D, L)) = S :-
	( D = set, S = exprLPortray(O,  "{", ", ",   "}",  L)
	; D = seq, S = exprLPortray(O,  "<", ", ",   ">",  L)
	; D = bag, S = exprLPortray(O, "[[", ", ",  "]]",  L)
	).
expr1Portray(O, powerset(X-_)) = S :-
	string__append_list(["powerset(", expr1Portray(O, X), ")"], S).
expr1Portray(O, setcomp(_, M)) = S :-
	( M = no, S = "{ .. }"
	; M = yes(X-_),
		string__append_list(["{ .. @ ", expr1Portray(O, X), " }"], S)
	).
expr1Portray(O, theta(_, X, D)) = S :-
	string__append_list(["theta ", sexprPortray(O,X)|strokeLPortray(D)], S).
expr1Portray(O, sexp(X)) = sexprPortray(O, X).
expr1Portray(O, ref(_, I, M)) = S :-
	S0 = identPortray(I),
	( M = no, S = S0
	; M = yes(L),
		% ( L = [X1],     op(pregen, I) ->
		( L = [X1],     map__search(O, I, pregen) ->
			string__append_list(
				["(", S0, " ", exprPortray(O, X1), ")"], S)
		% ; L = [X1, X2], op(ingen, I) ->
		; L = [X1, X2], map__search(O, I, ingen) ->
			string__append_list(["(", exprPortray(O, X1), " ", S0,
					" ", exprPortray(O, X2), ")"], S)
		;	S1 = exprLPortray(O, "[", ", ", "]", L),
			string__append_list([S0, S1], S)
		)
	).
expr1Portray(_, number(S)) = S.
expr1Portray(_, stringl(S)) = S.

:- func exprLPortray(operators, string, string, string, list(expr)) = string.
exprLPortray(O, Pre, In, Post, L) =
	string_portray_list(exprPortray(O), Pre, In, Post, L).

:- func sexprPortray(operators, sexpr) = string.
sexprPortray(O, sexpr(_, S, _)) = sexprPortray1(O, S).

:- func sexprPortray1(operators, sexpr1) = string.
sexprPortray1(O, ref(I, MLE)) = S :-
	( MLE = no, S1 = ""
	; MLE = yes(AL), S1 = exprLPortray(O, "[", ", ",  "]", AL)
	),
	string__append(identPortray(I), S1, S).
sexprPortray1(O, text(DL, PL)) = S :-
	string__append_list(["[", schemaPortray(O, DL, PL), "]"], S).
sexprPortray1(O, negation(X)) = S :-
	string__append_list(["not (", sexprPortray(O, X), ")"], S).
sexprPortray1(O, lbpred(C, X1, X2)) = S :-
	( C = disjunction, SC = " v "
	; C = conjunction, SC = " & "
	; C = implication, SC = " => "
	; C = equivalence, SC = " <=> "
	),
	string__append_list([
		"(", sexprPortray(O, X1), SC, sexprPortray(O, X2), ")"
		], S).
sexprPortray1(O, projection(_, X1, X2)) = S :-
	string__append_list([
		"(", sexprPortray(O, X1), " proj ", sexprPortray(O, X2), ")"
		], S).
sexprPortray1(O, hide(_, X, L)) = S :-
	string__append_list([
		sexprPortray(O, X), string_portray_list(identPortray, L)
		], S).
sexprPortray1(O, quantification(Q, Sch, X)) = S :-
	( Q = universal, QS = "forall"
	; Q = exists, QS = "exists"
	; Q = unique, QS = "exists_1"
	),
	string__append_list([
		"(", QS, sexprPortray(O, Sch), "@ ", sexprPortray(O, X), ")"
		], S).
sexprPortray1(O, renaming(X, R)) = S :-
	P = (func(I1-I2) = SP :-
		string__append_list(
			[identPortray(I2), "/", identPortray(I1)], SP)),
	string__append(sexprPortray(O, X), string_portray_list(P, R), S).
sexprPortray1(O, bsexpr(_, C, X1, X2)) = S :-
	( C = composition, SC = " ; "
	; C = piping,      SC = " >> "
	),
	string__append_list([
		"(", sexprPortray(O, X1), SC, sexprPortray(O, X2), ")"
		], S).
sexprPortray1(O, decoration(X, D)) = S :-
	string__append_list([sexprPortray(O, X)|strokeLPortray(D)], S).
sexprPortray1(O, pre(_, X)) = S :-
	string__append_list(["pre (", sexprPortray(O, X), ")"], S).

:- func schemaPortray(operators, list(decl), list(zpred)) = string.
schemaPortray(O, DL, PL) = S :-
	( PL = [], PS = " "
	; PL = [_|_], PS = " | ... "
	),
	string__append_list([
		string_portray_list(declPortray(O), "", "; ", "", DL), PS
		], S).

:- func declPortray(operators, decl) = string.
declPortray(O, decl(IL, E)) = S :-
	string__append_list([
		string_portray_list(identPortray, "", ", ", ": ", IL),
		exprPortray(O, E)
		], S).
declPortray(O, include(S)) = sexprPortray(O, S).

writeSpec(S) --> io__write(S), io__nl.
