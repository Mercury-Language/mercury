%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2005-2006, 2011 The University of Melbourne.
% Copyright (C) 2014, 2016-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% Main author: conway@cs.mu.oz.au.
%
% This module provides a bunch of parsing combinators directed towards
% parsing text (in some encoding or bunch of encodings). The parsing state
% that gets threaded through is polymorphic in the type of the result
% stored in it. This can cause problems if you construct a big combinator
% expression (particularly using the "or" combinator) where the type
% of this result in the initial parsing state is unbound and is inherited
% from its context. In this case, the combinator expression cannot be made
% into a static ground term (the typeinfo arguments which must come first
% are not known until runtime), so it gets constructed every time through.
% (See e.g. xml.parse.chars.m for some examples.)
% A useful way to avoid this problem, at least in some cases, is to
% bind the type variable by setting a dummy result value.
% e.g. instead of
%     parseChar -->
%         a or b or c or d or e or ....
% you can write
%     :- type dummy ---> dummy.
%     parseChar -->
%         return(dummy),
%         a or b or c or d or e or ....
%
% This does have a slight runtime cost (doing the return), but it has
% the benefit that it makes that great big combinator expression a
% constants - a big win.
%
%---------------------------------------------------------------------------%
:- module parsing.

:- interface.

:- import_module string, unicode.
:- import_module io, list, map, unit, univ.

:- mode pdi == in.
:- mode puo == out.

:- type entityName
	--->	anon
	;	internal(string)
	;	external(string)
	.

:- type entity
	--->	entity(
		    curr	:: int,
		    leng	:: int,
		    text	:: string,
		    name	:: entityName
		).

:- type encoding
	--->	some [Enc] (enc(Enc) => encoding(Enc)).

:- func mkEntity(string) = entity.
:- func mkEntity(entityName, string) = entity.

:- typeclass encoding(Enc) where [
	(pred decode(Enc, unicode, entity, entity)),
	(mode decode(in, out, in, out) is semidet),
	(pred encode(Enc, list(unicode), string)),
	(mode encode(in, in, out) is det)
].

:- func (mkEncoding(Enc) = encoding) <= encoding(Enc).

:- typeclass global(K, V) where [].

:- type globals == map(univ, univ).

:- type pstate(T).

:- type parse(T)
	--->	ok(T)
	;	error(string).

:- pred pstate(entity, encoding, globals, io, pstate(unit)).
:- mode pstate(in, in, in, di, puo) is det.

:- pred finish(parse(T1), pstate(T1), io).
:- mode finish(out, pdi, uo) is det.

:- pred try(parser(T1, T2),
	    pred(T2, pstate(T2), pstate(T3)),
	    pred(string, pstate(T1), pstate(T3)),
	    pred(string, pstate(T1), pstate(T3)),
	    pstate(T1), pstate(T3)).
:- mode try(in(parser),
	    pred(in, pdi, puo) is det,
	    pred(in, pdi, puo) is det,
	    pred(in, pdi, puo) is det, pdi, puo) is det.

:- pred parse(parser(T1, T2), parse(T2), pstate(T1), pstate(T2)).
:- mode parse(in(parser), out, pdi, puo) is det.

:- pred parseEntity(parser(T1, T2), entity, pstate(T1), pstate(T2)).
:- mode parseEntity(in(parser), in, pdi, puo) is det.

:- pred tok(pstate(_), pstate(unicode)).
:- mode tok(pdi, puo) is det.

:- pred return(T, pstate(_), pstate(T)).
:- mode return(in, pdi, puo) is det.

:- pred return(pstate(_), pstate(unit)).
:- mode return(pdi, puo) is det.

:- pred fail(string, pstate(_), pstate(_)).
:- mode fail(in, pdi, puo) is det.

:- pred error(string, pstate(_), pstate(_)).
:- mode error(in, pdi, puo) is det.

:- pred setEncoding(encoding, pstate(T1), pstate(T1)).
:- mode setEncoding(in, pdi, puo) is det.

:- pred getEncoding(encoding, pstate(T1), pstate(T1)).
:- mode getEncoding(out, pdi, puo) is det.

:- pred lit1(unicode, T, pstate(_), pstate(T)).
:- mode lit1(in, in, pdi, puo) is det.

:- pred lit1(unicode, pstate(_), pstate(unicode)).
:- mode lit1(in, pdi, puo) is det.

:- pred lit(string, pstate(_), pstate(string)).
:- mode lit(in, pdi, puo) is det.

:- pred lit(string, T, pstate(_), pstate(T)).
:- mode lit(in, in, pdi, puo) is det.

:- pred quote(pstate(_), pstate(unicode)).
:- mode quote(pdi, puo) is det.

:- pred io(pred(T1, io, io), T1, pstate(T2), pstate(T2)).
:- mode io(pred(out, di, uo) is det, out, pdi, puo) is det.

:- pred io(pred(io, io), pstate(T2), pstate(T2)).
:- mode io(pred(di, uo) is det, pdi, puo) is det.

:- pred mkString(list(unicode), string, pstate(T1), pstate(T1)).
:- mode mkString(in, out, pdi, puo) is det.

:- type (A, B) ---> (A, B).

:- type opt(T) ---> no ; yes(T).

:- type parser(T1, T2) == pred(pstate(T1), pstate(T2)).

:- inst parser == (pred(pdi, puo) is det).

:- pred and(parser(T1, T2), parser(T2, T3), pstate(T1), pstate((T2, T3))).
:- mode and(in(parser), in(parser), pdi, puo) is det.

:- pred or(parser(T1, T2), parser(T1, T2), pstate(T1), pstate(T2)).
:- mode or(in(parser), in(parser), pdi, puo) is det.

:- pred then(parser(W, T), pred(T, pstate(T), pstate(U)), pstate(W), pstate(U)).
:- mode then(in(parser), pred(in, pdi, puo) is det, pdi, puo) is det.

:- pred star(parser(T1, T2), pstate(T1), pstate(list(T2))).
:- mode star(in(parser), pdi, puo) is det.

:- pred plus(parser(T1, T2), pstate(T1), pstate(list(T2))).
:- mode plus(in(parser), pdi, puo) is det.

:- pred opt(parser(T1, T2), T2, pstate(T1), pstate(T2)).
:- mode opt(in(parser), in, pdi, puo) is det.

:- pred opt(parser(T1, T2), pstate(T1), pstate(opt(T2))).
:- mode opt(in(parser), pdi, puo) is det.

:- pred opt(opt(T0), pred(T0, pstate(T1), pstate(T2)),
		parser(T1, T2), pstate(T1), pstate(T2)).
:- mode opt(in, in(pred(in, pdi, puo) is det), in(parser), pdi, puo) is det.

:- pred upto(parser(T1, T2), parser(T1, T3),
		pstate(T1), pstate((list(T2), T3))).
:- mode upto(in(parser), in(parser), pdi, puo) is det.

:- pred range(unicode, unicode, pstate(_), pstate(unicode)).
:- mode range(in, in, pdi, puo) is det.

:- pred '-'(unicode, unicode, pstate(T1), pstate(unicode)).
:- mode '-'(in, in, pdi, puo) is det.

:- pred wrap(parser(T1, T2), pred(T2, T3), pstate(T1), pstate(T3)).
:- mode wrap(in(parser), pred(in, out) is det, pdi, puo) is det.

:- pred x(parser(T1, T2), pstate(T1), pstate(unit)).
:- mode x(in(parser), pdi, puo) is det.

:- pred fst(parser(S, (T,U)), pstate(S), pstate(T)).
:- mode fst(in(parser), pdi, puo) is det.

:- pred snd(parser(S, (T,U)), pstate(S), pstate(U)).
:- mode snd(in(parser), pdi, puo) is det.

:- pred except(list(unicode), pstate(T1), pstate(unicode)).
:- mode except(in, pdi, puo) is det.

:- pred no(parser(T1, T2), pstate(T1), pstate(opt(T3))).
:- mode no(in(parser), pdi, puo) is det.

:- pred yes(parser(T1, T2), pstate(T1), pstate(opt(T2))).
:- mode yes(in(parser), pdi, puo) is det.

:- pred filter(parser(T1, list(opt(T2))), pstate(T1), pstate(list(T2))).
:- mode filter(in(parser), pdi, puo) is det.

:- pred no(T1, opt(T2)).
:- mode no(in, out) is det.

:- pred yes(T, opt(T)).
:- mode yes(in, out) is det.

:- pred list(parser(T1, T2), pstate(T1), pstate(list(T2))).
:- mode list(in(parser), pdi, puo) is det.

:- pred get(K, V, pstate(T3), pstate(T3)) <= global(K, V).
:- mode get(in, out, pdi, puo) is det.

:- pred set(K, V, pstate(T3), pstate(T3)) <= global(K, V).
:- mode set(in, in, pdi, puo) is det.

:- implementation.

:- import_module char, int, list, string.

:- type pstate(T)
	--->	s(
		    count	:: int,
		    entity	:: entity,
		    encoding	:: encoding,
		    status	:: status(T),
		    globals	:: globals,
		    io		:: io
		).

:- type status(T)
	--->	ok(T)
	;	fail(string)
	;	error(string)
	.

mkEntity(Str) = entity(0, Leng, Str, anon) :-
    length(Str, Leng).

mkEntity(Name, Str) = entity(0, Leng, Str, Name) :-
    length(Str, Leng).

mkEncoding(Enc) = 'new enc'(Enc).

pstate(Entity, Enc, Globs, IO, PS) :-
    PS = s(0, Entity, Enc, ok(unit), Globs, IO).

finish(Res, PS0, IO) :-
    status(Status, PS0, PS),
    (
	Status = ok(Stuff),
	Res = ok(Stuff)
    ;
	Status = fail(Msg),
	Res = error(Msg)
    ;
	Status = error(Msg),
	Res = error(Msg)
    ),
    IO = u(PS^io).

parse(P, Res) -->
    call(P),
    status(Status),
    {
	Status = ok(Stuff),
	Res = ok(Stuff)
    ;
	Status = fail(Msg),
	Res = error(Msg)
    ;
	Status = error(Msg),
	Res = error(Msg)
    }.

parseEntity(Parser, Entity, PS0, PS) :-
    E0 = PS0^entity,
    PS1 = PS0^entity := Entity,
    call(Parser, PS1, PS2),
    E1 = PS2^entity,
    (
        E1^curr = E1^leng
    ->
    	PS = PS2^entity := E0
    ;
    	error("parse finished before the end of the entity", PS2, PS)
    ).

:- pred actuate(parser(T1, T2), pstate(T1), pstate(T2)).
:- mode actuate(in(parser), pdi, puo) is det.

actuate(P) -->
    status(Status),
    (
    	{ Status = ok(_) },
	call(P)
    ;
    	{ Status = fail(Msg) },
	fail(Msg)
    ;
    	{ Status = error(Msg) },
	error(Msg)
    ).

try(P, S, F, E) -->
    mark(M, Ent),
    status(Status0),
    actuate(P),
    status(Status),
    (
    	{ Status = ok(X) },
	call(S, X)
    ;
    	{ Status = fail(Msg) },
	setStatus(Status0),
	reset(M, Ent),
	call(F, Msg)
    ;
    	{ Status = error(Msg) },
	setStatus(Status0),
	call(E, Msg)
    ).

then(P, T) -->
    actuate(P),
    status(Status1),
    (
    	{ Status1 = ok(X) },
	call(T, X)
    ;
    	{ Status1 = fail(Msg) },
	setStatus(fail(Msg))
    ;
    	{ Status1 = error(Msg) },
	setStatus(error(Msg))
    ).

:- pred mark(int, entity, pstate(T), pstate(T)).
:- mode mark(out, out, pdi, puo) is det.

mark(PS^count, PS^entity, PS, PS).

:- pred reset(int, entity, pstate(T), pstate(T)).
:- mode reset(in, in, pdi, puo) is det.

reset(Curr, Entity, PS0, PS) :-
    PS1 = PS0^count := Curr,
    PS = PS1^entity := Entity.

tok(PS0, PS) :-
    Entity0 = PS0^entity,
    enc(Enc) = PS0^encoding,
    ( decode(Enc, Uni, Entity0, Entity) ->
        PS1 = PS0^status := ok(Uni),
	PS2 = PS1^entity := Entity,
	PS = PS2^count := (PS2^count + 1)
    ;
        PS = PS0^status := fail("eof")
    ).

return(X, PS0, PS) :-
    PS = PS0^status := ok(X).

return -->
    return(unit).

fail(Msg, PS0, PS) :-
    PS = PS0^status := fail(Msg).

error(Msg, PS0, PS) :-
    PS = PS0^status := error(Msg).

setEncoding(Enc, PS0, PS) :-
	PS = PS0^encoding := Enc.

getEncoding(PS^encoding, PS, PS).

:- pred status(status(T), pstate(T), pstate(T)).
:- mode status(out, pdi, puo) is det.

status(PS^status, PS, PS).

:- pred setStatus(status(T1), pstate(T2), pstate(T1)).
:- mode setStatus(in, pdi, puo) is det.

setStatus(S, PS, PS^status := S).

lit1(U, R) -->
    tok				    then (pred(C::in, pdi, puo) is det -->
    ( { U = C } ->
    	return(R)
    ;
    	fail("character didn't match")
    )).

lit1(U) -->
    tok				    then (pred(C::in, pdi, puo) is det -->
    ( { U = C } ->
    	return(U)
    ;
    	fail("character didn't match")
    )).

lit(Str) -->
    { string.to_char_list(Str, Chars) },
    (lit2(Chars)		    then (pred(_::in, pdi, puo) is det -->
    return(Str)
    )).

lit(Str, Thing) -->
    { string.to_char_list(Str, Chars) },
    (lit2(Chars)		    then (pred(_::in, pdi, puo) is det -->
    return(Thing)
    )).

:- pred lit2(list(char), pstate(_), pstate(unit)).
:- mode lit2(in, pdi, puo) is det.

lit2([]) -->
    return(unit).
lit2([C|Is]) -->
    { char.to_int(C, I) },
    (tok			    then (pred(I0::in, pdi, puo) is det -->
    ( { I = I0 } ->
	lit2(Is)
    ;
    	fail("literal failed to match")
    ))).

quote -->
    tok				    then (pred(Q::in, pdi, puo) is det -->
    ( {
        Q = ('''')
    ;
        Q = ('"')
    } ->
    	return(Q)
    ;
    	fail("expected a quote")
    )).

io(Pred, Res, PS0, PS) :-
    call(Pred, Res, u(PS0^io), IO),
    PS = PS0^io := IO.

io(Pred, PS0, PS) :-
    call(Pred, u(PS0^io), IO),
    PS = PS0^io := IO.

mkString(UniCodes, String, PS, PS) :-
    enc(Enc) = PS^encoding,
    encode(Enc, UniCodes, String).

(A and B) -->
    actuate(A)			    then (pred(X::in, pdi, puo) is det -->
    actuate(B)			    then (pred(Y::in, pdi, puo) is det -->
    return((X, Y))
    )).

(A or B) -->
    try(A,
    	return,
	(pred(_::in, pdi, puo) is det --> call(B)),
	error).

star(P) -->
    star(P, []).

:- pred star(parser(T1, T2), list(T2), pstate(T1), pstate(list(T2))).
:- mode star(in(parser), in, pdi, puo) is det.

star(P, Xs0) -->
    status(Status0),
    mark(Start, _Ent),
    try(P,
    	(pred(X::in, pdi, puo) is det -->
	    mark(End, _EEnt),
	    ( { Start \= End } ->
		setStatus(Status0),
		star(P, [X|Xs0])
	    ;
	    	fail("star(null)")
	    )
	),
	(pred(_::in, pdi, puo) is det -->
	    { reverse(Xs0, Xs) },
	    return(Xs)
	),
	error
    ).

plus(P) -->
    status(Status0),
    (actuate(P)			   then (pred(X::in, pdi, puo) is det -->
    setStatus(Status0),
    star(P, [X])
    )).

opt(P, Def) -->
    try(P,
    	return,
	(pred(_::in, pdi, puo) is det -->
	    return(Def)
	),
	error
    ).

opt(no, _Yes, No) -->
    call(No).
opt(yes(Thing), Yes, _No) -->
    call(Yes, Thing).

opt(P) -->
    try(P,
    	(pred(X::in, pdi, puo) is det -->
	    return(yes(X))
	),
    	(pred(_::in, pdi, puo) is det -->
	    return(no)
	),
	error
    ).

upto(Rep, Fin) -->
    upto(Rep, Fin, []).

:- pred upto(parser(T1, T2), parser(T1, T3), list(T2),
		pstate(T1), pstate((list(T2), T3))).
:- mode upto(in(parser), in(parser), in, pdi, puo) is det.

upto(Rep, Fin, Rs0) -->
    status(Status0),
    try(Fin,
    	(pred(F::in, pdi, puo) is det -->
	    { reverse(Rs0, Rs) },
	    return((Rs, F))
	),
	(pred(_::in, pdi, puo) is det -->
	    setStatus(Status0),
	    (Rep		    then (pred(R::in, pdi, puo) is det -->
	    setStatus(Status0),
	    upto(Rep, Fin, [R|Rs0])
	))),
	error
    ).

range(F, L) -->
    tok				    then (pred(C::in, pdi, puo) is det -->
    ( { F =< C, C =< L } ->
    	return(C)
    ;
    	fail("not in range")
    )).

(F - L) -->
    range(F, L).

wrap(P, Q) -->
    P				    then (pred(X::in, pdi, puo) is det -->
    { call(Q, X, W) },
    return(W)
    ).

x(P) -->
    P				    then (pred(_::in, pdi, puo) is det -->
    return(unit)
    ).

fst(P) -->
    P				    then (pred((T, _)::in, pdi, puo) is det -->
    return(T)
    ).

snd(P) -->
    P				    then (pred((_, T)::in, pdi, puo) is det -->
    return(T)
    ).

except(Exclusions) -->
    tok				    then (pred(C::in, pdi, puo) is det -->
    ( { not member(C, Exclusions) } ->
        return(C)
    ;
    	fail("excluded character")
    )).

no(Parser) -->
    Parser			    then (pred(_::in, pdi, puo) is det -->
    return(no)
    ).

yes(Parser) -->
    Parser			    then (pred(X::in, pdi, puo) is det -->
    return(yes(X))
    ).

filter(Parser) -->
    Parser			    then (pred(Xs0::in, pdi, puo) is det -->
    { filter1(Xs0, Xs) },
    return(Xs)
    ).

:- pred filter1(list(opt(T))::in, list(T)::out) is det.

filter1([], []).
filter1([X0|Xs0], Xs) :-
    (
    	X0 = yes(X),
	filter1(Xs0, Xs1),
	Xs = [X|Xs1]
    ;
    	X0 = no,
	filter1(Xs0, Xs)
    ).

list(P) -->
    P				    then (pred(X::in, pdi, puo) is det -->
    return([X])
    ).

no(_, no).

yes(T, yes(T)).

get(Key, Val, PS, PS) :-
	lookup(PS^globals, univ(Key), Val0),
	det_univ_to_type(Val0, Val).

set(Key, Val, PS0, PS) :-
	map.set(univ(Key), univ(Val), PS0^globals, Globals),
	PS = PS0^globals := Globals.

:- func u(T) = T.
:- mode (u(in) = uo) is det.

u(X) = Y :-
    unsafe_promise_unique(X, Y).

