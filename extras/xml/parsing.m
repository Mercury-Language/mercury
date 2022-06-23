%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000, 2005-2006, 2011 The University of Melbourne.
% Copyright (C) 2014, 2016-2018, 2021-2022 The Mercury team.
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
%
% A useful way to avoid this problem, at least in some cases, is to bind
% the type variable by setting a dummy result value. e.g. instead of
%
%     parseChar -->
%         a or b or c or d or e or ....
%
% you can write
%
%     :- type dummy ---> dummy.
%     parseChar -->
%         return(dummy),
%         a or b or c or d or e or ....
%
% This does have a slight runtime cost (doing the return), but it has
% the benefit that it makes that great big combinator expression a
% constant - a big win.
%
%---------------------------------------------------------------------------%

:- module parsing.
:- interface.

:- import_module unicode.

:- import_module io.
:- import_module list.
:- import_module map.
:- import_module unit.
:- import_module univ.

%---------------------------------------------------------------------------%

:- mode pdi == in.
:- mode puo == out.

:- type entity_name
    --->    entity_anon
    ;       entity_internal(string)
    ;       entity_external(string).

:- type entity
    --->    entity(
                name    :: entity_name,
                text    :: string,
                size    :: int,
                curr    :: int
            ).

:- type encoding
    --->    some [Enc] (enc(Enc) => encoding(Enc)).

:- func make_entity(string) = entity.
:- func make_entity(entity_name, string) = entity.

:- typeclass encoding(Enc) where [
    (pred decode(Enc::in, unicode::out, entity::in, entity::out) is semidet),
    (pred encode(Enc::in, list(unicode)::in, string::out) is det)
].

:- func (make_encoding(Enc) = encoding) <= encoding(Enc).

:- typeclass global(K, V) where [].

:- type globals == map(univ, univ).

:- type pstate(T).

:- type parse(T)
    --->    ok(T)
    ;       error(string).

:- pred pstate(entity::in, encoding::in, globals::in,
    io::di, pstate(unit)::puo) is det.

:- pred finish(parse(T1)::out, pstate(T1)::pdi, io::uo) is det.

:- pred try_parse(parser(T1, T2)::in(parser),
    pred(T2, pstate(T2), pstate(T3))::in(pred(in, pdi, puo) is det),
    pred(string, pstate(T1), pstate(T3))::in(pred(in, pdi, puo) is det),
    pred(string, pstate(T1), pstate(T3))::in(pred(in, pdi, puo) is det),
    pstate(T1)::pdi, pstate(T3)::puo) is det.

:- pred parse(parser(T1, T2)::in(parser), parse(T2)::out,
    pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred parse_entity(parser(T1, T2)::in(parser), entity::in,
    pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred tok(pstate(T1)::pdi, pstate(unicode)::puo) is det.

:- pred return(T2::in, pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred return_unit(pstate(T1)::pdi, pstate(unit)::puo) is det.

:- pred record_failure(string::in, pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred record_error(string::in, pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred set_encoding(encoding::in, pstate(T1)::pdi, pstate(T1)::puo) is det.

:- pred get_encoding(encoding::out, pstate(T1)::pdi, pstate(T1)::puo) is det.

:- pred lit(string::in, T2::in, pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred lit(string::in, pstate(T1)::pdi, pstate(string)::puo) is det.

:- pred lit1(unicode::in, T2::in, pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred lit1(unicode::in, pstate(T1)::pdi, pstate(unicode)::puo) is det.

:- pred quote(pstate(T1)::pdi, pstate(unicode)::puo) is det.

:- pred io(pred(T1, io, io)::in(pred(out, di, uo) is det), T1::out,
    pstate(T2)::pdi, pstate(T2)::puo) is det.

:- pred io(pred(io, io)::in(pred(di, uo) is det),
    pstate(T2)::pdi, pstate(T2)::puo) is det.

:- pred make_string(list(unicode)::in, string::out,
    pstate(T2)::pdi, pstate(T2)::puo) is det.

:- type and_then(A, B)
    --->    and_then(A, B).

:- type opt(T)
    --->    no
    ;       yes(T).

:- type parser(T1, T2) == pred(pstate(T1), pstate(T2)).
:- inst parser == (pred(pdi, puo) is det).

:- pred and(parser(T1, T2)::in(parser), parser(T2, T3)::in(parser),
    pstate(T1)::pdi, pstate(and_then(T2, T3))::puo) is det.

:- pred or(parser(T1, T2)::in(parser), parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred then(parser(W, T)::in(parser),
    pred(T, pstate(T), pstate(U))::in(pred(in, pdi, puo) is det),
    pstate(W)::pdi, pstate(U)::puo) is det.

:- pred star(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(list(T2))::puo) is det.

:- pred plus(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(list(T2))::puo) is det.

:- pred call_opt(opt(T0)::in,
    pred(T0, pstate(T1), pstate(T2))::in(pred(in, pdi, puo) is det),
    parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred opt_default(parser(T1, T2)::in(parser), T2::in,
    pstate(T1)::pdi, pstate(T2)::puo) is det.

:- pred opt(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(opt(T2))::puo) is det.

:- pred upto(parser(T1, T2)::in(parser), parser(T1, T3)::in(parser),
    pstate(T1)::pdi, pstate(and_then(list(T2), T3))::puo) is det.

:- pred range(unicode::in, unicode::in,
    pstate(T1)::pdi, pstate(unicode)::puo) is det.

:- pred '-'(unicode::in, unicode::in,
    pstate(T1)::pdi, pstate(unicode)::puo) is det.

:- pred wrap(parser(T1, T2)::in(parser),
    pred(T2, T3)::in(pred(in, out) is det),
    pstate(T1)::pdi, pstate(T3)::puo) is det.

:- pred x(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(unit)::puo) is det.

:- pred first(parser(S, and_then(T, U))::in(parser),
    pstate(S)::pdi, pstate(T)::puo) is det.

:- pred second(parser(S, and_then(T, U))::in(parser),
    pstate(S)::pdi, pstate(U)::puo) is det.

:- pred except(list(unicode)::in,
    pstate(T1)::pdi, pstate(unicode)::puo) is det.

:- pred no(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(opt(T3))::puo) is det.

:- pred yes(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(opt(T2))::puo) is det.

:- pred filter(parser(T1, list(opt(T2)))::in(parser),
    pstate(T1)::pdi, pstate(list(T2))::puo) is det.

:- pred return_no(T1::in, opt(T2)::out) is det.

:- pred return_yes(T::in, opt(T)::out) is det.

:- pred list(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(list(T2))::puo) is det.

:- pred get_global(K::in, V::out, pstate(T)::pdi, pstate(T)::puo) is det
    <= global(K, V).

:- pred set_global(K::in, V::in, pstate(T)::pdi, pstate(T)::puo) is det
    <= global(K, V).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module char.
:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

    % Most of the time, what we are parsing is the text stored as
    % one of the arguments of the entity field. We step over the characters
    % of that text by updating the current pointer in the entity, without
    % doing any I/O.
    %
    % We however *do* need access to the I/O state, in two circumstances.
    %
    % - When processing external entities.
    % - When reporting warnings, e.g. about duplicate declarations.
    %
    % When this code was originally written, state variable notation
    % did not yet exist, which is why putting the I/O state inside
    % the parser state was the obvious way to handle this. The code
    % of the parser in xml.parse.m now relies heavily on all components
    % of the parser state being part of pstate(...), and the pervasive use
    % of higher order constructs that take a pair of pstates makes it
    % effectively impossible to break up the pstate into pieces without
    % a complete rewrite.
    %
    % There are arguments in favor of such a rewrite.
    %
    % - Putting the I/O state into the pstate requires unsafe code, in the
    %   form of unsafe_promise_unique operations every time the I/O state
    %   is taken out of the pstate.
    %
    % - Despite the convention that the pstate arguments used by DCG notation
    %   have pdi/puo modes (which are aliases for in/out respectively),
    %   we do have code that backtracks over updates to the pstate. The only
    %   reason why this works is because the I/O state type is a dummy type.
    %
    % - Taking the status out of the parser state would allow us to write code
    %   that *has* no data to store inside current status. This would avoid
    %   the compiler warnings about unresolved polymorphism for the handful
    %   of predicates in xml.parser.m that only
    %
    %   - match keywords, and
    %   - update the global field,
    %
    %   neither of which constrain the T inside the status(T).
    %
    % There are also arguments against such a rewrite, besides the amount of
    % of work required.
    %
    % - The fact that even with state variable notation, the explicit passing
    %   around of the (pieces of the) parse state would create clutter in
    %   the code. This is because most parser predicates, even those that
    %   do not themselves do I/O or touch the globals or the encoding field
    %   would have to pass them around in read/write pairs of arguments,
    %   in order to allow let them reach the predicates that need them
    %   near the leaves of the call tree>
    %
:- type pstate(T)
    --->    pstate(
                count    :: int,
                entity   :: entity,
                encoding :: encoding,
                status   :: status(T),
                globals  :: globals,
                io       :: io
            ).

:- type status(T)
    --->    ps_ok(T)
    ;       ps_fail(string)
    ;       ps_error(string).

make_entity(Str) = entity(entity_anon, Str, Size, 0) :-
    string.length(Str, Size).

make_entity(Name, Str) = entity(Name, Str, Size, 0) :-
    string.length(Str, Size).

make_encoding(Enc) = 'new enc'(Enc).

pstate(Entity, Enc, Globs, IO, PS) :-
    PS = pstate(0, Entity, Enc, ps_ok(unit), Globs, IO).

finish(Res, PS0, IO) :-
    get_status(Status, PS0, PS),
    (
        Status = ps_ok(Stuff),
        Res = ok(Stuff)
    ;
        Status = ps_fail(Msg),
        Res = error(Msg)
    ;
        Status = ps_error(Msg),
        Res = error(Msg)
    ),
    unsafe_promise_unique(PS ^ io, IO).

try_parse(P, S, F, E) -->
    mark(M, Ent),
    get_status(Status0),
    actuate(P),
    get_status(Status),
    (
        { Status = ps_ok(X) },
        call(S, X)
    ;
        { Status = ps_fail(Msg) },
        set_status(Status0),
        reset(M, Ent),
        call(F, Msg)
    ;
        { Status = ps_error(Msg) },
        set_status(Status0),
        call(E, Msg)
    ).

parse(P, Res) -->
    call(P),
    get_status(Status),
    {
        Status = ps_ok(Stuff),
        Res = ok(Stuff)
    ;
        Status = ps_fail(Msg),
        Res = error(Msg)
    ;
        Status = ps_error(Msg),
        Res = error(Msg)
    }.

parse_entity(Parser, Entity, !PS) :-
    E0 = !.PS ^ entity,
    !PS ^ entity := Entity,
    call(Parser, !PS),
    E1 = !.PS ^ entity,
    ( if E1 ^ curr = E1 ^ size then
        !PS ^ entity := E0
    else
        record_error("parse finished before the end of the entity", !PS)
    ).

:- pred actuate(parser(T1, T2)::in(parser),
    pstate(T1)::pdi, pstate(T2)::puo) is det.

actuate(P) -->
    get_status(Status),
    (
        { Status = ps_ok(_) },
        call(P)
    ;
        { Status = ps_fail(Msg) },
        record_failure(Msg)
    ;
        { Status = ps_error(Msg) },
        record_error(Msg)
    ).

:- pred mark(int::out, entity::out, pstate(T)::pdi, pstate(T)::puo) is det.

mark(!.PS ^ count, !.PS ^ entity, !PS).

:- pred reset(int::in, entity::in, pstate(T)::pdi, pstate(T)::puo) is det.

reset(Count, Entity, !PS) :-
    !PS ^ count := Count,
    !PS ^ entity := Entity.

tok(!PS) :-
    enc(Enc) = !.PS ^ encoding,
    Entity0 = !.PS ^ entity,
    ( if decode(Enc, Uni, Entity0, Entity) then
        !PS ^ status := ps_ok(Uni),
        !PS ^ entity := Entity,
        Count0 = !.PS ^ count,
        !PS ^ count := Count0 + 1
    else
        !PS ^ status := ps_fail("eof")
    ).

return(X, !PS) :-
    !PS ^ status := ps_ok(X).

return_unit -->
    return(unit).

record_failure(Msg, !PS) :-
    !PS ^ status := ps_fail(Msg).

record_error(Msg, !PS) :-
    !PS ^ status := ps_error(Msg).

set_encoding(Enc, !PS) :-
    !PS ^ encoding := Enc.

get_encoding(PS ^ encoding, PS, PS).

:- pred get_status(status(T)::out, pstate(T)::pdi, pstate(T)::puo) is det.

get_status(PS ^ status, PS, PS).

:- pred set_status(status(T1)::in, pstate(T2)::pdi, pstate(T1)::puo) is det.

set_status(S, PS, PS ^ status := S).

lit(Str, Thing) -->
    { string.to_char_list(Str, Chars) },
    (lit2(Chars)            then (pred(_::in, pdi, puo) is det -->
    return(Thing)
    )).

lit(Str) -->
    { string.to_char_list(Str, Chars) },
    (lit2(Chars)            then (pred(_::in, pdi, puo) is det -->
    return(Str)
    )).

lit1(U, R) -->
    tok                 then (pred(C::in, pdi, puo) is det -->
    ( if { U = C } then
        return(R)
    else
        record_failure("character didn't match")
    )).

lit1(U) -->
    tok                 then (pred(C::in, pdi, puo) is det -->
    ( if { U = C } then
        return(U)
    else
        record_failure("character didn't match")
    )).

:- pred lit2(list(char)::in, pstate(_)::pdi, pstate(unit)::puo) is det.

lit2([]) -->
    return(unit).
lit2([C | Is]) -->
    { char.to_int(C, I) },
    (tok                then (pred(I0::in, pdi, puo) is det -->
    ( if { I = I0 } then
        lit2(Is)
    else
        record_failure("literal failed to match")
    ))).

quote -->
    tok                 then (pred(Q::in, pdi, puo) is det -->
    ( if
        {
            Q = ('''')
        ;
            Q = ('"')
        }
    then
        return(Q)
    else 
        record_failure("expected a quote")
    )).

io(Pred, Res, !PS) :-
    unsafe_promise_unique(!.PS ^ io, IO0),
    call(Pred, Res, IO0, IO),
    !PS ^ io := IO.

io(Pred, !PS) :-
    unsafe_promise_unique(!.PS ^ io, IO0),
    call(Pred, IO0, IO),
    !PS ^ io := IO.

make_string(UniCodes, String, PS, PS) :-
    enc(Enc) = PS ^ encoding,
    encode(Enc, UniCodes, String).

(A and B) -->
    actuate(A)              then (pred(X::in, pdi, puo) is det -->
    actuate(B)              then (pred(Y::in, pdi, puo) is det -->
    return(and_then(X, Y))
    )).

(A or B) -->
    try_parse(A,
        return,
        (pred(_::in, pdi, puo) is det --> call(B)),
        record_error).

then(P, T) -->
    actuate(P),
    get_status(Status1),
    (
        { Status1 = ps_ok(X) },
        call(T, X)
    ;
        { Status1 = ps_fail(Msg) },
        set_status(ps_fail(Msg))
    ;
        { Status1 = ps_error(Msg) },
        set_status(ps_error(Msg))
    ).

star(P) -->
    star(P, []).

:- pred star(parser(T1, T2)::in(parser), list(T2)::in,
    pstate(T1)::pdi, pstate(list(T2))::puo) is det.

star(P, Xs0) -->
    get_status(Status0),
    mark(Start, _Ent),
    try_parse(P,
        ( pred(X::in, pdi, puo) is det -->
            mark(End, _EEnt),
            ( if { Start \= End } then
                set_status(Status0),
                star(P, [X | Xs0])
            else
                record_failure("star(null)")
            )
        ),
        ( pred(_::in, pdi, puo) is det -->
            { list.reverse(Xs0, Xs) },
            return(Xs)
        ),
        record_error
    ).

plus(P) -->
    get_status(Status0),
    (actuate(P)            then (pred(X::in, pdi, puo) is det -->
    set_status(Status0),
    star(P, [X])
    )).

call_opt(no, _Yes, No) -->
    call(No).
call_opt(yes(Thing), Yes, _No) -->
    call(Yes, Thing).

opt_default(P, Def) -->
    try_parse(P,
        return,
        ( pred(_::in, pdi, puo) is det -->
            return(Def)
        ),
        record_error
    ).

opt(P) -->
    try_parse(P,
        ( pred(X::in, pdi, puo) is det -->
            return(yes(X))
        ),
        ( pred(_::in, pdi, puo) is det -->
            return(no)
        ),
        record_error
    ).

upto(Rep, Fin) -->
    upto(Rep, Fin, []).

:- pred upto(parser(T1, T2)::in(parser), parser(T1, T3)::in(parser),
    list(T2)::in, pstate(T1)::pdi, pstate(and_then(list(T2), T3))::puo) is det.

upto(Rep, Fin, Rs0) -->
    get_status(Status0),
    try_parse(Fin,
        ( pred(F::in, pdi, puo) is det -->
            { list.reverse(Rs0, Rs) },
            return(and_then(Rs, F))
        ),
        ( pred(_::in, pdi, puo) is det -->
            set_status(Status0),
            (Rep            then (pred(R::in, pdi, puo) is det -->
            set_status(Status0),
            upto(Rep, Fin, [R | Rs0])
        ))),
        record_error
    ).

range(F, L) -->
    tok                 then (pred(C::in, pdi, puo) is det -->
    ( if { F =< C, C =< L } then
        return(C)
    else
        record_failure("not in range")
    )).

(F - L) -->
    range(F, L).

wrap(P, Q) -->
    P                   then (pred(X::in, pdi, puo) is det -->
    { call(Q, X, W) },
    return(W)
    ).

x(P) -->
    P                   then (pred(_::in, pdi, puo) is det -->
    return(unit)
    ).

first(P) -->
    P                   then (pred(and_then(T, _)::in, pdi, puo) is det -->
    return(T)
    ).

second(P) -->
    P                   then (pred(and_then(_, T)::in, pdi, puo) is det -->
    return(T)
    ).

except(Exclusions) -->
    tok                 then (pred(C::in, pdi, puo) is det -->
    ( if { list.member(C, Exclusions) } then
        record_failure("excluded character")
    else
        return(C)
    )).

no(Parser) -->
    Parser              then (pred(_::in, pdi, puo) is det -->
    return(no)
    ).

yes(Parser) -->
    Parser              then (pred(X::in, pdi, puo) is det -->
    return(yes(X))
    ).

filter(Parser) -->
    Parser              then (pred(Xs0::in, pdi, puo) is det -->
    { filter1(Xs0, Xs) },
    return(Xs)
    ).

:- pred filter1(list(opt(T))::in, list(T)::out) is det.

filter1([], []).
filter1([X0 | Xs0], Xs) :-
    (
        X0 = yes(X),
        filter1(Xs0, Xs1),
        Xs = [X | Xs1]
    ;
        X0 = no,
        filter1(Xs0, Xs)
    ).

return_no(_, no).

return_yes(T, yes(T)).

list(P) -->
    P                   then (pred(X::in, pdi, puo) is det -->
    return([X])
    ).

get_global(Key, Val, !PS) :-
    map.lookup(!.PS ^ globals, univ(Key), Val0),
    det_univ_to_type(Val0, Val).

set_global(Key, Val, !PS) :-
    map.set(univ(Key), univ(Val), !.PS ^ globals, Globals),
    !PS ^ globals := Globals.
