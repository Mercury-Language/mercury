%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: zparser.m
% main author: philip

:- module zparser.

:- interface.
:- import_module list, word, zabstract, ztoken.

:- type parseResult ---> ok(spec) ; error(list(string)). 
	% Error list in reverse so caller can add to it

:- type schema_table.

:- func init_schema_table = schema_table.

:- pred specification(ztoken_list, parseResult,
				schema_table, schema_table, flags, flags).
:- mode specification(in, out, in, out, in, out) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.
:- import_module maybe, pair, string, int, require, assoc_list.

:- type schema_name == pair(maybe(operation), word).

:- type schema_table == assoc_list(schema_name, int).

init_schema_table = [].

:- type default_defn ---> default_defn(zcontext, operation, word, int).

:- type pstate ---> p(
	string, 	% input stream name
	list(string),	% warning/error messages
	status,		% error indicator
	ztoken_list,	% tokens being parsed
	flags,		% debugging flags
	schema_table,	% declared schema names + #gen parameters
	list(default_defn),% delta and xi schemas defined by default
	ref		% for generating unque refs to indentifier references
	).

:- pred new_ref(ref::out, pstate::in, pstate::out) is det.
new_ref(R, p(SN, IO, S, TL, F, SL, D, R), p(SN, IO, S, TL, F, SL, D, R + 1)).

:- pred make_ref(zcontext::in, ident::in, maybe(list(expr))::in, expr::out, 
					pstate::in, pstate::out) is det.
make_ref(C, I, M, ref(Ref, I, M)-C) --> new_ref(Ref).

:- pred make_sexpr(zcontext::in, sexpr1::in, sexpr::out,
						pstate::in, pstate::out) is det.
make_sexpr(C, S, sexpr(Ref, S, C)) --> new_ref(Ref).

% :- import_module unsafe, io.

:- pred schema_declared(schema_name, int, pstate, pstate).
:- mode schema_declared(in, out, in, out) is semidet.
schema_declared(W, I) -->
	=(p(_, _, _, _, _, SL, _, _)),
		% {unsafe_perform_io(io__write(W)),
		% unsafe_perform_io(io__nl),
		% unsafe_perform_io(io__write(SL)),
		% unsafe_perform_io(io__nl)},
	{assoc_list__search(SL, W, I)}.

:- pred schema_declare(schema_name, int, pstate, pstate).
:- mode schema_declare(in, in, in, out) is det.
schema_declare(W, I,
	p(SN, IO, S, TL, F,      SL,  D, R),
	p(SN, IO, S, TL, F, [W-I|SL], D, R)).

:- pred schema_define(zcontext, operation, word, int, pstate, pstate).
:- mode schema_define(in, in, in, in, in, out) is det.
schema_define(C, O, W, I,
	p(SN, IO, S, TL, F, SL, 		          D , R),
	p(SN, IO, S, TL, F, SL, [default_defn(C, O, W, I)|D], R)).

:- pred add_abbrev(list(ident), pstate, pstate).
:- mode add_abbrev(in, in, out) is det.
add_abbrev(IdentL,
		p(SN, IO, S, TL, F0, SL, D, R),
		p(SN, IO, S, TL,  F, SL, D, R)) :-
	list__append(IdentL, abbreviations(F0), Abbreviations),
	set_abbreviations(Abbreviations, F0, F).

:- pred add_monot(list(ident), pstate, pstate).
:- mode add_monot(in, in, out) is det.
add_monot(IdentL,
		p(SN, IO, S, TL, F0, SL, D, R),
		p(SN, IO, S, TL,  F, SL, D, R)) :-
	list__append(IdentL, monotonics(F0), Monotonics),
	set_monotonics(Monotonics, F0, F).

:- import_module map.  % Used only in add_operat/4---should be in word.m
:- pred add_operat(op, list(ident), pstate, pstate).
:- mode add_operat(in, in, in, out) is det.
add_operat(Op, IdentL,
		p(SN, IO, S, TL, F0, SL, D, R),
		p(SN, IO, S, TL,  F, SL, D, R)) :-
	add_operators(Op, IdentL, F0, F).

:- pred add_loglib(list(ident), pstate, pstate).
:- mode add_loglib(in, in, out) is det.
add_loglib(IdentL,
		p(SN, IO, S, TL, F0, SL, D, R),
		p(SN, IO, S, TL,  F, SL, D, R)) :-
	list__append(IdentL, loglib_ids(F0), Loglib_ids),
	set_loglib_ids(Loglib_ids, F0, F).

:- pred set_generate(flag, pstate, pstate).
:- mode set_generate(in, in, out) is det.
set_generate(Flag,
		p(SN, IO, S, TL, F0, SL, D, R),
		p(SN, IO, S, TL,  F, SL, D, R)) :-
	set_generating_logic(Flag, F0, F).

% :- pred zdebugging(pstate, pstate).
% :- mode zdebugging(in, out) is semidet.
% zdebugging --> =(p(_, _, _, _, F, _, _, _)), {debugging(F)}.

:- pragma inline(pred(x/3)).
:- pred x(ztoken, pstate, pstate).
:- mode x(out, in, out) is semidet.
x(T, p(SN, IO, S, [T-_|TL], F, SL, D, R), p(SN, IO, S, TL, F, SL, D, R)).

:- pragma inline(pred(x/4)).
:- pred x(ztoken, zcontext, pstate, pstate).
:- mode x(out, out, in, out) is semidet.
x(T, C, p(SN, IO, S, [T-C|TL], F, SL, D, R), p(SN, IO, S, TL, F, SL, D, R)).

:- pragma inline(pred(add_tokens/3)).
:- pred add_tokens(ztoken_list, pstate, pstate).
:- mode add_tokens(in, in, out) is det.
add_tokens(L, p(SN, IO, S, TL0, F, SL, D, R), p(SN, IO, S, TL, F, SL, D, R)) :-
	append(L, TL0, TL).

:- pragma inline(pred(c/3)).
% A context of 0 should never be added to a construct,
% because there must have been some tokens from which
% to derive the construct.  The 0 case is only to make c/3 det.
:- pred c(zcontext, pstate, pstate).
%:- mode c(out, in, out) is semidet.
:- mode c(out, in, out) is det.
%c(C) --> =(p(_, _, _, [_-C|_], _, _, _, _)).
c(C) --> =(p(_, _, _, TL, _, _, _, _)), {TL = [_-C|_] ; TL = [], C = 0}.

:- pred expect(ztoken, pstate, pstate).
:- mode expect(in, in, out) is det.
expect(T) -->
	( x(T) ->
		{true}
	;	{ztokenPortray(T, ST)},
		zerror(ST)
	).

:- pred parserError(string, ztoken_list, string, string).
:- mode parserError(in, in, in, out) is det.
parserError(SN, Input, Mesg, EMesg) :-
	( list__split_list(10, Input, L0, R) ->
		L = L0, ( R = [], End = "" ; R = [_|_], End = " .." )
	;	L = Input, End = ""
	),
	( L = [], LNS = "eof"
	; L = [_-LN|_], string__int_to_base_string(LN, 10, LNS)
	),
	list__length(L, N),
	ztokenPortrayL(L, TL),
	list__duplicate(N, " ", SL),
	list__zip(SL, TL, STL),
	string__append_list(STL, ST),
	% string__append_list([SN,":",LNS,": ... >>",ST,End,
	string__append_list([SN,LNS,": ... >>",ST,End,".\nError: ",Mesg],
									EMesg).

:- pred zerror(string, pstate, pstate, pstate).
:- mode zerror(in, in, in, out) is det.
zerror(S0, p(_, _, _, ETL, _, _, _, _), p(SN,    E ,     _, TL, F, SL, D, R),
					p(SN, [S|E], error, TL, F, SL, D, R)) :-
	parserError(SN, ETL, S0, S).

:- pred zerror(string, pstate, pstate).
:- mode zerror(in, in, out) is det.
zerror(S0, p(SN,    E ,     _, TL, F, SL, D, R),
	   p(SN, [S|E], error, TL, F, SL, D, R)) :-
	string__append(S0, " expected", S1),
	parserError(SN, TL, S1, S).

%%%
% B.1.1	SPECIFICATION

specification(TL, Result, Schemas0, Schemas, F0, F) :-
	SN = "",	% SN and Status fields MARKED FOR DELETION
	P0 = p(SN, [], ok, TL, F0, Schemas0, [], 1),	% Ref = 1 (0 reserved)
	specification1([], S0, P0, P),
	P = p(_, Errors, Status, _, F, Schemas, _, _),
	( Status = ok, list__reverse(S0, S), Result = ok(S)
	; Status = error, Result = error(Errors)
	).

:- pred specification1(spec, spec, pstate, pstate).
:- mode specification1(in, out, in, out) is det.
specification1(L0, L) -->
	( =(I0), x(B), {paragraph(B)} ->
		( c(C), paragraph(B, MP) ->
			( x(zEND) ->
				{true}
			;	( =(p(_, _, ok, _, _, _, _, _)) ->
					zerror("Z paragraph end")
				;	{true}
				)
			),
			add_default_ops(L0, L1),
			{ MP = no, L2 = L1 ; MP = yes(P), L2 = [P-C|L1] },
			specification1(L2, L)
		;	zerror("paragraph not parsed", I0),
			consume_to_end, % consume_to_end(B),
			specification1(L0, L)
		)
	; x(_) ->			% Remove tokens before next par
		specification1(L0, L)
	; {L = L0}
	).

%%%
% PARAGRAPHS

:- pred paragraph(ztoken).
:- mode paragraph(in) is semidet.
paragraph(pragma(_)).
paragraph(zBEGIN).
paragraph(zAX).
paragraph(zSCH).
paragraph(zGEN).

:- pred paragraph(ztoken, maybe(par1), pstate, pstate).
:- mode paragraph(in, out, in, out) is semidet.
paragraph(pragma(Pragma), no) --> pragma(Pragma).
paragraph(zBEGIN, yes(P)) --> item(P),
	( semicolon -> c(C), add_tokens([zEND-C, zBEGIN-C]) ; {true} ).
paragraph(zAX   , yes(P)) --> axiomatic_def(P).
paragraph(zSCH  , yes(P)) --> schema_box(P).
paragraph(zGEN  , yes(P)) --> generic_def(P).

:- pred pragma(pragma, pstate, pstate).
:- mode pragma(in, in, out) is semidet.
pragma(abbrev) --> nameOrOpList(L), add_abbrev(L).
pragma(monot) --> nameOrOpList(L), add_monot(L).
pragma(syntax) --> op_type(Op), nameOrOpList(L), add_operat(Op, L).
pragma(loglib) --> nameOrOpList(L), add_loglib(L).
pragma(logicgen) --> set_generate(on).
pragma(nologicgen) --> set_generate(off).
% pragma(email) -->

:- pred nameOrOpList(list(ident), pstate, pstate).
:- mode nameOrOpList(out, in, out) is det.
nameOrOpList(L) -->
	( x(name(I)) ->
		nameOrOpList(L1), {L = [I|L1]}
	; x(op(_, I)) ->
		nameOrOpList(L1), {L = [I|L1]}
	;	{L = []}
	).

:- pred op_type(op, pstate, pstate).
:- mode op_type(out, in, out) is semidet.
op_type(Op) --> x(name(id(no, S, []))), op_type(S, Op).

:- pred op_type(string, op, pstate, pstate).
:- mode op_type(in, out, in, out) is semidet.
op_type("infun", infun(N)) -->
	x(number(NS)), {string__to_int(NS, N), 1 =< N, N =< 6}.
op_type("postfun", postfun) --> [].
op_type("inrel", inrel) --> [].
op_type("prerel", prerel) --> [].
op_type("ingen", ingen) --> [].
op_type("pregen", pregen) --> [].

:- pred semicolon(pstate, pstate).
:- mode semicolon(in, out) is semidet.
semicolon --> x(T), {T = zSEMICOLON ; T = newline}.

%%%
% B.1.2	GIVEN SET

:- pred item(par1, pstate, pstate).
:- mode item(out, in, out) is semidet.
item(I) -->
	( x(zSQBRA) -> identL1(IL), expect(zSQKET),
		{I = given(IL)}
	; ident(Id), x(zFREEEQUALS) -> branchL1(BL),
		new_ref(Ref), {I = data(Ref, Id, BL)}
	; schema_name(Ident), gen_formals(F), x(defs) ->
		{Ident = id(Op, Name, _), list__length(F, N)},
		schema_declare(Op-Name, N),
		( schema_exp(S1) ->
			{S = S1}
		;	zerror("schema expression"),
			consume_to([zSEMICOLON, newline]),
			c(C), make_sexpr(C, text([], []), S)
		),
		{I = sdef(Ident, F, S)}
	; def_lhs(D, F), x(zDEFINEEQUAL) ->
		( expression(X1) ->
			{X = X1}
		;	zerror("expression"),
			{X = number("")-0}	% arbitrary token
		),
		{I = eqeq(D, F, X)}
	; predicate(P), {I = zpred(P)}
	).

:- pred identL1(list(ident), pstate, pstate).
:- mode identL1(out, in, out) is det.
identL1(L) --> parse_list1(x(zCOMMA), ident, zerror("ident"), L).

%%%
% B.1.3	STRUCTURED SET

:- pred branchL1(list(branch), pstate, pstate).
:- mode branchL1(out, in, out) is det.
branchL1(L) --> parse_list1(x(zVBAR), branch, zerror("branch"), L).

:- pred branch(branch, pstate, pstate).
:- mode branch(out, in, out) is semidet.
branch(branch(Ref, I, M)) -->
	( x(zBRA) -> op_name(I), expect(zKET),
		expect(zFREEBRA), expression(X), expect(zFREEKET),
		{M = yes(X)}
	; ident(I),
		( x(zFREEBRA) -> expression(X), expect(zFREEKET),
			{M = yes(X)}
		;	{M = no}
		)
	),
	new_ref(Ref).

%%%
% B.1.4	GLOBAL DEFINITION

:- pred axiomatic_def(par1, pstate, pstate).
:- mode axiomatic_def(out, in, out) is det.
axiomatic_def(define([], S)) --> text(S).

%%%
% B.1.5	GENERIC DEFINITION

:- pred generic_def(par1, pstate, pstate).
:- mode generic_def(out, in, out) is det.
generic_def(define(F, S)) --> gen_formals(F), text(S).

:- pred def_lhs(ident, formals, pstate, pstate).
:- mode def_lhs(out, out, in, out) is semidet.
def_lhs(I, F) -->
	( x(op(pregen, I0)) ->
		ident(I1), {I = I0, F = [I1]}
	; ident(I1), x(op(ingen, I0)) ->
		ident(I2), {I = I0, F = [I1, I2]}
	; var_name(I), gen_formals(F)
	).

%%%
% B.1.6	SCHEMA DEFINITION

:- pred schema_box(par1, pstate, pstate).
:- mode schema_box(out, in, out) is det.
schema_box(sdef(Ident, F, S)) -->
	expect(left_brace),
	( schema_name(Ident0) -> expect(right_brace),
		{Ident = Ident0, Ident = id(Op, Name, _)},
		gen_formals(F), {list__length(F, N)},
		schema_declare(Op-Name, N),
		text(S)
	;	zerror("schema_name"),
		{Ident = id(no, "*error name*", []), F = []},
		make_sexpr(0, text([], []), S)
	).

:- pred text(sexpr, pstate, pstate).
:- mode text(out, in, out) is det.
text(S) -->
	c(C),
	decl_part(L), opt_predicate(Pred),
	make_sexpr(C, text(L, Pred), S).

%%%
% B.1.7	DECLARATION

:- pred decl_part(list(decl), pstate, pstate).
:- mode decl_part(out, in, out) is det.
decl_part(L) --> parse_list1(semicolon, decl_elem, zerror("decl_elem"), L).

:- pred decl_elem(decl, pstate, pstate).
:- mode decl_elem(out, in, out) is semidet.
decl_elem(D) -->
	% More than one name, or the colon, will distinguish a
	% basic declaration from a schema reference.
	( decl_nameL(L), {L = [_|_]}, x(zCOLON) ->	% basic_decl
		( expression(X) ->
			{D = decl(L, X)}
		;	zerror("expression"),
			c(C), {D = decl(L, number("Err")-C)}
		)
	; 	schema_exp(S), {D = include(S)}
	).

:- pred decl_nameL(list(ident), pstate, pstate).
:- mode decl_nameL(out, in, out) is det.
decl_nameL(L) --> parse_list(x(zCOMMA), decl_name, zerror("decl_name"), L).

:- pred decl_nameL1(list(ident), pstate, pstate).
:- mode decl_nameL1(out, in, out) is det.
decl_nameL1(L) --> parse_list1(x(zCOMMA), decl_name, zerror("decl_name"), L).

%%%
% B.1.8	SCHEMA TEXT

%%%
% B.1.9	SCHEMA

:- pred schema_exp(sexpr, pstate, pstate).
:- mode schema_exp(out, in, out) is semidet.
schema_exp(S) -->
	c(C),
	( quantifier(Q) -> schema_text(T), expect(zDOT), schema_exp(S0),
		make_sexpr(C, quantification(Q, T, S0), S)
	;	log_sch(S)
	).

:- pred log_sch(sexpr::out, pstate::in, pstate::out) is semidet.
log_sch(P) --> parse_left1(x(zIFF), log_sch1, s_lbpred(equivalence), P).

:- pred log_sch1(sexpr::out, pstate::in, pstate::out) is semidet.
log_sch1(P) --> parse_right1(x(zIMPLIES), log_sch2, s_lbpred(implication), P).

:- pred log_sch2(sexpr::out, pstate::in, pstate::out) is semidet.
log_sch2(P) --> parse_left1(x(zOR), log_sch3, s_lbpred(disjunction), P).

:- pred log_sch3(sexpr::out, pstate::in, pstate::out) is semidet.
log_sch3(P) --> parse_left1(x(zAND), logsch4, s_lbpred(conjunction), P).

:- pred s_lbpred(lconn, zcontext, sexpr, sexpr, sexpr, pstate, pstate).
:- mode s_lbpred(in, in, in, in, out, in, out) is det.
s_lbpred(T, C, S0, S1, S) --> make_sexpr(C, lbpred(T, S0, S1), S).

:- pred logsch4(sexpr, pstate, pstate).
:- mode logsch4(out, in, out) is semidet.
logsch4(S) -->
	( x(zNOT, C) ->		% SNegation
		logsch4(S0), make_sexpr(C, negation(S0), S)
	;	cmpndsch(S)
	).

:- pred cmpndsch(sexpr::out, pstate::in, pstate::out) is semidet.
cmpndsch(S) --> parse_left1(sconn_op, cmpndsch1, bsexpr, S).

:- pred bsexpr(pair(sconn, zcontext), sexpr, sexpr, sexpr, pstate, pstate).
:- mode bsexpr(in, in, in, out, in, out) is det.
bsexpr(SConn-C, S0, S1, S) -->
	new_ref(Ref), make_sexpr(C, bsexpr(Ref, SConn, S0, S1), S).

:- pred sconn_op(pair(sconn, zcontext), pstate, pstate).
:- mode sconn_op(out, in, out) is semidet.
sconn_op(sconn(T)-C) --> x(T, C).

:- func sconn(ztoken) = sconn.
:- mode sconn(in) = out is semidet.
sconn(zCOMPOSE) = composition.
sconn(pipe) = piping.

:- pred cmpndsch1(sexpr, pstate, pstate).
:- mode cmpndsch1(out, in, out) is semidet.
cmpndsch1(S) --> cmpndsch2(S0), cmpndsch1_tail(S0, S).

:- pred cmpndsch1_tail(sexpr, sexpr, pstate, pstate).
:- mode cmpndsch1_tail(in, out, in, out) is semidet.
cmpndsch1_tail(S0, S) -->
	( x(zSQBRA, C), renameL1(RL) ->	% SRenaming
		expect(zSQKET),
		make_sexpr(C, renaming(S0, RL), S1),
		cmpndsch1_tail(S1, S)
	; x(zHIDING, C) ->		% SHiding
		expect(zBRA), decl_nameL1(L), expect(zKET),
		new_ref(Ref), make_sexpr(C, hide(Ref, S0, L), S1),
		cmpndsch1_tail(S1, S)
	;	{S = S0}
	).

:- pred cmpndsch2(sexpr, pstate, pstate).
:- mode cmpndsch2(out, in, out) is semidet.
cmpndsch2(S) --> cmpndsch3(S0), cmpndsch2_tail(S0, S).

:- pred cmpndsch2_tail(sexpr, sexpr, pstate, pstate).
:- mode cmpndsch2_tail(in, out, in, out) is semidet.
cmpndsch2_tail(S0, S) -->
	( x(zPROJECTION, C) ->
		log_sch(S1),
		new_ref(Ref), make_sexpr(C, projection(Ref, S0, S1), S2),
		cmpndsch2_tail(S2, S)
	;	{S = S0}
	).

:- pred cmpndsch3(sexpr, pstate, pstate).
:- mode cmpndsch3(out, in, out) is semidet.
cmpndsch3(S) --> 
	c(C),
	( x(zPRESCH) ->
		cmpndsch3(S0), new_ref(Ref), make_sexpr(C, pre(Ref, S0), S)
	;	cmpndsch4(S)
	).

:- pred cmpndsch4(sexpr, pstate, pstate).
:- mode cmpndsch4(out, in, out) is semidet.
cmpndsch4(S) --> basicsch(S0), cmpndsch4_tail(S0, S).

:- pred cmpndsch4_tail(sexpr, sexpr, pstate, pstate).
:- mode cmpndsch4_tail(in, out, in, out) is semidet.
cmpndsch4_tail(S0, S) -->
	( x(decoration(D), C) ->
		make_sexpr(C, decoration(S0, D), S1),
		cmpndsch4_tail(S1, S)
	;	{S = S0}
	).

:- pred basicsch(sexpr, pstate, pstate).
:- mode basicsch(out, in, out) is semidet.
basicsch(S) -->
	c(C),
	( x(zSQBRA) ->	% SConstruction
		schema_text(S0), expect(zSQKET),
		make_sexpr(C, text([include(S0)], []), S)
		% This otherwise redundant wrapper is put around S0
		% so that we can distingush between eg.
		% \lambda x, y: \nat @ ...  and
		% \lambda [x, y: \nat] @ ...
		% where the first has two args and the second one schema arg.
	; x(zBRA) ->
		schema_exp(S), expect(zKET)
	; schema_ref(S)
	).

:- pred schema_text(sexpr, pstate, pstate).
:- mode schema_text(out, in, out) is det.
schema_text(SExpr) -->
	c(C),
	decl_part(D), ( x(zVBAR) -> predicate1(P0), {P = [P0]} ; {P = []} ),
	( {D = [include(SExpr0)], P = []} ->
		{SExpr = SExpr0}
	;	make_sexpr(C, text(D, P), SExpr)
	).

:- pred add_default_ops(spec::in, spec::out, pstate::in, pstate::out) is det.
add_default_ops(L0, L, p(SN0, IO0, S0, TL, F0, SL0, D0, R0),
	   p(SN, IO, S, TL, F, SL, [], R)) :-
	list__map(default_op, D0, LLD),
	list__condense(LLD, LD0),
	specification1(L0, L,
			p(SN0, IO0, S0, LD0, F0, SL0, [], R0),
			p(SN , IO , S , LD , F , SL , D , R )),
	( LD = [] -> true ; error("add_default_ops/4: tokens left over") ),
	(  D = [] -> true ; error("add_default_ops/4: defaults added") ).

:- pred default_op(default_defn::in, ztoken_list::out) is det.
default_op(default_defn(C, Op, S, NGen), Tokens) :-
	N = name(id(no, S, [])),
	N_p = [N, decoration([prime])],
	% N_p = name(id(no, S, [prime])),
	( NGen = 0 ->
		ArgTokens = []
	;	list__duplicate(NGen, S, Gens0),
		P = (pred(Base::in, Param::out, I::in, I+1::out) is det :-
			string__int_to_string(I, Suffix),
			string__append(Base, Suffix, Param0),
			Param = name(id(no, Param0, []))),
		list__map_foldl(P, Gens0, Gens, 1, _),
		list__append([zSQBRA|Gens], [zSQKET], ArgTokens)
	),
	( Op = delta,
		OpToken = 'Delta',
		PredTokens = []
	; Op = xi,
		OpToken = 'Xi',
		list__condense([[zST, zTHETA, N], ArgTokens, [zEQUALS,
			zTHETA | N_p], ArgTokens], PredTokens)
	),
	list__condense(
		[[zSCH, left_brace, OpToken, N, right_brace], ArgTokens,
		[N], ArgTokens, [zSEMICOLON | N_p], ArgTokens,
		PredTokens, [zEND]], Tokens0),
	list__map((pred(I::in, O::out) is det :-
		O = I-C
	), Tokens0, Tokens).

:- pred schema_ref(sexpr, pstate, pstate).
:- mode schema_ref(out, in, out) is semidet.
schema_ref(SREF) -->
	c(C),
	=(I),
	schema_name(Ident),
	{Ident = id(M, Name, _D)},	% _D is always []  --- change 
	( schema_declared(M-Name, _) ->
		{true}
	;	{M = yes(Op)},			% Must fail if M = no since
%		schema_declared(no-Name, NGen),	% Ident may not be a schema name
%		schema_define(C, Op, Name, NGen),
%		schema_declare(M-Name, NGen)
		( schema_declared(no-Name, NGen) ->
			schema_define(C, Op, Name, NGen),
			schema_declare(M-Name, NGen)
		;	zerror("undefined schema name", I)
		)
	),
	( \+ (x(zSQBRA), rename(_)) -> % actuals look a lot like renaming
		opt_gen_actuals(A)
	;	{A = no}
	),
%	opt_renaming(R),
	make_sexpr(C, ref(id(M, Name, []), A), SREF).

% The above can be improved by reporting `actuals expected' after [
% in opt_gen_actuals, and by having an opt_renaming and expected_renaming.

%%%
% RENAMING
:- pred opt_renaming(maybe(renaming), pstate, pstate).
:- mode opt_renaming(out, in, out) is det.
opt_renaming(L ) -->
	( x(zSQBRA) ->
		{L = yes(L0)}, renameL1(L0), expect(zSQKET)
	;	{L = no}
	).

:- pred renameL(renaming, pstate, pstate).
:- mode renameL(out, in, out) is det.
renameL(L) --> parse_list(x(zCOMMA), rename, zerror("rename"), L).

:- pred renameL1(renaming, pstate, pstate).
:- mode renameL1(out, in, out) is det.
renameL1(L) --> parse_list1(x(zCOMMA), rename, zerror("rename"), L).

:- pred rename(pair(ident), pstate, pstate).
:- mode rename(out, in, out) is semidet.
rename(N1-N0) --> decl_name(N0), x(zRENAME), decl_name(N1).
% rename from N1 to N0

%%%
% B.1.10 PREDICATE

:- pred opt_predicate(list(zpred), pstate, pstate).
:- mode opt_predicate(out, in, out) is det.
opt_predicate(P) --> ( x(zST) -> opt_predicate1(P) ; {P = []} ).

:- pred opt_predicate1(list(zpred), pstate, pstate).
:- mode opt_predicate1(out, in, out) is det.
opt_predicate1(L) -->
	parse_list1(x(newline), predicate, zerror("predicate"), L).

:- pred predicate1(zpred, pstate, pstate).
:- mode predicate1(out, in, out) is det.
predicate1(P) -->
	( predicate(P0) ->
		{P = P0}
	;	zerror("predicate"), {P = truth-0}
	).

:- pred predicate(zpred, pstate, pstate).
:- mode predicate(out, in, out) is semidet.
predicate(P) -->
	c(C),
	( quantifier(Q) -> schema_text(S), expect(zDOT), predicate1(P0),
		{P = quantification(Q, S, P0)-C}
	; x(zLET) -> let_defL1(L), expect(zDOT), predicate(P0),
		{P = let(L, P0)-C}
	; log_pred(P)
	).

:- pred log_pred(zpred::out, pstate::in, pstate::out) is semidet.
log_pred(P) --> c(C), parse_left(x(zIFF), log_pred1, lbpred(C, equivalence), P).

:- pred log_pred1(zpred::out, pstate::in, pstate::out) is semidet.
log_pred1(P) -->
	c(C), parse_right(x(zIMPLIES), log_pred2, lbpred(C, implication), P).

:- pred log_pred2(zpred::out, pstate::in, pstate::out) is semidet.
log_pred2(P) --> c(C), parse_left(x(zOR), log_pred3, lbpred(C, disjunction), P).

:- pred log_pred3(zpred::out, pstate::in, pstate::out) is semidet.
log_pred3(P) -->
	c(C), parse_left(x(zAND), basic_pred, lbpred(C, conjunction), P).

:- pred lbpred(zcontext, lconn, zpred, zpred, zpred).
:- mode lbpred(in, in, in, in, out) is det.
lbpred(C, T, P0, P1, lbpred(T, P0, P1)-C).

:- pred basic_pred(zpred, pstate, pstate).
:- mode basic_pred(out, in, out) is semidet.
basic_pred(P) -->
	c(C),
	( x(op(prerel, Op)) ->		% pre_rel_pred
		expression0(X),		% 0 safe?
		make_ref(C, Op, no, REF),
		{P = membership(X, REF)-C}
	; x(zTRUE) ->
		{P = truth-C}
	; x(zFALSE) ->
		{P = falsehood-C}
	; x(zNOT) ->
		basic_pred(P0), {P = negation(P0)-C}
	; x(zSQBRA) ->
		schema_text(S), expect(zSQKET),
		{P = sexpr(S)-C}
	% ; x(zPRESCH) ->			% schema_pred - ( schema )
	% 	basicsch(S0), new_ref(Ref), make_sexpr(C, pre(Ref, S0), S),
	% 	{P = sexpr(S)-C}
	; x(zBRA), predicate(P0) ->
		expect(zKET), {P = P0}	% BUG: (expr) or (pred)
	; expression0(X), rel_tail(X, M), {M = yes(_)} ->	% 0 safe?
		{M = yes(P)}
		% actually want to do this if X is not just an ident
	% ; expression(X), x(odot), predicate(P0), {P = subst(X, P0)-C}
	% ; schema_ref(R), {P = sexpr(R)-C}
	% Limited form of schema expression below (with schema_ref prefix)
	; cmpndsch(S), {P = sexpr(S)-C}		% schema_pred - ( schema )
	).

:- pred rel_tail(expr, maybe(zpred), pstate, pstate).
:- mode rel_tail(in, out, in, out) is semidet.
rel_tail(X0, P) -->
	c(C),
	( x(zEQUALS) ->
		expression01(X1),	% simpler with partial modes! % 0 safe?
		rel_tail_tail(equality(X0, X1)-C, X1, P)
	; x(zMEMBER) ->
		expression01(X1),	% 0 safe?
		rel_tail_tail(membership(X0, X1)-C, X1, P)
	; x(inrel), expect(left_brace), c(CI), ident(I), expect(right_brace) ->
		{X0 = _-C0},
		expression01(X1),	% 0 safe?
		make_ref(CI, I, no, REF),
		rel_tail_tail(membership(tuple([X0, X1])-C0, REF)-C, X1, P)
	; x(op(inrel, R)) ->
		{X0 = _-C0},
		expression01(X1),	% 0 safe?
		make_ref(C, R, no, REF),
		rel_tail_tail(membership(tuple([X0, X1])-C0, REF)-C, X1, P)
	;	{P = no}
	).

:- pred rel_tail_tail(zpred, expr, maybe(zpred), pstate, pstate).
:- mode rel_tail_tail(in, in, out, in, out) is semidet.
rel_tail_tail(P0, X, yes(P)) -->
	c(C),
	rel_tail(X, M),
	{ M = yes(P1) -> P = lbpred(conjunction, P0, P1)-C ; P = P0 }.


%%%
% LET DEFINITIONS

:- pred let_defL1(assoc_list(ident, expr), pstate, pstate).
:- mode let_defL1(out, in, out) is det.
let_defL1(L) --> parse_list1(x(zSEMICOLON), let_def, zerror("let_def"), L).

:- pred let_def(pair(ident, expr), pstate, pstate).
:- mode let_def(out, in, out) is semidet.
let_def(N-X) --> var_name(N), x(zDEFINEEQUAL), expression(X).

%%%
% B.1.11 EXPRESSION

:- pred expression01(expr, pstate, pstate).
:- mode expression01(out, in, out) is det.
expression01(X) -->
	( expression0(X0) ->
		{X = X0}
	;	zerror("expression0"), {X = number("")-0}
	).

:- pred expression0(expr, pstate, pstate).
:- mode expression0(out, in, out) is semidet.
expression0(X) -->
	( x(zIF, C) ->	% if_then_else
		predicate(P),
		expect(zTHEN), expression(X0),
		expect(zELSE), expression(X1), {X = if(P, X0, X1)-C}
	; x(zMU, C) ->	% defn_descr
		schema_text(T), opt_at_exp(X1), {X = mu(T, X1)-C}
	; x(zLET, C) ->	% let expression
		let_defL1(L), expect(zDOT), expression(X0),
		{X = let(L, X0)-C}
	;	expression(X)
	).

:- pred opt_at_exp(maybe(expr), pstate, pstate).
:- mode opt_at_exp(out, in, out) is semidet.
opt_at_exp(X) --> ( x(zDOT) -> expression(X0), {X = yes(X0)} ; {X = no} ).

:- pred expression(expr, pstate, pstate).
:- mode expression(out, in, out) is semidet.
expression(X) --> expression1(X1), expression_tail(X1, X).

:- pred expression_tail(expr, expr, pstate, pstate).
:- mode expression_tail(in, out, in, out) is det.
expression_tail(X0, X) -->
	( x(op(ingen, Op), C) ->	% in_gen_exp
%%% TESTING
%		expression(X1),
%		make_ref(C, Op, yes([X0, X1]), X)
		( expression(X1) ->
			make_ref(C, Op, yes([X0, X1]), X)
		;	zerror("expression"), {X = X0}
		)
	; 	{X = X0}
	).

:- pred expression1(expr, pstate, pstate).
:- mode expression1(out, in, out) is semidet.
expression1(X) -->
	expression2L(X0, L0),
	{ L0 = [], X = X0 ; L0 = [_|_], X0 = _-C, X = product([X0|L0])-C }.

:- pred expression2L(expr, list(expr), pstate, pstate).
:- mode expression2L(out, out, in, out) is semidet.
expression2L(X, L) -->
	expression2(X),
	( x(zCROSS) ->	% cart_product
		( expression2L(X1, L1) ->
			{L = [X1|L1]}
		;	zerror("expression"), {L = []}
		)
	;	{L = []}
	).

:- pred expression2(expr, pstate, pstate).
:- mode expression2(out, in, out) is semidet.
expression2(X) --> expression2(1, X).

:- pred expression2(word__priority, expr, pstate, pstate).
:- mode expression2(in, out, in, out) is semidet.
expression2(P, X) --> expression3(X1), expression2_tail(P, X1, X).

:- pred expression2_tail(word__priority, expr, expr, pstate, pstate).
:- mode expression2_tail(in, in, out, in, out) is det.
expression2_tail(P, X0, X) -->
%	( x(op(infun(P), Op), C) ->	% in_fun_exp
% 	% Special case---see func minus below
	( (( x(op(infun(P2), Op0), CT) ->
		{Op = Op0, C = CT, P1 = P2}
	  ;	x(minus, C), {P1 = 3, Op = id(no, "- (binary)", [])}
	  ), {P1 >= P} ) -> % in_fun_exp
% TESTING
%		expression2(P1+1, X1),
%		make_ref(C, Op, no, REF),
%		{X0 = _-C0},
%		new_ref(R), {X2 = zapply(R, REF, tuple([X0, X1])-C0)-C},
		( expression2(P1+1, X1) ->
			make_ref(C, Op, no, REF),
			{X0 = _-C0},
			new_ref(R), {X2 = zapply(R, REF, tuple([X0, X1])-C0)-C}
		;	zerror("expression"),	% Message could be more specific
			{X2 = X0}
		),
		expression2_tail(P, X2, X)
	; {X = X0}
	).

:- pred expression3(expr, pstate, pstate).
:- mode expression3(out, in, out) is semidet.
expression3(X) -->
	( x(zPSET, C) -> % power_set is treated like a prefix generic
		expression5(X0),
		{X = powerset(X0)-C}
	% special case for unary minus
	; x(minus, C) ->
		decoration(D), expression5(X0),
		make_ref(C, id(no, "- (unary)", D), no, REF),
		new_ref(R), {X = zapply(R, REF, X0)-C}
	; x(op(pregen, Op), C) ->	% pre_gen_exp
		expression5(X0),
		make_ref(C, Op, yes([X0]), X)
	; expression4(X)
	).

:- pred expression4(expr, pstate, pstate).
:- mode expression4(out, in, out) is semidet.
expression4(X) --> expression5(X0), expression4_tail(X0, X).

:- pred expression4_tail(expr, expr, pstate, pstate).
:- mode expression4_tail(in, out, in, out) is semidet.
expression4_tail(X0, X) -->
	%( expression4(X1) ->
	( expression5(X1) ->
		{X1 = _-C}, new_ref(R), expression4_tail(zapply(R, X0, X1)-C, X)
	;	{X = X0}
	).

:- pred expression5(expr, pstate, pstate).
:- mode expression5(out, in, out) is semidet.
expression5(X) --> expression5_head(X0), expression5_tail(X0, X).

:- pred expression5_head(expr, pstate, pstate).
:- mode expression5_head(out, in, out) is semidet.
expression5_head(X) -->
	% if_then_else not yet implemented
%	( c(C), schema_ref_expr(S) ->	% schema_exp
	( c(C), schema_ref(S) ->	% schema_exp
		{X = sexp(S)-C}		% schema_ref fails if name not declared
	; c(C), var_name(V) ->		% ident, gen_instance
			% can start with a zBRA so before bracketed expression
		opt_gen_actuals(A), make_ref(C, V, A, X)
	; x(zSETBRA, C) ->		% set_extn, set_comp
		( expressionL(L), x(zSETKET) ->	% order important here
			new_ref(REF),
			{X = display(REF, set, L)-C}
		;	schema_text(T), opt_at_exp(X0),
			{X = setcomp(T, X0)-C},
			expect(zSETKET)
		)
	; x(zBRA, C) ->
		expression0(X0),
		( x(zKET) ->
			{X = X0}
		; {X0 = lambda(_, _)-_ ; X0 = mu(_, _)-_ ; X0 = let(_, _)-_} ->
			=(I), zerror("`)' expected after lambda, mu or LET", I),
			{X = X0}
		; x(zCOMMA) ->		% tuple
			expressionL1(L),
			expect(zKET),
			{X = tuple([X0|L])-C}
		;	=(I), zerror("comma expected after tuple element", I),
			{X = X0}
		)
	; x(langle, C) ->
		expressionL(L), expect(rangle),
		new_ref(REF), {X = display(REF, seq, L)-C}
	; x(lbag, C) ->
		expressionL(L), expect(rbag),
		new_ref(REF), {X = display(REF, bag, L)-C}
	; x(zTHETA, C) ->
		basicsch(S), decoration(D),
		new_ref(REF), {X = theta(REF, S, D)-C}
	; x(zLAMBDA, C) ->
		schema_text(T), expect(zDOT), expression0(X0),  % is this safe?
		{X = lambda(T, X0)-C}
	; x(number(N), C) ->
		{X = number(N)-C}
	; x(string(N), C),
		{X = stringl(N)-C}
	).

:- pred expression5_tail(expr, expr, pstate, pstate).
:- mode expression5_tail(in, out, in, out) is semidet.
expression5_tail(X0, X) -->
	( x(limg, C) ->
		expression0(X1), expect(rimg), decoration(D),
		make_ref(C, id(no, "_(|_|)", D), no, REF),
		new_ref(R), {X2 = zapply(R, REF, tuple([X0,X1])-C)-C},
		expression5_tail(X2, X)
	; x(op(postfun, Op), C) ->	% post_fun_exp
		make_ref(C, Op, no, REF),
		new_ref(R), {X2 = zapply(R, REF, X0)-C},
		expression5_tail(X2, X)
	; x(caret, C) ->		% superscript
		expect(left_brace), expression(X1), expect(right_brace),
		make_ref(C, id(no, "iter", []), no, REF),
		new_ref(R), new_ref(R1),
		{X2 = zapply(R1, zapply(R, REF, X1)-C, X0)-C},
		expression5_tail(X2, X)
	; x(zSELECT, C) ->			% bind_selection
		( x(number(N)) ->
			{X2 = tupleselection(X0, N)-C}
		; var_name(VN),
			new_ref(R), {X2 = select(R, X0, VN)-C}
		),
		expression5_tail(X2, X)
	; {X = X0}
	).
%%%
% NAMES AND IDENTIFIERS

:- pred ident(ident, pstate, pstate).
:- mode ident(out, in, out) is semidet.
% ident(id(no, W, D)) --> x(word(W)), decoration(D).
ident(I) --> x(name(I)).

:- pred decl_name(ident, pstate, pstate).
:- mode decl_name(out, in, out) is semidet.
decl_name(N) --> ( op_name(N1) -> {N = N1} ; ident(N) ).

:- pred var_name(ident, pstate, pstate).
:- mode var_name(out, in, out) is semidet.
% YUK: minus needs to revert to being a special token if this is required
% (ambiguity between unary and binary operator `-' requires special handling)
var_name(N) --> ( x(zBRA) -> op_name(N), x(zKET) ; ident(N) ).
% var_name(N) --> ( x(zBRA) -> op_name(N), x(zKET) ; ident(N), {name(N) \= minus} ).

:- pred op_name(ident, pstate, pstate).
:- mode op_name(out, in, out) is semidet.
op_name(Op) -->
	( x(underscore) ->
		( x(limg), x(underscore), x(rimg) ->
			{Op = id(no, "_(|_|)", D)}, decoration(D)
		; x(minus) ->
			{Op = id(no, "- (binary)", D)}, decoration(D),
			x(underscore)
		; in_sym(Op1) ->
			{Op = Op1}, x(underscore)
		; post_sym(Op1) ->
			{Op = Op1}
		; {Op = id(no, "don't handle wrong op_names", [])}
		)
	; x(minus) ->
		{Op = id(no, "- (unary)", D)}, decoration(D)
		% {Op = id(no, "-", D)}, decoration(D)
	; pre_sym(Op), x(underscore)
	).

:- pred in_sym(ident, pstate, pstate).
:- mode in_sym(out, in, out) is semidet.
in_sym(Op) --> x(op(O, Op)), {O=infun(_) ; O=ingen ; O=inrel}.

:- pred pre_sym(ident, pstate, pstate).
:- mode pre_sym(out, in, out) is semidet.
pre_sym(Op) --> x(op(O, Op)), {O=pregen ; O=prerel}.

:- pred post_sym(ident, pstate, pstate).
:- mode post_sym(out, in, out) is semidet.
post_sym(Op) --> x(op(postfun, Op)).

%:- func minus = ztoken.
% If minus is a binary operator, any newlines before it are soft
% and therefore ignored.  This would mean that a line couldn't start
% with a unary minus, eg. a negative number.  We therefore treat minus as a
% special case.
% minus = op(prefun(3), id(no, "-", [])).
% minus = name(id(no, "-", [])).

:- pred gen_formals(formals, pstate, pstate).
:- mode gen_formals(out, in, out) is det.
gen_formals(L) --> ( x(zSQBRA) -> identL1(L), expect(zSQKET) ; {L = []} ).

:- pred opt_gen_actuals(maybe(list(expr)), pstate, pstate).
:- mode opt_gen_actuals(out, in, out) is det.
opt_gen_actuals(L) -->
	( x(zSQBRA) -> expressionL1(L0), expect(zSQKET),
		{L = yes(L0)}
	;	{L = no}
	).

:- pred decoration(decoration, pstate, pstate).
:- mode decoration(out, in, out) is det.
decoration(D) --> ( x(decoration(D0)) -> {D = D0} ; {D = []} ).
	
:- pred schema_name(ident, pstate, pstate).
:- mode schema_name(out, in, out) is semidet.
schema_name(id(M, S, [])) -->
	x(T),
	( {T = 'Delta', M = yes(delta)}, x(name(I), C)
	; {T = 'Xi', M = yes(xi)}, x(name(I), C)
	; {T = name(I), M = no}, c(C)
	),
	{I = id(M0, S, D)},
	{ M0 = no -> true ; error("schema_name/3--operation in lexer ident") },
	% Remove decoration from schema name if any
	( {D = []} ; {D = [_|_]}, add_tokens([decoration(D)-C]) ).

%%%
% Utilities

:- pred quantifier(quantifier::out, pstate::in, pstate::out) is semidet. 
quantifier(Q) --> x(T), {quantifierToken(T, Q)}.

:- pred quantifierToken(ztoken::in, quantifier::out) is semidet. 
quantifierToken(zFORALL, universal).
quantifierToken(zEXISTS, exists).
quantifierToken(zEXISTS1, unique).

:- pred consume_to(list(ztoken), pstate, pstate).
:- mode consume_to(in, in, out) is det.
consume_to(L) -->
	( x(T), {list__member(T, [zEND|L])} ->
		{true}
	; x(_) ->
		consume_to(L)
	;	zerror("paragraph end")
	).

:- pred consume_to_end(pstate, pstate).
:- mode consume_to_end(in, out) is det.
consume_to_end -->
	( x(zEND) ->
		{true}
	; x(_) ->
		consume_to_end
	;
		zerror("paragraph end")
	).

:- pred expressionL(list(expr), pstate, pstate).
:- mode expressionL(out, in, out) is det.
expressionL(L) --> parse_list(x(zCOMMA), expression, zerror("expression"), L).

:- pred expressionL1(list(expr), pstate, pstate).
:- mode expressionL1(out, in, out) is det.
expressionL1(L) --> parse_list1(x(zCOMMA), expression, zerror("expression"), L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Higher order parsing
%

:- pred parse_right1(
	pred(Z, Y, Y)::pred(out, in, out) is semidet,	% parse infix operator
	pred(X, Y, Y)::pred(out, in, out) is semidet,	% parse operand
	pred(Z, X, X, X, Y, Y)::pred(in, in, in, out, in, out) is det,
					% use Z and two X's to construct new X
	X::out, Y::in, Y::out) is semidet.
parse_right1(Infix, Operand, Construct, Tree) -->
	Operand(Left), parse_right1(Left, Infix, Operand, Construct, Tree).

% parse_right1/7 is a service routine for parse_right1/6
:- pred parse_right1(X, pred(Z, Y, Y), pred(X, Y, Y), pred(Z, X, X, X, Y, Y),
								X, Y, Y).
:- mode parse_right1(in, pred(out, in, out) is semidet,
			pred(out, in, out) is semidet,
			pred(in, in, in, out, in, out) is det,
			out, in, out) is semidet.
parse_right1(Left, Infix, Operand, Construct, Tree) -->
	( Infix(Result) ->
		Operand(Left_of_Right),
		parse_right1(Left_of_Right, Infix, Operand, Construct, Right),
		Construct(Result, Left, Right, Tree)
	;	{Tree = Left}
	).

:- pred parse_right(pred(Y, Y), pred(X, Y, Y), pred(X, X, X), X, Y, Y).
:- mode parse_right(pred(in, out) is semidet, pred(out, in, out) is semidet,
			pred(in, in, out) is det, out, in, out) is semidet.
parse_right(Infix, Operand, Construct, Tree) -->
	Operand(Left), parse_right(Left, Infix, Operand, Construct, Tree).

% parse_right/7 is a service routine for parse_right/6
:- pred parse_right(X, pred(Y, Y), pred(X, Y, Y), pred(X, X, X), X, Y, Y).
:- mode parse_right(in, pred(in, out) is semidet, pred(out, in, out) is semidet,
			pred(in, in, out) is det, out, in, out) is semidet.
parse_right(Left, Infix, Operand, Construct, Tree) -->
	( Infix ->
		Operand(Left_of_Right),
		parse_right(Left_of_Right, Infix, Operand, Construct, Right),
		{Construct(Left, Right, Tree)}
	;	{Tree = Left}
	).

:- pred parse_left1(
	pred(Z, Y, Y)::pred(out, in, out) is semidet,	% parse infix operator
	pred(X, Y, Y)::pred(out, in, out) is semidet,	% parse operand
	pred(Z, X, X, X, Y, Y)::pred(in, in, in, out, in, out) is det,
					% use Z and two X's to construct new X
	X::out, Y::in, Y::out) is semidet.
parse_left1(Infix, Operand, Construct, Tree) -->
	Operand(Left), parse_left1(Left, Infix, Operand, Construct, Tree).

% parse_left1/7 is a service routine for parse_left1/6
:- pred parse_left1(X, pred(Z, Y, Y), pred(X, Y, Y), pred(Z, X, X, X, Y, Y),
								X, Y, Y).
:- mode parse_left1(in, pred(out, in, out) is semidet,
			pred(out, in, out) is semidet,
			pred(in, in, in, out, in, out) is det,
			out, in, out) is semidet.
parse_left1(Left, Infix, Operand, Construct, Tree) -->
	( Infix(Result) ->
		Operand(Right),
		Construct(Result, Left, Right, Tree1),
		parse_left1(Tree1, Infix, Operand, Construct, Tree)
	;	{Tree = Left}
	).

:- pred parse_left(pred(Y, Y), pred(X, Y, Y), pred(X, X, X), X, Y, Y).
:- mode parse_left(pred(in, out) is semidet, pred(out, in, out) is semidet,
			pred(in, in, out) is det, out, in, out) is semidet.
parse_left(Infix, Operand, Construct, Tree) -->
	Operand(Left), parse_left(Left, Infix, Operand, Construct, Tree).

% parse_left/7 is a service routine for parse_left/6
:- pred parse_left(X, pred(Y, Y), pred(X, Y, Y), pred(X, X, X), X, Y, Y).
:- mode parse_left(in, pred(in, out) is semidet, pred(out, in, out) is semidet,
			pred(in, in, out) is det, out, in, out) is semidet.
parse_left(Left, Infix, Operand, Construct, Tree) -->
	( Infix ->
		Operand(Right),
		{Construct(Left, Right, Tree1)},
		parse_left(Tree1, Infix, Operand, Construct, Tree)
	;	{Tree = Left}
	).

:- pred parse_list(pred(Y, Y), pred(X, Y, Y), pred(Y, Y), list(X), Y, Y).
:- mode parse_list(pred(in, out) is semidet, pred(out, in, out) is semidet,
			pred(in, out) is det, out, in, out) is det.
parse_list(Separator, Element, Error, L) -->
	( Element(E) ->
		{L = [E|L1]},
		( Separator ->
			parse_list1(Separator, Element, Error, L1)
		;	{L1 = []}
		)
	;	{L = []}
	).

:- pred parse_list1(pred(Y, Y), pred(X, Y, Y), pred(Y, Y), list(X), Y, Y).
:- mode parse_list1(pred(in, out) is semidet, pred(out, in, out) is semidet,
			pred(in, out) is det, out, in, out) is det.
parse_list1(Separator, Element, Error, L) -->
	( Element(E) ->
		{L = [E|L1]},
		( Separator ->
			parse_list1(Separator, Element, Error, L1)
		;	{L1 = []}
		)
	;	{L = []},
		Error
	).
