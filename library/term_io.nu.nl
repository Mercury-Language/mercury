%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: term_io.nu.nl.
% Main author: fjh.
%
% This file provides implementations for some of the predicates declared
% in term_io.m using non-logical NU-Prolog.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- term_io__op(P, T, O, IO0, _) when P and T and ground(O) and IO0.
term_io__op(Prec, Type, OpName) -->
	{ name(Op, OpName), op(Prec, Type, Op) },
	io__update_state.

:- term_io__current_ops(_, IO0, _) when IO0.
term_io__current_ops(Ops) -->
	{ findall(op(Prec, Type, OpName),
		  (currentOp(Prec, Type, Op), name(Op, OpName)),
		  Ops)
	}. 

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- term_io__read_term(_, IO0, _) when IO0.
term_io__read_term(Result) -->
	io__input_stream(Stream),
	io__stream_name(Stream, StreamName),
	{ nuprolog ->
	    term_io__get_token_list(Tokens0, LineNumber),
	    term__context_init(StreamName, LineNumber, Context),
	    convert_tokens(Tokens0, Tokens),
	    ( treadTerm(Tokens, Term0, NameList, VarList) ->
		% DCG expansion is now done by prog_io.m.
		% expandTerm(Term0, Term1), 
		Term1 = Term0,
		( nonvar(Term1), eof(Term1) ->
			Result = eof
		;
			convert_term(Term1, NameList, VarList, Context,
					VarSet, Term),
			Result = term(VarSet, Term)
		)
	    ;
		% NU-Prolog just dumps to error message to stderr.
		% This is the best we can do:
		Result = error("NU-Prolog syntax error", LineNumber)
	    )
	;
	    lineCount(Stream, LineNumber),
	    term__context_init(StreamName, LineNumber, Context),
	    ( read_variables(Term1, NameVarList) ->
		split_var_names(NameVarList, NameList, VarList),
		( nonvar(Term1), eof(Term1) ->
			Result = eof
		;
			convert_term(Term1, NameList, VarList, Context,
					VarSet, Term),
			Result = term(VarSet, Term)
		)
	    ;
		Result = error("Prolog syntax error", LineNumber)
	    )
	},
	io__update_state.

split_var_names([], [], []).
split_var_names([Name=Var|Rest], [N|Names], [Var|Vars]) :-
	name(Name, N),
	split_var_names(Rest, Names, Vars).

	% convert "double-quoted string" tokens into
	% '$string'("double_quoted string") so that they don't get
	% confused with lists of integers.
	% NB we need to take care that we don't use $string directly,
	% so that this file is self-applicable.

magic_string_atom(Name) :-
	name(Name, "$string").

convert_tokens([], []).
convert_tokens([Tok0|Toks0], Toks) :-
	Tok0 = [Val|Type],
	( Type = atom, Val = '[]' ->
		magic_string_atom(String),
		Toks = [[String|atom], ['('|atom], [""|string], [')'|atom]
			| Toks1]
	; Type = string ->
		magic_string_atom(String),
		Toks = [[String|atom], ['('|atom], Tok0, [')'|atom] | Toks1]
	;
		Toks = [Tok0 | Toks1]
	),
	convert_tokens(Toks0, Toks1).


	% This gets a term's worth of tokens, and
	% also returns the linenumber at the start of the term.
	% (Actually we return the linenumber after the first token
	% of the term.)

term_io__get_token_list(Tokens, LineNumber) :-
	getToken(Token, Type),
	currentInput(Stream),
	lineCount(Stream, LineNumber),
	( Type = end_of_file ->
		Tokens = [[Token|Type]]
	;
		getTokenList(Tokens0),
		Tokens = [[Token|Type] | Tokens0]
	).

%-----------------------------------------------------------------------------%

	% The "varmap" ADT.
	% A varmap is a mapping from variables to (var, name).
	% This is used when converting terms to our nice ground representation.

:- type varmap == list(varmap_2).
:- type varmap_2 ---> var(prolog_free_var, maybe_name, maybe_id).
:- type maybe_name ---> name(string) ; no_name.
:- type maybe_id ---> id(var) ; no_id.
:- type prolog_free_var == any.

	% Initialize a varmap, given a list of variables and a list
	% of their corresponding names.
:- pred varmap__init(list(prolog_free_var), list(string), varmap).
varmap__init([], [], []).
varmap__init([Var|Vars], [Name|Names], [var(Var,name(Name),no_id)|Rest]) :-
	varmap__init(Vars, Names, Rest).

	% Set the id for a variable.
:- pred varmap__set_id(varmap, prolog_free_var, var, varmap).
varmap__set_id([], Var, Id, [var(Var, no_name, id(Id))]).
varmap__set_id([V0|VarMap0], Var, Id, [V|VarMap]) :-
	V0 = var(ThisVar, Name, OldId),
	( Var == ThisVar ->
		(OldId = no_id ->
			true
		;
			error("term_io.nu.nl: internal error (varmap)"),
			fail
		),
		V = var(ThisVar, Name, id(Id)),
		VarMap = VarMap0
	;
		V = V0,
		varmap__set_id(VarMap0, Var, Id, VarMap)
	).

	% Lookup the name and id of a variable.
:- pred varmap__lookup(varmap, prolog_free_var, maybe_name, maybe_id).
varmap__lookup([], _, no_name, no_id).
varmap__lookup([var(V,N,I)|Rest], Var, Name, Id) :-
	( Var == V ->
		Name = N,
		Id = I
	;
		varmap__lookup(Rest, Var, Name, Id)
	).

%-----------------------------------------------------------------------------%

	% Given a term, a list of the named variables in the term and
	% a list of their corresponding names, return a VarSet and
	% a properly structured ground representation of that term.
convert_term(Term0, NameList, VarList, Context, VarSet, Term) :-
	varset__init(VarSet0),
	varmap__init(VarList, NameList, VarMap0),
	convert_term_2(Term0, VarSet0, VarMap0, Context, Term, VarSet, _).

convert_term_2(Term0, VarSet0, VarMap0, Context, Term, VarSet, VarMap) :-
	( var(Term0) ->
		varmap__lookup(VarMap0, Term0, Name, Id),
		convert_term_3(Id, Name, Term0, VarSet0, VarMap0,
				VarId, VarSet, VarMap),
		Term = term__variable(VarId)
	; integer(Term0) ->
		Term = term__functor(term__integer(Term0), [], Context),
		VarSet = VarSet0,
		VarMap = VarMap0
	; float(Term0) ->
		Term = term__functor(term__float(Term0), [], Context),
		VarSet = VarSet0,
		VarMap = VarMap0
	; magic_string_atom(S),
	  Term0 =.. [S, String] ->
		Term = term__functor(term__string(String), [], Context),
		VarSet = VarSet0,
		VarMap = VarMap0
	; functor(Term0, F ,_) ->
		name(F, Name),
		Term = term__functor(term__atom(Name), Args, Context),
		Term0 =.. [_|Args0],
		convert_term_2_list(Args0, VarSet0, VarMap0,
					Context, Args, VarSet, VarMap)
	;
		fail
	).

	% convert a list of terms
convert_term_2_list([], VarSet, VarMap, _Context, [], VarSet, VarMap).
convert_term_2_list([X0|Xs0], VarSet0, VarMap0, Context, [X|Xs], VarSet,
		VarMap) :-
	convert_term_2(X0, VarSet0, VarMap0, Context, X, VarSet1, VarMap1),
	convert_term_2_list(Xs0, VarSet1, VarMap1, Context, Xs, VarSet, VarMap).

	% If a variable does not already have an Id, then get the
	% VarSet to allocate a new id for that variable and save
	% the id in the VarMap.
convert_term_3(id(VarId), _, _, VarSet, VarMap, VarId, VarSet, VarMap).
convert_term_3(no_id, Name, Var, VarSet0, VarMap0, VarId, VarSet, VarMap) :-
	varset__new_var(VarSet0, VarId, VarSet1),
	varmap__set_id(VarMap0, Var, VarId, VarMap),
	convert_term_4(Name, VarId, VarSet1, VarSet).

	% If a variable has a name, then notify the VarSet of that name.
convert_term_4(no_name, _, VarSet, VarSet).
convert_term_4(name(Name), VarId, VarSet0, VarSet) :-
	varset__name_var(VarSet0, VarId, Name, VarSet).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
