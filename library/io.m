%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% This file encapsulates all the file I/O.
% We implement a purely logical I/O system using Prolog's horrible
% non-logical I/O primitives.
% This includes predicates to read and write terms in the
% nice ground representation provided in ground.nl.
%
% This library is still pretty yucko because it's based on
% the old dec-10 Prolog I/O (see, seeing, seen, tell, telling, told)
% instead of the stream-based I/O. 
% TODO: fix this.

:- module io.
:- import_module integer, float, string, list, varset, term, require.
:- interface.

% External interface: imported predicate

% :- pred main_predicate(list(string), io__state, io__state).
%	main_predicate(ArgStrings, IOState0, IOState1).
%		This module provides startup code which calls main_predicate/3.

% External interface: exported types

% :- type io__state.

% External interface: exported predicates

:- pred io__progname(string).
%	io__progname(Progname).
%		Returns the name that the program was invoked with.

:- pred io__write_string(string, io__state, io__state).
%	io__write_string(String, IO0, IO1).
%		Writes a string to standard output.

:- pred io__write_int(integer, io__state, io__state).
%	io__write_int(Int, IO0, IO1).
%		Writes an integer to standard output.

:- type res ---> ok ; error.
:- pred io__see(string, res, io__state, io__state).
%	io__see(File, Result, IO0, IO1).
%		As per Prolog see/1. Result is either 'ok' or 'error'.

:- pred io__seen(io__state, io__state).
%	io__seen(IO0, IO1).
%		As per Prolog seen/0.

:- pred io__tell(string, res, io__state, io__state).
%	io__tell(File, Result, IO0, IO1).
%		As per Prolog tell/1. Result is either 'ok' or 'error'.

:- pred io__told(io__state, io__state).
%	io__told(IO0, IO1).
%		As per Prolog told/0.

:- type op_type ---> fx; fy; xf; yf; xfx; xfy; yfx; fxx; fxy; fxx; fyy.
:- pred io__op(integer, op_type, string, io__state, io__state).
%	io__op(Prec, Type, OpName, IOState0, IOState1).
%		Define an operator as per Prolog op/3 for future calls to
%		io__read_term.

:- type read_term ---> eof ; error(string) ; term(varset, term).
:- pred io__read_term(read_term, io__state, io__state).

%	io__read_term(Result, IO0, IO1).
%		Read a term from standard input. Similar to NU-Prolog
%		read_term/2, except that resulting term is in the ground
%		representation. Binds Result to either 'eof' or
%		'term(VarSet, Term)'.

:- pred io__write_term(varset, term, io__state, io__state).
%	io__write_term(VarSet, Term, IO0, IO1).
%		Writes a term to standard output.

:- pred io__write_term_nl(varset, term, io__state, io__state).
%	io__write_term_nl(VarSet, Term, IO0, IO1).
%		As above, except it appends a period and new-line.

:- pred io__write_constant(const, io__state, io__state).
%	io__write_constant(Const, IO0, IO1).
%		Writes a constant (integer, float, or atom) to stdout.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- dynamic(main_predicate/3).	% needed so that spy will work.
:- spy(main_predicate/3).	% for debugging

:- type io__state ---> io__state(io__state_2).
:- type io__state_2 ---> old ; current.

%-----------------------------------------------------------------------------%

:- pred main(list(atom)).
main(Args) :-
	run(Args).

:- pred run(list(atom)).
run(Args) :-
	atoms_to_strings(Args,ArgStrings),
	save_progname(ArgStrings),
	io__call(main_predicate(ArgStrings)).

:- pred io__call(pred).
io__call(Goal) :-
	io__init_state(IOState0),
	solutions([], ( call(Goal, IOState0, IOState1),
			io__final_state(IOState1) ), Solutions),
	io__call_2(Solutions).

:- pred io__call_2(list(_)).
io__call_2(Solutions) :-
	(if Solutions = [] then
		write('io.nl: error: top-level goal failed.\n')
	else if Solutions = [_] then
		true
	else
		write('io.nl: error: top-level goal not deterministic.\n')
	).

:- pred atoms_to_strings(list(atom), list(string)).
atoms_to_strings([],[]).
atoms_to_strings(A.As,S.Ss) :-
	name(A,S),
	atoms_to_strings(As,Ss).

% ?- dynamic(io__progname/1).
% save_progname(Progname._) :-
% 	assert(io__progname(Progname)).

	% !! this is wrong, but is necessary to avoid bugs in nit.
save_progname(_).
io__progname("typecheck").
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- io__op(P, T, O, IO0, _) when P and T and ground(O) and IO0.
io__op(Prec, Type, OpName) -->
	{ name(Op, OpName), op(Prec, Type, Op) },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's see/1 and seen/0.

:- io__see(File, _, IO0, _) when ground(File) and IO0.
io__see(File, Result) -->
	{
		name(FileName, File),
		(if see(FileName) then
			Result = ok
		else
			Result = error
		)
	},
	io__update_state.

:- io__seen(IO0, _) when IO0.
io__seen -->
	{ seen },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% Declarative versions of Prolog's tell/1 and told/0.

:- io__tell(File, _, IO0, _) when ground(File) and IO0.
io__tell(File, Result) -->
	{
		name(FileName, File),
		(if tell(FileName) then
			Result = ok
		else
			Result = error
		)
	},
	io__update_state.

:- io__told(IO0, _) when IO0.
io__told -->
	{ told },
	io__update_state.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- io__read_term(_, IO0, _) when IO0.
io__read_term(Result) -->
	{
	    getTokenList(Tokens0),
	    convert_tokens(Tokens0, Tokens),
	    ( treadTerm(Tokens, Term0, NameList, VarList) ->
		expandTerm(Term0, Term1),
		( nonvar(Term1), eof(Term1) ->
			Result = eof
		;
			convert_term(Term1, NameList, VarList, VarSet, Term),
			Result = term(VarSet, Term)
		)
	    ;
		% NU-Prolog just dumps to error message to stderr.
		% This is the best we can do:
		Result = error("syntax error")
	    )
	},
	io__update_state.

	% convert "double-quoted string" tokens into
	% '$string'("double_quoted string") so that they don't get
	% confused with lists of integers.

convert_tokens([], []).
convert_tokens(Tok0.Toks0, Toks) :-
	Tok0 = _Val.Type,
	(Type = string ->
		Toks = ['$string'.atom, '('.atom, Tok0, ')'.atom | Toks1]
	;
		Toks = [Tok0 | Toks1]
	),
	convert_tokens(Toks0, Toks1).

%-----------------------------------------------------------------------------%

% XXX nested module

:- module varmap.
:- export_pred varmap__init, varmap__set_id, varmap__lookup.
:- export_type maybe_name, maybe_id.

	% The "varmap" ADT.
	% A varmap is a mapping from variables to (var_id, name).
	% This is used when converting terms to our nice ground representation.

:- type varmap == list(varmap_2).
:- type varmap_2 ---> var(var, maybe_name, maybe_id).
:- type maybe_name ---> name(string) ; no_name.
:- type maybe_id ---> id(var_id) ; no_id.
:- type var == any.

	% Initialize a varmap, given a list of variables and a list
	% of their corresponding names.
:- pred varmap__init(list(var), list(string), varmap).
varmap__init([], [], []).
varmap__init(Var.Vars, Name.Names, var(Var,name(Name),no_id).Rest) :-
	varmap__init(Vars, Names, Rest).

	% Set the id for a variable.
:- pred varmap__set_id(varmap, var, var_id, varmap).
varmap__set_id([], Var, Id, [var(Var, no_name, id(Id))]).
varmap__set_id(V0.VarMap0, Var, Id, V.VarMap) :-
	V0 = var(ThisVar, Name, OldId),
	( Var == ThisVar ->
		require(OldId = no_id, "io.nl: internal error (varmap)"),
		V = var(ThisVar, Name, id(Id)),
		VarMap = VarMap0
	;
		V = V0,
		varmap__set_id(VarMap0, Var, Id, VarMap)
	).

	% Lookup the name and id of a variable.
:- pred varmap__lookup(varmap, var, maybe_name, maybe_id).
varmap__lookup([], _, no_name, no_id).
varmap__lookup(var(V,N,I).Rest, Var, Name, Id) :-
	( Var == V ->
		Name = N,
		Id = I
	;
		varmap__lookup(Rest, Var, Name, Id)
	).
:- end_module varmap.

%-----------------------------------------------------------------------------%

	% Given a term, a list of the named variables in the term and
	% a list of their corresponding names, return a VarSet and
	% a properly structured ground representation of that term.
convert_term(Term0, NameList, VarList, VarSet, Term) :-
	varset__init(VarSet0),
	varmap__init(VarList, NameList, VarMap0),
	convert_term_2(Term0, VarSet0, VarMap0, Term, VarSet, _).

convert_term_2(Term0, VarSet0, VarMap0, Term, VarSet, VarMap) :-
	( var(Term0) ->
		varmap__lookup(VarMap0, Term0, Name, Id),
		convert_term_3(Id, Name, Term0, VarSet0, VarMap0,
				VarId, VarSet, VarMap),
		Term = term_variable(VarId)
	; integer(Term0) ->
		Term = term_functor(term_integer(Term0),[]),
		VarSet = VarSet0,
		VarMap = VarMap0
	; float(Term0) ->
		Term = term_functor(term_float(Term0),[]),
		VarSet = VarSet0,
		VarMap = VarMap0
	; Term0 = '$string'(String) ->
		Term = term_functor(term_string(String),[]),
		VarSet = VarSet0,
		VarMap = VarMap0
	; functor(Term0, F ,_) ->
		name(F, Name),
		Term = term_functor(term_atom(Name),Args),
		Term0 =.. [_|Args0],
		convert_term_2_list(Args0, VarSet0, VarMap0,
					Args, VarSet, VarMap)
	;
		fail
	).

	% convert a list of terms
convert_term_2_list([], VarSet, VarMap, [], VarSet, VarMap).
convert_term_2_list(X0.Xs0, VarSet0, VarMap0, X.Xs, VarSet, VarMap) :-
	convert_term_2(X0, VarSet0, VarMap0, X, VarSet1, VarMap1),
	convert_term_2_list(Xs0, VarSet1, VarMap1, Xs, VarSet, VarMap).

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

	% write a term to standard output.
	% use the variable names specified by varset and write _N
	% for all unnamed variables with N starting at 0.

:- io__write_term(V, T, IO0, _) when ground(V) and ground(T) and IO0.
io__write_term(VarSet, Term) -->
	{ io__write_term_2(Term, VarSet, 0, _, _) },
	io__update_state.

io__write_term_2(term_variable(Id), VarSet0, N0, VarSet, N) :-
	(if some [Name] varset__lookup_name(VarSet0, Id, Name) then
		N = N0,
		VarSet = VarSet0,
		write_string(Name)
	else
		intToString(N0, Num),
		append("_", Num, VarName),
		varset__name_var(VarSet0, Id, VarName, VarSet),
		N is N0 + 1,
		write_string(VarName)
	).
io__write_term_2(term_functor(Functor,Args), VarSet0, N0, VarSet, N) :-
	(if some [PrefixArg] (
		Args = [PrefixArg],
		io__unary_prefix_op(Functor)
	    )
	then
		write('('),
		io__write_constant_2(Functor),
		write(' '),
		io__write_term_2(PrefixArg, VarSet0, N0, VarSet, N),
		write(')')
	else
	if some [PostfixArg] (
		Args = [PostfixArg],
		io__unary_postfix_op(Functor)
	    )
	then
		write('('),
		io__write_term_2(PostfixArg, VarSet0, N0, VarSet, N),
		write(' '),
		io__write_constant_2(Functor),
		write(')')
	else
	if some [Arg1, Arg2] (
		Args = [Arg1, Arg2],
		io__infix_op(Functor)
	    )
	then
		write('('),
		io__write_term_2(Arg1, VarSet0, N0, VarSet1, N1),
		write(' '),
		io__write_constant_2(Functor),
		write(' '),
		io__write_term_2(Arg2, VarSet1, N1, VarSet, N),
		write(')')
	else
		io__write_constant_2(Functor),
		(if some [X,Xs] Args = X.Xs then
			write('('),
			io__write_term_2(X, VarSet0, N0, VarSet1, N1),
			io__write_term_args(Xs, VarSet1, N1, VarSet, N),
			write(')')
		else
			N = N0,
			VarSet = VarSet0
		)
	).

io__current_op(Type, Prec, term_atom(OpName)) :-
	name(Op, OpName),
	currentOp(Type, Prec, Op).

io__infix_op(Op) :-
	some [Type, Prec] (
		io__current_op(Prec, Type, Op),
		member(Type, [xfx, xfy, yfx])
	).

io__unary_prefix_op(Op) :-
	some [Type, Prec] (
		io__current_op(Prec, Type, Op),
		member(Type, [fx, fy])
	).

io__unary_postfix_op(Op) :-
	some [Type, Prec] (
		io__current_op(Prec, Type, Op),
		member(Type, [xf, yf])
	).

	% write the remaining arguments
io__write_term_args([], VarSet, N, VarSet, N).
io__write_term_args(X.Xs, VarSet0, N0, VarSet, N) :-
	write(', '),
	io__write_term_2(X, VarSet0, N0, VarSet1, N1),
	io__write_term_args(Xs, VarSet1, N1, VarSet, N).

	% write the functor
io__write_constant(Const) -->
	io__update_state,
	{ io__write_constant_2(Const) }.

	% write the functor
io__write_constant_2(term_integer(I))   :- write(I).
io__write_constant_2(term_float(F)) :- write(F).
io__write_constant_2(term_atom(A))  :- write_string(A).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- io__write_int(I, IO0, _) when I and IO0.
io__write_int(I) -->
	{ write(I) },
	io__update_state.

%-----------------------------------------------------------------------------%

:- io__write_string(S, IO0, _) when ground(S) and IO0.
io__write_string(S) -->
	{ write_string(S) },
	io__update_state.
	
write_string(S) :-
	format("~s",[S]).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__write_term_nl(VarSet, Term) -->
	io__write_term(VarSet, Term),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% These predicates are used to enforce correct usage
	% of io__states. io__update_state uses destructive assignment
	% to ensure that once an io state has been used it can't be
	% used again.

:- pred io__init_state(io__state).
io__init_state(io__state(current)).

:- pred io__update_state(io__state, io__state).
io__update_state(IOState0, IOState) :-
	require(IOState0 = io__state(current),
		"io.nl: cannot retry I/O operation"),
	$replacn(1, IOState0, old),
	IOState = io__state(current).

:- pred io__final_state(io__state).
io__final_state(IOState) :-
	io__update_state(IOState, _).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
