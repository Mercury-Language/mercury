%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% File: term_io.nl.
% Main author: fjh.
%
% This file encapsulates all the term I/O.
% This exports predicates to read and write terms in the
% nice ground representation provided in term.nl.
% Some of the predicates here are actually implemented in non-logical
% NU-Prolog in term_io.nu.nl.
%
% This library is still pretty yucko because it's based on
% the old dec-10 Prolog I/O (see, seeing, seen, tell, telling, told)
% instead of the stream-based I/O. 
% TODO: fix this.
%
% XXX:  The prefixes in this module should be changed from io__ to term_io__.
%
%-----------------------------------------------------------------------------%

:- module term_io.
:- interface.
:- import_module io, int, float, string, list, varset, term, char.

% External interface: imported predicate

:- type op_type ---> fx; fy; xf; yf; xfx; xfy; yfx; fxx; fxy; fyx; fyy.
:- pred io__op(int, op_type, string, io__state, io__state).
:- mode io__op(in, in, in, di, uo).
%	io__op(Prec, Type, OpName, IOState0, IOState1).
%		Define an operator as per Prolog op/3 for future calls to
%		io__read_term.

:- type op_details ---> op(int, op_type, string).
:- pred io__current_ops(list(op_details), io__state, io__state).
:- mode io__current_ops(out, di, uo).
%		Return a list containing all the current operator definitions.
%		Does not modify the io__state.

:- type read_term ---> eof ; error(string, int) ; term(varset, term).
:- pred io__read_term(read_term, io__state, io__state).
:- mode io__read_term(out, di, uo).

%	io__read_term(Result, IO0, IO1).
%		Read a term from standard input. Similar to NU-Prolog
%		read_term/2, except that resulting term is in the ground
%		representation. Binds Result to either `eof',
%		`term(VarSet, Term)', or `error(Message, LineNumber)'.

:- pred io__write_term(varset, term, io__state, io__state).
:- mode io__write_term(in, in, di, uo).
%		Writes a term to standard output.

:- pred io__write_term_nl(varset, term, io__state, io__state).
:- mode io__write_term_nl(in, in, di, uo).
%		As above, except it appends a period and new-line.

:- pred io__write_constant(const, io__state, io__state).
:- mode io__write_constant(in, di, uo).
%		Writes a constant (integer, float, or atom) to stdout.

:- pred io__write_variable(var, varset, io__state, io__state).
:- mode io__write_variable(in, in, di, uo).
%		Writes a variable to stdout.

:- pred mercury_quote_string(string, io__state, io__state).
:- mode mercury_quote_string(in, di, uo).
	% Given a string S, write S in double-quotes, with characters
	% escaped if necessary, to stdout.

:- pred mercury_quote_atom(string, io__state, io__state).
:- mode mercury_quote_atom(in, di, uo).
	% Given an atom-name A, write A, enclosed in single-quotes if necessary,
	% with characters escaped if necessary, to stdout.

:- pred mercury_quote_char(character, io__state, io__state).
:- mode mercury_quote_char(in, di, uo).
	% Given a character C, write C in single-quotes,
	% escaped if necessary, to stdout.

:- pred mercury_quote_single_char(character, io__state, io__state).
:- mode mercury_quote_single_char(in, di, uo).
	% Given a character C, write C, escaped if necessary, to stdout.
	% The character is not enclosed in quotes.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, require.

/*
:- external("NU-Prolog", io__op/4).
:- external("NU-Prolog", io__current_ops/3).
:- external("NU-Prolog", io__read_term/3).
:- external("NU-Prolog", io__write_term/4).
:- external("NU-Prolog", io__write_constant/3).
*/

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

	% write a variable to standard output.
	% use the variable names specified by varset and write _N
	% for all unnamed variables with N starting at 0.

io__write_variable(Variable, VarSet) -->
	io__write_variable_2(Variable, VarSet, 0, _, _).

:- pred io__write_variable_2(var, varset, int, varset, int,
				io__state, io__state).
:- mode io__write_variable_2(in, in, in, out, out, di, uo).

io__write_variable_2(Id, VarSet0, N0, VarSet, N) -->
	(
		{ varset__lookup_var(VarSet0, Id, Val) }
	->
		io__write_term_2(Val, VarSet0, N0, VarSet, N)
	;
		{ varset__lookup_name(VarSet0, Id, Name) }
	->
		{ N = N0 },
		{ VarSet = VarSet0 },
		io__write_string(Name)
	;
		% XXX problems with name clashes

		{ string__int_to_string(N0, Num) },
		{ string__append("_", Num, VarName) },
		{ varset__name_var(VarSet0, Id, VarName, VarSet) },
		{ N is N0 + 1 },
		io__write_string(VarName)
	).

%-----------------------------------------------------------------------------%

	% write a term to standard output.
	% use the variable names specified by varset and write _N
	% for all unnamed variables with N starting at 0.

io__write_term(VarSet, Term) -->
	io__write_term_2(Term, VarSet, 0, _, _).

:- pred io__write_term_2(term, varset, int, varset, int, io__state, io__state).
:- mode io__write_term_2(in, in, in, out, out, di, uo).

io__write_term_2(term__variable(Id), VarSet0, N0, VarSet, N) -->
	io__write_variable_2(Id, VarSet0, N0, VarSet, N).
io__write_term_2(term__functor(Functor, Args, _), VarSet0, N0, VarSet, N) -->
	(
		{ Functor = term__atom(".") },
		{ Args = [ListHead, ListTail] }
	->
		io__write_char('['),
		io__write_term_2(ListHead, VarSet0, N0, VarSet1, N1),
		io__write_list_tail(ListTail, VarSet1, N1, VarSet, N),
		io__write_char(']')
	;
		{ Functor = term__atom("{}") },
		{ Args = [BracedTerm] }
	->
		io__write_string("{ "),
		io__write_term_2(BracedTerm, VarSet0, N0, VarSet, N),
		io__write_string(" }")
	;
		{ Args = [PrefixArg] },
		io__unary_prefix_op(Functor, Result),
		{ Result = yes }
	->
		io__write_char('('),
		io__write_constant(Functor),
		io__write_char(' '),
		io__write_term_2(PrefixArg, VarSet0, N0, VarSet, N),
		io__write_char(')')
	;
		{ Args = [PostfixArg] },
		io__unary_postfix_op(Functor, Result),
		{ Result = yes }
	->
		io__write_char('('),
		io__write_term_2(PostfixArg, VarSet0, N0, VarSet, N),
		io__write_char(' '),
		io__write_constant(Functor),
		io__write_char(')')
	;
		{ Args = [Arg1, Arg2] },
		io__infix_op(Functor, Result),
		{ Result = yes }
	->
		io__write_char('('),
		io__write_term_2(Arg1, VarSet0, N0, VarSet1, N1),
		io__write_char(' '),
		io__write_constant(Functor),
		io__write_char(' '),
		io__write_term_2(Arg2, VarSet1, N1, VarSet, N),
		io__write_char(')')
	;
		io__write_constant(Functor),
		(
			{ Args = [X|Xs] }
		->
			io__write_char('('),
			io__write_term_2(X, VarSet0, N0, VarSet1, N1),
			io__write_term_args(Xs, VarSet1, N1, VarSet, N),
			io__write_char(')')
		;
			{ N = N0,
			  VarSet = VarSet0 }
		)
	).

:- pred io__write_list_tail(term, varset, int, varset, int,
				io__state, io__state).
:- mode io__write_list_tail(in, in, in, out, out, di, uo).

io__write_list_tail(Term, VarSet0, N0, VarSet, N) -->
	( 
		{ Term = term__variable(Id) },
		{ varset__lookup_var(VarSet0, Id, Val) }
	->
		io__write_list_tail(Val, VarSet0, N0, VarSet, N)
	;
		{ Term = term__functor(term__atom("."), [ListHead, ListTail], _) }
	->
		io__write_string(", "),
		io__write_term_2(ListHead, VarSet0, N0, VarSet1, N1),
		io__write_list_tail(ListTail, VarSet1, N1, VarSet, N)
	;
		{ Term = term__functor(term__atom("[]"), [], _) }
	->
		{ VarSet = VarSet0 },
		{ N = N0 }
	;
		io__write_string(" | "),
		io__write_term_2(Term, VarSet0, N0, VarSet, N)
	).

:- pred io__infix_op(const, bool, io__state, io__state).
:- mode io__infix_op(in, out, di, uo).

:- pred io__unary_prefix_op(const, bool, io__state, io__state).
:- mode io__unary_prefix_op(in, out, di, uo).

:- pred io__unary_postfix_op(const, bool, io__state, io__state).
:- mode io__unary_postfix_op(in, out, di, uo).

/*
:- external("NU-Prolog", io__infix_op/3).
:- external("NU-Prolog", io__unary_prefix_op/3).
:- external("NU-Prolog", io__unary_postfix_op/3).
*/

%-----------------------------------------------------------------------------%

:- pred io__write_term_args(list(term), varset, int, varset, int,
				io__state, io__state).
:- mode io__write_term_args(in, in, in, out, out, di, uo).

	% write the remaining arguments
io__write_term_args([], VarSet, N, VarSet, N) --> [].
io__write_term_args([X|Xs], VarSet0, N0, VarSet, N) -->
	io__write_string(", "),
	io__write_term_2(X, VarSet0, N0, VarSet1, N1),
	io__write_term_args(Xs, VarSet1, N1, VarSet, N).

%-----------------------------------------------------------------------------%

	% write the functor
io__write_constant(term__integer(I)) -->
	io__write_int(I).
io__write_constant(term__float(F)) -->
	io__write_float(F).
io__write_constant(term__atom(A))  -->
	mercury_quote_atom(A).
io__write_constant(term__string(S)) -->
	io__write_char('"'),
	mercury_quote_string(S),
	io__write_char('"').

%-----------------------------------------------------------------------------%

mercury_quote_char(C) -->
	io__write_char('\''),
	mercury_quote_single_char(C),
	io__write_char('\'').

mercury_quote_atom(S) -->
	( { string__is_alnum_or_underscore(S) } ->
		io__write_string(S)
	;
		io__write_char('\''),
		mercury_quote_string(S),
		io__write_char('\'')
	).

mercury_quote_string(S0) -->
	( { string__first_char(S0, Char, S1) } ->
		mercury_quote_single_char(Char),
		mercury_quote_string(S1)
	;
		[]
	).

mercury_quote_single_char(Char) -->
	( { mercury_quote_special_char(Char, QuoteChar) } ->
		io__write_char('\\'),
		io__write_char(QuoteChar)
	; { is_mercury_source_char(Char) } ->
		io__write_char(Char)
	;
		{ mercury__escape_char(Char, String) },
		io__write_string(String)
	).

:- pred mercury__escape_char(character, string).
:- mode mercury__escape_char(in, out) is det.

	% Convert a character to the corresponding octal escape code.

mercury__escape_char(Char, EscapeCode) :-
	char_to_int(Char, Int),
	string__int_to_base_string(Int, 8, OctalString0),
	string__pad_left(OctalString0, '0', 3, OctalString),
	string__first_char(EscapeCode, '\\', OctalString).

:- pred is_mercury_source_char(character).
:- mode is_mercury_source_char(in) is semidet.

	% Succeed if Char is a character which is allowed in
	% Mercury string and character literals.

is_mercury_source_char(Char) :-
	( is_alnum(Char) ->
		true
	; is_mercury_punctuation_char(Char) ->
		true
	;
		fail
	).

	% Currently we only allow the following characters.
	% XXX should we just use is_printable(Char) instead?

:- pred is_mercury_punctuation_char(character).
:- mode is_mercury_punctuation_char(in) is semidet.

is_mercury_punctuation_char(' ').
is_mercury_punctuation_char('!').
is_mercury_punctuation_char('@').
is_mercury_punctuation_char('#').
is_mercury_punctuation_char('$').
is_mercury_punctuation_char('%').
is_mercury_punctuation_char('^').
is_mercury_punctuation_char('&').
is_mercury_punctuation_char('*').
is_mercury_punctuation_char('(').
is_mercury_punctuation_char(')').
is_mercury_punctuation_char('-').
is_mercury_punctuation_char('_').
is_mercury_punctuation_char('+').
is_mercury_punctuation_char('=').
is_mercury_punctuation_char('`').
is_mercury_punctuation_char('~').
is_mercury_punctuation_char('{').
is_mercury_punctuation_char('}').
is_mercury_punctuation_char('[').
is_mercury_punctuation_char(']').
is_mercury_punctuation_char(';').
is_mercury_punctuation_char(':').
is_mercury_punctuation_char('\'').
is_mercury_punctuation_char('"').
is_mercury_punctuation_char('"').
is_mercury_punctuation_char(',').
is_mercury_punctuation_char('<').
is_mercury_punctuation_char('>').
is_mercury_punctuation_char('.').
is_mercury_punctuation_char(',').
is_mercury_punctuation_char('/').
is_mercury_punctuation_char('?').
is_mercury_punctuation_char('\\').
is_mercury_punctuation_char('|').

%-----------------------------------------------------------------------------%

:- pred mercury_quote_special_char(character, character).
:- mode mercury_quote_special_char(in, out).

mercury_quote_special_char('\'', '\'').
mercury_quote_special_char('\"', '"').
mercury_quote_special_char('\\', '\\').
mercury_quote_special_char('\n', 'n').
mercury_quote_special_char('\t', 't').
mercury_quote_special_char('\b', 'b').

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

io__write_term_nl(VarSet, Term) -->
	io__write_term(VarSet, Term),
	io__write_string(".\n").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
