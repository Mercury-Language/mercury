%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% File: term.nl.
% Main author: fjh.

% This file provides a type `term' used to represent Prolog terms,
% and various predicates to manipulate terms and substitutions.

%-----------------------------------------------------------------------------%

:- module term.
:- interface.
:- import_module int, string, float, list, map.

%-----------------------------------------------------------------------------%

:- type term		--->	term__functor(const, list(term), term__context)
			;	term__variable(var).
:- type const 		--->	term__atom(string)
			;	term__integer(int)
			;	term__string(string)
			;	term__float(float).
:- type comparison	--->	(>)
			;	(<)
			;	(=).

:- type var.
:- type var_supply.
:- type term__context.

%-----------------------------------------------------------------------------%

:- pred term__vars(term, list(var)).
:- mode term__vars(in, out) is det.
%	term__vars(Term, Vars)
%		Vars is the list of variables contained in Term, in the order 
%		obtained by traversing the term depth first, left-to-right.

:- pred term__vars_2(term, list(var), list(var)).
:- mode term__vars_2(in, in, out) is det.
%		As above, but with an accumulator.

:- pred term__vars_list(list(term), list(var)).
:- mode term__vars_list(in, out) is det.
%	term__vars_list(TermList, Vars)
%		Vars is the list of variables contained in TermList, in the
%		order obtained by traversing the list of terms depth-first,
%		left-to-right.

:- pred term__contains_var(term, var).
:- mode term__contains_var(in, in) is semidet.
:- mode term__contains_var(in, out) is nondet.
%	term__contains_var(Term, Var)
%		True if Term contains Var. (On backtracking returns all the 
%		variables contained in Term.)

:- pred term__contains_var_list(list(term), var).
:- mode term__contains_var_list(in, in) is semidet.
:- mode term__contains_var_list(in, out) is nondet.
%	term__contains_var_list(TermList, Var)
%		True if TermList contains Var. (On backtracking returns all the 
%		variables contained in Term.)

:- type substitution == map(var, term).

:- pred term__unify(term, term, substitution, substitution).
:- mode term__unify(in, in, in, out) is semidet.
%	term__unify(Term1, Term2, Bindings0, Bindings)
%		unify (with occur check) two terms with respect to a set
%	 	of bindings and possibly update the set of bindings

:- pred term__substitute(term, var, term, term).
:- mode term__substitute(in, in, in, out) is det.
%	term__substitute(Term0, Var, Replacement, Term) :
%		replace all occurrences of Var in Term0 with Replacement,
%		and return the result in Term.

:- pred term__substitute_list(list(term), var, term, list(term)).
:- mode term__substitute_list(in, in, in, out) is det.
%		as above, except for a list of terms rather than a single term

:- pred term__substitute_corresponding(list(var), list(term), term, term).
:- mode term__substitute_corresponding(in, in, in, out) is det.
%       term__substitute_corresponding(Vars, Repls, Term0, Term).
%		replace all occurrences of variables in Vars with
%		the corresponding term in Repls, and return the result in Term.
%		If Vars contains duplicates, or if Vars is not the same
%	        length as Repls, the behaviour is undefined and probably
%		harmful.

:- pred term__substitute_corresponding_list(list(var), list(term), list(term),
						list(term)).
:- mode term__substitute_corresponding_list(in, in, in, out) is det.
%       term__substitute_corresponding_list(Vars, Repls, TermList0, TermList).
%		As above, except applies to a list of terms rather than a
%		single term.

:- pred term__apply_rec_substitution(term, substitution, term).
:- mode term__apply_rec_substitution(in, in, out) is det.
%	term__apply_rec_substitution(Term0, Substitution, Term) :
%		recursively apply substitution to Term0 until
%		no more substitions can be applied, and then
%		return the result in Term.

:- pred term__apply_rec_substitution_to_list(list(term), substitution,
						list(term)).
:- mode term__apply_rec_substitution_to_list(in, in, out) is det.

:- pred term__apply_substitution(term, substitution, term).
:- mode term__apply_substitution(in, in, out) is det.
%	term__apply_substitution(Term0, Substitution, Term) :
%		apply substitution to Term0 and return the result in Term.

:- pred term__apply_substitution_to_list(list(term), substitution, list(term)).
:- mode term__apply_substitution_to_list(in, in, out) is det.
%	term__apply_substitution_to_list(TermList0, Substitution, TermList) :
%		as above, except for a list of terms rather than a single term


:- pred term__occurs(term, var, substitution).
:- mode term__occurs(in, in, in) is semidet.
%	term__occurs(Term0, Var, Substitution) :
%		true iff Var occurs in the term resulting after
%		applying Substitution to Term0.

:- pred term__occurs_list(list(term), var, substitution).
:- mode term__occurs_list(in, in, in) is semidet.
%		as above, except for a list of terms rather than a single term

:- pred term__relabel_variable(term, var, var, term).
:- mode term__relabel_variable(in, in, in, out) is det.
%	term__relabel_variable(Term0, OldVar, NewVar, Term) :
%		replace all occurences of OldVar in Term0 with
%		NewVar and put the result in Term.

:- pred term__relabel_variables(list(term), var, var, list(term)).
:- mode term__relabel_variables(in, in, in, out) is det.
%	term__relabel_variables(Terms0, OldVar, NewVar, Terms) :
%		same as term__relabel_variable but for a list of terms.


:- pred term__is_ground(term, substitution).
:- mode term__is_ground(in, in) is semidet.
%	term__is_ground(Term, Bindings) is true iff no variables contained
%		in Term are non-ground in Bindings.

:- pred term__compare(comparison, term, term, substitution).
:- mode term__compare(out, in, in, in) is semidet.
%	term__compare(Comparison, Term1, Term2, Bindings) is true iff
%		there is a binding of Comparison to <, =, or > such
%		that the binding holds for the two ground terms Term1
%		and Term2 with respect to the bindings in Bindings.
%		Fails if Term1 or Term2 is not ground (with respect to
%		the bindings in Bindings).

%-----------------------------------------------------------------------------%

	% To manage a supply of variables, use the following 2 predicates.
	% (We might want to give these a unique mode later.)

:- pred term__init_var_supply(var_supply).
:- mode term__init_var_supply(out) is det.
:- mode term__init_var_supply(in) is semidet. % implied
%	term__init_var_supply(VarSupply) :
%		returns a fresh var_supply for producing fresh variables.

:- pred term__create_var(var_supply, var, var_supply).
:- mode term__create_var(in, out, out) is det.
%	term__create_var(VarSupply0, Variable, VarSupply) :
%		create a fresh variable (var) and return the
%		updated var_supply.

:- pred term__var_to_int(var, int).
:- mode term__var_to_int(in, out) is det.
%		Convert a variable to an int.
%		Different variables map to different ints.
%		Other than that, the mapping is unspecified.
	
%-----------------------------------------------------------------------------%

	% Given a term context, return the source line number.

:- pred term__context_line(term__context, int).
:- mode term__context_line(in, out) is det.

	% Given a term context, return the source file.

:- pred term__context_file(term__context, string).
:- mode term__context_file(in, out) is det.

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.

:- pred term__context_init(int, term__context).
:- mode term__context_init(in, out) is det.

:- pred term__context_init(string, int, term__context).
:- mode term__context_init(in, in, out) is det.

	% Convert a list of terms which are all vars into a list
	% of vars.  Abort (call error/1) if the list contains
	% any non-variables.

:- pred term__term_list_to_var_list(list(term), list(var)).
:- mode term__term_list_to_var_list(in, out) is det.

	% Convert a list of terms which are all vars into a list
	% of vars (or vice versa).

:- pred term__var_list_to_term_list(list(var), list(term)).
:- mode term__var_list_to_term_list(in, out) is det.
:- mode term__var_list_to_term_list(out, in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.
:- import_module std_util, require.
:- import_module random.

%-----------------------------------------------------------------------------%

:- type var		==	int.
:- type var_supply	==	int.

%-----------------------------------------------------------------------------%

	% term__vars(Term, Vars) is true if Vars is the list of variables
	% contained in Term obtained by depth-first left-to-right traversal
	% in that order.

term__vars(Term, Vars) :-
	term__vars_2(Term, [], Vars).

term__vars_list(Terms, Vars) :-
	term__vars_2_list(Terms, [], Vars).

term__vars_2(term__variable(V), Vs, [V|Vs]).
term__vars_2(term__functor(_,Args,_), Vs0, Vs) :-
	term__vars_2_list(Args, Vs0, Vs).

:- pred term__vars_2_list(list(term), list(var), list(var)).
:- mode term__vars_2_list(in, in, out) is det.

term__vars_2_list([], Vs, Vs).
term__vars_2_list([T|Ts], Vs0, Vs) :-
	term__vars_2_list(Ts, Vs0, Vs1),
	term__vars_2(T, Vs1, Vs).

%-----------------------------------------------------------------------------%

	% term__contains_var(Term, Var) is true if Var occurs in Term.

term__contains_var(term__variable(V), V).
term__contains_var(term__functor(_, Args, _), V) :-
	term__contains_var_list(Args, V).

term__contains_var_list([T|_], V) :-
	term__contains_var(T, V).
term__contains_var_list([_|Ts], V) :-
	term__contains_var_list(Ts, V).

%-----------------------------------------------------------------------------%

	% term__contains_functor(Term, Functor, Args):
	%	term__functor(Functor, Args, _) is a subterm of Term.
	%
	% CURRENTLY NOT USED.

:- pred term__contains_functor(term, const, list(term)).
:- mode term__contains_functor(in, in, in) is semidet.
:- mode term__contains_functor(in, out, out) is nondet.

term__contains_functor(term__functor(Functor, Args, _), Functor, Args).
term__contains_functor(term__functor(_, Args, _), SubFunctor, SubArgs) :-
 	list__member(SubTerm, Args),
 	term__contains_functor(SubTerm, SubFunctor, SubArgs).

%-----------------------------------------------------------------------------%

	% term__subterm(Term, SubTerm):
	%	SubTerm is a subterm of Term.
	%
	% CURRENTLY NOT USED.

:- pred term__subterm(term, term).
:- mode term__subterm(in, in) is semidet.
:- mode term__subterm(in, out) is nondet.

term__subterm(Term, Term).
term__subterm(term__functor(_, Args, _), SubTerm) :-
	list__member(Term, Args),
	term__subterm(Term, SubTerm).

%-----------------------------------------------------------------------------%

	% Access predicates for the term__context data structure.
	% At the moment, the only context we store is the line
	% number.

:- type term__context	--->	term__context(string, int).
				% file, line number.

	% Given a term context, return the source line number.

term__context_line(term__context(_, LineNumber), LineNumber).

	% Given a term context, return the source file.

term__context_file(term__context(FileName, _), FileName).

	% Used to initialize the term context when reading in
	% (or otherwise constructing) a term.
	% term__context_init/2 is for old code; use
	% term__context_init/3 if possible.
	% XXX should this be /2?

term__context_init(LineNumber, term__context("", LineNumber)).

term__context_init(File, LineNumber, term__context(File, LineNumber)).

%-----------------------------------------------------------------------------%

	% Unify two terms (with occurs check), updating the bindings of
	% the variables in the terms.  

:- term__unify(X, Y, _, _) when X and Y.		% NU-Prolog indexing

term__unify(term__variable(X), term__variable(Y), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		( %%% if some [BindingOfY]
			map__search(Bindings0, Y, BindingOfY)
		->
			% both X and Y already have bindings - just
			% unify the terms they are bound to
			term__unify(BindingOfX, BindingOfY, Bindings0, Bindings)
		;
			% Y is a variable which hasn't been bound yet
			term__apply_rec_substitution(BindingOfX, Bindings0,
				SubstBindingOfX),
			( SubstBindingOfX = term__variable(Y) ->
			 	Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfX, Y, Bindings0),
				map__set(Bindings0, Y, SubstBindingOfX,
					Bindings)
			)
		)
	;
		( %%% if some [BindingOfY2]
			map__search(Bindings0, Y, BindingOfY2)
		->
			% X is a variable which hasn't been bound yet
			term__apply_rec_substitution(BindingOfY2, Bindings0,
				SubstBindingOfY2),
			( SubstBindingOfY2 = term__variable(X) ->
				Bindings = Bindings0
			;
				\+ term__occurs(SubstBindingOfY2, X, Bindings0),
				map__set(Bindings0, X, SubstBindingOfY2,
					Bindings)
			)
		;
			% both X and Y are unbound variables -
			% bind one to the other
			( X = Y ->
				Bindings = Bindings0
			;
				map__set(Bindings0, X, term__variable(Y),
					Bindings)
			)
		)
	).

term__unify(term__variable(X), term__functor(F, As, C), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		term__unify(BindingOfX, term__functor(F, As, C), Bindings0,
			Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term__functor(F, As, C), Bindings)
	).

term__unify(term__functor(F, As, C), term__variable(X), Bindings0, Bindings) :-
	( %%% if some [BindingOfX]
		map__search(Bindings0, X, BindingOfX)
	->
		term__unify(term__functor(F, As, C), BindingOfX, Bindings0,
			Bindings)
	;
		\+ term__occurs_list(As, X, Bindings0),
		map__set(Bindings0, X, term__functor(F, As, C), Bindings)
	).

term__unify(term__functor(F, AsX, _), term__functor(F, AsY, _)) -->
	term__unify_list(AsX, AsY).

:- pred term__unify_list(list(term), list(term), substitution, substitution).
:- mode term__unify_list(in, in, in, out) is semidet.

term__unify_list([], []) --> [].
term__unify_list([X | Xs], [Y | Ys]) -->
	term__unify(X, Y),
	term__unify_list(Xs, Ys).

%-----------------------------------------------------------------------------%

	% term__occurs(Term, Var, Subst) succeeds if Term contains Var,
	% perhaps indirectly via the substitution.  (The variable must
	% not be mapped by the substitution.)

term__occurs(term__variable(X), Y, Bindings) :-
	(
		X = Y
	->
		true
	;
		map__search(Bindings, X, BindingOfX),
		term__occurs(BindingOfX, Y, Bindings)
	).
term__occurs(term__functor(_F, As, _), Y, Bindings) :-
	term__occurs_list(As, Y, Bindings).

term__occurs_list([Term | Terms], Y, Bindings) :-
	(
		term__occurs(Term, Y, Bindings)
	->
		true
	;
		term__occurs_list(Terms, Y, Bindings)
	).

%-----------------------------------------------------------------------------%

	% term__substitute(Term0, Var, Replacement, Term) :
	%	replace all occurrences of Var in Term0 with Replacement,
	%	and return the result in Term.

term__substitute(term__variable(Var), SearchVar, Replacement, Term) :-
	(
		Var = SearchVar
	->
		Term = Replacement
	;
		Term = term__variable(Var)
	).
term__substitute(term__functor(Name, Args0, Context), Var, Replacement,
		 term__functor(Name, Args, Context)) :-
	term__substitute_list(Args0, Var, Replacement, Args).

term__substitute_list([], _Var, _Replacement, []).
term__substitute_list([Term0 | Terms0], Var, Replacement, [Term | Terms]) :-
	term__substitute(Term0, Var, Replacement, Term),
	term__substitute_list(Terms0, Var, Replacement, Terms).

term__substitute_corresponding(Ss, Rs, Term0, Term) :-
	map__init(Subst0),
	( term__substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
		term__apply_substitution(Term0, Subst, Term)
	;
		error("term__substitute_corresponding: different length lists")
	).

term__substitute_corresponding_list(Ss, Rs, TermList0, TermList) :-
	map__init(Subst0),
	( term__substitute_corresponding_2(Ss, Rs, Subst0, Subst) ->
		term__apply_substitution_to_list(TermList0, Subst, TermList)
	;
		error(
		  "term__substitute_corresponding_list: different length lists"
		)
	).

:- pred term__substitute_corresponding_2(list(var), list(term),
					substitution, substitution).
:- mode term__substitute_corresponding_2(in, in, in, out) is semidet.

term__substitute_corresponding_2([], [], Subst, Subst).
term__substitute_corresponding_2([S | Ss], [R | Rs], Subst0, Subst) :-
	map__set(Subst0, S, R, Subst1),
	term__substitute_corresponding_2(Ss, Rs, Subst1, Subst).

%-----------------------------------------------------------------------------%

term__apply_rec_substitution(term__variable(Var), Substitution, Term) :-
	(
		%some [Replacement]
		map__search(Substitution, Var, Replacement)
	->
		% recursively apply the substition to the replacement
		term__apply_rec_substitution(Replacement, Substitution, Term)
	;
		Term = term__variable(Var)
	).
term__apply_rec_substitution(term__functor(Name, Args0, Context), Substitution,
		 term__functor(Name, Args, Context)) :-
	term__apply_rec_substitution_to_list(Args0, Substitution, Args).

term__apply_rec_substitution_to_list([], _Substitution, []).
term__apply_rec_substitution_to_list([Term0 | Terms0], Substitution,
		[Term | Terms]) :-
	term__apply_rec_substitution(Term0, Substitution, Term),
	term__apply_rec_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

term__apply_substitution(term__variable(Var), Substitution, Term) :-
	(
		%some [Replacement]
		map__search(Substitution, Var, Replacement)
	->
		Term = Replacement
	;
		Term = term__variable(Var)
	).
term__apply_substitution(term__functor(Name, Args0, Context), Substitution,
		 term__functor(Name, Args, Context)) :-
	term__apply_substitution_to_list(Args0, Substitution, Args).

term__apply_substitution_to_list([], _Substitution, []).
term__apply_substitution_to_list([Term0 | Terms0], Substitution,
		[Term | Terms]) :-
	term__apply_substitution(Term0, Substitution, Term),
	term__apply_substitution_to_list(Terms0, Substitution, Terms).

%-----------------------------------------------------------------------------%

	% create a new supply of variables
term__init_var_supply(0).

	% We number variables using bit-reversed sequential numbers,
	% to ensure that our trees remain perfectly balanced.
	% Hopefully the overhead of bit-reversal isn't too high.

term__create_var(VarSupply0, Var, VarSupply) :-
	VarSupply is VarSupply0 + 1,
	bit_reverse(VarSupply, Var).

	% Reverse the (lower 32) bits of an integer.

:- pred bit_reverse(int::in, int::out) is det.
bit_reverse(X, Y) :-
	right_shift_octet(X, Bits0, X1),
	right_shift_octet(X1, Bits1, X2),
	right_shift_octet(X2, Bits2, X3),
	right_shift_octet(X3, Bits3, X4),
	bit_rev_octet(Bits0, RevBits0),
	bit_rev_octet(Bits1, RevBits1),
	bit_rev_octet(Bits2, RevBits2),
	bit_rev_octet(Bits3, RevBits3),
	left_shift_octet(X4, RevBits0, Y3),
	left_shift_octet(Y3, RevBits1, Y2),
	left_shift_octet(Y2, RevBits2, Y1),
	left_shift_octet(Y1, RevBits3, Y).

	% Right-shift an integer by one octet (byte), and return the
	% octet which was shifted out and the resulting right-shifted integer.

:- pred right_shift_octet(int::in, int::out, int::out) is det.
right_shift_octet(X0, Octet, X) :-
	Octet is X0 /\ 255,
	X is X0 >> 8.

	% Left-shift an integer by one octet (byte) and insert the
	% specified value in the low-order octet.

:- pred left_shift_octet(int::in, int::in, int::out) is det.
left_shift_octet(X0, Octet, X) :-
	X1 is X0 << 8,
	X is X1 \/ Octet.

	% The following predicate computes the bit-reversal of an octet (byte)
	% using a big lookup table.

:- pred bit_rev_octet(int::in, int::out) is det.
bit_rev_octet(X, Y) :-
	( bit_rev(X, Y0) ->
		Y = Y0
	;
		error("bit_rev failed - argument out of range")
	).

:- pred bit_rev(int::in, int::out) is semidet.
bit_rev(0, 0).
bit_rev(1, 128).
bit_rev(2, 64).
bit_rev(3, 192).
bit_rev(4, 32).
bit_rev(5, 160).
bit_rev(6, 96).
bit_rev(7, 224).
bit_rev(8, 16).
bit_rev(9, 144).
bit_rev(10, 80).
bit_rev(11, 208).
bit_rev(12, 48).
bit_rev(13, 176).
bit_rev(14, 112).
bit_rev(15, 240).
bit_rev(16, 8).
bit_rev(17, 136).
bit_rev(18, 72).
bit_rev(19, 200).
bit_rev(20, 40).
bit_rev(21, 168).
bit_rev(22, 104).
bit_rev(23, 232).
bit_rev(24, 24).
bit_rev(25, 152).
bit_rev(26, 88).
bit_rev(27, 216).
bit_rev(28, 56).
bit_rev(29, 184).
bit_rev(30, 120).
bit_rev(31, 248).
bit_rev(32, 4).
bit_rev(33, 132).
bit_rev(34, 68).
bit_rev(35, 196).
bit_rev(36, 36).
bit_rev(37, 164).
bit_rev(38, 100).
bit_rev(39, 228).
bit_rev(40, 20).
bit_rev(41, 148).
bit_rev(42, 84).
bit_rev(43, 212).
bit_rev(44, 52).
bit_rev(45, 180).
bit_rev(46, 116).
bit_rev(47, 244).
bit_rev(48, 12).
bit_rev(49, 140).
bit_rev(50, 76).
bit_rev(51, 204).
bit_rev(52, 44).
bit_rev(53, 172).
bit_rev(54, 108).
bit_rev(55, 236).
bit_rev(56, 28).
bit_rev(57, 156).
bit_rev(58, 92).
bit_rev(59, 220).
bit_rev(60, 60).
bit_rev(61, 188).
bit_rev(62, 124).
bit_rev(63, 252).
bit_rev(64, 2).
bit_rev(65, 130).
bit_rev(66, 66).
bit_rev(67, 194).
bit_rev(68, 34).
bit_rev(69, 162).
bit_rev(70, 98).
bit_rev(71, 226).
bit_rev(72, 18).
bit_rev(73, 146).
bit_rev(74, 82).
bit_rev(75, 210).
bit_rev(76, 50).
bit_rev(77, 178).
bit_rev(78, 114).
bit_rev(79, 242).
bit_rev(80, 10).
bit_rev(81, 138).
bit_rev(82, 74).
bit_rev(83, 202).
bit_rev(84, 42).
bit_rev(85, 170).
bit_rev(86, 106).
bit_rev(87, 234).
bit_rev(88, 26).
bit_rev(89, 154).
bit_rev(90, 90).
bit_rev(91, 218).
bit_rev(92, 58).
bit_rev(93, 186).
bit_rev(94, 122).
bit_rev(95, 250).
bit_rev(96, 6).
bit_rev(97, 134).
bit_rev(98, 70).
bit_rev(99, 198).
bit_rev(100, 38).
bit_rev(101, 166).
bit_rev(102, 102).
bit_rev(103, 230).
bit_rev(104, 22).
bit_rev(105, 150).
bit_rev(106, 86).
bit_rev(107, 214).
bit_rev(108, 54).
bit_rev(109, 182).
bit_rev(110, 118).
bit_rev(111, 246).
bit_rev(112, 14).
bit_rev(113, 142).
bit_rev(114, 78).
bit_rev(115, 206).
bit_rev(116, 46).
bit_rev(117, 174).
bit_rev(118, 110).
bit_rev(119, 238).
bit_rev(120, 30).
bit_rev(121, 158).
bit_rev(122, 94).
bit_rev(123, 222).
bit_rev(124, 62).
bit_rev(125, 190).
bit_rev(126, 126).
bit_rev(127, 254).
bit_rev(128, 1).
bit_rev(129, 129).
bit_rev(130, 65).
bit_rev(131, 193).
bit_rev(132, 33).
bit_rev(133, 161).
bit_rev(134, 97).
bit_rev(135, 225).
bit_rev(136, 17).
bit_rev(137, 145).
bit_rev(138, 81).
bit_rev(139, 209).
bit_rev(140, 49).
bit_rev(141, 177).
bit_rev(142, 113).
bit_rev(143, 241).
bit_rev(144, 9).
bit_rev(145, 137).
bit_rev(146, 73).
bit_rev(147, 201).
bit_rev(148, 41).
bit_rev(149, 169).
bit_rev(150, 105).
bit_rev(151, 233).
bit_rev(152, 25).
bit_rev(153, 153).
bit_rev(154, 89).
bit_rev(155, 217).
bit_rev(156, 57).
bit_rev(157, 185).
bit_rev(158, 121).
bit_rev(159, 249).
bit_rev(160, 5).
bit_rev(161, 133).
bit_rev(162, 69).
bit_rev(163, 197).
bit_rev(164, 37).
bit_rev(165, 165).
bit_rev(166, 101).
bit_rev(167, 229).
bit_rev(168, 21).
bit_rev(169, 149).
bit_rev(170, 85).
bit_rev(171, 213).
bit_rev(172, 53).
bit_rev(173, 181).
bit_rev(174, 117).
bit_rev(175, 245).
bit_rev(176, 13).
bit_rev(177, 141).
bit_rev(178, 77).
bit_rev(179, 205).
bit_rev(180, 45).
bit_rev(181, 173).
bit_rev(182, 109).
bit_rev(183, 237).
bit_rev(184, 29).
bit_rev(185, 157).
bit_rev(186, 93).
bit_rev(187, 221).
bit_rev(188, 61).
bit_rev(189, 189).
bit_rev(190, 125).
bit_rev(191, 253).
bit_rev(192, 3).
bit_rev(193, 131).
bit_rev(194, 67).
bit_rev(195, 195).
bit_rev(196, 35).
bit_rev(197, 163).
bit_rev(198, 99).
bit_rev(199, 227).
bit_rev(200, 19).
bit_rev(201, 147).
bit_rev(202, 83).
bit_rev(203, 211).
bit_rev(204, 51).
bit_rev(205, 179).
bit_rev(206, 115).
bit_rev(207, 243).
bit_rev(208, 11).
bit_rev(209, 139).
bit_rev(210, 75).
bit_rev(211, 203).
bit_rev(212, 43).
bit_rev(213, 171).
bit_rev(214, 107).
bit_rev(215, 235).
bit_rev(216, 27).
bit_rev(217, 155).
bit_rev(218, 91).
bit_rev(219, 219).
bit_rev(220, 59).
bit_rev(221, 187).
bit_rev(222, 123).
bit_rev(223, 251).
bit_rev(224, 7).
bit_rev(225, 135).
bit_rev(226, 71).
bit_rev(227, 199).
bit_rev(228, 39).
bit_rev(229, 167).
bit_rev(230, 103).
bit_rev(231, 231).
bit_rev(232, 23).
bit_rev(233, 151).
bit_rev(234, 87).
bit_rev(235, 215).
bit_rev(236, 55).
bit_rev(237, 183).
bit_rev(238, 119).
bit_rev(239, 247).
bit_rev(240, 15).
bit_rev(241, 143).
bit_rev(242, 79).
bit_rev(243, 207).
bit_rev(244, 47).
bit_rev(245, 175).
bit_rev(246, 111).
bit_rev(247, 239).
bit_rev(248, 31).
bit_rev(249, 159).
bit_rev(250, 95).
bit_rev(251, 223).
bit_rev(252, 63).
bit_rev(253, 191).
bit_rev(254, 127).
bit_rev(255, 255).

%-----------------------------------------------------------------------------%

	% To convert a variable to an int, we want to undo the bit-reversal.

term__var_to_int(Var0, Var) :-
	bit_reverse(Var0, Var).

%-----------------------------------------------------------------------------%

	% substitute a variable name in a term.
term__relabel_variable(term__functor(Const, Terms0, Cont), OldVar, NewVar,
				term__functor(Const, Terms, Cont)) :-
	term__relabel_variables(Terms0, OldVar, NewVar, Terms).
term__relabel_variable(term__variable(Var0), OldVar, NewVar,
				term__variable(Var)) :-
	(
		Var0 = OldVar
	->
		Var = NewVar
	;
		Var = Var0
	).

term__relabel_variables([], _, _, []).
term__relabel_variables([Term0|Terms0], OldVar, NewVar, [Term|Terms]):-
	term__relabel_variable(Term0, OldVar, NewVar, Term),
	term__relabel_variables(Terms0, OldVar, NewVar, Terms).

%-----------------------------------------------------------------------------%

:- term__term_list_to_var_list(Terms, Vars) when Terms or Vars. % Indexing

term__term_list_to_var_list(Terms, Vars) :-
	( term__var_list_to_term_list(Vars0, Terms) ->
		Vars = Vars0
	;
		error("term__term_list_to_var_list")
	).

:- term__var_list_to_term_list(Terms, Vars) when Terms or Vars. % Indexing

term__var_list_to_term_list([], []).
term__var_list_to_term_list([Var | Vars], [term__variable(Var) | Terms]) :-
	term__var_list_to_term_list(Vars, Terms).

%-----------------------------------------------------------------------------%

term__is_ground(term__variable(V), Bindings) :-
	map__search(Bindings, V, Binding),
	term__is_ground(Binding, Bindings).
term__is_ground(term__functor(_, Args, _), Bindings) :-
	term__is_ground_2(Args, Bindings).

:- pred term__is_ground_2(list(term), substitution).
:- mode term__is_ground_2(in, in) is semidet.

term__is_ground_2([], _Bindings).
term__is_ground_2([Term|Terms], Bindings) :-
	term__is_ground(Term, Bindings),
	term__is_ground_2(Terms, Bindings).
	
%-----------------------------------------------------------------------------%

term__compare(Cmp, Term1, Term2, Bindings) :-
	term__apply_rec_substitution(Term1, Bindings, TermA),
	term__is_ground(TermA, Bindings),
	term__apply_rec_substitution(Term2, Bindings, TermB),
	term__is_ground(TermB, Bindings),
	compare(Cmp0, TermA, TermB),
	(
		Cmp0 = (=)
	->
		Cmp = (=)
	;
		Cmp0 = (<)
	->
		Cmp = (<)
	;
		Cmp = (>)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
