%-----------------------------------------------------------------------------%
% Copyright (C) 1993-1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: portray.nl
% Main authors: fjh, pets.
%
% This file defines portray/1 (and thereby defines print/1) to print terms in
% a controlled way, so you don't wind up having to wade through several
% screenfuls of stuff to find the bit you're interested in.  Note that query
% results and debugging state are printed out using print/1.  You can control
% the depth limit, and separately the limit on the lengths of lists and the
% number of arguments shown for high-arity terms.  You can also "turn off"
% certain arguments of certain terms, based on the functor of the term, or
% even turn off the whole term.  Finally, you can define your own print
% methods for terms if you like, so lists of character codes can be printed as
% double-quoted strings, and maps can be printed out as associations.  Both of
% these are provided as examples.
% 
% This file also contains a definition for spyHook/2 that displays the term in
% full, so that you can get the full details by typing "|" in the debugger.
% Note that you need to compile it to a .no file - loading it as a .nl file
% doesn't work.
%
%-----------------------------------------------------------------------------%

:- dynamic print_depth/1.
:- dynamic print_length/1.
:- dynamic hidden/3.


%  Make the standard Prolog print/1 call my print_term/1.
portray(Term) :-
	!,
	print_term(Term).


%  print_depth(-Depth)
%  The current print depth limit is Depth.  If the depth limit is less than 0,
%  the print depth is unlimited.

print_depth(2).


%  set_print_depth(+Depth)
%  Set the print depth limit to Depth.  If the depth limit is less than 0,
%  the print depth is unlimited.

set_print_depth(Depth) :-
	retractall(print_depth(_)),
	assert(print_depth(Depth)).


%  print_length(-Length)
%  The current print length limit is Length.  The length limit controls the
%  display of long lists and terms with many arguments.  If the length limit
%  is less than 0, the print length is unlimited.

print_length(10).


%  set_print_length(+Length)
%  Set the print length limit to Length.  If the length limit is less than 0,
%  the print length is unlimited.

set_print_length(New) :-
	retractall(print_length(_)),
	assert(print_length(New)).


%  hide(+Spec)
%  hide(+Spec, +Args)
%  This predicate allows you to elide certain arguments of certain terms, or
%  whole terms, based on the functor.  Spec specifies a functor; it must be
%  either a Name/Arity, or just a functor name, in which case all arities are
%  specified.  Args specifies which arguments of such terms should be hidden.
%  Hidden terms are printed as just a hash (#) character.  Args, if given,
%  must be a list of integers specifying argument numbers to hide;
%  alternatively, it may be a single argument number to specify a single
%  argument to hide, or it may be the atom 'term', in which case only the
%  name and arity of the term are written within angle brackets, (e.g.,
%  <foo/3>).  If Args is not specified, it defaults to 'term'.

hide(Spec, Arg) :-
	Predspec = "hide/2",
	term_spec(Spec, Name, Arity, Predspec),
	argument_spec(Arg, Args, Predspec),
	hide_term_args(Name, Arity, Args).

hide(Spec) :-
	term_spec(Spec, Name, Arity, "hide/1"),
	hide_term_args(Name, Arity, term).

	
hide_term_args(Name, Arity, Args) :-
	(   nonvar(Arity) ->
		% handle case of specific arity
		(   clause(hidden(Name,Arity0,OldHidden),_),
		    nonvar(Arity0),
		    Arity0 == Arity ->
			% there was a specific entry for this arity
			retract((hidden(Name, Arity0, _):-_))
		;
		    clause(hidden(Name,Arity0,OldHidden), _),
		    var(Arity0) ->
			% there was a catch-all case
			true
		;
			% there was no case for this arity at all
			OldHidden = []
		),
		add_to_hidden_list(Args, OldHidden, NewHidden),
		asserta((hidden(Name, Arity, NewHidden) :- !))
	;   Args == term ->
		% simple case:  wants to hide all args for all arities
		retractall(hidden(Name, _, _)),
		assertz((hidden(Name, Arity, Args) :- !))
	;
		% the hard case:  add Args to set of args to hide for all
		% arities.  First we backtrack over all entries for any arity
		% replacing them with entries for the same arities, but with
		% the new arguments added....
		retract((hidden(Name, Arity0, OldHidden):-_)),
		add_to_hidden_list(Args, OldHidden, NewHidden),
		assertz((hidden(Name, Arity0, NewHidden) :- !)),
		fail
	;
	    % ... and then if there was a catch-all, we're done ...
	    clause(hidden(Name,Arity0,_), _),
	    var(Arity0) ->
		true
	;
		% ... if not, we add a new catch-all.
		sort(Args, SortedArgs),
		assertz((hidden(Name, _, SortedArgs) :- !))
	).


add_to_hidden_list(term, _, term) :-
	!.
add_to_hidden_list(Hidden1, Hidden2, Hidden) :-
	(   Hidden2 == term ->
		Hidden = term
	;
		append(Hidden1, Hidden2, Hidden3),
		sort(Hidden3, Hidden)
	).
	


%  show(+Spec, +Args)
%  show(+Spec)
%  show
%  Undo the effect of hiding argumments of some terms.  If Spec is given, stop
%  hiding the indicated arguments of the specified terms.  Spec is exactly as
%  for hide/2.  Args specifies which arguments to stop hiding; it must be a
%  list of argument numbers, or a single argument number, or the atom 'term',
%  indicating that no arguments should be hidden.  Args defaults to 'term'.
%  If Spec is not given, no arguments of any term will be hidden.
%
%  Note that these predicates will not affect the depth limit of printing.
%  Even if you ask to show a term in full, if it appears at the depth limit of
%  the term being printed, its arguments will not be shown.  If it appears
%  below the depth limit, you won't see it at all.

show(Spec, Args0) :-
	Predspec = "show/2",
	term_spec(Spec, Name, Arity, Predspec),
	argument_spec(Args0, Args, Predspec),
	show_term_args(Name, Arity, Args).

show(Spec) :-
	term_spec(Spec, Name, Arity, "show/1"),
	show_term_args(Name, Arity, term).

show :-
	retractall(hidden(_,_,_)).


show_term_args(Name, Arity, Args) :-
	(   var(Arity),
	    Args == term ->
		% simple case:  show all args of all arities of functor Name
		retractall(hidden(Name,_,_))
	;
		% just fix up all entries for functor Name
		retract((hidden(Name,Arity1,OldHidden):-_)),
		(   ( var(Arity) ; Arity == Arity1 ) ->
			remove_from_hidden_list(Args, Arity1, OldHidden,
						NewHidden)
		;
			NewHidden = OldHidden
		),
		assertz((hidden(Name, Arity1, NewHidden) :- !)),
		fail
	;
	    % After that, there's one nasty case:  we're saying to show
	    % arguments of a term with no specific entry, but with a
	    % catch-all clause.  In this case we must create a new clause.
	    nonvar(Arity),
	    clause(hidden(Name,Arity1,_), _),
	    Arity == Arity1 ->
		% There is a specific clause:  we're ok
		true
	;   
	    nonvar(Arity),
	    clause(hidden(Name,Arity1,CatchallHidden), _),
	    var(Arity1) ->
		% No specific, but there is a catch-all:  add a new clause
		remove_from_hidden_list(Args, Arity,
					CatchallHidden, NewHidden),
		asserta((hidden(Name, Arity, NewHidden) :- !))
	;
		% Either var(Arity) or no catch-all:  nothing more to do
		true
	).


remove_from_hidden_list(term, _, _, []) :-
	!.
remove_from_hidden_list(Shown, Arity, Hidden0, Hidden) :-
	(   Hidden0 \== term ->
		Hidden1 = Hidden0
	;   integer(Arity) ->
		list_between(1, Arity, Hidden1)
	;
		list_between(1, 255, Hidden1)
	),
	sort(Shown, Shown1),
	sorted_list_difference(Hidden1, Shown1, Hidden).



%  hidden(-Spec, -Args)
%  Spec is a Name/Arity term, and Args is a list of the argument numbers of
%  terms with that Name and Arity which are hidden, or the atom 'term' if the
%  entire term is hidden.  Arity may be the atom 'all', indicating that the
%  specified arguments are hidden for all terms with that Name and any arity.

hidden(Spec, Args) :-
	Spec = Name/Arity,
	clause(hidden(Name,Arity,Args), _),
	(   var(Arity) ->
		Arity = (all)
	;
		true
	).


term_spec(Spec, Name, Arity, Predspec) :-
	(   nonvar(Spec),
	    Spec = Name/Arity0,
	    atom(Name) ->
		(   integer(Arity0) ->
			Arity = Arity0
		;   Arity0 = (all) ->
			true			% leave Arity unbound
		;
			pred_error(Predspec, "invalid predicate specifier")
		)
	;   atom(Spec) ->
		Name = Spec			% leave Arity unbound
	;
		pred_error(Predspec, "invalid predicate specifier")
	).


argument_spec(Args0, Args, Predspec) :-
	(   integer(Args0) ->
		Args = [Args0]
	;   Args0 == term ->
		Args = Args0
	;   Args0 = [_|_] ->
		(   member(Oops, Args0),
		    \+ integer(Oops) ->
			pred_error(
			error(Predspec,
			      "non-integer in argument specification list"))
		;
			Args = Args0
		)
	;
		pred_error(Predspec,
			   "arg 2 must be an integer or list of integers")
	).


pred_error(Predspec, Msg) :-
	string__append(": ", Msg, Back1),
	string__append(Predspec, Back1, Back2),
	string__append("in call to ", Back2, Errmsg),
	error(Errmsg).


sorted_list_difference([], _, []).
sorted_list_difference([A|As], Bs, Cs) :-
	sorted_list_difference_1(Bs, A, As, Cs).

sorted_list_difference_1([], A, As, [A|As]).
sorted_list_difference_1([B|Bs], A, As, Cs) :-
	compare(Comparison, A, B),
	sorted_list_difference_2(Comparison, A, As, B, Bs, Cs).

sorted_list_difference_2(<, A, As, B, Bs, [A|Cs]) :-
	(   As == [] ->
		Cs = []
	;
		As = [A1|As1],
		compare(Comparison, A1, B),
		sorted_list_difference_2(Comparison, A1, As1, B, Bs, Cs)
	).
sorted_list_difference_2(=, _, As, _, Bs, Cs) :-
	sorted_list_difference(As, Bs, Cs).
sorted_list_difference_2(>, A, As, _, Bs, Cs) :-
	sorted_list_difference_1(Bs, A, As, Cs).


list_between(Nlow, Nhigh, L) :-
	(   Nlow =< Nhigh ->
		L = [Nlow|L1],
		Nlow1 is Nlow + 1,
		list_between(Nlow1, Nhigh, L1)
	;
		L = []
	).


%  print_term(+Term)
%  print_term(+Term, +DepthLimit, +Precedence)
%  Print out Term to the current output stream, respecting the current depth
%  and length limits, and the current set of hidden functors, and using the
%  user-supplied portray_term/3 hook predicate when it succeeds.  If
%  DepthLimit is supplied and is non-negative, then it specifies the maximum
%  number of levels of Term that should be printed.  Precedence, if supplied,
%  should be the precedence of the context in which the term will be printed.
%  DepthLimit defaults to the currently set depth limit; Precedence defaults
%  to 999.

print_term(Term) :-
	print_depth(Limit),
	print_term(Term, Limit, 999).


print_term(Term, Depth, Precedence) :-
	(   var(Term) ->
		write(Term)
	;
		Depth1 is Depth - 1,
		functor(Term, Name, Arity),
		(   portray_term(Term, Depth1, Precedence) ->
			true
		;   Arity =:= 0 ->
			writeq(Name)
		;   Depth1 =:= -1 ->
			format("<~q/~d>", [Name,Arity])
		;   
			standard_print_term(Term, Depth1, Precedence)
		)
	).


standard_print_term(Term, Depth, Precedence) :-
	functor(Term, Name, Arity),
	(   hidden(Name, Arity, Hidden) ->
		true
	;
		Hidden = []
	),
	(   Hidden == term,
	    print_depth(Depth1),
	    Depth =\= Depth1 - 1 ->
		format("<~q/~d>", [Name,Arity])
	;
		(   Hidden == term ->
			% Special case:  if the top level term we're
			% printing is hidden, we print it anyway, but hide all
			% of its arguments.  This is because it seems
			% undesirable to hide the entire term that was to be
			% printed.
			Depth2 = 0
		;
			Depth2 = Depth
		),
		(   Name == '.', Arity =:= 2, Hidden == [] ->
			print_length(Length), 
			print_list(Term, Depth2, Length, 999,
				   [], '[]', '[', ']', ',')
		;   Name == '{}', Arity =:= 1 ->
			write('{'),
			arg(1, Term, Arg),
			maybe_print_term(Arg, Depth2, 999, 1, Hidden, _),
			write('}')
		;   Arity =:= 1,
		    prefix_op(Name, NeededPrecedence, ArgPrecedence) ->
			open_paren_if_needed(Precedence, NeededPrecedence),
			writeq(Name),
			write(' '),
			arg(1,Term, Arg),
			maybe_print_term(Arg, Depth2, ArgPrecedence, 1,
					 Hidden, _),
			close_paren_if_needed(Precedence, NeededPrecedence)
		;   Arity =:= 1,
		    postfix_op(Name, NeededPrecedence, ArgPrecedence) ->
			open_paren_if_needed(Precedence, NeededPrecedence),
			arg(1,Term, Arg),
			maybe_print_term(Arg, Depth2, ArgPrecedence, 1,
					 Hidden, _),
			write(' '),
			writeq(Name),
			close_paren_if_needed(Precedence, NeededPrecedence)
		;   Arity =:= 2,
		    infix_op(Name, NeededPrecedence, Arg1Precedence,
			     Arg2Precedence) ->
			open_paren_if_needed(Precedence, NeededPrecedence),
			arg(1,Term, Arg1),
			maybe_print_term(Arg1, Depth2, Arg1Precedence, 1,
					 Hidden, H2),
			write(' '),
			(   Name == (',') ->
				write(Name)
			;
				writeq(Name)
			),
			write(' '),
			arg(2,Term, Arg2),
			maybe_print_term(Arg2, Depth2, Arg2Precedence, 2,
					 H2, _),
			close_paren_if_needed(Precedence, NeededPrecedence)
		;
			Term =.. [Name|Args],
			writeq(Name),
			print_length(Length),
			print_list(Args, Depth, Length, Precedence, Hidden,
				   '', '(', ')', ',')
		)
	).

prefix_op(Op, NeededPrecedence, ArgPrecedence) :-
	(   current_op(NeededPrecedence, fx, Op) ->
		ArgPrecedence is NeededPrecedence - 1
	;   current_op(NeededPrecedence, fy, Op) ->
		ArgPrecedence = NeededPrecedence
	;
		fail				% to keep NU Prolog happy
	).


postfix_op(Op, NeededPrecedence, ArgPrecedence) :-
	(   current_op(NeededPrecedence, xf, Op) ->
		ArgPrecedence is NeededPrecedence - 1
	;   current_op(NeededPrecedence, yf, Op) ->
		ArgPrecedence = NeededPrecedence
	;
		fail				% to keep NU Prolog happy
	).


infix_op(Op, NeededPrecedence, Arg1Precedence, Arg2Precedence) :-
	(   current_op(NeededPrecedence, xfx, Op) ->
		Arg1Precedence is NeededPrecedence - 1,
		Arg2Precedence = Arg1Precedence
	;   current_op(NeededPrecedence, xfy, Op) ->
		Arg1Precedence is NeededPrecedence - 1,
		Arg2Precedence = NeededPrecedence
	;   current_op(NeededPrecedence, yfx, Op) ->
		Arg1Precedence = NeededPrecedence,
		Arg2Precedence is NeededPrecedence - 1
	;
		fail				% to keep NU Prolog happy
	).


print_list([], _, _, _, _, Empty, _, _, _) :-
	write(Empty).
print_list([T|Ts], Depth, Limit, Precedence, Hidden, _, Start, End, Sep) :-
	write(Start),
	(   Limit =\= 0 ->
		maybe_print_term(T, Depth, Precedence, 1, Hidden, Hidden1),
		Limit1 is Limit + 1,		% first arg NOT to show
		print_list_tail(Ts, 2, Limit1, Hidden1, Depth, Precedence, Sep)
	;
		write('...')
	),
	write(End).
	
print_list_tail(Term, _, _, _, _, _, _) :-
	var(Term),
	!,
	write(' | '),
	write(Term).
print_list_tail([], _, _, _, _, _, _).
print_list_tail([T|Ts], Num, Limit, Hidden, Depth, Precedence, Sep) :-
	write(Sep),
	(   Num =\= Limit ->
		maybe_print_term(T, Depth, Precedence, Num, Hidden, Hidden1),
		Num1 is Num + 1,
		print_list_tail(Ts, Num1, Limit, Hidden1, Depth, Precedence,
				Sep)
	;
		write('...')
	).


maybe_print_term(T, Depth, Precedence, Num, Hidden0, Hidden) :-
	(   Hidden0 = [Num|Hidden] ->
		write('#')
	;
		Hidden = Hidden0,
		print_term(T, Depth, Precedence)
	).


open_paren_if_needed(Precedence, NeededPrecedence) :-
	(   Precedence >= NeededPrecedence ->
		true
	;
		write('(')
	).


close_paren_if_needed(Precedence, NeededPrecedence) :-
	(   Precedence >= NeededPrecedence ->
		true
	;
		write(')')
	).


historyLength(500).

spyHook(_, Term) :-
	!,
	interactive_display(1, Term).

interactive_display(Depth, Term) :-
	flushOutput(user_output),
	flushOutput(user_error),
	( portray(Term) ->
		true
	;
		write('<<'),
		write(Term),
		write('>>')
	),
	nl,
	flushOutput(user_output),
	flushOutput(user_error),
	( nonvar(Term) ->
		write(user_error, Depth),
		write(user_error, '> select arg to display (h for help): '),
		flushOutput(user_error),
		read(user_input, Num),
		( Num = 'a' ->
			write(user_error, Term),
			writeln(user_error, '.')
		; Num = 'e' ->
			fail
		; Num = 'r' ->
			true
		; Num = 0 ->
			true
		; Num = 'h' ->
			write('h = help'), nl,
			write('r = return (1 level)'), nl,
			write('e = exit (all levels)'), nl,
			write('a = display all'), nl,
			write('<number> = display nth argument'), nl,
			nl,
			flushOutput(user_output),
			flushOutput(user_error),
			interactive_display(Depth, Term)
		; arg(Num, Term, Arg) ->
			Depth1 is Depth + 1,
			interactive_display(Depth1, Arg),
			interactive_display(Depth, Term)
		;
			write('Invalid response'), nl,
			flushOutput(user_output),
			flushOutput(user_error),
			interactive_display(Depth, Term)
		)
	;
		true
	).


%  portray_term(+Term, +DepthLimit, +Precedence)
%  User-supplied hook predicate.  Like the standard Prolog portray/1 hook
%  predicate, this code should either fail without printing anything, or
%  succeed after printing out Term as the user would like to see it printed.
%  DepthLimit, if >= -1, is the number of levels of the subterms of Term that
%  should be printed without being elided.  If it is -1, then Term should
%  probably not be printed; only a marker indicating what sort of term it is.
%  The standard print_term code will print the name and arity of the functor
%  enclosed in angle brackets (e.g., <foo/3>) if portray_term/3 fails at this
%  depth level.  Be careful, though: if DepthLimit is negative, then there is
%  no depth limit.  Precedence is the precedence of the context in which the
%  term will be printed; if you wish to write a term with operators, then you
%  should parenthesize the output if Precedence is smaller than the precedence
%  of that operator.
%
%  In printing subterms, you should call print_term/3, described above.  Note
%  that DepthLimit argument passed to portray_term/3 is the depth limit for
%  the subterms of Term, so you should usually pass this value to print_term/3
%  for printing subterms without decrementing it.
%
%  The following code should serve as an example.

:- dynamic portray_term/3.			% NU Prolog doesn't support
						% :- multifile decls.

%  Print "strings" (lists of character codes) as double-quoted strings.
portray_term([N|Ns], _, _) :-
	integer(N),				% for efficiency
	printable_char_list([N|Ns]),
	!,
	format("""~s""", [[N|Ns]]).

%  Print maps as MAP{ key->value , key->value , ... }
portray_term(two(K,V,S1,S2), Depth, _) :-
	!,
	portray_map(two(K,V,S1,S2), Depth).
portray_term(three(K1,V1,K2,V2,S1,S2,S3), Depth, _) :-
	!,
	portray_map(three(K1,V1,K2,V2,S1,S2,S3), Depth).
portray_term(four(K1,V1,K2,V2,K3,V3,S1,S2,S3,S4), Depth, _) :-
	!,
	portray_map(four(K1,V1,K2,V2,K3,V3,S1,S2,S3,S4), Depth).


printable_char_list(0) :-			% hack to catch unbound tails
	!,
	fail.
printable_char_list([]).
printable_char_list([N|Ns]) :-
	integer(N),
	(   N >= 32, N =< 126 -> true
	;   N =:= 0'\n -> true
	;   N =:= 0'\r -> true
	;	N =:= 0'\t
	),
	printable_char_list(Ns).
	

portray_map(Map, Depth) :-
	(   Depth =:= -1 ->
		write('MAP{...}')
	;   
		tree234__tree234_to_assoc_list(Map, Alist),
		list__map(pair_to_arrow, Alist, Arrowlist),
		print_length(Length),
		Depth1 is Depth + 1,
		print_list(Arrowlist, Depth1, Length, 1200, [],
			   'MAP{}', 'MAP{', '}', ' , ')
	).
	
pair_to_arrow(X-Y, (X->Y)).


%-----------------------------------------------------------------------------%
