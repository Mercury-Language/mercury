	% This version is for debugging with np.

error(Message) :-
	format("<Software Error: ~s>\n", [Message]),
	ancestors(Anc),
	( Anc \= [] ->
		write('Stack trace:'), nl,
		stack_trace(Anc),
		interactive_display(1, Anc)
	;
		write('Stack trace unavailable.'), nl
	),
	fail.
?- spy error.

stack_trace([]).
stack_trace([Anc|Ancs]) :-
	print(Anc), write('.'), nl,
	stack_trace(Ancs).

