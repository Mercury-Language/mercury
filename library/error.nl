%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

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

stack_trace([]).
stack_trace([Anc|Ancs]) :-
	print(Anc), write('.'), nl,
	stack_trace(Ancs).

