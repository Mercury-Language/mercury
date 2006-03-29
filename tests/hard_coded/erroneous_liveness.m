:- module erroneous_liveness.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- implementation.
:- import_module univ, char, int, string, list, set, require.

:- type indexing ---> indexed ; unindexed.

:- type cardinality == int.	% >= 0

:- type entry == string.

:- type field ---> field(indexing, cardinality, set(entry)).

:- func init_field = field.
init_field = field(unindexed, 0, S) :- set__init(S).

%%%

:- type cost == int.		% >= 0

:- type buffer_size == int.	% >= 0

:- type tuple ---> tuple(entry, entry, entry).

:- type table ---> table(cost, buffer_size, field, field, field, set(tuple)).
				     %supplier, part, job

:- func init_table = table.
init_table = table(0, 0, init_field, init_field, init_field, S) :- set__init(S).

%%%
:- type column ---> supplier ; part ; job.

:- type operation
	--->	index(column)
	;	insert
	;	buffer(buffer_size)
	;	retrieve(column, entry)
	;	retrieve_all
	.
	
%%%

main -->
	set_globals(init_table),
	process_lines,
	get_globals(table(Cost, _, _, _, _, _)),
	io__write_string("Total cost was "),
	io__write_int(Cost),
	io__nl.

:- pred set_globals(table::in, io__state::di, io__state::uo) is det.
set_globals(G) --> {copy(G, G1), type_to_univ(G1, G2)}, io__set_globals(G2).

:- pred get_globals(table::out, io__state::di, io__state::uo) is det.
get_globals(G) -->
	io__get_globals(G0),
	{ univ_to_type(G0, G1) ->
		G = G1
	;	error("get_globals/3---univ_to_type failed.")
	}.

:- pred process_lines(io__state::di, io__state::uo) is det.
process_lines -->
	io__read_line(Result),
	( {Result = ok(CharList)},
		interpret(strings(CharList))
	; {Result = eof}
	; {Result = error(Error),
		io__error_message(Error, Message)}, abort(Message)
	).

:- pred interpret(list(string)::in, io__state::di, io__state::uo) is det.
interpret([]) --> abort("empty line").
interpret([H|T]) -->
	( {H = "index"} ->
		oneArg(H, T, Arg),
		( {Arg = "supplier"} ->
			{Column = supplier}
		; {Arg = "part"} ->
			{Column = part}
		; {Arg = "job"} ->
			{Column = job}
		;	{string__append("index on unrecognised column---",
								Arg, Message)},
			abort(Message)
		),
		operate(index(Column))
	; {H = "insert"} ->
		noArg(H, T),
		operate(insert)
	; {H = "buffer"} ->
		oneArg(H, T, Arg),
		( {string__to_int(Arg, BSize0), BSize0 >= 0} ->
			{BSize = BSize0}
		;	{string__append(
				"buffer size must be non-negative integer---",
								Arg, Message)},
			abort(Message) 
		),
		operate(buffer(BSize))
	; {H = "supplier"} ->
		oneArg(H, T, Arg),
		operate(retrieve(supplier, Arg))
	; {H = "part"} ->
		oneArg(H, T, Arg),
		operate(retrieve(part, Arg))
	; {H = "job"} ->
		oneArg(H, T, Arg),
		operate(retrieve(job, Arg))
	; {H = "spj"} ->
		noArg(H, T),
		operate(retrieve_all)
	;	{string__append("unrecognised command---", H, Message)},
		abort(Message)
	).

:- pred operate(operation::in, io__state::di, io__state::uo) is det.
operate(_) --> [].
	
:- pred noArg(string::in, list(string)::in,
					io__state::di, io__state::uo) is det.
noArg(_, []) --> [].
noArg(S, [_|_]) -->
	{string__append("no args expected for command---", S, Message)},
	abort(Message).

:- pred oneArg(string::in, list(string)::in, string::out,
					io__state::di, io__state::uo) is det.
oneArg(S, [], _) -->
	{string__append("one arg expected for command---", S, Message)},
	abort(Message).
oneArg(S, [H|T], H) -->
	( {T = []} ->
		{true}
	;	{string__append("only one arg expected for command---",
								S, Message)},
		abort(Message)
	).

:- func strings(list(char)) = list(string).
strings(CL) = SL :- strings(CL, [], [], SL1), list__reverse(SL1, SL).

:- pred strings(list(char), list(char), list(string), list(string)).
:- mode strings(in, in, in, out).
strings([], SCL, SL0, SL) :-
	addString(SCL, SL0, SL).
strings([H|T], SCL, SL0, SL) :-
	( char__is_whitespace(H) ->
		SCL1 = [], addString(SCL, SL0, SL1)
	;	SCL1 = [H|SCL], SL1 = SL0
	),
	strings(T, SCL1, SL1, SL).

:- pred addString(list(char), list(string), list(string)).
:- mode addString(in, in, out).
addString(SCL, SL0, SL) :-
	( SCL = [],
		SL = SL0
	; SCL = [_|_],
		list__reverse(SCL, SCL1),
		string__from_char_list(SCL1, S),
		SL = [S|SL0]
	).

:- pred abort(string::in, io__state::di, io__state::uo) is erroneous.
abort(Message) -->
	io__write_string("Error at line "),
	io__get_line_number(N),
	io__write_int(N),
	io__write_string(": "),
	{error(Message)}.
%	io__write_string(Message),
%	io__nl,
%	io__set_exit_status(1).


