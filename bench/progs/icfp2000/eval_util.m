:- module eval_util.

:- interface.

:- import_module io, std_util.
:- import_module eval, gml.

:- pred write_env(env::in, io__state::di, io__state::uo) is det.
:- pred write_stack(stack::in, io__state::di, io__state::uo) is det.

:- pred write_nice_exception(univ::in, io__state::di, io__state::uo) is det.

:- pred write_prog(int::in, gml_program::in,
		io__state::di, io__state::uo) is det.

:- type unequal_stacks_exception --->
		unequal_stacks_exception(string, stack, stack).

:- implementation.

:- import_module int, list, exception, map, string, gml.
:- import_module pprint.

write_nice_exception(E) -->
	io__stderr_stream(StdErr),
	io__set_output_stream(StdErr, OldStream),
	( 
		{ univ_to_type(E, unequal_stacks_exception(Msg, Stack, Opt)) }
	->
		io__write_string("Exception: "),
		io__write_string(Msg),
		io__nl,
		io__write_string("Unoptimized "),
		write_stack(Stack),
		io__write_string("Optimized "),
		write_stack(Opt)
	;
		{ univ_to_type(E, stack_env_exception(Msg, Env, Stack)) }
	->
		io__write_string("Exception: "),
		io__write_string(Msg),
		io__nl,
		write_env(Env),
		write_stack(Stack)
	; 
		{ univ_to_type(E,
			stack_env_token_exception(Msg, Env, Stack, Token)) }
	->
		io__write_string("Exception at token "),
		io__write(Token),
		io__write_string(" : "),
		io__write_string(Msg),
		io__nl,
		write_env(Env),
		write_stack(Stack)
	; 
		{ univ_to_type(E, parse_error(Msg)) }
	->
		io__write_string("Parse error: "),
		io__write_string(Msg)
	;
		{ univ_to_type(E, lexer_error(N, Msg)) }
	->
		io__format("Line %d: lexical error: %s ", [i(N), s(Msg)])
	;
		{ univ_to_type(E, program_error(Msg)) }
	->
		io__write_string("Program error: "),
		io__write_string(Msg)
	;
		{ univ_to_type(E, program_error(Msg, Stack)) }
	->
		io__write_string("Program error: "),
		io__write_string(Msg),
		io__nl,
		write_stack(Stack)
	;
		{ univ_to_type(E, S) }
	->
		io__write_string("Error: "),
		io__write_string(S)
	;
		io__write(E)
	),
	io__nl,
	io__set_output_stream(OldStream, _).


write_env(Env) --> 
	io__write_string("Environment:\n"),
	map__foldl(write_env_entry, Env).

:- pred write_env_entry(id::in, value::in, io__state::di, io__state::uo) is det.
write_env_entry(Id, Value) -->
	io__write_string(Id),
	io__write_string("\t: "),
	io__write(Value),
	io__nl.
	
write_stack(Stack) --> 
	io__write_string("Stack:\n"),
	list__foldl(write_stack_entry, Stack).

:- pred write_stack_entry(value::in, io__state::di, io__state::uo) is det.
write_stack_entry(Value) -->
	write(80, to_doc(3, Value)),
	io__nl.

%-----------------------------------------------------------------------------%

write_prog(_, []) --> [].
write_prog(Indent, [Group | Groups]) -->
	write_group(Indent, Group),
	io__nl,
	write_prog(Indent, Groups).

:- pred write_group(int::in, token_group::in,
		io__state::di, io__state::uo) is det.

write_group(Indent, single_token(SingleToken)) -->
	indent(Indent),
	write(SingleToken).
write_group(Indent, function(List)) -->
	indent(Indent),
	io__write_string("{\n"),
	write_prog(Indent + 1, List),
	indent(Indent),
	io__write_string("}").
write_group(Indent, array(List)) -->
	indent(Indent),
	io__write_string("[\n"),
	write_prog(Indent + 1, List),
	indent(Indent),
	io__write_string("]").

:- pred indent(int::in, io__state::di, io__state::uo) is det.

indent(Indent) -->
	( { Indent = 0 } ->
		[]
	;
		io__write_string("   "),		
		indent(Indent - 1)
	).

%-----------------------------------------------------------------------------%

