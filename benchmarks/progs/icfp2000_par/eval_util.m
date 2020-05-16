%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module eval_util.

:- interface.

:- import_module eval.
:- import_module gml.

:- import_module io.
:- import_module univ.

:- pred write_env(env::in, io::di, io::uo) is det.
:- pred write_stack(stack::in, io::di, io::uo) is det.

:- pred write_nice_exception(univ::in, io::di, io::uo) is det.

:- pred write_prog(int::in, gml_program::in, io::di, io::uo) is det.

:- type unequal_stacks_exception
    --->    unequal_stacks_exception(string, stack, stack).

:- implementation.

:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module pprint.
:- import_module string.

write_nice_exception(E, !IO) :-
	io.stderr_stream(StdErr, !IO),
	io.set_output_stream(StdErr, OldStream, !IO),
	(
		univ_to_type(E, unequal_stacks_exception(Msg, Stack, Opt))
	->
		io.write_string("Exception: ", !IO),
		io.write_string(Msg, !IO),
		io.nl(!IO),
		io.write_string("Unoptimized ", !IO),
		write_stack(Stack, !IO),
		io.write_string("Optimized ", !IO),
		write_stack(Opt, !IO)
	;
		univ_to_type(E, stack_env_exception(Msg, Env, Stack))
	->
		io.write_string("Exception: ", !IO),
		io.write_string(Msg, !IO),
		io.nl(!IO),
		write_env(Env, !IO),
		write_stack(Stack, !IO)
	;
		univ_to_type(E,
			stack_env_token_exception(Msg, Env, Stack, Token))
	->
		io.write_string("Exception at token ", !IO),
		io.write(Token, !IO),
		io.write_string(" : ", !IO),
		io.write_string(Msg, !IO),
		io.nl(!IO),
		write_env(Env, !IO),
		write_stack(Stack, !IO)
	;
		univ_to_type(E, parse_error(Msg))
	->
		io.write_string("Parse error: ", !IO),
		io.write_string(Msg, !IO)
	;
		univ_to_type(E, lexer_error(N, Msg))
	->
		io.format("Line %d: lexical error: %s ", [i(N), s(Msg)], !IO)
	;
		univ_to_type(E, program_error(Msg))
	->
		io.write_string("Program error: ", !IO),
		io.write_string(Msg, !IO)
	;
		univ_to_type(E, program_error(Msg, Stack))
	->
		io.write_string("Program error: ", !IO),
		io.write_string(Msg, !IO),
		io.nl(!IO),
		write_stack(Stack, !IO)
	;
		univ_to_type(E, S)
	->
		io.write_string("Error: ", !IO),
		io.write_string(S, !IO)
	;
		io.write(E, !IO)
	),
	io.nl(!IO),
	io.set_output_stream(OldStream, _, !IO).

write_env(Env, !IO) :-
	io.write_string("Environment:\n", !IO),
	map.foldl(write_env_entry, Env, !IO).

:- pred write_env_entry(id::in, value::in, io::di, io::uo) is det.

write_env_entry(Id, Value, !IO) :-
	io.write_string(Id, !IO),
	io.write_string("\t: ", !IO),
	io.write(Value, !IO),
	io.nl(!IO).

write_stack(Stack, !IO) :-
	io.write_string("Stack:\n", !IO),
	list.foldl(write_stack_entry, Stack, !IO).

:- pred write_stack_entry(value::in, io::di, io::uo) is det.

write_stack_entry(Value, !IO) :-
	write(80, to_doc(3, Value), !IO),
	io.nl(!IO).

%-----------------------------------------------------------------------------%

write_prog(_, [], !IO).
write_prog(Indent, [Group | Groups], !IO) :-
	write_group(Indent, Group, !IO),
	io.nl(!IO),
	write_prog(Indent, Groups, !IO).

:- pred write_group(int::in, token_group::in, io::di, io::uo) is det.

write_group(Indent, single_token(SingleToken), !IO) :-
	indent(Indent, !IO),
	write(SingleToken, !IO).
write_group(Indent, function(List), !IO) :-
	indent(Indent, !IO),
	io.write_string("{\n", !IO),
	write_prog(Indent + 1, List, !IO),
	indent(Indent, !IO),
	io.write_string("}", !IO).
write_group(Indent, array(List), !IO) :-
	indent(Indent, !IO),
	io.write_string("[\n", !IO),
	write_prog(Indent + 1, List, !IO),
	indent(Indent, !IO),
	io.write_string("]", !IO).

:- pred indent(int::in, io::di, io::uo) is det.

indent(Indent, !IO) :-
	( Indent = 0 ->
		true
	;
		io.write_string("   ", !IO),
		indent(Indent - 1, !IO)
	).

%-----------------------------------------------------------------------------%

