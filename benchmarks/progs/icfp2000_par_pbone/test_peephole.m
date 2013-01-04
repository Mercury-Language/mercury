:- module test_peephole.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module gml, peephole.
:- import_module int, list.

main -->
	tokenize(Toks),
	{ parse(Toks, Prog) },
	io__write_string("% ------------- Initial -------------- %\n\n"),
	write_prog(0, Prog),
	io__write_string("% -------------   Opt   -------------- %\n\n"),
	{ peephole(Prog, OptProg) },
	% { OptProg = name_apart(Prog) },
	write_prog(0, OptProg).

:- pred write_prog(int::in, gml_program::in,
		io__state::di, io__state::uo) is det.

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
