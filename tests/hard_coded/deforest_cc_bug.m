:- module deforest_cc_bug.
:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module list, std_util.

main -->
	bug(1, [2,3,4,5,6,7]).

:- pred bug(int::in, list(int)::in, io__state::di, io__state::uo) is cc_multi.
:- pragma no_inline(bug/4).
	
bug(FirstArg, ArgsRest) -->
	{ get_inputs_and_result(FirstArg, ArgsRest, Inputs, _) },
	my_write_list(Inputs, write_decl_atom_arg),
	io__nl.

:- pred write_decl_atom_arg(int, io__state, io__state).
:- mode write_decl_atom_arg(in, di, uo) is cc_multi.

write_decl_atom_arg(Arg) -->
	cc_multi_equal,
	io__write_int(Arg).

:- pred get_inputs_and_result(int, list(int), list(int), int).
:- mode get_inputs_and_result(in, in, out, out) is det.

get_inputs_and_result(A, [], [], A).
get_inputs_and_result(A1, [A2 | As], [A1 | Inputs0], Result) :-
	get_inputs_and_result(A2, As, Inputs0, Result).

:- pred my_write_list(list(int)::in,
	pred(int, io__state, io__state)::(pred(in, di, uo) is cc_multi),
	io__state::di, io__state::uo) is cc_multi.

my_write_list([], _OutputPred) --> [].
my_write_list([E|Es], OutputPred) -->
	OutputPred(E),
	my_write_list(Es, OutputPred).

%-----------------------------------------------------------------------------%
