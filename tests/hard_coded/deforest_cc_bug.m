%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module deforest_cc_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module list.
:- import_module std_util.

main(!IO) :-
    bug(1, [2, 3, 4, 5, 6, 7], !IO).

:- pred bug(int::in, list(int)::in, io::di, io::uo) is cc_multi.
:- pragma no_inline(bug/4).

bug(FirstArg, ArgsRest, !IO) :-
    get_inputs_and_result(FirstArg, ArgsRest, Inputs, _),
    my_write_list(Inputs, write_decl_atom_arg, !IO),
    io.nl(!IO).

:- pred write_decl_atom_arg(int, io, io).
:- mode write_decl_atom_arg(in, di, uo) is cc_multi.

write_decl_atom_arg(Arg, !IO) :-
    cc_multi_equal(!IO),
    io.write_int(Arg, !IO).

:- pred get_inputs_and_result(int, list(int), list(int), int).
:- mode get_inputs_and_result(in, in, out, out) is det.

get_inputs_and_result(A, [], [], A).
get_inputs_and_result(A1, [A2 | As], [A1 | Inputs0], Result) :-
    get_inputs_and_result(A2, As, Inputs0, Result).

:- pred my_write_list(list(int)::in,
    pred(int, io, io)::(pred(in, di, uo) is cc_multi),
    io::di, io::uo) is cc_multi.

my_write_list([], _OutputPred, !IO).
my_write_list([E | Es], OutputPred, !IO) :-
    OutputPred(E, !IO),
    my_write_list(Es, OutputPred, !IO).

%---------------------------------------------------------------------------%
