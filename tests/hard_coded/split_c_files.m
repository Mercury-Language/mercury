%% This is a regression test: mmc versions 0.8 and 0.9 generated incorrect
%% C code for this test case, when compiled with --split-c-files --debug.

:- module split_c_files.

:- interface.

:- import_module list, io.


:- pred main(state::di, state::uo) is det.

:- pred app(list(T),list(T),list(T)).
:- mode app(out,in,in) is nondet.

:- implementation.
:- import_module std_util.

main -->
	{ solutions((pred(X::out) is nondet :- app(X, [4], [1,2,3,4])),
		L) },
	write_list(L, "\n", print),
	nl.

split_c_files:(app(Z76,Ys,A77)) :-
        =(Z76,[]),
        =(A77,Ys).
split_c_files:(app(Z76,A77,B77)) :-
        =(B77,[X|Zs]),
        split_c_files:(app(Xs,A77,Zs)),
        =(Z76,[X|Xs]).
