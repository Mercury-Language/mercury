% Test case for solutions
% 
% Author: trd
%
% This test case exercises a number of features:
%	- The ability to modecheck equivalence insts correctly (the
%	  Mercury compiler of Jan 20 1997 couldn't compile code like
%	  this - in fact if you made mypred == ground it still couldn't
%	  get it right).
%	- Solutions of higher order predicates
%	- In non-conservative GC grades, deep_copy of closures (since
%	  solutions is implemented using deep copy on them).
%	- Higher order syntax -- P(5, 1, X)

:- module ho_solns.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module list, std_util, int.

main -->
	{ unsorted_solutions(foo, List0) },
	{ convert_list(List0, List) },
	use_list(List).

:- type mypred == (pred(int, int, int)).
:- inst mypred = (pred(in, in, out) is det).

:- pred convert_list(list(T), list(T)).
:- mode convert_list(in, out(list_skel(mypred))) is det.

:- pragma c_code(
	convert_list(L0 :: in, L :: out(list_skel(mypred))), "
{
	L = L0;
}
").

:- pred use_list(list(mypred), io__state, io__state).
:- mode use_list(in(list_skel(mypred)), di, uo) is det.
use_list([]) --> [].
use_list([P | Ps]) --> 
	{ P(5, 1, X) },
	io__write_int(X),
	io__write_string("\n"),
	use_list(Ps).

:- pred foo(mypred).
:- mode foo(out(mypred)) is multi.

foo(X) :- X = (pred(A::in, B::in, C::out) is det :- C = A + B).
foo(X) :- X = (pred(A::in, B::in, C::out) is det :- C = A * B).
foo(X) :- X = (pred(A::in, B::in, C::out) is det :- C = A - B).

