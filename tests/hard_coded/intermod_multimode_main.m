:- module intermod_multimode_main.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module intermod_multimode.

:- pragma promise_pure(main/2).
main -->
	{ In = 42 },
	{ In2 = In },		% this line (and the use of `In2' below,
				% rather than `In') is needed to avoid
				% triggering an unrelated bug -- see
				% tests/valid/mode_selection.m.

	% test pure functions
	print(func0), nl,
	print(func1(In)), nl,
	print(func1(_Out0)), nl,
	print(func2(In, In2)), nl,
	print(func2(In, _Out1)), nl,
	print(func2(_Out2, In)), nl,
	print(func2(_Out3, _Out4)), nl,

	% test impure predicates
	{ impure test0 },
	{ impure test1(In) },
	{ impure test1(_Out10) },
	{ impure test2(In, In) },
	{ impure test2(In, _Out11) },
	{ impure test2(_Out12, In) },
	{ impure test2(_Out13, _Out14) },
	
	{ get_determinism((pred(1::out) is semidet), Det) },
	io__write(Det),
	io__nl.

