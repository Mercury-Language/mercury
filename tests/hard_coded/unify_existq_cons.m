:- module unify_existq_cons.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module enum, char, int.

:- type tc
	---> some [T] tc(T) => enum(T). 

main -->
	(
		{ p('new tc'('a'))
		; p('new tc'(2))
		}
	->
		io__write_string("test failed\n")
	;
		io__write_string("test succeeded\n")	
	).

:- pred p(tc::in) is semidet.

% Mode analysis must treat the headvar unification here as a construction
% followed by a var-var unification. If it treats it as a deconstruction
% the argument unifications will be ill-typed.
p('new tc'(1)).

