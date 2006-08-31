% We disallow impure parallel conjuncts as they can introduce concurrency
% issues.

:- module impure_par_conj.
:- interface.
:- import_module io.

:- impure pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

main(!IO) :-
    ( semipure get_mut(_)
    & true
    ),
    ( true
    & impure set_mut(1)
    & true
    ),
    ( true
    & impure set_mut(1)
    & true
    & semipure get_mut(_)
    & true
    ),
    (
	% This would be okay if the mutable was `thread_local' (which it isn't,
	% as they're not implemented yet) or if you are absolutely certain that
	% this mutable is only used by one thread (as in this case).
	promise_pure
	(
	    semipure get_mut(X),
	    impure set_mut(X + 1)
	)
    &
	true
    ).

:- mutable(mut, int, 0, ground, [untrailed]).
