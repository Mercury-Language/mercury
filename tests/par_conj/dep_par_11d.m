% see comments in code for explanation
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%         Key Type: term.var(parse_tree.prog_data.prog_var_type)
%         Key Value: var(10)
%         Value Type: ll_backend.var_locn.var_state

:- module dep_par_11d.

:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.
:- import_module list.

main(!IO) :-
    par_conj(Y),
    io.write_int(Y, !IO),
    io.nl(!IO).

:- pred par_conj(int::out) is det.
:- pragma no_inline(par_conj/1).

par_conj(Y) :-
    (
        X = one
    &
        U = 2,	 % prevent flattening of par conj
        (
	    % wait(X)
            V = X + U
        &
	    % wait(X)
	    % This second wait would be optimised away (leading to a compiler
	    % abort).  The trick is that we need to rename the occurrences of X
	    % in different parallel conjuncts separately.

	    % wait(V)
            Y = V + X
        )
    ).

:- func one = int.
:- pragma no_inline(one/0).
one = 1.
