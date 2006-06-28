% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%        Key Type: term.var(parse_tree.prog_data.prog_var_type)
%        Key Value: var(4)
%        Value Type: ll_backend.var_locn.var_state

:- module dep_par_20.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- type t ---> t.

main(!IO) :-
    ( T = t
    & X = T
    & q(X, Y)
    ),
    io.print(Y, !IO),
    io.nl(!IO).

:- pred q(t::in, t::out) is det.
:- pragma no_inline(q/2).
q(X, X).
