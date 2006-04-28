% The saved_vars pass was not pushing unifications that assign
% constants to variables into parallel conjunctions.
%
% Uncaught Mercury exception:
% Software Error: map.lookup: key not found
%         Key Type: term.var(parse_tree.prog_data.prog_var_type)
%         Key Value: var(4)
%         Value Type: ll_backend.var_locn.var_state

:- module par_saved_const.
:- interface.
:- import_module int.
:- import_module list.

:- pred p(list(int)::out) is det.

:- implementation.

p(M) :-
    L = [1,2],
    L = [H|_],
    ( X = H
    & true
    ),
    M = [X|L].
