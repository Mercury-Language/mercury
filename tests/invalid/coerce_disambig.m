%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module coerce_disambig.
:- interface.

:- type cord(T).

:- pred member(T, cord(T)).
:- mode member(out, in) is nondet.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.  % includes list.member/2

:- type nonempty_list(T) =< list(T)
    --->    [T | list(T)].

:- type cord(T)
    --->    empty_node
    ;       list_node(nonempty_list(T)).

member(X, Node) :-
    (
        Node = empty_node
        % This branch deliberately does not bind X. We should get a mode
        % mismatch error instead of any errors about coerce.
    ;
        Node = list_node(Xs),
        % member/2 is ambiguous
        % but coerce to list(T) is valid
        % while coerce to cord(T) is invalid
        % so the user should not need to disambiguate.
        member(X, coerce(Xs))
    ).
