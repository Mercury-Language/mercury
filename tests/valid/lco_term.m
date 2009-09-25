%-----------------------------------------------------------------------------%
% An interim version of the compiler aborted when performing the LCMC
% transformation for high-level data.

:- module lco_term.
:- interface.

:- import_module list.
:- import_module univ.

%-----------------------------------------------------------------------------%

:- type lco_term(T)
    --->    functor(
                const,
                list(lco_term(T))
            )
    ;       variable.

:- type const
    --->    atom(string)
    ;       integer(int).

:- pred univ_to_term(univ::in, lco_term(_)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module deconstruct.

%-----------------------------------------------------------------------------%

univ_to_term(Univ, Term) :-
    deconstruct(univ_value(Univ), canonicalize, FunctorString,
        _FunctorArity, FunctorArgs),
    univ_list_to_term_list(FunctorArgs, TermArgs),
    Term = functor(atom(FunctorString), TermArgs).

:- pred univ_list_to_term_list(list(univ)::in, list(lco_term(T))::out) is det.

univ_list_to_term_list([], []).
univ_list_to_term_list([Value|Values], [Term|Terms]) :-
    univ_to_term(Value, Term),
    univ_list_to_term_list(Values, Terms).

%-----------------------------------------------------------------------------%
