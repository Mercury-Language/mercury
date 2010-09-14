% vim: ts=4 sw=4 et ft=mercury
% This is a test case for Mantis bug 159. The bug was that mode checking and
% unique mode checking did not eliminate a from_ground_term scope that
% constructed a variable which was unused except for its type (variable L
% in pickles/0 below), even though they eliminated construction unifications
% that constructed similarly unused variables.

:- module bug159.

:- interface.

:- type pickles ---> pickles(int).

:- func pickles = pickles.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module univ.
:- import_module type_desc.
:- import_module construct.
:- import_module deconstruct.
:- import_module require.

:- pragma memo(pickles/0).

pickles = !:P :-
    !:P = pickles(0),
    % The size of this term should be (and currently is) above the
    % from_ground_term threshold.
    L = [1,2,3],
    register_pickle(type_ctor(type_of(L)), pickle_list_as_array, !P).

:- type byte_buffer == int.

:- type maybe_pickle == pred(pickles, univ, byte_buffer, byte_buffer).
:- inst maybe_pickle == (pred(in, in, di, uo) is det).

:- pred register_pickle(type_ctor_desc::in, maybe_pickle::in(maybe_pickle),
    pickles::in, pickles::out) is det.

register_pickle(_T, _N, pickles(I), pickles(I+1)) :-
    true.

:- pred pickle_list_as_array `with_type` maybe_pickle `with_inst` maybe_pickle.

pickle_list_as_array(_Pickles, _UnivList, !BB) :-
    error("foo").
