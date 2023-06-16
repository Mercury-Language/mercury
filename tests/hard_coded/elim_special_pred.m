%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for elimination of special predicates in base_type_infos.
%
% We can use arg/3 and det_arg/3 to retrieve arguments of types, and
% unify them. Analysis procedures may incorrectly conclude that
% we cannot call the unification procedure (or other procedures), and
% so eliminate it.
%
% The Mercury compiler of February 13th, 1997 failed an earlier version
% of this test - the test ended with a runtime error indicating that
% an unused predicate had been called.
%
% Author: trd

:- module elim_special_pred.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module deconstruct.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module string.
:- import_module term.
:- import_module univ.

:- type enum
    --->    one
    ;       two
    ;       three.

:- type fruit
    --->    banana(enum).

main(!IO) :-
    X = banana(three),
    Y = banana(two),
    det_arg(X, canonicalize, 0, PseudoXArg),
    type_to_univ(PseudoXArg, XArg),
    det_arg(Y, canonicalize, 0, PseudoYArg),
    type_to_univ(PseudoYArg, YArg),
    ( if XArg = YArg then
        io.write_string("same\n", !IO)
    else
        io.write_string("different\n", !IO)
    ).
