%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module ambig_pred.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

% :- type a ---> foo; bar.
% :- type b ---> bar; baz.
% :- type c ---> baz; foo.
% :- pred confuse(a, T, T).
% :- pred confuse(T, b, T).
% :- pred confuse(T, T, c).

:- implementation.

:- import_module ambig_pred_helper_1.
:- import_module ambig_pred_helper_2.
:- import_module ambig_pred_helper_3.
:- import_module ambig_pred_helper_4.

main(!IO) :-
    ambig(_, _, _),
    io.write_string("a", !IO).

:- pred ambig(a, a, a).
:- mode ambig(out, out, out) is det.

ambig(foo, foo, foo) :-
    A = foo,
    B = bar,
    C = baz,
    confuse(A, B, C),
    confuse(A, C, B).

:- pred constrain(T, T).
:- mode constrain(in, out) is det.

constrain(A, A).

:- end_module ambig_pred.
