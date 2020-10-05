%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test case for use of univ_to_type on higher order things.
%
% The Mercury compiler of 2nd of February 1997 was unable to correctly
% detect the runtime type error in this program.
% We convert the pred(int, int, int) to pred(float, int, int) via
% type_to_univ and univ_to_type.
% Unless the arguments of the pred are checked when converting
% univ_to_type, the will add a float to an integer.
%
% Correct behaviour is to give the message:
%   Nice try, but you can't fool univ_to_type!
%
% Author: trd
%

:- module ho_univ_to_type.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module require.
:- import_module univ.

:- type mypred == (pred(int, int, int)).
:- type mypred2 == (pred(float, int, int)).
:- inst mypred == (pred(in, in, out) is det).

:- type fpred
    --->    fpred(mypred2).
:- inst fpred
    --->    fpred(mypred).

main(!IO) :-
    foo(Pred0),
    type_to_univ(Pred0, Univ),
    ( if univ_to_type(Univ, Pred1) then
        convert_inst(fpred(Pred1), fpred(Pred2)),
        Pred2(5.0, 1, X),
        io.write_int(X, !IO),
        io.write_string("\n", !IO)
    else
        io.write_string("Nice try, but you can't fool univ_to_type!\n", !IO)
    ).

:- pred foo(mypred).
:- mode foo(out(mypred)) is det.

foo(X) :-
    X = (pred(A::in, B::in, C::out) is det :- C = A + B).

% Some hacky pragma foreign_proc to allow use to change an
% inst from `ground' to `pred(in, in, out) is det'.

:- pred convert_inst(fpred::in, fpred::out(fpred)) is det.
:- pragma foreign_proc("C",
    convert_inst(Pred1::in, Pred2::out(fpred)),
    [will_not_call_mercury, promise_pure],
"
{
    Pred2 = Pred1;
}
").
:- pragma foreign_proc("C#",
    convert_inst(Pred1::in, Pred2::out(fpred)),
    [will_not_call_mercury, promise_pure], "
{
    Pred2 = Pred1;
}
").
:- pragma foreign_proc("Java",
    convert_inst(Pred1::in, Pred2::out(fpred)),
    [will_not_call_mercury, promise_pure],
"
{
    Pred2 = Pred1;
}
").
:- pragma foreign_proc("Erlang",
    convert_inst(Pred1::in, Pred2::out(fpred)),
    [will_not_call_mercury, promise_pure], "
    Pred2 = Pred1
").
