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
% 	Nice try, but you can't fool univ_to_type!
%
% Author: trd
%

:- module ho_univ_to_type.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module std_util, int, require.

:- type mypred == (pred(int, int, int)).
:- type mypred2 == (pred(float, int, int)).
:- inst mypred = (pred(in, in, out) is det).

main -->
	{ foo(Pred0) },
	{ type_to_univ(Pred0, Univ) },
	( 
		{ univ_to_type(Univ, Pred1) }
	->
		{ convert_inst(Pred1, Pred2) },
		{ Pred2(5.0, 1, X) },
		io__write_int(X),
		io__write_string("\n")
	;
		io__write_string("Nice try, but you can't fool univ_to_type!\n")
	).


:- pred foo(mypred).
:- mode foo(out(mypred)) is det.

foo(X) :- X = (pred(A::in, B::in, C::out) is det :- C = A + B).

% Some hacky pragma c_code to allow use to change an
% inst from `ground' to `pred(in, in, out) is det'.

:- pred convert_inst(mypred2::in, mypred2::out(mypred)) is det.

:- pragma c_code(convert_inst(Pred1::in, Pred2::out(mypred)), "
{
	Pred2 = Pred1;
}
").


