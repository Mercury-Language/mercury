%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% foo/2 is an example of a predidate that is tail recursive in non-trailing
% grades, but not tail recursive in trailing grades.
%
% The .err_exp file is for non-trailing grades. It should be empty.
% The .err_exp2 file is for trailing grades.
%

:- module trail_non_tailcall.
:- interface.

:- import_module int.

:- pred foo(int::in, int::out) is cc_nondet.

:- implementation.

:- pragma require_tail_recursion(pred(foo/2)).

foo(X, Y) :-
   % In trailing grades, the compiler
   % - creates a trail ticket before this model_semi disjunction
   %   (as it would also do before a model_det disjunction), and then
   % - adds code to the end of each disjunct to prune this ticket.
   (
      X > 10,
      Y = 3
   ;
      X = 5,
      foo(X + 1, Y)
   ).
