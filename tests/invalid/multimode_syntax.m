%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multimode_syntax.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO).

:- func func0 = string.
:- mode func0 = out is det.
func0 = "func0 = out" :: out. % missing parentheses

:- func func1(int) = string.
:- mode func1(in) = out is det.
:- mode func1(out) = out is det.
func1(_::in) = "func1(in) = out".  % missing mode annotation on return value
func1(0) = ("func1(out) = out" :: out). % missing mode annotation on argument

:- func func2(int, int) = string.
:- mode func2(in, in) = out is det.
func2(_::in, _::in) = (R::out) :-
    R = "func2(in, in) = out".
func2(_::in, 0::out) = (R::out) :-  % reference to undeclared mode
    R = "func2(in, out) = out".
func2(0::out, _::in) = (R::out) :-
    R = "func2(out, in) = out".
func2(0::out, 0::out) = (R::out) :-
    R = "func2(out, out) = out".

:- func func2b(int, int) = string.
func2b(_::in, _::out) = (R::out) :- % another reference to undeclared mode
    R = "func3(in, out) = out".

:- impure pred pred2b(int, int).
pred2b(_::in, 0::out) :-        % another reference to undeclared mode
    impure puts("func3(in, out) = out").

:- impure pred test2(int, int).
:- mode test2(in, in) is det.
:- mode test2(in, out) is det.
:- mode test2(out, in) is det.
:- mode test2(out, out) is det.
test2(_::in, _) :-          % missing mode annotation on 2nd arg
    impure puts("test2(in, in)").
test2(_, 0::out) :-         % missing mode annotation on 1st arg
    impure puts("test2(in, out)").
test2(0::out, _::in) :-
    impure puts("test2(out, in)").
test2(0::out, 0::out) :-
    impure puts("test2(out, out)").

:- impure pred puts(string::in) is det.
puts(_).
:- pragma foreign_proc("C",
    puts(S::in),
    [will_not_call_mercury],
"
    puts(S);
").
