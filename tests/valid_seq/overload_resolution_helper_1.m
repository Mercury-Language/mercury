%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module overload_resolution_helper_1.

:- interface.

:- import_module int.
:- pred baz(int::in) is semidet.

:- func plusone(int :: in) = (int :: out) is det.

:- implementation.

:- type t
    --->    f(int)
    ;       g(int1).

    % Check that local types used only in other type declarations are put
    % in the `.opt' file.
:- type int1
    --->    int1(int).

:- mode int_mode == int_mode1.
:- mode int_mode1 == in.

baz(X) :-
    T = f(1),
    bar(T, X).

:- pred bar(t::in, int::int_mode) is semidet.

bar(T, 2) :-
    Pred =
        ( pred(T1::in, Int::int_mode) is semidet :-
            T1 = f(1),
            Int = 2
        ),
    Pred(T, 2).

% One version of the compiler incorrectly wrote this declaration to the
% .opt file as `:- pragma inline((overload_resolution_helper_1:plusone)/2).'
%       -- bromage  20 Nov 1997
:- pragma inline(plusone/1).

plusone(Int0) = Int :-
    Int = Int0 + 1.
