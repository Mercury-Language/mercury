%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module int_impl_imports_helper_1.
:- interface.

:- type bar == int.

:- func one_bar = bar.
:- func add_bars(bar, bar) = bar.

:- implementation.

:- import_module int.

one_bar = 42.

add_bars(A, B) = A + B.
