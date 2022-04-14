%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Compiling this module should generate an error message since
% it tries to cast a non-standard func inst to ground.

:- module ho_default_func_2.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- include_module ho_default_func_2.sub.
:- include_module ho_default_func_2.id.

:- import_module ho_default_func_2.sub.
:- import_module ho_default_func_2.id.

:- import_module int.
:- import_module std_util.

main(!IO) :-
    baz(IdF),
    eq(getval(IdF), F),
    do_io(F, !IO).

:- func foo(int) = int.

foo(X) = X + 1.

:- func bar(int) = int.
:- mode bar(out) = in is det.

bar(X) = X + 1.
