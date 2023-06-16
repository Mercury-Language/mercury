%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Mercury 0.7.3 generated incorrect code for this test case.
%

:- module func_ctor_ambig.
:- interface.
:- import_module io.

:- type t.
:- func bar = int.
:- func baz = t.
:- func bar2 = int.
:- func baz2 = t.

:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

main(!IO) :-
    io.print("bar = ", !IO),  io.print_line(bar, !IO),
    io.print("bar2 = ", !IO), io.print_line(bar2, !IO),
    io.print("baz = ", !IO),  io.print_line(baz, !IO),
    io.print("baz2 = ", !IO), io.print_line(baz2, !IO).

:- type t
    --->    ambig.

:- func ambig = int.

ambig = 42.

bar = ambig.

baz = ambig.

bar2 = func_ctor_ambig.ambig.

baz2 = func_ctor_ambig.ambig.
