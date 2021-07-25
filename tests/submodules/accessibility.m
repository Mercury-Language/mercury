%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A regression test -- the compiler issues a spurious error for this test case.

:- module accessibility.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module sub_a.

:- module nested.
:- interface.

% This line should _not_ be needed,
% since it already occurs in the containing module.
% :- import_module sub_a.

:- import_module sub_a.sub1.
:- type t3 == sub_a.sub1.t2.
:- end_module nested.

main(!IO) :-
    io.write_string("Hello.\n", !IO).
