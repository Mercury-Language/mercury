%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Rotd-2011-11-21 (and a bit before) apply the direct arg functor optimisation
% to the outer_public/0 type in the daf_bug_helper_2 submodule but not in its
% parent module, daf_bug_helper_1,
% "Parent Value" and "Child Value" below should be identical.
%

:- module daf_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module daf_bug_helper_1.
:- import_module daf_bug_helper_1.daf_bug_helper_2.

main(!IO) :-
    io.write_string("Parent Value = ", !IO),
    io.write_line(parent_value, !IO),
    io.write_string("Child Value =  ", !IO),
    io.write_line(child_value, !IO),

    io.write_string("Parent Value2 = ", !IO),
    io.write_line(parent_value2, !IO),
    io.write_string("Child Value2 =  ", !IO),
    io.write_line(child_value2, !IO).
