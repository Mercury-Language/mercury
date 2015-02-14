%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Rotd-2011-11-21 (and a bit before) apply the direct arg functor optimisation
% to the outer_public/0 type in the daf_bug_sub submodule but not in its
% parent, daf_bug_parent, module.
% "Parent Value" and "Child Value" below should be identical.
%
:- module daf_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module daf_bug_parent.
:- import_module daf_bug_parent.daf_bug_sub.

main(!IO) :-
    io.write_string("Parent Value = ", !IO),
    io.write(parent_value, !IO),
    io.nl(!IO),
    io.write_string("Child Value =  ", !IO),
    io.write(child_value, !IO),
    io.nl(!IO),

    io.write_string("Parent Value2 = ", !IO),
    io.write(parent_value2, !IO),
    io.nl(!IO),
    io.write_string("Child Value2 =  ", !IO),
    io.write(child_value2, !IO),
    io.nl(!IO).
