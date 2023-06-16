%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Versions of the compiler up to April 19, 2006 had a bug that caused this
% program to crash. The bug was that when frameopt wanted to find out whether
% a block of instructions referred to stack variables, it did not look past
% pragma_c_code LLDS instructions. As a result, the generated code included
% a (redundant) assignment to a stack variable in a section of code that,
% after frameopt, did not have a stack frame anymore. It therefore overwrote
% part of its caller's stack frame, which caused a crash.
%
% The bug showed up in YesLogic's Prince, and was isolated to this test case
% by Michael Day. The bug actually occurred when optimizing get_max_width in
% prince_frameopt_css.style.m.

:- module prince_frameopt.

:- interface.

:- import_module io.

:- pred main(io, io).
:- mode main(di, uo) is det.

:- implementation.

:- import_module string.

:- import_module prince_frameopt_helper_1.
:- import_module prince_frameopt_helper_1.prince_frameopt_helper_2.

main(!IO) :-
    io.write_string("About to crash\n", !IO),
    PRules = new_prules,
    io.write_line(PRules, !IO),
    io.write_string("Done\n", !IO).
