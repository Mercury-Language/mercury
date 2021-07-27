%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test case is a cut down version of magicbox.m:
%
%   Gillian Tee     : pptee@cs.mu.oz.au
%   Leslie Gondor   : leslieg@cs.mu.oz.au
%
%   433-481 - Agent Programming Languages - Project 1
%   Department of Computer Science and Software Engineering
%   The University of Melbourne
%   March/April, 2004
%
%---------------------------------------------------------------------------%
%
% The problem this test case tests for is an overstrong sanity check in
% determinism analysis. The problem scenario is as follows:
%
% 1. The arguments_handler predicate is in a first_soln context.
% 2. So is the outer if-then-else, and its then-part and else-part.
% 3. The then-part, the call to string.to_int, can fail.
% 4. The condition of the other if-then-else is thus in an all_soln context.
% 5. Parts 3 and 4 also hold for the inner if-then-else, making it naturally
%    nondet, which is converted into cc_nondet by its being inside a first_soln
%    context.
% 6. The then part of the outer if-then-else is semidet, the else part is
%    cc_nondet. The sanity check complains that only one of the two arms of the
%    switch seems to be in first_soln context. It doesn't see that the then
%    part is in a first_soln context too; its detism doesn't say that.

:- module magicbox.

:- interface.

:- import_module io.

:- pred arguments_handler(io::di, io::uo, int::out) is cc_multi.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module string.

arguments_handler(!IO, Length) :-
    % Check for correct command-line arguments:
    io.command_line_arguments(Arguments, !IO),
    ( if
        % Valid arguments - Scenario 1
        list.member("-1", Arguments, ArgsSubS1),
        list.index0(ArgsSubS1, 1, LengthStr)
    then
        string.to_int(LengthStr, Length)
    else if
        % Valid arguments - Scenario 2
        list.member("-2", Arguments, ArgsSubS2),
        list.index0(ArgsSubS2, 2, LengthStr)
    then
        string.to_int(LengthStr, Length)
    else
        Length = 0
    ).
