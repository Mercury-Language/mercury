%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test for bug #455.
% Compile with: mmc --intermod-opt -m bug455.
%
%   Making Mercury/cs/bug455_mod_a.c
%   Uncaught Mercury exception:
%   Software Error: check_hlds.simplify.simplify_goal_call: predicate
%   `check_hlds.simplify.simplify_goal_call.simplify_make_binary_op_goal_expr'/8:
%   Unexpected: cannot find unchecked_left_shift
%
% The problem was that the list of predicates that simplification may introduce
% did not include the uint and fixed size integers.
%

:- module bug455.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bug455_helper_1.

main(!IO) :-
    write_int(bug455_helper_1.foo_int, !IO),
    nl(!IO),

    write_int8(bug455_helper_1.foo_int8, !IO),
    nl(!IO),

    write_int16(bug455_helper_1.foo_int16, !IO),
    nl(!IO),

    write_int32(bug455_helper_1.foo_int32, !IO),
    nl(!IO),

    write_int64(bug455_helper_1.foo_int64, !IO),
    nl(!IO),

    write_uint(bug455_helper_1.foo_uint, !IO),
    nl(!IO),

    write_uint8(bug455_helper_1.foo_uint8, !IO),
    nl(!IO),

    write_uint16(bug455_helper_1.foo_uint16, !IO),
    nl(!IO),

    write_uint32(bug455_helper_1.foo_uint32, !IO),
    nl(!IO),

    write_uint64(bug455_helper_1.foo_uint64, !IO),
    nl(!IO).
