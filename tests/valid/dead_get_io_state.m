%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Before the fix committed on 2023 oct 6, this code resulted in
% this compiler abort in LLDS grades:
%
%   Uncaught Mercury exception:
%       Software Error: predicate
%           `ll_backend.var_locn.clobber_lval_in_var_state_map'/6:
%       Unexpected: empty state
%
% The chain of events that lead to this abort was the following.
%
% - The trace goal transformation in goal_expr_to_goal.m adds
%
%   - a call to unsafe_get_io_state just before the call to string.format and
%   - a call to unsafe_set_io_state just after the call to unexpected.
%
%   Since the goal inside the trace goal's scope does not refer to !IO,
%   the variable returned by unsafe_get_io_state as the initial I/O state
%   is also the variable passed back to unsafe_set_io_state.
%
%   At this point, this variable is a nonlocal variable in both of these calls.
%
% - Mode checking, noticing that the call to unexpected cannot succeed,
%   deletes the call to unsafe_set_io_state. This makes the variable
%   returned by unsafe_get_io_state *local* to that call.
%
% - Simplification declines to delete the call to unsafe_get_io_state,
%   because goal_can_loop_or_throw_imaf returns can_loop_or_throw for this
%   call, even though this builtin cannot do either of those things.
%
% - The LLDS code generator recognizes that unsafe_get_io_state is a builtin,
%   and invokes generate_builtin to generate code for. generate_builtin
%   applied the builtin's translation by magically binding the variable
%   representing the I/O state WITHOUT CHECKING WHETHER THIS VARIABLE IS LIVE.
%
% - The call to the string.format (or rather, the calls that format_call.m
%   expands the call to string.format into) require flushing live variables
%   to the stack, since the call may clobber all the registers. The variable
%   bound by unsafe_get_io_state had no stack slot (values of dummy types never
%   do) and was not known to be a constant, so having the call clobbering
%   all the registers would have destroyed its state. This is why the code
%   generator raised the exception shown above.
%
% The fix was to make generate_builtin ignore bindings if they are made to
% variables that are ignored outside the builtin.
%

:- module dead_get_io_state.
:- interface.

:- pred test(int::in, int::in, int::out) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

test(A, B, Z) :-
    ( if A < B then
        trace [io(!IO)] (
            string.format("A = %d, B = %d\n", [i(A), i(B)], Msg),
            unexpected($pred, Msg)
        ),
        Z = A
    else
        Z = B
    ).
