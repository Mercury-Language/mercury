%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% A test case to exercise the code for tabling enums.

:- module test_enum.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

:- pragma require_feature_set([memo]).

:- type start_cond_type
    --->    start_cond_initial
    ;       start_cond_ord.

main(!IO) :-
    test(start_cond_initial, 100, !IO),
    test(start_cond_initial, 100, !IO),
    test(start_cond_ord, 200, !IO),
    test(start_cond_ord, 200, !IO).

:- pred test(start_cond_type::in, int::in, io::di, io::uo) is det.

test(Cond, In, !IO) :-
    Out = get_start_state(Cond, In),
    io.write(Cond, !IO),
    io.format(" %d: %d\n", [i(In), i(Out)], !IO).

:- pragma no_inline(get_start_state/2).
:- pragma memo(get_start_state/2).

:- func get_start_state(start_cond_type, int) = int.

get_start_state(StartCond, Anchor) = State :-
    (
        StartCond = start_cond_initial,
        test_exec(Anchor + 10, State)
    ;
        StartCond = start_cond_ord,
        test_exec(Anchor + 20, State)
    ).

:- pred test_exec(int::in, int::out) is det.

:- pragma foreign_proc("C",
    test_exec(In::in, Out::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    printf(""test_exec %"" MR_INTEGER_LENGTH_MODIFIER ""d\\n"", In);
    Out = In + 1;
").
