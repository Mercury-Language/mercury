%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module func_trail_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

:- pragma promise_pure(main/2).
main(!IO) :-
    ( impure trail_test ->
        io.write_string("Success\n", !IO)
    ;
        io.write_string("Failure\n", !IO)
    ).

:- impure pred trail_test is failure.

trail_test :-
    small_int(I),
    impure trail_test_message("before", I, 0),
    impure enter(I),
    small_int(J),
    impure trail_test_message("inside", I, J),
    impure leave(I),
    impure trail_test_message("after", I, J),
    fail.

:- pred small_int(int::out) is multi.

small_int(1).
small_int(2).
small_int(3).

:- impure pred trail_test_message(string::in, int::in, int::in) is det.

:- impure pred enter(int::in) is det.
:- impure pred leave(int::in) is det.

:- pragma foreign_decl("C",
"
    #include <stdio.h>
    #include ""mercury_trail.h""

    void enter_failing(int handle, MR_untrail_reason reason);
    void leave_failing(int handle, MR_untrail_reason reason);
").

:- pragma foreign_proc("C",
    trail_test_message(Prefix::in, I::in, J::in),
    [will_not_call_mercury, will_not_modify_trail],
"
printf(\"%s: %d %d\\n\", (char *)Prefix, (int)I, (int)J);
").

:- pragma foreign_code("C", "

void enter_failing(int handle, MR_untrail_reason reason) {
    switch (reason) {
        case MR_exception:
        case MR_undo:
        case MR_retry:
/*      printf(""enter_failing: exception/undo/retry\n""); */
            printf(\"=> fail back inside: %d\\n\", handle);
        break;
        default:
        printf(\"enter_failing: default\\n\");
        break;
    }
}

void leave_failing(int handle, MR_untrail_reason reason) {
    switch (reason) {
        case MR_exception:
        case MR_undo:
        case MR_retry:
/*      printf(""leave_failing: exception/undo/retry\n""); */
            printf(\"<= fail back outside: %d\\n\", handle);
        break;
        case MR_commit:
        case MR_solve:
        printf(\"leave_failing: commit/solve\\n\");
        break;
        default:
        printf(\"leave_failing: default\\n\");
        /* we may need to do something if reason == MR_gc */
        break;
    }
}
").

:- pragma foreign_proc("C",
    enter(I::in),
    [will_not_call_mercury, may_modify_trail],
"
    printf(\">> enter (%d)\\n\", (int) I);
    MR_trail_function(leave_failing, (void *) I);
").

:- pragma foreign_proc("C",
    leave(I::in),
    [will_not_call_mercury, may_modify_trail],
"
    printf(\"<< leave (%d)\\n\", (int) I);
    MR_trail_function(enter_failing, (void *) I);
").
