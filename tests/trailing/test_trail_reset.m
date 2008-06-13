% vim: ft=mercury ts=4 et
:- module test_trail_reset.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    ( test(10, _) ->
        io.write_string("Call to test/1 succeeded\n", !IO)
    ;
        io.write_string("Call to test/1 failed\n", !IO)
    ),
    reset_trail(!IO).

:- pred test(int::in, int::out) is semidet.

test(X, 3) :-
    add_trail_entry(X),
    add_trail_entry(X + 1).

:- pragma foreign_decl("C", "

#include \"mercury_trail.h\"
#include <stdio.h>

extern void
foo(void *, MR_untrail_reason);

").

:- pred add_trail_entry(int::in) is semidet.

:- pragma foreign_proc("C",
    add_trail_entry(X::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_trail_function(foo, (void *) X);
    SUCCESS_INDICATOR = MR_TRUE;
").

:- pred reset_trail(io::di, io::uo) is det.

:- pragma foreign_proc("C",
    reset_trail(IO0::di, IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MR_reset_trail();
    IO = IO0;
").

:- pragma foreign_code("C", "

void
foo(void *value, MR_untrail_reason reason)
{
    printf(\"calling function trail entry with %ld: \",
        (long) value);

    switch (reason) {
    
        case MR_commit:
            printf(\"commit\\n\");
            break;
                
        case MR_gc:
            printf(\"gc\\n\");
            break;

        default:
            printf(\"unexpected trail reason\");
    }
}
").
