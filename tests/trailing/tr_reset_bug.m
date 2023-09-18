%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 et
%---------------------------------------------------------------------------%
%
% MR_reset_trail() in rotd-2008-09-04 and before
% was not resetting MR_trail_ptr.
%

:- module tr_reset_bug.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.

main(!IO) :-
    save_trail_ptr(!IO),
    ( add_trail_entries(10) ->
        do_success_stuff(Result, !IO),
        (
            Result = yes,
            io.write_string("passed\n", !IO)
        ;
            Result = no,
            io.write_string("failed\n", !IO)
        )
    ;
        true
    ).

:- pred save_trail_ptr(io::di, io::uo) is det.
:- pragma foreign_proc("C",
    save_trail_ptr(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    pre = MR_trail_ptr;
").

:- pred add_trail_entries(int::in) is semidet.
:- pragma foreign_proc("C",
    add_trail_entries(N::in),
    [will_not_call_mercury, promise_pure],
"

    MR_Integer i;

    for (i = 0; i < N; i++) {
        MR_trail_function(my_func, (void *) i)
    }
    SUCCESS_INDICATOR = MR_TRUE;
").

:- pragma foreign_decl("C", "

extern MR_TrailEntry *pre;

extern void
my_func(void *data, MR_untrail_reason reason);

").

:- pragma foreign_code("C", "

MR_TrailEntry *pre;

void
my_func(void *data, MR_untrail_reason reason)
{
}
").

:- pred do_success_stuff(bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    do_success_stuff(Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    MR_TrailEntry *post;

    MR_reset_trail();
    post = MR_trail_ptr;

    Result = (post == pre) ? MR_YES : MR_NO;
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
