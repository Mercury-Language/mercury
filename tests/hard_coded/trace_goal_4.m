%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Check trace goals do not get optimised away unexpectedly.

:- module trace_goal_4.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    ( if
        trace [] impure progress_report(1),
        fail
    then
        true
    else
        trace [] impure progress_report(2)
    ).

:- impure pred progress_report(int::in) is det.

:- pragma foreign_proc("C",
    progress_report(X::in),
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    printf(""Progress reported %"" MR_INTEGER_LENGTH_MODIFIER ""d\\n"", X);
").

:- pragma foreign_proc("C#",
    progress_report(X::in),
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    System.Console.WriteLine(""Progress reported "" + X);
").

:- pragma foreign_proc("Java",
    progress_report(X::in),
    [will_not_call_mercury, thread_safe, tabled_for_io],
"
    System.out.println(""Progress reported "" + X);
").
