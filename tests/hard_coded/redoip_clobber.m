%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a code generation bug, fixed
% on 16 november 1999.
%
% The bug was that if the failure continuation was a known address,
% the code in code_info.m for implementing commits assumed that the
% redoip slot of the top nondet stack frame had this address in it,
% and that therefore it could override this slot without remembering
% what was originally in it.
%
% The determinism declarations of foo and bar are intentionally too loose;
% the bug does not reveal itself, even if it exists, with the correct, tight
% determinism declarations. The test for the bug also requires compilation
% with inlining turned off.

:- module redoip_clobber.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module std_util.

:- pred foo(int).
:- mode foo(out) is nondet.

foo(X) :- bar(X), fail.
foo(X) :- X = 2.

:- pred bar(int).
:- mode bar(out) is nondet.

bar(X) :- X = 1.

:- pred use(int).
:- mode use(in) is semidet.

:- pragma foreign_proc("C",
    use(X::in),
    [will_not_call_mercury, promise_pure],
"
    /*
    ** To exhibit the bug, this predicate needs only to fail.
    ** However, the symptom of the bug is an infinite loop.
    ** To detect the presence of the bug in finite time,
    ** we abort execution if this code is executed too many times.
    **
    ** We mention X here to shut up a warning.
    */

    static int counter = 0;

    if (++counter > 100) {
        MR_fatal_error(""the bug is back"");
    }

    SUCCESS_INDICATOR = MR_FALSE;
").
:- pragma foreign_proc("C#",
    use(_X::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Java",
    use(_X::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = false;
").
:- pragma foreign_proc("Erlang",
    use(_X::in),
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = false
").

main(!IO) :-
    ( if foo(X), use(X) then
        io.write_string("Succeeded.\n", !IO)
    else
        io.write_string("Failed.\n", !IO)
    ).
