%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is a regression test. Versions of the compiler before 2016 april 10
% did not generate an error message for the violation of the requirement
% imposed by the require_switch_arms_detism scope below, because common
% subexpression elimination pulled the semidet unifications involving Args
% out of the switch arms, causing two problems: first, the code remaining
% in each switch arm was now det, so the violation had disappeared,
% and second, since the goal inside the require_switch_arms_detism scope
% was now not a switch but a conjunction of the pulled-out-of-the-arms
% unifications and the modified switch, the require_switch_arms_detism scope
% did not actually check anything anyway (since it can check only switches,
% not conjunctions).

:- module require_switch_arms_detism.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

main(!IO) :-
    ( if test_switch_arms(":-", ["a", "b", "c"], Result) then
        io.format("Success: %s\n", [s(Result)], !IO)
    else
        io.format("Failure\n", [], !IO)
    ).

:- pred test_switch_arms(string::in, list(string)::in, string::out) is semidet.

test_switch_arms(Functor, Args, Result) :-
    require_switch_arms_det [Functor]
    (
        Functor = ":-",
        Args = [Arg1, Arg2],
        Result = ":- " ++ Arg1 ++ Arg2
    ;
        Functor = "--->",
        Args = [Arg1, Arg2],
        Result = "---> " ++ Arg1 ++ Arg2
    ;
        Functor = "=",
        Args = [Arg1, Arg2],
        Result = "= " ++ Arg1 ++ Arg2
    ).
