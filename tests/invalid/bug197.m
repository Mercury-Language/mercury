%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The following program causes Mercury 11.01 to abort with
% Uncaught Mercury exception:
% Software Error: hlds.make_hlds.state_var: predicate
% `hlds.make_hlds.state_var.handle_arm_updated_state_vars'/7:
% Unexpected: BeforeStatus = status_known_ro
%
% (Compile with mmc -C.)

:- module bug197.
:- interface.

:- type ss
    --->    ss.

:- implementation.

:- import_module list.

:- pred foo(string::in, list(T)::in, ss::mdi, ss::muo) is semidet.

foo(Ls, !SS) :-
    list.foldl(
        ( pred(Name::in, L::in, !.A::in, !:A::out) is semidet :-
            (
                Name = "foo"
            ;
                Name = "bar",
                bar(L, !SS)
            )
        ), Ls, 10, _).

:- pred bar(T::in, ss::mdi, ss::muo) is semidet.

bar(_, !SS) :-
    semidet_fail.
