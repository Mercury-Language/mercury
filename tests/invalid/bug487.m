%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module bug487.
:- interface.

:- type scenario.
:- type jsolution.

:- pred do_reconstruct_route(scenario::in, jsolution::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module require.

%---------------------------------------------------------------------------%

:- type jsolution
    --->    jsolution.
:- type solution
    --->    solution.
:- type scenario
    --->    scenario.

do_reconstruct_route(Scenario, JSolution) :-
    some [!SS] (
        % The error is caused by this line missing.
        % create_state(!:SS),
        ( if
            construct_solution(Scenario, Solution, !SS)
        then
            solution_to_java(Solution, JSolution, !.SS, _)
        else
            unexpected($pred, "should not get here")
        )
    ).

:- pred create_state(ss::muo) is det.

create_state(ss).

:- pred construct_solution(scenario::in, solution::out, ss::mdi, ss::muo)
    is semidet.

construct_solution(_, solution, !SS) :-
    semidet_fail.

:- type ss
    --->    ss.

:- pred solution_to_java(solution::in, jsolution::out, ss::mdi, ss::muo)
    is det.

solution_to_java(_, jsolution, !SS).

%---------------------------------------------------------------------------%
:- end_module bug487.
%---------------------------------------------------------------------------%
