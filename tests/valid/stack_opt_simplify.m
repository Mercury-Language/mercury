%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Regression test. `stack_opt_cell' used to call `detect_liveness_proc'
% without doing a simplification on the procedure beforehand:
%
% % mmc --optimize-saved-vars -C stack_opt_simplify.m
% stack_opt_simplify.m:023: In `bar':
% stack_opt_simplify.m:023:   warning: determinism declaration could be tighter.
% stack_opt_simplify.m:023:   Declared `semidet', inferred `erroneous'.
% Uncaught Mercury exception:
% Software Error: goal_info_get_post_births: no code_gen_info

:- module stack_opt_simplify.
:- interface.
:- pred foo is det.

:- implementation.
:- import_module require.

foo :-
    ( if bar then
        true
    else
        true
    ).

    % If declared as `is erroneous' then the abort doesn't occur.
:- pred bar is semidet.

bar :-
    error("bar").
