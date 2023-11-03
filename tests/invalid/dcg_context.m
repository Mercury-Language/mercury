%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This test is derived from a piece of code posted to m-users on 2023 oct 28
% that the compiler generated error messages for, but for which those messages
% were hard to understand:
%
% dcg_context.m:076:   The variable `Class' is ground in some branches but not
% dcg_context.m:076:   others.
% dcg_context.m:076:     In this branch, `Class' is free.
% dcg_context.m:076:     In this branch, `Class' is ground.
% dcg_context.m:076:   The variable `MaybeSuper' is ground in some branches but
% dcg_context.m:076:   not others.
% dcg_context.m:076:     In this branch, `MaybeSuper' is free.
% dcg_context.m:076:     In this branch, `MaybeSuper' is ground.
%
% The cause of the bug was the absence of an else part in the nested if.
% Since DCG clauses may have if-thens without elses, this is not itself a bug,
% but it would have been the job of this else part to bind Class and
% MaybeSuper, and mode analysis reported the absence of producers for
% these two variables in the compiler-generated else part of that if-then.
%
% The compiler generated the misleading contexts because it used the
% context of the if-then as a whole, which is the context of the "if" token,
% as the context of both
%
% - the compiler-generated then part, which consists of the actual code
%   of the then-part in the source code, plus unifications of DCG vars, and
% - the compiler-generated else part, which consists of just unifications
%   of DCG vars.
%
% We have no better context to use for the else part, but for the then part,
% we can use the context of the then part in the source code. In most programs,
% this should be different from the context of the "if" token, and the
% error message will be easi*er* to understand. It is still not easy in that
% the context we print for the else part will be at a *smaller* line number
% than the context for the then part, but the part of parse_dcg_goal.m
% that parses such goals has no access to any context that would be more
% meaningful, and such a context may not even exist. (Just consider code
% in which one line contains the whole then part and the parentheses closing
% the if-then, and following line containing the code following the if-then
% as a whole.)
%
% Note that this fix does not help if the then-part is on the same line
% as the "if" token. However, in that case, the programmer can blame
% only himself/herself for the lack of helpfulness of the line numbers
% in the error message :-(
%
%---------------------------------------------------------------------------%

:- module dcg_context.
:- interface.

:- import_module list.
:- import_module maybe.

:- type ps
    --->    ps(int, string).

:- type sr_stack == list(sr).

:- type sr
    --->    tk(int, string)
    ;       sexp(float, list(sr)).

:- pred get_class_super(ps::out, maybe(ps)::out,
    sr_stack::in, sr_stack::out) is det.

:- implementation.

get_class_super(Class, MaybeSuper) -->
    ( if [tk(Cp, Cb)] then
        { Class = ps(Cp, Cb) },
        { MaybeSuper = no }
    else if [sexp(_, [tk(Cp, Cb), tk(Sp, Sb)])] then
        { Class = ps(Cp, Cb) },
        { MaybeSuper = yes(ps(Sp, Sb)) }
    % The bug is that there is no else case here.
    ).
