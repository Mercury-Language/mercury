%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for github issue #65.
%
% Any copyright is dedicated to the Public Domain.
% http://creativecommons.org/publicdomain/zero/1.0/
%
% Released by Transnat Games for testing purposes.
%
% Causes the compiler to abort with the message:
%
% Software Error: ml_backend.ml_code_gen:
%   predicate `ml_backend.ml_code_gen.ml_gen_maybe_convert_goal_code_model'/7:
%   Unexpected: semi in det
%
% Removing the :- pragma inline in the implementation, changing our special
% insts to just di/uo or in/out, dummying the body of find_bind any further,
% removing EITHER of the functors of bind/0, replacing the element type with
% something like a list of strings, or making the functors of bind/0 hold
% predicates of the same type stops the abort.
%
% So yes, even though this is pretty complex, it seems like removing close to
% anything still here stops the assert from happening. I guess I got "lucky"
% finding this one at all?
%
%---------------------------------------------------------------------------%
%
% The abort happened when compiling the reduce predicate.
% The root cause was the interaction of two things.
%
% - The invocation of BPred generates an inst for ReduceResult
%   (bind_result_unique_error) that lists the three possible cons_ids
%   it can be bound to (ok, error1 and error2) *without module qualification*.
%
% - When the following call to bind_result is inlined, the result is a switch
%   on ReduceResult whose arms list those same three function symbols,
%   but in a *fully module qualified form*.
%
% The unqualified vs qualified mismatch led the pre-code-generation invocation
% of simplification to conclude that *none* of the arms of the switch
% were actually reachable. It therefore deleted the switch and replaced it
% with "fail". This meant that the Bind = b(...) arm of the switch on Bind
% could fail, which in turn made the whole body of reduce semidet.
% The code generator expected the body goal to be det, hence the abort.
%
% The fix was change the code that pushes types into bound insts
% to copy the module qualifier from the type constructor (which is required to
% be module qualified) into the bound insts (which are *not* required to 
% be module qualified). This eliminates the mismatch.
%
%---------------------------------------------------------------------------%

:- module gh65.

:- interface.

:- import_module list.
:- use_module rbtree.
:- use_module maybe.

%---------------------------------------------------------------------------%

:- type element
    --->    atom(string)
    ;       list(list.list(element)).

:- type result == maybe.maybe_error(element).

%---------------------------------------------------------------------------%

:- inst maybe_unique_error
    --->    maybe.ok(ground)
    ;       maybe.error(unique).

:- inst maybe_clobbered_error
    --->    maybe.ok(ground)
    ;       maybe.error(clobbered).

:- mode res_uo == free >> maybe_unique_error.
:- mode res_di == maybe_unique_error >> maybe_clobbered_error.

%---------------------------------------------------------------------------%

:- type bind_result(T, E)
    --->    ok(T)
    ;       error2(reduce :: T, execute :: E)    % Reduce OK, execute error.
    ;       error1(E).

:- type bind_result == bind_result(element, string).

:- inst bind_result_unique_error
    --->    ok(ground)
    ;       error2(ground, unique)
    ;       error1(unique).

:- inst bind_result_clobbered_error
    --->    ok(ground)
    ;       error2(ground, clobbered)
    ;       error1(clobbered).

:- mode bind_res_uo == free >> bind_result_unique_error.
:- mode bind_res_di == bind_result_unique_error >> bind_result_clobbered_error.

%---------------------------------------------------------------------------%

:- type bind
    --->    a(
                pred(list.list(element)::in, result::res_uo,
                    tree::in, tree::out) is det
            )
    ;       b(
                pred(list.list(element)::in, bind_result::bind_res_uo,
                    tree::in, tree::out) is det
            ).

:- type bind_spec
    --->    variadic(string)
    ;       args(string, int).

:- type tree == rbtree.rbtree(bind_spec, bind).

%---------------------------------------------------------------------------%

:- pred reduce(element, result, tree, tree).
:- mode reduce(in, res_uo, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.
%---------------------------------------------------------------------------%

:- use_module exception.
:- use_module int.
:- use_module string.

%---------------------------------------------------------------------------%

:- type process
    --->    execute
    ;       reduce.

%---------------------------------------------------------------------------%

reduce(atom(T), maybe.ok(atom(T)), !Tree).
reduce(list(Elements), Result, !Tree) :-
    ( if
        Elements = [atom(Tag) | Tail],
        find_bind(Tag, !.Tree, Bind)
    then
        (
            Bind = a(APred),
            APred(Tail, Result, !Tree)
        ;
            Bind = b(BPred),
            BPred(Tail, ReduceResult, !Tree),
            bind_result(reduce, ReduceResult, Result)
        )
    else
        Result = maybe.ok(list(Elements))
    ).

%---------------------------------------------------------------------------%

:- pred find_bind(string, rbtree.rbtree(bind_spec, bind), bind).
:- mode find_bind(in, in, out) is semidet.

find_bind(Name, Tree, Out) :-
    Variadic = variadic(Name),
    rbtree.search(Tree, Variadic, Out).

%---------------------------------------------------------------------------%

:- pred bind_result(process, bind_result(T, E), maybe.maybe_error(T, E)).
:- mode bind_result(in, bind_res_di, res_uo) is det.

:- pragma inline(bind_result/3).

bind_result(Process, BindResult, Result) :-
    (
        BindResult = ok(T),
        Result = maybe.ok(T)
    ;
        BindResult = error2(T, Error),
        (
            Process = reduce,
            Result = maybe.ok(T)
        ;
            Process = execute,
            Result = maybe.error(builtin.unsafe_promise_unique(Error))
        )
    ;
        BindResult = error1(Error),
        Result = maybe.error(builtin.unsafe_promise_unique(Error))
    ).

%---------------------------------------------------------------------------%
