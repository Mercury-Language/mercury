%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
%
% This is cut-down version of the colipa program by Volker Wysk sent to m-rev
% on 3 January 2024.
%
% Reference:
% https://lists.mercurylang.org/archives/reviews/2024-January/024517.html
%
% Compiling this module in a debug grade resulted in:
%
% Uncaught Mercury exception:
% Software Error: predicate `ll_backend.liveness.require_equal'/4:
%    Unexpected: branches of if-then-else disagree on liveness
% First: ArgDesc_3, Values_4, MDef_5, Default_6,
%    TypeClassInfo_for_argument_21, TypeInfo_22_22
% Rest:  ArgDesc_3, Values_4, MDef_5, Default_6, TypeInfo_22_22
%
% Compilation in non-debug grades succeeded.
%
%---------------------------------------------------------------------------%
%
% The problem involved the inner switch on TArgs in the if-then-else.
% The cause was the following.
%
% Debug grades use type_info liveness, which means that type info
% variables describing the type that a type variable (such as T in this case)
% is bound to must remain live as long as a value whose type includes that
% type variable is live. This is also true of the typeclass info variables
% that contain such type infos.
%
% The second argument of the get_from_default predicate is output,
% which means that it is alive even after calls to that predicate.
% Together with the fact that its type includes the type variable T,
% this means that the typeclass info for "argument(T)" must remain live
% until the end of the procedure body.
%
% The error message above reported that the compiler believed that
%
% - TypeClassInfo_for_argument_21 was live at the end of the then part
%   of the if-then-else, which is correct, and that
%
% - TypeClassInfo_for_argument_21 was not live at the end of the else part
%   of the if-then-else, which is ... *arguable*.
%
% The root of the problem is that the program point at the end of the
% else part is not reachable, since all three arms of the switch on TArgs
% throw an exception. (Actually, the compiler sees as two two-way switches,
% one nested in the other, not as one three-way switch, though this detail
% does not matter for this bug.) What variables we say *would* be live
% if execution somehow *did* reach the end of the else part does not really
% matter, since we don't use that info for anything, *with one exception*.
% That exception is that we have sanity checks that insist on all branches
% of a branched control structure agree on the set of live variables at
% the ends of those branches. The compiler abort message is the result
% of the failure of this sanity check.
%
% The liveness phase of the compiler does three or four passes on each
% procedure body:
%
% - the detect_liveness pass, which finds the first reference to each
%   variable that gives it its value,
%
% - the detect_deadness pass, which finds the last reference to each
%   variable that uses its value,
%
% - an optional pass to extend variables' lives, so that mdb can print them
%   if the user asks for them, even after they would normally become dead,
%   and
%
% - a pass to annotate goals that can fail with info about what variables
%   we need to preserve for the program point at which execution resumes
%   after such failure.
%
% In this case, the sanity check that aborted the compiler was in the last
% pass, but the problem was caused by code in the second, which included
% TypeClassInfo_for_argument_21 in the post-death set of the goal that
% took out of it the type info variable for T, thus killing it before
% the end of the else part. It did this because this (compiler-generated)
% goal was the last reference it saw, and *due to all branches through
% the switch on TArgs throwing exceptions*, it knew that the end of the
% procedure body was not reachable. This reasoning was correct, but it
% disregarded the presence of the sanity checks in the last pass.
%
% I (zs) see three possible approaches for fixing this bug.
%
% Approach one would simply disable all sanity checks that insist on
% agreement on the set of live vars at the ends of different branches
% in a branched control structure. This would include not just the sanity
% checks in the fourth pass in the liveness module, but all similar checks
% in all of the compiler's analysis and code generation modules.
% This would be discard a defense against bugs in quite a large part
% of the compiler, and hence would be a pretty bad idea.
%
% Approach two would be to make TypeClassInfo_for_argument_21 live at
% the end of the else part by adding it to the else part goal's post-birth set.
% The post-birth set is specifically designed for this use case; it is
% only ever used to tell the code generator "execution will never reach
% the end of this goal, but consider this variable to be born at the very end
% of it when comparing the set of live variables at the end of this branch
% with the livenesses at the ends of the other branches in this structure".
% This would almost certainly work, but it would mean that this variable
% first dies in the else part, and then gets born again. Besides this being
% inelegant, the seeming resurrection of a previously-dead variable
% has also historically posed problems for the optional delay_death pass.
%
% The fix I (zs) chose uses approach three, which is to make
% TypeClassInfo_for_argument_21 live at the end of the else part
% by preventing it being killed in the first place. It was being killed
% by code that handled the arms of the branched control structures
% (switches, disjunctions, or if-then-else) whose handling of arms that
% cannot succeed was set up to handle situations in which some *other* arm
% of the branched control structure *could* succeed. Its design seems to have
% considered the possibility that *none* of the branches could succeed, but
% the implementation did not :-( The fix is to make the implementation match
% the old design, which is: when we find that the current branch being
% considered cannot succeed but none of other branches can succeed either,
% then don't kill the variables that we used to kill.
%
%---------------------------------------------------------------------------%

:- module bug572.
:- interface.

:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

:- type cla
    --->    cla(string).

:- typeclass argument(T) where [].

:- type argdesc.

%---------------------------------------------------------------------------%

:- pred get_from_default(argdesc::in, list(T)::out) is det <= argument(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module map.
:- import_module maybe.
:- import_module exception.
:- import_module type_desc.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type argdesc
    --->    some [T] argdesc(
                argdesc_default :: maybe(T)
            ).

%---------------------------------------------------------------------------%

get_from_default(ArgDesc, Values) :-
    MDef = ArgDesc ^ argdesc_default,
    (
        MDef = yes(Default),
        ( if
            private_builtin.typed_unify(Default, Val0)
        then
            Values = [Val0]
        else
            ValuesTypeDesc = type_of(Values),
            type_ctor_and_args(ValuesTypeDesc, _, TArgs),
            (
                TArgs = [],
                throw("no type args")
            ;
                TArgs = [_],
                throw("mismatch")
            ;
                TArgs = [_, _ | _],
                throw("too many type args")
            )
        )
    ;
        MDef = no,
        Values = []
    ).

%---------------------------------------------------------------------------%
