%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug #548.
% The bug is described below at its point of occurrence.
%

:- module bug548.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

    % This type has more nonconstant constructors than we have
    % primary tag values, forcing the constructors at the end
    % to share a single primary tag value.
    %
    % For this code to test the bug we want to test,
    %
    % - f0 should have one ptag value to itself, and
    % - f9 should use another ptag value, which it should share with other
    %   data constructors.
    %
    % This type definition should ensure that, regardless of whether
    % the platform is 32 bit or 64 bit.
    %
    % Type definitions in general are more likely to happen to have tag pairs
    % like that on 32 bit platforms, and in fact this bug was noticed during
    % a bootcheck in the hlc.gc.pregen grade, which forces tag allocation
    % to pretend we are on a 32 bit platform.
    %
    % Giving each data constructor two arguments just ensures that
    % no constructor is allocated direct_arg tags.
:- type t
    --->    f0(int, int)
    ;       f1(int, int)
    ;       f2(int, int)
    ;       f3(int, int)
    ;       f4(int, int)
    ;       f5(int, int)
    ;       f6(int, int)
    ;       f7(int, int)
    ;       f8(int, int)
    ;       f9(int, int).

main(!IO) :-
    test_pred(f0(1, 2), [3, 7], 10, N),
    io.format("%d\n", [i(N)], !IO).

    % This predicate is a cut-down version of gen_aux_proc_goal
    % in loop_inv.m in the Mantis bug report.
    %
:- pred test_pred(t::in, list(int)::in, int::in, int::out) is det.
:- pragma no_inline(pred(test_pred/4)).

test_pred(T, List, A, N) :-
    (
        ( T = f1(_, _)
        ; T = f2(_, _)
        ; T = f3(_, _)
        ; T = f4(_, _)
        ; T = f5(_, _)
        ; T = f6(_, _)
        ; T = f7(_, _)
        ; T = f8(_, _)
        ),
        N = 11
    ;
        ( T = f0(_, _)
        ; T = f9(_, _)
        ),
        % The two bugs we are testing for occurred when ml_tag_switch.m
        % duplicated the code of this switch arm for two different
        % primary tag values, one unshared (as is f0) and one shared
        % (as is f9).
        %
        % One bug was that auxiliary MLDS functions inside divisible_by_some
        % were generated once and then used twice, resulting in duplicate
        % function definitions.
        %
        % The other bug involved the fact that the local variables of this arm
        % are put into an environment structure by the implementation of the
        % commit implicit in the "some [I]" quantification inlined here.
        % When this arm is duplicated, we get duplicate field names in this
        % environment structure *even if we do not duplicate the MLDS code
        % of this case, but generate it fresh for each ptag*, because
        %
        % - the name of each MLDS variable representing a HLDS variable
        %   is constructed solely from the HLDS name and number of the HLDS
        %   variable, and
        %
        % - the name of each field in the environment structure that represents
        %   an MLDS variable is constructed solely from the name of that MLDS
        %   variable.
        %
        % At neither stage did we add any kind of suffix to ensure the
        % uniqueness of the name. (For the problem with duplicate aux function
        % definitions, generating the code of this arm fresh for each ptag
        % does fix the problem, because for aux MLDS functions, we *do*
        % add a unique numerical suffix to their name.)
        ( if divisible_by_some(List, A) then
            N = A
        else
            N = 42
        )
    ).

    % This predicate plays the role of invariant_goal in loop_inv.m
    % in the Mantis bug report.
    %
:- pred divisible_by_some(list(int)::in, int::in) is semidet.
:- pragma inline(pred(divisible_by_some/2)).

divisible_by_some(List, N) :-
    some [I] (
        % The code inside this scope is nondet, which is why
        % the MLDS code we generate for it includes both an environment
        % structure (for model_non continuations) and a commit.
        list.member(I, List),
        N mod I = 0
    ).
