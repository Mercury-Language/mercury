%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module test_repn.
:- interface.

:- import_module test_repn_sub.

:- import_module char.

:- type only_functor_local1
    --->    ofl1_f1(
                animal, d, int8, animal, uint32, animal
            ).

:- type only_functor_local2
    --->    ofl2_f1(
                nt(d), animal, int8, uint16
            ).

:- type only_functor_remote1
    --->    ofr1_f1(                                    % on 64 bit machines
                animal, animal, d,                      % word 0
                int,                                    % word 1
                string,                                 % word 2
                animal, int8, animal, uint32, animal    % word 3
            ).

:- type only_functor_remote2
    --->    ofr2_f1(                                    % on 64 bit machines
                d, nt(animal), d, animal, d,            % word 0
                int,                                    % word 1
                string,                                 % word 2
                animal, int8, animal, uint32, animal    % word 3
            ).

:- type more_functors_local1
    --->    mfl1_f1(
                animal, int8, animal, uint32, animal
            )
    ;       mfl1_f2(
                animal, animal
            ).

:- type more_functors_local2
    --->    mfl2_f1
    ;       mfl2_f2
    ;       mfl2_f3
    ;       mfl2_f4(
                animal, int8, animal, uint32, animal
            )
    ;       mfl2_f5(
                animal, animal
            ).

:- type more_functors_remote1
    --->    mfr1_f1
    ;       mfr1_f2
    ;       mfr1_f3
    ;       mfr1_f4(
                int, int
            )
    ;       mfr1_f5(
                int, float
            ).

:- type more_functors_local_remote1
    --->    mflr1_f1
    ;       mflr1_f2
    ;       mflr1_f3
    ;       mflr1_f4(
                char, animal
            )
    ;       mflr1_f5(
                int, int
            ).

:- type more_functors_local_remote2
    --->    mflr2_f1
    ;       mflr2_f2
    ;       mflr2_f3
    ;       mflr2_f4(
                char, animal
            )
    ;       mflr2_f5(
                float, int, float
            ).

:- type more_functors_local_remote3
    --->    mflr3_f1
    ;       mflr3_f2
    ;       mflr3_f3(
                char, animal
            )
    ;       some [A, B] mflr3_f4(
                int8, animal, d, A, B, float, int, float
            )
    ;       mflr3_f5(
                int, int
            )
    ;       mflr3_f6(
                int, int
            )
    ;       mflr3_f7(
                int, int
            )
    ;       mflr3_f8(
                int, int
            )
    ;       mflr3_f9(
                int, int
            )
    ;       mflr3_f10(
                int, int
            )
    ;       some [C, D, E] mflr3_f11(
                nt(d), animal, char, C, D, E, int, int
            )
    ;       mflr3_f12(
                eqv(nt(eqv(d))), animal, char, int, int
            )
    ;       mflr3_f13(
                animal, nt(d), char, int, int
            )
    ;       mflr3_f13(
                animal, char, nt(nt(d)), int, int
            ).

:- type more_functors_local_remote_da1
    --->    mflrd1_f1
    ;       mflrd1_f2
    ;       mflrd1_f3
    ;       mflrd1_f5(
                wa
            )
    ;       mflrd1_f6(
                wa
            )
    ;       mflrd1_f7(
                wa
            )
    ;       mflrd1_f8(
                wa
            )
    ;       mflrd1_f9(
                wa
            )
    ;       mflrd1_f10(
                wa
            )
    ;       mflrd1_f11(
                int, int
            ).

:- type abs.
:- pragma foreign_type("C", abs, "void *",
    [can_pass_as_mercury_type, word_aligned_pointer]).
:- pragma foreign_type("C#", abs, "object",
    [can_pass_as_mercury_type]).
