/*
** vim: ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include "mercury_imp.h"
#include "mercury_typeclass_info.h"

/*

As an example, consider the following declarations, which we want to be able
to support in the future, although they are not allowed by the current system:

:- module sample.

:- typeclass foo(T1, T2) <= bar(T1), baz(T2), quux(float, T1) where [
    pred method1(T1::in, T2::in, W::in, W::out) is det,
    func method2(list(T1)::in, int::in, T2::in) = T::out is det
].

:- instance foo(list(U1), T2) <= foo(U1, T2), boo(int, U1).

We should generate the following data structures from the above declarations.
(The names of the global variables are tentative.)

*/

extern	struct MR_TypeClass_Struct
                        mercury_data__type_class__sample__bar__1;
extern	struct MR_TypeClass_Struct
                        mercury_data__type_class__sample__baz__1;
extern	struct MR_TypeClass_Struct
                        mercury_data__type_class__sample__quux__2;
extern	struct MR_TypeCtorInfo_Struct
                        mercury_data__type_ctor_info__builtin__int_0;
extern	struct MR_TypeCtorInfo_Struct
                        mercury_data__type_ctor_info__builtin__float_0;

const MR_TypeClassMethod mercury_data__type_class_method__sample__foo_2_1 =
{
    "method1",
    4,
    MR_PREDICATE
};

const MR_TypeClassMethod mercury_data__type_class_method__sample__foo_2_2 =
{
    "method2",
    4,
    MR_FUNCTION
};

const MR_ConstString mercury_data__type_class_id_tvar_names__sample__foo_2[] =
{
    "T1",
    "T2",
    "W",
};

const MR_TypeClassMethod *mercury_data__type_class_id_methods__sample__foo_2[] =
{
    /* the form of what goes here is backend-dependent */
    &mercury_data__type_class_method__sample__foo_2_1,
    &mercury_data__type_class_method__sample__foo_2_2,
};

const MR_TypeClassId mercury_data__type_class_id__sample__foo__2 =
{
    "sample",
    "foo",
    2,
    3,                  /* T1, T2, W */
    2,
    mercury_data__type_class_id_tvar_names__sample__foo_2,
    mercury_data__type_class_id_methods__sample__foo_2
};

const MR_TypeClassSkel_1Struct
mercury_data__type_class_skel__sample__bar__1_var_1
= {
    &mercury_data__type_class__sample__bar__1,
    {
        (MR_PseudoTypeInfo) 1,
    }
};

const MR_TypeClassSkel_1Struct
mercury_data__type_class_skel__sample__baz__1_var_2
= {
    &mercury_data__type_class__sample__baz__1,
    {
        (MR_PseudoTypeInfo) 2,
    }
};

const MR_TypeClassSkel_2Struct
mercury_data__type_class_skel__sample__quux__2_float_var_1
= {
    &mercury_data__type_class__sample__quux__2,
    {
        (MR_PseudoTypeInfo) &mercury_data__type_ctor_info__builtin__float_0,
        (MR_PseudoTypeInfo) 1,
    }
};

const MR_TypeClassSkel mercury_data__type_class_supers__sample__foo__2[] =
{
    MR_STD_TYPECLASS_SKEL_ADDR(
        mercury_data__type_class_skel__sample__bar__1_var_1),
    MR_STD_TYPECLASS_SKEL_ADDR(
        mercury_data__type_class_skel__sample__baz__1_var_2),
    MR_STD_TYPECLASS_SKEL_ADDR(
        mercury_data__type_class_skel__sample__quux__2_float_var_1),
};

MR_TypeClassStruct mercury_data__type_class__sample__foo__2 =
{
    &mercury_data__type_class_id__sample__foo__2,
    3,
    mercury_data__type_class_supers__sample__foo__2
};

const MR_PseudoTypeInfo
mercury_data__instance_args__sample__foo__2__list__list__1_var_1_var_2[]
= {
    /* <pseudotypeinfo for list(1)>, */
    (MR_PseudoTypeInfo) 0,
    (MR_PseudoTypeInfo) 2,
};

const MR_TypeClassSkel_2Struct
mercury_data__type_class_skel__sample__foo__2_var_1_var_2
= {
    &mercury_data__type_class__sample__foo__2,
    {
        (MR_PseudoTypeInfo) 1,
        (MR_PseudoTypeInfo) 2,
    }
};

const MR_TypeClassSkel_2Struct
mercury_data__type_class_skel__sample__boo__2_int_var_1
= {
    &mercury_data__type_class__sample__foo__2,
    {
        (MR_PseudoTypeInfo) &mercury_data__type_ctor_info__builtin__int_0,
        (MR_PseudoTypeInfo) 1,
    }
};

const MR_TypeClassSkel
mercury_data__instance_constraints__sample__foo__2__list__list__1_var_1_var_2[]
= {
    MR_STD_TYPECLASS_SKEL_ADDR(
        mercury_data__type_class_skel__sample__foo__2_var_1_var_2),
    MR_STD_TYPECLASS_SKEL_ADDR(
        mercury_data__type_class_skel__sample__boo__2_int_var_1),
};

MR_InstanceStruct
mercury_data__instance__sample__foo__2__list__list__1_var_1_var_2
= {
    &mercury_data__type_class__sample__foo__2,
    mercury_data__instance_args__sample__foo__2__list__list__1_var_1_var_2,
    2,              /* U1, T2 */
    2,
    mercury_data__instance_constraints__sample__foo__2__list__list__1_var_1_var_2,
    /* the form of the methods in the method block is backend-dependent */
    /* <method block pointer> */
    NULL
};
