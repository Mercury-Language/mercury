/*
** Copyright (C) 2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
**
*/

#ifndef MERCURY_TYPE_DESC_H
#define MERCURY_TYPE_DESC_H

#include "mercury_wrapper.h" /* for MR_address_of_type_ctor_info_for_pred ... */
#include "mercury_types.h"
#include "mercury_type_info.h"

/*
** Values of type `types:type_desc' are represented the same way as
** values of type `private_builtin:type_info' (this representation is
** documented in compiler/polymorphism.m). Some parts of the library
** (e.g. the gc initialization code) depend on this.
** The C type corresponding to these Mercury types is `MR_TypeInfo'.
**
** Values of type `types:type_ctor_desc' are not guaranteed to be
** represented the same way as values of type `private_builtin:type_ctor_info'.
** The representations *are* in fact identical for first order types, but they
** differ for higher order and tuple types. Instead of a type_ctor_desc
** being a structure containing a pointer to the type_ctor_info for pred/0
** or func/0 and an arity, we have a single small encoded integer. This
** integer is four times the arity, plus zero, one or two; plus zero encodes a
** predicate, plus one encodes a function, plus two encodes a tuple.
** The maximum arity that can be encoded is given by MR_MAX_VARIABLE_ARITY
** (see below).
** The C type corresponding to types:type_ctor_desc is `MR_TypeCtorDesc'.
*/

/*
** Declare the MR_TypeCtorDesc ADT.
**
** Note that `struct MR_TypeCtorDesc_Struct' is deliberately left undefined.
** MR_TypeCtorDesc is declared as a pointer to a dummy structure only
** in order to allow the C compiler to catch errors in which things other
** than MR_TypeCtorDescs are given as arguments to macros that depend on their
** arguments being MR_TypeCtorDescs. The actual value is either a small integer
** or a pointer to a MR_TypeCtorInfo_Struct structure, as described above.
*/

typedef struct MR_TypeCtorDesc_Struct *MR_TypeCtorDesc;

/*
** The maximum arity that can be encoded should be set to twice the maximum
** number of general purpose registers, since an predicate or function having
** more arguments that this would run out of registers when passing the input
** arguments, or the output arguments, or both.
**
** XXX When tuples were added this was reduced to be the maximum number
** of general purpose registers, to reduce the probability that the
** `small' integers for higher-order and tuple types are confused with
** type_ctor_info pointers. This still allows higher-order terms with
** 1024 arguments, which is more than ../LIMITATIONS promises.
*/

#define MR_MAX_VARIABLE_ARITY		MR_MAX_VIRTUAL_REG

/*
** Constructors for the MR_TypeCtorDesc ADT
*/

#define MR_TYPECTOR_DESC_MAKE_PRED(Arity)				\
	( (MR_TypeCtorDesc) ((Arity) * 4) )
#define MR_TYPECTOR_DESC_MAKE_FUNC(Arity)				\
	( (MR_TypeCtorDesc) ((Arity) * 4 + 1) )
#define MR_TYPECTOR_DESC_MAKE_TUPLE(Arity)				\
	( (MR_TypeCtorDesc) ((Arity) * 4 + 2) )
#define MR_TYPECTOR_DESC_MAKE_FIXED_ARITY(type_ctor_info)		\
	( MR_CHECK_EXPR_TYPE(type_ctor_info, MR_TypeCtorInfo),		\
	  (MR_TypeCtorDesc) type_ctor_info )

/*
** Access macros for the MR_TypeCtor ADT.
**
** The MR_TYPECTOR_DESC_GET_VA_* macros should only be called if
** MR_TYPECTOR_DESC_IS_VARIABLE_ARITY() returns true.
** The MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO() macro
** should only be called if MR_TYPECTOR_DESC_IS_VARIABLE_ARITY() returns false.
*/

#define MR_TYPECTOR_DESC_IS_VARIABLE_ARITY(T)				\
	( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),			\
	  (MR_Unsigned) (T) <= (4 * MR_MAX_VARIABLE_ARITY + 2) )
#define MR_TYPECTOR_DESC_GET_FIXED_ARITY_TYPE_CTOR_INFO(T)		\
	( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),			\
	  (MR_TypeCtorInfo) (T) )
#define MR_TYPECTOR_DESC_GET_VA_ARITY(T)				\
	( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),			\
	  (MR_Unsigned) (T) / 4 )
#define MR_TYPECTOR_DESC_GET_VA_NAME(T)					\
	( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),			\
	  (MR_ConstString) (((MR_Unsigned) (T) % 4 == 0)		\
		? "pred"						\
		: (((MR_Unsigned) (T) % 4 == 1)				\
			? "func"					\
			: "{}" )) )
#define MR_TYPECTOR_DESC_GET_VA_MODULE_NAME(T)				\
	( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),			\
	  (MR_ConstString) "builtin" )
#define MR_TYPECTOR_DESC_GET_VA_TYPE_CTOR_INFO(T)			\
	( MR_CHECK_EXPR_TYPE(T, MR_TypeCtorDesc),			\
	  ((MR_Unsigned) (T) % 4 == 0)					\
		? (MR_address_of_type_ctor_info_for_pred)		\
		: (((MR_Unsigned) (T) % 4 == 1)				\
			? (MR_address_of_type_ctor_info_for_func)	\
			: (MR_address_of_type_ctor_info_for_tuple) ) )

/*
** Create and return a MR_TypeCtorDesc that describes the same type as
** type_ctor_info. If type_ctor_info is of variable arity, take the arity
** from type_info, which should be the type_info that type_ctor_info was
** extracted from.
*/

extern	MR_TypeCtorDesc MR_make_type_ctor_desc(MR_TypeInfo type_info,
				MR_TypeCtorInfo type_ctor_info);

/*
** Given type_info, return the MR_TypeCtorDesc describing its outermost type
** constructor in *type_ctor_desc_ptr and a list of the typeinfos of its
** argument types in *arg_type_info_list_ptr. If collapse_equivalences is
** MR_TRUE, then expand out the equivalences in type_info first.
**
** You need to wrap MR_{save/restore}_transient_registers() around
** calls to this function.
*/


extern	void		MR_type_ctor_and_args(MR_TypeInfo type_info,
				MR_bool collapse_equivalences,
				MR_TypeCtorDesc *type_ctor_desc_ptr,
				MR_Word *arg_type_info_list_ptr);

/*
** ML_make_type(arity, type_ctor_info, arg_types_list):
**
** Construct and return a type_info for a type using the specified type_ctor
** for the type constructor, and using the arguments specified in
** arg_types_list for the type arguments (if any).
**
** Assumes that the arity of the type constructor represented by type_ctor_info
** and the length of the arg_types_list are both equal to `arity'.
**
** You need to wrap MR_{save/restore}_transient_registers() around
** calls to this function.
*/

extern	MR_TypeInfo	MR_make_type(int arity, MR_TypeCtorDesc type_ctor_desc,
				MR_Word arg_type_list);

#endif	/* MERCURY_TYPE_DESC_H */
