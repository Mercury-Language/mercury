/*
** Copyright (C) 1999-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury.h - This file defines the macros, types, etc. that
** are used when generating high-level C code.
** (For the low-level C code, see mercury_imp.h.)
*/

#ifndef MERCURY_H
#define MERCURY_H

/*---------------------------------------------------------------------------*/
/*
** Header files to include
*/

#include "mercury_conf.h"
#include "mercury_types.h" 
#include "mercury_float.h"	/* for the `Float' type */
#include "mercury_tags.h"
#include "mercury_grade.h"
#include "mercury_thread.h"	/* for the MR_*_GLOBAL_LOCK() macros */
#include "mercury_std.h"
#include "mercury_type_info.h"

#ifdef CONSERVATIVE_GC
  #include "gc.h"
  #ifdef INLINE_ALLOC
    #include "gc_inl.h"
  #endif
#endif

#include <setjmp.h>	/* for jmp_buf etc., which are used for commits */
#include <string.h>	/* for strcmp(), which is used for =/2 on strings */

/*---------------------------------------------------------------------------*/
/*
** Type definitions
*/

/*
** The following types are used to represent the Mercury builtin types.
** See mercury_types.h and mercury_float.h.
*/
typedef Char	MR_Char;
typedef Float	MR_Float;
typedef Integer	MR_Integer;
typedef String	MR_String;

/*
** The MR_Box type is used for representing polymorphic types.
*/
typedef Word	MR_Box;

/*
** With the low-level data representation, the MR_Word type
** is used for representing user-defined types.
*/
typedef Word	MR_Word;

/*
** Typedefs used for the types of RTTI data structures.
** Many of these types are defined in mercury_type_info.h,
** but the ones which are used only by the MLDS back-end
** are defined here.
*/
typedef struct MR_TypeCtorInfo_Struct	MR_TypeCtorInfo_Struct;
typedef MR_DuExistLocn			MR_DuExistLocnArray[];
typedef ConstString			MR_ConstStringArray[];
typedef MR_PseudoTypeInfo		MR_PseudoTypeInfoArray[];
typedef const MR_EnumFunctorDesc *	MR_EnumFunctorDescPtrArray[];
typedef const MR_DuFunctorDesc *	MR_DuFunctorDescPtrArray[];
typedef MR_DuPtagLayout			MR_DuPtagLayoutArray[];
typedef union MR_TableNode_Union * *	MR_TableNodePtrPtr[];

/*
** XXX Currently we hard-code the declarations of the first
** five of these type-info struct types; this imposes a fixed
** limit of 5 on the arity of types.  (If this is exceeded,
** you'll get an undeclared type error in the generated C code.)
** Note that the code for compare and unify in runtime/mercury.c
** also has a fixed limit of 5 on the arity of types.
** Fortunately types with a high arity tend not to be used very
** often, so this is probably OK for now...
*/

#ifndef MR_HO_PseudoTypeInfo_Struct1_GUARD
#define MR_HO_PseudoTypeInfo_Struct1_GUARD
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct1, 1);
#endif

#ifndef MR_HO_PseudoTypeInfo_Struct2_GUARD
#define MR_HO_PseudoTypeInfo_Struct2_GUARD
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct2, 2);
#endif

#ifndef MR_HO_PseudoTypeInfo_Struct3_GUARD
#define MR_HO_PseudoTypeInfo_Struct3_GUARD
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct3, 3);
#endif

#ifndef MR_HO_PseudoTypeInfo_Struct4_GUARD
#define MR_HO_PseudoTypeInfo_Struct4_GUARD
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct4, 4);
#endif

#ifndef MR_HO_PseudoTypeInfo_Struct5_GUARD
#define MR_HO_PseudoTypeInfo_Struct5_GUARD
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct5, 5);
#endif

#ifndef MR_FO_PseudoTypeInfo_Struct1_GUARD
#define MR_FO_PseudoTypeInfo_Struct1_GUARD
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct1, 1);
#endif

#ifndef MR_FO_PseudoTypeInfo_Struct2_GUARD
#define MR_FO_PseudoTypeInfo_Struct2_GUARD
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct2, 2);
#endif

#ifndef MR_FO_PseudoTypeInfo_Struct3_GUARD
#define MR_FO_PseudoTypeInfo_Struct3_GUARD
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct3, 3);
#endif

#ifndef MR_FO_PseudoTypeInfo_Struct4_GUARD
#define MR_FO_PseudoTypeInfo_Struct4_GUARD
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct4, 4);
#endif

#ifndef MR_FO_PseudoTypeInfo_Struct5_GUARD
#define MR_FO_PseudoTypeInfo_Struct5_GUARD
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct5, 5);
#endif

typedef struct MR_HO_PseudoTypeInfo_Struct1 MR_HO_PseudoTypeInfo_Struct1;
typedef struct MR_HO_PseudoTypeInfo_Struct2 MR_HO_PseudoTypeInfo_Struct2;
typedef struct MR_HO_PseudoTypeInfo_Struct3 MR_HO_PseudoTypeInfo_Struct3;
typedef struct MR_HO_PseudoTypeInfo_Struct4 MR_HO_PseudoTypeInfo_Struct4;
typedef struct MR_HO_PseudoTypeInfo_Struct5 MR_HO_PseudoTypeInfo_Struct5;

typedef struct MR_FO_PseudoTypeInfo_Struct1 MR_FO_PseudoTypeInfo_Struct1;
typedef struct MR_FO_PseudoTypeInfo_Struct2 MR_FO_PseudoTypeInfo_Struct2;
typedef struct MR_FO_PseudoTypeInfo_Struct3 MR_FO_PseudoTypeInfo_Struct3;
typedef struct MR_FO_PseudoTypeInfo_Struct4 MR_FO_PseudoTypeInfo_Struct4;
typedef struct MR_FO_PseudoTypeInfo_Struct5 MR_FO_PseudoTypeInfo_Struct5;

/*---------------------------------------------------------------------------*/
/*
** Declarations of contants and variables
*/

/* declare MR_TypeCtorInfo_Structs for the builtin types */
extern const MR_TypeCtorInfo_Struct
	mercury__builtin__builtin__type_ctor_info_int_0,
	mercury__builtin__builtin__type_ctor_info_string_0,
	mercury__builtin__builtin__type_ctor_info_float_0,
	mercury__builtin__builtin__type_ctor_info_character_0,
	mercury__builtin__builtin__type_ctor_info_void_0,
	mercury__builtin__builtin__type_ctor_info_c_pointer_0,
	mercury__builtin__builtin__type_ctor_info_pred_0,
	mercury__builtin__builtin__type_ctor_info_func_0,
	mercury__array__array__type_ctor_info_array_1,
	mercury__std_util__std_util__type_ctor_info_univ_0,
	mercury__std_util__std_util__type_ctor_info_type_info_0,
	mercury__private_builtin__private_builtin__type_ctor_info_type_ctor_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_type_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_typeclass_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_base_typeclass_info_1;

/*
** The compiler generates references to this constant.
** This avoids the need for the mlds__rval type to
** have a `sizeof' operator.
*/
#ifdef MR_AVOID_MACROS
  enum { mercury__private_builtin__SIZEOF_WORD = sizeof(MR_Word); }
#else
  #define mercury__private_builtin__SIZEOF_WORD sizeof(MR_Word)
#endif

/*
** When generating code which passes an io__state or a store__store
** to a polymorphic procedure, or which does a higher-order call
** that passes one of these, then we need to generate a reference to
** a dummy variable.  We use this variable for that purpose.
*/
extern	Word	mercury__private_builtin__dummy_var;

/*---------------------------------------------------------------------------*/
/*
** Macro / inline function definitions
*/

/*
** MR_new_object():
**	Allocates memory on the garbage-collected heap.
*/

#ifdef INLINE_ALLOC
  #ifndef __GNUC__
    #error "INLINE_ALLOC requires GNU C"
  #endif
  /*
  ** This must be a macro, not an inline function, because
  ** GNU C's `__builtin_constant_p' does not work inside
  ** inline functions
  */
  #define MR_GC_MALLOC_INLINE(bytes)                                    \
        ( __extension__ __builtin_constant_p(bytes) &&			\
	  (bytes) <= 16 * sizeof(MR_Word)				\
        ? ({    void * temp;                                            \
                /* if size > 1 word, round up to multiple of 8 bytes */	\
                MR_Word rounded_bytes =					\
			( (bytes) <= sizeof(MR_Word)			\
			? sizeof(MR_Word)				\
			: 8 * (((bytes) + 7) / 8)			\
			);						\
                MR_Word num_words = rounded_bytes / sizeof(MR_Word);	\
                GC_MALLOC_WORDS(temp, num_words);                       \
		/* return */ temp;					\
          })                                                            \
        : GC_MALLOC(bytes)                         			\
        )
  /* XXX why do we need to cast to MR_Word here? */
  #define MR_new_object(type, size, name) \
  		((MR_Word) (type *) MR_GC_MALLOC_INLINE(size))
#else
  /* XXX why do we need to cast to MR_Word here? */
  #define MR_new_object(type, size, name) \
  		((MR_Word) (type *) GC_MALLOC(size)) 
#endif

/* this should probably go in mercury_std.h */
#if defined(__GNUC__) 
  #define MR_INLINE __inline__
  #define MR_EXTERN_INLINE extern __inline__
#elif defined(__cplusplus) || __STDC_VERSION__ >= 199901
  #define MR_INLINE inline
  #define MR_EXTERN_INLINE extern inline
#else
  #define MR_INLINE static
  #define MR_EXTERN_INLINE static
#endif

/*
** Code to box/unbox floats
**
** XXX we should optimize the case where sizeof(MR_Float) == sizeof(MR_Box)
*/ 

MR_EXTERN_INLINE MR_Box MR_box_float(MR_Float f);

MR_EXTERN_INLINE MR_Box
MR_box_float(MR_Float f) {
	MR_Float *ptr = (MR_Float *)
		MR_new_object(MR_Float, sizeof(MR_Float), "float");
	*ptr = f;
	return (MR_Box) ptr;
}

#ifdef MR_AVOID_MACROS
  MR_EXTERN_INLINE MR_Float MR_unbox_float(MR_Box b);

  MR_EXTERN_INLINE
  MR_Float MR_unbox_float(MR_Box b) {
	return *(MR_Float *)b;
  }
#else
  #define MR_unbox_float(ptr) (*(MR_Float *)ptr)
#endif

/*-----------------------------------------------------------------------------*/
/*
** Function declarations
*/

bool mercury__builtin__unify_2_p_0(Word type_info, MR_Box, MR_Box);
void mercury__builtin__index_2_p_3(Word type_info, MR_Box, Integer *);
void mercury__builtin__compare_3_p_0(Word type_info, Word *, MR_Box, MR_Box);
void mercury__builtin__compare_3_p_1(Word type_info, Word *, MR_Box, MR_Box);
void mercury__builtin__compare_3_p_2(Word type_info, Word *, MR_Box, MR_Box);
void mercury__builtin__compare_3_p_3(Word type_info, Word *, MR_Box, MR_Box);

/*-----------------------------------------------------------------------------*/

#endif /* not MERCURY_H */
