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
#include "mercury_ho_call.h"	/* for the `MR_Closure' type */

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
** XXX this is a hack to work-around the current lack of
** support for `pragma export'.
*/
#ifndef MR_HIGHLEVEL_DATA
#define ML_report_uncaught_exception \
		mercury__exception__report_uncaught_exception_3_p_0
#define ML_throw_io_error		mercury__io__throw_io_error_1_p_0
#define ML_io_finalize_state		mercury__io__finalize_state_2_p_0
#define ML_io_init_state		mercury__io__init_state_2_p_0
#define ML_io_stderr_stream		mercury__io__stderr_stream_3_p_0
#define ML_io_stdin_stream		mercury__io__stdin_stream_3_p_0
#define ML_io_stdout_stream		mercury__io__stdout_stream_3_p_0
#endif

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
typedef ConstString MR_ConstString;

/*
** The MR_Box type is used for representing polymorphic types.
*/
typedef void 	*MR_Box;

/*
** The MR_ClosurePtr type is used for representing higher-order types.
*/
typedef const MR_Closure *MR_ClosurePtr;

/*
** With the low-level data representation, the MR_Word type
** is used for representing user-defined types.
*/
typedef Word	MR_Word;

/*
** Define some names for types that differ depending
** on whether --high-level-data is enabled.
*/
#ifdef MR_HIGHLEVEL_DATA
  typedef MR_Integer /* really `enum mercury__builtin__comparison_result_0' */
  	MR_Comparison_Result;
  typedef struct mercury__builtin__void_0_s * MR_Void;
  typedef struct mercury__builtin__c_pointer_0_s * MR_C_Pointer;
  typedef MR_ClosurePtr MR_Pred;
  typedef MR_ClosurePtr MR_Func;
  typedef struct mercury__array__array_1_s * MR_Array;
  typedef struct mercury__std_util__univ_0_s * MR_Univ;
  typedef struct mercury__std_util__type_desc_0_s * MR_Type_Desc;
  typedef struct mercury__private_builtin__type_info_1_s * MR_Type_Info;
  typedef struct mercury__private_builtin__type_ctor_info_1_s * MR_Type_Ctor_Info;
  typedef struct mercury__private_builtin__typeclass_info_1_s * MR_TypeClass_Info;
  typedef struct mercury__private_builtin__base_typeclass_info_1_s *
  	MR_Base_TypeClass_Info;
#else
  /* for --no-high-level-data, they're all just `MR_Word' */
  typedef MR_Word MR_Comparison_Result;
  typedef MR_Word MR_Void;
  typedef MR_Word MR_C_Pointer;
  typedef MR_Word MR_Pred;
  typedef MR_Word MR_Func;
  typedef MR_Word MR_Array;
  typedef MR_Word MR_Univ;
  typedef MR_Word MR_Type_Desc;
  typedef MR_Word MR_Type_Info;
  typedef MR_Word MR_Type_Ctor_Info;
  typedef MR_Word MR_TypeClass_Info;
  typedef MR_Word MR_Base_TypeClass_Info;
#endif

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
typedef MR_Box				MR_BaseTypeclassInfo[];

/*
** XXX Currently we hard-code the declarations of the first
** five of these type-info struct types; this imposes a fixed
** limit of 10 on the arity of types.  (If this is exceeded,
** you'll get a parse error in the generated C code, due to
** an undeclared type.)
** Note that the code for compare and unify in runtime/mercury.c
** also has a fixed limit of 5 on the arity of types.
** Fortunately types with a high arity tend not to be used very
** often, so this is probably OK for now...
*/

MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct1, 1);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct2, 2);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct3, 3);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct4, 4);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct5, 5);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct6, 6);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct7, 7);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct8, 8);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct9, 9);
MR_HIGHER_ORDER_PSEUDOTYPEINFO_STRUCT(MR_HO_PseudoTypeInfo_Struct10, 10);

MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct1, 1);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct2, 2);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct3, 3);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct4, 4);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct5, 5);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct6, 6);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct7, 7);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct8, 8);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct9, 9);
MR_FIRST_ORDER_PSEUDOTYPEINFO_STRUCT(MR_FO_PseudoTypeInfo_Struct10, 10);

typedef struct MR_HO_PseudoTypeInfo_Struct1 MR_HO_PseudoTypeInfo_Struct1;
typedef struct MR_HO_PseudoTypeInfo_Struct2 MR_HO_PseudoTypeInfo_Struct2;
typedef struct MR_HO_PseudoTypeInfo_Struct3 MR_HO_PseudoTypeInfo_Struct3;
typedef struct MR_HO_PseudoTypeInfo_Struct4 MR_HO_PseudoTypeInfo_Struct4;
typedef struct MR_HO_PseudoTypeInfo_Struct5 MR_HO_PseudoTypeInfo_Struct5;
typedef struct MR_HO_PseudoTypeInfo_Struct6 MR_HO_PseudoTypeInfo_Struct6;
typedef struct MR_HO_PseudoTypeInfo_Struct7 MR_HO_PseudoTypeInfo_Struct7;
typedef struct MR_HO_PseudoTypeInfo_Struct8 MR_HO_PseudoTypeInfo_Struct8;
typedef struct MR_HO_PseudoTypeInfo_Struct9 MR_HO_PseudoTypeInfo_Struct9;
typedef struct MR_HO_PseudoTypeInfo_Struct10 MR_HO_PseudoTypeInfo_Struct10;

typedef struct MR_FO_PseudoTypeInfo_Struct1 MR_FO_PseudoTypeInfo_Struct1;
typedef struct MR_FO_PseudoTypeInfo_Struct2 MR_FO_PseudoTypeInfo_Struct2;
typedef struct MR_FO_PseudoTypeInfo_Struct3 MR_FO_PseudoTypeInfo_Struct3;
typedef struct MR_FO_PseudoTypeInfo_Struct4 MR_FO_PseudoTypeInfo_Struct4;
typedef struct MR_FO_PseudoTypeInfo_Struct5 MR_FO_PseudoTypeInfo_Struct5;
typedef struct MR_FO_PseudoTypeInfo_Struct6 MR_FO_PseudoTypeInfo_Struct6;
typedef struct MR_FO_PseudoTypeInfo_Struct7 MR_FO_PseudoTypeInfo_Struct7;
typedef struct MR_FO_PseudoTypeInfo_Struct8 MR_FO_PseudoTypeInfo_Struct8;
typedef struct MR_FO_PseudoTypeInfo_Struct9 MR_FO_PseudoTypeInfo_Struct9;
typedef struct MR_FO_PseudoTypeInfo_Struct10 MR_FO_PseudoTypeInfo_Struct10;

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
	mercury__std_util__std_util__type_ctor_info_type_desc_0,
	mercury__private_builtin__private_builtin__type_ctor_info_type_ctor_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_type_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_typeclass_info_1,
	mercury__private_builtin__private_builtin__type_ctor_info_base_typeclass_info_1;

/*
** XXX this is a bit of a hack: really we should change it so that
** the generated MLDS code always qualifies things with `builtin:',
** but currently it doesn't, so we use the following #defines as
** a work-around.
*/
#define mercury__builtin____type_ctor_info_int_0 \
	mercury__builtin__builtin__type_ctor_info_int_0
#define mercury__builtin____type_ctor_info_string_0 \
	mercury__builtin__builtin__type_ctor_info_string_0
#define mercury__builtin____type_ctor_info_float_0 \
	mercury__builtin__builtin__type_ctor_info_float_0
#define mercury__builtin____type_ctor_info_character_0 \
	mercury__builtin__builtin__type_ctor_info_character_0
#define mercury__builtin____type_ctor_info_pred_0 \
	mercury__builtin__builtin__type_ctor_info_pred_0

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
  #define MR_new_object(type, size, name) \
  		((type *) MR_GC_MALLOC_INLINE(size))
#else
  #define MR_new_object(type, size, name) \
  		((type *) GC_MALLOC(size)) 
#endif

/*
** Code to box/unbox floats
**
** XXX we should optimize the case where sizeof(MR_Float) == sizeof(MR_Box)
*/ 

#if defined(__GNUC__) && !defined(MR_AVOID_MACROS)
  #define MR_box_float(f) ({						\
	MR_Float *MR_box_float_ptr = (MR_Float *)			\
		MR_new_object(MR_Float, sizeof(MR_Float), "float");	\
	*MR_box_float_ptr = (f);					\
	/* return */ (MR_Box) MR_box_float_ptr;				\
  })
#else
  MR_EXTERN_INLINE MR_Box MR_box_float(MR_Float f);

  MR_EXTERN_INLINE MR_Box
  MR_box_float(MR_Float f) {
	MR_Float *ptr = (MR_Float *)
		MR_new_object(MR_Float, sizeof(MR_Float), "float");
	*ptr = f;
	return (MR_Box) ptr;
  }
#endif

#ifdef MR_AVOID_MACROS
  MR_EXTERN_INLINE MR_Float MR_unbox_float(MR_Box b);

  MR_EXTERN_INLINE
  MR_Float MR_unbox_float(MR_Box b) {
	return *(MR_Float *)b;
  }
#else
  #define MR_unbox_float(ptr) (*(MR_Float *)ptr)
#endif

#ifdef MR_AVOID_MACROS
  MR_EXTERN_INLINE void mercury__private_builtin__unsafe_type_cast_2_p_0(
  	MR_Box src, MR_Box *dest);

  MR_EXTERN_INLINE void mercury__private_builtin__unsafe_type_cast_2_p_0(
  	MR_Box src, MR_Box *dest)
  {
  	*dest = src;
  }
#else
  #define mercury__private_builtin__unsafe_type_cast_2_p_0(src, dest) \
	(*(dest) = (src))
#endif

/*---------------------------------------------------------------------------*/
/*
** Function declarations
*/

bool mercury__builtin__unify_2_p_0(MR_Type_Info, MR_Box, MR_Box);
void mercury__builtin__compare_3_p_0(MR_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);
void mercury__builtin__compare_3_p_1(MR_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);
void mercury__builtin__compare_3_p_2(MR_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);
void mercury__builtin__compare_3_p_3(MR_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);

bool mercury__builtin____Unify____int_0_0(MR_Integer x, MR_Integer y); 
bool mercury__builtin____Unify____string_0_0(MR_String x, MR_String y); 
bool mercury__builtin____Unify____float_0_0(MR_Float x, MR_Float y); 
bool mercury__builtin____Unify____character_0_0(MR_Char x, MR_Char); 
bool mercury__builtin____Unify____void_0_0(MR_Void x, MR_Void y); 
bool mercury__builtin____Unify____c_pointer_0_0(
	MR_C_Pointer x, MR_C_Pointer y); 
bool mercury__builtin____Unify____func_0_0(MR_Func x, MR_Func y); 
bool mercury__builtin____Unify____pred_0_0(MR_Pred x, MR_Pred y); 
bool mercury__array____Unify____array_1_0(MR_Type_Info type_info,
	MR_Array x, MR_Array y);
bool mercury__std_util____Unify____univ_0_0(MR_Univ x, MR_Univ y); 
bool mercury__std_util____Unify____type_desc_0_0(
	MR_Type_Desc x, MR_Type_Desc y); 
bool mercury__private_builtin____Unify____type_ctor_info_1_0(
	MR_Type_Info type_info, MR_Type_Ctor_Info x, MR_Type_Ctor_Info y); 
bool mercury__private_builtin____Unify____type_info_1_0(
	MR_Type_Info type_info, MR_Type_Info x, MR_Type_Info y); 
bool mercury__private_builtin____Unify____typeclass_info_1_0(
	MR_Type_Info type_info, MR_TypeClass_Info x, MR_TypeClass_Info y); 
bool mercury__private_builtin____Unify____base_typeclass_info_1_0(
	MR_Type_Info type_info, MR_Base_TypeClass_Info x,
	MR_Base_TypeClass_Info y); 

void mercury__builtin____Compare____int_0_0(
	MR_Comparison_Result *result, MR_Integer x, MR_Integer y);
void mercury__builtin____Compare____string_0_0(
	MR_Comparison_Result *result, MR_String x, MR_String y);
void mercury__builtin____Compare____float_0_0(
	MR_Comparison_Result *result, MR_Float x, MR_Float y);
void mercury__builtin____Compare____character_0_0(
	MR_Comparison_Result *result, MR_Char x, MR_Char y);
void mercury__builtin____Compare____void_0_0(
	MR_Comparison_Result *result, MR_Void x, MR_Void y);
void mercury__builtin____Compare____c_pointer_0_0(
	MR_Comparison_Result *result, MR_C_Pointer x, MR_C_Pointer y);
void mercury__builtin____Compare____func_0_0(
	MR_Comparison_Result *result, MR_Func x, MR_Func y);
void mercury__builtin____Compare____pred_0_0(
	MR_Comparison_Result *result, MR_Pred x, MR_Pred y); 
void mercury__array____Compare____array_1_0(MR_Type_Info type_info,
	MR_Comparison_Result *result, MR_Array x, MR_Array y);
void mercury__std_util____Compare____univ_0_0(
	MR_Comparison_Result *result, MR_Univ x, MR_Univ y);
void mercury__std_util____Compare____type_desc_0_0(
	MR_Comparison_Result *result, MR_Type_Desc x, MR_Type_Desc y);
void mercury__private_builtin____Compare____type_ctor_info_1_0(
	MR_Type_Info type_info, MR_Comparison_Result *result,
	MR_Type_Ctor_Info x, MR_Type_Ctor_Info y);
void mercury__private_builtin____Compare____type_info_1_0(
	MR_Type_Info type_info, MR_Comparison_Result *result,
	MR_Type_Info x, MR_Type_Info y);
void mercury__private_builtin____Compare____typeclass_info_1_0(
	MR_Type_Info type_info, MR_Comparison_Result *result,
	MR_TypeClass_Info x, MR_TypeClass_Info y);
void mercury__private_builtin____Compare____base_typeclass_info_1_0(
	MR_Type_Info type_info, MR_Comparison_Result *result,
	MR_Base_TypeClass_Info x, MR_Base_TypeClass_Info y);

/*---------------------------------------------------------------------------*/

/*
** XXX this is a hack to work-around the current lack of
** support for `pragma export'.
*/
void ML_io_print_to_cur_stream(MR_Word ti, MR_Word x);
void ML_io_print_to_stream(MR_Word ti, MR_Word stream, MR_Word x);
void ML_report_uncaught_exception(MR_Word ti);
void ML_throw_io_error(MR_String);

/*---------------------------------------------------------------------------*/

#endif /* not MERCURY_H */
