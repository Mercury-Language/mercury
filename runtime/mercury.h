/*
** Copyright (C) 1999-2002 The University of Melbourne.
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

/* Everything in this file is specific to the high-level-code back-end */
#ifdef MR_HIGHLEVEL_CODE

/*---------------------------------------------------------------------------*/
/*
** Header files to include
*/

#include "mercury_conf.h"
#include "mercury_types.h" 
#include "mercury_float.h"	/* for the `MR_Float' type */
#include "mercury_tags.h"
#include "mercury_grade.h"
#include "mercury_thread.h"	/* for the MR_*_GLOBAL_LOCK() macros */
#include "mercury_std.h"	/* for the MR_CALL macro (and others) */
#include "mercury_type_info.h"
#include "mercury_ho_call.h"	/* for the `MR_Closure' type */
#include "mercury_bootstrap.h"
#include "mercury_memory.h"	/* for memory allocation routines */
#include "mercury_type_tables.h"	/* for MR_register_type_ctor_info */

#ifdef MR_CONSERVATIVE_GC
  #include "gc.h"
  #ifdef MR_INLINE_ALLOC
    #include "gc_inl.h"
  #endif
#else
  #include "mercury_regs.h"		/* for MR_hp */
  #include "mercury_engine.h"		/* for MR_fake_reg (needed by MR_hp) */
  #include "mercury_overflow.h"		/* for MR_heap_overflow_check() */
  #ifdef MR_NATIVE_GC
    #include "mercury_accurate_gc.h"	/* for MR_garbage_collect() */
    #include "mercury_layout_util.h"	/* for MR_materialize_closure...() */
  #endif
#endif

#if defined(MR_MPROF_PROFILE_CALLS) || defined(MR_MPROF_PROFILE_TIME)
  #include "mercury_prof.h"		/* for MR_prof_call_profile	*/
					/* and MR_set_prof_current_proc	*/
#endif

#ifdef MR_MPROF_PROFILE_MEMORY
  #include "mercury_heap_profile.h"	/* for MR_record_allocation */
#endif

#if defined(MR_MPROF_PROFILE_CALLS) || defined(MR_MPROF_PROFILE_MEMORY) || defined(MR_MPROF_PROFILE_TIME)
  #include "mercury_goto.h"		/* for MR_init_entry */
#endif

#include <setjmp.h>	/* for jmp_buf etc., which are used for commits */
#include <string.h>	/* for strcmp(), which is used for =/2 on strings */

/*---------------------------------------------------------------------------*/
/*
** Type definitions
*/

/*
** The continuation function types used for implementing
** nondeterministic procedures.
*/
typedef void MR_CALL (*MR_NestedCont) (void); /* for --gcc-nested-functions */
typedef void MR_CALL (*MR_Cont) (void *); /* for --no-gcc-nested-functions */

/*
** The jmp_buf type used by MR_builtin_setjmp()
** to save the stack context when implementing commits.
*/
#ifdef __GNUC__
  /*
  ** For GCC, we use `__builtin_setjmp' and `__builtin_longjmp'.
  ** These are documented (in gcc/builtins.c in the GCC source code)
  ** as taking for their parameter a pointer to an array of five words. 
  */
  typedef void *MR_builtin_jmp_buf[5];
#else
  /* Otherwise we use the standard jmp_buf type */
  typedef jmp_buf MR_builtin_jmp_buf;
#endif

/*
** The types uses to represent the Mercury builtin types,
** MR_Char, MR_Float, MR_Integer, MR_String, and MR_ConstString,
** are defined in mercury_types.h and mercury_float.h.
*/

/*
** The MR_Word type, which is used for representing user-defined
** types when we're using the low-level data representation,
** is defined in runtime/mercury_types.h.
*/

/*
** The MR_Box type, which is used for representing polymorphic 
** types, is defined in runtime/mercury_types.h.
*/

/*
** The MR_ClosurePtr type is used for representing higher-order types.
*/
typedef const MR_Closure *MR_ClosurePtr;

/*
** Define some names for types that differ depending
** on whether --high-level-data is enabled.
** These types all correspond to Mercury data types.
** Some of the have `Mercury_' in their name, to distinguish
** them from the corresponding C data type.
** E.g. `MR_Mercury_Type_Info' (below) is the abstract type that the
** Mercury compiler generates for a type_info argument, whereas
** `MR_TypeInfo' (defined in runtime/mercury_type_info.h) is the
** concrete C type that is used by the C code in the runtime.
*/
#ifdef MR_HIGHLEVEL_DATA
  typedef MR_Integer /* really `enum mercury__builtin__comparison_result_0' */
  	MR_Comparison_Result;
  typedef struct mercury__builtin__void_0_s * MR_Void;
  typedef struct mercury__builtin__c_pointer_0_s * MR_C_Pointer;
  typedef struct mercury__private_builtin__heap_pointer_0_s * MR_Heap_Pointer;
  typedef MR_ClosurePtr MR_Pred;
  typedef MR_ClosurePtr MR_Func;
  typedef struct mercury__array__array_1_s * MR_Array;
  typedef struct mercury__std_util__univ_0_s * MR_Univ;
  typedef struct mercury__type_desc__type_desc_0_s * MR_Type_Desc;
  typedef struct mercury__type_desc__type_ctor_desc_0_s * MR_Type_Ctor_Desc;
  typedef struct mercury__private_builtin__type_info_1_s *
  	MR_Mercury_Type_Info;
  typedef struct mercury__private_builtin__type_ctor_info_1_s *
  	MR_Mercury_Type_Ctor_Info;
  typedef struct mercury__private_builtin__typeclass_info_1_s *
  	MR_Mercury_TypeClass_Info;
  typedef struct mercury__private_builtin__base_typeclass_info_1_s *
  	MR_Mercury_Base_TypeClass_Info;
#else
  /* for --no-high-level-data, they're all just `MR_Word' */
  typedef MR_Word MR_Comparison_Result;
  typedef MR_Word MR_Void;
  typedef MR_Word MR_C_Pointer;
  typedef MR_Word MR_Heap_Pointer;
  typedef MR_Word MR_Pred;
  typedef MR_Word MR_Func;
  typedef MR_Word MR_Array;
  typedef MR_Word MR_Univ;
  typedef MR_Word MR_Type_Desc;
  typedef MR_Word MR_Type_Ctor_Desc;
  typedef MR_Word MR_Mercury_Type_Info;
  typedef MR_Word MR_Mercury_Type_Ctor_Info;
  typedef MR_Word MR_Mercury_TypeClass_Info;
  typedef MR_Word MR_Mercury_Base_TypeClass_Info;
#endif

/*
** Tuples are always just arrays of polymorphic terms.
*/
typedef MR_Box *MR_Tuple;

/*
** Typedefs used for the types of RTTI data structures.
** Many of these types are defined in mercury_type_info.h,
** but the ones which are used only by the MLDS back-end
** are defined here.
*/
typedef struct MR_TypeCtorInfo_Struct	MR_TypeCtorInfo_Struct;
typedef const MR_EnumFunctorDesc *	MR_EnumFunctorDescPtr;
typedef const MR_DuFunctorDesc *	MR_DuFunctorDescPtr;
typedef union MR_TableNode_Union * *	MR_TableNodePtrPtr;
typedef MR_Box				MR_BaseTypeclassInfo;
typedef const void * 			MR_ReservedAddrs;
typedef const MR_ReservedAddrFunctorDesc *MR_ReservedAddrFunctors;


/*
** XXX Currently we hard-code the declarations of the first
** ten of these type-info struct types; this imposes a fixed
** limit of 20 on the arity of types.  (If this is exceeded,
** you'll get a parse error in the generated C code, due to
** an undeclared type.)
** Note that the code for compare and unify in runtime/mercury.c
** also has a fixed limit of 5 on the arity of types (other than
** higher-order and tuple types, which have no limit).
** Fortunately types with a high arity tend not to be used very
** often, so this is probably OK for now...
*/

MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct1, 1);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct2, 2);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct3, 3);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct4, 4);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct5, 5);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct6, 6);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct7, 7);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct8, 8);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct9, 9);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct10, 10);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct11, 11);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct12, 12);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct13, 13);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct14, 14);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct15, 15);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct16, 16);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct17, 17);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct18, 18);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct19, 19);
MR_VAR_ARITY_PSEUDOTYPEINFO_STRUCT(MR_VA_PseudoTypeInfo_Struct20, 20);

MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct1, 1);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct2, 2);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct3, 3);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct4, 4);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct5, 5);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct6, 6);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct7, 7);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct8, 8);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct9, 9);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct10, 10);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct11, 11);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct12, 12);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct13, 13);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct14, 14);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct15, 15);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct16, 16);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct17, 17);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct18, 18);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct19, 19);
MR_FIXED_ARITY_PSEUDOTYPEINFO_STRUCT(MR_FA_PseudoTypeInfo_Struct20, 20);

MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct1, 1);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct2, 2);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct3, 3);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct4, 4);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct5, 5);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct6, 6);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct7, 7);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct8, 8);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct9, 9);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct10, 10);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct11, 11);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct12, 12);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct13, 13);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct14, 14);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct15, 15);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct16, 16);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct17, 17);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct18, 18);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct19, 19);
MR_VAR_ARITY_TYPEINFO_STRUCT(MR_VA_TypeInfo_Struct20, 20);

MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct1, 1);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct2, 2);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct3, 3);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct4, 4);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct5, 5);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct6, 6);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct7, 7);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct8, 8);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct9, 9);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct10, 10);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct11, 11);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct12, 12);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct13, 13);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct14, 14);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct15, 15);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct16, 16);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct17, 17);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct18, 18);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct19, 19);
MR_FIXED_ARITY_TYPEINFO_STRUCT(MR_FA_TypeInfo_Struct20, 20);

/*
** Since standard C doesn't support zero-sized arrays,
** we use the same type for unary higher-order pseudo-type-infos
** as for zero-arity higher-order pseudo-type-infos.
*/

typedef struct MR_VA_PseudoTypeInfo_Struct1 MR_VA_PseudoTypeInfo_Struct0;
typedef struct MR_VA_PseudoTypeInfo_Struct1 MR_VA_PseudoTypeInfo_Struct1;
typedef struct MR_VA_PseudoTypeInfo_Struct2 MR_VA_PseudoTypeInfo_Struct2;
typedef struct MR_VA_PseudoTypeInfo_Struct3 MR_VA_PseudoTypeInfo_Struct3;
typedef struct MR_VA_PseudoTypeInfo_Struct4 MR_VA_PseudoTypeInfo_Struct4;
typedef struct MR_VA_PseudoTypeInfo_Struct5 MR_VA_PseudoTypeInfo_Struct5;
typedef struct MR_VA_PseudoTypeInfo_Struct6 MR_VA_PseudoTypeInfo_Struct6;
typedef struct MR_VA_PseudoTypeInfo_Struct7 MR_VA_PseudoTypeInfo_Struct7;
typedef struct MR_VA_PseudoTypeInfo_Struct8 MR_VA_PseudoTypeInfo_Struct8;
typedef struct MR_VA_PseudoTypeInfo_Struct9 MR_VA_PseudoTypeInfo_Struct9;
typedef struct MR_VA_PseudoTypeInfo_Struct10 MR_VA_PseudoTypeInfo_Struct10;
typedef struct MR_VA_PseudoTypeInfo_Struct11 MR_VA_PseudoTypeInfo_Struct11;
typedef struct MR_VA_PseudoTypeInfo_Struct12 MR_VA_PseudoTypeInfo_Struct12;
typedef struct MR_VA_PseudoTypeInfo_Struct13 MR_VA_PseudoTypeInfo_Struct13;
typedef struct MR_VA_PseudoTypeInfo_Struct14 MR_VA_PseudoTypeInfo_Struct14;
typedef struct MR_VA_PseudoTypeInfo_Struct15 MR_VA_PseudoTypeInfo_Struct15;
typedef struct MR_VA_PseudoTypeInfo_Struct16 MR_VA_PseudoTypeInfo_Struct16;
typedef struct MR_VA_PseudoTypeInfo_Struct17 MR_VA_PseudoTypeInfo_Struct17;
typedef struct MR_VA_PseudoTypeInfo_Struct18 MR_VA_PseudoTypeInfo_Struct18;
typedef struct MR_VA_PseudoTypeInfo_Struct19 MR_VA_PseudoTypeInfo_Struct19;
typedef struct MR_VA_PseudoTypeInfo_Struct20 MR_VA_PseudoTypeInfo_Struct20;

typedef struct MR_FA_PseudoTypeInfo_Struct1 MR_FA_PseudoTypeInfo_Struct1;
typedef struct MR_FA_PseudoTypeInfo_Struct2 MR_FA_PseudoTypeInfo_Struct2;
typedef struct MR_FA_PseudoTypeInfo_Struct3 MR_FA_PseudoTypeInfo_Struct3;
typedef struct MR_FA_PseudoTypeInfo_Struct4 MR_FA_PseudoTypeInfo_Struct4;
typedef struct MR_FA_PseudoTypeInfo_Struct5 MR_FA_PseudoTypeInfo_Struct5;
typedef struct MR_FA_PseudoTypeInfo_Struct6 MR_FA_PseudoTypeInfo_Struct6;
typedef struct MR_FA_PseudoTypeInfo_Struct7 MR_FA_PseudoTypeInfo_Struct7;
typedef struct MR_FA_PseudoTypeInfo_Struct8 MR_FA_PseudoTypeInfo_Struct8;
typedef struct MR_FA_PseudoTypeInfo_Struct9 MR_FA_PseudoTypeInfo_Struct9;
typedef struct MR_FA_PseudoTypeInfo_Struct10 MR_FA_PseudoTypeInfo_Struct10;
typedef struct MR_FA_PseudoTypeInfo_Struct11 MR_FA_PseudoTypeInfo_Struct11;
typedef struct MR_FA_PseudoTypeInfo_Struct12 MR_FA_PseudoTypeInfo_Struct12;
typedef struct MR_FA_PseudoTypeInfo_Struct13 MR_FA_PseudoTypeInfo_Struct13;
typedef struct MR_FA_PseudoTypeInfo_Struct14 MR_FA_PseudoTypeInfo_Struct14;
typedef struct MR_FA_PseudoTypeInfo_Struct15 MR_FA_PseudoTypeInfo_Struct15;
typedef struct MR_FA_PseudoTypeInfo_Struct16 MR_FA_PseudoTypeInfo_Struct16;
typedef struct MR_FA_PseudoTypeInfo_Struct17 MR_FA_PseudoTypeInfo_Struct17;
typedef struct MR_FA_PseudoTypeInfo_Struct18 MR_FA_PseudoTypeInfo_Struct18;
typedef struct MR_FA_PseudoTypeInfo_Struct19 MR_FA_PseudoTypeInfo_Struct19;
typedef struct MR_FA_PseudoTypeInfo_Struct20 MR_FA_PseudoTypeInfo_Struct20;

typedef struct MR_VA_TypeInfo_Struct1 MR_VA_TypeInfo_Struct0;
typedef struct MR_VA_TypeInfo_Struct1 MR_VA_TypeInfo_Struct1;
typedef struct MR_VA_TypeInfo_Struct2 MR_VA_TypeInfo_Struct2;
typedef struct MR_VA_TypeInfo_Struct3 MR_VA_TypeInfo_Struct3;
typedef struct MR_VA_TypeInfo_Struct4 MR_VA_TypeInfo_Struct4;
typedef struct MR_VA_TypeInfo_Struct5 MR_VA_TypeInfo_Struct5;
typedef struct MR_VA_TypeInfo_Struct6 MR_VA_TypeInfo_Struct6;
typedef struct MR_VA_TypeInfo_Struct7 MR_VA_TypeInfo_Struct7;
typedef struct MR_VA_TypeInfo_Struct8 MR_VA_TypeInfo_Struct8;
typedef struct MR_VA_TypeInfo_Struct9 MR_VA_TypeInfo_Struct9;
typedef struct MR_VA_TypeInfo_Struct10 MR_VA_TypeInfo_Struct10;
typedef struct MR_VA_TypeInfo_Struct11 MR_VA_TypeInfo_Struct11;
typedef struct MR_VA_TypeInfo_Struct12 MR_VA_TypeInfo_Struct12;
typedef struct MR_VA_TypeInfo_Struct13 MR_VA_TypeInfo_Struct13;
typedef struct MR_VA_TypeInfo_Struct14 MR_VA_TypeInfo_Struct14;
typedef struct MR_VA_TypeInfo_Struct15 MR_VA_TypeInfo_Struct15;
typedef struct MR_VA_TypeInfo_Struct16 MR_VA_TypeInfo_Struct16;
typedef struct MR_VA_TypeInfo_Struct17 MR_VA_TypeInfo_Struct17;
typedef struct MR_VA_TypeInfo_Struct18 MR_VA_TypeInfo_Struct18;
typedef struct MR_VA_TypeInfo_Struct19 MR_VA_TypeInfo_Struct19;
typedef struct MR_VA_TypeInfo_Struct20 MR_VA_TypeInfo_Struct20;

typedef struct MR_FA_TypeInfo_Struct1 MR_FA_TypeInfo_Struct1;
typedef struct MR_FA_TypeInfo_Struct2 MR_FA_TypeInfo_Struct2;
typedef struct MR_FA_TypeInfo_Struct3 MR_FA_TypeInfo_Struct3;
typedef struct MR_FA_TypeInfo_Struct4 MR_FA_TypeInfo_Struct4;
typedef struct MR_FA_TypeInfo_Struct5 MR_FA_TypeInfo_Struct5;
typedef struct MR_FA_TypeInfo_Struct6 MR_FA_TypeInfo_Struct6;
typedef struct MR_FA_TypeInfo_Struct7 MR_FA_TypeInfo_Struct7;
typedef struct MR_FA_TypeInfo_Struct8 MR_FA_TypeInfo_Struct8;
typedef struct MR_FA_TypeInfo_Struct9 MR_FA_TypeInfo_Struct9;
typedef struct MR_FA_TypeInfo_Struct10 MR_FA_TypeInfo_Struct10;
typedef struct MR_FA_TypeInfo_Struct11 MR_FA_TypeInfo_Struct11;
typedef struct MR_FA_TypeInfo_Struct12 MR_FA_TypeInfo_Struct12;
typedef struct MR_FA_TypeInfo_Struct13 MR_FA_TypeInfo_Struct13;
typedef struct MR_FA_TypeInfo_Struct14 MR_FA_TypeInfo_Struct14;
typedef struct MR_FA_TypeInfo_Struct15 MR_FA_TypeInfo_Struct15;
typedef struct MR_FA_TypeInfo_Struct16 MR_FA_TypeInfo_Struct16;
typedef struct MR_FA_TypeInfo_Struct17 MR_FA_TypeInfo_Struct17;
typedef struct MR_FA_TypeInfo_Struct18 MR_FA_TypeInfo_Struct18;
typedef struct MR_FA_TypeInfo_Struct19 MR_FA_TypeInfo_Struct19;
typedef struct MR_FA_TypeInfo_Struct20 MR_FA_TypeInfo_Struct20;

/* the next two blocks of #defines are for bootstrapping */

#define	MR_FO_PseudoTypeInfo_Struct0 MR_FA_PseudoTypeInfo_Struct0
#define	MR_FO_PseudoTypeInfo_Struct1 MR_FA_PseudoTypeInfo_Struct1
#define	MR_FO_PseudoTypeInfo_Struct2 MR_FA_PseudoTypeInfo_Struct2
#define	MR_FO_PseudoTypeInfo_Struct3 MR_FA_PseudoTypeInfo_Struct3
#define	MR_FO_PseudoTypeInfo_Struct4 MR_FA_PseudoTypeInfo_Struct4
#define	MR_FO_PseudoTypeInfo_Struct5 MR_FA_PseudoTypeInfo_Struct5
#define	MR_FO_PseudoTypeInfo_Struct6 MR_FA_PseudoTypeInfo_Struct6
#define	MR_FO_PseudoTypeInfo_Struct7 MR_FA_PseudoTypeInfo_Struct7
#define	MR_FO_PseudoTypeInfo_Struct8 MR_FA_PseudoTypeInfo_Struct8
#define	MR_FO_PseudoTypeInfo_Struct9 MR_FA_PseudoTypeInfo_Struct9
#define	MR_FO_PseudoTypeInfo_Struct10 MR_FA_PseudoTypeInfo_Struct10
#define	MR_FO_PseudoTypeInfo_Struct11 MR_FA_PseudoTypeInfo_Struct11
#define	MR_FO_PseudoTypeInfo_Struct12 MR_FA_PseudoTypeInfo_Struct12
#define	MR_FO_PseudoTypeInfo_Struct13 MR_FA_PseudoTypeInfo_Struct13
#define	MR_FO_PseudoTypeInfo_Struct14 MR_FA_PseudoTypeInfo_Struct14
#define	MR_FO_PseudoTypeInfo_Struct15 MR_FA_PseudoTypeInfo_Struct15
#define	MR_FO_PseudoTypeInfo_Struct16 MR_FA_PseudoTypeInfo_Struct16
#define	MR_FO_PseudoTypeInfo_Struct17 MR_FA_PseudoTypeInfo_Struct17
#define	MR_FO_PseudoTypeInfo_Struct18 MR_FA_PseudoTypeInfo_Struct18
#define	MR_FO_PseudoTypeInfo_Struct19 MR_FA_PseudoTypeInfo_Struct19
#define	MR_FO_PseudoTypeInfo_Struct20 MR_FA_PseudoTypeInfo_Struct20

#define	MR_HO_PseudoTypeInfo_Struct0 MR_VA_PseudoTypeInfo_Struct0
#define	MR_HO_PseudoTypeInfo_Struct1 MR_VA_PseudoTypeInfo_Struct1
#define	MR_HO_PseudoTypeInfo_Struct2 MR_VA_PseudoTypeInfo_Struct2
#define	MR_HO_PseudoTypeInfo_Struct3 MR_VA_PseudoTypeInfo_Struct3
#define	MR_HO_PseudoTypeInfo_Struct4 MR_VA_PseudoTypeInfo_Struct4
#define	MR_HO_PseudoTypeInfo_Struct5 MR_VA_PseudoTypeInfo_Struct5
#define	MR_HO_PseudoTypeInfo_Struct6 MR_VA_PseudoTypeInfo_Struct6
#define	MR_HO_PseudoTypeInfo_Struct7 MR_VA_PseudoTypeInfo_Struct7
#define	MR_HO_PseudoTypeInfo_Struct8 MR_VA_PseudoTypeInfo_Struct8
#define	MR_HO_PseudoTypeInfo_Struct9 MR_VA_PseudoTypeInfo_Struct9
#define	MR_HO_PseudoTypeInfo_Struct10 MR_VA_PseudoTypeInfo_Struct10
#define	MR_HO_PseudoTypeInfo_Struct11 MR_VA_PseudoTypeInfo_Struct11
#define	MR_HO_PseudoTypeInfo_Struct12 MR_VA_PseudoTypeInfo_Struct12
#define	MR_HO_PseudoTypeInfo_Struct13 MR_VA_PseudoTypeInfo_Struct13
#define	MR_HO_PseudoTypeInfo_Struct14 MR_VA_PseudoTypeInfo_Struct14
#define	MR_HO_PseudoTypeInfo_Struct15 MR_VA_PseudoTypeInfo_Struct15
#define	MR_HO_PseudoTypeInfo_Struct16 MR_VA_PseudoTypeInfo_Struct16
#define	MR_HO_PseudoTypeInfo_Struct17 MR_VA_PseudoTypeInfo_Struct17
#define	MR_HO_PseudoTypeInfo_Struct18 MR_VA_PseudoTypeInfo_Struct18
#define	MR_HO_PseudoTypeInfo_Struct19 MR_VA_PseudoTypeInfo_Struct19
#define	MR_HO_PseudoTypeInfo_Struct20 MR_VA_PseudoTypeInfo_Struct20

/*
** The chain of stack frames, used for accurate GC.
**
** Any changes to this struct may require changes to
** compiler/ml_elim_nested.m, which generates structs
** that whose initial members have to match the layout here,
** and which assumes that the `prev' is at offset zero.
*/
struct MR_StackChain {
	struct MR_StackChain *prev;
	void (*trace)(void *this_frame);
};

/*---------------------------------------------------------------------------*/
/*
** Declarations of contants and variables
*/

#ifdef MR_NATIVE_GC
  /*
  ** This points to the start of the MR_StackChain frame list.
  ** XXX Using a global variable for this is not thread-safe.
  **     We should probably use a GNU C global register variable.
  */
  #ifdef MR_THREAD_SAFE
    #error "Sorry, not supported: --high-level-code --gc accurate --thread-safe"
  #endif
  extern void *mercury__private_builtin__stack_chain;
#endif

/* declare MR_TypeCtorInfo_Structs for the builtin types */
extern const MR_TypeCtorInfo_Struct
	mercury__builtin__builtin__type_ctor_info_int_0,
	mercury__builtin__builtin__type_ctor_info_string_0,
	mercury__builtin__builtin__type_ctor_info_float_0,
	mercury__builtin__builtin__type_ctor_info_character_0,
	mercury__builtin__builtin__type_ctor_info_void_0,
	mercury__builtin__builtin__type_ctor_info_c_pointer_0,
	mercury__private_builtin__private_builtin__type_ctor_info_heap_pointer_0,
	mercury__builtin__builtin__type_ctor_info_pred_0,
	mercury__builtin__builtin__type_ctor_info_func_0,
	mercury__builtin__builtin__type_ctor_info_tuple_0,
	mercury__array__array__type_ctor_info_array_1,
	mercury__std_util__std_util__type_ctor_info_univ_0,
	mercury__type_desc__type_desc__type_ctor_info_type_ctor_desc_0,
	mercury__type_desc__type_desc__type_ctor_info_type_desc_0,
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
#define mercury__builtin____type_ctor_info_func_0 \
	mercury__builtin__builtin__type_ctor_info_func_0
#define mercury__builtin____type_ctor_info_tuple_0 \
	mercury__builtin__builtin__type_ctor_info_tuple_0
#define mercury__builtin____type_ctor_info_void_0 \
	mercury__builtin__builtin__type_ctor_info_void_0

/*
** The compiler used to generate references to this constant.
** XXX This should only be needed for bootstrapping now.
*/
#ifdef MR_AVOID_MACROS
  enum { mercury__private_builtin__SIZEOF_WORD = sizeof(MR_Word) };
#else
  #define mercury__private_builtin__SIZEOF_WORD sizeof(MR_Word)
#endif

/*
** When generating code which passes an io__state or a store__store
** to a polymorphic procedure, or which does a higher-order call
** that passes one of these, then we need to generate a reference to
** a dummy variable.  We use this variable for that purpose.
*/
extern	MR_Word	mercury__private_builtin__dummy_var;

/*---------------------------------------------------------------------------*/
/*
** Macro / inline function definitions
*/

/*
** These macros expand to the either the standard setjmp()/longjmp()
** or to the GNU __builtin_setjmp() and __builtin_longjmp().
** The GNU versions are the same as the standard versions,
** except that they are more efficient, and that they have two
** restrictions:
**	1.  The second argument to __builtin_longjmp() must always be `1'.
**	2.  The call to __builtin_longjmp() must not be in the same
**	    function as the call to __builtin_setjmp().
*/
#if (__GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 8))
  #define MR_builtin_setjmp(buf)	__builtin_setjmp((buf))
  #define MR_builtin_longjmp(buf, val)	__builtin_longjmp((buf), (val))
#else
  #define MR_builtin_setjmp(buf)	setjmp((buf))
  #define MR_builtin_longjmp(buf, val)	longjmp((buf), (val))
#endif

/*
** MR_new_object():
**	Allocates memory on the garbage-collected heap.
*/

#ifdef MR_CONSERVATIVE_GC
  #ifdef MR_INLINE_ALLOC
    #ifndef __GNUC__
      #error "MR_INLINE_ALLOC requires GNU C"
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
  #else /* !MR_INLINE_ALLOC */
    #define MR_new_object(type, size, name) \
  		((type *) GC_MALLOC(size)) 
  #endif /* !MR_INLINE_ALLOC */

#else /* !MR_CONSERVATIVE_GC */

  #ifndef __GNUC__
    /*
    ** We need GNU C's `({...})' expressions.
    ** It's not worth worrying about compilers other than GNU C for
    ** this obscure combination of options.
    */
    #error "For C compilers other than GNU C, `--high-level-code' requires `--gc conservative'"
  #endif

  /*
  ** XXX Note that currently we don't need to worry about alignment here,
  **     other than word alignment, because floating point fields will
  **	 be boxed if they don't fit in a word.
  **     This would need to change if we ever start using unboxed
  **     fields whose alignment requirement is greater than one word.
  */
  #define MR_new_object(type, size, name)				\
     ({ 								\
        size_t MR_new_object_num_words;					\
        MR_Word MR_new_object_ptr;					\
									\
	MR_new_object_num_words = 					\
		((size) + sizeof(MR_Word) - 1) / sizeof(MR_Word);	\
	MR_incr_hp(MR_new_object_ptr, MR_new_object_num_words);		\
	/* return */ (type *) MR_new_object_ptr;			\
      })

#endif

/*
** Code to box/unbox floats
**
** Note that this code is also duplicated in mercury.c.
** XXX we should optimize the case where sizeof(MR_Float) == sizeof(MR_Box)
*/ 

#if defined(__GNUC__) && !defined(MR_AVOID_MACROS)
  #define MR_box_float(f) ({						\
	MR_Float *MR_box_float_ptr;					\
									\
	MR_make_hp_float_aligned();					\
	MR_box_float_ptr = 						\
		MR_new_object(MR_Float, sizeof(MR_Float), "float");	\
	*MR_box_float_ptr = (f);					\
	/* return */ (MR_Box) MR_box_float_ptr;				\
  })
#else
  MR_EXTERN_INLINE MR_Box MR_box_float(MR_Float f);

  MR_EXTERN_INLINE MR_Box
  MR_box_float(MR_Float f) {
	MR_Float *ptr;

	MR_make_hp_float_aligned();
	ptr = MR_new_object(MR_Float, sizeof(MR_Float), "float");
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

/*
** Like MR_box_float, but always an external function, never a macro
** or an inline function.  This is used by the `--target asm'
** GCC back-end interface.
*/
MR_Box MR_asm_box_float(MR_Float f);

/*
** MR_GC_check():
**	Check to see if we need to do a garbage collection, and if so, do it.
*/
#define MR_GC_check()							\
	do {								\
		if ((char *) MR_hp >=					\
		    MR_ENGINE(MR_eng_heap_zone)->gc_threshold)		\
		{							\
			MR_save_registers();				\
			MR_garbage_collect();				\
			MR_restore_registers();				\
		}							\
	} while (0)

/*---------------------------------------------------------------------------*/
/*
** Function declarations
*/

MR_bool MR_CALL mercury__builtin__unify_2_p_0(MR_Mercury_Type_Info,
	MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_0(MR_Mercury_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_1(MR_Mercury_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_2(MR_Mercury_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);
void MR_CALL mercury__builtin__compare_3_p_3(MR_Mercury_Type_Info,
	MR_Comparison_Result *, MR_Box, MR_Box);

MR_bool MR_CALL mercury__builtin____Unify____int_0_0(MR_Integer x,
	MR_Integer y); 
MR_bool MR_CALL mercury__builtin____Unify____string_0_0(MR_String x,
	MR_String y); 
MR_bool MR_CALL mercury__builtin____Unify____float_0_0(MR_Float x, MR_Float y); 
MR_bool MR_CALL mercury__builtin____Unify____character_0_0(MR_Char x, MR_Char); 
MR_bool MR_CALL mercury__builtin____Unify____void_0_0(MR_Void x, MR_Void y); 
MR_bool MR_CALL mercury__builtin____Unify____c_pointer_0_0(
	MR_C_Pointer x, MR_C_Pointer y); 
MR_bool MR_CALL mercury__private_builtin____Unify____heap_pointer_0_0(
	MR_Heap_Pointer x, MR_Heap_Pointer y); 
MR_bool MR_CALL mercury__builtin____Unify____func_0_0(MR_Func x, MR_Func y); 
MR_bool MR_CALL mercury__builtin____Unify____pred_0_0(MR_Pred x, MR_Pred y); 
MR_bool MR_CALL mercury__builtin____Unify____tuple_0_0(
	MR_Mercury_Type_Info type_info, MR_Tuple x, MR_Tuple y); 
MR_bool MR_CALL mercury__type_desc____Unify____type_ctor_desc_0_0(
	MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y); 
MR_bool MR_CALL mercury__type_desc____Unify____type_desc_0_0(
	MR_Type_Desc x, MR_Type_Desc y); 
MR_bool MR_CALL mercury__private_builtin____Unify____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y); 
MR_bool MR_CALL mercury__private_builtin____Unify____type_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y); 
MR_bool MR_CALL mercury__private_builtin____Unify____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y); 
MR_bool MR_CALL mercury__private_builtin____Unify____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Mercury_Base_TypeClass_Info x,
	MR_Mercury_Base_TypeClass_Info y); 

void MR_CALL mercury__builtin____Compare____int_0_0(
	MR_Comparison_Result *result, MR_Integer x, MR_Integer y);
void MR_CALL mercury__builtin____Compare____string_0_0(
	MR_Comparison_Result *result, MR_String x, MR_String y);
void MR_CALL mercury__builtin____Compare____float_0_0(
	MR_Comparison_Result *result, MR_Float x, MR_Float y);
void MR_CALL mercury__builtin____Compare____character_0_0(
	MR_Comparison_Result *result, MR_Char x, MR_Char y);
void MR_CALL mercury__builtin____Compare____void_0_0(
	MR_Comparison_Result *result, MR_Void x, MR_Void y);
void MR_CALL mercury__builtin____Compare____c_pointer_0_0(
	MR_Comparison_Result *result, MR_C_Pointer x, MR_C_Pointer y);
void MR_CALL mercury__private_builtin____Compare____heap_pointer_0_0(
	MR_Comparison_Result *result, MR_Heap_Pointer x, MR_Heap_Pointer y);
void MR_CALL mercury__builtin____Compare____func_0_0(
	MR_Comparison_Result *result, MR_Func x, MR_Func y);
void MR_CALL mercury__builtin____Compare____pred_0_0(
	MR_Comparison_Result *result, MR_Pred x, MR_Pred y); 
void MR_CALL mercury__builtin____Compare____tuple_0_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Tuple x, MR_Tuple y); 
void MR_CALL mercury__type_desc____Compare____type_ctor_desc_0_0(
	MR_Comparison_Result *result,
	MR_Type_Ctor_Desc x, MR_Type_Ctor_Desc y);
void MR_CALL mercury__type_desc____Compare____type_desc_0_0(
	MR_Comparison_Result *result, MR_Type_Desc x, MR_Type_Desc y);
void MR_CALL mercury__private_builtin____Compare____type_ctor_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Ctor_Info x, MR_Mercury_Type_Ctor_Info y);
void MR_CALL mercury__private_builtin____Compare____type_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Type_Info x, MR_Mercury_Type_Info y);
void MR_CALL mercury__private_builtin____Compare____typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_TypeClass_Info x, MR_Mercury_TypeClass_Info y);
void MR_CALL mercury__private_builtin____Compare____base_typeclass_info_1_0(
	MR_Mercury_Type_Info type_info, MR_Comparison_Result *result,
	MR_Mercury_Base_TypeClass_Info x, MR_Mercury_Base_TypeClass_Info y);

/*---------------------------------------------------------------------------*/

#endif /* MR_HIGHLEVEL_CODE */

#endif /* not MERCURY_H */
