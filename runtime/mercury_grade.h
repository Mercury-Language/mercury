/*
** Copyright (C) 1997-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_grades.h - defines the MR_GRADE macro.
**
** This is used to get the linker to ensure that different object files
** were compiled with consistent grades.
**
** Any condition compilation macros that affect link compatibility should be
** included here. For documentation on the meaning of these macros, see
** runtime/mercury_conf_param.h.
**
** IMPORTANT: any changes here may also require changes to
**	runtime/mercury_conf_param.h
** 	scripts/init_grade_options.sh-subr
** 	scripts/parse_grade_options.sh-subr
** 	scripts/final_grade_options.sh-subr
** 	scripts/mgnuc.in
** 	scripts/ml.in
**	compiler/handle_options.m
**	compiler/mercury_compile.m
*/

#ifndef MERCURY_GRADES_H
#define MERCURY_GRADES_H

#include "mercury_std.h"	/* for MR_STRINGIFY and MR_PASTE2 */
#include "mercury_tags.h"	/* for MR_TAGBITS */

/*
** Here we build up the MR_GRADE macro part at a time,
** based on the compilation flags.
**
** IMPORTANT: any changes here will probably require similar
** changes to compiler/handle_options.m and scripts/mgnuc.in.
*/

/*
** This part of the grade is a binary compatibility version number.
** You should increment it any time you make a change that breaks
** binary backwards compatibility.
** Note that the binary compatibility version number has no direct
** relationship with the source release number (which is in ../VERSION).
**
** It is a good idea to inspect all code for RTTI version number checks
** and remove them when increasing the binary compatibility version number.   
** Searching for MR_RTTI_VERSION__ should find all code related to the
** RTTI version number.
*/

#define MR_GRADE_PART_0	v6_

#ifdef MR_HIGHLEVEL_CODE

  #ifdef MR_HIGHLEVEL_DATA
    #define MR_GRADE_PART_1	MR_PASTE2(MR_GRADE_PART_0, hl)
  #else
    #define MR_GRADE_PART_1	MR_PASTE2(MR_GRADE_PART_0, hlc)
  #endif

  #ifdef MR_USE_GCC_NESTED_FUNCTIONS
    #define MR_GRADE_PART_2	MR_PASTE2(MR_GRADE_PART_1, _nest)
  #else
    #define MR_GRADE_PART_2	MR_GRADE_PART_1
  #endif

#else /* ! MR_HIGHLEVEL_CODE */

  #ifdef MR_USE_ASM_LABELS
    #define MR_GRADE_PART_1	MR_PASTE2(MR_GRADE_PART_0, asm_)
  #else
    #define MR_GRADE_PART_1	MR_GRADE_PART_0
  #endif

  #ifdef MR_USE_GCC_NONLOCAL_GOTOS
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_PART_2	MR_PASTE2(MR_GRADE_PART_1, fast)
    #else
      #define MR_GRADE_PART_2	MR_PASTE2(MR_GRADE_PART_1, jump)
    #endif
  #else
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_PART_2	MR_PASTE2(MR_GRADE_PART_1, reg)
    #else
      #define MR_GRADE_PART_2	MR_PASTE2(MR_GRADE_PART_1, none)
    #endif
  #endif

#endif /* ! MR_HIGHLEVEL_CODE */

#ifdef MR_THREAD_SAFE
  #define MR_GRADE_PART_3	MR_PASTE2(MR_GRADE_PART_2, _par)
#else
  #define MR_GRADE_PART_3	MR_GRADE_PART_2
#endif

#ifdef MR_CONSERVATIVE_GC
  #define MR_GRADE_PART_4	MR_PASTE2(MR_GRADE_PART_3, _gc)
#elif defined(MR_NATIVE_GC)
  #define MR_GRADE_PART_4	MR_PASTE2(MR_GRADE_PART_3, _agc)
#else
  #define MR_GRADE_PART_4	MR_GRADE_PART_3
#endif

#ifdef MR_DEEP_PROFILING
  #define MR_GRADE_PART_5	MR_PASTE2(MR_GRADE_PART_4, _profdeep)
  #if defined(MR_MPROF_PROFILE_TIME) || defined(MR_MPROF_PROFILE_CALLS) \
	|| defined(MR_MPROF_PROFILE_MEMORY)
    /*
    ** Deep profiling is completely separate from the other profiling
    ** alternatives, and there is no point in allowing their combination.
    */
    #error "Invalid combination of profiling options"
  #endif
#else
  #ifdef MR_MPROF_PROFILE_TIME
    #ifdef MR_MPROF_PROFILE_CALLS
      #ifdef MR_MPROF_PROFILE_MEMORY
      #define MR_GRADE_PART_5	MR_PASTE2(MR_GRADE_PART_4, _profall)
    #else
      #define MR_GRADE_PART_5	MR_PASTE2(MR_GRADE_PART_4, _prof)
    #endif
  #else
      #ifdef MR_MPROF_PROFILE_MEMORY
      /*
      ** Memory profiling interferes with time profiling,
      ** so there's no point in allowing this.
      */
      #error "Invalid combination of profiling options"
    #else
      /* Currently useless, but... */
      #define MR_GRADE_PART_5	MR_PASTE2(MR_GRADE_PART_4, _proftime)
    #endif
  #endif
  #else
    #ifdef MR_MPROF_PROFILE_CALLS
      #ifdef MR_MPROF_PROFILE_MEMORY
      #define MR_GRADE_PART_5	MR_PASTE2(MR_GRADE_PART_4, _memprof)
    #else
      #define MR_GRADE_PART_5	MR_PASTE2(MR_GRADE_PART_4, _profcalls)
    #endif
  #else
      #ifdef MR_MPROF_PROFILE_MEMORY
      /*
      ** Call-graph memory profiling requires call profiling,
      ** and call profiling is reasonably cheap, so there's
      ** no point in allowing this.
      */
      #error "Invalid combination of profiling options"
    #else
      #define MR_GRADE_PART_5	MR_GRADE_PART_4
    #endif
  #endif
  #endif
#endif

#ifdef MR_USE_TRAIL
  #define MR_GRADE_PART_6	MR_PASTE2(MR_GRADE_PART_5, _tr)
#else
  #define MR_GRADE_PART_6	MR_GRADE_PART_5
#endif

#ifdef MR_RESERVE_TAG
  #define MR_GRADE_PART_6b	MR_PASTE2(MR_GRADE_PART_6, _rt)
#else
  #define MR_GRADE_PART_6b	MR_GRADE_PART_6
#endif

#ifdef MR_USE_MINIMAL_MODEL
  #define MR_GRADE_PART_7	MR_PASTE2(MR_GRADE_PART_6b, _mm)
#else
  #define MR_GRADE_PART_7	MR_GRADE_PART_6b
#endif

#if defined(MR_USE_TRAIL) && defined(MR_USE_MINIMAL_MODEL)
  #error "trailing and minimal model tabling are not compatible"
#endif

#if MR_TAGBITS == 0
  #define MR_GRADE_PART_8	MR_PASTE2(MR_GRADE_PART_7, _notags)
#elif defined(MR_HIGHTAGS)
  #define MR_GRADE_PART_8	MR_PASTE2(MR_GRADE_PART_7, \
  					MR_PASTE2(_hightags, MR_TAGBITS))
#else
  #define MR_GRADE_PART_8	MR_PASTE2(MR_GRADE_PART_7, \
  					MR_PASTE2(_tags, MR_TAGBITS))
#endif

#ifdef MR_BOXED_FLOAT
  #define MR_GRADE_PART_9	MR_GRADE_PART_8
#else				/* "ubf" stands for "unboxed float" */
  #define MR_GRADE_PART_9	MR_PASTE2(MR_GRADE_PART_8, _ubf)
#endif

#ifdef MR_NEW_MERCURYFILE_STRUCT
  #define MR_GRADE_PART_10	MR_PASTE2(MR_GRADE_PART_9, _file)
#else
  #define MR_GRADE_PART_10	MR_GRADE_PART_9
#endif

#if defined(MR_USE_REGPARM) && defined(MR_HIGHLEVEL_CODE) && defined(__i386__)
  #define MR_GRADE_PART_11	MR_PASTE2(MR_GRADE_PART_10, _regparm)
#elif defined(MR_PIC_REG) && defined(MR_USE_GCC_GLOBAL_REGISTERS) && \
					defined(__i386__)
  #define MR_GRADE_PART_11	MR_PASTE2(MR_GRADE_PART_10, _picreg)
#else
  #define MR_GRADE_PART_11	MR_GRADE_PART_10
#endif

/*
** Stack traces aren't strictly binary incompatible - but if you
** try to do a stack trace you might find it doesn't work very
** well unless all modules are compiled in with --stack-trace.
** Hence we consider it effectively binary incompatible.
** Similar considerations apply to procedure call tracing.
*/
#if defined(MR_STACK_TRACE)
  #if defined(MR_REQUIRE_TRACING)
    #define MR_GRADE_PART_12	MR_PASTE2(MR_GRADE_PART_11, _debug)
  #else
    #define MR_GRADE_PART_12	MR_PASTE2(MR_GRADE_PART_11, _strce)
  #endif
#else
  #if defined(MR_REQUIRE_TRACING)
    #define MR_GRADE_PART_12	MR_PASTE2(MR_GRADE_PART_11, _trace)
  #else
    #define MR_GRADE_PART_12	MR_GRADE_PART_11
  #endif
#endif

#define MR_GRADE		MR_GRADE_PART_12

#define MR_GRADE_VAR		MR_PASTE2(MR_grade_,MR_GRADE)
#define MR_GRADE_STRING 	MR_STRINGIFY(MR_GRADE)

extern const char MR_GRADE_VAR;

/*
** Here we do the same thing as above, but this time we build up a string
** containing the options to pass to the compiler to select this grade.
*/
#ifdef MR_HIGHLEVEL_CODE

  #ifdef MR_HIGHLEVEL_DATA
    #define MR_GRADE_OPT_PART_1		"hl"
  #else
    #define MR_GRADE_OPT_PART_1		"hlc"
  #endif

  #ifdef MR_USE_GCC_NESTED_FUNCTIONS
    #define MR_GRADE_OPT_PART_2		MR_GRADE_OPT_PART_1  "_nest"
  #else
    #define MR_GRADE_OPT_PART_2		MR_GRADE_OPT_PART_1
  #endif

#else /* ! MR_HIGHLEVEL_CODE */

  #ifdef MR_USE_ASM_LABELS
    #define MR_GRADE_OPT_PART_1		"asm_"
  #else
    #define MR_GRADE_OPT_PART_1		""
  #endif

  #ifdef MR_USE_GCC_NONLOCAL_GOTOS
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "fast"
    #else
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "jump"
    #endif
  #else
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "reg"
    #else
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "none"
    #endif
  #endif

#endif /* ! MR_HIGHLEVEL_CODE */

#ifdef MR_THREAD_SAFE
  #define MR_GRADE_OPT_PART_3	MR_GRADE_OPT_PART_2 ".par"
#else
  #define MR_GRADE_OPT_PART_3	MR_GRADE_OPT_PART_2
#endif
#ifdef MR_CONSERVATIVE_GC
  #define MR_GRADE_OPT_PART_4	MR_GRADE_OPT_PART_3 ".gc"
#elif defined(MR_NATIVE_GC)
  #define MR_GRADE_OPT_PART_4	MR_GRADE_OPT_PART_3 ".agc"
#else
  #define MR_GRADE_OPT_PART_4	MR_GRADE_OPT_PART_3
#endif

#ifdef MR_MPROF_PROFILE_TIME
  #ifdef MR_MPROF_PROFILE_CALLS
    #ifdef MR_MPROF_PROFILE_MEMORY
      #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".profall"
    #else
      #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".prof"
    #endif
  #else
    #ifdef MR_MPROF_PROFILE_MEMORY
      /*
      ** Memory profiling interferes with time profiling,
      ** so there's no point in allowing this.
      */
      #error "Invalid combination of profiling options"
    #else
      /* Currently useless "but... */
      #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".proftime"
    #endif
  #endif
#else
  #ifdef MR_MPROF_PROFILE_CALLS
    #ifdef MR_MPROF_PROFILE_MEMORY
      #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".memprof"
    #else
      #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".profcalls"
    #endif
  #else
    #ifdef MR_MPROF_PROFILE_MEMORY
      /*
      ** Call-graph memory profiling requires call profiling,
      ** and call profiling is reasonably cheap, so there's
      ** no point in allowing this.
      */
      #error "Invalid combination of profiling options"
    #else
      #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4
    #endif
  #endif
#endif

#ifdef MR_USE_TRAIL
  #define MR_GRADE_OPT_PART_6	MR_GRADE_OPT_PART_5 ".tr"
#else
  #define MR_GRADE_OPT_PART_6	MR_GRADE_OPT_PART_5
#endif

#ifdef MR_RESERVE_TAG
  #define MR_GRADE_OPT_PART_6b	MR_GRADE_OPT_PART_6 ".rt"
#else
  #define MR_GRADE_OPT_PART_6b	MR_GRADE_OPT_PART_6
#endif

#ifdef MR_USE_MINIMAL_MODEL
  #define MR_GRADE_OPT_PART_7	MR_GRADE_OPT_PART_6b ".mm"
#else
  #define MR_GRADE_OPT_PART_7	MR_GRADE_OPT_PART_6b
#endif

/*
** Parts 8-9 above (i.e. tag bits, and (un)boxed float)
** are documented as "not for general use", and can't be set via the
** `--grade' option; we don't bother to pass them on.
**
** Likewise part 10 above (i.e. MR_NEW_MERCURYFILE_STRUCT)
** can't be set by the `--grade' option; it's intended to be
** set by the configure script at configuration time.
** So we don't bother to pass it on.
*/

#if defined(MR_USE_REGPARM) && defined(MR_HIGHLEVEL_CODE) && defined(__i386__)
  #define MR_GRADE_OPT_PART_11	MR_GRADE_OPT_PART_7 ".regparm"
#elif defined(MR_PIC_REG) && defined(MR_USE_GCC_GLOBAL_REGISTERS) && \
					defined(__i386__)
  #define MR_GRADE_OPT_PART_11	MR_GRADE_OPT_PART_7 ".picreg"
#else
  #define MR_GRADE_OPT_PART_11	MR_GRADE_OPT_PART_7
#endif

/*
** Stack traces aren't strictly binary incompatible - but if you
** try to do a stack trace you might find it doesn't work very
** well unless all modules are compiled in with --stack-trace.
** Hence we consider it effectively binary incompatible.
** Similar considerations apply to procedure call tracing.
*/
#if defined(MR_STACK_TRACE)
  #if defined(MR_REQUIRE_TRACING)
    #define MR_GRADE_OPT_PART_12	MR_GRADE_OPT_PART_11 ".debug"
  #else
    #define MR_GRADE_OPT_PART_12	MR_GRADE_OPT_PART_11 ".strce"
  #endif
#else
  #if defined(MR_REQUIRE_TRACING)
    #define MR_GRADE_OPT_PART_12	MR_GRADE_OPT_PART_11 ".trace"
  #else
    #define MR_GRADE_OPT_PART_12	MR_GRADE_OPT_PART_11
  #endif
#endif

#define MR_GRADE_OPT		MR_GRADE_OPT_PART_12

#endif /* MERCURY_GRADES_H */
