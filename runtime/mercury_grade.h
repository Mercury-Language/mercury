/*
** Copyright (C) 1997-2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_grades.h - defines the MR_GRADE macro.
**
** This is used to get the linker to ensure that different object files
** were compiled with consistent grades.
**
** Any conditional compilation macros that affect link compatibility
** should be included here. For documentation on the meaning of these macros,
** see runtime/mercury_conf_param.h.
**
** IMPORTANT: any changes here may also require changes to
**	runtime/mercury_conf_param.h
** 	scripts/init_grade_options.sh-subr
** 	scripts/parse_grade_options.sh-subr
** 	scripts/final_grade_options.sh-subr
** 	scripts/mgnuc.in
** 	scripts/ml.in
**	compiler/handle_options.m
**	compiler/compile_target_code.m
*/

#ifndef MERCURY_GRADES_H
#define MERCURY_GRADES_H

#include "mercury_std.h"	/* for MR_STRINGIFY and MR_PASTE2 */
#include "mercury_tags.h"	/* for MR_TAGBITS */

/*
** The following series of tests define two macros piece by piece.
**
** MR_GRADE encodes the value of all the grade options; we use it to ensure
** that all object files linked together have the same values of these options.
**
** MR_GRADE_OPT encodes the values of only the grade options that it makes
** sense to change on the compiler command line.
*/

/*
** Part 0 of the grade is a binary compatibility version number.
** You should increment it any time you make a change that breaks
** binary backwards compatibility.
** Note that the binary compatibility version number has no direct
** relationship with the source release number (which is in ../VERSION).
**
** It is a good idea to inspect all code for RTTI version number checks
** and remove them when increasing the binary compatibility version number.   
** Searching for MR_RTTI_VERSION__ should find all code related to the
** RTTI version number.
**
** The MR_GRADE_EXEC_TRACE_VERSION_NO and MR_GRADE_DEEP_PROF_VERSION_NO
** macros should be incremented when a change breaks binary backwards
** compatibility only in debugging and deep profiling grades respectively.
*/

#define MR_GRADE_PART_0	v12_
#define MR_GRADE_EXEC_TRACE_VERSION_NO	1
#define MR_GRADE_DEEP_PROF_VERSION_NO	1

#ifdef MR_HIGHLEVEL_CODE

  #ifdef MR_HIGHLEVEL_DATA
    #define MR_GRADE_PART_1		MR_PASTE2(MR_GRADE_PART_0, hl)
    #define MR_GRADE_OPT_PART_1		"hl"
  #else
    #define MR_GRADE_PART_1		MR_PASTE2(MR_GRADE_PART_0, hlc)
    #define MR_GRADE_OPT_PART_1		"hlc"
  #endif

  #ifdef MR_USE_GCC_NESTED_FUNCTIONS
    #define MR_GRADE_PART_2		MR_PASTE2(MR_GRADE_PART_1, _nest)
    #define MR_GRADE_OPT_PART_2		MR_GRADE_OPT_PART_1  "_nest"
  #else
    #define MR_GRADE_PART_2		MR_GRADE_PART_1
    #define MR_GRADE_OPT_PART_2		MR_GRADE_OPT_PART_1
  #endif

#else /* ! MR_HIGHLEVEL_CODE */

  #ifdef MR_USE_ASM_LABELS
    #define MR_GRADE_PART_1		MR_PASTE2(MR_GRADE_PART_0, asm_)
    #define MR_GRADE_OPT_PART_1		"asm_"
  #else
    #define MR_GRADE_PART_1		MR_GRADE_PART_0
    #define MR_GRADE_OPT_PART_1		""
  #endif

  #ifdef MR_USE_GCC_NONLOCAL_GOTOS
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_PART_2		MR_PASTE2(MR_GRADE_PART_1, fast)
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "fast"
    #else
      #define MR_GRADE_PART_2		MR_PASTE2(MR_GRADE_PART_1, jump)
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "jump"
    #endif
  #else
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_PART_2		MR_PASTE2(MR_GRADE_PART_1, reg)
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "reg"
    #else
      #define MR_GRADE_PART_2		MR_PASTE2(MR_GRADE_PART_1, none)
      #define MR_GRADE_OPT_PART_2	MR_GRADE_OPT_PART_1 "none"
    #endif
  #endif

#endif /* ! MR_HIGHLEVEL_CODE */

#ifdef MR_THREAD_SAFE
  #define MR_GRADE_PART_3	MR_PASTE2(MR_GRADE_PART_2, _par)
  #define MR_GRADE_OPT_PART_3	MR_GRADE_OPT_PART_2 ".par"
#else
  #define MR_GRADE_PART_3	MR_GRADE_PART_2
  #define MR_GRADE_OPT_PART_3	MR_GRADE_OPT_PART_2
#endif

#if defined(MR_MPS_GC)
  #define MR_GRADE_PART_4	MR_PASTE2(MR_GRADE_PART_3, _mps)
  #define MR_GRADE_OPT_PART_4	MR_GRADE_OPT_PART_3 ".mps"
#elif defined(MR_BOEHM_GC) || defined(MR_CONSERVATIVE_GC)
  #define MR_GRADE_PART_4	MR_PASTE2(MR_GRADE_PART_3, _gc)
  #define MR_GRADE_OPT_PART_4	MR_GRADE_OPT_PART_3 ".gc"
#elif defined(MR_NATIVE_GC)
  #define MR_GRADE_PART_4	MR_PASTE2(MR_GRADE_PART_3, _agc)
  #define MR_GRADE_OPT_PART_4	MR_GRADE_OPT_PART_3 ".agc"
#else
  #define MR_GRADE_PART_4	MR_GRADE_PART_3
  #define MR_GRADE_OPT_PART_4	MR_GRADE_OPT_PART_3
#endif

#ifdef MR_DEEP_PROFILING
  #define MR_GRADE_PART_5	MR_PASTE3(MR_GRADE_PART_4, _profdeep, MR_GRADE_DEEP_PROF_VERSION_NO)
  #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".profdeep" MR_STRINGIFY(MR_GRADE_DEEP_PROF_VERSION_NO)
  #if defined(MR_MPROF_PROFILE_TIME) || defined(MR_MPROF_PROFILE_CALLS) \
	|| defined(MR_MPROF_PROFILE_MEMORY)
    /*
    ** Deep profiling is completely separate from the other profiling
    ** alternatives, and there is no point in allowing their combination.
    */
    #error "Invalid combination of profiling options"
  #endif
#else /* ! MR_DEEP_PROFILING */
  #ifdef MR_MPROF_PROFILE_TIME
    #ifdef MR_MPROF_PROFILE_CALLS
      #ifdef MR_MPROF_PROFILE_MEMORY
        #define MR_GRADE_PART_5		MR_PASTE2(MR_GRADE_PART_4, _profall)
        #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".profall"
      #else /* ! MR_MPROF_PROFILE_MEMORY */
        #define MR_GRADE_PART_5		MR_PASTE2(MR_GRADE_PART_4, _prof)
        #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".prof"
      #endif /* ! MR_MPROF_PROFILE_MEMORY */
    #else /* ! MR_MPROF_PROFILE_CALLS */
      #ifdef MR_MPROF_PROFILE_MEMORY
        /*
        ** Memory profiling interferes with time profiling,
        ** so there's no point in allowing this.
        */
        #error "Invalid combination of profiling options"
      #else /* ! MR_MPROF_PROFILE_MEMORY */
        /* Currently useless, but... */
        #define MR_GRADE_PART_5		MR_PASTE2(MR_GRADE_PART_4, _proftime)
        #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".proftime"
      #endif /* MR_MPROF_PROFILE_MEMORY */
    #endif /* MR_MPROF_PROFILE_CALLS */
  #else /* ! MR_MPROF_PROFILE_TIME */
    #ifdef MR_MPROF_PROFILE_CALLS
      #ifdef MR_MPROF_PROFILE_MEMORY
        #define MR_GRADE_PART_5		MR_PASTE2(MR_GRADE_PART_4, _memprof)
        #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".memprof"
      #else /* ! MR_MPROF_PROFILE_MEMORY */
        #define MR_GRADE_PART_5		MR_PASTE2(MR_GRADE_PART_4, _profcalls)
        #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4 ".profcalls"
      #endif /* MR_MPROF_PROFILE_MEMORY */
    #else /* ! MR_MPROF_PROFILE_CALLS */
      #ifdef MR_MPROF_PROFILE_MEMORY
        /*
        ** Call-graph memory profiling requires call profiling,
        ** and call profiling is reasonably cheap, so there's
        ** no point in allowing this.
        */
        #error "Invalid combination of profiling options"
      #else /* ! MR_MPROF_PROFILE_MEMORY */
        #define MR_GRADE_PART_5		MR_GRADE_PART_4
        #define MR_GRADE_OPT_PART_5	MR_GRADE_OPT_PART_4
      #endif /* MR_MPROF_PROFILE_MEMORY */
    #endif /* MR_MPROF_PROFILE_CALLS */
  #endif /* ! MR_MPROF_PROFILE_TIME */
#endif /* MR_DEEP_PROFILING */

#ifdef MR_USE_TRAIL
  #define MR_GRADE_PART_6	MR_PASTE2(MR_GRADE_PART_5, _tr)
  #define MR_GRADE_OPT_PART_6	MR_GRADE_OPT_PART_5 ".tr"
#else
  #define MR_GRADE_PART_6	MR_GRADE_PART_5
  #define MR_GRADE_OPT_PART_6	MR_GRADE_OPT_PART_5
#endif

#ifdef MR_RESERVE_TAG
  #define MR_GRADE_PART_7	MR_PASTE2(MR_GRADE_PART_6, _rt)
  #define MR_GRADE_OPT_PART_7	MR_GRADE_OPT_PART_6 ".rt"
#else
  #define MR_GRADE_PART_7	MR_GRADE_PART_6
  #define MR_GRADE_OPT_PART_7	MR_GRADE_OPT_PART_6
#endif

#ifdef MR_USE_MINIMAL_MODEL
  #define MR_GRADE_PART_8	MR_PASTE2(MR_GRADE_PART_7, _mm)
  #define MR_GRADE_OPT_PART_8	MR_GRADE_OPT_PART_7 ".mm"
#else
  #define MR_GRADE_PART_8	MR_GRADE_PART_7
  #define MR_GRADE_OPT_PART_8	MR_GRADE_OPT_PART_7
#endif

/*
** Minimal model tabling works by saving and restoring segments of the nondet
** stack. Since in high level code grades we don't have a nondet stack that
** we can save and restore, minimal model tabling is fundamentally incompatible
** with high level code.
*/

#if  defined(MR_USE_MINIMAL_MODEL) && defined(MR_HIGHLEVEL_CODE)
  #error "high level code and minimal model tabling are not compatible"
#endif

/*
** Saving and restoring the trail state would not be sufficient
** to handle the combination of trailing and minimal model tabling.
** Consider the following sequence of events:
**
**	execution enters a goal being committed across
**	a new entry is pushed on the trail
**	a tabled goal suspends,
**		causing the saving of a trail segment
**		and then a failure
**	the goal being committed across fails,
**		which invokes a failed commit on the trail entry
**	...
**	the tabled goal is resumed,
**		causing the restoring of the saved trail segment
**		and then a success
**	the goal being committed across now succeeds,
**		which invokes a successful commit on the trail entry
**
** The trail handler will be thoroughly confused by such a sequence.
**
** Until we can figure out (and implement) a fix for this problem,
** minimal model tabling and trailing cannot be used together.
*/

#if defined(MR_USE_TRAIL) && defined(MR_USE_MINIMAL_MODEL)
  #error "trailing and minimal model tabling are not compatible"
#endif

/*
** Native gc needs to be able to redirect pointers to the heap. Minimal model
** tabling takes snapshots of stack segments that may contain pointers to the
** heap, but there is currently no mechanism implemented to trace and redirect
** such pointers.
**
** Minimal model tabling has no problems with conservative gc or with no gc,
** since in those grades pointers are stable, and conservative gc looks for
** roots in stack segments copies the same way as in the rest of the address
** space.
*/

#if defined(MR_USE_MINIMAL_MODEL) && defined(MR_NATIVE_GC)
    #error "minimal model tabling and native gc are not compatible"
#endif

/*
** Parts 9-10 (i.e. tag bits, and (un)boxed float) are documented as
** "not for general use", and can't be set via the `--grade' option;
** we therefore can't make them part of the grade option string.
**
** Likewise part 11 (i.e. MR_NEW_MERCURYFILE_STRUCT) can't be set
** by the `--grade' option; it is intended to be set by the configure script
** at configuration time. So we don't include it in the grade option string.
*/

#if MR_TAGBITS == 0
  #define MR_GRADE_PART_9	MR_PASTE2(MR_GRADE_PART_8, _notags)
#elif defined(MR_HIGHTAGS)
  #define MR_GRADE_PART_9	MR_PASTE2(MR_GRADE_PART_8, \
  					MR_PASTE2(_hightags, MR_TAGBITS))
#else
  #define MR_GRADE_PART_9	MR_PASTE2(MR_GRADE_PART_8, \
  					MR_PASTE2(_tags, MR_TAGBITS))
#endif
#define MR_GRADE_OPT_PART_9	MR_GRADE_OPT_PART_8

#ifdef MR_BOXED_FLOAT
  #define MR_GRADE_PART_10	MR_GRADE_PART_9
#else				/* "ubf" stands for "unboxed float" */
  #define MR_GRADE_PART_10	MR_PASTE2(MR_GRADE_PART_9, _ubf)
#endif
#define MR_GRADE_OPT_PART_10	MR_GRADE_OPT_PART_9

#ifdef MR_NEW_MERCURYFILE_STRUCT
  #define MR_GRADE_PART_11	MR_PASTE2(MR_GRADE_PART_10, _file)
#else
  #define MR_GRADE_PART_11	MR_GRADE_PART_10
#endif
#define MR_GRADE_OPT_PART_11	MR_GRADE_OPT_PART_10

#if defined(MR_USE_REGPARM) && defined(MR_HIGHLEVEL_CODE) && defined(__i386__)
  #define MR_GRADE_PART_12	MR_PASTE2(MR_GRADE_PART_11, _regparm)
  #define MR_GRADE_OPT_PART_12	MR_GRADE_OPT_PART_11 ".regparm"
#elif defined(MR_PIC_REG) && defined(MR_USE_GCC_GLOBAL_REGISTERS) && \
					defined(__i386__)
  #define MR_GRADE_PART_12	MR_PASTE2(MR_GRADE_PART_11, _picreg)
  #define MR_GRADE_OPT_PART_12	MR_GRADE_OPT_PART_11 ".picreg"
#else
  #define MR_GRADE_PART_12	MR_GRADE_PART_11
  #define MR_GRADE_OPT_PART_12	MR_GRADE_OPT_PART_11
#endif

#if defined(MR_DECL_DEBUG)
  #define MR_GRADE_PART_13		MR_PASTE3(MR_GRADE_PART_12, _decldebug, MR_GRADE_EXEC_TRACE_VERSION_NO)
  #define MR_GRADE_OPT_PART_13		MR_GRADE_OPT_PART_12 ".decldebug" MR_STRINGIFY(MR_GRADE_EXEC_TRACE_VERSION_NO)
  #if ! defined(MR_EXEC_TRACE)
    #error "declarative debugging require execution tracing"
  #endif
#else
  #if defined(MR_EXEC_TRACE)
    #define MR_GRADE_PART_13		MR_PASTE3(MR_GRADE_PART_12, _debug, MR_GRADE_EXEC_TRACE_VERSION_NO)
    #define MR_GRADE_OPT_PART_13	MR_GRADE_OPT_PART_12 ".debug" MR_STRINGIFY(MR_GRADE_EXEC_TRACE_VERSION_NO)
    #else
      #define MR_GRADE_PART_13		MR_GRADE_PART_12
      #define MR_GRADE_OPT_PART_13	MR_GRADE_OPT_PART_12
    #endif
#endif

#ifdef MR_RECORD_TERM_SIZES
  #ifdef MR_RECORD_TERM_SIZES_AS_CELLS
    #define MR_GRADE_PART_14		MR_PASTE2(MR_GRADE_PART_13, _tsc)
    #define MR_GRADE_OPT_PART_14	MR_GRADE_OPT_PART_13 ".tsc"
  #else
    #define MR_GRADE_PART_14		MR_PASTE2(MR_GRADE_PART_13, _tsw)
    #define MR_GRADE_OPT_PART_14	MR_GRADE_OPT_PART_13 ".tsw"
  #endif
#else
  #define MR_GRADE_PART_14	MR_GRADE_PART_13
  #define MR_GRADE_OPT_PART_14	MR_GRADE_OPT_PART_13
#endif

#define MR_GRADE		MR_GRADE_PART_14
#define MR_GRADE_OPT		MR_GRADE_OPT_PART_14

#define MR_GRADE_VAR		MR_PASTE2(MR_grade_,MR_GRADE)
#define MR_GRADE_STRING 	MR_STRINGIFY(MR_GRADE)

extern const char MR_GRADE_VAR;

#endif /* MERCURY_GRADES_H */
