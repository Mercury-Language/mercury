/*
** vim:ts=4 sw=4 expandtab
*/
/*
** Copyright (C) 1997-2011 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_grade.h - defines the MR_GRADE macro.
**
** This is used to get the linker to ensure that different object files
** were compiled with consistent grades.
**
** Any conditional compilation macros that affect link compatibility
** should be included here. For documentation on the meaning of these macros,
** see runtime/mercury_conf_param.h.
**
** IMPORTANT: any changes here may also require changes to
**      runtime/mercury_conf_param.h
**      scripts/init_grade_options.sh-subr
**      scripts/canonical_grade.sh-subr
**      scripts/parse_grade_options.sh-subr
**      scripts/final_grade_options.sh-subr
**      scripts/mgnuc.in
**      scripts/ml.in
**      compiler/handle_options.m
**      compiler/compile_target_code.m
**      configure.in
*/

#ifndef MERCURY_GRADES_H
#define MERCURY_GRADES_H

#include "mercury_std.h"        /* for MR_STRINGIFY and MR_PASTE2 */
#include "mercury_tags.h"       /* for MR_TAGBITS */

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
** The MR_GRADE_EXEC_TRACE_VERSION_NO, MR_GRADE_DEEP_PROF_VERSION_NO and
** MR_GRADE_LLC_PAR_VERSION_NO macros should be incremented when a change
** breaks binary backwards compatibility only in debugging, deep profiling
** and low-level C parallel grades respectively.
*/

#define MR_GRADE_PART_0 v18_
#define MR_GRADE_EXEC_TRACE_VERSION_NO  11
#define MR_GRADE_DEEP_PROF_VERSION_NO   4
#define MR_GRADE_LLC_PAR_VERSION_NO 1

#ifdef MR_HIGHLEVEL_CODE

  #ifdef MR_HIGHLEVEL_DATA
    #define MR_GRADE_PART_1             MR_PASTE2(MR_GRADE_PART_0, hl)
    #define MR_GRADE_OPT_PART_1         "hl"
  #else
    #define MR_GRADE_PART_1             MR_PASTE2(MR_GRADE_PART_0, hlc)
    #define MR_GRADE_OPT_PART_1         "hlc"
  #endif

  #ifdef MR_USE_GCC_NESTED_FUNCTIONS
    #define MR_GRADE_PART_2             MR_PASTE2(MR_GRADE_PART_1, _nest)
    #define MR_GRADE_OPT_PART_2         MR_GRADE_OPT_PART_1  "_nest"
  #else
    #define MR_GRADE_PART_2             MR_GRADE_PART_1
    #define MR_GRADE_OPT_PART_2         MR_GRADE_OPT_PART_1
  #endif

  /*
  ** This grade component is repeated below version information.
  */
  #ifdef MR_THREAD_SAFE
    #define MR_GRADE_PART_3       MR_PASTE2(MR_GRADE_PART_2, _par)
    #define MR_GRADE_OPT_PART_3   MR_GRADE_OPT_PART_2 ".par"
  #else
    #define MR_GRADE_PART_3       MR_GRADE_PART_2
    #define MR_GRADE_OPT_PART_3   MR_GRADE_OPT_PART_2
  #endif

#else /* ! MR_HIGHLEVEL_CODE */

  #ifdef MR_USE_ASM_LABELS
    #define MR_GRADE_PART_1             MR_PASTE2(MR_GRADE_PART_0, asm_)
    #define MR_GRADE_OPT_PART_1         "asm_"
  #else
    #define MR_GRADE_PART_1             MR_GRADE_PART_0
    #define MR_GRADE_OPT_PART_1         ""
  #endif

  #ifdef MR_USE_GCC_NONLOCAL_GOTOS
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_PART_2           MR_PASTE2(MR_GRADE_PART_1, fast)
      #define MR_GRADE_OPT_PART_2       MR_GRADE_OPT_PART_1 "fast"
    #else
      #define MR_GRADE_PART_2           MR_PASTE2(MR_GRADE_PART_1, jump)
      #define MR_GRADE_OPT_PART_2       MR_GRADE_OPT_PART_1 "jump"
    #endif
  #else
    #ifdef MR_USE_GCC_GLOBAL_REGISTERS
      #define MR_GRADE_PART_2           MR_PASTE2(MR_GRADE_PART_1, reg)
      #define MR_GRADE_OPT_PART_2       MR_GRADE_OPT_PART_1 "reg"
    #else
      #define MR_GRADE_PART_2           MR_PASTE2(MR_GRADE_PART_1, none)
      #define MR_GRADE_OPT_PART_2       MR_GRADE_OPT_PART_1 "none"
    #endif
  #endif

  /*
  ** This grade component is repeated above without the version information.
  */
  #ifdef MR_THREAD_SAFE
    #define MR_GRADE_PART_3       MR_PASTE3(MR_GRADE_PART_2, _par, MR_GRADE_LLC_PAR_VERSION_NO)
    #define MR_GRADE_OPT_PART_3   MR_GRADE_OPT_PART_2 ".par"
  #else
    #define MR_GRADE_PART_3       MR_GRADE_PART_2
    #define MR_GRADE_OPT_PART_3   MR_GRADE_OPT_PART_2
  #endif

#endif /* ! MR_HIGHLEVEL_CODE */

#if defined(MR_MPS_GC)
  #define MR_GRADE_PART_4       MR_PASTE2(MR_GRADE_PART_3, _mps)
  #define MR_GRADE_OPT_PART_4   MR_GRADE_OPT_PART_3 ".mps"
#elif defined(MR_HGC)
  #define MR_GRADE_PART_4       MR_PASTE2(MR_GRADE_PART_3, _hgc)
  #define MR_GRADE_OPT_PART_4   MR_GRADE_OPT_PART_3 ".hgc"
#elif defined(MR_BOEHM_GC_DEBUG)
  #define MR_GRADE_PART_4       MR_PASTE2(MR_GRADE_PART_3, _gcd)
  #define MR_GRADE_OPT_PART_4   MR_GRADE_OPT_PART_3 ".gcd"
#elif defined(MR_BOEHM_GC) || defined(MR_CONSERVATIVE_GC)
  #define MR_GRADE_PART_4       MR_PASTE2(MR_GRADE_PART_3, _gc)
  #define MR_GRADE_OPT_PART_4   MR_GRADE_OPT_PART_3 ".gc"
#elif defined(MR_NATIVE_GC)
  #define MR_GRADE_PART_4       MR_PASTE2(MR_GRADE_PART_3, _agc)
  #define MR_GRADE_OPT_PART_4   MR_GRADE_OPT_PART_3 ".agc"
#else
  #define MR_GRADE_PART_4       MR_GRADE_PART_3
  #define MR_GRADE_OPT_PART_4   MR_GRADE_OPT_PART_3
#endif

#ifdef MR_DEEP_PROFILING
  #define MR_GRADE_PART_5       MR_PASTE3(MR_GRADE_PART_4, _profdeep, MR_GRADE_DEEP_PROF_VERSION_NO)
  #define MR_GRADE_OPT_PART_5   MR_GRADE_OPT_PART_4 ".profdeep"
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
        #define MR_GRADE_PART_5         MR_PASTE2(MR_GRADE_PART_4, _profall)
        #define MR_GRADE_OPT_PART_5     MR_GRADE_OPT_PART_4 ".profall"
      #else /* ! MR_MPROF_PROFILE_MEMORY */
        #define MR_GRADE_PART_5         MR_PASTE2(MR_GRADE_PART_4, _prof)
        #define MR_GRADE_OPT_PART_5     MR_GRADE_OPT_PART_4 ".prof"
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
        #define MR_GRADE_PART_5         MR_PASTE2(MR_GRADE_PART_4, _proftime)
        #define MR_GRADE_OPT_PART_5     MR_GRADE_OPT_PART_4 ".proftime"
      #endif /* MR_MPROF_PROFILE_MEMORY */
    #endif /* MR_MPROF_PROFILE_CALLS */
  #else /* ! MR_MPROF_PROFILE_TIME */
    #ifdef MR_MPROF_PROFILE_CALLS
      #ifdef MR_MPROF_PROFILE_MEMORY
        #define MR_GRADE_PART_5         MR_PASTE2(MR_GRADE_PART_4, _memprof)
        #define MR_GRADE_OPT_PART_5     MR_GRADE_OPT_PART_4 ".memprof"
      #else /* ! MR_MPROF_PROFILE_MEMORY */
        #define MR_GRADE_PART_5         MR_PASTE2(MR_GRADE_PART_4, _profcalls)
        #define MR_GRADE_OPT_PART_5     MR_GRADE_OPT_PART_4 ".profcalls"
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
        #define MR_GRADE_PART_5         MR_GRADE_PART_4
        #define MR_GRADE_OPT_PART_5     MR_GRADE_OPT_PART_4
      #endif /* MR_MPROF_PROFILE_MEMORY */
    #endif /* MR_MPROF_PROFILE_CALLS */
  #endif /* ! MR_MPROF_PROFILE_TIME */
#endif /* MR_DEEP_PROFILING */

#ifdef MR_RECORD_TERM_SIZES
  #ifdef MR_RECORD_TERM_SIZES_AS_CELLS
    #define MR_GRADE_PART_6             MR_PASTE2(MR_GRADE_PART_5, _tsc)
    #define MR_GRADE_OPT_PART_6         MR_GRADE_OPT_PART_5 ".tsc"
  #else
    #define MR_GRADE_PART_6             MR_PASTE2(MR_GRADE_PART_5, _tsw)
    #define MR_GRADE_OPT_PART_6         MR_GRADE_OPT_PART_5 ".tsw"
  #endif
#else
  #define MR_GRADE_PART_6       MR_GRADE_PART_5
  #define MR_GRADE_OPT_PART_6   MR_GRADE_OPT_PART_5
#endif

#ifdef MR_USE_TRAIL
   #ifdef MR_TRAIL_SEGMENTS
    #define MR_GRADE_PART_7     MR_PASTE2(MR_GRADE_PART_6, _trseg)
    #define MR_GRADE_OPT_PART_7 MR_GRADE_OPT_PART_6 ".trseg"
  #else
    #define MR_GRADE_PART_7     MR_PASTE2(MR_GRADE_PART_6, _tr)
    #define MR_GRADE_OPT_PART_7 MR_GRADE_OPT_PART_6 ".tr"
   #endif /* ! MR_TRAIL_SEGMENTS */
#else
  #define MR_GRADE_PART_7       MR_GRADE_PART_6
  #define MR_GRADE_OPT_PART_7   MR_GRADE_OPT_PART_6
#endif

/*
** Grade component 8 used to be used for the .rt (reserve tag) grades.
** It is currently unused.
*/
#define MR_GRADE_PART_8         MR_GRADE_PART_7
#define MR_GRADE_OPT_PART_8     MR_GRADE_OPT_PART_7

#ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
  #ifdef MR_USE_MINIMAL_MODEL_OWN_STACKS
    #error "Invalid combination of minimal model tabling options"
  #endif
#endif

#ifdef MR_USE_MINIMAL_MODEL_STACK_COPY
  #ifdef MR_MINIMAL_MODEL_DEBUG
    #define MR_GRADE_PART_9     MR_PASTE2(MR_GRADE_PART_8, _dmmsc)
    #define MR_GRADE_OPT_PART_9 MR_GRADE_OPT_PART_8 ".dmmsc"
  #else
    #define MR_GRADE_PART_9     MR_PASTE2(MR_GRADE_PART_8, _mmsc)
    #define MR_GRADE_OPT_PART_9 MR_GRADE_OPT_PART_8 ".mmsc"
  #endif
#elif MR_USE_MINIMAL_MODEL_OWN_STACKS
  #ifdef MR_MINIMAL_MODEL_DEBUG
    #define MR_GRADE_PART_9     MR_PASTE2(MR_GRADE_PART_8, _dmmos)
    #define MR_GRADE_OPT_PART_9 MR_GRADE_OPT_PART_8 ".dmmos"
  #else
    #define MR_GRADE_PART_9     MR_PASTE2(MR_GRADE_PART_8, _mmos)
    #define MR_GRADE_OPT_PART_9 MR_GRADE_OPT_PART_8 ".mmos"
  #endif
#else
  #define MR_GRADE_PART_9       MR_GRADE_PART_8
  #define MR_GRADE_OPT_PART_9   MR_GRADE_OPT_PART_8
#endif

/*
** One implementation of minimal model tabling works by saving and restoring
** segments of the nondet stack, the other by creating a separate stack for
** each generator. Since in high level code grades we don't have a nondet
** stack that we can save and restore and we can't establish extra stacks,
** both forms of minimal model tabling are fundamentally incompatible
** with high level code.
*/

#if defined(MR_HIGHLEVEL_CODE) && \
        (defined(MR_USE_MINIMAL_MODEL_STACK_COPY) || \
        defined(MR_USE_MINIMAL_MODEL_OWN_STACKS))
  #error "high level code and minimal model tabling are not compatible"
#endif

/*
** Saving and restoring the trail state would not be sufficient
** to handle the combination of trailing and minimal model tabling.
** Consider the following sequence of events:
**
**      execution enters a goal being committed across
**      a new entry is pushed on the trail
**      a tabled goal suspends,
**              causing the saving of a trail segment
**              and then a failure
**      the goal being committed across fails,
**              which invokes a failed commit on the trail entry
**      ...
**      the tabled goal is resumed,
**              causing the restoring of the saved trail segment
**              and then a success
**      the goal being committed across now succeeds,
**              which invokes a successful commit on the trail entry
**
** The trail handler will be thoroughly confused by such a sequence.
**
** Until we can figure out (and implement) a fix for this problem,
** minimal model tabling (either form) and trailing cannot be used together.
*/

#if defined(MR_USE_TRAIL) && \
        (defined(MR_USE_MINIMAL_MODEL_STACK_COPY) || \
        defined(MR_USE_MINIMAL_MODEL_OWN_STACKS))
  #error "trailing and minimal model tabling are not compatible"
#endif

/*
** Native gc needs to be able to redirect pointers to the heap. Minimal model
** tabling takes snapshots of stack segments that may contain pointers to the
** heap, or creates extra stacks that may contain pointers to the heap,
** but there is currently no mechanism implemented to trace and redirect
** such pointers.
**
** Minimal model tabling has no problems with conservative gc or with no gc,
** since in those grades pointers are stable, and conservative gc looks for
** roots in stack segments copies the same way as in the rest of the address
** space.
*/

#if defined(MR_NATIVE_GC) && \
        (defined(MR_USE_MINIMAL_MODEL_STACK_COPY) || \
        defined(MR_USE_MINIMAL_MODEL_OWN_STACKS))
    #error "minimal model tabling and native gc are not compatible"
#endif

/*
** Parts 10-11 (i.e. tag bits, and (un)boxed float) are documented as
** "not for general use", and can't be set via the `--grade' option;
** we therefore can't make them part of the grade option string.
**
** Single-precision floats do form part of the grade option string
** and they imply unboxed floats.
**
** Part 12 (i.e. MR_NEW_MERCURYFILE_STRUCT) can't be set
** by the `--grade' option; it is intended to be set by the configure script
** at configuration time. So we don't include it in the grade option string.
*/

#if MR_TAGBITS == 0
  #define MR_GRADE_PART_10      MR_PASTE2(MR_GRADE_PART_9, _notags)
#elif defined(MR_HIGHTAGS)
  #define MR_GRADE_PART_10      MR_PASTE2(MR_GRADE_PART_9, \
                                        MR_PASTE2(_hightags, MR_TAGBITS))
#else
  #define MR_GRADE_PART_10      MR_PASTE2(MR_GRADE_PART_9, \
                                        MR_PASTE2(_tags, MR_TAGBITS))
#endif
#define MR_GRADE_OPT_PART_10    MR_GRADE_OPT_PART_9

#if defined(MR_PREGENERATED_DIST)
  #define MR_GRADE_PART_11      MR_PASTE2(MR_GRADE_PART_10, _pregen)
  #define MR_GRADE_OPT_PART_11  MR_GRADE_OPT_PART_10 ".pregen"
#elif defined(MR_USE_SINGLE_PREC_FLOAT)
  #if defined(MR_BOXED_FLOAT)
    #error "single-precision floats implies unboxed floats"
  #endif
  #define MR_GRADE_PART_11      MR_PASTE2(MR_GRADE_PART_10, _spf)
  #define MR_GRADE_OPT_PART_11  MR_GRADE_OPT_PART_10 ".spf"
#elif defined(MR_BOXED_FLOAT)
  #define MR_GRADE_PART_11      MR_GRADE_PART_10
  #define MR_GRADE_OPT_PART_11  MR_GRADE_OPT_PART_10
#else                           /* "ubf" stands for "unboxed float" */
  #define MR_GRADE_PART_11      MR_PASTE2(MR_GRADE_PART_10, _ubf)
  #define MR_GRADE_OPT_PART_11  MR_GRADE_OPT_PART_10
#endif

#ifdef MR_NEW_MERCURYFILE_STRUCT
  #define MR_GRADE_PART_12      MR_PASTE2(MR_GRADE_PART_11, _file)
#else
  #define MR_GRADE_PART_12      MR_GRADE_PART_11
#endif
#define MR_GRADE_OPT_PART_12    MR_GRADE_OPT_PART_11

#if defined(MR_USE_REGPARM) && defined(MR_HIGHLEVEL_CODE) && defined(__i386__)
  #define MR_GRADE_PART_13      MR_PASTE2(MR_GRADE_PART_12, _regparm)
  #define MR_GRADE_OPT_PART_13  MR_GRADE_OPT_PART_12 ".regparm"
#elif defined(MR_PIC_REG) && defined(MR_USE_GCC_GLOBAL_REGISTERS) && \
                                        defined(__i386__)
  #define MR_GRADE_PART_13      MR_PASTE2(MR_GRADE_PART_12, _picreg)
  #define MR_GRADE_OPT_PART_13  MR_GRADE_OPT_PART_12 ".picreg"
#else
  #define MR_GRADE_PART_13      MR_GRADE_PART_12
  #define MR_GRADE_OPT_PART_13  MR_GRADE_OPT_PART_12
#endif

#if defined(MR_DECL_DEBUG)
  #define MR_GRADE_PART_14              MR_PASTE3(MR_GRADE_PART_13, _decldebug, MR_GRADE_EXEC_TRACE_VERSION_NO)
  #define MR_GRADE_OPT_PART_14          MR_GRADE_OPT_PART_13 ".decldebug"
  #if ! defined(MR_EXEC_TRACE)
    #error "declarative debugging requires execution tracing"
  #endif
#else
  #if defined(MR_EXEC_TRACE)
    #define MR_GRADE_PART_14            MR_PASTE3(MR_GRADE_PART_13, _debug, MR_GRADE_EXEC_TRACE_VERSION_NO)
    #define MR_GRADE_OPT_PART_14        MR_GRADE_OPT_PART_13 ".debug"
    #else
      #if defined(MR_SS_DEBUG)
        #define MR_GRADE_PART_14        MR_PASTE3(MR_GRADE_PART_13, _ssdebug, MR_GRADE_EXEC_TRACE_VERSION_NO)
        #define MR_GRADE_OPT_PART_14    MR_GRADE_OPT_PART_13 ".ssdebug"
      #else
        #define MR_GRADE_PART_14        MR_GRADE_PART_13
        #define MR_GRADE_OPT_PART_14    MR_GRADE_OPT_PART_13
      #endif
    #endif
#endif

#if defined(MR_LL_DEBUG)
  #define MR_GRADE_PART_15      MR_PASTE2(MR_GRADE_PART_14, _ll_debug)
  #define MR_GRADE_OPT_PART_15  MR_GRADE_OPT_PART_14 ".ll_debug"
#else
  #define MR_GRADE_PART_15      MR_GRADE_PART_14
  #define MR_GRADE_OPT_PART_15  MR_GRADE_OPT_PART_14
#endif

#if defined(MR_EXTEND_STACKS_WHEN_NEEDED)
  #define MR_GRADE_PART_16      MR_PASTE2(MR_GRADE_PART_15, _exts)
  #define MR_GRADE_OPT_PART_16  MR_GRADE_OPT_PART_15 ".exts"
  #if defined(MR_HIGHLEVEL_CODE)
    #error "--extend-stacks-when-needed and --high-level-code are not compatible"
  #endif
  #if defined(MR_STACK_SEGMENTS)
    #error "--extend-stacks-when-needed and --stack-segments are not compatible"
  #endif
#elif defined(MR_STACK_SEGMENTS)
  #define MR_GRADE_PART_16      MR_PASTE2(MR_GRADE_PART_15, _stseg)
  #define MR_GRADE_OPT_PART_16  MR_GRADE_OPT_PART_15 ".stseg"
  #if defined(MR_HIGHLEVEL_CODE)
    #error "--stack-segments and --high-level-code are not compatible"
  #endif
#else
  #define MR_GRADE_PART_16      MR_GRADE_PART_15
  #define MR_GRADE_OPT_PART_16  MR_GRADE_OPT_PART_15
#endif

#if defined(MR_USE_REGIONS)
  #if defined(MR_RBMM_DEBUG)
    #if defined(MR_RBMM_PROFILING)
      #define MR_GRADE_PART_17          MR_PASTE2(MR_GRADE_PART_16, _rbmmdp)
      #define MR_GRADE_OPT_PART_17      MR_GRADE_OPT_PART_16 ".rbmmdp"
    #else
      #define MR_GRADE_PART_17          MR_PASTE2(MR_GRADE_PART_16, _rbmmd)
      #define MR_GRADE_OPT_PART_17      MR_GRADE_OPT_PART_16 ".rbmmd"
    #endif
  #else
    #if defined(MR_RBMM_PROFILING)
      #define MR_GRADE_PART_17          MR_PASTE2(MR_GRADE_PART_16, _rbmmp)
      #define MR_GRADE_OPT_PART_17      MR_GRADE_OPT_PART_16 ".rbmmp"
    #else
      #define MR_GRADE_PART_17          MR_PASTE2(MR_GRADE_PART_16, _rbmm)
      #define MR_GRADE_OPT_PART_17      MR_GRADE_OPT_PART_16 ".rbmm"
    #endif
  #endif
#else
  #define MR_GRADE_PART_17      MR_GRADE_PART_16
  #define MR_GRADE_OPT_PART_17  MR_GRADE_OPT_PART_16
#endif

#if defined(MR_THREADSCOPE)
  #define MR_GRADE_PART_18      MR_PASTE2(MR_GRADE_PART_17, _threadscope)
  #define MR_GRADE_OPT_PART_18  MR_GRADE_OPT_PART_17 ".threadscope"
#else
  #define MR_GRADE_PART_18      MR_GRADE_PART_17
  #define MR_GRADE_OPT_PART_18  MR_GRADE_OPT_PART_17
#endif

#define MR_GRADE                MR_GRADE_PART_18
#define MR_GRADE_OPT            MR_GRADE_OPT_PART_18

#define MR_GRADE_VAR            MR_PASTE2(MR_grade_,MR_GRADE)
#define MR_GRADE_STRING         MR_STRINGIFY(MR_GRADE)

extern const char MR_GRADE_VAR;

#endif /* MERCURY_GRADES_H */
