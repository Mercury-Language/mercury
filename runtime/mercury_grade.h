/*
** Copyright (C) 1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_grades.h - defines the MR_GRADE macro.
**
** This is used to get the linker to ensure that different object files
** were compiled with consistent grades.
**
** Any condition compilation macros that affect link compatibility
** should be included here.
**
** IMPORTANT: any changes here may also require changes to
** 	scripts/parse_grade_options.sh-subr
** 	scripts/ml.in
**	compiler/handle_options.m
**	compiler/mercury_compile.m
*/

#ifndef MERCURY_GRADES_H
#define MERCURY_GRADES_H

/* convert a macro to a string */
#define MR_STRINGIFY(x)		MR_STRINGIFY_2(x)
#define MR_STRINGIFY_2(x)	#x

/* paste two macros together */
#define MR_PASTE2(p1,p2)	MR_PASTE2_2(p1,p2)
#define MR_PASTE2_2(p1,p2)	p1##p2

/* paste 9 macros together */
#define MR_PASTE10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
				MR_PASTE10_2(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)
#define MR_PASTE10_2(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
				p1##p2##p3##p4##p5##p6##p7##p8##p9##p10

/*
** Here we build up the MR_GRADE macro part at a time,
** based on the compilation flags.
**
** IMPORTANT: any changes here will probably require similar
** changes to compiler/handle_options.m and scripts/mgnuc.in.
*/

#ifdef USE_ASM_LABELS
  #define MR_GRADE_PART_1	asm_
#else
  #define MR_GRADE_PART_1
#endif

#ifdef USE_GCC_NONLOCAL_GOTOS
  #ifdef USE_GCC_GLOBAL_REGISTERS
    #define MR_GRADE_PART_2	fast
  #else
    #define MR_GRADE_PART_2	jump
  #endif
#else
  #ifdef USE_GCC_GLOBAL_REGS
    #define MR_GRADE_PART_2	reg
  #else
    #define MR_GRADE_PART_2	none
  #endif
#endif

#ifdef CONSERVATIVE_GC
  #define MR_GRADE_PART_3	_gc
#elif defined(NATIVE_GC)
  #define MR_GRADE_PART_3	_agc
#else
  #define MR_GRADE_PART_3
#endif

#ifdef PROFILE_TIME
  #ifdef PROFILE_CALLS
    #ifdef PROFILE_MEMORY
      #define MR_GRADE_PART_4	_profall
    #else
      #define MR_GRADE_PART_4	_prof
    #endif
  #else
    #ifdef PROFILE_MEMORY
      /*
      ** Memory profiling interferes with time profiling,
      ** so there's no point in allowing this.
      */
      #error "Invalid combination of profiling options"
    #else
      /* Currently useless, but... */
      #define MR_GRADE_PART_4	_proftime
    #endif
  #endif
#else
  #ifdef PROFILE_CALLS
    #ifdef PROFILE_MEMORY
      #define MR_GRADE_PART_4	_memprof
    #else
      #define MR_GRADE_PART_4	_profcalls
    #endif
  #else
    #ifdef PROFILE_MEMORY
      /*
      ** Call-graph memory profiling requires call profiling,
      ** and call profiling is reasonably cheap, so there's
      ** no point in allowing this.
      */
      #error "Invalid combination of profiling options"
    #else
      #define MR_GRADE_PART_4
    #endif
  #endif
#endif

#ifdef MR_USE_TRAIL
  #define MR_GRADE_PART_5	_tr
#else
  #define MR_GRADE_PART_5
#endif

#if TAGBITS == 0
  #define MR_GRADE_PART_6	_notags
#elif defined(HIGHTAGS)
  #define MR_GRADE_PART_6	MR_PASTE2(_hightags, TAGBITS)
#else
  #define MR_GRADE_PART_6	MR_PASTE2(_tags, TAGBITS)
#endif

#ifdef BOXED_FLOAT
  #define MR_GRADE_PART_7
#else				/* "ubf" stands for "unboxed float" */
  #define MR_GRADE_PART_7	_ubf
#endif

#ifdef COMPACT_ARGS
  #define MR_GRADE_PART_8	
#else				/* "sa" stands for "simple args" */
  #define MR_GRADE_PART_8	_sa
#endif

#ifdef SPEED
  #define MR_GRADE_PART_9
#else
  #define MR_GRADE_PART_9	_debug
#endif

#ifdef PIC_REG
  #define MR_GRADE_PART_10	_picreg
#else
  #define MR_GRADE_PART_10
#endif

#define MR_GRADE		MR_PASTE10(			\
					MR_GRADE_PART_1,	\
					MR_GRADE_PART_2,	\
					MR_GRADE_PART_3,	\
					MR_GRADE_PART_4,	\
					MR_GRADE_PART_5,	\
					MR_GRADE_PART_6,	\
					MR_GRADE_PART_7,	\
					MR_GRADE_PART_8,	\
					MR_GRADE_PART_9,	\
					MR_GRADE_PART_10	\
				)

#define MR_GRADE_VAR		MR_PASTE2(MR_grade_,MR_GRADE)
#define MR_GRADE_STRING 	MR_STRINGIFY(MR_GRADE)

extern const char MR_GRADE_VAR;

#endif /* MERCURY_GRADES_H */
