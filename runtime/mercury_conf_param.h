/*
** Copyright (C) 1997-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_conf_param.h:
**	Defines various configuration parameters. 
**
**	Configuration parameters fall into three groups.
**	They can be set automatically by autoconf.
**	They can be passed on the command line (e.g. the mgnuc
**	script sets some options based on the grade).
**	Or their values can be implied by the settings of other parameters.
**
**	The ones defined in mercury_conf.h are determined by autoconf.
**	The remainder are documented and/or defined by this file,
**	#included by mercury_conf.h.
*/

/*
** IMPORTANT NOTE:
** This file must not contain any #include statements,
** and may not define any global variables,
** for reasons explained in mercury_imp.h.
** This file should contain _only_ configuration macros.
*/

#ifndef MERCURY_CONF_PARAM_H
#define MERCURY_CONF_PARAM_H

/*---------------------------------------------------------------------------*/
/*
** Documentation for configuration parameters which can be set on the
** command line via `-D'.
*/

/*
** Code generation options:
**
** MR_HIGHLEVEL_CODE
** MR_HIGHLEVEL_DATA
** MR_USE_GCC_NESTED_FUNCTIONS
** USE_GCC_GLOBAL_REGISTERS
** USE_GCC_NONLOCAL_GOTOS
** USE_ASM_LABELS
** CONSERVATIVE_GC
** NATIVE_GC		[not yet working]
** NO_TYPE_LAYOUT
** BOXED_FLOAT
** MR_USE_TRAIL
** MR_USE_MINIMAL_MODEL
**	See the documentation for
**		--high-level-code
**		--high-level-data
**		--gcc-nested-functions
**		--gcc-global-registers
**		--gcc-non-local-gotos
**		--gcc-asm-labels
**		--gc conservative
**		--gc accurate		[not yet working]
**		--no-type-layout
**		--unboxed-float
**		--use-trail
**		--use-minimal-model
**	(respectively) in the mmc help message or the Mercury User's Guide.
**
** USE_SINGLE_PREC_FLOAT:
**	Use C's `float' rather than C's `double' for the
**	Mercury floating point type (`Float').
**
** USE_TYPE_TO_TERM:
**	Include `type_to_term' and `term_to_type' fields in type_infos.
**	[This is obsolete. USE_TYPE_LAYOUT is a better solution.]
**
** PARALLEL
**	Enable support for parallelism [not yet working].
**
** MR_NO_BACKWARDS_COMPAT
**	Disable backwards compatibility with C code using obsolete low-level
**	constructs, e.g. referring to variables and macros without their MR_
**	prefixes.
**
** MR_EXTRA_BACKWARDS_COMPAT
**	Add extra backwards compatibility with C code using obsolete low-level
**	constructs, e.g. referring to variables and macros without their MR_
**	prefixes.
*/

/*
** Runtime checking options:
**
** MR_CHECK_FOR_OVERFLOW
**	(Implied by MR_LOWLEVEL_DEBUG.)
**	Check for overflow of the various memory
**	areas, e.g. heap, det stack, nondet stack,
**	before every access that might result in overflow. 
**	Causes the generated code to become bigger and less efficient.
**	Slows down compilation.
**
**	Normally MR_CHECK_FOR_OVERFLOW is not set, since
**	we trap overflows using mprotect().
*/

/*
** Debugging options:
**
** MR_STACK_TRACE
**	Require the inclusion of the layout information needed by error/1
**	and the debugger to print stack traces. This effect is achieved by
**	including MR_STACK_TRACE in the mangled grade (see mercury_grade.h).
**
** MR_REQUIRE_TRACING
**	Require that all Mercury procedures linked in should be compiled
**	with at least interface tracing.  This effect is achieved
**	by including MR_REQUIRE_TRACING in the mangled grade
**	(see mercury_grade.h).
**	Note that MR_REQUIRE_TRACING is talking about execution tracing,
**	not stack tracing; these are two independently configurable features.
**
** MR_LOWLEVEL_DEBUG
**	Enables various low-level debugging stuff,
**	that was in the distant past used to debug
**	the low-level code generation.
**	Causes the generated code to become VERY big and VERY inefficient.
**	Slows down compilation a LOT.
**
** MR_DEBUG_DD_BACK_END
**	Enables low-level debugging messages on the operation of the
**	declarative debugging back end.
**
** MR_DEBUG_GOTOS
**	(Implied by MR_LOWLEVEL_DEBUG.)
**	Enables low-level debugging of gotos.
**	Causes the generated code to become bigger and less efficient.
**	Slows down compilation.
**
** MR_DEBUG_AGC_SCHEDULING
**	Display debugging information while scheduling accurate garbage
**	collection.
**
** MR_DEBUG_AGC_COLLECTION
**	Display debugging information while collecting garbage using the
**	accurate garbage collector.
**
** MR_DEBUG_AGC_FORWARDING
**	Display debugging information when leaving or finding forwarding
**	pointers during accurate garbage collection.
**
** MR_DEBUG_AGC_PRINT_VARS
**	Display the values of live variables during accurate garbage
**	collection.
**
** MR_DEBUG_AGC
** 	Turn on all debugging information for accurate garbage
** 	collection.  (Equivalent to all MR_DEBUG_AGC_* macros above).
**
** MR_TABLE_DEBUG
** 	Enables low-level debugging messages from the tabling system.
*/

#if MR_DEBUG_AGC
  #define MR_DEBUG_AGC_SCHEDULING
  #define MR_DEBUG_AGC_COLLECTION
  #define MR_DEBUG_AGC_FORWARDING
  #define MR_DEBUG_AGC_PRINT_VARS
#endif

/*
** MR_LABEL_STRUCTS_INCLUDE_NUMBER
**	Include a label number in each label layout structure.
*/

/*
** Profiling options:
**
** MEASURE_REGISTER_USAGE
** Enable this if you want to measure the number of times
** each register is used.  (Note that the measurement includes
** uses which occur inside debugging routines, so to get an accurate
** count you should not also enable low-level debugging.)
**
** PROFILE_CALLS
** Enables call count profiling.
**
** PROFILE_TIME
** Enables time profiling.
**
** PROFILE_MEMORY
** Enables profiling of memory usage.
*/

/*
** Experimental options:
**
** MR_TRACE_HISTOGRAM
** Enable this if you want to count the number of execution tracing events
** at various call depths.
**
** MR_CTOR_REP_STATS
** If you want to keep statistics on the number of times the generic unify,
** index and compare functions are invoked with type constructors of the
** various kinds of representations, then set this macro to a string giving
** the name of the file to which the statistics should be appended when the
** program exits.
**
** MR_TABLE_STATISTICS
** Enable this if you want to gather statistics about the operation of the
** tabling system. The results are reported via io__report_tabling_stats.
*/

/*---------------------------------------------------------------------------*/
/*
** Settings of configuration parameters which can be passed on
** the command line, but which are also implied by other parameters.
*/

/* MR_LOWLEVEL_DEBUG implies MR_DEBUG_GOTOS and MR_CHECK_FOR_OVERFLOW */
#ifdef MR_LOWLEVEL_DEBUG
  #define MR_DEBUG_GOTOS
  #define MR_CHECK_FOR_OVERFLOW
#endif

/*---------------------------------------------------------------------------*/
/*
** Configuration parameters whose values are determined by the settings
** of other configuration parameters.  These parameters should not be
** set on the command line.
**
** You must make sure that you don't test the value of any of these parameters
** before its conditional definition.
*/

/*
** Static code addresses are available unless using gcc non-local gotos,
** without assembler labels.
*/

#ifdef MR_STATIC_CODE_ADDRESSES
  #error "MR_STATIC_CODE_ADDRESSES should not be defined on the command line"
#endif
#if !defined(USE_GCC_NONLOCAL_GOTOS) || defined(USE_ASM_LABELS)
  #define MR_STATIC_CODE_ADDRESSES
#endif

/*
** MR_INSERT_LABELS     -- labels need to be inserted into the label table. 
**			   (this also means the initialization code needs
**			   to be run some time before the first use of the
**			   label table).
*/

#ifdef MR_INSERT_LABELS
  #error "MR_INSERT_LABELS should not be defined on the command line"
#endif
#if defined(MR_STACK_TRACE) || defined(NATIVE_GC) || defined(MR_DEBUG_GOTOS)
  #define MR_INSERT_LABELS
#endif

/*
** MR_INSERT_ENTRY_LABEL_NAMES -- the entry label table should contain
**				  the names of labels as well as their
**				  addresses and layouts (label names are
**				  quite big, so prefer not to include them
**				  unless they are necessary).
*/

#ifdef MR_INSERT_ENTRY_LABEL_NAMES
  #error "MR_INSERT_ENTRY_LABEL_NAMES should not be defined on the command line"
#endif
#if defined(PROFILE_CALLS) || defined(MR_LOWLEVEL_DEBUG) \
		|| defined(MR_DEBUG_AGC_SCHEDULING)
  #define MR_INSERT_ENTRY_LABEL_NAMES
#endif

/*
** MR_INSERT_INTERNAL_LABEL_NAMES -- the internal label table should contain
**				     the names of labels as well as their
**				     addresses and layouts (label names are
**				     quite big, so prefer not to include them
**				     unless they are necessary).
*/

#ifdef MR_INSERT_INTERNAL_LABEL_NAMES
  #error "MR_INSERT_INTERNAL_LABEL_NAMES should not be defined on the command line"
#endif
#if defined(MR_LOWLEVEL_DEBUG) || defined(MR_DEBUG_AGC_SCHEDULING)
  #define MR_INSERT_INTERNAL_LABEL_NAMES
#endif

/*
** MR_NEED_INITIALIZATION_AT_START -- the module specific initialization code
**				      must be run before any Mercury code
**				      is run.
**
** You need to run initialization code for grades without static
** code addresses, for profiling, and any time you need to insert
** labels into the label table.
*/

#ifdef MR_NEED_INITIALIZATION_AT_START
  #error "MR_NEED_INITIALIZATION_AT_START should not be defined on the command line"
#endif
#if !defined(MR_STATIC_CODE_ADDRESSES) || defined(PROFILE_CALLS) \
	|| defined(PROFILE_TIME) || defined(DEBUG_LABELS)
  #define MR_NEED_INITIALIZATION_AT_START
#endif

/*
** MR_MAY_NEED_INITIALIZATION -- the module specific initialization code
**				 may be needed, either at start or later.
**
** You need to run initialization code for grades without static
** code addresses, for profiling, and any time you need to insert
** labels into the label table.
*/

#ifdef MR_MAY_NEED_INITIALIZATION
  #error "MR_MAY_NEED_INITIALIZATION should not be defined on the command line"
#endif
#if defined(MR_NEED_INITIALIZATION_AT_START) || defined(MR_INSERT_LABELS)
  #define MR_MAY_NEED_INITIALIZATION
#endif

/*---------------------------------------------------------------------------*/

/*
** Memory protection and signal handling.
*/

#if defined(HAVE_SIGINFO) && defined(PC_ACCESS)
  #define MR_CAN_GET_PC_AT_SIGNAL
#endif

#if defined(HAVE_MPROTECT) && defined(HAVE_SIGINFO)
  #define MR_CHECK_OVERFLOW_VIA_MPROTECT
#endif

/*---------------------------------------------------------------------------*/

#endif /* MERCURY_CONF_PARAM_H */
