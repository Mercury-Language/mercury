// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1997-2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2013-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

// mercury_conf_param.h:
// Defines various configuration parameters.
//
// Configuration parameters fall into three groups.
// They can be set automatically by autoconf.
// They can be passed on the command line (e.g. the mgnuc script
// sets some options based on the grade).
// Or their values can be implied by the settings of other parameters.
//
// The ones defined in mercury_conf.h are determined by autoconf.
// The remainder are documented and/or defined by this file,
// #included by mercury_conf.h.

// IMPORTANT NOTE:
// This file must not contain any #include statements,
// and may not define any global variables,
// for reasons explained in mercury_imp.h.
// This file should contain _only_ configuration macros.

#ifndef MERCURY_CONF_PARAM_H
#define MERCURY_CONF_PARAM_H

////////////////////////////////////////////////////////////////////////////

// Win32 API specific.

// MR_WIN32
// The Win32 API is available.
//
// MR_WIN32_GETSYSTEMINFO
// Is GetSystemInfo() available?

#ifdef _WIN32
  #define MR_WIN32
  #define MR_WIN32_GETSYSTEMINFO
  #define MR_WIN32_GETPROCESSTIMES
#endif

////////////////////////////////////////////////////////////////////////////

// Mac OS X specific.

// MR_USE_LIBDISPATCH is defined if we should use libdispatch to provide
// concurrency operations.  Currently, we only do this on OS X.  In the
// future, we may do so for other OSs (e.g. FreeBSD).

#if defined(__APPLE__) && defined(__MACH__)
  #define MR_MAC_OSX
  #if defined(MR_HAVE_DISPATCH_DISPATCH_H)
     #define MR_USE_LIBDISPATCH
  #endif
#endif

////////////////////////////////////////////////////////////////////////////

// MinGW specific.

// NOTE: __MINGW32__ is defined on both 32- and 64-bit MinGW.

#if defined(__MINGW32__)
  #define MR_MINGW
#endif

#if defined(__MINGW64__)
  #define MR_MINGW64
#endif

// On x86_64-w64-mingw32, we need to define the macro __USE_MINGW_ANSI_STDIO
// in order to ensure that we get versions of printf, sprintf and friends
// that work properly with long long format specifiers.

#if defined(__MINGW64__) && !defined(__USE_MINGW_ANSI_STDIO)
  #define __USE_MINGW_ANSI_STDIO 1
#endif

////////////////////////////////////////////////////////////////////////////

// Cygwin specific.

#if defined(__CYGWIN__)
  #define MR_CYGWIN
  #if defined(__LP64__)
     #define MR_CYGWIN64
  #else
     #define MR_CYGWIN32
  #endif
#endif

////////////////////////////////////////////////////////////////////////////

// Solaris/SunOS specific.

#if defined(__sun)
  #define MR_SOLARIS
#endif

////////////////////////////////////////////////////////////////////////////

// C compilers.

// MR_CLANG
// The C compiler is clang.
//
// MR_GNUC
// The C compiler is GCC.
// The value of this macro gives the major version number.
// We use this macro instead of __GNUC__ since clang also defines __GNUC__.
//
// MR_MSVC
// The C compiler is Visual C.
// The value of this macro gives the version number.

#if defined(__clang__)
  #define MR_CLANG __clang__
#elif defined(__GNUC__)
  #define MR_GNUC __GNUC__
#elif defined(_MSC_VER)
  #define MR_MSVC _MSC_VER
#endif

////////////////////////////////////////////////////////////////////////////
// Documentation for configuration parameters which can be set on the
// command line via `-D'.

// Code generation options:
//
// MR_HIGHLEVEL_CODE
// MR_HIGHLEVEL_DATA
// MR_USE_GCC_GLOBAL_REGISTERS
// MR_USE_GCC_NONLOCAL_GOTOS
// MR_USE_ASM_LABELS
// MR_CONSERVATIVE_GC (= boehm_gc)
// MR_BOEHM_GC
// MR_HGC
// MR_NATIVE_GC
// MR_NO_TYPE_LAYOUT
// MR_BOXED_FLOAT
// MR_BOXED_INT64S
// MR_USE_TRAIL
// MR_USE_MINIMAL_MODEL_STACK_COPY
// MR_USE_MINIMAL_MODEL_OWN_STACKS
// MR_MINIMAL_MODEL_DEBUG
// MR_PREGENERATED_DIST
// MR_USE_SINGLE_PREC_FLOAT
// MR_EXTEND_STACKS_WHEN_NEEDED
// MR_STACK_SEGMENTS
// MR_TRAIL_SEGMENTS
// MR_INLINE_ALLOC
// MR_TAGBITS
// MR_USE_REGIONS
// MR_RBMM_DEBUG
// MR_RBMM_PROFILING
// See the documentation for
//    --high-level-code
//    --high-level-data
//    --gcc-global-registers
//    --gcc-non-local-gotos
//    --gcc-asm-labels
//    --gc conservative
//    --gc accurate
//    --no-type-layout
//    --unboxed-float
//    --use-trail
//    --use-minimal-model
//    --minimal-model-debug
//    --pregenerated-dist
//    --single-prec-float
//    --extend-stacks-when-needed
//    --stack-segments
//    --trail-segments
//    --inline-alloc
//    --pic-reg
//    --tags
//    --num-tag-bits
//    --use-regions
//    --use-regions-debug
//    --use-regions-profiling
// (respectively) in the mmc help message or the Mercury User's Guide.
//
// MR_PIC
// The generated object code must be position independent.
// See runtime/mercury_goto.h.
//
// MR_THREAD_SAFE
// Enable support for parallelism.
//
// MR_THREADSCOPE
// Enable support for parallelism profiling, aka 'threadscope'. This is a
// grade component. This works only with the low level C parallel grades.
//
// MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
// Enable support for profiling the parallel runtime system. This collects
// counts and timings of certain runtime events. It is implied by
// MR_THREADSCOPE and must be enabled at runtime with the
// --profile-parallel-execution runtime option.
//
// MR_NO_BACKWARDS_COMPAT
// Disable backwards compatibility with C code using obsolete low-level
// constructs, e.g. referring to variables and macros without their MR_
// prefixes.
//
// MR_NO_CONF_BACKWARDS_COMPAT
// Disable backwards compatibility with C code using obsolete
// configuration macros without MR_ prefixes.
//
// MR_EXTRA_BACKWARDS_COMPAT
// Add extra backwards compatibility with C code using obsolete low-level
// constructs, e.g. referring to variables and macros without their MR_
// prefixes.
//
// MR_CHECK_DU_EQ
// When unifying or comparing two values of discriminated union types,
// check first whether the values (which are usually pointers) are equal.
//
// MR_DISABLE_CHECK_DU_EQ
// MR_CHECK_DU_EQ is turned on by default; this macro prevents this.

// Runtime checking options:
//
// MR_CHECK_FOR_OVERFLOW
// (Implied by MR_LOWLEVEL_DEBUG.)
// Check for overflow of the various memory
// areas, e.g. heap, det stack, nondet stack,
// before every access that might result in overflow.
// Causes the generated code to become bigger and less efficient.
// Slows down compilation.
//
// Normally MR_CHECK_FOR_OVERFLOW is not set, since
// we trap overflows using mprotect().
//
// MR_CHECK_TYPECLASS_REFS
// Check for improper use of typeclass_infos and base_typeclass_infos.
//
// MR_INCLUDE_SWITCH_DEFAULTS
// When performing switches over enum types defined in the runtime,
// include a default case even if the switch is complete, to guard against
// e.g. memory corruption of the switched-on data item taking it outside
// the legal range of that enum.

// Debugging options:
//
// MR_STACK_TRACE
// Require the inclusion of the layout information needed by error/1
// and the debugger to print stack traces. Set from the values of
// MR_EXEC_TRACE and MR_DEEP_PROFILING.
//
// MR_EXEC_TRACE
// Require that all Mercury procedures linked in should be compiled
// with at least interface tracing. This effect is achieved by including
// MR_EXEC_TRACE in the mangled grade (see mercury_grade.h).
//
// MR_DECL_DEBUG
// Require that all Mercury procedures linked in should be compiled
// with a trace level that supports declarative debugging. This effect
// is achieved by including MR_DECL_DEBUG in the mangled grade
// (see mercury_grade.h).
//
// Setting MR_DECL_DEBUG requires MR_EXEC_TRACE to be set also.
//
// MR_SS_DEBUG
// Enable source-to-source debugging on all Mercury procedures.
// This effect is achieved by including MR_SS_DEBUG in the mangled
// grade (see mercury_grade.h).
//
// MR_TRACE_COUNT_DEBUG
// Enables runtime checking of the invariants involving the implementation
// of the --trace-count runtime option.
//
// MR_EXEC_TRACE_INFO_IN_CONTEXT
// (Implied by MR_USE_MINIMAL_MODEL_OWN_STACKS.)
// Allows the debugger to distinguish between different contexts.
// Currently used only by own stack minimal model tabling.
//
// MR_LOWLEVEL_DEBUG
// Enables various low-level debugging stuff that was in the distant past
// used to debug the low-level code generation.
// Causes the generated code to become VERY big and VERY inefficient.
// Slows down compilation a LOT.
//
// MR_DEEP_PROFILING_LOWLEVEL_DEBUG
// Enables the debugging of the code that builds the deep profiling graph.
//
// MR_DEEP_PROFILING_DEBUG
// Enables the debugging of the code that writes out deep profiling data
// files by also printing out the same information in a human readable form.
//
// MR_DEEP_PROFILING_DETAIL_DEBUG
// Enables the debugging of the code that writes out the atomic components
// (integers, strings, pointers, etc) of the deep profiling data structures.
//
// MR_DEEP_PROFILING_LOG
// Enables the code that writes out a log of the actions of the deep
// profiling code.
//
// MR_DEBUG_DD_BACK_END
// Enables low-level debugging messages on the operation of the
// declarative debugging back end.
//
// MR_DEBUG_GOTOS
// (Implied by MR_LOWLEVEL_DEBUG.)
// Enables low-level debugging of gotos.
// Causes the generated code to become bigger and less efficient.
// Slows down compilation.
//
// MR_DEBUG_HEAP_ALLOC
// (Implied by MR_LOWLEVEL_DEBUG.)
// Uses functions to do memory allocation. These functions can generate
// diagnostic output, enforce invariants, and one can put breakpoints
// on them.
//
// MR_DEBUG_AGC_SCHEDULING
// Display debugging information while scheduling accurate garbage
// collection (for the low-level back-end).
//
// MR_DEBUG_AGC_FORWARDING
// Display debugging information when leaving or finding forwarding
// pointers during accurate garbage collection.
//
// MR_DEBUG_AGC_SAVED_HPS
// Display debugging information about saved heap pointers
// during accurate garbage collection.
//
// MR_DEBUG_AGC_PRINT_VARS
// Display the values of live variables during accurate garbage collection.
//
// MR_DEBUG_AGC_SMALL_HEAP
// Use a small heap (52k) to trigger garbage collection more often.
// This is the same as setting MERCURY_OPTIONS="--heap-size 52".
//
// MR_DEBUG_AGC_ALL
// Turn on all debugging information for accurate garbage collection.
// (Equivalent to all MR_DEBUG_AGC_* macros above).
//
// Note that general debugging information about accurate garbage collection
// is printed if -dG is included in MERCURY_OPTIONS. This works even if
// none of the MR_DEBUG_AGC_* macros are enabled.
//
// MR_TABLE_DEBUG
// Enables low-level debugging messages from the parts of the tabling system
// that rely on hand-written code in the runtime system's libraries.
// Low level debugging messages from code generated by the Mercury compiler,
// even if using macros defined in the runtime directory, are controlled by
// the --table-debug option of mmc.
//
// MR_DEBUG_RETRY
// Enables low-level debugging messages from retry operations in the debugger.
//
// MR_DEBUG_LABEL_NAMES
// Registers labels and their names, enabling label addresses to be converted
// back to a form in which they are usable by a developer.
// Implied by MR_DEEP_PROFILING_LOWLEVEL_DEBUG, MR_TABLE_DEBUG, and
// MR_DEBUG_RETRY.
//
// MR_DEBUG_LABEL_GOAL_PATHS
// When printing label names, print the goal path of the label as well,
// if this information is available.
// Meaningful only if MR_DEBUG_LABEL_NAMES is defined.
//
// MR_LOWLEVEL_ADDR_DEBUG
// Enables the printing of raw addresses in debugging output even for
// things (such as stack slots and labels) that can be identified by more
// human-friendly handles (such as stack offsets and label names).
//
// MR_DEBUG_JMPBUFS
// Enables low-level debugging messages from MR_call_engine and the
// code handling exceptions.
//
// MR_DEBUG_LVAL_REP
// Enables low-level debugging messages from routines concerned with
// the representation of lvals in the RTTI system.
//
// MR_DEBUG_MDPROF_SIGNAL
// Enables low-level debugging messages from the signal handling
// functions in the deep profiler.
//
// MR_STACK_EXTEND_DEBUG
// Enables low-level debugging messages when extending the stacks.
//
// MR_DEBUG_STACK_SEGMENTS
// Enables low-level debugging messages when updating the list of
// stack segments.
//
// MR_DEBUG_STACK_SEGMENTS_SET_SIZE
// If set, we reduce the effective size of each segment
// to this number of words.
//
// MR_DEBUG_TRAIL_SEGMENTS
// Enables low-level debugging messages when updating the list of
// trail segments.
//
// MR_TRACE_CHECK_INTEGRITY
// Enables the -i and --integrity options on mdb's forward movement
// commands, which cause the debugger to check the integrity of the
// representations of all the terms reachable from the stack.
//
// MR_STM_DEBUG
// Enables low-level debugging messages from the code that implements
// transactions used by software transactional memory.
//
// MR_DEBUG_DWORD_ALIGNMENT
// Enables runtime tests for misaligned double-word pointers.

#ifdef MR_DEEP_PROFILING_DETAIL_DEBUG
  #define MR_DEEP_PROFILING_DEBUG
#endif

// Runtime code alternatives. --- Sometimes there is more than one way to do
// something, a more efficient/optimal way, and a more portable way. Use these
// options to execute the more portable code even on systems where the more
// optimal code is safe. This is useful to make test coverage a bit more even.
//
// options MR_AVOID_HANDWRITTEN_ASSEMBLER and MR_AVOID_COMPILER_INTRINSICS are
// not mutually exclusive. The first is relevant when we prefer handwritten
// assembler (in some cases this is more efficient). Where as the second is
// relevant when we prefer compiler intrinsics (usually because there's no
// advantage to handwritten assembler other than when the intrinsics are not
// supported by the compiler).
//
// MR_AVOID_HANDWRITTEN_ASSEMBLER
// Avoid using handwritten assembler in the runtime. This will usually
// default to compiler intrinsics or C code, see runtime/mercury_atomic_ops.h
//
// MR_AVOID_COMPILER_INTRINSICS
// Avoid using compiler intrinsics. This will usually fall back to hand
// written assembler, therefore it is only supported on architectures where
// hand written assembler code exists.
//
// MR_DO_NOT_USE_CPU_RELAX
// Do not compile in CPU relax instructions in spin loops; see the 'pause'
// instruction on x86. This is used when defining MR_PAUSE in
// runtime/mercury_atomic_ops.h, MR_PAUSE is used in spin-wait loops and the
// implementation of spin locks which is also in mercury_atomic_ops.h.

// Execution tracing and deep profiling both need stack traces, e.g.
// simulate exits from calls between an exception being thrown and being
// caught. Stack tracing is therefore automatically enabled in debugging and
// deep profiling grades.
//
// In theory, we could allow stack traces to be enabled even in non-debug,
// non-deep-profiling grades. However, if you try to do a stack trace, you
// would find it doesn't work very well unless all modules are compiled
// with stack tracing. We could define a grade for situations in which
// MR_STACK_TRACE is defined but MR_EXEC_TRACE and MR_DEEP_PROFILING aren't,
// but such a grade wouldn't be very useful. We therefore ensure that
// MR_STACK_TRACE is set iff at least one of MR_EXEC_TRACE and
// MR_DEEP_PROFILING is set.

#ifdef MR_STACK_TRACE
  #error "MR_STACK_TRACE set independently"
#endif
#if defined(MR_EXEC_TRACE) || defined(MR_DEEP_PROFILING)
  #define MR_STACK_TRACE
#endif

#ifdef MR_HIGHLEVEL_CODE
  #ifdef MR_LOWLEVEL_DEBUG
    #error "MR_HIGHLEVEL_CODE and MR_LOWLEVEL_DEBUG are not supported together"
  #endif
  #ifdef MR_DEBUG_DD_BACK_END
    #error "MR_HIGHLEVEL_CODE and MR_DEBUG_DD_BACK_END are not supported together"
  #endif
  #ifdef MR_DEBUG_GOTOS
    #error "MR_HIGHLEVEL_CODE and MR_DEBUG_GOTOS are not supported together"
  #endif
  #ifdef MR_DEBUG_LABEL_NAMES
    #error "MR_HIGHLEVEL_CODE and MR_DEBUG_LABEL_NAMES are not supported together"
  #endif
  #ifdef MR_LOWLEVEL_ADDR_DEBUG
    #error "MR_HIGHLEVEL_CODE and MR_LOWLEVEL_ADDR_DEBUG are not supported together"
  #endif
  #ifdef MR_DEBUG_LVAL_REP
    #error "MR_HIGHLEVEL_CODE and MR_DEBUG_LVAL_REP are not supported together"
  #endif
#endif

#if MR_DEBUG_AGC_ALL
  #define MR_DEBUG_AGC_SCHEDULING
  #define MR_DEBUG_AGC_COLLECTION
  #define MR_DEBUG_AGC_FORWARDING
  #define MR_DEBUG_AGC_SAVED_HPS
  #define MR_DEBUG_AGC_PRINT_VARS
  #define MR_DEBUG_AGC_SMALL_HEAP
#endif

// MR_LABEL_STRUCTS_INCLUDE_NUMBER
// Include a label number in each label layout structure.

// Profiling options:
//
// MR_MEASURE_REGISTER_USAGE
// Enable this if you want to measure the number of times each register
// is used. (Note that the measurement includes uses which occur inside
// debugging routines, so to get an accurate count you should not also
// enable low-level debugging.)
//
// MR_DO_CALL_STATS
// Enable this is you want to collect statistics about the number of arguments
// hidden inside closures. The stats will be appended to the file named by the
// HO_CALL_STATS environment variable.
//
// MR_MPROF_PROFILE_CALLS
// Enables call count profiling for mprof.
//
// MR_MPROF_PROFILE_TIME
// Enables time profiling for mprof.
//
// MR_MPROF_PROFILE_MEMORY
// Enables profiling of memory usage for mprof.
//
// MR_DEEP_PROFILING
// Enables deep profiling.
//
// MR_RECORD_TERM_SIZES
// Augments heap cells with an extra word recording the size of the term.
// For implementors only.
//
// MR_RECORD_TERM_SIZES_AS_CELLS
// Record the size of the term as the number of heap cells it occupies.
// If MR_RECORD_TERM_SIZES_AS_CELLS is not defined, the default is
// to record term sizes as the number of heap words. Meaningful only if
// MR_RECORD_TERM_SIZES is defined. For implementors only.
//
// MR_DEEP_PROFILING_EXPLICIT_CALL_COUNTS
// If defined, we explicitly record the number of calls in each
// call_site_dynamic, instead of computing it from the other port counts.
// Useful only for measuring the overhead of the recording. Defining this macro
// makes the generated Deep.data files incompatible with the assumptions
// of read_profile.m and measurements.m in the deep_profiler directory.
// For implementors only.
//
// MR_DEEP_PROFILING_PERF_TEST
// Allows the selective performance testing of various aspects of deep
// profiling. For implementors only.
//
// MR_USE_ACTIVATION_COUNTS
// Selects the activation counter approach to deep profiling over the
// save/restore approach (the two approaches are documented in the deep
// profiling paper). For implementors only.

// Experimental options:
//
// MR_TRACE_HISTOGRAM
// Enable this if you want to count the number of execution tracing events
// at various call depths.
//
// MR_TYPE_CTOR_STATS
// If you want to keep statistics on the number of times the generic unify,
// index and compare functions are invoked with various kinds of type
// constructors, then set this macro to a string giving the name of the file
// to which the statistics should be appended when the program exits.
// Note that calls to the generic compare_representation are counted as
// calls to compare.
//
// MR_TABLE_STATISTICS
// Enable this if you want to gather statistics about the operation of the
// tabling system. The results are reported via io__report_tabling_stats.
//
// MR_STACK_FRAME_STATS
// If you want to gather statistics about the number and size of stack frames,
// then set this macro to a string giving the name of the file to which
// the statistics should be appended when the program exits.
//
// MR_COMPARE_BY_RTTI
// Enable this if you want to perform unifications and comparisons on types
// with standard equality by interpreting the RTTI data structures instead of
// invoking the type-specific unify and compare procedures. The last time we
// measured it, this lead to about a 6% slowdown. Since the code interpreting
// the data structures calls C functions, defining this macro also leads to
// problems if user-defined unify procedures abort: the exception could be
// transmitted to the parent Mercury code only by catching and retransmitting
// it, which, for efficiency reasons, the code doesn't do.
//
// MR_UNCONDITIONAL_STRUCTURE_REUSE
// Enable this to bypass the check that a cell was allocated by Boehm GC
// before reusing it.
//
// MR_RBMM_USE_MACROS
// By default, all RBMM operations are implemented by functions, but if
// this macro is enabled, we implement this with macros. Macros avoid the
// overhead of function calls, but in large programs, having lots of copies
// of the macro body will have a negative impact on locality, because we can
// expect many copies to not be in the instruction cache, while function
// bodies called from all over the place definitely should stay in the
// instruction cache.

#if defined(MR_THREAD_SAFE) && defined(MR_TRACE_HISTOGRAM)
  #error "MR_THREAD_SAFE and MR_TRACE_HISTOGRAM are not supported together"
#endif

#if defined(MR_THREAD_SAFE) && defined(MR_TYPE_CTOR_STATS)
  #error "MR_THREAD_SAFE and MR_TYPE_CTOR_STATS are not supported together"
#endif

#if defined(MR_THREAD_SAFE) && defined(MR_TABLE_STATISTICS)
  #error "MR_THREAD_SAFE and MR_TABLE_STATISTICS are not supported together"
#endif

#if defined(MR_THREAD_SAFE) && defined(MR_STACK_FRAME_STATS)
  #error "MR_THREAD_SAFE and MR_STACK_FRAME_STATS are not supported together"
#endif

// If the execution engine uses multiple contexts, we want separate event
// counters, call counters and depth counters in each context. Currently,
// we use multiple contexts only in parallel grades, for which the debugger
// doesn't (yet) work, and in own stack minimal model grades.

#ifdef MR_USE_MINIMAL_MODEL_OWN_STACKS
  #define MR_EXEC_TRACE_INFO_IN_CONTEXT
#endif

#ifdef MR_MINIMAL_MODEL_DEBUG
  #define MR_TABLE_STATISTICS
#endif

////////////////////////////////////////////////////////////////////////////
// Settings of configuration parameters which can be passed on
// the command line, but which are also implied by other parameters.

// MR_PREGENERATED_DIST overrides configured values to take values compatible
// with pre-generated C files in the source distribution.
// Any changes here may require changes in compiler/handle_options.m.

#ifdef MR_PREGENERATED_DIST
  #undef  MR_LOW_TAG_BITS
  #define MR_LOW_TAG_BITS  2
  #define MR_BOXED_FLOAT   1
  #define MR_BOXED_INT64S  1
#endif

// MR_PIC means that we are generating position independent code,
// i.e. that the file was compiled with the gcc option `-fpic' or equivalent.

#if (defined(__pic__) || defined(__PIC__))
  #define MR_PIC 1
#endif

// NOTE: MR_PIC_REG is currently unused and does not have any affect.
// The following describes what it was previously used for:
//
// Should we keep the GOT register (e.g. ebx on i386) free for PIC code?
// We need to do this if we are generating position independent code
// (MR_PIC), or if we are linking with position independent Mercury code
// (in which case -DMR_PIC_REG will be passed on the command line).
//
// The GOT register is only needed for Unix-style shared libraries.
// Windows DLLs do not use the GOT register. So don't do this if
// __CYGWIN__ or _WIN32 is defined, even if -DMR_PIC_REG was passed
// on the command line.
//
// #if defined(MR_PIC)
//   #define MR_PIC_REG 1
// #endif
// #if defined(__CYGWIN__) || defined(_WIN32)
//   #undef MR_PIC_REG
// #endif

// MR_LOWLEVEL_DEBUG implies MR_DEBUG_GOTOS and MR_CHECK_FOR_OVERFLOW
#ifdef MR_LOWLEVEL_DEBUG
  #define MR_DEBUG_GOTOS
  #define MR_CHECK_FOR_OVERFLOW
#endif

// MR_DEEP_PROFILING_PORT_COUNTS
// Enables deep profiling of port counts.
//
// MR_DEEP_PROFILING_TIMING
// Enables deep profiling of time (obtained via clock interrupt signals).
// This is not currently supported on Windows; we always disable it on that
// platform.
//
// MR_DEEP_PROFILING_CALL_SEQ
// Enables deep profiling of time (obtained by counting call sequence numbers).
//
// MR_DEEP_PROFILING_MEMORY
// Enables deep profiling of memory usage.
//
// MR_DEEP_PROFILING_COVERAGE
// Enables deep profiling code coverage support. (Required for
// auto-parallelisation).
//
// MR_DEEP_PROFILING_COVERAGE_STATIC.
// Enables the outmoded static coverage profiling code. This disables the new
// dynamic coverage profiling code.
//
// MR_DEEP_PROFILING_COVERAGE_DYNAMIC
// Enables the collection of separate coverage information for each deep
// context in which a procedure is used.
//
// This does not need to be specified explicitly, as it is the default,
// which can be overridden by MR_DEEP_PROFILING_COVERAGE_STATIC.

#ifdef MR_DEEP_PROFILING
  // This is the default set of measurements in deep profiling grades.
  #define MR_DEEP_PROFILING_PORT_COUNTS
  #ifndef MR_DEEP_PROFILING_PERF_TEST
    #define MR_DEEP_PROFILING_TIMING
    #define MR_DEEP_PROFILING_CALL_SEQ
    #define MR_DEEP_PROFILING_MEMORY
    #define MR_DEEP_PROFILING_COVERAGE
    #ifndef MR_DEEP_PROFILING_COVERAGE_STATIC
      #define MR_DEEP_PROFILING_COVERAGE_DYNAMIC
    #else
      #undef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
    #endif
  #else
    #ifdef MR_DEEP_PROFILING_COVERAGE
      #ifndef MR_DEEP_PROFILING_COVERAGE_STATIC
        #define MR_DEEP_PROFILING_COVERAGE_DYNAMIC
      #else
        #undef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
      #endif
    #else
      #undef MR_DEEP_PROFILING_COVERAGE_DYNAMIC
      #undef MR_DEEP_PROFILING_COVERAGE_STATIC
    #endif
  #endif
#else
  #undef  MR_DEEP_PROFILING_PORT_COUNTS
  #undef  MR_DEEP_PROFILING_TIMING
  #undef  MR_DEEP_PROFILING_CALL_SEQ
  #undef  MR_DEEP_PROFILING_MEMORY
  #undef  MR_DEEP_PROFILING_COVERAGE
  #undef  MR_DEEP_PROFILING_COVERAGE_DYNAMIC
  #undef  MR_DEEP_PROFILING_COVERAGE_STATIC
#endif

#if !defined(MR_DISABLE_CHECK_DU_EQ)
  #define MR_CHECK_DU_EQ
#endif

// Time profiling is not currently supported on Windows.

#if defined(MR_WIN32)
  #undef MR_DEEP_PROFILING_TIMING
#endif

////////////////////////////////////////////////////////////////////////////
// Configuration parameters whose values are determined by the settings
// of other configuration parameters. These parameters should not be
// set on the command line.
//
// You must make sure that you don't test the value of any of these parameters
// before its conditional definition.

// The Boehm collector is our default garbage collector,
// and it is a conservative collector.
//
// If MR_CONSERVATIVE_GC is defined without specifying which collector to use,
// then default to using the Boehm collector.

#if defined(MR_HGC) || defined(MR_BOEHM_GC)
  #ifndef MR_CONSERVATIVE_GC
    #define MR_CONSERVATIVE_GC
  #endif
#elif defined(MR_CONSERVATIVE_GC)
  #define MR_BOEHM_GC
#endif

// MR_MIGHT_RECLAIM_HP_ON_FAILURE should be set if the grade allows
// the heap to be reset on failure.
//
// XXX In the long run it would be nice to allow heap reclamation on
// failure with accurate GC, but this requires liveness-accuracy,
// which is not yet implemented; see the comments in the TODO list in
// compiler/ml_elim_nested.m.

#if !defined(MR_CONSERVATIVE_GC) && !defined(MR_NATIVE_GC)
  #define MR_MIGHT_RECLAIM_HP_ON_FAILURE
#endif

// MR_RECLAIM_HP_ON_FAILURE should be set if C code in the current translation
// unit should reclaim heap on failure of a subgoal. Note that this only
// affects heap reclamation in C code, not in Mercury code; heap reclamation
// in Mercury code is determined by mmc options (e.g.
// `--reclaim-hp-on-semidet-failure') which affect the generated C code.
//
// This is defined separately from MR_MIGHT_RECLAIM_HP_ON_FAILURE because
// in theory different translation units might be compiled with different
// settings; it might be important to reclaim heap in some translation units
// but not others. But currently we always reclaim heap on failure if we can.

#ifdef MR_MIGHT_RECLAIM_HP_ON_FAILURE
  #define MR_RECLAIM_HP_ON_FAILURE
#endif

// Some sanity checking
#ifdef MR_RECLAIM_HP_ON_FAILURE
  #ifndef MR_MIGHT_RECLAIM_HP_ON_FAILURE
    #error "MR_RECLAIM_HP_ON_FAILURE && ! MR_MIGHT_RECLAIM_HP_ON_FAILURE"
  #endif
  #ifdef MR_CONSERVATIVE_GC
     // Heap reclamation on failure is not supported with conservative GC,
     // because the conservative collectors don't provide any way to do it.

     #error "MR_RECLAIM_HP_ON_FAILURE and MR_CONSERVATIVE_GC both defined"
  #endif
  #ifdef MR_NATIVE_GC
     // Heap reclamation on failure is not supported with accurate GC,
     // because it requires liveness accuracy, which is not yet implemented.
     // See the comments in the TODO list in compiler/ml_elim_nested.m.

     #error "MR_RECLAIM_HP_ON_FAILURE and MR_NATIVE_GC both defined"
  #endif
#endif

// Static code addresses are available unless using gcc non-local gotos,
// without assembler labels.

#ifdef MR_STATIC_CODE_ADDRESSES
  #error "MR_STATIC_CODE_ADDRESSES should not be defined on the command line"
#endif
#if !defined(MR_USE_GCC_NONLOCAL_GOTOS) || defined(MR_USE_ASM_LABELS)
  #define MR_STATIC_CODE_ADDRESSES
#endif

// Whether we are in a grade which supports the low-level parallel
// conjunction execution mechanism.

#ifdef MR_LL_PARALLEL_CONJ
  #error "MR_LL_PARALLEL_CONJ may not be defined on the command line"
#endif
#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)
  #define MR_LL_PARALLEL_CONJ
#endif

#ifdef MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
  #error "MR_PROFILE_PARALLEL_EXECUTION_SUPPORT may only be implied by MR_THREADSCOPE"
#endif
#ifdef MR_THREADSCOPE
  #define MR_PROFILE_PARALLEL_EXECUTION_SUPPORT
#endif

// Memory attribution profiling requires the procedure names from
// memory profiling and hooks in Boehm GC.

#ifdef MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
  #error "MR_MPROF_PROFILE_MEMORY_ATTRIBUTION should not be defined "   \
    "on the command line"
#endif
#if defined(MR_BOEHM_GC) && defined(MR_MPROF_PROFILE_MEMORY)
  #define MR_MPROF_PROFILE_MEMORY_ATTRIBUTION
#endif

// XXX document MR_BYTECODE_CALLABLE

// MR_DEBUG_LABEL_NAMES
// We need to be able to convert code addresses into the names of the labels
// they correspond to.

// These debugging facilities require label names
#if defined(MR_DEEP_PROFILING_LOWLEVEL_DEBUG) || defined(MR_TABLE_DEBUG) \
    || defined(MR_DEBUG_RETRY)
  #define MR_DEBUG_LABEL_NAMES
#endif

// MR_INSERT_LABELS
// Labels need to be inserted into the label table. (This also means the
// initialization code needs to be run some time before the first use of the
// label table).
//
// Note that for the MLDS back-end, the calls to MR_init_entry()
// that insert the function addresses in the label table are only
// output if the right compiler options are enabled. So if you change
// the condition of this `#ifdef', and you want your changes to apply
// to the MLDS back-end too, you may also need to change the
// `need_to_init_entries' predicate in compiler/mlds_to_c.m.

#ifdef MR_INSERT_LABELS
  #error "MR_INSERT_LABELS should not be defined on the command line"
#endif
#if defined(MR_STACK_TRACE) || defined(MR_NATIVE_GC)                    \
    || defined(MR_DEBUG_GOTOS) || defined(MR_BYTECODE_CALLABLE)         \
    || defined(MR_DEBUG_LABEL_NAMES)
  #define MR_INSERT_LABELS
#endif

// MR_INSERT_ENTRY_LABEL_NAMES
// The entry label table should contain the names of labels as well as their
// addresses and layouts (label names are quite big, so prefer not to include
// them unless they are necessary).

#ifdef MR_INSERT_ENTRY_LABEL_NAMES
  #error "MR_INSERT_ENTRY_LABEL_NAMES should not be defined on the command line"
#endif
#if defined(MR_MPROF_PROFILE_CALLS) || defined(MR_DEBUG_GOTOS)          \
    || defined(MR_DEBUG_AGC_SCHEDULING) || defined(MR_DEBUG_LABEL_NAMES)
  #define MR_INSERT_ENTRY_LABEL_NAMES
#endif

// MR_INSERT_INTERNAL_LABEL_NAMES
// The internal label table should contain the names of labels as well as
// their addresses and layouts (label names are quite big, so prefer not
// to include them unless they are necessary).

#ifdef MR_INSERT_INTERNAL_LABEL_NAMES
  #error "MR_INSERT_INTERNAL_LABEL_NAMES should not be defined on the command line"
#endif
#if defined(MR_DEBUG_GOTOS) || defined(MR_DEBUG_AGC_SCHEDULING)         \
    || defined(MR_DEBUG_LABEL_NAMES)
  #define MR_INSERT_INTERNAL_LABEL_NAMES
#endif

// MR_NEED_ENTRY_LABEL_ARRAY
// we need an array of the procedure entry code addresses and possibly
// their label names, sorted by the code address before use.
//
// This is required by garbage collection and for some kinds of low level
// debugging.
//
// MR_NEED_ENTRY_LABEL_INFO
// we need to register procedure entry code addresses.
//
// This is required in order to construct the sorted array of procedure entry
// code addresses, to let the mprof profiling system turn program counter
// samples back into procedure names, and to let accurate gc find out the
// layout of stack frames.

#if defined(MR_NATIVE_GC) || defined(MR_DEBUG_GOTOS)                    \
    || defined(MR_INSERT_ENTRY_LABEL_NAMES)
  #define MR_NEED_ENTRY_LABEL_ARRAY
#endif

#if defined(MR_NEED_ENTRY_LABEL_ARRAY) || defined(MR_MPROF_PROFILE_CALLS)
  #define MR_NEED_ENTRY_LABEL_INFO
#endif

// MR_NEED_INITIALIZATION_AT_START
// The module specific initialization code must be run before any Mercury code
// is run.
//
// You need to run initialization code for grades without static code
// addresses, for profiling, and any time you need to insert labels into
// the label table.

#ifdef MR_NEED_INITIALIZATION_AT_START
  #error "MR_NEED_INITIALIZATION_AT_START should not be defined on the command line"
#endif
#if !defined(MR_STATIC_CODE_ADDRESSES) || defined(MR_MPROF_PROFILE_CALLS) \
    || defined(MR_MPROF_PROFILE_TIME) || defined(MR_DEBUG_LABEL_NAMES)
  #define MR_NEED_INITIALIZATION_AT_START
#endif

// MR_MAY_NEED_INITIALIZATION
// The module specific initialization code may be needed, either at start
// or later.
//
// You need to run initialization code for grades without static code
// addresses, for profiling, and any time you need to insert labels
// into the label table.

#ifdef MR_MAY_NEED_INITIALIZATION
  #error "MR_MAY_NEED_INITIALIZATION should not be defined on the command line"
#endif
#if defined(MR_NEED_INITIALIZATION_AT_START) || defined(MR_INSERT_LABELS)
  #define MR_MAY_NEED_INITIALIZATION
#endif

////////////////////////////////////////////////////////////////////////////

// Memory protection and signal handling.

#if defined(MR_HAVE_SIGINFO) && defined(MR_PC_ACCESS)
  #define MR_CAN_GET_PC_AT_SIGNAL
#endif

// MR_CHECK_OVERFLOW_VIA_MPROTECT
// Can check for overflow of various memory zones using mprotect() like
// functionality.

#if (defined(MR_HAVE_MPROTECT) && defined(MR_HAVE_SIGINFO))
  #define MR_CHECK_OVERFLOW_VIA_MPROTECT
#endif

// MR_PROTECTPAGE
// MR_protect_pages() can be defined to provide the same functionality
// as the system call mprotect().

#if defined(MR_HAVE_MPROTECT)
  #define MR_PROTECTPAGE
#endif

// MR_MSVC_STRUCTURED_EXCEPTIONS
// Use Microsoft Visual C structured exceptions for signal handling.

#if defined(_MSC_VER)
  #define MR_MSVC_STRUCTURED_EXCEPTIONS
#endif

////////////////////////////////////////////////////////////////////////////

// MR_HAVE_THREAD_PINNING is defined if we can pin threads, either with
// sched_setaffinity or hwloc.

#if defined(MR_HAVE_HWLOC) || \
    (defined(MR_HAVE_SCHED_GETAFFINITY) && \
      defined(MR_HAVE_SCHED_SETAFFINITY) && \
      defined(MR_HAVE_SCHED_CPUSET_MACROS))
  #define MR_HAVE_THREAD_PINNING
#endif

////////////////////////////////////////////////////////////////////////////

#endif // MERCURY_CONF_PARAM_H
