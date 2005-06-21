/*
** Copyright (C) 1998-2001, 2003-2005 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef MERCURY_STACK_TRACE_H
#define MERCURY_STACK_TRACE_H

#include "mercury_regs.h"
#include "mercury_stack_layout.h"
#include <stdio.h>

/*
** mercury_stack_trace.h -
**	Definitions for use by the stack tracing.
*/

/*---------------------------------------------------------------------------*/

/*
** MR_dump_stack
**
** Given the succip, det stack pointer and current frame, generate a
** stack dump showing the name of each active procedure on the
** stack. If include_trace_data data is set, also print the
** call event number, call sequence number and depth for every
** traced procedure.
** NOTE: MR_dump_stack will assume that the succip is for the
** topmost stack frame.  If you call MR_dump_stack from some
** pragma c_code, that may not be the case.
** Due to some optimizations (or lack thereof) the MR_dump_stack call
** may end up inside code that has a stack frame allocated, but
** that has a succip for the previous stack frame.
** Don't call MR_dump_stack from Mercury pragma c_code (calling
** from other C code in the runtime is probably ok, provided the
** succip corresponds to the topmost stack frame).
** (See library/require.m for a technique for calling MR_dump_stack
** from Mercury).
** If you need a more convenient way of calling from Mercury code,
** it would probably be best to make an impure predicate defined
** using `:- external'.
*/

extern	void	MR_dump_stack(MR_Code *success_pointer,
			MR_Word *det_stack_pointer,
			MR_Word *current_frame, MR_bool include_trace_data);

/*
** MR_dump_stack_from_layout
**
** This function does the same job and makes the same assumptions
** as MR_dump_stack, but instead of the succip, it takes the label
** layout of the current point in the current procedure as input.
** It also takes a parameter that tells it where to put the stack dump
** and flags that say whether to include execution trace data and/or
** line numbers. If limit is nonzero, dumps at most limit frames.
**
** If the entire wanted part of the stack was printed successfully,
** the return value is NULL; otherwise, it is a string indicating
** why the dump was cut short.
*/

typedef	void		(*MR_Print_Stack_Record)(FILE *fp,
				const MR_Proc_Layout *proc_layout,
				int count, int level,
				MR_Word *base_sp, MR_Word * base_curfr,
				const char *filename, int linenumber,
				const char *goal_path,
				MR_bool context_mismatch);

extern	const char	*MR_dump_stack_from_layout(FILE *fp,
				const MR_Label_Layout *label_layout,
				MR_Word *det_stack_pointer,
				MR_Word *current_frame,
				MR_bool include_trace_data,
				MR_bool include_contexts,
				int frame_limit, int line_limit,
				MR_Print_Stack_Record print_stack_record);

/*
** MR_dump_nondet_stack
**
** This function dumps the control slots of the nondet stack.
** If limit_addr is nonnull, dumps only frames above limit_addr.
** If limit is nonzero, dumps at most limit frames.
** The output format is not meant to be intelligible to non-implementors.
*/

extern	void	MR_dump_nondet_stack(FILE *fp, MR_Word *limit_addr,
			int frame_limit, int line_limit, MR_Word *maxfr);

/*
** MR_dump_nondet_stack_from_layout
**
** This function dumps the nondet stack.
** If limit_addr is nonnull, dumps only frames above limit_addr.
** If limit is nonzero, dumps at most limit frames.
** The output format is not meant to be intelligible to non-implementors.
*/

extern	void	MR_dump_nondet_stack_from_layout(FILE *fp,
			MR_Word *limit_addr, int frame_limit, int line_limit,
			MR_Word *maxfr, const MR_Label_Layout *label_layout,
			MR_Word *base_sp, MR_Word *base_curfr);

/*
** MR_traverse_nondet_stack_from_layout
**
** This function traverses the nondet stack, calling the specified
** function for each frame.
*/

typedef void MR_Traverse_Nondet_Frame_Func(void *user_data,
			const MR_Label_Layout *layout, MR_Word *base_sp,
			MR_Word *base_curfr);

extern	void	MR_traverse_nondet_stack_from_layout(
			MR_Word *maxfr, const MR_Label_Layout *label_layout,
			MR_Word *base_sp, MR_Word *base_curfr,
			MR_Traverse_Nondet_Frame_Func *traverse_frame_func,
			void *traverse_frame_func_data);

/*
** MR_find_nth_ancestor
**
** Return the layout structure of the return label of the call
** ancestor_level levels above the current call. Label_layout
** tells us how to decipher the stack of the current call, while
** *stack_trace_sp and *stack_trace_curfr tell us where it is.
** On return, *stack_trace_sp and *stack_trace_curfr will be
** set up to match the specified ancestor.
**
** If the required stack walk is not possible (e.g. because some
** stack frames have no layout information, or because the stack
** does not have the required depth), the return value will be NULL,
** and problem will point to an error message.
*/

extern	const MR_Label_Layout *MR_find_nth_ancestor(
			const MR_Label_Layout *label_layout,
			int ancestor_level, MR_Word **stack_trace_sp,
			MR_Word **stack_trace_curfr, const char **problem);

/*
** MR_stack_walk_step
**
** This function takes the entry_layout for the current stack
** frame (which is the topmost stack frame from the two stack
** pointers given), and moves down one stack frame, i.e. to the
** caller's frame, setting the stack pointers to their new levels.
**
** return_label_layout will be set to the stack_layout of the
** continuation label, or NULL if the bottom of the stack has
** been reached.
**
** The meanings of the possible return values from MR_stack_walk_step
** are as follows:
**
** MR_STEP_OK:			everything is fine.
** MR_STEP_ERROR_BEFORE:	entry_layout has no valid stack trace info.
** MR_STEP_ERROR_AFTER:		entry_layout has valid stack trace info,
**				but its caller does not.
**
** If a MR_stack_walk_step encounters a problem, it will set problem_ptr
** to point to a string representation of the error.
**
** Note that for nondetermistic code, this function will only
** traverse the success continuations (via MR_succfr),
** not the frames which represent failure continuations
** (which would be accessible via MR_redofr).
*/

typedef enum {
	MR_STEP_ERROR_BEFORE,
	MR_STEP_ERROR_AFTER,
	MR_STEP_OK
} MR_Stack_Walk_Step_Result;

extern  MR_Stack_Walk_Step_Result
		MR_stack_walk_step(const MR_Proc_Layout *entry_layout,
			const MR_Label_Layout **return_label_layout,
			MR_Word **stack_trace_sp_ptr,
			MR_Word **stack_trace_curfr_ptr,
			const char **problem_ptr);

/*
** MR_stack_trace_bottom should be set to the address of global_success,
** the label main/2 goes to on success. Stack dumps terminate when they
** reach a stack frame whose saved succip slot contains this address.
*/

extern	MR_Code	*MR_stack_trace_bottom;

/*
** MR_nondet_stack_trace_bottom should be set to the address of the buffer
** nondet stack frame created before calling main. Nondet stack dumps terminate
** when they reach a stack frame whose redoip contains this address. Note that
** the redoip and redofr slots of this frame may be hijacked.
*/

extern	MR_Word	*MR_nondet_stack_trace_bottom;

/*
** The different Mercury determinisms are internally represented by integers.
** This array gives the correspondance with the internal representation and
** the names that are usually used to denote determinisms.
*/

extern	const char *MR_detism_names[];

/*
** MR_find_context attempts to look up the file name and line number
** corresponding to a label identified by its layout structure. If successful,
** it fills in *fileptr and *lineptr accordingly, and returns MR_TRUE;
** otherwise, it returns MR_FALSE.
*/

extern	MR_bool	MR_find_context(const MR_Label_Layout *label,
			const char **fileptr, int *lineptr);

/*
** MR_print_call_trace_info prints the call event number, call sequence number
** and call depth of the call stored in the stack frame of the procedure
** identified by the given proc layout. It requires the procedure to have
** trace layout information, and the relevant one of base_sp and base_curfr
** to be non-NULL, since these numbers are stored in stack slots.
**
** MR_maybe_print_call_trace_info calls MR_print_call_trace_info if
** include_trace_data is MR_TRUE and the other conditions required by
** MR_print_call_trace_info are satisfied.
*/

extern	void	MR_print_call_trace_info(FILE *fp,
			const MR_Proc_Layout *entry,
			MR_Word *base_sp, MR_Word *base_curfr);

extern	void	MR_maybe_print_call_trace_info(FILE *fp,
			MR_bool include_trace_data,
			const MR_Proc_Layout *entry,
			MR_Word *base_sp, MR_Word *base_curfr);

/*
** MR_print_proc_id prints an identification of the given procedure,
** consisting of "pred" or "func", module name, pred or func name, arity,
** mode number and determinism. It does not output a newline, so that
** the caller can put something else after the procedure id on the same line.
*/

extern	void	MR_print_proc_id(FILE *fp, const MR_Proc_Layout *entry);

/*
** MR_print_pred_id prints everything that MR_print_proc_id does, except
** the mode number and determinism.
*/

extern	void	MR_print_pred_id(FILE *fp, const MR_Proc_Layout *entry);

/*
** MR_print_proc_spec prints a string that uniquely specifies the given
** procedure to the debugger.
*/

extern	void	MR_print_proc_spec(FILE *fp, const MR_Proc_Layout *entry);

/*
** MR_print_proc_separate prints a string that uniquely specifies the given
** procedure to the debugger, with each component of a name in a separate field
** to allow the output to be processed by tools (e.g. awk scripts).
*/

extern	void	MR_print_proc_separate(FILE *fp, const MR_Proc_Layout *entry);

/*
** MR_print_proc_id_trace_and_context prints an identification of the given
** procedure, together with call trace information (if available), a context
** within the procedure, and possibly a context identifying the caller.
** The position argument says where (if anywhere) the contexts should appear.
*/

typedef	enum {
	MR_CONTEXT_NOWHERE,
	MR_CONTEXT_BEFORE,
	MR_CONTEXT_AFTER,
	MR_CONTEXT_PREVLINE,
	MR_CONTEXT_NEXTLINE
} MR_Context_Position;

extern	void	MR_print_proc_id_trace_and_context(FILE *fp,
			MR_bool include_trace_data, MR_Context_Position pos,
			const MR_Proc_Layout *entry,
			MR_Word *base_sp, MR_Word *base_curfr,
			const char *path, const char *filename, int lineno,
			MR_bool print_parent, const char *parent_filename,
			int parent_lineno, int indent);

/*
** MR_dump_stack_record_print() prints one line of a stack dump.
*/

extern	void	MR_dump_stack_record_print(FILE *fp,
			const MR_Proc_Layout *entry_layout, int count,
			int start_level, MR_Word *base_sp, MR_Word *base_curfr,
			const char *filename, int linenumber,
			const char *goal_path, MR_bool context_mismatch);

/*
** Find the first call event on the stack whose event number or sequence number
** is less than or equal to the given event number or sequence number.  The
** level of the call in the stack is returned.  This can then be passed to
** MR_trace_retry as the ancestor_level.  If no such call is found then -1 is
** returned and problem is set to the reason why the call could not be
** found.
*/

typedef	enum {
	MR_FIND_FIRST_CALL_BEFORE_SEQ,
	MR_FIND_FIRST_CALL_BEFORE_EVENT
} MR_find_first_call_seq_or_event;

extern	int	MR_find_first_call_less_eq_seq_or_event(
			MR_find_first_call_seq_or_event seq_or_event,
			MR_Unsigned seq_no_or_event_no,
			const MR_Label_Layout *label_layout,
			MR_Word *det_stack_pointer, MR_Word *current_frame,
			const char **problem);

#endif /* MERCURY_STACK_TRACE_H */
