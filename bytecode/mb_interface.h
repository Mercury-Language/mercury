/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** Native-bytecode interface
**
*/

#ifndef MB_INTERFACE_H
#define	MB_INTERFACE_H

#include "mb_interface_stub.h"

#include "mb_util.h"

/* Returns pointer to the stub that calls corresponding function */
MB_Native_Addr		MB_native_get_return_det(void);
MB_Native_Addr		MB_native_get_return_temp_det(void);
MB_Native_Addr		MB_native_get_return_temp_nondet(void);
MB_Native_Addr		MB_native_get_return_nondet(void);

MB_Native_Addr		MB_native_get_do_redo(void);
MB_Native_Addr		MB_native_get_do_fail(void);
MB_Native_Addr		MB_native_get_unify_2(void);
MB_Native_Addr		MB_native_get_compare_3(void);

/* Find the native code entry point for a procedure */
MB_Native_Addr		MB_code_find_proc_native(MB_CString_Const module_name,
					MB_CString_Const pred_name,
					MB_Word mode_num, MB_Word arity,
					MB_Bool is_func);
MB_Native_Addr		MB_code_find_proc_native(MB_CString_Const module_name, 
					MB_CString_Const pred_name,
					MB_Word mode_num, MB_Word arity,
					MB_Bool is_func);

/* Get type info. Will abort program if it cannot find it */
MR_TypeCtorInfo		MB_type_find_ctor_info_guaranteed(
				MB_CString_Const module_name,
				MB_CString_Const type_name,
				MB_Word type_arity);

/**************************************************************/
/*
** det stack
** 
** Each normal det stack frame looks like the following:
**  sp-1:   [succip]
**  sp-2:   [temp 0]
**  sp-3:   [temp 1]
**            ...
**  sp-n:   [var 0]
**  sp-n-1: [var 1]
**            ...
**
** If model semidet, then temp 0 is the semidet success indicator
** This is why putting the temps straight after the fixed stack
** slots allows SEMIDET_SUCCESS to be in a fixed location
**
*/

/* saved succip */
#define MB_DETFRAME_SUCCIP			(1)

/* fixed size of deterministic stack frame */
#define MB_DETFRAME_SIZE			(1)

/* semidet success indicator (loaded into r0 at endof_proc) */
#define MB_DETFRAME_SEMIDET_SUCCESS		(2)

/*
** An interface det stack frame is pushed when bytecode wishes
** to jump to native code and later return to bytecode. Succip
** will have been set to a stub that reads the interface stack
** frame and directs control appropriately (MB_bytecode_return_det_stub)
**  sp-1:   [succip in bytecode to return to] 
**  sp-2:   [saved machine state initial frame]
*/

/* bytecode return address for stub */
#define MB_DETFRAME_INTERFACE_BCRETIP		(1)
#define MB_DETFRAME_INTERFACE_BCINITFR		(2)

/* Size of a deterministic interface frame */
#define MB_DETFRAME_INTERFACE_SIZE		(2)

/*
** Nondet stack
**
** An ordinary stack frame looks like so:
** curfr[ 0]	prevfr  (frame below this one)
** curfr[-1]	redoip  (ip    to use at redo) = do_fail
** curfr[-2]	redofr  (curfr to use at redo)
** curfr[-3]	succip  (ip    to use at success [caller return] )
** curfr[-4]	succfr  (frame to use at success [calling frame])
** curfr[-5]	bcretip (stack slot with next bytecode IP to return
**                       to following a call to native code)
** curfr[-6]	initfr  (saved initfr)
** then follows var[0] to var[n]
** then follows temp[0] to temp[n]
**
** A temp stack frame from nondet code looks like so:
** curfr[ 0]	prevfr   (frame below this one)
** curfr[-1]	redoip   (ip    to use at redo) = MB_bytecode_return_temp_nondet
** curfr[-2]	redofr   (curfr to use at redo)
** curfr[-3]	bcredoip (bytecode ip to return to when redoing this frame)
** 
** A temp stack frame from det code looks like so:
** curfr[ 0]	prevfr   (frame below this one)
** curfr[-1]	redoip   (ip    to use at redo) = MB_bytecode_return_temp_det
** curfr[-2]	redofr   (curfr to use at redo)
** curfr[-3]	detfr    (deterministic stack frame (used by accurate GC))
** curfr[-4]	bcredoip (bytecode ip to return to when redoing this frame)
** curfr[-5]	initfr   (saved initfr)
** 
** When entering nondet code:
**  If initfr is not already set, set it to maxfr
**  Push normal nondet stack frame
**
** When calling native code from nondet code:
**  bcretip    = next bytecode instruction after the call
**  MB_succip = MB_bytecode_return_nondet_stub which calls
**              MB_bytecode_return_nondet which reads bcretip and jumps
**              to the correct bytecode instruction
**
** When returning from nondet code:
**  Check if curfr->prevfr = initfr then this procedure was the one that
**   set initfr.
**  If it was, we should return to native code address succip.
**  If it wasn't, we should return to bytecode address succip.
*/

/* Present in all stack frames on the nondet stack */
#define MB_FRAME_PREVFR			(0)
#define MB_FRAME_REDOIP			(1)
#define MB_FRAME_REDOFR			(2)

/* Present in temporary nondet stack frames on the nondet stack */
#define MB_FRAME_TEMP_NONDET_BCREDOIP	(3)

/* Present in temporary det stack frames on the nondet stack */
#define MB_FRAME_DETFR			(3)
#define MB_FRAME_TEMP_DET_BCREDOIP	(4)
#define MB_FRAME_TEMP_DET_BCINITFR	(5)

/* Present in all normal nondet stack frames */
#define MB_FRAME_SUCCIP			(3)
#define MB_FRAME_SUCCFR			(4)

#define MB_FRAME_BCRETIP		(5)
#define MB_FRAME_BCINITFR		(6)

/*
** Note that the 3-4-5 size of the original native stack frames no longer
** applies; bytecode stack frames can have sizes of 4-6-7.
** The redoip of a bytecode stack frame will always be one of:
** MR_do_fail
** MB_bytecode_return_temp_det_stub
** MB_bytecode_return_temp_nondet_stub
**
*/
/* size of normal nondet stack frame */
#define MB_FRAME_NORMAL_SIZE		7

/* size of temp nondet stack frame created by model det/semidet code */
#define MB_FRAME_TEMP_DET_SIZE		6

/* size of temp nondet stack frame created by model nondet code */
#define MB_FRAME_TEMP_NONDET_SIZE	4

/* Invalid frame address */
#define MB_FRAME_INVALID	((MB_Word) (-1))

/*
** semidet success flags: stored in a temp slot until the procedure
** returns, when it is returned in a register
*/
#define MB_SEMIDET_SUCCESS	TRUE
#define MB_SEMIDET_FAILURE	FALSE

#define MB_SEMIDET_SUCCESS_REG	1

/**************************************************************/
/* register definitions */
#define MB_reg(n)	MR_virtual_reg(n)
#define MB_succip	MR_virtual_succip
#define MB_sp		MR_virtual_sp
#define MB_curfr	MR_virtual_curfr
#define MB_maxfr	MR_virtual_maxfr

/* Det stack: slot 1 is the top (used - slot 0 is unused) */
#define MB_stackvar(x)	((MB_sp)[-((x) + MB_DETFRAME_SIZE + 1)])

#define MB_stackitem(x)	((MB_sp)[-(x)])

#define MB_incr_sp(x)	(		\
			MB_sp += (x),	\
			(void)0		\
			)

#define MB_decr_sp(x)	MB_incr_sp(-(x))

/* Nondet stack - same as with det statck */
#define MB_frameitem(frame_ptr, x) ((frame_ptr)[-(x)])
#define MB_framevar(x)	(MB_frameitem(MB_curfr, MB_FRAME_NORMAL_SIZE))

#define MB_fr_prevfr(frame_ptr)			\
	((MB_Word *)	MB_frameitem(frame_ptr, MB_FRAME_PREVFR))
#define MB_fr_redoip(frame_ptr)			\
			MB_frameitem(frame_ptr, MB_FRAME_REDOIP)
#define MB_fr_redofr(frame_ptr)			\
			MB_frameitem(frame_ptr, MB_FRAME_REDOFR)

	
#define MB_fr_temp_nondet_bcredoip(frame_ptr)	\
			MB_frameitem(frame_ptr, MB_FRAME_TEMP_NONDET_BCREDOIP)

	
#define MB_fr_detfr(frame_ptr)			\
			MB_frameitem(frame_ptr, MB_FRAME_DETFR)
#define MB_fr_temp_det_bcredoip(frame_ptr)	\
			MB_frameitem(frame_ptr, MB_FRAME_TEMP_DET_BCREDOIP)
#define MB_fr_temp_det_bcinitfr(frame_ptr)	\
			MB_frameitem(frame_ptr, MB_FRAME_TEMP_DET_BCINITFR)

	
#define MB_fr_succip(frame_ptr)			\
			MB_frameitem(frame_ptr, MB_FRAME_SUCCIP)
#define MB_fr_succfr(frame_ptr)			\
	((MB_Word *)	MB_frameitem(frame_ptr, MB_FRAME_SUCCFR))
#define MB_fr_bcretip(frame_ptr) 		\
			MB_frameitem(frame_ptr, MB_FRAME_BCRETIP)
#define MB_fr_bcinitfr(frame_ptr) 		\
			MB_frameitem(frame_ptr, MB_FRAME_BCINITFR)

#define MB_frame_size(frame_ptr) \
			(frame_ptr - (MB_Word *) MB_fr_prevfr(frame_ptr))
/**************************************************************/
/* tags */
#include "mercury_tags.h"
#define MB_mktag(t)		MR_mktag(t)
#define MB_mkbody(p)		MR_mkbody(p)
#define MB_tag(w)		MR_tag(w)
#define MB_body(w,t)		MR_body(w,t)
#define MB_mkword(t,p)		MR_mkword(t,p)
#define MB_strip_tag(w)		MR_strip_tag(w)
#define MB_field(t, w, i)	MR_field(t,w,i)


#endif	/* MB_INTERFACE_H */
