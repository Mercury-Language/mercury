
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

#include "mb_basetypes.h"
#include "mb_module.h"
#include "mb_util.h"

typedef struct {
	/* if cached_ip is NULL, this procedure hasn't been looked up yet */
	MB_Bytecode_Addr cached_ip;

	const char	*module_name;
	const char	*pred_name;
	MB_Word		proc_num;
	MB_Word		arity;
	MB_Bool		is_func;
} MB_Call;

/*
** Entry point for a native code call to det bytecode.
** Returns native code address to return to
*/
MB_Native_Addr		MB_bytecode_call_entry(MB_Call *bytecode_call);

/*
** Return to deterministic code after call to native code.
** Returns native code address to return to.
** Determines bytecode address to jump to by the contents of the
** MB_DETFRAME_BC_SUCCIP det stack slot
*/
MB_Native_Addr		MB_bytecode_return_det(void);

/* Returns pointer to the stub that calls bytecode_return_det */
MB_Native_Addr		MB_native_get_return_det(void);

/* Find the native code entry point for a procedure */
MB_Native_Addr		MB_code_find_proc_native(MB_CString_Const module,
			MB_CString_Const pred, MB_Word proc,
			MB_Word arity, MB_Bool is_func);

/**************************************************************/
/*
** det stack
** 
** Each normal det stack frame looks like the following:
**  sp-1:   [succip]
**  sp-2:   [var 0]
**  sp-3:   [var 1]
**            ...
**  sp-n:   [temp 0]
**  sp-n-1: [temp 1]
**            ...
**
** If model semidet, then temp 0 is the semidet success indicator
*/

/* saved succip */
#define MB_DETFRAME_SUCCIP			(1)

/* fixed size of deterministic stack frame */
#define MB_DETFRAME_SIZE			(1)

/*
**
** An interface det stack frame is pushed when bytecode wishes
** to jump to native code and later return to bytecode. succip
** will have been set to a stub that reads the interface stack
** frame and directs control appropriately
**  sp-1:   [succip in bytecode to return to] 
**  sp-2:   [initial frame]
**
** The initial frame field is used to determine whether a procedure should
** return to bytecode or native code when it reaches endof_proc. If it reaches
** an endof_proc instruction and after removing its stack frame finds that the
** (det or nondet) stack pointer is equal to the initial frame it knows that
** it is the most ancestral called procedure and should return to native code
** 
*/

/* bytecode return address for stub */
#define MB_DETFRAME_INTERFACE_BC_SUCCIP		(1)
#define MB_DETFRAME_INTERFACE_INITIAL_FRAME	(2)

/* Size of a deterministic interface frame */
#define MB_DETFRAME_INTERFACE_SIZE		(2)

/*
** Nondet stack
**
** An ordinary stack frame looks like so:
** curfr[ 0]	prevfr
** curfr[-1]	redoip
** curfr[-2]	redofr
** curfr[-3]	succip
** curfr[-4]	succfr
** then follows var[0] to var[n]
** then follows temp[0] to temp[n]
** 
*/
#define MB_FRAME_PREVFR		(0)
#define MB_FRAME_REDOIP		(1)
#define MB_FRAME_REDOFR		(2)

#define MB_FRAME_DETFR		(3)

#define MB_FRAME_SUCCIP		(3)
#define MB_FRAME_SUCCFR		(4)

/* size of normal nondet stack frame */
#define MB_FRAME_SIZE		5

/* size of temp nondet stack frame created by model det/semidet code */
#define MB_FRAME_TEMP_DET_SIZE	4

/* size of temp nondet stack frame created by model nondet code */
#define MB_FRAME_TEMP_SIZE	3

/* Invalid frame address */
#define MB_FRAME_INVALID	((MB_Word) (-1))

/*
** semidet success flags: stored in a temp slot until the procedure
** returns, when it is returned in a register
*/
#define MB_SEMIDET_SUCCESS	TRUE
#define MB_SEMIDET_FAILURE	FALSE

#define MB_SEMIDET_SUCCESS_REG	1
#define MB_SEMIDET_SUCCESS_SLOT	0

/**************************************************************/
/* register definitions */
#define MB_reg(n)	MR_virtual_reg(n)
#define MB_succip	MR_virtual_succip
#define MB_sp		MR_virtual_sp
#define MB_curfr	MR_virtual_curfr
#define MB_maxfr	MR_virtual_maxfr

/* Det stack: 1 is the top (used - slot 0 is unused) */
#define MB_stackitem(x)	((MB_sp)[-(x)])

/* Nondet stack - same as with det statck */
#define MB_frameitem(x)	((MB_maxfr)[-(x)])

#define MB_incr_sp(x)	(		\
			MB_sp += (x),	\
			(void)0		\
			)

#define MB_decr_sp(x)	MB_incr_sp(-(x))

/**************************************************************/
/* tags */
#include "mercury_tags.h"
#define MB_mktag(t)		MR_mktag(t)
#define MB_mkbody(b)		MR_mkbody(b)
#define MB_tag(t)		MR_tag(t)
#define MB_body(w,t)		MR_body(w,t)
#define MB_mkword(t,b)		MR_mkword(t,b)
#define MB_strip_tag(w)		MR_strip_tag(w)


#endif	/* MB_INTERFACE_H */
