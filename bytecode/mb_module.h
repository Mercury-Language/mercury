
/*
** Copyright (C) 2000-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** Code module
**
*/

#ifndef MB_MODULE_H
#define	MB_MODULE_H

#include <limits.h>

#include "mb_bytecode.h"
#include "mb_util.h"

struct MB_Module_Struct;
typedef struct MB_Module_Struct	MB_Module;

/*
** Special code addresses. INVALID_ADR must be last as the asserts
** assume that any address above INVALID_ADR is a special code
**
** If you alter these, ensure MB_ip_special reflects this
*/
#define MB_CODE_DO_FAIL		((MB_Bytecode_Addr) (-1))
#define MB_CODE_DO_REDO		((MB_Bytecode_Addr) (-2))
#define MB_CODE_NATIVE_RETURN	((MB_Bytecode_Addr) (-3))
#define MB_CODE_INVALID_ADR	((MB_Bytecode_Addr) (-4))

/* Ensure a module is loaded */
MB_Module	*MB_module_load_name(MB_CString_Const module_name);
MB_Module	*MB_module_load(MB_CString_Const module_name, FILE *fp);
/* Unload a module */
void		MB_module_unload(MB_Module *module);

/*
** Returns a pointer to a given module.
** If the module is not loaded, loads it.
*/
MB_Module	*MB_module_get(MB_CString_Const module_name);

/* Return the number of bytecodes loaded so far */
MB_Unsigned	MB_code_size(void);

/* Return the bytecode id of the bytecode at a given address */
MB_Byte		MB_code_get_id(MB_Bytecode_Addr addr);

/* Get the procedure model that a bytecode at a given address is in */
#define MB_ISDET_NO		0	/* nondet */
#define MB_ISDET_YES		1	/* det, semidet */
MB_Byte		MB_code_get_det(MB_Bytecode_Addr addr);

/* Get the bytecode argument at a given address */
MB_Bytecode_Arg	*MB_code_get_arg(MB_Bytecode_Addr addr);

/* Get the predicate in which the following address resides */
MB_Bytecode_Addr MB_code_get_pred_addr(MB_Bytecode_Addr addr);

/* Get the procedure in which the following address resides */
MB_Bytecode_Addr MB_code_get_proc_addr(MB_Bytecode_Addr addr);

MB_Bytecode_Addr MB_code_find_proc(MB_CString_Const module,
			MB_CString_Const pred,
			MB_Word proc,
			MB_Word arity,
			MB_Bool is_func);

/* Returns a code address clipped into the valid code range */
MB_Bytecode_Addr MB_code_range_clamp(MB_Bytecode_Addr addr);

/* True if the code address is 'normal' (not invalid or one of MB_CODE_xxx) */
MB_Bool		MB_ip_normal(MB_Bytecode_Addr ip);

/* True if the code address is one of MB_CODE_xxx */
MB_Bool		MB_ip_special(MB_Bytecode_Addr ip);

/* True if a native code address */
MB_Bool		MB_ip_native(MB_Bytecode_Addr ip);
	
/* Allocate memory in the code argument data array */
#define MB_CODE_DATA_ALLOC(type, number) \
	((type *) (MB_code_data_alloc_words(MB_NUMBLOCKS(sizeof(type)*(number), sizeof(MB_Word)))))

MB_Word		*MB_code_data_alloc_words(MB_Word num_words);

/*
** This is only here so pointer arithmetic will work; you shold never need
** to use any of these fields: the MB_BCID_xxx wrappers in mb_module.c are
** the only things that should use them
*/
struct MB_BCId_Struct {
	MB_Unsigned	id	: 7;
	MB_Unsigned	is_det	: 1;
	MB_Unsigned	arg	: MB_WORD_BITS - (7 + 1);
};

#endif	/* MB_MODULE_H */

