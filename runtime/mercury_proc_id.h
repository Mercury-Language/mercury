/*
** Copyright (C) 2003-2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_proc_id.h - definitions for recording procedure ids */

#ifndef MERCURY_PROC_ID_H
#define MERCURY_PROC_ID_H

#include "mercury_types.h"		/* for MR_ConstString etc */
#include "mercury_tags.h"		/* for MR_DEFINE_BUILTIN_ENUM_CONST */

/*
** This type indicates whether a procedure came from a predicate or a function.
** This enum should EXACTLY match the definition of the `pred_or_func' type
** in browser/util.m.
*/

typedef	enum {
	MR_DEFINE_BUILTIN_ENUM_CONST(MR_PREDICATE),
	MR_DEFINE_BUILTIN_ENUM_CONST(MR_FUNCTION)
} MR_PredFunc;

/*
** MR_Proc_Id is a union. The usual alternative identifies ordinary
** procedures, while the other alternative identifies automatically generated
** unification, comparison and index procedures. The meanings of the fields
** in both forms are the same as in procedure labels. The runtime system
** can figure out which form is present by using the macro
** MR_PROC_LAYOUT_COMPILER_GENERATED, which will return true only if
** the procedure is of the second type.
**
** The compiler generates MR_User_Proc_Id and MR_UCI_Proc_Id structures
** in order to avoid having to initialize the MR_Proc_Id union through the
** inapplicable alternative, since the C standard in widespread use now
** doesn't support that.
**
** The places that know about the structure of procedure ids include
** browser/dl.m and besides all the places that refer to the C types below.
*/

struct MR_User_Proc_Id_Struct {
	MR_PredFunc		MR_user_pred_or_func;
	MR_ConstString		MR_user_decl_module;
	MR_ConstString		MR_user_def_module;
	MR_ConstString		MR_user_name;
	MR_int_least16_t	MR_user_arity;
	MR_int_least16_t	MR_user_mode;
};

struct MR_UCI_Proc_Id_Struct {
	MR_ConstString		MR_uci_type_name;
	MR_ConstString		MR_uci_type_module;
	MR_ConstString		MR_uci_def_module;
	MR_ConstString		MR_uci_pred_name;
	MR_int_least16_t	MR_uci_type_arity;
	MR_int_least16_t	MR_uci_mode;
};

union MR_Proc_Id_Union {
	MR_User_Proc_Id		MR_proc_user;
	MR_UCI_Proc_Id		MR_proc_uci;
};

#define	MR_PROC_ID_COMPILER_GENERATED(proc_id)				\
	((MR_Unsigned) (proc_id).MR_proc_user.MR_user_pred_or_func	\
	 	> MR_FUNCTION)

#endif /* not MERCURY_PROC_ID_H */
