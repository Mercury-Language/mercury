/*
** Copyright (C) 1999-2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_ho_call.h - defines the structure of closures.
**
** The places in the system that know about the layouts of closures are
**
**	compiler/unify_gen.m (unify_gen__generate_construction_2)
**	runtime/mercury_ho_call.[ch]
**
** Any changes here will need to be reflected in the other places as well.
*/

#ifndef	MERCURY_HO_CALL_H
#define	MERCURY_HO_CALL_H

#include "mercury_stack_layout.h"	/* for MR_Closure_Id etc */
#include "mercury_type_info.h"		/* for MR_PseudoTypeInfo */

/*
** A closure layout structure identifies a procedure, and contains
** the information required to identify the types of the arguments
** in any closure that calls that procedure. It is represented as a
** vector of words containing
**
**	a pointer to an MR_Closure_Id structure
**	a pointer to information about the locations of typeinfos
**		for the type parameters of the procedure
**		(NULL if there are no type parameters)
**	one word giving the number of arguments of the procedure (M)
**	M words giving pseudotypeinfos for the arguments
**
** A closure that refers to the procedure may not (and probably will not)
** contain values for all the arguments of the procedure, but the closure
** layout structure has information about all arguments. This is to make
** the creation of a closure from another closure by adding some more
** hidden arguments as fast as possible. There is no problem in finding
** out which pseudotypeinfo describes which hidden argument, because if
** the closure contains n hidden arguments, these must be the first n arguments
** of the procedure.
**
** The typeinfo and typeclassinfo arguments describing the actual types bound
** to type vars are always at the start of the argument list. A closure can
** contain arg i but not arg j only if i < j; this means that if a closure
** contains a non-typeinfo argument j, it also contains all the typeinfo
** and typeclassinfo arguments of the procedure and therefore (directly or
** indirectly) all the typeinfos that may be referred to in the pseudotypeinfo
** for argument j. (If we ever allow code to take the address of a procedure
** whose signature includes an existential type, we may have to rethink this.)
**
** The MR_Live_Lvals inside MR_Type_Param_Locns, which encode the locations
** of the typeinfos for the type variables in the signature of the procedure,
** assume that argument i is in register ri. While this will be true at the
** time of the call, code that wants to manipulate the closure as an
** independent entity will have to substitute the argument vector in the
** closure itself for the register file.
**
** Note that if a module is compiled without typeinfo liveness, then closures
** will not have any layout information. This will be indicated by the value
** of num_all_args being negative, which says that the only field of this
** structure containing valid information is proc_id.
**
** The Dyn_Link variant is for closures created by browser/dl.m. The closure_id
** field of such closures will contain an invalid proc_id (which you can tell
** from the negative arity) and a creation context that is also different from
** other closures: instead of specifying the source context where the closure
** is created, it puts a sequence number into the field that normally contains
** the line number.
*/

typedef struct MR_Closure_Layout_Struct {
	MR_Closure_Id		*MR_closure_id;
	MR_Type_Param_Locns	*MR_closure_type_params;
	MR_Integer		MR_closure_num_all_args;
	MR_PseudoTypeInfo	MR_closure_arg_pseudo_type_info
					[MR_VARIABLE_SIZED];
} MR_Closure_Layout;

typedef struct MR_Closure_Dyn_Link_Layout_Struct {
	MR_Closure_Id		*MR_closure_dl_id;
	MR_Type_Param_Locns	*MR_closure_dl_type_params;
	MR_Integer		MR_closure_dl_num_all_args;
} MR_Closure_Dyn_Link_Layout;

/*
** A closure is a vector of words containing:
**
**	one word pointing to the closure layout structure of the procedure
**	one word pointing to the code of the procedure
**	one word giving the number of arguments hidden in the closure (N)
**	N words representing the N hidden arguments
**
** The reason why the closure layout pointer is first is that most operations
** on closures do not need to access that word, and this way it does not
** have be brought into the cache.
**
** Note that the arguments are numbered from one, but the array subscripts
** start at zero. To prevent this from being a problem, we use a deliberately
** ugly name for the field containing the array, and provide a nicer way of
** referring to the array via a macro. The arguments of this macro number
** the arguments from one.
*/

typedef struct MR_Closure_Struct {
	MR_Closure_Layout	*MR_closure_layout;
	MR_Code			*MR_closure_code;
	MR_Unsigned		MR_closure_num_hidden_args;
	MR_Word			MR_closure_hidden_args_0[MR_VARIABLE_SIZED];
} MR_Closure;

#define	MR_closure_hidden_args(i)	MR_closure_hidden_args_0[(i) - 1]

#endif /* not MERCURY_HO_CALL_H */
