/*
** Copyright (C) 1999-2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_bootstrap.h
**
** Temporary definitions only needed for bootstrapping and/or
** for backwards compatibility.  All of the definitions here
** will go away eventually, so don't use them!
*/

#ifndef	MERCURY_BOOTSTRAP_H
#define	MERCURY_BOOTSTRAP_H

/*---------------------------------------------------------------------------*/
/*
** This stuff is enabled by default,
** but you can disable it by defining MR_NO_BACKWARDS_COMPAT.
*/

#define MR_TypeCtorInfo_struct  MR_TypeCtorInfo_Struct

#ifndef MR_NO_BACKWARDS_COMPAT

/* 
** For a long time the Mercury C types were defined as Char, Float,
** Integer, Word, etc.  There will doubtless be lots of C code in
** libraries that relies upon these names.  
**
** People may have written code that relies upon these names, so  
** if you remove these names you need to give warning (unlike some of
** the other changes in this file).
*/

typedef MR_Word 		Word;
typedef MR_Code 		Code;
typedef MR_Char 		Char;
typedef MR_Float 		Float;
typedef MR_Float64 		Float64;
typedef MR_Integer 		Integer;
typedef MR_Unsigned 		Unsigned;
typedef MR_UnsignedChar 	UnsignedChar;
typedef MR_String 		String;
typedef MR_ConstString 		ConstString;
typedef MR_Bool 		Bool;


#define	COMPARE_EQUAL		MR_COMPARE_EQUAL
#define	COMPARE_LESS		MR_COMPARE_LESS
#define	COMPARE_GREATER		MR_COMPARE_GREATER

#define make_aligned_string_copy(a,b)	MR_make_aligned_string_copy((a),(b))
#define make_aligned_string(a,b)	MR_make_aligned_string((a),(b))
#define string_equal(a,b)		MR_string_equal((a),(b))
#define string_const(a,b)		MR_string_const((a),(b))
#define hash_string(s)			MR_hash_string((s))
#define fatal_error(s)			MR_fatal_error((s))

/*
** The list manipulation macros are available for use by ordinary Mercury
** programmers. People may have written code using these macros before their
** documented names were changed, so removing backward compatibility for them
** requires more warning than for the other things in this file.
*/

#define	list_is_empty(l)	MR_list_is_empty(l)
#define	list_head(l)		MR_list_head(l)
#define	list_tail(l)		MR_list_tail(l)
#define	list_empty()		MR_list_empty()
#define	list_cons(h, t)		MR_list_cons((h), (t))

#endif	/* MR_NO_BACKWARDS_COMPAT */

/*---------------------------------------------------------------------------*/
/*
** This stuff is not enabled by default.
** To enable it, you must explicitly define MR_EXTRA_BACKWARDS_COMPAT.
*/
#ifdef	MR_EXTRA_BACKWARDS_COMPAT

#define succip			MR_succip
#define hp			MR_hp
#define sp			MR_sp
#define curfr			MR_curfr
#define maxfr			MR_maxfr

/* stuff from mercury_stacks.h */

#define	detstackvar(n)		MR_stackvar(n)

#define	incr_sp_push_msg(n, m)	MR_incr_sp_push_msg((n), (m))
#define	decr_sp_pop_msg(n)	MR_decr_sp_pop_msg(n)
#define	incr_sp(n)		MR_incr_sp(n)
#define	decr_sp(n)		MR_decr_sp(n)

#define	push(w)		(					\
				*MR_sp = (MR_Word) (w),		\
				debugpush(*MR_sp, MR_sp),	\
				MR_sp = MR_sp + 1,		\
				detstack_overflow_check(),	\
				(void)0				\
			)

#define	pop()		(					\
				MR_sp = MR_sp - 1,		\
				debugpop(*MR_sp, MR_sp),	\
				detstack_underflow_check(),	\
				/* return */ *MR_sp		\
			)

#define	NONDET_FIXED_SIZE	MR_NONDET_FIXED_SIZE

#define	framevar(n)		MR_framevar((n) + 1)

#define	bt_prevfr(fr)		MR_prevfr_slot(fr)
#define	bt_redoip(fr)		MR_redoip_slot(fr)
#define	bt_redofr(fr)		MR_redofr_slot(fr)
#define	bt_succip(fr)		MR_succip_slot(fr)
#define	bt_succfr(fr)		MR_succfr_slot(fr)
#define	bt_prednm(fr)		MR_prednm_slot(fr)
#define	bt_var(fr, n)		MR_based_framevar(fr, (n) + 1)

#define	curprevfr		MR_prevfr_slot(MR_curfr)
#define	curredoip		MR_redoip_slot(MR_curfr)
#define	curredofr		MR_redofr_slot(MR_curfr)
#define	cursuccip		MR_succip_slot(MR_curfr)
#define	cursuccfr		MR_succfr_slot(MR_curfr)
#define	curprednm		MR_prednm_slot(MR_curfr)

#define prednm_slot(fr)		"unknown"
#define MR_prednm_slot(fr)	"unknown"

#define	mkframe(p, s, r)	MR_mkframe((p), (s), (r))
#define	mkpragmaframe(p,s,n,r)	MR_mkpragmaframe((p), (s), n, (r))
#define	mktempframe(r)		MR_mktempframe(r)
#define	mkdettempframe(r)	MR_mkdettempframe(r)
#define	succeed()		MR_succeed()
#define	succeed_discard()	MR_succeed_discard()
#define	fail()			MR_fail()
#define	redo()			MR_redo()

/* stuff from mercury_tags.h */

#define	WORDBITS		MR_WORDBITS

#define	mktag(t)		MR_mktag(t)
#define	unmktag(w)		MR_unmktag(w)
#define	tag(w)			MR_tag(w)
#define	mkbody(i)		MR_mkbody(i)
#define	unmkbody(w)		MR_unmkbody(w)
#define	body(w, t)		MR_body((w), (t))
#define	strip_tag(w)		MR_strip_tag(w)

#define	mkword(t, p)		MR_mkword((t), (p))
#define	field(t, p, i)		MR_field((t), (p), (i))
#define	const_field(t, p, i)	MR_const_field((t), (p), (i))
#define	mask_field(p, i)	MR_mask_field((p), (i))
#define	const_mask_field(p, i)	MR_const_mask_field((p), (i))

/*
** The code generated by Mercury 0.8 includes references to these macros.
*/

#define MR_OBTAIN_GLOBAL_C_LOCK()	MR_OBTAIN_GLOBAL_LOCK("pragma c code")
#define MR_RELEASE_GLOBAL_C_LOCK()	MR_RELEASE_GLOBAL_LOCK("pragma c code")


/*
** Generated code will create references to base_type_* which has been
** renamed.  Builtin types use hand-defined type_ctor_*, so we need to
** #define the old names for these structures so the stage 1 compiler
** will link.  The stage 2 compiler will just generate type_ctor_*
** references anyway, so then these #defines are not needed.
*/

#define MR_INIT_BASE_TYPE_INFO		MR_INIT_TYPE_CTOR_INFO

#define mercury_data_array__base_type_functors_array_1 \
	mercury_data_array__type_ctor_functors_array_1 

#define mercury_data_array__base_type_functors_array_1_struct \
	mercury_data_array__type_ctor_functors_array_1_struct

#define mercury_data_array__base_type_layout_array_1 \
	mercury_data_array__type_ctor_layout_array_1 

#define mercury_data_array__base_type_layout_array_1_struct \
	mercury_data_array__type_ctor_layout_array_1_struct


#define mercury_data_builtin__base_type_functors_c_pointer_0 \
	mercury_data_builtin__type_ctor_functors_c_pointer_0 

#define mercury_data_builtin__base_type_functors_c_pointer_0_struct \
	mercury_data_builtin__type_ctor_functors_c_pointer_0_struct

#define mercury_data_builtin__base_type_layout_c_pointer_0 \
	mercury_data_builtin__type_ctor_layout_c_pointer_0 

#define mercury_data_builtin__base_type_layout_c_pointer_0_struct \
	mercury_data_builtin__type_ctor_layout_c_pointer_0_struct


#define mercury_data_std_util__base_type_functors_type_info_0 \
	mercury_data_std_util__type_ctor_functors_type_info_0 

#define mercury_data_std_util__base_type_functors_type_info_0_struct \
	mercury_data_std_util__type_ctor_functors_type_info_0_struct

#define mercury_data_std_util__base_type_layout_type_info_0 \
	mercury_data_std_util__type_ctor_layout_type_info_0 

#define mercury_data_std_util__base_type_layout_type_info_0_struct \
	mercury_data_std_util__type_ctor_layout_type_info_0_struct


#define mercury_data_std_util__base_type_functors_univ_0 \
	mercury_data_std_util__type_ctor_functors_univ_0 

#define mercury_data_std_util__base_type_functors_univ_0_struct \
	mercury_data_std_util__type_ctor_functors_univ_0_struct

#define mercury_data_std_util__base_type_layout_univ_0 \
	mercury_data_std_util__type_ctor_layout_univ_0 

#define mercury_data_std_util__base_type_layout_univ_0_struct \
	mercury_data_std_util__type_ctor_layout_univ_0_struct

#define mercury_data___base_type_info_int_0_struct \
	mercury_data___type_ctor_info_int_0_struct

#define mercury_data___base_type_info_int_0 \
	mercury_data___type_ctor_info_int_0

#define mercury_data___base_type_info_string_0_struct \
	mercury_data___type_ctor_info_string_0_struct

#define mercury_data___base_type_info_string_0 \
	mercury_data___type_ctor_info_string_0

#define mercury_data___base_type_info_character_0 \
	mercury_data___type_ctor_info_character_0

#define mercury_data___base_type_info_character_0_struct \
	mercury_data___type_ctor_info_character_0_struct

#define mercury_data___base_type_info_float_0 \
	mercury_data___type_ctor_info_float_0

#define mercury_data___base_type_info_float_0_struct \
	mercury_data___type_ctor_info_float_0_struct

#define mercury_data___base_type_info_pred_0 \
	mercury_data___type_ctor_info_pred_0

#define mercury_data___base_type_info_pred_0_struct \
	mercury_data___type_ctor_info_pred_0_struct

#define mercury_data_private_builtin__base_type_info_type_info_1 \
	mercury_data_private_builtin__type_ctor_info_type_info_1

#define mercury_data_private_builtin__base_type_info_type_info_1_struct \
	mercury_data_private_builtin__type_ctor_info_type_info_1_struct


/*
** These definitions are needed to bootstrap the change of all
** type_ctor_info structures to use the MR_TypeCtorInfo type.
*/

#define mercury_data___type_ctor_info_int_0_struct \
	MR_TypeCtorInfo_struct
#define mercury_data___type_ctor_info_string_0_struct \
	MR_TypeCtorInfo_struct
#define mercury_data___type_ctor_info_float_0_struct \
	MR_TypeCtorInfo_struct
#define mercury_data___type_ctor_info_character_0_struct \
	MR_TypeCtorInfo_struct
#define mercury_data___type_ctor_info_pred_0_struct \
	MR_TypeCtorInfo_struct
#define mercury_data___type_ctor_info_func_0_struct \
	MR_TypeCtorInfo_struct

#endif	/* MR_EXTRA_BACKWARDS_COMPAT */

#endif	/* MERCURY_BOOTSTRAP_H */
