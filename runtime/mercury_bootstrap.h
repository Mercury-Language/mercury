/*
** Copyright (C) 1999-2002 The University of Melbourne.
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

/*
** These will be needed until we regularize the module-qualification
** of builtin types.
*/

#define	mercury_data_builtin__type_ctor_info_func_0 \
	mercury_data___type_ctor_info_func_0
#define	mercury_data_builtin__type_ctor_info_pred_0 \
	mercury_data___type_ctor_info_pred_0
#define	mercury_data_builtin__type_ctor_info_tuple_0 \
	mercury_data___type_ctor_info_tuple_0

/*
** This stuff is enabled by default,
** but you can disable it by defining MR_NO_BACKWARDS_COMPAT.
*/

#ifndef MR_NO_BACKWARDS_COMPAT

/*
** bool, TRUE and FALSE appear in the generated code.
** Once the installed compilers no longer generate these, they should
** be moved into the `#ifndef MR_NO_BACKWARDS_COMPAT' section.
*/
#ifdef IN_GCC
  /*
  ** We need to make sure that we pick up GCC's definition of bool, 
  ** to ensure that we don't define `bool' below.  Otherwise we get
  ** conflicts because some declarations use the <stdbool.h> definition
  ** of bool (an enum), and some use our definition (a #define for char)
  */
  #include "config.h"
  #include "system.h"
#endif

#ifndef bool
#define bool			char
#endif

#ifndef TRUE
#define TRUE			MR_TRUE
#endif
#ifndef FALSE
#define FALSE			MR_FALSE
#endif

/* 
** For a long time the Mercury C types were defined as Char, Float,
** Integer, Word, etc.  There will doubtless be lots of C code in
** libraries that relies upon these names.  
**
** People may have written code that relies upon these names, so  
** if you remove these names you need to give warning (unlike some of
** the other changes in this file).
*/

#include "mercury_types.h"
#include "mercury_float.h"

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
/*
** MR_Bool is the C representation for the Mercury type bool__bool.
** For ordinary booleans, use MR_bool in mercury_std.h.
*/
typedef MR_intptr_t		MR_Bool;
typedef MR_Bool			Bool;

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

/* Stuff from mercury_std.h */
#define streq(s1, s2)		MR_streq(s1, s2)
#define strdiff(s1, s2)		MR_strdiff(s1, s2)
#define strtest(s1, s2)		MR_strtest(s1, s2)
#define strneq(s1, s2, n)	MR_strneq(s1, s2, n)
#define strndiff(s1, s2, n)	MR_strndiff(s1, s2, n)
#define strntest(s1, s2, n)	MR_strntest(s1, s2, n)

#ifndef max
#define max(a, b)		MR_max(a, b)
#endif
#ifndef min
#define min(a, b)		MR_min(a, b)
#endif

#ifndef NO_RETURN
#define NO_RETURN		MR_NO_RETURN
#endif

#endif	/* !MR_NO_BACKWARDS_COMPAT */

/*---------------------------------------------------------------------------*/
/*
** This stuff is not enabled by default.
** To enable it, you must explicitly define MR_EXTRA_BACKWARDS_COMPAT.
*/
#ifdef	MR_EXTRA_BACKWARDS_COMPAT

#define	COMPARE_EQUAL		MR_COMPARE_EQUAL
#define	COMPARE_LESS		MR_COMPARE_LESS
#define	COMPARE_GREATER		MR_COMPARE_GREATER

#define make_aligned_string_copy(a,b)	MR_make_aligned_string_copy((a),(b))
#define make_aligned_string(a,b)	MR_make_aligned_string((a),(b))
#define string_equal(a,b)		MR_string_equal((a),(b))
#define string_const(a,b)		MR_string_const((a),(b))
#define hash_string(s)			MR_hash_string((s))
#define fatal_error(s)			MR_fatal_error((s))

#define do_redo				MR_do_redo
#define do_fail				MR_do_fail
#define do_reset_hp_fail		MR_do_reset_hp_fail
#define do_reset_framevar0_fail		MR_do_reset_framevar0_fail
#define do_succeed			MR_do_succeed
#define do_not_reached			MR_do_not_reached
#define exception_handler_do_fail	MR_exception_handler_do_fail

#define	r1				MR_r1
#define	r2				MR_r2
#define	r3				MR_r3
#define	r4				MR_r4
#define	r5				MR_r5
#define	r6				MR_r6
#define	r7				MR_r7
#define	r8				MR_r8
#define	r9				MR_r9
#define	r10				MR_r10
#define	r11				MR_r11
#define	r12				MR_r12
#define	r13				MR_r13
#define	r14				MR_r14
#define	r15				MR_r15
#define	r16				MR_r16
#define	r17				MR_r17
#define	r18				MR_r18
#define	r19				MR_r19
#define	r20				MR_r20
#define	r21				MR_r21
#define	r22				MR_r22
#define	r23				MR_r23
#define	r24				MR_r24
#define	r25				MR_r25
#define	r26				MR_r26
#define	r27				MR_r27
#define	r28				MR_r28
#define	r29				MR_r29
#define	r30				MR_r30
#define	r31				MR_r31
#define	r32				MR_r32

#define	r(N)				MR_r(N)

#define	NUM_REAL_REGS			MR_NUM_REAL_REGS

#define	progname			MR_progname
#define	program_entry_point		MR_program_entry_point
#define	address_of_init_gc		MR_address_of_init_gc
#define	address_of_init_modules		MR_address_of_init_modules
#define	address_of_init_modules_debugger	\
					MR_address_of_init_modules_debugger
#define	address_of_init_modules_type_tables	\
					MR_address_of_init_modules_type_tables
#define	do_init_modules			MR_do_init_modules
#define	do_init_modules_debugger	MR_do_init_modules_debugger
#define	do_init_modules_type_tables	MR_do_init_modules_type_tables
#define	time_at_last_stat		MR_time_at_last_stat
#define	time_at_start			MR_time_at_start

#define	deep_copy(w, ti, l, u)		MR_deep_copy((w), (ti), (l), (u))
#define	agc_deep_copy(w, ti, l, u)	MR_agc_deep_copy((w), (ti), (l), (u))

#define	init_context(context)		MR_init_context(context)
#define	create_context()		MR_create_context()
#define	destroy_context(context)	MR_destroy_context(context)
#define	init_thread_stuff(context)	MR_init_thread_stuff(context)
#define	finalize_runqueue()		MR_finalize_runqueue()
#define	flounder()			MR_flounder()
#define	runnext()			MR_runnext()
#define	schedule(context)		MR_schedule(context)
#define	set_min_heap_reclamation_point(c) MR_set_min_heap_reclamation_point(c)
#define	save_hp_in_context(context)	MR_save_hp_in_context(context)
#define	load_context(cptr)		MR_load_context(cptr)
#define	save_context(cptr)		MR_save_context(cptr)

#define	load_engine_regs(eng)		MR_load_engine_regs(eng)
#define	save_engine_regs(eng)		MR_save_engine_regs(eng)

#define	tag_incr_hp_n(d, t, c)		MR_tag_incr_hp_n((d), (t), (c))
#define	tag_incr_hp_atomic(d, t, c)	MR_tag_incr_hp_atomic((d), (t), (c))
#define	tag_incr_hp(d, t, c)		MR_tag_incr_hp((d), (t), (c))
#define	mark_hp(d)			MR_mark_hp((d))
#define	restore_hp(d)			MR_restore_hp((d))
#define	hp_alloc(c)			MR_hp_alloc((c))
#define	hp_alloc_atomic(c)		MR_hp_alloc_atomic((c))
#define	tag_incr_hp_msg(d, t, c, p, ty)	MR_tag_incr_hp_msg((d), (t),	    \
						(c), p, (ty))
#define	tag_incr_hp_atomic_msg(d, t, c, p, ty)				    \
					MR_tag_incr_hp_atomic_msg((d), (t), \
						(c), p, (ty))
#define	incr_hp(d, c)			MR_incr_hp((d), (c))
#define	incr_hp_msg(d, c, p, t)		MR_incr_hp_msg((d), (c), p, (t))
#define	incr_hp_atomic(d, c)		MR_incr_hp_atomic((d), (c))
#define	incr_hp_atomic_msg(d, c, p, t)	MR_incr_hp_atomic_msg((d), (c), p, (t))
#define	incr_saved_hp(A, B)		MR_incr_saved_hp((A), (B))
#define	incr_saved_hp_atomic(A, B)	MR_incr_saved_hp_atomic((A), (B))
#define	save_transient_hp()		MR_save_transient_hp()
#define	restore_transient_hp()		MR_restore_transient_hp()

#define	create1(w1)			MR_create1((w1))
#define	create2(w1, w2)			MR_create2((w1), (w2))
#define	create3(w1, w2, w3)		MR_create3((w1), (w2), (w3))
#define	create1_msg(w1, p, t)		MR_create1_msg((w1), p, (t))
#define	create2_msg(w1, w2, p, t)	MR_create2_msg((w1), (w2), p, (t))
#define	create3_msg(w1, w2, w3, p, t)	MR_create3_msg((w1), (w2), (w3), p, (t))

#define	paste(a,b)			MR_paste(a,b)
#define	stringify(s)			MR_stringify(s)
#define	entry(label)			MR_entry(label)
#define	skip(label)			MR_skip(label)

#define	make_label(n, a, l)		MR_make_label((n), (a), (l))
#define	make_label_ai(n, a, l)		MR_make_label_ai((n), (a), (l))
#define	make_label_sl(n, a, l)		MR_make_label_sl((n), (a), (l))
#define	make_local(n, a, l)		MR_make_local((n), (a), (l))
#define	make_local_ai(n, a, l)		MR_make_local_ai((n), (a), (l))
#define	make_local_sl(n, a, l)		MR_make_local_sl((n), (a), (l))
#define	make_entry(n, a, l)		MR_make_entry((n), (a), (l))
#define	make_entry_ai(n, a, l)		MR_make_entry_ai((n), (a), (l))
#define	make_entry_sl(n, a, l)		MR_make_entry_sl((n), (a), (l))

#define	init_label(l)			MR_init_label(l)
#define	init_label_ai(l)		MR_init_label_ai(l)
#define	init_label_sl(l)		MR_init_label_sl(l)
#define	init_local(l)			MR_init_local(l)
#define	init_local_ai(l)		MR_init_local_ai(l)
#define	init_local_sl(l)		MR_init_local_sl(l)
#define	init_entry(l)			MR_init_entry(l)
#define	init_entry_ai(l)		MR_init_entry_ai(l)
#define	init_entry_sl(l)		MR_init_entry_sl(l)

#define	Declare_entry(l)		MR_declare_entry(l)
#define	Define_extern_entry(l)		MR_define_extern_entry(l)
#define	Define_entry(l)			MR_define_entry(l)
#define	Declare_static(l)		MR_declare_static(l)
#define	Define_static(l)		MR_define_static(l)
#define	Declare_local(l)		MR_declare_local(l)
#define	Define_local(l)			MR_define_local(l)
#define	Declare_label(l)		MR_declare_label(l)
#define	Define_label(l)			MR_define_label(l)

#define	ASM_JUMP(label)			MR_ASM_JUMP(label)
#define	JUMP(label)			MR_JUMP(label)
#define	ENTRY(label)			MR_ENTRY(label)
#define	STATIC(label)			MR_STATIC(label)
#define	LOCAL(label)			MR_LOCAL(label)
#define	LABEL(label)			MR_LABEL(label)
#define	GOTO(label)			MR_GOTO(label)
#define	GOTO_ENTRY(label)		MR_GOTO_ENTRY(label)
#define	GOTO_STATIC(label)		MR_GOTO_STATIC(label)
#define	GOTO_LOCAL(label)		MR_GOTO_LOCAL(label)
#define	GOTO_LABEL(label)		MR_GOTO_LABEL(label)

/*
** These two cannot be defined in terms of their MR_ equivalents,
** since that loses token structure. We therefore duplicate the definition.
*/

#define	COMPUTED_GOTO(val, labels)			\
	{						\
		static MR_Code *jump_table[] = {	\
			labels				\
		};					\
		MR_GOTO(jump_table[val]);		\
	}

#define	AND				,

#define	BEGIN_MODULE(m)			MR_BEGIN_MODULE(m)
#define	BEGIN_CODE			MR_BEGIN_CODE
#define	END_MODULE			MR_END_MODULE

#define	ModuleFunc			MR_ModuleFunc

#define	noprof_localcall(label)		MR_noprof_localcall(label)
#define	noprof_call(label, cont)	MR_noprof_call(label, cont)
#define	noprof_call_localret(label, cont)	\
					MR_noprof_call_localret(label, cont)
#define	localcall(label, cont, cur)	MR_localcall(label, cont, cur)
#define	call(proc, cont, cur)		MR_call(proc, cont, cur)
#define	call_localret(proc, cont, cur)	MR_call_localret(proc, cont, cur)
#define	localtailcall(label, cur)	MR_localtailcall(label, cur)
#define	tailcall(proc, cur)		MR_tailcall(proc, (cur))
#define	noprof_tailcall(proc)		MR_noprof_tailcall(proc)
#define	proceed()			MR_proceed()

#define	set_prof_current_proc(target)	 MR_set_prof_current_proc(target)
#define	update_prof_current_proc(target) MR_update_prof_current_proc(target)

#define	float_to_word(f)		MR_float_to_word(f)
#define	word_to_float(w)		MR_word_to_float(w)
#define	float_const(f)			MR_float_const(f)
#define	hash_float(f)			MR_hash_float(f)

#if 0
/*
** All the places that use these macros
** should have been updated to use the new forms.
*/
#define LVALUE_CAST(type, lval)		MR_LVALUE_CAST((type), (lval))
#define LVALUE_SEQ(expr, lval)		MR_LVALUE_SEQ((expr), (lval))
#define LVALUE_COND(expr, x, y)		MR_LVALUE_COND((expr), (x), (y))

#define	count_usage(num, reg)		MR_count_usage((num), (reg))
#define	saved_reg(save_area, n)		MR_saved_reg((save_area), (n))
#define	virtual_reg(n)			MR_virtual_reg((n))
#endif

/*---------------------------------------------------------------------------*/

#define MR_TypeCtorInfo_struct  MR_TypeCtorInfo_Struct

#define	save_regs_to_mem(save_area)					\
				MR_save_regs_to_mem(save_area)
#define	restore_regs_from_mem(save_area) 				\
				MR_restore_regs_from_mem(save_area)

#define	save_transient_regs_to_mem(save_area)				\
				MR_save_transient_regs_to_mem(save_area)
#define	restore_transient_regs_from_mem(save_area)			\
				MR_restore_transient_regs_from_mem(save_area)

#define	save_registers()		MR_save_registers()
#define	restore_registers()		MR_restore_registers()

#define	save_transient_registers()	MR_save_transient_registers()
#define	restore_transient_registers()	MR_restore_transient_registers()

#define	save_transient_hp()		MR_save_transient_hp()
#define	restore_transient_hp()		MR_restore_transient_hp()

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

#endif	/* MR_EXTRA_BACKWARDS_COMPAT */

#endif	/* MERCURY_BOOTSTRAP_H */
