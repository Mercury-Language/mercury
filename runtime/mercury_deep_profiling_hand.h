/*
** Copyright (C) 2001, 2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_deep_profiling_hand.h -- definitions for deep profiling
** for use in hand-written procedures.
*/

#ifndef MERCURY_DEEP_PROFILING_HAND_H
#define MERCURY_DEEP_PROFILING_HAND_H

#include "mercury_std.h"
#include "mercury_deep_profiling.h"

#define	MR_proc_static_user_builtin_name(name, arity, mode)		\
	MR_PASTE6(mercury_data__proc_static__,				\
		name, _, arity, _, mode)
#define	MR_call_sites_user_builtin_name(name, arity, mode)		\
	MR_PASTE6(mercury_data__proc_static_call_sites__mercury__,	\
		name, _, arity, _, mode)

#define	MR_proc_static_user_name(module, name, arity, mode)		\
	MR_PASTE8(mercury_data__proc_static__,				\
		module, __, name, _, arity, _, mode)
#define	MR_call_sites_user_name(module, name, arity, mode)		\
	MR_PASTE8(mercury_data__proc_static_call_sites__mercury__,	\
		module, __, name, _, arity, _, mode)

#define	MR_proc_static_compiler_name(module, name, type, arity, mode)	\
	MR_PASTE10(mercury_data__proc_static__,				\
		name, _, module, __, type, _, arity, _, mode)
#define	MR_call_sites_compiler_name(module, name, type, arity, mode)	\
	MR_PASTE10(mercury_data__proc_static_call_sites__mercury__,	\
		name, _, module, __, type, _, arity, _, mode)

#ifdef MR_USE_ACTIVATION_COUNTS
  #define	MR_maybe_activation_count_field		0,
#else
  #define	MR_maybe_activation_count_field
#endif

#define	MR_proc_static_user_builtin_empty(name, arity, mode, file, line, interface)\
	MR_User_ProcStatic						\
	MR_proc_static_user_builtin_name(name, arity, mode) = {		\
		{							\
			MR_PREDICATE,					\
			"builtin",					\
			"builtin",					\
			MR_STRINGIFY(name),				\
			arity, 						\
			mode, 						\
		},							\
		file,							\
		line,							\
		interface,						\
		0,							\
		NULL,							\
		MR_maybe_activation_count_field				\
		NULL							\
	}

#define	MR_proc_static_compiler_empty(module, name, type, arity, mode, file, line, interface) \
	MR_Compiler_ProcStatic						\
	MR_proc_static_compiler_name(module, name, type, arity, mode) = { \
		{							\
			MR_STRINGIFY(type),				\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(name),				\
			arity, 						\
			mode, 						\
		},							\
		file,							\
		line,							\
		interface,						\
		0,							\
		NULL,							\
		MR_maybe_activation_count_field				\
		NULL							\
	}

#define	MR_proc_static_user_empty(module, name, arity, mode, file, line, interface)\
	MR_User_ProcStatic						\
	MR_proc_static_user_name(module, name, arity, mode) = {		\
		{							\
			MR_PREDICATE,					\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(name),				\
			arity, 						\
			mode, 						\
		},							\
		file,							\
		line,							\
		interface,						\
		0,							\
		NULL,							\
		MR_maybe_activation_count_field				\
		NULL							\
	}

#define	MR_proc_static_user_plain(module, name, arity, mode, cmodule, cname, carity, cmode, file, line, interface)\
	static const MR_CallSiteStatic					\
	MR_call_sites_user_name(module, name, arity, mode)[] = {	\
		{ MR_normal_call, (MR_ProcStatic *)			\
		&MR_proc_static_user_name(cmodule, cname, carity, cmode),\
		NULL, "", line, "" }					\
	};								\
									\
	MR_User_ProcStatic						\
	MR_proc_static_user_name(module, name, arity, mode) = {		\
		{							\
			MR_PREDICATE,					\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(name),				\
			arity, 						\
			mode, 						\
		},							\
		file,							\
		line,							\
		interface,						\
		1,							\
		MR_call_sites_user_name(module, name, arity, mode),	\
		MR_maybe_activation_count_field				\
		NULL							\
	}

#define	MR_proc_static_compiler_plain(module, name, type, arity, mode, cmodule, cname, carity, cmode, file, line, interface)\
	static const MR_CallSiteStatic					\
	MR_call_sites_compiler_name(module, name, type, arity, mode)[] = {\
		{ MR_normal_call, (MR_ProcStatic *)			\
		&MR_proc_static_user_name(cmodule, cname, carity, cmode),\
		NULL, "", line, "" }					\
	};								\
									\
	MR_Compiler_ProcStatic						\
	MR_proc_static_compiler_name(module, name, type, arity, mode) = {\
		{							\
			MR_STRINGIFY(type),				\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(name),				\
			arity, 						\
			mode, 						\
		},							\
		file,							\
		line,							\
		interface,						\
		1,							\
		MR_call_sites_compiler_name(module, name, type, arity, mode),\
		MR_maybe_activation_count_field				\
		NULL							\
	}

#define	MR_proc_static_user_ho(module, name, arity, mode, file, line, interface)	\
	static const MR_CallSiteStatic					\
	MR_call_sites_user_name(module, name, arity, mode)[] = {	\
		{ MR_higher_order_call, NULL,				\
		NULL, "", line, "" }					\
	};								\
									\
	MR_User_ProcStatic						\
	MR_proc_static_user_name(module, name, arity, mode) = {		\
		{							\
			MR_PREDICATE,					\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(module),				\
			MR_STRINGIFY(name),				\
			arity, 						\
			mode, 						\
		},							\
		file,							\
		line,							\
		interface,						\
		1,							\
		MR_call_sites_user_name(module, name, arity, mode),	\
		MR_maybe_activation_count_field				\
		NULL							\
	}

/*****************************************************************************/

#define	MR_deep_det_call_ac(proclabel, procstatic, first_slot, label)	\
	MR_r1 = (MR_Word) (MR_Word *) &procstatic;			\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__det_call_port_code_ac_3_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel));		\
	MR_stackvar(first_slot) = MR_r1;	/* TopCSD */		\
	MR_stackvar(first_slot+1) = MR_r2;	/* MiddleCSD */		\
	(void) 0

#define	MR_deep_det_exit_ac(proclabel, first_slot, label)		\
	MR_r1 = MR_stackvar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_stackvar(first_slot+1);	/* MiddleCSD */		\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__det_exit_port_code_ac_2_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

/*****************************************************************************/

#define	MR_deep_det_call_sr(proclabel, procstatic, first_slot, label)	\
	MR_r1 = (MR_Word) (MR_Word *) &procstatic;			\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__det_call_port_code_sr_4_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel));		\
	MR_stackvar(first_slot) = MR_r1;	/* TopCSD */		\
	MR_stackvar(first_slot+1) = MR_r2;	/* MiddleCSD */		\
	MR_stackvar(first_slot+2) = MR_r3;	/* OldActivationPtr */	\
	(void) 0

#define	MR_deep_det_exit_sr(proclabel, first_slot, label)		\
	MR_r1 = MR_stackvar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_stackvar(first_slot+1);	/* MiddleCSD */		\
	MR_r3 = MR_stackvar(first_slot+2);	/* OldActivationPtr */	\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__det_exit_port_code_sr_3_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

/*****************************************************************************/

#define	MR_deep_semi_call_ac(proclabel, procstatic, first_slot, label)	\
	MR_r1 = (MR_Word) (MR_Word *) &procstatic;			\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__semi_call_port_code_ac_3_0),\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel));		\
	MR_stackvar(first_slot) = MR_r1;	/* TopCSD */		\
	MR_stackvar(first_slot+1) = MR_r2;	/* MiddleCSD */		\
	(void) 0

#define	MR_deep_semi_exit_ac(proclabel, first_slot, label)		\
	MR_r1 = MR_stackvar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_stackvar(first_slot+1);	/* MiddleCSD */		\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__semi_exit_port_code_ac_2_0),\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#define	MR_deep_semi_fail_ac(proclabel, first_slot, label)		\
	MR_r1 = MR_stackvar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_stackvar(first_slot+1);	/* MiddleCSD */		\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__semi_fail_port_code_ac_2_0),\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

/*****************************************************************************/

#define	MR_deep_semi_call_sr(proclabel, procstatic, first_slot, label)	\
	MR_r1 = (MR_Word) (MR_Word *) &procstatic;			\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__semi_call_port_code_sr_4_0),\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel));		\
	MR_stackvar(first_slot) = MR_r1;	/* TopCSD */		\
	MR_stackvar(first_slot+1) = MR_r2;	/* MiddleCSD */		\
	MR_stackvar(first_slot+2) = MR_r3;	/* OldActivationPtr */	\
	(void) 0

#define	MR_deep_semi_exit_sr(proclabel, first_slot, label)		\
	MR_r1 = MR_stackvar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_stackvar(first_slot+1);	/* MiddleCSD */		\
	MR_r3 = MR_stackvar(first_slot+2);	/* OldActivationPtr */	\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__semi_exit_port_code_sr_3_0),\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#define	MR_deep_semi_fail_sr(proclabel, first_slot, label)		\
	MR_r1 = MR_stackvar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_stackvar(first_slot+1);	/* MiddleCSD */		\
	MR_r3 = MR_stackvar(first_slot+2);	/* OldActivationPtr */	\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__semi_fail_port_code_sr_3_0),\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

/*****************************************************************************/

#define	MR_deep_non_call_ac(proclabel, procstatic, first_slot, label)	\
	MR_r1 = (MR_Word) (MR_Word *) &procstatic;			\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_call_port_code_ac_4_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel));		\
	MR_framevar(first_slot) = MR_r1;	/* TopCSD */		\
	MR_framevar(first_slot+1) = MR_r2;	/* MiddleCSD */		\
	MR_framevar(first_slot+3) = MR_r3;	/* NewActivationPtr */	\
	(void) 0

#define	MR_deep_non_exit_ac(proclabel, first_slot, label)		\
	MR_r1 = MR_framevar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_framevar(first_slot+1);	/* MiddleCSD */		\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_exit_port_code_ac_2_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#define	MR_deep_non_redo_ac(proclabel, first_slot, label)		\
	MR_r1 = MR_framevar(first_slot+1);	/* MiddleCSD */		\
	MR_r2 = MR_framevar(first_slot+2);	/* NewActivationPtr */	\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_redo_port_code_ac_2_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#define	MR_deep_non_fail_ac(proclabel, first_slot, label)		\
	MR_r1 = MR_framevar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_framevar(first_slot+1);	/* MiddleCSD */		\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_fail_port_code_ac_2_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

/*****************************************************************************/

#define	MR_deep_non_call_sr(proclabel, procstatic, first_slot, label)	\
	MR_r1 = (MR_Word) (MR_Word *) &procstatic;			\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_call_port_code_sr_5_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel));		\
	MR_framevar(first_slot) = MR_r1;	/* TopCSD */		\
	MR_framevar(first_slot+1) = MR_r2;	/* MiddleCSD */		\
	MR_framevar(first_slot+3) = MR_r3;	/* OldActivationPtr */	\
	MR_framevar(first_slot+4) = MR_r4;	/* NewActivationPtr */	\
	(void) 0

#define	MR_deep_non_exit_sr(proclabel, first_slot, label)		\
	MR_r1 = MR_framevar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_framevar(first_slot+1);	/* MiddleCSD */		\
	MR_r3 = MR_framevar(first_slot+2);	/* OldActivationPtr */	\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_exit_port_code_sr_3_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#define	MR_deep_non_redo_sr(proclabel, first_slot, label)		\
	MR_r1 = MR_framevar(first_slot+1);	/* MiddleCSD */		\
	MR_r2 = MR_framevar(first_slot+3);	/* NewActivationPtr */	\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_redo_port_code_sr_2_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#define	MR_deep_non_fail_sr(proclabel, first_slot, label)		\
	MR_r1 = MR_framevar(first_slot);	/* TopCSD */		\
	MR_r2 = MR_framevar(first_slot+1);	/* MiddleCSD */		\
	MR_r3 = MR_framevar(first_slot+2);	/* OldActivationPtr */	\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__non_fail_port_code_sr_3_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

/*****************************************************************************/

#ifdef	MR_USE_ACTIVATION_COUNTS
  #define	MR_deep_det_call(proclabel, procstatic, first_slot, label) \
		MR_deep_det_call_ac(proclabel, procstatic, first_slot, label)
  #define	MR_deep_det_exit(proclabel, first_slot, label)		\
		MR_deep_det_exit_ac(proclabel, first_slot, label)

  #define	MR_deep_semi_call(proclabel, procstatic, first_slot, label) \
		MR_deep_semi_call_ac(proclabel, procstatic, first_slot, label)
  #define	MR_deep_semi_exit(proclabel, first_slot, label)		\
		MR_deep_semi_exit_ac(proclabel, first_slot, label)
  #define	MR_deep_semi_fail(proclabel, first_slot, label)		\
		MR_deep_semi_fail_ac(proclabel, first_slot, label)

  #define	MR_deep_non_call(proclabel, procstatic, first_slot, label) \
		MR_deep_non_call_ac(proclabel, procstatic, first_slot, label)
  #define	MR_deep_non_exit(proclabel, first_slot, label)		\
		MR_deep_non_exit_ac(proclabel, first_slot, label)
  #define	MR_deep_non_redo(proclabel, first_slot, label)		\
		MR_deep_non_redo_ac(proclabel, first_slot, label)
  #define	MR_deep_non_fail(proclabel, first_slot, label)		\
		MR_deep_non_fail_ac(proclabel, first_slot, label)
#else
  #define	MR_deep_det_call(proclabel, procstatic, first_slot, label) \
		MR_deep_det_call_sr(proclabel, procstatic, first_slot, label)
  #define	MR_deep_det_exit(proclabel, first_slot, label)		\
		MR_deep_det_exit_sr(proclabel, first_slot, label)

  #define	MR_deep_semi_call(proclabel, procstatic, first_slot, label) \
		MR_deep_semi_call_sr(proclabel, procstatic, first_slot, label)
  #define	MR_deep_semi_exit(proclabel, first_slot, label)		\
		MR_deep_semi_exit_sr(proclabel, first_slot, label)
  #define	MR_deep_semi_fail(proclabel, first_slot, label)		\
		MR_deep_semi_fail_sr(proclabel, first_slot, label)

  #define	MR_deep_non_call(proclabel, procstatic, first_slot, label) \
		MR_deep_non_call_sr(proclabel, procstatic, first_slot, label)
  #define	MR_deep_non_exit(proclabel, first_slot, label)		\
		MR_deep_non_exit_sr(proclabel, first_slot, label)
  #define	MR_deep_non_redo(proclabel, first_slot, label)		\
		MR_deep_non_redo_sr(proclabel, first_slot, label)
  #define	MR_deep_non_fail(proclabel, first_slot, label)		\
		MR_deep_non_fail_sr(proclabel, first_slot, label)
#endif

/*****************************************************************************/

/*
** MR_deep_prepare_normal_call and MR_deep_prepare_ho_call are for use
** only from procedures that live on the det stack. For procedures that live
** on the nondet stack, you will need variants that get MiddleCSD from the
** appropriate framevar.
*/

#define	MR_deep_prepare_normal_call(proclabel, first_slot, label, site)	\
	MR_r1 = site;							\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__prepare_for_normal_call_1_0),\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#define	MR_deep_prepare_ho_call(proclabel, first_slot, label, site, closure) \
	MR_r1 = site;							\
	MR_r2 = closure;						\
	MR_call_localret(MR_ENTRY(					\
		mercury__profiling_builtin__prepare_for_ho_call_2_0),	\
		label, MR_ENTRY(proclabel));				\
    MR_define_label(label);						\
	MR_update_prof_current_proc(MR_LABEL(proclabel))

#endif	/* MERCURY_DEEP_PROFILING_HAND_H */
