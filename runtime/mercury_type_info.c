/*
INIT mercury_sys_init_type_info
ENDINIT
*/
/*
** Copyright (C) 1995-1997 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** type_info.c -
**	Definitions for type_infos, type_layouts, and
**	type_functors tables needed by the Mercury runtime system..
*/

#include "mercury_imp.h"
#include "mercury_type_info.h"

/*---------------------------------------------------------------------------*/

	/* base_type_layout for `pred' */
	/* (this is used for all higher-order types) */

const struct mercury_data___base_type_layout_pred_0_struct {
	TYPE_LAYOUT_FIELDS
} mercury_data___base_type_layout_pred_0 = {
	make_typelayout_for_all_tags(TYPELAYOUT_CONST_TAG, 
		mkbody(TYPELAYOUT_PREDICATE_VALUE))
};

	/* base_type_functors for `pred' */
	/* (this is used for all higher-order types) */

const struct mercury_data___base_type_functors_pred_0_struct {
	Integer f1;
} mercury_data___base_type_functors_pred_0 = {
	MR_TYPEFUNCTORS_SPECIAL
};


	/* 
	** base_type_info for `func' 
	** (this is used for all higher-order func types) 
	**
	** Note: we use the special predicates, functors and layout for
	** `pred'.
	*/

Declare_entry(mercury__builtin_unify_pred_2_0);
Declare_entry(mercury__builtin_index_pred_2_0);
Declare_entry(mercury__builtin_compare_pred_3_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_func_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_func_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_pred_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_pred_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_pred_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_pred_0,
	(const Word *) & mercury_data___base_type_functors_pred_0,
	(const Word *) string_const("func", 4)
#endif
};

	/*
	** base_type_info for `pred' 
	** (this is used for all higher-order pred types) 
	*/

Declare_entry(mercury__builtin_unify_pred_2_0);
Declare_entry(mercury__builtin_index_pred_2_0);
Declare_entry(mercury__builtin_compare_pred_3_0);
MR_STATIC_CODE_CONST struct mercury_data___base_type_info_pred_0_struct {
	Integer f1;
	Code *f2;
	Code *f3;
	Code *f4;
#ifdef USE_TYPE_TO_TERM
	Code *f5;
	Code *f6;
#endif
#ifdef USE_TYPE_LAYOUT
	const Word *f7;
	const Word *f8;
	const Word *f9;
#endif
} mercury_data___base_type_info_pred_0 = {
	((Integer) 0),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_unify_pred_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_index_pred_2_0)),
	MR_MAYBE_STATIC_CODE(ENTRY(mercury__builtin_compare_pred_3_0)),
#ifdef  USE_TYPE_LAYOUT
	(const Word *) & mercury_data___base_type_layout_pred_0,
	(const Word *) & mercury_data___base_type_functors_pred_0,
	(const Word *) string_const("pred", 4)
#endif
};

Define_extern_entry(mercury__builtin_unify_pred_2_0);
Define_extern_entry(mercury__builtin_index_pred_2_0);
Define_extern_entry(mercury__builtin_compare_pred_3_0);
Declare_label(mercury__builtin_compare_pred_3_0_i4);

Define_extern_entry(mercury__builtin_unify_pred_2_0);

BEGIN_MODULE(mercury__builtin_unify_pred_module)
	init_entry(mercury__builtin_unify_pred_2_0);
BEGIN_CODE

/* code for predicate 'builtin_unify_pred'/2 in mode 0 */
Define_entry(mercury__builtin_unify_pred_2_0);
	incr_sp_push_msg(2, "mercury_builtin:builtin_unify_pred");
	fatal_error("attempted unification of higher-order terms");
END_MODULE


Define_extern_entry(mercury__builtin_index_pred_2_0);

BEGIN_MODULE(mercury__builtin_index_pred_module)
	init_entry(mercury__builtin_index_pred_2_0);
BEGIN_CODE

/* code for predicate 'builtin_index_pred'/2 in mode 0 */
Define_entry(mercury__builtin_index_pred_2_0);
	r1 = (Integer) -1;
	proceed();
END_MODULE

Define_extern_entry(mercury__builtin_compare_pred_3_0);

BEGIN_MODULE(mercury__builtin_compare_pred_module)
	init_entry(mercury__builtin_compare_pred_3_0);
BEGIN_CODE

/* code for predicate 'builtin_compare_pred'/3 in mode 0 */
Define_entry(mercury__builtin_compare_pred_3_0);
	incr_sp_push_msg(2, "mercury_builtin:builtin_compare_pred");
	fatal_error("attempted comparison of higher-order terms");
END_MODULE

/*---------------------------------------------------------------------------*/
void mercury_sys_init_type_info(void); /* suppress gcc warning */
void mercury_sys_init_type_info(void) {
	mercury__builtin_unify_pred_module();
	mercury__builtin_index_pred_module();
	mercury__builtin_compare_pred_module();
}
