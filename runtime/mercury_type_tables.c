/*
** Copyright (C) 2000,2002 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** This module manages tables that list the definitions of the types (and
** eventually type class instances) defined in the program.
**
** The sizes of these tables can vary by several orders of magnitude,
** so using a fixed size hash table would not be a good idea. This is why
** we build on the implementation of expandable hash tables in the
** mercury_tabling module.
*/

#ifndef MR_HIGHLEVEL_CODE
  #include "mercury_imp.h"
#endif
#include "mercury_type_info.h"
#include "mercury_type_tables.h"
#include "mercury_tabling.h"
#include "mercury_misc.h"
#include <string.h>

static	MR_TableNode	MR_type_ctor_table = { 0 };

#define	names_match(tc1, module_name, type_name, arity)			\
	( MR_streq(MR_type_ctor_name(tc1), type_name)			\
	&& MR_streq(MR_type_ctor_module_name(tc1), module_name)		\
	&& tc1->MR_type_ctor_arity == arity)

#define	names_match_tc(tc1, tc2)					\
	( MR_streq(MR_type_ctor_name(tc1), MR_type_ctor_name(tc2))	\
	&& MR_streq(MR_type_ctor_module_name(tc1),			\
		MR_type_ctor_module_name(tc2))				\
	&& tc1->MR_type_ctor_arity == tc2->MR_type_ctor_arity )

void
MR_register_type_ctor_info(MR_TypeCtorInfo type_ctor_info)
{
	MR_TrieNode	slot;
	MR_Dlist	*element_ptr;
	MR_TypeCtorInfo	cur_type_ctor_info;

	slot = MR_string_hash_lookup_or_add(&MR_type_ctor_table,
			MR_type_ctor_name(type_ctor_info));

	MR_for_dlist (element_ptr, slot->MR_type_table) {
		cur_type_ctor_info =
			(MR_TypeCtorInfo) MR_dlist_data(element_ptr);

		if (names_match_tc(type_ctor_info, cur_type_ctor_info)) {
			if (cur_type_ctor_info == type_ctor_info) {
				/* type_ctor_info has been registered before */
				return;
			} else {
				MR_fatal_error("MR_register_type_ctor_info: "
					"ambiguous type ctor");
			}
		}
	}

	slot->MR_type_table = MR_dlist_addhead(slot->MR_type_table,
		type_ctor_info);
}

MR_TypeCtorInfo
MR_lookup_type_ctor_info(const char *module_name, const char *type_name,
	int arity)
{
	MR_TrieNode	slot;
	MR_Dlist	*element_ptr;
	MR_TypeCtorInfo	cur_type_ctor_info;

	slot = MR_string_hash_lookup_or_add(&MR_type_ctor_table, type_name);

	MR_for_dlist (element_ptr, slot->MR_type_table) {
		cur_type_ctor_info =
			(MR_TypeCtorInfo) MR_dlist_data(element_ptr);

		if (names_match(cur_type_ctor_info, module_name, type_name,
			arity))
		{
			return cur_type_ctor_info;
		}
	}

	return NULL;
}
