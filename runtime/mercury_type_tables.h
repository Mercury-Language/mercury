/*
** Copyright (C) 2000 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_type_tables.h -
**  This module manages tables that list the definitions of the types and
**  type class instances defined in the program.
*/

#ifndef MERCURY_TYPE_TABLES_H
#define MERCURY_TYPE_TABLES_H

#include "mercury_type_info.h"

/*
** Register the given type_ctor_info in the type table, so that it can be found
** by later calls to MR_lookup_type_ctor_info.
**
** The mercury_<module>_init_type_tables function generated automatically
** by the Mercury compiler for every module should call this function to
** register the type_ctor_infos of all the types defined in that module.
*/

extern	void		MR_register_type_ctor_info(
				MR_TypeCtorInfo type_ctor_info);

/*
** Find out if there is a type named type_name defined in module module_name
** with the given arity. If there is, return its type_ctor_info; if not, return
** NULL.
*/

extern	MR_TypeCtorInfo	MR_lookup_type_ctor_info(const char *module_name,
				const char *type_name, int arity);

#endif /* not MERCURY_TYPE_TABLES */
