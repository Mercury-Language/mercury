/*
** Copyright (C) 1994-1998 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_label.h defines the interface to the label table, which is a pair of
** hash tables, one mapping from procedure names and the other from
** addresses to label information.
** The label information includes the name, address of the code, and
** layout information for that label.
*/

#ifndef	MERCURY_LABEL_H
#define	MERCURY_LABEL_H

#include "mercury_types.h"	/* for `Code *' */
#include "mercury_dlist.h"		/* for `List' */

typedef struct s_label {
	const char	*e_name;   /* name of the procedure	     */
	Code		*e_addr;   /* address of the code	     */
	Word		*e_layout; /* layout info for the label      */
} Label;

extern	void	do_init_entries(void);
extern	Label	*insert_entry(const char *name, Code *addr,
			Word * entry_layout_info);
extern	Label	*lookup_label_name(const char *name);
extern	Label	*lookup_label_addr(const Code *addr);
extern	List	*get_all_labels(void);

extern  int 	entry_table_size;
	/* expected number of entries in the table */
	/* we allocate 8 bytes per entry */

#endif /* not MERCURY_LABEL_H */
