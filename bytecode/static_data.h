
/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: static_data.h,v 1.2 1997-03-25 03:09:15 aet Exp $
*/


#if	! defined(STATIC_DATA_H)
#define	STATIC_DATA_H

/*
 *	XXX: We should also have:
 *		- Strings area to store all embedded strings
 *		in the byte stream. This should have appropriate
 *		connections to the code area, stack and heap so we
 *		can, for instance, look up predicate names from
 *		the code area, and lookup constructor names from
 *		the heap.
 */

/*
 *	XXX: We have an arbitrary maximum size for the table
 *	of labels in a module. This is not good.
 */
#define		MAX_LABELS	1000

typedef struct Label_table {
	Address	labels[MAX_LABELS];	/* array : label -> address */
	short	max_label;
} Label_table;

typedef struct Proc_info {
	Byte		proc_id;
	Determinism	det;
	short		label_count;
	short		temp_count;
	CString		*var_infos; /* Use as an array of CStrings */
	short		var_count;
	Address		proc_entry;
} Proc_info;

typedef struct Pred_info {
	CString		pred_name;
	short		proc_count;
	Proc_info	*proc_tab; /* Use as an array of Proc_infos */
} Pred_info;

typedef struct Module_Info {
	Label_table		label_tab;
	Pred_info		*pred_tab; /* Use as an array of Pred_infos */
	Address			module_entry;
} Module_Info;

#endif	/* STATIC_DATA_H */
