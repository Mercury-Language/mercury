
/*
** Copyright (C) 1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
**
** $Id: static_data.h,v 1.5 1997-04-26 05:57:07 fjh Exp $
*/


#ifndef MB_STATIC_DATA_H
#define	MB_STATIC_DATA_H

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
#define		MB_MAX_LABELS	1000

typedef struct MB_Label_table_struct {
	MB_Address	labels[MB_MAX_LABELS];	/* array : label -> address */
	short		max_label;
} MB_Label_table;

typedef struct MB_Proc_info_struct {
	MB_Byte		proc_id;
	MB_Determinism	det;
	short		label_count;
	short		temp_count;
	MB_CString	*var_infos; /* Use as an array of MB_CStrings */
	short		var_count;
	MB_Address	proc_entry;
} MB_Proc_info;

typedef struct MB_Pred_info_struct {
	MB_CString	pred_name;
	short		proc_count;
	MB_Proc_info	*proc_tab; /* Use as an array of MB_Proc_infos */
} MB_Pred_info;

typedef struct MB_Module_Info_struct {
	MB_Label_table		label_tab;
	MB_Pred_info		*pred_tab; /* an array of MB_Pred_infos */
	MB_Address		module_entry;
} MB_Module_Info;

#endif	/* MB_STATIC_DATA_H */
