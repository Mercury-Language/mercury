/*
 *	$Id: static_data.h,v 1.1 1997-02-13 06:00:17 aet Exp $
 *
 *	Copyright: The University of Melbourne, 1996
 */

#if	! defined(STATIC_DATA_H)
#define	STATIC_DATA_H


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
