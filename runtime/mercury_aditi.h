/*
** Copyright (C) 2003-2004 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/*
** mercury_aditi.h - definitions for interfacing with Aditi.
*/

#ifndef	MERCURY_ADITI_H
#define	MERCURY_ADITI_H

#include "mercury_stack_layout.h"	/* for MR_Determinism */
#include "mercury_type_info.h"		/* for MR_TypeInfo */

/*
** MR_Aditi_Proc_Info_Struct describes the top-down procedures created
** for complex join conditions in bottom-up Aditi procedures.
** These procedures will only ever have two arguments -- an
** input and an output, both of which will be tuples.
*/
typedef struct MR_Aditi_Proc_Info_Struct {
	MR_ProcAddr	MR_aditi_proc_addr;
	MR_String	MR_aditi_proc_name;
	MR_TypeInfo	MR_aditi_input_type_info;
	MR_TypeInfo	MR_aditi_output_type_info;
	MR_Determinism	MR_aditi_proc_detism;
} MR_Aditi_Proc_Info;

#ifndef MR_STATIC_CODE_ADDRESSES

  #define MR_INIT_ADITI_PROC_INFO(api, addr) \
		do { (api).MR_aditi_proc_addr = (addr); } while (0)

#else /* MR_STATIC_CODE_ADDRESSES */

  #define MR_INIT_ADITI_PROC_INFO(api, addr) \
  		do { } while (0)

#endif /* MR_STATIC_CODE_ADDRESSES */

#endif	/* not MERCURY_ADITI_H */
