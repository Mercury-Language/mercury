#ifndef	LABEL_H
#define	LABEL_H

#include	"list.h"

typedef struct s_label
{
	const char	*e_name;   /* name of the procedure	     */
	Code		*e_addr;   /* address of the code	     */
} Label;

/*
** Taking the address of a label can inhibit gcc's optimization,
** because it assumes that anything can jump there.
** Therefore we want to do it only if we're debugging.
** Or if we need the label address for profiling.
*/

#if defined(SPEED) && !defined(DEBUG_GOTOS)
#define	make_label(n, a)	/* nothing */
#else
#define	make_label(n, a)	make_entry((n),(a))
#endif

#if defined(SPEED) && !defined(DEBUG_GOTOS) && !defined(USE_PROFILING)
#define make_local(n, a)	/* nothing */
#else 
#define make_local(n, a)	make_entry((n),(a))
#endif

#define make_entry(n, a)	makeentry(n, a)

extern	void	init_entries(void);
extern	Label	*makeentry(const char *name, Code *addr);
extern	Label	*lookup_label_name(const char *name);
extern	Label	*lookup_label_addr(const Code *addr);
extern	List	*get_all_labels(void);

#endif
