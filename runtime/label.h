#ifndef	LABEL_H
#define	LABEL_H

#ifndef	LIST_H
#include	"list.h"
#endif

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
#define	makelabel(n, a)	/* nothing */
#else
#define	makelabel(n, a)	makeentry((n),(a))
#endif

#if defined(USE_PROFILING) || defined(DEBUG_GOTOS)
#define makelocalentry(n, a)	makeentry((n),(a))
#else 
#define makelocalentry(n, a)	/* nothing */
#endif

extern	void	init_entries(void);
extern	Label	*makeentry(const char *name, Code *addr);
extern	Label	*lookup_label_name(const char *name);
extern	Label	*lookup_label_addr(const Code *addr);
extern	List	*get_all_labels(void);

#endif
