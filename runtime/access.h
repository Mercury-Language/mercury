/* more access functions, defined in access.c */

#ifndef		LIST_H
#include	"list.h"
#endif

extern	void	reset(void);
extern	void	help(void);
extern	Word	get_reg(int);
extern	Word	set_reg(int, Word);
extern	Word	get_mem(Word *);
extern	Word	set_mem(Word *, Word);
extern	Word	createn(List *);
extern	int	getflexline(const char *, FILE *, char **, int *);
