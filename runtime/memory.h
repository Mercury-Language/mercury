#ifndef	MEMORY_H
#define	MEMORY_H

#define MAX_VIRTUAL_REG	1024
#define NUM_SPECIAL_REG 5
#define MAX_FAKE_REG	(NUM_SPECIAL_REG + MAX_VIRTUAL_REG)

/* reserve MAX_FAKE_REG virtual regs,
 * numbered from 0 to MAX_FAKE_REG-1 */
extern	Word	fake_reg[MAX_FAKE_REG];
extern	Word	virtual_reg_map[MAX_VIRTUAL_REG+1];

/* these arrays are of size MAX_FAKE_REG */
extern	Word	*saved_regs;
extern	Word 	*num_uses;

/* beginning of allocated areas */
extern	Word	*heap;
extern	Word	*detstack;
extern	Word	*nondstack;

/* beginning of used areas */
extern	Word	*heapmin;
extern	Word	*detstackmin;
extern	Word	*nondstackmin;

/* highest locations actually used */
extern	Word	*heapmax;
extern	Word	*detstackmax;
extern	Word	*nondstackmax;

/* end of allocated areas */
extern	Word	*heapend;
extern	Word	*detstackend;
extern	Word	*nondstackend;

/* beginning of redzones */
extern	caddr_t	heap_zone;
extern	caddr_t	detstack_zone;
extern	caddr_t	nondstack_zone;

extern	int	heap_zone_left;
extern	int	detstack_zone_left;
extern	int	nondstack_zone_left;

extern	void	init_memory(void);

#endif
