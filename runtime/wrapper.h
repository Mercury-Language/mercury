#ifndef	TEST_HARNESS_H
#define	TEST_HARNESS_H


extern	int	heap_size;
extern	int	detstack_size;
extern	int	nondstack_size;

extern	int	heap_zone_size;
extern	int	detstack_zone_size;
extern	int	nondstack_zone_size;

extern	int	pcache_size;

extern	int	r1val;
extern	int	r2val;
extern	int	r3val;

extern	bool	check_space;

extern	int	repcounter;
extern	char	scratchbuf[];

#endif
