#ifndef	ENGINE_H
#define	ENGINE_H

#define	PROGFLAG	0
#define	GOTOFLAG	1
#define	CALLFLAG	2
#define	HEAPFLAG	3
#define	DETSTACKFLAG	4
#define	NONDSTACKFLAG	5
#define	FINALFLAG	6
#define	MEMFLAG		7
#define	DETAILFLAG	8
#define	MAXFLAG		9
/* DETAILFLAG should be the last real flag */

#define	progdebug	debugflag[PROGFLAG]
#define	gotodebug	debugflag[GOTOFLAG]
#define	calldebug	debugflag[CALLFLAG]
#define	heapdebug	debugflag[HEAPFLAG]
#define	detstackdebug	debugflag[DETSTACKFLAG]
#define	nondstackdebug	debugflag[NONDSTACKFLAG]
#define	finaldebug	debugflag[FINALFLAG]
#define	memdebug	debugflag[MEMFLAG]
#define	detaildebug	debugflag[DETAILFLAG]

extern	bool	debugflag[];

extern	void	init_engine(void);
extern	void	call_engine(Code *entry_point);

extern	void	special_labels_module(void); /* unnecessary */

#define	doredo			ENTRY(do_redo)
#define	dofail			ENTRY(do_fail)
#define	doresethpfail		ENTRY(do_reset_hp_fail)
#define	doresetframevar0fail	ENTRY(do_reset_framevar0_fail)
#define	dosucceed		ENTRY(do_succeed)
#define	donotreached		ENTRY(do_not_reached)

extern	EntryPoint	doredo;
extern	EntryPoint	dofail;
extern	EntryPoint	doresethpfail;
extern	EntryPoint	doresetframevar0fail;
extern	EntryPoint	dosucceed;
extern	EntryPoint	donotreached;

#endif
