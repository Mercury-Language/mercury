#ifndef	AUX_H
#define	AUX_H

extern	void	mkframe_msg(void);
extern	void	modframe_msg(void);
extern	void	succeed_msg(void);
extern	void	succeeddiscard_msg(void);
extern	void	fail_msg(void);
extern	void	redo_msg(void);
extern	void	call_msg(const Code *proc, const Code *succcont);
extern	void	tailcall_msg(const Code *proc);
extern	void	proceed_msg(void);
extern	void	cr1_msg(Word val0, const Word *addr);
extern	void	cr2_msg(Word val0, Word val1, const Word *addr);
extern	void	incr_hp_msg(Word val, const Word *addr);
extern	void	incr_sp_msg(Word val, const Word *addr);
extern	void	decr_sp_msg(Word val, const Word *addr);
extern	void	push_msg(Word val, const Word *addr);
extern	void	pop_msg(Word val, const Word *addr);
extern	void	goto_msg(const Code *addr);

extern	void	printint(Word n);
extern	void	printstring(const char *s);
extern	void	printheap(const Word *h);
extern	void	printdetstack(const Word *s);
extern	void	printnondstack(const Word *s);
extern	void	dumpframe(const Word *);
extern	void	dumpnondstack(void);
extern	void	printlist(Word p);
extern	void	printlabel(const Code *w);
extern	void	printframe(const char *);
extern	void	printregs(const char *msg);

extern	Word	do_mklist(int start, int len);

#if __GNUC__
	#define NO_RETURN __attribute__((noreturn))
#else
	#define NO_RETURN
#endif
extern	void	fatal_error(const char *msg) NO_RETURN;

#endif
