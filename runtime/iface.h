#ifndef	IFACE_H
#define	IFACE_H

typedef	enum {Null, Reset, Help, Call, Redo, Print} Action;

extern	int	yyparse(void);
extern	void	yykwinit(void);
extern	void	yyreinit(void);

extern	Word	*ifacedetstackmin;

#endif
