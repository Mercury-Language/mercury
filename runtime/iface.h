/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#ifndef	IFACE_H
#define	IFACE_H

typedef	enum {Null, Reset, Help, Call, Redo, Print} Action;

extern	int	yyparse(void);
extern	void	yykwinit(void);
extern	void	yyreinit(void);

extern	Word	*ifacedetstackmin;

#endif
