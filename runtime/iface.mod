/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
#include	"imp.h"
#include	"access.h"
#include	"iface.h"

#define	PROMPT		"cmd> "
#define	LINESIZE	80

Action	action;
char	*act_predname;
int	act_value;

char	*cmdline;
int	cmdsize;
int	cmdlen;
int	cmdcur;

Word	*ifacedetstackmin;

static	Word	*cmdfr;
static	int	cmdtarget;

BEGIN_MODULE(iface_module)
	/* module initialization */
	/* XXX this needs to go elsewhere */
	cmdsize = LINESIZE;
	cmdline = make_many(char, cmdsize);
	yykwinit();
BEGIN_CODE

do_iface_input:
	proceed();

do_iface:
	cmdfr = maxfr;
	ifacedetstackmin = sp;

/*
** we use gotos instead of a while loop because we want
** the labels iface_succ and iface_fail to be outside braces
*/

get_cmd:
	cmdlen = getflexline(PROMPT, stdin, &cmdline, &cmdsize);
	/* printf("got len %d, line <%s>\n", cmdlen, cmdline); */
	if (cmdlen <= 0)
	{
		printf("normal exit\n");
		exit(0);
	}

	cmdcur = 0;
	yyreinit();
	if (yyparse())
	{
		printf("parser failed\n");
		GOTO_LABEL(get_cmd);
	}

	switch (action)
	{

case Null:	;

when Help:	printf("no help text available yet\n");

when Print:	printf("%d\n", act_value);

when Reset:	maxfr = cmdfr;

when Call:	cmdtarget = whichlabel(act_predname);
		if (cmdtarget < 0)
		{
			printf("predicate %s unknown\n", act_predname);
			GOTO_LABEL(get_cmd);
		}

		/* make get_reg/set_reg operate on saved registers */
		restore_transient_registers();
		maxfr = cmdfr;
		mkframe("iface_1", 0, LABEL(iface_fail));
		call(entries[cmdtarget].e_addr, LABEL(iface_succ),
			LABEL(do_iface));

when Redo:	if (maxfr <= cmdfr)
		{
			printf("no active call to redo\n");
			GOTO_LABEL(get_cmd);
		}
		
		restore_transient_registers();
		redo();

otherwise:	printf("internal iface error\n");
		exit(1);

	}

	GOTO_LABEL(get_cmd);

iface_succ:
	save_transient_registers();
	printf("success\n");
	GOTO_LABEL(get_cmd);

iface_fail:
	save_transient_registers();
	printf("failure\n");
	GOTO_LABEL(get_cmd);

END_MODULE
