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

static	Word	*cmdcp;
static	int	cmdtarget;

BEGIN_MODULE(iface_module)
	/* module initialization */
	cmdsize = LINESIZE;
	cmdline = malloc_mtype(char, cmdsize);
BEGIN_CODE

do_iface_input:
	proceed();

do_iface:
	cmdcp = maxcp;
	while ((cmdlen = getflexline(PROMPT, stdin, &cmdline, &cmdsize)) > 0)
	{
		cmdcur = 0;
		if (yyparse())
		{
			printf("parser failed\n");
			continue;
		}

		switch (action)
		{

	case Help:	printf("no help text available yet\n");

	when Print:	printf("%d\n", act_value);

	when Reset:	maxcp = cmdcp;

	when Call:	cmdtarget = whichlabel(act_predname);
			if (cmdtarget < 0)
			{
				printf("predicate %s unknown\n", act_predname);
				continue;
			}

			/* make get_reg/set_reg operate on saved registers */
			restore_registers();
			maxcp = cmdcp;
			mkcp("iface_1", 0, LABEL(iface_fail));
			call(entries[cmdtarget].e_addr, LABEL(iface_succ));

	when Redo:	if (maxcp <= cmdcp)
			{
				printf("no active call to redo\n");
				continue;
			}
			
			restore_registers();
			redo();

	otherwise:	printf("internal iface error\n");
			exit(1);

		}

	iface_succ:
		save_registers();
		printf("success\n");
		continue;

	iface_fail:
		save_registers();
		printf("failure\n");
		continue;
	}

	exit(0);

END_MODULE
