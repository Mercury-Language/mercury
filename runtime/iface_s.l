%{
/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
**	Scanner for the interface to the Mercury interpeter
**	(This is unfinished.)
*/


#include	<string.h>
#include	"imp.h"
#include	"table.h"
#include	"iface.h"
#include	"iface_g.h"

extern	char	*cmdline;
extern	int	cmdlen;
extern	int	cmdcur;

#undef	YY_INPUT
#if 1
#define	YY_INPUT(buf,result,max_size)	\
		{ 						\
			reg	int	more;			\
								\
			more = cmdlen - cmdcur;			\
			if (more > max_size)			\
				more = max_size;		\
								\
			strncpy(buf, cmdline+cmdcur, more);	\
			cmdcur += more;				\
			if (more > 0)				\
			{					\
				result = more;			\
				buf[more+1] = '\0';		\
			}					\
			else					\
			{					\
				result = YY_NULL;		\
				buf[0] = '\0';			\
			}					\
								\
			/*					\
			printf("new buf <%s>\n", buf);		\
			printf("more %d, max %d, res %d\n",	\
				more, max_size, result);	\
			*/					\
		}
#else
#define	YY_INPUT(buf,result,max_size)	\
		{ 						\
			if (cmdcur < cmdlen)			\
			{					\
				buf[0] = cmdline[cmdcur];	\
				cmdcur += 1;			\
				result = 1;			\
				/*				\
				printf("new bufchar <%c>\n",	\
					buf[0]);		\
				*/				\
			}					\
			else					\
			{					\
				result = YY_NULL;		\
				/*				\
				printf("no bufchar\n");		\
				*/				\
			}					\
		}
#endif

typedef	struct	s_kwentry
{
	char	*kw_name;
	int	kw_val;
} Kwentry;

extern	void	yykwinit(void);
static	char *	new_name(const char *);

static	const void *	name_key(const void *);
static	bool		name_equal(const void *, const void *);
static	const void *	kw_key(const void *);
static	bool		kw_equal(const void *, const void *);

/* XXX this initialization should be done in init_table() */
static	Table	kw_tab =   { TABLESIZE, NULL, kw_key,   tab_hash, kw_equal   };
static	Table	name_tab = { TABLESIZE, NULL, name_key, tab_hash, name_equal };

#define	mkkwentry(name, val)	do {					\
					reg	Kwentry	*entry;		\
									\
					entry = make(Kwentry);		\
					entry->kw_name = new_name(name);\
					entry->kw_val  = val;		\
					insert_table(kw_tab, entry);	\
				} while (0)

extern	int	yylex(void);

int	yylineno = 0;
%}

digit	[0-9]
alpha	[a-zA-Z_]
alnum	[a-zA-Z_0-9]

%%

"("			{	return LPAREN;		}
")"			{	return RPAREN;		}

"0x"{digit}+		{
				sscanf(&yytext[2], "%x", &yylval.Uint);
				return NUM;
			}

{digit}+		{
				sscanf(yytext, "%d", &yylval.Uint);
				return NUM;
			}

\"([^"]|\\\")*\"	{
				/* strip off quotes */
				yytext[yyleng-1] = '\0';
				yylval.Ustr = new_name(&yytext[1]);
				return STR;
			}

{alpha}{alnum}*		{
				reg	const Kwentry	*kwentry;

				yylval.Ustr = new_name(yytext);
				kwentry = (const Kwentry *) lookup_table(kw_tab, yylval.Ustr);
				if (kwentry != NULL)
					return kwentry->kw_val;
				else
					return ID;
			}

[ \t\n]+		{
				/* do nothing */
			}

.			{
				return GARBAGE;
			}

%%

void
yykwinit(void)
{
	init_table(name_tab);
	init_table(kw_tab);
	mkkwentry("reset", RESET);
	mkkwentry("help", HELP);
	mkkwentry("call", CALL);
	mkkwentry("redo", REDO);
	mkkwentry("debug", DEBUG);
	mkkwentry("nodebug", NODEBUG);
	mkkwentry("det", DETTOKEN);
	mkkwentry("nondet", NONDETTOKEN);
	mkkwentry("heap", HEAPTOKEN);
	mkkwentry("call", CALLTOKEN);
	mkkwentry("goto", GOTOTOKEN);
	mkkwentry("final", FINALTOKEN);
	mkkwentry("detail", DETAILTOKEN);
	mkkwentry("all", ALLTOKEN);
	mkkwentry("printregs", PRINTREGS);
	mkkwentry("dumpframe", DUMPFRAME);
	mkkwentry("dumpnondet", DUMPNONDSTACK);
	mkkwentry("tag", TAG);
	mkkwentry("body", BODY);
	mkkwentry("field", FIELD);
	mkkwentry("setreg", SETREG);
	mkkwentry("getreg", GETREG);
	mkkwentry("setmem", SETMEM);
	mkkwentry("getmem", GETMEM);
	mkkwentry("create", CREATE);
	mkkwentry("push", PUSH);
	mkkwentry("pop", POP);
}

void
yyreinit(void)
{
	yy_init = 1;
}

/*
**	Save the given string in the name table if not already there;
**	return its new address. This address is unique, so comparing
**	two identifiers for equality can be done by comparing their
**	addresses.
*/

static char *
new_name(const char *str)
{
	reg	void 	*old;
	reg	char	*copy;

	if ((old = lookup_table(name_tab, str)) != NULL)
		return (char *) old;

	copy = make_many(char, strlen(str) + 1);
	strcpy(copy, str);
	insert_table(name_tab, copy);

	return copy;
}

static const void *
name_key(const void * entry)
{
	return entry;
}

static bool
name_equal(const void *key1, const void *key2)
{
	return streq((const char *) key1, (const char *) key2);
}

static const void *
kw_key(const void *entry)
{
	return ((const Kwentry *) entry)->kw_name;
}

static bool
kw_equal(const void *key1, const void *key2)
{
	return streq((const char *) key1, (const char *) key2);
}
