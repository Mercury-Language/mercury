%{
/*
**	Scanner for the interface to the Ptah interpeter
*/

static	char
rcs_id[] = "$Header: /srv/scratch/dev/togit/repository/mercury/runtime/Attic/iface_s.l,v 1.1 1993-11-25 05:22:37 zs Exp $";

#include	"imp.h"
#include	"table.h"
#include	"iface_g.h"

struct	s_kwentry
{
	char	*kw_name;
	int	kw_val;
} Kwentry;

static	Table	kwtable;

#define	mkkwentry(name, val)	do {					\
					reg	Kwentry	entry;		\
									\
					entry = make(Kwentry);		\
					entry->kw_name = new_name(name);\
					entry->kw_val  = val;		\
					insert_table(kwtable, entry);	\
				} while (0)
%}

%start	edge middle

digit	[0-9]
alpha	[a-zA-Z_]
alnum	[a-zA-Z_0-9]

%%

"("			{	return LPAREN;		}
")"			{	return RPAREN;		}

"0x"{digit}+		{
				sscanf(yytext, "%x", &yylval.Unum);
				return NUM;
			}

{digit}+		{
				sscanf(yytext, "%d", &yylval.Unum);
				return NUM;
			}

\"([^"]|\\\")*\"	{
				/* strip off quotes */
				yytext[yyleng-1] = '\0';
				yylval.Ustr = new_name(yytext+1);
				return STR;
			}

{alpha}{alnum}*		{
				yylval.Ustr = new_name(yytext);
				kwentry = lookup_table(keywords, yylval.Ustr);
				if (kwentry != (Entry *) NULL)
					return kwentry->kw_val;
				else
					return ID;
			}

"\n"			{	return EOF;		}

%%

int
yywrap(void)
{
	return 1;
}

void
yykwinit(void)
{
	init_table(kwtable);
	mkkwentry("reset", RESET);
	mkkwentry("call", CALL);
	mkkwentry("redo", REDO);
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
