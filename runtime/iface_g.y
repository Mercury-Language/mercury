%{
/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/
/*
**	Grammar for the interface to the Mercury interpeter
**	(This is unfinished.)
*/

/*
The following line is used by yyrepl:
YYREPL	YY	yy
*/

#include	<ctype.h>
#include	"imp.h"
#include	"iface.h"
#include	"list.h"
#include	"access.h"

extern	Action		action;
extern	const char	*act_predname;
extern	int		act_value;

extern	int	yylex(void);
extern	void	yyerror(const char *);
%}

%union
{
	int		Uint;
	const char	*Ustr;
	Word		Uword;
	List		*Ulist;
}

%token		RESET HELP CALL REDO
%token		DEBUG NODEBUG
%token		DETTOKEN NONDETTOKEN HEAPTOKEN CALLTOKEN
%token		GOTOTOKEN FINALTOKEN DETAILTOKEN ALLTOKEN
%token		PRINTREGS DUMPFRAME DUMPNONDSTACK
%token		TAG BODY FIELD
%token		SETREG GETREG SETMEM GETMEM
%token		CREATE PUSH POP
%token		LPAREN RPAREN
%token	<Uint>	NUM
%token	<Ustr>	STR
%token	<Ustr>	ID
%token		GARBAGE

%type	<Uint>	debug
%type	<Uint>	flag
%type	<Uword>	cmd
%type	<Uword>	expr
%type	<Ulist>	expr_l

%start		line

%%

/**********************************************************************/
/*		Entries						      */

line	:	RESET
		{
			action = Reset;
		}
	|	HELP
		{
			action = Help;
		}
	|	CALL ID
		{
			action = Call;
			act_predname = $2;
		}
	|	REDO
		{
			action = Redo;
		}
	|	cmd
		{
			action = Print;
			act_value = $1;
		}
	|	debug flag
		{
			action = Null;
			if ($2 >= 0)
				debugflag[$2] = $1;
			else
			{
				reg	int	i;

				for (i = 0; i < DETAILFLAG; i++)
					debugflag[i] = $1;
			}
		}
	|	PRINTREGS
		{
			action = Null;
			printregs("");
		}
	|	DUMPFRAME
		{
			action = Null;
			dumpframe(curfr);
		}
	|	DUMPNONDSTACK
		{
			action = Null;
			dumpnondstack();
		}
	|	/* empty */
		{
			action = Null;
		}
	;

debug	:	DEBUG
		{	$$ = 1;				}
	|	NODEBUG
		{	$$ = 0;				}
	;

flag	:	DETTOKEN
		{	$$ = DETSTACKFLAG;		}
	|	NONDETTOKEN
		{	$$ = NONDSTACKFLAG;		}
	|	HEAPTOKEN
		{	$$ = HEAPFLAG;			}
	|	CALLTOKEN
		{	$$ = CALLFLAG;			}
	|	GOTOTOKEN
		{	$$ = CALLFLAG;			}
	|	FINALTOKEN
		{	$$ = FINALFLAG;			}
	|	DETAILTOKEN
		{	$$ = DETAILFLAG;		}
	|	ALLTOKEN
		{	$$ = -1;			}
	;

cmd	:	TAG NUM expr
		{	$$ = mkword($2, $3);		}
	|	BODY NUM expr
		{	$$ = body($2, $3);		}
	|	FIELD NUM NUM expr
		{	$$ = field($3, $2, $4);		}
	|	GETREG NUM
		{	$$ = get_reg($2);		}
	|	GETMEM expr
		{	$$ = get_mem((Word *) $2);	}
	|	SETREG NUM expr
		{	$$ = set_reg($2, $3);		}
	|	SETMEM expr expr
		{	$$ = set_mem((Word *) $2, $3);	}
	|	CREATE expr_l
		{	$$ = createn($2);		}
	|	PUSH expr
		{	push($2); $$ = $2;		}
	|	POP
		{
			$$ = pop();
			if (sp < ifacedetstackmin)
			{
				printf("stack underflow\n");
				push($$);
				$$ = 0;
			}
		}
	;

expr	:	NUM
		{	$$ = (Word) $1;			}
	|	STR
		{	$$ = (Word) $1; /* XXX */	}
	|	LPAREN cmd RPAREN
		{	$$ = $2;			}
	;

/* From here on, the grammar is JUNK				      */
/**********************************************************************/
/*		Lists						      */

expr_l	:	expr
		{	$$ = makelist((void *)$1);		}
	|	expr_l expr
		{	$$ = addtail($1, (void *)$2);		}
	;

%%

void
yyerror(const char *s)
{
	printf("%s\n", s);
}
