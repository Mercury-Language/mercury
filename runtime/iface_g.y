%{
/*
**	Grammar for the interface to the Ptah interpeter
*/

#ifndef	lint
static	char
rcs_id[] = "$Header: /srv/scratch/dev/togit/repository/mercury/runtime/Attic/iface_g.y,v 1.3 1993-12-02 05:17:15 zs Exp $";
#endif

#include	<ctype.h>
#include	"imp.h"
#include	"iface.h"
#include	"list.h"
#include	"access.h"

extern	Action	action;
extern	char	*act_predname;
extern	int	act_value;

extern	int	yylex(void);
extern	void	yyerror(const char *);
%}

%union
{
	int	Uint;
	char	*Ustr;
	Word	Uword;
	List	*Ulist;
}

%token		RESET HELP
%token		CALL REDO
%token		TAG BODY FIELD
%token		SETREG GETREG SETMEM GETMEM
%token		CREATE PUSH POP
%token		LPAREN RPAREN
%token	<Uint>	NUM
%token	<Ustr>	STR
%token	<Ustr>	ID
%token		GARBAGE

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
	|	/* empty */
		{
			action = Null;
		}
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
		{	$$ = pop();			}
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
		{	$$ = makelist($1);		}
	|	expr_l expr
		{	$$ = addtail($1, $2);		}
	;

%%

void
yyerror(const char *s)
{
	printf("%s\n", s);
}
