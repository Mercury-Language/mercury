%{
/*
**	Grammar for the interface to the Ptah interpeter
*/

static	char
rcs_id[] = "$Header: /srv/scratch/dev/togit/repository/mercury/runtime/Attic/iface_g.y,v 1.1 1993-11-24 10:56:05 zs Exp $";

#include	"imp.h"

extern	int	yylineno;
extern	char	*yyfile;
%}

%union
{
	int	Uint;
	char	*Ustr;
	Word	Uword;
	List	*Ulist;
}

%token		RESET
%token		CALL
%token		SETREG GETREG SETMEM GETMEM
%token		CREATE PUSH POP
%token		TAG BODY FIELD
%token		LPAREN RPAREN
%token	<Uint>	NUM
%token	<Ustr>	STR
%token	<Ustr>	ID

%type	<Ucast>	expr
%type	<Ulist>	expr_l

%start		cmd

%%

/**********************************************************************/
/*		Entries						      */

cmd	:	RESET
		{
			reset();
		}
	|	CALL ID
		{
			printf("call %s\n", $2);
		}
	|	expr
		{
			printf("%x\n", $1);
		}
	;

expr	:	NUM
		{	$$ = (Word) $1;		}
	|	STR
		{	$$ = (Word) $1;		}
	|	TAG NUM expr
		{	$$ = mkword($2, $3);	}
	|	BODY NUM expr
		{	$$ = body($2, $3);	}
	|	FIELD NUM NUM expr
		{	$$ = field($3, $2, $4);	}
	|	GETREG NUM
		{	$$ = getreg($2);	}
	|	GETMEM expr
		{	$$ = getmem($2);	}
	|	SETREG NUM expr
		{	$$ = setreg($2, $3);	}
	|	SETMEM expr expr
		{	$$ = setmem($2, $3);	}
	|	CREATE expr_l
		{	$$ = create($2);	}
	|	PUSH expr
		{	$$ = push($2);		}
	|	POP
		{	$$ = pop();		}
	|	LPAREN expr RPAREN
		{	$$ = $2;		}
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

yyerror(s)
char	*s;
{
	extern	int	yyleng;
	extern	char	yytext[];
	char		buf[80];

	if (yychar <= 0)
	{
		sprintf(buf, "premature EOF");
		yylineno--;
	}
	or (yytext[0] == '\n' || yytext[0] == '\f')
		sprintf(buf, "%s at end of line", s);
	or (isprint(yytext[0]))
		sprintf(buf, "%s at symbol %s", s, yytext);
	else
		sprintf(buf, "%s at \\%o", s, yytext[0]);
	
	if (cakedebug)
		printf("%s, %d: %s, token %d\n", yyfile, yylineno, buf, yychar);
	else
		printf("%s, %d: %s\n", yyfile, yylineno, buf);
}
