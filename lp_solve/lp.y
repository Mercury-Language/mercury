/* ========================================================================= */
/* NAME  : lp.y                                                              */
/* ========================================================================= */


%token VAR CONS SIGN AR_M_OP RE_OP END_C COMMA COLON MINIMISE MAXIMISE


%{
#include "lpkit.h" 
#include "lpglob.h"
#include "read.h"

/* globals */
char Last_var[NAMELEN];
char Constraint_name[NAMELEN];
int Lin_term_count;
REAL f;
int x;
int Sign;
int isign; 	/* internal_sign variable to make sure nothing goes wrong */
		/* with lookahead */
int make_neg;	/* is true after the relational operator is seen in order */
		/* to remember if lin_term stands before or after re_op */
%}



%start inputfile
%%

inputfile	:
{
  init_read();
  isign = 0;
  make_neg = 0;
  Sign = 0;
} 
		  objective_function
		  constraints
                  int_declarations
		;

constraints	: constraint
		| constraints
		  constraint
		;

constraint      : real_constraint
                | VAR COLON 
{
  add_constraint_name(Last_var, Rows);
}
                  real_constraint
                ;


real_constraint	: x_lineair_sum
		  RE_OP
{
  store_re_op();
  make_neg = 1; 
}
		  x_lineair_sum
                  END_C
{
  if(Lin_term_count == 0)
    {
      fprintf(stderr, "WARNING line %d: constraint contains no variables\n",
	      yylineno);
      null_tmp_store();
    }

  if(Lin_term_count  > 1)
    Rows++;

  if(Lin_term_count == 1)
    store_bounds();

  Lin_term_count = 0;
  isign = 0 ;
  make_neg = 0;
  Constraint_name[0] = '\0';
}
		;

int_declarations: /* EMPTY */
                | real_int_decls
                ;

real_int_decls  : int_declaration
                | real_int_decls int_declaration
                ;

int_declaration : int_declarator vars END_C
                ;

int_declarator  : VAR {/* check_decl(yytext);*/}/* check it here !! */
                ;

vars            : VAR {add_int_var((char *)yytext);}
                | vars VAR {add_int_var((char *)yytext);}
                | vars COMMA VAR {add_int_var((char *)yytext);}
                ;

x_lineair_sum	: x_lineair_term 
		| SIGN
{
  isign = Sign; 
}
		  x_lineair_term
		| x_lineair_sum
		  SIGN
{
  isign = Sign; 
}
		  x_lineair_term
		;

x_lineair_term	: lineair_term
		| CONS
{
  if (    (isign || !make_neg)
      && !(isign && !make_neg)) /* but not both! */
    f = -f;
  rhs_store(f);
  isign = 0;
}
		;

lineair_sum	: lineair_term 
		| SIGN
{
  isign = Sign;
}
		  lineair_term
		| lineair_sum
		  SIGN
{
  isign = Sign;
}
		  lineair_term
		;

lineair_term	: VAR
{
  if (    (isign || make_neg)
      && !(isign && make_neg)) /* but not both! */
    var_store(Last_var, Rows, (REAL) - 1);
  else
    var_store(Last_var, Rows, (REAL)1);
  isign = 0;
}
		| CONS 
		  VAR
{
  if (    (isign || make_neg)
      && !(isign && make_neg)) /* but not both! */
    f = -f;
  var_store(Last_var, Rows, f);
  isign = 0;
}
		| CONS 
		  AR_M_OP
		  VAR
{
  if (    (isign || make_neg)
      && !(isign && make_neg)) /* but not both! */
    f = -f;
  var_store(Last_var, Rows, f);
  isign = 0;
}
		;

objective_function:   MAXIMISE real_of
{
  Maximise = TRUE;
}
                    | MINIMISE real_of
{
  Maximise = FALSE;
}
                    | real_of
                ;

real_of:            lineair_sum
		    END_C
{
  Rows++;
  Lin_term_count  =  0;
  isign = 0;
  make_neg = 0;
}
		;
%%
#include "lex.c"
