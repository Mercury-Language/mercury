/*
   ============================================================================
   NAME : read.c

   PURPOSE : translation of lp-problem and storage in sparse matrix

   SHORT : Subroutines for yacc program to store the input in an intermediate
   data-structure. The yacc and lex programs translate the input.  First the
   problemsize is determined and the date is read into an intermediate
   structure, then readinput fills the sparse matrix.

   USAGE : call yyparse(); to start reading the input.  call readinput(); to
   fill the sparse matrix.
   ============================================================================
   Rows : contains the amount of rows + 1. Rows-1 is the amount of constraints
   (no bounds) Rows also contains the rownr 0 which is the objective function

   Columns : contains the amount of columns (different variable names found in
   the constraints)

   Nonnuls : contains the amount of nonnuls = sum of different entries of all
   columns in the constraints and in the objectfunction

   Hash_tab : contains all columnnames on the first level of the structure the
   row information is kept under each column structure in a linked list (also
   the objective funtion is in this structure) Bound information is also
   stored under under the column name

   First_rside : points to a linked list containing all relational operators
   and the righthandside values of the constraints the linked list is in
   reversed order with respect to the rownumbers
   ============================================================================ */
#include "lpkit.h"
#include "lpglob.h"
#include <string.h>
#include <limits.h>

short            *relat;
int              Verbose;
constraint_name  *First_constraint_name;
hashelem         *Hash_tab[HASHSIZE];
rside            *First_rside;
tmp_store_struct tmp_store;
short            Ignore_decl;

/*
 * errorhandeling routine for yyparse()
 */
void yyerror(char *string)
{
  fprintf(stderr, "PARSING ERROR: %s on line %d, quiting\n", string, yylineno);
  exit(1);
}

void check_decl(char *str)
{
  if(strcmp(str, "int"))
    {
      fprintf(stderr, "Unknown declaration specifier %s on line %d, ignored\n",
	      str, yylineno);
      Ignore_decl = TRUE;
    }
}

/* make a good hash function for any int size */
/* inspired by Aho, Sethi and Ullman, Compilers ..., p436 */
#define HASH_1 sizeof(unsigned int)
#define HASH_2 (sizeof(unsigned int) * 6)
#define HASH_3 (0xF0 << ((sizeof(unsigned int) - 1) * CHAR_BIT))

static int hashval(const char *string)
{
  unsigned int result = 0, tmp;

  for(; *string; string++)
    {
      result = (result << HASH_1) + *string;
      if(tmp = result & HASH_3)
	{
	  /* if any of the most significant bits is on */
	  result ^= tmp >> HASH_2; /* xor them in in a less significant part */
	  result ^= tmp;	/* and reset the most significant bits to 0 */
	}
    }
  return(result % HASHSIZE);
} /* hashval */

/*
 * Returns a pointer to hashelement with name = variable
 * If this hashelement does not exists, gethash() returns a NULL pointer
 */
static hashelem *gethash(char *variable) 
{
  hashelem *h_tab_p;
  for(h_tab_p = Hash_tab[hashval(variable)];
      h_tab_p != NULL;
      h_tab_p = h_tab_p->next)
    if(strcmp(variable, h_tab_p->colname) == 0)
      return(h_tab_p);
  return(h_tab_p);
} /* gethash */

void add_int_var(char *name)
{
  hashelem *hp;
  
  if(Verbose)
    fprintf(stderr, "int: %s\n", name);
  if(!(hp = gethash(name)))
    fprintf(stderr,
	    "Unknown variable %s declared integer on line %d, ignored\n",
	    name, yylineno);
  else if(hp->must_be_int)
    fprintf(stderr, "Variable %s declared integer more than once on line %d\n",
	    name, yylineno);
  else
    hp->must_be_int = TRUE;
}

/*
 * initialisation of hashtable and globals.
 */
void init_read(void)
{
  int i;
  Rows = 0;
  Non_zeros = 0;
  Columns = 0;
  for(i = 0; i<HASHSIZE; Hash_tab[i++] = NULL);
  CALLOC(First_rside, 1, rside);
  First_rside->value = 0;
  /* first row (nr 0) is always the objective function */
  First_rside->relat = OF;
} /* init */


/*
 * searchs in column-list (p is pointer to first element of column-list)
 * for column->row = row.
 * getrow() returns a pointer to this column structure.
 * If not found a NULL-pointer is returned
 */
static column *getrow(column *p,
		      int row)
{
  for(; p != NULL; p = p->next)
    if(p->row == row)
      return(p);
  return(p) ;
} /* getrow */

/*
 * Creates a bound record.
 * Set lowbo = 0 and upbo = Infinite
 *
 */
static bound *create_bound_rec(void)
{
  bound *bp;
  CALLOC(bp, 1, bound);
  bp->upbo = DEF_INFINITE;
  bp->lowbo = 0;
  return(bp);
} /* create_bound_rec */

/*
 * clears the tmp_store variable after all information has been copied
 */
void null_tmp_store(void)
{
  tmp_store.value = 0;
  tmp_store.rhs_value = 0;
}

/*
 * variable : pointer to text array with name of variable
 * row      : the rownumber of the constraint
 * value    : value of matrixelement
 *            A(row, variable).
 * Sign     : (global)  determines the sign of value.
 * store()  : stores value in matrix
 *	      A(row, variable). If A(row, variable) already contains data,
 *	      value is added to the existing value.
 */
static void store(char *variable,
		  int row,
		  REAL value) 
{ 
  hashelem *h_tab_p;
  column *col_p;
  int hv;
  
  if(value == 0)
    {
      fprintf(stderr,
	      "(store) Warning, variable %s has een effective coefficient of 0 on line %d. Ignored.\n",
	      variable, yylineno);
      return;
    }
  
  if((h_tab_p = gethash(variable)) == NULL)
    {
      CALLOC(h_tab_p, 1, hashelem);
      Columns++; /* counter for calloc of final array */
      hv = hashval(variable);
      h_tab_p->next = Hash_tab[hv];
      Hash_tab[hv] = h_tab_p;
      strcpy(h_tab_p->colname, variable);
      CALLOC(h_tab_p->col, 1, column);
      Non_zeros++; /* for calloc of final arrays */
      h_tab_p->col->row = row;
      h_tab_p->col->value = value;
    }
  else
    if((col_p = getrow(h_tab_p->col, row)) == NULL)
      {
	CALLOC(col_p, 1, column);
	Non_zeros++; /* for calloc of final arrays */
	col_p->value = value;
	col_p->row = row;
	col_p->next = h_tab_p->col;
	h_tab_p->col = col_p;
      }
    else
      col_p->value += value;
} /* store */

/*
 * store relational operator given in yylex[0] in the rightside list.
 * Also checks if it constaint was a bound and if so stores it in the
 * boundslist
 */
void store_re_op(void)
{
  short tmp_relat;

  switch(yytext[0]) {
    
  case '=':
    tmp_relat = EQ;
    break;
    
  case '>':
    tmp_relat=GE;
    break;
    
  case '<':
    tmp_relat=LE;
    break;
    
  default:
    fprintf(stderr, "Error: unknown relational operator %s on line %d\n",
	    yytext, yylineno);
    exit(1);
    break;
  }

  if(Lin_term_count > 1) /* it is not a bound */
    First_rside->relat = tmp_relat;
  else /* could be a bound */
    tmp_store.relat = tmp_relat;
} /* save_re_op */

/*
 * store RHS value in the rightside structure
 * if type = true then
 */
void rhs_store(REAL value)
{
  if(Lin_term_count > 1) /* not a bound */
    First_rside->value += value;
  else /* a bound */
    tmp_store.rhs_value += value;
} /* RHS_store */

/*
 * store all data in the right place
 * count the amount of lineair terms in a constraint
 * only store in data-structure if the constraint is not a bound
 */
void var_store(char *var, int row, REAL value)
{
  if(strlen(var) > MAXSTRL)
    {
      fprintf(stderr,
	      "Variable name too long, at most %d characters allowed\n",
	      MAXSTRL);
      exit(1);
    }
  /* also in a bound the same var name can occur more than once. Check for
     this. Don't increment Lin_term_count */
  
  if(Lin_term_count != 1 || strcmp(tmp_store.name, var) != 0)
    Lin_term_count++;

  /* always store objective function with rownr == 0. */
  if(row == 0)
    {
      store(var,  row,  value);
      return;
    }
  
  if(Lin_term_count == 1) /* don't store yet. could be a bound */
    {
      strcpy(tmp_store.name, var);
      tmp_store.row = row;
      tmp_store.value += value;
      return;
    }
  
  if(Lin_term_count == 2) /* now you can also store the first variable */
    {
      rside *rp;

      /* make space for the rhs information */
      CALLOC(rp, 1, rside);
      rp->next = First_rside;
      First_rside = rp;
      First_rside->value = tmp_store.rhs_value;
      First_rside->relat = tmp_store.relat;
      
      if(tmp_store.value != 0)
        store(tmp_store.name, tmp_store.row, tmp_store.value);
      else
	fprintf(stderr,
		"Warning, variable %s has een effective coefficient of 0 on line %d. Ignored.\n",
		tmp_store.name, yylineno);
      
      null_tmp_store();
    }
  
  store(var, row, value);
} /* var_store */



/*
 * store the information in tmp_store because it is a bound
 */
void store_bounds(void)
{
  if(tmp_store.value != 0)
    {
      hashelem *h_tab_p;
      int hv;
      REAL boundvalue;
      
      if((h_tab_p = gethash(tmp_store.name)) == NULL)
        {
          /* a new columnname is found, create an entry in the hashlist */
          CALLOC(h_tab_p, 1, hashelem);
          Columns++;		/* counter for calloc of final array */
	  hv            = hashval(tmp_store.name);
	  h_tab_p->next = Hash_tab[hv];
          Hash_tab[hv]  = h_tab_p;
          strcpy(h_tab_p->colname, tmp_store.name);
          /* create a place to store bounds information */
          h_tab_p->bnd = create_bound_rec();
        }
      else if(h_tab_p->bnd == NULL)
	/* create a place to store bounds information */
	h_tab_p->bnd = create_bound_rec();

      /* else bound_rec already exists */
      
      if(tmp_store.value < 0) /* divide by negative number, */
	/* relational operator may change */
        {
          if(tmp_store.relat == GE)
            tmp_store.relat = LE;
          else if(tmp_store.relat == LE)
            tmp_store.relat = GE;
        }
      /* Check sanity of bound; all variables should be positive */
      boundvalue = tmp_store.rhs_value / tmp_store.value;
      if(   ((tmp_store.relat == EQ) && (boundvalue < 0))
	 || ((tmp_store.relat == LE) && (boundvalue < 0))) /* Error */
	{
	  fprintf(stderr,
		  "Error on line %d: variables must always be non-negative\n",
		  yylineno);
	  exit(1);
	}
      
      if((tmp_store.relat == GE) && (boundvalue <= 0)) /* Warning */
	fprintf(stderr,
		"Warning on line %d: useless bound; variables are always >= 0\n",
		yylineno);
      
      /* bound seems to be sane, add it */
      if((tmp_store.relat == GE) || (tmp_store.relat == EQ))
        {
	  if(h_tab_p->bnd->lowbo <  boundvalue)
	    h_tab_p->bnd->lowbo = boundvalue;
	  else
	    fprintf(stderr, "Ineffective lower bound on line %d, ignored\n",
		    yylineno);
	}
      if((tmp_store.relat == LE) || (tmp_store.relat == EQ))
        {
	  if(h_tab_p->bnd->upbo >  boundvalue)
	    h_tab_p->bnd->upbo  = boundvalue;
	  else
	    fprintf(stderr, "Ineffective upper bound on line %d, ignored\n",
		    yylineno);
	}
      
      /* check for empty range */
      if(h_tab_p->bnd->upbo < h_tab_p->bnd->lowbo)
	{
	  fprintf(stderr,
		  "Error: bound on line %d contradicts earlier bounds, exiting\n",
		  yylineno);
	  exit(1);
	}
    }
  else /* tmp_store.value = 0 ! */
    {
      fprintf(stderr,
	      "Error, variable %s has een effective coefficient of 0 in bound on line %d. Exiting.\n",
	      tmp_store.name, yylineno);
      exit(1);
    }
  
  null_tmp_store();
} /* store_bounds */

void add_constraint_name(char *name, int row)
{
  constraint_name *cnp;

  if(!First_constraint_name)	/* first time only */
    {
      CALLOC(First_constraint_name, 1, constraint_name);
    }
  else
    {
      cnp = First_constraint_name;
      CALLOC(First_constraint_name, 1, constraint_name);
      First_constraint_name->next = cnp;
    }
  strcpy(First_constraint_name->name, name);
  First_constraint_name->row = row;
}

/*
 * transport the data from the intermediate structure to the sparse matrix
 * and free the intermediate structure
 */
void readinput(lprec *lp)
{
  int    i, j, index, nn_ind;
  column *cp,*tcp; /* tcp (temporary cp) points to memory-space to free */
  hashelem *hp,*thp;
  bound *bp;
  rside *rp;
  constraint_name *cnp;
   
  /* fill names with the rownames */
  for(cnp = First_constraint_name; cnp; cnp = cnp->next)
    strcpy(lp->row_name[cnp->row], cnp->name);

  for(i = Rows;i >= 0;i--)
    {
      rp = First_rside;
      relat[i] = rp->relat;
      lp->orig_rh[i] = rp->value;
      First_rside = rp->next;
      free(rp); /* free memory when data has been read */
    }
  
  /* change upperbound to zero if the relational operator is the equal sign */
  for(i = 1; i <= Rows; i++)
    if(relat[i] == EQ)
      lp->orig_upbo[i] = 0;

  for(i = 0; i <= Rows; i++)
    if(strcmp(lp->row_name[i], "")==0)
      sprintf(lp->row_name[i],"r_%06d",i); 
  
  /* start reading the Hash_list structure */
  index = 0;
  nn_ind = 0;

  for(i = 0;i < HASHSIZE;i++)
    {
      hp = Hash_tab[i];
      while(hp != NULL)
        {
	  /* put an index in the cend array when a new name is found */
	  lp->col_end[index++] = nn_ind;
	  
	  /* check if it must be an integer variable */
	  if(hp->must_be_int)
	    {
              lp->must_be_int[Rows + index]=TRUE;
	    }
	  /* check for bound */
	  if(hp->bnd != NULL)
	    {
	      bp = hp->bnd;
              lp->orig_lowbo[Rows+index] = bp->lowbo;
              lp->orig_upbo[Rows+index] = bp->upbo;
              free(bp); /* free memory when data has been read*/
	    }
	  
	  /* copy name of column variable */
	  strcpy(lp->col_name[index], hp->colname);
	  
	  /* put matrix values in sparse matrix */
          cp = hp->col;
          while(cp!=NULL)
            {
              lp->mat[nn_ind].row_nr = cp->row;
	      lp->mat[nn_ind].value = cp->value;
	      nn_ind++;
	      tcp = cp;
	      cp = cp->next;
	      free(tcp); /* free memory when data has been read */
	    }
	  thp = hp;
	  hp = hp->next;
          free(thp); /* free memory when data has been read */
        }
    }
  lp->col_end[index] = nn_ind; 

  /* the following should be replaced by a call to the MPS print routine MB */
  
  if(Verbose)
    {
      printf("\n");
      printf("**********Data read**********\n");
      printf("Rows    : %d\n", Rows);
      printf("Columns : %d\n", Columns);
      printf("Nonnuls : %d\n", Non_zeros);
      printf("NAME          LPPROB\n");
      printf("ROWS\n");
      for(i = 0; i <= Rows; i++)
	{
	  if(relat[i] == LE)
	    printf(" L  ");
	  else if(relat[i] == EQ)
	    printf(" E  ");
	  else if(relat[i] == GE)
	    printf(" G  ");
	  else if(relat[i] == OF)
	    printf(" N  ");
	  printf("%s\n", lp->row_name[i]);
	}
      
      printf("COLUMNS\n");
      j = 0;
      for(i = 0; i < Non_zeros; i++)
	{
	  if(i == lp->col_end[j])
	    j++;
	  printf("    %-8s  %-8s  %g\n", lp->col_name[j],
		 lp->row_name[lp->mat[i].row_nr], (double)lp->mat[i].value);
	}

      printf("RHS\n");
      for(i = 0; i <= Rows; i++)
	{
	  printf("    RHS       %-8s  %g\n", lp->row_name[i],
		 (double)lp->orig_rh[i]);
	}
      
      printf("BOUNDS\n");
      for(i = Rows + 1; i <= Sum; i++)
	{
	  if(lp->orig_upbo[i] < lp->infinite)
	    printf(" UP BND       %-8s  %g\n", lp->col_name[i - Rows],
		   (double)lp->orig_upbo[i]);
	  if(lp->orig_lowbo[i] > 0)
	    printf(" LO BND       %-8s  %g\n", lp->col_name[i - Rows],
		   (double)lp->orig_lowbo[i]);
	}
      
      printf("ENDATA\n");
    }
} /* readinput */

lprec *read_lp_file(FILE *input, short verbose, nstring lp_name)
{
  lprec *lp;
  int i;
  Verbose = verbose; 

  if(Lp != NULL)
    {
      Lp->active = FALSE;
      Lp = NULL;
    }

  yyin = input;
  Maximise=TRUE;  
  yyparse();
  fclose(yyin);  
  
  CALLOC(lp, 1, lprec);

  Rows--;
  Sum = Rows + Columns;

  strcpy(lp->lp_name, lp_name);

  lp->active = FALSE;
  lp->verbose = FALSE;
  lp->print_duals = FALSE;
  lp->print_sol = FALSE;
  lp->debug = FALSE;
  lp->print_at_invert = FALSE;
  lp->trace = FALSE;

  lp->rows = Rows;
  lp->columns = Columns;
  lp->sum = Rows + Columns;
  lp->rows_alloc = Rows;
  lp->columns_alloc = Columns;
  lp->sum_alloc = lp->sum;

  CALLOC(lp->row_name, lp->rows + 1, nstring);
  CALLOC(lp->col_name, lp->columns + 1, nstring);
  lp->names_used = TRUE;

  lp->obj_bound = DEF_INFINITE;
  lp->bb_rule = FIRST_NI;
  lp->break_at_int = FALSE;
  lp->infinite = DEF_INFINITE;
  lp->epsilon = DEF_EPSILON;
  lp->epsb = DEF_EPSB;
  lp->epsd = DEF_EPSD;
  lp->epsel = DEF_EPSEL;
  lp->non_zeros = Non_zeros;
  lp->mat_alloc = Non_zeros;
  lp->row_end_valid = FALSE;
  MALLOC(lp->mat, Non_zeros, matrec);
  CALLOC(lp->col_no, Non_zeros, int);
  CALLOC(lp->col_end, Columns + 1, int);
  CALLOC(lp->row_end, Rows + 1, int);
  CALLOC(lp->orig_rh, Rows + 1, REAL);
  CALLOC(lp->rh, Rows + 1, REAL);
  CALLOC(lp->rhs, Rows + 1, REAL);
  CALLOC(lp->must_be_int, Sum + 1, short);
  MALLOC(lp->orig_upbo, Sum + 1, REAL);
  CALLOC(lp->upbo, Sum + 1, REAL);
  CALLOC(lp->orig_lowbo, Sum + 1, REAL);
  CALLOC(lp->lowbo, Sum + 1, REAL);
  for(i = 0; i <= Sum; i++)
    {
      lp->orig_upbo[i] = lp->infinite;
      lp->orig_lowbo[i] = 0;
    }
  lp->basis_valid = TRUE;
  CALLOC(lp->bas, Rows+1, int);
  CALLOC(lp->basis, Sum + 1, short);
  CALLOC(lp->lower, Sum + 1, short);
  for(i = 0; i <= Rows; i++)
    {
      lp->bas[i] = i;
      lp->basis[i] = TRUE;
    }
  for(i = Rows + 1; i <= Sum; i++)
    lp->basis[i] = FALSE;
  for(i = 0 ; i <= Sum; i++)
    lp->lower[i] = TRUE;
 
  lp->eta_valid = TRUE;
  lp->eta_size = 0;
  lp->eta_alloc = 10000;
  lp->max_num_inv=DEFNUMINV;

  CALLOC(lp->eta_value, 10000, REAL);
  CALLOC(lp->eta_row_nr, 10000, int);
  CALLOC(lp->eta_col_end, Rows + lp->max_num_inv + 1, int);

  lp->iter = 0;
  lp->total_iter = 0;
  CALLOC(lp->solution, Sum + 1, REAL);
  CALLOC(lp->best_solution, Sum + 1, REAL);
  CALLOC(lp->duals, Rows + 1, REAL);

  lp->maximise = FALSE;
  lp->floor_first = TRUE;

  lp->scaling_used = FALSE;

  CALLOC(lp->ch_sign, Rows + 1, short);

  for(i = 0; i <= Rows; i++)
    lp->ch_sign[i] = FALSE;

  lp->valid = FALSE; 

  CALLOC(relat, Rows+1, short);
  readinput(lp);
  if(Maximise)
    set_maxim(lp);
  for(i = 1; i <= Rows; i++)
    set_constr_type(lp, i, relat[i]);
  free(relat);
  return(lp); 
}
