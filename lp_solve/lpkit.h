/*
  Main header file of the LP_SOLVE toolkit.
  
  By Jeroen Dirks, 21-2-95

  include this file in your program and link with liblps.a
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#define FAIL -1

#define FALSE   0
#define TRUE    1

#define DEFNUMINV 50

/* solve status values */
#define OPTIMAL     	0
#define MILP_FAIL   	1
#define INFEASIBLE  	2
#define UNBOUNDED   	3
#define FAILURE     	4
#define RUNNING     	5

/* lag_solve extra status values */
#define FEAS_FOUND   	6
#define NO_FEAS_FOUND 	7
#define BREAK_BB	8

#define FIRST_NI	0
#define RAND_NI		1

#define LE      0
#define EQ      1
#define GE      2
#define OF      3

#define	my_abs(x)	((x) < 0 ? -(x) : (x))
#define my_min(x, y)    ((x) < (y) ? (x) : (y))
#define my_max(x, y)    ((x) > (y) ? (x) : (y))

#define MAX_WARN_COUNT 20

#ifdef CHECK
#define my_round(val, eps) { \
	REAL absv; \
        absv = ((val) < 0 ? -(val) : (val)); \
        if(absv < (eps)) \
          val = 0; \
	if(Warn_count < MAX_WARN_COUNT) \
	  { \
	    if(absv > 0.5 * (eps) && absv < 2 * (eps)) \
	      { \
		Warn_count++; \
		fprintf(stderr, \
			"Warning Value close to epsilon V: %e E: %e\n", \
			(double)absv, (double)(eps)); \
		if(Warn_count == MAX_WARN_COUNT) \
		  { \
		    fprintf(stderr, \
			    "*** Surpressing further rounding warnings\n"); \
		  } \
	      } \
	  } \
}

#else
#define my_round(val,eps) { \
	if (((val) < 0 ? -(val) : (val)) < (eps)) \
          val = 0; \
}
#endif


#define DEF_INFINITE  1e24 /* limit for dynamic range */
#define DEF_EPSB      5.01e-7 /* for rounding RHS values to 0 determine	
				 infeasibility basis */
#define DEF_EPSEL     1e-8 /* for rounding other values (vectors) to 0 */
#define DEF_EPSD      1e-6 /* for rounding reduced costs to zero */
#define DEF_EPSILON   1e-3 /* to determine if a float value is integer */
 
#define PREJ          1e-3  /* pivot reject (try others first) */

#ifndef REAL /* to allow -DREAL=<float type> while compiling */
#define REAL double
#endif

#define HASHSIZE  10007 /* prime number is better, MB */
#define ETA_START_SIZE 10000 /* start size of array Eta. Realloced if needed */
#define FNAMLEN 64
#define NAMELEN 25
#define MAXSTRL (NAMELEN-1)
#define STD_ROW_NAME_PREFIX "r_"

#define CALLOC(ptr, nr, type) if(!(ptr = (type *) calloc((size_t)(nr), sizeof(type)))) {fprintf(stderr, "calloc failed on line %d of file %s\n", __LINE__, __FILE__); exit(FAIL); }

#define MALLOC(ptr, nr, type) if(!(ptr = (type *) malloc((size_t)((nr) * sizeof(type))))) {fprintf(stderr, "malloc failed on line %d of file %s\n", __LINE__, __FILE__); exit(FAIL); }

#define REALLOC(ptr, nr, type) if(!(ptr = (type *) realloc(ptr,(size_t)(nr)*sizeof(type)))) {fprintf(stderr, "realloc failed on line %d of file %s\n", __LINE__, __FILE__); exit(FAIL); }

#define MALLOCCPY(nptr,optr,nr,type) {MALLOC(nptr, nr, type); memcpy(nptr, optr, (size_t)((nr) * sizeof(type)));}

#define MEMCPY(nptr,optr,nr,type) {memcpy(nptr,optr, (size_t)((nr) * sizeof(type)));}

typedef char nstring[NAMELEN];

typedef struct _matrec
{
  int row_nr;
  REAL value;
} matrec;

typedef struct _column
{
  int            row;
  float          value;
  struct _column *next ;
} column;

typedef struct _constraint_name
{
  char                    name[NAMELEN];
  int                     row;
  struct _constraint_name *next;
} constraint_name;

typedef struct _bound
{
  REAL          upbo;
  REAL          lowbo;
} bound;

typedef struct _tmp_store_struct
{
  nstring name;
  int     row;
  REAL    value;
  REAL    rhs_value;
  short   relat;
} tmp_store_struct;

typedef struct _hashelem
{
  nstring          colname;
  struct _hashelem *next;
  struct _column   *col;
  struct _bound    *bnd;
  int              must_be_int;
} hashelem;

typedef struct _rside /* contains relational operator and rhs value */
{
  REAL          value;
  struct _rside *next;
  short         relat;
} rside;


/* fields indicated with ## may be modified directly */
/* pointers will have there size in the comments */

typedef struct _lprec
{
  nstring   lp_name;		/* the name of the lp */

  short     active;		/*TRUE if the globals point to this structure*/
  short     verbose;            /* ## Verbose flag */
  short     print_duals;        /* ## PrintDuals flag for PrintSolution */
  short     print_sol;          /* ## used in lp_solve */
  short     debug;              /* ## Print B&B information */
  short     print_at_invert;    /* ## Print information at every reinversion */
  short     trace;              /* ## Print information on pivot selection */
  short     anti_degen;		/* ## Do perturbations */

  int	    rows;               /* Nr of constraint rows in the problem */
  int       rows_alloc;      	/* The allocated memory for Rows sized data */
  int       columns;            /* The number of columns (= variables) */
  int       columns_alloc;  
  int       sum;                /* The size of the variables + the slacks */
  int       sum_alloc;

  short     names_used;         /* Flag to indecate if names for rows and
				   columns are used */
  nstring   *row_name;		/* rows_alloc+1 */
  nstring   *col_name;		/* columns_alloc+1 */

 /* Row[0] of the sparce matrix is the objective function */

  int       non_zeros;          /* The number of elements in the sparce matrix*/
  int       mat_alloc;		/* The allocated size for matrix sized 
				   structures */
  matrec    *mat;               /* mat_alloc :The sparse matrix */
  int       *col_end;           /* columns_alloc+1 :Cend[i] is the index of the
		 		   first element after column i.
				   column[i] is stored in elements 
				   col_end[i-1] to col_end[i]-1 */
  int       *col_no;            /* mat_alloc :From Row 1 on, col_no contains the
				   column nr. of the
                                   nonzero elements, row by row */
  short     row_end_valid;	/* true if row_end & col_no are valid */
  int       *row_end;           /* rows_alloc+1 :row_end[i] is the index of the 
				   first element in Colno after row i */
  REAL      *orig_rh;           /* rows_alloc+1 :The RHS after scaling & sign 
				  changing, but before `Bound transformation' */
  REAL      *rh;		/* rows_alloc+1 :As orig_rh, but after Bound 
				   transformation */
  REAL      *rhs;		/* rows_alloc+1 :The RHS of the curent simplex 	
				   tableau */
  short     *must_be_int;       /* sum_alloc+1 :TRUE if variable must be 
				   Integer */
  REAL      *orig_upbo;         /* sum_alloc+1 :Bound before transformations */
  REAL      *orig_lowbo;	/*  "       "                   */
  REAL      *upbo;              /*  "       "  :Upper bound after transformation 
				   & B&B work*/
  REAL      *lowbo;             /*  "       "  :Lower bound after transformation
				   & B&B work */

  short     basis_valid;        /* TRUE is the basis is still valid */
  int       *bas;               /* rows_alloc+1 :The basis column list */
  short     *basis;             /* sum_alloc+1 : basis[i] is TRUE if the column
				   is in the basis */
  short     *lower;             /*  "       "  :TRUE is the variable is at its 
				   lower bound (or in the basis), it is FALSE
				   if the variable is at its upper bound */

  short     eta_valid;          /* TRUE if current Eta structures are valid */
  int       eta_alloc;          /* The allocated memory for Eta */
  int       eta_size;           /* The number of Eta columns */
  int       num_inv;            /* The number of real pivots */
  int       max_num_inv;        /* ## The number of real pivots between 
				   reinvertions */
  REAL      *eta_value;         /* eta_alloc :The Structure containing the
				   values of Eta */
  int       *eta_row_nr;         /*  "     "  :The Structure containing the Row
				   indexes of Eta */
  int       *eta_col_end;       /* rows_alloc + MaxNumInv : eta_col_end[i] is
				   the start index of the next Eta column */

  short	    bb_rule;		/* what rule for selecting B&B variables */

  short     break_at_int;       /* TRUE if stop at first integer better than
                                   break_value */
  REAL      break_value;        

  REAL      obj_bound;          /* ## Objective function bound for speedup of 
				   B&B */
  int       iter;               /* The number of iterations in the simplex
				   solver (LP) */
  int       total_iter;         /* The total number of iterations (B&B) (ILP)*/ 
  int       max_level;          /* The Deepest B&B level of the last solution */
  int	    total_nodes;	/* total number of nodes processed in b&b */
  REAL      *solution;          /* sum_alloc+1 :The Solution of the last LP, 
				   0 = The Optimal Value, 
                                   1..rows The Slacks, 
				   rows+1..sum The Variables */
  REAL      *best_solution;     /*  "       "  :The Best 'Integer' Solution */
  REAL      *duals;             /* rows_alloc+1 :The dual variables of the
				   last LP */
  
  short     maximise;           /* TRUE if the goal is to maximise the 
				   objective function */
  short     floor_first;        /* TRUE if B&B does floor bound first */
  short     *ch_sign;           /* rows_alloc+1 :TRUE if the Row in the matrix
				   has changed sign 
                                   (a`x > b, x>=0) is translated to 
				   s + -a`x = -b with x>=0, s>=0) */ 

  short     scaling_used;	/* TRUE if scaling is used */
  short     columns_scaled;     /* TRUE is the columns are scaled too, Only use
		 		   if all variables are non-integer */
  REAL      *scale;             /* sum_alloc+1 :0..Rows the scaling of the Rows,
				   Rows+1..Sum the scaling of the columns */

  int	    nr_lagrange;	/* Nr. of Langrangian relaxation constraints */
  REAL	    **lag_row;		/* NumLagrange, columns+1:Pointer to pointer of 
				   rows */
  REAL      *lag_rhs;		/* NumLagrange :Pointer to pointer of Rhs */
  REAL      *lambda;		/* NumLagrange :Lambda Values */
  short     *lag_con_type;      /* NumLagrange :TRUE if constraint type EQ */
  REAL      lag_bound;		/* the lagrangian lower bound */

  short     valid;		/* Has this lp pased the 'test' */
  REAL      infinite;           /* ## numercal stuff */
  REAL      epsilon;            /* ## */
  REAL      epsb;               /* ## */
  REAL      epsd;               /* ## */
  REAL      epsel;              /* ## */
} lprec;



/* function interface for the user */

lprec *make_lp(int rows, int columns);
/* create and initialise a lprec structure
   defaults:
   Empty (Rows * Columns) matrix,
   Minimise the objective function
   constraints all type <=
   Upperbounds all Infinite
   no integer variables
   floor first in B&B
   no scaling
   default basis */

lprec *read_lp_file(FILE *input, short verbose, nstring lp_name);
/* create and read an .lp file from input (input must be open) */

void delete_lp(lprec *lp);
/* Remove problem from memory */

lprec *copy_lp(lprec *lp);
/* copy a lp structure */

void set_mat(lprec *lp, int row, int column, REAL value);
/* fill in element (Row,Column) of the matrix
   Row in [0..Rows] and Column in [1..Columns] */

void set_obj_fn(lprec *lp, REAL *row);
/* set the objective function (Row 0) of the matrix */
void str_set_obj_fn(lprec *lp, char *row);
/* The same, but with string input */

void add_constraint(lprec *lp, REAL *row, short constr_type, REAL rh);
/* Add a constraint to the problem,
   row is the constraint row,
   rh is the right hand side,
   constr_type is the type of constraint (LE (<=), GE(>=), EQ(=)) */
void str_add_constraint(lprec *lp, char *row_string ,short constr_type, REAL rh);
/* The same, but with string input */

void del_constraint(lprec *lp,int del_row);
/* Remove constrain nr del_row from the problem */

void add_lag_con(lprec *lp, REAL *row, short con_type, REAL rhs);
/* add a Lagrangian constraint of form Row' x contype Rhs */
void str_add_lag_con(lprec *lp, char *row, short con_type, REAL rhs);
/* The same, but with string input */

void add_column(lprec *lp, REAL *column);
/* Add a Column to the problem */
void str_add_column(lprec *lp, char *col_string);
/* The same, but with string input */

void del_column(lprec *lp, int column);
/* Delete a column */

void set_upbo(lprec *lp, int column, REAL value);
/* Set the upperbound of a variable */

void set_lowbo(lprec *lp, int column, REAL value);
/* Set the lowerbound of a variable */

void set_int(lprec *lp, int column, short must_be_int);
/* Set the type of variable, if must_be_int = TRUE then the variable must be integer */

void set_rh(lprec *lp, int row, REAL value);
/* Set the right hand side of a constraint row */

void set_rh_vec(lprec *lp, REAL *rh);
/* Set the right hand side vector */
void str_set_rh_vec(lprec *lp, char *rh_string);
/* The same, but with string input */

void set_maxim(lprec *lp);
/* maximise the objective function */

void set_minim(lprec *lp);
/* minimise the objective function */

void set_constr_type(lprec *lp, int row, short con_type);
/* Set the type of constraint in row Row (LE, GE, EQ) */

void set_row_name(lprec *lp, int row, nstring new_name);
/* Set the name of a constraint row, make sure that the name has < 25 characters */

void set_col_name(lprec *lp, int column, nstring new_name);
/* Set the name of a varaible column, make sure that the name has < 25 characters */

void auto_scale(lprec *lp);
/* Automatic scaling of the problem */

void unscale(lprec *lp);
/* Remove all scaling from the problem */

int solve(lprec *lp);
/* Solve the problem */

int lag_solve(lprec *lp, REAL start_bound, int num_iter, short verbose);
/* Do NumIter iterations with Lagrangian relaxation constraints */

void reset_basis(lprec *lp);
/* Reset the basis of a problem, can be usefull in case of degeneracy - JD */

REAL mat_elm(lprec *lp, int row, int column);
/* get a single element from the matrix */

void get_row(lprec *lp, int row_nr, REAL *row);
/* fill row with the row row_nr from the problem */

void get_column(lprec *lp, int col_nr, REAL *column);
/* fill column with the column col_nr from the problem */

void get_reduced_costs(lprec *lp, REAL *rc);
/* get the reduced costs vector */

short is_feasible(lprec *lp, REAL *values);
/* returns TRUE if the vector in values is a feasible solution to the lp */

short column_in_lp(lprec *lp, REAL *column);
/* returns TRUE if column is already present in lp. (Does not look at bounds
   and types, only looks at matrix values */

lprec *read_mps(FILE *input, short verbose);
/* read a MPS file */

void write_MPS(lprec *lp, FILE *output);
/* write a MPS file to output */

void write_LP(lprec *lp, FILE *output);
/* write a LP file to output */

void print_lp(lprec *lp);
/* Print the current problem, only usefull in very small (test) problems. 
  Shows the effect of scaling */

void print_solution(lprec *lp);
/* Print the solution to stdout */

void print_duals(lprec *lp);
/* Print the dual variables of the solution */

void print_scales(lprec *lp);
/* If scaling is used, print the scaling factors */




/* functions used internaly by the lp toolkit */
void error(char *format, ...);
void inc_mat_space(lprec *lp, int max_extra);
void inc_row_space(lprec *lp);
void inc_col_space(lprec *lp);
void unscale_columns(lprec *lp);
void set_globals(lprec *lp);
void ftran(int start, int end, REAL *pcol);
void btran(REAL *row);
void invert(void);






