#include "lpkit.h"
#include "lpglob.h"
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

/* Globals */
lprec   *Lp=NULL; /* pointer to active problem */
int     Rows;
int     Columns;
int     Sum;
int     Non_zeros;
int     Level;
matrec  *Mat;
int     *Col_no;
int     *Col_end;
int     *Row_end;
REAL    *Orig_rh;
REAL    *Rh;
REAL    *Rhs;
short   *Must_be_int;
REAL    *Orig_upbo;
REAL    *Orig_lowbo;
REAL    *Upbo;
REAL    *Lowbo;
int     *Bas;
short   *Basis;
short   *Lower;
int     Eta_alloc; 
int     Eta_size;           
REAL    *Eta_value;
int     *Eta_row_nr;
int     *Eta_col_end;
int     Num_inv;
REAL    *Solution;
REAL    *Best_solution;
REAL    Infinite;
REAL    Epsilon;
REAL    Epsb;
REAL    Epsd;
REAL    Epsel;

REAL	TREJ;
REAL	TINV;

short   Maximise;
short   Floor_first;
REAL    Extrad;

int     Warn_count; /* used in CHECK version of rounding macro */

void error(char *format, ...)
{
  va_list ap;
  va_start(ap, format);
  vfprintf(stderr, format, ap);
  fputc('\n', stderr);
  va_end(ap);
  exit(FAIL);
}

lprec *make_lp(int rows, int columns)
{
  lprec *newlp;
  int i, sum;  

  sum=rows+columns;
  if(rows < 0 || columns < 0)
    error("rows < 0 or columns < 0");
  CALLOC(newlp, 1, lprec);

  strcpy(newlp->lp_name, "unnamed");
  newlp->active=FALSE;
  newlp->verbose=FALSE;
  newlp->print_duals=FALSE;
  newlp->print_sol=FALSE;
  newlp->debug=FALSE;
  newlp->print_at_invert=FALSE;
  newlp->trace=FALSE;

  newlp->rows=rows;
  newlp->columns=columns;
  newlp->sum=sum;
  newlp->rows_alloc=rows;
  newlp->columns_alloc=columns;
  newlp->sum_alloc=sum;
  newlp->names_used=FALSE;

  newlp->obj_bound=DEF_INFINITE;
  newlp->infinite=DEF_INFINITE;
  newlp->epsilon=DEF_EPSILON;
  newlp->epsb=DEF_EPSB;
  newlp->epsd=DEF_EPSD;
  newlp->epsel=DEF_EPSEL;
  newlp->non_zeros=0;
  newlp->mat_alloc=1;
  CALLOC(newlp->mat, newlp->mat_alloc, matrec);
  CALLOC(newlp->col_no, newlp->mat_alloc, int);
  CALLOC(newlp->col_end, columns + 1, int);
  CALLOC(newlp->row_end, rows + 1, int);
  newlp->row_end_valid=FALSE;
  CALLOC(newlp->orig_rh, rows + 1, REAL);
  CALLOC(newlp->rh, rows + 1, REAL);
  CALLOC(newlp->rhs, rows + 1, REAL);
  CALLOC(newlp->must_be_int, sum + 1, short);
  for(i = 0; i <= sum; i++)
    newlp->must_be_int[i]=FALSE;
  CALLOC(newlp->orig_upbo, sum + 1, REAL);
  for(i = 0; i <= sum; i++)
    newlp->orig_upbo[i]=newlp->infinite;
  CALLOC(newlp->upbo, sum + 1, REAL);
  CALLOC(newlp->orig_lowbo, sum + 1, REAL);
  CALLOC(newlp->lowbo, sum + 1, REAL);

  newlp->basis_valid=TRUE;
  CALLOC(newlp->bas, rows+1, int);
  CALLOC(newlp->basis, sum + 1, short);
  CALLOC(newlp->lower, sum + 1, short);
  for(i = 0; i <= rows; i++)
    {
      newlp->bas[i]=i;
      newlp->basis[i]=TRUE;
    }
  for(i = rows + 1; i <= sum; i++)
    newlp->basis[i]=FALSE;
  for(i = 0 ; i <= sum; i++)
    newlp->lower[i]=TRUE;
 
  newlp->eta_valid=TRUE;
  newlp->eta_size=0;
  newlp->eta_alloc=10000;
  newlp->max_num_inv=DEFNUMINV;

  newlp->nr_lagrange=0;

  CALLOC(newlp->eta_value, newlp->eta_alloc, REAL);
  CALLOC(newlp->eta_row_nr, newlp->eta_alloc, int);
  CALLOC(newlp->eta_col_end, newlp->rows_alloc + newlp->max_num_inv, int);

  newlp->bb_rule=FIRST_NI;
  newlp->break_at_int=FALSE;
  newlp->break_value=0;

  newlp->iter=0;
  newlp->total_iter=0;
  CALLOC(newlp->solution, sum + 1, REAL);
  CALLOC(newlp->best_solution, sum + 1, REAL);
  CALLOC(newlp->duals, rows + 1, REAL);

  newlp->maximise = FALSE;
  newlp->floor_first = TRUE;

  newlp->scaling_used = FALSE;
  newlp->columns_scaled = FALSE;

  CALLOC(newlp->ch_sign, rows + 1, short);

  for(i = 0; i <= rows; i++)
    newlp->ch_sign[i] = FALSE;

  newlp->valid = FALSE; 

  return(newlp);
}

void delete_lp(lprec *lp)
{
  int i; 

  if(lp->active)
    Lp=NULL;
  if(lp->names_used)
    {
      free(lp->row_name);
      free(lp->col_name);
    }
  free(lp->mat);
  free(lp->col_no);
  free(lp->col_end);
  free(lp->row_end);
  free(lp->orig_rh);
  free(lp->rh);
  free(lp->rhs);
  free(lp->must_be_int);
  free(lp->orig_upbo);
  free(lp->orig_lowbo);
  free(lp->upbo);
  free(lp->lowbo);
  free(lp->bas);
  free(lp->basis);
  free(lp->lower);
  free(lp->eta_value);
  free(lp->eta_row_nr);
  free(lp->eta_col_end);
  free(lp->solution);
  free(lp->best_solution);
  free(lp->duals);
  free(lp->ch_sign);
  if(lp->scaling_used)
    free(lp->scale);
  if(lp->nr_lagrange>0)
    {
      free(lp->lag_rhs);
      free(lp->lambda);
      free(lp->lag_con_type);
      for(i=0; i < lp->nr_lagrange; i++)
        free(lp->lag_row[i]);
      free(lp->lag_row);
    }  

  free(lp);
  lp=NULL;
}  

lprec *copy_lp(lprec *lp)
{
  lprec *newlp;
  int i, rowsplus, colsplus, sumplus;
  
  rowsplus=lp->rows_alloc+1;
  colsplus=lp->columns_alloc+1;
  sumplus=lp->sum_alloc+1;

  MALLOCCPY(newlp, lp, 1, lprec); /* copy all non pointers */

  newlp->active=FALSE;

  if(newlp->names_used)
    {
      MALLOCCPY(newlp->col_name, lp->col_name, colsplus, nstring);
      MALLOCCPY(newlp->row_name, lp->row_name, rowsplus, nstring);
    }

  MALLOCCPY(newlp->mat, lp->mat, newlp->mat_alloc, matrec);
  MALLOCCPY(newlp->col_end, lp->col_end, colsplus, int);
  MALLOCCPY(newlp->col_no, lp->col_no, newlp->mat_alloc, int);
  MALLOCCPY(newlp->row_end, lp->row_end, rowsplus, int);
  MALLOCCPY(newlp->orig_rh, lp->orig_rh, rowsplus, REAL);
  MALLOCCPY(newlp->rh, lp->rh, rowsplus, REAL);
  MALLOCCPY(newlp->rhs, lp->rhs, rowsplus, REAL);
  MALLOCCPY(newlp->must_be_int, lp->must_be_int, sumplus, short);
  MALLOCCPY(newlp->orig_upbo, lp->orig_upbo, sumplus, REAL);
  MALLOCCPY(newlp->orig_lowbo, lp->orig_lowbo, sumplus, REAL);
  MALLOCCPY(newlp->upbo, lp->upbo, sumplus, REAL);
  MALLOCCPY(newlp->lowbo, lp->lowbo, sumplus, REAL);
  MALLOCCPY(newlp->bas, lp->bas, rowsplus, int);
  MALLOCCPY(newlp->basis, lp->basis, sumplus, short);
  MALLOCCPY(newlp->lower, lp->lower, sumplus, short);
  MALLOCCPY(newlp->eta_value, lp->eta_value, lp->eta_alloc, REAL);
  MALLOCCPY(newlp->eta_row_nr, lp->eta_row_nr, lp->eta_alloc, int);
  MALLOCCPY(newlp->eta_col_end, lp->eta_col_end,
	    lp->rows_alloc + lp->max_num_inv, int);
  MALLOCCPY(newlp->solution, lp->solution, sumplus, REAL);
  MALLOCCPY(newlp->best_solution, lp->best_solution, sumplus, REAL);
  MALLOCCPY(newlp->duals, lp->duals, rowsplus, REAL);
  MALLOCCPY(newlp->ch_sign, lp->ch_sign, rowsplus, short);

  if(newlp->scaling_used)
    MALLOCCPY(newlp->scale, lp->scale, sumplus, REAL);

  if(newlp->nr_lagrange > 0)
    {
      MALLOCCPY(newlp->lag_rhs, lp->lag_rhs, newlp->nr_lagrange, REAL);
      MALLOCCPY(newlp->lambda, lp->lambda, newlp->nr_lagrange, REAL);
      MALLOCCPY(newlp->lag_con_type, lp->lag_con_type, newlp->nr_lagrange,
		short);
      MALLOC(newlp->lag_row, newlp->nr_lagrange, REAL*);
      for(i = 0; i < newlp->nr_lagrange; i++)
        MALLOCCPY(newlp->lag_row[i], lp->lag_row[i], colsplus, REAL);
    }
  return(newlp);
}

void inc_mat_space(lprec *lp, int maxextra)
{
   if(lp->non_zeros + maxextra >= lp->mat_alloc)
     {
       lp->mat_alloc = lp->non_zeros + maxextra;
       REALLOC(lp->mat, lp->mat_alloc, matrec);
       REALLOC(lp->col_no, lp->mat_alloc, int);
       if (lp->active)
         {
           Mat=lp->mat;
           Col_no=lp->col_no;
         }
     }
}
 
void inc_row_space(lprec *lp)
{
  if(lp->rows > lp->rows_alloc)
    {
      lp->rows_alloc=lp->rows+10;
      lp->sum_alloc=lp->rows_alloc+lp->columns_alloc;
      REALLOC(lp->orig_rh, lp->rows_alloc + 1, REAL);
      REALLOC(lp->rh, lp->rows_alloc + 1, REAL);
      REALLOC(lp->rhs, lp->rows_alloc + 1, REAL);
      REALLOC(lp->orig_upbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->upbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->orig_lowbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->lowbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->solution, lp->sum_alloc + 1, REAL);
      REALLOC(lp->best_solution, lp->sum_alloc + 1,  REAL);
      REALLOC(lp->row_end, lp->rows_alloc + 1, int);
      REALLOC(lp->basis, lp->sum_alloc + 1, short);
      REALLOC(lp->lower, lp->sum_alloc + 1, short);
      REALLOC(lp->must_be_int, lp->sum_alloc + 1, short);
      REALLOC(lp->bas, lp->rows_alloc + 1, int);
      REALLOC(lp->duals, lp->rows_alloc + 1, REAL);
      REALLOC(lp->ch_sign, lp->rows_alloc + 1, short);
      REALLOC(lp->eta_col_end, lp->rows_alloc + lp->max_num_inv, int);
      if(lp->names_used)
        REALLOC(lp->row_name, lp->rows_alloc + 1, nstring);
      if(lp->scaling_used)
        REALLOC(lp->scale, lp->sum_alloc + 1, REAL);
      if(lp->active)
        set_globals(lp); 
    }
}

void inc_col_space(lprec *lp)
{
  if(lp->columns >= lp->columns_alloc)
    {
      lp->columns_alloc=lp->columns+10;
      lp->sum_alloc=lp->rows_alloc+lp->columns_alloc;
      REALLOC(lp->must_be_int, lp->sum_alloc + 1, short);
      REALLOC(lp->orig_upbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->upbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->orig_lowbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->lowbo, lp->sum_alloc + 1, REAL);
      REALLOC(lp->solution, lp->sum_alloc + 1, REAL);
      REALLOC(lp->best_solution, lp->sum_alloc + 1,  REAL);
      REALLOC(lp->basis, lp->sum_alloc + 1, short);
      REALLOC(lp->lower, lp->sum_alloc + 1, short);
      if(lp->names_used)
        REALLOC(lp->col_name, lp->columns_alloc + 1, nstring);
      if(lp->scaling_used)
        REALLOC(lp->scale, lp->sum_alloc + 1, REAL);
      REALLOC(lp->col_end, lp->columns_alloc + 1, int);
      if(lp->active)
        set_globals(lp);
    }
}

void set_mat(lprec *lp, int Row, int Column, REAL Value)
{
  int elmnr, lastelm, i;

  if(Row > lp->rows || Row < 0)
    error("Row out of range");
  if(Column > lp->columns || Column < 1)
    error("Column out of range");
  if(lp->scaling_used)
    Value *= lp->scale[Row] * lp->scale[lp->rows + Column];
  
   if(TRUE /*abs(Value) > lp->epsilon*/)
    {
       if (lp->basis[Column] == TRUE && Row > 0)
         lp->basis_valid = FALSE;
       lp->eta_valid = FALSE;
       elmnr = lp->col_end[Column-1];
       while((elmnr < lp->col_end[Column]) ?
	     (lp->mat[elmnr].row_nr != Row) : FALSE)
         elmnr++;

       if((elmnr != lp->col_end[Column]) ?
	  (lp->mat[elmnr].row_nr == Row) : FALSE )
         if (lp->scaling_used)
           {
             if(lp->ch_sign[Row])
               lp->mat[elmnr].value = 
		 -Value * lp->scale[Row] * lp->scale[Column];
             else
               lp->mat[elmnr].value =
		 Value * lp->scale[Row] * lp->scale[Column];
           }
         else
           {
             if(lp->ch_sign[Row])
               lp->mat[elmnr].value = -Value;
             else
               lp->mat[elmnr].value = Value;
           }
       else
         {
           /* check if more space is needed for matrix */
           inc_mat_space(lp,1);

           /* Shift the matrix */
           lastelm=lp->non_zeros; 
           for(i = lastelm; i > elmnr ; i--)
             lp->mat[i]=lp->mat[i-1];
           for(i = Column; i <= lp->columns; i++)
             lp->col_end[i]++;

           /* Set new element */
           lp->mat[elmnr].row_nr=Row;

           if (lp->scaling_used)
             {
               if(lp->ch_sign[Row])
                 lp->mat[elmnr].value=-Value*lp->scale[Row]*lp->scale[Column];
               else
                 lp->mat[elmnr].value=Value*lp->scale[Row]*lp->scale[Column];
             }
           else
             {
               if(lp->ch_sign[Row])
                  lp->mat[elmnr].value=-Value;
               else
                 lp->mat[elmnr].value=Value;
             }

           lp->row_end_valid=FALSE;
            
           lp->non_zeros++;
           if (lp->active)
             Non_zeros=lp->non_zeros;
        }      
    }
}

void set_obj_fn(lprec *lp, REAL *row)
{
  int i;
  for(i = 1; i <= lp->columns; i++)
    set_mat(lp, 0, i, row[i]);
}

void str_set_obj_fn(lprec *lp, char *row)
{
  int  i;
  REAL *arow;
  char *p, *newp;
  CALLOC(arow, lp->columns + 1, REAL);
  p = row;
  for(i = 1; i <= lp->columns; i++)
    {
      arow[i] = (REAL) strtod(p, &newp);
      if(p==newp)
	error("Bad string in str_set_obj_fn");
      else
	p=newp; 
    }
  set_obj_fn(lp, arow);
  free(arow);
}


void add_constraint(lprec *lp, REAL *row, short constr_type, REAL rh)
{
  matrec *newmat;
  int  i, j;
  int  elmnr;
  int  stcol;
  int  *addtoo;

  MALLOC(addtoo, lp->columns + 1, int)  

    for(i = 1; i <= lp->columns; i++)
      if(row[i]!=0)
	{
	  addtoo[i]=TRUE;
	  lp->non_zeros++;
	}
      else
	addtoo[i]=FALSE;

  MALLOC(newmat, lp->non_zeros, matrec);
  inc_mat_space(lp, 0);
  lp->rows++;
  lp->sum++;
  inc_row_space(lp);

  if(lp->scaling_used)
    {
      /* shift scale */
      for(i=lp->sum; i > lp->rows; i--)
        lp->scale[i]=lp->scale[i-1];
      lp->scale[lp->rows]=1;
    }

  if(lp->names_used)
    sprintf(lp->row_name[lp->rows], "r_%d", lp->rows);

  if(lp->scaling_used && lp->columns_scaled)
    for(i = 1; i <= lp->columns; i++)
      row[i] *= lp->scale[lp->rows+i];
     
  if(constr_type==GE)
    lp->ch_sign[lp->rows] = TRUE;
  else
    lp->ch_sign[lp->rows] = FALSE;

  elmnr = 0;
  stcol = 0;
  for(i = 1; i <= lp->columns; i++)
    {
      for(j = stcol; j < lp->col_end[i]; j++)
        {  
	  newmat[elmnr].row_nr=lp->mat[j].row_nr;
	  newmat[elmnr].value=lp->mat[j].value;
	  elmnr++;
        }
      if(addtoo[i])
        {
	  if(lp->ch_sign[lp->rows])
	    newmat[elmnr].value = -row[i];
	  else
	    newmat[elmnr].value = row[i];
	  newmat[elmnr].row_nr = lp->rows;
	  elmnr++;
        }
      stcol=lp->col_end[i];
      lp->col_end[i]=elmnr;
    }    
  
  memcpy(lp->mat, newmat, lp->non_zeros*sizeof(matrec));
 
  free(newmat);
  free(addtoo);

  for(i=lp->sum ; i > lp->rows; i--)
    {
      lp->orig_upbo[i]=lp->orig_upbo[i-1];
      lp->orig_lowbo[i]=lp->orig_lowbo[i-1];
      lp->basis[i]=lp->basis[i-1];
      lp->lower[i]=lp->lower[i-1];
      lp->must_be_int[i]=lp->must_be_int[i-1];
    }

  for(i= 1 ; i <= lp->rows; i++)
    if(lp->bas[i] >= lp->rows)
      lp->bas[i]++;

  if(constr_type==LE || constr_type==GE)
    {
      lp->orig_upbo[lp->rows]=lp->infinite;
    }
  else if(constr_type==EQ)
    {
      lp->orig_upbo[lp->rows]=0;
    }
  else
    {
      fprintf(stderr, "Wrong constraint type\n");
      exit(FAIL);
    }

  lp->orig_lowbo[lp->rows]=0;

  if(constr_type==GE && rh != 0)
    lp->orig_rh[lp->rows]=-rh;
  else
    lp->orig_rh[lp->rows]=rh;  

  lp->row_end_valid=FALSE;
 
  lp->bas[lp->rows]=lp->rows;
  lp->basis[lp->rows]=TRUE;
  lp->lower[lp->rows]=TRUE;   
 
  if(lp->active)
    set_globals(lp);
  lp->eta_valid=FALSE;
}

void str_add_constraint(lprec *lp,
			char *row_string,
			short constr_type,
			REAL rh)
{
  int  i;
  REAL *aRow;
  char *p, *newp;
  CALLOC(aRow, lp->columns + 1, REAL);
  p = row_string;
 
  for(i = 1; i <= lp->columns; i++)
    {
      aRow[i] = (REAL) strtod(p, &newp);
      if(p==newp)
	error("Bad string in str_add_constr");
      else
	p=newp; 
    }
  add_constraint(lp, aRow, constr_type, rh);
  free(aRow);
}

void del_constraint(lprec *lp, int del_row)
{
  int i, j;
  unsigned elmnr;
  int startcol;

  if(del_row<1 || del_row>lp->rows)
    {
      fprintf(stderr, "There is no constraint nr. %d\n", del_row);
      exit(FAIL);
    }

  elmnr=0;
  startcol=0;

  for(i = 1; i <= lp->columns; i++)
    {
      for(j=startcol; j < lp->col_end[i]; j++)
	{
	  if(lp->mat[j].row_nr!=del_row)
	    {
	      lp->mat[elmnr]=lp->mat[j];
	      if(lp->mat[elmnr].row_nr > del_row)
		lp->mat[elmnr].row_nr--;
	      elmnr++;
	    }
	  else
	    lp->non_zeros--;
	}
      startcol=lp->col_end[i];
      lp->col_end[i]=elmnr;
    }
  for(i = del_row; i < lp->rows; i++)
    {
      lp->orig_rh[i] = lp->orig_rh[i+1];
      lp->ch_sign[i] = lp->ch_sign[i+1];
      lp->bas[i] = lp->bas[i+1];
      if(lp->names_used)
        strcpy(lp->row_name[i], lp->row_name[i+1]);
    }
  for(i = 1; i < lp->rows; i++)
    if(lp->bas[i] >  del_row)
      lp->bas[i]--;

  for(i=del_row; i < lp->sum; i++)
    {
      lp->lower[i]=lp->lower[i+1];
      lp->basis[i]=lp->basis[i+1];
      lp->orig_upbo[i]=lp->orig_upbo[i+1];
      lp->orig_lowbo[i]=lp->orig_lowbo[i+1];
      lp->must_be_int[i]=lp->must_be_int[i+1];
      if(lp->scaling_used)
        lp->scale[i]=lp->scale[i+1];
    }

  lp->rows--;
  lp->sum--;

  lp->row_end_valid=FALSE;
  
  if(lp->active)
    set_globals(lp);
  lp->eta_valid=FALSE;
  lp->basis_valid=FALSE; 
}

void add_lag_con(lprec *lp, REAL *row, short con_type, REAL rhs)
{
  int i;
  REAL sign;
  if(con_type == LE || con_type == EQ)
    sign = 1;
  else if(con_type == GE)
    sign = -1;
  else
    error("con_type not implemented\n");

  lp->nr_lagrange++;
  if(lp->nr_lagrange==1)
    {
      CALLOC(lp->lag_row, lp->nr_lagrange, REAL*);
      CALLOC(lp->lag_rhs, lp->nr_lagrange, REAL);
      CALLOC(lp->lambda, lp->nr_lagrange, REAL);
      CALLOC(lp->lag_con_type, lp->nr_lagrange, short);
    }
  else
    {
      REALLOC(lp->lag_row, lp->nr_lagrange, REAL*);
      REALLOC(lp->lag_rhs, lp->nr_lagrange, REAL);
      REALLOC(lp->lambda, lp->nr_lagrange, REAL);
      REALLOC(lp->lag_con_type, lp->nr_lagrange, short);
    }
  CALLOC(lp->lag_row[lp->nr_lagrange-1], lp->columns+1, REAL);
  lp->lag_rhs[lp->nr_lagrange-1]=rhs * sign;
  for( i=1; i <= lp->columns; i++)
    lp->lag_row[lp->nr_lagrange-1][i]=row[i] * sign;
  lp->lambda[lp->nr_lagrange-1]=0;
  lp->lag_con_type[lp->nr_lagrange-1]=(con_type == EQ);
}

void str_add_lag_con(lprec *lp, char *row, short con_type, REAL rhs)
{
  int  i;
  REAL *a_row;
  char *p, *new_p;
  CALLOC(a_row, lp->columns + 1, REAL);
  p = row;
 
  for(i = 1; i <= lp->columns; i++)
    {
      a_row[i] = (REAL) strtod(p, &new_p);
      if(p==new_p)
	error("Bad string in str_add_lag_con");
      else
	p=new_p; 
    }
  add_lag_con(lp, a_row, con_type, rhs);
  free(a_row);
}


void add_column(lprec *lp, REAL *column)
{
  int i, elmnr;

  lp->columns++;
  lp->sum++;
  inc_col_space(lp);
  inc_mat_space(lp, lp->rows+1);

  if(lp->scaling_used)
    {
      for(i = 0; i <= lp->rows; i++)
	column[i]*=lp->scale[i];
      lp->scale[lp->sum]=1;
    }

  elmnr=lp->col_end[lp->columns-1];
  for(i = 0 ; i <= lp->rows ; i++)
    if(column[i] != 0)
      {
	lp->mat[elmnr].row_nr=i;
	if(lp->ch_sign[i])
	  lp->mat[elmnr].value=-column[i];
	else
	  lp->mat[elmnr].value=column[i];
	lp->non_zeros++;
	elmnr++;
      }
  lp->col_end[lp->columns]=elmnr;
  lp->orig_lowbo[lp->sum]=0;
  lp->orig_upbo[lp->sum]=lp->infinite;
  lp->lower[lp->sum]=TRUE;
  lp->basis[lp->sum]=FALSE;
  lp->must_be_int[lp->sum]=FALSE;
  if(lp->names_used)
    sprintf(lp->col_name[lp->columns], "var_%d", lp->columns);

 
  lp->row_end_valid=FALSE;

  if(lp->active)
    {
      Sum=lp->sum;
      Columns=lp->columns;
      Non_zeros=lp->non_zeros;
    }
}

void str_add_column(lprec *lp, char *col_string)
{
  int  i;
  REAL *aCol;
  char *p, *newp;
  CALLOC(aCol, lp->rows + 1, REAL);
  p = col_string;
 
  for(i = 0; i <= lp->rows; i++)
    {
      aCol[i] = (REAL) strtod(p, &newp);
      if(p==newp)
	error("Bad string in str_add_column");
      else
	p=newp; 
    }
  add_column(lp, aCol);
  free(aCol);
}

void del_column(lprec *lp, int column)
{
  int i, j, from_elm, to_elm, elm_in_col;
  if(column > lp->columns || column < 1)
    error("Column out of range in del_column");
  for(i = 1; i <= lp->rows; i++)
    {
      if(lp->bas[i]==lp->rows+column)
        lp->basis_valid=FALSE;
      else if(lp->bas[i] > lp->rows+column)
        lp->bas[i]--;
    }
  for(i = lp->rows+column; i < lp->sum; i++)
    {
      if(lp->names_used)
        strcpy(lp->col_name[i-lp->rows], lp->col_name[i-lp->rows+1]);
      lp->must_be_int[i]=lp->must_be_int[i+1];
      lp->orig_upbo[i]=lp->orig_upbo[i+1];
      lp->orig_lowbo[i]=lp->orig_lowbo[i+1];
      lp->upbo[i]=lp->upbo[i+1];
      lp->lowbo[i]=lp->lowbo[i+1];
      lp->basis[i]=lp->basis[i+1];
      lp->lower[i]=lp->lower[i+1];
      if(lp->scaling_used)
        lp->scale[i]=lp->scale[i+1];
    }
  for(i = 0; i < lp->nr_lagrange; i++)
    for(j = column; j <= lp->columns; j++)
      lp->lag_row[i][j]=lp->lag_row[i][j+1];
  to_elm=lp->col_end[column-1];
  from_elm=lp->col_end[column];
  elm_in_col=from_elm-to_elm;
  for(i = from_elm; i < lp->non_zeros; i++)
    {
      lp->mat[to_elm]=lp->mat[i];
      to_elm++;
    }
  for(i = column; i < lp->columns; i++)
    lp->col_end[i]=lp->col_end[i+1]-elm_in_col;
  lp->non_zeros -= elm_in_col;
  lp->row_end_valid=FALSE;
  lp->eta_valid=FALSE;

  lp->sum--;
  lp->columns--;
  if(lp->active)
    set_globals(lp);
}

void set_upbo(lprec *lp, int column, REAL value)
{
  if(column > lp->columns || column < 1)
    error("Column out of range");
  if(lp->scaling_used)
    value /= lp->scale[lp->rows + column];
  if(value < lp->orig_lowbo[lp->rows + column])
    error("Upperbound must be >= lowerbound"); 
  lp->eta_valid = FALSE;
  lp->orig_upbo[lp->rows+column] = value;
}

void set_lowbo(lprec *lp, int column, REAL value)
{
  if(column > lp->columns || column < 1)
    error("Column out of range");
  if(lp->scaling_used)
    value /= lp->scale[lp->rows + column];
  if(value > lp->orig_upbo[lp->rows + column])
    error("Upperbound must be >= lowerbound"); 
  lp->eta_valid = FALSE;
  lp->orig_lowbo[lp->rows+column] = value;
}

void set_int(lprec *lp, int column, short must_be_int)
{
  if(column > lp->columns || column < 1)
    error("Column out of range");
  lp->must_be_int[lp->rows+column]=must_be_int;
  if(must_be_int==TRUE)
    if(lp->columns_scaled)
      unscale_columns(lp);
}

void set_rh(lprec *lp, int row, REAL value)
{
  if(row > lp->rows || row < 0)
    error("Row out of Range");
  if(row == 0)			/* setting of RHS of OF not meaningful */
    {
      fprintf(stderr,
	      "Warning: attempt to set RHS of objective function, ignored\n");
      return;
    }
  if(lp->scaling_used)
    if(lp->ch_sign[row])
      lp->orig_rh[row] = -value * lp->scale[row];
    else
      lp->orig_rh[row] = value * lp->scale[row];
  else
    if(lp->ch_sign[row])
      lp->orig_rh[row] = -value;
    else
      lp->orig_rh[row] = value;
  lp->eta_valid = FALSE;
} 

void set_rh_vec(lprec *lp, REAL *rh)
{
  int i;
  if(lp->scaling_used)
    for(i = 1; i <= lp->rows; i++)
      if(lp->ch_sign[i])
        lp->orig_rh[i]=-rh[i]*lp->scale[i];
      else
        lp->orig_rh[i]=rh[i]*lp->scale[i];
  else
    for(i=1; i <= lp->rows; i++)
      if(lp->ch_sign[i])
        lp->orig_rh[i]=-rh[i];
      else
        lp->orig_rh[i]=rh[i];
  lp->eta_valid=FALSE;
}

void str_set_rh_vec(lprec *lp, char *rh_string)
{
  int  i;
  REAL *newrh;
  char *p, *newp;
  CALLOC(newrh, lp->rows + 1, REAL);
  p = rh_string;
 
  for(i = 1; i <= lp->rows; i++)
    {
      newrh[i] = (REAL) strtod(p, &newp);
      if(p==newp)
	error("Bad string in str_set_rh_vec");
      else
	p=newp; 
    }
  set_rh_vec(lp, newrh);
  free(newrh);
}


void set_maxim(lprec *lp)
{
  int i;
  if(lp->maximise==FALSE)
    {
      for(i = 0; i < lp->non_zeros; i++)
	if(lp->mat[i].row_nr==0)
	  lp->mat[i].value*=-1;
      lp->eta_valid=FALSE;
    } 
  lp->maximise=TRUE;
  lp->ch_sign[0]=TRUE;
  if(lp->active)
    Maximise=TRUE;
}

void set_minim(lprec *lp)
{
  int i;
  if(lp->maximise==TRUE)
    {
      for(i = 0; i < lp->non_zeros; i++)
	if(lp->mat[i].row_nr==0)
	  lp->mat[i].value = -lp->mat[i].value;
      lp->eta_valid=FALSE;
    } 
  lp->maximise=FALSE;
  lp->ch_sign[0]=FALSE;
  if(lp->active)
    Maximise=FALSE;
}

void set_constr_type(lprec *lp, int row, short con_type)
{
  int i;
  if(row > lp->rows || row < 1)
    error("Row out of Range");
  if(con_type==EQ)
    {
      lp->orig_upbo[row]=0;
      lp->basis_valid=FALSE;
      if(lp->ch_sign[row])
        {
          for(i = 0; i < lp->non_zeros; i++)
            if(lp->mat[i].row_nr==row)
              lp->mat[i].value*=-1;
          lp->eta_valid=FALSE;
          lp->ch_sign[row]=FALSE;
          if(lp->orig_rh[row]!=0)
            lp->orig_rh[row]*=-1;
        }
    }
  else if(con_type==LE)
    {
      lp->orig_upbo[row]=lp->infinite;
      lp->basis_valid=FALSE;
      if(lp->ch_sign[row])
        {
          for(i = 0; i < lp->non_zeros; i++)
            if(lp->mat[i].row_nr==row)
              lp->mat[i].value*=-1;
          lp->eta_valid=FALSE;
          lp->ch_sign[row]=FALSE;
          if(lp->orig_rh[row]!=0)
            lp->orig_rh[row]*=-1;
        }
    }
  else if(con_type==GE)
    {
      lp->orig_upbo[row]=lp->infinite;
      lp->basis_valid=FALSE;
      if(!lp->ch_sign[row])
        {
          for(i = 0; i < lp->non_zeros; i++)
            if(lp->mat[i].row_nr==row)
              lp->mat[i].value*=-1;
          lp->eta_valid=FALSE;
          lp->ch_sign[row]=TRUE;
          if(lp->orig_rh[row]!=0)
            lp->orig_rh[row]*=-1;
        }
    } 
  else
    error("Constraint type not (yet) implemented");
}

REAL mat_elm(lprec *lp, int row, int column)
{
  REAL value;
  int elmnr;
  if(row < 0 || row > lp->rows)
    error("Row out of range in mat_elm");
  if(column < 1 || column > lp->columns)
    error("Column out of range in mat_elm");
  value=0;
  elmnr=lp->col_end[column-1];
  while(lp->mat[elmnr].row_nr != row && elmnr < lp->col_end[column])
    elmnr++;
  if(elmnr != lp->col_end[column])
    {
      value = lp->mat[elmnr].value;
      if(lp->ch_sign[row])
        value = -value;
      if(lp->scaling_used)
        value /= lp->scale[row] * lp->scale[lp->rows + column];
    }
  return(value);
}


void get_row(lprec *lp, int row_nr, REAL *row)
{
  int i, j;

  if(row_nr <0 || row_nr > lp->rows)
    error("Row nr. out of range in get_row");
  for(i = 1; i <= lp->columns; i++)
    {
      row[i]=0;
      for(j=lp->col_end[i-1]; j < lp->col_end[i]; j++)
        if(lp->mat[j].row_nr==row_nr)
          row[i]=lp->mat[j].value;
      if(lp->scaling_used)
        row[i] /= lp->scale[lp->rows+i] * lp->scale[row_nr];
    }
  if(lp->ch_sign[row_nr])
    for(i=0; i <= lp->columns; i++)
      if(row[i]!=0)
        row[i] = -row[i];
}

void get_column(lprec *lp, int col_nr, REAL *column)
{
  int i;

  if(col_nr < 1 || col_nr > lp->columns)
    error("Col. nr. out of range in get_column");
  for(i = 0; i <= lp->rows; i++)
    column[i]=0;
  for(i = lp->col_end[col_nr-1]; i < lp->col_end[col_nr]; i++)
    column[lp->mat[i].row_nr]=lp->mat[i].value;
  for(i = 0; i <= lp->rows; i++)
    if(column[i] !=0)
      {
	if(lp->ch_sign[i])
	  column[i]*=-1;
	if(lp->scaling_used)
	  column[i]/=(lp->scale[i] * lp->scale[lp->rows+col_nr]);
      }
}

void get_reduced_costs(lprec *lp, REAL *rc)
{
  int varnr, i, j;
  REAL f;

  if(!lp->basis_valid)
    error("Not a valid basis in get_reduced_costs");
  set_globals(lp);
  if(!lp->eta_valid)
    invert();  
  for(i = 1; i <= lp->sum; i++)
    rc[i] = 0;
  rc[0] = 1;
  btran(rc);
  for(i = 1; i <= lp->columns; i++)
    {
      varnr = lp->rows + i;
      if(!Basis[varnr])
        if(Upbo[varnr] > 0)
	  {
            f = 0;
	    for(j = Col_end[i - 1]; j < Col_end[i]; j++)
	      f += rc[Mat[j].row_nr] * Mat[j].value;
            rc[varnr] = f;
	  }
    }
  for(i = 1; i <= Sum; i++)
    my_round(rc[i], Epsd);
}   

short is_feasible(lprec *lp, REAL *values)
{
  int i, elmnr;
  REAL *this_rhs;
  REAL dist;

  if(lp->scaling_used)
    {
      for(i = lp->rows+1; i <= lp->sum; i++)
        if(   values[i - lp->rows] < lp->orig_lowbo[i]*lp->scale[i]
	   || values[i - lp->rows] > lp->orig_upbo[i]*lp->scale[i])
          return(FALSE);
    }
  else
    {
      for(i = lp->rows+1; i <= lp->sum; i++)
        if(   values[i - lp->rows] < lp->orig_lowbo[i]
	   || values[i - lp->rows] > lp->orig_upbo[i])
          return(FALSE);
    }
  CALLOC(this_rhs, lp->rows+1, REAL)
    for(i = 1; i <= lp->columns; i++)
      for(elmnr = lp->col_end[i - 1]; elmnr < lp->col_end[i]; elmnr++)
	this_rhs[lp->mat[elmnr].row_nr] += lp->mat[elmnr].value * values[i]; 
  for(i = 1; i <= lp->rows; i++)
    {
      dist = lp->orig_rh[i] - this_rhs[i];
      my_round(dist, 0.001)
	if((lp->orig_upbo[i] == 0 && dist != 0) || dist < 0)
	  {
	    free(this_rhs);
	    return(FALSE);
	  }     
    } 
  free(this_rhs);
  return(TRUE);
}

short column_in_lp(lprec *lp, REAL *testcolumn)
{
  int i, j;
  short ident;
  REAL value;

  if(lp->scaling_used)
    for(i = 1; i <= lp->columns; i++)
      {
        ident = TRUE;
        j = lp->col_end[i-1];
        while(ident && (j < lp->col_end[i]))
          {
            value = lp->mat[j].value;
            if(lp->ch_sign[lp->mat[j].row_nr])
              value = -value;
            value /= lp->scale[lp->rows+i];
            value /= lp->scale[lp->mat[j].row_nr];
            value -= testcolumn[lp->mat[j].row_nr];
            if(my_abs(value) >  0.001) /* should be some epsilon? MB */
              ident=FALSE;
            j++; 
          }
        if(ident)
          return(TRUE);
      }
  else
    for(i = 1; i <= lp->columns; i++)
      {
        ident = TRUE;
        j = lp->col_end[i-1];
        while(ident && j < lp->col_end[i])
          {
            value = lp->mat[j].value;
            if(lp->ch_sign[lp->mat[j].row_nr])
              value *= -1;
            value -= testcolumn[lp->mat[j].row_nr];
            if( my_abs(value) >  0.001 )
              ident=FALSE;
            j++;
          }
        if(ident)
          return(TRUE);
      }
  return(FALSE);
}


void print_lp(lprec *lp)
{
  int i, j;
  REAL *fatmat;
  CALLOC(fatmat, (lp->rows + 1) * lp->columns, REAL);
  for(i = 1; i <= lp->columns; i++)
    for(j = lp->col_end[i-1]; j < lp->col_end[i]; j++)
      fatmat[(i - 1) * (lp->rows + 1) + lp->mat[j].row_nr]=lp->mat[j].value;

  printf("problem name: %s\n", lp->lp_name);
  printf("          ");
  for(j = 1; j <= lp->columns; j++)
    if(lp->names_used)
      printf("%8s ", lp->col_name[j]);
    else
      printf("Var[%3d] ", j);
  if(lp->maximise)
    {
      printf("\nMaximise  ");
      for(j = 0; j < lp->columns; j++)
	printf("% 8.2f ",-fatmat[j*(lp->rows+1)]);
    }
  else
    {
      printf("\nMinimize  ");
      for(j = 0; j < lp->columns; j++)
	printf("% 8.2f ", fatmat[j*(lp->rows+1)]);
    }
  printf("\n");
  for(i = 1; i <= lp->rows; i++)
    {
      if(lp->names_used)
	printf("%9s ", lp->row_name[i]);
      else
	printf("Row[%3d]  ", i);
      for(j = 0; j < lp->columns; j++)
	if(lp->ch_sign[i] && fatmat[j*(lp->rows+1)+i] != 0)
	  printf("% 8.2f ",-fatmat[j*(lp->rows+1)+i]);
	else
	  printf("% 8.2f ", fatmat[j*(lp->rows+1)+i]);
      if(lp->orig_upbo[i]==lp->infinite)
	if(lp->ch_sign[i])
	  printf(">= ");
	else
	  printf("<= ");
      else
	printf(" = ");
      if(lp->ch_sign[i])
	printf("% 8.2f\n",-lp->orig_rh[i]);
      else
	printf("% 8.2f\n", lp->orig_rh[i]);
    }
  printf("Type      ");
  for(i = 1; i <= lp->columns; i++)
    if(lp->must_be_int[lp->rows+i]==TRUE)
      printf("     Int ");
    else
      printf("    Real ");
  printf("\nupbo      ");
  for(i = 1; i <= lp->columns; i++)
    if(lp->orig_upbo[lp->rows+i]==lp->infinite)
      printf("     Inf ");
    else
      printf("% 8.2f ", lp->orig_upbo[lp->rows+i]);
  printf("\nlowbo     ");
  for(i = 1; i <= lp->columns; i++)
    printf("% 8.2f ", lp->orig_lowbo[lp->rows+i]);
  printf("\n");
  for(i = 0; i < lp->nr_lagrange; i++)
    {
      printf("lag[%3d]  ", i);
      for(j = 1; j <= lp->columns; j++)
	printf("% 8.2f ", lp->lag_row[i][j]);
      if(lp->orig_upbo[i]==lp->infinite)
	if(lp->lag_con_type[i] == GE)
	  printf(">= ");
	else if(lp->lag_con_type[i] == LE)
	  printf("<= ");
	else if(lp->lag_con_type[i] == EQ)
	  printf(" = ");
      printf("% 8.2f\n", lp->lag_rhs[i]);
    }

  free(fatmat);
}  

void set_row_name(lprec *lp, int row, nstring new_name)
{
  int i;

  if(!lp->names_used)
    {
      CALLOC(lp->row_name, lp->rows_alloc + 1, nstring);
      CALLOC(lp->col_name, lp->columns_alloc + 1, nstring);
      lp->names_used=TRUE;
      for(i = 0; i <= lp->rows; i++)
        sprintf(lp->row_name[i], "r_%d", i);
      for(i = 1; i <= lp->columns; i++)
        sprintf(lp->col_name[i], "var_%d", i);
    }
  strcpy(lp->row_name[row], new_name);
}

void set_col_name(lprec *lp, int column, nstring new_name)
{
  int i;
 
  if(!lp->names_used)
    {
      CALLOC(lp->row_name, lp->rows_alloc + 1, nstring);
      CALLOC(lp->col_name, lp->columns_alloc + 1, nstring);
      lp->names_used=TRUE;
      for(i = 0; i <= lp->rows; i++)
        sprintf(lp->row_name[i], "r_%d", i);
      for(i = 1; i <= lp->columns; i++)
        sprintf(lp->col_name[i], "var_%d", i);
    }
  strcpy(lp->col_name[column], new_name);
}

static REAL minmax_to_scale(REAL min, REAL max)
{
  REAL scale;

  /* should do something sensible when min or max is 0, MB */
  if((min == 0) || (max == 0))
    return((REAL)1);

  scale = 1 / pow(10, (log10(min) + log10(max)) / 2);
  return(scale);
}

void unscale_columns(lprec *lp)
{
  int i, j;

  /* unscale mat */
  for(j = 1; j <= lp->columns; j++)
    for(i = lp->col_end[j - 1]; i < lp->col_end[j]; i++)
      lp->mat[i].value /= lp->scale[lp->rows + j];

  /* unscale bounds as well */
  for(i = lp->rows + 1; i < lp->sum; i++)
    {
      if(lp->orig_lowbo[i] != 0)
	lp->orig_lowbo[i] *= lp->scale[i];
      if(lp->orig_upbo[i] != lp->infinite)
	lp->orig_upbo[i] *= lp->scale[i];
    }
    
  for(i=lp->rows+1; i<= lp->sum; i++)
    lp->scale[i]=1;
  lp->columns_scaled=FALSE;
  lp->eta_valid=FALSE;
}

void unscale(lprec *lp)
{
  int i, j;
  
  if(lp->scaling_used)
    {

      /* unscale mat */
      for(j = 1; j <= lp->columns; j++)
        for(i = lp->col_end[j - 1]; i < lp->col_end[j]; i++)
          lp->mat[i].value /= lp->scale[lp->rows + j];

      /* unscale bounds */
      for(i = lp->rows + 1; i < lp->sum; i++)
	{
	  if(lp->orig_lowbo[i] != 0)
	    lp->orig_lowbo[i] *= lp->scale[i];
	  if(lp->orig_upbo[i] != lp->infinite)
	    lp->orig_upbo[i] *= lp->scale[i];
	}
    
      /* unscale the matrix */
      for(j = 1; j <= lp->columns; j++)
        for(i = lp->col_end[j-1]; i < lp->col_end[j]; i++)
          lp->mat[i].value /= lp->scale[lp->mat[i].row_nr];

      /* unscale the rhs! */
      for(i = 0; i <= lp->rows; i++)
        lp->orig_rh[i] /= lp->scale[i];
      
      free(lp->scale);
      lp->scaling_used=FALSE;
      lp->eta_valid=FALSE;
    }
}


void auto_scale(lprec *lp)
{
  int i, j, row_nr, IntUsed;
  REAL *row_max, *row_min, *scalechange, absval;

  if(!lp->scaling_used)
    {
      MALLOC(lp->scale, lp->sum_alloc + 1, REAL);
      for(i=0; i <= lp->sum; i++)
	lp->scale[i]=1;
    }
  
  MALLOC(row_max, lp->rows + 1, REAL);
  MALLOC(row_min, lp->rows + 1, REAL);
  MALLOC(scalechange, lp->sum + 1, REAL);

  /* initialise min and max values */
  for(i = 0; i <= lp->rows; i++)
    {
      row_max[i]=0;
      row_min[i]=lp->infinite;
    }

  /* calculate min and max absolute values of rows */
  for(j = 1; j <= lp->columns; j++)
    for(i = lp->col_end[j - 1]; i < lp->col_end[j]; i++)
      {
	row_nr = lp->mat[i].row_nr;
	absval = my_abs(lp->mat[i].value);
	if(absval!=0)
	  {
	    row_max[row_nr] = my_max(row_max[row_nr], absval);
	    row_min[row_nr] = my_min(row_min[row_nr], absval);
	  }
      }    
  /* calculate scale factors for rows */
  for(i = 0; i <= lp->rows; i++)
    {
      scalechange[i] = minmax_to_scale(row_min[i], row_max[i]);
      lp->scale[i] *= scalechange[i];
    }

  /* now actually scale the matrix */
  for(j = 1; j <= lp->columns; j++)
    for(i = lp->col_end[j - 1]; i < lp->col_end[j]; i++)
      lp->mat[i].value *= scalechange[lp->mat[i].row_nr];

  /* and scale the rhs! */
  for(i = 0; i <= lp->rows; i++)
    lp->orig_rh[i] *= scalechange[i];

  free(row_max);
  free(row_min);
  
  IntUsed=FALSE;
  i=lp->rows+1;
  while(!IntUsed && i <= lp->sum)
    {
      IntUsed=lp->must_be_int[i];
      i++;
    }
  if(!IntUsed)
    {  
      REAL col_max, col_min;

      /* calculate scales */
      for(j = 1; j <= lp->columns; j++)
	{
	  col_max = 0;
	  col_min = lp->infinite;
	  for(i = lp->col_end[j - 1]; i < lp->col_end[j]; i++)
	    {
	      if(lp->mat[i].value!=0)
		{
		  col_max = my_max(col_max, my_abs(lp->mat[i].value));
		  col_min = my_min(col_min, my_abs(lp->mat[i].value));
		}
	    }
	  scalechange[lp->rows + j]  = minmax_to_scale(col_min, col_max);
	  lp->scale[lp->rows + j] *= scalechange[lp->rows + j];
	}

      /* scale mat */
      for(j = 1; j <= lp->columns; j++)
	for(i = lp->col_end[j - 1]; i < lp->col_end[j]; i++)
	  lp->mat[i].value *= scalechange[lp->rows + j];

      /* scale bounds as well */
      for(i = lp->rows + 1; i < lp->sum; i++)
	{
	  if(lp->orig_lowbo[i] != 0)
	    lp->orig_lowbo[i] /= scalechange[i];
	  if(lp->orig_upbo[i] != lp->infinite)
	    lp->orig_upbo[i] /= scalechange[i];
	}
      lp->columns_scaled=TRUE;
    }
  free(scalechange);
  lp->scaling_used=TRUE;
  lp->eta_valid=FALSE;
}

void reset_basis(lprec *lp)
{
  lp->basis_valid=FALSE;
}

void print_solution(lprec *lp)
{
  int i;

  fprintf(stdout, "Value of objective function: %16.10g\n",
	  (double)lp->best_solution[0]);

  /* print normal variables */
  for(i = 1; i <= lp->columns; i++)
    if(lp->names_used)
      fprintf(stdout, "%-10s%16.5g\n", lp->col_name[i],
	      (double)lp->best_solution[lp->rows+i]);
    else
      fprintf(stdout, "Var [%4d]  %16.5g\n", i,
	      (double)lp->best_solution[lp->rows+i]);

  /* print achieved constraint values */
  if(lp->verbose)
    {
      fprintf(stdout, "\nActual values of the constraints:\n");
      for(i = 1; i <= lp->rows; i++)
        if(lp->names_used)
	  fprintf(stdout, "%-10s%16.5g\n", lp->row_name[i],
		  (double)lp->best_solution[i]);
        else
          fprintf(stdout, "Row [%4d]  %16.5g\n", i,
		  (double)lp->best_solution[i]);  
    }

  if((lp->verbose || lp->print_duals))
    {
      if(lp->max_level != 1)
        fprintf(stdout,
		"These are the duals from the node that gave the optimal solution.\n");
      else
        fprintf(stdout, "\nDual values:\n");
      for(i = 1; i <= lp->rows; i++)
        if(lp->names_used)
	  fprintf(stdout, "%-10s%16.5g\n", lp->row_name[i],
		  (double)lp->duals[i]);
        else
          fprintf(stdout, "Row [%4d]  %16.5g\n", i, (double)lp->duals[i]); 
    }
} /* Printsolution */

void write_LP(lprec *lp, FILE *output)
{
  int i, j;
  REAL *row;
  
  MALLOC(row, lp->columns+1, REAL);
  if(lp->maximise)
    fprintf(output, "max:");
  else
    fprintf(output, "min:");

  get_row(lp, 0, row);
  for(i = 1; i <= lp->columns; i++)
    if(row[i] != 0)
      {
        if(row[i] == -1)
          fprintf(output, " -");
        else if(row[i] == 1)
          fprintf(output, " +");
        else 
          fprintf(output, " %+g ", row[i]);
        if(lp->names_used)
          fprintf(output, "%s", lp->col_name[i]);
        else
          fprintf(output, "x%d", i);
      }
  fprintf(output, ";\n");

  for(j = 1; j <= lp->rows; j++)
    {
      if(lp->names_used)
        fprintf(output, "%s:", lp->row_name[j]);
      get_row(lp, j, row);
      for(i = 1; i <= lp->columns; i++)
        if(row[i] != 0)
          {
            if(row[i] == -1)
              fprintf(output, " -");
            else if(row[i] == 1)
              fprintf(output, " +");
            else 
              fprintf(output, " %+g ", row[i]);
            if(lp->names_used)
              fprintf(output, "%s", lp->col_name[i]);
            else
              fprintf(output, "x%d", i);
          }
      if(lp->orig_upbo[j] == 0)
        fprintf(output, " =");
      else if(lp->ch_sign[j])
        fprintf(output, " >");
      else
        fprintf(output, " <");
      if(lp->ch_sign[j])
        fprintf(output, " %g;\n",-lp->orig_rh[j]);
      else
        fprintf(output, " %g;\n", lp->orig_rh[j]);
    }
  for(i = lp->rows+1; i <= lp->sum; i++)
    {
      if(lp->orig_lowbo[i] != 0)
	{
	  if(lp->names_used)
	    fprintf(output, "%s > %g;\n", lp->col_name[i - lp->rows],
		    lp->orig_lowbo[i]);
	  else
	    fprintf(output, "x%d > %g;\n", i - lp->rows,
		    lp->orig_lowbo[i]);
	}
      if(lp->orig_upbo[i] != lp->infinite)
	{
	  if(lp->names_used)
	    fprintf(output, "%s < %g;\n", lp->col_name[i - lp->rows],
		    lp->orig_upbo[i]);
	  else
	    fprintf(output, "x%d < %g;\n", i - lp->rows, lp->orig_upbo[i]);
	}
    }


  i=1;
  while(!lp->must_be_int[lp->rows+i]  && i <= lp->columns)
    i++;
  if(i <= lp->columns)
    {
      if(lp->names_used)  
        fprintf(output, "\nint %s", lp->col_name[i]);
      else
        fprintf(output, "\nint x%d", i);
      i++;
      for(; i <= lp->columns; i++)
        if(lp->must_be_int[lp->rows+i])
          if(lp->names_used)  
            fprintf(output, ",%s", lp->col_name[i]);
          else
            fprintf(output, ", x%d", i);
      fprintf(output, ";\n");
    }
  free(row);
}




void write_MPS(lprec *lp, FILE *output)
{
  int i, j, marker;
  REAL *column;


  MALLOC(column, lp->rows+1, REAL);
  marker=0;   
  fprintf(output, "NAME          %s\n", lp->lp_name);
  fprintf(output, "ROWS\n");
  for(i = 0; i <= lp->rows; i++)
    {
      if(i==0)
	fprintf(output, " N  ");
      else
	if(lp->orig_upbo[i]==lp->infinite)
	  if(lp->ch_sign[i])
	    fprintf(output, " G  ");
	  else
	    fprintf(output, " L  ");
	else
	  fprintf(output, " E  ");
      if(lp->names_used)
	fprintf(output, "%s\n", lp->row_name[i]);
      else
	fprintf(output, "r_%d\n", i);
    }
      
  fprintf(output, "COLUMNS\n");
  j = 0;
  for(i = 1; i <= lp->columns; i++)
    {
      if((lp->must_be_int[i+lp->rows]) && (marker % 2)==0)
	{
	  fprintf(output,
		  "    MARK%04d  'MARKER'                 'INTORG'\n",
		  marker);
	  marker++;
	}
      if((!lp->must_be_int[i+lp->rows]) && (marker % 2)==1)
	{
	  fprintf(output,
		  "    MARK%04d  'MARKER'                 'INTEND'\n",
		  marker);
	  marker++;
	}
      get_column(lp, i, column);
      j=0;
      if(lp->maximise)
	{
	  if(column[j] != 0)
	    { 
	      if(lp->names_used)
		fprintf(output, "    %-8s  %-8s  %g\n", lp->col_name[i],
			lp->row_name[j], -column[j]);
	      else
		fprintf(output, "    var_%-4d  r_%-6d  %g\n", i, j,
			-column[j]);
	    }
	} 
      else
	{
	  if(column[j] != 0)
	    { 
	      if(lp->names_used)
		fprintf(output, "    %-8s  %-8s  %g\n", lp->col_name[i],
			lp->row_name[j], column[j]);
	      else
		fprintf(output, "    var_%-4d  r_%-6d  %g\n", i, j,
			column[j]);
	    }
	}
      for(j=1; j <= lp->rows; j++)
	if(column[j] != 0)
	  { 
	    if(lp->names_used)
	      fprintf(output, "    %-8s  %-8s  %g\n", lp->col_name[i],
		      lp->row_name[j], column[j]);
	    else
	      fprintf(output, "    var_%-4d  r_%-6d  %g\n", i, j,
		      column[j]);
	  }
    }
  if((marker % 2) ==1)
    {
      fprintf(output, "    MARK%04d  'MARKER'                 'INTEND'\n",
	      marker);
      marker++;
    }

  fprintf(output, "RHS\n");
  for(i = 1; i <= lp->rows; i++)
    {
      if(lp->ch_sign[i])
	{
	  if(lp->names_used)
	    fprintf(output, "    RHS       %-8s  %g\n", lp->row_name[i],
		    (double)-lp->orig_rh[i]);
	  else
	    fprintf(output, "    RHS       r_%-6d  %g\n", i,
		    (double)-lp->orig_rh[i]);
	}
      else
	{
	  if(lp->names_used)
	    fprintf(output, "    RHS       %-8s  %g\n", lp->row_name[i],
		    (double)lp->orig_rh[i]);
	  else
	    fprintf(output, "    RHS       r_%-6d  %g\n", i,
		    (double)lp->orig_rh[i]);
	}
    }
      
  fprintf(output, "BOUNDS\n");
  if(lp->names_used)
    for(i = lp->rows + 1; i <= lp->sum; i++)
      {
	if(lp->orig_upbo[i] < lp->infinite)
	  fprintf(output, " UP BND       %-8s  %g\n",
		  lp->col_name[i- lp->rows], (double)lp->orig_upbo[i]);
	if(lp->orig_lowbo[i] != 0)
	  fprintf(output, " LO BND       %-8s  %g\n",
		  lp->col_name[i- lp->rows], (double)lp->orig_lowbo[i]);
      }
  else
    for(i = lp->rows + 1; i <= lp->sum; i++)
      {
	if(lp->orig_upbo[i] < lp->infinite)
	  fprintf(output, " UP BND       var_%-4d  %g\n",
		  i - lp->rows, (double)lp->orig_upbo[i]);
	if(lp->orig_lowbo[i] != 0)
	  fprintf(output, " LO BND       var_%-4d  %g\n", i - lp->rows,
		  (double)lp->orig_lowbo[i]);
      }
  fprintf(output, "ENDATA\n");
  free(column);
}

void print_duals(lprec *lp)
{
  int i;
  for(i = 1; i <= lp->rows; i++)
    if(lp->names_used)
      fprintf(stdout, "%10s [%3d] % 10.4f\n", lp->row_name[i], i,
	      lp->duals[i]);
    else
      fprintf(stdout, "Dual       [%3d] % 10.4f\n", i, lp->duals[i]);
}

void print_scales(lprec *lp)
{
  int i;
  if(lp->scaling_used)
    {
      for(i = 0; i <= lp->rows; i++)
        fprintf(stdout, "Row[%3d]    scaled at % 10.6f\n", i, lp->scale[i]);
      for(i = 1; i <= lp->columns; i++)
        fprintf(stdout, "Column[%3d] scaled at % 10.6f\n", i,
		lp->scale[lp->rows + i]);
    }
}
