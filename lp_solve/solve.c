#include <stdlib.h>
#include <string.h>
#include "lpkit.h"
#include "lpglob.h"
#include "debug.h"

/* Globals used by solver */
short JustInverted;
short Status;
short Doiter;
short DoInvert;
short Break_bb;

void set_globals(lprec *lp)
{
  if(Lp != NULL)
    Lp->active = FALSE;
  Lp = lp;
  Rows = lp->rows;
  Columns = lp->columns;
  Sum = Rows + Columns;
  Non_zeros = lp->non_zeros;
  Mat = lp->mat;
  Col_no = lp->col_no;
  Col_end = lp->col_end;
  Row_end = lp->row_end;
  Rh = lp->rh;
  Rhs = lp->rhs;
  Orig_rh = lp->orig_rh;
  Must_be_int = lp->must_be_int;
  Orig_upbo = lp->orig_upbo;
  Orig_lowbo = lp->orig_lowbo;
  Upbo = lp->upbo;
  Lowbo = lp->lowbo;
  Bas = lp->bas;
  Basis = lp->basis;
  Lower = lp->lower;
  Eta_alloc = lp->eta_alloc;
  Eta_size = lp->eta_size;
  Num_inv = lp->num_inv;
  Eta_value = lp->eta_value;
  Eta_row_nr = lp->eta_row_nr;
  Eta_col_end = lp->eta_col_end;
  Solution = lp->solution;
  Best_solution = lp->best_solution;
  Infinite = lp->infinite;
  Epsilon = lp->epsilon;
  Epsb = lp->epsb;
  Epsd = lp->epsd;
  Epsel = lp->epsel;

  /* ?? MB */
  TREJ = TREJ;
  TINV = TINV;

  Maximise = lp->maximise;
  Floor_first = lp->floor_first;
  lp->active = TRUE;
}

void ftran(int start,
	   int end,
	   REAL *pcol)
{
  int  i, j, k, r;
  REAL theta;

  for(i = start; i <= end; i++)
    {
      k = Eta_col_end[i] - 1;
      r = Eta_row_nr[k];
      theta = pcol[r];
      if(theta != 0)
	for(j = Eta_col_end[i - 1]; j < k; j++)
	  pcol[Eta_row_nr[j]] += theta * Eta_value[j]; /* cpu expensive line */
      pcol[r] *= Eta_value[k];
    }
  /* round small values to zero */
  for(i = 0; i <= Rows; i++)
    my_round(pcol[i], Epsel);
} /* ftran */

void btran(REAL *row)
{
  int  i, j, k;
  REAL f;

  for(i = Eta_size; i >= 1; i--)
    {
      f = 0;
      k = Eta_col_end[i] - 1;
      for(j = Eta_col_end[i - 1]; j <= k; j++)
	f += row[Eta_row_nr[j]] * Eta_value[j];
      my_round(f, Epsel);
      row[Eta_row_nr[k]] = f;
    }
} /* btran */


static short Isvalid(lprec *lp)
{
  int i, j, *rownum, *colnum;
  int *num, row_nr;

  if(!lp->row_end_valid)
    {
      MALLOC(num, lp->rows + 1, int);
      MALLOC(rownum, lp->rows + 1, int);
      for(i = 0; i <= lp->rows; i++)
        {
          num[i] = 0;
          rownum[i] = 0;
        }
      for(i = 0; i < lp->non_zeros; i++)
        rownum[lp->mat[i].row_nr]++;
      lp->row_end[0] = 0;
      for(i = 1; i <= lp->rows; i++)
        lp->row_end[i] = lp->row_end[i - 1] + rownum[i];
      for(i = 1; i <= lp->columns; i++)
        for(j = lp->col_end[i - 1]; j < lp->col_end[i]; j++)
          {
    	    row_nr = lp->mat[j].row_nr;
	    if(row_nr != 0)
	      {
	        num[row_nr]++;
	        lp->col_no[lp->row_end[row_nr - 1] + num[row_nr]] = i;
	      }
          }
      free(num);
      free(rownum);
      lp->row_end_valid = TRUE;
    }
  if(lp->valid)
    return(TRUE);
  CALLOC(rownum, lp->rows + 1, int);
  CALLOC(colnum, lp->columns + 1, int);
  for(i = 1 ; i <= lp->columns; i++)
    for(j = lp->col_end[i - 1]; j < lp->col_end[i]; j++)
      {
        colnum[i]++;
        rownum[lp->mat[j].row_nr]++;
      }
  for(i = 1; i <= lp->columns; i++)
    if(colnum[i] == 0)
      if(lp->names_used)
        fprintf(stderr, "Warning: Variable %s not used in any constraints\n",
		lp->col_name[i]);
      else
        fprintf(stderr, "Warning: Variable %d not used in any constaints\n",
		i);
  free(rownum);
  free(colnum);
  lp->valid = TRUE;
  return(TRUE);
} 

static void resize_eta(void)
{
  Eta_alloc *= 2;
  REALLOC(Eta_value, Eta_alloc, REAL);
  Lp->eta_value = Eta_value;
  REALLOC(Eta_row_nr, Eta_alloc, int);
  Lp->eta_row_nr = Eta_row_nr;
} /* resize_eta */


static void condensecol(int row_nr,
			REAL *pcol)
{
  int i, elnr;
  
  elnr = Eta_col_end[Eta_size];

  if(elnr + Rows + 2 > Eta_alloc) /* maximum local growth of Eta */
    resize_eta();

  for(i = 0; i <= Rows; i++)
    if(i != row_nr && pcol[i] != 0)
      {
	Eta_row_nr[elnr] = i;
	Eta_value[elnr] = pcol[i];
	elnr++;
      }
  Eta_row_nr[elnr] = row_nr;
  Eta_value[elnr] = pcol[row_nr];
  elnr++;
  Eta_col_end[Eta_size + 1] = elnr;
} /* condensecol */


static void addetacol(void)
{
  int  i, j, k;
  REAL theta;
  
  j = Eta_col_end[Eta_size];
  Eta_size++;
  k = Eta_col_end[Eta_size];
  theta = 1 / (REAL) Eta_value[k - 1];
  Eta_value[k - 1] = theta;
  for(i = j; i < k - 1; i++)
    Eta_value[i] *= -theta;
  JustInverted = FALSE;
} /* addetacol */

static void setpivcol(short lower, 
		      int varin,
		      REAL   *pcol)
{
  int  i, colnr;
  REAL f;
  
  if(lower)
    f = 1;
  else
    f = -1;
  for(i = 0; i <= Rows; i++)
    pcol[i] = 0;
  if(varin > Rows)
    {
      colnr = varin - Rows;
      for(i = Col_end[colnr - 1]; i < Col_end[colnr]; i++)
	pcol[Mat[i].row_nr] = Mat[i].value * f;
      pcol[0] -= Extrad * f;
    }
  else
    if(lower)
      pcol[varin] = 1;
    else
      pcol[varin] = -1;
  ftran(1, Eta_size, pcol);
} /* setpivcol */


static void minoriteration(int colnr,
			   int row_nr)
{
  int  i, j, k, wk, varin, varout, elnr;
  REAL piv = 0, theta;
  
  varin = colnr + Rows;
  elnr = Eta_col_end[Eta_size];
  wk = elnr;
  Eta_size++;
  if(Extrad != 0)
    {
      Eta_row_nr[elnr] = 0;
      Eta_value[elnr] = -Extrad;
      elnr++;
    }
  for(j = Col_end[colnr - 1] ; j < Col_end[colnr]; j++)
    {
      k = Mat[j].row_nr;
      if(k == 0 && Extrad != 0)
        Eta_value[Eta_col_end[Eta_size -1]] += Mat[j].value;
      else if(k != row_nr)
	{
	  Eta_row_nr[elnr] = k;
	  Eta_value[elnr] = Mat[j].value;
	  elnr++;
	}
      else
	piv = Mat[j].value;
    }
  Eta_row_nr[elnr] = row_nr;
  Eta_value[elnr] = 1 / (REAL) piv;
  elnr++;
  theta = Rhs[row_nr] / (REAL) piv;
  Rhs[row_nr] = theta;
  for(i = wk; i < elnr - 1; i++)
    Rhs[Eta_row_nr[i]] -= theta * Eta_value[i];
  varout = Bas[row_nr];
  Bas[row_nr] = varin;
  Basis[varout] = FALSE;
  Basis[varin] = TRUE;
  for(i = wk; i < elnr - 1; i++)
    Eta_value[i] /= - (REAL) piv;
  Eta_col_end[Eta_size] = elnr;
} /* minoriteration */

static void rhsmincol(REAL theta,
		      int row_nr,
		      int varin)
{
  int  i, j, k, varout;
  REAL f;
  
  if(row_nr > Rows + 1)
    {
      fprintf(stderr, "Error: rhsmincol called with row_nr: %d, rows: %d\n",
	      row_nr, Rows);
      fprintf(stderr, "This indicates numerical instability\n");
      exit(1);
    }
  j = Eta_col_end[Eta_size];
  k = Eta_col_end[Eta_size + 1];
  for(i = j; i < k; i++)
    {
      f = Rhs[Eta_row_nr[i]] - theta * Eta_value[i];
      my_round(f, Epsb);
      Rhs[Eta_row_nr[i]] = f;
    }
  Rhs[row_nr] = theta;
  varout = Bas[row_nr];
  Bas[row_nr] = varin;
  Basis[varout] = FALSE;
  Basis[varin] = TRUE;
} /* rhsmincol */


void invert(void)
{
  int    i, j, v, wk, numit, varnr, row_nr, colnr, varin;
  REAL   theta;
  REAL   *pcol;
  short  *frow;
  short  *fcol;
  int    *rownum, *col, *row;
  int    *colnum;

  if(Lp->print_at_invert) 
    fprintf(stderr, "Start Invert iter %7d eta_size %4d rhs[0] %16.4f \n",
	    Lp->iter, Eta_size,-Rhs[0]); 
 
  CALLOC(rownum, Rows + 1, int);
  CALLOC(col, Rows + 1, int);
  CALLOC(row, Rows + 1, int);
  CALLOC(pcol, Rows + 1, REAL);
  CALLOC(frow, Rows + 1, short);
  CALLOC(fcol, Columns + 1, short);
  CALLOC(colnum, Columns + 1, int);
 
  for(i = 0; i <= Rows; i++)
    frow[i] = TRUE;

  for(i = 0; i < Columns; i++)
    fcol[i] = FALSE;

  for(i = 0; i < Rows; i++)
    rownum[i] = 0;

  for(i = 0; i <= Columns; i++)
    colnum[i] = 0;

  for(i = 0; i <= Rows; i++)
    if(Bas[i] > Rows)
      fcol[Bas[i] - Rows - 1] = TRUE;
    else
      frow[Bas[i]] = FALSE;

  for(i = 1; i <= Rows; i++)
    if(frow[i])
      for(j = Row_end[i - 1] + 1; j <= Row_end[i]; j++)
	{
	  wk = Col_no[j];
	  if(fcol[wk - 1])
	    {
	      colnum[wk]++;
	      rownum[i - 1]++;
	    }
	}
  for(i = 1; i <= Rows; i++)
    Bas[i] = i;
  for(i = 1; i <= Rows; i++)
    Basis[i] = TRUE;
  for(i = 1; i <= Columns; i++)
    Basis[i + Rows] = FALSE;

  for(i = 0; i <= Rows; i++)
    Rhs[i] = Rh[i];

  for(i = 1; i <= Columns; i++)
    {
      varnr = Rows + i;
      if(!Lower[varnr])
	{
	  theta = Upbo[varnr];
	  for(j = Col_end[i - 1]; j < Col_end[i]; j++)
	    Rhs[Mat[j].row_nr] -= theta * Mat[j].value;
	}
    }
  for(i = 1; i <= Rows; i++)
    if(!Lower[i])
      Rhs[i] -= Upbo[i];
  Eta_size = 0;
  v = 0;
  row_nr = 0;
  Num_inv = 0;
  numit = 0;
  while(v < Rows)
    {
      row_nr++;
      if(row_nr > Rows)
	row_nr = 1;
      v++;
      if(rownum[row_nr - 1] == 1)
	if(frow[row_nr])
	  {
	    v = 0;
	    j = Row_end[row_nr - 1] + 1;
	    while(!(fcol[Col_no[j] - 1]))
	      j++;
	    colnr = Col_no[j];
	    fcol[colnr - 1] = FALSE;
	    colnum[colnr] = 0;
	    for(j = Col_end[colnr - 1]; j < Col_end[colnr]; j++)
	      if(frow[Mat[j].row_nr])
		rownum[Mat[j].row_nr - 1]--;
	    frow[row_nr] = FALSE;
	    minoriteration(colnr, row_nr);
	  }
    }
  v = 0;
  colnr = 0;
  while(v <Columns)
    {
      colnr++;
      if(colnr > Columns)
	colnr = 1;
      v++;
      if(colnum[colnr] == 1)
	if(fcol[colnr - 1])
	  {
	    v = 0;
	    j = Col_end[colnr - 1] + 1;
	    while(!(frow[Mat[j - 1].row_nr]))
	      j++;
	    row_nr = Mat[j - 1].row_nr;
	    frow[row_nr] = FALSE;
	    rownum[row_nr - 1] = 0;
	    for(j = Row_end[row_nr - 1] + 1; j <= Row_end[row_nr]; j++)
	      if(fcol[Col_no[j] - 1])
		colnum[Col_no[j]]--;
	    fcol[colnr - 1] = FALSE;
	    numit++;
	    col[numit - 1] = colnr;
	    row[numit - 1] = row_nr;
	  }
    }
  for(j = 1; j <= Columns; j++)
    if(fcol[j - 1])
      {
	fcol[j - 1] = FALSE;
	setpivcol(Lower[Rows + j], j + Rows, pcol);
	row_nr = 1;
	while((!(frow[row_nr] && pcol[row_nr])) && row_nr <= Rows)
	  row_nr++; /* this sometimes sets row_nr to Rows + 1 and makes
		       rhsmincol crash. Solved in 2.0? MB */
        if(row_nr == Rows + 1)
          error("Inverting failed");
	frow[row_nr] = FALSE;
        condensecol(row_nr, pcol);
	theta = Rhs[row_nr] / (REAL) pcol[row_nr];
	rhsmincol(theta, row_nr, Rows + j);
	addetacol();
      }
  for(i = numit - 1; i >= 0; i--)
    {
      colnr = col[i];
      row_nr = row[i];
      varin = colnr + Rows;
      for(j = 0; j <= Rows; j++)
	pcol[j] = 0;
      for(j = Col_end[colnr - 1]; j < Col_end[colnr]; j++)
	pcol[Mat[j].row_nr] = Mat[j].value;
      pcol[0] -= Extrad;
      condensecol(row_nr, pcol);
      theta = Rhs[row_nr] / (REAL) pcol[row_nr];
      rhsmincol(theta, row_nr, varin);
      addetacol();
    }
  for(i = 1; i <= Rows; i++)
    my_round(Rhs[i], Epsb);
  if(Lp->print_at_invert) 
    fprintf(stderr,
	    "End Invert                eta_size %4d rhs[0] %16.4f\n",
	    Eta_size,-Rhs[0]); 

  JustInverted = TRUE;
  DoInvert = FALSE;
  free(rownum);
  free(col);
  free(row);
  free(pcol);
  free(frow);
  free(fcol);
  free(colnum);
} /* invert */

static short colprim(int *colnr,
		     short minit,
		     REAL   *drow)
{
  int  varnr, i, j;
  REAL f, dpiv;
  
  dpiv = -Epsd;
  (*colnr) = 0;
  if(!minit)
    {
      for(i = 1; i <= Sum; i++)
	drow[i] = 0;
      drow[0] = 1;
      btran(drow);
      for(i = 1; i <= Columns; i++)
	{
	  varnr = Rows + i;
	  if(!Basis[varnr])
	    if(Upbo[varnr] > 0)
	      {
		f = 0;
		for(j = Col_end[i - 1]; j < Col_end[i]; j++)
		  f += drow[Mat[j].row_nr] * Mat[j].value;
		drow[varnr] = f;
	      }
	}
      for(i = 1; i <= Sum; i++)
        my_round(drow[i], Epsd);
    }
  for(i = 1; i <= Sum; i++)
    if(!Basis[i])
      if(Upbo[i] > 0)
	{
	  if(Lower[i])
	    f = drow[i];
	  else
	    f = -drow[i];
	  if(f < dpiv)
	    {
	      dpiv = f;
	      (*colnr) = i;
	    }
	}
  if(Lp->trace)
    if((*colnr)>0)
      fprintf(stderr, "col_prim:%7d, reduced cost: % 18.10f\n",
	      (*colnr), dpiv);
    else
      fprintf(stderr,
	      "col_prim: no negative reduced costs found, optimality!\n");
  if(*colnr == 0)
    {
      Doiter   = FALSE;
      DoInvert = FALSE;
      Status   = OPTIMAL;
    }
  return((*colnr) > 0);
} /* colprim */

static short rowprim(int colnr,
                     int *row_nr,
		     REAL *theta,
		     REAL *pcol)
{
  int  i;
  REAL f, quot; 

  (*row_nr) = 0;
  (*theta) = Infinite;
  for(i = 1; i <= Rows; i++)
    {
      f = pcol[i];
      if(my_abs(f) < TREJ)
        f = 0;
      if(f != 0)
	{
          quot = 2 * Infinite;
	  if(f > 0)
	    quot = Rhs[i] / (REAL) f;
	  else
	    if(Upbo[Bas[i]] < Infinite)
	      quot = (Rhs[i] - Upbo[Bas[i]]) / (REAL) f;
          my_round(quot, Epsel);
	  if(quot < (*theta))
	    {
              (*theta) = quot;
	      (*row_nr) = i;
	    }
	}
    }
  if((*row_nr) == 0)  
    for(i = 1; i <= Rows; i++)
      {
        f = pcol[i];
        if(f != 0)
	  {
            quot = 2 * Infinite;
	    if(f > 0)
	      quot = Rhs[i] / (REAL) f;
	    else
	      if(Upbo[Bas[i]] < Infinite)
	        quot = (Rhs[i] - Upbo[Bas[i]]) / (REAL) f;
            my_round(quot, Epsel);
	    if(quot < (*theta))
	      {
                (*theta) = quot;
	        (*row_nr) = i;
	      }
	  }
      }

  if((*theta) < 0)
    {
      fprintf(stderr, "Warning: Numerical instability, qout = %f\n", (*theta));
      fprintf(stderr, "pcol[%d] = % 18.10f, rhs[%d] = % 18.8f , upbo = % f\n",
	      (*row_nr), f, (*row_nr), Rhs[(*row_nr)], Upbo[Bas[(*row_nr)]]);
    }
  if((*row_nr) == 0)
    {
      if(Upbo[colnr] == Infinite)
        {
          Doiter   = FALSE;
          DoInvert = FALSE;
          Status   = UNBOUNDED;
        }
      else
        {
          i = 1;
          while(pcol[i] >= 0 && i <= Rows)
            i++;
          if(i > Rows) /* empty column with upperbound! */
            {
              Lower[colnr] = FALSE;
              Rhs[0] += Upbo[colnr]*pcol[0];
              Doiter = FALSE;
              DoInvert = FALSE;
            }
          else if(pcol[i]<0)
            {
              (*row_nr) = i;
            }
        }
    }
  if((*row_nr) > 0)
    Doiter = TRUE;
  if(Lp->trace)
    fprintf(stderr, "row_prim:%7d, pivot element:% 18.10f\n", (*row_nr),
	    pcol[(*row_nr)]);

  return((*row_nr) > 0);
} /* rowprim */

static short rowdual(int *row_nr)
{
  int   i;
  REAL  f, g, minrhs;
  short artifs;

  (*row_nr) = 0;
  minrhs = -Epsb;
  i = 0;
  artifs = FALSE;
  while(i < Rows && !artifs)
    {
      i++;
      f = Upbo[Bas[i]];
      if(f == 0 && (Rhs[i] != 0))	
	{
	  artifs = TRUE;
	  (*row_nr) = i;
        }
      else
	{
	  if(Rhs[i] < f - Rhs[i])
	    g = Rhs[i];
	  else
	    g = f - Rhs[i];
	  if(g < minrhs)
	    {
	      minrhs = g;
	      (*row_nr) = i;
	    }
	}
    }

  if(Lp->trace)
    {  
      if((*row_nr)>0)
        { 
          fprintf(stderr,
		  "row_dual:%7d, rhs of selected row:           % 18.10f\n",
		  (*row_nr), Rhs[(*row_nr)]);
          if(Upbo[Bas[(*row_nr)]] < Infinite)
            fprintf(stderr,
		    "               upper bound of basis variable:    % 18.10f\n",
		    Upbo[Bas[(*row_nr)]]);
        }
      else
        fprintf(stderr, "row_dual: no infeasibilities found\n");
    }
    
  return((*row_nr)>0);
} /* rowdual */

static short coldual(int row_nr,
		     int *colnr,
		     short minit,
		     REAL *prow,
		     REAL *drow)
{
  int  i, j, r, varnr;
  REAL theta, quot, pivot, d, f, g;
  
  Doiter = FALSE;
  if(!minit)
    {
      for(i = 0; i <= Rows; i++)
	{
	  prow[i] = 0;
	  drow[i] = 0;
	}
      drow[0] = 1;
      prow[row_nr] = 1;
      for(i = Eta_size; i >= 1; i--)
	{
	  d = 0;
	  f = 0;
	  r = Eta_row_nr[Eta_col_end[i] - 1];
	  for(j = Eta_col_end[i - 1]; j < Eta_col_end[i]; j++)
	    {
	      /* this is where the program consumes most cpu time */
	      f += prow[Eta_row_nr[j]] * Eta_value[j];
	      d += drow[Eta_row_nr[j]] * Eta_value[j];
	    }
          my_round(f, Epsel);
	  prow[r] = f;
          my_round(d, Epsel);
	  drow[r] = d;
	}
      for(i = 1; i <= Columns; i++)
	{
	  varnr = Rows + i;
	  if(!Basis[varnr])
	    {
	      d = - Extrad * drow[0];
	      f = 0;
	      for(j = Col_end[i - 1]; j < Col_end[i]; j++)
		{
		  d = d + drow[Mat[j].row_nr] * Mat[j].value;
		  f = f + prow[Mat[j].row_nr] * Mat[j].value;
		}
	      drow[varnr] = d;
	      prow[varnr] = f;
	    }
	}
      for(i = 0; i <= Sum; i++)
	{
          my_round(prow[i], Epsel);
          my_round(drow[i], Epsd);
	}
    }
  if(Rhs[row_nr] > Upbo[Bas[row_nr]])
    g = -1;
  else
    g = 1;
  pivot = 0;
  (*colnr) = 0;
  theta = Infinite;
  for(i = 1; i <= Sum; i++)
    {
      if(Lower[i])
	d = prow[i] * g;
      else
	d = -prow[i] * g;
      if((d < 0) && (!Basis[i]) && (Upbo[i] > 0))
	{
	  if(Lower[i])
	    quot = -drow[i] / (REAL) d;
	  else
	    quot = drow[i] / (REAL) d;
	  if(quot < theta)
	    {
	      theta = quot;
	      pivot = d;
	      (*colnr) = i;
	    }
	  else if((quot == theta) && (my_abs(d) > my_abs(pivot)))
	    {
	      pivot = d;
	      (*colnr) = i;
	    }
	}
    }
  if(Lp->trace)
    fprintf(stderr, "col_dual:%7d, pivot element:  % 18.10f\n", (*colnr),
	    prow[(*colnr)]);

  if((*colnr)>0)
    Doiter = TRUE;

  return((*colnr) > 0);
} /* coldual */

static void iteration(int row_nr,
		      int varin,
		      REAL *theta,
		      REAL up,
		      short *minit,
		      short *low,
		      short primal,
                      REAL *pcol)
{
  int  i, k, varout;
  REAL f;
  REAL pivot;
  
  Lp->iter++;
  (*minit) = (*theta) > (up + Epsb);
  if((*minit))
    {
      (*theta) = up;
      (*low) = !(*low);
    }
  k = Eta_col_end[Eta_size + 1];
  pivot = Eta_value[k - 1];
  for(i = Eta_col_end[Eta_size]; i < k; i++)
    {
      f = Rhs[Eta_row_nr[i]] - (*theta) * Eta_value[i];
      my_round(f, Epsb);
      Rhs[Eta_row_nr[i]] = f;
    }
  if(!(*minit))
    {
      Rhs[row_nr] = (*theta);
      varout = Bas[row_nr];
      Bas[row_nr] = varin;
      Basis[varout] = FALSE;
      Basis[varin] = TRUE;
      if(primal && pivot < 0)
	Lower[varout] = FALSE;
      if(!(*low) && up < Infinite)
	{
	  (*low) = TRUE;
	  Rhs[row_nr] = up - Rhs[row_nr];
	  for(i = Eta_col_end[Eta_size]; i < k; i++)
	    Eta_value[i] = -Eta_value[i];
	}
      addetacol();
      Num_inv++;
    }
  if(Lp->trace)
    {
      fprintf(stderr, "Theta = %16.4g ", (*theta));
      if((*minit))
        {
          if(!Lower[varin])
            fprintf(stderr,
		    "Iteration:%6d, variable%5d changed from 0 to its upper bound of %12f\n",
		    Lp->iter, varin, Upbo[varin]);
          else
            fprintf(stderr,
		    "Iteration:%6d, variable%5d changed its upper bound of %12f to 0\n",
		    Lp->iter, varin, Upbo[varin]);
        }
      else
        fprintf(stderr,
		"Iteration:%6d, variable%5d entered basis at:% 18.10f\n",
		Lp->iter, varin, Rhs[row_nr]);
      if(!primal)
	{
	  f = 0;
	  for(i = 1; i <= Rows; i++)
	    if(Rhs[i] < 0)
	      f -= Rhs[i];
	    else
	      if(Rhs[i] > Upbo[Bas[i]])
		f += Rhs[i] - Upbo[Bas[i]];
	  fprintf(stderr, "feasibility gap of this basis:% 18.10f\n",
		  (double)f);
	}
      else
	fprintf(stderr,
		"objective function value of this feasible basis: % 18.10f\n",
		(double)Rhs[0]);
    }
} /* iteration */


static int solvelp(void)
{
  int    i, j, varnr;
  REAL   f, theta;
  short  primal;
  REAL   *drow, *prow, *Pcol;
  short  minit;
  int    colnr, row_nr;
  short  *test; 

  CALLOC(drow, Sum + 1, REAL);
  CALLOC(prow, Sum + 1, REAL);
  CALLOC(Pcol, Rows + 1, REAL);
  CALLOC(test, Sum +1, short); 

  Lp->iter = 0;
  minit = FALSE;
  Status = RUNNING;
  DoInvert = FALSE;
  Doiter = FALSE;
  i = 0;
  primal = TRUE;
  while(i != Rows && primal)
    {
      i++;
      primal = Rhs[i] >= 0 && Rhs[i] <= Upbo[Bas[i]];
    }
  if(Lp->trace)
    {
      if(primal)
        fprintf(stderr, "Start at feasible basis\n");
      else
        fprintf(stderr, "Start at infeasible basis\n");
    } 
  if(!primal)
    {
      drow[0] = 1;
      for(i = 1; i <= Rows; i++)
	drow[i] = 0;
      Extrad = 0;
      for(i = 1; i <= Columns; i++)
	{
	  varnr = Rows + i;
	  drow[varnr] = 0;
	  for(j = Col_end[i - 1]; j < Col_end[i]; j++)
	    if(drow[Mat[j].row_nr] != 0)
	      drow[varnr] += drow[Mat[j].row_nr] * Mat[j].value;
          if(drow[varnr] < Extrad)
            Extrad = drow[varnr];
	}
    }
  else
    Extrad = 0;
  if(Lp->trace)
    fprintf(stderr, "Extrad = %f\n", Extrad);
  minit = FALSE;

  while(Status == RUNNING)
    {
      Doiter = FALSE;
      DoInvert = FALSE;

      if(primal)
        {
          if(colprim(&colnr, minit, drow))
	    {
	      setpivcol(Lower[colnr], colnr, Pcol);
	      if(rowprim(colnr, &row_nr, &theta, Pcol))
		condensecol(row_nr, Pcol);
	          
	    }
        }
      else /* not primal */
        {
	  if(!minit)
	    rowdual(&row_nr);
          if(row_nr > 0 )
	    {
	      if(coldual(row_nr, &colnr, minit, prow, drow))
	        {
	          setpivcol(Lower[colnr], colnr, Pcol);
                  /* getting div by zero here ... MB */
		  if(Pcol[row_nr] == 0)
	            {
		      fprintf(stderr,
			      "An attempt was made to divide by zero (Pcol[%d])\n",
			      row_nr);
		      fprintf(stderr,
			      "This indicates numerical instability\n");
                      Doiter = FALSE;
                      if(!JustInverted)
                        {
                          fprintf(stderr, "Reinverting Eta\n");
                          DoInvert = TRUE;
                        }
                      else
                        {
                          fprintf(stderr, "Can't reinvert, failure\n");
                          Status = FAILURE;
                        }
		    }
                  else
                    {
                      condensecol(row_nr, Pcol);
	              f = Rhs[row_nr] - Upbo[Bas[row_nr]];
	              if(f > 0)
	                {
	                  theta = f / (REAL) Pcol[row_nr];
	                  if(theta <= Upbo[colnr])
	                    Lower[Bas[row_nr]] = !Lower[Bas[row_nr]];
	                }
	              else /* f <= 0 */
	                theta = Rhs[row_nr] / (REAL) Pcol[row_nr];
	            }
                }
              else
                Status = INFEASIBLE;
       	    }
          else
            {
              primal   = TRUE;
              Doiter   = FALSE;
              Extrad   = 0;
              DoInvert = TRUE;
            }	  
        }
      if(Doiter)
        iteration(row_nr, colnr, &theta, Upbo[colnr], &minit, &Lower[colnr],
		  primal, Pcol);
      if(Num_inv >= Lp->max_num_inv)
        DoInvert = TRUE;
      if(DoInvert)
        {
          if(Lp->print_at_invert)
	    fprintf(stderr, "Inverting: Primal = %d\n", primal);
          invert();
        }
    } 

  Lp->total_iter += Lp->iter;
 
  free(drow);
  free(prow);
  free(Pcol);
  free(test);

  return(Status);
} /* solvelp */


static short is_int(REAL value)
{
  REAL   tmp;

  tmp = value - (REAL)floor((double)value);
  if(tmp < Epsilon)
    return(TRUE);
  if(tmp > (1 - Epsilon))
    return(TRUE);
  return(FALSE);
} /* is_int */

static void construct_solution(REAL   *sol)
{
  int    i, j, basi;
  REAL   f;

  /* zero all results of rows */
  memset(sol, '\0', (Rows + 1) * sizeof(REAL));

  if(Lp->scaling_used)
    {
      for(i = Rows + 1; i <= Sum; i++)
        sol[i] = Lowbo[i] * Lp->scale[i];
      for(i = 1; i <= Rows; i++)
        {
          basi = Bas[i];
          if(basi > Rows)
	    sol[basi] += Rhs[i] * Lp->scale[basi];
        }
      for(i = Rows + 1; i <= Sum; i++)
        if(!Basis[i] && !Lower[i])
          sol[i] += Upbo[i] * Lp->scale[i];

      for(j = 1; j <= Columns; j++)
        {
          f = sol[Rows + j];
          if(f != 0)
	    for(i = Col_end[j - 1]; i < Col_end[j]; i++)
	      sol[Mat[i].row_nr] += (f / Lp->scale[Rows+j])
		* (Mat[i].value / Lp->scale[Mat[i].row_nr]);
        }
  
      for(i = 0; i <= Rows; i++)
        {
          if(my_abs(sol[i]) < Epsb)
	    sol[i] = 0;
          else
	    if(Lp->ch_sign[i])
	      sol[i] = -sol[i];
        }
    }
  else
    {
      for(i = Rows + 1; i <= Sum; i++)
        sol[i] = Lowbo[i];
      for(i = 1; i <= Rows; i++)
        {
          basi = Bas[i];
          if(basi > Rows)
	    sol[basi] += Rhs[i];
        }
      for(i = Rows + 1; i <= Sum; i++)
        if(!Basis[i] && !Lower[i])
          sol[i] += Upbo[i];
      for(j = 1; j <= Columns; j++)
        {
          f = sol[Rows + j];
          if(f != 0)
	    for(i = Col_end[j - 1]; i < Col_end[j]; i++)
	      sol[Mat[i].row_nr] += f * Mat[i].value;
        }
  
      for(i = 0; i <= Rows; i++)
        {
          if(my_abs(sol[i]) < Epsb)
	    sol[i] = 0;
          else
	    if(Lp->ch_sign[i])
	      sol[i] = -sol[i];
        }
    }
} /* construct_solution */

static void calculate_duals(void)
{
  int i;

  /* initialise */
  for(i = 1; i <= Rows; i++)
    Lp->duals[i] = 0;
  Lp->duals[0] = 1;
  btran(Lp->duals);
  if(Lp->scaling_used)
    for(i = 1; i <= Rows; i++)
      Lp->duals[i] *= Lp->scale[i]/Lp->scale[0];

  /* the dual values are the reduced costs of the slacks */
  /* When the slack is at its upper bound, change the sign. Can this happen? */
  for(i = 1; i <= Rows; i++)
    {
      if(Lp->basis[i])
        Lp->duals[i] = 0;
      else if( Lp->ch_sign[0] == Lp->ch_sign[i])
        Lp->duals[i] = -Lp->duals[i];
    }
}

static int milpsolve(REAL   *upbo,
		     REAL   *lowbo,
		     short  *sbasis,
		     short  *slower,
		     int    *sbas)
{
  int i, j, failure, notint, is_worse;
  REAL theta, tmpreal;

  if(Break_bb)
    return(BREAK_BB);
  Level++;
  Lp->total_nodes++;
  if(Level > Lp->max_level)
    Lp->max_level = Level;
  debug_print("starting solve");
  /* make fresh copies of upbo, lowbo, rh as solving changes them */
  memcpy(Upbo,  upbo,    (Sum + 1)  * sizeof(REAL));
  memcpy(Lowbo, lowbo,   (Sum + 1)  * sizeof(REAL));
  memcpy(Basis, sbasis,  (Sum + 1)  * sizeof(short));
  memcpy(Lower, slower,  (Sum + 1)  * sizeof(short));
  memcpy(Bas,   sbas,    (Rows + 1) * sizeof(int));
  memcpy(Rh,    Orig_rh, (Rows + 1) * sizeof(REAL));

  if(Lp->anti_degen)
    {
      for(i = 1; i <= Columns; i++)
        {
          tmpreal = (REAL) (rand() % 100 * 0.00001);
	  if(tmpreal > Epsb)
            Lowbo[i + Rows] -= tmpreal;
          tmpreal = (REAL) (rand() % 100 * 0.00001);
	  if(tmpreal > Epsb)
            Upbo[i + Rows] += tmpreal;
        }
      Lp->eta_valid = FALSE;
    }

  if(!Lp->eta_valid)
    {
      for(i = 1; i <= Columns; i++)
	if(Lowbo[Rows + i] != 0)
	  {
	    theta = Lowbo[ Rows + i];
	    if(Upbo[Rows + i]<Infinite)
	      Upbo[Rows + i] -= theta;
	    for(j = Col_end[i - 1]; j < Col_end[i]; j++)
	      Rh[Mat[j].row_nr] -= theta * Mat[j].value;
	  }
      invert();
      Lp->eta_valid = TRUE;
    }

  failure = solvelp();

  if(Lp->anti_degen)
    {
      memcpy(Upbo,  upbo,     (Sum + 1)  * sizeof(REAL));
      memcpy(Lowbo, lowbo,    (Sum + 1)  * sizeof(REAL));
      memcpy(Rh,    Orig_rh,  (Rows + 1) * sizeof(REAL));

      for(i = 1; i <= Columns; i++)
        if(Lowbo[Rows + i] != 0)
          {
            theta = Lowbo[ Rows + i];
	    if(Upbo[Rows + i]<Infinite)
              Upbo[Rows + i] -= theta;
            for(j = Col_end[i - 1]; j < Col_end[i]; j++)
              Rh[Mat[j].row_nr] -= theta * Mat[j].value;
          }
      invert();
      Lp->eta_valid = TRUE;
      failure = solvelp();
    }

  if(failure != OPTIMAL)
    debug_print("this problem has no solution, it is %s",
		(failure == UNBOUNDED) ? "unbounded" : "infeasible");

  if(failure == INFEASIBLE && Lp->verbose)
    fprintf(stderr, "level%4d INF\n", Level);

  if(failure == OPTIMAL)	/* there is a solution */
    {
      construct_solution(Solution);

      debug_print("a solution was found");
      debug_print_solution();

      /* if this solution is worse than the best sofar, this branch must die */
      if(Maximise)
	is_worse = Solution[0] <= Best_solution[0];
      else			/* minimising! */
	is_worse = Solution[0] >= Best_solution[0];

      if(is_worse)
	{
	  if(Lp->verbose)
	    fprintf(stderr, "level%4d OPT NOB value %f bound %f\n",
		    Level, Solution[0], Best_solution[0]); 
	  debug_print("but it was worse than the best sofar, discarded");
	  Level--;
	  return(MILP_FAIL);
	}

      /* check if solution contains enough ints */
      if(Lp->bb_rule == FIRST_NI)
        {
          notint = 0;
          i = Rows + 1;
          while(i <= Sum && notint == 0)
            {
	      if(Must_be_int[i] && !is_int(Solution[i]))
		if(lowbo[i] == upbo[i]) /* this var is already fixed */
		  {
		    fprintf(stderr,
			    "Warning: integer var %d is already fixed at %d, but has non-integer value %g\n",
			    i - Rows, (int)lowbo[i], Solution[i]);
		    fprintf(stderr, "Perhaps the -e option should be used\n");
		  }
		else
		  notint = i;
              i++;
            }
        }
      if(Lp->bb_rule == RAND_NI)
        {
          int nr_not_int, select_not_int;
          nr_not_int = 0;
          for(i = Rows + 1; i <= Sum; i++)
            if(Must_be_int[i] && !is_int(Solution[i]))
              nr_not_int++;
          if(nr_not_int == 0)
            notint = 0;
          else
            {
              select_not_int=(rand() % nr_not_int) + 1;
              i = Rows + 1;
              while(select_not_int > 0)
                {
                  if(Must_be_int[i] && !is_int(Solution[i]))
                    select_not_int--;
                  i++;
                }
              notint = i - 1;
            }
        }

      if(Lp->verbose == TRUE)
        if(notint)
          fprintf(stderr, "level %3d OPT     value %f\n", Level, Solution[0]);
        else
          fprintf(stderr, "level %3d OPT INT value %f\n", Level, Solution[0]);

      if(notint)		/* there is at least one value not yet int */
	{
	  /* set up two new problems */
	  REAL   *new_upbo, *new_lowbo;
	  REAL   new_bound;
          short  *new_lower,*new_basis;
	  int    *new_bas;
          int     resone, restwo;

	  /* allocate room for them */
	  MALLOC(new_upbo,  Sum + 1, REAL);
	  MALLOC(new_lowbo, Sum + 1, REAL);
          MALLOC(new_lower, Sum + 1, short);
          MALLOC(new_basis, Sum + 1, short);
          MALLOC(new_bas, Rows + 1, int);
	  memcpy(new_upbo,  upbo,  (Sum + 1) * sizeof(REAL));
          memcpy(new_lowbo, lowbo, (Sum + 1) * sizeof(REAL));
          memcpy(new_lower, Lower, (Sum + 1) * sizeof(short));
          memcpy(new_basis, Basis, (Sum + 1) * sizeof(short));
          memcpy(new_bas, Bas, (Rows +1) * sizeof(int));
   
          if(Lp->names_used)
            debug_print("not enough ints. Selecting var %s, val: %10.3g",
			Lp->col_name[notint - Rows],
			(double)Solution[notint]);
          else
            debug_print("not enough ints. Selecting Var [%5d], val: %10.3g",
			notint, (double)Solution[notint]);
	  debug_print("current bounds:\n");
	  debug_print_bounds(upbo, lowbo);

          if(Floor_first)
            {
              new_bound = ceil(Solution[notint]) - 1;
              /* this bound might conflict */
              if(new_bound < lowbo[notint])
	        {
                  debug_print("New upper bound value %g conflicts with old lower bound %g\n",
			      (double)new_bound, (double)lowbo[notint]);
                  resone = MILP_FAIL;
	        }
	      else		/* bound feasible */
	        {
	          new_upbo[notint] = new_bound;
                  debug_print("starting first subproblem with bounds:");
	          debug_print_bounds(new_upbo, lowbo);
		  Lp->eta_valid = FALSE;
	          resone = milpsolve(new_upbo, lowbo, new_basis, new_lower,
				     new_bas);
                  Lp->eta_valid = FALSE;
	        }
              new_bound += 1;
	      if(new_bound > upbo[notint])
	        {
                  debug_print("New lower bound value %g conflicts with old upper bound %g\n",
			      (double)new_bound, (double)upbo[notint]);
	          restwo = MILP_FAIL;
	        }
	      else		/* bound feasible */
		{
		  new_lowbo[notint] = new_bound;
		  debug_print("starting second subproblem with bounds:");
		  debug_print_bounds(upbo, new_lowbo);
		  Lp->eta_valid = FALSE;
		  restwo = milpsolve(upbo, new_lowbo, new_basis, new_lower,
				     new_bas);
		  Lp->eta_valid = FALSE;
		}
	    }
          else			/* take ceiling first */
            {
              new_bound = ceil(Solution[notint]);
              /* this bound might conflict */
	      if(new_bound > upbo[notint])
	        {
		  debug_print("New lower bound value %g conflicts with old upper bound %g\n",
			      (double)new_bound, (double)upbo[notint]);
		  resone = MILP_FAIL;
	        }
	      else		/* bound feasible */
	        {
	          new_lowbo[notint] = new_bound;
                  debug_print("starting first subproblem with bounds:");
	          debug_print_bounds(upbo, new_lowbo);
                  Lp->eta_valid = FALSE;
                  resone = milpsolve(upbo, new_lowbo, new_basis, new_lower,
				     new_bas);
                  Lp->eta_valid = FALSE;
	        }
              new_bound -= 1;
	      if(new_bound < lowbo[notint])
	        {
                  debug_print("New upper bound value %g conflicts with old lower bound %g\n",
			      (double)new_bound, (double)lowbo[notint]);
	          restwo = MILP_FAIL;
	        }
	      else		/* bound feasible */
	        {
	          new_upbo[notint] = new_bound;
                  debug_print("starting second subproblem with bounds:");
     	          debug_print_bounds(new_upbo, lowbo);
	          Lp->eta_valid = FALSE;
	          restwo = milpsolve(new_upbo, lowbo, new_basis, new_lower,
				     new_bas);
                  Lp->eta_valid = FALSE;
		}
	    }
          if(resone && restwo)	/* both failed and must have been infeasible */
	    failure = INFEASIBLE;
	  else
	    failure = OPTIMAL;

	  free(new_upbo);
	  free(new_lowbo);
          free(new_basis);
          free(new_lower);
          free(new_bas);
	}
      else /* all required values are int */
	{
          debug_print("--> valid solution found");

	  if(Maximise)
	    is_worse = Solution[0] < Best_solution[0];
	  else
	    is_worse = Solution[0] > Best_solution[0];

	  if(!is_worse) /* Current solution better */
	    {
              if(Lp->debug || (Lp->verbose && !Lp->print_sol))
	        fprintf(stderr,
			"*** new best solution: old: %g, new: %g ***\n",
			(double)Best_solution[0], (double)Solution[0]);
	      memcpy(Best_solution, Solution, (Sum + 1) * sizeof(REAL));
              calculate_duals();
              if(Lp->print_sol)
                print_solution(Lp); 
              if(Lp->break_at_int)
                {
                  if(Maximise  &&  (Best_solution[0] > Lp->break_value))
                    Break_bb = TRUE;
                  if(!Maximise  &&  (Best_solution[0] < Lp->break_value))
                    Break_bb = TRUE;
                }
	    }
	}
    }

  /* failure can have the values OPTIMAL, UNBOUNDED and INFEASIBLE. */
  Level--;
  return(failure);
} /* milpsolve */

int solve(lprec *lp)
{
  int result, i;

  if(!lp->active)
    set_globals(lp);

  lp->total_iter  = 0;
  lp->max_level   = 1;
  lp->total_nodes = 0;

  if(Isvalid(lp))
    {
      if(Maximise && lp->obj_bound == Infinite)
	Best_solution[0]=-Infinite;
      else if(!Maximise && lp->obj_bound==-Infinite)
	Best_solution[0] = Infinite;
      else
	Best_solution[0] = lp->obj_bound;

      Level = 0;

      if(!lp->basis_valid)
	{
	  for(i = 0; i <= lp->rows; i++)
	    {
	      lp->basis[i] = TRUE;
	      lp->bas[i] = i;
	    }
	  for(i = lp->rows+1; i <= lp->sum; i++)
	    lp->basis[i] = FALSE;
	  for(i = 0; i <= lp->sum; i++)
	    lp->lower[i] = TRUE;
	  lp->basis_valid = TRUE;
	}

      lp->eta_valid = FALSE;
      Break_bb      = FALSE;
      result        = milpsolve(Orig_upbo, Orig_lowbo, Basis, Lower, Bas); 
      lp->eta_size  = Eta_size;
      lp->eta_alloc = Eta_alloc;
      lp->num_inv   = Num_inv;

      return(result);
    }

  /* if we get here, Isvalid(lp) failed. I suggest we return FAILURE */
  return(FAILURE);
}

int lag_solve(lprec *lp, REAL start_bound, int num_iter, short verbose)
{
  int i, j, result, citer;
  short status, OrigFeas, AnyFeas, same_basis;
  REAL *OrigObj, *ModObj, *SubGrad, *BestFeasSol;
  REAL Zub, Zlb, Ztmp, pie;
  REAL rhsmod, Step, SqrsumSubGrad;
  int   *old_bas;
  short *old_lower;

  /* allocate mem */  
  MALLOC(OrigObj, lp->columns + 1, REAL);
  CALLOC(ModObj, lp->columns + 1, REAL);
  CALLOC(SubGrad, lp->nr_lagrange, REAL);
  CALLOC(BestFeasSol, lp->sum + 1, REAL);
  MALLOCCPY(old_bas, lp->bas, lp->rows + 1, int);
  MALLOCCPY(old_lower, lp->lower, lp->sum + 1, short);

  get_row(lp, 0, OrigObj);
 
  pie = 2;  

  if(lp->maximise)
    {
      Zub = DEF_INFINITE;
      Zlb = start_bound;
    }
  else
    {
      Zlb = -DEF_INFINITE;
      Zub = start_bound;
    }
  status   = RUNNING; 
  Step     = 1;
  OrigFeas = FALSE;
  AnyFeas  = FALSE;
  citer    = 0;

  for(i = 0 ; i < lp->nr_lagrange; i++)
    lp->lambda[i] = 0;

  while(status == RUNNING)
    {
      citer++;

      for(i = 1; i <= lp->columns; i++)
        {
          ModObj[i] = OrigObj[i];
          for(j = 0; j < lp->nr_lagrange; j++)
            {
              if(lp->maximise)
                ModObj[i] -= lp->lambda[j] * lp->lag_row[j][i]; 
              else  
                ModObj[i] += lp->lambda[j] * lp->lag_row[j][i];
	    }
        }
      for(i = 1; i <= lp->columns; i++)
        {  
          set_mat(lp, 0, i, ModObj[i]);
          /* fprintf(stderr, "%f ", ModObj[i]); */
        }
      rhsmod = 0;
      for(i = 0; i < lp->nr_lagrange; i++)
        if(lp->maximise)
          rhsmod += lp->lambda[i] * lp->lag_rhs[i];
        else
          rhsmod -= lp->lambda[i] * lp->lag_rhs[i];
 
      if(verbose)
        {
          fprintf(stderr, "Zub: %10f Zlb: %10f Step: %10f pie: %10f Feas %d\n",
		  Zub, Zlb, Step, pie, OrigFeas);
          for(i = 0; i < lp->nr_lagrange; i++)
            fprintf(stderr, "%3d SubGrad %10f lambda %10f\n", i, SubGrad[i],
		    lp->lambda[i]);
        }

      if(verbose && lp->sum < 20)
        print_lp(lp);

      result = solve(lp);

      if(verbose && lp->sum < 20)
        { 
          print_solution(lp);
        }

      same_basis = TRUE;
      i = 1;
      while(same_basis && i < lp->rows)
        {
          same_basis = (old_bas[i] == lp->bas[i]);
          i++;
        }
      i = 1;
      while(same_basis && i < lp->sum)
        {
          same_basis=(old_lower[i] == lp->lower[i]);
          i++;
        }
      if(!same_basis)
        {
          memcpy(old_lower, lp->lower, (lp->sum+1) * sizeof(short));
          memcpy(old_bas, lp->bas, (lp->rows+1) * sizeof(int));
          pie *= 0.95;
        }

      if(verbose)
        fprintf(stderr, "result: %d  same basis: %d\n", result, same_basis);
      
      if(result == UNBOUNDED)
        {
          for(i = 1; i <= lp->columns; i++)
            fprintf(stderr, "%5f ", ModObj[i]);
          exit(FAIL);
        }

      if(result == FAILURE)
        status = FAILURE;

      if(result == INFEASIBLE)
        status = INFEASIBLE;
      
      SqrsumSubGrad = 0;
      for(i = 0; i < lp->nr_lagrange; i++)
        {
          SubGrad[i]= -lp->lag_rhs[i];
          for(j = 1; j <= lp->columns; j++)
            SubGrad[i] += lp->best_solution[lp->rows + j] * lp->lag_row[i][j];
          SqrsumSubGrad += SubGrad[i] * SubGrad[i];
        }

      OrigFeas = TRUE;
      for(i = 0; i < lp->nr_lagrange; i++)
        if(lp->lag_con_type[i])
          {
            if(my_abs(SubGrad[i]) > lp->epsb)
              OrigFeas = FALSE;
          }
        else if(SubGrad[i] > lp->epsb)
          OrigFeas = FALSE;

      if(OrigFeas)
        {
          AnyFeas = TRUE;
          Ztmp = 0;
          for(i = 1; i <= lp->columns; i++)
            Ztmp += lp->best_solution[lp->rows + i] * OrigObj[i];
          if((lp->maximise) && (Ztmp > Zlb))
	    {
	      Zlb = Ztmp;
	      for(i = 1; i <= lp->sum; i++)
		BestFeasSol[i] = lp->best_solution[i];
	      BestFeasSol[0] = Zlb;
	      if(verbose)
		fprintf(stderr, "Best feasible solution: %f\n", Zlb);
	    }
          else if(Ztmp < Zub)
	    {
	      Zub = Ztmp;
	      for(i = 1; i <= lp->sum; i++)
		BestFeasSol[i] = lp->best_solution[i];
	      BestFeasSol[0] = Zub;
	      if(verbose)
		fprintf(stderr, "Best feasible solution: %f\n", Zub);
	    }
        }      

      if(lp->maximise)
        Zub = my_min(Zub, rhsmod + lp->best_solution[0]);
      else
        Zlb = my_max(Zlb, rhsmod + lp->best_solution[0]);

      if(my_abs(Zub-Zlb)<0.001)
        {  
	  status = OPTIMAL;
        }
      Step = pie * ((1.05*Zub) - Zlb) / SqrsumSubGrad;  
 
      for(i = 0; i < lp->nr_lagrange; i++)
        {
          lp->lambda[i] += Step * SubGrad[i];
          if(!lp->lag_con_type[i] && lp->lambda[i] < 0)
            lp->lambda[i] = 0;
        }
 
      if(citer == num_iter && status==RUNNING)
        if(AnyFeas)
          status = FEAS_FOUND;
        else
          status = NO_FEAS_FOUND;
    }

  for(i = 0; i <= lp->sum; i++)
    lp->best_solution[i] = BestFeasSol[i];
 
  for(i = 1; i <= lp->columns; i++)
    set_mat(lp, 0, i, OrigObj[i]);

  if(lp->maximise)
    lp->lag_bound = Zub;
  else
    lp->lag_bound = Zlb;
  free(BestFeasSol);
  free(SubGrad);
  free(OrigObj);
  free(ModObj);
  free(old_bas);
  free(old_lower);
  
  return(status);
}

