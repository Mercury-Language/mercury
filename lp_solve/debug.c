#include "lpkit.h"
#include "lpglob.h"
#include <stdarg.h>


static void print_indent(void)
{
  int i;

  fprintf(stderr, "%2d", Level);
  if(Level < 50) /* useless otherwise */
    for(i = Level; i > 0; i--)
      fprintf(stderr, "--");
  else
    fprintf(stderr, " *** too deep ***");
  fprintf(stderr, "> ");
} /* print_indent */


void debug_print_solution()
{
  int i;

  if(Lp->debug)
    for (i = Rows + 1; i <= Sum; i++)
      {
	print_indent();
        if (Lp->names_used)
	  fprintf(stderr, "%-10s%16.5g\n", Lp->col_name[i - Rows],
		(double)Solution[i]);
        else 
          fprintf(stderr, "Var[%5d]   %16.5g\n", i - Rows,
		  (double)Solution[i]);
      }
} /* debug_print_solution */


void debug_print_bounds(REAL *upbo, REAL *lowbo)
{
  int i;

  if(Lp->debug)
    for(i = Rows + 1; i <= Sum; i++)
      {
	if(lowbo[i] != 0)
	  {
	    print_indent();
            if (Lp->names_used)
	      fprintf(stderr, "%s > %10.3g\n", Lp->col_name[i - Rows],
		      (double)lowbo[i]);
            else
              fprintf(stderr, "Var[%5d]  > %10.3g\n", i - Rows,
		      (double)lowbo[i]);
	  }
	if(upbo[i] != Infinite)
	  {
	    print_indent();
	    if (Lp->names_used)
              fprintf(stderr, "%s < %10.3g\n", Lp->col_name[i - Rows],
		      (double)upbo[i]);
            else
              fprintf(stderr, "Var[%5d]  < %10.3g\n", i - Rows,
		      (double)upbo[i]);
          }
      }
} /* debug_print_bounds */


void debug_print(char *format, ...)
{
  va_list ap;

  if(Lp->debug)
    {
      va_start(ap, format);
      print_indent();
      vfprintf(stderr, format, ap);
      fputc('\n', stderr);
      va_end(ap);
    }
} /* debug_print */

