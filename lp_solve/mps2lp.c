#include "lpkit.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
  lprec *lp;
  if (argc != 1)
    {  
      printf("mps to lp file converter by Jeroen J. Dirks (j.j.dirks@twi.tudelft.nl)\n");
      printf("Usage: mps2lp < inputfile.mps >  outputfile.lp\n");
    }
  else
    {
      fprintf(stderr,"reading mps file\n");
      lp = read_mps(stdin, FALSE);
      if (lp != NULL) 
        {
          fprintf(stderr,"writing lp file\n");
          write_LP(lp,stdout);
        }
    }

  return(0);
}
