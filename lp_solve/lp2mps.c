#include "lpkit.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
  lprec *lp;
  if (argc != 1)
    {  
      printf("lp to mps file converter by Jeroen J. Dirks (j.j.dirks@twi.tudelft.nl)\n");
      printf("Usage: lp2mps < inputfile.lp >  outputfile.mps\n");
    }
  else
    {
      fprintf(stderr,"reading lp file\n");
      lp = read_lp_file(stdin, FALSE, "from_lp_file");
      if (lp != NULL) 
        {
          fprintf(stderr,"writing mps file\n");
          write_MPS(lp, stdout);
        }
    }

  return(0);
}
