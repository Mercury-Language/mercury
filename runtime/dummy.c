/*
** This dummy function is in a file of its own to ensure
** that gcc can't inline it. Similarly for two two pointers.
*/

#include "dummy.h"

void	*global_pointer;
void	*global_pointer_2;

void	*volatile volatile_global_pointer;

void dummy_function_call(void)
{
}
