/*
**  File: code/main.mod.
**  Main author: fjh.
** 
**  A default do-nothing implementation of main/2.
*/

#include "imp.h"

BEGIN_MODULE(main_module)
BEGIN_CODE

mercury__main_2_0:
	r2 = r1;
	proceed();

END_MODULE
