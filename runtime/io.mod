#include "imp.h"

BEGIN_MODULE(io_module)

BEGIN_CODE

mercury__io__write_string_3_0:
	printf("%s", r1);
	proceed();

mercury__io__write_int_3_0:
	printf("%d", r1);
	proceed();

mercury__error_1_0:
	fprintf(stderr,"Software error: %s\n", r1);
	abort();

END_MODULE
