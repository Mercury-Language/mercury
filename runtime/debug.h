#ifndef DEBUG_H
#define DEBUG_H

#ifdef DEBUG_ON
	#define DEBUG(X) X
#else
	#define DEBUG(X)
#endif

#define DONT(X)

#endif
