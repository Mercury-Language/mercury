
/* DEFINITIONS FOR OVERFLOW CHECKS */

#define IF(cond, val)	((cond) ? (val) : 0)

#ifdef	SPEED

#define	heap_overflow_check()		((void)0)
#define	detstack_overflow_check()	((void)0)
#define	detstack_underflow_check()	((void)0)
#define	nondstack_overflow_check()	((void)0)
#define	nondstack_underflow_check()	((void)0)

#else

#define	heap_overflow_check()					\
			(					\
				IF (hp >= heapend,(		\
					fatal_error("heap overflow") \
				)),				\
				IF (hp > heapmax,(		\
					heapmax = hp		\
				)),				\
				(void)0				\
			)

#define	detstack_overflow_check()				\
			(					\
				IF (sp >= detstackend,(		\
					fatal_error("stack overflow") \
				)),				\
				IF (sp > detstackmax,(		\
					detstackmax = sp	\
				)),				\
				(void)0				\
			)

#define	detstack_underflow_check()				\
			(					\
				IF (sp < detstackmin,(		\
					fatal_error("stack underflow") \
				)),				\
				(void)0				\
			)

#define	nondstack_overflow_check()				\
			(					\
				IF (maxfr >= nondstackend,(	\
					fatal_error("nondstack overflow") \
				)),				\
				IF (maxfr > nondstackmax,(	\
					nondstackmax = maxfr	\
				)),				\
				(void)0				\
			)

#define	nondstack_underflow_check()				\
			(					\
				IF (maxfr < nondstackmin,(	\
					fatal_error("nondstack underflow") \
				)),				\
				(void)0				\
			)

#endif
