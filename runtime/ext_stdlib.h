#ifndef	EXT_STDLIB_H
#define	EXT_STDLIB_H

extern	long	a64l(const char *);
extern	int	dup2(int, int);
extern	char	*ecvt(double, int, int *, int *);
extern	char	*fcvt(double, int, int *, int *);
extern	char	*qecvt(long double, int, int *, int *);
extern	char	*qfcvt(long double, int, int *, int *);
extern	char	*qgcvt(long double, int, char *);

#ifdef HAVE_CWD_DECL
#include <unistd.h>
#else
extern	char	*getcwd(char *, size_t);
#endif

extern	char	*getlogin(void);
extern	int	getopt(int, char *const *, const char *);
extern	int	getsubopt(char **, char *const *, char **);
extern	char	*optarg;
extern	int	optind, opterr, optopt;
extern	char	*getpass(const char *);
extern	int	getpw(uid_t, char *);
extern	char	*gcvt(double, int, char *);
extern	int	isatty(int);
extern	char	*l64a(long);
extern	void	*memalign(size_t, size_t);
extern	char	*mktemp(char *);
extern	int	putenv(char *);
extern	char	*realpath(char *, char *);
extern	void	swab(const char *, char *, int);
extern	char	*ttyname(int);
extern	int	ttyslot(void);
extern	void	*valloc(size_t);
extern	char	*ptsname(int);
extern	int	 grantpt(int);
extern	int	 unlockpt(int);

extern	double	drand48(void);
extern	double	erand48(unsigned short *);
extern	long	jrand48(unsigned short *);
extern	void	lcong48(unsigned short *);
extern	long	lrand48(void);
extern	long	mrand48(void);
extern	long	nrand48(unsigned short *);
extern	unsigned	short *seed48(unsigned short *);
extern	void	srand48(long);

#endif /* EXT_STDLIB_H */
