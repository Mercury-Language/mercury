#ifndef	GETOPT_H
#define	GETOPT_H

#define	GETOPTHUH	'?'
#define	GETOPTDONE	(-1)

extern int	getopt(int, char *const*, const char *);

extern char	*optarg;
extern int	opterr;
extern int	optind;
extern int	optopt;

#endif
