#ifndef	EXT_STDIO_H
#define	EXT_STDIO_H

extern	FILE	*popen(const char *, const char *);
extern	char	*cuserid(char *);
extern	char	*tempnam(const char *, const char *);
extern	int	getw(FILE *);
extern	int	putw(int, FILE *);
extern	int	pclose(FILE *);

#endif
