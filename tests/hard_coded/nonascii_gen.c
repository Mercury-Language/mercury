#include <stdio.h>

int
main(void)
{
	int	i;

	for (i = 1; i < 256; i++) {
		if (i != '\n') {
			putchar(i);
		}
	}

	putchar('\n');

	for (i = 1; i < 256; i++) {
		if (i != '\n') {
			putchar(i);
		}
	}

	putchar('\n');
	return 0;
}
