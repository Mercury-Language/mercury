#include	<stdlib.h>
#include	"imp.h"
#include	"table.h"

#define	ENTRY_TABLE_SIZE	512

extern	const void	*entry_name(const void *entry);
extern	const void	*entry_addr(const void *entry);
extern	bool		equal_name(const void *name1, const void *name2);
extern	bool		equal_addr(const void *addr1, const void *addr2);
extern	int		hash_name(const void *name);
extern	int		hash_addr(const void *addr);

Table	entry_name_table =
	{ENTRY_TABLE_SIZE, NULL, entry_name, hash_name, equal_name};
Table	entry_addr_table =
	{ENTRY_TABLE_SIZE, NULL, entry_addr, hash_addr, equal_addr};

void
init_entries(void)
{
	init_table(entry_name_table);
	init_table(entry_addr_table);
}

Label *
makeentry(const char *name, Code *addr)
{
	Label	*entry;

	entry = make(Label);
	entry->e_name  = name;
	entry->e_addr  = addr;

#ifdef	LABEL_DEBUG
	printf("inserting label %s at %x\n", name, addr);
#endif

	if (insert_table(entry_name_table, entry))
		printf("duplicated label name %s\n", name);

	if (insert_table(entry_addr_table, entry))
		printf("duplicated label addr %x\n", addr);

	return entry;
}

Label *
lookup_label_addr(const Code *addr)
{
#ifdef	LABEL_DEBUG
	printf("looking for label at %x\n", addr);
#endif

	return (Label *) lookup_table(entry_addr_table, addr);
}

Label *
lookup_label_name(const char *name)
{
#ifdef	LABEL_DEBUG
	printf("looking for label %s\n", name);
#endif

	return (Label *) lookup_table(entry_name_table, name);
}

const void *
entry_name(const void *entry)
{
	return (const void *) (((const Label *) entry)->e_name);
}

const void *
entry_addr(const void *entry)
{
	return (const void *) (((const Label *) entry)->e_addr);
}

bool
equal_name(const void *name1, const void *name2)
{
	return streq(((const char *) name1), ((const char *) name2));
}

bool
equal_addr(const void *addr1, const void *addr2)
{
	return ((const Code *) addr1) == ((const Code *) addr2);
}

int
hash_name(const void *name)
{
	return str_to_int(((const char *) name)) % ENTRY_TABLE_SIZE;
}

int
hash_addr(const void *addr)
{
	return (((int) ((const Code *) addr)) >> 3) % ENTRY_TABLE_SIZE;
}
