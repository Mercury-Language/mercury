/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"imp.h"
#include	"table.h"
#include	"prof.h"
#include	"init.h"

#include	<string.h>

static	const void	*entry_name(const void *entry);
static	const void	*entry_addr(const void *entry);
static	bool		equal_name(const void *name1, const void *name2);
static	bool		equal_addr(const void *addr1, const void *addr2);
static	int		hash_name(const void *name);
static	int		hash_addr(const void *addr);

int	entry_table_size = (1 << 16);	/* 64k */

Table	entry_name_table =
	{0, NULL, entry_name, hash_name, equal_name};
Table	entry_addr_table =
	{0, NULL, entry_addr, hash_addr, equal_addr};

void do_init_entries(void)
{
	static bool done = FALSE;
	if (!done) {
		entry_name_table.ta_size = entry_table_size;
		entry_addr_table.ta_size = entry_table_size;
		init_table(entry_name_table);
		init_table(entry_addr_table);
		done = TRUE;
	}
}

#if defined(NATIVE_GC)
void insert_gc_entry(const char *name, int det, int offset)
{
	Label	*entry;

	do_init_entries();

	if ((entry = lookup_label_name(name)) != NULL) {
#ifndef SPEED
		if (progdebug)
			printf("adding det %d offset %d to label %s\n", det, 
				offset, name);
#endif /* SPEED */


		/* Same entry for both tables */

		entry->e_det = det;
		entry->e_offset = offset;
	}
	else {
		entry = make(Label);
		entry->e_name  = name;
		entry->e_det  = det;
		entry->e_offset = offset;
#ifndef	SPEED
		if (progdebug)
			printf("inserting label %s det %d offset %d %s\n", 
				name, det, offset);
#endif /* SPEED */
		if (insert_table(entry_name_table, entry))
			printf("duplicated label name %s\n", name);

		/* We don't have the address, so we don't insert an */
		/* entry into the entry_addr_table table */
	}
}
#endif /* NATIVE_GC */


Label *insert_entry(const char *name, Code *addr)
{
	Label	*entry;

	do_init_entries();

#ifdef	PROFILE_CALLS
	prof_output_addr_decls(name, addr);
#endif
	
#if !defined(NATIVE_GC)

	entry = make(Label);
	entry->e_name  = name;
	entry->e_addr  = addr;

#ifndef	SPEED
	if (progdebug)
		printf("inserting label %s at %p\n", name, addr);
#endif /* SPEED */

	if (insert_table(entry_name_table, entry))
		printf("duplicated label name %s\n", name);
	
	/* two labels at same location will happen quite often */
	/* when the code generated between them turns out to be empty */

	(void) insert_table(entry_addr_table, entry);

#else /* NATIVE_GC is defined */
	if ((entry = lookup_label_name(name)) != NULL) {
#ifndef SPEED
		if (progdebug)
			printf("adding address %p to label %s\n", addr, name);
#endif /* SPEED */
		entry->e_addr = addr;
		(void) insert_table(entry_addr_table, entry);
	}
	else {
		entry = make(Label);
		entry->e_name  = name;
		entry->e_addr  = addr;
#ifndef	SPEED
		if (progdebug)
			printf("inserting label %s at %p\n", name, addr);
#endif /* SPEED */
		if (insert_table(entry_name_table, entry))
			printf("duplicated label name %s\n", name);
		(void) insert_table(entry_addr_table, entry);
	}
#endif /* NATIVE_GC */
	return entry;
}

Label *lookup_label_addr(const Code *addr)
{
	do_init_entries();
	do_init_modules();
#ifndef	SPEED
	if (progdebug)
		printf("looking for label at %p\n", addr);
#endif

	return (Label *) lookup_table(entry_addr_table, addr);
}

Label *lookup_label_name(const char *name)
{
	do_init_entries();
	do_init_modules();
#ifndef	SPEED
	if (progdebug)
		printf("looking for label %s\n", name);
#endif

	return (Label *) lookup_table(entry_name_table, name);
}

List *get_all_labels(void)
{
	do_init_entries();
	do_init_modules();
	return get_all_entries(entry_name_table);
}

static const void *entry_name(const void *entry)
{
	return (const void *) (((const Label *) entry)->e_name);
}

static const void *entry_addr(const void *entry)
{
	return (const void *) (((const Label *) entry)->e_addr);
}

static bool equal_name(const void *name1, const void *name2)
{
	return streq(((const char *) name1), ((const char *) name2));
}

static bool equal_addr(const void *addr1, const void *addr2)
{
	return ((const Code *) addr1) == ((const Code *) addr2);
}

static int hash_name(const void *name)
{
	return str_to_int(((const char *) name)) % entry_table_size;
}

static int hash_addr(const void *addr)
{
	return (((Unsigned) addr) >> 3) % entry_table_size;
}
