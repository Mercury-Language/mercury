/*
**      Profiling module
*/

#include 	"debug.h"

#include        "prof.h"
#include        "std.h"
#include	"imp.h"

static	FILE	  *declfptr = NULL;
static	prof_node *addr_pair_table[PROF_TABLE_SIZE] = {NULL};
DEBUG(
	/* To show the distribution of the hash function */
	static	int 	indice_count[PROF_TABLE_SIZE] = {0};
)

/*
**	prof_call_profile:
**		Saves the callee, caller pair into a hash table. If the
**		address pair already exists then it increments a count.
*/
void prof_call_profile(Code *Callee, Code *Caller)
{
        prof_node *temp, *prev, *new_node;
	int indice;

	indice = hash_addr_pair(Callee, Caller);

        temp = prev = addr_pair_table[indice];
	DEBUG(
		indice_count[indice]++; 
	)

	/* Special case of when pointer in array is NULL */
	if (!temp) {
		new_node = make(prof_node);
		new_node->Callee = Callee;
		new_node->Caller = Caller;
		new_node->count = 1;
		new_node->next = NULL;
		addr_pair_table[indice] = new_node;
		return;
	}

        while (temp) {
                if ( (temp->Callee == Callee) && (temp->Caller == Caller) ) {
                        temp->count++;
                        return;
                }
                prev = temp;
                temp = temp->next;
        }

        new_node = make(prof_node);
        new_node->Callee = Callee;
        new_node->Caller = Caller;
        new_node->count = 1;
        new_node->next = NULL;
        prev->next = new_node;

        return;
}

/*
**	prof_output_addr_pair_table :
**		Writes the hash table to a file called "addrpair.out".
**		Callee then caller followed by count.
*/
void prof_output_addr_pair_table(void)
{
	FILE *fptr;
	int  i;
	prof_node *current;

	if ( (fptr = fopen("addrpair.out", "w")) ) {
		for (i = 0; i < PROF_TABLE_SIZE ; i++) {
			current = addr_pair_table[i];
			while (current) {
				fprintf(fptr, "%p %p %lu\n", current->Callee,
					current->Caller, current->count);
				current = current->next;
			}
		}
	}
	else {
		fprintf(stderr, "%p\nCouldn't create addrpair.out\n", fptr);
		exit(1);
	}

DEBUG(
	for (i = 0; i < PROF_TABLE_SIZE ; i++) 
		printf("%d :\t%d\n", i, indice_count[i]);
)
}

/*
**	prof_output_addr_decls:
**		Ouputs the main predicate labels as well as their machine
**		addresses to a file called "addrdecl.out".
**		At the moment I think the best place to insert this call
**		is in the makeentry call in label.c
*/
void prof_output_addr_decls(const char *name, const Code *address)
{
	if (declfptr) {
		fprintf(declfptr, "%p\t%s\n", address, name);
	}
	else {
		if ( (declfptr = fopen("addrdecl.out", "w") ) ) {
			fprintf(declfptr, "%p\t%s\n", address, name);
		}
		else {
			fprintf(stderr, "\nCouldn't create addrdecl.out\n");
			exit(1);
		}
	}
	return;
}
