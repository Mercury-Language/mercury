/*
**      Profiling module
*/

#define 	DEBUG_ON
#include 	"debug.h"

#include        "prof.h"
#include        "std.h"
#include	"imp.h"

prof_node *addr_pair_table[PROF_TABLE_SIZE] = {NULL};
DEBUG(
	int       indice_count[PROF_TABLE_SIZE] = {0};
)

void prof_profile(void *Callee, void *Caller)
{
        prof_node *temp, *prev, *new_node;
	int indice;

	indice = hash_addr_pair(Callee, Caller);

        temp = prev = addr_pair_table[indice];
	DEBUG(
		indice_count[indice]++; 
	)

	if (!temp) {
		new_node = malloc(sizeof(prof_node));
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

        new_node = malloc(sizeof(prof_node));
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
