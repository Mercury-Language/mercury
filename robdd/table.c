#include <stdio.h>
#include <stdlib.h>
#include "bryant.h"
#include "table.h"

#define TABLESIZE 4096

node *lookupUniqueTable(int val, node *tr, node *fa);
void insertUniqueTable(node *newnode);

node *checkComputed(node *f,node *g, node *h);
void insertComputed(node *f, node *g, node *h, node *result);

void printUnique();	/* prints no. of entries in Unique table */

static node *unique[TABLESIZE];
static iteEntry	computed[TABLESIZE];

static int findHashUnique(int var, node *tr, node *fa);
static int findHashComputed(node *f, node *g, node *h);

node *lookupUniqueTable(int val, node *tr, node *fa)
/* checks to see if node (val,tr,fa) exists, and if so returns pointer to
that node, otherwise returns null */
{
        extern node *unique[];
        node *entry;
        node *result;
	int hash;

        result = NULL;
	/* find hash value - get 12 bits, ie. 4096, size of unique */
	hash = findHashUnique(val,tr,fa);
        entry = unique[hash];
        while(entry!=NULL)
        {
                if((entry->tr==tr) && (entry->fa ==fa) && (entry->value==val))
                {
                        result=entry;
                        break;
                }
                entry=entry->unique;
        }
        return result;
}
void insertUniqueTable(node *newnode)
{
/* inserts the node newnode into the unique table */
/* insertion is at head of list */
        extern node *unique[];
        node *first;
        node *newentry;
	int hash;

	/* find hash value - get 12 bits, ie. 4096, size of unique */
	hash = findHashUnique(newnode->value,newnode->tr,newnode->fa);

	newnode->unique = unique[hash];
	unique[hash] = newnode;
}

void printUnique()
{
        node *t;
        int i;
        int cnt;
	int total;
	int longest;

	total = 0;
	longest = 0;
        for (i=0; i<TABLESIZE; i++)
        {
                cnt = 0;
                t = unique[i];
                while (t !=NULL)
                {
                        cnt++;
			if (cnt > longest)
				longest = cnt;
                        t= t->unique;
                }
		if(total > 130000)
		{
			if (cnt != 0)
                		printf("unique[%d]: %d\n",i,cnt);
		}
		total = total+cnt;
        }
	printf("total nodes is %d: longest string is %d\n",total,longest);
}

node *checkComputed(node *f,node *g, node *h)
{
	extern iteEntry computed[];
	int hash;

	hash = findHashComputed(f,g,h);

	if ((computed[hash].f == f) &&
	(computed[hash].g == g) &&
	(computed[hash].h == h)) 
	{
		return computed[hash].where;
	}
	return NULL;
}
void insertComputed(node *f, node *g, node *h, node *result)
{
	extern iteEntry computed[];
	int hash;
	int i;

	/* find hash value - get 12 bits, ie. 4096, size of computed */
	/* check hashing function, g or h is always zero or one */

	hash = findHashComputed(f,g,h);

	computed[hash].f = f;
	computed[hash].g = g;
	computed[hash].h = h;
	computed[hash].where = result;

/*
	printf("in computed \n");
	for (i=0; i<TABLESIZE; i++)
	{
		if (computed[i].f != NULL)
			printf(" %d",i);
	}
	printf("\n");
*/
}

static int findHashUnique(int var, node *tr, node *fa)
{
	/* find hash value - get 12 bits, ie. 4096, size of unique */
	return (var + ((long)tr << 1) +
			((long)fa >> 2)) & 0xfff;
}

static int findHashComputed(node *f, node *g, node *h)
{
	/* find hash value - get 12 bits, ie. 4096, size of computed */
	return  (((long)h) + ((long)g << 1) +
			((long)f >> 2)) & 0xfff;
}

