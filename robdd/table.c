/*
** Copyright (C) 2003-2004 Peter Schachte and The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include <stdio.h>
#include <stdlib.h>
#include "bryant.h"
#include "table.h"

#define TABLESIZE 4096

MR_ROBDD_node *lookupUniqueTable(int val, MR_ROBDD_node *tr, MR_ROBDD_node *fa);
void insertUniqueTable(MR_ROBDD_node *newnode);

MR_ROBDD_node *checkComputed(MR_ROBDD_node *f,MR_ROBDD_node *g, MR_ROBDD_node *h);
void insertComputed(MR_ROBDD_node *f, MR_ROBDD_node *g, MR_ROBDD_node *h, MR_ROBDD_node *result);

void printUnique();	/* prints no. of entries in Unique table */

static MR_ROBDD_node *unique[TABLESIZE];
static iteEntry	computed[TABLESIZE];

static int findHashUnique(int var, MR_ROBDD_node *tr, MR_ROBDD_node *fa);
static int findHashComputed(MR_ROBDD_node *f, MR_ROBDD_node *g, MR_ROBDD_node *h);

MR_ROBDD_node *lookupUniqueTable(int val, MR_ROBDD_node *tr, MR_ROBDD_node *fa)
/* checks to see if MR_ROBDD_node (val,tr,fa) exists, and if so returns pointer to
that MR_ROBDD_node, otherwise returns null */
{
        extern MR_ROBDD_node *unique[];
        MR_ROBDD_node *entry;
        MR_ROBDD_node *result;
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
void insertUniqueTable(MR_ROBDD_node *newnode)
{
/* inserts the MR_ROBDD_node newnode into the unique table */
/* insertion is at head of list */
        extern MR_ROBDD_node *unique[];
        MR_ROBDD_node *first;
        MR_ROBDD_node *newentry;
	int hash;

	/* find hash value - get 12 bits, ie. 4096, size of unique */
	hash = findHashUnique(newnode->value,newnode->tr,newnode->fa);

	newnode->unique = unique[hash];
	unique[hash] = newnode;
}

void printUnique()
{
        MR_ROBDD_node *t;
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

MR_ROBDD_node *checkComputed(MR_ROBDD_node *f,MR_ROBDD_node *g, MR_ROBDD_node *h)
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
void insertComputed(MR_ROBDD_node *f, MR_ROBDD_node *g, MR_ROBDD_node *h, MR_ROBDD_node *result)
{
	extern iteEntry computed[];
	int hash;
	int i;

	/* find hash value - get 12 bits, ie. 4096, size of computed */
	/* check hashing function, g or h is always MR_ROBDD_zero or MR_ROBDD_one */

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

static int findHashUnique(int var, MR_ROBDD_node *tr, MR_ROBDD_node *fa)
{
	/* find hash value - get 12 bits, ie. 4096, size of unique */
	return (var + ((long)tr << 1) +
			((long)fa >> 2)) & 0xfff;
}

static int findHashComputed(MR_ROBDD_node *f, MR_ROBDD_node *g, MR_ROBDD_node *h)
{
	/* find hash value - get 12 bits, ie. 4096, size of computed */
	return  (((long)h) + ((long)g << 1) +
			((long)f >> 2)) & 0xfff;
}

