#include "mercury_lib.h"	/* must come first */

#include <stdio.h>

#include "c_main.h"

typedef MR_Word MercuryList;

static void print_list(MercuryList);

void c_main(void) {
	MR_Integer value;
	MercuryList list;

	printf("In c_main().\n");

	/*
	** call the C function foo_test(), which is the interface
	** to the Mercury predicate foo/1 in mode
	** 	:- mode foo(in) is semidet.
	*/
	printf("foo_test(42) returns %s\n", foo_test(42) ? "TRUE" : "FALSE");
	printf("foo_test(43) returns %s\n", foo_test(43) ? "TRUE" : "FALSE");

	/*
	** call the C function one_foo(), which is the interface
	** to the Mercury predicate foo/1 in mode
	** 	:- mode foo(out) is cc_multi.
	*/
	one_foo(&value);
	printf("one_foo(&value) gives value = %ld\n", (long) value);

	/*
	** call the C function foo_list(), which is the interface
	** to the Mercury predicate foo/1 in mode
	** 	:- mode foo(out) is multi.
	*/
	printf("foo_list() = ");
	foo_list(&list);
	print_list(list);
	printf("\n");

	/*
	** call the C functions bar(), bar_test(), and bar_inverse(),
	** which are the C interfaces to the Mercury function bar/1
	** in the modes
	**	:- mode bar(in) = out is det.
	**	:- mode bar(out) = in is det.
	**	:- mode bar(in) = in is det.
	** respectively.
	*/
	printf("bar(100) = %ld\n", (long) bar(100));
	printf("bar_test(100, 101) returns %s\n",
		(bar_test(100, 101) ? "TRUE" : "FALSE"));
	printf("bar_test(100, 200) returns %s\n",
		(bar_test(100, 200) ? "TRUE" : "FALSE"));
	bar_inverse(&value, 101);
	printf("bar_inverse(&value, 101) gives value = %ld\n", (long) value);
	bar_inverse(&value, 200);
	printf("bar_inverse(&value, 200) gives value = %ld\n", (long) value);

	if (baz(1, &value)) {
		printf("baz(1, &value) returns TRUE with value = %ld\n",
			(long) value);
	} else {
		printf("baz(100, &value) returns FALSE\n");
	}
	if (baz(100, &value)) {
		printf("baz(100, &value) returns TRUE with value = %ld\n",
			(long) value);
	} else {
		printf("baz(100, &value) returns FALSE\n");
	}

	printf("Returning from c_main()...\n");
}

static void print_list(MercuryList list) {
	if (MR_list_is_empty(list)) {
		printf("[]");
	} else {
		printf("[");
		printf("%ld", (long) MR_list_head(list));
		list = MR_list_tail(list);
		while (!MR_list_is_empty(list)) {
			printf(", %ld", (long) MR_list_head(list));
			list = MR_list_tail(list);
		}
		printf("]");
	}
}
