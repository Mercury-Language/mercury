#include <stdio.h>

#include "c_main.h"
#include "mercury_lib.h"

typedef Word MercuryList;

static void print_list(MercuryList);

void c_main(void) {
	Integer value;
	MercuryList list;

	printf("In c_main().\n");

	/*
	** call the C function foo_test(), which is the interface
	** to the Mercury predicate foo/1 in mode
	** 	:- mode foo(in) is semidet.
	*/
	foo_test(42, &value);
	printf("foo_test(42) is %s\n", value ? "TRUE" : "FALSE");
	foo_test(43, &value);
	printf("foo_test(43) is %s\n", value ? "TRUE" : "FALSE");

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

	printf("Returning from c_main()...\n");
}

static void print_list(MercuryList list) {
	if (list_is_empty(list)) {
		printf("[]");
	} else {
		printf("[");
		printf("%ld", (long) list_head(list));
		list = list_tail(list);
		while (!list_is_empty(list)) {
			printf(", %ld", (long) list_head(list));
			list = list_tail(list);
		}
		printf("]");
	}
}
