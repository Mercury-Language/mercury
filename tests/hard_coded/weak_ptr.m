%
% Test the RTS' weak pointer support.
%
% Running this with GC_PRINT_STATS=1 GC_PRINT_VERBOSE_STATS=1 in the
% environment will show that this support is working.
%

% The output of this test is extreamly volatile.  It depends on the garbage
% collector's state (which objects get collected when) which can depend on a
% lot of other things.  Therefore we don't compare it's output with expected
% output.  The best we can do is test that it exits cleanly.

:- module weak_ptr.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
    test(!IO).

:- pragma foreign_decl("C", local,
"
#include \"gc.mh\"

struct list {
    int             item;
    struct list*    next;
    MR_weak_ptr     prev;
};

typedef struct list list;

static list* build_list(void);

static void traverse_forwards(list* list);

static void traverse_backwards(list* list);

static list* get_tail(list* list);

static list* drop(int n, list* list);
").

:- pragma foreign_code("C",
"
static list*
build_list(void)
{
    list*   head;
    list*   cur;
    list*   prev;
    int     i;

    cur = MR_GC_malloc(sizeof(struct list));
    cur->item = 0;
    cur->next = NULL;
    cur->prev = MR_NULL_WEAK_PTR;
    prev = cur;
    head = cur;

    for (i = 1; i < 10000; i++) {
        cur = MR_GC_malloc(sizeof(struct list));
        cur->item = i;
        cur->next = NULL;

        prev->next = cur;
        MR_new_weak_ptr(&(cur->prev), prev);

        prev = cur;
    }

    /* Help the GC */
    cur = NULL;
    prev = NULL;

    return head;
}

static void
traverse_forwards(list* cur)
{
    printf(\"Forwards: \");

    while (NULL != cur) {
        printf(\"%d \", cur->item);
        cur = cur->next;
    }

    printf(\"\\n\");
}

static void
traverse_backwards(list* cur)
{
    printf(\"Backwards: \");

    while (NULL != cur) {
        printf(\"%d \", cur->item);
        cur = MR_weak_ptr_read(&(cur->prev));
    }

    printf(\"\\n\");
}

static list*
get_tail(list* cur)
{
    list *prev = NULL;

    while (NULL != cur) {
        prev = cur;
        cur = cur->next;
    }

    return prev;
}

static list*
drop(int n, list* cur)
{
    int i;

    for (i = 0; (i < n) && (cur != NULL); i++) {
        cur = cur->next;
    }

    return cur;
}
").

:- pred test(io::di, io::uo) is det.

:- pragma foreign_proc("C", test(_IO0::di, _IO::uo),
    [will_not_throw_exception, thread_safe, promise_pure],
    "
        list*   list_head;
        list*   list_tail;

        ML_garbage_collect();

        list_head = build_list();
        ML_garbage_collect();

        list_tail = get_tail(list_head);
        traverse_forwards(list_head);
        ML_garbage_collect();

        traverse_backwards(list_tail);
        ML_garbage_collect();

        list_head = drop(9000, list_head);
        ML_garbage_collect();
        traverse_forwards(list_head);
        traverse_backwards(list_tail);

    ").

test(!IO) :-
    % We provide a weak_ptr.exp2 file so that the test passes in non C
    % grades.  Java and C# provide their own weak pointer code that we do
    % not need to test as part of the Mercury test suite.
    io.write_string("Test not supported in this grade.\n", !IO).

