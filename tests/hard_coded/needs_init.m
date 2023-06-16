%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module needs_init.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- pred init_modules(io::di, io::uo) is det.
:- pragma import(init_modules(di, uo), "do_init_modules").

main(!IO) :-
    init_modules(!IO),
    io.write_string("world!\n", !IO).

:- pred hello(io::di, io::uo) is det.
:- pragma export(hello(di, uo), "run_at_init_time_init").

hello(!IO) :-
    io.write_string("hello, ", !IO).

:- pragma foreign_decl("C",
"
#include <stdio.h>
void run_at_init_time_init_type_tables(void);
void run_at_init_time_write_out_proc_statics(FILE *);
void run_at_init_time_init_debugger(void);
").

:- pragma foreign_code("C",
"
/*
INIT run_at_init_time
ENDINIT
*/

void run_at_init_time_init_type_tables() {}
void run_at_init_time_write_out_proc_statics(FILE *f) {}
void run_at_init_time_init_debugger() {}
").
