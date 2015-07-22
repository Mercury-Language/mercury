%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module backend_external_func.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.

main(!IO) :-
    p(1, !IO),
    q(2, !IO),
    io.format("p(1, 2, 3) = %d\n", [i(p(1, 2, 3))], !IO),
    io.format("q(1, 2, 3) = %d\n", [i(q(1, 2, 3))], !IO).

%---------------------------------------------------------------------------%

    % in llds grades: external
    % in mlds grades: foreign_proc 
:- func p(int, int, int) = int.
:- pragma external_func(p/3, [low_level_backend]).

    % in llds grades: foreign_proc
    % in mlds grades: external 
:- func q(int, int, int) = int.
:- pragma external_func(q/3, [high_level_backend]).

%---------------------------------------------------------------------------%

    % these should NOT be treated as external
:- pred p(int::in, io::di, io::uo) is det.
:- pred q(int::in, io::di, io::uo) is det.

p(N, !IO) :-
    io.format("p(%d)\n", [i(N)], !IO).

q(N, !IO) :-
    io.format("q(%d)\n", [i(N)], !IO).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    p(A::in, B::in, C::in) = (R::out),
    [will_not_call_mercury, promise_pure, high_level_backend],
"
#ifdef MR_HIGHLEVEL_CODE
    R = (A + B + C);
#else
    R = (A + B + C) * 1000;
#endif
").

:- pragma foreign_proc("C",
    q(A::in, B::in, C::in) = (R::out),
    [will_not_call_mercury, promise_pure, low_level_backend],
"
#ifdef MR_HIGHLEVEL_CODE
    R = (A + 2*B + 3*C) * 1000;
#else
    R = (A + 2*B + 3*C);
#endif
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C",
"
#ifdef MR_HIGHLEVEL_CODE

MR_Integer MR_CALL
backend_external_func__q_3_f_0(MR_Integer A, MR_Integer B, MR_Integer C)
{
    return (A + 2*B + 3*C);
}

#else

MR_define_extern_entry(mercury__fn__backend_external_func__p_3_0);

MR_BEGIN_MODULE(backend_external_func_module)
    MR_init_entry(mercury__fn__backend_external_func__p_3_0);
MR_BEGIN_CODE
MR_define_entry(mercury__fn__backend_external_func__p_3_0);
    {
        MR_Integer A = MR_r1;
        MR_Integer B = MR_r2;
        MR_Integer C = MR_r3;
        MR_Integer R = (A + B + C);
        MR_r1 = R;
    }
    MR_proceed();
MR_END_MODULE
#endif

/* Ensure that the initialization code for the above module gets run. */
/*
INIT mercury_sys_init_backend_external_func_module
*/

extern  void
mercury_sys_init_backend_external_func_module_init(void);

extern  void
mercury_sys_init_backend_external_func_module_init_type_tables(void);

extern  void
mercury_sys_init_backend_external_func_module_write_out_proc_statics(FILE *fp);

void
mercury_sys_init_backend_external_func_module_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    backend_external_func_module();
#endif
}

void
mercury_sys_init_backend_external_func_module_write_out_proc_statics(FILE *fp)
{
}

void
mercury_sys_init_backend_external_func_module_init_type_tables(void)
{
    /* no types to register */
}
").

%---------------------------------------------------------------------------%
