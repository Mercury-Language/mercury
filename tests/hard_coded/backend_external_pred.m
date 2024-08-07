%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module backend_external_pred.

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
:- pred p(int::in, io::di, io::uo) is det.
:- pragma external_pred(p/3, [low_level_backend]).

    % in llds grades: foreign_proc
    % in mlds grades: external 
:- pred q(int::in, io::di, io::uo) is det.
:- pragma external_pred(q/3, [high_level_backend]).

%---------------------------------------------------------------------------%

    % these should NOT be treated as external
:- func p(int, int, int) = int.
:- func q(int, int, int) = int.

p(A, B, C) = A + B + C.
q(A, B, C) = A + 2*B + 3*C.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    p(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, high_level_backend],
"
#ifdef MR_HIGHLEVEL_CODE
    printf(""p(%"" MR_INTEGER_LENGTH_MODIFIER ""d): "", N);
    printf(""expected highlevel, found highlevel, OK\\n"");
#else
    printf(""p(%"" MR_INTEGER_LENGTH_MODIFIER ""d): "", N);
    printf(""expected highlevel, found lowlevel, BUG\\n"");
#endif
").

:- pragma foreign_proc("C",
    q(N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, low_level_backend],
"
#ifdef MR_HIGHLEVEL_CODE
    printf(""q(%"" MR_INTEGER_LENGTH_MODIFIER ""d): "", N);
    printf(""expected lowlevel, found highlevel, BUG\\n"");
#else
    printf(""q(%"" MR_INTEGER_LENGTH_MODIFIER ""d): "", N);
    printf(""expected lowlevel, found lowlevel, OK\\n"");
#endif
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C",
"
#ifdef MR_HIGHLEVEL_CODE

void MR_CALL
backend_external_pred__q_3_p_0(MR_Integer n)
{
    printf(""q(%"" MR_INTEGER_LENGTH_MODIFIER ""d): "", n);
    printf(""expected highlevel, found highlevel, OK\\n"");
}

#else

MR_define_extern_entry(mercury__backend_external_pred__p_3_0);

MR_BEGIN_MODULE(backend_external_pred_module)
    MR_init_entry(mercury__backend_external_pred__p_3_0);
MR_BEGIN_CODE
MR_define_entry(mercury__backend_external_pred__p_3_0);
    printf(""p(%"" MR_INTEGER_LENGTH_MODIFIER ""d): "", MR_r1);
    printf(""expected lowlevel, found lowlevel, OK\\n"");
    MR_proceed();
MR_END_MODULE
#endif

/* Ensure that the initialization code for the above module gets run. */
/*
INIT mercury_sys_init_backend_external_pred_module
*/

extern  void
mercury_sys_init_backend_external_pred_module_init(void);

extern  void
mercury_sys_init_backend_external_pred_module_init_type_tables(void);

extern  void
mercury_sys_init_backend_external_pred_module_write_out_proc_statics(FILE *fp);

void
mercury_sys_init_backend_external_pred_module_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    backend_external_pred_module();
#endif
}

void
mercury_sys_init_backend_external_pred_module_write_out_proc_statics(FILE *fp)
{
}

void
mercury_sys_init_backend_external_pred_module_init_type_tables(void)
{
    /* no types to register */
}
").

%---------------------------------------------------------------------------%
