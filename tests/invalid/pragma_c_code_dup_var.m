%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Test errors for variables occurring multiple times in
% a `:- pragma foreign_proc' argument list.
%
% The .err_exp{,2,3} files are for C, Java and C# respectively.
%
%---------------------------------------------------------------------------%

:- module pragma_c_code_dup_var.

:- interface.

:- import_module io.

:- type buffer == string.
:- type object == c_pointer.
:- type unmarshalled(T)
    --->    unmarshalled(T).
:- type signed_long_int
    --->    signed_short_int(c_pointer).

:- func bread_impl(object, object, unmarshalled(buffer), unmarshalled(buffer),
    unmarshalled(signed_long_int), io, io) =
    unmarshalled(signed_long_int).
:- mode bread_impl(in, out, in, out, in, di, uo) = out is det.

:- implementation.

:- pragma foreign_proc("C",
    bread_impl(MC_Object0::in, MC_Object::out, Buf::in, Buf::out,
        Nbyte::in, MC_IO0::di, MC_IO::uo) = (Mc_returnval::out),
    [promise_pure, will_not_call_mercury],
"
    Mc_returnval = apache_gen__apache__request__bread(MC_Object0, &MC_Object,
        Buf, &Buf, Nbyte);
    MC_IO = MC_IO0;
").
:- pragma foreign_proc("Java",
    bread_impl(MC_Object0::in, MC_Object::out, Buf::in, Buf::out,
        Nbyte::in, MC_IO0::di, MC_IO::uo) = (Mc_returnval::out),
    [promise_pure, will_not_call_mercury],
"
    Mc_returnval = apache_gen__apache__request__bread(MC_Object0, &MC_Object,
        Buf, &Buf, Nbyte);
    MC_IO = MC_IO0;
").
:- pragma foreign_proc("C#",
    bread_impl(MC_Object0::in, MC_Object::out, Buf::in, Buf::out,
        Nbyte::in, MC_IO0::di, MC_IO::uo) = (Mc_returnval::out),
    [promise_pure, will_not_call_mercury],
"
    Mc_returnval = apache_gen__apache__request__bread(MC_Object0, &MC_Object,
        Buf, &Buf, Nbyte);
    MC_IO = MC_IO0;
").
