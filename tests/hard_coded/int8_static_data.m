%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

% Regression test for a problem where the static data we were generating for
% ground terms (in low-leve C grades) was incorrect for sub-word sized
% integers.  We compile this test with -O0 because optimizations will mask
% this bug in some grades.

:- module int8_static_data.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int8.

:- import_module exception.
:- import_module list.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    As = [-128i8],
    foo(As, !IO).

%---------------------------------------------------------------------------%

:- pred foo(list(int8)::in, io::di, io::uo) is cc_multi.

foo(As, !IO) :-
    (
        As = []
    ;
        As = [A | _],
        ( try []
            Result0 = myabs(A)
        then
            % If the bug is present we will erroneously print "-128" here.
            ResultStr = int8_to_string(Result0)
        catch_any _ ->
            % If things are working correctly the above call to myabs/1 should
            % throw an exception.
            ResultStr = "<<exception>>"
        ),
        io.write_string(ResultStr, !IO),
        io.nl(!IO)
    ).

:- func myabs(int8) = int8.

myabs(Num) =
    ( if Num = my_min_int8 then
        throw(software_error("int8.myabs: abs(min_int8) would overflow"))
    else
        unchecked_abs(Num)
    ).

:- pragma no_inline(my_min_int8/0).
:- func my_min_int8 = int8.

my_min_int8 = -128i8.

%---------------------------------------------------------------------------%
:- end_module int8_static_data.
%---------------------------------------------------------------------------%
