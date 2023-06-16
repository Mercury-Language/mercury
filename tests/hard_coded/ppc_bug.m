%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% The following program compiles incorrectly on PPC/MacOS X in grade reg.gc.
% find_nth_yes/4 ends up throwing an exception instead of returning `YesPos =
% 3'.  Passing `--no-optimize-fulljumps' causes the test to pass.
%
% The test passes with gcc 2.95.2 (Apple version) but fails with gcc 3.3
% (Apple version).
%
% The problem was that gcc's `-floop-optimize' options was incompatible
% with our use of global registers.  The fix is to make sure that option
% is disabled on powerpc-apple-darwin.

:- module ppc_bug.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.
:- import_module bool.
:- import_module std_util.
:- import_module exception.

main(!IO) :-
    find_nth_yes([yes, no, yes, yes], 2, 1, X),
    io.write_int(X, !IO),
    io.nl(!IO).

:- pred find_nth_yes(list(bool)::in, int::in, int::in, int::out) is det.

find_nth_yes([], _, _, _) :-
    throw("no").
find_nth_yes([B | Bs], N, Cur, YesPos) :-
    (
        B = no,
        find_nth_yes(Bs, N, Cur + 1, YesPos)
    ;
        B = yes,
        ( if N = 1 then
            YesPos = Cur
        else
            find_nth_yes(Bs, N - 1, Cur + 1, YesPos)
        )
    ).
