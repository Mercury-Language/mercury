%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%
% Test the `:- finalise finalpred' directive.
%
%-----------------------------------------------------------------------------%

:- module finalise_decl.

:- interface.

:- import_module io.

:- pred main(io :: di, io :: uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- finalise i1/2.
:- finalize i2/2.

:- pred i1(io::di, io::uo) is det.
i1(!IO) :- io.print("This is the first finalise pred, i1/2.\n", !IO).

:- pred i2(io::di, io::uo) is det.
i2(!IO) :- io.print("This is the second finalise pred, i2/2.\n", !IO).

main(!IO) :- io.print("This is main/2.\n", !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
