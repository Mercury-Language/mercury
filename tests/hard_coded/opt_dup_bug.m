% This is a regression test for a bug reported by Ian on 14 Nov 2006.
% The bug manifests itself only if all three of --optimize-dups,
% --optimize-frames and --optimize-peep are turned on, as they are
% at the default optimization level. The bug was actually in peephole
% optimization; the other two optimizations just created the circumstances
% that tickled the bug. The bug is shown in this diff, created using
% --debug-opt:
%
% -       mkframe(pred opt_dup_bug.t/2-0, 0, no, do_fail)
% +       mkframe(pred opt_dup_bug.t/2-0, 0, no, local_8)
%                 late setup
%  local_14:
% -       redoip_slot(maxfr) := code_addr_const(opt_dup_bug_t_2_0_i8)
% -               hijack redoip to effect resume point
%
% The transformation is correct for code that enters the code above at the top,
% but for code that enters by jumping to local_14, it changes the semantics of
% the program, yielding an infinite loop.

:- module opt_dup_bug.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module solutions.

main(!IO) :-
    solutions(t("2"), Solutions),
    io.write_list(Solutions, "\n", io.write, !IO),
    io.nl(!IO).

:- pred t(string, string).
:- mode t(in, out) is nondet.

t("2", "a").
t("2", "b").
t("2", "c").
t("3", "b").
t("3", "c").
t("4", "c").
