:- module mostly_uniq1.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is multi.

:- implementation.

:- pred p(int::out) is multi.

p(1).
p(2).

% This should be a unique mode error, since the io-state is
% only mostly_unique, because p/1 has multiple solutions
% and we didn't declare main as cc_multi.

main --> { p(X) }, io__write_int(X).
