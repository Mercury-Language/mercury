:- module mostly_uniq2.
:- interface.
:- import_module io.

:- pred foo(io__state::di, io__state::uo) is multi.

:- implementation.

% This should be a unique mode error, since the I/O state
% is only mostly_unique, since we didn't declare foo as cc_multi.

foo --> io__write_int(1).
foo --> io__write_int(2).
