% Example module for use with dynamic linking.
% The driver program dl_test.m dynamically loads the object code
% for this module and then calls the predicate hello/2.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module hello.
:- interface.
:- import_module io.

:- pred hello(state::di, state::uo) is det.

:- implementation.

hello --> print("Hello, world\n").
