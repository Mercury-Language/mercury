% Example module for use with dynamic linking.
% The driver program dl_test.m dynamically loads the object code
% for this module and then calls the procedures defined here,
% e.g. hello/2.

% This source file is hereby placed in the public domain.  -fjh (the author).

:- module hello.
:- interface.
:- import_module io.
:- import_module float, int.

% a very basic test: print "Hello world"
:- pred hello(state::di, state::uo) is det.

% test passing floating point arguments
:- func add3(float, float, float) = float.

% test passing integer arguments
:- func add3int(int, int, int) = int.

:- implementation.

hello --> print("Hello, world\n").

add3(X, Y, Z) = X + Y + Z.
add3int(X, Y, Z) = X + Y + Z.
