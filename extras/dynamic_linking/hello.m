%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% Example module for use with dynamic linking.
% The driver program dl_test.m dynamically loads the object code
% for this module and then calls the procedures defined here,
% e.g. hello/2.
%
% This source file is hereby placed in the public domain.  -fjh (the author).
%
%-----------------------------------------------------------------------------%

:- module hello.
:- interface.

:- import_module float.
:- import_module int.
:- import_module io.

%-----------------------------------------------------------------------------%

    % A very basic test: print "Hello world".
    %
:- pred hello(io::di, io::uo) is det.

    % Test passing floating point arguments.
    %
:- func add3(float, float, float) = float.

    % Test passing integer arguments.
    %
:- func add3int(int, int, int) = int.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

hello(!IO) :-
    io.print("Hello, world\n", !IO).

add3(X, Y, Z) = X + Y + Z.

add3int(X, Y, Z) = X + Y + Z.

%-----------------------------------------------------------------------------%
:- end_module hello.
%-----------------------------------------------------------------------------%
