% vim: ts=4 sw=4 expandtab ft=mercury
%
% This is a regression test for Mantis bug #369.
%
% The bugs were that (a) for a module like this one, which calls
% stream.string_writer.format but does not *explicitly* import stream.m,
% the compiler did not *implicitly* import stream.m, and (b) it also
% did not protect stream.put from being deleted by dead predicate elimination.

:- module stream_put_bug.

:- interface.

:- import_module io.

:- interface.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.
:- use_module stream.
:- use_module stream.string_writer.
:- use_module string.builder.

main(!IO) :-
    some [!State] (
        !:State = string.builder.init,
        stream.string_writer.format(string.builder.handle,
            "answer to life universe everything %d\n", [i(42)], !State),
        stream.string_writer.format(string.builder.handle,
            "bug %s\n", [s("not present")], !State),
        io.write_string(string.builder.to_string(!.State), !IO)
    ).
