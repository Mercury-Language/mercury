%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module dir_fold.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module dir.
:- import_module int.
:- import_module io.file.
:- import_module unit.

%---------------------------------------------------------------------------%

main(!IO) :-
    DirName = "empty",
    dir.make_directory(DirName, ResMkdir, !IO),
    (
        ResMkdir = ok,
        % dir.foldl2 used to leak a file descriptor on empty directories.
        % Repeat enough times to make it apparent.  On Linux, the default
        % maximum number of open files to a process is 1024.
        repeat(DirName, 1025, Res, !IO),
        io.file.remove_file(DirName, _, !IO),
        (
            Res = ok,
            io.write_string("done.\n", !IO)
        ;
            Res = error(Error),
            report_error(io.error_message(Error), !IO)
        )
    ;
        ResMkdir = error(Error),
        report_error(io.error_message(Error), !IO)
    ).

:- pred repeat(string::in, int::in, io.res::out, io::di, io::uo) is det.

repeat(DirName, Count, Res, !IO) :-
    ( if Count > 0 then
        dir.foldl2(nothing, DirName, unit, Res0, !IO),
        (
            Res0 = ok(_ : unit),
            repeat(DirName, Count - 1, Res, !IO)
        ;
            Res0 = error(_, Error),
            Res = error(Error)
        )
    else
        Res = ok
    ).

:- pred nothing(string::in, string::in, io.file_type::in, bool::out,
    unit::in, unit::out, io::di, io::uo) is det.

nothing(_DirName, _BaseName, _FileType, Continue, !Data, !IO) :-
    Continue = yes.

:- pred report_error(string::in, io::di, io::uo) is det.

report_error(Error, !IO) :-
    io.stderr_stream(Stream, !IO),
    io.write_string(Stream, Error, !IO),
    io.nl(Stream, !IO),
    io.set_exit_status(1, !IO).
