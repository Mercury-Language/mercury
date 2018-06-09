%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2007, 2010-2011 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: listing.m.
% Author: Ralph Becket <rafe@cs.mu.oz.au>
%
% Support for providing file listing functionality in the debugger.
%
% Unfortunately, scanning large files such as library/io.m byte-by-byte
% in a debugging grade is likely to exhaust the stack, because debugging
% grades do not support tail recursion. Instead we have to handle this
% aspect using a bit of C code.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.listing.
:- interface.

:- import_module io.
:- import_module list.

%---------------------------------------------------------------------------%

:- type search_path.
:- type line_no    == int.
:- type path_name  == string.
:- type file_name  == string.
:- type c_file_ptr.                     % For passing `FILE *' arguments.

    % Construct an empty search_path structure.
    %
:- func new_list_path = search_path.

    % Get/set/clear the stack of directories searched for FileName matches by
    %   list_file/7.
    %
:- func get_list_path(search_path::in) = (list(path_name)::out) is det.
:- pred set_list_path(list(path_name)::in,
    search_path::in, search_path::out) is det.
:- pred clear_list_path(search_path::in, search_path::out) is det.

    % push_list_path(Dir, !Path):
    %
    % Push Dir on to the stack of directories searched for FileName
    % matches by list_file/7.
    %
:- pred push_list_path(path_name::in, search_path::in, search_path::out)
    is det.

    % pop_list_path(!Path):
    %
    % Pop the last Dir pushed on to the stack of directories.
    % Does nothing if the search path stack is empty.
    %
:- pred pop_list_path(search_path::in, search_path::out) is det.

    % list_file(OutStrm, ErrStrm, FileName, FirstLine, LastLine, MarkLine,
    %   Path, !IO):
    %
    % Print, on OutStrm, the lines from FileName with numbers in the range
    % FirstLine.. LastLine (the first line is numbered 1). We mark the line
    % numbered MarkLine with a chevron; we indent all other lines
    % appropriately.
    %
    % We search for the file matching FileName by first looking in the current
    % working directory or, failing that, by prepending each Dir on the
    % search path stack in turn until a match is found. If no match is found,
    % we print an error message.
    %
    % We report any errors on ErrStrm.
    %
:- pred list_file(c_file_ptr::in, c_file_ptr::in, file_name::in, line_no::in,
    line_no::in, line_no::in, search_path::in, io::di, io::uo) is det.

    % As above, but implemented without foreign code. This is used by the
    % source-to-source debugger which does not enable debugging in standard
    % library so does not suffer the problem of excessive stack usage.
    %
:- pred list_file_portable(io.output_stream::in, io.output_stream::in,
    file_name::in, line_no::in, line_no::in, line_no::in, search_path::in,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module dir.
:- import_module int.
:- import_module maybe.
:- import_module type_desc.

%---------------------------------------------------------------------------%

:- type search_path  == list(path_name).

:- pragma foreign_type("C", c_file_ptr, "FILE *", [can_pass_as_mercury_type]).
    % stub.
:- pragma foreign_type("C#", c_file_ptr, "object").
:- pragma foreign_type("Java", c_file_ptr, "java.lang.Object").
:- pragma foreign_type("Erlang", c_file_ptr, "").

    % These predicates are called from trace/mercury_trace_internal.c.
    %
:- pragma foreign_export("C", new_list_path = out,
    "ML_LISTING_new_list_path").
:- pragma foreign_export("C", get_list_path(in) = out,
    "ML_LISTING_get_list_path").
:- pragma foreign_export("C", set_list_path(in, in, out),
    "ML_LISTING_set_list_path").
:- pragma foreign_export("C", clear_list_path(in, out),
    "ML_LISTING_clear_list_path").
:- pragma foreign_export("C", push_list_path(in, in, out),
    "ML_LISTING_push_list_path").
:- pragma foreign_export("C", pop_list_path(in, out),
    "ML_LISTING_pop_list_path").
:- pragma foreign_export("C", list_file(in, in, in, in, in, in, in, di, uo),
    "ML_LISTING_list_file").

:- func listing_type = type_desc.
:- pragma foreign_export("C", listing_type = out, "ML_LISTING_listing_type").

listing_type = type_of(Path) :-
    clear_list_path(Path @ [], _).

%---------------------------------------------------------------------------%

new_list_path = [].

%---------------------------------------------------------------------------%

get_list_path(Path) = Path.

set_list_path(Dirs, _, Dirs).

clear_list_path(_, []).

%---------------------------------------------------------------------------%

push_list_path(Dir, Path, [Dir | Path]).

%---------------------------------------------------------------------------%

pop_list_path([],         []).
pop_list_path([_ | Path], Path).

%---------------------------------------------------------------------------%

list_file(OutStrm, ErrStrm, FileName, FirstLine, LastLine, MarkLine, Path,
        !IO) :-
    ( if dir.path_name_is_absolute(FileName) then
        io.open_input(FileName, Result0, !IO),
        (
            Result0 = ok(InStream),
            InStrm = mercury_stream_to_c_FILE_star(InStream),
            print_lines_in_range_c(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStream, !IO)
        ;
            Result0 = error(Error),
            ErrorMsg = io.error_message(Error),
            write_to_c_file(ErrStrm, "mdb: cannot open file ", !IO),
            write_to_c_file(ErrStrm, FileName, !IO),
            write_to_c_file(ErrStrm, ": ", !IO),
            write_to_c_file(ErrStrm, ErrorMsg, !IO),
            write_to_c_file(ErrStrm, "\n", !IO)
        )
    else
        find_and_open_file([dir.this_directory | Path], FileName, Result, !IO),
        (
            Result = yes(InStream),
            InStrm = mercury_stream_to_c_FILE_star(InStream),
            print_lines_in_range_c(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStream, !IO)
        ;
            Result = no,
            write_to_c_file(ErrStrm, "mdb: cannot find file ", !IO),
            write_to_c_file(ErrStrm, FileName, !IO),
            write_to_c_file(ErrStrm, "\n", !IO)
        )
    ).

:- pred write_to_c_file(c_file_ptr::in, string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_to_c_file(ErrStrm::in, Str::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    fputs(Str, (FILE *)ErrStrm);
").

%---------------------------------------------------------------------------%

list_file_portable(OutStrm, ErrStrm, FileName, FirstLine, LastLine,
        MarkLine, Path, !IO) :-
    ( if dir.path_name_is_absolute(FileName) then
        io.open_input(FileName, Result0, !IO),
        (
            Result0 = ok(InStrm),
            print_lines_in_range_m(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStrm, !IO)
        ;
            Result0 = error(Error),
            ErrorMsg = io.error_message(Error),
            io.write_string(ErrStrm, "mdb: cannot open file ", !IO),
            io.write_string(ErrStrm, FileName, !IO),
            io.write_string(ErrStrm, ": ", !IO),
            io.write_string(ErrStrm, ErrorMsg, !IO),
            io.write_string(ErrStrm, "\n", !IO)
        )
    else
        find_and_open_file([dir.this_directory | Path], FileName, Result, !IO),
        (
            Result = yes(InStrm),
            print_lines_in_range_m(InStrm, OutStrm, 1, FirstLine, LastLine,
                MarkLine, !IO),
            io.close_input(InStrm, !IO)
        ;
            Result = no,
            io.write_string(ErrStrm, "mdb: cannot find file ", !IO),
            io.write_string(ErrStrm, FileName, !IO),
            io.write_string(ErrStrm, "\n", !IO)
        )
    ).

%---------------------------------------------------------------------------%

    % Search for the first file with the given name on the search path
    % that we can open for reading and return the complete file name
    % (including the path component) and input stream handle.
    %
:- pred find_and_open_file(search_path::in, file_name::in,
    maybe(io.input_stream)::out, io::di, io::uo) is det.

find_and_open_file([], _, no, !IO).
find_and_open_file([Dir | Path], FileName, Result, !IO) :-
    io.open_input(Dir / FileName, Result0, !IO),
    (
        Result0 = ok(InStream),
        Result  = yes(InStream)
    ;
        Result0 = error(_),
        find_and_open_file(Path, FileName, Result, !IO)
    ).

:- func mercury_stream_to_c_FILE_star(io.input_stream) = c_file_ptr.

:- pragma foreign_proc("C",
    mercury_stream_to_c_FILE_star(InStream::in) = (InStrm::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    InStrm = MR_file(*(MR_unwrap_input_stream(InStream)));
").

%---------------------------------------------------------------------------%

    % print_lines_in_range(InStrm, OutStrm, ThisLine, FirstLine, LastLine,
    %   MarkLine, !IO):
    %
    % Print the lines numbered FirstLine to LastLine from InStrm
    % on OutStrm (the current line number is taken as ThisLine).
    % Each line is printed indented with "  ", except for the line
    % numbered MarkLine, if it occurs in the range FirstLine .. LastLine,
    % which is indented with "> ".
    %
:- pred print_lines_in_range_c(c_file_ptr::in, c_file_ptr::in,
    line_no::in, line_no::in, line_no::in, line_no::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    print_lines_in_range_c(InStrm::in, OutStrm::in, ThisLine::in,
        FirstLine::in, LastLine::in, MarkLine::in, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    if (FirstLine <= ThisLine && ThisLine <= LastLine) {
        const char *s = (ThisLine == MarkLine) ? \"> \" : \"  \";
        fputs(s, (FILE *)OutStrm);
    }
    while(ThisLine <= LastLine) {
        int c = fgetc((FILE *)InStrm);
        if (c == EOF) {
            fputc('\\n', (FILE *)OutStrm);
            break;
        }
        if (FirstLine <= ThisLine) {
            fputc(c, (FILE *)OutStrm);
        }
        if (c == '\\n') {
            ThisLine++;
            if (FirstLine <= ThisLine && ThisLine <= LastLine)  {
                const char *s = (ThisLine == MarkLine) ? \"> \" : \"  \";
                fputs(s, (FILE *)OutStrm);
            }
        }
    }
").

%---------------------------------------------------------------------------%

:- pred print_lines_in_range_m(io.input_stream::in, io.output_stream::in,
    line_no::in, line_no::in, line_no::in, line_no::in, io::di, io::uo) is det.

print_lines_in_range_m(InStrm, OutStrm, ThisLine, FirstLine, LastLine,
        MarkLine, !IO) :-
    io.read_line_as_string(InStrm, Res, !IO),
    (
        Res = ok(Line),
        ( if FirstLine =< ThisLine, ThisLine =< LastLine then
            ( if ThisLine = MarkLine then
                io.write_string(OutStrm, "> ", !IO)
            else
                io.write_string(OutStrm, "  ", !IO)
            ),
            io.write_string(OutStrm, Line, !IO)
        else
            true
        ),
        print_lines_in_range_m(InStrm, OutStrm, ThisLine + 1, FirstLine,
            LastLine, MarkLine, !IO)
    ;
        Res = eof
    ;
        Res = error(Error),
        io.write_string(OutStrm, "Error: ", !IO),
        io.write_string(OutStrm, io.error_message(Error), !IO),
        io.write_string(OutStrm, "\n", !IO)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
