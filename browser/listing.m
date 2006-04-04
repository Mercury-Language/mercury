%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%-----------------------------------------------------------------------------%
% listing.m
% Ralph Becket <rafe@cs.mu.oz.au>
% Thu Oct  6 16:07:01 EST 2005
%
% Support for providing file listing functionality in the debugger.
%
% Unfortunately, scanning large files such as library/io.m byte-by-byte
% in a debugging grade is likely to exhaust the stack, because debugging
% grades do not support tail recursion.  Instead we have to handle this
% aspect using a bit of C code.
%
%-----------------------------------------------------------------------------%

:- module mdb.listing.
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

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
    % FirstLine ..  LastLine (the first line is numbered 1).
    % The line numbered MarkLine is marked with a chevron, all
    % other lines are indented appropriately.
    %
    % A file matching FileName is searched for by first looking
    % in the current working directory or, failing that, by
    % prepending each Dir on the search path stack in
    % turn until a match is found.  If no match is found then
    % an error message is printed.
    %
    % Any errors are reported on ErrStrm.
    %
:- pred list_file(c_file_ptr::in, c_file_ptr::in, file_name::in, line_no::in,
    line_no::in, line_no::in, search_path::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module array.
:- import_module char.
:- import_module dir.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module type_desc.

%-----------------------------------------------------------------------------%

:- type search_path  == list(path_name).

:- pragma foreign_type("C", c_file_ptr, "FILE *", [can_pass_as_mercury_type]).

    % These predicates are called from trace/mercury_trace_internal.c.
    %
:- pragma export(new_list_path = out,
    "ML_LISTING_new_list_path").
:- pragma export(get_list_path(in) = out,
    "ML_LISTING_get_list_path").
:- pragma export(set_list_path(in, in, out),
    "ML_LISTING_set_list_path").
:- pragma export(clear_list_path(in, out),
    "ML_LISTING_clear_list_path").
:- pragma export(push_list_path(in, in, out),
    "ML_LISTING_push_list_path").
:- pragma export(pop_list_path(in, out),
    "ML_LISTING_pop_list_path").
:- pragma export(list_file(in, in, in, in, in, in, in, di, uo),
    "ML_LISTING_list_file").

:- pred listing_type(type_desc::out) is det.
:- pragma export(listing_type(out), "ML_LISTING_listing_type").

listing_type(type_of(Path)) :-
    clear_list_path(Path @ [], _).

%-----------------------------------------------------------------------------%

new_list_path = [].

%-----------------------------------------------------------------------------%

get_list_path(Path) = Path.

set_list_path(Dirs, _, Dirs).

clear_list_path(_, []).

%-----------------------------------------------------------------------------%

push_list_path(Dir, Path, [Dir | Path]).

%-----------------------------------------------------------------------------%

pop_list_path([],         []).
pop_list_path([_ | Path], Path).

%-----------------------------------------------------------------------------%

list_file(OutStrm, ErrStrm, FileName, FirstLine, LastLine, MarkLine, Path,
        !IO) :-
    find_and_open_file([dir.this_directory | Path], FileName, Result, !IO),
    (
        Result = yes(InStrm),
        print_lines_in_range(InStrm, OutStrm, 1, FirstLine, LastLine,
            MarkLine, !IO)
    ;
        Result = no,
        write_to_c_file(ErrStrm, "mdb: cannot find file ", !IO),
        write_to_c_file(ErrStrm, FileName, !IO),
        write_to_c_file(ErrStrm, "\n", !IO)
    ).

:- pred write_to_c_file(c_file_ptr::in, string::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    write_to_c_file(ErrStrm::in, Str::in, IO0::di, IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    fputs(Str, (FILE *)ErrStrm);
    IO = IO0;
").

%-----------------------------------------------------------------------------%

    % Search for the first file with the given name on the search path
    % that we can open for reading and return the complete file name
    % (including the path component) and input stream handle.
    %
:- pred find_and_open_file(search_path::in, file_name::in,
    maybe(c_file_ptr)::out, io::di, io::uo) is det.

find_and_open_file([], _, no, !IO).

find_and_open_file([Dir | Path], FileName, Result, !IO) :-
    io.open_input(Dir / FileName, Result0, !IO),
    (
        Result0 = ok(InStream),
        InStrm = mercury_stream_to_c_FILE_star(InStream),
        Result  = yes(InStrm)
    ;
        Result0 = error(_),
        find_and_open_file(Path, FileName, Result, !IO)
    ).

:- func mercury_stream_to_c_FILE_star(io.input_stream) = c_file_ptr.

:- pragma foreign_proc("C",
    mercury_stream_to_c_FILE_star(InStream::in) = (InStrm::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    InStrm = MR_file(*InStream);
").

%-----------------------------------------------------------------------------%

    % print_lines_in_range(InStrm, OutStrm, ThisLine, FirstLine, LastLine,
    %   MarkLine, !IO):
    %
    % Print the lines numbered FirstLine to LastLine from InStrm
    % on OutStrm (the current line number is taken as ThisLine).
    % Each line is printed indented with "  ", except for the line
    % numbered MarkLine, if it occurs in the range FirstLine .. LastLine,
    % which is indented with "> ".
    %
:- pred print_lines_in_range(c_file_ptr::in, c_file_ptr::in,
    line_no::in, line_no::in, line_no::in, line_no::in, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    print_lines_in_range(InStrm::in, OutStrm::in, ThisLine::in, FirstLine::in,
        LastLine::in, MarkLine::in, IO0::di, IO::uo),
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
    IO = IO0;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
