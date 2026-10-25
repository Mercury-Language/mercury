%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% This is the full regression test for Mantis bug #6.
% This program caused the following assertion failure in the LLDS backend:
%
% Software Error: jumpopt.m: Unexpected: adjust_livevals:
%      BetweenLivevals and PrevLivevals differ
%
% This is the original version of this test case; there is a cut-down
% version in bug6.m.
%
%-----------------------------------------------------------------------------%

:- module bug6_full.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bug6_full_helper.

:- import_module char.
:- import_module exception.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if Args = [] then
        read_dna_from_stream(io.stdin_stream, empty_dna, DNA0, !IO)
      else
        read_dna_from_files(Args, empty_dna, DNA0, !IO)
    ),
    execute(DNA0, DNA),
    write_dna(DNA, !IO).

%-----------------------------------------------------------------------------%

:- pred read_dna_from_files(list(string)::in, dna::in, dna::out,
    io::di, io::uo) is det.

read_dna_from_files([], !DNA, !IO).
read_dna_from_files([FileName | FileNames], !DNA, !IO) :-
    io.open_input(FileName, Result, !IO),
    (
        Result = ok(Stream),
        read_dna_from_stream(Stream, !DNA, !IO),
        io.close_input(Stream, !IO),
        read_dna_from_files(FileNames, !DNA, !IO)
    ;
        Result = error(Error),
        throw("bug6_full.read_dna_from_files: "
            ++ io.error_message(Error) : string)
    ).

%-----------------------------------------------------------------------------%

:- pred read_dna_from_stream(io.input_stream::in, dna::in, dna::out,
    io::di, io::uo) is det.

read_dna_from_stream(Stream, !DNA, !IO) :-
    io.read_char(Stream, Result, !IO),
    (
        Result = ok(Char),
        ( if      Char = 'I' then !:DNA = append_icfp(!.DNA, i)
          else if Char = 'C' then !:DNA = append_icfp(!.DNA, c)
          else if Char = 'F' then !:DNA = append_icfp(!.DNA, f)
          else if Char = 'P' then !:DNA = append_icfp(!.DNA, p)
          else
            throw("bug6_full.read_dna_from_stream: bad char: " ++
                char_to_string(Char) : string)
        ),
        read_dna_from_stream(Stream, !DNA, !IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        throw("bug6_full.read_dna_from_stream: io error" ++
            io.error_message(Error) : string)
    ).

%-----------------------------------------------------------------------------%

:- pred write_dna(dna::in, io::di, io::uo) is det.

write_dna(DNA, !IO) :-
    write_dnas([DNA], !IO).

:- pred write_dnas(dnas::in, io::di, io::uo) is det.

write_dnas(!.DNAs, !IO) :-
    ( if dna_head_tail(ICFP, !DNAs) then
        (   ICFP = i,   io.write_char('I', !IO)
        ;   ICFP = c,   io.write_char('C', !IO)
        ;   ICFP = f,   io.write_char('F', !IO)
        ;   ICFP = p,   io.write_char('P', !IO)
        ),
        write_dnas(!.DNAs, !IO)
      else
        io.nl(!IO)
    ).

%-----------------------------------------------------------------------------%

:- pred execute(dna::in, dna::out) is det.

execute(!DNA).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
