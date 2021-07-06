%-----------------------------------------------------------------------------%
% Copyright (C) 1995-1999, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%

% file: muz.cpp
% main author: philip

:- module muz.
%:- pragma source_file("muz.cpp").

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module bool.
:- import_module dict.
:- import_module getopt.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module typecheck.
:- import_module word.
:- import_module zabstract.
:- import_module zparser.
:- import_module ztoken_io.
:- import_module ztype.

:- pred short_option(character::in, option::out) is semidet.
:- pred long_option(string::in, option::out) is semidet.
:- pred option_defaults(string::in, option::out, option_data::out) is multi.

:- type option
    --->    abbreviate
    ;       debug
    ;       help
    ;       toolkit.

short_option('a', abbreviate).
short_option('d', debug).
short_option('?', help).
short_option('h', help).
short_option('p', toolkit).     % used by fuzz: 'p' for prelude
short_option('t', toolkit).

long_option("abbreviate", abbreviate).
long_option("debug", debug).
long_option("help", help).
long_option("prelude", toolkit).    % cf. short option 'p' used by fuzz
long_option("toolkit", toolkit).

option_defaults(_, abbreviate, bool(bool.yes)).
option_defaults(_, debug, bool(bool.no)).
option_defaults(_, help, bool(bool.no)).
option_defaults(T, toolkit, maybe_string(yes(T))).

:- func get_flags(option_table(option)) = flags.

get_flags(Option_Table) = !:F :-
    !:F = defaults,
    getopt.lookup_bool_option(Option_Table, debug, Debug),
    (
        Debug = yes,
        set_debugging_on(!F)
    ;
        Debug = no
    ),
    getopt.lookup_maybe_string_option(Option_Table, toolkit, Toolkit),
    set_toolkit(Toolkit, !F).

main(!IO) :-
    io.set_exit_status(0, !IO),
    io.get_environment_var("MUZ_TOOLKIT", MT, !IO),
    ( MT = no, DT = "/usr/local/apps/muz/lib/toolkit.tex"
    ; MT = yes(DT)
    ),
    Option_Ops =
        option_ops_multi(short_option, long_option, option_defaults(DT)),
    io.command_line_arguments(AL0, !IO),
    getopt.process_options(Option_Ops, AL0, AL, Maybe_Option_Table),
    (
        Maybe_Option_Table = error(Message),
        zmessage("muz", [option_error_to_string(Message)], !IO),
        usage(!IO)
    ;
        Maybe_Option_Table = ok(Option_Table),
        ( if getopt.lookup_bool_option(Option_Table, help, yes) then
            usage(!IO)
        else if AL = [] then
            zmessage("muz", ["Filename expected (for stdin use -)"], !IO),
            usage(!IO)
        else
            main(Option_Table, AL, !IO),
            io.get_exit_status(Status, !IO),
            io.stderr_stream(StdErr, !IO),
            ( if Status = 0 then
                io.write_string(StdErr, "No errors detected.\n", !IO)
            else
                io.write_string(StdErr, "Errors detected.\n", !IO)
            )
        )
    ).

:- pred main(option_table(option)::in, list(string)::in,
    io::di, io::uo) is det.

main(Option_Table, AL, !IO) :-
    Flags0 = get_flags(Option_Table),
    P0 = (pred(I::in, IF::out) is det :- IF = I - Flags0),
    list.map(P0, AL, AL1),
    MToolkit = toolkit(Flags0),
    (
        MToolkit = no,
        AL2 = AL1
    ;
        MToolkit = yes(Toolkit),
        AL2 = [Toolkit-defaults | AL1]
    ),
    openInputs(AL2, IOResults, !IO),
    io.get_exit_status(Status, !IO),
    ( if Status = 0 then
        getopt.lookup_bool_option(Option_Table, abbreviate, Abbrev),
        % The handling of flags and pragmas here is really ugly.
        % These two structures need to be rethought.
        processFiles(IOResults, Abbrev, zpragmaInit, ZPragma,
            finish(dict.init, [])-init_schema_table, Phase, !IO),
        set_zpragma(ZPragma, Flags0, _Flags),
        ( if Phase = finish(_Dict, _TP) - _ then
            true
        else
            io.set_exit_status(1, !IO)
        )
    else
        true
    ).

:- type zinput
    --->    zinput(io.input_stream, flags).

:- pred openInputs(list(pair(string, flags))::in, list(zinput)::out,
    io::di, io::uo) is det.

openInputs([], [], !IO).
openInputs([Filename - Flags | T], Oks, !IO) :-
    ( if Filename = "-" then
        io.input_stream(Stdin, !IO),
        Result = ok(Stdin),
        io.input_stream_name(Filename1, !IO) %Filename1="<standard input>"
    else
        io.open_input(Filename, Result, !IO),
        Filename1 = Filename
    ),
    (
        Result = error(IOError),
        io.error_message(IOError, Message),
        zmessage(Filename1, [Message], !IO), % sets exit status to 1
        openInputs(T, Oks, !IO)
    ;
        Result = ok(Stream),
        Oks = [zinput(Stream, Flags) | Oks1],
        % Kludge to handle first line pragmas.
        io.putback_char(Stream, '\n', !IO),
        openInputs(T, Oks1, !IO)
    ).

:- pred zmessage(string::in, list(string)::in, io::di, io::uo) is det.

zmessage(F, ML, !IO) :-
    P =
        ( pred(S0::in, S::out) is det :-
            string.append_list([F, ": ", S0, ".\n"], S)
        ),
    list.map(P, ML, ML1),
    list.reverse(ML1, ML2),
    io.stderr_stream(StdErr, !IO),
    io.write_strings(StdErr, ML2, !IO),
    io.set_exit_status(1, !IO).

:- pred processFiles(list(zinput)::in, bool::in, zpragma::in, zpragma::out,
    zphase0::in, zphase0::out, io::di, io::uo) is det.

processFiles([], _, ZPragma, ZPragma, Phase, Phase, !IO).
processFiles([zinput(Stream, Flags0) | Rest], Abbrev, ZPragma0, ZPragma,
        Phase0, Phase, !IO) :-
    io.set_input_stream(Stream, _, !IO),
    set_zpragma(ZPragma0, Flags0, Flags),
    io.input_stream_name(Filename, !IO),
    processFile(Filename, Abbrev, Flags, Flags1, Phase0, Phase1, !IO),
    io.close_input(Stream, !IO),
    ( if Phase1 = finish(_, _) - _ then
        % Do not process later files if there are earlier errors.
        processFiles(Rest, Abbrev, zpragma(Flags1), ZPragma,
            Phase1, Phase, !IO)
    else
        ZPragma = zpragma(Flags1),
        Phase = Phase1
    ).

:- type typed_par == triple(par, subst, ptypes).

:- type zphase0 == pair(zphase, schema_table).

% Used to indicate the earliest phase in which errors have occured.
:- type zphase
    --->    lexical
    ;       syntax
    ;       typecheck(dict)
    ;       finish(dict, list(pair(typed_par, flag))).

:- pred processFile(string::in, bool::in, flags::in, flags::out,
    zphase0::in, zphase0::out, io::di, io::uo) is det.

processFile(Filename, Abbrev, F0, F, P0 - ST0, P, !IO) :-
    ( if debugging(F0) then
        io.write_strings(["Processing ", Filename, " ...\n"], !IO)
    else
        true
    ),
    readTokenList(operators(F0), TResult, !IO),
    (
        TResult = ok(TS),
        ( if debugging(F0) then
            writeTokenList(TS, !IO)
        else
            true
        ),
        specification(TS, Result, ST0, ST1, F0, F1),
        ( if Abbrev = no then
            set_abbreviations([], F1, F2)
        else
            F2 = F1
        ),
        (
            Result = ok(Spec),
            ( if debugging(F2) then
                writeSpec(Spec, !IO)
            else
                true
            ),
            ( if ( P0 = typecheck(D) ; P0 = finish(D, _) ) then
                zcheck(F2, Spec, Status1, D, D1, !IO),
                ( if Status1 = yes(TSpec1), P0 = finish(_, TSpec0) then
                    G = generating_logic(F2),
                    HoP =
                        ( pred(TP::in, TPG::out) is det :-
                            TPG = TP - G
                        ),
                    list.map(HoP, TSpec1, TSpec2),
                    list.append(TSpec0, TSpec2, TSpec),
                    P1 = finish(D1, TSpec)
                else
                    P1 = typecheck(D1)
                )
            else
                P1 = syntax
            )
        ;
            Result = error(ErrorList),
            zmessage(Filename, ErrorList, !IO),
            P1 = syntax
        ),
        processFile(Filename, Abbrev, F2, F, P1 - ST1, P, !IO)
    ;
        TResult = eof,
        F = F0,
        P = P0 - ST0
    ;
        TResult = error(S),
        zmessage(Filename, [S], !IO),
        F = F0,
        P = lexical - ST0
    ),
    ( if debugging(F0) then
        io.write_strings(["... finished ", Filename, "\n"], !IO)
    else
        true
    ).

:- pred usage(io::di, io::uo) is det.

usage(!IO) :-
    io.stderr_stream(StdErr, !IO),
    io.write_strings(StdErr, [
"Melbourne University Z typechecker, version 0.1\n",
"Copyright (C) 1996, 1997, 1998 The University of Melbourne\n",
"Usage: muz [options] <filename(s)>\n",
"Options:\n",
    "\t-a-, --no-abbreviate\n",
        "\t\tTurn off use of type abbreviations.\n",
    "\t-t <toolkit>, --toolkit <toolkit>\n",
        "\t\tTypecheck with the specified toolkit, overiding the\n",
        "\t\tbuiltin default and MUZ_TOOLKIT environment variable\n",
        "\t\t(-t- for typechecking without a toolkit).\n",

    "\t-?, -h, --help\n",
        "\t\tPrint this usage message.\n",
    "\t-d, --debug\n",
        "\t\tWrite debugging information to stdout.\n"
    ], !IO),
    io.set_exit_status(1, !IO).
