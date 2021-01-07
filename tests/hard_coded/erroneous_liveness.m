%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module erroneous_liveness.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module char.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module univ.

:- type indexing
    --->    indexed
    ;       unindexed.

:- type cardinality == int. % >= 0

:- type entry == string.

:- type field
    --->    field(indexing, cardinality, set(entry)).

:- func init_field = field.
init_field = field(unindexed, 0, S) :- set.init(S).

%%%

:- type cost == int.        % >= 0

:- type buffer_size == int. % >= 0

:- type tuple
    --->    tuple(entry, entry, entry).

:- type table
    --->    table(cost, buffer_size, field, field, field, set(tuple)).
                     % supplier, part, job

:- func init_table = table.
init_table = table(0, 0, init_field, init_field, init_field, S) :- set.init(S).

%%%
:- type column
    --->    supplier
    ;       part
    ;       job.

:- type operation
    --->    index(column)
    ;       insert
    ;       buffer(buffer_size)
    ;       retrieve(column, entry)
    ;       retrieve_all.

%%%

main(!IO) :-
    set_globals(init_table, !IO),
    process_lines(!IO),
    get_globals(table(Cost, _, _, _, _, _), !IO),
    io.write_string("Total cost was ", !IO),
    io.write_int(Cost, !IO),
    io.nl(!IO).

:- pred set_globals(table::in, io::di, io::uo) is det.

set_globals(G, !IO) :-
    copy(G, G1),
    type_to_univ(G1, G2),
    io.set_globals(G2, !IO).

:- pred get_globals(table::out, io::di, io::uo) is det.

get_globals(G, !IO) :-
    io.get_globals(G0, !IO),
    ( if univ_to_type(G0, G1) then
        G = G1
    else
        error("get_globals/3---univ_to_type failed.")
    ).

:- pred process_lines(io::di, io::uo) is det.

process_lines(!IO) :-
    io.read_line(Result, !IO),
    (
        Result = ok(CharList),
        interpret(strings(CharList), !IO)
    ;
        Result = eof
    ;
        Result = error(Error),
        io.error_message(Error, Message),
        abort(Message, !IO)
    ).

:- pred interpret(list(string)::in, io::di, io::uo) is det.

interpret([], !IO) :-
    abort("empty line", !IO).
interpret([H | T], !IO) :-
    ( if H = "index" then
        oneArg(H, T, Arg, !IO),
        ( if Arg = "supplier" then
            Column = supplier
        else if Arg = "part" then
            Column = part
        else if Arg = "job" then
            Column = job
        else
            string.append("index on unrecognised column---",
                Arg, Message),
            abort(Message, !IO)
        ),
        operate(index(Column), !IO)
    else if H = "insert" then
        noArg(H, T, !IO),
        operate(insert, !IO)
    else if H = "buffer" then
        oneArg(H, T, Arg, !IO),
        ( if string.to_int(Arg, BSize0), BSize0 >= 0 then
            BSize = BSize0
        else
            string.append("buffer size must be non-negative integer---",
                Arg, Message),
            abort(Message, !IO)
        ),
        operate(buffer(BSize), !IO)
    else if H = "supplier" then
        oneArg(H, T, Arg, !IO),
        operate(retrieve(supplier, Arg), !IO)
    else if H = "part" then
        oneArg(H, T, Arg, !IO),
        operate(retrieve(part, Arg), !IO)
    else if H = "job" then
        oneArg(H, T, Arg, !IO),
        operate(retrieve(job, Arg), !IO)
    else if H = "spj" then
        noArg(H, T, !IO),
        operate(retrieve_all, !IO)
    else
        string.append("unrecognised command---", H, Message),
        abort(Message, !IO)
    ).

:- pred operate(operation::in, io::di, io::uo) is det.

operate(_, !IO).

:- pred noArg(string::in, list(string)::in, io::di, io::uo) is det.

noArg(_, [], !IO).
noArg(S, [_ | _], !IO) :-
    string.append("no args expected for command---", S, Message),
    abort(Message, !IO).

:- pred oneArg(string::in, list(string)::in, string::out,
    io::di, io::uo) is det.

oneArg(S, [], _, !IO) :-
    string.append("one arg expected for command---", S, Message),
    abort(Message, !IO).
oneArg(S, [H | T], H, !IO) :-
    ( if T = [] then
        true
    else
        string.append("only one arg expected for command---",
            S, Message),
        abort(Message, !IO)
    ).

:- func strings(list(char)) = list(string).

strings(CL) = SL :-
    strings(CL, [], [], SL1),
    list.reverse(SL1, SL).

:- pred strings(list(char), list(char), list(string), list(string)).
:- mode strings(in, in, in, out).

strings([], SCL, SL0, SL) :-
    addString(SCL, SL0, SL).
strings([H | T], SCL, SL0, SL) :-
    ( if char.is_whitespace(H) then
        SCL1 = [], addString(SCL, SL0, SL1)
    else
        SCL1 = [H | SCL], SL1 = SL0
    ),
    strings(T, SCL1, SL1, SL).

:- pred addString(list(char), list(string), list(string)).
:- mode addString(in, in, out).
addString(SCL, SL0, SL) :-
    (
        SCL = [],
        SL = SL0
    ;
        SCL = [_ | _],
        list.reverse(SCL, SCL1),
        string.from_char_list(SCL1, S),
        SL = [S | SL0]
    ).

:- pred abort(string::in, io::di, io::uo) is erroneous.

abort(Message, !IO) :-
    io.write_string("Error at line ", !IO),
    io.get_line_number(N, !IO),
    io.write_int(N, !IO),
    io.write_string(": ", !IO),
    error(Message).
%   io.write_string(Message),
%   io.nl,
%   io.set_exit_status(1).

