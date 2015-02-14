%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Nested try goals.

:- module try_syntax_6.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

%---------------------------------------------------------------------------%

main(!IO) :-
    io.write_string("test_1:\n", !IO),
    test_1(!IO),

    io.write_string("\ntest_2:\n", !IO),
    test_2(!IO),

    io.write_string("\ntest_3:\n", !IO),
    test_3(!IO),

    io.write_string("\ntest_4:\n", !IO),
    test_4,

    io.write_string("\ntest_5:\n", !IO),
    test_5(!IO).

    % Nesting in try goal.
    %
:- pred test_1(io::di, io::uo) is cc_multi.

test_1(!IO) :-
    ( try []
        ( try []
            throw(3)
        then
            true
        catch 4 ->
            trace [io(!TIO)] (
                io.write_string("caught 4 (WRONG)\n", !TIO)
            )
        )
    then
        true
    else
        true
    catch 3 ->
        io.write_string("caught 3 (RIGHT)\n", !IO)
    ).

    % Nesting in then part.
    %
:- pred test_2(io::di, io::uo) is cc_multi.

test_2(!IO) :-
    ( try []
        (try []
            true
        then
            ( try []
                throw(3)
            then
                true
            catch 4 ->
                trace [io(!TIO)] (
                    io.write_string("caught 4 (WRONG)\n", !TIO)
                )
            )
        else
            true
        catch 3 ->
            trace [io(!TIO)] (
                io.write_string("caught 3 in middle try (WRONG)\n", !TIO)
            )
        )
    then
        true
    catch 3 ->
        io.write_string("caught 3 in outer try (RIGHT)\n", !IO)
    ).

    % Nesting in else part.
    %
:- pred test_3(io::di, io::uo) is cc_multi.

test_3(!IO) :-
    ( try []
        ( try []
            fail
        then
            true % unreachable
        else
            % Nesting in else part.
            (try []
                throw(1) % should NOT be caught by the outer try
            then
                true
            )
        catch 1 ->
            trace [io(!TIO)] (
                io.write_string("caught 1 in inner try (WRONG)\n", !TIO)
            )
        )
    then
        true
    catch 1 ->
        io.write_string("caught 1 in outer try (RIGHT)\n", !IO)
    ).

    % Nesting in catch part.
:- pred test_4 is det.

test_4 :-
    ( try []
        ( try []
            throw(1)
        then
            true
        catch 1 ->
            (try []
                throw(1)
            then
                true % unreachable
            catch 1 ->
                trace [io(!IO)] (
                    io.write_string("caught 1 in inner try (RIGHT)\n", !IO)
                ),
                throw(2)
            )
        catch 2 ->
            trace [io(!IO)] (
                io.write_string("caught 2 in middle try (WRONG)\n", !IO)
            )
        )
    then
        true
    catch 2 ->
        trace [io(!IO)] (
            io.write_string("caught 2 in outer try (RIGHT)\n", !IO)
        )
    ).

    % Nesting in catch_any part.
    %
:- pred test_5(io::di, io::uo) is cc_multi.

test_5(!IO) :-
    ( try []
        throw(1)
    then
        true
    catch_any X ->
        ( try []
            throw(X)
        then
            true
        catch 1 ->
            io.write_string("caught 1 (RIGHT)\n", !IO)
        )
    ).
