%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% Try goal, rethrowing if no catch patterns match.

:- module try_syntax_3.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module string.

%---------------------------------------------------------------------------%

main(!IO) :-
    ( try []
        throw("This should not be caught.")
    then
        % unreachable
        X = "WRONG"
    else
        % unreachable
        X = "WRONG"
    catch E ->
        X = "caught int: " ++ string.from_int(E)
    catch E @ "This should be caught." ->
        X = "caught string: " ++ E
    % If none of the catch pattern match then the exception will be rethown.
    ),
    io.write_string(X, !IO),
    io.nl(!IO).
