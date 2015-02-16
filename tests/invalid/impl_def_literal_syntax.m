%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module impl_def_literal_syntax.
:- interface.

:- import_module io.

:- pred f(io::di, io::uo) is det.
:- pred g(io::di, io::uo) is det.
:- pred h(io::di, io::uo) is det.

:- pred w($file).               % bad
:- pred $pred.                  % bad
:- $pred u.                     % bad
:- pred v(string::in($file)).   % bad

:- implementation.

:- import_module string.
:- import_module $file.         % bad

$line.                          % bad

:- inst myinst
    --->    $file.              % bad

:- inst myinst2
    --->    a($file).           % bad

:- inst myinst3($file) == ground. % bad

:- inst $file                   % bad
    --->    make_no_sense.

f(!IO) :-
    io.write($file ++ "hi", !IO). % ok

g(!IO) :-
    io.write($ file, !IO).      % bad

h(!IO) :-
    io.write($'file', !IO).     % bad

p($ME_GRIMLOCK).                % bad
q($123).                        % bad

:- pred r(character, character).
r($, ($)).                      % ok
