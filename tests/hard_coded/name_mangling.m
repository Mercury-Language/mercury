%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module name_mangling.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- type 'a strange type name'
    --->    'a strange functor'
    ;       'another strange functor'.

:- type 'another wierd type' == int.

:- type (A, B) ---> (A, B).
:- inst (A, B) ---> (A, B).
:- func mk_pair(A, B) = (A, B).

:- pred 'this is a test' is det.

:- pred 'this is another test'('a strange type name'::out) is multi.

:- pred 'yet another test'('another wierd type'::out) is det.

:- implementation.
:- import_module solutions.

mk_pair(A, B) = (A, B).

'this is a test'.

'this is another test'('a strange functor').
'this is another test'('another strange functor').

'yet another test'(42).

main(!IO) :-
    'this is a test',
    io.write_line('a strange functor', !IO),
    io.write_line('another strange functor', !IO),
    solutions('this is another test', List),
    io.write_line(List, !IO).
