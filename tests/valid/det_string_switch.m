% This is a regression test for a bug in MLDS code generation where it would
% abort while generating the code of a deterministic string switch.
%
% Uncaught Mercury exception:
% Software Error: ml_code_util.m: Unexpected: ml_gen_failure: `fail' has determinism `det'

:- module det_string_switch.
:- interface.

:- import_module io.

% Need this many cases to make a string switch.
:- inst bar
    --->    "a"
    ;       "b"
    ;       "c"
    ;       "d"
    ;       "e"
    ;       "f"
    ;       "g"
    ;       "h".

:- pred foo(string::in(bar), io::di, io::uo) is det.

:- implementation.

foo("a", !IO).
foo("b", !IO).
foo("c", !IO).
foo("d", !IO). 
foo("e", !IO).
foo("f", !IO).
foo("g", !IO).
foo("h", !IO).
