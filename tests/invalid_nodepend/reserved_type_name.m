%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module reserved_type_name.

:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type int
    --->    int_a
    ;       int_b.

:- type uint
    --->    uint_a
    ;       uint_b.

:- type int8
    --->    int8_a
    ;       int8_b.

:- type uint8
    --->    uint8_a
    ;       uint8_b.

:- type int16
    --->    int16_a
    ;       int16_b.

:- type uint16
    --->    uint16_a
    ;       uint16_b.

:- type int32
    --->    int32_a
    ;       int32_b.

:- type uint32
    --->    uint32_a
    ;       uint32_b.

:- type int64
    --->    int64_a
    ;       int64_b.

:- type uint64
    --->    uint64_a
    ;       uint64_b.

:- type float
    --->    float4_a
    ;       float4_b.

main(!IO) :-
    io.write_line(int_a, !IO),
    io.write_line(uint_a, !IO),
    io.write_line(int8_a, !IO),
    io.write_line(uint8_a, !IO),
    io.write_line(int16_a, !IO),
    io.write_line(uint16_a, !IO),
    io.write_line(int32_a, !IO),
    io.write_line(uint32_a, !IO),
    io.write_line(int64_a, !IO),
    io.write_line(uint64_a, !IO),
    io.write_line(float_a, !IO).
    % We get a "did you mean" suggestion about "float_a -> float"
    % that we do not get about e.g, "uint64_a" -> uint64" that seems jarring.
    % The reason for that is that
    %
    % - this module import module io,
    % - the io module import module term, and
    % - the term module has a function symbol named float, but does not have
    %   function symbols named e.g. uint64.
    %
    % We could restrict "did you mean" suggestions to only symbols that
    % are defined either in the current module or the modules it imports
    % directly (i.e. whose .int files it reads), leaving out the ones it
    % imports indirectly (i.e. whose .int2 files it reads). This would be
    % more consistent, but also less helpful. We choose helpfulness over
    % consistency.
