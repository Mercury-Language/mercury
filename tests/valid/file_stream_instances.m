%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module file_stream_instances.
:- interface.

:- import_module io.

:- typeclass file_stream(Stream) where [].

:- instance file_stream(io.input_stream).
:- instance file_stream(io.output_stream).
:- instance file_stream(io.binary_input_stream).
:- instance file_stream(io.binary_output_stream).

:- implementation.

:- instance file_stream(io.input_stream) where [].
:- instance file_stream(io.output_stream) where [].
:- instance file_stream(io.binary_input_stream) where [].
:- instance file_stream(io.binary_output_stream) where [].
