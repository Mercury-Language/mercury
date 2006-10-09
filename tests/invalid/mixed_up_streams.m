% rotd-2006-09-27 and before didn't complain if we pass a binary_input_stream
% where a binary_output_stream was expected.
:- module mixed_up_streams.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	io.binary_input_stream(BinaryInput, !IO),
	io.write_byte(BinaryInput, 0x1, !IO).
