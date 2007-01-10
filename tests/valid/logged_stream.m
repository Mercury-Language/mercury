% $ mmc -e logged_stream.m
% logged_stream.m:041: Inconsistent instance declaration for typeclass
% logged_stream.m:041:   `stream.input'/3 with functional dependency
% logged_stream.m:041:   `(Stream -> Error)'.
% logged_stream.m:027: Here is the conflicting instance.
:- module logged_stream.

:- interface.

:- import_module stream.
:- import_module io.

:- type logged_stream(S, L)
    --->    logged_stream(
                stream  :: S,
                logged  :: L
            ).

:- type logged_error(E)
    --->    error(E)
    .

:- instance stream(logged_stream(S, L), io) <= (stream(S, io), stream(L, io)).
:- instance error(logged_error(E)) <= error(E).

% Comment the following line out and this code compiles fine.
:- instance input(logged_stream(S, L), io, logged_error(E)) <= (input(S, io, E), output(L, io)).

:- implementation.

:- instance stream(logged_stream(S, L), io) <= (stream(S, io), stream(L, io)) where [
    (name(logged_stream(S, _), Name, !State) :-
        name(S, Name, !State)
    )
].

:- instance error(logged_error(E)) <= error(E) where [
    error_message(error(E)) = error_message(E)
].

:- instance input(logged_stream(S, L), io, logged_error(E)) <= (input(S, io, E), output(L, io)) where [].
