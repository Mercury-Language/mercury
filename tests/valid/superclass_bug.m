:- module superclass_bug.
:- interface.

:- type s ---> s.
:- type e ---> e.
:- type c ---> c.

:- typeclass my_stream(Stream, State) <= (Stream -> State) where
[
         pred name(Stream::in, string::out, State::di, State::uo) is det
].

:- typeclass my_god(Stream, State, Error)
     <= ( my_stream(Stream, State), (Stream -> Error) ) where [].

:- typeclass my_input(Stream, Unit, State, Error)
     <= my_god(Stream, State, Error) where [].

:- type tab_expander(S).

:- instance my_stream(tab_expander(S), s)
     <= my_input(S, c, s, e).

:- implementation.

:- type tab_expander(S)
     --->    tab_expander(int, S).

:- instance my_stream(tab_expander(S), s)
     <= my_input(S, c, s, e) where 
[
    pred(name/4) is foo 
].

:- pred foo(tab_expander(S)::in, string::out, s::di, s::uo) is det
 	<= my_input(S, c, s, e).
foo(tab_expander(_, Stream), Name, !S) :-
 	name(Stream, Name, !S).
