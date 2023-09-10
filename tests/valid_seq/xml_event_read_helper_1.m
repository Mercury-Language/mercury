%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%

:- module xml_event_read_helper_1.
:- interface.

:- type event_or_eof.

:- func get_value(string) = event_or_eof.

:- implementation.

get_value(S) = event(current_and_next_event(data(S), next_event_unknown_yet)).

:- type content_event
    --->    data(string).

:- type maybe_next_event
    --->    etag_past_empty_elem_tag(string)
    ;       next_event_unknown_yet.

:- type current_and_next_event
    --->    current_and_next_event(content_event, maybe_next_event).

:- type event_or_eof
    --->    event(current_and_next_event)
    ;       eof_event.
