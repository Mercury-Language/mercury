%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et

% This is an example program to test the CGI library.

:- module form_test.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module cgi.
:- import_module html.

:- import_module bool.
:- import_module list.
:- import_module maybe.

%----------------------------------------------------------------------------%

    % NOTE: you will need to change this to suit your system.
    %
:- func my_url = string.

my_url = "http://hydra.cs.mu.oz.au/cgi-bin/mercury/form_test".

:- pred np(io::di, io::uo) is det.

np(!IO) :-
    html.output_markup(np, !IO).

main(!IO) :-
    cgi.get_form(MaybeFormEntries, !IO),
    (
        MaybeFormEntries = yes(FormEntries),
        html.output_content_type_html(!IO),
        html.output_header([title(text("Form Test"))], !IO),
        html.output_markup(heading(1, text("Fields Entered:")), !IO),
        io.print("<code>", !IO),
        io.print(FormEntries, !IO),
        io.print("</code>", !IO), np(!IO),
        io.print("<pre>", !IO),
        io.write_list(FormEntries, "\n", print, !IO),
        io.print("</pre>", !IO), np(!IO),
        html.output_form_start(my_url, !IO),
        io.print("Name: ", !IO),
        html.output_field("name", text(60, 90, ""), !IO), np(!IO),
        io.print("Address: ", !IO),
        html.output_field("address", textarea(3, 60, ""), !IO), np(!IO),
        io.print("Bozo?", !IO),
        html.output_field("bozo", checkbox(no, "yes"), !IO), np(!IO),
        html.output_field("submit", submit("OK"), !IO), np(!IO),
        html.output_form_end(!IO)
    ;
        MaybeFormEntries = no
    ).
