% This is an example program to test the CGI library.

:- module form_test.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module cgi, html, std_util, list, bool.

:- func my_url = string.
my_url = "http://hydra.cs.mu.oz.au/cgi-bin/mercury/form_test".

:- pred np(io__state::di, io__state::uo) is det.
np -->
	html__output_markup(np).

main -->
	cgi__get_form(MaybeFormEntries),
	(
		{ MaybeFormEntries = yes(FormEntries) }
	->
		html__output_content_type_html,
		html__output_header([title(text("Form Test"))]),
		html__output_markup(heading(1, text("Fields Entered:"))),
		print("<code>"),
		print(FormEntries),
		print("</code>"), np,
		print("<pre>"),
		write_list(FormEntries, "\n", print),
		print("</pre>"),
		np,
		html__output_form_start(my_url),
		print("Name: "),
		html__output_field("name", text(60, 90, "")), np,
		print("Address: "),
		html__output_field("address", textarea(3, 60, "")), np,
		print("Bozo?"),
		html__output_field("bozo", checkbox(no, "yes")), np,
		html__output_field("submit", submit("OK")), np,
		html__output_form_end
	;
		[]
	).
