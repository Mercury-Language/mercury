% A sample Mercury servlet.
:- module servlet.

:- interface.

:- import_module io.

:- type request.
:- type response.

:- pred handle_get(request::in, response::in, io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module string.

handle_get(Request, Response, !IO) :-
    get_request_uri(Request, URI, !IO),
    set_content_type(Response, "text/html", !IO),
    Msg = string.append_list([
        "<html>\n",
        "<head>\n",
        "<title>Mercury App Engine Sample</title>\n",
        "</head>\n",
        "<body>\n",
        "<h2>You requested the URL: ", URI, "</h2>\n",
        "</body>\n",
        "</html>\n"]),
    write_response(Response, Msg, !IO).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("Java", "
import java.io.IOException;
import javax.servlet.http.*;
").

:- pragma foreign_code("Java", "
public static class Servlet extends HttpServlet {
    public void doGet(HttpServletRequest req, HttpServletResponse resp)
            throws IOException {
        jmercury.servlet.handle_get(req, resp);
    }
}
").

:- pragma foreign_export("Java", handle_get(in, in, di, uo), "handle_get").

:- pragma foreign_type("Java", request,
    "javax.servlet.http.HttpServletRequest").

:- pragma foreign_type("Java", response,
    "javax.servlet.http.HttpServletResponse").

:- pred write_response(response::in, string::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    write_response(Response::in, Str::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    try {
        Response.getWriter().print(Str);
    }
    catch (Exception e) {
        throw new RuntimeException(e);
    }
").

:- pred set_content_type(response::in, string::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    set_content_type(Response::in, ContentType::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    Response.setContentType(ContentType);
").

:- pred get_request_uri(request::in, string::out, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    get_request_uri(Request::in, URI::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury],
"
    URI = Request.getRequestURI();
").

:- end_module servlet.
