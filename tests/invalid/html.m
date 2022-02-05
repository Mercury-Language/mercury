%-----------------------------------------------------------------------------%
% vim: ft=mercury ff=unix ts=4 sw=4 et
%-----------------------------------------------------------------------------%

:- module html.
:- interface.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % Elements that are allowed inside the body tag of an HTML document
    %
:- type body_elem
    --->    ul(list(body_elem))     % Unordered list tag
    ;       li(list(body_elem))     % List item tag
    ;       text(string).

%-----------------------------------------------------------------------------%

    % Top-level body elements.
    % The li tag is not allowed as a top-level element. It must be a direct
    % child of the ul tag.
    %
:- inst top_body_elem for body_elem/0
    --->    ul(non_empty_list(li(top_body_elem)))
            % The ul tag allows only li tags as top level children.

    ;       text(ground).

:- inst li(I) for body_elem/0
    --->    li(non_empty_list(I)).

%-----------------------------------------------------------------------------%

:- inst non_empty_list(I) for list/1
    --->    [I | list_skel(I)].

%-----------------------------------------------------------------------------%

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
:- implementation.
%-----------------------------------------------------------------------------%

main(!IO).

:- func init_body_elem =
    (list(body_elem)::out(non_empty_list(top_body_elem))) is det.

init_body_elem =
    % [ul([li([text("List item")])])].  % This line compiles without errors

    [ul([li([])])].
        % Mantis bug #528 was about this line causing a compiler abort.
        % Since that bug has been fixed, the expected result is an error
        % message about the mode error on this line (the `li' tag requires
        % a non-empty list as its argument).
