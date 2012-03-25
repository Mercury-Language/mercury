%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
%
% A Mercury version of the GLFW listmodes example.
%
%-----------------------------------------------------------------------------%

:- module listmodes.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module glfw.
:- import_module int.
:- import_module list.
:- import_module string.

%-----------------------------------------------------------------------------%

main(!IO) :-
    glfw.init(!IO),
    glfw.get_desktop_mode(DTMode, !IO),
    io.format("Desktop mode: %d x %d x %d\n\n", [
        i(DTMode ^ width), 
        i(DTMode ^ height), 
        i(DTMode ^ red_bits + DTMode ^ green_bits + DTMode ^ blue_bits)], !IO),
    io.write_string("Available modes:\n", !IO),
    glfw.get_video_modes(VMs, !IO),
    list.foldl2(write_video_mode, VMs, 0, _, !IO),
    glfw.terminate(!IO).

:- pred write_video_mode(video_mode::in, int::in, int::out,
    io::di, io::uo) is det.

write_video_mode(VM, !N, !IO) :-
    VM = video_mode(W, H, R, G, B),
    io.format("%3d: %d x %d x %d\n", [i(!.N), i(W), i(H), i(R + G + B)], !IO),
    !:N = !.N + 1.

%-----------------------------------------------------------------------------%
:- end_module listmodes.
%-----------------------------------------------------------------------------%
