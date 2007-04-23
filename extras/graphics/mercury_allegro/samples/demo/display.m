%-----------------------------------------------------------------------------%

:- module display.
:- interface.

:- import_module allegro.
:- import_module allegro.bitmap.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- type animation_type
    --->    double_buffer
    /*;       page_flip*/
    .

:- some [D]
   pred init_display(int::in, int::in, animation_type::in,
            maybe(D)::out, io::di, io::uo) is det => display(D).

:- typeclass display(D) where [
    pred clear_display(D::in, D::out, io::di, io::uo) is det,
    pred prepare_display(bitmap::out, D::in, D::out, io::di, io::uo) is det,
    pred flip_display(D::in, D::out, io::di, io::uo) is det,
    pred destroy_display(D::in, io::di, io::uo) is det
].

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module allegro.blit.
:- import_module allegro.graphics.
:- import_module allegro.prim.

%-----------------------------------------------------------------------------%

init_display(Width, Height, AnimType, MaybeDisplay, !IO) :-
    (
        AnimType = double_buffer,
        create_bitmap(Width, Height, MaybeMemoryBuffer, !IO),
        (
            MaybeMemoryBuffer = yes(MemoryBuffer),
            MaybeDisplay = yes(double_buffer(MemoryBuffer))
        ;
            MaybeMemoryBuffer = no,
            MaybeDisplay = no
        )
    /*
    ;
        AnimType = page_flip,
        create_video_bitmap(Width, Height, MaybePage1, !IO),
        create_video_bitmap(Width, Height, MaybePage2, !IO),
        (if
            MaybePage1 = yes(Page1),
            MaybePage2 = yes(Page2)
        then
            MaybeDisplay = 'new myuniv'(yes(page_flip(Page1, Page2)))
        else
            MaybeDisplay = 'new myuniv'(no)
        )
    */
    ).

:- type myuniv
    ---> some [D] myuniv(get_myuniv :: maybe(D)) => display(D).

%-----------------------------------------------------------------------------%

:- type double_buffer ---> double_buffer(bitmap).

:- instance display(double_buffer) where [
    pred(clear_display/4)   is double_buffer_clear_display,
    pred(prepare_display/5) is double_buffer_prepare_display,
    pred(flip_display/4)    is double_buffer_flip_display,
    pred(destroy_display/3) is double_buffer_destroy_display
].

:- pred double_buffer_clear_display(double_buffer::in, double_buffer::out,
        io::di, io::uo) is det.

double_buffer_clear_display(DB @ double_buffer(Buffer), DB, !IO) :-
    det_screen(Screen, _, _, !IO),
    clear_bitmap(Screen, !IO),
    clear_bitmap(Buffer, !IO).

:- pred double_buffer_prepare_display(bitmap::out,
        double_buffer::in, double_buffer::out, io::di, io::uo) is det.

double_buffer_prepare_display(Buffer, DB @ double_buffer(Buffer), DB, !IO) :-
    clear_bitmap(Buffer, !IO).

:- pred double_buffer_flip_display(double_buffer::in, double_buffer::out,
        io::di, io::uo) is det.

double_buffer_flip_display(DB @ double_buffer(Buffer), DB, !IO) :-
    det_screen(Screen, ScreenW, ScreenH, !IO),
    blit(Buffer, Screen, 0, 0, 0, 0, ScreenW, ScreenH, !IO).

:- pred double_buffer_destroy_display(double_buffer::in, io::di, io::uo)
        is det.

double_buffer_destroy_display(double_buffer(Buffer), !IO) :-
    destroy_bitmap(Buffer, !IO).

%-----------------------------------------------------------------------------%

:- type page_flip ---> page_flip(bitmap, bitmap).

:- instance display(page_flip) where [
    pred(clear_display/4)   is page_flip_clear_display,
    pred(prepare_display/5) is page_flip_prepare_display,
    pred(flip_display/4)    is page_flip_flip_display,
    pred(destroy_display/3) is page_flip_destroy_display
].

:- pred page_flip_clear_display(page_flip::in, page_flip::out, io::di, io::uo)
        is det.

page_flip_clear_display(PF @ page_flip(Page1, Page2), PF, !IO) :-
    clear_bitmap(Page1, !IO),
    clear_bitmap(Page2, !IO),
    det_screen(Screen, _, _, !IO),
    show_video_bitmap(Screen, !IO).

:- pred page_flip_prepare_display(bitmap::out, page_flip::in, page_flip::out,
        io::di, io::uo) is det.

page_flip_prepare_display(B, page_flip(A, B), page_flip(B, A), !IO) :-
    clear_bitmap(B, !IO).

:- pred page_flip_flip_display(page_flip::in, page_flip::out, io::di, io::uo)
        is det.

page_flip_flip_display(PF @ page_flip(A, _), PF, !IO) :-
    show_video_bitmap(A, !IO).

:- pred page_flip_destroy_display(page_flip::in, io::di, io::uo) is det.

page_flip_destroy_display(page_flip(Page1, Page2), !IO) :-
    destroy_bitmap(Page1, !IO),
    destroy_bitmap(Page2, !IO).

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
