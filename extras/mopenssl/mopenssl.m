%---------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
% Copyright (C) 2015, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
%
% mopenssl.m
% Peter Ross <pro@missioncriticalit.com>
%
% Binding to the openssl library.
%
%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- module mopenssl.
:- interface.

:- import_module stream.
:- import_module net.
:- import_module net.tcp.

:- import_module io.
:- import_module list.

%------------------------------------------------------------------------------%

:- typeclass password_cb(T) where [
    pred password_cb(T::in, string::out, io::di, io::uo) is det
].

:- type ssl_exception
    --->    ssl_exception(
                list(string)
            ).

:- type ssl_method.
:- type ssl_ctx.

:- type ssl.

:- instance stream(ssl, io).

:- func sslv23_method = ssl_method.

:- pred ssl_ctx_new(ssl_method::in, ssl_ctx::out, io::di, io::uo) is det.

:- pred ssl_ctx_use_certificate_chain_file(ssl_ctx::in, string::in, io::di, io::uo) is det.

:- pred ssl_ctx_set_default_passwd_cb(ssl_ctx::in, T::in, io::di, io::uo) is det <= password_cb(T).

    % XXX need to add the flags.
:- pred ssl_ctx_use_private_key_file(ssl_ctx::in, string::in, io::di, io::uo) is det.

:- pred ssl_ctx_load_verify_locations(ssl_ctx::in, string::in, string::in, io::di, io::uo) is det.

    %
    % Create a ssl connection on top of a tcp connections.
    %
:- pred ssl(ssl_ctx::in, tcp::in, ssl::out, io::di, io::uo) is det.

:- pred ssl_connect(ssl::in, io::di, io::uo) is det.
:- pred ssl_accept(ssl::in, io::di, io::uo) is det.

    %
    % If the SSL connection is buffered flush the stream.
    %
:- pred ssl_flush(ssl::in, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module string.

:- pragma foreign_decl(c, "
#include <openssl/ssl.h>
#include <openssl/bio.h>
#include <openssl/err.h>
").

:- pragma foreign_decl(c, local, "
#define BUFFER_SIZE 2048

#ifdef MR_THREAD_SAFE
  static pthread_mutex_t *lock_cs;
  static long *lock_count;
#endif
").

:- initialise ssl_library_init/2.

    % This predicate must be called before any other predicate.
:- pred ssl_library_init(io::di, io::uo) is det.

:- pragma foreign_proc(c,
        ssl_library_init(IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    int i;

    SSL_library_init();
    SSL_load_error_strings();

#ifdef MR_THREAD_SAFE
    lock_cs=OPENSSL_malloc(CRYPTO_num_locks() * sizeof(pthread_mutex_t));
    lock_count=OPENSSL_malloc(CRYPTO_num_locks() * sizeof(long));

    for (i = 0; i < CRYPTO_num_locks(); i++) {
        lock_count[i]=0;
        pthread_mutex_init(&(lock_cs[i]), NULL);
    }

    CRYPTO_set_id_callback((unsigned long (*)())pthreads_thread_id);
    CRYPTO_set_locking_callback((void (*)())pthreads_locking_callback);
#endif

    IO = IO0;
").

:- pragma foreign_code(c, "
#ifdef MR_THREAD_SAFE
void pthreads_locking_callback(int mode, int type, char *file, int line)
{
    if (mode & CRYPTO_LOCK) {
        pthread_mutex_lock(&(lock_cs[type]));
        lock_count[type]++;
    } else {
        pthread_mutex_unlock(&(lock_cs[type]));
    }
}

unsigned long pthreads_thread_id(void)
{
    unsigned long ret;
    ret = (unsigned long) pthread_self();
    return ret;
}
#endif
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc(c, sslv23_method = (Method::out),
        [thread_safe, promise_pure], "
    Method = SSLv23_method();
").

%------------------------------------------------------------------------------%

:- pragma foreign_proc(c,
        ssl_ctx_new(Method::in, Context::out, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    Context = SSL_CTX_new(Method);
    IO = IO0;
").

:- pragma foreign_proc(c,
        ssl_ctx_use_certificate_chain_file(Context::in, File::in, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    if(!SSL_CTX_use_certificate_chain_file(Context, File)) {
        MOPENSSL_throw_error();
    };
    IO = IO0;
").

%------------------------------------------------------------------------------%

:- type password_cb
    ---> some [T] password_cb(T) => password_cb(T).

ssl_ctx_set_default_passwd_cb(Context, PasswordCB, !IO) :-
    Data = 'new password_cb'(PasswordCB),
    ssl_ctx_set_default_passwd_cb_2(Context, Data, !IO).

:- pred ssl_ctx_set_default_passwd_cb_2(ssl_ctx::in, password_cb::in, io::di, io::uo) is det.

:- pragma foreign_proc(c,
        ssl_ctx_set_default_passwd_cb_2(Context::in, Data::in, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    MR_Word *user_data;

#ifdef MR_CONSERVATIVE_GC
    user_data = MR_GC_NEW(MR_Word);
#else
    user_data = MR_NEW(MR_Word);
#endif

    *user_data = Data;

    SSL_CTX_set_default_passwd_cb(Context, MOPENSSL_password_cb);
    SSL_CTX_set_default_passwd_cb_userdata(Context, user_data);

    IO = IO0;
").

:- pragma foreign_decl(c, local, "
static int MOPENSSL_password_cb(char *buf, int size, int rwflag, void *user_data);
").

:- pragma foreign_code(c, "
static int MOPENSSL_password_cb(char *buf, int size, int rwflag, void *userData)
{
    MR_String password;
    MR_Word *user_data = (MR_Word *) userData;

    MOPENSSL_call_password_cb(*user_data, &password);

    strncpy(buf, (char *)(password), size);
    buf[size - 1] = '\\0';
    
    return strlen(buf);
}
").

:- pred call_password_cb(password_cb::in, string::out, io::di, io::uo) is det.
:- pragma foreign_export("C", call_password_cb(in, out, di, uo),
    "MOPENSSL_call_password_cb").
call_password_cb(password_cb(P), Password, !IO) :-
    password_cb(P, Password, !IO).

%------------------------------------------------------------------------------%

:- pragma foreign_decl(c, local, "
void MOPENSSL_throw_error(void);
").

:- pragma foreign_code(c, "
void MOPENSSL_throw_error()
{
    long error;
    char string[BUFFER_SIZE];
    MR_String s;
    MR_Word list;

    list = MOPENSSL_empty();

    error = ERR_get_error();
    while (error) {
        ERR_error_string_n(error, string, BUFFER_SIZE);

        s = MR_make_string((char *) ""%s"", string);
        list = MOPENSSL_cons(s, list);

        error = ERR_get_error();
    }

    MOPENSSL_throw_exception(list);

    return;
}
").

:- func empty = list(string).
:- pragma foreign_export("C", empty = out, "MOPENSSL_empty").
empty = [].

:- func mycons(string, list(string)) = list(string).
:- pragma foreign_export("C", mycons(in, in) = (out), "MOPENSSL_cons").
mycons(H, T) = [H|T].

:- pred throw_ssl_exception(list(string)::in) is erroneous.
:- pragma foreign_export("C", throw_ssl_exception(in),
    "MOPENSSL_throw_exception").
throw_ssl_exception(L) :-
    throw(ssl_exception(L)).

%------------------------------------------------------------------------------%

:- pragma foreign_proc(c,
        ssl_ctx_use_private_key_file(Context::in, File::in, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    if(!SSL_CTX_use_PrivateKey_file(Context, File, SSL_FILETYPE_PEM)) {
        MOPENSSL_throw_error();
    };
    IO = IO0;
").

:- pragma foreign_proc(c,
        ssl_ctx_load_verify_locations(Context::in, File::in, Path::in, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    if(!SSL_CTX_load_verify_locations(Context, File, Path)) {
        MOPENSSL_throw_error();
    };
    IO = IO0;
").


%------------------------------------------------------------------------------%

:- type ssl
    --->    ssl(
                name    :: string,
                handle  :: ssl_handle
            ).

ssl(Context, Tcp, SSL, !IO) :-
    FD = socket_fd(Tcp),
    Size = default_buffer_size,
    ssl_handle(Context, FD, Size, Handle, !IO),
    SSL = ssl("SSL", Handle).
    
    % The default block size for sending SSL is 16Kb.
    % Thus we will buffer a little bit less than this
    % to make sure that we are always sending full blocks.
    % At the moment we have 384 bytes left over.
:- func default_buffer_size = int.

default_buffer_size = 16000.

:- pred ssl_handle(ssl_ctx::in, int::in, int::in, ssl_handle::out, io::di, io::uo) is det.

:- pragma foreign_proc(c, ssl_handle(Context::in, FD::in, Size::in, Handle::out, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    SSL *ssl;
    BIO *bio, *buffer_bio;

    ssl = SSL_new(Context);

    bio = BIO_new_socket(FD, BIO_CLOSE);
    
        /* Create a buffer and link it with the socket */
    buffer_bio = BIO_new(BIO_f_buffer());
    BIO_set_buffer_size(buffer_bio, Size);
    bio = BIO_push(buffer_bio, bio);

    SSL_set_bio(ssl, bio, bio);

    Handle = MR_GC_NEW(MOPENSSL_ssl);
    Handle->ssl = ssl;

    IO = IO0;
").

%------------------------------------------------------------------------------%

ssl_flush(SSL, !IO) :-
    Handle = SSL ^ handle,
    ssl_flush_c(Handle, !IO).

:- pred ssl_flush_c(ssl_handle::in, io::di, io::uo) is det.

:- pragma foreign_proc(c, ssl_flush_c(Ssl::in, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    BIO_flush(SSL_get_wbio(Ssl->ssl));
    IO = IO0;
").

%------------------------------------------------------------------------------%

ssl_connect(SSL, !IO) :-
    Handle = SSL ^ handle,
    ssl_connect_c(Handle, !IO).

:- pred ssl_connect_c(ssl_handle::in, io::di, io::uo) is det.
:- pragma foreign_proc(c, ssl_connect_c(Ssl::in, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    if(SSL_connect(Ssl->ssl) <= 0) {
        MOPENSSL_throw_error();
    };
    IO = IO0;
").

ssl_accept(SSL, !IO) :-
    Handle = SSL ^ handle,
    ssl_accept_c(Handle, !IO).

:- pred ssl_accept_c(ssl_handle::in, io::di, io::uo) is det.
:- pragma foreign_proc(c, ssl_accept_c(Ssl::in, IO0::di, IO::uo),
        [thread_safe, promise_pure, tabled_for_io], "
    if(SSL_accept(Ssl->ssl) <= 0) {
        MOPENSSL_throw_error();
    };
    IO = IO0;
").
    

%------------------------------------------------------------------------------%

:- instance error(ssl_error).

:- instance stream(ssl, io) where [
	(stream__name(ssl(Name, _), Name, !IO))
].
:- instance error(ssl_error) where [
    (error_message(error(S)) = S)
].


:- instance input(ssl, io).
:- instance reader(ssl, character, io, ssl_error).

:- instance output(ssl, io).
:- instance writer(ssl, character, io).
:- instance writer(ssl, string, io).

:- instance input(ssl, io) where [].
:- instance reader(ssl, character, io, ssl_error) where [
    (get(S, Result, !IO) :-
        mopenssl.read_char(S ^ handle, C, IsCharRead, !IO),
        ( IsCharRead = yes,
            Result = ok(C)
        ; IsCharRead = no,
            mopenssl.get_error(S ^ handle, Err, IsError, !IO),
            ( IsError = yes,
                Result = error(error(Err))
            ; IsError = no,
                Result = eof
            )
        )
    )
].

:- instance output(ssl, io) where [
    pred(flush/3) is ssl_flush
].
:- instance writer(ssl, character, io) where [
    (put(Ssl, C, !IO) :-
        mopenssl.write_char(Ssl ^ handle, C, IsCharWritten, !IO),
        ( IsCharWritten = yes,
            true
        ; IsCharWritten = no,
            mopenssl.get_error(Ssl ^ handle, Err, _IsError, !IO),
            throw_ssl_exception(["put(char): " ++ Err])
        )
    )
].
:- instance writer(ssl, string, io) where [
    (put(Ssl, S, !IO) :-
        mopenssl.write_string(Ssl ^ handle, S, IsStrWritten, !IO),
        ( IsStrWritten = yes,
            true
        ; IsStrWritten = no,
            mopenssl.get_error(Ssl ^ handle, Err, _IsError, !IO),
            throw_ssl_exception(["put(char): " ++ Err])
        )
    )
].

%------------------------------------------------------------------------------%

:- type ssl_handle.
:- pragma foreign_type(c, ssl_handle, "MOPENSSL_ssl *").

:- pragma foreign_decl(c, local, "
typedef struct {
    SSL     *ssl;
} MOPENSSL_ssl;
").

:- type ssl_error ---> error(string).

:- pred get_error(ssl_handle::in, string::out, bool::out, io::di, io::uo)
    is det.
:- pragma foreign_proc(c,
    get_error(_Ssl::in, Msg::out, Success::out, _IO0::di, _IO::uo),
	[thread_safe, promise_pure, tabled_for_io],
"{
    long error;
    char string[BUFFER_SIZE];

    error = ERR_get_error();
    if (error) {
        ERR_error_string_n(error, string, BUFFER_SIZE);

        Msg = MR_make_string((char *) ""%s"", string);
		Success = MR_TRUE;
        
    } else {
        /* 
        ** We set Msg in case the debugger wants to print its value.
        */
        Msg = MR_make_string_const("""");
		Success = MR_FALSE;
    }
}").

:- pred read_char(ssl_handle::in, character::out, bool::out, io::di, io::uo)
    is det.
:- pragma foreign_proc(c, 
    read_char(Ssl::in, Chr::out, Success::out, _IO0::di, _IO::uo),
    [thread_safe, promise_pure, tabled_for_io],
"
    int nchars;
    nchars = SSL_read(Ssl->ssl, &Chr, 1);
    if (nchars > 0) {
		Success = MR_TRUE;
    } else if (nchars == 0) {
        Success = MR_FALSE;
        Chr = 0;
    } else {
        Success = MR_FALSE;
        Chr = 0;
    }
").

:- pred write_char(ssl_handle::in, character::in, bool::out, io::di, io::uo)
    is det.
:- pragma foreign_proc(c,
    write_char(Ssl::in, Chr::in, Success::out, _IO0::di, _IO::uo),
    [thread_safe, promise_pure, tabled_for_io],
"
    int nchars;
    nchars = SSL_write(Ssl->ssl, &Chr, 1);

    if (nchars > 0) {
        Success = MR_TRUE;
    } else {
        Success = MR_FALSE;
    }
").

:- pred write_string(ssl_handle::in, string::in, bool::out, io::di, io::uo)
    is det.
:- pragma foreign_proc(c,
    write_string(Ssl::in, Str::in, Success::out, _IO0::di, _IO::uo),
    [thread_safe, promise_pure, tabled_for_io],
"
    int nchars;
    int length;
    length = strlen(Str);

    /* fprintf(stderr, ""\\nAttempt to write %d: '%s'\\n"", length, Str); */
    if (length > 0) {
        nchars = SSL_write(Ssl->ssl, Str, length);

        /*
        switch (SSL_get_error(Ssl->ssl, nchars))
        {
            case SSL_ERROR_NONE:
                fprintf(stderr, ""SSL_ERROR_NONE\\n"");
                break;
            case SSL_ERROR_WANT_WRITE:
                fprintf(stderr, ""SSL_ERROR_WANT_WRITE\\n"");
                break;
            case SSL_ERROR_WANT_READ:
                fprintf(stderr, ""SSL_ERROR_WANT_READ\\n"");
                break;
            default:
                fprintf(stderr, ""default!!!\\n"");
                break;
        }

        fprintf(stderr, ""\\nWrote %d of %d: '%s'\\n"", nchars, length, Str);
        */

        if (nchars > 0) {
            Success = MR_TRUE;
        } else {
            Success = MR_FALSE;
        }
    } else {
        Success = MR_TRUE;
    }
").

%------------------------------------------------------------------------------%

    %
    % Place the foreign_type declarations in the interface,
    % but don't make them visual to casual inspection.
    %
:- interface.

:- pragma foreign_type(c, ssl_method, "SSL_METHOD *").
:- pragma foreign_type(c, ssl_ctx, "SSL_CTX *").

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et tw=0 wm=0
