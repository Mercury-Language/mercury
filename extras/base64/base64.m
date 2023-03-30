%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%------------------------------------------------------------------------------%
%
% Author: Michel Dehennin <mdh@missioncriticalit.com>
%
% Original C code is public domain code provided by TeX User Group
% http://www.tug.org/ftp/vm/base64-decode.c
% http://www.tug.org/ftp/vm/base64-encode.c
%
% This code is in the public domain.
%
%------------------------------------------------------------------------------%

:- module base64.
:- interface.

:- import_module list.

%------------------------------------------------------------------------------%

    % Encode a string in base64.
    %
:- pred encode64(string::in, string::out) is det.

    % Encodes a list of bytes as a base 64 string.
    %
    % Note this means that the integers must be in the range 0-255.
    % This constraint is not checked.
    %
:- pred encode64_byte_list(list(int)::in, string::out) is det.

    % Decodes a base64 string to clear text.
    %
    % WARNING: If the resulting string contains non-terminating null
    % characters, as in a PDF file for instance, the string is likely to
    % be truncated.
    %
:- pred decode64(string::in, string::out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module char.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module map.
:- import_module require.
:- import_module std_util.
:- import_module string.

%------------------------------------------------------------------------------%

encode64(Data, Base64) :-
    encode64(Data, string.length(Data), Base64).

:- pragma foreign_decl("C", "#include <string.h>").

:- pragma foreign_decl("C", local, "
static unsigned char alphabet[64] =
    \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\";
").

:- pred encode64(string::in, int::in, string::out) is det.

:- pragma foreign_proc("C",
    encode64(Data::in, Len::in, Base64::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_duplicate],
"
    MR_Word base64_buff;
    char *base64_ptr;

    int cols, bits, c, char_count;
    int i;

    // Base64 encoded data uses 4:3 times more space than the original string.
    // We need to foresee an extra MR_Word for the string terminator character.
    MR_offset_incr_hp_atomic(base64_buff, 0,
        ((((Len + 2) / 3) * 4) + sizeof(MR_Word)) / sizeof(MR_Word));
    base64_ptr = (char *) base64_buff;

    char_count = 0;
    bits = 0;
    cols = 0;

    for (i = 0; i < Len; i++) {
        // We need to cast to an unsigned char otherwise we might get negative
        // values.
        c = (unsigned char) Data[i];
        bits += c;

        char_count++;
        if (char_count == 3) {
            *base64_ptr++ = alphabet[bits >> 18];
            *base64_ptr++ = alphabet[(bits >> 12) & 0x3f];
            *base64_ptr++ = alphabet[(bits >> 6) & 0x3f];
            *base64_ptr++ = alphabet[bits & 0x3f];

            // Invalidates the size of allocated memory.
//          cols += 4;
//          if (cols == 72) {
//              putchar('\n');
//              cols = 0;
//          }
            bits = 0;
            char_count = 0;
        } else {
            bits <<= 8;
        }
    }

    if (char_count != 0) {
        bits <<= 16 - (8 * char_count);
        *base64_ptr++ = alphabet[bits >> 18];
        *base64_ptr++ = alphabet[(bits >> 12) & 0x3f];
        if (char_count == 1) {
            *base64_ptr++ = '=';
            *base64_ptr++ = '=';
        } else {
            *base64_ptr++ = alphabet[(bits >> 6) & 0x3f];
            *base64_ptr++ = '=';
        }
//      if (cols > 0) {
//          putchar('\n');
//      }
    }

    *base64_ptr = '\\0';

    Base64 = (char *) base64_buff;
").

:- pragma foreign_proc("C",
    decode64(Base64::in, Data::out),
    [will_not_call_mercury, thread_safe, promise_pure, may_not_duplicate],
"
    MR_Word data_buff;
    char *data_ptr;

    static char inalphabet[256], decoder[256];
    int i, bits, c, char_count, errors = 0;

    // Decoded data uses 3:4 of the space of the Base64 string.
    MR_offset_incr_hp_atomic(data_buff, 0,
        (((strlen(Base64) * 3) / 4) + sizeof(MR_Word)) / sizeof(MR_Word));
    data_ptr = (char *) data_buff;

    for (i = (sizeof alphabet) - 1; i >= 0 ; i--) {
        inalphabet[alphabet[i]] = 1;
        decoder[alphabet[i]] = i;
    }

    char_count = 0;
    bits = 0;
    for (i = 0; i < strlen(Base64); i++) {
        // We need to cast to an unsigned char, otherwise we might get
        // negative values.
        c = (unsigned char) Base64[i];
        if (c == '=') {
            break;
        }

        // Skip invalid characters.
        if (c > 255 || ! inalphabet[c]) {
            continue;
        }

        bits += decoder[c];
        char_count++;
        if (char_count == 4) {
            *data_ptr++ = (bits >> 16);
            *data_ptr++ = ((bits >> 8) & 0xff);
            *data_ptr++ = (bits & 0xff);
            bits = 0;
            char_count = 0;
        } else {
            bits <<= 6;
        }
    }

    if (i == strlen(Base64)) {
        if (char_count) {
            fprintf(stderr,
                \"base64 encoding incomplete: at least %d bits truncated\",
                ((4 - char_count) * 6));
            errors++;
        }
    } else { /* c == '=' */
        switch (char_count) {
            case 1:
                fprintf(stderr,
                    \"base64 encoding incomplete: at least 2 bits missing\");
                errors++;
                break;
            case 2:
                *data_ptr++ = (bits >> 10);
                break;
            case 3:
                *data_ptr++ = (bits >> 16);
                *data_ptr++ = ((bits >> 8) & 0xff);
                break;
        }
    }

    *data_ptr = '\\0';

    Data = (char *) data_buff;
").

    % We define our own version of map/2 so that we can apply the
    % --optimize-constructor-last-call optimization (this optimization
    % is turned off for the standard library by default).
    %
:- func base64.map(func(X) = Y, list(X)) = list(Y).

base64.map(_, []) =  [].
base64.map(F, [H0 | T0]) = [H | T] :-
    H = F(H0),
    T = base64.map(F, T0).

encode64_byte_list(Bytes, Base64) :-
    Chars = base64.map(char.det_from_int, Bytes),
    Length = list.length(Chars),
    String = string.from_char_list(Chars),
    encode64(String, Length, Base64).

%------------------------------------------------------------------------------%
:- end_module base64.
%------------------------------------------------------------------------------%
