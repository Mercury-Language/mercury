%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009, 2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: md4.
% Main author: wangp.
%
% This module contains an implementation of the MD4 message digest algorithm.
% The C code is adapted from:
%
%   a implementation of MD4 designed for use in the SMB authentication protocol
%   Copyright (C) Andrew Tridgell 1997-1998.
%
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; either version 2 of the License, or
%   (at your option) any later version.
%
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
%
%   You should have received a copy of the GNU General Public License
%   along with this program; if not, write to the Free Software
%   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
%
%-----------------------------------------------------------------------------%

:- module libs.md4.
:- interface.

:- func md4sum(string) = string.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.   % Required by non-C grades.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "local", "

#include ""mercury_string.h""

struct mdfour {
    MR_uint_least32_t   A;
    MR_uint_least32_t   B;
    MR_uint_least32_t   C;
    MR_uint_least32_t   D;
    MR_uint_least32_t   totalN;
    unsigned char       tail[64];
    unsigned            tail_len;
};

static void mdfour_begin(struct mdfour *md);
static void mdfour_update(struct mdfour *md, const unsigned char *in, int n);
static void mdfour_tail(struct mdfour *m, const unsigned char *in, int n);
static void mdfour_result(const struct mdfour *md, unsigned char out[12]);

").

:- pragma foreign_code("C", "

#define MASK32          (0xffffffff)

#define F(X,Y,Z)        ((((X)&(Y)) | ((~(X))&(Z))))
#define G(X,Y,Z)        ((((X)&(Y)) | ((X)&(Z)) | ((Y)&(Z))))
#define H(X,Y,Z)        (((X)^(Y)^(Z)))
#define lshift(x,s)     (((((x)<<(s))&MASK32) | (((x)>>(32-(s)))&MASK32)))

#define ROUND1(a,b,c,d,k,s) \
    a = lshift((a + F(b,c,d) + M[k]) & MASK32, s)
#define ROUND2(a,b,c,d,k,s) \
    a = lshift((a + G(b,c,d) + M[k] + 0x5A827999) & MASK32, s)
#define ROUND3(a,b,c,d,k,s) \
    a = lshift((a + H(b,c,d) + M[k] + 0x6ED9EBA1) & MASK32, s)

/* this applies md4 to 64 byte chunks */
static void mdfour64(struct mdfour *m, MR_uint_least32_t *M)
{
    MR_uint_least32_t AA, BB, CC, DD;
    MR_uint_least32_t A,B,C,D;

    A = m->A; B = m->B; C = m->C; D = m->D;
    AA = A; BB = B; CC = C; DD = D;

    ROUND1(A,B,C,D,  0,  3);  ROUND1(D,A,B,C,  1,  7);
    ROUND1(C,D,A,B,  2, 11);  ROUND1(B,C,D,A,  3, 19);
    ROUND1(A,B,C,D,  4,  3);  ROUND1(D,A,B,C,  5,  7);
    ROUND1(C,D,A,B,  6, 11);  ROUND1(B,C,D,A,  7, 19);
    ROUND1(A,B,C,D,  8,  3);  ROUND1(D,A,B,C,  9,  7);
    ROUND1(C,D,A,B, 10, 11);  ROUND1(B,C,D,A, 11, 19);
    ROUND1(A,B,C,D, 12,  3);  ROUND1(D,A,B,C, 13,  7);
    ROUND1(C,D,A,B, 14, 11);  ROUND1(B,C,D,A, 15, 19);

    ROUND2(A,B,C,D,  0,  3);  ROUND2(D,A,B,C,  4,  5);
    ROUND2(C,D,A,B,  8,  9);  ROUND2(B,C,D,A, 12, 13);
    ROUND2(A,B,C,D,  1,  3);  ROUND2(D,A,B,C,  5,  5);
    ROUND2(C,D,A,B,  9,  9);  ROUND2(B,C,D,A, 13, 13);
    ROUND2(A,B,C,D,  2,  3);  ROUND2(D,A,B,C,  6,  5);
    ROUND2(C,D,A,B, 10,  9);  ROUND2(B,C,D,A, 14, 13);
    ROUND2(A,B,C,D,  3,  3);  ROUND2(D,A,B,C,  7,  5);
    ROUND2(C,D,A,B, 11,  9);  ROUND2(B,C,D,A, 15, 13);

    ROUND3(A,B,C,D,  0,  3);  ROUND3(D,A,B,C,  8,  9);
    ROUND3(C,D,A,B,  4, 11);  ROUND3(B,C,D,A, 12, 15);
    ROUND3(A,B,C,D,  2,  3);  ROUND3(D,A,B,C, 10,  9);
    ROUND3(C,D,A,B,  6, 11);  ROUND3(B,C,D,A, 14, 15);
    ROUND3(A,B,C,D,  1,  3);  ROUND3(D,A,B,C,  9,  9);
    ROUND3(C,D,A,B,  5, 11);  ROUND3(B,C,D,A, 13, 15);
    ROUND3(A,B,C,D,  3,  3);  ROUND3(D,A,B,C, 11,  9);
    ROUND3(C,D,A,B,  7, 11);  ROUND3(B,C,D,A, 15, 15);

    A += AA; B += BB;
    C += CC; D += DD;

    A &= MASK32; B &= MASK32;
    C &= MASK32; D &= MASK32;

    m->A = A; m->B = B; m->C = C; m->D = D;
}

static void copy64(MR_uint_least32_t *M, const unsigned char *in)
{
    int i;

    for (i=0; i<16; i++) {
        M[i] = (in[i*4+3]<<24) | (in[i*4+2]<<16) |
               (in[i*4+1]<<8) | (in[i*4+0]<<0);
    }
}

static void copy4(unsigned char *out, MR_uint_least32_t x)
{
    out[0] = x & 0xFF;
    out[1] = (x>>8) & 0xFF;
    out[2] = (x>>16) & 0xFF;
    out[3] = (x>>24) & 0xFF;
}

static void mdfour_begin(struct mdfour *md)
{
    md->A = 0x67452301;
    md->B = 0xefcdab89;
    md->C = 0x98badcfe;
    md->D = 0x10325476;
    md->totalN = 0;
    md->tail_len = 0;
}

static void mdfour_update(struct mdfour *md, const unsigned char *in, int n)
{
    MR_uint_least32_t M[16];

    if (in == NULL) {
        mdfour_tail(md, md->tail, md->tail_len);
        return;
    }

    if (md->tail_len) {
        int len = 64 - md->tail_len;
        if (len > n) {
            len = n;
        }
        MR_memcpy(md->tail+md->tail_len, in, len);
        md->tail_len += len;
        n -= len;
        in += len;
        if (md->tail_len == 64) {
            copy64(M, md->tail);
            mdfour64(md, M);
            md->totalN += 64;
            md->tail_len = 0;
        }
    }

    while (n >= 64) {
        copy64(M, in);
        mdfour64(md, M);
        in += 64;
        n -= 64;
        md->totalN += 64;
    }

    if (n) {
        MR_memcpy(md->tail, in, n);
        md->tail_len = n;
    }
}

static void mdfour_tail(struct mdfour *m, const unsigned char *in, int n)
{
    unsigned char buf[128];
    MR_uint_least32_t M[16];
    MR_uint_least32_t b;

    m->totalN += n;

    b = m->totalN * 8;

    MR_memset(buf, 0, 128);
    if (n) {
        MR_memcpy(buf, in, n);
    }
    buf[n] = 0x80;

    if (n <= 55) {
        copy4(buf+56, b);
        copy64(M, buf);
        mdfour64(m, M);
    } else {
        copy4(buf+120, b);
        copy64(M, buf);
        mdfour64(m, M);
        copy64(M, buf+64);
        mdfour64(m, M);
    }
}

static void mdfour_result(const struct mdfour *m, unsigned char *out)
{
    copy4(out, m->A);
    copy4(out+4, m->B);
    copy4(out+8, m->C);
    copy4(out+12, m->D);
}

").

:- pragma no_determinism_warning(md4sum/1).
:- pragma foreign_proc("C",
    md4sum(In::in) = (Digest::out),
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
    const char hex[16] = ""0123456789abcdef"";
    struct mdfour md;
    unsigned char sum[16];
    char hexbuf[sizeof(sum) * 2 + 1];
    char *p;
    size_t i;

    mdfour_begin(&md);
    mdfour_update(&md, (const unsigned char *) In, strlen(In));
    mdfour_update(&md, NULL, 0);
    mdfour_result(&md, sum);

    /* Convert to hexadecimal string representation. */
    p = hexbuf;
    for (i = 0; i < sizeof(sum); i++) {
        *p++ = hex[(sum[i] & 0xf0) >> 4];
        *p++ = hex[(sum[i] & 0x0f)];
    }
    *p = '\\0';

    MR_make_aligned_string_copy(Digest, hexbuf);
").

%-----------------------------------------------------------------------------%

    % Implementation for non-C backends left as an exercise for the reader.
    %
md4sum(_) = _ :-
    sorry($file, $pred).

%-----------------------------------------------------------------------------%
:- end_module libs.md4.
%-----------------------------------------------------------------------------%
