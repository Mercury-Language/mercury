%-----------------------------------------------------------------------------%

:- module mt.

:- interface.

:- type mt.

:- pred init_genrand(int::in, mt::uo) is det.
:- pred genrand_int32(int::out, mt::di, mt::uo) is det.

:- import_module int.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("C", mt, "MTState *").

:- pragma foreign_proc("C",
    init_genrand(Seed::in, State::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    State = _mt__init_genrand(Seed);
").

:- pragma foreign_proc("C",
    genrand_int32(Value::out, State0::di, State::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = _mt__genrand_int32(State0);
    State = State0;
").

:- pragma foreign_decl("C",
"
typedef struct MTState MTState;

MTState *_mt__init_genrand(unsigned long s);
unsigned long _mt__genrand_int32(MTState *st);
").

:- pragma foreign_code("C",
"
/* Period parameters */  
#define N 624
#define M 397
#define MATRIX_A 0x9908b0dfUL   /* constant vector a */
#define UPPER_MASK 0x80000000UL /* most significant w-r bits */
#define LOWER_MASK 0x7fffffffUL /* least significant r bits */

struct MTState {
    unsigned long mt[N]; /* the array for the state vector  */
    int mti; /* mti==N+1 means mt[N] is not initialized */
};

/* initializes mt[N] with a seed */
MTState *_mt__init_genrand(unsigned long s)
{
    MTState *st = MR_GC_NEW(MTState);

    st->mt[0]= s & 0xffffffffUL;
    for (st->mti=1; st->mti<N; st->mti++) {
        st->mt[st->mti] = 
	    (1812433253UL * (st->mt[st->mti-1] ^ (st->mt[st->mti-1] >> 30)) + st->mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        st->mt[st->mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }

    return st;
}

/* generates a random number on [0,0xffffffff]-interval */
unsigned long _mt__genrand_int32(MTState *st)
{
    unsigned long y;
    static unsigned long mag01[2]={0x0UL, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    if (st->mti >= N) { /* generate N words at one time */
        int kk;

#if 0
        if (st->mti == N+1)   /* if init_genrand() has not been called, */
            init_genrand(st, 5489UL); /* a default initial seed is used */
#endif

        for (kk=0;kk<N-M;kk++) {
            y = (st->mt[kk]&UPPER_MASK)|(st->mt[kk+1]&LOWER_MASK);
            st->mt[kk] = st->mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        for (;kk<N-1;kk++) {
            y = (st->mt[kk]&UPPER_MASK)|(st->mt[kk+1]&LOWER_MASK);
            st->mt[kk] = st->mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
        }
        y = (st->mt[N-1]&UPPER_MASK)|(st->mt[0]&LOWER_MASK);
        st->mt[N-1] = st->mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1UL];

        st->mti = 0;
    }
  
    y = st->mt[st->mti++];

    /* Tempering */
    y ^= (y >> 11);
    y ^= (y << 7) & 0x9d2c5680UL;
    y ^= (y << 15) & 0xefc60000UL;
    y ^= (y >> 18);

    return y;
}
").

%-----------------------------------------------------------------------------%
% vi:ft=mercury:ts=8:sts=4:sw=4:et
