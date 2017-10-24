# vim: ft=dockerfile tw=78 ts=4 sw=4 et
ARG MERCURY_BOOTSTRAP_TAG=alpine
FROM sebgod/mercury-bootstrap:${MERCURY_BOOTSTRAP_TAG} as bootstrap

FROM sebgod/mercury-depend:${MERCURY_BOOTSTRAP_TAG}

ARG MERCURY_DEV_DEFAULT_GRADE=asm_fast.gc
ARG MERCURY_DEV_LIBGRADES=${MERCURY_DEV_DEFAULT_GRADE}
ARG MERCURY_DEV_PARALLEL=-j3

ENV MERCURY_TMP /var/tmp
ENV MERCURY_DEV_PREFIX /usr/local/mercury
ENV MERCURY_BOOTSTRAP_PREFIX /usr/local/mercury-bootstrap
ENV PATH_ORIG $PATH
ENV PATH ${MERCURY_BOOTSTRAP_PREFIX}/bin:$PATH

WORKDIR $MERCURY_TMP/mercury
COPY . .
COPY --from=bootstrap ${MERCURY_BOOTSTRAP_PREFIX} ${MERCURY_BOOTSTRAP_PREFIX}

RUN ( ./prepare.sh \
        && ./configure \ 
            --enable-libgrades=$MERCURY_DEV_DEFAULT_GRADE \
            --with-default-grade=$MERCURY_DEV_LIBGRADES \
            --prefix=$MERCURY_DEV_PREFIX \
        && make depend \
        && make PARALLEL=$MERCURY_DEV_PARALLEL \
        && make doc \
        && tools/bootcheck $MERCURY_DEV_PARALLEL \
        && cd stage2 \
        && make install PARALLEL=$MERCURY_DEV_PARALLEL \
        && rm -fR * \
        && rm -fR ${MERCURY_BOOTSTRAP_PREFIX} \
        )

ENV PATH ${MERCURY_DEV_PREFIX}/bin:${PATH_ORIG}

ENTRYPOINT ["mmc"]