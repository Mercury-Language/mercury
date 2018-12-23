# vim: ft=dockerfile tw=78 ts=4 sw=4 et

# Supported Debian base images
#   - stretch
#   - jessie
ARG MERCURY_DEPEND_TAG=stretch
FROM debian:${MERCURY_DEPEND_TAG}

ARG MERCURY_DL=http://dl.mercurylang.org/deb/
ARG MERCURY_TMP=/var/tmp
ARG MERCURY_DEV_DEFAULT_GRADE=asm_fast.gc
ARG MERCURY_DEV_LIBGRADES=${MERCURY_DEV_DEFAULT_GRADE}
ARG MERCURY_DEV_PARALLEL=-j3
ARG MERCURY_DEV_STAGE=1
ARG MERCURY_DEV_PREFIX=/usr/local/mercury
# When using a source tarball, the source needs to be the top level directory,
# e.g. `mercury-srcdist-rotd-2017-10-19'
ARG MERCURY_DEV_SOURCE=.

ENV MERCURY_DEV_TARGET $MERCURY_TMP/mercury

# install packaged compiler for bootstrapping
RUN apt-get install -y curl && \
    ( curl -fsSL https://paul.bone.id.au/paul.asc | apt-key add - ) && \
    printf "deb $MERCURY_DL $(lsb_release -cs) main\ndeb-src $MERCURY_DL $(lsb_release -cs) main" \
        > /etc/apt/sources.list.d/mercury.list && \
    apt-get update && apt-get install -y \
        mercury-rotd-recommended

WORKDIR $MERCURY_DEV_TARGET
COPY ${MERCURY_DEV_SOURCE} .

# Checking for configure enables using
#   - `docker build http://uri.to.bootstrapped.tar.gz',
#   - `docker build https://github.com/:user:/mercury.git'
#   - `docker build .'
RUN ( \
        ([ -f ./configure ] || ./prepare.sh) \
        && ./configure \
            --enable-libgrades=$MERCURY_DEV_LIBGRADES \
            --with-default-grade=$MERCURY_DEFAULT_GRADE \
            --prefix=$MERCURY_DEV_PREFIX \
        && ([ -f ./configure ] || make depend) \
        && make PARALLEL=$MERCURY_DEV_PARALLEL \
        && tools/bootcheck $MERCURY_DEV_PARALLEL \
        && ([$MERCURY_DEV_STAGE -eq 1] \
            || ( cd stage2 && make install PARALLEL=$MERCURY_DEV_PARALLEL ) \
        ) \
        && rm -fR ${MERCURY_BOOTSTRAP_TARGET} \
        && rm -fR $MERCURY_DEV_TARGET \
    )

ENV PATH ${MERCURY_DEV_PREFIX}/bin:$PATH

ENTRYPOINT ["mmc"]
