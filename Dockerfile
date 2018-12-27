# vim: ft=dockerfile tw=78 ts=4 sw=4 et

# Supported Debian base images
#   - stretch
#   - jessie
ARG MERCURY_DEPEND_TAG=stretch
ARG MERCURY_DEV_PREFIX=/usr/local/mercury
ARG MERCURY_DEV_TARGET=/var/tmp/mercury

# first stage, base image
FROM debian:${MERCURY_DEPEND_TAG} AS base
RUN apt-get update && apt-get install -y \
    apt-utils \
    gcc \
    libc-dev \
    make \
    pkg-config

FROM base AS bootstrap

ARG MERCURY_DL=http://dl.mercurylang.org/deb/
ARG MERCURY_DEV_DEFAULT_GRADE=asm_fast.gc
ARG MERCURY_DEV_LIBGRADES=${MERCURY_DEV_DEFAULT_GRADE}
ARG MERCURY_DEV_PARALLEL=-j3
# When using a source tarball, the source needs to be the top level directory,
# e.g. `mercury-srcdist-rotd-2017-10-19'
ARG MERCURY_DEV_SOURCE=.
# inherited variables
ARG MERCURY_DEV_PREFIX
ARG MERCURY_DEV_TARGET

ENV APT_KEY_DONT_WARN_ON_DANGEROUS_USAGE y

# install packaged compiler for bootstrapping
# first install curl and lsb-realse so we can add the public key for downloading
# Mercury packages.
# Then add the remote repository and install all packages required for building
# the Mercury compiler from source.
RUN ( echo 'debconf debconf/frontend select Noninteractive' \
        | debconf-set-selections ) && \
    apt-get update && apt-get install -y \
        curl \
        gnupg2 \
        lsb-release && \
    ( curl -fsSL https://paul.bone.id.au/paul.asc | apt-key add - ) && \
    printf "%s $MERCURY_DL $(lsb_release -cs) main\n" "deb" "deb-src" \
        > /etc/apt/sources.list.d/mercury.list && \
    apt-get update && apt-get install -y \
        autoconf \
        automake \
        bison \
        flex \
        mercury-rotd-recommended

WORKDIR $MERCURY_DEV_TARGET
COPY ${MERCURY_DEV_SOURCE} .

# Checking for configure enables using
#   - `docker build http://uri.to.bootstrapped.tar.gz',
#   - `docker build https://github.com/:user:/mercury.git'
#   - `docker build .'
#
# Bootcheck fails currently: ( MAKE_DIR=`pwd`/scripts PATH=$PATH:`pwd`/scripts tools/bootcheck $MERCURY_DEV_PARALLEL ) \
RUN ( \
        ([ -f ./configure ] || ./prepare.sh) \
        && ./configure \
            --enable-libgrades=$MERCURY_DEV_LIBGRADES \
            --with-default-grade=$MERCURY_DEFAULT_GRADE \
            --prefix=$MERCURY_DEV_PREFIX \
            --disable-symlinks \
        && make PARALLEL=$MERCURY_DEV_PARALLEL install \
        && rm -fR ${MERCURY_BOOTSTRAP_TARGET} \
        && rm -fR $MERCURY_DEV_TARGET \
    )

FROM base AS compiler
ARG MERCURY_DEV_PREFIX
ARG MERCURY_DEV_TARGET
WORKDIR $MERCURY_DEV_TARGET
COPY --from=bootstrap $MERCURY_DEV_PREFIX $MERCURY_DEV_PREFIX
ENV PATH ${MERCURY_DEV_PREFIX}/bin:$PATH

ENTRYPOINT ["mmc"]
