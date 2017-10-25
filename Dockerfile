# vim: ft=dockerfile tw=78 ts=4 sw=4 et

# Supported base image types:
#   - latest: image based on the latest Debian release
#   - alpine: Alpine Linux generates smaller images,
#             but creating the doucmentation does not work yet.
ARG MERCURY_BOOTSTRAP_TAG=latest
FROM sebgod/mercury-bootstrap:${MERCURY_BOOTSTRAP_TAG} as bootstrap

FROM sebgod/mercury-depend:${MERCURY_BOOTSTRAP_TAG}

ARG MERCURY_TMP=/var/tmp
ARG MERCURY_DEV_DEFAULT_GRADE=asm_fast.gc
ARG MERCURY_DEV_LIBGRADES=${MERCURY_DEV_DEFAULT_GRADE}
ARG MERCURY_DEV_PARALLEL=-j3
ARG MERCURY_DEV_STAGE=1
ARG MERCURY_DEFAULT_PREFIX=/usr/local
ARG MERCURY_DEV_PREFIX=${MERCURY_DEFAULT_PREFIX}/mercury
# The prefix of an existing Mercury installation,
# by default the one from the bootstrap stage.
# If required, can be unset s.th. no bootstrapping will occur.
ARG MERCURY_BOOTSTRAP_PREFIX=${MERCURY_DEFAULT_PREFIX}/mercury-bootstrap
# When using a source tarball, the source needs to be the top level directory,
# e.g. `mercury-srcdist-rotd-2017-10-19'
ARG MERCURY_DEV_SOURCE=.

# if bootstrap prefix is defined, add it to the path
ENV PATH_ORIG $PATH
ENV PATH ${MERCURY_BOOTSTRAP_PREFIX:+${MERCURY_BOOTSTRAP_PREFIX}/bin:}$PATH
# if MERCURY_BOOSTRAP_PREFIX is not set, turn the `COPY --from=bootstrap' operation
# into a no-op.
ENV MERCURY_BOOTSTRAP_SOURCE ${MERCURY_BOOTSTRAP_PREFIX:-${MERCURY_TMP}}
ENV MERCURY_BOOTSTRAP_TARGET ${MERCURY_BOOTSTRAP_PREFIX:-${MERCURY_TMP}/bootstrap}
ENV MERCURY_DEV_TARGET $MERCURY_TMP/mercury

WORKDIR $MERCURY_DEV_TARGET
COPY ${MERCURY_DEV_SOURCE} .
COPY --from=bootstrap ${MERCURY_BOOTSTRAP_SOURCE} ${MERCURY_BOOTSTRAP_TARGET}

# Checking for configure enables using `docker build http://uri.to.bootstrapped.tar.gz',
# `docker build https://github.com/:user:/mercury.git' and `docker build .'
RUN ( \
        ([ -f ./configure ] || ./prepare.sh) \
        && ./configure \
            --enable-libgrades=$MERCURY_DEV_DEFAULT_GRADE \
            --with-default-grade=$MERCURY_DEV_LIBGRADES \
            --prefix=$MERCURY_DEV_PREFIX \
        && ([ -f ./configure ] || make depend) \
        && make PARALLEL=$MERCURY_DEV_PARALLEL \
        && make doc \
        && tools/bootcheck $MERCURY_DEV_PARALLEL \
        && ([$MERCURY_DEV_STAGE -eq 1] \
            || ( cd stage2 && make install PARALLEL=$MERCURY_DEV_PARALLEL ) \
        ) \
        && rm -fR ${MERCURY_BOOTSTRAP_TARGET} \
        && rm -fR $MERCURY_DEV_TARGET \
    )

# Using PATH_ORIG ensures that we do not include the now deleted bootstrapped compiler
ENV PATH ${MERCURY_DEV_PREFIX}/bin:$PATH_ORIG

ENTRYPOINT ["mmc"]