FROM python:3-alpine


ENV PYFINTS_RELEASE 0.2.1
ENV PYFINTS_TAR_FILE v${PYFINTS_RELEASE}.tar.gz
ENV PYFINTS_FOLDER python-fints-${PYFINTS_RELEASE}
ENV PYTHONPATH=/app/${PYFINTS_FOLDER}

ADD https://github.com/raphaelm/python-fints/archive/${PYFINTS_TAR_FILE} /


RUN mkdir /app && \
    mkdir /fints2ledger && \
    tar -zxf ${PYFINTS_TAR_FILE} -C /app && \
    ls /app && \
    python /app/${PYFINTS_FOLDER}/setup.py install

VOLUME "/fints2ledger"

WORKDIR /fints2ledger