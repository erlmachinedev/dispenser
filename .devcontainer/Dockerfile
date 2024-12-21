# [Choice] alpine,...
ARG VARIANT="alpine"
FROM erlang:${VARIANT}

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=1001
ARG GRPNAME=vscodegrp

ENV DEBIAN_FRONTEND=noninteractive

RUN apk update

RUN apk add make
RUN apk add git
RUN apk add curl

RUN apk add --no-cache openssh

RUN addgroup -g $USER_GID $GRPNAME \
    && adduser -u $USER_UID -g $GRPNAME -D $USERNAME
USER $USERNAME
