FROM ubuntu:latest

RUN DEBIAN_FRONTEND="noninteractive" apt-get update -qy &&\
    DEBIAN_FRONTEND="noninteractive" apt-get install haskell-platform haskell-stack git-all -qy &&\
    git clone https://github.com/pmbittner/FeatureTraceRecording.git /data/source &&\
    cd /data/source &&\
    git checkout ArtefactSubmission

WORKDIR /data/source
RUN stack update
RUN stack upgrade

## Enable colours in terminal
ENV TERM="xterm-256color" 
## Enable printing utf-8 colours
ENV LANG=C.UTF-8
RUN stack build --copy-bins

## this must be set after stack build to ensure the folder exists
ENV PATH=="/root/.local/bin:${PATH}"

RUN echo "\n\n### DONE ###\n\n"

CMD stack run