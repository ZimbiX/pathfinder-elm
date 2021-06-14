FROM alpine

RUN apk add --update \
  bash \
  curl \
  jq \
  npm

RUN npm install npm@latest -g

ENV ELM_VERSION 0.19.1

RUN \
  curl -L -o elm.gz "https://github.com/elm/compiler/releases/download/$ELM_VERSION/binary-for-linux-64-bit.gz" && \
  gunzip elm.gz && \
  chmod +x elm && \
  mv elm /usr/local/bin/
