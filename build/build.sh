#!/bin/sh

here=`dirname $0`
projectRoot="$here/.."
r=`cd "$projectRoot" && pwd`
p="$r/package"
p2="$p/docs/en"
s="$r/dist/webradar-document"

(
  cabal install filepather && \
  cabal install missingh && \
  cd "$r" && \
  bin/spellcheck-noninteractive && \
  bin/lint && \
  bin/all && \
  ( if [ -d "$p" ]; then rm -rf "$p"; fi ) && \
  mkdir -p "$p2" && \
  cp -R "$s"/* $p2
)
