#!/usr/bin/env bash
set -euo pipefail

CURL="curl -fsSkL --retry 9 --retry-delay 9"
GHRAW="https://raw.githubusercontent.com"
WORKDIR=$(mktemp -d)

cp vilpy.el "$WORKDIR"
cp vilpy-test.el "$WORKDIR"

cd "$WORKDIR"
pwd "$WORKDIR"

$CURL -O ${GHRAW}/abo-abo/avy/master/avy.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/swiper.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/ivy.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/ivy-overlay.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/ivy-faces.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/colir.el
$CURL -O ${GHRAW}/clojure-emacs/clojure-mode/master/clojure-mode.el

emacs -Q --batch -L . -f batch-byte-compile avy.el ivy.el swiper.el clojure-mode.el vilpy.el
emacs -Q -L . -batch -l ert -l clojure-mode -l vilpy-test.el -f ert-run-tests-batch-and-exit
