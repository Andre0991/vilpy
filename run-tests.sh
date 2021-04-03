#!/usr/bin/env bash
set -euo pipefail

CURL="curl -fsSkL --retry 9 --retry-delay 9"
GHRAW="https://raw.githubusercontent.com"
WORKDIR=$(mktemp -d)

cp lispy.el "$WORKDIR"
cp lispy-test.el "$WORKDIR"

cd "$WORKDIR"
pwd "$WORKDIR"

$CURL -O ${GHRAW}/victorhge/iedit/master/iedit.el
$CURL -O ${GHRAW}/victorhge/iedit/master/iedit-rect.el
$CURL -O ${GHRAW}/victorhge/iedit/master/iedit-lib.el
$CURL -O ${GHRAW}/abo-abo/avy/master/avy.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/swiper.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/ivy.el
$CURL -O ${GHRAW}/abo-abo/hydra/master/hydra.el
$CURL -O ${GHRAW}/abo-abo/hydra/master/lv.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/ivy-overlay.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/ivy-faces.el
$CURL -O ${GHRAW}/abo-abo/swiper/master/colir.el
$CURL -O ${GHRAW}/clojure-emacs/clojure-mode/master/clojure-mode.el

# TODO: Why is it necessary to byte compile lispy.el before running the cmd below?
emacs -Q --batch -L . -f batch-byte-compile iedit.el iedit-rect.el iedit-lib.el avy.el ivy.el swiper.el hydra.el clojure-mode.el lispy.el
emacs -Q -L . -batch -l ert -l clojure-mode -l lispy-test.el -f ert-run-tests-batch-and-exit