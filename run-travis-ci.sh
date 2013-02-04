#!/bin/sh -e

cd "$(dirname "$0")"

exec emacs --script ~/.emacs.d/init.el
