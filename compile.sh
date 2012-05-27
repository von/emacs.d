#!/bin/sh
# Compile all my Emacs start up files.
#
# Kudos:
# https://curiousprogrammer.wordpress.com/2009/03/04/compiling-at-emacs-startup/

emacs --batch \
    -l setup-load-path.el \
    --eval "(batch-byte-compile-if-not-done)" \
    my-*.el setup-load-path.el init.el

exit 0

