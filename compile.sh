#!/bin/sh

# Exit on error
set -e

emacs="/Applications/DarwinPorts/Emacs.app/Contents/MacOS/Emacs"

# Compile any files that aren't yet compiled
for file in *.el; do
  if test ! -e ${file}c ; then
    echo "Compiling $file"
    $emacs -batch -nw -u ${USER} -f batch-byte-recompile-directory .
  fi
done

# Now have emacs recompile any that need it
$emacs -batch -nw -u ${USER} -f batch-byte-recompile-directory .
