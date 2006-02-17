#!/bin/sh

#
# This code was supplied by Dan McMahill. It makes sure the tty is in the
# right state. Apparently this was needed on NetBSD.
#

# save the tty settings
stty -a | grep "\-extproc" 2>&1 > /dev/null
if [ $? = 0 ]; then
  OLD=-extproc
else
  OLD=extproc
fi
#OLD=`stty -g`

# make sure one critical tty setting is correct
stty -extproc

# run yacas
yacas $*

# restore the tty setting
stty $OLD

