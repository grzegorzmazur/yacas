#!/bin/sh

SFUSER=$1

if [ -z "$SFUSER" ]; then
    echo "usage: $0 <sourceforge_user_name>"
    exit 1
fi

ssh -x $SFUSER,yacas@shell.sourceforge.net create
cat homepage.tgz | ssh -x $SFUSER@shell.sourceforge.net 'tar zxvf - -C /home/project-web/yacas/htdocs'
ssh -x $SFUSER,yacas@shell.sourceforge.net shutdown
