#!/bin/sh

cat homepage.tgz | ssh ayalpinkus@shell.sourceforge.net 'tar zxvf - -C /home/groups/y/ya/yacas/htdocs'

