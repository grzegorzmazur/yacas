#!/bin/sh

#  yacas_UNINSTALL.sh
#  YACAS
#
#  Created by Marta Noga on 06.04.2014.
#
if [ APPLE ]
then
 INSTALL_PATH="/usr"
else
 INSTALL_PATH="/usr/local"
fi

echo "Removing binary"
rm ${INSTALL_PATH}/bin/yacas

echo "Removing libraries"
rm ${INSTALL_PATH}/lib/libyacas.*

echo "Removing scripts"
rm -rf ${INSTALL_PATH}/share/yacas

echo "Removing headers"
rm -rf ${INSTALL_PATH}/include/yacas

if [ APPLE ]
then
  echo "Removing framework"
  rm -rf /Library/Frameworks/yacas.framework
fi