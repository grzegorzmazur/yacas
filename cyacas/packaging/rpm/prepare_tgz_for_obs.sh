#/usr/bin/bash

YACAS_VERSION=1.8.0

wget https://github.com/grzegorzmazur/yacas/archive/v${YACAS_VERSION}.tar.gz
tar xzf v${YACAS_VERSION}.tar.gz
cd yacas-${YACAS_VERSION}/cyacas/yacas-gui/resources
mkdir external_packages
for u in `grep URL CMakeLists.txt | sed  "s/ *URL *//"`; do wget $u; mv `basename $u` external_packages; done
mv CMakeLists.txt CMakeLists.txt.orig
sed "s@https://github.com/.*/archive@\$\{CMAKE_CURRENT_SOURCE_DIR\}/external_packages@" < CMakeLists.txt.orig > CMakeLists.txt
cd ../../../..
tar czf yacas-${YACAS_VERSION}-obs.tar.gz yacas-${YACAS_VERSION}
rm -rf yacas-${YACAS_VERSION}
