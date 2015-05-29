
***************
Getting started
***************

============
Installation
============

MacOS X
-------

* Download `yacas-1.3.6-Darwin.dmg <http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-Darwin.dmg/download>`_
* Double-click the file to open it

Microsoft Windows
-----------------

* Depending on the version you use, download
    * `yacas-1.3.6-win64.exe <http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-win64.exe/download>`_ (Windows 7 or newer, 64 bit)
    * `yacas-1.3.6-win32.exe <http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-win32.exe/download>`_ (Windows 7 or newer, 32 bit)
    * `yacas-1.3.6-winxp.exe <http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-winxp.exe/download>`_ (Windows XP)
* Run the downloaded installator program, which will guide you through the installation process

Alternatively, download `yacas-1.3.6-win64.zip <http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-win64.zip/download>`_
and unpack it. Then, you can run yacas by executing yacas.exe from the bin subdirectory. This method, while not recommended , allows
one to use yacas without having to install it system-wide.

.. _installation_linux:

Linux
-----



=========================
Installation from sources
=========================

Getting sources
---------------

Either download `yacas-1.3.6.tar.gz <http://sourceforge.net/projects/yacas/files/yacas-source/1.3/yacas-1.3.6.tar.gz/download>`_
and unpack it or 

Compilation
-----------

MacOS X
~~~~~~~

Microsoft Windows
~~~~~~~~~~~~~~~~~

Linux
~~~~~

    mkdir build
    cd build
    cmake ..
    make

To install newly built binaries either make install or build the binary packages using make package and `install it <install_linux>`_