
***************
Getting started
***************

============
Installation
============

MacOS X
-------
* Download `yacas-1.4.0-Darwin.dmg
  <https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-Darwin.dmg>`_
  from `<https://github.com/grzegorzmazur/yacas/releases/>`_
* Double-click the file to open it

Microsoft Windows
-----------------
* Depending on the Microsoft Windows version you use, download
  
  * `yacas-1.4.0-win64.exe
    <https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-win64.exe>`_
    (Windows 7 or newer, 64 bit)
  * `yacas-1.4.0-win32.exe
    <https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-win32.exe>`_
    (Windows 7 or newer, 32 bit)
  * `yacas-1.4.0-winxp.exe <https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-winxp.exe>`_ (Windows XP)

  from `<https://github.com/grzegorzmazur/yacas/releases/>`_
* Run the downloaded installator program, which will guide you through the installation process

Alternatively, download either `yacas-1.4.0-win64.zip
<https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-win64.zip/download>`_
or `yacas-1.4.0-win32.zip
<https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-win32.zip/download>`_
and unpack it. Then, you can run yacas by executing ``yacas.exe`` from
the ``bin`` subdirectory. This method, while not recommended, allows
one to use yacas without having to install it system-wide.

Ubuntu
------
* Download `yacas-1.4.0-1_amd64.deb
  <https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-1_amd64.deb>`_
  from `<https://github.com/grzegorzmazur/yacas/releases/>`_
* Double-click the file to open it in Ubuntu Software Center and click
  the :guilabel:`Install` button

=========================
Installation from sources
=========================

Getting sources
---------------

Version 1.4.0 can be downloaded from
`<https://github.com/grzegorzmazur/yacas/archive/v1.4.0.zip>`_ or
`<https://github.com/grzegorzmazur/yacas/archive/v1.4.0.tar.gz>`_,
while the current development version is accessible from
`<https://github.com/grzegorzmazur/yacas/archive/master.zip>`_.

Compilation
-----------

MacOS X
~~~~~~~

* open ``Terminal`` window
* change directory to the yacas source directory
* execute

  .. code-block:: bash
                
     mkdir build
     cd build
     cmake -G Xcode ..
                  
* open generated project in ``Xcode``

Microsoft Windows
~~~~~~~~~~~~~~~~~

* open ``Command Prompt`` window
* change directory to the yacas source directory
* execute

  .. code-block:: bat
                
     mkdir build
     cd build
     cmake -G "Visual Studio 14 2015 Win64" ..

* open generated project in ``Visual Studio``

Linux
~~~~~

* open ``Terminal`` window
* change directory to the yacas source directory
* execute

  .. code-block:: bash
                
     mkdir build
     cd build
     cmake ..
     make

* to install newly built binaries either ``make install`` or build the
  binary package using ``make package`` and install it

Java
~~~~
* open ``Terminal`` or ``Command Prompt`` window
* change directory to the yacas source directory
* execute ``ant jar``
