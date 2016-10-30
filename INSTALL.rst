============
Installation
============

Yacas is available for a variety of platforms. See 
`<http://www.yacas.org/getting_started/downloads/>`_ for binary packages
and installation instructions.

=========================
Installation from sources
=========================

Getting sources
---------------

Version 1.6.0 can be downloaded from
`<https://github.com/grzegorzmazur/yacas/archive/v1.6.0.zip>`_ or
`<https://github.com/grzegorzmazur/yacas/archive/v1.6.0.tar.gz>`_,
while the current development version is accessible from
`<https://github.com/grzegorzmazur/yacas/archive/develop.zip>`_.

Compilation
-----------

Common build options
^^^^^^^^^^^^^^^^^^^^
`ENABLE_CYACAS_CONSOLE`
   Build text console for the native yacas engine. Enabled by default.

`ENABLE_CYACAS_GUI`
   Build graphical interface for the native yacas engine. Requires Qt 5.5. Enabled by default.

`ENABLE_CYACAS_KERNEL`
   Build native yacas kernel for Jupyter Notebook. Requires Boost, ZeroMQ and zmqpp. Disabled by default.

`ENABLE_JYACAS`
   Build the Java yacas engine and text console for it. Disabled by default.

`ENABLE_DOCS`
   Generate HTML documentation. Disabled by default.

MacOS X
~~~~~~~

* Open ``Terminal`` window
* Change directory to the yacas source directory
* Execute

  .. code-block:: bash
                
     mkdir build
     cd build
     cmake -G Xcode [-Dcommon_option=value ...] ..
                  
* Open generated project in ``Xcode`` and build the Release variant

Microsoft Windows
~~~~~~~~~~~~~~~~~

* Open ``Command Prompt`` window
* Change directory to the yacas source directory
* Execute

  .. code-block:: bat
                
     mkdir build
     cd build
     cmake -G "Visual Studio 14 2015 Win64" [-Dcommon_option=value ...] ..

* Open generated project in ``Visual Studio`` and build the Release variant

Linux
~~~~~

* Open ``Terminal`` window
* Change directory to the yacas source directory
* Execute

  .. code-block:: bash
                
     mkdir build
     cd build
     cmake -DCMAKE_BUILD_TYPE=Release [-Dcommon_option=value ...] ..
     make

* To install newly built binaries execute ``make install``

Java
~~~~
* Open ``Terminal`` or ``Command Prompt`` window
* Change directory to the yacas source directory
* Execute ``ant jar``


yacas-online
~~~~~~~~~~~~

* build yacas using emscripten

  .. code-block:: bat

     mkdir build_js
     cd build_js
     cmake -DCMAKE_TOOLCHAIN_FILE=<EMSCRIPTEN_ROOT>/cmake/Modules/Platform/Emscripten.cmake \
     -DENABLE_CYACAS_GUI=No -DENABLE_CYACAS_KERNEL=No -DENABLE_JYACAS=No -DENABLE_DOCS=No -DCMAKE_BUILD_TYPE=Release ..
     cd ..

  where ``<EMSCRIPTEN_ROOT>`` stands for the Emscripten root directory

* copy

  * ``build_js/cyacas/yacas/yacas.js``
  * ``build_js/cyacas/yacas/yacas.js.mem``
  * ``cyacas/yacas-gui/resources/yacas-online.html``
  * ``cyacas/yacas-gui/resources/jquery/``
  * ``cyacas/yacas-gui/resources/mathbar/``
  * ``cyacas/yacas-gui/resources/plot3d/``
  * ``cyacas/yacas-gui/resources/yagy_ui/``

  to the installation directory
