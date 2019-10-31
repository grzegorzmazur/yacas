---
layout: page-fullwidth
title: Downloads
subheadline: All of yacas at your fingertips
permalink: /getting_started/downloads/
breadcrumb: true
---

<div class="row">
<div class="medium-4 medium-push-8 columns" markdown="1">
<div class="panel radius" markdown="1">
**Table of Contents**
{: #toc }
*  TOC
{:toc}
</div>
</div>

<div class="medium-8 medium-pull-4 columns" markdown="1">

## Current stable version (1.8.0)
* source code: [tgz](https://github.com/grzegorzmazur/yacas/archive/v1.8.0.tar.gz) and [zip](https://github.com/grzegorzmazur/yacas/archive/v1.8.0.zip)

## Development version
  * [repository](https://github.com/grzegorzmazur/yacas/tree/develop)
  * [source code](https://github.com/grzegorzmazur/yacas/archive/develop.zip)

## Archive
* Current stable version (1.7.0)
  * source code: [tgz](https://github.com/grzegorzmazur/yacas/archive/v1.7.0.tar.gz) and [zip](https://github.com/grzegorzmazur/yacas/archive/v1.7.0.zip)
  * binary packages for
    * [Java](https://github.com/grzegorzmazur/yacas/releases/download/v1.7.0/yacas-1.7.0.jar)
    * Microsoft Windows (7 or newer)
      * [archive (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.7.0/yacas-1.7.0-win64.zip)
* yacas version 1.6.1
  * source code: [tgz](https://github.com/grzegorzmazur/yacas/archive/v1.6.1.tar.gz) and [zip](https://github.com/grzegorzmazur/yacas/archive/v1.6.1.zip)
  * binary packages for
    * [Java](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-1.6.1.jar)
    * Linux
      * Distribution-independent
        * Snap
          * Make sure you have snap installed
          * Open terminal and execute `snap install yacas`
          * To run yacas in text console execute `yacas`
          * To run yacas in graphical mode execute `yacas.gui`
        * Flatpak
          * Make sure you have flatpak installed
          * Open terminal and execute
            `flatpak remote-add --from gnome https://sdk.gnome.org/gnome.flatpakrepo`
            followed by `flatpak install gnome org.gnome.Platform//3.22`
          * Download [text](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-1.6.1.flatpak) and/or [graphical](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-gui-1.6.1.flatpak) yacas console bundles.
          * Execute `flatpak install  --user --bundle yacas-1.6.1.flatpak` to install yacas text console and/or `flatpak install  --user --bundle yacas-gui-1.6.1.flatpak` to install yacas graphical console
          * To run yacas in text console execute `flatpak run org.yacas.yacas`
          * To run yacas in graphic mode execute `flatpak run org.yacas.yacas-gui`
      * RedHat
        * Fedora 24 (64 bit):
          * [common files (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-common-1.6.1-1.fc24.x86_64.rpm)
          * [documentation (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-doc-1.6.1-1.fc24.x86_64.rpm)
          * [text console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-console-1.6.1-1.fc24.x86_64.rpm)
          * [graphical console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-gui-1.6.1-1.fc24.x86_64.rpm)
          * [development (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-devel-1.6.1-1.fc24.x86_64.rpm)
          * [debug info (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-debuginfo-1.6.1-1.fc24.x86_64.rpm)
        * Fedora 23 (64 bit):
          * [common files (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-common-1.6.1-1.fc23.x86_64.rpm)
          * [documentation (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-doc-1.6.1-1.fc23.x86_64.rpm)
          * [text console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-console-1.6.1-1.fc23.x86_64.rpm)
          * [graphical console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-gui-1.6.1-1.fc23.x86_64.rpm)
          * [development (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-devel-1.6.1-1.fc23.x86_64.rpm)
          * [debug info (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-debuginfo-1.6.1-1.fc23.x86_64.rpm)
        * installation:
          * download all mandatory files
          * download at least one of alternative files
          * optionally, download the optional files
          * open terminal and execute `sudo dnf install Downloads/yacas-*-1.6.1-1.fc*.x86_64.rpm`
      * Ubuntu
        * 16.10 (64 bit):
          * [common files (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-common_1.6.1-1yakkety1_all.deb)
          * [documentation (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-doc_1.6.1-1yakkety1_all.deb)
          * [text console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-console_1.6.1-1yakkety1_amd64.deb)
          * [graphical console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-gui_1.6.1-1yakkety1_amd64.deb)
          * [development (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-dev_1.6.1-1yakkety1_amd64.deb)
          * [Jupyter Notebook kernel (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-kernel_1.6.1-1yakkety1_amd64.deb)
        * 16.04 (64 bit):
          * [common files (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-common_1.6.1-1xenial1_all.deb)
          * [documentation (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-doc_1.6.1-1xenial1_all.deb)
          * [text console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-console_1.6.1-1xenial1_amd64.deb)
          * [graphical console (alternative)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-gui_1.6.1-1xenial1_amd64.deb)
          * [development (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-dev_1.6.1-1xenial1_amd64.deb)
          * [Jupyter Notebook kernel (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-kernel_1.6.1-1xenial1_amd64.deb)
        * installation:
          * download all mandatory files
          * download at least one of alternative files
          * optionally, download the optional files
          * open terminal and execute `sudo apt install ./Downloads/yacas-*_1.6.1-1*1_*.deb`
    * [macOS](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-1.6.1-macOS.pkg)
    * Microsoft Windows (7 or newer)
      * [archive (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.6.1/yacas-1.6.1-win64.zip)
* yacas version 1.5.0
  * source code: [tgz](https://github.com/grzegorzmazur/yacas/archive/v1.5.0.tar.gz) and [zip](https://github.com/grzegorzmazur/yacas/archive/v1.5.0.zip)
  * binary packages for
    * [Java](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0.jar)
    * Linux
      * RedHat
        * Fedora 23 (64 bit):
          * [application (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0-1.fc23.x86_64.rpm)
          * [documentation (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-doc-1.5.0-1.fc23.x86_64.rpm)
          * [development (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-devel-1.5.0-1.fc23.x86_64.rpm)
          * [debug info (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-debuginfo-1.5.0-1.fc23.x86_64.rpm)
        * Fedora 22 (64 bit):
          * [application (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0-1.fc22.x86_64.rpm)
          * [documentation (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-doc-1.5.0-1.fc22.x86_64.rpm)
          * [development (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-devel-1.5.0-1.fc22.x86_64.rpm)
          * [debug info (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-debuginfo-1.5.0-1.fc22.x86_64.rpm)
      * Ubuntu
        * 16.04 (64 bit):
          * [application (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas_1.5.0-1xenial1_amd64.deb)
          * [documentation (mandatory)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-doc_1.5.0-1xenial1_all.deb)
          * [development (optional)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-dev_1.5.0-1xenial1_amd64.deb)
    * Microsoft Windows (7 or newer)
      * [installer (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0-win64.exe)
      * [installer (32 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0-win32.exe)
      * [archive (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0-win64.zip)
      * [archive (32 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0-win32.zip)
    * [OS X](https://github.com/grzegorzmazur/yacas/releases/download/v1.5.0/yacas-1.5.0-Darwin.dmg)
* yagy version 1.1.0
  * source code: [tgz](https://github.com/grzegorzmazur/yagy/archive/v1.1.0.tar.gz) and [zip](https://github.com/grzegorzmazur/yagy/archive/v1.1.0.zip)
  * binary packages for
    * Linux
      * [Debian 8.2 (64 bit)](https://github.com/grzegorzmazur/yagy/releases/download/v1.1.0/yagy_1.1.0-1jessie_amd64.deb)
      * [Fedora 23 (64 bit)](https://github.com/grzegorzmazur/yagy/releases/download/v1.1.0/yagy-1.1.0-1.fc23.x86_64.rpm)
      * Ubuntu
        * [15.10 (64 bit)](https://github.com/grzegorzmazur/yagy/releases/download/v1.1.0/yagy_1.1.0-1wily_amd64.deb)
        * [14.04 (64 bit)](https://github.com/grzegorzmazur/yagy/releases/download/v1.1.0/yagy_1.1.0-1trusty_amd64.deb)
    * Microsoft Windows (7 or newer)
      * [installer (64 bit)](https://github.com/grzegorzmazur/yagy/releases/download/v1.1.0/yagy-1.1.0-win64.exe)
      * [archive (64 bit)](https://github.com/grzegorzmazur/yagy/releases/download/v1.1.0/yagy-1.1.0-win64.zip)
    * [OS X](https://github.com/grzegorzmazur/yagy/releases/download/v1.1.0/yagy-1.1.0.dmg)
* yacas version 1.4.2
  * source code: [tgz](https://github.com/grzegorzmazur/yacas/archive/v1.4.2.tar.gz) and [zip](https://github.com/grzegorzmazur/yacas/archive/v1.4.2.zip)
  * binary packages for
    * [Java](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas-1.4.2.jar)
    * Linux
      * [Debian 8.2 (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas_1.4.2-1jessie_amd64.deb)
      * [Fedora 23 (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas-1.4.2-1.fc23.x86_64.rpm)
      * Ubuntu
        * [15.10 (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas_1.4.2-1wily_amd64.deb)
        * [14.04 (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas_1.4.2-1trusty_amd64.deb)
    * Microsoft Windows (7 or newer)
      * [installer (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas-1.4.2-win64.exe)
      * [installer (32 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas-1.4.2-win32.exe)
      * [archive (64 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas-1.4.2-win64.zip)
      * [archive (32 bit)](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas-1.4.2-win32.zip)
    * [OS X](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.2/yacas-1.4.2-Darwin.dmg)
* yacas version 1.4.0
  * source code: [tgz](https://github.com/grzegorzmazur/yacas/archive/v1.4.0.tar.gz) and [zip](https://github.com/grzegorzmazur/yacas/archive/v1.4.0.zip)
  * binary packages for
    * [Mac OS X](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas-1.4.0-Darwin.dmg)
    * [Ubuntu](https://github.com/grzegorzmazur/yacas/releases/download/v1.4.0/yacas_1.4.0-1_amd64.deb)
* yacas version 1.3.6
  * [source code](http://sourceforge.net/projects/yacas/files/yacas-source/1.3/yacas-1.3.6.tar.gz/download)
  * binary packages for
    * [Mac OS X](http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-Darwin.dmg/download)
    * Microsoft Windows 7 and newer, [64 bit](http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-win64.exe/download) and [32 bit](http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-win32.exe/download) variants
    * [Microsoft Windows XP](http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6-winxp.exe/download)
    * [Java](http://sourceforge.net/projects/yacas/files/yacas-binary/yacas-1.3.6.jar/download)
* yagy version 1.0.1
  * [source code](http://sourceforge.net/projects/yagy/files/yagy-1.0.1.tar.gz/download)
  * binary packages for
    * [Mac OS X](http://sourceforge.net/projects/yagy/files/yagy-1.0.1.dmg/download)
    * [Microsoft Windows 7 and newer, 64 bit](http://sourceforge.net/projects/yagy/files/yagy-1.0.1-win64.exe/download)
    * [Microsoft Windows XP](http://sourceforge.net/projects/yagy/files/yagy-1.0.1-win64.exe/download)
    * [Ubuntu 64 bit](https://sourceforge.net/projects/yagy/files/yagy_1.0.0-1_amd64.deb/download)
</div>
</div>
