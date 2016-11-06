# -*- rpm-spec -*-
#
# This file and all modifications and additions to the pristine
# package are under the same license as the package itself.
#
# norootforbuild
#

Name:           yacas
Version:        1.6.1
Release:        1%{?dist}
Summary:        Easy to use, general purpose computer algebra system
%if 0%{?suse_version}
Group:          Productivity/Scientific/Math
%else
Group:          Applications/Engineering
%endif
%if 0%{?suse_version}
License:        LGPL-2.0+
%else
License:        LGPLv2+
%endif
URL:            http://www.yacas.org
Source:         v%{version}.tar.gz
BuildRequires:  cmake gcc-c++ python-sphinx python-sphinx_rtd_theme qt5-qtwebkit-devel qt5-qtsvg-devel
%description
Yacas is an easy to use, general purpose Computer Algebra System, a
program for symbolic manipulation of mathematical expressions. It uses
its own programming language designed for symbolic as well as
arbitrary-precision numerical computations. The system has a library
of scripts that implement many of the symbolic algebra operations; new
algorithms can be easily added to the library. Yacas comes with
extensive documentation covering the scripting language, the
functionality that is already implemented in the system and the
algorithms used.
%prep
%setup -q
%build
cmake -DCMAKE_BUILD_TYPE=Release -DENABLE_DOCS=ON -DENABLE_CYACAS_KERNEL=OFF -DCMAKE_INSTALL_PREFIX=%{_prefix} .
%{__make} %{?_smp_mflags}
%check
%{__make} test
%install
%{__make} DESTDIR=%{buildroot} install
%clean
rm -rf %{buildroot}

%package console
Summary:        Yacas text console
%if 0%{?suse_version}
Group:          Productivity/Scientific/Math
%else
Group:          Applications/Engineering
%endif
Requires: yacas-common = %{version}, yacas-doc = %{version}
%description console
Yacas is an easy to use, general purpose Computer Algebra System, a
program for symbolic manipulation of mathematical expressions. It
uses its own programming language designed for symbolic as well as
arbitrary-precision numerical computations. The system has a library
of scripts that implement many of the symbolic algebra operations;
new algorithms can be easily added to the library. Yacas comes with
extensive documentation covering the scripting language, the
functionality that is already implemented in the system and the
algorithms used.
%files console
%defattr(-,root,root,-)
"/usr/bin/yacas"

%package common
Summary:        Yacas common files
%if 0%{?suse_version}
Group:          Productivity/Scientific/Math
%else
Group:          Applications/Engineering
%endif
%description common
Common files for yacas and yacas GUI. Yacas is an easy to use, general
purpose Computer Algebra System, a program for symbolic manipulation 
of mathematical expressions. It uses its own programming language
designed for symbolic as well as arbitrary-precision numerical
computations. The system has a library of scripts that implement
many of the symbolic algebra operations; new algorithms can be easily
added to the library. Yacas comes with extensive documentation 
covering the scripting language, the functionality that is already
implemented in the system and the algorithms used.
%files common
%dir "/usr/share/yacas"
%dir "/usr/share/yacas/scripts"
"/usr/share/yacas/scripts/*"
%dir "/usr/share/yacas/tests"
"/usr/share/yacas/tests/*"

%package gui
Summary:        Yacas GUI
%if 0%{?suse_version}
Group:          Productivity/Scientific/Math
%else
Group:          Applications/Engineering
%endif
Requires: yacas-common = %{version}, yacas-doc = %{version}
%description gui
Graphical User Interface for yacas. Yacas is an easy to use, general
purpose Computer Algebra System, a program for symbolic manipulation
of mathematical expressions. It uses its own programming language
designed for symbolic as well as arbitrary-precision numerical
computations. The system has a library of scripts that implement many
of the symbolic algebra operations; new algorithms can be easily
added to the library. Yacas comes with extensive documentation
covering the scripting language, the functionality that is already
implemented in the system and the algorithms used.
%files gui
%defattr(-,root,root,-)
"/usr/bin/yacas-gui"
%dir "/usr/share/yacas/resources"
"/usr/share/yacas/resources/*"
%dir "/usr/yacas/icons"
"/usr/share/icons/*"
%dir "/usr/share/pixmaps"
"/usr/share/pixmaps/*"
"/usr/share/applications/yacas-gui.desktop"

%package doc
Summary:        Yacas documentation
%if 0%{?suse_version}
Group:          Documentation/HTML
%else
Group:          Documentation
%endif
%description doc
Yacas documentation. Yacas is an easy to use, general purpose Computer
Algebra System, a program for symbolic manipulation of mathematical
expressions. It uses its own programming language designed for
symbolic as well as arbitrary-precision numerical computations. The
system has a library of scripts that implement many of the symbolic
algebra operations; new algorithms can be easily added to the
library. Yacas comes with extensive documentation covering the
scripting language, the functionality that is already implemented in
the system and the algorithms used.
%files doc
%defattr(-,root,root,-)
"/usr/share/man/man1/yacas.1.gz"
%dir "/usr/share/yacas/documentation"
%dir "/usr/share/yacas/documentation/singlehtml"
%docdir "/usr/share/yacas/documentation/singlehtml"
"/usr/share/yacas/documentation/singlehtml/*"

%package devel
Summary:        Yacas development files
%if 0%{?suse_version}
Group:          Development/Libraries/C and C++
%else
Group:          Development/Libraries
%endif
Requires: yacas-common = %{version}
%description devel
Header files and libraries necessary for yacas development.  Yacas is
an easy to use, general purpose Computer Algebra System, a program for
symbolic manipulation of mathematical expressions. It uses its own
programming language designed for symbolic as well as
arbitrary-precision numerical computations. The system has a library
of scripts that implement many of the symbolic algebra operations; new
algorithms can be easily added to the library. Yacas comes with
extensive documentation covering the scripting language, the
functionality that is already implemented in the system and the
algorithms used.
%files devel
%defattr(-,root,root,-)
"/usr/lib/libyacas.a"
%dir "/usr/include/yacas"
"/usr/include/yacas/*"

%changelog
* Sun Nov 06 2016 Grzegorz Mazur <teoretyk@gmail.com> 1.6.1-1
- Changes are none, there is only the now (Ayal Pinkus)
