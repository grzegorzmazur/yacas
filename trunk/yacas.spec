# yacas.spec.  Generated from yacas.spec.in by configure.

%define prefix /usr

Name: yacas
Version: 1.0.62rev3
Release: 1
Group: Applications/Math
Copyright: GPL
URL: http://www.xs4all.nl/~apinkus/yacas.html
Summary: Yet Another Computer Algebra System

Source: http://www.xs4all.nl/~apinkus/yacas-1.0.62rev3.tar.gz
BuildRoot: /tmp/yacas-1.0.62rev3-root

%description
Yacas (Yet Another Computer Algebra System) is a small and highly flexible
computer algebra language. The syntax uses a infix-operator grammar
parser. The distribution contains a small library of mathematical
functions, but its real strength is in the language in which you can
easily write your own symbolic manipulation algorithms. It supports
arbitrary precision arithmetic.

%prep

%setup

%build
CXXFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%{prefix}
make

%install
rm -rf $RPM_BUILD_ROOT
make install-strip prefix=$RPM_BUILD_ROOT%{prefix}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc docs/*.html docs/*.gif
%{prefix}/bin/yacas
%{prefix}/share/yacas/
