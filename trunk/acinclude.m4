# @synopsis AC_FUNC_ACCEPT_ARGTYPES
#
# Checks the data types of the three arguments to accept(). Results are
# placed into the symbols ACCEPT_TYPE_ARG[123], consistent with the
# following example:
#
#	#define ACCEPT_TYPE_ARG1 int
#	#define ACCEPT_TYPE_ARG2 struct sockaddr *
#	#define ACCEPT_TYPE_ARG3 socklen_t *
#
# This macro requires AC_CHECK_HEADERS to have already verified the
# presence or absence of sys/types.h and sys/socket.h.
#
# @version $Id: acinclude.m4,v 1.1.1.1 2000-11-09 19:22:51 livshits Exp $
#
# @author Daniel Richard G. <skunk@mit.edu>
#
# NOTE: This is just a modified version of the AC_FUNC_SELECT_ARGTYPES
# macro. Credit for that one goes to David MacKenzie et. al.

AC_DEFUN(AC_FUNC_ACCEPT_ARGTYPES,
[AC_MSG_CHECKING([types of arguments for accept()])
 AC_CACHE_VAL(ac_cv_func_accept_arg1,dnl
 [AC_CACHE_VAL(ac_cv_func_accept_arg2,dnl
  [AC_CACHE_VAL(ac_cv_func_accept_arg3,dnl
   [for ac_cv_func_accept_arg1 in 'int' 'unsigned int'; do
     for ac_cv_func_accept_arg2 in 'struct sockaddr *' 'void *'; do
      for ac_cv_func_accept_arg3 in 'socklen_t *' 'size_t *' 'unsigned int *' 'int *'; do
       AC_TRY_COMPILE(dnl
[#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
extern accept ($ac_cv_func_accept_arg1, $ac_cv_func_accept_arg2, $ac_cv_func_accept_arg3);],,dnl
        [ac_not_found=no ; break 3], ac_not_found=yes)
      done
     done
    done
   ])dnl AC_CACHE_VAL
  ])dnl AC_CACHE_VAL
 ])dnl AC_CACHE_VAL
 if test "$ac_not_found" = yes; then
  ac_cv_func_accept_arg1=int 
  ac_cv_func_accept_arg2='struct sockaddr *' 
  ac_cv_func_accept_arg3='socklen_t *'
 fi
 AC_MSG_RESULT([$ac_cv_func_accept_arg1, $ac_cv_func_accept_arg2, $ac_cv_func_accept_arg3])
 AC_DEFINE_UNQUOTED(ACCEPT_TYPE_ARG1,$ac_cv_func_accept_arg1)
 AC_DEFINE_UNQUOTED(ACCEPT_TYPE_ARG2,$ac_cv_func_accept_arg2)
 AC_DEFINE_UNQUOTED(ACCEPT_TYPE_ARG3,$ac_cv_func_accept_arg3)
])


dnl @synopsis TYPE_SOCKLEN_T
dnl
dnl Check whether sys/socket.h defines type socklen_t. Please note
dnl that some systems require sys/types.h to be included before
dnl sys/socket.h can be compiled.
dnl
dnl @version $Id: acinclude.m4,v 1.1.1.1 2000-11-09 19:22:51 livshits Exp $
dnl @author Lars Brinkhoff <lars@nocrew.org>
dnl
AC_DEFUN(TYPE_SOCKLEN_T,
[AC_CACHE_CHECK([for socklen_t], ac_cv_type_socklen_t,
[
  AC_TRY_COMPILE(
  [#include <sys/types.h>
   #include <sys/socket.h>],
  [socklen_t len = 42; return 0;],
  ac_cv_type_socklen_t=yes,
  ac_cv_type_socklen_t=no)
])
  if test $ac_cv_type_socklen_t != yes; then
    AC_DEFINE(socklen_t, int)
  fi
])
