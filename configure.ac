#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([hkl],[4.0.4-rc3],[picca@synchrotron-soleil.fr])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR(config)

AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL
AC_PROG_RANLIB

# Checks for libraries.
AX_PATH_GSL

# Checks for header files.
AC_HEADER_STDBOOL
AC_HEADER_STDC
AC_HEADER_TIME
AC_CHECK_HEADERS([float.h stdlib.h string.h sys/time.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([floor gettimeofday memmove memset sqrt strdup])

# gtk-doc
GTK_DOC_CHECK([1.9],[--flavour no-tmpl])

dnl ***********
dnl *** asy ***
dnl ***********

AC_PATH_PROG([ASY], [asy], [echo])

# add an option for the ghkl build
AC_ARG_ENABLE([ghkl],
	AS_HELP_STRING([--enable-ghkl],
			[turn on the ghkl gui [[default=yes]]]),
	[case "${enableval}" in
	      yes) ghkl=true ;;
      	      no)  ghkl=false ;;
      	      *) AC_MSG_ERROR([bad value ${enableval} for --enable-ghkl]) ;;
	esac],[ghkl=true])
AM_CONDITIONAL([GHKL], [test x$ghkl = xtrue])

# gtkmm + glademm
AM_COND_IF([GHKL],
	[PKG_CHECK_MODULES([GTKMM], [gtkmm-2.4 >= 2.18])])

AC_CONFIG_FILES([Makefile
		 hkl/Makefile
		 test/Makefile
		 hkl.pc
		 Documentation/Makefile
		 Documentation/api/Makefile
		 Documentation/figures/Makefile
		 Documentation/sphinx/Makefile
		 Documentation/sphinx/source/conf.py
		 gui/Makefile
])

AC_OUTPUT
