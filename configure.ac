#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([hkl],[4.0.0],[picca@synchrotron-soleil.fr])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR(config)

AM_INIT_AUTOMAKE(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

# Checks for programs.
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
GTK_DOC_CHECK(1.9)

# gtk+
PKG_CHECK_MODULES([GTK], [gtk+-2.0 >= 2.16])

AC_CONFIG_FILES([Makefile
		 hkl/Makefile
		 test/Makefile
		 hkl.pc
		 Documentation/Makefile
		 Documentation/api/Makefile
		 gui/Makefile
])

AC_OUTPUT
