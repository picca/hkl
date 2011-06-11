#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.65])
AC_INIT([hkl],[5.0.0],[picca@synchrotron-soleil.fr])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR(config)

AM_INIT_AUTOMAKE([silent-rules tar-ustar])

# Checks for programs.
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_CC_C99
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
LT_INIT
AM_SILENT_RULES

# Checks for libraries.
AX_PATH_GSL

# Checks for header files.
AC_HEADER_STDBOOL
AC_HEADER_STDC
AC_HEADER_TIME
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([float.h limits.h malloc.h memory.h stddef.h stdint.h stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([floor getcwd gettimeofday memmove memset pow select sqrt strchr strdup strstr])

dnl ***************
dnl *** gtk-doc ***
dnl ***************

GTK_DOC_CHECK([1.9],[--flavour no-tmpl])

dnl ***********
dnl *** asy ***
dnl ***********

AC_PATH_PROG([ASY], [asy], [echo])

dnl *********************************************
dnl *** took from the bullet configure script ***
dnl *********************************************

AC_CANONICAL_HOST
case "$host" in
        *-*-mingw*|*-*-cygwin*)
                AC_DEFINE(PLATFORM_WIN32, 1, [Platform is Win32])
                opengl_LIBS="-lunsupported_platform"
                PLATFORM_STRING="Win32"
                ;;
        *-*-linux*)
                AC_DEFINE(PLATFORM_LINUX, 1, [Platform is Linux])
                opengl_LIBS="-lGL -lGLU"
                PLATFORM_STRING="Linux"
                ;;
        *-*-darwin*)
                AC_DEFINE(PLATFORM_APPLE, 1, [Platform is Apple])
                opengl_LIBS="-framework AGL -framework OpenGL -framework GLUT"
                PLATFORM_STRING="Apple"
                ;;
        *)
                AC_MSG_WARN([*** Please add $host to configure.ac checks!])
                ;;
esac
AC_SUBST(opengl_LIBS)

case "$host" in
        i?86-* | k?-* | athlon-* | pentium*-)
                AC_DEFINE(ARCH_X86, 1, [Architecture is x86])
                ARCH_SPECIFIC_CFLAGS=""
                ARCH_STRING="X86"
                ;;
        x86_64-*)
                AC_DEFINE(ARCH_X86_64, 1, [Architecture is x86-64])
                ARCH_SPECIFIC_CFLAGS="-DUSE_ADDR64"
                ARCH_STRING="X86-64"
                ;;
        ppc-* | powerpc-*)
                AC_DEFINE(ARCH_PPC, 1, [Architecture is PowerPC])
                ARCH_SPECIFIC_CFLAGS=""
                ARCH_STRING="PowerPC"
                ;;
        *)
                AC_MSG_ERROR([Unknown Architecture])
                ;;
esac
AC_C_BIGENDIAN

#----------------------------------------------------------------------------
# Package configuration switches.
#----------------------------------------------------------------------------
AC_ARG_ENABLE([multithreaded],
	[AS_HELP_STRING([--enable-multithreaded],[build BulletMultiThreaded (default NO)])],
	[disable_multithreaded=no], [disable_multithreaded=yes])
AC_MSG_CHECKING([BulletMultiThreaded])
AS_IF([test "$disable_multithreaded" = yes], [build_multithreaded=no], [build_multithreaded=yes])
AC_MSG_RESULT([$build_multithreaded])
AM_CONDITIONAL([CONDITIONAL_BUILD_MULTITHREADED], [test "$build_multithreaded" = yes])

AC_ARG_ENABLE([demos],
    [AS_HELP_STRING([--disable-demos],
	    [disable Bullet demos])],
    [],
    [enable_demos=yes])
AM_CONDITIONAL([CONDITIONAL_BUILD_DEMOS], [false])
if test "x$enable_demos" != xno; then
    AC_MSG_NOTICE([Building Bullet demos])
    AM_CONDITIONAL([CONDITIONAL_BUILD_DEMOS],[true])
fi



AC_ARG_ENABLE([debug],
    [AS_HELP_STRING([--enable-debug],[build with debugging information (default NO)])],
    [], [enable_debug=no])

AC_MSG_CHECKING([build mode])
AS_IF([test $enable_debug = yes], [build_mode=debug], [build_mode=optimize])
AC_MSG_RESULT([$build_mode])

CFLAGS="$ARCH_SPECIFIC_CFLAGS $CFLAGS"
CXXFLAGS="$ARCH_SPECIFIC_CFLAGS $CXXFLAGS $CFLAGS"

### end bullet configure script

dnl *******************************
dnl *** add an option for hkl3d ***
dnl *******************************

AC_ARG_ENABLE([hkl3d],
	AS_HELP_STRING([--disable-hkl3d],
		[do not compile the hkl3d library]),
	[],
	[enable_hkl3d=yes])
AM_CONDITIONAL([HKL3D], [test x$enable_hkl3d = xyes])
AM_COND_IF([HKL3D],
	   [PKG_CHECK_MODULES([G3D], [libg3d >= 0.0.8])
	   dnl check for yaml
	   AC_CHECK_LIB(yaml, yaml_get_version,
       	   		[AC_CHECK_HEADERS(yaml.h, , yaml_found=no)])
           if test x$yaml_found = xno; then
              AC_MSG_ERROR([Could not find yaml.h])
           fi
	   YAML_CFLAGS=""
	   AC_SUBST(YAML_CFLAGS)
	   YAML_LIBS="-lyaml"
	   AC_SUBST(YAML_LIBS)
])

dnl ****************************************
dnl *** add an option for the ghkl build ***
dnl ****************************************

AC_ARG_ENABLE([gui],
	AS_HELP_STRING([--disable-gui],
		[do not compile the gui interface]),
	[],
	[enable_gui=yes])
AM_CONDITIONAL([GUI], [test x$enable_gui = xyes])
AM_COND_IF([GUI],
	[PKG_CHECK_MODULES([GTKMM], [gtkmm-2.4 >= 2.18])
	AM_COND_IF([HKL3D],
		   [PKG_CHECK_MODULES([GLU], [glu >= 7.7.1])
	 	    PKG_CHECK_MODULES([GTKGLEXTMM], [gtkglextmm-1.2])
	])
])

dnl ************************************
dnl *** Enable lcov coverage reports ***
dnl ************************************

AC_ARG_ENABLE(gcov,
	AS_HELP_STRING([--enable-gcov],
		[Enable gcov]),
	[use_gcov=$enableval],
	[use_gcov=no])

if test "x$use_gcov" = "xyes"; then
   dnl we need gcc:
   if test "$GCC" != "yes"; then
      AC_MSG_ERROR([GCC is required for --enable-gcov])
   fi

  dnl Check if ccache is being used
  AC_CHECK_PROG(SHTOOL, shtool, shtool)
  case `$SHTOOL path $CC` in
    *ccache*[)] gcc_ccache=yes;;
    *[)] gcc_ccache=no;;
  esac

  if test "$gcc_ccache" = "yes" && (test -z "$CCACHE_DISABLE" || test "$CCACHE_DISABLE" != "1"); then
    AC_MSG_ERROR([ccache must be disabled when --enable-gcov option is used. You can disable ccache by setting environment variable CCACHE_DISABLE=1.])
  fi

  ltp_version_list="1.6 1.7 1.8 1.9"
  AC_CHECK_PROG(LTP, lcov, lcov)
  AC_CHECK_PROG(LTP_GENHTML, genhtml, genhtml)

  if test "$LTP"; then
    AC_CACHE_CHECK([for ltp version], glib_cv_ltp_version, [
      glib_cv_ltp_version=invalid
      ltp_version=`$LTP -v 2>/dev/null | $SED -e 's/^.* //'`
      for ltp_check_version in $ltp_version_list; do
        if test "$ltp_version" = "$ltp_check_version"; then
          glib_cv_ltp_version="$ltp_check_version (ok)"
        fi
      done
    ])
  else
    ltp_msg="To enable code coverage reporting you must have one of the following LTP versions installed: $ltp_version_list"
    AC_MSG_ERROR([$ltp_msg])
  fi

  case $glib_cv_ltp_version in
    ""|invalid[)]
      ltp_msg="You must have one of the following versions of LTP: $ltp_version_list (found: $ltp_version)."
      AC_MSG_ERROR([$ltp_msg])
      LTP="exit 0;"
      ;;
  esac

  if test -z "$LTP_GENHTML"; then
    AC_MSG_ERROR([Could not find genhtml from the LTP package])
  fi

  AC_DEFINE(HAVE_GCOV, 1, [Whether you have gcov])

  dnl Remove all optimization flags from CFLAGS
  changequote({,})
  CFLAGS=`echo "$CFLAGS" | $SED -e 's/-O[0-9]*//g'`
  CXXFLAGS=`echo "$CXXFLAGS" | $SED -e 's/-O[0-9]*//g'`
  changequote([,])

  dnl Add the special gcc flags
  CFLAGS="$CFLAGS -O0 -fprofile-arcs -ftest-coverage"
  CXXFLAGS="$CXXFLAGS -O0 -fprofile-arcs -ftest-coverage"
  LDFLAGS="$LDFLAGS -lgcov"
fi

AC_CONFIG_FILES([Makefile
		 hkl/Makefile
		 hkl.pc
		 hkl3d/Makefile
		 hkl3d/bullet/Makefile
		 hkl3d/bullet/src/Makefile
		 hkl3d.pc
		 test/Makefile
		 test/hkl/Makefile
		 test/hkl3d/Makefile
		 test/tap/Makefile
		 Documentation/Makefile
		 Documentation/api/Makefile
		 Documentation/figures/Makefile
		 Documentation/sphinx/Makefile
		 Documentation/sphinx/source/conf.py
		 gui/Makefile
		 gui/hkl3d/Makefile
		 data/Makefile
])

AC_OUTPUT
