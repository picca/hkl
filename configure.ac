#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([hkl],[5.0.0],[picca@synchrotron-soleil.fr])
AC_CONFIG_SRCDIR([config.h.in])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR(config)

AM_INIT_AUTOMAKE([tar-ustar])

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
GTK_DOC_CHECK(1.9)

### took from the bullet configure script

AC_CANONICAL_HOST
case "$host" in
        *-*-mingw*|*-*-cygwin*)
                AC_DEFINE(PLATFORM_WIN32, 1, [Platform is Win32])
                opengl_LIBS="-lunsupported_platform"
                PLATFORM_STRING="Win32"
                ;;
        *-*-linux*)
                AC_DEFINE(PLATFORM_LINUX, 1, [Platform is Linux])
                opengl_LIBS="-lGL -lGLU -lglut"
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
	[AC_HELP_STRING([--enable-multithreaded],
		[build BulletMultiThreaded (default NO)])],
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
    [AC_HELP_STRING([--enable-debug],
	[build with debugging information (default NO)])],
    [], [enable_debug=no])

AC_MSG_CHECKING([build mode])
AS_IF([test $enable_debug = yes], [build_mode=debug], [build_mode=optimize])
AC_MSG_RESULT([$build_mode])

CFLAGS="$ARCH_SPECIFIC_CFLAGS $CFLAGS"
CXXFLAGS="$ARCH_SPECIFIC_CFLAGS $CXXFLAGS $CFLAGS"

### end bullet configure script

# add an option for hkl3d
AC_ARG_ENABLE([hkl3d],
	AS_HELP_STRING([--enable-hkl3d],
			[turn on the hkl3d library [[default=yes]]]),
	[case "${enableval}" in
	      yes) hkl3d=true ;;
      	      no)  hkl3d=false ;;
      	      *) AC_MSG_ERROR([bad value ${enableval} for --enable-hkl3d]) ;;
	esac],[hkl3d=true])
AM_CONDITIONAL([HKL3D], [test x$hkl3d = xtrue])
AM_COND_IF([HKL3D],
	   [PKG_CHECK_MODULES([G3D], [libg3d >= 0.0.8])
	   # yaml check hack
       	   AC_CHECK_HEADERS(yaml.h, , yaml_found=no)
           if test x$yaml_found = xno; then
              AC_MSG_ERROR([Could not find yaml.h])
           fi
	   YAML_CFLAGS=""
	   AC_SUBST(YAML_CFLAGS)
	   YAML_LIBS="-lyaml"
	   AC_SUBST(YAML_LIBS)
])

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
AM_COND_IF([GHKL],
	[PKG_CHECK_MODULES([GTKMM], [gtkmm-2.4 >= 2.18])
	AM_COND_IF([HKL3D],
		   [PKG_CHECK_MODULES([GLU], [glu >= 7.7.1])
	 	    PKG_CHECK_MODULES([GTKGLEXTMM], [gtkglextmm-1.2])
	])
])

AC_CONFIG_FILES([Makefile
		 hkl/Makefile
		 hkl.pc
		 hkl3d/Makefile
		 hkl3d/bullet/Makefile
		 hkl3d/bullet/src/Makefile
		 hkl3d/ConvexDecomposition/Makefile
		 hkl3d/GIMPACTUtils/Makefile
		 hkl3d/Serialize/Makefile
		 hkl3d/Serialize/BulletFileLoader/Makefile
		 hkl3d/Serialize/BulletWorldImporter/Makefile
		 hkl3d.pc
		 test/Makefile
		 test/hkl/Makefile
		 test/hkl3d/Makefile
		 Documentation/Makefile
		 Documentation/api/Makefile
		 gui/Makefile
		 gui/DiffabsDemo/Makefile
		 gui/hkl3d/Makefile
		 gui/OpenGL/Makefile
		 data/Makefile
])

AC_OUTPUT
