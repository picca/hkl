bin_PROGRAMS = hkl3d

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	$(GSL_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(GTK_CFLAGS) \
	$(HDF5_CFLAGS)

AM_LDFLAGS = \
	-Wl,--export-dynamic \
	$(top_builddir)/hkl/libhkl.la \
	$(GSL_LIBS) \
	$(GLIB_LIBS) \
	$(GTK_LIBS) \
	$(HDF5_LIBS)

hkl3d_LDADD = \
	$(top_builddir)/hkl/libhkl.la

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
