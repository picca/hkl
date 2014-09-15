AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(abs_top_srcdir)/tests \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

check_LIBRARIES = libtap.a
libtap_a_SOURCES = \
	basic.c \
	basic.h \
	float.h \
	float.c \
	macros.h \
	hkl-tap.h \
	hkl-tap.c

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
