AM_CFLAGS = \
	-I$(abs_top_srcdir)/tests \
	-I$(top_srcdir)

check_LIBRARIES = libtap.a
libtap_a_SOURCES = \
	basic.c \
	basic.h \
	float.h \
	float.c \
	macros.h \
	hkl.h \
	hkl.c

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
