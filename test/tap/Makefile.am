AM_CFLAGS = \
	-I$(abs_top_srcdir)/test \
	-I$(top_srcdir) \
	-I$(top_srcdir)/ccan

check_LIBRARIES = libtap.a
libtap_a_SOURCES = \
	basic.c \
	basic.h \
	hkl.h \
	hkl.c

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
