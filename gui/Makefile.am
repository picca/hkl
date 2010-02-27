bin_PROGRAMS = ghkl

AM_CXXFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	$(GTKMM_CFLAGS)

AM_LDFLAGS = \
	$(top_builddir)/hkl/libhkl.la \
	$(GTKMM_LIBS)

ghkl_SOURCES = \
	axespinbutton.cpp \
	hklwindow.cpp \
	pseudoaxespinbutton.cpp \
	main.cpp

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax