AM_CFLAGS=-I$(top_srcdir)

noinst_LTLIBRARIES=libccan.la
libccan_la_SOURCES = \
	str/debug.c \
	str/str.c \
	str/str.h \
	str/str_debug.h \
	build_assert/build_assert.h \
	darray/darray.h \
	ptr_valid/ptr_valid.c \
	ptr_valid/ptr_valid.h \
	noerr/noerr.h \
	noerr/noerr.c \
	autodata/autodata.c \
	autodata/autodata.h \
	check_type/check_type.h \
	compiler/compiler.h \
	array_size/array_size.h \
	list/list.h \
	list/list.c \
	container_of/container_of.h

ccanincludedir=$(includedir)/hkl-@VMAJ@/hkl/ccan
nobase_ccaninclude_HEADERS= \
	darray/darray.h
