#------------------------------------------------------------------------------
# HKL : library for diffractometer (eulerian, kappa, 6C, 4C ...)
#------------------------------------------------------------------------------
VERS = 1.1.0

#------------------------------------------------------------------------------
# DOXYGEN STUFFS
#------------------------------------------------------------------------------
DOXYGEN_ROOT =
DOXYGEN = $(DOXYGEN_ROOT)/bin/doxygen
DOXYGEN_CONFIG = $(ASL_ROOT)/doc/doxygen/asl-doxygen

#------------------------------------------------------------------------------
# COMPILER STUFFS
#------------------------------------------------------------------------------
CC = c++
AR_SL = $(CC) -fPIC -shared
CXX_FLAGS = -g -W -Wall -O3 -pipe -D_REENTRANT -D_GNU_SOURCE -DASL_BUILD
VERS_OPT = -Wl,-soname,
LIB_NAME = libHKL
SL_EXT = lib


#------------------------------------------------------------------------------
# SOURCE FILE LIST
#------------------------------------------------------------------------------
OBJS =  \
	constants.obj \
	cristal.obj \
	diffractometer.obj \
	eulerian_ diffractometer6C.obj \
	eulerian_angleconfiguration4C.obj \
	eulerian_angleConfiguration6C.obj \
	eulerian_bissectormode4C.obj \
	eulerian_mode6C.obj \
	HKLException.obj \
	kappa_angleconfiguration4C.obj \
	reflection.obj \
	smatrix.obj \
	source.obj \
	svector.obj

#------------------------------------------------------------------------------
# RULES FOR SHARED LIBRARY
#------------------------------------------------------------------------------
.SUFFIXES: .obj .cpp

all: $(LIB_NAME).$(SL_EXT)

$(LIB_NAME).$(SL_EXT): $(OBJS)
	$(AR_SL) -o $(LIB_NAME).$(SL_EXT).$(VERS) \
	$(VERS_OPT)$(LIB_NAME).$(SL_EXT).$(VERS) $(OBJS) \
	-lACE -lpci_dask2k -lpci_dask

cristal.cpp: \
        constants.h \
        cristal.h \
        svecmat.h

diffractometer.cpp: \
        angleconfig.h \
        constants.h \
        cristal.h \
        diffractometer.h \
        HKLException.h mode.h \
        reflection.h \
        source.h \
        svecmat.h

eulerian_ diffractometer6C.cpp: \
        angleconfig.h \
        constants.h \
        cristal.h \
        diffractometer.h \
        HKLException.h \
        mode.h \
        reflection.h \
        source.h \
        svecmat.h

eulerian_angleconfiguration4C.cpp: \
        angleconfig.h

eulerian_angleConfiguration6C.cpp: \
	angleconfig.h \
	constants.h

eulerian_bissectormode4C.cpp: \
	angleconfig.h \
	constants.h \
	HKLException.h \
	mode.h \
	svecmat.h

eulerian_mode6C.cpp: \
	angleconfig.h \
	constants.h \
	HKLException.h \
	mode.h \
	svecmat.h

HKLException.cpp: \
	HKLException.h

kappa_angleconfiguration4C.cpp: \
	angleconfig.h

reflection.cpp: \
	angleconfig.h \
	constants.h \
	reflection.h \
	svecmat.h

smatrix.cpp: \
	svecmat.h

source.cpp: \
	source.h

svector.cpp: \
	svecmat.h

.cpp.obj:
	$(CC) $(CXX_FLAGS) -c $< -o $@

#------------------------------------------------------------------------------
# RULE: doc
#------------------------------------------------------------------------------
doc:
	$(DOXYGEN) $(DOXYGEN_CONFIG)

#------------------------------------------------------------------------------
# RULE: clean
#------------------------------------------------------------------------------
clean:
	rm -f $(LIB_NAME).* *.obj




