#------------------------------------------------------------------------------
# HKL : library for diffractometer (eulerian, kappa, 6C, 4C ...)
#------------------------------------------------------------------------------
VERS = 1.1.0

#------------------------------------------------------------------------------
# DOXYGEN STUFFS
#------------------------------------------------------------------------------
DOXYGEN_ROOT = /home/delos/doxygen-1.4.1
DOXYGEN = $(DOXYGEN_ROOT)/bin/doxygen
#DOXYGEN_CONFIG = $(ASL_ROOT)/doc/doxygen/asl-doxygen
DOXYGEN_CONFIG = $(DOXYGEN_ROOT)/Doxyfile

#------------------------------------------------------------------------------
# COMPILER STUFFS
#------------------------------------------------------------------------------
CC = c++
AR = ar rv
AR_SL = $(CC) -fPIC -shared
#CXX_FLAGS = -g -W -Wall -O3 -pipe -D_REENTRANT -D_GNU_SOURCE
CXX_FLAGS = -g -W -Wall -O0 -pipe -D_REENTRANT -D_GNU_SOURCE
VERS_OPT = -Wl,-soname,
LIB_NAME = libHKL
EXE = libuser
SL_EXT = so
ST_EXT = lib


#------------------------------------------------------------------------------
# SOURCE FILE LIST
#------------------------------------------------------------------------------
OBJS =  \
	constants.obj \
	cristal.obj \
	diffractometer.obj \
	eulerian_diffractometer6C.obj \
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

all: $(LIB_NAME).$(ST_EXT) $(EXE)

$(EXE): $(LIB_NAME).$(ST_EXT) LibUser.obj
	$(CC) -o $(EXE) LibUser.obj $(LIB_NAME).$(ST_EXT)

$(LIB_NAME).$(ST_EXT): $(OBJS)
	$(AR) $(LIB_NAME).$(ST_EXT) $(OBJS)
#	$(AR) -o $(LIB_NAME).$(SL_EXT).$(VERS) \
#	$(VERS_OPT)$(LIB_NAME).$(SL_EXT).$(VERS) $(OBJS) \


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

eulerian_diffractometer6C.cpp: \
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

LibUser.cpp: \
	source.h \
	cristal.h \
	svecmat.h \
	constants.h \
	reflection.h \
	angleconfig.h \
	HKLException.h \
	diffractometer.h


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
	rm -f $(LIB_NAME).* *.obj *~ core
