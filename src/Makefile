#------------------------------------------------------------------------------
# ASL : the ADLink Support (or Shared) Library
#------------------------------------------------------------------------------
VERS = 1.1.0

#------------------------------------------------------------------------------
# ACE ROOT DIRECTORY (i.e. location of the ACE_wrappers directory)
#------------------------------------------------------------------------------
#ACE_ROOT = ../../ace/ACE_wrappers

#------------------------------------------------------------------------------
# ADLINK D2K-DASK ROOT DIRECTORY
#------------------------------------------------------------------------------
#AD_D2KDASK_ROOT = ../../adlink/d2k-dask-163

#------------------------------------------------------------------------------
# ADLINK PCI-DASK ROOT DIRECTORY
#------------------------------------------------------------------------------
#AD_PCIDASK_ROOT = ../../adlink/pci-dask-406

#------------------------------------------------------------------------------
# DOXYGEN ROOT DIR
#------------------------------------------------------------------------------
DOXYGEN_ROOT =

#------------------------------------------------------------------------------
# ASL ROOT DIRECTORY
#------------------------------------------------------------------------------
#ASL_ROOT = ..

#------------------------------------------------------------------------------
# ASL ROOT DIRECTORY
#------------------------------------------------------------------------------
#ASL_LIB_DIR = $(ASL_ROOT)/lib

#------------------------------------------------------------------------------
# INCLUDE
#------------------------------------------------------------------------------
#INC_DIRS = -I$(ACE_ROOT) \
#           -I$(AD_D2KDASK_ROOT)/include \
#	   -I$(AD_PCIDASK_ROOT)/include \
#           -I$(ASL_ROOT)/include \
#           -I.

#------------------------------------------------------------------------------
# LIBRARIES
#------------------------------------------------------------------------------
#LIB_DIRS = -L$(ACE_ROOT)/ace \
#           -L$(AD_D2KDASK_ROOT)/lib \
#    	   -L$(AD_PCIDASK_ROOT)/lib

#------------------------------------------------------------------------------
# LIBRARIES
#------------------------------------------------------------------------------
OBJS_DIR = ./objs

#------------------------------------------------------------------------------
# COMPILER STUFFS
#------------------------------------------------------------------------------
CC = c++
#----------------------------------------------
AR_SL = $(CC) -fPIC -shared
#----------------------------------------------
CXX_FLAGS = -g -W -Wall -O3 -pipe -D_REENTRANT -D_GNU_SOURCE -DASL_BUILD
#$(INC_DIRS)
#----------------------------------------------
VERS_OPT = -Wl,-soname,
#----------------------------------------------
SL_EXT = lib

#------------------------------------------------------------------------------
# DOXYGEN STUFFS
#------------------------------------------------------------------------------
DOXYGEN = $(DOXYGEN_ROOT)/bin/doxygen
#----------------------------------------------
DOXYGEN_CONFIG = $(ASL_ROOT)/doc/doxygen/asl-doxygen

#------------------------------------------------------------------------------
# SHARED LIBRAY NAME
#------------------------------------------------------------------------------
LIB_NAME = libHKL

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

.cpp.obj:
	@./cr_dir $(OBJS_DIR)
#	@./cr_dir $(OBJS_DIR)/adlink
	$(CC) $(CXX_FLAGS) -c $< -o $*.obj

#------------------------------------------------------------------------------
# RULE: all
#------------------------------------------------------------------------------
all: $(LIB_NAME).$(SL_EXT)

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


#------------------------------------------------------------------------------
# RULE: $(LIB_NAME).$(SL_EXT)
#------------------------------------------------------------------------------
$(LIB_NAME).$(SL_EXT): $(OBJS)
#	@./cr_dir $(ASL_LIB_DIR)
	$(AR_SL) -o $(LIB_NAME).$(SL_EXT).$(VERS) \
	$(VERS_OPT)$(LIB_NAME).$(SL_EXT).$(VERS) $(OBJS) \
	-lACE -lpci_dask2k -lpci_dask

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




