#! /usr/bin/env python

"""
help       -> scons -h
compile    -> scons
clean      -> scons -c
install    -> scons install
uninstall  -> scons -c install
configure  -> scons configure prefix=/tmp/ita debug=full extraincludes=/usr/local/include:/tmp/include prefix=/usr/local

Run from a subdirectory -> scons -u
The variables are saved automatically after the first run (look at cache/kde.cache.py, ..)
"""

###################################################################
# LOAD THE ENVIRONMENT AND SET UP THE TOOLS
###################################################################

## We assume that 'bksys' is our admin directory
import sys, os
#sys.path.append('bksys')

## Import the main configuration tool
from bksys import configure

config = {
          'modules'  : 'generic cppunit',
          'builddir' : 'build', # put all object files under 'build/'
          'colorful' : 0,
          'arguments' : ARGUMENTS
         }

# and the config.h

env=configure(config)

###################################################################
# SCRIPTS FOR BUILDING THE TARGETS
###################################################################

subdirs = Split("""
                src
                test
                """)

env.subdirs(subdirs)
env.dist('hkl', '2.1.0')

Export('env')
