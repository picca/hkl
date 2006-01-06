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
sys.path.append('bksys')

## Import the main configuration tool
from generic import configure

config = {
          'modules'  : 'generic cppunit',
          'builddir' : 'build', # put all object files under 'build/'
          'config.h' : 1, # mechanism should be ok
          'rpath'    : 1, # incomplete
          'bootstrap': 1, # incomplete
          #'colorful' : not os.environ.has_key('NOCOLORS'), # only with scons >= 0.96.91 - now miniscons
          'colorful' : 0
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
