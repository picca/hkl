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

## Import the main configuration tool
from bksys import configure

config = {
		  'name' : 'hkl',
		  'version' : '2.1.0',
          'modules'  : 'generic cppunit',
          'colorful' : 0,
          'arguments' : ARGUMENTS
         }

env=configure(config)

###################################################################
# SCRIPTS FOR BUILDING THE TARGETS
###################################################################

subdirs = Split("""
                src
                test
                """)

env.subdirs(subdirs)

Export('env')
