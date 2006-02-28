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

import sys
print 'building on %s platform' % sys.platform

## Import the main configuration tool
from bksys import configure

# get the modules and targets depending on the platform
modules = ['generic']
subdirs = ['src']
builddir = 'build'
if sys.platform == 'linux2':
  modules += ['cppunit', 'boost_python']
  subdirs += ['test', 'binding/python', 'doc/example']
  builddir += '-linux'
elif sys.platform == 'win32':
  builddir += '-win32'

#configure the environment
config = {
          'pkgname' : 'hkl',
          'pkgversion' : '2.1.0',
          'builddir' : builddir,
          'modules'  : modules,
          'colorful' : 0,
          'arguments' : ARGUMENTS
         }

env=configure(config)

###################################################################
# SCRIPTS FOR BUILDING THE TARGETS
###################################################################

env.subdirs(subdirs)

Export('env')
