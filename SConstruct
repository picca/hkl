# -*- coding: iso-8859-1 -*-

dirs = Split("""
             src/SConscript
             test/SConscript
             """)

import os

platform = os.name

print platform

env = Environment(PLATFORM = platform)

if env['PLATFORM'] == 'posix':
  dirs.append('binding/SConscript')
  env = Environment(CXXFLAGS = '-g -pg -W -Wall -O2 -pipe -D_REENTRANT -D_GNU_SOURCE -pedantic', LINKFLAGS = '-pg')
elif env['PLATFORM'] == 'nt':
  env =  Environment(CCFLAGS = '/W3 /GX /GR /Gy /Zm500 /MD /Op')
                                                              
Export('env')

SConscript(dirs)
