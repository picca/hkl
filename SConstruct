# -*- coding: iso-8859-1 -*-

dirs = Split("""
             src/SConscript
             test/SConscript
             """)

import os

platform = os.name

env = Environment(PLATFORM = platform)

#On récupère les options de compilation
# scons debug=[0,1] et/ou profile=[0/1]
debug = int(ARGUMENTS.get('debug', 0))
profile = int(ARGUMENTS.get('profile', 0))

if profile:
  debug = 1
  print "Debug and Profile version"
elif debug:
  print "Debug Version"
else:
  print "Production version"

if platform == 'posix':
  env.Append(CCFLAGS = ['-W', '-Wall', '-O2', '-pipe', '-D_REENTRANT', '-D_GNU_SOURCE', '-pedantic'])
  env.Append(LDFLAGS = ['-Wl', '-O1'])
  if debug:
    env.Append(CCFLAGS = ['-g','-DDEBUG'])
  if profile:
    env.Append(CCFLAGS = ['-pg'])
    env.Append(LINKFLAGS = ['-pg'])
  dirs.append('binding/SConscript')
elif platform in ['nt', 'win32']:
  env.Append(CCFLAGS = ['/Ox', '/W3','/GX', '/GR', '/Gy', '/Zm500', '/MD', '/Op'])
  if debug:
    env.Append(CCFLAGS = ['/Z7'])
                                                              
Export('env')

SConscript(dirs)
