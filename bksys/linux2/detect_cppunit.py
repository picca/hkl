# Frederic-Emmanuel PICCA, 2006

import os
import SCons.Util

CLVar = SCons.Util.CLVar
  
def detect(env):
  if env.has_key('CPPUNIT_LIBPATH') and env.has_key('CPPUNIT_CPPPATH'):
    env['CPPUNIT_LIBS'] = ['cppunit', 'dl']
  
  cppunit_config = env.WhereIs('cppunit-config')
  if cppunit_config:
    cxxflag = SCons.Util.CLVar(
      os.popen(cppunit_config+' --cflags 2>/dev/null').read().strip() )
    linkflag = SCons.Util.CLVar(
      os.popen(cppunit_config+' --libs 2>/dev/null').read().strip() )
    if cxxflag:
      env['CPPUNIT_CXXFLAGS'] = cxxflag
    if linkflag:
      env['CPPUNIT_LINKFLAGS'] = linkflag

import cppunit
class cppunitobj(cppunit.cppunitobj):
  def __init__(self, env):
    cppunit.cppunitobj.__init__(self, env)
