# Frederic-Emmanuel PICCA, 2006
	
def detect(env):
  print "je suis passe par la"
  if env.has_key('CPPUNIT_LIBPATH') and env.has_key('CPPUNIT_CPPPATH'):
    env.AppendUnique(CPPUNIT_CXXFLAGS=['/GR'])
    env.AppendUnique(CPPUNIT_LIBS=['cppunit'])
  else:
    env.pprint('RED', "for windows please provide the cppunitincludes and cppunitlibs")

import cppunit
class cppunitobj(cppunit.cppunitobj):
  def __init__(self, env):
    cppunit.cppunitobj.__init__(self, env)
