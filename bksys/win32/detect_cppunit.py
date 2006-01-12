# Frederic-Emmanuel PICCA, 2006
	
def detect(env):
	if env.has_key('CPPUNIT_LIBPATH') and env.has_key('CPPUNIT_CPPPATH'):
		env.AppendUnique(CPPUNIT_CXXFLAGS=['/GR'])
		env.AppendUnique(CXXFLAGS=['/GR'])
		env.AppendUnique(CPPUNIT_LIBS=['cppunit'])
		env.AppendUnique(LIBS=['cppunit'])
	else:
		env.pprint('RED', "for windows please provide the cppunitincludes and cppunitlibs")
