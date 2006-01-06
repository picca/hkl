# Frederic-Emmanuel PICCA, 2006
	
def detect(env):
	if env.has_key('CPPUNIT_LIBPATH') and env.has_key('CPPUNIT_CPPPATH'):
		env['LINKFLAGS_CPPUNIT'] = "\LIBPATH:%s cppunit.lib" % env['CPPUNIT_LIBPATH']
		env['CXXFLAGS_CPPUNIT'] = "\I%s" % env['CPPUNIT_CPPPATH']
		return True
	else:
		env.pprint('RED', "for windows please provide the cppunitincludes and cppunitlibs")
		return False
