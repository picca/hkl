# Frederic-Emmanuel PICCA, 2006
	
def detect(env):
	if env.has_key('CPPUNIT_LIBPATH') and env.has_key('CPPUNIT_CPPPATH'):
		env.Append(LIBS = 'cppunit')
		return True
	else:
		env.pprint('RED', "for windows please provide the cppunitincludes and cppunitlibs")
		return False
