# Frederic-Emmanuel PICCA, 2006

import os
import SCons.Util

CLVar = SCons.Util.CLVar
	
def detect(context, env):			
	context.Message('Checking for cppunit... ')	
	cppunit_config = env.find_program('cppunit-config', os.environ['PATH'].split(':'))
	if cppunit_config:
		env.ParseConfig(cppunit_config+' --cflags --libs');
		env['CXXFLAGS_CPPUNIT'] = SCons.Util.CLVar(
			os.popen(cppunit_config+' --cflags 2>/dev/null').read().strip() );
		env['LINKFLAGS_CPPUNIT'] = SCons.Util.CLVar(
			os.popen(cppunit_config+' --libs 2>/dev/null').read().strip() );
		context.Result(True)
	else:
		context.Result(False)
		env.Exit(1)
