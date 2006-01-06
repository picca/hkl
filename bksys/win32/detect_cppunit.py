# Frederic-Emmanuel PICCA, 2006

import os
import SCons.Util

CLVar = SCons.Util.CLVar
	
def detect(context):			
	context.Message('Checking for cppunit ...')
	oldenv = context.env.Copy()
	if context.env.has_key('CPPUNIT_CPPPATH'):
		cpppath = context.env['CPPUNIT_CPPPATH']
		context.env.Append(CPPPATH = cpppath)
	if context.env.has_key('CPPUNIT_LIBPATH'):
		libpath = context.env['CPPUNIT_LIBPATH']
		context.env.Append(LIBPATH = libpath)
	context.env.Append(LIBS = 'cppunit')
	if not ret:
		context.env = oldenv.Copy()
	context.Result( ret )
    	return ret
