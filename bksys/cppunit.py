# Made from scons qt4.py
# Frederic-Emmanuel PICCA, 2006 <picca at synchrotron-soleil fr>

"""
Run scons -h to display the associated help, or look below ..
"""

import os

def exists(env):
	return True

cppunit_test_file = """
#include <cppunit/ui/text/TestRunner.h>
int main(int argc, char **argv)
{
  //CppUnit::Test *suite = CppUnit::TestFactoryRegistry::getRegistry().makeTest();
  CppUnit::TextUi::TestRunner runner;
  
  //runner.addTest( suite );
  //runner.setOutputter( new CppUnit::CompilerOutputter( &runner.result(), std::cerr ) );
  
  bool wasSuccessful = runner.run();

  return wasSuccessful ? 0 : 1;
}
"""

def generate(env):
	import sys
	"""Set up the cppunit environment and builders"""	
	if env['HELP']:
		p=env.pprint
		p('BOLD','*** cppunit options ***')
		p('BOLD','--------------------')
		p('BOLD','* cppunitincludes ','cppunit includes path (/usr/include/cppunit on debian, ..)')
		p('BOLD','* cppunitlibs     ','cppunit libraries path, for linking the program')
		p('BOLD','* scons configure cppunitincludes=/usr/include/cppunit cppunitlibs=/usr/lib\n')
		return
			
	# Detect the environment - replaces ./configure implicitely and store the options into a cache
	from SCons.Options import Options
	cachefile=env['CACHEDIR']+'cppunit.cache.py'
	opts = Options(cachefile)
	opts.AddOptions(
		('CACHED_CPPUNIT', 'whether CPPUNIT  was found'),	
		('CPPUNIT_CPPPATH', 'path to the cppunit includes'),
		('CPPUNIT_LIBPATH', 'path to the cppunit libraries'),
		('CXXFLAGS_CPPUNIT', 'additional compilation flags'),
		('LINKFLAGS_CPPUNIT','')
		#('RPATH_CPPUNIT','')
	)
	opts.Update(env)
	
	# Look in the command line for the test target
	if 'test' in sys.argv:
		env.pprint('CYAN', "compilation and execution of test applications enabled")
		env['_RUNTESTS_'] = 1
	else:
		env['_RUNTESTS_'] = 0	
	
	# Reconfigure when things are missing
	if not env['HELP'] and (env['_CONFIGURE_'] or (not env.has_key('CACHED_CPPUNIT'))):
		env['_CONFIGURE_']=1

		# Erase all the options keys
		for opt in opts.options:
			if env.has_key(opt.key): env.__delitem__(opt.key)
		
		# Parse the command line
		if env['ARGS'].get('cppunitincludes',0):
			env['CPPUNIT_CPPPATH']=env['ARGS']['cppunitincludes']
			env.Append(CPPPATH = env['CPPUNIT_CPPPATH'])
		if env['ARGS'].get('cppunitlibs', 0):
			env['CPPUNIT_LIBPATH']=env['ARGS']['cppunitlibs']
			env.Append(LIBPATH = env['CPPUNIT_LIBPATH'])
		
		# Load and run the platform specific configuration part	
		import sys
		if sys.platform == 'darwin':
			sys.path.append('bksys'+os.sep+'osx')
			from detect_cppunit import detect
		elif env['WINDOWS']:
			sys.path.append('bksys'+os.sep+'win32')
			from detect_cppunit import detect
		else:
			sys.path.append('bksys'+os.sep+'unix')
			from detect_cppunit import detect			
	
		#detect the cppunit library flags
		detect(env)
		
		def Check_cppunit(context):
			context.Message('Checking for cppunit ...')
			ret = context.TryLink(cppunit_test_file, '.cpp')
			context.Result(ret)
			return ret
					
		conf = env.Configure(custom_tests = { 'Check_cppunit' : Check_cppunit} )
		if not conf.Check_cppunit():
			env.pprint('RED', 'please install cppunit or set correctly the cppunit paths!')
			env.Exit(1)
		else:
			env = conf.Finish()
			#save the configuration
			env['CACHED_CPPUNIT'] = 1
			opts.Save(cachefile, env)

	#define the cppunitobj
	import generic
	class cppunitobj(generic.genobj):
		def __init__(self, senv=None):
			if senv: generic.genobj.__init__(self, 'program', senv)
			else: generic.genobj.__init__(self, 'program', env)

		def execute(self):
			self.env=self.orenv.Copy()
			self.env.AppendUnique(LINKFLAGS=self.env['LINKFLAGS_CPPUNIT'])
			self.env.AppendUnique(CXXFLAGS=self.env['CXXFLAGS_CPPUNIT'])
			generic.genobj.execute(self)
			if self.env['_RUNTESTS_']:
				self.env.AddPostAction(self.target, '@$TARGET')

	# Attach the functions to the environment so that SConscripts can use them
	from SCons.Script.SConscript import SConsEnvironment
	SConsEnvironment.cppunitobj=cppunitobj

