# Made from scons qt4.py
# Frederic-Emmanuel PICCA, 2006 <picca at synchrotron-soleil fr>

"""
Run scons -h to display the associated help, or look below ..
"""

import os, sys

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
  """Set up the cppunit environment and builders"""  
  if env['HELP']:
    p=env.pprint
    p('BOLD','*** cppunit options ***')
    p('BOLD','--------------------')
    p('BOLD','* cppunit_cpppath ','cppunit includes path (/usr/include/cppunit on debian, ..)')
    p('BOLD','* cppunit_libpath     ','cppunit libraries path, for linking the program')
    p('BOLD','* scons configure cppunit_cpppath=/usr/include/cppunit cppunit_libpath=/usr/lib\n')
    return
  
  # Detect the environment - replaces ./configure implicitely and store the options into a cache
  from SCons.Options import Options
  cachefile=os.path.join(env['_CACHEDIR_'],'cppunit.cache.py')
  opts = Options(cachefile)
  opts.AddOptions(
    ('CPPUNIT_CACHED', 'whether CPPUNIT  was found'),
    ('CPPUNIT_RUN', ''),  
    ('CPPUNIT_CPPPATH', 'path to the cppunit includes'),
    ('CPPUNIT_LIBPATH', 'path to the cppunit libraries'),
    ('CPPUNIT_LIBS', 'path to the cppunit libraries'),
    ('CPPUNIT_CXXFLAGS', 'additional compilation flags'),
    ('CPPUNIT_LINKFLAGS','link flag')
  )
  opts.Update(env)
  
  #if test explicitely 
  if 'test' in sys.argv:
    if env.has_key('CPPUNIT_RUN'):
      if not env['CPPUNIT_RUN']:
        env['_CONFIGURE_']=1
        
  # Reconfigure when things are missing
  if not env['HELP'] and (env['_CONFIGURE_'] or (not env.has_key('CPPUNIT_CACHED'))):
    # Erase all the options keys
    for opt in opts.options:
      if env.has_key(opt.key): env.__delitem__(opt.key)
    
    # Parse the command line
    if 'test' in sys.argv:
      env['CPPUNIT_RUN']=1
      env.pprint('CYAN', 'compiling with test-suite')
      
      if env['_ARGS_'].get('cppunit_cpppath',0):
        cppunit_cpppath = env['_ARGS_']['cppunit_cpppath'].split(os.pathsep)
        env['CPPUNIT_CPPPATH']=cppunit_cpppath
      if env['_ARGS_'].get('cppunit_libpath', 0):
        cppunit_libpath = env['_ARGS_']['cppunit_libpath'].split(os.pathsep)
        env['CPPUNIT_LIBPATH'] = cppunit_libpath
      # Load and run the platform specific configuration part
      from detect_cppunit import detect      
      detect(env)

      #copy the environment
      lenv = env.Copy()
      LIB = 'CPPUNIT'
      if lenv.has_key(LIB+'_CPPPATH'): lenv.AppendUnique(CPPPATH = lenv[LIB+'_CPPPATH'])
      if lenv.has_key(LIB+'_LIBPATH'): lenv.AppendUnique(LIBPATH = lenv[LIB+'_LIBPATH'])
      if lenv.has_key(LIB+'_LIBS'): lenv.AppendUnique(LIBS = lenv[LIB+'_LIBS'])
      if lenv.has_key(LIB+'_CCFLAGS'): lenv.AppendUnique(CCFLAGS = lenv[LIB+'_CCFLAGS'])
      if lenv.has_key(LIB+'_CXXFLAGS'): lenv.AppendUnique(CXXFLAGS = lenv[LIB+'_CXXFLAGS'])
      if lenv.has_key(LIB+'_LINKFLAGS'): lenv.AppendUnique(LINKFLAGS = lenv[LIB+'_LINKFLAGS'])
          
      def Check_cppunit(context):
        context.Message('Checking for cppunit ...')
        ret = context.TryLink(cppunit_test_file, '.cpp')
        context.Result(ret)
        return ret
          
      conf = lenv.Configure(custom_tests = { 'Check_cppunit' : Check_cppunit} )
      if not conf.Check_cppunit():
        lenv.pprint('RED', 'please install cppunit or set correctly the cppunit paths!')
        lenv.Exit(1)
      lenv = conf.Finish()
    else:
      env['CPPUNIT_RUN']=0
        
    env['CPPUNIT_CACHED'] = 1
    opts.Save(cachefile, env)
  
  # Attach the functions to the environment so that SConscripts can use them
  from SCons.Script.SConscript import SConsEnvironment
  from detect_cppunit import cppunitobj
  SConsEnvironment.cppunitobj=cppunitobj

  env.Export('env')

#define the cppunitobj
import generic
class cppunitobj(generic.genobj):
  def __init__(self, senv=None):
    if senv: generic.genobj.__init__(self, 'program', senv)
    else: generic.genobj.__init__(self, 'program', env)
    
  def execute(self):
    if self.orenv['CPPUNIT_RUN']:
      self.uselibs += ['CPPUNIT']
      generic.genobj.execute(self)
      self.env.AddPostAction(self.target, '@$TARGET')
