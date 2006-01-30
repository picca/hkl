# Made from scons qt4.py
# Frederic-Emmanuel PICCA, 2006 <picca at synchrotron-soleil fr>

"""
Run scons -h to display the associated help, or look below ..
"""

import os, sys

def exists(env):
  return True

boost_python_test_file = """
#include <boost/python.hpp>
int main(int argc, char **argv)
{
  return 1;
}
"""

def generate(env):
  """Set up the cppunit environment and builders"""  
  if env['HELP']:
    p=env.pprint
    p('BOLD','*** boost_python options ***')
    p('BOLD','--------------------')
    p('BOLD','* boost_python_cpppath ','boost_python includes path (/usr/include/boost on debian, ..)')
    p('BOLD','* boost_python_libpath     ','boost_python libraries path, for linking the program')
    p('BOLD','* scons configure boost_python_includes=/usr/include/cppunit boost_python_libpath=/usr/lib\n')
    return
  
  # Detect the environment - replaces ./configure implicitely and store the options into a cache
  from SCons.Options import Options
  cachefile=os.path.join(env['_CACHEDIR_'],'boost_python.cache.py')
  opts = Options(cachefile)
  opts.AddOptions(
    ('BOOST_PYTHON_CACHED', 'whether CPPUNIT  was found'),
    ('BOOST_PYTHON_CPPPATH', 'path to the boost_python includes'),
    ('BOOST_PYTHON_LIBPATH', 'path to the boost_python libraries'),
    ('BOOST_PYTHON_LIBS', 'path to the boost_python libraries'),
    ('BOOST_PYTHON_CXXFLAGS', 'additional compilation flags'),
    ('BOOST_PYTHON_LINKFLAGS','link flag')
  )
  opts.Update(env)
        
  # Reconfigure when things are missing
  if not env['HELP'] and (env['_CONFIGURE_'] or (not env.has_key('BOOST_PYTHON_CACHED'))):
    # look for needed Tools
    env.Tool('python')
    
    # Erase all the options keys
    for opt in opts.options:
      if env.has_key(opt.key): env.__delitem__(opt.key)
    
    # Parse the command line      
    if env['_ARGS_'].get('boost_python_cpppath',0):
      env['BOOST_PYTHON_CPPPATH']=env['_ARGS_']['boost_python_cpppath']
    if env['_ARGS_'].get('boost_python_libpath', 0):
      env['BOOST_PYTHON_LIBPATH']=env['_ARGS_']['boost_python_libpath']
  
    # Load and run the platform specific configuration part
    from detect_boost_python import detect      
    detect(env)
  
    #copy the environment
    lenv = env.Copy()
    LIBS = ['PYTHON', 'BOOST_PYTHON']
    for LIB in LIBS:
      if lenv.has_key(LIB+'_CPPPATH'): lenv.AppendUnique(CPPPATH = lenv[LIB+'_CPPPATH'])
      if lenv.has_key(LIB+'_LIBPATH'): lenv.AppendUnique(LIBPATH = lenv[LIB+'_LIBPATH'])
      if lenv.has_key(LIB+'_LIBS'): lenv.AppendUnique(LIBS = lenv[LIB+'_LIBS'])
      if lenv.has_key(LIB+'_CCFLAGS'): lenv.AppendUnique(CCFLAGS = lenv[LIB+'_CCFLAGS'])
      if lenv.has_key(LIB+'_CXXFLAGS'): lenv.AppendUnique(CXXFLAGS = lenv[LIB+'_CXXFLAGS'])
      if lenv.has_key(LIB+'_LINKFLAGS'): lenv.AppendUnique(LINKFLAGS = lenv[LIB+'_LINKFLAGS'])
        
    def Check_boost_python(context):
      context.Message('Checking for boost_python ...')
      ret = context.TryLink(boost_python_test_file, '.cpp')
      context.Result(ret)
      return ret
        
    conf = lenv.Configure(custom_tests = { 'Check_boost_python' : Check_boost_python} )
    if not conf.Check_boost_python():
      lenv.pprint('RED', 'please install boost_python or set correctly the boost_python paths!')
      lenv.Exit(1)
    lenv = conf.Finish()
        
    env['BOOST_PYTHON_CACHED'] = 1
    opts.Save(cachefile, env)
  
  # Attach the functions to the environment so that SConscripts can use them
  from SCons.Script.SConscript import SConsEnvironment
  from detect_boost_python import boost_pythonobj
  SConsEnvironment.boost_pythonobj=boost_pythonobj

  env.Export('env')

#define the cppunitobj
import generic
class boost_pythonobj(generic.genobj):
  def __init__(self, senv=None):
    if senv: generic.genobj.__init__(self, 'shlib', senv)
    else: generic.genobj.__init__(self, 'shlib', env)
    
  def execute(self):
    self.uselibs += ['PYTHON', 'BOOST_PYTHON']  
    generic.genobj.execute(self)
    self.env.AddPostAction(self.target, '@$TARGET')
