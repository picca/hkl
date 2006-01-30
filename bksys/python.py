# Made from scons qt4.py
# Frederic-Emmanuel PICCA, 2006 <picca at synchrotron-soleil fr>

"""
Run scons -h to display the associated help, or look below ..
"""

import os, sys

def exists(env):
  return True

def generate(env):
  """Set up the python environment and builders"""  
  if env['HELP']:
    p=env.pprint
    p('BOLD','*** python options ***')
    p('BOLD','--------------------')
    p('BOLD','* python_cpppath ','python includes path (/usr/include/pythonx.x on debian, ..)')
    p('BOLD','* python_libpath     ','python libraries path, for linking the program')
    p('BOLD','* scons configure python_includes=/usr/include/pythonx.x python_libpath=/usr/lib\n')
    return
  
  # Detect the environment - replaces ./configure implicitely and store the options into a cache
  from SCons.Options import Options
  cachefile=os.path.join(env['_CACHEDIR_'],'python.cache.py')
  opts = Options(cachefile)
  opts.AddOptions(
    ('PYTHON_CACHED', 'whether python was found'),
    ('PYTHON_VERSION', 'the python version'),
    ('PYTHON_CPPPATH', 'path to the boost_python includes'),
    ('PYTHON_LIBPATH', 'path to the boost_python libraries'),
    ('PYTHON_LIBS', 'path to the boost_python libraries'),
    ('PYTHON_CXXFLAGS', 'additional compilation flags'),
    ('PYTHON_LINKFLAGS','link flag')
  )
  opts.Update(env)
        
  # Reconfigure when things are missing
  if not env['HELP'] and (env['_CONFIGURE_'] or (not env.has_key('PYTHON_CACHED'))):
    
    # Erase all the options keys
    for opt in opts.options:
      if env.has_key(opt.key): env.__delitem__(opt.key)
    
    # Parse the command line 
    if env['_ARGS_'].get('python_cpppath',0):
      env['PYTHON_CPPPATH']=env['_ARGS_']['python_cpppath']
    if env['_ARGS_'].get('python_libpath', 0):
      env['PYTHON_LIBPATH']=env['_ARGS_']['python_libpath']
  
    # Load and run the platform specific configuration part
    from detect_python import detect      
    detect(env)
    
    python_test_file = """
#include <python%s/Python.h>
int main(int argc, char **argv)
{
  return 1;
}
""" % env['PYTHON_VERSION']

    #copy the environment
    lenv = env.Copy()
    LIB = 'PYTHON'
    if lenv.has_key(LIB+'_CPPPATH'): lenv.AppendUnique(CPPPATH = lenv[LIB+'_CPPPATH'])
    if lenv.has_key(LIB+'_LIBPATH'): lenv.AppendUnique(LIBPATH = lenv[LIB+'_LIBPATH'])
    if lenv.has_key(LIB+'_LIBS'): lenv.AppendUnique(LIBS = lenv[LIB+'_LIBS'])
    if lenv.has_key(LIB+'_CCFLAGS'): lenv.AppendUnique(CCFLAGS = lenv[LIB+'_CCFLAGS'])
    if lenv.has_key(LIB+'_CXXFLAGS'): lenv.AppendUnique(CXXFLAGS = lenv[LIB+'_CXXFLAGS'])
    if lenv.has_key(LIB+'_LINKFLAGS'): lenv.AppendUnique(LINKFLAGS = lenv[LIB+'_LINKFLAGS'])
            
    def Check_python(context):
      context.Message('Checking for python ...')
      ret = context.TryLink(python_test_file, '.cpp')
      context.Result(ret)
      return ret
        
    conf = lenv.Configure(custom_tests = { 'Check_python' : Check_python} )
    if not conf.Check_python():
      env.pprint('RED', 'please install python or set correctly the python paths!')
      env.Exit(1)
    lenv = conf.Finish()
        
    env['PYTHON_CACHED'] = 1
    opts.Save(cachefile, env)

  env.Export('env')
