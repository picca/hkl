# Made from scons qt4.py
# Frederic-Emmanuel PICCA, 2006 <picca at synchrotron-soleil fr>

"""
Run scons -h to display the associated help, or look below ..
"""

TOOL = 'GTKMM'
USETOOLS = []

test_file = """
#include <boost/python.hpp>
int main(int argc, char **argv)
{
  return 1;
}
"""

tool = TOOL.lower()

LIBS = USETOOLS + [TOOL]
  
import os, sys
import SCons.Util
CLVar = SCons.Util.CLVar

def exists(env):
  return True


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

  # Add the needed Tools
  if USETOOLS:
    for t in USETOOLS:
      env.Tool(t.lower())
      
  # Detect the environment - replaces ./configure implicitely and store the options into a cache
  from SCons.Options import Options
  cachefile=os.path.join(env['_CACHEDIR_'],tool+'.cache.py')
  opts = Options(cachefile)
  opts.AddOptions(
    (TOOL+'_CACHED', 'whether %s was found' % TOOL),
    (TOOL+'_CPPPATH', 'path to the %s includes' % tool),
    (TOOL+'_LIBPATH', 'path to the %s libraries' % tool),
    (TOOL+'_LIBS', 'path to the %s libraries' % tool),
    (TOOL+'_CXXFLAGS', 'additional compilation flags'),
    (TOOL+'_LINKFLAGS','link flag')
  )
  opts.Update(env)
        
  # Reconfigure when things are missing
  if not env['HELP'] and (env['_CONFIGURE_'] or (not env.has_key(TOOL+'_CACHED'))):
    # Erase all the options keys
    for opt in opts.options:
      if env.has_key(opt.key): env.__delitem__(opt.key)
    
    # Parse the command line      
    if env['_ARGS_'].get('%s_cpppath' % tool,0):
      env[TOOL+'_CPPPATH']=env['_ARGS_']['%s_cpppath' % tool]
    if env['_ARGS_'].get('%s_libpath' % tool, 0):
      env[TOOL+'_LIBPATH']=env['_ARGS_']['%s_libpath' % tool]
  
    # now the platform specific part
    if sys.platform == 'linux2':
      if env.has_key(TOOL+'_LIBPATH') and env.has_key(TOOL+'_CPPPATH'):
        env[TOOL+'_LIBS'] = ['cppunit', 'dl']
      else:        
        pgm_config = env.WhereIs('pkg-config')
        if pgm_config:  
          env[TOOL+'_CXXFLAGS'] = SCons.Util.CLVar(
            os.popen(pgm_config+'gtkmm2.4 --cflags 2>/dev/null').read().strip() );
          env[TOOL+'_LINKFLAGS'] = SCons.Util.CLVar(
            os.popen(pgm_config+'gtkmm2.4 --libs 2>/dev/null').read().strip() );
    elif sys.platform in ['win32', 'nt']:
      env.pprint('RED', 'Platform not already proposed')
    else:
      env.pprint('RED', 'Platform not already proposed')
      
    # check if the library is set properly
    # copy the environment
    lenv = env.Copy()
    for LIB in LIBS:
      if lenv.has_key(LIB+'_CPPPATH'): lenv.AppendUnique(CPPPATH = lenv[LIB+'_CPPPATH'])
      if lenv.has_key(LIB+'_LIBPATH'): lenv.AppendUnique(LIBPATH = lenv[LIB+'_LIBPATH'])
      if lenv.has_key(LIB+'_LIBS'): lenv.AppendUnique(LIBS = lenv[LIB+'_LIBS'])
      if lenv.has_key(LIB+'_CCFLAGS'): lenv.AppendUnique(CCFLAGS = lenv[LIB+'_CCFLAGS'])
      if lenv.has_key(LIB+'_CXXFLAGS'): lenv.AppendUnique(CXXFLAGS = lenv[LIB+'_CXXFLAGS'])
      if lenv.has_key(LIB+'_LINKFLAGS'): lenv.AppendUnique(LINKFLAGS = lenv[LIB+'_LINKFLAGS'])
        
    def Check_tool(context):
      context.Message('Checking for %s ...' % tool)
      ret = context.TryLink(test_file, '.cpp')
      context.Result(ret)
      return ret
        
    conf = lenv.Configure(custom_tests = { 'Check_tool' : Check_tool} )
    if not conf.Check_tool():
      lenv.pprint('RED', 'please install %s or set correctly the %s paths!' % (tool, tool))
      lenv.Exit(1)
    lenv = conf.Finish()
    
    # end of configuration     
    env[TOOL+'_CACHED'] = 1
    opts.Save(cachefile, env)
  
  # Attach the functions to the environment so that SConscripts can use them
  from SCons.Script.SConscript import SConsEnvironment
  SConsEnvironment.gtkmmobj=gtkmmobj

  env.Export('env')

#define the gtkmmobj
import generic
class gtkmmobj(generic.genobj):
  def __init__(self, target, senv=None):
    if senv: generic.genobj.__init__(self, target, senv)
    else: generic.genobj.__init__(self, target, env)
    
  def execute(self):
    self.uselibs += LIBS
    generic.genobj.execute(self)
    self.env.AddPostAction(self.target, '@$TARGET')
