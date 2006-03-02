import os, sys

colors={
  'BOLD'  :"\033[1m",
  'RED'   :"\033[91m",
  'GREEN' :"\033[92m",
  'YELLOW':"\033[93m", # unreadable on white backgrounds - fix konsole ?
  'BLUE'  :"\033[94m",
  'CYAN'  :"\033[96m",
  'NORMAL':"\033[0m",}

#Helper functions
def pprint(lenv, col, str, label=''):
  if lenv['_USECOLORS_']:
    print "%s%s%s %s" % (colors[col], str, colors['NORMAL'], label)
  else:
    print "%s %s" % (str, label)

def subdirs(lenv, folders):
  for folder in folders:
    lenv['CURBUILDDIR'] = folder[1:]
    lenv.SConscript(os.path.join(folder, 'SConscript'), build_dir = os.path.join(lenv['_BUILDDIR_'], folder), duplicate=1)

#add Helpers
from SCons.Script.SConscript import SConsEnvironment
SConsEnvironment.subdirs = subdirs
SConsEnvironment.pprint = pprint  
          
## CONFIGURATION: configure the project - this is the entry point
def configure(config):
  from SCons import Environment

  pkgname = ''
  pkgversion = ''
  cp_method = 'soft-copy'
  tool_path = ['bksys']
  build_dir = 'build-' + sys.platform
  cache_dir = 'cache-' + sys.platform
  use_colors = 0
  arguments = {}

  #add the bksys tool before all others
  mytools = ['default', 'bksys']
  
  # process the options
  for key in config.keys():
    if   key == 'pkgname' : pkgname = config[key]
    elif key == 'pkgversion' : pkgversion = config[key]
    elif key == 'modules' : mytools += config[key]
    elif key == 'builddir' : build_dir = config[key]
    elif key == 'cp_method' : cp_method = config[key]
    elif key == 'cachedir' : cache_dir = config[key]
    elif key == 'colorful' : use_colors = config[key]
    elif key == 'arguments' : arguments = config[key]
    else: print 'unknown key: '+key
  
  #Append the right path to find the platfrom specific functions
  sys.path.append(os.path.join('bksys',sys.platform))
    
  # make sure the build dir and cache dir are available
  # TODO what if it is in a non-existing subdirectory ? (ita)
  if not os.path.exists(build_dir): os.mkdir(build_dir)
  if not os.path.exists(cache_dir): os.mkdir(cache_dir)

  # bksys colors
  if os.environ.has_key('NOCOLORS') or sys.platform=='win32':
    use_colors=0
  
  # now build the environment
  env = Environment.Environment( _BUILDDIR_=build_dir,
    _USECOLORS_=use_colors, _CACHEDIR_=cache_dir, tools=mytools, 
    toolpath=tool_path, _ARGS_=arguments, _BKSYS_COLORS_=colors )
  
  #-- SCons cache directory
  # This avoids recompiling the same files over and over again: 
  # very handy when working with cvs
  # TODO: not portable so add a win32 ifdef
  if sys.platform=='win32' or os.getuid() != 0:
    env.CacheDir( os.path.join(os.getcwd(),cache_dir,'objects') )

  #  Avoid spreading .sconsign files everywhere - keep this line
  env.SConsignFile( os.path.join(cache_dir,'scons_signatures') )
  
  ## no colors if user does not want them
  if use_colors:
    c='%scompiling%s $TARGET' % (colors['GREEN'], colors['NORMAL'])
    l='%slinking%s $TARGET' % (colors['YELLOW'], colors['NORMAL'])
    env['CCCOMSTR']    =c
    env['SHCCCOMSTR']  =c
    env['CXXCOMSTR']   =c
    env['SHCXXCOMSTR'] =c
    env['LINKCOMSTR']  =l
    env['SHLINKCOMSTR']=l
          
  # at this point the help was displayed if asked to, then quit
  if env['HELP']: env.Exit(0)
    
  # we want symlinks by default
  env.SetOption('duplicate', cp_method)

  # now the post-configuration
  # write the config.h file and exit
  if env['_CONFIGURE_']:
    if env.has_key('_CONFIG_H_'):
      # put the files into the builddir
      dest=open(os.path.join(build_dir, 'config.h'), 'w')
      dest.write('#ifndef BKSYS_CONFIG_H\n#define BKSYS_CONFIG_H\n\n/* defines are added below */\n')
      for file in env['_CONFIG_H_']:
        dest.write('#include "config-%s.h"\n' % file)
      dest.write('\n\n#endif\n')
      dest.close()

    env.pprint('GREEN','configuration done - run scons to compile now')
    env.Exit(1)

  return env
