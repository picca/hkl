# vi:filetype=python:expandtab:tabstop=2:shiftwidth=2
import os, sys

#sys.path.append('tool')

#----------------------------------------------------------
# Required runtime environment
#----------------------------------------------------------

# Please use at least 0.98.0
EnsureSConsVersion(0, 98, 0)

#----------------------------------------------------------
# Global definitions
#----------------------------------------------------------

# some global settings

dirs_common = ['include/hkl', 'src', 'test', 'Documentation']

#----------------------------------------------------------
# platform dependent settings
#----------------------------------------------------------
print os.name
if os.name == 'nt':
  platform_name = 'win32'
  dirs_platform = []
elif os.name == 'posix' and sys.platform != 'cygwin':
  platform_name = sys.platform
  dirs_platform = []

#---------------------------------------------------------
# Handling options
#----------------------------------------------------------
option_file = 'config-' + platform_name + '.py'

opts = Options(option_file)
opts.AddOptions(
  # debug or release build
  EnumOption('mode', 'Building method', 'debug', allowed_values = ('debug', 'release')),
  # test
  BoolOption('test', 'Build and run the unit test', True),
  # gsl
  PathOption('gsl_inc_path', 'where we can find the gsl/*.h files', None),
  PathOption('gsl_lib_path', 'where we can find the gsl library files', None),
  # packaging
  PathOption('DESTDIR', 'Where to install the package', '/'),
  PathOption('prefix', 'the package is build for this path', '/usr')
)

#---------------------------------------------------------
# Setting up environment
#---------------------------------------------------------
env = Environment(tools = ['default', 'packaging'], options = opts)

# create the build directory
build_dir = os.path.join(env['mode'], platform_name)
if not os.path.isdir(build_dir):
  os.makedirs(build_dir)

# -h will print out help info
Help(opts.GenerateHelpText(env))

#add the default cxxflags and ldflags depending on the platform
cflags = []
cxxflags = []
cppdefines = []
linkflags = []

if platform_name == 'linux2':
  cflags += ['-Wall', '-std=gnu99']
  cxxflags += ['-Wall']
elif platform_name == 'win32':
  cflags += ['/GX', '/MD', '/GR']
  cxxflags += ['/GX', '/MD', '/GR']

#add the debug flag if needed
mode = None
if env.has_key('mode'):
  mode = env['mode']
if mode == 'debug':
  if platform_name == 'win32':
    cflags += ['/ZI']
    cxxflags += ['/ZI']
  elif platform_name == 'linux2':
    cflags += ['-g', '-O0']
    cxxflags += ['-g', '-O0']
elif mode == 'release':
  cppdefines += ['NDEBUG']
  if platform_name == 'linux2': 
    cflags += ['-O2']
    cxxflags += ['-O2']
  elif platform_name == 'win32':
    cflags += ['/Op']
    cxxflags += ['/Op']

env.AppendUnique(CFLAGS = cflags)
env.AppendUnique(CXXFLAGS = cxxflags)
env.AppendUnique(CPPDEFINES = cppdefines)
env.AppendUnique(LINKFLAGS = linkflags)

# Create a builder for tests
def builder_unit_test(target, source, env):
    app = str(source[0].abspath)
    lenv = os.environ
    lenv['LD_LIBRARY_PATH'] = os.path.join(build_dir, 'src')
    if os.spawnle(os.P_WAIT, app, app, lenv) == 0:
      open(str(target[0]),'w').write("PASSED\n")
    else:
      return -1

bld = Builder(action = builder_unit_test)
env.Append(BUILDERS = {'Test' :  bld})
opts.Save(option_file, env)

#----------------------------------------------------------
# Building
#----------------------------------------------------------

#put the SConsignFile in one place
sconsign_file = os.path.join(build_dir, '.sconsign')
env.SConsignFile(sconsign_file)

#no default target add manually
Default(None)
dirs = dirs_common + dirs_platform
for dir in dirs:
  file = os.path.join(dir, 'SConscript')
  env.SConscript(file, variant_dir = os.path.join(build_dir, dir), duplicate = 0, exports = 'env')

#----------------------------------------------------------
# Packaging
#----------------------------------------------------------

#trick to put the right sources in the package at the right place
sources = [ s.path.replace(os.path.join(build_dir, ''), '') for s in env.FindSourceFiles()]

env.Package(NAME           = 'hkl',
            VERSION        = '3.0.0',
            PACKAGEVERSION = 0,
            PACKAGETYPE    = ['src_tarbz2'],
            LICENSE        = 'gpl',
            SUMMARY        = 'Diffractometer computation library',
            DESCRIPTION    = 'Diffractometer computation library',
            SOURCE_URL     = 'http://foo.org/foo-1.2.3.tar.gz',
            source         = sources + ['README']
)
