# vi:filetype=python:expandtab:tabstop=2:shiftwidth=2
import os, sys

#sys.path.append('tool')

#----------------------------------------------------------
# Required runtime environment
#----------------------------------------------------------

# FIXME: I remember lyx requires higher version of python?
EnsurePythonVersion(1, 5)
# Please use at least 0.96.91 (not 0.96.1)
EnsureSConsVersion(0, 96, 91)

#----------------------------------------------------------
# Global definitions
#----------------------------------------------------------

# some global settings
PACKAGE_VERSION = '2.1.99'
DEVEL_VERSION = True
default_build_mode = 'debug'

PACKAGE = 'hkl'
PACKAGE_BUGREPORT = 'picca@synchrotron-soleil.fr'
PACKAGE_NAME = 'hkl'
PACKAGE_TARNAME = 'hkl'
PACKAGE_STRING = '%s %s' % (PACKAGE_NAME, PACKAGE_VERSION)

dirs = ['src', 'test', 'doc', 'binding/python']

#----------------------------------------------------------
# platform dependent settings
#----------------------------------------------------------
if os.name == 'nt':
  platform_name = 'win32'
elif os.name == 'posix' and sys.platform != 'cygwin':
  platform_name = sys.platform

#---------------------------------------------------------
# Handling options
#----------------------------------------------------------
option_file = 'config-' + platform_name + '.py'

opts = Options(option_file)
opts.AddOptions(
  # debug or release build
  EnumOption('mode', 'Building method', default_build_mode, allowed_values = ('debug', 'release')),
  # FIXME: not implemented
  BoolOption('profile', '(NA) Whether or not enable profiling', False),
  # cppunit
  BoolOption('test', 'Build and run the unit test', True),
  PathOption('cppunit_lib_path', 'Path to cppunit library directory', None),
  PathOption('cppunit_inc_path', 'Path to cppunit includes directory', None),
)

#---------------------------------------------------------
# Setting up environment
#---------------------------------------------------------
env = Environment(toolpath = ['tool'], tools = ['default', 'doxygen'], options = opts)

# create the build directory
build_dir = os.path.join(env['mode'], platform_name)
if not os.path.isdir(build_dir):
  os.makedirs(build_dir)

# -h will print out help info
Help(opts.GenerateHelpText(env))

#add the default cxxflags depending on the platform
cxxflags = []
if platform_name == 'win32':
  cxxflags += ['/GX', '/MD', '/GR']

#add the debug flag if needed
mode = None
if env.has_key('mode'):
  mode = env['mode']
if  mode == 'debug':
  if platform_name == 'win32':
    cxxflags += []
  elif platform_name == 'linux2':
    cxxflags += ['-g', '-O0']
elif mode == 'release':
  cxxflags += ['-O2']
env.AppendUnique(CXXFLAGS = cxxflags)

# Create a builder for tests
def builder_unit_test(target, source, env):
    app = str(source[0].abspath)
    if os.spawnl(os.P_WAIT, app, app) == 0:
      open(str(target[0]),'w').write("PASSED\n")
    else:
      return 1

bld = Builder(action = builder_unit_test)
env.Append(BUILDERS = {'Test' :  bld})
opts.Save(option_file, env)

#----------------------------------------------------------
# Start building
#----------------------------------------------------------

#put the SConsignFile in one place
sconsign_file = os.path.join(build_dir, '.sconsign')
env.SConsignFile(sconsign_file)

#no default target add manually
Default(None)
for dir in dirs:
  file = os.path.join(dir, 'SConscript')
  env.SConscript(file, build_dir = os.path.join(build_dir, dir), duplicate = 0, exports = 'env')

