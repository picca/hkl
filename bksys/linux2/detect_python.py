# Frederic-Emmanuel PICCA, 2006

import os,sys
import SCons.Util
from distutils import sysconfig

CLVar = SCons.Util.CLVar
  
def detect(env):
  #get the python version
  env['PYTHON_VERSION'] = sys.version[:3]
  #the install dir of the libraries
  env['PYTHON_DIR'] = sysconfig.get_python_lib()
  
  if env.has_key('PYTHON_LIBPATH') and env.has_key('PYTHON_CPPPATH'):
    env['PYTHON_LIBS'] = ['python%s' % env['PYTHON_VERSION']]
  else:
    env['PYTHON_CPPPATH'] = [sysconfig.get_python_inc()]
    env['PYTHON_LIBS'] = ['python%s' % env['PYTHON_VERSION']]
