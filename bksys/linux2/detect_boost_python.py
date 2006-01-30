# Frederic-Emmanuel PICCA, 2006

import os
import SCons.Util

CLVar = SCons.Util.CLVar
  
def detect(env):
  if env.has_key('BOOST_PYTHON_LIBPATH') and env.has_key('BOOST_PYTHON_CPPPATH'):
    env['BOOST_PYTHON_LIBS'] = ['boost_python']
  else:
    env['BOOST_PYTHON_LIBS'] = ['boost_python']

import boost_python
class boost_pythonobj(boost_python.boost_pythonobj):
  def __init__(self, env):
    boost_python.boost_pythonobj.__init__(self, env)
