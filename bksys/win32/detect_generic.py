#! /usr/bin/env python
## 
# @file  
# win32 related function for several bksys tasks like scanning environment,  
# shared library and .la file creation, creating source packages and 
# project cleaning 
#
                          
## detect win32 specific settings 
def detect(env):
	env.AppendUnique(GENERIC_CXXFLAGS='/GX')

import generic
class genobj(generic.genobj):
	def __init__(self, val, env):
		generic.genobj.__init__(self, val, env)
		pass
