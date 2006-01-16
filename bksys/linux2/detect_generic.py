#! /usr/bin/env python

def detect(env):
	# gcc settings
	env.AppendUnique(GENERIC_CCFLAGS='-D_GNU_SOURCE')

import generic
class genobj(generic.genobj):
	def __init__(self, val, env):
		generic.genobj.__init__(self, val, env)
		pass
