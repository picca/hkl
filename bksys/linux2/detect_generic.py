#! /usr/bin/env python

def detect(env):
	# gcc settings
	env.AppendUnique(GENCCFLAGS='-D_GNU_SOURCE')

import generic
class genobj(generic.genobj):
	def __init__(self, val, env):
		generic.genobj.__init__(self, val, env)
		pass
