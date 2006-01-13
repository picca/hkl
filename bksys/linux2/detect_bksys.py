#! /usr/bin/env python
import bksys

def detect(env):
	# default prefix for the installation
	if not env.has_key('PREFIX'):
		env['PREFIX'] = '/usr'
	
	# Debug mode
	if env.has_key('BKS_DEBUG'):
		env.AppendUnique(BKSYS_CCFLAGS='-g')
		env.AppendUnique(BKSYS_LINKFLAGS='-g')
	else:
		env.AppendUnique(BKSYS_CCFLAGS=['-O2', '-DNDEBUG'])

class dist(bksys.dist):
	def __init__(self, env, name, version=''):
		bksys.dist.__init__(self, env, name, version)
	
	def build(self):
		self.create_dir()
		import tarfile
		tar = tarfile.open(self.package+'.tar.bz2','w:bz2')
		tar.add(self.package)
		tar.close()
		self.clean_dir()
					
def distclean(env):
	## Remove the cache directory
	import os, shutil
	if os.path.isdir(env['CACHEDIR']): shutil.rmtree(env['CACHEDIR'])
	os.popen("find . -name \"*.pyc\" | xargs rm -rf")
	env.Exit(0)

