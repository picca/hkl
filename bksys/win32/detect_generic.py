#! /usr/bin/env python
## 
# @file  
# win32 related function for several bksys tasks like scanning environment,  
# shared library and .la file creation, creating source packages and 
# project cleaning 
#
                          
## detect win32 specific settings 
def detect(env):
	import os, sys
	import SCons.Util
	
	#default prefix for the installation
	if not env.has_key('PREFIX'):
		env['PREFIX'] = r'C:\'
			
	# (rh) The flags from GENCCFLAGS seems to be added to GENCXXFLAGS, 
	# so there is no need to duplicate settings in GENCXXGLAGS
	if env.has_key('BKS_DEBUG'):
		env.AppendUnique(GENCCFLAGS=['-Od','-ZI','-MDd'])
		env.AppendUnique(GENLINKFLAGS=['/INCREMENTAL:YES', '/DEBUG'])
	else:
		env.AppendUnique(GENCCFLAGS=['-MD'])
		env.AppendUnique(GENLINKFLAGS=[''])
	
## create source package
def dist(env):
	if not version: VERSION=os.popen("cat VERSION").read().rstrip()
	else: VERSION=version
	FOLDER  = appname+'-'+VERSION
	TMPFOLD = ".tmp"+FOLDER
	ARCHIVE = FOLDER+'.tar.bz2'

	# check if the temporary directory already exists
	os.popen('rm -rf %s %s %s' % (FOLDER, TMPFOLD, ARCHIVE) )

	# create a temporary directory
	startdir = os.getcwd()

	os.popen("mkdir -p "+TMPFOLD)	
	os.popen("cp -R * "+TMPFOLD)
	os.popen("mv "+TMPFOLD+" "+FOLDER)

	# remove scons-local if it is unpacked
	os.popen("rm -rf %s/scons %s/sconsign %s/scons-local-0.96.1" % (FOLDER, FOLDER, FOLDER))

	# remove our object files first
	os.popen("find %s -name \"cache\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \"build\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \"*.pyc\" | xargs rm -f " % FOLDER)

	# CVS cleanup
	os.popen("find %s -name \"CVS\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \".cvsignore\" | xargs rm -rf" % FOLDER)

	# Subversion cleanup
	os.popen("find %s -name .svn -type d | xargs rm -rf" % FOLDER)

	# GNU Arch cleanup
	os.popen("find %s -name \"{arch}\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \".arch-i*\" | xargs rm -rf" % FOLDER)

	# Create the tarball (coloured output)
	env.pprint('GREEN', 'Writing archive '+ARCHIVE)
	os.popen("tar cjf %s %s " % (ARCHIVE, FOLDER))

	# Remove the temporary directory
	os.popen('rm -rf '+FOLDER)
	env.Exit(0)

## Remove the cache directory
def distclean(env):
	import os, shutil
	if os.path.isdir(env['CACHEDIR']): shutil.rmtree(env['CACHEDIR'])
	os.popen("find . -name \"*.pyc\" | xargs rm -rf")
	env.Exit(0)
