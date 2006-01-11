#! /usr/bin/env python

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

def dist(env, appname, version=''):
	import os
	if not version: VERSION=os.popen("cat VERSION").read().rstrip()
	else: VERSION=version
	FOLDER  = appname+'-'+VERSION
	TMPFOLD = ".tmp"+FOLDER
	ARCHIVE = FOLDER+'.tar.bz2'

	## check if the temporary directory already exists
	os.popen('rm -rf %s %s %s' % (FOLDER, TMPFOLD, ARCHIVE) )

	## create a temporary directory
	startdir = os.getcwd()

	os.popen("mkdir -p "+TMPFOLD)	
	os.popen("cp -R * "+TMPFOLD)
	os.popen("mv "+TMPFOLD+" "+FOLDER)

	## remove scons-local if it is unpacked
	os.popen("rm -rf %s/scons %s/sconsign %s/scons-local-*" % (FOLDER, FOLDER, FOLDER))

	## remove our object files first
	os.popen("find %s -name \"cache\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \"build\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \"*.pyc\" | xargs rm -f " % FOLDER)

	## CVS cleanup
	os.popen("find %s -name \"CVS\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \".cvsignore\" | xargs rm -rf" % FOLDER)

	## Subversion cleanup
	os.popen("find %s -name .svn -type d | xargs rm -rf" % FOLDER)

	## GNU Arch cleanup
	os.popen("find %s -name \"{arch}\" | xargs rm -rf" % FOLDER)
	os.popen("find %s -name \".arch-i*\" | xargs rm -rf" % FOLDER)

	## Create the tarball (coloured output)
	env.pprint('GREEN', 'Writing archive '+ARCHIVE)
	os.popen("tar cjf %s %s " % (ARCHIVE, FOLDER))

	## Remove the temporary directory
	os.popen('rm -rf '+FOLDER)
	env.Exit(0)

def distclean(env):
	## Remove the cache directory
	import os, shutil
	if os.path.isdir(env['CACHEDIR']): shutil.rmtree(env['CACHEDIR'])
	os.popen("find . -name \"*.pyc\" | xargs rm -rf")
	env.Exit(0)

