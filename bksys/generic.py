## 
# @file 
# bksys core 
# 
# (\@) Thomas Nagy, 2005
# 
#  Run scons -h to display the associated help, or look below

#import os, re, types, sys, string, shutil, stat, glob
#import os
#import SCons.Defaults
#import SCons.Tool
#import SCons.Util
import os
from SCons.Options import Options#, PathOption

## Scons-specific function, do not remove
def exists(env):
	return true

## Entry point of the module generic
def generate(env):
	if env['HELP']:
		p=env.pprint
		p('BOLD','\n*** Generic options ***')
		p('BOLD','--------------------')
		p('BOLD','* extraincludes','a list of paths separated by ":"')
		p('BOLD','* extralibs','a list of paths separated by ":"')
		p('BOLD','* scons configure extraincludes=/tmp/include:/usr/local extralibs=/usr/local/lib')
		return

	# load the options
	cachefile=os.path.join(env['_CACHEDIR_'],'generic.cache.py')
	opts = Options(cachefile)
	opts.AddOptions(
		('GENERIC_CACHED', 'is the project configured' ),
		('GENERIC_CPPPATH', 'extra includes path'),
		('GENERIC_LIBPATH', 'extra libs path'),
		('GENERIC_CCFLAGS', 'extra CC flags'),
		('GENERIC_CXXFLAGS', 'extra CXX flags'),
		('GENERIC_LINKFLAGS', 'extra link flags')
	)
	opts.Update(env)

	# Configure the environment if needed
	if not env['HELP'] and (env['_CONFIGURE_'] or not env.has_key('GENERIC_CACHED')):
		# Erase all the options keys
		for opt in opts.options:
			if env.has_key(opt.key): env.__delitem__(opt.key)

		# User-specified include paths
		env['EXTRAINCLUDES'] = env['_ARGS_'].get('extraincludes', None)
		if env['EXTRAINCLUDES']:
			env['EXTRAINCLUDES'] = env['EXTRAINCLUDES'].split(os.pathsep)
			env.pprint('CYAN','** extra include paths for the project set to:',env['EXTRAINCLUDES'])

		# User-specified library search paths
		env['EXTRALIBS'] = env['_ARGS_'].get('extralibs', None)
		if env['EXTRALIBS']:
			env['EXTRALIBS'] = env['EXTRALIBS'].split(os.pathsep)
			env.pprint('CYAN','** extra library search paths for the project set to:',env['EXTRALIBS'])
		
		#import the platform specific part
		# path to find the detect_generic class is set in configure
		from detect_generic import detect
		detect(env)

		# And finally save the options in the cache
		env['GENERIC_CACHED']=1
		opts.Save(cachefile, env)


	if env.has_key('GENERIC_CPPPATH'): env.AppendUnique(CPPPATH = env['GENERIC_CPPPATH'] )
	if env.has_key('GENERIC_LIBPATH'): env.AppendUnique(LIBPATH = env['GENERIC_LIBPATH'] )
	if env.has_key('GENERIC_CCFLAGS'): env.AppendUnique(CCFLAGS = env['GENERIC_CCFLAGS'] )
	if env.has_key('GENERIC_CXXFLAGS'): env.AppendUnique(CXXFLAGS = env['GENERIC_CXXFLAGS'] )
	if env.has_key('GENERIC_LINKFLAGS'): env.AppendUnique(LINKFLAGS = env['GENERIC_LINKFLAGS'] )

	env.Export('env')


## class for building binary targets like programs, shared libraries, static libs, 
#  loadable modules and convenience libraries
class genobj:
	## construct a binary target 
	#
	# @val type of binary object "program", "shlib", "staticlib", "convenience"
	# @env used scons environment 
	def __init__(self, val, env):
		if not val in ["program", "shlib", "staticlib", "convenience"]:
			env.pprint('RED', 'unknown object type given to genobj: '+val)
			env.Exit(1)

		self.type  = val
		self.orenv = env
		self.env   = None

		## list of source files 
		self.source=''
		## target name 
		self.target=''

		## flags for c compiler (may be platform dependent)
		self.ccflags   =''
		## additional include path(es) (using '/' on every platform)  
		self.includes  =''

		## flags for linker (may be platform dependent)
		self.linkflags =''
		## list of path(es) containing required libraries (use with libs class member)
		self.libpaths  =''
		## list of libraries which should be included in this binary object
		# use the basic library name without any platform specific prefix or suffixes 
		# on linux e.g. use xyz for the real library named libxyz.so 
		self.libs      =''

		# vars used by shlibs
		self.vnum=''

		## a directory where to install the targets (optional)
		self.instdir=''
		if self.instdir=='shlib':
			self.instdir=self.env['PREFIX']+os.sep+'lib'
		elif self.instdir=='program':
			self.instdir=self.env['PREFIX']+os.sep+'bin'

	## When an object is created and the sources, targets, etc are given
	# the execute command calls the SCons functions like Program, Library, etc
	def execute(self):
		# copy the environment if a subclass has not already done it
		if not self.env:
			self.env = self.orenv.Copy()

		if (not self.source or len(self.source) == 0):
			self.env.pprint('RED',"no source file given to object - self.source for "+self.target)
			self.env.Exit(1)

		if not self.target:
			self.env.pprint('RED',"no target given to object - self.target")
			self.env.Exit(1)

		self.env.AppendUnique(CPPPATH = ['.', os.path.join('#',self.env['_BUILDDIR_'])])

		if len(self.includes)>0: self.env.AppendUnique(CPPPATH = self.includes)
		if len(self.ccflags)>0: self.env.AppendUnique(CCFLAGS = self.ccflags)
		if len(self.libs)>0: self.env.AppendUnique(LIBS=self.libs)

		# Settings for static and shared libraries
		if len(self.libpaths)>0: self.env.PrependUnique(LIBPATH=self.libpaths)
		if len(self.linkflags)>0: self.env.PrependUnique(LINKFLAGS=self.linkflags)

		# The target to return - IMPORTANT no more self.env modification is possible after this part
		ret=None
		if self.type=='shlib':
			ret=self.env.bksys_shlib(self.target, self.source, self.instdir, 
				self.libprefix, self.vnum)
		elif self.type=='program':
			ret=self.env.Program(self.target, self.source)
			if not self.env.has_key('NOAUTOINSTALL') and self.instdir:
				ins=self.env.bksys_install(self.instdir, ret, perms=self.perms)
		elif self.type=='staticlib' or self.type=='convenience':
			ret=self.env.bksys_staticlib(self.target, self.source, self.instdir)

from SCons.Script.SConscript import SConsEnvironment
SConsEnvironment.genobj=genobj
