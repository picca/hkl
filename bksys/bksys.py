## 
# @file 
# bksys core 
# 
# (\@) Thomas Nagy, 2005
# 
#  Run scons -h to display the associated help, or look below

#import os, re, types, sys, string, shutil, stat, glob
#import SCons.Defaults
#import SCons.Tool
#import SCons.Util
from SCons.Script.SConscript import SConsEnvironment
from SCons.Options import Options#, PathOption
import os

def pprint(env, col, str, label=''):
	if env['_USECOLORS_']:
		print "%s%s%s %s" % (env['BKSYS_COLORS'][col], str, env['BKSYS_COLORS']['NORMAL'], label)
	else:
		print "%s %s" % (str, label)

class dist:
	def __init__(self, env, name, version=''):
		import os
		if not version:
			f = open('VERSION', 'r')
			if f:
				version=f.readline().rstrip()
			else:
				version='please_set_version'
		self.env = env
		self.name = name
		self.version = version
		self.package = name + '-' + version
		self.cachedir = env['CACHEDIR']
		self.builddir = env['_BUILDDIR_']

	def clean_dir(self):
		import shutil
		#remove a previous directory
		if os.path.exists(self.package):
			shutil.rmtree(self.package)
		
	def create_dir(self):
		import shutil
		self.clean_dir()
		#remove an old package directory
		if os.path.exists(self.package):
			shutil.rmtree(self.package)

		#copy the project into the package directory
		shutil.copytree('.', self.name+'-'+self.version)
		#Enter into the package directory and remode unnecessary files
		os.chdir(self.package)
		for (root, dirs, filenames) in os.walk('.'):
			clean_dirs = []
			for d in dirs:
				if d in ['CVS', self.cachedir, self.builddir]:
					shutil.rmtree(os.path.join(root,d))
				elif d.startswith('.'):
					shutil.rmtree(os.path.join(root,d))
				else:
					clean_dirs += d
			dirs = clean_dirs
					
			to_remove = False
			for f in list(filenames):
				if f.startswith('.'):
					to_remove = True
				elif f.endswith('~'):
					to_remove = True
				elif f.endswith('.pyc'):
					to_remove = True
				elif f.endswith('.bak'):
					to_remove = True
				elif f.endswith('.orig'):
					to_remove = True
				elif f in ['config.log']:
					to_remove = True
				elif f.endswith('.tar.bz2'):
					to_remove = True
				elif f.endswith('.zip'):
					to_remove = True
				
				if to_remove:
					os.remove(os.path.join(root, f))
					to_remove = False
		#go back to the root directory
		os.chdir('../')

		
## Scons-specific function, do not remove
def exists(env):
	return true

## Entry point of the bksys module
def generate(env):

	## Bksys requires scons >= 0.96
	try: env.EnsureSConsVersion(0, 96, 91)
	except:
		pprint(env, 'RED', 'Your scons version is too old, make sure to use 0.96.91')
		env.Exit(1)

	## attach the helper functions to "env"
	SConsEnvironment.pprint = pprint
	
	env['HELP']=0
	if '--help' in sys.argv or '-h' in sys.argv or 'help' in sys.argv: env['HELP']=1
	if env['HELP']:
		p=env.pprint
		p('BOLD','*** Instructions ***')
		p('BOLD','--------------------')
		p('BOLD','* scons           ','to compile')
		p('BOLD','* scons -j4       ','to compile with several instances')
		p('BOLD','* scons install   ','to compile and install')
		p('BOLD','* scons -c install','to uninstall')
		p('BOLD','* debug        ','debug=1 (-g) or debug=full (-g3, slower) else use environment CXXFLAGS, or -O2 by default')
		p('BOLD','* prefix       ','the installation path')
		p('BOLD','* scons configure debug=full prefix=/usr/local extraincludes=/tmp/include:/usr/local extralibs=/usr/local/lib')
		p('BOLD','* scons install prefix=/opt/local DESTDIR=/tmp/blah\n')
		return

	#Parse the command line
	#debug
	if 'debug' in sys.argv:
		if env['ARGS'].has_key('debug'):
			env['BKS_DEBUG']=env['ARGS']['debug']
		else:
			env['BKS_DEBUG']=1
		env.pprint('CYAN','** Enabling debug for the project **')

	if 'install' in sys.argv:
		env.Alias('install',None)
		env['_INSTALL_']=1
	else:
		env['_INSTALL_']=0
		
	if 'configure' in sys.argv:
		env.Alias('configure', None)
		env['_CONFIGURE_']=1
	else:
		env['_CONFIGURE_']=0
	
	# load the options
	cachefile=os.path.join(env['CACHEDIR'], 'bksys.cache.py')
	opts = Options(cachefile)
	opts.AddOptions(
		('BKSYS_CACHED', 'is the project configured' ),
		('BKSYS_PREFIX', 'prefix for installation' ),
		('BKSYS_DEBUG', 'debug level: full, trace, or just something' ),
		('BKSYS_CCFLAGS', 'CC flags'),
		('BKSYS_LINKFLAGS', 'link flags')
	)
	opts.Update(env)
	# Configure the environment if needed
	if not env['HELP'] and (env.has_key('_CONFIGURE_') or not env.has_key('BKSYS_CACHED')):
		# Erase all the options keys
		for opt in opts.options:
			if env.has_key(opt.key): env.__delitem__(opt.key)	
		
		#User-specified prefix
		if env['ARGS'].has_key('prefix'):
			env['PREFIX'] = os.path.abspath( env['ARGS']['prefix'] )
			env.pprint('CYAN','** installation prefix for the project set to:',env['PREFIX'])
		
		#import the platform specific part
		# path to find the detect_generic class is set in configure
		from detect_bksys import detect
		detect(env)

		# And finally save the options in the cache
		env['BKSYS_CACHED']=1
		opts.Save(cachefile, env)

	#update the environment with the right flags
	if env.has_key('BKSYS_CCFLAGS'): env.AppendUnique(CCFLAGS=env['BKSYS_CCFLAGS'])
	if env.has_key('BKSYS_LINKFLAGS'): env.AppendUnique(LINKFLAGS=env['BKSYS_LINKFALGS'])
	
	## Install files on 'scons install'
	def bksys_install(lenv, subdir, files, destfile=None, perms=None):
		if not env['_INSTALL_']: return
		basedir = env['DESTDIR']
		install_list=None
		if not destfile: install_list = env.Install(lenv.join(basedir,subdir), lenv.make_list(files))
		elif subdir:     install_list = env.InstallAs(lenv.join(basedir,subdir,destfile), lenv.make_list(files))
		else:            install_list = env.InstallAs(lenv.join(basedir,destfile), lenv.make_list(files))
		# FIXME: this will not work with a list of files
		#if perms and install_list: lenv.AddPostAction(install_list, lenv.Chmod(install_list, perms))
		env.Alias('install', install_list)
		return install_list

	def bksys_insttype(lenv, type, subdir, files, perms=None):
		lenv.bksys_install( lenv.join(lenv.getInstDirForResType(type),subdir), files, destfile=None, perms=perms)

	## Writes a .la file, used by libtool 
	def build_la_file(target, source, env):
		dest=open(target[0].path, 'w')
		sname=source[0].name
		dest.write("# Generated by ltmain.sh - GNU libtool 1.5.18 - (pwn3d by bksys)\n#\n#\n")
		if not env['BKSYS_STATICLIB']:
			if len(env['BKSYS_VNUM'])>0:
				vnum=env['BKSYS_VNUM']
				nums=vnum.split('.')
				src=source[0].name
				if env['WINDOWS']: # TODO: add support for msvc 
					name = src.split('-')[0] + '.a'
					dest.write("dlname='%s'\n" % (sname) )
					dest.write("library_names='%s %s'\n" % (sname,name) )
				elif env['MAC']:
					name = src.split('.')[0]
					dest.write("dlname='%s'\n" % (name + '.' + str(nums[0]) + '.dylib') )
					dest.write("library_names='%s %s %s'\n" % (sname, name + '.' + str(nums[0]) + '.dylib', name + '.' + nums[0] + '.dylib') )
				else:
					name = src.split('so.')[0] + 'so'
					strn = src+" "+name+"."+str(nums[0])+" "+name
					dest.write("dlname='%s'\n" % (name+'.'+str(nums[0])) )
					dest.write("library_names='%s'\n" % (strn) )
			else:
				if env['WINDOWS']: # TODO: add support for msvc 
					src=source[0].name
					name = src.split('.')[0] + '.a'
					dest.write("dlname='%s'\n" % sname)
					dest.write("library_names='%s %s'\n" % (sname, name) )
				elif env['MAC']:
					src=source[0].name
					name = src.split('.')[0] + '.dylib'
					dest.write("dlname='%s'\n" % (sname) )
					dest.write("library_names='%s'\n" % (name) )
				else:
					dest.write("dlname='%s'\n" % sname)
					dest.write("library_names='%s %s %s'\n" % (sname, sname, sname) )
			dest.write("old_library=''\n")
		else:
			dest.write("dlname=''\n")
			dest.write("library_names='%s'\n" % (sname) )
			dest.write("old_library='%s'\n" % (sname) )

		dest.write("dependency_libs=''\ncurrent=0\n")
		dest.write("age=0\nrevision=0\ninstalled=yes\nshouldnotlink=no\n")
		dest.write("dlopen=''\ndlpreopen=''\n")
		dest.write("libdir='%s'" % env['BKSYS_DESTDIR'])
		dest.close()
		#return 0
	
	## template for compiling message 
	def string_la_file(target, source, env):
		blue=''
		normal=''
		if env['_USECOLORS_']:
			blue=env['BKS_COLORS']['BLUE']
			normal=env['BKS_COLORS']['NORMAL']
		return "%screating%s %s" % (blue, normal, target[0].path)

	## Writes a .dep file used for platform independent dependency checking 
	def build_dep_file(target, source, env):
		dest=open(target[0].path, 'w')
		dest.write("#bksys dependency file\n")
		dest.close()
		#return 0
	
	## template for compiling message 
	def string_dep_file(target, source, env):
		blue=''
		normal=''
		if env['_USECOLORS_']:
			blue=env['BKS_COLORS']['BLUE']
			normal=env['BKS_COLORS']['NORMAL']
		return "%screating%s %s" % (blue, normal, target[0].path)

	## Build symlinks
	def symlink_command(target, source, env):
		os.symlink( str(source[0].name), target[0].path)

	## template for symbolic linking targets message
	def symlink_str(target, source, env):
		yellow=''
		normal=''
		if env['_USECOLORS_']:
			yellow=env['BKS_COLORS']['YELLOW']
			normal=env['BKS_COLORS']['NORMAL']
		return "%ssymlinking%s %s (-> %s)" % (yellow, normal, target[0].path, str(source[0].name) )
	symlink = env.Action(symlink_command, symlink_str)
	env['BUILDERS']['SymLink'] = env.Builder(action=symlink)

	## Function for building shared libraries
	def bksys_shlib(lenv, ntarget, source, libdir, libprefix='lib', vnum='', noinst=None):
		"""Installs a shared library, with or without a version number, and create a
		.la file for use by libtool.
		
		If library version numbering is to be used, the version number
		should be passed as a period-delimited version number (e.g.
		vnum = '1.2.3').  This causes the library to be installed
		with its full version number, and with symlinks pointing to it.
		
		For example, for libfoo version 1.2.3, install the file
		libfoo.so.1.2.3, and create symlinks libfoo.so and
		libfoo.so.1 that point to it.
		"""
		# parameter can be a list
		if type(ntarget) is types.ListType: target=ntarget[0]
		else: target=ntarget

		thisenv = lenv.Copy() # copying an existing environment is +/- cheap
		thisenv['BKSYS_DESTDIR']=libdir
		thisenv['BKSYS_VNUM']=vnum
		thisenv['SHLIBPREFIX']=libprefix
		thisenv['BKSYS_STATICLIB']=0

		if len(vnum)>0:
			num=vnum.split('.')[0]
			if sys.platform == 'darwin':
				thisenv['SHLIBSUFFIX']='.'+vnum+'.dylib'
			elif env['WINDOWS']:
				thisenv['SHLIBSUFFIX']='-'+num+'.dll'
			else:
				thisenv['SHLIBSUFFIX']='.so.'+vnum
			thisenv.Depends(target, thisenv.Value(vnum))
			lst=target.split(os.sep)
			tname=lst[len(lst)-1]
			libname=tname.split('.')[0]
			# TODO: proper handling of bundles (LD* instead of LIB* in scons?)
			# TODO: -undefined dynamic_lookup == "allow undefined", need proper support for
			# TODO: -install_name is doing rpath-type-stuff manually, how do we want to handle rpath cleanly?
			# -no-undefined and similar in such a way that's cross-platform
			# DF: do we ever want shlibs with undefined symbols? How about we avoid that? :)
			# IIRC windows DLLs can't have undefined symbols.  OSX can, but it can be tricky. dyld prefers all defined symbols
			if sys.platform == 'darwin':
				thisenv.AppendUnique(LINKFLAGS = ["-undefined","error","-install_name", "%s/%s.%s.dylib" % (libdir, libprefix+libname, num)] )
			elif not env['WINDOWS']:
				thisenv.AppendUnique(LINKFLAGS = ["-Wl,--no-undefined","-Wl,--soname=%s.so.%s" % (libprefix+libname, num)] )

		# Fix against a scons bug - shared libs and ordinal out of range(128)
		if type(source) is types.ListType:
			src2=[]
			for i in source: src2.append( str(i) )
			source=src2

		library_list = thisenv.SharedLibrary(target, source)
		if thisenv['LIBTOOL']:
			lafile_list  = thisenv.LaFile(libprefix+target, library_list)
		if thisenv['DEPFILE']:
			depfile_list = thisenv.DepFile(libprefix+target, library_list)

		# Install the libraries automatically
		if not thisenv.has_key('NOAUTOINSTALL') and not noinst and libdir:
			inst_lib=thisenv.bksys_install(libdir, library_list)
			if thisenv['LIBTOOL']:
				thisenv.bksys_install(libdir, lafile_list)	

		# Handle the versioning
		if not env['WINDOWS'] and len(vnum)>0:
			nums=vnum.split('.')
			symlinkcom = ('cd $SOURCE.dir && rm -f $TARGET.name && ln -s $SOURCE.name $TARGET.name')
			if sys.platform == 'darwin':
				tg = libprefix+target+'.'+vnum+'.dylib'
				nm1 = libprefix+target+'.dylib'
				nm2 = libprefix+target+'.'+nums[0]+'.dylib'
			else:
				tg = libprefix+target+'.so.'+vnum
				nm1 = libprefix+target+'.so'
				nm2 = libprefix+target+'.so.'+nums[0]

			thisenv.SymLink(target=nm1, source=library_list)
			thisenv.SymLink(target=nm2, source=library_list)

			if env['_INSTALL_'] and libdir:
				link1 = env.join(str(inst_lib[0].dir), nm1)
				link2 = env.join(str(inst_lib[0].dir), nm2)
				src   = str(inst_lib[0].name)
				env.Alias('install', env.SymLink(target=link1, source=src))
				env.Alias('install', env.SymLink(target=link2, source=src))
		return library_list

	## link static library 
	def bksys_staticlib(lenv,target,source,libdir,libprefix='lib',noinst=None):
		thisenv = lenv.Copy() 
		library_list = thisenv.StaticLibrary(target, source)
		thisenv['BKSYS_VNUM']=''
		thisenv['BKSYS_DESTDIR']=libdir
		thisenv['BKSYS_STATICLIB']=1
		
		# TODO: install
		return library_list

	## Declare scons scripts to process
	def subdirs(lenv, folders):
		for folder in folders:
			lenv['CURBUILDDIR'] = folder[1:]
			lenv.SConscript(os.path.join(folder, 'SConscript'), build_dir = os.path.join(env['_BUILDDIR_'], folder), duplicate=1)
	"""
	## Links against a shared library made in the project 
	def link_local_shlib(lenv, str):
		lst = lenv.make_list(str)
		for afile in lst:
			import re
			file = slashify(afile)

			if sys.platform == 'darwin':
				reg=re.compile("(.*)/lib(.*).(dep|la|so|dylib)$")
			else:
				reg=re.compile("(.*)/lib(.*).(dep|la|so)$")
			result=reg.match(file)
			if not result:
				if sys.platform == 'darwin':
					reg = re.compile("(.*)/lib(.*)\.(\d+)\.(dep|la|so|dylib)")
				else:
					reg = re.compile("(.*)/lib(.*)\.(dep|la|so)\.(\d+)")
				result=reg.match(file)
				if not result:
					print "Unknown la file given "+file
					continue
				dir  = result.group(1)
				link = result.group(2)
			else:
				dir  = result.group(1)
				link = result.group(2)

			lenv.AppendUnique(LIBS = [link])
			lenv.PrependUnique(LIBPATH = [dir])

	## Links against a static library made in the project 
	def link_local_staticlib(lenv, str):
		lst = lenv.make_list(str)
		for afile in lst:
			import re
			file = slashify(afile)
			reg = re.compile("(.*)/(lib.*.(dep|la|a)$)")
			result = reg.match(file)
			if not result:
				print "Unknown archive file given "+file
				continue
			f=SCons.Node.FS.default_fs.File(file)
			lenv.Append(LIBPATH=[f.dir])
			lenv.Append(LIBS=[f.name])

	def create_file(target, source, env):
		f = open(str(target[0]), 'wb')
		f.write(source[0].get_contents())
		f.close()
	
	env['BUILDERS']['CreateFile'] = env.Builder(action = create_file)
	"""
	
	SConsEnvironment.bksys_install = bksys_install
	SConsEnvironment.bksys_insttype = bksys_insttype
	SConsEnvironment.bksys_shlib   = bksys_shlib
	SConsEnvironment.bksys_staticlib   = bksys_staticlib

	SConsEnvironment.subdirs = subdirs
	#SConsEnvironment.link_local_shlib = link_local_shlib
	#SConsEnvironment.link_local_staticlib = link_local_staticlib

	env.Export('env')
