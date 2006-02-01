import os

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
		p('BOLD','* scons configure debug=full prefix=/usr/local')
		p('BOLD','* scons install prefix=/opt/local DESTDIR=/tmp/blah\n')
		return

	#Parse the command line
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
	
	#Create the clean target
	env.Clean('clean', ['.sconf_temp', 'config.log', env['_CACHEDIR_'], env['_BUILDDIR_']])
	if 'clean' in sys.argv:
	  env.SetOption('clean', 1)
	  	  
	# load the options
	from SCons.Options import Options
	cachefile=os.path.join(env['_CACHEDIR_'], 'bksys.cache.py')
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
	if not env['HELP'] and (env['_CONFIGURE_'] or not env.has_key('BKSYS_CACHED')):
		# Erase all the options keys
		for opt in opts.options:
			if env.has_key(opt.key): env.__delitem__(opt.key)	

		if 'debug' in sys.argv:
			if env['_ARGS_'].has_key('debug'):
				env['BKSYS_DEBUG']=env['_ARGS_']['debug']
			else:
				env['BKSYS_DEBUG']=1
			env.pprint('CYAN','** Enabling debug for the project **')
		
		#User-specified prefix
		if env['_ARGS_'].has_key('prefix'):
			env['BKSYS_PREFIX'] = os.path.abspath( env['_ARGS_']['prefix'] )
			env.pprint('CYAN','** installation prefix for the project set to:',env['BKSYS_PREFIX'])
		
		#import the platform specific part
		# path to find the detect_generic class is set in configure
		from detect_bksys import detect
		detect(env)

		# And finally save the options in the cache
		env['BKSYS_CACHED']=1
		opts.Save(cachefile, env)

	#update the environment with the right flags
	if env.has_key('BKSYS_CCFLAGS'): env.AppendUnique(CCFLAGS=env['BKSYS_CCFLAGS'])
	if env.has_key('BKSYS_LINKFLAGS'): env.AppendUnique(LINKFLAGS=env['BKSYS_LINKFLAGS'])

	env.Export('env')

#bksys common classes	
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
		self.cachedir = env['_CACHEDIR_']
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
	
