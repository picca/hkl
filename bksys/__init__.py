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
#from SCons.Script.SConscript import SConsEnvironment
#from SCons.Options import Options, PathOption

import os, sys

## CONFIGURATION: configure the project - this is the entry point
def configure(dict):
	from SCons import Environment

	cp_method  = 'soft-copy'
	tool_path  = ['bksys']
	build_dir  = '.'
	cache_dir  = 'cache'+os.sep
	use_colors = 0
	arguments  = {}

	#add the bksys tool before all others
	mytools = ['default', 'bksys']
	
	# process the options
	for key in dict.keys():
		if   key == 'modules'	: mytools    += dict[key].split()
		elif key == 'builddir'	: build_dir  = dict[key]
		elif key == 'cp_method'	: cp_method  = dict[key]
		elif key == 'cachedir'	: cache_dir  = dict[key]
		elif key == 'colorful'	: use_colors = dict[key]
		elif key == 'arguments' : arguments = dict[key]
		else: print 'unknown key: '+key
	
	#Append the right path to find the platfrom specific functions
	sys.path.append('bksys'+os.sep+sys.platform)

	# make sure the build dir and cache dir are available
	# TODO what if it is in a non-existing subdirectory ? (ita)
	if not os.path.exists(build_dir): os.mkdir(build_dir)
	if not os.path.exists(cache_dir): os.mkdir(cache_dir)

	# bksys colors
	colors={
	'BOLD'  :"\033[1m",
	'RED'   :"\033[91m",
	'GREEN' :"\033[92m",
	'YELLOW':"\033[93m", # unreadable on white backgrounds - fix konsole ?
	'BLUE'  :"\033[94m",
	'CYAN'  :"\033[96m",
	'NORMAL':"\033[0m",}
	
	# now build the environment
	env = Environment.Environment( ENV=os.environ, _BUILDDIR_=build_dir,
		_USECOLORS_=use_colors, CACHEDIR=cache_dir, tools=mytools, 
		toolpath=tool_path, ARGS=arguments, BKS_COLORS=colors )
	
	#-- SCons cache directory
	# This avoids recompiling the same files over and over again: 
	# very handy when working with cvs
	# TODO: not portable so add a win32 ifdef
	if sys.platform=='win32' or os.getuid() != 0:
		env.CacheDir( os.path.join(os.getcwd(),'cache','objects') )

	#  Avoid spreading .sconsign files everywhere - keep this line
	env.SConsignFile( cache_dir+'scons_signatures' )
	
	## no colors if user does not want them
	if os.environ.has_key('NOCOLORS') or sys.platform=='win32':
		env['_USECOLORS_']=0
	else:
		c='%scompiling%s $TARGET' % (colors['GREEN'], colors['NORMAL'])
		l='%slinking%s $TARGET' % (colors['YELLOW'], colors['NORMAL'])
		env['CCCOMSTR']    =c
		env['SHCCCOMSTR']  =c
		env['CXXCOMSTR']   =c
		env['SHCXXCOMSTR'] =c
		env['LINKCOMSTR']  =l
		env['SHLINKCOMSTR']=l
					
	# at this point the help was displayed if asked to, then quit
	if env['HELP']: env.Exit(0)

	# we want symlinks by default
	env.SetOption('duplicate', cp_method)

	# now the post-configuration
	# write the config.h file and exit
	if env['_CONFIGURE_']:
		if env.has_key('_CONFIG_H_'):
			# put the files into the builddir
			dest=open(os.path.join(build_dir, 'config.h'), 'w')
			dest.write('#ifndef BKSYS_CONFIG_H\n#define BKSYS_CONFIG_H\n\n/* defines are added below */\n')
			for file in env['_CONFIG_H_']:
				dest.write('#include "config-%s.h"\n' % file)
			dest.write('\n\n#endif\n')
			dest.close()

		env.pprint('GREEN','configuration done - run scons to compile now')
		env.Exit(1)

	return env
