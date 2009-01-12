#! /usr/bin/env python
# encoding: utf-8
# Thomas Nagy, 2006-2008 (ita)

# the following two variables are used by the target "waf dist"
VERSION='3.0.0-rc1'
APPNAME='hkl'

# these variables are mandatory ('/' are converted automatically)
srcdir = '.'
blddir = 'build'

def set_options(opt):
	opt.tool_options('compiler_cc')
	opt.tool_options('misc')

def configure(conf):
	conf.check_tool('compiler_cc')
	conf.check_tool('misc')
	conf.check_cfg(atleast_pkgconfig_version='0.0.0')
	conf.check_cfg(package='gsl', args='--cflags --libs')
	conf.env['HKL_VERSION'] = VERSION.split('-')[0]

def build(bld):
	bld.add_subdirs('src test')

	#install the headers
	bld.install_files('${PREFIX}/include/hkl', 'include/hkl/*.h')

	#create the pkg-config file hkl.pc.in -> hkl.pc
	bld.new_task_gen(
			features = 'subst',
			source = 'hkl.pc.in',
			target = 'hkl.pc',
			dict = bld.env)
	bld.install_files('${PREFIX}/lib/pkgconfig', 'hkl.pc')



def shutdown():
	# Unit tests are run when "check" target is used
	import UnitTest
	ut = UnitTest.unit_test()
	ut.change_to_testfile_dir = True
	ut.run()
	ut.print_results()
