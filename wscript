#! /usr/bin/env python
# encoding: utf-8
# Thomas Nagy, 2006-2008 (ita)
# Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>, 2006-2010
# Jens Krüger <Jens.Krueger@frm2.tum.de>, 2009-2010

import UnitTest, os, Build, Options

# the following two variables are used by the target "waf dist"
f = open('configure.ac')
lines = f.readlines()
f.close()
line = [line for line in lines if 'AC_INIT' in line ][0]
VERSION_FULL = line.split(',')[1][1:-1]
APPNAME='hkl'
VERSION = VERSION_FULL.split('-')[0]

def set_options(opt):
	opt.tool_options('compiler_cc')
	opt.tool_options('misc')
	opt.add_option('--soleil', action='store_true', default=False, help='Build for the Soleil site')

def configure(conf):
	conf.check_tool('compiler_cc')
	conf.check_tool('misc')
	if Options.options.soleil:
		conf.env.LIB_GSL = ['GSL', 'GSLcblas', 'm']
		conf.env.LIBPATH_GSL = os.environ['SOLEIL_ROOT'] + '/sw-support/GSL/lib'
		conf.env.CPPPATH_GSL = os.environ['SOLEIL_ROOT'] + '/sw-support/GSL/include'
	else:
		conf.check_cfg(atleast_pkgconfig_version='0.0.0')
		conf.check_cfg(package='gsl', args='--cflags --libs')

	conf.env.VERSION = VERSION
	conf.env.prefix = conf.env.PREFIX

def build(bld):
	bld.add_subdirs('src test')

	#install the headers
	bld.install_files('${PREFIX}/include', 'include/hkl.h')
	bld.install_files('${PREFIX}/include/hkl', 'include/hkl/*.h')

	#create the pkg-config file hkl.pc.in -> hkl.pc
	bld.new_task_gen(
			features = 'subst',
			source = 'hkl.pc.in',
			target = 'hkl.pc',
			dict = bld.env)
	bld.install_files('${PREFIX}/lib/pkgconfig', 'hkl.pc')

def check(context):
	# Unit tests are run when "check" target is used
	ut = UnitTest.unit_test()
	ut.change_to_testfile_dir = True
	ut.want_to_see_test_output = True
	ut.want_to_see_test_error = True
	ut.run()
	ut.print_results()
