import fbuild
import fbuild.builders.text
import fbuild.builders.c
import fbuild.path

#extract the version number from the official autotools
f = open('configure.ac')
VERSION = [ l for l in f.readlines() if 'AC_INIT' in l][0].split(',')[1][1:-1]
f.close()

def build(ctx):
    #hkl.pc
    fbuild.builders.text.autoconf_config_file(ctx, 'hkl.pc', 'hkl.pc.in',
                                              {'VERSION': VERSION,
                                               'prefix' : '/usr/local'})
    
    #shared
    shared = fbuild.builders.c.guess_shared(ctx)
    hkl = shared.build_lib('hkl', fbuild.path.Path.glob('src/*.c'), includes=['include'])
    ctx.logger.log(' * running %s:' % hkl)
    
    #static and tests
    static = fbuild.builders.c.guess_static(ctx)
    hkl_static = static.build_lib('hkl', fbuild.path.Path.glob('src/*.c'), includes=['include'])
    ctx.logger.log(' * running %s:' % hkl_static)
    bench = static.build_exe('bench', fbuild.path.Path.glob('test/bench.c'),
                             includes=['include'], libs=[hkl_static])
    ctx.logger.log(' * running %s:' % bench)
    ctx.execute([bench])
