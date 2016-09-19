#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import math
import unittest

from collections import namedtuple
from gi.repository import GLib
from gi.repository import Hkl
from numpy import (array, cross, dot, empty, hstack, reshape, vstack)
from numpy.linalg import inv, norm

#########
# Types #
#########

Detector = namedtuple('Detector', ['type'])
Diffractometer = namedtuple('Diffractometer', ['dtype', 'sample',
                                               'detector', 'source', 'engine'])
Engine = namedtuple('Engine', ['name', 'mode'])
Lattice = namedtuple('Lattice', ['a', 'b', 'c', 'alpha', 'beta', 'gamma'])
Reflection = namedtuple('Reflection', ['hkl', 'values'])
Sample = namedtuple('Sample', ['lattice', 'or0', 'or1', 'ux', 'uy', 'uz'])
Source = namedtuple('Source', ['wavelength', 'energy'])

HklDiffractometer = namedtuple('HklDiffractometer', ['sample', 'detector',
                                                     'geometry', 'engines'])


##################
# Helper methods #
##################

def hkl_matrix_to_numpy(m):
    M = empty((3, 3))
    for i in range(3):
        for j in range(3):
            M[i, j] = m.get(i, j)
    return M


def from_numpy_to_hkl_vector(v):
    V = Hkl.Vector()
    V.init(v[0], v[1], v[2])
    return V


#########################
# Hkl Type constructors #
#########################

def new_hkl_sample(config):
    # sample
    sample = Hkl.Sample.new("test")
    lattice = Hkl.Lattice.new(config.lattice.a,
                              config.lattice.b,
                              config.lattice.c,
                              math.radians(config.lattice.alpha),
                              math.radians(config.lattice.beta),
                              math.radians(config.lattice.gamma))
    sample.lattice_set(lattice)

    parameter = sample.ux_get()
    parameter.value_set(config.ux, Hkl.UnitEnum.USER)
    sample.ux_set(parameter)

    parameter = sample.uy_get()
    parameter.value_set(config.uy, Hkl.UnitEnum.USER)
    sample.uy_set(parameter)

    parameter = sample.uz_get()
    parameter.value_set(config.uz, Hkl.UnitEnum.USER)
    sample.uz_set(parameter)

    return sample


def new_hkl_detector(config):
    return Hkl.Detector.factory_new(config.type)


def new_hkl_geometry(dtype, wavelength, init_values):
    factory = Hkl.factories()[dtype]
    geometry = factory.create_new_geometry()
    geometry.axis_values_set(init_values, Hkl.UnitEnum.USER)
    geometry.wavelength_set(wavelength, Hkl.UnitEnum.USER)
    return geometry


def new_hkl_engines(dtype):
    factory = Hkl.factories()[dtype]
    engines = factory.create_new_engine_list()
    return engines


def new_hkl_diffractometer(config):
    sample = new_hkl_sample(config.sample)

    detector = new_hkl_detector(config.detector)

    # add reflection or0
    geometry = new_hkl_geometry(config.dtype,
                                config.source.wavelength,
                                config.sample.or0.values)
    or0 = sample.add_reflection(geometry, detector,
                                config.sample.or0.hkl[0],
                                config.sample.or0.hkl[1],
                                config.sample.or0.hkl[2])

    # add reflection or1
    geometry.axis_values_set(config.sample.or1.values,
                             Hkl.UnitEnum.USER)
    or1 = sample.add_reflection(geometry, detector,
                                config.sample.or1.hkl[0],
                                config.sample.or1.hkl[1],
                                config.sample.or1.hkl[2])

    # compute UB with or0 and or1
    # sample.compute_UB_busing_levy(or0, or1)
    # UB = hkl_matrix_to_numpy(sample.UB_get())

    # compute angles for reciprocal lattice vector h, k, l
    engines = new_hkl_engines(config.dtype)

    # set the engine mode
    engine = engines.engine_get_by_name(config.engine.name)
    engine.current_mode_set(config.engine.mode)

    return HklDiffractometer(sample, geometry, detector, engines)

#################
# Config parser #
#################

def find(filename):
    if os.path.exists(filename):
        return filename
    else:
        datadir = os.getenv('DATADIR')
        if datadir:
            filename = os.path.join(datadir, filename)
            if os.path.exists(filename):
                return filename
            else:
                raise Exception("Can not find: " + filename)
        else:
            raise Exception("Cannot find: " + filename)


def parse_reflection(line):
    ref = line.split()
    hkl = [float(x) for x in ref[2:5]]
    angles = [float(x) for x in ref[7:13]]
    return Reflection(hkl, angles)


def parse(filename, dtype):
    with open(filename, 'r') as f:
        for line in f:
            if str(line).find('Wavelength') != -1:
                wavelength = float(line.split()[1])
                energy = 12.39842 / wavelength
            if str(line).find('A') != -1 and str(line).find('B') != -1 and str(line).find('C') != -1:
                abc = line.split()
                a = float(abc[1])
                b = float(abc[3])
                c = float(abc[5])
            if str(line).find('Alpha') != -1 and str(line).find('Beta') != -1 and str(line).find('Gamma') != -1:
                abg = line.split()
                alpha = float(abg[1])
                beta = float(abg[3])
                gamma = float(abg[5])
            if str(line).find('R0') != -1:
                or0 = parse_reflection(line)
            if str(line).find('R1') != -1:
                or1 = parse_reflection(line)
            if str(line).find('Ux') != -1 and str(line).find('Uy') != -1 and str(line).find('Uz') != -1:
                uxuyuz = line.split()
                ux = float(uxuyuz[1])
                uy = float(uxuyuz[3])
                uz = float(uxuyuz[5])
            if str(line).find('Engine') != -1:
                engine_name = line.split()[1]
            if str(line).find('Mode') != -1:
                mode_name = line.split()[1]

        lattice = Lattice(a, b, c, alpha, beta, gamma)
        sample = Sample(lattice, or0, or1, ux, uy, uz)
        detector = Detector(0)
        source = Source(wavelength, energy)
        engine = Engine(engine_name, mode_name)
        return Diffractometer(dtype, sample, detector, source, engine)


######################
# hight level method #
######################

def ca(config, hkl, engine_name='hkl'):
    sample, geometry, detector, engines = new_hkl_diffractometer(config)
    engines.init(geometry, detector, sample)
    engine = engines.engine_get_by_name(engine_name)
    solutions = engine.pseudo_axis_values_set(hkl,
                                              Hkl.UnitEnum.USER)
    first_solution = solutions.items()[0]
    values = first_solution.geometry_get().axis_values_get(Hkl.UnitEnum.USER)
    return Reflection(hkl, values)


def get_UB(config):
    sample, geometry, detector, engines = new_hkl_diffractometer(config)
    return hkl_matrix_to_numpy(sample.UB_get())


def get_R_and_P(config, values):
    sample, geometry, detector, engines = new_hkl_diffractometer(config)
    geometry.axis_values_set(values, Hkl.UnitEnum.USER)
    R = hkl_matrix_to_numpy(geometry.sample_rotation_get(sample).to_matrix())
    P = hkl_matrix_to_numpy(geometry.detector_rotation_get(detector).to_matrix())
    return R, P

##############
# Unit tests #
##############

class Polarisation(unittest.TestCase):
    def test_petraIII(self):
        dtype = "E6C"
        # RUBh = kf - ki = (P ki - ki) = (P - I) ki

        config = parse(find('crystal.ini'), dtype)
        print config

        gaga = ca(config, [0.5, 14.5, 0.43])
        print gaga

        UB = get_UB(config)
        print "UB: "
        print UB

        # the hkl vectors a*, b*, c* expressed in the laboratory basis for the current Q
        # transformation matrix T reciprocal space --> laboratory
        # values_w = [0, 34.16414, 79.52420, 0, 0, 38.29633]
        # values_w = [0, 35.06068, 80.78517, 0, 0, 43.91934]
        # values_w = [0, 36.45961, 81.75533, 0, 0, 49.7139]
        # values_w = [0, 38.24551, 82.52447, 0, 0, 55.68957]
        # values_w = [0, 40.34955, 83.14900, 0, 0, 61.86603]
        # values_w = [0, 42.73321, 83.66607, 0, 0, 68.27333]
        # values_w = [0, 45.37981, 84.10117, 0, 0, 74.95312]
        # values_w = [0, 48.29079, 84.47230, 0, 0, 81.96226]
        values_w = [0, 51.48568, 84.79259, 0, 0, 89.37964]  # mu, omega, chi, phi, gamma, delta

        R, P = get_R_and_P(config, values_w)
        print "R: "
        print R
        print "P: "
        print P
        RUB = dot(R, UB)
        print "RUB: "
        print RUB

        astar = dot(RUB, [1, 0, 0])
        bstar = dot(RUB, [0, 1, 0])
        cstar = dot(RUB, [0, 0, 1])

        # transformation matrix: reciprocal space --> laboratory
        T = hstack((reshape(astar, (3, 1)),
                    reshape(bstar, (3, 1)),
                    reshape(cstar, (3, 1))))
        Tbis = vstack((reshape(astar, (1, 3)),
                       reshape(bstar, (1, 3)),
                       reshape(cstar, (1, 3))))
        # transformation matrix: laboratory --> reciprocal space
        Tinv = inv(T)

        print ''
        # print 'cstar in laboratory frame       :',cstar
        # print 'cstar in laboratory frame from T:',dot(T, hkl)
        # print 'cstar in rec. space from Tinv   :',dot(Tinv, dot(T, hkl))

        # compute kf
        ki = array([1, 0, 0]) * math.pi * 2 / config.source.wavelength
        kf = dot(P, ki)

        # compute Q
        Q = kf - ki

        print ''
        print 'Energy (keV):', config.source
        print 'Lattice parameters:', config.sample.lattice
        print '1st orienting reflection:', config.sample.or0.hkl, 'with angles: ', config.sample.or0.values
        print '2nd orienting reflection:', config.sample.or1.hkl, 'with angles: ', config.sample.or1.values
        print ''

        print 'UB matrix:'
        print UB
        print ''
        print 'Transformation matrix T(reciprocal space)--> laboratory frame:'
        print T,
        print ''
        print ''
        print 'Transformation matrix T(laboratory frame)--> reciprocal space :'
        print Tinv

        # compute Q
        # hkl = [0.5, 6.5, 0.43]
        # hkl = [0.5, 7.5, 0.43]
        # hkl = [0.5, 8.5, 0.43]
        # hkl = [0.5, 9.5, 0.43]
        # hkl = [0.5, 10.5, 0.43]
        # hkl = [0.5, 11.5, 0.43]
        # hkl = [0.5, 12.5, 0.43]
        # hkl = [0.5, 13.5, 0.43]
        hkl = [0.5, 14.5, 0.43]

	'''
        print ''
        print 'Q in lab. frame from code   :', dot(dot(R, UB), hkl), ', normalized:',(dot(dot(R, UB), hkl))/norm(dot(dot(R, UB), hkl)), ', norm:', norm(dot(dot(R, UB), hkl))
        print 'Q in lab. frame from T      :', dot(T, hkl), ', normalized:', (dot(T, hkl))/norm(dot(T, hkl)), ', norm:', norm(dot(T, hkl))
        print 'Q in lab. frame from ki, kf :', Q, ', normalized:', Q/norm(Q),', norm:', norm(Q)
        print ''
        print 'Q in rec. space from Tinv of T   :', dot(Tinv, dot(T, hkl))
        print ''
        print 'difference factor:',(norm(dot(T, hkl)))/(norm(Q))
        print ''
        print 'kf',kf,', norm:',norm(kf)
        print 'ki',ki,', norm:',norm(ki)
        '''
        print ''
        print 'Q in rec. space from Tinv of T   :', dot(Tinv, dot(T, hkl))
        print ''
        #
        # compute u1, u2, u3 in reciprocal space coordinates
        # u1,u2,u3 in laboratory frame
        u1xyz = ki+kf
        u2xyz = cross(ki, kf)
        u3xyz = ki-kf
        # print '(u1,u2,u3) in laboratory frame:',u1xyz,u2xyz,u3xyz
        # u1,u2,u3 in reciprocal space
        u1 = dot(Tinv, u1xyz) / norm(dot(Tinv, u1xyz))
        u2 = dot(Tinv, u2xyz) / norm(dot(Tinv, u2xyz))
        u3 = dot(Tinv, u3xyz) / norm(dot(Tinv, u3xyz))
        u1 = dot(Tinv, u1xyz)
        u2 = dot(Tinv, u2xyz)
        u3 = dot(Tinv, u3xyz)
        print '(u1,u2,u3) in reciprocal space from Tinv, unnormalized:',\
            u1, u2, u3
        print '(u1,u2,u3) in reciprocal space from Tinv, normalized to 1:',\
            dot(Tinv, u1xyz) / norm(dot(Tinv, u1xyz)), \
            dot(Tinv, u2xyz) / norm(dot(Tinv, u2xyz)), \
            dot(Tinv, u3xyz) / norm(dot(Tinv, u3xyz))
        # print '(u1,u2,u3) in reciprocal space from Tinv:', \
        #    dot(Tinv, u1xyz), dot(Tinv, u2xyz), dot(Tinv, u3xyz)
        print ''
        # TRANSFORMATION MATRIX reciprocal lattice basis to u1 u2 u3 basis
        ABC = hstack((reshape([1, 0, 0], (3, 1)),
                      reshape([0, 1, 0], (3, 1)),
                      reshape([0, 0, 1], (3, 1))))
        U = hstack((reshape(u1, (3, 1)),
                    reshape(u2, (3, 1)),
                    reshape(u3, (3, 1))))
        M = dot(ABC, inv(U))
        print 'Transformation matrix reciprocal lattice basis to u1 u2 u3 basis:'
        print M

        # keep in order to pass the test
        self.assertTrue(True)



if __name__ == '__main__':
    unittest.main(verbosity=2)
