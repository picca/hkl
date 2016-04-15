#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy
import unittest

from gi.repository import GLib
from gi.repository import Hkl


def new_sample(a, b, c, alpha, beta, gamma, ux, uy, uz):
    # sample
    sample = Hkl.Sample.new("test")
    lattice = Hkl.Lattice.new(a, b, c,
                              math.radians(alpha),
                              math.radians(beta),
                              math.radians(gamma))
    sample.lattice_set(lattice)

    parameter = sample.ux_get()
    parameter.value_set(ux, Hkl.UnitEnum.USER)
    sample.ux_set(parameter)

    parameter = sample.uy_get()
    parameter.value_set(uy, Hkl.UnitEnum.USER)
    sample.uy_set(parameter)

    parameter = sample.uz_get()
    parameter.value_set(uz, Hkl.UnitEnum.USER)
    sample.uz_set(parameter)

    return sample


def new_geometry(dtype, init_values):
    factory = Hkl.factories()[dtype]
    geometry = factory.create_new_geometry()
    geometry.axis_values_set(init_values, Hkl.UnitEnum.USER)
    return geometry


def hkl_matrix_to_numpy(m):
    M = numpy.empty((3, 3))
    for i in range(3):
        for j in range(3):
            M[i, j] = m.get(i, j)
    return M


def from_numpy_to_hkl_vector(v):
    V = Hkl.Vector()
    V.init(v[0], v[1], v[2])
    return V


class Polarisation(unittest.TestCase):
    def test_petraIII(self):
        # RUBh = kf - ki = (P ki - ki) = (P - I) ki

        sample = new_sample(1.54, 1.54, 1.54,
                            90, 90, 90,
                            -90, 0, 0)
        UB = hkl_matrix_to_numpy(sample.UB_get())

        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

        values_w = [0, 30, 0, 0, 0, 60]  # mu, omega, chi, phi, gamma, delta
        geometry = new_geometry("E6C", values_w)

        # get rotation as quaternions
        qs = geometry.sample_rotation_get(sample)
        qd = geometry.detector_rotation_get(detector)

        # R = hkl_matrix_to_numpy(geometry.R_get())  # get the full sample rotation matrix
        # P = hkl_matrix_to_numpy(geometry.P_get())  # get the full detector sample rotation matrix

        ki = [1, 0, 0]

        hkl = [1, 1, 1]
        # v = numpy.dot(numpy.dot(R, UB), hkl) # the hkl vector express in the laboratory basis.

        # print v
        self.assertTrue(True)

if __name__ == '__main__':
    unittest.main(verbosity=2)
