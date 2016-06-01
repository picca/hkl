#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import unittest

from gi.repository import GLib
from gi.repository import Hkl
from numpy import (array, dot, empty)

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


def new_geometry(dtype, wavelength, init_values):
    factory = Hkl.factories()[dtype]
    geometry = factory.create_new_geometry()
    geometry.axis_values_set(init_values, Hkl.UnitEnum.USER)
    geometry.wavelength_set(wavelength, Hkl.UnitEnum.USER)
    return geometry


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


class Polarisation(unittest.TestCase):
    def test_petraIII(self):
        # RUBh = kf - ki = (P ki - ki) = (P - I) ki

        sample = new_sample(1.54, 1.54, 1.54,
                            90, 90, 90,
                            -90, 0, 0)

        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

        # add reflection or0
        values_w = [0, 30, 0, 0, 0, 60]  # mu, omega, chi, phi, gamma, delta
        wavelength = 1.4878
        geometry = new_geometry("E6C", wavelength, values_w)
        or0 = sample.add_reflection(geometry, detector, 0, 0, 1)

        # add reflection or1
        values_w = [0, 30, 90, 0, 0, 60]  # mu, omega, chi, phi, gamma, delta
        geometry.axis_values_set(values_w, Hkl.UnitEnum.USER)
        or1 = sample.add_reflection(geometry, detector, 0, 1, 0)

        # compute UB
        sample.compute_UB_busing_levy(or0, or1)
        UB = hkl_matrix_to_numpy(sample.UB_get())

        # the hkl vector express in the laboratory basis.
        hkl = [1, 1, 1]
        R = hkl_matrix_to_numpy(geometry.sample_rotation_get(sample).to_matrix())

        v = dot(dot(R, UB), hkl)

        # compute kf
        ki = array([1, 0, 0]) * math.pi * 2 / wavelength
        P = hkl_matrix_to_numpy(geometry.detector_rotation_get(detector).to_matrix())
        kf = dot(P, ki)

        Q = kf - ki

        # print v
        self.assertTrue(True)

if __name__ == '__main__':
    unittest.main(verbosity=2)
