#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This file is part of the hkl library.

The hkl library is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

The hkl library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.

Copyright (C) 2012-2013 Synchrotron SOLEIL
                        L'Orme des Merisiers Saint-Aubin
                        BP 48 91192 GIF-sur-YVETTE CEDEX
Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
"""

import sys
import math
import unittest
from gi.repository import GLib
from gi.repository import Hkl


class TestAPI(unittest.TestCase):

    @unittest.skip("factory not yet ready")
    def test_factory_api(self):
        """
        enforce the Factory API
        """
        # factories dict <name, Factory>
        factories = Hkl.factories()
        for key, value in factories.iteritems():
            self.assertTrue(type(key) == str)
            self.assertTrue(type(value) == Hkl.Factory)

        kappa6C_factory = factories['Kappa6C']
        geometry = kappa6C_factory.create_new_geometry()
        engines = kappa6C_factory.create_new_engine_list()

    def test_detector_api(self):
        """
        enforce the detector API
        """

        # create an 0D HklDetector
        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
        self.assertTrue(type(detector) is Hkl.Detector)

        # mount the detector on the 2nd holder of the geometry
        # yes numbering follow the C convention !
        detector.idx_set(1)

    def test_geometry_api(self):
        """
        enforce the geometry API
        """

        # get the config for a given geometry and create the
        # corresponding HklGeometry
        factory = Hkl.factories()['K6C']
        geometry = factory.create_new_geometry()

        # axes names are accessible
        self.assertTrue(
            isinstance([axis.name_get() for axis in geometry.axes()],
                       list))

        # set the geometry axes values
        values_w = [0, 30, 0, 0, 0, 60]
        geometry.set_axes_values_unit(values_w)
        values_r = geometry.get_axes_values_unit()

        # check that the read and write values of the geometry are
        # almost equals
        for r, w in zip(values_w, values_r):
            self.assertAlmostEqual(r, w)

        # check that we can access the axes
        axes = geometry.axes()
        self.assertTrue(len([axis.name_get() for axis in axes]) != 0)
        for axis in axes:
            self.assertTrue(type(axis.name_get()) is str)
            axis.min_max_unit_set(0, math.radians(180))

    def test_mode_api(self):
        """
        enforce the HklMode API
        """
        factory = Hkl.factories()['K6C']
        engines = factory.create_new_engine_list()
        engine = engines.get_by_name("hkl")

        # check for all modes
        for mode in engine.modes():
            self.assertTrue(type(mode) is Hkl.Mode)
            self.assertTrue(type(mode.name()) is str)

            # check the parameters
            parameters = mode.parameters()
            self.assertTrue(type(parameters) is Hkl.ParameterList)
            for parameter in parameters.parameters():
                self.assertTrue(type(parameter) is Hkl.Parameter)

    def test_engine_api(self):
        """
        enforce the HklEngine API
        """

        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
        detector.idx_set(1)

        factory = Hkl.factories()['K6C']
        geometry = factory.create_new_geometry()
        values_w = [0., 30., 0., 0., 0., 60.]
        geometry.set_axes_values_unit(values_w)

        sample = Hkl.Sample.new("toto")
        lattice = sample.lattice_get()
        lattice.set(1.54, 1.54, 1.54,
                    math.radians(90.0),
                    math.radians(90.0),
                    math.radians(90.0))
        sample.lattice_set(lattice)

        # compute all the pseudo axes managed by all engines
        engines = factory.create_new_engine_list()
        engines.init(geometry, detector, sample)
        engines.get()

        # get the hkl engine and do a computation
        hkl = engines.get_by_name("hkl")
        values = hkl.pseudo_axes().values_unit_get()

        # check for all modes
        for mode in hkl.modes():
            self.assertTrue(type(mode) is Hkl.Mode)

        # set the hkl engine and get the results
        for _ in range(100):
            try:
                hkl.set_values_unit(values)
            except GLib.GError, err:
                print values, err
            solutions = engines.geometries()
            self.assertTrue(type(solutions) is Hkl.GeometryList)
            for item in solutions.items():
                self.assertTrue(type(item) is Hkl.GeometryListItem)
                self.assertTrue(type(item.geometry()) is Hkl.Geometry)
            values[1] += .01

        # check that all the values computed are reachable
        for engine in engines.engines():
            self.assertTrue(type(engine) is Hkl.Engine)
            self.assertTrue(type(engine.name()) is str)
            for parameter in engine.pseudo_axes().parameters():
                self.assertTrue(type(parameter) is Hkl.Parameter)
                self.assertTrue(type(parameter.value_get()) is float)

    @unittest.skip("for testing figures")
    def test_doc_exemple(self):
        # execfile("../../Documentation/sphinx/source/bindings/python.py")
        execfile(
            "../../Documentation/sphinx/source/pyplots/trajectory_simple.py")
        execfile(
            "../../Documentation/sphinx/source/pyplots/trajectory_full.py")

        self.assertTrue(False)

    def test_sample_api(self):
        """
        enforce the HklSample API
        """

        # create a sample
        sample = Hkl.Sample.new("toto")
        self.assertTrue(sample.name_get() == "toto")

        # check that the copy constructor is working
        copy = sample.copy()
        self.assertTrue(copy.name_get() == sample.name_get())
        # we can change the name of the copy without affecting the original
        copy.name_set("titi")
        self.assertTrue(copy.name_get() != sample.name_get())

        # set the lattice parameters
        lattice = Hkl.Lattice.new(1.54, 1.54, 1.54,
                                  math.radians(90.),
                                  math.radians(90.),
                                  math.radians(90.))
        sample.lattice_set(lattice)

        # change the lattice parameter by expanding the tuple from
        # the get method. the lattice should not change.
        lattice.set(*lattice.get())

        # this new lattice is identical to the one from the sample
        self.assertTrue(lattice.get() == sample.lattice_get().get())

        # now change the lattice parameter
        lattice.set(1, 2, 3,
                    math.radians(90),
                    math.radians(90),
                    math.radians(90))

        # this new lattice is different from the one in the sample
        self.assertTrue(lattice.get() != sample.lattice_get().get())

        # gives access to the ux, uy, uz part
        ux = sample.ux_get()
        uy = sample.uy_get()
        uz = sample.uz_get()
        sample.ux_set(ux)
        sample.uy_set(uy)
        sample.uz_set(uz)

        # read and write the U matrix
        U = sample.U_get()
        UB = sample.UB_get()
        sample.UB_set(UB)

        # get the reciprocal lattice
        reciprocal = lattice.copy()
        lattice.reciprocal(reciprocal)

    def test_reflection_api(self):

        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
        detector.idx_set(1)

        factory = Hkl.factories()['K6C']
        geometry = factory.create_new_geometry()
        values_w = [0., 30., 0., 0., 0., 60.]
        geometry.set_axes_values_unit(values_w)

        sample = Hkl.Sample.new("toto")

        # add reflection
        r1 = sample.add_reflection(geometry, detector, 1, 1, 1)
        r2 = sample.add_reflection(geometry, detector, 1, 1, 1)

        # get the hkl part
        self.assertTrue(r2.hkl_get() == (1.0, 1.0, 1.0))
        r2.hkl_set(1, 0, 1)
        self.assertTrue(r2.hkl_get() == (1.0, 0.0, 1.0))

        # compute the angles
        print sample.get_reflection_mesured_angle(r1, r2)
        print sample.get_reflection_theoretical_angle(r1, r2)

        # remove all the reflections
        reflections = sample.reflections_get()
        for reflection in reflections:
            sample.del_reflection(reflection)

if __name__ == '__main__':
    unittest.main()