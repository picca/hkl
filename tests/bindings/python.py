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
        detector = Hkl.Detector().factory_new(Hkl.DetectorType(0))

        # mount the detector on the 2nd holder of the geometry
        # yes numbering follow the C convention !
        detector.idx = 1
        self.assertTrue(detector.idx == 1)

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
            isinstance([axis.parameter.name for axis in geometry.axes()],
                       list))

        # set the geometry axes values
        values_w = [0, 30, 0, 0, 0, 60]
        geometry.set_axes_values_unit(values_w)
        values_r = geometry.get_axes_values_unit()

        # check that the read and write values of the geometry are
        # almost equals
        for r, w in zip(values_w, values_r):
            self.assertAlmostEqual(r, w)

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

        detector = Hkl.Detector().factory_new(Hkl.DetectorType(0))
        detector.idx = 1

        factory = Hkl.factories()['K6C']
        geometry = factory.create_new_geometry()
        values_w = [0., 30., 0., 0., 0., 60.]
        geometry.set_axes_values_unit(values_w)

        sample = Hkl.Sample.new("toto", Hkl.SampleType.MONOCRYSTAL)
        sample.set_lattice(1.54, 1.54, 1.54,
                           math.radians(90.0),
                           math.radians(90.0),
                           math.radians(90.0))

        # compute all the pseudo axes managed by all engines
        engines = factory.create_new_engine_list()
        engines.init(geometry, detector, sample)
        engines.get()

        # get the hkl engine and do a computation
        hkl = engines.get_by_name("hkl")
        values = hkl.pseudo_axes().get_values_unit()

        # check for all modes
        for mode in hkl.modes():
            self.assertTrue(type(mode) is Hkl.Mode)

        # set the hkl engine and get the results
        for _ in range(100):
            try:
                hkl.pseudo_axes().set_values_unit(values)
                solutions = engines.geometries()
                for item in solutions.items():
                    item.geometry.get_axes_values_unit()
            except GLib.GError, err:
                print values, err
            values[1] += .01

        # for item in engines.geometries.items():
        #     print item.geometry.get_axes_values_unit()

        # check that all the values computed are reachable
        for engine in engines.engines():
            self.assertTrue(type(engine) is Hkl.Engine)
            self.assertTrue(type(engine.name()) is str)
            for parameter in engine.pseudo_axes().parameters():
                self.assertTrue(type(parameter) is Hkl.Parameter)
                self.assertTrue(type(parameter.get_value()) is float)

        # check the set result
        for item in engines.geometries().items():
            self.assertTrue(type(item) is Hkl.GeometryListItem)
            self.assertTrue(type(item.geometry) is Hkl.Geometry)

        self.assertTrue(True)

    @unittest.skip("for testing figures")
    def test_doc_exemple(self):
        #execfile("../../Documentation/sphinx/source/bindings/python.py")
        execfile("../../Documentation/sphinx/source/pyplots/trajectory_simple.py")
        execfile("../../Documentation/sphinx/source/pyplots/trajectory_full.py")

        self.assertTrue(False)

    def test_sample_api(self):
        """
        enforce the HklSample API
        """

        # create a sample
        sample = Hkl.Sample.new("toto", Hkl.SampleType.MONOCRYSTAL)
        self.assertTrue(sample.name == "toto")

        #set the lattice parameters
        sample.set_lattice(1.54, 1.54, 1.54,
                           math.radians(90.),
                           math.radians(90.),
                           math.radians(90.))


if __name__ == '__main__':
    unittest.main()
