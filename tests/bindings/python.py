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
    """Test all the Hkl API, if something brakes here it means that API
    has changed !!!
    """

    def test_vector_api(self):
        """
        enforce the Vector api
        """
        v = Hkl.Vector()
        self.assertTrue(type(v) == Hkl.Vector)
        self.assertTrue(type(v.data) == list)
        self.assertTrue(3 == len(v.data))

        del v

    def test_quaternion_api(self):
        """
        enforce the Vector api
        """
        q = Hkl.Quaternion()
        self.assertTrue(type(q) == Hkl.Quaternion)
        self.assertTrue(type(q.data) == list)
        self.assertTrue(4 == len(q.data))

        del q

    def test_factory_api(self):
        """
        enforce the Factory API
        """
        # factories dict <name, Factory>
        factories = Hkl.factories()
        for key, factory in factories.iteritems():
            self.assertTrue(type(key) == str)
            self.assertTrue(type(factory) == Hkl.Factory)

            # create all the geometry and engines
            geometry = factory.create_new_geometry()
            self.assertTrue(type(geometry) == Hkl.Geometry)
            engines = factory.create_new_engine_list()
            self.assertTrue(type(engines) == Hkl.EngineList)

        del factories

    def test_detector_api(self):
        """
        enforce the detector API
        """

        # create an 0D HklDetector
        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
        self.assertTrue(type(detector) is Hkl.Detector)

        del detector

    def test_geometry_api(self):
        """
        enforce the geometry API
        """

        # get the config for a given geometry and create the
        # corresponding HklGeometry
        factory = Hkl.factories()['K6C']
        geometry = factory.create_new_geometry()

        # set the geometry axes values
        values_w = [0, 30, 0, 0, 0, 60]
        geometry.axes_values_set(values_w, Hkl.UnitEnum.USER)
        values_r = geometry.axes_values_get(Hkl.UnitEnum.USER)

        # check that the read and write values of the geometry are
        # almost equals
        for r, w in zip(values_w, values_r):
            self.assertAlmostEqual(r, w)

        # check that we can access the axes
        axes_names = geometry.axes_names_get()
        for name in axes_names:
            axis = geometry.axis_get(name)
            axis.min_max_set(0, math.radians(180), Hkl.UnitEnum.USER)
            v = axis.axis_v_get()
            q = axis.quaternion_get()
            geometry.axis_set(name, axis)

        del geometry

    def test_engine_api(self):
        """
        enforce the HklEngine API
        """

        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

        factory = Hkl.factories()['K6C']
        geometry = factory.create_new_geometry()
        values_w = [0., 30., 0., 0., 0., 60.]
        geometry.axes_values_set(values_w, Hkl.UnitEnum.USER)

        sample = Hkl.Sample.new("toto")
        lattice = sample.lattice_get()
        lattice.set(1.54, 1.54, 1.54, 90, 90, 90, Hkl.UnitEnum.USER)
        sample.lattice_set(lattice)

        # compute all the pseudo axes managed by all engines
        engines = factory.create_new_engine_list()
        engines.init(geometry, detector, sample)
        engines.get()

        # get the hkl engine and do a computation
        hkl = engines.engine_get_by_name("hkl")
        values = hkl.pseudo_axes_values_get(Hkl.UnitEnum.USER)

        # check for all modes
        for mode in hkl.modes_names_get():
            self.assertTrue(type(mode) is str)

        # set the hkl engine and get the results
        for _ in range(100):
            try:
                solutions = hkl.pseudo_axes_values_set(values,
                                                       Hkl.UnitEnum.USER)
                self.assertTrue(type(solutions) is Hkl.GeometryList)
                for item in solutions.items():
                    self.assertTrue(type(item) is Hkl.GeometryListItem)
                    self.assertTrue(type(item.geometry_get()) is Hkl.Geometry)
                values[1] += .01
            except GLib.GError, err:
                print values, err

        # check that all the values computed are reachable
        for engine in engines.engines_get():
            self.assertTrue(type(engine) is Hkl.Engine)
            self.assertTrue(type(engine.name_get()) is str)
            self.assertTrue(type(engine.pseudo_axes_names_get()) is list)
            self.assertTrue(type(engine.modes_names_get()) is list)
            self.assertTrue(len(engine.modes_names_get()))
            for mode in engine.modes_names_get():
                self.assertTrue(type(mode) is str)
            values = engine.pseudo_axes_values_get(Hkl.UnitEnum.USER)
            self.assertTrue(type(values) is list)
            for value in values:
                self.assertTrue(type(value) is float)

        # check that all engine parameters and axes are reachables
        for engine in engines.engines_get():
            for mode in engine.modes_names_get():
                engine.current_mode_set(mode)
                parameters = engine.parameters_names_get()
                self.assertTrue(type(parameters) is list)
                axes_r = engine.axes_names_get(Hkl.EngineAxesNamesGet.READ)
                self.assertTrue(type(axes_r) is list)
                axes_w = engine.axes_names_get(Hkl.EngineAxesNamesGet.WRITE)
                self.assertTrue(type(axes_w) is list)

        # check all the capabilities
        for engine in engines.engines_get():
            capabilities = engine.capabilities_get()
            self.assertTrue(capabilities & Hkl.EngineCapabilities.READABLE)
            self.assertTrue(capabilities & Hkl.EngineCapabilities.WRITABLE)
            if engine.name_get() == "psi":
                self.assertTrue(capabilities & Hkl.EngineCapabilities.INITIALIZABLE)

        # check initialized_get/set
        for engine in engines.engines_get():
            initialized = engine.initialized_get()
            capabilities = engine.capabilities_get()
            if capabilities & Hkl.EngineCapabilities.INITIALIZABLE:
                engine.initialized_set(False)
                self.assertTrue(False == engine.initialized_get())
                engine.initialized_set(True)
                self.assertTrue(True == engine.initialized_get())

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
        a, b, c, alpha, beta, gamma = lattice.get(Hkl.UnitEnum.DEFAULT)
        lattice.set(a, b, c, alpha, beta, gamma, Hkl.UnitEnum.DEFAULT)

        # this new lattice is identical to the one from the sample
        v = lattice.get(Hkl.UnitEnum.DEFAULT)
        self.assertTrue(v == sample.lattice_get().get(Hkl.UnitEnum.DEFAULT))

        # now change the lattice parameter
        lattice.set(1, 2, 3, 90, 90, 90, Hkl.UnitEnum.USER)

        # this new lattice is different from the one in the sample
        v = lattice.get(Hkl.UnitEnum.DEFAULT)
        self.assertTrue(v != sample.lattice_get().get(Hkl.UnitEnum.DEFAULT))

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

        del sample
        del lattice

    def test_reflection_api(self):

        detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

        factory = Hkl.factories()['K6C']
        geometry = factory.create_new_geometry()
        values_w = [0., 30., 0., 0., 0., 60.]
        geometry.axes_values_set(values_w, Hkl.UnitEnum.USER)

        sample = Hkl.Sample.new("toto")

        # add reflection
        r1 = sample.add_reflection(geometry, detector, 1, 1, 1)
        r2 = sample.add_reflection(geometry, detector, 1, 1, 1)

        # get the hkl part
        self.assertTrue(r2.hkl_get() == (1.0, 1.0, 1.0))
        r2.hkl_set(1, 0, 1)
        self.assertTrue(r2.hkl_get() == (1.0, 0.0, 1.0))

        # compute the angles
        sample.get_reflection_mesured_angle(r1, r2)
        sample.get_reflection_theoretical_angle(r1, r2)

        # remove all the reflections
        reflections = sample.reflections_get()
        for reflection in reflections:
            sample.del_reflection(reflection)

        del reflections
        del sample

if __name__ == '__main__':
    unittest.main(verbosity=2)
