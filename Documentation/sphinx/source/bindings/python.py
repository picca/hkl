#!/bin/env python
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

Copyright (C) 2003-2012 Synchrotron SOLEIL
                        L'Orme des Merisiers Saint-Aubin
                        BP 48 91192 GIF-sur-YVETTE CEDEX
Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
"""

import math
from gi.repository import GLib
from gi.repository import Hkl

detector = Hkl.Detector().factory_new(Hkl.DetectorType(0))
detector.idx = 1

config = Hkl.geometry_factory_get_config_from_type(
    Hkl.GeometryType.KAPPA6C)
geometry = Hkl.Geometry.factory_newv(config, [math.radians(50.)])
values_w = [0., 30., 0., 0., 0., 60.]
geometry.set_axes_values_unit(values_w)
axes_names = [axis.parent_instance.name for axis in geometry.axes()]
print config.name, "diffractometer has", geometry.len,\
    "axes : ", axes_names
print values_w

sample = Hkl.Sample.new("toto", Hkl.SampleType.MONOCRYSTAL)
sample.set_lattice(1.54, 1.54, 1.54,
                   math.radians(90.0),
                   math.radians(90.0),
                   math.radians(90.))

# compute all the pseudo axes managed by all engines
engines = Hkl.PseudoAxisEngineList.factory(config)
engines.init(geometry, detector, sample)
engines.get()

# get the hkl engine and do a computation
hkl = engines.get_by_name("hkl")
values = hkl.get_values_unit()
print "read : ", values

# set the hkl engine and get the results
for _ in range(100):
    try:
        print
        hkl.set_values_unit(values)
        print hkl.get_values_unit()

        print("idx".center(15)),
        for name in axes_names:
            print("{}".format(name.center(15))),
        print

        for i, item in enumerate(engines.geometries.items()):
            read = item.geometry.get_axes_values_unit()
            print("{}".format(repr(i).center(15))),
            for value in read:
                print("{}".format(repr(value)[:15].center(15))),
            print
    except GLib.GError, err:
        print values, err
    values[1] += .01
