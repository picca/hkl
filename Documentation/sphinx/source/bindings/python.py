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

detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

factory = Hkl.factories()['K6C']
geometry = factory.create_new_geometry()
values_w = [0., 30., 0., 0., 0., 60.]
geometry.axes_values_set(values_w, Hkl.UnitEnum.USER)
axes_names = geometry.axes_names_get()
print geometry.name_get(), "diffractometer has", len(axes_names),\
    "axes : ", axes_names
print values_w

sample = Hkl.Sample.new("toto")
lattice = Hkl.Lattice.new(1.54, 1.54, 1.54,
                          math.radians(90.0),
                          math.radians(90.0),
                          math.radians(90.))
sample.lattice_set(lattice)

# compute all the pseudo axes managed by all engines
engines = factory.create_new_engine_list()
engines.init(geometry, detector, sample)
engines.get()

# get the hkl engine and do a computation
hkl = engines.engine_get_by_name("hkl")
values = hkl.pseudo_axes_values_get(Hkl.UnitEnum.USER)
print "read : ", values

# set the hkl engine and get the results
for _ in range(100):
    try:
        print
        solutions = hkl.pseudo_axes_values_set(values,
                                               Hkl.UnitEnum.USER)
        print hkl.pseudo_axes_values_get(Hkl.UnitEnum.USER)

        print("idx".center(15)),
        for name in axes_names:
            print("{}".format(name.center(15))),
        print

        for i, item in enumerate(solutions.items()):
            read = item.geometry_get().axes_values_get(Hkl.UnitEnum.USER)
            print("{}".format(repr(i).center(15))),
            for value in read:
                print("{}".format(repr(value)[:15].center(15))),
            print
    except GLib.GError, err:
        print values, err
    values[1] += .01
