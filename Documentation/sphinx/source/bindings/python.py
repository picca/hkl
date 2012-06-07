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
from gi.repository import Hkl

detector = Hkl.Detector().factory_new(Hkl.DetectorType(0))
detector.idx = 1

config = Hkl.geometry_factory_get_config_from_type(Hkl.GeometryType.KAPPA6C)

geometry = Hkl.Geometry.factory_newv(config, [50. * math.pi / 180.])

sample = Hkl.Sample.new("toto", Hkl.SampleType.MONOCRYSTAL)
sample.set_lattice(1.54, 1.54, 1.54, 90., 90., 90.)

engines = Hkl.PseudoAxisEngineList.factory(config)
engines.init(geometry, detector, sample)

# print the current geometry axes values
values = geometry.get_axes_values_unit()
print values

# set the axes values
values = [0, 30, 0, 0, 0, 60]
geometry.set_axes_values_unit(values)

# compute all the pseudo axes managed by all engines
engines.get()

# engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

# /* geometry -> pseudo */
# SET_AXES(geom, 30., 0., 0., 60.);
# hkl_pseudo_axis_engine_get(engine, NULL);
# CHECK_PSEUDOAXES(engine, 0., 0., 1.);
