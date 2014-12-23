#!/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy

from gi.repository import GLib
from gi.repository import Hkl

import matplotlib.pyplot as plt

sample = Hkl.Sample.new("toto")
lattice = Hkl.Lattice.new(1.54, 1.54, 1.54,
                          math.radians(90),
                          math.radians(90),
                          math.radians(90))
sample.lattice_set(lattice)

detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

factory = Hkl.factories()['K6C']
geometry = factory.create_new_geometry()
axes_names = geometry.axes_names_get()

# set the initial position
geometry.axes_values_set([0, 120, 0, -90, 0, 60], Hkl.UnitEnum.USER)

# get all engines for a given configuration
engines = factory.create_new_engine_list()

# prepare the engines to work with the related geometry, detector and
# sample
engines.init(geometry, detector, sample)

#[0, 0, 1] -> [0, 1, 1]
n = 10
h = numpy.linspace(0, 0, n + 1)
k = numpy.linspace(0, 1, n + 1)
l = numpy.linspace(1, 1, n + 1)

# get the hkl engine
hkl = engines.engine_get_by_name("hkl")
pseudo_axes_names = ["h", "k", "l"]

# compute the trajectory
motors_positions = []
for idx, hh, kk, ll in zip(range(n), h, k, l):
    try:
        solutions = hkl.pseudo_axes_values_set([hh, kk, ll],
                                               Hkl.UnitEnum.USER)
        first_solution = solutions.items()[0]
        # if no exception raised we have at least one solution
        # move the diffractometer to the solution
        engines.select_solution(first_solution)
        motors_positions.append(geometry.axes_values_get(Hkl.UnitEnum.USER))
    except GLib.GError, err:
        pass

plt.subplot(1, 2, 1)
plt.title("motors trajectory (1st solution)")
# reorder the motors_positions for the plot
motors_positions = numpy.array(motors_positions).T
for y, name in zip(motors_positions, axes_names):
    plt.plot(y, 'o-', label=name)
plt.legend(loc=2)
plt.ylim(-180, 180)
plt.xlabel("trajectory point index")
plt.ylabel("motor position (Degree)")

plt.subplot(1, 2, 2)
plt.title("hkl trajectory")
hkl_positions = numpy.array([h, k, l])
for y, name in zip(hkl_positions, pseudo_axes_names):
    plt.plot(y, 'o-', label=name)
plt.legend(loc=2)
plt.ylim(-0.1, 1.1)
plt.xlabel("trajectory point index")
plt.ylabel("pseudo motor position")
