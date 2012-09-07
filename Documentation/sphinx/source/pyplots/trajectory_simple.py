#!/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy

from gi.repository import GLib
from gi.repository import Hkl

import matplotlib.pyplot as plt

sample = Hkl.Sample.new("toto", Hkl.SampleType.MONOCRYSTAL)
sample.set_lattice(1.54, 1.54, 1.54,
                   math.radians(90),
                   math.radians(90),
                   math.radians(90))

detector = Hkl.Detector().factory_new(Hkl.DetectorType(0))
detector.idx = 1

config = Hkl.geometry_factory_get_config_from_type(
    Hkl.GeometryType.KAPPA6C)
geometry = Hkl.Geometry.factory_newv(config, [math.radians(50.)])
axes_names = [axis.parameter.name for axis in geometry.axes()]

# set the initial position
geometry.set_axes_values_unit([0, 120, 0, -90, 0, 60])

# get all engines for a given configuration
engines = Hkl.EngineList.factory(config)

# prepare the engines to work with the related geometry, detector and
# sample
engines.init(geometry, detector, sample)

#[0, 0, 1] -> [0, 1, 1]
n = 10
h = numpy.linspace(0, 0, n + 1)
k = numpy.linspace(0, 1, n + 1)
l = numpy.linspace(1, 1, n + 1)

# get the hkl engine
hkl = engines.get_by_name("hkl")
pseudo_axes_names = [parameter.name for parameter in hkl.pseudo_axes.parameters()]

# compute the trajectory
motors_positions = []
for idx, hh, kk, ll in zip(range(n), h, k, l):
    try:
        hkl.set_values_unit([hh, kk, ll])
        # if no exception raised we have at least one solution
        first = engines.geometries.items()[0].geometry
        positions = first.get_axes_values_unit()
        motors_positions.append(positions)
        # move the diffractometer to the solution
        geometry.set_axes_values_unit(positions)
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
