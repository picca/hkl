#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy

from gi.repository import GLib
from gi.repository import Hkl

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
detector.idx_set(1)

config = Hkl.geometry_factory_get_config_from_type(
    Hkl.GeometryType.KAPPA6C)
geometry = Hkl.Geometry.factory_newv(config, [math.radians(50.)])
delta = geometry.axes()[5]
# delta.parameter.range.min = 0
# values_w = [0., -60, 0., 90., 0., 60.]
values_w = [0., 120, 0., -90., 0., 60.]
geometry.set_axis_values_unit(values_w)
axis_names = [axis.name for axis in geometry.axes()]

sample = Hkl.Sample.new("toto")
lattice = sample.lattice_get()
lattice.set(1.54, 1.54, 1.54,
            math.radians(90.0),
            math.radians(90.0),
            math.radians(90.))
sample.lattice_set(lattice)

# get all engines for a given configuration
engines = Hkl.EngineList.factory(config)

# prepare the engines to work with the related geometry, detector and
# sample
engines.init(geometry, detector, sample)

#[0, 0, 1] -> [0, 1, 1]
n = 10
hkl0 = [0, 0, 1]
hkl1 = [0, 1, 1]
h = numpy.linspace(hkl0[0], hkl1[0], n + 1)
k = numpy.linspace(hkl0[1], hkl1[1], n + 1)
l = numpy.linspace(hkl0[2], hkl1[2], n + 1)

# get the hkl engine
hkl = engines.get_by_name("hkl")
eulerians = engines.get_by_name("eulerians")
pseudo_names = [
    pseudo_axis.parameter.name for pseudo_axis in eulerians.pseudo_axes()]

# set the hkl engine and get the results
trajectories = []

for idx, h, k, l in zip(range(n), h, k, l):
    try:
        hkl.set_values_unit([h, k, l])
        # print geometry.get_axis_values_unit()
        if engines.geometries.len != 4:
            print idx, h, k, l
        for i, item in enumerate(engines.geometries().items()):
            try:
                trajectories[i]
            except IndexError:
                trajectories.append([])
            values = item.geometry.get_axis_values_unit()
            print values, geometry.distance(item.geometry)
            trajectories[i].append(values)
        print
    except GLib.GError, err:
        pass

plt.plot(trajectories[0])
plt.ylim(-180, 180)


def full():
    plt.figure()
    maximum = min(4, len(trajectories))
    for i, trajectory in enumerate(trajectories):
        if i < maximum:
            plt.subplot(1, maximum, i + 1)
            plt.plot(trajectory)
            plt.ylim(-180, 180)
