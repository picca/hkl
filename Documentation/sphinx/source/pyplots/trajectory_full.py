#!/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy

import matplotlib.pyplot as plt

from gi.repository import GLib
from gi.repository import Hkl

sample = Hkl.Sample.new("toto", Hkl.SampleType.MONOCRYSTAL)
sample.set_lattice(1.54, 1.54, 1.54,
                   math.radians(90.0),
                   math.radians(90.0),
                   math.radians(90.))

detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
detector.idx_set(1)

factory = Hkl.factories()['K6C']
geometry = factory.create_new_geometry()
axes_names = [axis.name_get() for axis in geometry.axes()]
geometry.set_axes_values_unit([0., 120, 0., -90., 0., 60.])

engines = factory.create_new_engine_list()
engines.init(geometry, detector, sample)

n = 10
h = numpy.linspace(0, 0, n + 1)
k = numpy.linspace(0, 1, n + 1)
l = numpy.linspace(1, 1, n + 1)

# get the hkl engine
hkl = engines.get_by_name("hkl")

# set the hkl engine and get the results
trajectories = []
for hh, kk, ll in zip(h, k, l):
    try:
        hkl.set_values_unit([hh, kk, ll])
        for i, item in enumerate(engines.geometries().items()):
            try:
                trajectories[i]
            except IndexError:
                trajectories.append([])
            values = item.geometry().get_axes_values_unit()
            # print values, item.geometry.distance(geometry)
            trajectories[i].append(values)
        engines.select_solution(0)
        # print
    except GLib.GError, err:
        pass

for i, (trajectory, title) in enumerate(zip(trajectories[1:],
                                            ["2nd", "3rd", "4th"])):
    ax = plt.subplot(1, 3, i + 1)
    plt.title(title)
    plt.plot(trajectory, 'o-')
    plt.ylim(-180, 180)
    if i != 0:
        for tl in ax.get_yticklabels():
            tl.set_visible(False)
