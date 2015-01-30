#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

from gi.repository import GLib
from gi.repository import Hkl

sample = Hkl.Sample.new("toto")
lattice = Hkl.Lattice.new(1.54, 1.54, 1.54,
                          math.radians(90.0),
                          math.radians(90.0),
                          math.radians(90.))
sample.lattice_set(lattice)

detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))

factory = Hkl.factories()['K6C']
geometry = factory.create_new_geometry()
axis_names = geometry.axis_names_get()
geometry.axis_values_set([0., 120, 0., -90., 0., 60.],
                         Hkl.UnitEnum.USER)

engines = factory.create_new_engine_list()
engines.init(geometry, detector, sample)

n = 10
h = numpy.linspace(0, 0, n + 1)
k = numpy.linspace(0, 1, n + 1)
l = numpy.linspace(1, 1, n + 1)

# get the hkl engine
hkl = engines.engine_get_by_name("hkl")

# set the hkl engine and get the results
trajectories = []
for hh, kk, ll in zip(h, k, l):
    try:
        solutions = hkl.pseudo_axis_values_set([hh, kk, ll],
                                               Hkl.UnitEnum.USER)
        first_solution = solutions.items()[0]
        for i, item in enumerate(solutions.items()):
            try:
                trajectories[i]
            except IndexError:
                trajectories.append([])
            values = item.geometry_get().axis_values_get(Hkl.UnitEnum.USER)
            # print values, item.geometry.distance(geometry)
            trajectories[i].append(values)
        engines.select_solution(first_solution)
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
