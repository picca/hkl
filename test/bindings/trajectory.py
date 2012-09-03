#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt

from matplotlib.backends.backend_pdf import PdfPages
from gi.repository import GLib
from gi.repository import Hkl


def compute_hkl_trajectories(sample, detector, geometry, engine, engines):
    n = 10
    h = numpy.linspace(0, 0, n + 1)
    k = numpy.linspace(0, 1, n + 1)
    l = numpy.linspace(1, 1, n + 1)

    # set the hkl engine and get the results
    trajectories = []
    for hh, kk, ll in zip(h, k, l):
        try:
            engine.set_values_unit([hh, kk, ll])
            for i, item in enumerate(engines.geometries.items()):
                try:
                    trajectories[i]
                except IndexError:
                    trajectories.append([])
                values = item.geometry.get_axes_values_unit()
                #print values, item.geometry.distance(geometry)
                trajectories[i].append(values)
                if i == 0:
                    values0 = values
            geometry.set_axes_values_unit(values0)
        except GLib.GError, err:
            pass

    return trajectories

def plot_hkl_trajectory(filename, sample, detector, geometry, engines):
    axes_names = [axis.parameter.name for axis in geometry.axes()]

    hkl = engines.get_by_name("hkl")
    trajectories = compute_hkl_trajectories(sample, detector, geometry, hkl, engines)

    n = min(len(trajectories), 1)
    plt.clf()
    for i, trajectory in enumerate(trajectories):
        ax = plt.subplot(1, n, i + 1)
        plt.title("%d solution" % i)
        plt.plot(trajectory, 'o-')
        #plt.ylim(-180, 180)
        if i != 0:
            for tl in ax.get_yticklabels():
                tl.set_visible(False)
        if i+1 == n:
            break
    plt.suptitle(filename + " " + hkl.mode.info.name)
    pp.savefig()


pp = PdfPages('trajectories.pdf')

def main():
    sample = Hkl.Sample.new("toto", Hkl.SampleType.MONOCRYSTAL)
    sample.set_lattice(1.54, 1.54, 1.54,
                       math.radians(90.0),
                       math.radians(90.0),
                       math.radians(90.))

    detector = Hkl.Detector().factory_new(Hkl.DetectorType(0))
    detector.idx = 1

    for i in range(len(Hkl.GeometryType.__enum_values__)):
        gtype = Hkl.GeometryType(i)
        config = Hkl.geometry_factory_get_config_from_type(gtype)
        geometry = Hkl.Geometry.factory_newv(config, [math.radians(50.)])
        engines = Hkl.PseudoAxisEngineList.factory(config)
        engines.init(geometry, detector, sample)

        print gtype.value_nick,
        engines_names = [engine.info.name for engine in engines.engines()]
        if 'hkl' in engines_names:
            plot_hkl_trajectory(gtype.value_nick, sample, detector, geometry, engines)
            print
        else:
            print ' skiped'
    pp.close()

if __name__ == '__main__':
    main()
