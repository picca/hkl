#!/usr/bin/env python
# -*- coding: utf-8 -*-

import math
import numpy
import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt

from matplotlib.backends.backend_pdf import PdfPages
from matplotlib import rcParams
from gi.repository import GLib
from gi.repository import Hkl


def compute_hkl_trajectories(engine, hkl1=None, hkl2=None, n=100):
    """
    compute all the trajectories for a given engine already configured
    """
    if not hkl1:
        hkl1 = [0, 0, 1]
    if not hkl2:
        hkl2 = [0, 1, 1]

    h = numpy.linspace(hkl1[0], hkl2[0], n + 1)
    k = numpy.linspace(hkl1[1], hkl2[1], n + 1)
    l = numpy.linspace(hkl1[2], hkl2[2], n + 1)

    # set the hkl engine and get the results
    trajectories = []
    for hh, kk, ll in zip(h, k, l):
        try:
            engine.set_values_unit([hh, kk, ll])
            solutions = engine.engines_get().geometries()
            for i, item in enumerate(solutions.items()):
                try:
                    trajectories[i]
                except IndexError:
                    trajectories.append([])
                values = item.geometry().get_axes_values_unit()
                trajectories[i].append(values)
            engine.engines_get().select_solution(0)
        except GLib.GError, err:
            pass

    return trajectories


def _plot_legend(axes):
    plt.subplot(3, 4, 1)
    plt.title("legend")
    print "legende", 1
    for name in axes:
        plt.plot([0, 0], label=name)
    plt.legend()


def plot_hkl_trajectory(filename, geometry, engines,
                        hkl1=None, hkl2=None, n=100):
    """
    plot the trajectory for a engine. It is possible to limit the
    number of trajectory using the max_traj keyword
    """
    axes_names = [axis.name_get() for axis in geometry.axes()]

    hkl = engines.get_by_name("hkl")
    page = 1
    plt.clf()
    plt.suptitle("\"" + filename + "\" " + repr(
        hkl1) + " -> " + repr(hkl2) + " page " + str(page))
    _plot_legend(axes_names)
    idx = 2
    for mode in hkl.modes_get():
        hkl.select_mode(mode)
        trajectories = compute_hkl_trajectories(hkl, hkl1=hkl1, hkl2=hkl2, n=n)
        print "\"" + filename + "\"", idx, mode.name_get(), len(trajectories)

        plt.subplot(3, 4, idx)
        plt.title("%s" % (mode.name_get(),))
        if not len(trajectories):
            plt.text(0.5, 0.5, "Failed", size=20, rotation=0.,
                     ha="center", va="center",
                     bbox=dict(boxstyle="round",
                               ec=(1., 0.5, 0.5),
                               fc=(1., 0.8, 0.8),
                               )
                     )
            plt.draw()
        else:
            plt.ylim(-180, 180)
            if len(trajectories[0]) == 1:
                plt.plot(trajectories[0], 'o-')
            else:
                plt.plot(trajectories[0], '-')

        idx += 1
        if idx > 12:
            pp.savefig()
            plt.clf()
            page += 1
            _plot_legend(axes_names)
            plt.suptitle(filename + " " + repr(
                hkl1) + " -> " + repr(hkl2) + " page " + str(page))
            idx = 2
    pp.savefig()


pp = PdfPages('trajectories.pdf')
rcParams['font.size'] = 6


def main():
    sample = Hkl.Sample.new("toto")
    lattice = sample.lattice_get()
    lattice.set(1.54, 1.54, 1.54,
                math.radians(90.0),
                math.radians(90.0),
                math.radians(90.))
    sample.lattice_set(lattice)

    detector = Hkl.Detector.factory_new(Hkl.DetectorType(0))
    detector.idx_set(1)

    for key, factory in Hkl.factories().iteritems():
        geometry = factory.create_new_geometry()
        engines = factory.create_new_engine_list()

        # here we set the detector arm with only positiv values for
        # now tth or delta arm
        for axis in geometry.axes():
            if axis.name_get() in ["tth", "delta"]:
                axis.min_max_unit_set(0, 180.)

        engines.init(geometry, detector, sample)

        engines_names = [engine.name_get() for engine in engines.engines()]
        if 'hkl' in engines_names:
            plot_hkl_trajectory(key, geometry, engines,
                                hkl1=[0, 0, 1], hkl2=[0, 1, 1], n=100)
    pp.close()

if __name__ == '__main__':
    main()
