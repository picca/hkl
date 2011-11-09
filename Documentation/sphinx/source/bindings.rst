.. _bindings:

Bindings
########

The hkl library use the gobject-introspection to provide automatic
binding for a few languages.

Python
******

hkl computation::

  from gi.repository import Hkl

  detector = Hkl.Detector().factory_new(Hkl.DetectorType(0))
  detector.idx = 1

  # config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL);
  # geom = hkl_geometry_factory_new(config);
  # sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);

  # detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
  # detector->idx = 1;

  # engines = hkl_pseudo_axis_engine_list_factory(config);
  # hkl_pseudo_axis_engine_list_init(engines, geom, detector, sample);

  # engine = hkl_pseudo_axis_engine_list_get_by_name(engines, "hkl");

  # /* geometry -> pseudo */
  # SET_AXES(geom, 30., 0., 0., 60.);
  # hkl_pseudo_axis_engine_get(engine, NULL);
  # CHECK_PSEUDOAXES(engine, 0., 0., 1.);
