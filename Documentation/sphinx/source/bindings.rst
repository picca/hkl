.. _bindings:

Bindings
########

The hkl library use the gobject-introspection to provide automatic
binding for a few languages.

Python
******

hkl computation::

  from gi.repository import Hkl

  detector = Hkl.Detector().factory_new(getattr(Hkl.DetectorType, '0D'))
  detector.idx = 1
