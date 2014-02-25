SOLEIL SIRIUS TURRET
####################

Geometry
********

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 3 axes for the sample

  + **thetah** : rotating around the :math:`-\vec{z}` direction (0, 0, -1)
  + **alphay** : rotating around the :math:`\vec{y}` direction (0, 1, 0)
  + **alphax** : rotating around the :math:`\vec{x}` direction (1, 0, 0)

+ 2 axes for the detector

  + **delta** : rotation around the :math:`-\vec{z}` direction (0, 0, -1)
  + **gamma** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

PseudoAxes
**********

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode : **lifting_detector_thetah**

  + Axes : **"thetah"**, **"delta"**, **"gamma"**
  + Parameters : No Parameters

q2
==

PseudoAxis provided : **q**, **alpha**

where **q** is :math:`|\vec{Q}| = \frac{2 \tau}{\lambda} \sin{\theta}`
and **alpha** is the azimuth of :math:`\vec{Q}` in the ``yz``
plan. The origin of this angles is the :math:`\vec{y}` vector, and the
positive rotation along :math:`\vec{x}`

+ mode : **q2**

  + Axes : **"gamma"**, **"delta"**
  + Parameters : no parameter

qper_qpar
=========

PseudoAxis provided : **qper**, **qpar**

where **qper** and **qpar** are the perpendicular and parallel
composants of the :math:`|\vec{Q}|` vector projected respectively to
the surface vector :math:`\vec{n}` of coordinates :math:`x`,
:math:`y`, :math:`z` of the sample expressed in the sample
referential. The default value for :math:`n` is (0, 1, 0)

+ mode : **qper_qpar**

  + Axes : **"delta"**, **"gamma"**
  + Parameters : **"x"**, **"y"**, **"z"**
