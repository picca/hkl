SOLEIL SIXS MED1+2
##################

Geometry
********

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 2 axes for the sample

  + **pitch** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)
  + **mu** : rotation around the :math:`\vec{z}` direction (0, 0, 1)

+ 3 axis for the detector

  + **pitch** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)
  + **gamma** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **delta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

PseudoAxes
**********

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode **pitch_fixed**

  + Axes : **mu**, **gamma**, **delta**
  + Parameters : No parameter

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
