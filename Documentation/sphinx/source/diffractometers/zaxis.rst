Z-Axis
######

Geometry
********

For this geometry the **mu** axis is common to the sample and the detector.

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 2 axes for the sample

  + **mu** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **omega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 3 axis for the detector

  + **mu** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **delta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)
  + **gamma** : rotation around the :math:`\vec{z}` direction (0, 0, 1)

PseudoAxes
**********

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode **zaxis**

  + Axes : **omega**, **delta**, **gamma**
  + Parameters : No parameter

+ mode **reflectivity**

  + Axes : **mu**, **omega**, **delta**, **gamma**
  + Parameters : No parameter

  This mode add the reflectivity constraint ``mu = gamma``. The
  incomming beam angle and the outgoing beam angle are equals.

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
