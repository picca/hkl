SOLEIL SIXS MED2+2
##################

Geometry
********

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 3 axes for the sample

  + **beta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)
  + **mu** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **omega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 3 axis for the detector

  + **beta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)
  + **gamma** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **delta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

PseudoAxes
**********

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode **mu_fixed**

  + Axes : **omega**, **gamma**, **delta**
  + Parameters : No parameter

+ mode **reflectivity**

  + Axes : **mu**, **omega**, **gamma**, **delta**
  + Parameters : No parameter

  This mode add the reflectivity constraint ``2*mu = gamma``. The
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
