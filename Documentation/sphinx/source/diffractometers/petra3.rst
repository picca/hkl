PETRA3 P09 EH2
##############

Geometry
********

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 4 axes for the sample

  + **mu** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)
  + **omega** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **chi** : rotating around the :math:`\vec{x}` direction (1, 0, 0)
  + **phi** : rotating around the :math:`\vec{z}` direction (0, 0, 1)

+ 3 axis for the detector

  + **mu** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)
  + **delta** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **gamma** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

PseudoAxes
**********

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode **zaxis + alpha-fixed**

  + Axes : **"omega"**, **"gamma"**, **"delta"**
  + Parameters : No parameter

+ mode **zaxis + beta-fixed**

  + Axes : **"mu"**, **"omega"**, **"delta"**
  + Parameters : No parameter

+ mode **zaxis + alpha=beta**

  + Axes : **"mu"**, **"omega"**, **"gamma"**, **"delta"**
  + Parameters : No parameter

  This mode add the ``mu = gamma`` constrain.

+ mode **4-circles bissector_horizontal**

  + Axes: **"omega"**, **"chi"**, **"phi"**, **"delta"**
  + Parameters : No parameter

  This mode add the bissector constrain ``delta = 2 * omega``. In this
  mode the eulerian **"chi"** circle containt the vector of diffusion
  :math:`\vec{Q}`. So it is easy to know the orientation of the hkl
  plan.

+ mode **4-circles constant_omega_horizontal**

  + Axes: **"chi"**, **"phi"**, **"delta"**
  + Parameters : No parameter

  This mode do not move the **"omega"** axis.


+ mode **4-circles constant_chi_horizontal**

  + Axes: **"omega"**, **"phi"**, **"delta"**
  + Parameters : No parameter

  This mode do not move the **"chi"** axis.

+ mode **4-circles constant_phi_horizontal**

  + Axes: **"omega"**, **"chi"**, **"delta"**
  + Parameters : No parameter

  This mode do not move the **"phi"** axis.
