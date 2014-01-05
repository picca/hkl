Eulerian 4 circles
##################

.. figure:: ../../../figures/3S+1D.png
   :align: center
   :width: 8cm

   Schematic view of the diffractometer.

Geometry
********

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 3 axes for the sample

  + **omega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)
  + **chi** : rotating around the :math:`\vec{x}` direction (1, 0, 0)
  + **phi** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 1 axis for the detector

  + **tth** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

Pseudo axes
***********

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode **bissector**

  + Axes: **omega**, **chi**, **phi**, **tth**
  + Parameters : No parameter

  This mode add the bissector constrain ``tth = 2 * omega``. In this
  mode the **chi** circle containt the vector of diffusion
  :math:`\vec{Q}`. So it is easy to know the orientation of the hkl
  plan.

+ mode **constant_omega**

  + Axes : **chi**, **phi**, **tth**
  + Parameters : No parameter

  This mode do not move the current **omega** axis.

+ mode **constant_chi**

  + Axes :  **omega**, **phi**, **tth**
  + Parameters : No parameter

  This mode do not move the current **chi** axis.

+ mode **constant_phi**

  + Axes related : **omega**, **chi**, **tth**
  + Parameters : No parameter

  This mode do not move the current **phi** axis.

+ mode **double_diffraction**

  + Axes : **omega**, **chi**, **phi**, **tth**
  + Parameters : **h2**, **k2**, **l2**

  This mode put a second hkl vector (**h2**, **k2**, **l2**) in
  Bragg condition.  This is usefull sometimes when you want to explore
  two bragg peaks without moving your sample.

+ mode **psi_constant**

  + Axes :  **omega**, **chi**, **phi**, **tth**
  + Parameters : **h2**, **k2**, **l2**, **psi**

  This mode allow to fix the value of the pseudo axis **psi** at a
  constant value when you move around an **h**, **k** , **l**
  position. The (**h2**, **k2**, **l2**) vector is used as a reference
  for the computation of the **psi** pseudo axis value.

  You can retrive and ``freeze`` the current value of the **psi**
  pseudo axis value into the **psi** parameter when you initialize the
  mode. But you can also write directly the value of the desired
  **psi** parameter.

psi
===

PseudoAxis provided : **psi**

+ mode **psi**

  + Axes : **omega**, **chi**, **phi**, **tth**
  + Parameters : **h1**, **k1**, **l1**

q
=

PseudoAxis provided : **q**

where **q** is :math:`|\vec{Q}| = \frac{2 \tau}{\lambda} \sin{\theta}`

+ mode : **q**

  + Axes : **"tth"**
  + Parameters : no parameter

