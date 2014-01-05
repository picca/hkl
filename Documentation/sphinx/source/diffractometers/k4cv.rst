Kappa 4 circles vertical
########################

.. figure:: ../../../figures/k4cv.png
   :align: center
   :width: 8cm

   Schematic view of the diffractometer.

Geometry
********

For this geometry there is a special parameters called :math:`\alpha` which is the
angle between the kappa rotation axis and the  :math:`\vec{y}` direction.

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 3 axes for the sample

  + **komega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)
  + **kappa** : rotating around the :math:`\vec{x}` direction (0, :math:`-\cos\alpha`, :math:`-\sin\alpha`)
  + **kphi** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 1 axis for the detector

  + **tth** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

PseudoAxes
**********

eulerians
=========

PseudoAxes provides : **"omega"**, **"chi"**, **"phi"**

+ mode **eulerians**

  + Axes : **komega**, **kappa**, **kphi**
  + Parameters : **"solution"**

  When you compute the eulerians values from the kappa axes values,
  there is two possibilities, so the **"solution"** parameter when set
  0 or 1 allow to switch from one solution to the other.

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode **bissector**

  + Axes: **komega**, **kappa**, **kphi**, **tth**
  + Parameters : No parameter

  This mode add the bissector constrain ``tth = 2 * omega``. In this
  mode the equivalent eulerian **chi** circle containt the vector of
  diffusion :math:`\vec{Q}`. So it is easy to know the orientation of
  the hkl plan.

+ mode **constant_omega**

  + Axes : **"komega"**, **"kappa"**, **"kphi"**, **"tth"**
  + Parameters : **"omega"**

  This mode do not move the equivalent eulerian **omega** axis, fixed
  by the parameter of the mode.

+ mode **constant_chi**

  + Axes : **"komega"**, **"kappa"**, **"kphi"**, **"tth"**
  + Parameters : **"chi"**

  This mode do not move the equivalent eulerian **chi** axis fixed by
  the parameter of the mode.

+ mode **constant_phi**

  + Axes related : **"komega"**, **"kappa"**, **"kphi"**, **"tth"**
  + Parameters : **"phi"**

  This mode do not move the equivalent eulerian **phi** axis fixed by
  the parameter of the mode.

+ mode **double_diffraction**

  + Axes : **komega**, **kappa**, **kphi**, **tth**
  + Parameters : **h2**, **k2**, **l2**

  This mode put a second hkl vector (**h2**, **k2**, **l2**) in Bragg
  condition.  This is usefull sometimes when you want to explore two
  bragg peaks without moving your sample.

+ mode **psi_constant**

  + Axes :  **komega**, **kappa**, **kphi**, **tth**
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

  + Axes : **komega**, **kappa**, **kphi**, **tth**
  + Parameters : **h1**, **k1**, **l1**

q
=

PseudoAxis provided : **q**

where **q** is :math:`|\vec{Q}| = \frac{2 \tau}{\lambda} \sin{\theta}`

+ mode : **q**

  + Axes : **"tth"**
  + Parameters : no parameter
