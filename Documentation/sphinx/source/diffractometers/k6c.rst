Kappa 6 circles
###############

.. figure:: ../../../figures/k6c.png
   :align: center
   :width: 8cm

   Schematic view of the diffractometer.

Geometry
********

For this geometry there is a special parameters called :math:`\alpha` which is the
angle between the kappa rotation axis and the  :math:`\vec{y}` direction.

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 4 axes for the sample

  + **mu** : rotating around the :math:`\vec{z}` direction (0, 0, 1)
  + **komega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)
  + **kappa** : rotating around the :math:`\vec{x}` direction (0, :math:`-\cos\alpha`, :math:`-\sin\alpha`)
  + **kphi** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 2 axes for the detector

  + **gamma** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **delta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

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

+ mode **bissector_vertical**

  + Axes: **komega**, **kappa**, **kphi**, **delta**
  + Parameters : No parameter

  This mode add the bissector constrain ``tth = 2 * omega``. In this
  mode the equivalent eulerian **chi** circle containt the vector of
  diffusion :math:`\vec{Q}`. So it is easy to know the orientation of
  the hkl plan.

+ mode **constant_omega_vertical**

  + Axes: **"komega"**, **"kappa"**, **"kphi"**, **"delta"**
  + Parameters : **omega**

  This mode do not move the equivalent eulerian **omega** axis.

+ mode **constant_chi_vertical**

  + Axes: **"komega"**, **"kappa"**, **"kphi"**, **"delta"**
  + Parameters : **chi**

  This mode do not move the equivalent eulerian **chi** axis.

+ mode **constant_phi_vertical**

  + Axes : **"komega"**, **"kappa"**, **"kphi"**, **"delta"**
  + Parameters : **phi**

  This mode do not move the equivalent eulerian **phi** axis.

+ mode : **lifting_detector_kphi**

  + Axes : **"kphi"**, **"gamma"**, **"delta"**
  + Parameters : No Parameters

+ mode : **lifting_detector_mu**

  + Axes : **"mu"**, **"gamma"**, **"delta"**
  + Parameters : No Parameters

+ mode : **double_diffraction vertical**

  + Axes : **"komega"**, **"kappa"**, **"kphi"**, **"delta"**
  + Parameters : **h2**, **k2**, **l2**

  This mode put a second hkl vector (**h2**, **k2**, **l2**) in
  Bragg condition.  This is usefull sometimes when you want to explore
  two bragg peaks without moving your sample.

+ mode : **bissector_horizontal**

  + Axes : **"mu"**, **"komega"**, **"kappa"**, **"kphi"**, **"gamma"**
  + Parameters : No parameters

+ mode : **constant_phi_horizontal**

  + Axes : **"mu"**, **"komega"**, **"kappa"**, **"kphi"**, **"gamma"**
  + Parameters : **phi**

+ mode : **horizontal kphi constant**

  + Axes :  **"mu"**, **"komega"**, **"kappa"**, **"gamma"**
  + Parameters : no parameters

+ mode : **double_diffraction_horizontal**

  + Axes : **"mu"**, **"komega"**, **"kappa"**, **kphi**, **"gamma"**
  + Parameters : **h2**, **k2**, **l2**

  This mode put a second hkl vector (**h2**, **k2**, **l2**) in
  Bragg condition.  This is usefull sometimes when you want to explore
  two bragg peaks without moving your sample.

+ mode : **psi_constant_vertical**

  + Axes : **"komega"**, **"kappa"**, **kphi**, **"delta"**
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

+ mode **psi_vertical**

  + Axes : **komega**, **kappa**, **kphi**, **delta**
  + Parameters : **h1**, **k1**, **l1**

  The (**h1**, **k1**, **l1**) vector is used as a reference for the
  computation of the **psi** pseudo axis value.

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
