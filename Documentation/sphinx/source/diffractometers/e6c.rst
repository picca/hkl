Eulerian 6 circles
##################

.. figure:: ../../../figures/4S+2D.png
   :align: center
   :width: 8cm

   Schematic view of the diffractometer.

Geometry
********

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 4 axes for the sample

  + **mu** : rotating around the :math:`\vec{z}` direction (0, 0, 1)
  + **omega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)
  + **chi** : rotating around the :math:`\vec{x}` direction (1, 0, 0)
  + **phi** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 2 axes for the detector

  + **gamma** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **delta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

PseudoAxes
**********

hkl
===

PseudoAxes provided : **h**, **k** and **l**

+ mode **bissector_vertical**

  + Axes: **omega**, **chi**, **phi**, **delta**
  + Parameters : No parameter

  This mode add the bissector constrain ``delta = 2 * omega``. In this
  mode the **chi** circle containt the vector of diffusion
  :math:`\vec{Q}`. So it is easy to know the orientation of the hkl
  plan.

+ mode **constant_omega_vertical**

  + Axes: **"chi"**, **"phi"**, **"delta"**
  + Parameters : no parameter

  This mode do not move the **omega** axis.

+ mode **constant_chi_vertical**

  + Axes: **"omega"**, **"phi"**, **"delta"**
  + Parameters : no parameter

  This mode do not move the **chi** axis.

+ mode **constant_phi_vertical**

  + Axes : **"omega"**, **"chi"**, **"delta"**
  + Parameters : no parameter

  This mode do not move the **phi** axis.

+ mode : **lifting_detector_phi**

  + Axes : **"phi"**, **"gamma"**, **"delta"**
  + Parameters : No Parameters

+ mode : **lifting_detector_omega**

  + Axes : **"omega"**, **"gamma"**, **"delta"**
  + Parameters : No Parameters

+ mode : **lifting_detector_mu**

  + Axes : **"mu"**, **"gamma"**, **"delta"**
  + Parameters : No Parameters

+ mode : **double_diffraction vertical**

  + Axes : **"omega"**, **"chi"**, **"phi"**, **"delta"**
  + Parameters : **h2**, **k2**, **l2**

  This mode put a second hkl vector (**h2**, **k2**, **l2**) in Bragg
  condition.  This is usefull sometimes when you want to explore two
  bragg peaks without moving your sample.

+ mode : **bissector_horizontal**

  + Axes : **"mu"**, **"omega"**, **"chi"**, **"phi"**, **"gamma"**
  + Parameters : No parameters

+ mode : **double_diffraction_horizontal**

  + Axes : **"mu"**, **"omega"**, **"chi"**, **phi**, **"gamma"**
  + Parameters : **h2**, **k2**, **l2**

  This mode put a second hkl vector (**h2**, **k2**, **l2**) in Bragg
  condition.  This is usefull sometimes when you want to explore two
  bragg peaks without moving your sample.

+ mode : **psi_constant_vertical**

  + Axes : **"omega"**, **"chi"**, **phi**, **"delta"**
  + Parameters : **h2**, **k2**, **l2**, **psi**

  This mode allow to fix the value of the pseudo axis **psi** at a
  constant value when you move around an **h**, **k** , **l**
  position. The (**h2**, **k2**, **l2**) vector is used as a reference
  for the computation of the **psi** pseudo axis value.

  You can retrive and ``freeze`` the current value of the **psi**
  pseudo axis value into the **psi** parameter when you initialize the
  mode. But you can also write directly the value of the desired
  **psi** parameter.

+ mode : **psi_constant_horizontal**

  + Axes : **"omega"**, **"chi"**, **phi**, **"gamma"**
  + Parameters : **h2**, **k2**, **l2**, **psi**

  This mode allow to fix the value of the pseudo axis **psi** at a
  constant value when you move around an **h**, **k** , **l**
  position. The (**h2**, **k2**, **l2**) vector is used as a reference
  for the computation of the **psi** pseudo axis value.

  You can retrive and ``freeze`` the current value of the **psi**
  pseudo axis value into the **psi** parameter when you initialize the
  mode. But you can also write directly the value of the desired
  **psi** parameter.

+ mode **constant_mu_horizontal**

  + Axes : **"chi"**, **"phi"**, **"gamma"**
  + Parameters : no parameter

  This mode do not move the **mu** axis.

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
