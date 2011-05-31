.. _diffractometers:

Diffractometer
==============

Eulerian 4 circles
------------------

Geometries
``````````

Eulerian 4 circles vertical

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 3 axes for the sample

  + **omega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)
  + **chi** : rotating around the :math:`\vec{x}` direction (1, 0, 0)
  + **phi** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 1 axis for the detector

  + **tth** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

Soleil Mars Beamline

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 3 axes for the sample

  + **omega** : rotating around the :math:`\vec{z}` direction (0, -1, 0)
  + **chi** : rotating around the :math:`\vec{x}` direction (-1, 0, 0)
  + **phi** : rotating around the :math:`\vec{z}` direction (0, 0, 1)

+ 1 axis for the detector

  + **tth** : rotation around the :math:`\vec{z}` direction (0, -1, 0)

Pseudo axes
```````````

Pseudo axis **hkl**

PseudoAxes provided : **h**, **k** and **l**

mode **bissector**

+ Axes: **omega**, **chi**, **phi**, **tth**
+ Parameters : No parameter

This mode add the bissector constrain ``tth = 2 * omega``. In this
mode the **chi** circle containt the vector of diffusion
:math:`\vec{Q}`. So it is easy to know the orientation of the hkl
plan.

mode **constant_omega**

+ Axes : **chi**, **phi**, **tth**
+ Parameters : No parameter

This mode do not move the current **omega** axis.

mode **constant_chi**

+ Axes :  **omega**, **phi**, **tth**
+ Parameters : No parameter

This mode do not move the current **chi** axis.

mode **constant_phi**

+ Axes related : **omega**, **chi**, **tth**
+ Parameters : No parameter

This mode do not move the current **phi** axis.

mode **double_diffraction**

@item Axes : **omega**, **chi**, **phi**, **tth**
@item Parameters : **h2**, **k2**, **l2**

This mode put a second hkl vector (**h2**, **k2**, **l2**) in
Bragg condition.  This is usefull sometimes when you want to explore
two bragg peaks without moving your sample.

mode **psi_constant**

@itemize
@item Axes :  **omega**, **chi**, **phi**, **tth**
@item Parameters : **h2**, **k2**, **l2**, **psi**
@end itemize

This mode allow to fix the value of the pseudo axis **psi** at a constant value when you move
around an **h**, **k** ,**l** position. The (**h2**, **k2**, **l2**) vector is
used as a reference for the computation of the **psi** pseudo axis value.

You can retrive and ``freeze`` the current value of the **psi** pseudo axis value into the
**psi** parameter when you initialize the mode. But you can also write directly the value
of the desired **psi** parameter.

PseudoAxis **psi**

PseudoAxis provided : **psi**

mode **psi**


+ Axes : **omega**, **chi**, **phi**, **tth**
+ Parameters : **h1**, **k1**, **l1**

Eulerian 6 circles
------------------

Geometry
````````

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
``````````

Kappa 4 circles vertical
------------------------

Geometry
````````

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
``````````

Kappa 6 circles
---------------

Geometry
````````
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
``````````

Z-Axis
------

Geometry
````````

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
``````````

PseudoAxes provided : **h**, **k** and **l**

@subsubsection mode **zaxis**

+ Axes : **omega**, **delta**, **gamma**
+ Parameters : No parameter

@subsubsection mode **reflectivity**

+ Axes : **mu**, **omega**, **delta**, **gamma**
+ Parameters : No parameter

This mode add the reflectivity constraint ``mu = gamma``. The
incomming beam angle and the outgoing beam angle are equals.

SOLEIL SIXS MED2+2
------------------

Geometry
````````

+ xrays source fix allong the :math:`\vec{x}` direction (1, 0, 0)
+ 2 axes for the sample

  + **mu** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **omega** : rotating around the :math:`-\vec{y}` direction (0, -1, 0)

+ 3 axis for the detector

  + **gamma** : rotation around the :math:`\vec{z}` direction (0, 0, 1)
  + **delta** : rotation around the :math:`-\vec{y}` direction (0, -1, 0)

PseudoAxes
``````````

@subsection Pseudo axis **hkl**

PseudoAxes provided : **h**, **k** and **l**

@subsubsection mode @samp{mu_fixed}

+ Axes : **omega**, **gamma**, **delta**
+ Parameters : No parameter
