.. _pseudo:

PseudoAxes
##########

This section describe the calculations done by the library for the
different kind of pseudo axes.

Eulerians to Kappa angles
*************************

1st solution

.. math::
   :nowrap:

   \begin{eqnarray*}
	\kappa_\omega & = & \omega - p + \frac{\pi}{2} \\
	\kappa & = & 2 \arcsin\left(\frac{\sin\frac{\chi}{2}}{\sin\alpha}\right) \\
	\kappa_\phi & = &  \phi - p - \frac{\pi}{2}
   \end{eqnarray*}

or 2nd one

.. math::
   :nowrap:

   \begin{eqnarray*}
	\kappa_\omega & = & \omega - p - \frac{\pi}{2} \\
	\kappa & = & -2 \arcsin\left(\frac{\sin\frac{\chi}{2}}{\sin\alpha}\right) \\
	\kappa_\phi & = &  \phi - p + \frac{\pi}{2}
   \end{eqnarray*}

where

.. math:: 
   p = \arcsin\left(\frac{\tan\frac{\chi}{2}}{\tan\alpha}\right);

and :math:`\alpha` is the angle of the kappa axis with the :math:`\vec{y}` axis.

Kappa to Eulerians angles
*************************

1st solution

.. math::
   :nowrap:

   \begin{eqnarray*}
	\omega & = & \kappa_\omega + p - \frac{\pi}{2} \\
	\chi   & = & 2 \arcsin\left(\sin\frac{\kappa}{2} \sin\alpha\right) \\
	\phi   & = & \kappa_\phi + p + \frac{\pi}{2}
   \end{eqnarray*}

or 2nd one

.. math::
   :nowrap:

   \begin{eqnarray*}
	\omega & = & \kappa_\omega + p + \frac{\pi}{2} \\
	\chi   & = & -2 \arcsin\left(\sin\frac{\kappa}{2} \sin\alpha\right) \\
	\phi   & = & \kappa_\phi + p - \frac{\pi}{2}
   \end{eqnarray*}

where

.. math::
   p = \arctan\left(\tan\frac{\kappa}{2} \cos\alpha\right)


.. figure:: ../../figures/e2k_1.png
   :align: center
   :width: 8cm

   :math:`\omega = 0`,  :math:`\chi = 0`, :math:`\phi = 0`, 1st solution

.. figure:: ../../figures/e2k_2.png
   :align: center
   :width: 8cm

   :math:`\omega = 0`, :math:`\chi = 0`, :math:`\phi = 0`, 2nd solution

.. figure:: ../../figures/e2k_3.png
   :align: center
   :width: 8cm

   :math:`\omega = 0`, :math:`\chi = 90`, :math:`\phi = 0`, 1st solution

.. figure:: ../../figures/e2k_4.png
   :align: center
   :width: 8cm

   :math:`\omega = 0`, :math:`\chi = 90`, :math:`\phi = 0`, 2nd solution

Qper and Qpar
*************

this pseudo axis engine compute the perpendicular
(:math:`\left|\left|\vec{q_\text{per}}\right|\right|`) and parallel
(:math:`\left|\left|\vec{q_\text{par}}\right|\right|`) contribution of
:math:`\vec{Q}` relatively to the surface of the sample defined by the
:math:`\vec{n}` vector.

.. math::
   :nowrap:

   \begin{eqnarray*}
   \vec{q} & = & \vec{k_\text{f}} - \vec{k_\text{i}} \\
   \vec{q} & = & \vec{q_\text{per}} + \vec{q_\text{par}} \\
   \vec{q_\text{per}} & = & \frac{\vec{q} \cdot \vec{n}}{\left|\left|\vec{n}\right|\right|} \frac{\vec{n}}{\left|\left|\vec{n}\right|\right|}
   \end{eqnarray*}

