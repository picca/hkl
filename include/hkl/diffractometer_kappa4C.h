#ifndef _DIFFRACTOMETER_KAPPA4C_H_
#define _DIFFRACTOMETER_KAPPA4C_H_

#include "diffractometer.h"
#include "geometry_kappa4C.h"
#include "pseudoaxeengine_kappa4C.h"

/**
 * @page Diffractometer_kappa_4C Diffractometer kappa 4 Circles.
 *
 * @section Geometry_K4C Geometry.
 *
 * Nous allons nous inspirer du modèle de Busin et Levy pour décrire notre diffractomètre.
 * Les sens de rotation sont respectés mais le repère directe est choisi de façon à correspondre
 * au repère de laboratoire de la ligne CRYSTAL du synchrotron Soleil.
 * Les photons-X se propagent suivant le vecteur @f$\vec{x}@f$ et la direction verticale est suivant
 * le vecteur @f$\vec{z}@f$.
 * Ce diffractomètre est de type verticale (le vecteur de diffusion @f$\vec{Q}@f$ est dans le plan \a xOz).
 * Les axes permettant de décrire la configuration du diffractomètre sont les suivants:
 *  - échantillon
 *    - "komega"
 *    - "kappa"
 *    - "kphi"
 *  - Détecteur
 *    - "2theta"
 *
 * @section pseudoaxes_K4C Pseudo-axes
 *
 * Il est intéressant de piloter un diffractomètre kappa de la même façon qu'un diffractomètre eulérien.
 * Nous allons donc présenter ces pseudo-axes.
 *
 * @subsection pseudoaxes_K4C_eulerian Eulériens
 *
 * Pour trouver les relations mathématiques liant ces pseudo-axes aux axes kappa
 * nous alors utiliser les quaternions liés aux différents axes des diffractomètres.
 *  - Kappa.
 *  
 * @f{align*}
 *    && q_{\omega_\kappa} && = &&\cos\frac{\omega_\kappa}{2} && + && 0\times i && + && -\sin\frac{\omega_\kappa}{2} \times j && + && 0\times k && \\
 *    && q_\kappa && = && \cos\frac{\kappa}{2} && + && 0\times i && + && -\sin\frac{\kappa}{2}\cos\alpha \times j && + && -\sin\frac{\kappa}{2}\sin\alpha \times k &&\\
 *    && q_{\phi_\kappa} && = && \cos\frac{\phi_\kappa}{2} && + && 0\times i && + && -\sin\frac{\phi_\kappa}{2} \times j && + && 0 \times k &&
 * @f}
 *  - Eulerien.
 *  
 * @f{align*}
 *    && q_\omega && = &&\cos\frac{\omega}{2} && + && 0\times i && + && -\sin\frac{\omega}{2} \times j && + && 0\times k && \\
 *    && q_\chi && = && \cos\frac{\chi}{2} && + && \sin\frac{\chi}{2}\times i && + && 0 \times j && + && 0 \times k &&\\
 *    && q_\phi && = && \cos\frac{\phi}{2} && + && 0\times i && + && -\sin\frac{\phi}{2} \times j && + && 0 \times k &&
 * @f}
 *
 * On veut que:
 * @f[
 *  q_\omega q_\chi q_\phi = q_{\omega_\kappa} q_\kappa q_{\phi_\kappa} 
 * @f]
 *
 * Cela revient à résoudre le système suivant:
 * @f{align*}
 *  \cos\frac{\chi}{2} \cos\left(\frac{\omega}{2} + \frac{\phi}{2}\right)
 *    & = & \cos\frac{\kappa}{2} \cos\left(\frac{\omega_\kappa}{2} + \frac{\phi_\kappa}{2}\right) - \sin\frac{\kappa}{2} \cos\alpha \sin\left(\frac{\omega_\kappa}{2} + \frac{\phi_\kappa}{2}\right) \\
 *  \sin\frac{\chi}{2} \cos\left(\frac{\omega}{2} - \frac{\phi}{2}\right)
 *    & = & \sin\frac{\kappa}{2} \sin\alpha \sin\left(\frac{\omega_\kappa}{2} - \frac{\phi_\kappa}{2}\right) \\
 *  -\cos\frac{\chi}{2} \sin\left(\frac{\omega}{2} + \frac{\phi}{2}\right)
 *    & = & -\cos\frac{\kappa}{2} \sin\left(\frac{\omega_\kappa}{2} + \frac{\phi_\kappa}{2}\right) - \sin\frac{\kappa}{2} \cos\alpha \cos\left(\frac{\omega_\kappa}{2} + \frac{\phi_\kappa}{2}\right) \\
 *  \sin\frac{\chi}{2} \sin\left(\frac{\omega}{2} - \frac{\phi}{2}\right)
 *    & = & -\sin\frac{\kappa}{2} \sin\alpha \cos\left(\frac{\omega_\kappa}{2} - \frac{\phi_\kappa}{2}\right)
 * @f}
 *
 * @subsubsection pseudoaxes_K4C_eulerian2kappa Eulérien vers kappa.
 *
 * On trouve deux solutions.
 * @f{align*}
 *  \omega_\kappa & = \omega + \arcsin\left(\frac{\tan\frac{\chi}{2}}{\tan\alpha}\right) - \frac{\pi}{2} 
 *    && \mbox{}
 *    & \omega_\kappa & = \omega - \arcsin\left(\frac{\tan\frac{\chi}{2}}{\tan\alpha}\right) + \frac{\pi}{2} \\
 *  \kappa & = -2 \arcsin\left(\frac{\sin\frac{\chi}{2}}{\sin\alpha}\right)
 *    && ou
 *    & \kappa & = 2 \arcsin\left(\frac{\sin\frac{\chi}{2}}{\sin\alpha}\right)\\
 *  \phi_\kappa & = \phi + \arcsin\left(\frac{\tan\frac{\chi}{2}}{\tan\alpha}\right) + \frac{\pi}{2}
 *    && \mbox{}
 *    & \phi_\kappa & = \phi - \arcsin\left(\frac{\tan\frac{\chi}{2}}{\tan\alpha}\right) - \frac{\pi}{2}\\
 * @f}
 *
 * @subsubsection pseudoaxes_K4C_kappa2eulerian Kappa vers Eulérien.
 *
 * On à de la même façon deux solutions.
 * @f{align*}
 *  \omega & = \omega_\kappa + \arctan\left( \tan\frac{\kappa}{2} \cos\alpha \right) + \frac{\pi}{2} 
 *    &&
 *    & \omega & = \omega_\kappa + \arctan\left( \tan\frac{\kappa}{2} \cos\alpha \right) - \frac{\pi}{2} \\
 *  \chi & = -2 \arcsin\left( \sin\frac{\kappa}{2} \sin\alpha \right)
 *    && ou
 *    & \chi & = 2 \arcsin\left( \sin\frac{\kappa}{2} \sin\alpha \right)\\
 *  \phi & = \phi_\kappa + \arctan\left( \tan\frac{\kappa}{2} \cos\alpha \right) - \frac{\pi}{2}
 *    &&
 *    & \phi & = \phi_\kappa + \arctan\left( \tan\frac{\kappa}{2} \cos\alpha \right) + \frac{\pi}{2}\\
 * @f}
 *
 */

namespace hkl
  {
  namespace diffractometer
    {
    namespace kappa4C
      {

      /**
       *  @brief This class describes a four-circle Kappa diffractometer.
       * 
       * The 4C Kappa diffractometer can be seen as a 4C eulerian one provided that we use some formula from the
       * MHATT-CAT, Advanced Photon Source, Argonne National Laboratory (
       * <A HREF="http://www.mhatt.aps.anl.gov/~walko/kappa.pdf">MHATT-CATs Newport Kappa Diffractometer</A>
       * written by Donald A. Walko). Other interesting documentation can be found at the 
       * <A HREF="http://www.px.nsls.bnl.gov/kappa.html">Brookhaven National Laboratory</A>
       */
      class Vertical : public DiffractometerTemp<geometry::kappa4C::Vertical>
        {
        public:

          /**
           * @brief Default constructor
           */
          Vertical(double alpha);

          /**
           * @brief Destructor
           *
           * Destructor
           */
          virtual ~Vertical(void);
        };

    } // namespace kappa4C
  } // namespace diffractometer
} // namespace hkl

#endif // _DIFFRACTOMETER_KAPPA4C_H_
