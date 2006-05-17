#ifndef _DIFFRACTOMETER_TWOC_H
#define _DIFFRACTOMETER_TWOC_H

#include "diffractometer.h"

/**
 *  \page Diffractometer_2C Diffractometer 2C.
 *
 *  \section geometrie Geometrie
 *
 * Ce diffractometre est un diffractomètre deux cercles.
 * Les sens de rotation sont respectés mais le repère directe est choisi de façon à correspondre
 * au repère de laboratoire de la ligne CRYSTAL du synchrotron Soleil.
 * Les photons-X se propagent suivant le vecteur \f$ \vec{x} \f$ et la direction verticale est suivant
 * le vecteur \f$ \vec{z} \f$.
 * Ce diffractomètre est de type verticale (le vecteur de diffusion \f$ \vec{Q} \f$ est dans le plan \a xOz).
 * Les angles permettant de décrire la configuration du diffractomètre sont présentés sur la figure.
 *
 * \section pseudomotors Pseudo-moteurs
 *
 * \section modes Modes
 *
 * \subsection Symetric Symetric
 *
 * Dans ce mode on choisit d'avoir :
 * 
 * \f{eqnarray*}
 *    \mbox{"omega"} & = & \theta \\
 *    \mbox{"2theta"} & = & 2\theta
 * \f]
 * 
 * \subsection Fix_Incidence Fix Incidence
 *
 * Ce mode consiste à laisser \f$ \omega \f$ libre et à ne bouger que \f$ 2\theta \f$ :
 * "2theta" = \f$2\theta\f$
 */

namespace hkl {
    namespace diffractometer {
        namespace twoC {

            /**
             * The 2-circle vertical diffractometer.
             */
            class Vertical : public Diffractometer
            {
            public:

              Vertical(void); //!< Default constructor.

              virtual ~Vertical(void);  //!< Destructor destructor.
            };

        } // namepsace twoC
    } // namespace diffractometer
} // namespace hkl

#endif // _DIFFRACTOMETER_TWOC_H
