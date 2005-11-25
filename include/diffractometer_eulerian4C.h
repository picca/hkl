#ifndef _DIFFRACTOMETER_EULERIAN4C_H_
#define _DIFFRACTOMETER_EULERIAN4C_H_

#include "diffractometer.h"
#include "geometry_eulerian4C.h"
#include "pseudoaxe_eulerian4C.h"
#include "mode_eulerian4C.h"

/**
 *  \page Diffractometer_eulerian_4C Diffractometer Eulerian 4C.
 *
 *  \section geometrie Geometrie
 *
 * Nous allons nous inspirer du modèle de Busin et Levy pour décrire notre diffractomètre.
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
 * \subsection Bissector Bissector
 *
 * Dans ce mode on choisit d'avoir :
 * 
 * \f[
 *    \omega = \theta
 * \f]
 * 
 * L'equation fondamentale s'ecrit alors simplement:
 * 
 * \f{eqnarray*}
 *    h_\phi & = & 2 k_i \sin\theta \cos\chi \sin\phi \\
 *    k_\phi & = & 2 k_i \sin\theta \sin\chi \\
 *    l_\phi & = & 2 k_i \sin\theta \cos\chi \cos\phi
 * \f}
 * 
 * On a:
 * 
 * \f[
 *    h^2_\phi + k^2_\phi + l^2_\phi = 4 k_i \sin^2\theta
 * \f]
 * 
 * où \f$ k_{i} = \frac{\tau}{\lambda} \f$.
 * On peut donc écrire:
 * 
 * \f[
 *    \left| \sin\theta \right| = \frac{\sqrt{h^2_\phi + k^2_\phi + l^2_\phi}}{2 k_i}
 * \f]
 * 
 * Il faut donc enviseager les deux possibilité selon que \f$ \theta \f$ est positif ou bien négatif.
 *
 * \subsection Mode_Delta_Theta Delta Theta
 *
 * Ce mode consiste à décaler \f$ \omega \f$ par rapport à \f$ \theta \f$ d'une valeur constante \a C :
 * \f[
 *    \omega = \theta + C
 * \f]
 * 
 * Le système s'écrit alors comme suit:
 *
 * \f{eqnarray*}
 *    h_\phi & = & 2 k_i \sin\theta \left( \cos C \cos\chi \sin\phi + \sin C \cos\phi \right) \\
 *    k_\phi & = & 2 k_i \sin\theta \cos C \sin\chi \\
 *    l_\phi & = & 2 k_i \sin\theta \left( \cos C \cos\chi \cos\phi - \sin C \sin\phi \right)
 * \f}
 *
 * \subsection Mode_omega_constant Omega constant.
 *
 * Dans ce mode on choisit de garder \f$ \omega \f$ toujours constant:
 * \f[
 *    \omega = C
 * \f]
 *
 * \f{eqnarray*}
 *    h_\phi & = & 2 k_i \sin\theta \left( \cos(C-\theta) \cos\chi \sin\phi + \sin(C-\theta) \cos\phi \right) \\
 *    k_\phi & = & 2 k_i \sin\theta \cos(C-\theta) \sin\chi \\
 *    l_\phi & = & 2 k_i \sin\theta \left( \cos(C-\theta) \cos\chi \cos\phi - \sin(C-\theta) \sin\phi \right)
 * \f}
 *
 * \subsection Mode_chi_constant Chi constant.
 *
 * Dans ce mode on choisit de garder \f$ \chi \f$ toujours constant:
 * \f[  
 *  \chi = C
 * \f]
 *
 * \f{eqnarray*}
 *    h_\phi & = & 2 k_i \sin\theta \left[ \cos(\omega - \theta) \cos C \sin\phi + \sin(\omega-\theta) \cos\phi \right] \\
 *    k_\phi & = & 2 k_i \sin\theta \cos(\omega - \theta) \sin C \\
 *    l_\phi & = & 2 k_i \sin\theta \left[ \cos(\omega - \theta) \cos C \cos\phi - \sin(\omega - \theta) \sin\phi \right]
 * \f}
 *
 * \subsection Mode_phi_constant Phi constant.
 *
 * Dans ce mode on choisit de garder \f$ \phi \f$ toujours constant:
 * \f[  
 *  \phi = C
 * \f]
 *
 * \f{eqnarray*}
 *    h_\phi & = & 2 k_i \sin\theta \left[ \cos(\omega - \theta) \cos\chi \sin C + \sin(\omega-\theta) \cos C \right] \\
 *    k_\phi & = & 2 k_i \sin\theta \cos(\omega - \theta) \sin\chi \\
 *    l_\phi & = & 2 k_i \sin\theta \left[ \cos(\omega - \theta) \cos\chi \cos C - \sin(\omega - \theta) \sin C \right]
 * \f}
 */

 namespace hkl {
   namespace diffractometer {

     /**
      * The eulerian 4-circle diffractometer.
      * William R. Busing and Henri A. Levy "Angle calculation for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
      * <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
      */
     class Eulerian4C : public Diffractometer
     {
       public:

         /**
          * @brief Default constructor.
          * @return a new diffractometer_Eulerian4C diffractometer.
          *
          * Default constructor.
          */
         Eulerian4C(void);

         /**
          * @brief Destructor
          *
          * Destructor
          */
         virtual ~Eulerian4C(void);
     };

   } // namespace diffractometer
 } // namespace hkl

#endif // _DIFFRACTOMETER_EULERIAN4C_H_
