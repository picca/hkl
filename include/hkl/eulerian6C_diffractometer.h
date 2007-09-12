#ifndef _EULERIAN6C_DIFFRACTOMETER_H
#define _EULERIAN6C_DIFFRACTOMETER_H


#include "diffractometer.h"
#include "eulerian6C_mode.h"
#include "eulerian6C_pseudoaxeengine.h"
#include "eulerian6C_geometry.h"

namespace hkl
  {

  namespace eulerian6C
    {

    /**
     * \page Diffractometer_eulerian_6C Diffractometer Eulerian 6C.
     *
     * \section geometrie Geometrie
     *
     * Nous allons nous inspirer du modèle de You pour notre diffractomètre (fig. [cap:4S+2D]) ici présenté tous
     * les angles mis à zéro.
     * Les rayons-X arrivent suivant le vecteur \f$ \vec{x} \f$ (le repère est différent de celui de You).
     *
     * \section pseudomoteurs Pseudomoteurs
     *
     * Le principe des calcules de You est d'exprimer dans le repère du laboratoire le vecteur diffusion \f$ \vec{Q} \f$
     * de deux façons différentes.
     * Une première en utilisant les angles du goniomètre 4S puis une à partir des angles du détecteur 2D et de la connaissance
     * des coordonnées du vecteur incident.
     * En égalant les deux expressions, il obtient un système d'équation à 6 inconnus mais seulement 3 équations.
     * Pour être à même de résoudre le système il faut fixer des contraintes supplémentaire.
     * C'est ce que l'on appel les modes de fonctionnement du diffractomètre.
     * Il est commode de définir d'autres angles que ceux du diffractomètre relativement à des vecteurs
     * caractéristiques tel que le vecteur de diffusion \f$ \vec{Q} \f$  ou un vecteur pointant dans une direction particulière du cristal \f$ \vec{n} \f$.
     * Cette direction peut-être soit lié à la cristallographie du cristal soit à sa forme (une normale à une face).
     * La figure [cap:Pseudo-Angles-liés] représente les angles liés au vecteur de diffusion et à ce vecteur de référence.
     * Tout d'abord  \f$ \theta \f$ (angle entre \f$ \vec{Q} \f$ et le plan  yz) et qui correspond à l'angle de Bragg.
     * \f$ \vartheta \f$ qui est l'angle azimutal que fait la projection de \f$ \vec{Q} \f$ sur le plan \a yz et la direction  \a +y (fig [cap:Pseudo-Angles-liés]a).
     * Il y a ensuite les angles  \f$ \alpha \f$ et \f$ \varphi \f$ définits comme précédemment mais pour le vecteur
     * de référence \f$ \vec{n} \f$ (fig [cap:Pseudo-Angles-liés]b).
     * Et finalement les angles \f$ \tau \f$ (angle entDiffractometer_re \f$ \vec{Q} \f$ et \f$ \vec{n} \f$) et \f$ \psi \f$ qui
     * correspond à la rotation de \f$ \vec{n} \f$ autour du vecteur de diffusion \f$ \vec{Q} \f$ (fig [cap:Pseudo-Angles-liés]c).
     * L'origine de cet angle \f$ \psi \f$ est prise à zéro lorsque le vecteur \f$ \vec{n} \f$ est dans le plan de
     * diffraction (plan contenant \f$ \vec{Q} \f$ et \f$ \vec{k_{i}} \f$) (fig [cap:Pseudo-Angles-liés]d).
     * Il est alors possible d'exprimer ces pseudos angles en fonction des angles physique du diffractomètre.
     */
    class Diffractometer : public hkl::DiffractometerTemp<hkl::eulerian6C::Geometry>
      {
      public:
        Diffractometer();

        virtual ~Diffractometer();

      };

  } // namespace hkl::eulerian6C

} // namespace hkl
#endif
