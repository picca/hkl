#ifndef _DIFFRACTOMETER_H_
#define _DIFFRACTOMETER_H_

#include "geometry.h"
#include "samplelist.h"
#include "modelist.h"
#include "pseudoaxeenginelist.h"
#include "affinementlist.h"

using namespace std;

/**
 *
 * \mainpage
 *
 * L'objectif de cette librairie est de mêtre à disposition l'ensemble des outils permettant de piloter
 * un diffractomètre. L'ensemble des calcules présents dans cette librairie sont basés sur une équation
 * fondamentale.
 *
 * @ref Diffractometer
 * 
 * @ref Diffractometer_eulerian_4C
 *
 * @ref Diffractometer_eulerian_6C
 *
 * @ref Diffractometer_kappa_4C
 */

/**
 * @page Diffractometer Généralité.
 *
 * \section Equation_fondamentales Equations fondamentales
 * 
 * Le problème que nous devons résoudre est de calculer pour une famille de plan (h,k,l) donné,
 * les angles de rotation du diffractomètre qui permettent de le mettre en condition de diffraction.
 * Il faut donc exprimer les relations mathématiques qui lient les différents angles entre eux lorsque
 * la condition de Bragg est vérifiée. L'équation fondamentale est la suivante:
 * \f[
 *    \left( \prod_i S_i \right) \cdot U \cdot B \cdot \vec{h} = \left( \prod_i D_i - I \right)
 * \f]
 * \f[
 *    R \cdot U \cdot B \cdot \vec{h} = \vec{Q}
 * \f]
 * où \f$ \vec{h} \f$ est le vecteur (h,k,l) , \f$ \vec{k_i} \f$ est le vecteur incident,  \f$ S_i \f$ les
 * matrices de rotations des mouvements liés à l'échantillon, \f$ D_j \f$  les matrices de rotation
 * des mouvements liés au détecteur,
 * \a I  la matrice identité, \a U  la matrice d'orientation du cristal par rapport au repère de l'axe
 * sur lequel ce dernier est monté et \a B  la matrice de passage d'un repère non orthonormé
 * (celui du crystal réciproque) à un repère orthonormé. 
 * 
 * L'équation fondamentale nous permet d'écrire:
 * \f[
 *    U \cdot B \cdot \vec{h} = \tilde{R} \cdot \vec{Q}.
 * \f]
 * 
 * Cette équation est de 4 ou 6 inconnues pour seulement 3 équations.
 * Il faut donc imposer des contraintes pour résoudre ce système et ainsi orienter le diffractomètre.
 * Ces différentes contraintes définissent les modes de fonctionnement des diffractomètres.
 * 
 * \section Calcule_de_B Calcule de B.
 *
 * Si l'on connaît les paramètres cristallins du cristal étudié, il est très simple de calculer
 * cette matrice \a B :
 * \f[ 
 *    B=\left(
 *        \begin{matrix}
 *          a^{*} & b^{*}\cos\gamma^{*} & c^{*}\cos\beta^{*} \\
 *              0 & b^{*}\sin\gamma^{*} & -c^{*} \sin\beta^{*} \cos\alpha \\
 *              0 &                   0 & 1/c
 *        \end{matrix}
 *      \right)
 * \f]
 * 
 * Le calcule de \f$ a^\star \f$, \f$ b^\star \f$ et \f$ c^\star \f$
 * est obtenu de la façon suivante:
 * \f{eqnarray*}
 *    a^\star & = & \tau \frac{\sin\alpha}{aD} \\
 *    b^\star & = & \tau \frac{\sin\beta}{bD} \\
 *    c^\star & = & \tau \frac{\sin\gamma}{cD}
 * \f}
 * 
 * ou
 *
 * \f[
 *    D = \sqrt{1 - \cos^2\alpha - \cos^2\beta - \cos^2\gamma + 2\cos\alpha \cos\beta \cos\gamma}
 * \f]
 * 
 * pour obtenir les angles \f$ \alpha^\star \f$, \f$ \beta^\star \f$ et \f$ \gamma^\star \f$,
 * on passe par le calcule des sinus et cosinus.
 * \f[
 *  \begin{array}{cc}
 *    \cos\alpha^\star = \frac{\cos\beta \cos\gamma - \cos\alpha}{\sin\beta \sin\gamma} 
 *      & \sin\alpha^\star = \frac{D}{\sin\beta \sin\gamma} \\
 *    \cos\beta^\star = \frac{\cos\gamma \cos\alpha - \cos\beta}{\sin\gamma \sin\alpha} 
 *      & \sin\beta^\star = \frac{D}{\sin\gamma \sin\alpha} \\
 *    \cos\gamma^\star = \frac{\cos\alpha \cos\beta - \cos\gamma}{\sin\alpha \sin\beta} 
 *      & \sin\gamma^\star = \frac{D}{\sin\alpha \sin\beta} \\
 *  \end{array}
 * \f]
 *
 * \section Calcule_de_U Calcule de U.
 *
 * Il existe plusieurs façons de calculer \a U. Busing et Levy en a proposé plusieurs.
 * Nous allons présenter celle qui nécessite la mesure de seulement deux réflections ainsi que la
 * connaissance des paramètres cristallins.
 * Cette façon de calculer la matrice d'orientation \a U, peut être généralisée à n'importe quel
 * diffractomètre pour peu que la description des axes de rotation permette d'obtenir la matrice
 * de rotation de la machine \a R et le vecteur de diffusion \f$ \vec{Q} \f$.
 * Il est également possible de calculer \a U sans la connaîssance des paramètres cristallins.
 * il faut alors faire un affinement des paramètres. Cela revient à minimiser une fonction.
 * Nous allons utiliser la méthode du simplex pour trouver ce minimum et ainsi ajuster l'ensemble
 * des paramètres cristallins ainsi que la matrice d'orientation.
 * 
 * \subsection Algorithme_de_Busing_Levy Algorithme de Busing Levy.
 *
 * L'idée est de se placer dans le repère de l'axe sur lequel est monté l'échantillon.
 * On mesure deux réflections \f$ (\vec{h}_1, \vec{h}_2) \f$ ainsi que leurs angles associés.
 * Cela nous permet de calculer \a R et \f$ \vec{Q} \f$ pour chacune de ces reflections.
 * Nous avons alors ce système:
 * \f[
 *    U \cdot B \cdot \vec{h}_1
 * \f]
 * De façon à calculer facilement \a U, il est intéressant de définir deux trièdres orthonormé
 * \f$ T_{\vec{h}} \f$ et \f$ T_{\vec{Q}} \f$ à partir des vecteurs \f$ (B \cdot \vec{h}_1, B \cdot \vec{h}_2) \f$
 * et \f$ (\tilde{R}_1 \cdot \vec{Q}_1, \tilde{R}_2 \cdot \vec{Q}_2) \f$.
 * On a alors très simplement:
 * \f[
 *    U \cdot T_{\vec{h}} = T_{\vec{Q}}
 * \f]
 * Et donc:
 * \f[
 *    U = T_{\vec{Q}} \cdot \tilde{T}_{\vec{h}}
 * \f]
 *
 * \subsection Affinement_par_la_methode_du_simplex Affinement par la méthode du simplex
 *
 * Dans ce cas nous ne connaissons pas la matrice \a B, il faut alors mesurer plus de
 * deux réflections afin d'ajuster les 9 paramètres.
 * Six paramètres pour le crystal et trois pour la matrice d'orientation \a U.
 * Les trois paramètres qui permennt de representer \a U sont en fait les angles d'euler.
 * Il est donc nécessaire de connaitre la représentation Eulérien de la matrice \a U et réciproquement.
 * \f[
 *    U = X \cdot Y \cdot Z
 * \f]
 * où \a X est la matrice rotation suivant l'axe Ox et le premier angle d'Euler,
 * \a Y la matrice de rotation suivant l'axe Oy et le deuxième angle d'Euler et \a Z la matrice du troisième
 * angle d'Euler pour l'axe Oz.
 * \f[
 *      \left(
 *        \begin{matrix}
 *          1 & 0 & 0\\
 *          0 & A & -B\\
 *          0 & B & A
 *        \end{matrix}
 *      \right)
 *      \left(
 *        \begin{matrix}
 *          C & 0 & D\\
 *          0 & 1 & 0\\
 *         -D & 0 & C
 *        \end{matrix}
 *      \right)
 *      \left(
 *        \begin{matrix}
 *          E & -F & 0\\
 *          F & E & 0\\
 *          0 & 0 & 1
 *        \end{matrix}
 *      \right)
 * \f]
 * 
 * et donc:
 * 
 * \f[ 
 *    U = \left(
 *          \begin{matrix}
 *                CE &     -CF & D \\
 *            BDE+AF & -BDF+AE & -BC \\
 *           -ADE+BF &  ADF+BE & AC
 *          \end{matrix}
 *        \right)
 *  \f]
 */

namespace hkl
  {

  class Diffractometer : public HKLObject
    {
    public:

      /**
       * @brief The default destructor
       */
      virtual ~Diffractometer(void);

      /**
       * @brief compare two diffractometer
       * @param diffractometer The Diffractometer to compare with.
       * @return true if both are equals.
       */
      bool operator ==(Diffractometer const & diffractometer) const;

      ostream & printToStream(ostream & flux) const;
      ostream & toStream(ostream & flux) const;
      istream & fromStream(istream & flux);

      /**
       * @brief Get a pointer on the diffractometer Geometry.
       * @return The Geometry.
       */
      Geometry * geometry(void)
      {
        return _geometry;
      }

      /**
       * @brief Return the ModeList of the diffractometer.
       * @return the ModeList of the diffractometer.
       */
      ModeList & modes(void)
      {
        return _modes;
      }

      /**
       * @brief Return a pointer on the SampleList of the diffractometer.
       * @return The SampleList of the diffractometer.
       */
      SampleList * samples(void)
      {
        return _samples;
      }

      /**
       * @brief Return the PseudoAxeList of the diffractometer.
       * @return The PseudoAxeList of the diffractometer.
       */
      PseudoAxeList & pseudoAxes(void)
      {
        return _pseudoAxeEngines.pseudoAxes();
      }

      /**
       * @brief Return the AffinementList of the diffractometer.
       * @return the AffinementList of the diffractometer.
       */
      AffinementList & affinements(void)
      {
        return _affinements;
      }

    protected:
      Geometry * _geometry; //!< The current diffractometer Geometry.
      SampleList * _samples; //!< The SampleList of the diffractometers.
      ModeList _modes; //!< The available modes.
      PseudoAxeEngineList _pseudoAxeEngines; //!< The available PseudoAxes.
      AffinementList _affinements; //!< the available Affinement.

      /**
       * @brief The Default constructor -- protected to be sure that Diffractometer is an abstract class.
       * @param name The name of the Diffractometer.
       * @param description The description of the Diffractometer.
       */
      Diffractometer(MyString const & name, MyString const & description);
    };


  template<typename T>
  class DiffractometerTemp : public Diffractometer
    {
    public:

      /**
       * @brief The default destructor.
       */
      virtual ~DiffractometerTemp(void)
      {
        delete _samples;
      }

    protected:
      T  _geom_T; //!< The current diffractometer Geometry.

      /**
       * @brief the defaul constructor
       * @param name The DiffractometerTemp name.
       * @param description the DiffractometerTemp description.
       */
      DiffractometerTemp(MyString const & name, MyString const & description) :
          Diffractometer(name, description)
      {
        _geometry = &_geom_T;
        _samples = new SampleList(_geom_T);
      }
    };

} // namespace hkl

inline std::ostream &
operator << (std::ostream & flux, hkl::Diffractometer const & diffractometer)
{
  return diffractometer.printToStream(flux);
}

#endif // _DIFFRACTOMETER_H_
