//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/include/Attic/diffractometer.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class diffractometer, diffractometer_Eulerian4C, diffractometer_Kappa4C, diffractometer_Eulerian6C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.2 $

//

// $Log: diffractometer.h,v $
// Revision 1.2  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.1.2.44  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.1.2.42  2005/08/29 07:53:37  picca
//   GENERAL
//     + création des namespace:
//         hkl
//         hkl::angleConfiguration
//         hkl::angleConfiguration::eulerian4C
//         hkl::angleConfiguration::eulerian6C
//         hkl::angleConfiguration::kappa4C
//         hkl::diffractometer
//         hkl::diffractometer::eulerian4C
//         hkl::diffractometer::eulerian6C
//         hkl::diffractometer::kappa4C
//         hkl::mode
//         hkl::mode::eulerian4C
//         hkl::mode::eulerian6C
//         hkl::mode::eulerian6C::horizontal4C
//         hkl::mode::eulerian6C::vertical4C
//
//   AFFINEMENT
//     + Simplex method
//     + optimisation du Simplex en ajoutant le champ m_hkl_phi à la class Reflection.
//
//   ANGLECONFIGURATION
//     + création des classes Eulerian4C Eulerian6C Kappa4C
//
//   AXE
//     + derive Axe de Quaternion afin d'accélérer les calcules de getQ dans les différentes classes.
//
//   DIFFRACTOMETRE
//     + class Eulerian4C
//     + class Eulerian6C
//
//   MODES
//     + Ajout d'un champ commentaire pour décrire le mode et sa configuration.
//     + Mettre les paramètres de configuration sous forme de #Value pour pouvoir les nommer.
//     + Modifier la fonction computeAngles pour utiliser une référence sur aC et non un pointeur.
//     + Scinder le fichier mode.h en plusieurs suivant les diffractomètres.
//     - E4C
//       + Mode "Bissector"
//       + Mode "Delta Omega"
//       + Mode "Constant Omega"
//       + Mode "Constant Chi"
//       + Mode "Constant Phi"
//     - E6C
//       + Mode "Horizontal Eulerian 4C Bissector"
//       + Mode "Horizontal Eulerian 4C Delta Omega"
//       + Mode "horizontal Eulerian 4C Constant Omega"
//       + Mode "Horizontal Eulerian 4C Constant Chi"
//       + Mode "Horizontal Eulerian 4C Constant Phi"
//       + Mode "Vertical Eulerian 4C Bissector"
//       + Mode "Vertical Eulerian 4C Delta Omega"
//       + Mode "Vertical Eulerian 4C Constant Omega"
//       + Mode "Vertical Eulerian 4C Constant Chi"
//       + Mode "Vertical Eulerian 4C Constant Phi"
//
//   REFLECTIONS
//     + Ajout d'un champ m_hkl_phi ( R-1 * Q ) qui permet d'accélérer énormément le simplex.
//
//   DOCUMENTATION
//     + Réorganiser la mainpage de la documentation en plusieurs pages.
//     ~ API
//
//   BINDING
//     ~ python
//
//   FRONTEND
//     ~ Developper une interface graphique à la librairie pour la tester.
//
// Revision 1.1.2.41  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.1.2.40  2005/08/11 15:11:18  picca
// mise a jour de la documentation
//
// Revision 1.1.2.39  2005/08/01 16:17:57  picca
// en cours de modification des classes myvector et mymap
//
// Revision 1.1.2.38  2005/07/29 15:27:06  picca
// Modification de la classe diffractometer pour utiliser l'affinement.
//
// Revision 1.1.2.37  2005/07/28 16:11:30  picca
// en cours de modification de la partie affinement de la classe diffractometer.
//
// Revision 1.1.2.36  2005/07/27 15:50:35  picca
// ajout des test de la partie mode du diffractometre.
//
// Revision 1.1.2.35  2005/07/26 16:09:04  picca
// en cours de travail sur la partie mode du diffractometre.
//
// Revision 1.1.2.34  2005/07/12 16:07:20  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.1.2.33  2005/06/21 15:35:18  picca
// Surcharge du template template MyMap pour prendre en compte les map de pointeur
//
// Revision 1.1.2.32  2005/06/20 15:34:58  picca
// ajout des test pour fitParameter
//
// Revision 1.1.2.31  2005/06/20 14:00:16  picca
// version pour le premier device
//
// Revision 1.1.2.30  2005/06/13 16:03:13  picca
// avancement de l'affinement
//
// Revision 1.1.2.29  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.1.2.28  2005/06/06 08:59:26  picca
// mise a jour du binding python et de la classe dffractometer
//
// Revision 1.1.2.27  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.1.2.26  2005/06/02 11:45:15  picca
// modification pour obtenir une version utilisable du binding python
//
// Revision 1.1.2.25  2005/05/27 12:30:34  picca
// class: Reflection
// 	- ajout contructeur par default
// 	- ajout set(const Reflection & reflection) pour mettre Ã  jour un reflection Ã  partir d'une autre
// 	- ajout get_angleConfiguration et set_valueConfiguration
// 	- ajout get_source, set_source
// 	- remplacement getRelevance par get_relevance
// 	- remplacement getFlag par get_flag
//
// Revision 1.1.2.24  2005/05/26 14:36:54  picca
// class diffractometer
// - ajout getLattice, setLattice, getReciprocalLattice
// -ajout getCrystalLattice, setCrystalLattice, getCrystalReciprocalLattice
// -ajout des test correspondants.
// -ajout getReflection, getCrystalReflection
// -ajout d'un binding pour python de la classe diffractometer qui utilise la librairie boost_python
// -ajout d'un GUI en python + PyGtk
//
// Revision 1.1.2.23  2005/05/18 07:04:49  picca
// modification de getAxeLimits et computeHKL pour qu'elles utilise des pointeurs.
//
// Revision 1.1.2.22  2005/04/27 07:56:08  picca
// changement de nom de la mÃ©thode workWithCrystal en setCrystal
//
// Revision 1.1.2.21  2005/04/26 14:38:08  picca
// Ajout des fonctions modifyReflection et modifyReflectionOfCrystal Ã  la classe diffractometer
//
// Revision 1.1.2.20  2005/04/22 11:45:44  picca
// Ajout de la fonction setAxeAngle Ã  la classe AngleConfiguration
//
// Revision 1.1.2.19  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.1.2.18  2005/04/19 14:01:07  picca
// reecriture du node bissecteur du 4 cercles Eulerien
// -ajout des tests de setMode et computeAngles de la classe diffractoemters
// -ajout des test du mode bissecteur Eulerien 4C.
//
// Revision 1.1.2.17  2005/04/14 09:43:07  picca
// Ajout et test de la fonction computeHKL de la classe difrfactoemeter
//
// Revision 1.1.2.16  2005/04/12 07:09:52  picca
// Réécriture de la classe diffractoemeter
//
// Revision 1.1.2.15  2005/04/07 09:13:36  picca
// rewrite of diffractoemter_test
//
// Revision 1.1.2.14  2005/04/06 16:10:38  picca
// Probleme de caractere unicode dans un des commentaires de diffractoemter.h
//
// Revision 1.1.2.13  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.1.2.12  2005/03/31 14:30:42  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.1.2.11  2005/03/31 11:28:26  picca
// Modification de la classe Reflection.
// - ajout de 2 champs:
// 	m_source pour sauvegarder l'etat de la source pour chaque reflection.
// 	m_flag pour indiquer si oui ou non on utilise la reflection dans le calcule de U
// - ajout des getSet pour tous les champs de reflection.
// - ajout des test de ces getSet.
//
// Revision 1.1.2.10  2005/03/30 15:52:01  picca
// change the source class to store only the waveLength and the direction of the incidental beam
//
// Revision 1.1.2.9  2005/03/24 08:36:36  picca
// remplacement des fonction getSampleAxe et getDetectorAxe par getAxe dans la classe angleConfiguration
//
// Revision 1.1.2.8  2005/03/23 14:34:51  picca
// -surcharge des operateurs * *= pour les classes svector et smatrix
// -Suppression des methodes RightMultiply and LeftMutiply
//
// Revision 1.1.2.7  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.1.2.6  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.1.2.5  2005/03/11 10:45:29  picca
// -suppresion des attributs m_SizeOfArray, m_numberOfInsertedElements
// - changement de m_ReflectionList vers le type vector<reflection>
//
// Revision 1.1.2.4  2005/03/11 09:41:02  picca
// -changement de nom des classes eulerianDiffractometer4C -> diffractometer_Eulerian4C
// idem pour eulerian 6C et kappa 4C
//
// Revision 1.1.2.3  2005/03/11 08:36:28  picca
// - suppression de printToScreen
// - ajout de operator<< pour la classe diffractometer
// - ajout d-un fonction getReflection qui retourne un pointeur sur la reflection selectionne. (voir s'il ne faut pas faire une copie pour viter les fuites mmoires).
//
// Revision 1.1.2.2  2005/03/02 09:20:22  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.1.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.16  2005/02/11 14:30:17  picca
// documentation
//
// Revision 1.15  2005/01/27 09:23:53  delos
// Commentaires pour CVS en tete des fichiers
//

//

//

// copyleft :       Synchrotron SOLEIL

//                  L'Orme des Merisiers

//                  Saint-Aubin - BP 48

//                  91192 GIF-sur-YVETTE CEDEX

//

//-======================================================================
#ifndef _DIFFRACTOMETER_H_
#define _DIFFRACTOMETER_H_

#include <vector>
#include <string>
#include <iostream>
#include "axe.h"
#include "mode.h"
#include "source.h"
#include "cristal.h"
#include "lattice.h"
#include "svecmat.h"
#include "affinement.h"
#include "reflection.h"
#include "HKLException.h"
#include "angleconfiguration.h"

/**
 *
 * \mainpage
 *
 * L'objectif de cette librairie est de mêtre à disposition l'ensemble des outils permettant de piloter
 * un diffractomètre. L'ensemble des calcules présents dans cette librairie sont basés sur une équation
 * fondamentale.
 *
 * \ref Diffractometer
 * 
 * \ref Diffractometer_eulerian_4C
 *
 * \ref Diffractometer_eulerian_6C
 */

/**
 * \page Diffractometer Généralité.
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

namespace hkl {
  
/**
 * @brief The abstract base class to define all different kinds of diffractometers and drive experiments.
 */
class Diffractometer : public Object
{
public:

  /**
   * @brief Destructor
   */
  virtual ~Diffractometer(void);

  /**
   * @brief Print the state of the current diffractometer on a ostream.
   * @param flux The std::ostrema to write into.
   * @return the flux modified.
   */
  std::ostream & printToStream(std::ostream & flux) const;

/******************************/
/* Modification of the Source */
/******************************/
  
  /**
   * @brief Set the X-Ray wave length use by the diffractometer
   * @param wl the new wave length
   */
  void setWaveLength(double wl);
  
  /**
   * @brief Get the X-Ray wave length use by the diffractometer
   */
  double getWaveLength(void) const;

  
/******************************************/
/* Modification of the angleConfiguration */
/******************************************/
 
  /**
   * @brief Get a list of the axes names
   * @return the list of all the axes names.
   */
  std::vector<std::string> const getAxesNames(void) const;
   
  /**
   * @brief Get a list of the sample axes names
   * @return The list of all the sample axes names.
   */
  std::vector<std::string> const getAxesSampleNames(void) const;
   
  /**
   * @brief Get a list of the detector  axes names
   * @return The list of all the detector axes names.
   */
  std::vector<std::string> const getAxesDetectorNames(void) const;
   
  /**
   * @brief Set the %Axe current angle
   * @param name The %Axe name
   * @param angle the angle to set
   */
  void setAxeAngle(std::string const & name,
                   double angle) throw (HKLException);

  /**
   * @brief Get the %Axe current angle
   * @param name The %Axe name
   * @return the angle
   */
  double getAxeAngle(std::string const & name) throw (HKLException);
  
/*****************************/
/* Modifications of crystals */
/*****************************/

  /**
   * @brief Get a vector of string fill with the crystal names.
   */
  std::vector<std::string> const getCrystalNames(void) const;
  
  /**
   * @brief Get the name of the currentCrystal as a string
   */
  std::string const & getCurrentCrystalName(void) const throw (HKLException);
  
  /**
   * @brief Choose the crystal to work with.
   * @param name A std::string containing the name of the %Crystal to use with the diffractometer.
   */
  void setCurrentCrystal(std::string const & name) throw (HKLException);
  
  /**
   * @brief Add a new crystal into the crystal list.
   * @param name A std::string containing the name of the %Crystal to add.
   */
  void addNewCrystal(std::string const & name) throw (HKLException);
  
  /**
   * @brief Set the crystal Parameters
   * @param name
   * @param a
   * @param b
   * @param c
   * @param alpha
   * @param beta
   * @param gamma
   */
  void setCrystalLattice(std::string const & name,
                         double a, double b, double c,
                         double alpha, double beta, double gamma) throw (HKLException);

  /**
   * @brief Get the crystal Parameters
   * @param name
   * @param a
   * @param b
   * @param c
   * @param alpha
   * @param beta
   * @param gamma
   */
  void getCrystalLattice(std::string const & name,
                         double * a, double * b, double * c,
                         double * alpha, double * beta, double * gamma) const throw (HKLException);

  /**
   * @brief Get the crystal Parameters
   * @param name
   * @param a
   * @param b
   * @param c
   * @param alpha
   * @param beta
   * @param gamma
   */
  void getCrystalReciprocalLattice(std::string const & name,
                                   double * a, double * b, double * c,
                                   double * alpha, double * beta, double * gamma) const throw (HKLException);
  
  /**
   * @brief Get the values store in the %FitParameters of a %Crystal.
   * @param[in] crystal_name The name of the crystal.
   * @param[in] parameter_name The name of the parameter.
   * @param[out] value The value of the parameter.
   * @param[out] min The allow minimum value.
   * @param[out] max The allow maximum value.
   * @param[out] to_fit The flag saying if the parameter must be fit.
   */
  void
  getCrystalParameterValues(std::string const & crystal_name,
                            std::string const & parameter_name,
                            double * value,
                            double * min,
                            double *max,
                            bool * to_fit) const throw (HKLException);
  /**
   * @brief Set the values store in the %FitParameters of a %Crystal.
   * @param[in] crystal_name The name of the crystal.
   * @param[in] parameter_name The name of the parameter.
   * @param[in] value The value of the parameter.
   * @param[in] min The allow minimum value.
   * @param[in] max The allow maximum value.
   * @param[in] to_fit The flag saying if the parameter must be fit.
   */
  void
  setCrystalParameterValues(std::string const & crystal_name,
                            std::string const & parameter_name,
                            double value,
                            double min,
                            double max,
                            bool to_fit) throw (HKLException);
  
  /**
   * @brief get the UB matrix of a %Crystal.
   */
   smatrix getCrystal_UB(std::string const & name) const throw (HKLException);
   
  /**
   * @brief get the fitness of a %Crystal.
   * @param name The name of the %Crystal.
   * @return the fitness of the %Crystal.
   */
   double getCrystalFitness(std::string const & name) throw (HKLException);
   
  /**
   * @brief Delete a crystal from the crystal list.
   * @param name
   */
  void delCrystal(std::string const & name) throw (HKLException);
  
  /**
   * @brief Copy a crystal to an other one.
   * @param from Name of the copied crystal.
   * @param to Name of the new crystal.
   */
  void copyCrystalAsNew(std::string const & from,
                        std::string const & to) throw (HKLException);

/********************************/
/* Modifications of reflections */
/********************************/

  /**
   * @brief return the number of reflections of a crystal.
   * @param name the crystal name.
   * @return the number of reflections.
   */
  unsigned int getCrystalNumberOfReflection(std::string const & name) const throw (HKLException);
  
  /**
   * @brief add a reflection to the current crystal.
   * @param name The name of the crystal which containe the new reflection.
   * @param h
   * @param k
   * @param l
   * @param relevance
   * @param flag (is the reflection use for calculation).
   * @return the index of the reflection we have just added.
   */
  unsigned int addCrystalReflection(std::string const & name, 
                                    double h, double k, double l,
                                    int relevance, bool flag) throw (HKLException);

  /**
   * @brief get the angle values of a refelction of a crystal.
   * @param crystalName of the crystal
   * @param index of the reflection
   * @param axeName of the axe.
   */
  double getCrystalReflectionAxeAngle(std::string const & crystalName,
                                      unsigned int index,
                                      std::string const & axeName) const throw (HKLException);

  /**
   * @brief Modify a reflection of a crystal.
   * @param name the name of the crystal
   * @param index the index of the reflection.
   * @param h
   * @param k
   * @param l
   * @param relevance
   * @param flag
   */
  void setCrystalReflectionParameters(std::string const & name,
                                      unsigned int index,
                                      double h, double k, double l,
                                      int relevance, bool flag) throw (HKLException);
                            
  /**
   * @brief Get the parameters related to a reflection of a crystal.
   * @param name
   * @param index
   * @param h
   * @param k
   * @param l
   * @param relevance
   * @param flag
   */
  void getCrystalReflectionParameters(std::string const & name,
                                      unsigned int index,
                                      double * h, double * k, double *l,
                                      int * relevance, bool * flag) const throw (HKLException);

  /**
   * @brief delete the reflection from thr currentcrystal
   * @param name The name of the crystal wich containe the reflection list.
   * @param index The reflection to delete.
   */
  void delCrystalReflection(std::string const & name,
                            unsigned int index) throw (HKLException);

  /**
   * @brief Copy a reflection from a crytal to an other one.
   * @param from The first crystal.
   * @param ifrom The index of the reflection.
   * @param to The second crystal.
   */
  void copyCrystalReflectionFromTo(std::string const & from, 
                                   unsigned int ifrom,
                                   std::string const & to) throw (HKLException);
                            
 
/********************************/
/* Gestion des modes de calcule */
/********************************/

  /**
   * @brief Get the available modes.
   * @return An array of string with all modes.
   */
  std::vector<std::string> getModeNames(void) const;

  /**
   * @brief Get the name of the current Mode.
   * @return The name of the current Mode.
   */
  std::string const & getCurrentModeName(void) const throw (HKLException);
 
  /**
   * @brief Get a Mode description.
   * @param name The name of the mode.
   * @return The description of a Mode.
   */
  std::string const & getModeDescription(std::string const & name) const throw (HKLException);
 
  /**
   * @brief Get the parametres names use by a mode
   * @param name the name of the mode.
   * @return An array of string will all the parameters names.
   */
  std::vector<std::string> getModeParametersNames(std::string const & name) const throw (HKLException);

  /**
   * @brief get the parameter value of a Mode
   * @param mode_name The name of the mode.
   * @param parameter_name The name of the parameter.
   * @return The value of the parameter.
   */
  double getModeParameterValue(std::string const & mode_name,
                               std::string const & parameter_name) const throw (HKLException);
   
  /**
   * @brief Set the parameter value of a Mode
   * @param mode_name The name of the mode.
   * @param parameter_name The name of the parameter.
   * @param value The value to set.
   */
  void setModeParameterValue(std::string const & mode_name,
                             std::string const & parameter_name,
                             double value) throw (HKLException);
   
  /**
   * @brief Change the current computational mode.
   * @param name The name of the mode you want to use.
   */
  void setCurrentMode(std::string const & name) throw (HKLException);

/**************/
/* Affinement */
/**************/

  /**
   * @brief Get the available Affinement.
   * @return An array of string with all the affinement names.
   */
  std::vector<std::string> getAffinementNames(void) const;

  /**
   * @brief Get the maximum number of iteration for a fit methode.
   * @param name The name of the fit methode.
   * @return the maximum number of iterations.
   */
  unsigned int getAffinementMaxIteration(std::string const & name) const throw (HKLException);

  /**
   * @brief Set the maximum number of iteration for a fit methode.
   * @param name The name of the fit methode.
   * @param max the value to set.
   */
  void setAffinementMaxIteration(std::string const & name, unsigned int max) throw (HKLException);

  /**
   * @brief Get the number of iteration ran by a fit methode.
   * @param name The name of the fit methode.
   * @return the number of iterations.
   */
  unsigned int  getAffinementIteration(std::string const & name) const throw (HKLException);

/************/
/* Calcules */
/************/

  /**
   * @brief Compute the orientation matrix from two basic non-parallel reflections.
   *
   * Compute the orientation matrix from two basic non-parallel reflections.
   */
  void computeU(void) throw (HKLException);

  /**
   * @brief fit the crystal Parameters of a %Crystal
   * @param crystal_name The %Crystal name to fit.
   * @param method_name The %Affinement name methode to use.
   * @return The fitness of the fitted crystal.
   */
  double affineCrystal(std::string const & crystal_name, std::string const & method_name) throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @exception det(A)=0
   * 
   * Solve a linear system Ax = b where A is the product of the rotation matrices 
   * OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
   * scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
   */
  void computeHKL(double * h, double * k, double * l) throw (HKLException);

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   *
   *  The main function to get a sample of angles from (h,k,l).
   */
  void computeAngles(double h, double k, double l) throw (HKLException);

protected:
  std::string m_name;
  Source m_source; //!< The light source and its wave length.
  AngleConfiguration * m_aC; //!< The current diffractometer angle configuration.
  Crystal * m_crystal; //!< The crystal we are working with.
  CrystalList m_crystalList; //!< The crystal List of the diffractometer.
  Mode * m_mode; //!< The mode describes the way we use the diffractometer.
  ModeList m_modeList; //!< the available modes.
  AffinementList m_affinementList; //!< The available affinement methode.

  /**
   * @brief Default constructor
   *
   * - protected to make sure this class is abstract.
   */
  Diffractometer(void);
};

}

/**
   * @brief Surcharge de l'operateur << pour la class %Diffractometer
   * @param flux 
   * @param C 
   * @return 
   */
std::ostream & operator << (std::ostream & flux, hkl::Diffractometer const & diffractometer);

#endif // _DIFFRACTOMETER_H_
