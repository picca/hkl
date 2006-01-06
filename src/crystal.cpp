//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/crystal.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class cristal

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.8 $

//

// $Log: crystal.cpp,v $
// Revision 1.8  2006/01/06 16:24:29  picca
// * modification of the bksys files
//
// Revision 1.7  2005/12/06 09:31:01  picca
// *** empty log message ***
//
// Revision 1.6  2005/12/05 10:34:43  picca
// * When adding a reflection with the same (hkl) than another one, the flag is
//   automatically set to false.
//
// Revision 1.5  2005/11/16 12:42:49  picca
// * modified crystal::randomize to deal with different combination of alpha, beta and gamma fit.
//
// Revision 1.4  2005/11/14 13:34:14  picca
// * update the Simplex method.
//
// Revision 1.3  2005/10/25 14:27:31  picca
// * Object add m_description, accessor and test functions
//
// Revision 1.2  2005/10/25 11:32:11  picca
// * rename cristal.h and cristal.cpp -> crystal.h crystal.cpp
// * same for the cristal_test suite.
//
// Revision 1.1  2005/10/25 10:05:42  picca
// * cristal.cpp -> crystal.cpp
//
// Revision 1.11  2005/10/20 12:48:47  picca
// * right calculation for the number of usable reflections
// close: #976 #977
//
// Revision 1.10.2.1  2005/10/20 12:40:20  picca
// * modification of AngleConfiguration::getAxesNames()
// * add Reflection::isColinear() + test functions
// * add Crystal::isEnoughReflections() + test functions
// * remove crystal::getNumberOfReflectionsForCalculation() what a silly name :)
// * close #976 #977
//
// Revision 1.10  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.9.2.35  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.9.2.33  2005/08/30 13:55:15  picca
// *** empty log message ***
//
// Revision 1.9.2.32  2005/08/29 14:10:12  picca
// Modification de Axe pour ne plus hériter de quaternion.
//
// Revision 1.9.2.31  2005/08/29 07:53:37  picca
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
// Revision 1.9.2.30  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.9.2.29  2005/08/11 15:11:18  picca
// mise a jour de la documentation
//
// Revision 1.9.2.28  2005/07/25 15:54:31  picca
// L'affinement fonctionne.
//
// Revision 1.9.2.27  2005/07/22 14:57:16  picca
// en cours d'optimisation du fit
//
// Revision 1.9.2.26  2005/07/22 07:53:50  picca
// Now affinement_simplex is working
//
// Revision 1.9.2.25  2005/07/18 16:16:11  picca
// -ajout test fonctions pour Crystal:
// 	operator +=, -=, *=, /=, -, *
//
// Revision 1.9.2.24  2005/07/13 16:00:03  picca
// Travail en cours sur l'affinement
//
// Revision 1.9.2.23  2005/07/12 16:07:21  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.9.2.22  2005/06/23 16:29:04  picca
// Juste avant le depart pour montreal.
//
// Revision 1.9.2.21  2005/06/21 13:12:40  picca
// ajout du template MyMap
//
// Revision 1.9.2.20  2005/06/20 14:00:16  picca
// version pour le premier device
//
// Revision 1.9.2.19  2005/06/20 07:08:10  picca
// affinement suite
//
// Revision 1.9.2.18  2005/06/09 12:56:15  picca
// ajout Lattice::set et get anisi que des fonctions de test.
//
// Revision 1.9.2.17  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.9.2.16  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.9.2.15  2005/06/02 11:45:15  picca
// modification pour obtenir une version utilisable du binding python
//
// Revision 1.9.2.14  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.9.2.13  2005/04/20 08:29:00  picca
// -configuration de scons pour compiler sous Windows
// -modification du source pour compiler avec cl (VC++6.0)
//
// Revision 1.9.2.12  2005/04/14 09:43:07  picca
// Ajout et test de la fonction computeHKL de la classe difrfactoemeter
//
// Revision 1.9.2.11  2005/04/12 07:09:52  picca
// Réécriture de la classe diffractoemeter
//
// Revision 1.9.2.10  2005/04/06 16:10:38  picca
// Probleme de caractere unicode dans un des commentaires de diffractoemter.h
//
// Revision 1.9.2.9  2005/04/05 14:21:41  picca
// Ajout d'une exception à la méthode delReflection de la classe crystal
//
// Revision 1.9.2.8  2005/04/04 14:22:38  picca
// modification de la classe Crystal.
// -ajout des tests unitaires.
//
// Revision 1.9.2.7  2005/04/04 11:45:57  picca
// ajout de la classe lattice qui contient les paramètres cristallins.
//
// Revision 1.9.2.6  2005/04/01 14:59:01  picca
// Ajout de la classe Lattice qui contient les parametres cristallins des crystaux
//
// Revision 1.9.2.5  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.9.2.4  2005/03/31 14:30:43  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.9.2.3  2005/03/11 13:47:58  picca
// Ajout du constructeur par default cristal()
//
// Revision 1.9.2.2  2005/03/01 17:59:09  picca
// Deplacement des test de la classe cristal vers le fichier cristal_test et utilisation de la librairie cppunit pour ces dernier.
//
// Ajout de la surcharge de l'operateur<< pour afficher simplement la classe cristal
//
// suppression de la methode printOnScreen() et check_cristal()
//
// Revision 1.9.2.1  2005/03/01 08:52:01  picca
// automake et cppUnit
//
// Revision 1.9  2005/01/27 09:23:53  delos
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
// Cristal class functions to define the different lattices.

#include "crystal.h"

namespace hkl {

  Crystal::Crystal(void) :
    FitParameterList(),
    Object()
  {
    add(FitParameter("a", 0., 1., 10., true, constant::math::epsilon_1));
    add(FitParameter("b", 0., 1., 10., true, constant::math::epsilon_1));
    add(FitParameter("c", 0., 1., 10., true, constant::math::epsilon_1));
    add(FitParameter("alpha", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("beta", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("gamma", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 90. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("euler_x", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("euler_y", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("euler_z", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));

    m_U = smatrix(1., 0., 0.,
                  0., 1., 0.,
                  0., 0., 1.);
  }

  Crystal::Crystal(string const & name) :
    FitParameterList(),
    Object(name)
  {
    add(FitParameter("a", 0., 1., 10., true, constant::math::epsilon_1));
    add(FitParameter("b", 0., 1., 10., true, constant::math::epsilon_1));
    add(FitParameter("c", 0., 1., 10., true, constant::math::epsilon_1));
    add(FitParameter("alpha", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("beta", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("gamma", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 90. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("euler_x", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("euler_y", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
    add(FitParameter("euler_z", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));

    m_U = smatrix(1., 0., 0.,
                  0., 1., 0.,
                  0., 0., 1.);
  }

  Crystal::Crystal(Crystal const & crystal) :
    FitParameterList(crystal),
    Object(crystal),
    m_B(crystal.m_B),
    m_U(crystal.m_U),
    m_reflectionList(crystal.m_reflectionList)
  {}

  bool
  Crystal::operator ==(Crystal const & crystal) const
  {
    return FitParameterList::operator==(crystal)
      && Object::operator==(crystal)
      && m_B == crystal.m_B
      && m_U == crystal.m_U
      && m_reflectionList == crystal.m_reflectionList;
  }

  void
  Crystal::getLattice(double * a, double * b, double * c,
                      double * alpha, double * beta, double * gamma) const
  {
    FitParameterList::const_iterator iter = begin();
    FitParameterList::const_iterator last = end();

    *a = (*this)["a"].get_value();
    *b = (*this)["b"].get_value();
    *c = (*this)["c"].get_value();
    *alpha = (*this)["alpha"].get_value();
    *beta = (*this)["beta"].get_value();
    *gamma = (*this)["gamma"].get_value();
  }

  void
  Crystal::getReciprocalLattice(double * a_star, double * b_star, double * c_star,
                                double * alpha_star, double * beta_star, double * gamma_star) const
  {
    double a, b, c, alpha, beta, gamma;
    getLattice(&a, &b, &c, &alpha, &beta, &gamma);

    double D = sqrt( 1 
        - cos(alpha)*cos(alpha) 
        - cos(beta)*cos(beta)
        - cos(gamma)*cos(gamma)
        + 2*cos(alpha)*cos(beta)*cos(gamma));

    double cos_beta1 =
      (cos(beta)*cos(gamma) - cos(alpha)) /
      (sin(beta)*sin(gamma));
    double cos_beta2 = 
      (cos(gamma)*cos(alpha) - cos(beta)) /
      (sin(gamma)*sin(alpha));
    double cos_beta3 = 
      (cos(alpha)*cos(beta) - cos(gamma)) /
      (sin(alpha)*sin(beta));
    double sin_beta1 = D / (sin(beta) * sin(gamma));
    double sin_beta2 = D / (sin(gamma) * sin(alpha));
    double sin_beta3 = D / (sin(alpha) * sin(beta));

    *a_star = constant::physic::tau * sin(alpha) / (a * D);
    *b_star = constant::physic::tau * sin(beta) / (b * D);
    *c_star = constant::physic::tau * sin(gamma) / (c * D);

    *alpha_star = atan2(sin_beta1, cos_beta1);
    *beta_star = atan2(sin_beta2, cos_beta2);
    *gamma_star = atan2(sin_beta3, cos_beta3);
  }

  void
  Crystal::setLattice(double const & a, double const & b, double const & c,
                      double const & alpha, double const & beta, double const & gamma)
  {
    (*this)["a"].set_value(a);
    (*this)["b"].set_value(b);
    (*this)["c"].set_value(c);
    (*this)["alpha"].set_value(alpha);
    (*this)["beta"].set_value(beta);
    (*this)["gamma"].set_value(gamma);
    _computeB();
  }

  unsigned int
  Crystal::addReflection(Reflection const & reflection) throw (HKLException)
  {
    // test the validity of the reflection
    if (fabs(reflection.get_geometry().get_source().get_waveLength()) < constant::math::epsilon_0)
      throw HKLException("The waveLength is equal to zero.",
                         "The source is not properly configure",
                         "Crystal::addReflection");
    
    // If the reflection already exist put the flag to false
    if (reflection.get_flag())
    {
      ReflectionList::iterator iter(m_reflectionList.begin());
      ReflectionList::iterator end(m_reflectionList.end());
      while(iter != end)
      {
        if (fabs(reflection.get_h() - iter->get_h()) < constant::math::epsilon_0
            && fabs(reflection.get_k() - iter->get_k()) < constant::math::epsilon_0
            && fabs(reflection.get_l() - iter->get_l()) < constant::math::epsilon_0)
        {
          m_reflectionList.push_back(reflection);
          m_reflectionList.back().set_flag(false);
          return m_reflectionList.size();
        }
        ++iter;
      }
    } 
    m_reflectionList.push_back(reflection);
    return m_reflectionList.size();
  }

  void
  Crystal::delReflection(unsigned int const & index ) throw (HKLException)
  {
    unsigned int nb_reflection = m_reflectionList.size();

    if (index >= nb_reflection){
      ostringstream reason;
      ostringstream description;

      reason << "The reflection number " << index << " is out of range";
      description << " you ask for the reflection " << index 
        << " deletion, but the cristal: " << get_name() << " containe only "
        << nb_reflection << " reflections";

      throw HKLException(reason.str(),
          description.str(),
          "Crystal::delReflection");
    }

    vector<Reflection>::iterator iter = m_reflectionList.begin();

    for(unsigned int i=0;i<index;i++)
      ++iter;

    m_reflectionList.erase(iter);
  }

  void
  Crystal::setReflection(unsigned int const & index,
                         Reflection const & reflection) throw (HKLException)
  {
    unsigned int nb_reflection = m_reflectionList.size();

    if (index >= nb_reflection){
      ostringstream reason;
      ostringstream description;

      reason << "The reflection number " << index << " is out of range";
      description << " you ask for the modification of the " << index 
        << "th reflection, but the cristal: " << get_name() << " containe only "
        << nb_reflection << " reflections";

      throw HKLException(reason.str(),
                         description.str(),
                         "Crystal::setReflection");
    }

    // If the reflection already exist put the flag to false      
    if (reflection.get_flag())
    {
      ReflectionList::iterator iter(m_reflectionList.begin());
      ReflectionList::iterator end(m_reflectionList.end());
      while(iter != end)
      {
        if (fabs(reflection.get_h() - iter->get_h()) < constant::math::epsilon_0
            && fabs(reflection.get_k() - iter->get_k()) < constant::math::epsilon_0
            && fabs(reflection.get_l() - iter->get_l()) < constant::math::epsilon_0)
        {
          m_reflectionList[index] = reflection;
          m_reflectionList[index].set_flag(false);
          return;
        }
        ++iter;
      }
    } 
    m_reflectionList[index] = reflection;
  }

  Reflection &
  Crystal::getReflection(unsigned int const & index) throw (HKLException)
  {
    unsigned int nb_reflection = m_reflectionList.size();

    if (index >= nb_reflection){
      ostringstream reason;
      ostringstream description;

      reason << "The reflection number " << index << " is out of range";
      description << " you ask for the reflection " << index 
        << " deletion, but the cristal: " << get_name() << " containe only "
        << nb_reflection << " reflections";

      throw HKLException(reason.str(),
          description.str(),
          "Crystal::getReflection");
    }

    return m_reflectionList[index];
  }

  Reflection const &
  Crystal::getReflection(unsigned int const & index) const throw (HKLException)
  {
    unsigned int nb_reflection = m_reflectionList.size();

    if (index >= nb_reflection){
      ostringstream reason;
      ostringstream description;

      reason << "The reflection number " << index << " is out of range";
      description << " you ask for the reflection " << index 
        << " deletion, but the cristal: " << get_name() << " containe only "
        << nb_reflection << " reflections";

      throw HKLException(reason.str(),
          description.str(),
          "Crystal::getReflection");
    }

    return m_reflectionList[index];
  }

  bool
  Crystal::isEnoughReflections(unsigned int nb_reflections) const
  {
    unsigned int nb_usable_reflections = 0;
    ReflectionList::const_iterator iter = m_reflectionList.begin();
    ReflectionList::const_iterator iter2 = m_reflectionList.begin();
    ReflectionList::const_iterator end = m_reflectionList.end();

    while(iter < end && nb_usable_reflections < nb_reflections)
    {
      if (iter->get_flag())
      {
        if (nb_usable_reflections == 0)
          nb_usable_reflections = 1;
        iter2 = iter;
        ++iter2;
        while(iter2 < end && nb_usable_reflections < nb_reflections)
        {
          if (iter2->get_flag())
          {
            if (!iter->isColinear(*iter2))
              nb_usable_reflections++;
          }
          ++iter2;
        }
      }
      ++iter;
    }
    if (nb_usable_reflections == nb_reflections)
      return true;
    else
      return false;
  }

  void
  Crystal::computeU(void) throw (HKLException)
  {
    if (!isEnoughReflections(2))
      throw HKLException("Not enought reflections (at least 2)",
                         "Please add reflections.",
                         "crystal::computeU");

    ReflectionList::iterator iter = m_reflectionList.begin();
    iter = _getNextReflectionIteratorForCalculation(iter);
    svector h1c = m_B * iter->getHKL();
    svector u1phi = iter->get_hkl_phi();

    ++iter;
    iter = _getNextReflectionIteratorForCalculation(iter);
    svector h2c = m_B * iter->getHKL();
    svector u2phi = iter->get_hkl_phi();

    // Compute matrix Tc from h1c and h2c.
    smatrix Tc = h1c.axisSystem(h2c).transpose();

    // Compute Tphi.
    smatrix Tphi = u1phi.axisSystem(u2phi);

    // Compute U from equation (27).
    m_U = Tphi;
    m_U *= Tc;
  }

  double
  Crystal::fitness(void) throw (HKLException)
  {
    unsigned int nb_reflections = 0;
    double fitness = 0.;
    svector hkl_phi, hkl_phi_c;

    if (!isEnoughReflections(1))
      throw HKLException("Not enought reflections",
                         "Please add reflections.",
                         "crystal::variance");

    _computeB();
    _computeU();
    ReflectionList::const_iterator iter = m_reflectionList.begin();
    ReflectionList::const_iterator end = m_reflectionList.end();
    while(iter != end){
      if (iter->get_flag())
      {
        hkl_phi = iter->get_hkl_phi();
        hkl_phi_c = m_U * m_B * iter->getHKL();
        hkl_phi -= hkl_phi_c;
        fitness += hkl_phi[0]*hkl_phi[0] + hkl_phi[1]*hkl_phi[1] + hkl_phi[2]*hkl_phi[2];
        nb_reflections++;
      }
      ++iter;
    }
    fitness /= 3*nb_reflections;

    return fitness;
  }

  void
  Crystal::randomize(void)
  {
    FitParameterList::randomize();
    svector a, b, c;
    svector axe;
   
    // La valeur des angles alpha, beta et gamma ne sont pas indépendant.
    // Il faut donc gérer les différents cas.
    
    FitParameter & Alpha = (*this)["alpha"];
    FitParameter & Beta = (*this)["beta"];
    FitParameter & Gamma = (*this)["gamma"];
   
    unsigned int angles_to_fit = Alpha.get_flagFit() + Beta.get_flagFit() + Gamma.get_flagFit();
   
    switch (angles_to_fit)
    {
      case 0:
        break;
      case 1:
        if (Alpha.get_flagFit()) // alpha
        {
          a.set(1, 0, 0);
          b = a.rotatedAroundVector(axe.randomize(a), Gamma.get_value());
          c = a.rotatedAroundVector(axe.randomize(a), Beta.get_value());
          Alpha.set_value(b.angle(c));
        }
        else if (Beta.get_flagFit()) { // beta
          a.set(1, 0, 0);
          b = a.rotatedAroundVector(axe.randomize(a), Gamma.get_value());
          c = b.rotatedAroundVector(axe.randomize(b), Alpha.get_value());
          Beta.set_value(a.angle(c));
        }
        else { // gamma
          a.set(1, 0, 0);
          c = a.rotatedAroundVector(axe.randomize(a), Beta.get_value());
          b = c.rotatedAroundVector(axe.randomize(c), Alpha.get_value());
          Gamma.set_value(a.angle(b));
        }
        break;
      case 2:
        if (Alpha.get_flagFit())
        {
          if (Beta.get_flagFit()) // alpha + beta
          {
            a.set(1, 0, 0);
            b = a.rotatedAroundVector(axe.randomize(a), Gamma.get_value());
            c.randomize(a, b);
            Alpha.set_value(b.angle(c));
            Beta.set_value(a.angle(c));
          } else { // alpha + gamma
            a.set(1, 0, 0);
            c = a.rotatedAroundVector(axe.randomize(a), Beta.get_value());
            b.randomize(a, c);
            Alpha.set_value(b.angle(c));
            Gamma.set_value(a.angle(b));
          }
        } else { // beta + gamma
          b.set(1, 0, 0);
          c = b.rotatedAroundVector(axe.randomize(b), Alpha.get_value());
          a.randomize(b, c);
          Beta.set_value(a.angle(c));
          Gamma.set_value(a.angle(b));
        }
        break;
      case 3:
        a.randomize();
        b.randomize(a);
        c.randomize(a, b);
        Alpha.set_value(b.angle(c));
        Beta.set_value(a.angle(c));
        Gamma.set_value(a.angle(b));
        break;
    }
    _computeB();
    _computeU();
  }

  // William R. Busing and Henri A. Levy "Angle calculation 
  // for 3- and 4- Circle X-ray and Neutron Diffractometer"
  // (1967) Acta Cryst., 22, 457-464.
  // Compute the matrix B from equation (3) page 458.
  void
  Crystal::_computeB(void)
  {
    double a_star, b_star, c_star, alpha_star, beta_star, gamma_star;
    getReciprocalLattice(&a_star, &b_star, &c_star, &alpha_star, &beta_star, &gamma_star);

    double c = (*this)["c"].get_value();

    m_B.set( a_star, b_star * cos(gamma_star),                   c_star * cos(beta_star),
                 0., b_star * sin(gamma_star), c_star * sin(beta_star) * cos(alpha_star),
                 0.,                       0.,                 constant::physic::tau / c);
  }

  void
  Crystal::_computeU(void)
  {
    double euler_x = (*this)["euler_x"].get_value();
    double euler_y = (*this)["euler_y"].get_value();
    double euler_z = (*this)["euler_z"].get_value();

    set_U(smatrix(euler_x, euler_y, euler_z));
  }

  ReflectionList::iterator &
  Crystal::_getNextReflectionIteratorForCalculation(ReflectionList::iterator & from) throw (HKLException)
  {
    ReflectionList::iterator end = m_reflectionList.end();

    while( from < end){
      if (from->get_flag())
        return from;
      ++from;
    }
    throw HKLException("No more reflection.",
        "Please add reflections.",
        "Crystal::_getNextReflectionIteratorForCalculation");
  }

  ostream &
  Crystal::printToStream(ostream & flux) const
  { 
    double a, b, c, alpha, beta, gamma;
    getLattice(&a, &b, &c, &alpha, &beta, &gamma);
    alpha *= constant::math::radToDeg;
    beta *= constant::math::radToDeg;
    gamma *= constant::math::radToDeg;
      
    double a_star, b_star, c_star, alpha_star, beta_star, gamma_star;
    getReciprocalLattice(&a_star, &b_star, &c_star, &alpha_star, &beta_star, &gamma_star);
    alpha_star *= constant::math::radToDeg;
    beta_star *= constant::math::radToDeg;
    gamma_star *= constant::math::radToDeg;
    

    flux << "cristal: " << get_name() << endl;
    flux << " Direct lattice (a, b, c) (alpha, beta, gamma): " 
      << a << " " << b << " " << c << " " << alpha << " " << beta << " " << gamma << endl;
    flux << " Reciprocal Lattice                           : "
      << a_star << " " << b_star << " " << c_star << " " << alpha_star << " " << beta_star << " " << gamma_star << endl;
    flux << "B: " << get_B() << endl;
    flux << "U: " << get_U() << endl;

    flux << endl;
    FitParameterList::printToStream(flux);
    flux << endl;

    ReflectionList::const_iterator iter = m_reflectionList.begin();
    ReflectionList::const_iterator end = m_reflectionList.end();
    while(iter != end){
      flux << *iter;
      ++iter;
    }
    return flux;
  }

  ostream &
  Crystal::toStream(ostream & flux) const
  {
    Object::toStream(flux);
    FitParameterList::toStream(flux);
    m_B.toStream(flux);
    m_U.toStream(flux);
    
    unsigned int nb_reflections = m_reflectionList.size();
    flux << " " << nb_reflections << endl;
    vector<Reflection>::const_iterator iter = m_reflectionList.begin();
    vector<Reflection>::const_iterator end = m_reflectionList.end();
    while(iter != end)
    {
      iter->toStream(flux);
      ++iter;
    }
    
    return flux;
  }
  
  istream &
  Crystal::fromStream(istream & flux)
  {
    Object::fromStream(flux);
    FitParameterList::fromStream(flux);
    m_B.fromStream(flux);
    m_U.fromStream(flux);
    
    unsigned int nb_reflections;
    unsigned int i;
    flux >> nb_reflections;
    for(i=0;i<nb_reflections;i++)
    {
      m_reflectionList.push_back(Reflection());
      m_reflectionList[i].fromStream(flux);
    }
    //_computeB();
    return flux;
  }
  
} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::Crystal const & crystal)
{ 
  return crystal.printToStream(flux);
}
