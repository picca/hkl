//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/diffractometer.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class diffractometer, eulerian_diffractometer4C, kappa_diffractometer4C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.14 $

//

// $Log: diffractometer.cpp,v $
// Revision 1.14  2005/10/20 12:48:47  picca
// * right calculation for the number of usable reflections
// close: #976 #977
//
// Revision 1.13.2.1  2005/10/20 12:40:20  picca
// * modification of AngleConfiguration::getAxesNames()
// * add Reflection::isColinear() + test functions
// * add Crystal::isEnoughReflections() + test functions
// * remove crystal::getNumberOfReflectionsForCalculation() what a silly name :)
// * close #976 #977
//
// Revision 1.13  2005/10/05 09:02:33  picca
// merge avec la branche head
//
// Revision 1.12.2.53  2005/09/07 08:20:49  picca
// *** empty log message ***
//
// Revision 1.12.2.51  2005/08/29 07:53:37  picca
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
// Revision 1.12.2.50  2005/08/18 16:30:15  picca
// Mise a jour de la documentation
//
// Revision 1.12.2.49  2005/08/11 15:11:18  picca
// mise a jour de la documentation
//
// Revision 1.12.2.48  2005/08/01 16:17:57  picca
// en cours de modification des classes myvector et mymap
//
// Revision 1.12.2.47  2005/07/29 15:27:06  picca
// Modification de la classe diffractometer pour utiliser l'affinement.
//
// Revision 1.12.2.46  2005/07/28 16:11:30  picca
// en cours de modification de la partie affinement de la classe diffractometer.
//
// Revision 1.12.2.45  2005/07/27 15:50:35  picca
// ajout des test de la partie mode du diffractometre.
//
// Revision 1.12.2.44  2005/07/26 16:09:04  picca
// en cours de travail sur la partie mode du diffractometre.
//
// Revision 1.12.2.43  2005/07/12 16:07:21  picca
// ajout de test pour la classe value et reecriture d'une partie de Crystal pour ne plus utiliser La classe Lattice mais les fitParameters.
//
// Revision 1.12.2.42  2005/06/22 15:04:36  picca
// surcharge de operator[] pour la classe AngleConfiguration
//
// Revision 1.12.2.41  2005/06/21 15:35:18  picca
// Surcharge du template template MyMap pour prendre en compte les map de pointeur
//
// Revision 1.12.2.40  2005/06/20 15:34:58  picca
// ajout des test pour fitParameter
//
// Revision 1.12.2.39  2005/06/20 14:00:16  picca
// version pour le premier device
//
// Revision 1.12.2.38  2005/06/13 16:03:13  picca
// avancement de l'affinement
//
// Revision 1.12.2.37  2005/06/09 13:41:10  picca
// ajout de la class object et reecriture de la class Axe derivant de cette class object
//
// Revision 1.12.2.36  2005/06/09 12:56:15  picca
// ajout Lattice::set et get anisi que des fonctions de test.
//
// Revision 1.12.2.35  2005/06/08 16:22:13  picca
// travail sur l'affinage
//
// Revision 1.12.2.34  2005/06/06 08:59:26  picca
// mise a jour du binding python et de la classe dffractometer
//
// Revision 1.12.2.33  2005/06/03 14:58:58  picca
// version avant modification pour compilation sous windows
//
// Revision 1.12.2.32  2005/06/02 11:45:15  picca
// modification pour obtenir une version utilisable du binding python
//
// Revision 1.12.2.31  2005/05/27 12:30:34  picca
// class: Reflection
//   - ajout contructeur par default
//   - ajout set(const Reflection & reflection) pour mettre Ã  jour un reflection Ã  partir d'une autre
//   - ajout get_angleConfiguration et set_valueConfiguration
//   - ajout get_source, set_source
//   - remplacement getRelevance par get_relevance
//   - remplacement getFlag par get_flag
//
// Revision 1.12.2.30  2005/05/26 14:36:54  picca
// class diffractometer
// - ajout getLattice, setLattice, getReciprocalLattice
// -ajout getCrystalLattice, setCrystalLattice, getCrystalReciprocalLattice
// -ajout des test correspondants.
// -ajout getReflection, getCrystalReflection
// -ajout d'un binding pour python de la classe diffractometer qui utilise la librairie boost_python
// -ajout d'un GUI en python + PyGtk
//
// Revision 1.12.2.29  2005/05/18 07:04:49  picca
// modification de getAxeLimits et computeHKL pour qu'elles utilise des pointeurs.
//
// Revision 1.12.2.28  2005/04/27 07:56:08  picca
// changement de nom de la mÃ©thode workWithCrystal en setCrystal
//
// Revision 1.12.2.27  2005/04/26 14:38:08  picca
// Ajout des fonctions modifyReflection et modifyReflectionOfCrystal Ã  la classe diffractometer
//
// Revision 1.12.2.26  2005/04/22 11:45:44  picca
// Ajout de la fonction setAxeAngle Ã  la classe AngleConfiguration
//
// Revision 1.12.2.25  2005/04/21 08:47:09  picca
// Modifications de Sconstruct pour windows.
//
// Revision 1.12.2.24  2005/04/19 14:01:07  picca
// reecriture du node bissecteur du 4 cercles Eulerien
// -ajout des tests de setMode et computeAngles de la classe diffractoemters
// -ajout des test du mode bissecteur Eulerien 4C.
//
// Revision 1.12.2.23  2005/04/14 09:43:07  picca
// Ajout et test de la fonction computeHKL de la classe difrfactoemeter
//
// Revision 1.12.2.22  2005/04/12 07:09:52  picca
// Réécriture de la classe diffractoemeter
//
// Revision 1.12.2.21  2005/04/06 16:10:38  picca
// Probleme de caractere unicode dans un des commentaires de diffractoemter.h
//
// Revision 1.12.2.20  2005/04/01 10:06:55  picca
// -Typography
// -ajout des fonctions de test sur la class crystal
//
// Revision 1.12.2.19  2005/03/31 14:30:43  picca
// Modification de la classe crystal
// - ajout d'un champ m_name
// - ajout d'un champ m_reflectionList pour stocker les reflections propres au cristal
//
// Modifications des autres classes pour prendre en compte ce changement.
//
// Revision 1.12.2.18  2005/03/31 11:28:26  picca
// Modification de la classe Reflection.
// - ajout de 2 champs:
//   m_source pour sauvegarder l'Ã©tat de la source pour chaque reflection.
//   m_flag pour indiquer si oui ou non on utilise la reflection dans le calcule de U
// - ajout des getSet pour tous les champs de reflection.
// - ajout des test de ces getSet.
//
// Revision 1.12.2.17  2005/03/30 15:52:01  picca
// change the source class to store only the waveLength and the direction of the incidental beam
//
// Revision 1.12.2.16  2005/03/24 08:36:36  picca
// remplacement des fonction getSampleAxe et getDetectorAxe par getAxe dans la classe angleConfiguration
//
// Revision 1.12.2.15  2005/03/23 16:38:53  picca
// -ajout de computeU dans la classe smatrix.
// -ajout de getQ() dans la classe angleConfiguration
// -Test de getQ().
//
// Revision 1.12.2.14  2005/03/23 14:34:51  picca
// -surcharge des operateurs * *= pour les classes svector et smatrix
// -Suppression des methodes RightMultiply and LeftMutiply
//
// Revision 1.12.2.13  2005/03/23 08:37:14  picca
// not it compile
//
// Revision 1.12.2.12  2005/03/23 07:00:27  picca
// major change
// -switch from autotools to scons
// -add the axe and quaternion class
// -modification of the angleconfiguration class
//
// Revision 1.12.2.11  2005/03/11 10:51:59  picca
// la suite ;)
//
// Revision 1.12.2.10  2005/03/11 10:45:29  picca
// -suppresion des attributs m_SizeOfArray, m_numberOfInsertedElements
// - changement de m_ReflectionList vers le type vector<reflection>
//
// Revision 1.12.2.9  2005/03/11 08:36:28  picca
// - suppression de printToScreen
// - ajout de operator<< pour la classe diffractometer
// - ajout d-un fonction getReflection qui retourne un pointeur sur la reflection selectionnée. (voir s'il ne faut pas faire une copie pour éviter les fuites mémoires).
//
// Revision 1.12.2.8  2005/03/10 16:18:55  picca
// -typo
// - remove the printOnScreen function of svector and smatrix
//
// Revision 1.12.2.7  2005/03/03 09:37:16  picca
// scission du fichier diffractometer.cpp en ses classes derivées eulerian4C et kappa4C (pour l'instant vide).
//
// Revision 1.12.2.6  2005/03/02 14:15:22  picca
// -surcharge de l'operateur<< de la classe angleconfiguration en utilisant les fonctions printToStream.
// -Surcharge de l'operateur== pour pouvoir faire les test avec la librairie cppunit.
// -Suppression des fonctions printOnScreens et consor
//
// Revision 1.12.2.5  2005/03/02 12:43:57  picca
// add the operator<< for the anglecalculation classes.
//
// Revision 1.12.2.4  2005/03/02 09:38:41  picca
// chngement des noms de classe pour les configurations
//
// Revision 1.12.2.3  2005/03/02 09:20:22  picca
// modif pour prendre en compte le renommage de angleconfig.h en angleconfiguration.h
//
// Revision 1.12.2.2  2005/03/02 08:13:30  picca
// Prise en compte de la suppression de printOnScreen de la classe source
//
// Revision 1.12  2005/01/27 09:23:53  delos
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
// Class diffractometer to drive experiments. Reference : William R. Busing and Henri A. Levy
// "Angle calculation for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967) Acta Cryst., 22, 457-464.
#include "diffractometer.h"

namespace hkl {

Diffractometer::Diffractometer(void)
{
  m_mode = NULL;
  m_crystal = NULL;

  // On s'occupe de remplir avec les bons affinements la liste.
  m_affinementList.add(new affinement::Simplex());
}

Diffractometer::~Diffractometer(void)
{
  AffinementList::iterator iter = m_affinementList.begin();
  AffinementList::iterator last = m_affinementList.end();

  while(iter != last){
    delete *iter;
    ++iter;
  }
}

//m_source
void
Diffractometer::setWaveLength(double wl)
{
  m_source.setWaveLength(wl);
}

double
Diffractometer::getWaveLength(void) const
{
  return m_source.get_waveLength();
}

//m_currentConfiguration

std::vector<std::string> const
Diffractometer::getAxesNames(void) const
{
  return m_aC->getAxesNames();
}
 
void
Diffractometer::setAxeAngle(std::string const & name,
                            double angle) throw (HKLException)
{
  (*m_aC)[name].set_value(angle);
}

double
Diffractometer::getAxeAngle(std::string const & name) throw (HKLException)
{
  return (*m_aC)[name].get_value();
}

//m_crystalList

std::vector<std::string> const
Diffractometer::getCrystalNames(void) const
{
  return m_crystalList.getNames();
}

std::string const &
Diffractometer::getCurrentCrystalName(void) const throw (HKLException)
{
    if (!m_crystal)
    throw HKLException("No crystal selected",
                       "Please select a crystal",
                       "Diffractometer::getCurrentCrystalName");
    
   return m_crystal->get_name();
}

void
Diffractometer::setCurrentCrystal(std::string const & name) throw (HKLException)
{
  m_crystal = &m_crystalList[name];
}

void
Diffractometer::addNewCrystal(std::string const & name) throw (HKLException)
{
  m_crystalList.add(Crystal(name));
}

void
Diffractometer::setCrystalLattice(std::string const & name,
                                  double a, double b, double c,
                                  double alpha, double beta, double gamma) throw (HKLException)
{
  m_crystalList[name].setLattice(a, b, c, alpha, beta, gamma);
}

void
Diffractometer::getCrystalLattice(std::string const & name,
                                  double * a, double * b, double * c,
                                  double * alpha, double * beta, double * gamma) const throw (HKLException)
{
  m_crystalList[name].getLattice(a, b, c, alpha, beta, gamma);  
}

void
Diffractometer::getCrystalReciprocalLattice(std::string const & name,
                                  double * a, double * b, double * c,
                                  double * alpha, double * beta, double * gamma) const throw (HKLException)
{
  m_crystalList[name].getReciprocalLattice(a, b, c, alpha, beta, gamma);   
}

void
Diffractometer::getCrystalParameterValues(std::string const & crystal_name,
                                          std::string const & parameter_name,
                                          double * value,
                                          double * min,
                                          double * max,
                                          bool * flagFit) const throw (HKLException)
{
  FitParameter const & fitparameter = m_crystalList[crystal_name][parameter_name];
  *value = fitparameter.get_value(); 
  *min = fitparameter.get_min(); 
  *max = fitparameter.get_max(); 
  *flagFit = fitparameter.get_flagFit(); 
}

void
Diffractometer::setCrystalParameterValues(std::string const & crystal_name,
                                          std::string const & parameter_name,
                                          double value,
                                          double min,
                                          double max,
                                          bool flagFit) throw (HKLException)
{
  FitParameter & fitparameter = m_crystalList[crystal_name][parameter_name];
  fitparameter.set_value(value); 
  fitparameter.set_min(min); 
  fitparameter.set_max(max); 
  fitparameter.set_flagFit(flagFit); 
}

smatrix
Diffractometer::getCrystal_UB(std::string const & name) const throw (HKLException)
{
  Crystal const & crystal = m_crystalList[name];
  return crystal.get_U() * crystal.get_B();
}

double
Diffractometer::getCrystalFitness(std::string const & name) throw (HKLException)
{
  return m_crystalList[name].fitness();
}

void
Diffractometer::delCrystal(std::string const & name) throw (HKLException)
{
  m_crystalList.remove(name);
}

void
Diffractometer::copyCrystalAsNew(std::string const & from,
                                 std::string const & to) throw (HKLException)
{
  Crystal crystal(m_crystalList[from]);
  crystal.set_name(to);
  m_crystalList.add(crystal);
}

// reflections

unsigned int
Diffractometer::getCrystalNumberOfReflection(std::string const & name) const throw (HKLException)
{
  return m_crystalList[name].get_reflectionList().size();
}

unsigned int
Diffractometer::addCrystalReflection(std::string const & name,
                                     double h, double k, double l,
                                     int relevance, bool flag) throw (HKLException)
{
  return m_crystalList[name].addReflection(Reflection(*m_aC, m_source, h, k, l, relevance, flag));
}

double
Diffractometer::getCrystalReflectionAxeAngle(std::string const & crystalName, 
                                             unsigned int index,
                                             std::string const & axeName) const throw (HKLException)
{
  return m_crystalList[crystalName].getReflection(index).get_angleConfiguration()[axeName].get_value();
}

void
Diffractometer::delCrystalReflection(std::string const & name,
                                     unsigned int index) throw (HKLException)
{
  m_crystalList[name].delReflection(index);
}

void
Diffractometer::copyCrystalReflectionFromTo(std::string const & from,
                                            unsigned int ifrom,
                                            std::string const & to) throw (HKLException)
{
  Crystal & from_crystal = m_crystalList[from];
  Crystal & to_crystal = m_crystalList[to];

  to_crystal.addReflection(from_crystal.getReflection(ifrom));
}

void
Diffractometer::getCrystalReflectionParameters(std::string const & name,
                                               unsigned int index,
                                               double * h, double * k, double *l,
                                               int * relevance, bool * flag) const throw (HKLException)
{
   Reflection const & R = m_crystalList[name].getReflection(index);
   *h = R.get_h();
   *k = R.get_k();
   *l = R.get_l();
   *relevance = R.get_relevance();
   *flag = R.get_flag();
}

void
Diffractometer::setCrystalReflectionParameters(std::string const & name,
                                               unsigned int index,
                                               double h, double k, double l,
                                               int relevance, bool flag) throw (HKLException)
{
    Reflection & r = m_crystalList[name].getReflection(index);
    r.set_h(h);
    r.set_k(k);
    r.set_l(l);
    r.set_relevance(relevance);
    r.set_flag(flag);
}

// Modes

std::vector<std::string>
Diffractometer::getModeNames(void) const
{
  return m_modeList.getNames();
}

std::string const &
Diffractometer::getCurrentModeName(void) const throw (HKLException)
{
  if (!m_mode)
    throw HKLException("no mode set",
                       "please set a mode",
                       "Diffractometer::getCurrentModeName");
  
  return m_mode->get_name();
}

std::string const &
Diffractometer::getModeDescription(std::string const & name) const throw (HKLException)
{
  return m_modeList[name]->get_description();
}

std::vector<std::string>
Diffractometer::getModeParametersNames(std::string const & name) const throw (HKLException)
{
  return m_modeList[name]->getParametersNames();
}

double
Diffractometer::getModeParameterValue(std::string const & mode_name,
                                      std::string const & parameter_name) const throw (HKLException)
{
  return m_modeList[mode_name]->getParameterValue(parameter_name);
}

void
Diffractometer::setModeParameterValue(std::string const & mode_name,
                                      std::string const & parameter_name,
                                      double value) throw (HKLException)
{
  m_modeList[mode_name]->setParameterValue(parameter_name, value);
}

void
Diffractometer::setCurrentMode(std::string const & name) throw (HKLException)
{
  try{
    m_mode = m_modeList[name];
  } catch (HKLException const &) {
    m_mode = NULL;
    throw;
  }
}

// Affinement functions


std::vector<std::string>
Diffractometer::getAffinementNames(void) const
{
  return m_affinementList.getNames();
}

unsigned int
Diffractometer::getAffinementMaxIteration(std::string const & name) const throw (HKLException)
{
  return m_affinementList[name]->get_nb_max_iteration();
}

void
Diffractometer::setAffinementMaxIteration(std::string const & name, unsigned int max) throw (HKLException)
{
  m_affinementList[name]->set_nb_max_iteration(max);
}

unsigned int
Diffractometer::getAffinementIteration(std::string const & name) const throw (HKLException)
{
  return m_affinementList[name]->get_nb_iteration();
}

double
Diffractometer::affineCrystal(std::string const & crystal_name, std::string const & method_name) throw (HKLException)
{
  Crystal & crystal = m_crystalList[crystal_name];
  Affinement * affinement = m_affinementList[method_name];
  
  unsigned int nb_parameters = crystal.getNumberOfParameterToFit();
  unsigned int nb_reflections = (unsigned int)ceil(nb_parameters / 3.);
  if (crystal.isEnoughReflections(nb_reflections))
  {
    affinement->fit(crystal);
    return affinement->get_fitness();
  } else {
    throw HKLException();
  }
}

//Calculation functions

void
Diffractometer::computeU(void) throw (HKLException)
{
  if (!m_crystal)
    throw HKLException("No crystal selected.",
                       "Please select a crystal before.",
                       "Diffractometer::computeU");
  m_crystal->computeU();
}

void
Diffractometer::computeHKL(double * h, double * k, double * l) throw (HKLException)
{
  if (!m_crystal)
      throw HKLException("No crystal selected.",
                         "Please select a crystal before.",
                         "Diffractometer::computeHKL");
  
  smatrix UB = m_crystal->get_U() * m_crystal->get_B();
  smatrix R = m_aC->getSampleRotationMatrix() * UB;
  double det;
  
  det  =  R.get(0,0)*(R.get(1,1)*R.get(2,2)-R.get(2,1)*R.get(1,2));
  det += -R.get(0,1)*(R.get(1,0)*R.get(2,2)-R.get(2,0)*R.get(1,2));
  det +=  R.get(0,2)*(R.get(1,0)*R.get(2,1)-R.get(2,0)*R.get(1,1));

  if (fabs(det) < constant::math::epsilon_1)
    throw HKLException(
      "det(R) is null",
      "La matrice rotation de la machine n'est pas valide",
      "Diffractometer::computeHKL");
  
  Quaternion const & qi = m_source.get_qi();
  svector q = m_aC->getQ(qi);
  
  double sum;
  
  sum =   q[0] * (R.get(1,1)*R.get(2,2)-R.get(1,2)*R.get(2,1));
  sum += -q[1] * (R.get(0,1)*R.get(2,2)-R.get(0,2)*R.get(2,1));
  sum +=  q[2] * (R.get(0,1)*R.get(1,2)-R.get(0,2)*R.get(1,1));
  *h = sum / det;

  sum =  -q[0] * (R.get(1,0)*R.get(2,2)-R.get(1,2)*R.get(2,0));
  sum +=  q[1] * (R.get(0,0)*R.get(2,2)-R.get(0,2)*R.get(2,0));
  sum += -q[2] * (R.get(0,0)*R.get(1,2)-R.get(0,2)*R.get(1,0));
  *k = sum / det;

  sum =   q[0] * (R.get(1,0)*R.get(2,1)-R.get(1,1)*R.get(2,0));
  sum += -q[1] * (R.get(0,0)*R.get(2,1)-R.get(0,1)*R.get(2,0));
  sum +=  q[2] * (R.get(0,0)*R.get(1,1)-R.get(0,1)*R.get(1,0));
  *l = sum / det;
}

void
Diffractometer::computeAngles(double h, double k, double l) throw (HKLException)
{
  if (!m_mode)
    throw HKLException("m_currentMode is null",
                       "The mode has not been set",
                       "Diffractometer::computeAngles");
  
  if (m_source.get_waveLength() < constant::math::epsilon_1)
    throw HKLException("lamdba is null",
                       "The wave length has not been set",
                       "Diffractometer::computeAngles");

  if ((fabs(h) < constant::math::epsilon_1) 
        && (fabs(k) < constant::math::epsilon_1)
        && (fabs(l) < constant::math::epsilon_1))
    throw HKLException("(h,k,l) is null",
                       "check your parameters",
                       "Diffractometer::computeAngles");
  
  if (!m_crystal)
    throw HKLException("No crystal selected.",
                       "Please select a crystal before.",
                       "Diffractometer::computeAngles");
  
  if (m_crystal->get_B() == smatrix())
    throw HKLException("null B matrix",
                       "Please set the crystal parameters correctly",
                       "Diffractometer::computeAngles");
  
  if (m_crystal->get_U() == smatrix())
    throw HKLException("null U matrix",
                       "Please compute the orientation matrix first",
                       "Diffractometer::computeAngles");

  try
  {
    smatrix UB = m_crystal->get_U() * m_crystal->get_B();
    double const & lambda = m_source.get_waveLength();
    
    m_mode->computeAngles(h, k, l, UB, lambda, *m_aC);
  } catch (const HKLException &) {
    throw;
  }
}

std::ostream &
Diffractometer::printToStream(std::ostream & flux) const
{
  //flux << std::showpoint << std::fixed << std::showpos;
  flux << std::endl;
  flux << "Diffractometer: " << get_name() << std::endl;
  flux << std::endl;
  flux << "Current angle configuration" << std::endl
    << *m_aC;
  flux << *m_mode << std::endl;
  
  CrystalList::const_iterator iter = m_crystalList.begin();
  CrystalList::const_iterator end = m_crystalList.end();
  while(iter != end){
    flux << iter->second;
    iter++;
  }
  return flux;
}

} // namespace hkl

std::ostream &
operator <<(std::ostream& flux, hkl::Diffractometer const & diffractometer)
{ 
  return diffractometer.printToStream(flux);
}

