//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/diffractometer.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class diffractometer, eulerianDiffractometer4C, kappaDiffractometer4C, eulerianDiffractometer6C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.16 $

//

// $Log: diffractometer.h,v $
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

#include "mode.h"
#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "reflection.h"
#include "angleconfig.h"
#include "HKLException.h"

/**
 * The abstract base class to define all different kinds of diffractometers and drive experiments.
 */
class diffractometer
{
protected:
  /**
   * \brief The orientation matrix.
   *
   * This orthogonal matrix relates the crystal cartesian system to the phi-axis system.
   * It is computed from at least two relevant reflections.
   */
  smatrix m_U;
  
  /**
   * \brief Product U * B.
   *
   * UB = U*B where B defines the crystal matrix and U the orientation matrix.
   * UB relates the reciprocal space to the PHI-axis system.
   */
  smatrix m_UB;

  mode* m_currentMode; //!< The mode describes the way we use the diffractometer.
  cristal m_currentCristal; //!< The crystal direct and reciprocal parameters and its matrix B.
  source m_currentSource; //!< The light source and its wave length.
  reflection* m_reflectionList; //!< The array to store up to 100 experiment results.
  const int m_sizeOfArray; //!< Size of the reflection array.
  int m_numberOfInsertedElements; //!< The number of reflections inserted into m_reflectionList.
  angleConfiguration* m_currentConfiguration; //!< The current diffractometer angle configuration.
 
  /**
   * \brief Default constructor
   *
   * - protected to make sure this class is abstract.
   */
  diffractometer();
  
  /**
   * \bried Constructor
   * @param currentCristal 
   * @param currentSource 
   * @param reflection1 
   * @param reflection2 
   * 
   * Protected to make sure this class is abstract.
   */
  diffractometer(cristal currentCristal, source currentSource, reflection& reflection1, reflection& reflection2);

  /**
   * \brief Constructor
   * @param currentCristal 
   * @param currentSource 
   *
   * Constructor designed for testing purposes - protected to make sure this class is abstract.
   */
  diffractometer(cristal currentCristal, source currentSource);

 
public:

  /**
   * \brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @return The computed sample of angles.
   *
   *  The main function to get a sample of angles from (h,k,l).
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l) = 0;

  /**
   * \brief Designed for testing with Rafin algorithm.
   * @param h  The scaterring vector first element.
   * @param k  The scaterring vector second element.
   * @param l  The scaterring vector third element.
   * @return  The computed sample of angles.
   *
   * Designed for testing with Rafin algorithm.
   */
  virtual angleConfiguration* computeAngles_Rafin(double h, double k, double l) = 0;

  /**
   * \brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param ac The diffractometer current angle configuration. 
   * \exception det(A)=0
   * 
   * Solve a linear system Ax = b where A is the product of the rotation matrices 
   * OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
   * scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
   */
  virtual void computeHKL(double& h, double& k, double& l, angleConfiguration* ac) = 0;

  /**
   * \brief Return the rotation matrix R for the current configuration.
   * 
   * Compute the matrix R describing a complex rotation
   * involving all the diffractometer circles.
   */
  virtual smatrix computeR() = 0;

  /**
   * \brief Return the rotation matrix R for the given configuration ac1.
   * @param ac1 the angle configuration
   *
   * Compute the matrix R describing a complex rotation
   * involving all the diffractometer circles.
   */
  virtual smatrix computeR(angleConfiguration* ac1) = 0;

  /**
   * \brief Set the angle configuration and compute the corresponding rotation matrices.
   * @param ac1 the angle configuration
   * 
   * Compute the matrix R describing a complex rotation
   * involving all the diffractometer circles.
   */
  virtual void setAngleConfiguration(angleConfiguration* ac1) = 0;

  /**
   * \brief Compute the orientation matrix from two basic non-parallel reflections.
   * @param ac1 The first angle configuration corresponding to (h1,k1,l1).
   * @param h1 The first reflection (h,k,l) first  component.
   * @param k1 The first reflection (h,k,l) second component.
   * @param l1 The first reflection (h,k,l) third  component.
   * @param ac2 The second reflection (h,k,l) first  component.
   * @param h2 The second reflection (h,k,l) second component.
   * @param k2 The second reflection (h,k,l) third  component.
   * @param l2 The second angle configuration corresponding to (h2,k2,l2).
   * @return The orientation matrix U.
   *
   * Compute the orientation matrix from two basic non-parallel reflections.
   */
  virtual smatrix computeU(
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2) = 0;

  /**
   * \brief Compute the orientation matrix from two basic non-parallel reflections.
   * @param r1 The first reflection.
   * @param r2 The second reflection.
   * @return The orientation matrix U.
   *
   * Compute the orientation matrix from two basic non-parallel reflections.
   */
  virtual smatrix computeU(reflection& r1, reflection& r2) = 0;

  /**
   * \brief Destructor
   */
  virtual ~diffractometer();

  virtual void printOnScreen() const; //!< Print the content of the fields.

  virtual void setMode(mode::diffractometer_mode currentMode)=0; //!< Change the current computational mode.

  /// Return the orientation matrix.
  smatrix get_U() const {return m_U;}/// Return the orientation matrix.

  /// Return the product of the orientation matrix by the crystal matrix.
  smatrix get_UB() const {return m_UB;}/// Return the product of the orientation matrix by the crystal matrix.

  /**
   * \brief Get h from the array of experimental reflections.
   * @param index 
   * @return h
   *
   * Get h from the array of experimental reflections.
   */
  double getReflection_h(int index) const;
  
  /**
   * \brief Get k from the array of experimental reflections.
   * @param index 
   * @return k
   *
   * Get k from the array of experimental reflections.
   */
  double getReflection_k(int index) const;
  
  /**
   * \brief Get l from the array of experimental reflections.
   * @param index 
   * @return l
   *
   * Get l from the array of experimental reflections.
   */
  double getReflection_l(int index) const;

  /**
   * \brief set a reflection
   * @param ac The angle configuration of the reflection
   * @param h of the reflection
   * @param k of the reflection
   * @param l of the reflection
   * @param r relevance of the reflection
   * @param index position of the reflection in the reflection list
   *
   * set a reflection
   */
  void setReflection(angleConfiguration* ac, double h, double k, double l, reflection::relevance r, int index);

  /**
   * \brief Get a reflection relevance
   * @param index the index position of the reflection in the reflection list.
   * @return The reflection relevance
   *
   * Get a reflection relevance corresponding to the index position
   * in the reflection list.
   */
  reflection::relevance getReflection_Relevance(int index) const;

  /**
   * \brief Get the angleconfiguration of the index reflection
   * @param index position of the reflection in the reflection list
   * @return The angle configuration
   *
   * Get the angleconfiguration of the index reflection.
   */
  angleConfiguration* getReflection_AngleConfiguration(int index) const;

  /**
   * \brief Change the crystal from the direct lattice parameters.
   * @param alpha1 angle between (a2,a3)
   * @param alpha2 angle between (a1,a3)
   * @param alpha3 angle between (a1,a2)
   * @param a1 first length
   * @param a2 second length
   * @param a3 third length
   *
   * Change the crystal from the direct lattice parameters.
   */
  void setCrystal(double alpha1, double alpha2, double alpha3, double a1, double a2, double a3);

  /**
   * \brief Change the crystal where the reciprocal lattice and matrix have already been computed.
   * @param C the cristal we are copying from.
   *
   * Change the crystal where the reciprocal lattice and matrix have already been computed.
   */
  void setCrystal(const cristal& C);

  /**
   * \brief Change the light source wave length as it is something usual in an experiment.
   * @param wl The new wave length.
   *
   * Change the light source wave length as it is something usual in an experiment.
   */
  void setWaveLength(double wl);


};

/**
 * The eulerian 4-circle diffractometer.
 * William R. Busing and Henri A. Levy "Angle calculation for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
 * <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
 */
class eulerianDiffractometer4C : public diffractometer
{
protected:
  smatrix m_OMEGA; //!< The matrix corresponding to the first circle.
  smatrix m_CHI; //!< The matrix corresponding to the second circle.
  smatrix m_PHI; //!< The matrix corresponding to the third circle.
  smatrix m_2THETA; //!< The matrix corresponding to the fourth circle i.e. the detector.
  bool m_directOmega; //!< To reverse the first circle rotation sense.
  bool m_directChi; //!< To reverse the second circle rotation sense.
  bool m_directPhi; //!< To reverse the third circle rotation sense.
  bool m_direct2Theta; //!< To reverse the fourth circle rotation sense i.e. the detector.

public:
  
  /**
   * \brief Default constructor.
   * @return a new eulerianDiffractometer4C diffractometer.
   *
   * Default constructor.
   */
  eulerianDiffractometer4C();
  
  /**
   * \brief Commun constructor.
   * @param currentCristal The cristal we are studying.
   * @param currentSource The source parameters.
   * @param reflection1 The first reflection to compute the U matrix.
   * @param reflection2 The second reflection to compute the U matrix.
   * @param currentMode The mode used by the diffractometer.
   * @return A new eulerianDiffractometer4C diffractometer.
   *
   *  Commun constructor.
   */
  eulerianDiffractometer4C(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2, mode::diffractometer_mode currentMode);

  /**
   * \brief Constructor designed for testing purposes.
   * @param currentCristal The cristal we are studying.
   * @param currentSource The source parameters.
   * @param currentMode The mode used by the diffractometer.
   * @return A new eulerianDiffractometer4C diffractometer.
   */
  eulerianDiffractometer4C(cristal currentCristal, source currentSource, mode::diffractometer_mode currentMode);

  /**
   * \brief Constructor designed for the 6C diffractometer.
   * @param currentCristal The cristal we are studying.
   * @param currentSource The source parameters.
   * @return A new eulerianDiffractometer4C diffractometer.
   *
   * Constructor designed for the 6C diffractometer.
   */
  eulerianDiffractometer4C(cristal currentCristal, source currentSource);

  /**
   * \brief Destructor
   *
   * Destructor
   */
  virtual ~eulerianDiffractometer4C();

  /// Compute the rotation matrix R for the current configuration.
  /**
   * \brief Compute the rotation matrix R for the current configuration.
   * @return the rotation matrix R
   *
   * Compute the rotation matrix R for the current configuration.
   */
  virtual smatrix computeR();
  
  /**
   * \brief Compute the rotation matrix R for a given configuration.
   * @param ac1 The angle configuration
   * @return The rotation matrix R
   *
   * Compute the rotation matrix R for a given configuration.
   */
  virtual smatrix computeR(angleConfiguration* ac1);

  /**
   * Change the current computational mode.
   * @param currentMode The mode you want to use.
   *
   *  Change the current computational mode.
   */
  virtual void setMode(mode::diffractometer_mode currentMode);

  /**
   * \brief Set the angle configuration and compute the corresponding rotation matrix.
   * @param ac1 The angle configuration
   *
   * Set the angle configuration and compute the corresponding rotation matrix.
   */
  virtual void setAngleConfiguration(angleConfiguration* ac1);

  /**
   * \brief Compute the orientation matrix from two non-parallel reflections and set the UB matrix.
   * @param ac1 The first  angle configuration corresponding to (h1,k1,l1).
   * @param h1 The first  reflection (h,k,l) first  component.
   * @param k1 The first  reflection (h,k,l) second component.
   * @param l1 The first  reflection (h,k,l) third  component.
   * @param ac2 The second angle configuration corresponding to (h2,k2,l2).
   * @param h2 The second reflection (h,k,l) first  component.
   * @param k2 The second reflection (h,k,l) second component.
   * @param l2 The second reflection (h,k,l) third  component.
   * @return The orientation matrix U.
   *
   * William R. Busing and Henri A. Levy "Angle calculation
   * for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967)
   * <A HREF="http://journals.iucr.org/index.html">  Acta Cryst.</A>, <B>22</B>, 457-464.
   * Compute h1c and h2c from equation (17) <BR>
   * h1c = B.h1 <BR>
   * h2c = B.h2 <BR>
   * h1phi = U.h1c <BR>
   * h2phi = U.h2c <BR>
   * u1phi = R1t.(1,0,0) <BR>
   * u2phi = R2t.(1,0,0) <BR>
   * h1phi // u1phi <BR>
   * h2phi // P(u1phi,u2phi) <BR>
   */
  virtual smatrix computeU(
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2);

  /**
   * \brief Compute the orientation matrix U from two non-parallel reflections and set the UB matrix.
   * @param r1 The first reflection.
   * @param r2 The second reflection.
   * @return The orientation matrix U.
   *
   * William R. Busing and Henri A. Levy "Angle calculation
   * for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967)
   * <A HREF="http://journals.iucr.org/index.html">  Acta Cryst.</A>, <B>22</B>, 457-464.
   * Compute h1c and h2c from equation (17) <BR>
   * h1c = B.h1 <BR>
   * h2c = B.h2 <BR>
   * h1phi = U.h1c <BR>
   * h2phi = U.h2c <BR>
   * u1phi = R1t.(1,0,0) <BR>
   * u2phi = R2t.(1,0,0) <BR>
   * h1phi // u1phi <BR>
   * h2phi // P(u1phi,u2phi) <BR>
   */
  virtual smatrix computeU(reflection& r1, reflection& r2);

  /**
   * @brief The main function to compute a diffractometer configuration from a given (h, k, l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @return The computed sample of angles.
   * \sa eulerian_bissectorMode4C::computeAngles()
   *
   * The main function to compute a diffractometer configuration from a given (h, k, l).
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l) throw (HKLException);

  /**
   * @brief Test function to compute a diffractometer configuration from a given (h, k, l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @return  The computed sample of angles.
   * \sa eulerian_bissectorMode4C::computeAngles_Rafin()
   *
   * Test function to compute a diffractometer configuration from a given (h, k, l).
   */
  angleConfiguration* computeAngles_Rafin(double h, double k, double l) throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param ac The diffractometer current angle configuration.
   * @exception det(A)=0
   *
   * Solve a linear system Ax = b where A is the product of the rotation matrices 
   * OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
   * scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
   */
  virtual void computeHKL(double& h, double& k, double& l, angleConfiguration* ac) throw (HKLException);

  virtual void printOnScreen() const; //!< print the diffractometer configuration on screen.

  /**
   * @brief Test all the main functionnalities.
   * @return 0 if everything's fine, otherwise the number of the failing test.
   *
   * Tests from 01 to 10 are basic tests to make sure
   * computing B, U  and angles from (h,k,l) are OK.<BR>
   * Tests from 11 to 20 are the same with differed settings. <BR>
   * Tests from 21 to 30 use Rafin algorithm to perform checks. <BR>
   * Tests from 31 to 40 compute angles from (h,k,l) and then (h,k,l) from these angles. <BR>
   */
  static int test_eulerian4C();

};

/**
 *  @brief This class describes a four-circle Kappa diffractometer.
 * 
 * The 4C Kappa diffractometer can be seen as a 4C eulerian one provided that we use some formula from the
 * MHATT-CAT, Advanced Photon Source, Argonne National Laboratory (
 * <A HREF="http://www.mhatt.aps.anl.gov/~walko/kappa.pdf">MHATT-CAT’s Newport Kappa Diffractometer</A>
 * written by Donald A. Walko). Other interesting documentation can be found at the 
 * <A HREF="http://www.px.nsls.bnl.gov/kappa.html">Brookhaven National Laboratory</A>
 */
class kappaDiffractometer4C : public diffractometer
{
protected:
  smatrix m_OMEGA; //!< The matrix corresponding to the first circle.
  smatrix m_KAPPA; //!< The matrix corresponding to the second circle.
  smatrix m_PHI; //!< The matrix corresponding to the third circle.
  smatrix m_2THETA; //!< The matrix corresponding to the detector circle.
  smatrix m_ALPHA; //!< The matrix corresponding to the diffractometer inclination.
  smatrix m_OPP_ALPHA; //!< The opposite matrix corresponding to the diffractometer inclination m_OPP_ALPHA = -m_ALPHA.
  bool m_directOmega; //!< To reverse the first circle rotation sense.
  bool m_directKappa; //!< To reverse the second circle rotation sense.
  bool m_directPhi; //!< To reverse the third circle rotation sense.
  bool m_direct2Theta; //!< To reverse the detector circle rotation sense.
  double m_kappa; //!< The incident angle.

public:

  /**
   * @brief Common Constructor
   * @param currentCristal The cristal we are studying.
   * @param currentSource The source
   * @param reflection1 The first reflection
   * @param reflection2 The second reflection 
   * @param currentMode The calculation mode of the diffractometer
   * @return a new diffractometer
   *
   * Common constructor
   */
  kappaDiffractometer4C(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2, mode::diffractometer_mode currentMode);

  /**
   * @brief Destructor
   *
   * Destructor
   */
  virtual ~kappaDiffractometer4C();

  /**
   * @brief Compute the rotation for the current configuration.
   * @return The rotation matrix R
   *
   * Compute the rotation for the current configuration.
   */
  virtual smatrix computeR();

  /**
   * @brief Compute the rotation for a given configuration.
   * @param ac1 The angle configuration of the diffractometer
   * @return The rotation matrix R
   *
   * Compute the rotation for a given configuration.
   */
  virtual smatrix computeR(angleConfiguration* ac1);

  /**
   * @brief Set the angle configuration and compute the corresponding rotation matrices.
   * @param ac1 The angle configuration of the diffractometer
   *
   * Set the angle configuration and compute the corresponding rotation matrices.
   */
  virtual void setAngleConfiguration(angleConfiguration* ac1);
  
   /**
   * @brief Compute the orientation matrix from two non-parallel reflections and set the UB matrix.
   * @param ac1 The first  angle configuration corresponding to (h1,k1,l1).
   * @param h1 The first  reflection (h,k,l) first  component.
   * @param k1 The first  reflection (h,k,l) second component.
   * @param l1 The first  reflection (h,k,l) third  component.
   * @param ac2 The second angle configuration corresponding to (h2,k2,l2).
   * @param h2 The second reflection (h,k,l) first  component.
   * @param k2 The second reflection (h,k,l) second component.
   * @param l2 The second reflection (h,k,l) third  component.
   * @return The orientation matrix U.
   *
   * Compute the orientation matrix from two non-parallel reflections and set the UB matrix.
   */
  virtual smatrix computeU(
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2);
  
  /**
   * @brief Compute the orientation matrix U from two non-parallel reflections and set the UB matrix.
   * @param r1 The first reflection.
   * @param r2 The second reflection.
   * @return The orientation matrix U.
   *
   * Compute the orientation matrix U from two non-parallel reflections and set the UB matrix.
   */
  virtual smatrix computeU(reflection& r1, reflection& r2);

  virtual void printOnScreen() const; //!< print the diffractometer configuration on screen.

};

/**
 * The eulerian 6-circle diffractometer as described in 
 * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
 * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
 * Two circles have been added from a 4C diffractometer, MU for the crystal and NU formula
 * the detector. According to H. You conventions the circle previously called Omega
 * has been renamed Eta and the detector circle called 2Theta has been renamed Delta.
 */
class eulerianDiffractometer6C : public eulerianDiffractometer4C
{
protected:
  smatrix m_MU; //!< The matrix corresponding to the fourth circle.
  smatrix m_NU; //!< The matrix corresponding to the detector second circle.
  bool m_directMu; //!< To reverse the fourth circle rotation sense.
  bool m_directNu; //!< To reverse the rotation sense of the detector second circle.

public:
    
  /**
   * @brief Default constructor
   * @return A new diffractometer
   */
  eulerianDiffractometer6C();

  /**
   * @brief Commun constructor.
   * @param currentCristal The cristal we are studying.
   * @param currentSource The source
   * @param reflection1 The first reflection used to compute the U matrix
   * @param reflection2 The second reflection used to comute the U matrix
   * @param currentMode The mode of calculation of the diffractometer.
   * @return A new diffractometer
   *
   * Commun constructor.
   */
  eulerianDiffractometer6C(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2, mode::diffractometer_mode currentMode);


  /**
   * @brief Destructor
   *
   * Destructor
   */
  virtual ~eulerianDiffractometer6C();

  /**
   * @brief Compute the rotation matrix R for the current configuration.
   * @return The rotation matrix R
   *
   * Compute the rotation matrix R for the current configuration.
   */
  virtual smatrix computeR();

  /**
   * @brief Compute the rotation matrix R for a given configuration.
   * @param ac1 The angle configuration.
   * @return The rotation matrix R
   *
   * Compute the rotation matrix R for a given configuration.
   */
  virtual smatrix computeR(angleConfiguration* ac1);

  /**
   * @brief Change the current computational mode.
   * @param currentMode The new mode you want use for the diffractometer
   *
   * Change the current computational mode.
   */
  virtual void setMode(mode::diffractometer_mode currentMode);

  /**
   * @brief Set the angle configuration and compute the corresponding rotation matrices.
   * @param ac1 THe angle configuration
   *
   * Set the angle configuration and compute the corresponding rotation matrices.
   */
  virtual void setAngleConfiguration(angleConfiguration* ac1);

  /**
   * @brief Compute the orientation matrix from two non-parallel reflections and set the UB matrix.
   * @param ac1 The first  angle configuration corresponding to (h1,k1,l1).
   * @param h1 The first  reflection (h,k,l) first  component.
   * @param k1 The first  reflection (h,k,l) second component.
   * @param l1 The first  reflection (h,k,l) third  component.
   * @param ac2 The second angle configuration corresponding to (h2,k2,l2).
   * @param h2 The second reflection (h,k,l) first  component.
   * @param k2 The second reflection (h,k,l) second component.
   * @param l2 The second reflection (h,k,l) third  component.
   * @return The orientation matrix U.
   *
   * Compute the orientation matrix from two non-parallel reflections and set the UB matrix.
   */
  virtual smatrix computeU(
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2);

  /**
   * @brief Compute the orientation matrix U from two non-parallel reflections and set the UB matrix.
   * @param r1 The first reflection.
   * @param r2 The second reflection.
   * @return The orientation matrix U.
   *
   * Compute the orientation matrix U from two non-parallel reflections and set the UB matrix.
   */
  virtual smatrix computeU(reflection& r1, reflection& r2);

  /**
   * @brief The main function to compute a diffractometer configuration from a given (h, k, l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @return The computed sample of angles.
   * \sa eulerian_bissectorMode4C::computeAngles()
   *
   * The main function to compute a diffractometer configuration from a given (h, k, l).
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l) throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param ac The diffractometer current angle configuration.
   * @exception det(A)=0
   *
   * Solve a linear system Ax = b where A is the product of the rotation matrices 
   * OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
   * scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
   */
  virtual void computeHKL(double& h, double& k, double& l, angleConfiguration* ac) throw (HKLException);

  virtual void printOnScreen() const; //!< print the diffractometer configuration on screen.

  /**
   * @brief Test all the main functionnalities.
   * @return 0 if everything's fine, otherwise the number of the failing test.
   */
  static int test_eulerian6C();

};

#endif // _DIFFRACTOMETER_H_
