#ifndef DIFFRACTOMETER
#define DIFFRACTOMETER

#include "mode.h"
#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "reflection.h"
#include "angleconfig.h"

/// The abstract base class to define all different kinds
/// of diffractometers and drive experiments.
class diffractometer
{
public:

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \return The computed sample of angles.
  /// \brief The main function to get a sample of angles from (h,k,l).
  virtual angleConfiguration* computeAngles(
    double h, double k, double l) = 0;

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \return The computed sample of angles.
  /// \brief Designed for testing with Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(
    double h, double k, double l) = 0;

  /// Solve a linear system Ax = b where A is the product of the rotation matrices 
  /// OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
  /// scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param ac The diffractometer current angle configuration.
  /// \exception when det(A)=0.
  virtual void computeHKL(
    double& h, double& k, double& l, angleConfiguration* ac) = 0;

  /// Compute the matrix R describing a complex rotation
  /// involving all the diffractometer circles.
  /// \brief Return the rotation matrix R for the current configuration.
  virtual smatrix computeR() = 0;
  /// Return the rotation matrix R for the given configuration ac1.
  virtual smatrix computeR(angleConfiguration* ac1) = 0;
  /// Set the angle configuration and compute the 
  /// corresponding rotation matrices according to the
  /// chosen rotation axes.
  virtual void setAngleConfiguration(angleConfiguration* ac1) = 0;

  /// Compute the orientation matrix from two basic reflections.
  virtual smatrix computeU(
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2) = 0;
  /// Compute the orientation matrix from two basic reflections.
  virtual smatrix computeU(reflection& r1, reflection& r2) = 0;

  // virtual smatrix computeUB() const = 0;
  virtual ~diffractometer();
  virtual void printOnScreen() const;

  /// Change the current computational mode.
  virtual void setMode(mode::diffractometer_mode currentMode)=0;

  /// Return the orientation matrix.
  smatrix get_U() const
  {return m_U;}

  /// Return the product of the orientation matrix by the crystal matrix.
  smatrix get_UB() const
  {return m_UB;}

  /// Get h from the array of experimental reflections.
  double getReflection_h(int index) const;
  /// Get k from the array of experimental reflections.
  double getReflection_k(int index) const;
  /// Get l from the array of experimental reflections.
  double getReflection_l(int index) const;

  void setReflection(angleConfiguration* ac,
    double h, double k, double l, 
    reflection::relevance r, int index);

  reflection::relevance 
    getReflection_Relevance(int index) const;

  angleConfiguration*
    getReflection_AngleConfiguration(int index) const;

  /// Change the crystal from the direct lattice parameters.
  void setCrystal(
    double alpha1, double alpha2, double alpha3,
    double a1, double a2, double a3);

  /// Change the crystal where the reciprocal lattice
  /// and matrix have already been computed.
  void setCrystal(const cristal& C);

  /// Change the light source wave length as it is
  /// something usual in an experiment.
  void setWaveLength(double wl);

protected:
  /// This orthogonal matrix relates the crystal 
  /// cartesian system to the phi-axis system. It is 
  /// computed from at least two relevant reflections.
  /// \brief The orientation matrix.
  smatrix m_U;
  /// UB = U*B where B defines the crystal matrix and 
  /// U the orientation matrix. UB relates the reciprocal
  /// space to the PHI-axis system.
  /// \brief Product U * B.
  smatrix m_UB;

  /// The mode describes the way we use the diffractometer.
  mode* m_currentMode;
  /// The light source and its wave length.
  source m_currentSource;
  /// The crystal direct and reciprocal parameters and its matrix B.
  cristal m_currentCristal;
  /// The array to store up to 100 experiment results.
  reflection* m_reflectionList;
  /// Size of the reflection array.
  const int m_sizeOfArray;
  /// The number of reflections inserted into m_reflectionList.
  int m_numberOfInsertedElements;
  /// The current diffractometer angle configuration.
  angleConfiguration* m_currentConfiguration;

  /// Commun constructor 
  /// - protected to make sure this class is abstract.
  diffractometer(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2);

  /// Constructor designed for testing purposes
  /// - protected to make sure this class is abstract.
  diffractometer(
    cristal currentCristal, source currentSource);

  /// Default constructor
  /// - protected to make sure this class is abstract.
  diffractometer();
};

/// The eulerian 4-circle diffractometer. 
/// William R. Busing and Henri A. Levy "Angle calculation 
/// for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
/// <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
class eulerianDiffractometer4C : public diffractometer
{
protected:
  /// The matrix corresponding to the first circle.
  smatrix m_OMEGA;
  /// The matrix corresponding to the second circle.
  smatrix m_CHI;
  /// The matrix corresponding to the third circle.
  smatrix m_PHI;
  /// The matrix corresponding to the fourth circle i.e. the detector.
  smatrix m_2THETA;
  /// To reverse the first circle rotation sense.
  bool m_directOmega;
  /// To reverse the second circle rotation sense.
  bool m_directChi;
  /// To reverse the third circle rotation sense.
  bool m_directPhi;
  /// To reverse the fourth circle rotation sense i.e. the detector.
  bool m_direct2Theta;

public:
  /// Commun constructor.
  eulerianDiffractometer4C(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2,
    mode::diffractometer_mode currentMode);

  /// Constructor designed for testing purposes.
  eulerianDiffractometer4C(
    cristal currentCristal, source currentSource,
    mode::diffractometer_mode currentMode);

  /// Default constructor.
  eulerianDiffractometer4C();

  ~eulerianDiffractometer4C();

  /// Compute the rotation matrix R for the current configuration.
  smatrix computeR();

  /// Compute the rotation matrix R for a given configuration.
  smatrix computeR(angleConfiguration* ac1);

  /// Change the current computational mode.
  void setMode(mode::diffractometer_mode currentMode);

  /// Set the angle configuration and compute the 
  /// corresponding rotation matrices according to the
  /// chosen rotation axes.
  void setAngleConfiguration(angleConfiguration* ac1);

  /// William R. Busing and Henri A. Levy "Angle calculation
  /// for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967)
  /// <A HREF="http://journals.iucr.org/index.html">  Acta Cryst.</A>, <B>22</B>, 457-464.
  /// Compute h1c and h2c from equation (17) <BR>
  /// h1c = B.h1 <BR>
  /// h2c = B.h2 <BR>
  /// h1phi = U.h1c <BR>
  /// h2phi = U.h2c <BR>
  /// u1phi = R1t.(1,0,0) <BR>
  /// u2phi = R2t.(1,0,0) <BR>
  /// h1phi // u1phi <BR>
  /// h2phi // P(u1phi,u2phi) <BR>
  /// \brief Compute the orientation matrix from two reflections.
  smatrix computeU(
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2);

  /// William R. Busing and Henri A. Levy "Angle calculation
  /// for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967)
  /// <A HREF="http://journals.iucr.org/index.html">  Acta Cryst.</A>, <B>22</B>, 457-464.
  /// Compute h1c and h2c from equation (17) <BR>
  /// h1c = B.h1 <BR>
  /// h2c = B.h2 <BR>
  /// h1phi = U.h1c <BR>
  /// h2phi = U.h2c <BR>
  /// u1phi = R1t.(1,0,0) <BR>
  /// u2phi = R2t.(1,0,0) <BR>
  /// h1phi // u1phi <BR>
  /// h2phi // P(u1phi,u2phi) <BR>
  /// \brief Compute the orientation matrix U from two reflections and set the UB matrix.
  /// \param r1 The first reflection.
  /// \param r2 The second reflection.
  /// \return The orientation matrix U.
  smatrix computeU(reflection& r1, reflection& r2);

  /// \brief The main function to compute a diffractometer configuration from a given (h, k, l).
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \return The computed sample of angles.
  /// \sa eulerian_bissectorMode4C::computeAngles()
  angleConfiguration* computeAngles(double h, double k, double l);

  /// \brief Test function to compute a diffractometer configuration from a given (h, k, l).
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \return The computed sample of angles.
  /// \sa eulerian_bissectorMode4C::computeAngles_Rafin()
  angleConfiguration* computeAngles_Rafin(double h, double k, double l);

  /// Solve a linear system Ax = b where A is the product of the rotation matrices 
  /// OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
  /// scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param ac The diffractometer current angle configuration.
  /// \exception when det(A)=0.
  void computeHKL(double& h, double& k, double& l, angleConfiguration* ac);

  void printOnScreen() const;

  /// Tests from 01 to 10 are basic tests to make sure
  /// computing B, U  and angles from (h,k,l) are OK. <BR>
  /// Tests from 11 to 20 are the same with differed settings. <BR>
  /// Tests from 21 to 30 use Rafin algorithm to perform checks. <BR>
  /// Tests from 31 to 40 compute angles from (h,k,l) and then (h,k,l) from these angles. <BR>
  /// \brief Test all the main functionnalities.
  /// \return 0 if everything's fine, otherwise the number of the failing test.
  static int test_eulerian4C();

};

class kappaDiffractometer4C : public diffractometer
{
protected:
  /// The matrix corresponding to the first circle.
  smatrix m_OMEGA;
  /// The matrix corresponding to the second circle.
  smatrix m_KAPPA;
  /// The matrix corresponding to the third circle.
  smatrix m_PHI;
  /// The matrix corresponding to the fourth circle i.e. the detector.
  smatrix m_2THETA;
  /// The matrix corresponding to the diffractometer inclination.
  smatrix m_ALPHA;
  /// The opposite matrix corresponding to the diffractometer inclination m_OPP_ALPHA = -m_ALPHA.
  smatrix m_OPP_ALPHA;
  /// To reverse the first circle rotation sense.
  bool m_directOmega;
  /// To reverse the second circle rotation sense.
  bool m_directKappa;
  /// To reverse the third circle rotation sense.
  bool m_directPhi;
  /// To reverse the fourth circle rotation sense i.e. the detector.
  bool m_direct2Theta;
  /// The incident angle.
  double m_kappa;

public:
  kappaDiffractometer4C(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2,
    mode::diffractometer_mode currentMode);

  ~kappaDiffractometer4C();

  /// Compute the rotation for the current configuration.
  smatrix computeR();

  /// Compute the rotation for a given configuration.
  smatrix computeR(angleConfiguration* ac1);

  /// Set the angle configuration and compute the 
  /// corresponding rotation matrices according to the
  /// chosen rotation axes.
  void setAngleConfiguration(angleConfiguration* ac1);

  smatrix computeU(
    angleConfiguration* ac1, double h1, double k1, double l1,
    angleConfiguration* ac2, double h2, double k2, double l2);

  smatrix computeU(
    reflection& r1, reflection& r2);

  void printOnScreen() const;

};

#endif
