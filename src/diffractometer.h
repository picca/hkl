/// Class diffractometer to drive experiments. Reference :
/// William R. Busing and Henri A. Levy "Angle calculation
/// for 3- and 4- Circle X-ray and Neutron Diffractometer"
/// (1967) Acta Cryst., 22, 457-464.
#ifndef DIFFRACTOMETER
#define DIFFRACTOMETER

#include "mode.h"
#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "reflection.h"
#include "angleconfig.h"

/// The abstract base class to define all
/// different kinds of diffractometers.
class diffractometer
{
public:

  /// Return the matrix describing a complex rotation
  /// involving all the diffractometer circles. Return
  /// the matrix for the current configuration.
  virtual smatrix computeR() = 0;
  /// Return the matrix for the given configuration.
  virtual smatrix computeR(angleConfiguration* ac1) = 0;
  /// Set the angle configuration and compute the 
  /// corresponding rotation matrices according to the
  /// chosen rotation axes.
  virtual void setAngleConfiguration(
    angleConfiguration* ac1) = 0;

  virtual smatrix computeU(
    angleConfiguration* ac1, int h1, int k1, int l1,
    angleConfiguration* ac2, int h2, int k2, int l2) = 0;
  virtual smatrix computeU(
    reflection& r1, reflection& r2) = 0;

  ///virtual smatrix computeUB() const = 0;
  virtual ~diffractometer();
  virtual void printOnScreen() const;

  /// Working on the array of experimental reflections.
  int getReflection_h(int) const;
  int getReflection_k(int) const;
  int getReflection_l(int) const;

  void setReflection(angleConfiguration* ac,
    int h, int k, int l, 
    reflection::relevance r, int index);

  reflection::relevance 
    getReflection_Relevance(int) const;

  angleConfiguration*
    getReflection_AngleConfiguration(int) const;

protected:
  /// The orthogonal matrix which relates the cristal 
  /// cartesian system to the phi-axis system. It is 
  /// computed from at least two relevant reflections.
  smatrix m_U;
  /// Product U * B where B defines the 
  /// cristal lattice.
  smatrix m_UB;

  mode* m_currentMode;
  source m_currentSource;
  cristal m_currentCristal;
  /// The array to store up to 100 experiment results.
  reflection* m_reflectionList;
  /// Size of the reflection array.
  const int m_sizeOfArray;
  int m_numberOfInsertedElements;
  angleConfiguration* m_currentConfiguration;

  /// All the constructors are protected to make sure 
  /// this class is abstract.
  diffractometer(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2);

  diffractometer(
    cristal currentCristal, source currentSource);
};

class eulerianDiffractometer4C : public diffractometer
{
protected:
  /// The four matrices corresponding to the circles.
  smatrix m_OMEGA;
  smatrix m_CHI;
  smatrix m_PHI;
  smatrix m_2THETA;
  bool m_directOmega;
  bool m_directChi;
  bool m_directPhi;
  bool m_direct2Theta;

public:
  eulerianDiffractometer4C(
    cristal currentCristal, source currentSource,
    reflection& reflection1, reflection& reflection2,
    mode::diffractometer_mode currentMode);

  eulerianDiffractometer4C(
    cristal currentCristal, source currentSource,
    mode::diffractometer_mode currentMode);

  ~eulerianDiffractometer4C();

  /// Compute the rotation for the current configuration.
  smatrix computeR();

  /// Compute the rotation for a given configuration.
  smatrix computeR(angleConfiguration* ac1);

  /// Set the angle configuration and compute the 
  /// corresponding rotation matrices according to the
  /// chosen rotation axes.
  void setAngleConfiguration(angleConfiguration* ac1);

  smatrix computeU(
    angleConfiguration* ac1, int h1, int k1, int l1,
    angleConfiguration* ac2, int h2, int k2, int l2);

  smatrix computeU(
    reflection& r1, reflection& r2);

  /// The main function to compute a diffractometer 
  /// configuration from given h, k, l.
  angleConfiguration* computeAngles(
    double h,double k,double l);

  void printOnScreen() const;

  /// Return 0 if everything's fine, otherwise
  /// return the number of the failing test.
  static int test_eulerian4C();

};

class kappaDiffractometer4C : public diffractometer
{
protected:
  /// The four matrices corresponding to the circles
  /// and two other related to the kappa incidence:
  /// matrix alpha and its opposite.
  smatrix m_OMEGA;
  smatrix m_KAPPA;
  smatrix m_PHI;
  smatrix m_2THETA;
  smatrix m_ALPHA;
  smatrix m_OPP_ALPHA;
  bool m_directOmega;
  bool m_directKappa;
  bool m_directPhi;
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
    angleConfiguration* ac1, int h1, int k1, int l1,
    angleConfiguration* ac2, int h2, int k2, int l2);

  smatrix computeU(
    reflection& r1, reflection& r2);

  void printOnScreen() const;

};

#endif
