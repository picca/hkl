/// This file defines the mode telling how to use
/// the diffractometer.
//
//  abstract class mode
//            |
//  abstract class eulerian_mode ---------
//            |                           |
//class eulerian_bissectorMode4C & eulerian_bissectorMode4C
//
//
//  abstract class mode
//            |
//  abstract class kappa_mode ------------
//            |                           |
//class kappa_bissectorMode4C & kappa_bissectorMode4C
//

#ifndef MODE
#define MODE

#include "svecmat.h"
#include "angleconfig.h"

class mode
{
public:
  enum diffractometer_mode
  {
    bissector,
    constantOmega
  };

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief The main function to get a sample of angles from (h,k,l).
  virtual angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief Designed for testing with Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param A = OMEGA * CHI * PHI * U * B.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param ac The diffractometer current angle configuration.
  /// \param lambda The wave length.
  virtual void computeHKL(
    const smatrix& A,
    double& h, double& k, double& l,
    angleConfiguration* ac,
    double lambda) const = 0;

  virtual void printOnScreen() const;

  virtual ~mode();

protected:
  /// Default constructor.
  /// - protected to make sure this class is abstract.
  mode();
};

class eulerian_mode : public mode
{
public:
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief The main function to get a sample of angles from (h,k,l).
  virtual angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief Designed for testing with Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param A = OMEGA * CHI * PHI * U * B.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param ac The diffractometer current angle configuration.
  /// \param lambda The wave length.
  virtual void computeHKL(
    const smatrix& A,
    double& h, double& k, double& l,
    angleConfiguration* ac,
    double lambda) const = 0;

  virtual void printOnScreen() const;

  virtual ~eulerian_mode();

protected:
  /// Default constructor.
  /// - protected to make sure this class is abstract.
  eulerian_mode();
};

class kappa_mode : public mode
{
public:
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief The main function to get a sample of angles from (h,k,l).
  virtual angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief Designed for testing implementing Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param A = OMEGA * CHI * PHI * U * B.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param ac The diffractometer current angle configuration.
  /// \param lambda The wave length.
  virtual void computeHKL(
    const smatrix& A,
    double& h, double& k, double& l,
    angleConfiguration* ac,
    double lambda) const = 0;

  virtual void printOnScreen() const;

  virtual ~kappa_mode();

protected:
  /// Default constructor.
  /// - protected to make sure this class is abstract.
  kappa_mode();

  /// The incident angle, its typical value is around 50°.
  double m_alpha;
};

/// The eulerian 4-circle diffractometer in bisector mode.
/// William R. Busing and Henri A. Levy "Angle calculation 
/// for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
/// <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
class eulerian_bissectorMode4C : public eulerian_mode
{
public:
  /// Default constructor.
  eulerian_bissectorMode4C();

  /// Solving equation (19) from :
  /// William R. Busing and Henri A. Levy "Angle calculation
  /// for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967)
  /// <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
  /// - R11 * hphi1 + R12 * hphi2 + R13 * hphi3 = q
  /// - R21 * hphi1 + R22 * hphi2 + R23 * hphi3 = 0
  /// - R31 * hphi1 + R32 * hphi2 + R33 * hphi3 = 0
  ///
  /// - hphi1 = q(-sin(omega)*sin(phi)+cos(omega)*cos(chi)*cos(phi))
  /// - hphi2 = q( sin(omega)*cos(phi)+cos(omega)*cos(chi)*sin(phi))
  /// - hphi3 = q*cos(omega)*sin(chi)
  ///
  /// If omega is constant :
  /// - chi = arcsin(hphi3 / q*cos(omega))
  /// - sin(phi) = (hphi1*sin(omega)-hphi2*cos(omega)*cos(chi)) / D
  /// - cos(phi) = (hphi2*sin(omega)+hphi1*cos(omega)*cos(chi)) / D
  ///
  /// where D = q*[cos(omega)*cos(omega)*cos(chi)*cos(chi) + sin(omega)*sin(omega)]
  /// \brief The main function to get a sample of angles from (h,k,l).
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \sa computeAngles_Rafin(), eulerianDiffractometer4C::test_eulerian4C()
  angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const;

  /// \brief Designed for testing with Rafin algorithm. Based on a geometric approach.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \sa computeAngles(), eulerianDiffractometer4C::test_eulerian4C()
  angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const;

  /// Solve a linear system Ax = b where A is the product of the rotation matrices 
  /// OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
  /// scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param A = OMEGA * CHI * PHI * U * B.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param ac The diffractometer current angle configuration.
  /// \param lambda The wave length.
  /// \exception when det(A)=0.
  void computeHKL(
    const smatrix& A,
    double& h, double& k, double& l,
    angleConfiguration* ac,
    double lambda) const;

  ~eulerian_bissectorMode4C();

  void printOnScreen() const;

};

/*
class eulerian_constantOmegaMode4C : public eulerian_mode
{
public:
  eulerian_constantOmegaMode4C();

  angleConfiguration computeAngles(
    int h, int k, int l) const;

  eulerian_angleConfiguration4C convertFromKappa(
    kappa_angleConfiguration4C kap) const;
};

class kappa_bissectorMode4C : public kappa_mode
{
public:
  kappa_bissectorMode4C();

  angleConfiguration computeAngles(
    int h, int k, int l) const;

  kappa_angleConfiguration4C convertFromEulerian(
    eulerian_angleConfiguration4C eul) const;

  eulerian_angleConfiguration4C convertFromKappa(
    kappa_angleConfiguration4C kap) const;
};
*/

#endif
