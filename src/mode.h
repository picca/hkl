/// This file defines the mode telling how to use
/// the diffractometer.
//
//  abstract class mode
//            |
//  abstract class eulerian_mode ---------
//            |                           |
//class eulerian_bissectorMode4C & eulerian_constantOmegaMode4C
//            |
//            |---------------------------------------------------------------------------
//            |                                       |                                   |      
//class eulerian_horizontal4CBissectorMode6C & eulerian_vertical4CBissectorMode6C & eulerian_lifting3CDetectorMode6C
//
//
//  abstract class mode
//            |
//  abstract class kappa_mode ------------
//            |                           |
//class kappa_bissectorMode4C & kappa_constantOmegaMode4C
//

#ifndef MODE
#define MODE

#include "svecmat.h"
#include "angleconfig.h"

/// This class defines how to use a diffractomer.
class mode
{
public:
  enum diffractometer_mode
  {
    // Eulerian 4C modes.
    bissector,
    constantOmega,
    // Eulerian 6C modes.
    vertical4CBissector6C,
    horizontal4CBissector6C,
    lifting3CDetector6C
  };

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief The main function to get a sample of angles from (h,k,l).
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief Designed for testing with Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \param ac The diffractometer current angle configuration.
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const = 0;

  virtual void printOnScreen() const;

  virtual ~mode();

protected:
  /// Default constructor.
  /// - protected to make sure this class is abstract.
  mode();
};

/// This class defines how to use an eulerian diffractomer.
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
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief Designed for testing with Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \param ac The diffractometer current angle configuration.
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const = 0;

  virtual void printOnScreen() const;

  virtual ~eulerian_mode();

protected:
  /// Default constructor.
  /// - protected to make sure this class is abstract.
  eulerian_mode();
};

/// This class defines how to use a kappa diffractomer.
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
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \brief Designed for testing implementing Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param A = OMEGA * (-ALPHA) * KAPPA * ALPHA * PHI * U * B.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param ac The diffractometer current angle configuration.
  /// \param lambda The wave length.
  virtual void computeHKL(const smatrix& A, double& h, double& k, double& l, angleConfiguration* ac, double lambda) const = 0;

  virtual void printOnScreen() const;

  virtual ~kappa_mode();

protected:
  /// Default constructor.
  /// - protected to make sure this class is abstract.
  kappa_mode();

  /// The incident angle, its typical value is around 50°.
  double m_alpha;
};

/// The eulerian 4-circle diffractometer in bisector mode. William R. Busing and Henri A. Levy
/// "Angle calculation for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
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
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const;

  /// \brief Designed for testing with Rafin algorithm. Based on a geometric approach.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \sa computeAngles(), eulerianDiffractometer4C::test_eulerian4C()
  angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const;

  /// Solve a linear system Ax = b where A is the product of the rotation matrices 
  /// OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
  /// scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \param ac The diffractometer current angle configuration.
  /// \exception det(A)=0
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const;

  virtual ~eulerian_bissectorMode4C();

  void printOnScreen() const;

};

/// The eulerian 6-circle diffractometer in horizontal bisector mode as described in <BR>
/// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
/// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
/// In this mode delta = eta = 0, so the scattering vector formula becomes :<BR>
/// Q = ||Q|| * (0., -sin(theta), cos(theta)) where theta comes from the Bragg relation :<BR>
/// 2tau * sin(theta) = ||Q|| * lambda
/// \brief Using an eulerian 6-circle diffractometer as an horizontal 4C eulerian one in bisector mode.
class eulerian_horizontal4CBissectorMode6C : public eulerian_bissectorMode4C
{
public:
  /// Default constructor.
  eulerian_horizontal4CBissectorMode6C();

  /// Solving equation (11) from :
  /// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
  /// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
  /// MU.ETA.CHI.PHI.U.B.(h,k,l) = Q
  /// \brief The main function to get a sample of angles from (h,k,l).
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \sa eulerianDiffractometer4C::test_eulerian4C()
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const;

  /// Solve a linear system Ax = b where A is the product of the rotation matrices MU, ETA, CHI, PHI
  /// by the orientation matrix U and the crystal matrix B. b is the scattering vector
  /// ||Q|| * (0., -sin(theta), cos(theta)) and x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \param ac The diffractometer current angle configuration.
  /// \exception det(A)=0
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const;

  virtual ~eulerian_horizontal4CBissectorMode6C();

  void printOnScreen() const;

};

/// The eulerian 6-circle diffractometer in vertical bisector mode as described in <BR>
/// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
/// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
/// In this mode mu = nu = 0, so the scattering vector formula becomes :<BR>
/// Q = ||Q|| * (cos(theta), -sin(theta), 0.) where theta comes from the Bragg relation :<BR>
/// 2tau * sin(theta) = ||Q|| * lambda
/// \brief Using an eulerian 6-circle diffractometer as an vertical 4C eulerian one in bisector mode.
class eulerian_vertical4CBissectorMode6C : public eulerian_bissectorMode4C
{
public:
  /// Default constructor.
  eulerian_vertical4CBissectorMode6C();

  /// Solving equation (11) from :
  /// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
  /// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
  /// MU.ETA.CHI.PHI.U.B.(h,k,l) = Q
  /// \brief The main function to get a sample of angles from (h,k,l).
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \sa eulerianDiffractometer4C::test_eulerian4C()
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const;

  /// Solve a linear system Ax = b where A is the product of the rotation matrices MU, ETA, CHI, PHI
  /// by the orientation matrix U and the crystal matrix B. b is the scattering vector
  /// ||Q|| * (cos(theta), -sin(theta), 0.) and x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \param ac The diffractometer current angle configuration.
  /// \exception det(A)=0
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const;

  virtual ~eulerian_vertical4CBissectorMode6C();

  void printOnScreen() const;

};

/// The eulerian 6-circle diffractometer in 3-circles lifting detector mode. We solve equations described in <BR>
/// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
/// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
/// In this mode eta = chi = phi = 0. <BR>
/// To move the crystal we only use the mu circle, to move the detector we use both delta and nu. <BR>
/// The scattering vector is :<BR>
/// Q = (tau/lambda) * (sin(delta), cos(nu)*cos(delta)-1, sin(nu)*cos(delta))
/// \brief The eulerian 6-circle diffractometer as a 3-circles lifting detector geometry.
class eulerian_lifting3CDetectorMode6C : public eulerian_bissectorMode4C
{
public:
  /// Default constructor.
  eulerian_lifting3CDetectorMode6C();

  /// Solving equation (11) from :
  /// H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
  /// <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
  /// MU.U.B.(h,k,l) = Q <BR>
  /// In this mode :
  /// - eta = chi = phi = 0.
  /// - delta = arcsin(hphi1 / kk) where kk = tau/lambda
  /// - nu = arccos[(1-Q²/kk²)/(2cos(delta))]
  /// - sin(mu)*(hphi2²+hphi3²) = -hphi3*kk*(cos(delta)*cos(nu)-1)+hphi2*kk*sin(nu)*cos(delta)
  /// - cos(mu)*(hphi2²+hphi3²) =  hphi2*kk*(cos(delta)*cos(nu)-1)+hphi3*kk*sin(nu)*cos(delta)
  /// \brief The main function to get a sample of angles from (h,k,l).
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \return The computed sample of angles.
  /// \sa eulerianDiffractometer4C::test_eulerian4C()
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const;

  /// Solve a linear system Ax = b where A is the product of the rotation matrices MU, ETA, CHI, PHI
  /// by the orientation matrix U and the crystal matrix B. b is the scattering vector
  /// (tau/lambda)*(sin(delta), cos(delta).cos(nu)-1, cos(delta).sin(nu)), x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \param ac The diffractometer current angle configuration.
  /// \exception det(A)=0
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const;

  virtual ~eulerian_lifting3CDetectorMode6C();

  void printOnScreen() const;

};

/// The eulerian 4-circle diffractometer in constant omega mode. William R. Busing and Henri A. Levy
/// "Angle calculation for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
/// <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
class eulerian_constantOmegaMode4C : public eulerian_mode
{
public:
  /// Default constructor.
  eulerian_constantOmegaMode4C(double constantOmega);

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
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const;

  /// Solve a linear system Ax = b where A is the product of the rotation matrices 
  /// OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
  /// scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
  /// \brief Compute (h,k,l) from a sample of angles.
  /// \param h The scaterring vector first element.
  /// \param k The scaterring vector second element.
  /// \param l The scaterring vector third element.
  /// \param UB The product of the orientation matrix U by the crystal matrix B.
  /// \param lambda The wave length.
  /// \param ac The diffractometer current angle configuration.
  /// \exception det(A)=0
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const;

  double getConstantOmega() const
  {return m_constantOmega;}

  double setConstantOmega(double constantOmega)
  {m_constantOmega = constantOmega;}

  virtual ~eulerian_constantOmegaMode4C();

  void printOnScreen() const;

private:
  double m_constantOmega;

};

/*
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
