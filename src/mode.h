//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/mode.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class mode, eulerian_bissectorMode4C, eulerian_constantOmegaMode4C,
// eulerian_horizontal4CBissectorMode6C, eulerian_vertical4CBissectorMode6C, eulerian_lifting3CDetectorMode6C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.17 $

//

// $Log: mode.h,v $
// Revision 1.17  2005/02/11 15:52:45  picca
// documentation
//
// Revision 1.16  2005/01/27 09:23:53  delos
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

/// Define the way we use the diffractometer.
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

#ifndef _MODE_H_
#define _MODE_H_

#include "svecmat.h"
#include "angleconfig.h"
#include "HKLException.h"

/**
 * This class defines how to use a diffractomer.
 */
class mode
{
public:

  enum diffractometer_mode
  {
    // Eulerian 4C modes.
    bissector, //!< bissector mode
    constantOmega, //!< constantOmega
    // Eulerian 6C modes.
    vertical4CBissector6C, //!< vertical4CBissector6C
    horizontal4CBissector6C, //!< horizontal4CBissector6C
    lifting3CDetector6C //!< lifting3CDetector6C
  };
  
  /**
   * @brief Destructor
   *
   * Destructor
   */
  virtual ~mode();

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * The main function to get a sample of angles from (h,k,l).
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /**
   * @brief Designed for testing with Rafin algorithm.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Designed for testing with Rafin algorithm.
   */
  virtual angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @param ac The diffractometer current angle configuration.
   *
   * Compute (h,k,l) from a sample of angles.
   */
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const = 0;

  virtual void printOnScreen() const; //!< Print the mode on the screen

protected:
  /**
   * @brief Default constructor.
   * @return a new mode
   *
   * Default constructor - protected to make sure this class is abstract.
   */
  mode();
};

/**
 * This class defines how to use an eulerian diffractomer.
 */
class eulerian_mode : public mode
{
public:

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * The main function to get a sample of angles from (h,k,l).
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /**
   * @brief Designed for testing with Rafin algorithm.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Designed for testing with Rafin algorithm.
   */  
   virtual angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @param ac The diffractometer current angle configuration.
   *
   * Compute (h,k,l) from a sample of angles.
   */
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const = 0;

  virtual void printOnScreen() const; //!< Print the mode on the screen

  /**
   * @brief Destructor.
   *
   * Destructor
   */
  virtual ~eulerian_mode();

protected:

  /**
   * @brief Default constructor.
   * @return a new eulerian_mode
   *
   * Default constructor - protected to make sure this class is abstract.
   */
  eulerian_mode();
};

/**
 * This class defines how to use a kappa diffractomer.
 */
class kappa_mode : public mode
{
public:

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * The main function to get a sample of angles from (h,k,l).
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /**
   * @brief Designed for testing with Rafin algorithm.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Designed for testing with Rafin algorithm.
   */
  virtual angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const = 0;

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param A = OMEGA * (-ALPHA) * KAPPA * ALPHA * PHI * U * B.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param ac The diffractometer current angle configuration.
   * @param lambda The wave length.
   *
   * Compute (h,k,l) from a sample of angles.
   */
  virtual void computeHKL(const smatrix& A, double& h, double& k, double& l, angleConfiguration* ac, double lambda) const = 0;

  virtual void printOnScreen() const; //!< Print the mode on the screen

  /**
   * @brief Destructor.
   *
   * Destructor
   */
  virtual ~kappa_mode();

protected:

  /**
   * @brief Default constructor.
   * @return a new kappa_mode
   *
   * Default constructor - protected to make sure this class is abstract.
   */
  kappa_mode();

  double m_alpha; //!< The incident angle, its typical value is around 50°.
};

/**
 * The eulerian 4-circle diffractometer in bisector mode. William R. Busing and Henri A. Levy
 * "Angle calculation for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
 * <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
 */
class eulerian_bissectorMode4C : public eulerian_mode
{
public:

  /**
   * @brief Default constructor.
   * @return a new eulerian_bissectorMode4C
   *
   * Default constructor.
   */
  eulerian_bissectorMode4C();

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Solving equation (19) from :
   * William R. Busing and Henri A. Levy "Angle calculation
   * for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967)
   * <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
   * - R11 * hphi1 + R12 * hphi2 + R13 * hphi3 = q
   * - R21 * hphi1 + R22 * hphi2 + R23 * hphi3 = 0
   * - R31 * hphi1 + R32 * hphi2 + R33 * hphi3 = 0
   *
   * - hphi1 = q(-sin(omega)*sin(phi)+cos(omega)*cos(chi)*cos(phi))
   * - hphi2 = q( sin(omega)*cos(phi)+cos(omega)*cos(chi)*sin(phi))
   * - hphi3 = q*cos(omega)*sin(chi)
   *
   * If omega is constant :
   * - chi = arcsin(hphi3 / q*cos(omega))
   * - sin(phi) = (hphi1*sin(omega)-hphi2*cos(omega)*cos(chi)) / D
   * - cos(phi) = (hphi2*sin(omega)+hphi1*cos(omega)*cos(chi)) / D
   *
   * where D = q*[cos(omega)*cos(omega)*cos(chi)*cos(chi) + sin(omega)*sin(omega)]
   * @sa computeAngles_Rafin(), eulerianDiffractometer4C::test_eulerian4C()
   *
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const throw (HKLException);

  /**
   * @brief Designed for testing with Rafin algorithm. Based on a geometric approach.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Designed for testing with Rafin algorithm.
   * @sa computeAngles(), eulerianDiffractometer4C::test_eulerian4C()
   */
  angleConfiguration* computeAngles_Rafin(double h, double k, double l, const smatrix& UB, double lambda) const throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @param ac The diffractometer current angle configuration.
   *
   * Solve a linear system Ax = b where A is the product of the rotation matrices
   * OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is theta
   * scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
   */
   virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const throw (HKLException);

  /**
   * @brief Destructor.
   *
   * Destructor
   */
  virtual ~eulerian_bissectorMode4C();

  void printOnScreen() const; //!< Print the mode on the screen
};

/**
 * @brief Using an eulerian 6-circle diffractometer as an horizontal 4C eulerian one in bisector mode.
 *
 * The eulerian 6-circle diffractometer in horizontal bisector mode as described in <BR>
 * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
 * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
 * In this mode delta = eta = 0, so the scattering vector formula becomes :<BR>
 * Q = ||Q|| * (0., -sin(theta), cos(theta)) where theta comes from the Bragg relation :<BR>
 * 2tau * sin(theta) = ||Q|| * lambda
 * 
 */
class eulerian_horizontal4CBissectorMode6C : public eulerian_bissectorMode4C
{
public:

  /**
   * @brief Default constructor.
   * @return a new eulerian_horizontal4CBissectorMode6C
   *
   * Default constructor.
   */
  eulerian_horizontal4CBissectorMode6C();

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Solving equation (11) from :
   * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
   * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
   * MU.ETA.CHI.PHI.U.B.(h,k,l) = Q
   * @sa eulerianDiffractometer4C::test_eulerian4C()
   */  
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @param ac The diffractometer current angle configuration.
   *
   * Solve a linear system Ax = b where A is the product of the rotation matrices MU, ETA, CHI, PHI
   * by the orientation matrix U and the crystal matrix B. b is the scattering vector
   * ||Q|| * (0., -sin(theta), cos(theta)) and x = (h,k,l). Raise an exception when det(A)=0.
   * @exception det(A)=0
   */  
   virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const throw (HKLException);

  /**
   * @brief Destructor.
   *
   * Destructor
   */
  virtual ~eulerian_horizontal4CBissectorMode6C();

  void printOnScreen() const; //!< Print the mode on the screen

};

/**
 * @brief Using an eulerian 6-circle diffractometer as an vertical 4C eulerian one in bisector mode.
 *
 * The eulerian 6-circle diffractometer in vertical bisector mode as described in <BR>
 * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
 * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
 * In this mode mu = nu = 0, so the scattering vector formula becomes :<BR>
 * Q = ||Q|| * (cos(theta), -sin(theta), 0.) where theta comes from the Bragg relation :<BR>
 * 2tau * sin(theta) = ||Q|| * lambda
 */
class eulerian_vertical4CBissectorMode6C : public eulerian_bissectorMode4C
{
public:
  
  /**
   * @brief Default constructor.
   * @return a new eulerian_vertical4CBissectorMode6C
   *
   * Default constructor.
   */
  eulerian_vertical4CBissectorMode6C();

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Solving equation (11) from :
   * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
   * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
   * MU.ETA.CHI.PHI.U.B.(h,k,l) = Q
   * @sa eulerianDiffractometer4C::test_eulerian4C()
   */  
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @param ac The diffractometer current angle configuration.
   *
   * Solve a linear system Ax = b where A is the product of the rotation matrices MU, ETA, CHI, PHI
   * by the orientation matrix U and the crystal matrix B. b is the scattering vector
   * ||Q|| * (cos(theta), -sin(theta), 0.) and x = (h,k,l). Raise an exception when det(A)=0.
   * @exception det(A)=0
   */  
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const throw (HKLException);

  /**
   * @brief Destructor.
   *
   * Destructor
   */
  virtual ~eulerian_vertical4CBissectorMode6C();

  void printOnScreen() const; //!< Print the mode on the screen

};

/**
 * @brief The eulerian 6-circle diffractometer as a 3-circles lifting detector geometry.
 *
 * The eulerian 6-circle diffractometer in 3-circles lifting detector mode. We solve equations described in <BR>
 * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
 * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.<BR>
 * In this mode eta = chi = phi = 0. <BR>
 * To move the crystal we only use the mu circle, to move the detector we use both delta and nu. <BR>
 * The scattering vector is :<BR>
 * Q = (tau/lambda) * (sin(delta), cos(nu)*cos(delta)-1, sin(nu)*cos(delta))
 */
class eulerian_lifting3CDetectorMode6C : public eulerian_bissectorMode4C
{
public:

  /**
   * @brief Default constructor.
   * @return a new eulerian_lifting3CDetectorMode6C
   *
   * Default constructor.
   */
  eulerian_lifting3CDetectorMode6C();

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Solving equation (11) from :
   * H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
   * <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
   * MU.U.B.(h,k,l) = Q <BR>
   * In this mode :
   * - eta = chi = phi = 0.
   * - delta = arcsin(hphi1 / kk) where kk = tau/lambda
   * - nu = arccos[(1-Q²/kk²)/(2cos(delta))]
   * - sin(mu)*(hphi2²+hphi3²) = -hphi3*kk*(cos(delta)*cos(nu)-1)+hphi2*kk*sin(nu)*cos(delta)
   * - cos(mu)*(hphi2²+hphi3²) =  hphi2*kk*(cos(delta)*cos(nu)-1)+hphi3*kk*sin(nu)*cos(delta)
   * @sa eulerianDiffractometer4C::test_eulerian4C()
   */
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @param ac The diffractometer current angle configuration.
   *
   * Solve a linear system Ax = b where A is the product of the rotation matrices MU, ETA, CHI, PHI
   * by the orientation matrix U and the crystal matrix B. b is the scattering vector
   * (tau/lambda)*(sin(delta), cos(delta).cos(nu)-1, cos(delta).sin(nu)), x = (h,k,l). Raise an exception when det(A)=0.
   */  
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const throw (HKLException);

  /**
   * @brief Destructor.
   *
   * Destructor
   */
  virtual ~eulerian_lifting3CDetectorMode6C();

  void printOnScreen() const; //!< Print the mode on the screen

};

/**
 * The eulerian 4-circle diffractometer in constant omega mode. William R. Busing and Henri A. Levy
 * "Angle calculation for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
 * <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
 */
class eulerian_constantOmegaMode4C : public eulerian_mode
{
public:

  /**
   * @brief Default constructor.
   * @return a new eulerian_constantOmegaMode4C
   *
   * Default constructor.
   */
  eulerian_constantOmegaMode4C(double constantOmega);

  /**
   * @brief The main function to get a sample of angles from (h,k,l).
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @return The computed sample of angles.
   *
   * Solving equation (19) from :
   * William R. Busing and Henri A. Levy "Angle calculation
   * for 3- and 4- Circle X-ray and Neutron Diffractometer" (1967)
   * <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
   * - R11 * hphi1 + R12 * hphi2 + R13 * hphi3 = q
   * - R21 * hphi1 + R22 * hphi2 + R23 * hphi3 = 0
   * - R31 * hphi1 + R32 * hphi2 + R33 * hphi3 = 0
   *
   * If omega is constant :
   * - chi = arcsin(hphi3 / q*cos(omega))
   * - sin(phi) = (hphi1*sin(omega)-hphi2*cos(omega)*cos(chi)) / D
   * - cos(phi) = (hphi2*sin(omega)+hphi1*cos(omega)*cos(chi)) / D
   *
   * where D = q*[cos(omega)*cos(omega)*cos(chi)*cos(chi) + sin(omega)*sin(omega)]
   * @sa computeAngles_Rafin(), eulerianDiffractometer4C::test_eulerian4C()
   */  
  virtual angleConfiguration* computeAngles(double h, double k, double l, const smatrix& UB, double lambda) const throw (HKLException);

  /**
   * @brief Compute (h,k,l) from a sample of angles.
   * @param h The scaterring vector first element.
   * @param k The scaterring vector second element.
   * @param l The scaterring vector third element.
   * @param UB The product of the orientation matrix U by the crystal matrix B.
   * @param lambda The wave length.
   * @param ac The diffractometer current angle configuration.
   *
   * Solve a linear system Ax = b where A is the product of the rotation matrices 
   * OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is theta
   * scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
   * @exception det(A)=0
   */  
  virtual void computeHKL(double& h, double& k, double& l, const smatrix& UB, double lambda, angleConfiguration* ac) const throw (HKLException);

  double getConstantOmega() const {return m_constantOmega;} //!< Get the ConstantOmega parameter
  
  void setConstantOmega(double constantOmega) {m_constantOmega = constantOmega;} //!< Set the ConstantOmega parameter

  /**
   * @brief Destructor.
   *
   * Destructor
   */
  virtual ~eulerian_constantOmegaMode4C();

  void printOnScreen() const; //!< Print the mode on the screen

private:

  double m_constantOmega; //!< The constantOmega parameter

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

#endif // _MODE_H_
