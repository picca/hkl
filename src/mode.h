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

  /// The main function to get a sample of 
  /// angles from (h,k,l).
  virtual angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// Designed for testing implementing Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
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
  /// The main function to get a sample of 
  /// angles from (h,k,l).
  virtual angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// Designed for testing implementing Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
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
  /// The main function to get a sample of 
  /// angles from (h,k,l).
  virtual angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const = 0;

  /// Designed for testing implementing Rafin algorithm.
  virtual angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
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
/// <A HREF="http://journals.iucr.org/index.html"> Acta Cryst. </A>, 22, 457-464.
class eulerian_bissectorMode4C : public eulerian_mode
{
public:
  /// Default constructor.
  eulerian_bissectorMode4C();

  /// The main function to get a sample of 
  /// angles from (h,k,l).
  angleConfiguration* computeAngles(
    double h, double k, double l,
    const smatrix& UB,
    double lambda) const;

  /// Designed for testing implementing Rafin algorithm.
  angleConfiguration* computeAngles_Rafin(
    double h, double k, double l,
    const smatrix& UB,
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
