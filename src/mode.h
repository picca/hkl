// This file defines the mode telling how to use
// the diffractometer.
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

  virtual angleConfiguration* computeAngles(
    int h, int k, int l,
    const smatrix& UB,
    double lembda) const = 0;

  virtual void printOnScreen() const;

  virtual ~mode();

protected:
  mode();
};

class eulerian_mode : public mode
{
public:
  virtual angleConfiguration* computeAngles(
    int h, int k, int l,
    const smatrix& UB,
    double lembda) const = 0;

  virtual void printOnScreen() const;

  virtual ~eulerian_mode();

protected:
  eulerian_mode();
};

class kappa_mode : public mode
{
public:
  virtual angleConfiguration* computeAngles(
    int h, int k, int l,
    const smatrix& UB,
    double lembda) const = 0;

  virtual void printOnScreen() const;

  virtual ~kappa_mode();

protected:
  kappa_mode();

  // The incident angle, its typical value is around 50°.
  double m_alpha;
};

class eulerian_bissectorMode4C : public eulerian_mode
{
public:
  eulerian_bissectorMode4C();

  angleConfiguration* computeAngles(
    int h, int k, int l,
    const smatrix& UB,
    double lembda) const;

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
