#ifndef CONSTANTS
#define CONSTANTS

// This file contains all the constants we use in the HKL
// project. They are stored as static variables so we do
// not have to create instances of such objects.
//
//  abstract class constants ---------
//            |                       |
//class physicalConstants   mathematicalConstants
class constants
{
protected:
  constants()
  {}
};

class physicalConstants : public constants
{
protected:
  // We have to deal with two different conventions, if
  // (a1,a2,a3) is the direct lattice and (b1,b2,b3) the
  // reciprocal one, a1 * b1 = 1 or a1 * b1 = 2PI so we
  // introduce tau = 1 or 2PI according to the user's choice.
  static double m_tau;

public:
  
  physicalConstants()
  {}

  static double getTau()            {return m_tau;}

  static void setTau(double tau)    {m_tau = tau;}
};

class mathematicalConstants : public constants
{
protected:
  // The common value of pi 3.14159265358979323846
  static double m_PI;

  // The precision factors, m_EPSILON_0 is quite slack
  // 1e-6 while m_EPSILON_1 is 1e-10.
  static double m_EPSILON_0;
  static double m_EPSILON_1;

  // All the computations are performed in radians, however
  // if we want to have them in degrees (to print them out 
  // for example) we just need to multiply them by 
  // its value = (180. / PI)
  static double m_convertAnglesToDegrees;
  // Same thing as before but the other way.
  static double m_convertAnglesToRadians;

public:

  mathematicalConstants()
  {}

  static double getPI()             {return m_PI;}

  static double getEpsilon0()       {return m_EPSILON_0;}

  static double getEpsilon1()       {return m_EPSILON_1;}

  static double convertAnglesToDegrees()
                                    {return m_convertAnglesToDegrees;}

  static double convertAnglesToRadians()
                                    {return m_convertAnglesToRadians;}

};


#endif
