//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/constants.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class constant, physicalConstants, mathematicalConstants

// (Delos Vincent) - 26 janv. 2005

//

// $Author: delos $

//

// $Revision: 1.4 $

//

// $Log: constants.h,v $
// Revision 1.4  2005/01/27 09:23:53  delos
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
#ifndef CONSTANTS
#define CONSTANTS

/// This file contains all the constants we use in the HKL
/// project. They are stored as static variables so we do
/// not have to create instances of such objects.
///
//  abstract class constants ---------
//            |                       |
//  class physicalConstants   mathematicalConstants
class constants
{
protected:
  constants()
  {}
};

/// Store all the basic physical constants we need to
/// define conventions.
class physicalConstants : public constants
{
protected:
  /// We have to deal with two different conventions, if
  /// (a1,a2,a3) is the direct lattice and (b1,b2,b3) the
  /// reciprocal one, a1 * b1 = 1 or a1 * b1 = 2PI so we
  /// introduce tau = 1 or 2PI according to the user's choice.
  static double m_tau;

public:
  
  physicalConstants()
  {}

  static double getTau()            {return m_tau;}

  static void setTau(double tau)    {m_tau = tau;}
};

/// Store all the basic mathematical constants we need.
class mathematicalConstants : public constants
{
protected:
  /// The usual value of pi 3.14159265358979323846
  static double m_PI;

  /// This precision factor is used to test if two 
  /// angles are the same.
  /// \brief The first precision factor. 
  static double m_EPSILON_0;
  /// This precision factor is used to test if a double 
  /// precision number is null.
  /// \brief The second precision factor. 
  static double m_EPSILON_1;

  /// All the computations are performed in radians, 
  /// however if we want to have them in degrees (to 
  /// print them out for example) we just need to 
  /// multiply them by its value.
  /// \brief To convert an angle in degrees (180 / PI).
  static double m_convertAnglesToDegrees;
  /// To convert an angle in radians (PI / 180)
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
