//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/constants.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class constant, physicalConstants, mathematicalConstants

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.5 $

//

// $Log: constants.h,v $
// Revision 1.5  2005/02/10 14:09:34  picca
// documentation
//
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
#ifndef _CONSTANTS_H_
#define _CONSTANTS_H_

/**
 * \brief class to describe constants
 *
 * All constants are stored as static variables so we do
 * not have to create instances of such objects
 */
class constants
{
protected:
  /**
   * \brief Constructor
   *
   * An empty constructor because all elements are staticly stored
   */
  constants() {}
};

/**
 * \brief containe all the physical constants.
 *
 * Store all the basic physical constants we need to
 * define conventions.
 */
class physicalConstants : public constants
{
protected:

  /**
   * \brief the \f$\tau\f$ constant.
   * 
   * We have to deal with two different conventions, if
   * (a1,a2,a3) is the direct lattice and (b1,b2,b3) the
   * reciprocal one, \f$ a1 b1 = 1 \f$ or \f$ a1 * b1 = 2\pi \f$ so we
   * introduce \f$ \tau = 1 \f$ or \f$ \tau=2\pi \f$ according to the user's choice.
   */
  static double m_tau;

public:
   
   /**
   * \brief Constructor
   *
   * An empty constructor because all elements are staticly stored
   */
  physicalConstants() {}

  static double getTau()            {return m_tau;} //!< Get m_tau
  static void   setTau(double tau)  {m_tau = tau;} //!< Set m_tau
};

/**
 * \brief containe all the mathematical constants.
 *
 * Store all the basic mathematical constants we need to
 * do computations.
 */
class mathematicalConstants : public constants
{
protected:

  /**
   * \brief The \f$\pi\f$ number
   * 
   * The usual value of \f$\pi\f$ 3.14159265358979323846
   */
  static double m_PI;

  /**
   * \brief The first precision factor.
   *
   * This precision factor is used to test if two 
   * angles are the same.
   */
  static double m_EPSILON_0;
  
  /**
   * \brief The second precision factor.
   * 
   * This precision factor is used to test if a double 
   * precision number is null.
   */
  static double m_EPSILON_1;

  /**
   * \brief To convert from radian to degree \f$ \times 180 / \pi \f$.
   *
   * All the computations are performed in radians, 
   * however if we want to have them in degrees (to 
   * print them out for example) we just need to 
   * multiply them by this value.
   */
  static double m_convertAnglesToDegrees;
   
   /**
   * \brief To convert degree into radian \f$ \times \pi/180 \f$.
   *
   * All the computations are performed in radians, 
   * however if we have a data in degrees (to 
   * use them for computation for example) we just need to 
   * multiply them by this value.
   */
  static double m_convertAnglesToRadians;

public:

  mathematicalConstants()
  {}

  static double getPI()                   {return m_PI;} //!< Get m_PI
  static double getEpsilon0()             {return m_EPSILON_0;} //!< Get m_EPSILON_0
  static double getEpsilon1()             {return m_EPSILON_1;} //!< Get m_EPSILON_1
  static double convertAnglesToDegrees()  {return m_convertAnglesToDegrees;} //!< Get the convertAnglesToDegrees constant
  static double convertAnglesToRadians()  {return m_convertAnglesToRadians;} //!< Get the convertAnglesToRadians constant

};

#endif _CONSTANTS_H_
