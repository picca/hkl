//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/reflection.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class reflection

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.8 $

//

// $Log: reflection.h,v $
// Revision 1.8  2005/02/08 17:03:08  picca
// update the documentation
//
// Revision 1.7  2005/02/08 15:51:05  picca
// update the documenattion
//
// Revision 1.6  2005/01/27 09:23:53  delos
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
#ifndef REFLECTION
#define REFLECTION

#include "angleconfig.h"

/**
 * The class reflection defines a configuration where a diffraction occurs. It
 * is defined by a set of angles, the 3 integers associated to the reciprocal
 * lattice and its relevance to make sure we only take into account significant
 * reflections.
 */
class reflection
{
public:
  /**
   * \brief The enumeration "relevance" to make sure we only take into account significant reflections.
   */
  enum relevance
  {
    notVerySignificant = 0, ///< not very significant reflection
    Significant, ///< significant reflection
    VerySignificant, ///< very significant reflection
    Best ///< Best reflection
  };

  /**
   * \brief Constructor
   *
   * Create a new reflection with all is parameters set to zero.
   */
  reflection();

  /**
   * \brief Constructor from parameters
   * \param this_angleConfiguration #angleConfiguration The angle configuration
   * \param h the h number of the reflection
   * \param k the k number of the reflection
   * \param l the l number of the reflection
   * \param this_relevance #relevance the relevance of this reflection
   *
   * Create a new reflection and populate is members with the parameters
   */
  reflection(angleConfiguration* this_angleConfiguration, double h, double k, double l, relevance this_relevance);

  /**
   * \brief Destructor
   *
   * Release the memory of the reflection this
   */
  ~reflection();

  /**
   * \brief get the angle configuration of the reflection
   * 
   * Get the angle configuration of the reflection this
   */
  angleConfiguration* getAngleConfiguration() const;

  /**
   * \brief get the h parameter of the reflection
   *
   * Get the h parameter of the reflection this
   */
  double get_h() const;

  /**
   * \brief get the k parameter of the reflection
   *
   * Get the k parameter of the reflection this
   */
  double get_k() const;

  /**
   * \brief get the l parameter of the reflection
   *
   * Get the l parameter of the reflection this
   */
  double get_l() const;

  /**
   * \brief get the relevance parameter of the reflection
   *
   * Get the relevance parameter of the reflection this
   */
  relevance getRelevance() const;

  /**
   * \brief compute the angle between two reflections
   * \param h2 the h parameters of the second reflection
   * \param k2 the k parameters of the second reflection
   * \param l2 the l parameters of the second reflection
   *
   * Compute the angle between two reflections to get an idea about their level
   * of relevance (return the absolute value). As an example it can detect if
   * (m_h, m_k, m_l) and (h2, k2, l2) are parallel.
   */
  double computeAngle(double h2, double k2, double l2) const;


  /**
   * \brief Set the reflections parameters
   * \param this_angleConfiguration #angleConfiguration of the reflection to set
   * \param h
   * \param k
   * \param l
   * \param this_relevance #relevance of the reflection
   *
   * Set the parameters of the reflection.
   * Make a copy of the angle configuration to make sure we don't share it in
   * memory.
   */
  void set(angleConfiguration* this_angleConfiguration, double h, double k, double l, relevance this_relevance);

  /**
   * \brief Print the reflection
   *
   * Print on stdout the reflection configuration
   */
  void printOnScreen() const;

  // Designed to test computeAngle().
  static double test_computeAngle();

private:
  /// The corresponding angle configuration.
  angleConfiguration* m_setOfAngles;
  /// The first of the three numbers (h,k,l).
  double m_h;
  /// The second of the three numbers (h,k,l).
  double m_k;
  /// The third of the three numbers (h,k,l).
  double m_l;
  /// Its associated relevance.
  relevance m_relevance;

};

#endif
