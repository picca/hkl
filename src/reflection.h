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

// $Revision: 1.7 $

//

// $Log: reflection.h,v $
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

  reflection();

  /// Make a copy of the angle configuration to make sure we don't share it in memory.
  reflection(angleConfiguration* this_angleConfiguration, double h, double k, double l, relevance this_relevance);

  ~reflection();

  angleConfiguration* getAngleConfiguration() const
  {return m_setOfAngles;}

  double get_h() const
  {return m_h;}

  double get_k() const
  {return m_k;}

  double get_l() const
  {return m_l;}

  relevance getRelevance() const
  {return m_relevance;}

  /// Compute the angle between two reflections to get an idea about their level of relevance (return the 
  /// absolute value). As an example it can detect if (m_h, m_k, m_l) and (h2, k2, l2) are parallel.
  double computeAngle(double h2, double k2, double l2) const;

  /// Designed to test computeAngle().
  static double test_computeAngle();

  /// Make a copy of the angle configuration to make sure we don't share it in memory.
  void set(angleConfiguration* this_angleConfiguration, double h, double k, double l, relevance this_relevance);

  void printOnScreen() const;

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
