/// The class reflection defines a configuration where a
/// diffraction occurs. It is defined by a set of angles,
/// the 3 integers associated to the reciprocal lattice
/// and its relevance to make sure we only take into
/// account significant reflections.

#ifndef REFLECTION
#define REFLECTION

#include "angleconfig.h"

class reflection
{
public:
  /// The enumeration "relevance" to make sure we only 
  /// take into account significant reflections.
  enum relevance
  {
    notVerySignificant,
    Significant,
    VerySignificant,
    Best
  };

  reflection();

  /// Make a copy of the angle configuration to make sure 
  /// we don't share it in memory.
  reflection(angleConfiguration* this_angleConfiguration,
    double h, double k, double l, 
    relevance this_relevance);

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

  /// Compute the angle between two reflections to get an
  /// idea about their level of relevance (return the 
  /// absolute value). As an example it can detect if
  /// (m_h, m_k, m_l) and (h2, k2, l2) are parallel.
  double computeAngle(
    double h2, double k2, double l2) const;

  /// Designed to test computeAngle().
  static double test_computeAngle();

  /// Make a copy of the angle configuration to make sure
  /// we don't share it in memory.
  void set(angleConfiguration* this_angleConfiguration,
    double h, double k, double l, 
    relevance this_relevance);

  void printOnScreen() const;

private:
  angleConfiguration* m_setOfAngles;
  /// A reflection can be defined by a set of three numbers
  /// (h,k,l) and its associated relevance.
  double m_h;
  double m_k;
  double m_l;
  relevance m_relevance;

};

#endif
