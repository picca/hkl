// The class reflection defines a configuration where
// a diffraction occurs. It is defined by a set of 
// angles, and the three integers associated to the 
// reciprocal lattice, and also its relevance to
// make sure we only take into account significant 
// reflections.

#ifndef REFLECTION
#define REFLECTION

#include "angleconfig.h"

class reflection
{
public:
  // The enumeration "relevance" to make sure we
  // only take into account significant reflections.
  enum relevance
  {
    notVerySignificant,
    Significant,
    VerySignificant,
    Best
  };

  reflection();

  // Make a copy of the angle configuration to
  // make sure we don't share it into the memory 
  reflection(angleConfiguration* this_angleConfiguration,
    int h, int k, int l, relevance this_relevance);

  ~reflection();

  angleConfiguration* getAngleConfiguration() const
  {return m_setOfAngles;}

  int get_h() const
  {return m_h;}

  int get_k() const
  {return m_k;}

  int get_l() const
  {return m_l;}

  relevance getRelevance() const
  {return m_relevance;}

  // Make a copy of the angle configuration to
  // make sure we don't share it into the memory 
  void set(angleConfiguration* this_angleConfiguration,
    int h, int k, int l, relevance this_relevance);

  void printOnScreen() const;

private:
  angleConfiguration* m_setOfAngles;
  // A reflection can be defined by a set of 
  // three integers (h,k,l). All these 
  // reflections are stored into these arrays.
  int m_h;
  int m_k;
  int m_l;
  relevance m_relevance;

};

#endif
