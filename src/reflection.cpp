#include "reflection.h"
#include "angleconfig.h"
#include <iostream.h>

reflection::reflection()
{
  // Make a copy to make sure we don't share memory 
  // which is going to be difficult to delete after.
  m_setOfAngles = 0;
  m_relevance = relevance::notVerySignificant;
  m_h = 0;
  m_k = 0;
  m_l = 0;
}

reflection::reflection(
  angleConfiguration* this_angleConfiguration,
    int h, int k, int l, relevance this_relevance)
{
  // Make a copy to make sure we don't share memory 
  // which is going to be difficult to delete after.
  m_setOfAngles = this_angleConfiguration->makeCopy();
  m_relevance = this_relevance;
  m_h = h;
  m_k = k;
  m_l = l;
}

void reflection::set(
  angleConfiguration* this_angleConfiguration,
    int h, int k, int l, relevance this_relevance)
{
  // Make a copy to make sure we don't share memory 
  // which is going to be difficult to delete after.
  m_setOfAngles = this_angleConfiguration->makeCopy();
  m_relevance = this_relevance;
  m_h = h;
  m_k = k;
  m_l = l;
}

reflection::~reflection()
{
  delete m_setOfAngles;
}

void reflection::printOnScreen() const
{
  cout << endl << "CLASS reflection";
  cout << endl
    << "h = " << m_h << '\t'
    << "k = " << m_k << '\t'
    << "l = " << m_l << '\t'
    << "relevance = " << m_relevance;
  m_setOfAngles->printOnScreen();
}
