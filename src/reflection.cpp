#include "svecmat.h"
#include "constants.h"
#include "reflection.h"
#include "angleconfig.h"
#include <iostream.h>
#include <math.h>

reflection::reflection()
{
  // Make a copy to make sure we don't share memory 
  // which is going to be difficult to delete after.
  m_setOfAngles = 0;
  m_relevance = relevance::notVerySignificant;
  m_h = 0.;
  m_k = 0.;
  m_l = 0.;
}

reflection::reflection(
  angleConfiguration* this_angleConfiguration,
    double h, double k, double l, relevance this_relevance)
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
    double h, double k, double l, relevance this_relevance)
{
  // Make a copy to make sure we don't share memory 
  // which is going to be difficult to delete after.
  m_setOfAngles = this_angleConfiguration->makeCopy();
  m_relevance = this_relevance;
  m_h = h;
  m_k = k;
  m_l = l;
}

// Return the angle between two reflections, it belongs 
// to [0, PI] (return only the absolute value).
double reflection::computeAngle(
  double h2, double k2, double l2) const
{
  double dot_product = h2 * m_h + k2 * m_k + l2 * m_l;
  double length1 = sqrt(m_h*m_h + m_k*m_k + m_l*m_l);
  double length2 = sqrt(h2*h2 + k2*k2 + l2*l2);
  double cosine = dot_product / (length1*length2);

  return acos(cosine);
}

// Designed to test computeAngle().
double reflection::test_computeAngle()
{
  // Create an angle configuration.
  eulerian_angleConfiguration4C* eul4C_1 =
    new eulerian_angleConfiguration4C(
      0., 0., 0., 0.);

  // Create the first reflection.
  /////////////
  // TEST 1 //
  ///////////
  reflection r1(eul4C_1, 1, 0, 0, 
    reflection::relevance::Best);
  double ang = r1.computeAngle(1.,0.,0.) *
    mathematicalConstants::convertAnglesToDegrees();
  cout << endl;
  cout << "ang1 = " << ang << endl;

  /////////////
  // TEST 2 //
  ///////////
  reflection r2(eul4C_1, 1, 0, 0, 
    reflection::relevance::Best);
  ang = r2.computeAngle(0.,1.,0.) *
    mathematicalConstants::convertAnglesToDegrees();
  cout << endl;
  cout << "ang2 = " << ang << endl;

  /////////////
  // TEST 3 //
  ///////////
  reflection r3(eul4C_1, 14.65, 14.65, 0, 
    reflection::relevance::Best);
  ang = r3.computeAngle(-14.65, 14.65, 0.) *
    mathematicalConstants::convertAnglesToDegrees();
  cout << endl;
  cout << "ang3 = " << ang << endl;

  /////////////
  // TEST 4 //
  ///////////
  reflection r4(eul4C_1, 0., 15.178, 0, 
    reflection::relevance::Best);
  ang = r4.computeAngle(-14.65, 14.65, 0.) *
    mathematicalConstants::convertAnglesToDegrees();
  cout << endl;
  cout << "ang4 = " << ang << endl;

  /////////////
  // TEST 5 //
  ///////////
  reflection r5(eul4C_1, 1., 1., 1, 
    reflection::relevance::Best);
  ang = r5.computeAngle(1., 1., -1.) *
    mathematicalConstants::convertAnglesToDegrees();
  cout << endl;
  cout << "ang5 = " << ang << endl;




  delete eul4C_1;

  return 0;
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
