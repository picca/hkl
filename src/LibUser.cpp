#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "constants.h"
#include "reflection.h"
#include "angleconfig.h"
#include "HKLException.h"
#include "diffractometer.h"
#include <iostream.h>

#define PI 3.14159265358979323846


// File to test matrix and vector implementation.

int main ()
{
  int h, k, l;
  /////////////
  // SOURCE //
  ///////////
  source this_source2(0.7093,2.36,5.68);

  cout << endl << "**********************";
  cout << endl << "***** TRICLINIC *****";
  cout << endl << "********************";
  cristal triclinic_cristal2(
    89.99 * PI / 180., 89.963 * PI / 180., 119.99 * PI / 180.,
    18.423, 18.417, 18.457);
    //18.423 / (2 * PI), 18.417 / (2 * PI), 18.457 / (2 * PI));
  triclinic_cristal2.printOnScreen();

  double degToRad = 3.141592654 / 180.;
  eulerian_angleConfiguration4C* eul4C_3 =
    new eulerian_angleConfiguration4C(
      (2.542-5.044/2.)*degToRad,
      -26.15*degToRad,
      92.925*degToRad,
      5.044*degToRad);
  reflection r3(eul4C_3, 2, -2, 0, 
    reflection::relevance::Best);
  delete eul4C_3;
  //r1.printOnScreen();

  eulerian_angleConfiguration4C* eul4C_4 =
    new eulerian_angleConfiguration4C(
      (2.538-5.095/2.)*degToRad,
      71.19*degToRad,
      -12.37*degToRad,
      5.095*degToRad);
  reflection r4(eul4C_4, -2, 0, 0, 
    reflection::relevance::Best);
  delete eul4C_4;
  //r2.printOnScreen();

  cout << endl << "****************************";
  cout << endl << "******** TRICLINIC ********";
  cout << endl << "**************************";
  h = 10;
  k = -8;
  l = 4;
  try
  {
    eulerianDiffractometer4C*  pdiff_4C_10 = new
      eulerianDiffractometer4C(
      triclinic_cristal2, this_source2, r3, r4,
      mode::diffractometer_mode::bissector);
    delete pdiff_4C_10;

  }
  catch (HKLException& _exc)
  {
    cout << endl << "ERROR !!!" << endl;
    ErrorList::iterator theIterator;
    for (theIterator = _exc.errors.begin();
          theIterator != _exc.errors.end();
          theIterator++)
    {
      cout << theIterator->reason.c_str() << endl;
      cout << theIterator->desc.c_str() << endl;
      cout << theIterator->origin.c_str() << endl;
    }
  }    
  
  //diff_4C_10.computeU(r3, r4);
  eulerian_angleConfiguration4C* eac10 = 0;
  try
  {
    eulerianDiffractometer4C diff_4C_10(
      triclinic_cristal2, this_source2, r3, r4,
      mode::diffractometer_mode::bissector);
    eac10 = (eulerian_angleConfiguration4C*)
      diff_4C_10.computeAngles(h,k,l);
    cout << endl << "*********************";
    cout << endl << "SOLUTION FROM ("<<h<<","<<k<<","<<l<<")";
    eac10->printDegreesOnScreen();
    delete eac10;
    diff_4C_10.printOnScreen();

  }
  catch (HKLException& _exc)
  {
    if (eac10 != 0)
      delete eac10;
    cout << endl << "ERROR !!!" << endl;
    ErrorList::iterator theIterator;
    for (theIterator = _exc.errors.begin();
          theIterator != _exc.errors.end();
          theIterator++)
    {
      cout << theIterator->reason.c_str() << endl;
      cout << theIterator->desc.c_str() << endl;
      cout << theIterator->origin.c_str() << endl;
    }
  }
  //triclinic_cristal2.printOnScreen();


  /////////////////////////////////////////////////////////////////
  try
  {

    cout << endl;
    cout << "=======================" << endl;
    cout << "TEST E4C = " << eulerianDiffractometer4C::test_eulerian4C() << endl;

    cout << "=======================" << endl;
    cout << "TEST CRYSTALS = " << cristal::test_cristals() << endl;

    physicalConstants::setTau(2.*mathematicalConstants::getPI());
    cout << endl;
    cout << "=======================" << endl;
    cout << "TEST E4C = " << eulerianDiffractometer4C::test_eulerian4C() << endl;

  }
  catch (HKLException& _exc)
  {
    cout << endl << "ERROR !!!" << endl;
    ErrorList::iterator theIterator;
    for (theIterator = _exc.errors.begin();
          theIterator != _exc.errors.end();
            theIterator++)
    {
      cout << theIterator->reason.c_str() << endl;
      cout << theIterator->desc.c_str() << endl;
      cout << theIterator->origin.c_str() << endl;
    }
  }
  //triclinic_cristal2.printOnScreen();


  cout << endl;




  cout << endl;

  return (0);
}
