#include "source.h"
#include "cristal.h"
#include "svecmat.h"
#include "reflection.h"
#include "angleconfig.h"
#include "diffractometer.h"
#include <iostream.h>

#define PI 3.14159265358979323846


// File to test matrix and vector implementation.

int main ()
{
  /*
  svector v1(1., 0.3, 3.141592654);
  svector v2(0.3, -1., 1.);
  svector v3(v1);

  v1.printOnScreen();
  v2.printOnScreen();
  v3.printOnScreen();

  cout << endl << "(v1,v2) scalar product is : " << v1.scalar(v2);
  cout << endl << "(v2,v1) scalar product is : " << v2.scalar(v1);
  cout << endl << "v1 norm2 is : " << v1.norm2(); //=sqrt(10.959604403666763716)
  cout << endl << "v2 norm2 is : " << v2.norm2(); //=sqrt(2.09)
  cout << endl << "v1 infinite norm is : " << v1.norminf(); //=pi
  cout << endl << "v2 infinite norm is : " << v2.norminf(); //=1.

  cout << endl << "Matrices";
  smatrix I(1.,0.,0.,0.,1.,0.,0.,0.,1.);
  smatrix I2(I);
  I.printOnScreen();
  I2.printOnScreen();
  v1.multiplyOnTheLeft(I);
  cout << endl << "v1";
  v1.printOnScreen();
  //1       0.3     3.14159

  smatrix M1(1.,2.,3.,0.,1.,1.,2.,0.,1.);
  smatrix M2(0.,2.,1.,3.,1.,2.,0.,2.,0.);
  smatrix M2bis(M2);
  cout << endl << "Matrix M2bis";
  M2bis.printOnScreen();
//  Matrix M2bis
//0       2       1
//3       1       2
//0       2       0
  M1.testMultiplication(M2);
  //testMultiplication() output
//M1
//2       2       3
//7       7       12
//0       2       2

//M3
//2       2       3
//7       7       12
//0       2       2

  smatrix M3(1.,2.,3.,0.,1.,1.,2.,0.,1.);
  smatrix M4(0.,2.,1.,3.,1.,2.,0.,2.,0.);
  // M4 = M4 * M3
  M4.multiplyOnTheRight(M3);
  cout << endl << "Matrix M4 after 1st multiplication";
  M4.printOnScreen();
 // M4.printOnScreen();
//|0       2       1| |1      2     3|
//|3       1       2|*|0      1     1| =
//|0       2       0| |2      0     1|
//
//|2       2       3 |
//|7       7       12|
//|0       2       2 |

  smatrix M5(1.,2.,3.,0.,1.,1.,2.,0.,1.);
  M4.multiplyOnTheRight(M5);
  cout << endl << "Matrix M4 after 2nd multiplication";
  M4.printOnScreen();
//Matrix M4 after 2nd multiplication
//8       6       11
//31      21      40
//4       2       4

  v2.multiplyOnTheRight(M5);
  cout << endl << "Vector v2 after matrix multiplication on the right";
  v2.printOnScreen();
 // M4.printOnScreen();
//                       |1      2     3|
//|0.3       -1       1|*|0      1     1| =
//                       |2      0     1|
//
//|2.3       -0.4       0.9 |
//
  v2.multiplyOnTheLeft(M5);
  cout << endl << "Vector v2 after matrix multiplication on the left";
  v2.printOnScreen();
 // M4.printOnScreen();
//|1      2     3|
//|0      1     1| |2.3       -0.4       0.9 | =
//|2      0     1|
//
//|4.2     0.5     5.5|
//
  */
  /*
  cristal this_cristal1(
    1.570796827,1.570796827,1.570796827,
    1.570796827,1.570796827,1.570796827,
    1.,1.,1.,1.,1.,1.);
  this_cristal1.printOnScreen();

  cristal this_cristal2(
    0.7853981635,1.570796827,0.7853981635,
    1.570796827,0.7853981635,1.570796827,
    1.,2.,3.,4.,5.,6.);
  this_cristal2.printOnScreen();

  source this_source(1.,2.36,5.68);
  this_source.printOnScreen();
  */
/*
  double degToRad = 3.141592654 / 180.;
  double omegaInf = -5 * degToRad;
  double omegaSup = 70 * degToRad;
  double thetaInf =-10 * degToRad;
  double thetaSup =130 * degToRad;
  double chiInf  = -40 * degToRad;
  double chiSup  = 130 * degToRad;
  double phiInf  =   0 * degToRad;
  double phiSup  = 360 * degToRad;
  
  cout << endl << "********************";
  cout << endl << "***** EULERIAN *****";
  cout << endl << "********************";

  // Eulerian
  eulerian_angleConfiguration4C* eul4C = new
    eulerian_angleConfiguration4C(
    180.*degToRad, 0., 0., 90.*degToRad,
    omegaInf, omegaSup, chiInf, chiSup,
    phiInf, phiSup, thetaInf, thetaSup);
  eul4C->printOnScreen();
  eul4C->printStaticOnScreen();

  angleConfiguration* this_ac1 = eul4C->makeCopy();
  cout << endl << "ANGLE CONFIGURATION COPY";
  this_ac1->printOnScreen();
  eulerian_angleConfiguration4C* this_ac_eul4C = 
    (eulerian_angleConfiguration4C*)this_ac1;
  cout << endl << "EULERIAN ANGLE CONFIGURATION COPY";
  this_ac_eul4C->printOnScreen();

  delete eul4C;
  delete this_ac1;
  
  cout << endl << "*****************";
  cout << endl << "***** KAPPA *****";
  cout << endl << "*****************";

  // Kappa
  kappa_angleConfiguration4C* kap4C = new
    kappa_angleConfiguration4C(
      45.*degToRad, 45.*degToRad,
     -45.*degToRad,-45.*degToRad,
     -90.*degToRad, 90.*degToRad,
     -90.*degToRad, 90.*degToRad,
    -180.*degToRad,180.*degToRad,
    -135.*degToRad, 45.*degToRad);
  kap4C->printOnScreen();
  kap4C->printStaticOnScreen();

  angleConfiguration* this_ac2 = kap4C->makeCopy();
  cout << endl << "ANGLE CONFIGURATION COPY";
  this_ac2->printOnScreen();
  kappa_angleConfiguration4C* this_ac_kap4C = 
    (kappa_angleConfiguration4C*)this_ac2;
  cout << endl << "KAPPA ANGLE CONFIGURATION COPY";
  this_ac_kap4C->printOnScreen();

  delete kap4C;
  delete this_ac2;
*/
/*
  cristal this_cristal1(
    1.570796827,1.570796827,1.570796827,
    1.570796827,1.570796827,1.570796827,
    1.,1.,1.,1.,1.,1.);
  this_cristal1.printOnScreen();

  cristal this_cristal2(
    0.7853981635,0.7853981635,0.7853981635,
    1.,2.,3.);
  this_cristal2.printOnScreen();

  cristal this_cristal3(
    0.7853981635,0.7853981635,0.7853981635,
    1.,1.,1.);
  this_cristal3.printOnScreen();

  source this_source(1.,2.36,5.68);
  this_source.printOnScreen();

  double degToRad = 3.141592654 / 180.;
  double omegaInf = -5. * degToRad;
  double omegaSup = 70. * degToRad;
  double thetaInf =-10. * degToRad;
  double thetaSup =130. * degToRad;
  double chiInf  = -40. * degToRad;
  double chiSup  = 130. * degToRad;
  double phiInf  =   0. * degToRad;
  double phiSup  = 360. * degToRad;
  eulerian_angleConfiguration4C* eul4C_1 =
    new eulerian_angleConfiguration4C(
      180.*degToRad, 0., 0., 90.*degToRad,
      omegaInf, omegaSup, chiInf, chiSup,
      phiInf, phiSup, thetaInf, thetaSup);
  reflection r1(eul4C_1, 6, 6, 6, 
    reflection::relevance::Best);
  delete eul4C_1;
  r1.printOnScreen();

  eulerian_angleConfiguration4C* eul4C_2 =
    new eulerian_angleConfiguration4C(
      90.*degToRad, 0., 0., 180.*degToRad,
      omegaInf, omegaSup, chiInf, chiSup,
      phiInf, phiSup, thetaInf, thetaSup);
  reflection r2(eul4C_2, 15, 10, 8, 
    reflection::relevance::Best);
  delete eul4C_2;
  r2.printOnScreen();

  eulerianDiffractometer4C diff_4C(
    this_cristal2,this_source,
    r1,r2,mode::diffractometer_mode::bissector);

  eulerian_angleConfiguration4C* eac =
    (eulerian_angleConfiguration4C*)
    diff_4C.computeAngles(1, 1, 1);

  delete eac;

  diff_4C.printOnScreen();
*/
  // COMPUTING B
  double degToRad = 3.141592654 / 180.;
  cristal cubic_cristal1(
    1.570796827,1.570796827,1.570796827,
    1.,1.,1.);
  cubic_cristal1.printOnScreen();

  cristal orthorombic_cristal1(
    1.5707963267948966,
    1.5707963267948966,
    1.5707963267948966,
    1.,3.,4.);
  orthorombic_cristal1.printOnScreen();

  cout << endl << "**********************";
  cout << endl << "***** HEXAGONAL *****";
  cout << endl << "********************";
  cristal hexagonal_cristal1(
    PI / 2., 2. * PI / 3., PI / 2.,
    1.,2.,1.);
  hexagonal_cristal1.printOnScreen();

  cout << endl << "**********************";
  cout << endl << "***** HEXAGONAL *****";
  cout << endl << "********************";
  cristal hexagonal_cristal2(
    2 * PI / 3., PI / 2., PI / 2.,
    2.,1.,1.);
  hexagonal_cristal2.printOnScreen();

  int h = 1;
  int k = 0;
  int l = 0;
  // COMPUTING U
  source this_source(1.,2.36,5.68);

  eulerianDiffractometer4C diff_4C(
    orthorombic_cristal1,this_source,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac =
    (eulerian_angleConfiguration4C*)
    diff_4C.computeAngles(h,k,l);
  cout << endl << "**********************";
  cout << endl << "SOLUTION FROM ("<<h<<","<<k<<","<<l<<")";
  eac->printOnScreen();
  delete eac;

  h = 0;
  k = 1;
  l = 0;
  eulerianDiffractometer4C diff_4C_2(
    orthorombic_cristal1,this_source,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac2 =
    (eulerian_angleConfiguration4C*)
    diff_4C_2.computeAngles(h,k,l);
  cout << endl << "**********************";
  cout << endl << "SOLUTION FROM ("<<h<<","<<k<<","<<l<<")";
  eac2->printOnScreen();
  delete eac2;

  h = 0;
  k = 0;
  l = 1;
  eulerianDiffractometer4C diff_4C_3(
    orthorombic_cristal1,this_source,
    mode::diffractometer_mode::bissector);
  eulerian_angleConfiguration4C* eac3 =
    (eulerian_angleConfiguration4C*)
    diff_4C_3.computeAngles(h,k,l);
  cout << endl << "**********************";
  cout << endl << "SOLUTION FROM ("<<h<<","<<k<<","<<l<<")";
  eac3->printOnScreen();
  delete eac3;

  //diff_4C.printOnScreen();


  cout << endl;

  return (0);
}
/* 
Vector and matrix output generated on October 19th 2004, 3 p.m.

1       0.3     3.14159

0.3     -1      1

1       0.3     3.14159

(v1,v2) scalar product is : 3.14159
(v2,v1) scalar product is : 3.14159
v1 norm2 is : 3.31053
v2 norm2 is : 1.44568
v1 infinite norm is : 3.14159
v2 infinite norm is : 1
Matrices
1       0       0
0       1       0
0       0       1

1       0       0
0       1       0
0       0       1

v1
1       0.3     3.14159

Matrix M2bis
0       2       1
3       1       2
0       2       0

M1

2       2       3
7       7       12
0       2       2

M3

2       2       3
7       7       12
0       2       2

Matrix M4 after 1st multiplication
2       2       3
7       7       12
0       2       2

Matrix M4 after 2nd multiplication
8       6       11
31      21      40
4       2       4

Vector v2 after matrix multiplication on the right
2.3     -0.4    0.9

Vector v2 after matrix multiplication on the left
4.2     0.5     5.5

Press any key to continue
*/

/*
Cristal output generated on October 19th 2004 5 p.m.


CLASS cristal
Direct lattice
alpha1=1.5708   alpha2=1.5708   alpha3=1.5708

a1=1    a2=1    a3=1

Reciprocal lattice
beta1=1.5708    beta2=1.5708    beta3=1.5708

b1=1    b2=1    b3=1

1       -5.00205e-007   -5.00205e-007
0       1       5.00205e-007
0       0       1

CLASS cristal
Direct lattice
alpha1=0.785398 alpha2=1.5708   alpha3=0.785398

a1=1    a2=2    a3=3

Reciprocal lattice
beta1=1.5708    beta2=0.785398  beta3=1.5708

b1=4    b2=5    b3=6

4       -2.50103e-006   4.24264
0       5       -3
0       0       0.333333

CLASS source
Wave length = 1 Monochromator angle = 2.36      Undulator gap = 5.68

Press any key to continue
*/
/* Angle configuration output generated October 22th 11.00 a.m.

********************
***** EULERIAN *****
********************
CLASS eulerian_angleConfiguration4C
Omega = 3.14159 Chi = 0 Phi = 0 2Theta = 1.5708

CLASS eulerian_angleConfiguration4C static variables
[OmegaInf = -0.0872665   OmegaSup = 1.22173]    [ChiInf = -0.698132      ChiSup
= 2.26893]      [PhiInf = 0      PhiSup = 6.28319]      [2ThetaInf = -0.174533
 2ThetaSup = 2.26893]

ANGLE CONFIGURATION COPY
CLASS eulerian_angleConfiguration4C
Omega = 3.14159 Chi = 0 Phi = 0 2Theta = 1.5708

EULERIAN ANGLE CONFIGURATION COPY
CLASS eulerian_angleConfiguration4C
Omega = 3.14159 Chi = 0 Phi = 0 2Theta = 1.5708

*****************
***** KAPPA *****
*****************
CLASS kappa_angleConfiguration4C
Omega = 0.785398        Kappa = 0.785398        Phi = -0.785398 2Theta = -0.7853
98

CLASS kappa_angleConfiguration4C static variables
[OmegaInf = -1.5708      OmegaSup = 1.5708]     [KappaInf = -1.5708      KappaSu
p = 1.5708]     [PhiInf = -3.14159       PhiSup = 3.14159]      [2ThetaInf = -2.
35619    2ThetaSup = 0.785398]

ANGLE CONFIGURATION COPY
CLASS kappa_angleConfiguration4C
Omega = 0.785398        Kappa = 0.785398        Phi = -0.785398 2Theta = -0.7853
98

KAPPA ANGLE CONFIGURATION COPY
CLASS kappa_angleConfiguration4C
Omega = 0.785398        Kappa = 0.785398        Phi = -0.785398 2Theta = -0.7853
98

Press any key to continue
*/
/*

CLASS cristal
Direct lattice
alpha1=1.5708   alpha2=1.5708   alpha3=1.5708

a1=1    a2=1    a3=1

Reciprocal lattice
beta1=1.5708    beta2=1.5708    beta3=1.5708

b1=1    b2=1    b3=1

1       -5.00205e-007   -5.00205e-007
0       1       5.00205e-007
0       0       1

CLASS cristal
Direct lattice
alpha1=0.785398 alpha2=1.5708   alpha3=0.785398

a1=1    a2=2    a3=3

Reciprocal lattice
beta1=1.5708    beta2=0.785398  beta3=1.5708

b1=4    b2=5    b3=6

4       -2.50103e-006   4.24264
0       5       -3
0       0       0.333333

CLASS source
Wave length = 1 Monochromator angle = 2.36      Undulator gap = 5.68

CLASS reflection
h = 6   k = 6   l = 6   relevance = 3
CLASS eulerian_angleConfiguration4C
Omega = 3.14159 Chi = 0 Phi = 0 2Theta = 1.5708

CLASS reflection
h = 15  k = 10  l = 8   relevance = 3
CLASS eulerian_angleConfiguration4C
Omega = 1.5708  Chi = 0 Phi = 0 2Theta = 3.14159

CLASS diffractometer
CLASS cristal
Direct lattice
alpha1=0.785398 alpha2=1.5708   alpha3=0.785398

a1=1    a2=2    a3=3

Reciprocal lattice
beta1=1.5708    beta2=0.785398  beta3=1.5708

b1=4    b2=5    b3=6

4       -2.50103e-006   4.24264
0       5       -3
0       0       0.333333

CLASS source
Wave length = 1 Monochromator angle = 2.36      Undulator gap = 5.68

CLASS reflection
h = 6   k = 6   l = 6   relevance = 3
CLASS eulerian_angleConfiguration4C
Omega = 3.14159 Chi = 0 Phi = 0 2Theta = 1.5708

CLASS reflection
h = 15  k = 10  l = 8   relevance = 3
CLASS eulerian_angleConfiguration4C
Omega = 1.5708  Chi = 0 Phi = 0 2Theta = 3.14159

CLASS eulerianDiffractometer4C
Press any key to continue
*/
