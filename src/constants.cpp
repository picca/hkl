#include "constants.h"

double physicalConstants::m_tau = 1.;

// The common value of pi
double mathematicalConstants::m_PI = 3.14159265358979323846;

// The precision factors
double mathematicalConstants::m_EPSILON_0 = 1.e-6;
double mathematicalConstants::m_EPSILON_1 = 1.e-10;

// All the computations are performed in radians, however 
// if we want to have them in degrees (to print them out 
// for example) we just need to multiply them by (180. / PI)
double mathematicalConstants::m_convertAnglesToDegrees = 57.2957795130823208;

// Same thing as before but the other way.
double mathematicalConstants::m_convertAnglesToRadians = 0.01745329251994330;

