#include "constants.h"

double physicalConstants::m_tau = 1.;

/// The usual value of pi
double mathematicalConstants::m_PI = 3.14159265358979323846;

/// This precision factor is used to test if two 
/// angles are the same.
/// \brief The first precision factor. 
double mathematicalConstants::m_EPSILON_0 = 1.e-6;
/// This precision factor is used to test if a double 
/// precision number is null.
/// \brief The second precision factor. 
double mathematicalConstants::m_EPSILON_1 = 1.e-10;

/// All the computations are performed in radians, however 
/// if we want to have them in degrees (to print them out 
/// for example) we just need to multiply them by (180 / PI)
/// \brief To convert an angle in degrees (180 / PI).
double mathematicalConstants::m_convertAnglesToDegrees = 57.2957795130823208;

/// To convert an angle in radians (PI / 180)
double mathematicalConstants::m_convertAnglesToRadians = 0.01745329251994330;

