#include "svecmat.h"
#include <iostream>
#include <math.h>

// The class vector to store an array of double precision numbers.

// Default constructor.
svector::svector()
{
  m_v1 = 0.;
  m_v2 = 0.;
  m_v3 = 0.;
}

// Constructor to allocate a 3D vector and populate it with data.
svector::svector(const double el1, const double el2, const double el3)
{
  m_v1 = el1;
  m_v2 = el2;
  m_v3 = el3;
}

// Copy constructor.
svector::svector(const svector& v)
{
  m_v1 = v.m_v1;
  m_v2 = v.m_v2;
  m_v3 = v.m_v3;
}

double svector::get_X() const
{
  return m_v1;
}

double svector::get_Y() const
{
  return m_v2;
}

double svector::get_Z() const
{
  return m_v3;
}

// Scalar product.
double svector::scalar(const svector& u) const
{
  double t = m_v1 * u.m_v1 + m_v2 * u.m_v2 + m_v3 * u.m_v3;
  return t;
}

// Vector length.
double svector::norm2() const
{
  double t = m_v1 * m_v1 + m_v2 * m_v2 + m_v3 * m_v3;
  return sqrt(t);
}

// Infinite norm.
double svector::norminf() const
{
  double t = 0.0;

  if (fabs(m_v1) > fabs(m_v2))
    t = fabs(m_v1);
  else
    t = fabs(m_v2);
  if (fabs(m_v3) > t)
    t = fabs(m_v3);
  return t;
}

// v = v.M
void svector::multiplyOnTheRight(const smatrix& M)
{
  svector u(*this);

  m_v1 = u.m_v1 * M.m_mat11 + u.m_v2 * M.m_mat21 + u.m_v3 * M.m_mat31;
  m_v2 = u.m_v1 * M.m_mat12 + u.m_v2 * M.m_mat22 + u.m_v3 * M.m_mat32;
  m_v3 = u.m_v1 * M.m_mat13 + u.m_v2 * M.m_mat23 + u.m_v3 * M.m_mat33;
}

// v = M.v
void svector::multiplyOnTheLeft(const smatrix& M)
{
  svector u(*this);

  m_v1 = u.m_v1 * M.m_mat11 + u.m_v2 * M.m_mat12 + u.m_v3 * M.m_mat13;
  m_v2 = u.m_v1 * M.m_mat21 + u.m_v2 * M.m_mat22 + u.m_v3 * M.m_mat23;
  m_v3 = u.m_v1 * M.m_mat31 + u.m_v2 * M.m_mat32 + u.m_v3 * M.m_mat33;
}

void svector::printOnScreen() const
{
  std::cout << std::endl;
  std::cout << m_v1 << '\t' << m_v2 << '\t' << m_v3 << std::endl;
}

// Compute a colinear unit vector and store its length.
// unitVector = this / ||this|| = this / length
void svector::unitVector(svector& _unitVector, double& length) const
{
  double t = sqrt(m_v1 * m_v1 + m_v2 * m_v2 + m_v3 * m_v3);
  if (fabs(t) < 0.000001)
  {
    _unitVector.m_v1 = 0.;
    _unitVector.m_v2 = 0.;
    _unitVector.m_v3 = 0.;
    length = 0.;
  }
  else
  {
    _unitVector.m_v1 = m_v1 / t;
    _unitVector.m_v2 = m_v2 / t;
    _unitVector.m_v3 = m_v3 / t;
    length = t;
  }
}

// Vectorial product : Z = this * Y
void svector::vectorialProduct(const svector& Y, svector& Z) const
{
  Z.m_v3 = m_v1 * Y.m_v2 - m_v2 * Y.m_v1;
  Z.m_v2 = m_v3 * Y.m_v1 - m_v1 * Y.m_v3;
  Z.m_v1 = m_v2 * Y.m_v3 - m_v3 * Y.m_v2;
}

// Creation of a axis system with unit vectors.
// M = (vector1, vector2, vector3) where
// vector1 = this / || this ||
// vector2 = U / || U ||
// vector3 = vector1 * vector2
void svector::axisSystem(const svector Y, smatrix& M) const
{
  double t;
  svector XX;
  svector YY;
  svector ZZ;
  svector Z;

  vectorialProduct(Y,Z);

  unitVector(XX,t);
  Z.unitVector(ZZ,t);

  ZZ.vectorialProduct(XX,YY);

  M.set(
    XX.m_v1, YY.m_v1, ZZ.m_v1,
    XX.m_v2, YY.m_v2, ZZ.m_v2,
    XX.m_v3, YY.m_v3, ZZ.m_v3);
}
