
#include "svector.h"

namespace hkl {

svector::svector() :
  _x(0),
  _y(0),
  _z(0)
{
}

svector::svector(double x, double y, double z) :
  _x(x),
  _y(y),
  _z(z)
{
}

svector::svector(const svector & source) :
  _x(source._x),
  _y(source._y),
  _z(source._z)
{
}

svector::~svector() 
{
}

svector & svector::operator=(const svector & source) 
{
  _x = source._x;
  _y = source._y;
  _z = source._z;
}

double & svector::x() 
{
  return _x;
}

double & svector::y() 
{
  return _y;
}

double & svector::z() 
{
  return _z;
}

double const & svector::x() const 
{
  return _x;
}

double const & svector::y() const 
{
  return _x;
}

double const & svector::z() const 
{
  return _z;
}

bool svector::operator==(const svector & v) const 
{
  return fabs(_x - v._x) < Constants::math::epsilon
    && fabs(_y - v._y) < Constants::math::epsilon
    && fabs(_z - v._z) < Constants::math::epsilon;
}

svector & svector::operator*=(const svector & v) 
{
  _x *= v._x;
  _y *= v._y;
  _z *= v._z;
  
  return *this;
}

svector & svector::operator*=(const smatrix & M) 
{
  double x, y, z;
  x = _x;
  y = _y;
  z = _z;
  
  _x = x * M._m11 + y * M._m21 + z * M._m31;
  _y = x * M._m12 + y * M._m22 + z * M._m32;
  _z = x * M._m13 + y * M._m23 + z * M._m33;
  
  return *this;
}

svector & svector::operator*=(const double & d) 
{
  _x *= d;
  _y *= d;
  _z *= d;
  
  return *this;
}

svector & svector::operator/=(const double & d) 
{
  _x /= d;
  _y /= d;
  _z /= d;
  
  return *this;
}

svector & svector::operator-=(const svector & v) 
{
  _x -= v._x;
  _y -= v._y;
  _z -= v._z;
  
  return *this;
}

double svector::sum() const 
{
  return _x + _y + _z;
}

void svector::set(double x, double y, double z) 
{
  _x = x;
  _y = y;
  _z = z;
}

double svector::scalar(const svector & v) const 
{
  return _x * v._x + _y * v._y + _z * v._z;
}

svector svector::vectorialProduct(const svector & v) const 
{
  svector z;
  
  z._x = _y * v._z - _z * v._y;
  z._y = _z * v._x - _x * v._z;
  z._z = _x * v._y - _y * v._x;
  
  return z;
}

double svector::angle(svector & v) const 
{
  double norm_v = v.norm2();
  double norm_this = norm2();
  double norm = norm_v * norm_this;
  
  double cosine = scalar(v) / norm;
  
  return acos(cosine);
}

smatrix svector::axisSystem(const svector & v) const 
{
  smatrix M;
  
  svector XX = normalize();
  svector ZZ = vectorialProduct(v).normalize();
  svector YY = ZZ.vectorialProduct(XX);
  
  M.set(XX._x, YY._x, ZZ._x,
        XX._y, YY._y, ZZ._y,
        XX._z, YY._z, ZZ._z);
  
  return M;
}

double svector::norm2() const 
{
  return sqrt(_x * _x + _y * _y + _z * _z);
}

svector svector::normalize() const 
{
  double norm = this->norm2();
  return svector(_x / norm, _y / norm, _z / norm);
}

bool svector::isColinear(const svector & v) const 
{
  if ((fabs(_x - v._x) <= Constants::math::epsilon
     && fabs(_y - v._y) <= Constants::math::epsilon
     && fabs(_z - v._z) <= Constants::math::epsilon)
     ||
     (fabs(_x + v._x) <= Constants::math::epsilon
      && fabs(_y + v._y) <= Constants::math::epsilon
      && fabs(_z + v._z) <= Constants::math::epsilon))
    return true;
  else
    return false;
}

void svector::randomize() 
{
  unsigned int i;
  
  _x = -1 + 2 * rand()/(RAND_MAX+1.0);
  _y = -1 + 2 * rand()/(RAND_MAX+1.0);
  _z = -1 + 2 * rand()/(RAND_MAX+1.0);
}

void svector::randomize(const svector & v) 
{
  unsigned int i;
  bool ko = true;
  do
    {
      _x = -1 + 2 * rand()/(RAND_MAX+1.0);
      _y = -1 + 2 * rand()/(RAND_MAX+1.0);
      _z = -1 + 2 * rand()/(RAND_MAX+1.0);
      if (!operator==(v))
        ko = false;
    }
  while (ko);
}

void svector::randomize(const svector & v1, const svector & v2) 
{
  unsigned int i;
  bool ko = true;
  do
    {
      _x = -1 + 2 * rand()/(RAND_MAX+1.0);
      _y = -1 + 2 * rand()/(RAND_MAX+1.0);
      _z = -1 + 2 * rand()/(RAND_MAX+1.0);
      if (!operator==(v1) && !operator==(v2))
        ko = false;
    }
  while (ko);
}

smatrix::smatrix() :
  _m11(0), _m12(0), _m13(0),
  _m21(0), _m22(0), _m23(0),
  _m31(0), _m32(0), _m33(0)
{
}

smatrix::~smatrix() 
{
}

smatrix::smatrix(const smatrix & source) :
  _m11(source._m11), _m12(source._m12), _m13(source._m13),
  _m21(source._m21), _m22(source._m22), _m23(source._m23),
  _m31(source._m31), _m32(source._m32), _m33(source._m33)
{
}

smatrix & smatrix::operator=(const smatrix & source) 
{
  _m11 = source._m11;
  _m12 = source._m12;
  _m13 = source._m13;
  _m21 = source._m21;
  _m22 = source._m22;
  _m23 = source._m23;
  _m31 = source._m31;
  _m32 = source._m32;
  _m33 = source._m33;
}

void smatrix::set(double m11, double m12, double m13, double m21, double m22, double m23, double m31, double m32, double m33) 
{
  _m11 = m11;
  _m12 = m12;
  _m13 = m13;
  _m21 = m21;
  _m22 = m22;
  _m23 = m23;
  _m31 = m31;
  _m32 = m32;
  _m33 = m33;
}


} // namespace hkl
