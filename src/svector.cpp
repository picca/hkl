#include "svecmat.h"

namespace hkl
  {

  svector::svector() :
      _x(0),
      _y(0),
      _z(0)
  {}

  svector::svector(double const & a, double const & b, double const & c) :
      _x(a),
      _y(b),
      _z(c)
  {}

  svector::svector(svector const & v) :
      _x(v._x),
      _y(v._y),
      _z(v._z)
  {}

  /*
  double &
  svector::operator[](unsigned int i)
  {
    return _data[i];
  }

  double const &
  svector::operator[](unsigned int i) const
    {
      return _data[i];
    }
  */

  bool
  svector::operator ==(svector const & v) const
    {
      return fabs(_x - v._x) < constant::math::epsilon_0
             && fabs(_y - v._y) < constant::math::epsilon_0
             && fabs(_z - v._z) < constant::math::epsilon_0;
    }

  svector &
  svector::operator*=(svector const & v)
  {
    _x *= v._x;
    _y *= v._y;
    _z *= v._z;
    //_data *= v._data;

    return *this;
  }

  svector &
  svector::operator*= (smatrix const & M)
  {
    double x, y, z;
    x = _x;
    y = _y;
    z = _z;

    _x = x * M.m_mat11 + y * M.m_mat21 + z * M.m_mat31;
    _y = x * M.m_mat12 + y * M.m_mat22 + z * M.m_mat32;
    _z = x * M.m_mat13 + y * M.m_mat23 + z * M.m_mat33;

    return *this;
  }

  svector &
  svector::operator*= (double const & d)
  {
    _x *= d;
    _y *= d;
    _z *= d;

    return *this;
  }

  svector &
  svector::operator/= (double const & d)
  {
    _x /= d;
    _y /= d;
    _z /= d;

    return *this;
  }

  svector &
  svector::operator-=(svector const & v)
  {
    _x -= v._x;
    _y -= v._y;
    _z -= v._z;

    return *this;
  }

  double
  svector::sum(void) const
    {
      return _x + _y + _z;
    }

  void
  svector::set(double const & a, double const & b, double const & c)
    {
      _x = a;
      _y = b;
      _z = c;
    }

  // Scalar product.
  double
  svector::scalar(svector const & v) const
    {
      return _x * v._x + _y * v._y + _z * v._z;
    }

  svector
  svector::vectorialProduct(svector const & v) const
    {
      svector z;

      z._x = _y * v._z - _z * v._y;
      z._y = _z * v._x - _x * v._z;
      z._z = _x * v._y - _y * v._x;

      return z;
    }

  double
  svector::angle(svector const & v) const
    {
      double norm_v = v.norm2();
      double norm_this = norm2();
      double norm = norm_v * norm_this;

      double cosine = scalar(v) / norm;

      return acos(cosine);
    }

  smatrix
  svector::axisSystem(svector const & v) const
    {
      smatrix M;

      svector XX = normalize();
      svector ZZ = vectorialProduct(v).normalize();
      svector YY = ZZ.vectorialProduct(XX);

      M.set(
        XX._x, YY._x, ZZ._x,
        XX._y, YY._y, ZZ._y,
        XX._z, YY._z, ZZ._z);

      return M;
    }

  double
  svector::norm2(void) const
    {
      return sqrt(_x * _x + _y * _y + _z * _z);
    }

  double
  svector::norminf(void) const
    {
      double t = 0.0;

      if (fabs(_x) > fabs(_y))
        t = fabs(_x);
      else
        t = fabs(_y);
      if (fabs(_z) > t)
        t = fabs(_z);
      return t;
    }

  svector
  svector::normalize(void) const
    {
      double norm = this->norm2();
      return svector(_x / norm, _y / norm, _z / norm);
    }

  bool
  svector::isColinear(svector const & v) const
    {
      if ((fabs(_x - v._x) <= constant::math::epsilon
           && fabs(_y - v._y) <= constant::math::epsilon
           && fabs(_z - v._z) <= constant::math::epsilon)
          ||
          (fabs(_x + v._x) <= constant::math::epsilon
           && fabs(_y + v._y) <= constant::math::epsilon
           && fabs(_z + v._z) <= constant::math::epsilon))
        return true;
      else
        return false;
    }

  void
  svector::randomize(void)
  {
    unsigned int i;

    _x = -1 + 2 * rand()/(RAND_MAX+1.0);
    _y = -1 + 2 * rand()/(RAND_MAX+1.0);
    _z = -1 + 2 * rand()/(RAND_MAX+1.0);
  }

  svector &
  svector::randomize(svector const & v)
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
    return *this;
  }

  svector &
  svector::randomize(svector const & v1, svector const & v2)
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
    return *this;
  }

  svector
  svector::rotatedAroundVector(svector const & axe, double const & angle) const
    {
      double c = cos(angle);
      double s = sin(angle);
      svector axe_n = axe.normalize();
      svector v;

      v._x = (c + (1 - c) * axe_n._x * axe_n._x) * _x;
      v._x += ((1 - c) * axe_n._x * axe_n._y - axe_n._z * s) * _y;
      v._x += ((1 - c) * axe_n._x * axe_n._z + axe_n._y * s) * _z;

      v._y = ((1 - c) * axe_n._x * axe_n._y + axe_n._z * s) * _x;
      v._y += (c + (1 - c) * axe_n._y * axe_n._y) * _y;
      v._y += ((1 - c) * axe_n._y * axe_n._z - axe_n._x * s) * _z;

      v._z = ((1 - c) * axe_n._x * axe_n._z - axe_n._y * s) * _x;
      v._z += ((1 - c) * axe_n._y * axe_n._z + axe_n._x * s) * _y;
      v._z += (c + (1 - c) * axe_n._z * axe_n._z) * _z;

      return v;
    }

  ostream &
  svector::printToStream(ostream & flux) const
    {
      flux << "<" << _x << ", " << _y << ", " << _z << ">";
      return flux;
    }

  ostream &
  svector::toStream(ostream & flux) const
    {
      flux << setprecision(constant::math::precision)
      << " " << _x
      << " " << _y
      << " " << _z
      << endl;
      return flux;
    }

  istream &
  svector::fromStream(istream & flux)
  {
    flux >> setprecision(constant::math::precision)
    >> _x
    >> _y
    >> _z;
    return flux;
  }
} //namespace hkl
