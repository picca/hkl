#include "svecmat.h"

namespace hkl
  {

  svector::svector() :
      _data(valarray<double>(0., 3))
  {}

  svector::svector(double const & a, double const & b, double const & c) :
      _data(valarray<double>(0., 3))
  {
    _data[X] = a;
    _data[Y] = b;
    _data[Z] = c;
  }

  svector::svector(svector const & v) :
      _data(v._data)
  {}

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

  bool
  svector::operator ==(svector const & v) const
    {
      valarray<double> diff(v._data);
      diff -= _data;
      unsigned int i;

      for(i=0; i<3; i++)
        if (fabs(diff[i]) > constant::math::epsilon_0)
          return false;

      return true;
    }

  svector &
  svector::operator*=(svector const & v)
  {
    _data *= v._data;

    return *this;
  }

  svector &
  svector::operator*= (smatrix const & M)
  {
    svector u(*this);

    _data[X] = u._data[X] * M.m_mat11 + u._data[Y] * M.m_mat21 + u._data[Z] * M.m_mat31;
    _data[Y] = u._data[X] * M.m_mat12 + u._data[Y] * M.m_mat22 + u._data[Z] * M.m_mat32;
    _data[Z] = u._data[X] * M.m_mat13 + u._data[Y] * M.m_mat23 + u._data[Z] * M.m_mat33;

    return *this;
  }

  svector &
  svector::operator*= (double const & d)
  {
    _data *= d;

    return *this;
  }

  svector &
  svector::operator-=(svector const & v)
  {
    _data -= v._data;

    return *this;
  }

  double
  svector::sum(void) const
    {
      return _data.sum();
    }

  void
  svector::set(double const & a, double const & b, double const & c)
    {
      _data[X] = a;
      _data[Y] = b;
      _data[Z] = c;
    }

  // Scalar product.
  double
  svector::scalar(svector const & u) const
    {
      valarray<double> res(u._data);
      res *= _data;
      return res.sum();
    }

  svector
  svector::vectorialProduct(svector const & v) const
    {
      svector z;

      z._data[X] = _data[Y] * v._data[Z] - _data[Z] * v._data[Y];
      z._data[Y] = _data[Z] * v._data[X] - _data[X] * v._data[Z];
      z._data[Z] = _data[X] * v._data[Y] - _data[Y] * v._data[X];

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
        XX._data[X], YY._data[X], ZZ._data[X],
        XX._data[Y], YY._data[Y], ZZ._data[Y],
        XX._data[Z], YY._data[Z], ZZ._data[Z]);

      return M;
    }

  double
  svector::norm2(void) const
    {
      return sqrt(_data[X] * _data[X] + _data[Y] * _data[Y] + _data[Z] * _data[Z]);
    }

  double
  svector::norminf(void) const
    {
      double t = 0.0;

      if (fabs(_data[X]) > fabs(_data[Y]))
        t = fabs(_data[X]);
      else
        t = fabs(_data[Y]);
      if (fabs(_data[Z]) > t)
        t = fabs(_data[Z]);
      return t;
    }

  svector
  svector::normalize(void) const
    {
      double norm = this->norm2();
      return svector(_data[X] / norm, _data[Y] / norm, _data[Z] / norm);
    }

  void
  svector::randomize(void)
  {
    unsigned int i;

    for(i=0;i<3;i++)
      _data[i] = -1 + 2 * rand()/(RAND_MAX+1.0);
  }

  svector &
  svector::randomize(svector const & v)
  {
    unsigned int i;
    bool not_ok = true;
    do
      {
        for(i=0;i<3;i++)
          _data[i] = -1 + 2 * rand()/(RAND_MAX+1.0);
        if (!operator==(v))
          not_ok = false;
      }
    while (not_ok);
    return *this;
  }

  svector &
  svector::randomize(svector const & v1, svector const & v2)
  {
    unsigned int i;
    bool not_ok = true;
    do
      {
        for(i=0;i<3;i++)
          _data[i] = -1 + 2 * rand()/(RAND_MAX+1.0);
        if (!operator==(v1) && !operator==(v2))
          not_ok = false;
      }
    while (not_ok);
    return *this;
  }

  svector
  svector::rotatedAroundVector(svector const & axe, double const & angle) const
    {
      double c = cos(angle);
      double s = sin(angle);
      svector axe_n = axe.normalize();
      svector v;

      v._data[X] = (c + (1 - c) * axe_n._data[0] * axe_n._data[0]) * _data[0];
      v._data[X] += ((1 - c) * axe_n._data[0] * axe_n._data[1] - axe_n._data[2] * s) * _data[1];
      v._data[X] += ((1 - c) * axe_n._data[0] * axe_n._data[2] + axe_n._data[1] * s) * _data[2];

      v._data[Y] = ((1 - c) * axe_n._data[0] * axe_n._data[1] + axe_n._data[2] * s) * _data[0];
      v._data[Y] += (c + (1 - c) * axe_n._data[1] * axe_n._data[1]) * _data[1];
      v._data[Y] += ((1 - c) * axe_n._data[1] * axe_n._data[2] - axe_n._data[0] * s) * _data[2];

      v._data[Z] = ((1 - c) * axe_n._data[0] * axe_n._data[2] - axe_n._data[1] * s) * _data[0];
      v._data[Z] += ((1 - c) * axe_n._data[1] * axe_n._data[2] + axe_n._data[0] * s) * _data[1];
      v._data[Z] += (c + (1 - c) * axe_n._data[2] * axe_n._data[2]) * _data[2];

      return v;
    }

  ostream &
  svector::printToStream(ostream & flux) const
    {
      flux << "<" << _data[X] << ", " << _data[Y] << ", " << _data[Z] << ">";
      return flux;
    }

  ostream &
  svector::toStream(ostream & flux) const
    {
      flux << setprecision(constant::math::precision)
      << " " << _data[X]
      << " " << _data[Y]
      << " " << _data[Z]
      << endl;
      return flux;
    }

  istream &
  svector::fromStream(istream & flux)
  {
    flux >> setprecision(constant::math::precision)
    >> _data[X]
    >> _data[Y]
    >> _data[Z];
    return flux;
  }
} //namespace hkl
