
#include "svector.h"

namespace hkl {

svector::svector() :
  _x(0),
  _y(0),
  _z(0)
{
  // Bouml preserved body begin 0001F802
  // Bouml preserved body end 0001F802
}

svector::svector(double x, double y, double z) :
  _x(x),
  _y(y),
  _z(z)
{
  // Bouml preserved body begin 0001FA02
  // Bouml preserved body end 0001FA02
}

svector::svector(const hkl::svector & source) :
  _x(source._x),
  _y(source._y),
  _z(source._z)
{
  // Bouml preserved body begin 0001F902
  // Bouml preserved body end 0001F902
}

double & svector::x() 
{
  // Bouml preserved body begin 0001FA82
      return _x;
  // Bouml preserved body end 0001FA82
}

double & svector::y() 
{
  // Bouml preserved body begin 0001FB02
      return _y;
  // Bouml preserved body end 0001FB02
}

double & svector::z() 
{
  // Bouml preserved body begin 0001FB82
      return _z;
  // Bouml preserved body end 0001FB82
}

double const & svector::x() const 
{
  // Bouml preserved body begin 0001FC02
      return _x;
  // Bouml preserved body end 0001FC02
}

double const & svector::y() const 
{
  // Bouml preserved body begin 0001FC82
      return _y;
  // Bouml preserved body end 0001FC82
}

double const & svector::z() const 
{
  // Bouml preserved body begin 0001FD02
      return _z;
  // Bouml preserved body end 0001FD02
}

bool svector::operator==(const hkl::svector & v) const 
{
  // Bouml preserved body begin 0001FD82
      return fabs(_x - v._x) < constant::math::epsilon
        && fabs(_y - v._y) < constant::math::epsilon
        && fabs(_z - v._z) < constant::math::epsilon;
  // Bouml preserved body end 0001FD82
}

hkl::svector & svector::operator*=(const hkl::svector & v) 
{
  // Bouml preserved body begin 0001FE02
      _x *= v._x;
      _y *= v._y;
      _z *= v._z;
      
      return *this;
  // Bouml preserved body end 0001FE02
}

hkl::svector & svector::operator*=(const hkl::smatrix & M) 
{
  // Bouml preserved body begin 00020082
      double x, y, z;
      x = _x;
      y = _y;
      z = _z;
      
      _x = x * M._m11 + y * M._m21 + z * M._m31;
      _y = x * M._m12 + y * M._m22 + z * M._m32;
      _z = x * M._m13 + y * M._m23 + z * M._m33;
      
      return *this;
  // Bouml preserved body end 00020082
}

hkl::svector & svector::operator*=(const double & d) 
{
  // Bouml preserved body begin 00020102
      _x *= d;
      _y *= d;
      _z *= d;
      
      return *this;
  // Bouml preserved body end 00020102
}

hkl::svector & svector::operator/=(const double & d) 
{
  // Bouml preserved body begin 00020182
      _x /= d;
      _y /= d;
      _z /= d;
      
      return *this;
  // Bouml preserved body end 00020182
}

hkl::svector & svector::operator-=(const hkl::svector & v) 
{
  // Bouml preserved body begin 00020202
      _x -= v._x;
      _y -= v._y;
      _z -= v._z;
      
      return *this;
  // Bouml preserved body end 00020202
}

double svector::sum() const 
{
  // Bouml preserved body begin 00020282
      return _x + _y + _z;
  // Bouml preserved body end 00020282
}

void svector::set(double x, double y, double z) 
{
  // Bouml preserved body begin 00020302
      _x = x;
      _y = y;
      _z = z;
  // Bouml preserved body end 00020302
}

double svector::scalar(const hkl::svector & v) const 
{
  // Bouml preserved body begin 00020382
      return _x * v._x + _y * v._y + _z * v._z;
  // Bouml preserved body end 00020382
}

hkl::svector svector::vectorialProduct(const hkl::svector & v) const 
{
  // Bouml preserved body begin 00020402
      svector z;
      
      z._x = _y * v._z - _z * v._y;
      z._y = _z * v._x - _x * v._z;
      z._z = _x * v._y - _y * v._x;
      
      return z;
  // Bouml preserved body end 00020402
}

double svector::angle(const hkl::svector & v) const 
{
  // Bouml preserved body begin 00020482
      double norm_v = v.norm2();
      double norm_this = norm2();
      double norm = norm_v * norm_this;
      
      double cosine = scalar(v) / norm;
      // problem with round
      if (cosine >= 1 ) return 0;
      if (cosine <= -1 ) return constant::math::pi;
      return acos(cosine);
  // Bouml preserved body end 00020482
}

hkl::smatrix svector::axisSystem(const hkl::svector & v) const 
{
  // Bouml preserved body begin 00020502
      smatrix M;
      
      svector XX = normalize();
      svector ZZ = vectorialProduct(v).normalize();
      svector YY = ZZ.vectorialProduct(XX);
      
      M.set(XX._x, YY._x, ZZ._x,
            XX._y, YY._y, ZZ._y,
            XX._z, YY._z, ZZ._z);
      
      return M;
  // Bouml preserved body end 00020502
}

double svector::norm2() const 
{
  // Bouml preserved body begin 00020582
      return sqrt(_x * _x + _y * _y + _z * _z);
  // Bouml preserved body end 00020582
}

hkl::svector svector::normalize() const 
{
  // Bouml preserved body begin 00020602
      double norm = this->norm2();
      return svector(_x / norm, _y / norm, _z / norm);
  // Bouml preserved body end 00020602
}

bool svector::isColinear(const hkl::svector & v) const 
{
  // Bouml preserved body begin 00020682
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
  // Bouml preserved body end 00020682
}

void svector::randomize() 
{
  // Bouml preserved body begin 00020702
      _x = -1 + 2 * rand()/(RAND_MAX+1.0);
      _y = -1 + 2 * rand()/(RAND_MAX+1.0);
      _z = -1 + 2 * rand()/(RAND_MAX+1.0);
  // Bouml preserved body end 00020702
}

hkl::svector & svector::randomize(const hkl::svector & v) 
{
  // Bouml preserved body begin 00020782
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
  // Bouml preserved body end 00020782
}

hkl::svector & svector::randomize(const hkl::svector & v1, const hkl::svector & v2) 
{
  // Bouml preserved body begin 00020802
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
  // Bouml preserved body end 00020802
}

/**
 * \brief rotate a vector around another one with an angle.
 * \param axe The svector corresponding to the rotation axe.
 * \param angle the angle of rotation.
 * \return The new vector.
 */
hkl::svector svector::rotatedAroundVector(const hkl::svector & axe, double angle) const 
{
  // Bouml preserved body begin 00020902
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
  // Bouml preserved body end 00020902
}

std::ostream & svector::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00020982
      flux << "<" << _x << ", " << _y << ", " << _z << ">";
      return flux;
  // Bouml preserved body end 00020982
}

std::ostream & svector::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00020A02
      flux << std::setprecision(constant::math::precision)
        << " " << _x
        << " " << _y
        << " " << _z
        << std::endl;
      return flux;
  // Bouml preserved body end 00020A02
}

/*!
 * \brief Restore a svector from a stream.
 * \param flux The stream containing the svector to restore.
 */
std::istream & svector::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00020A82
      flux >> std::setprecision(constant::math::precision)
        >> _x
        >> _y
        >> _z;
      return flux;
  // Bouml preserved body end 00020A82
}

double svector::norminf() const 
{
  // Bouml preserved body begin 00021182
      double t = 0.0;
      
      if (fabs(_x) > fabs(_y))
        t = fabs(_x);
      else
        t = fabs(_y);
      if (fabs(_z) > t)
        t = fabs(_z);
      return t;
  // Bouml preserved body end 00021182
}

smatrix::smatrix() :
  _m11(0), _m12(0), _m13(0),
  _m21(0), _m22(0), _m23(0),
  _m31(0), _m32(0), _m33(0)
{
  // Bouml preserved body begin 0001FE82
  // Bouml preserved body end 0001FE82
}

smatrix::smatrix(double m11, double m12, double m13, double m21, double m22, double m23, double m31, double m32, double m33) :
  _m11(m11), _m12(m12), _m13(m13),
  _m21(m21), _m22(m22), _m23(m23),
  _m31(m31), _m32(m32), _m33(m33)
{
  // Bouml preserved body begin 00020D02
  // Bouml preserved body end 00020D02
}

smatrix::smatrix(double euler_x, double euler_y, double euler_z) 
{
  // Bouml preserved body begin 00020D82
      double A = cos(euler_x);
      double B = sin(euler_x);
      double C = cos(euler_y);
      double D = sin(euler_y);
      double E = cos(euler_z);
      double F = sin(euler_z);
      double AD = A * D;
      double BD = B * D;
      
      _m11 = C*E;
      _m12 =-C*F;
      _m13 = D;
      _m21 = BD * E + A * F;
      _m22 =-BD * F + A * E;
      _m23 =-B * C;
      _m31 =-AD * E + B * F;
      _m32 = AD * F + B * E;
      _m33 = A * C;
  // Bouml preserved body end 00020D82
}

smatrix::smatrix(const hkl::smatrix & source) :
  _m11(source._m11), _m12(source._m12), _m13(source._m13),
  _m21(source._m21), _m22(source._m22), _m23(source._m23),
  _m31(source._m31), _m32(source._m32), _m33(source._m33)
{
  // Bouml preserved body begin 0001FF82
  // Bouml preserved body end 0001FF82
}

bool smatrix::operator==(const hkl::smatrix & M) const 
{
  // Bouml preserved body begin 00020E02
      if (fabs(_m11 - M._m11) < constant::math::epsilon
          && fabs(_m12 - M._m12) < constant::math::epsilon
          && fabs(_m13 - M._m13) < constant::math::epsilon
          && fabs(_m21 - M._m21) < constant::math::epsilon
          && fabs(_m22 - M._m22) < constant::math::epsilon
          && fabs(_m23 - M._m23) < constant::math::epsilon
          && fabs(_m31 - M._m31) < constant::math::epsilon
          && fabs(_m32 - M._m32) < constant::math::epsilon
          && fabs(_m33 - M._m33) < constant::math::epsilon)
        return true;
      else
        return false;
  // Bouml preserved body end 00020E02
}

hkl::smatrix & smatrix::operator*=(const hkl::smatrix & M) 
{
  // Bouml preserved body begin 00020E82
      double m11 = _m11;
      double m12 = _m12;
      double m13 = _m13;
      double m21 = _m21;
      double m22 = _m22;
      double m23 = _m23;
      double m31 = _m31;
      double m32 = _m32;
      double m33 = _m33;
      
      double M11 = M._m11;
      double M12 = M._m12;
      double M13 = M._m13;
      double M21 = M._m21;
      double M22 = M._m22;
      double M23 = M._m23;
      double M31 = M._m31;
      double M32 = M._m32;
      double M33 = M._m33;
      
      _m11 = m11 * M11 + m12 * M21 + m13 * M31;
      _m12 = m11 * M12 + m12 * M22 + m13 * M32;
      _m13 = m11 * M13 + m12 * M23 + m13 * M33;
      _m21 = m21 * M11 + m22 * M21 + m23 * M31;
      _m22 = m21 * M12 + m22 * M22 + m23 * M32;
      _m23 = m21 * M13 + m22 * M23 + m23 * M33;
      _m31 = m31 * M11 + m32 * M21 + m33 * M31;
      _m32 = m31 * M12 + m32 * M22 + m33 * M32;
      _m33 = m31 * M13 + m32 * M23 + m33 * M33;
      
      return *this;
  // Bouml preserved body end 00020E82
}

hkl::smatrix smatrix::operator*(const hkl::smatrix & M) const 
{
  // Bouml preserved body begin 00020F02
      smatrix result(*this);
      
      result *= M;
      
      return result;
  // Bouml preserved body end 00020F02
}

hkl::svector smatrix::operator*(const hkl::svector & v) const 
{
  // Bouml preserved body begin 00020F82
      svector result;
      
      result._x = v._x * _m11 + v._y * _m12 + v._z * _m13;
      result._y = v._x * _m21 + v._y * _m22 + v._z * _m23;
      result._z = v._x * _m31 + v._y * _m32 + v._z * _m33;
      
      return result;
  // Bouml preserved body end 00020F82
}

void smatrix::set(double m11, double m12, double m13, double m21, double m22, double m23, double m31, double m32, double m33) 
{
  // Bouml preserved body begin 00020882
      _m11 = m11;
      _m12 = m12;
      _m13 = m13;
      _m21 = m21;
      _m22 = m22;
      _m23 = m23;
      _m31 = m31;
      _m32 = m32;
      _m33 = m33;
  // Bouml preserved body end 00020882
}

void smatrix::set(double euler_x, double euler_y, double euler_z) 
{
  // Bouml preserved body begin 00021102
      double A = cos(euler_x);
      double B = sin(euler_x);
      double C = cos(euler_y);
      double D = sin(euler_y);
      double E = cos(euler_z);
      double F = sin(euler_z);
      double AD = A * D;
      double BD = B * D;
      
      _m11 = C*E;
      _m12 =-C*F;
      _m13 = D;
      _m21 = BD * E + A * F;
      _m22 =-BD * F + A * E;
      _m23 =-B * C;
      _m31 =-AD * E + B * F;
      _m32 = AD * F + B * E;
      _m33 = A * C;
  // Bouml preserved body end 00021102
}

double smatrix::get(unsigned int i, unsigned int j) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00020C82
      if (i==0 && j==0)
        return _m11;
      if (i==1 && j==0)
        return _m21;
      if (i==2 && j==0)
        return _m31;
      
      if (i==0 && j==1)
        return _m12;
      if (i==1 && j==1)
        return _m22;
      if (i==2 && j==1)
        return _m32;
      
      if (i==0 && j==2)
        return _m13;
      if (i==1 && j==2)
        return _m23;
      if (i==2 && j==2)
        return _m33;
      else
        HKLEXCEPTION("Unable to get such an element",
                     "i>=3 or i<0 or j>=3 or j<0");
  // Bouml preserved body end 00020C82
}

hkl::svector smatrix::asEulerian() const 
{
  // Bouml preserved body begin 00021002
      svector eulerian;
      double angle_x, angle_y, angle_z;
      double tx, ty;
      
      angle_y = asin( _m13 );        /* Calculate Y-axis angle */
      double C = cos( angle_y );
      if (fabs(C) > constant::math::epsilon)
      {             /* Gimball lock? */
          tx =  _m33 / C;           /* No, so get X-axis angle */
          ty = -_m23 / C;
          angle_x = atan2( ty, tx );
          tx =  _m11 / C;            /* Get Z-axis angle */
          ty = -_m12 / C;
          angle_z = atan2( ty, tx );
      }
      else
      {                               /* Gimball lock has occurred */
          angle_x  = 0.;              /* Set X-axis angle to zero */
          tx      =  _m22;         /* And calculate Z-axis angle */
          ty      =  _m21;
          angle_z  = atan2( ty, tx );
      }
      eulerian.set(angle_x, angle_y, angle_z);
      
      return eulerian;
  // Bouml preserved body end 00021002
}

hkl::smatrix smatrix::transpose() 
{
  // Bouml preserved body begin 00021082
      smatrix M(*this);
      
      _m11 = M._m11;
      _m12 = M._m21;
      _m13 = M._m31;
      _m21 = M._m12;
      _m22 = M._m22;
      _m23 = M._m32;
      _m31 = M._m13;
      _m32 = M._m23;
      _m33 = M._m33;
      
      return (*this);
  // Bouml preserved body end 00021082
}

std::ostream & smatrix::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00020B02
      flux << std::endl;
      flux << std::showpoint << std::showpos;
      flux << _m11 << '\t' << _m12 << '\t' << _m13 << std::endl;
      flux << _m21 << '\t' << _m22 << '\t' << _m23 << std::endl;
      flux << _m31 << '\t' << _m32 << '\t' << _m33 << std::endl;
      flux << std::noshowpoint << std::noshowpos << std::dec;
      return flux;
  // Bouml preserved body end 00020B02
}

std::ostream & smatrix::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00020B82
      flux << std::setprecision(constant::math::precision);
      flux << " " << _m11 << " " << _m12 << " " << _m13;
      flux << " " << _m21 << " " << _m22 << " " << _m23;
      flux << " " << _m31 << " " << _m32 << " " << _m33 << std::endl;
      return flux;
  // Bouml preserved body end 00020B82
}

/*!
 * \brief Restore a smatrix from a stream.
 * \param flux The stream containing the smatrix to restore.
 */
std::istream & smatrix::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00020C02
      flux >> std::setprecision(constant::math::precision);
      flux >> _m11 >> _m12 >> _m13;
      flux >> _m21 >> _m22 >> _m23;
      flux >> _m31 >> _m32 >> _m33;
      return flux;
  // Bouml preserved body end 00020C02
}


} // namespace hkl

/**
 * \brief Surcharge de l'operateur << pour la class svector
 * @param flux 
 * @param m 
 * @return 
 */
std::ostream & operator << (std::ostream & flux, hkl::svector const & v)
{
  return v.printToStream(flux);
}

/**
 * \brief Surcharge de l'operateur << pour la class smatrix
 * @param flux 
 * @param m 
 * @return 
 */
std::ostream & operator << (std::ostream & flux, hkl::smatrix const & m)
{
  return m.printToStream(flux);
}

