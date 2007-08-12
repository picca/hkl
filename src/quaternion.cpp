
#include "quaternion.h"

namespace hkl {

 /*!
  * \brief Default constructor
  * 
  * Create a new quaternion and set all its components to 0.0
  */
Quaternion::Quaternion() :
  _a(1),
  _b(0),
  _c(0),
  _d(0)
{
  // Bouml preserved body begin 00021202
  // Bouml preserved body end 00021202
}

/*!
 * \brief This constructor creates a quaternion and populates it
 * \param a
 * \param b
 * \param c
 * \param d
 * 
 * Create a new quaternion with a, b, c and d as coordinates.
 */
Quaternion::Quaternion(double a, double b, double c, double d) :
  _a(a),
  _b(b),
  _c(c),
  _d(d) 
{
  // Bouml preserved body begin 00021282
  // Bouml preserved body end 00021282
}

/*!
 * \brief This constructor creates a quaternion and populates it
 * \param v
 * 
 * Create a new quaternion from a svector.
 */
Quaternion::Quaternion(const hkl::svector & v) 
{
  // Bouml preserved body begin 00022302
      _a = 0;
      _b = v.x();
      _c = v.y();
      _d = v.z();
  // Bouml preserved body end 00022302
}

/*!
 * \brief This constructor creates a quaternion from an angle and a vector
 * \param angle the rotation angle.
 * \param v the axe of the rotation.
 */
Quaternion::Quaternion(double angle, const hkl::svector & v) 
{
  // Bouml preserved body begin 00022382
      double norm = v.norm2();
      
      _a = cos(angle/2.);
      _b = sin(angle/2.) * v.x()/norm;
      _c = sin(angle/2.) * v.y()/norm;
      _d = sin(angle/2.) * v.z()/norm;
  // Bouml preserved body end 00022382
}

/*!
 * \brief Copy constructor.
 * \param q The Quaternion to copy from.
 */
Quaternion::Quaternion(const hkl::Quaternion & source) :
  _a(source._a),
  _b(source._b),
  _c(source._c),
  _d(source._d)
{
  // Bouml preserved body begin 00021302
  // Bouml preserved body end 00021302
}

/**
 * @brief Set the Quaternion parameters
 * @param a The 1st element.
 * @param b The 2nd element.
 * @param c The 3rd element.
 * @param d The 4th element.
 */
void Quaternion::set(double a, double b, double c, double d) 
{
  // Bouml preserved body begin 00039E02
    _a = a;
    _b = b;
    _c = c;
    _d = d;
  // Bouml preserved body end 00039E02
}

double & Quaternion::a() 
{
  // Bouml preserved body begin 00021482
      return _a;
  // Bouml preserved body end 00021482
}

double & Quaternion::b() 
{
  // Bouml preserved body begin 00021502
      return _b;
  // Bouml preserved body end 00021502
}

double & Quaternion::c() 
{
  // Bouml preserved body begin 00021582
      return _c;
  // Bouml preserved body end 00021582
}

double & Quaternion::d() 
{
  // Bouml preserved body begin 00022402
      return _d;
  // Bouml preserved body end 00022402
}

double const & Quaternion::a() const 
{
  // Bouml preserved body begin 00021602
      return _a;
  // Bouml preserved body end 00021602
}

double const & Quaternion::b() const 
{
  // Bouml preserved body begin 00021682
      return _b;
  // Bouml preserved body end 00021682
}

double const & Quaternion::c() const 
{
  // Bouml preserved body begin 00021702
      return _c;
  // Bouml preserved body end 00021702
}

double const & Quaternion::d() const 
{
  // Bouml preserved body begin 00022482
      return _d;
  // Bouml preserved body end 00022482
}

bool Quaternion::operator==(const hkl::Quaternion & q) const 
{
  // Bouml preserved body begin 00021782
      return fabs(_a - q._a) < constant::math::epsilon
        && fabs(_b - q._b) < constant::math::epsilon
        && fabs(_c - q._c) < constant::math::epsilon
        && fabs(_d - q._d) < constant::math::epsilon;
  // Bouml preserved body end 00021782
}

/*!
 * \brief Add a Quaternion to another one.
 * \param q The Quaternion to add.
 * \return A reference to the Quaternion which was added.
 */
hkl::Quaternion & Quaternion::operator+=(const hkl::Quaternion & q) 
{
  // Bouml preserved body begin 00022782
      _a += q._a;
      _b += q._b;
      _c += q._c;
      _d += q._d;
      
      return *this;
  // Bouml preserved body end 00022782
}

/*!
 * \brief Substract a Quaternion to another one.
 * \param q The Quaternion to substract.
 * \return A reference to the Quaternion which was substracted.
 */
hkl::Quaternion & Quaternion::operator-=(const hkl::Quaternion & q) 
{
  // Bouml preserved body begin 00022802
      _a -= q._a;
      _b -= q._b;
      _c -= q._c;
      _d -= q._d;
      
      return *this;
  // Bouml preserved body end 00022802
}

/*!
 * \brief Multiply a Quaternion to another one.
 * \param q The Quaternion to multiply.
 * \return A reference to the Quaternion which was multiplyed.
 */
hkl::Quaternion & Quaternion::operator*=(const hkl::Quaternion & q) 
{
  // Bouml preserved body begin 00021802
      Quaternion q1(*this);
      Quaternion q2;
      
      q2._a = q1._a*q._a - q1._b*q._b - q1._c*q._c - q1._d*q._d;
      q2._b = q1._a*q._b + q1._b*q._a + q1._c*q._d - q1._d*q._c;
      q2._c = q1._a*q._c - q1._b*q._d + q1._c*q._a + q1._d*q._b;
      q2._d = q1._a*q._d + q1._b*q._c - q1._c*q._b + q1._d*q._a;
      
      *this = q2;
      
      return *this;
  // Bouml preserved body end 00021802
}

/*!
 * \brief Divide a Quaternion by a double.
 * \param d The double to divide by.
 * \return A reference to the Quaternion which was modified.
 */
hkl::Quaternion & Quaternion::operator/=(const double & d) 
{
  // Bouml preserved body begin 00021982
      _a /= d;
      _b /= d;
      _c /= d;
      _d /= d;
      
      return *this;
  // Bouml preserved body end 00021982
}

/*!
 * \brief Compute de norm of the Quaternion.
 * \return The norme of the Quaternion.
 */
double Quaternion::norm2() const 
{
  // Bouml preserved body begin 00021D82
      return sqrt(_a * _a + _b * _b + _c * _c + _d * _d);
  // Bouml preserved body end 00021D82
}

/*!
 * \brief Compute the conjugated Quaternion.
 * \return The conjugate Quaternion.
 */
hkl::Quaternion Quaternion::conjugate() const 
{
  // Bouml preserved body begin 00022502
      return Quaternion(_a, -_b, -_c, -_d);
  // Bouml preserved body end 00022502
}

/*!
 * \brief Compute the dot product of a Quaternion.
 * \param q The Quaternion.
 * \return The dot Product.
 */
double Quaternion::dotProduct(const hkl::Quaternion & q) const 
{
  // Bouml preserved body begin 00022582
      Quaternion q1 = (*this).conjugate();
      q1 *= q;
      
      Quaternion q2 = q.conjugate();
      q2 *= (*this);
      
      q1 += q2;
      
      return q1._a/2.;
  // Bouml preserved body end 00022582
}

/*!
 * \brief Compute the invert Quaternion.
 * \return The invert Quaternion.
 */
hkl::Quaternion Quaternion::invert() const 
{
  // Bouml preserved body begin 00022602
      Quaternion q = (*this).conjugate();
      q /= (*this).dotProduct(*this);
      
      return q;
  // Bouml preserved body end 00022602
}

/*!
 * \brief Compute the rotation matrix of a Quaternion.
 * \return The rotation matrix of a Quaternion.
 *
 * to convert a quaternion to a Matrix:
 * \f$ q = a + b \cdot i + c \cdot j + d \cdot k \f$
 * 
 * \f$
 * \left(
 *   \begin{array}{ccc}
 *     a^2+b^2-c^2-d^2 & 2bc-2ad         & 2ac+2bd\\
 *     2ad+2bc         & a^2-b^2+c^2-d^2 & 2cd-2ab\\
 *     2bd-2ac         & 2ab+2cd         & a^2-b^2-c^2+d^2
 *   \end{array}
 * \right)
 * \f$
 */
hkl::smatrix Quaternion::asMatrix() const 
{
  // Bouml preserved body begin 00022682
      smatrix m(_a*_a+_b*_b-_c*_c-_d*_d, 2*(_b*_c-_a*_d),         2*(_a*_c+_b*_d),
                2*(_a*_d+_b*_c),         _a*_a-_b*_b+_c*_c-_d*_d, 2*(_c*_d-_a*_b),
                2*(_b*_d-_a*_c),         2*(_a*_b+_c*_d),         _a*_a-_b*_b-_c*_c+_d*_d);
      
      return m;
  // Bouml preserved body end 00022682
}

/*!
 * \brief Decompose a Quaternion into a rotation angle and an Axe of rotation.
 * \param[out] angle The angle of the rotation will be strore in this variable.
 * \param[out] axe The axe of rotation will be store in this variable.
 */
void Quaternion::getAngleAndAxe(double & angle, hkl::svector & axe) const 
{
  // Bouml preserved body begin 00021A82
      double norm = norm2();
      // compute the angle
      double cos_angle = _a / norm;
      angle = acos(cos_angle) * 2.0;
      // compute the axe
      // cout << " " << (*this)[0];
      double sin_angle = sin(angle / 2);
      if (fabs(sin_angle) > constant::math::epsilon)
      {
          svector myaxe;
          myaxe.x() = _b / sin_angle / norm;
          myaxe.y() = _c / sin_angle / norm;
          myaxe.z() = _d / sin_angle / norm;
      
          if (myaxe.x()+axe.x() < constant::math::epsilon
              && myaxe.y()+axe.y() < constant::math::epsilon
              && myaxe.z()+axe.z() < constant::math::epsilon) // myaxe == -axe
            {
              angle = -angle;
            }
          else
              if (!(myaxe == axe))
                  axe = myaxe;
      
          angle = convenience::normalizeAngle(angle);
      }
  // Bouml preserved body end 00021A82
}

/**
 * @brief Get the rotating axe of the Quaternion.
 * @return The rotating axe of the Quaternion.
 */
hkl::svector Quaternion::getAxe() const 
{
  // Bouml preserved body begin 00022702
      svector axe;
      double norm = norm2();
      // compute the angle
      double cos_angle = _a / norm;
      double angle = acos(cos_angle) * 2.0;
      
      // compute the axe
      double sin_angle = sin(angle / 2);
      if (fabs(sin_angle) > constant::math::epsilon)
      {
          axe.x() = _b / sin_angle / norm;
          axe.y() = _c / sin_angle / norm;
          axe.z() = _d / sin_angle / norm;
      }
      return axe;
      
  // Bouml preserved body end 00022702
}

ostream & Quaternion::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00022102
      flux << _a << " + " << _b << " i + " << _c << " j + " << _d << " k";
      
      return flux;
  // Bouml preserved body end 00022102
}

ostream & Quaternion::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00022182
      flux << setprecision(constant::math::precision)
        << " " << _a
        << " " << _b
        << " " << _c
        << " " << _d
        << endl;
      
      return flux;
  // Bouml preserved body end 00022182
}

/*!
 * \brief Restore a Quaternion from a stream.
 * \param flux The stream containing the Quaternion to restore.
 */
istream & Quaternion::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00022202
      flux >> setprecision(constant::math::precision)
        >> _a
        >> _b
        >> _c
        >> _d;
      
      return flux;
  // Bouml preserved body end 00022202
}


} // namespace hkl
