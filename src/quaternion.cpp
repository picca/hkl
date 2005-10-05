#include "quaternion.h"

namespace hkl {

Quaternion::Quaternion(void) :
  std::valarray<double>(0., 4)
{
  (*this)[0] = 1.;
}

Quaternion::Quaternion(double const & el1, double const & el2, double const & el3, double const & el4) :
  std::valarray<double>(0., 4)
{
  (*this)[0] = el1;
  (*this)[1] = el2;
  (*this)[2] = el3;
  (*this)[3] = el4;
}

Quaternion::Quaternion(double const & angle, svector const & v) :
  std::valarray<double>(0., 4)
{
  double norm = sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);
  
  (*this)[0] = cos(angle/2.);
  (*this)[1] = sin(angle/2.) * v[0]/norm;
  (*this)[2] = sin(angle/2.) * v[1]/norm;
  (*this)[3] = sin(angle/2.) * v[2]/norm;
}

Quaternion::Quaternion(Quaternion const & q) :
  std::valarray<double>(q)
{}

bool
Quaternion::operator ==(Quaternion const & q) const
{
  unsigned int i;
  
  for(i=0;i<4;i++)
    if ( fabs(q[i] - (*this)[i]) > constant::math::epsilon_1 )
      return false;
  return true;
}

Quaternion &
Quaternion::operator *=(Quaternion const & q)
{
  Quaternion q1(*this);
  Quaternion q2;

  q2[0] = q1[0]*q[0] - q1[1]*q[1] - q1[2]*q[2] - q1[3]*q[3];
  q2[1] = q1[0]*q[1] + q1[1]*q[0] + q1[2]*q[3] - q1[3]*q[2];
  q2[2] = q1[0]*q[2] - q1[1]*q[3] + q1[2]*q[0] + q1[3]*q[1];
  q2[3] = q1[0]*q[3] + q1[1]*q[2] - q1[2]*q[1] + q1[3]*q[0];

  *this = q2;

  return *this;
}

Quaternion &
Quaternion::operator /=(double const & d)
{
  for(int i=0;i<4;i++)
    (*this)[i] /= d;

  return *this;
}

double
Quaternion::norm2(void) const
{
  double norm2 = 0.0;
  unsigned int i;
  
  for(i=0;i<4;i++)
    norm2 += (*this)[i] * (*this)[i];
  
  return sqrt(norm2);
}

Quaternion
Quaternion::conjugate(void) const
{
  return Quaternion((*this)[0], -(*this)[1], -(*this)[2], -(*this)[3]);
}

double 
Quaternion::dotProduct(Quaternion const & q) const
{
  Quaternion q1 = (*this).conjugate();
  q1 *= q;
  
  Quaternion q2 = q.conjugate();
  q2 *= (*this);

  q1 += q2;

  return q1[0]/2.;
}

Quaternion
Quaternion::invert(void) const
{
  Quaternion q = (*this).conjugate();
  q /= (*this).dotProduct(*this);
  
  return q;
}

smatrix
Quaternion::asMatrix(void) const
{
  smatrix m((*this)[0]*(*this)[0]+(*this)[1]*(*this)[1]-(*this)[2]*(*this)[2]-(*this)[3]*(*this)[3],
              2*((*this)[1]*(*this)[2]-(*this)[0]*(*this)[3]),
              2*((*this)[0]*(*this)[2]+(*this)[1]*(*this)[3]),
            2*((*this)[0]*(*this)[3]+(*this)[1]*(*this)[2]),
              (*this)[0]*(*this)[0]-(*this)[1]*(*this)[1]+(*this)[2]*(*this)[2]-(*this)[3]*(*this)[3],
              2*((*this)[2]*(*this)[3]-(*this)[0]*(*this)[1]),
            2*((*this)[1]*(*this)[3]-(*this)[0]*(*this)[2]),
              2*((*this)[0]*(*this)[1]+(*this)[2]*(*this)[3]),
              (*this)[0]*(*this)[0]-(*this)[1]*(*this)[1]-(*this)[2]*(*this)[2]+(*this)[3]*(*this)[3]);

  return m;
}

} // namespace hkl

std::ostream&
operator <<(std::ostream& flux, hkl::Quaternion const & q)
{
  flux << std::endl;
  flux << q[0] << " + " << q[1] << " i + " << q[2] << " j + " << q[3] << " k" << std::endl;
  
  return flux;
}
