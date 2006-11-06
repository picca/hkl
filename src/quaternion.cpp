#include "quaternion.h"
#include "convenience.h"

namespace hkl
  {

  Quaternion::Quaternion(void)
      : valarray<double>(0., 4)
  {
    (*this)[0] = 1.;
  }

  Quaternion::Quaternion(double const & el1, double const & el2, double const & el3, double const & el4) :
      valarray<double>(0., 4)
  {
    (*this)[0] = el1;
    (*this)[1] = el2;
    (*this)[2] = el3;
    (*this)[3] = el4;
  }

  Quaternion::Quaternion(double const & angle, svector const & v) :
      valarray<double>(0., 4)
  {
    double norm = sqrt(v[0]*v[0]+v[1]*v[1]+v[2]*v[2]);

    (*this)[0] = cos(angle/2.);
    (*this)[1] = sin(angle/2.) * v[0]/norm;
    (*this)[2] = sin(angle/2.) * v[1]/norm;
    (*this)[3] = sin(angle/2.) * v[2]/norm;
  }

  Quaternion::Quaternion(svector const & v) :
      valarray<double>(0., 4)
  {
    (*this)[1] = v[0];
    (*this)[2] = v[1];
    (*this)[3] = v[2];
  }

  Quaternion::Quaternion(Quaternion const & q) :
      valarray<double>(q)
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

  void
  Quaternion::getAngleAndAxe(double & angle, svector & axe) const
    {
      double norm = norm2();
      // compute the angle
      double cos_angle = (*this)[0] / norm;
      angle = acos(cos_angle) * 2.0;

      // compute the axe
      // cout << " " << (*this)[0];
      double sin_angle = sin(angle / 2);
      if (fabs(sin_angle) > constant::math::epsilon_1)
        {
          svector myaxe;
          myaxe[0] = (*this)[1] / sin_angle / norm;
          myaxe[1] = (*this)[2] / sin_angle / norm;
          myaxe[2] = (*this)[3] / sin_angle / norm;

          if (myaxe[0]+axe[0] < constant::math::epsilon_0
              && myaxe[1]+axe[1] < constant::math::epsilon_0
              && myaxe[2]+axe[2] < constant::math::epsilon_0) // myaxe == -axe
            {
              angle = -angle;
            }
          else
            if (!(myaxe == axe))
              axe = myaxe;

          angle = convenience::normalizeAngle(angle);
        }
    }

  svector
  Quaternion::getAxe(void) const
    {
      svector axe;
      double norm = norm2();
      // compute the angle
      double cos_angle = (*this)[0] / norm;
      double angle = acos(cos_angle) * 2.0;

      // compute the axe
      // cout << " " << (*this)[0];
      double sin_angle = sin(angle / 2);
      if (fabs(sin_angle) > constant::math::epsilon_1)
        {
          axe[0] = (*this)[1] / sin_angle / norm;
          axe[1] = (*this)[2] / sin_angle / norm;
          axe[2] = (*this)[3] / sin_angle / norm;
        }
      return axe;
    }

  ostream &
  Quaternion::toStream(ostream & flux) const
    {
      flux << setprecision(constant::math::precision)
      << " " << (*this)[0]
      << " " << (*this)[1]
      << " " << (*this)[2]
      << " " << (*this)[3]
      << endl;

      return flux;
    }

  istream &
  Quaternion::fromStream(istream & flux)
  {
    flux >> setprecision(constant::math::precision)
    >> (*this)[0]
    >> (*this)[1]
    >> (*this)[2]
    >> (*this)[3];

    return flux;
  }

} // namespace hkl

ostream&
operator <<(ostream& flux, hkl::Quaternion const & q)
{
  flux << q[0] << " + " << q[1] << " i + " << q[2] << " j + " << q[3] << " k";

  return flux;
}
