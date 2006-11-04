#include "svecmat.h"

namespace hkl
  {

  // Default constructor.
  smatrix::smatrix(void)
  {
    m_mat11 = 0.;
    m_mat12 = 0.;
    m_mat13 = 0.;
    m_mat21 = 0.;
    m_mat22 = 0.;
    m_mat23 = 0.;
    m_mat31 = 0.;
    m_mat32 = 0.;
    m_mat33 = 0.;
  }

  // Constructor to allocate a 3D matrix and populate it with data.
  smatrix::smatrix(double e11, double e12, double e13,
                   double e21, double e22, double e23,
                   double e31, double e32, double e33)
  {
    m_mat11 = e11;
    m_mat12 = e12;
    m_mat13 = e13;
    m_mat21 = e21;
    m_mat22 = e22;
    m_mat23 = e23;
    m_mat31 = e31;
    m_mat32 = e32;
    m_mat33 = e33;
  }

  smatrix::smatrix(double euler_x, double euler_y, double euler_z)
  {
    double A = cos(euler_x);
    double B = sin(euler_x);
    double C = cos(euler_y);
    double D = sin(euler_y);
    double E = cos(euler_z);
    double F = sin(euler_z);
    double AD = A * D;
    double BD = B * D;

    m_mat11 = C*E;
    m_mat12 =-C*F;
    m_mat13 = D;
    m_mat21 = BD * E + A * F;
    m_mat22 =-BD * F + A * E;
    m_mat23 =-B * C;
    m_mat31 =-AD * E + B * F;
    m_mat32 = AD * F + B * E;
    m_mat33 = A * C;
  }

  // Copy constructor.
  smatrix::smatrix(smatrix const & M)
  {
    m_mat11 = M.m_mat11;
    m_mat12 = M.m_mat12;
    m_mat13 = M.m_mat13;
    m_mat21 = M.m_mat21;
    m_mat22 = M.m_mat22;
    m_mat23 = M.m_mat23;
    m_mat31 = M.m_mat31;
    m_mat32 = M.m_mat32;
    m_mat33 = M.m_mat33;
  }

  bool
  smatrix::operator ==(smatrix const & M) const
    {
      if (fabs(m_mat11 - M.m_mat11) < constant::math::epsilon_1
          && fabs(m_mat12 - M.m_mat12) < constant::math::epsilon_1
          && fabs(m_mat13 - M.m_mat13) < constant::math::epsilon_1
          && fabs(m_mat21 - M.m_mat21) < constant::math::epsilon_1
          && fabs(m_mat22 - M.m_mat22) < constant::math::epsilon_1
          && fabs(m_mat23 - M.m_mat23) < constant::math::epsilon_1
          && fabs(m_mat31 - M.m_mat31) < constant::math::epsilon_1
          && fabs(m_mat32 - M.m_mat32) < constant::math::epsilon_1
          && fabs(m_mat33 - M.m_mat33) < constant::math::epsilon_1)
        return true;
    }

  smatrix &
  smatrix::operator *= (smatrix const & M)
  {
    smatrix M1(*this);
    smatrix M2 = M;

    m_mat11 = M1.m_mat11 * M2.m_mat11 + M1.m_mat12 * M2.m_mat21 + M1.m_mat13 * M2.m_mat31;
    m_mat12 = M1.m_mat11 * M2.m_mat12 + M1.m_mat12 * M2.m_mat22 + M1.m_mat13 * M2.m_mat32;
    m_mat13 = M1.m_mat11 * M2.m_mat13 + M1.m_mat12 * M2.m_mat23 + M1.m_mat13 * M2.m_mat33;
    m_mat21 = M1.m_mat21 * M2.m_mat11 + M1.m_mat22 * M2.m_mat21 + M1.m_mat23 * M2.m_mat31;
    m_mat22 = M1.m_mat21 * M2.m_mat12 + M1.m_mat22 * M2.m_mat22 + M1.m_mat23 * M2.m_mat32;
    m_mat23 = M1.m_mat21 * M2.m_mat13 + M1.m_mat22 * M2.m_mat23 + M1.m_mat23 * M2.m_mat33;
    m_mat31 = M1.m_mat31 * M2.m_mat11 + M1.m_mat32 * M2.m_mat21 + M1.m_mat33 * M2.m_mat31;
    m_mat32 = M1.m_mat31 * M2.m_mat12 + M1.m_mat32 * M2.m_mat22 + M1.m_mat33 * M2.m_mat32;
    m_mat33 = M1.m_mat31 * M2.m_mat13 + M1.m_mat32 * M2.m_mat23 + M1.m_mat33 * M2.m_mat33;

    return *this;
  }

  smatrix
  smatrix::operator * (smatrix const & M2) const
    {
      smatrix result(*this);

      result *= M2;

      return result;
    }

  svector
  smatrix::operator *(svector const & v) const
    {
      svector result;

      result[0] = v[0] * m_mat11 + v[1] * m_mat12 + v[2] * m_mat13;
      result[1] = v[0] * m_mat21 + v[1] * m_mat22 + v[2] * m_mat23;
      result[2] = v[0] * m_mat31 + v[1] * m_mat32 + v[2] * m_mat33;

      return result;
    }


  void
  smatrix::set(smatrix const & M)
    {
      m_mat11 = M.m_mat11;
      m_mat12 = M.m_mat12;
      m_mat13 = M.m_mat13;
      m_mat21 = M.m_mat21;
      m_mat22 = M.m_mat22;
      m_mat23 = M.m_mat23;
      m_mat31 = M.m_mat31;
      m_mat32 = M.m_mat32;
      m_mat33 = M.m_mat33;
    }

  void
  smatrix::set(double euler_x, double euler_y, double euler_z)
    {
      double A = cos(euler_x);
      double B = sin(euler_x);
      double C = cos(euler_y);
      double D = sin(euler_y);
      double E = cos(euler_z);
      double F = sin(euler_z);
      double AD = A * D;
      double BD = B * D;

      m_mat11 = C*E;
      m_mat12 =-C*F;
      m_mat13 = D;
      m_mat21 = BD * E + A * F;
      m_mat22 =-BD * F + A * E;
      m_mat23 =-B * C;
      m_mat31 =-AD * E + B * F;
      m_mat32 = AD * F + B * E;
      m_mat33 = A * C;
    }

  // Set all the fields.
  void
  smatrix::set(double el11, double el12, double el13,
                 double el21, double el22, double el23,
                 double el31, double el32, double el33)
    {
      m_mat11 = el11;
      m_mat12 = el12;
      m_mat13 = el13;
      m_mat21 = el21;
      m_mat22 = el22;
      m_mat23 = el23;
      m_mat31 = el31;
      m_mat32 = el32;
      m_mat33 = el33;
    }

  // Transposition.
  smatrix &
  smatrix::transpose(void)
  {
    smatrix M(*this);

    m_mat11 = M.m_mat11;
    m_mat12 = M.m_mat21;
    m_mat13 = M.m_mat31;
    m_mat21 = M.m_mat12;
    m_mat22 = M.m_mat22;
    m_mat23 = M.m_mat32;
    m_mat31 = M.m_mat13;
    m_mat32 = M.m_mat23;
    m_mat33 = M.m_mat33;

    return (*this);
  }

  double
  smatrix::get(int i, int j) const throw (HKLException)
    {
      if (i==0 && j==0)
        return m_mat11;
      if (i==1 && j==0)
        return m_mat21;
      if (i==2 && j==0)
        return m_mat31;

      if (i==0 && j==1)
        return m_mat12;
      if (i==1 && j==1)
        return m_mat22;
      if (i==2 && j==1)
        return m_mat32;

      if (i==0 && j==2)
        return m_mat13;
      if (i==1 && j==2)
        return m_mat23;
      if (i==2 && j==2)
        return m_mat33;
      else
        HKLEXCEPTION("Unable to get such an element",
                     "i>=3 or i<0 or j>=3 or j<0");
    }

  svector
  smatrix::asEulerian(void) const
    {
      svector eulerian;
      double angle_x, angle_y, angle_z;
      double tx, ty;

      angle_y = asin( m_mat13 );        /* Calculate Y-axis angle */
      double C = cos( angle_y );
      if (fabs(C) > constant::math::epsilon_0)
        {             /* Gimball lock? */
          tx =  m_mat33 / C;           /* No, so get X-axis angle */
          ty = -m_mat23 / C;
          angle_x = atan2( ty, tx );
          tx =  m_mat11 / C;            /* Get Z-axis angle */
          ty = -m_mat12 / C;
          angle_z = atan2( ty, tx );
        }
      else
        {                               /* Gimball lock has occurred */
          angle_x  = 0.;                      /* Set X-axis angle to zero */
          tx      =  m_mat22;                 /* And calculate Z-axis angle */
          ty      =  m_mat21;
          angle_z  = atan2( ty, tx );
        }
      eulerian.set(angle_x, angle_y, angle_z);

      return eulerian;
    }

  ostream &
  smatrix::toStream(ostream & flux) const
    {
      flux << setprecision(constant::math::precision);
      flux << " " << m_mat11 << " " << m_mat12 << " " << m_mat13;
      flux << " " << m_mat21 << " " << m_mat22 << " " << m_mat23;
      flux << " " << m_mat31 << " " << m_mat32 << " " << m_mat33 << endl;
      return flux;
    }

  istream &
  smatrix::fromStream(istream & flux)
  {
    flux >> setprecision(constant::math::precision);
    flux >> m_mat11 >> m_mat12 >> m_mat13;
    flux >> m_mat21 >> m_mat22 >> m_mat23;
    flux >> m_mat31 >> m_mat32 >> m_mat33;
    return flux;
  }
} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::smatrix const & m)
{
  flux << endl;
  flux << showpoint << showpos;
  flux << m.get(0,0) << '\t' << m.get(0,1) << '\t' << m.get(0,2) << endl;
  flux << m.get(1,0) << '\t' << m.get(1,1) << '\t' << m.get(1,2) << endl;
  flux << m.get(2,0) << '\t' << m.get(2,1) << '\t' << m.get(2,2) << endl;
  flux << noshowpoint << noshowpos << dec;
  return flux;
}
