#include "svecmat.h"

namespace hkl {

    svector::svector(): 
      valarray<double>(0., 3)
    {}

    svector::svector(double const & a, double const & b, double const & c) :
      valarray<double>(0., 3)
    {
      (*this)[0] = a;
      (*this)[1] = b;
      (*this)[2] = c;
    }

    svector::svector(svector const & v) :
      valarray<double>(v)
    {}

    bool
    svector::operator ==(svector const & v) const
      {
        unsigned int i;

        for(i=0; i<3; i++)
            if (fabs((*this)[i]-v[i]) > constant::math::epsilon_0)
                return false;

        return true;
      }

    svector &
    svector::operator*= (smatrix const & M)
      {
        svector u(*this);

        (*this)[X] = u[X] * M.m_mat11 + u[Y] * M.m_mat21 + u[Z] * M.m_mat31;
        (*this)[Y] = u[X] * M.m_mat12 + u[Y] * M.m_mat22 + u[Z] * M.m_mat32;
        (*this)[Z] = u[X] * M.m_mat13 + u[Y] * M.m_mat23 + u[Z] * M.m_mat33;

        return *this;
      }

    svector &
    svector::operator*= (double const & d)
      {
        (*this)[X] *= d;
        (*this)[Y] *= d;
        (*this)[Z] *= d;

        return *this;
      }

    void
    svector::set(double const & a, double const & b, double const & c)
      {
        (*this)[X] = a;
        (*this)[Y] = b;
        (*this)[Z] = c;
      }

    // Scalar product.
    double
    svector::scalar(svector const & u) const
      {
        return (*this)[X] * u[X] + (*this)[Y] * u[Y] + (*this)[Z] * u[Z];
      }

    svector
    svector::vectorialProduct(svector const & v) const
      {
        svector z;

        z[X] = (*this)[Y] * v[Z] - (*this)[Z] * v[Y];
        z[Y] = (*this)[Z] * v[X] - (*this)[X] * v[Z];
        z[Z] = (*this)[X] * v[Y] - (*this)[Y] * v[X];

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
              XX[X], YY[X], ZZ[X],
              XX[Y], YY[Y], ZZ[Y],
              XX[Z], YY[Z], ZZ[Z]);

        return M;
      }

    double
    svector::norm2(void) const
      {
        return sqrt((*this)[X] * (*this)[X] + (*this)[Y] * (*this)[Y] + (*this)[Z] * (*this)[Z]);
      }

    double
    svector::norminf(void) const
      {
        double t = 0.0;

        if (fabs((*this)[X]) > fabs((*this)[Y]))
            t = fabs((*this)[X]);
        else
            t = fabs((*this)[Y]);
        if (fabs((*this)[Z]) > t)
            t = fabs((*this)[Z]);
        return t;
      }

    svector
    svector::normalize(void) const
      {
        double norm = this->norm2();
        return svector((*this)[X] / norm, (*this)[Y] / norm, (*this)[Z] / norm);
      }

    void
    svector::randomize(void)
      {
        unsigned int i;

        for(i=0;i<3;i++)
            (*this)[i] = -1 + 2 * rand()/(RAND_MAX+1.0);
      }

    svector &
    svector::randomize(svector const & v)
      {
        unsigned int i;
        bool not_ok = true;
        do
          {
            for(i=0;i<3;i++)
                (*this)[i] = -1 + 2 * rand()/(RAND_MAX+1.0);
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
                (*this)[i] = -1 + 2 * rand()/(RAND_MAX+1.0);
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

        v[X] = (c + (1 - c) * axe_n[0] * axe_n[0]) * (*this)[0];
        v[X] += ((1 - c) * axe_n[0] * axe_n[1] - axe_n[2] * s) * (*this)[1];
        v[X] += ((1 - c) * axe_n[0] * axe_n[2] + axe_n[1] * s) * (*this)[2];

        v[Y] = ((1 - c) * axe_n[0] * axe_n[1] + axe_n[2] * s) * (*this)[0];
        v[Y] += (c + (1 - c) * axe_n[1] * axe_n[1]) * (*this)[1];
        v[Y] += ((1 - c) * axe_n[1] * axe_n[2] - axe_n[0] * s) * (*this)[2];

        v[Z] = ((1 - c) * axe_n[0] * axe_n[2] - axe_n[1] * s) * (*this)[0];
        v[Z] += ((1 - c) * axe_n[1] * axe_n[2] + axe_n[0] * s) * (*this)[1];
        v[Z] += (c + (1 - c) * axe_n[2] * axe_n[2]) * (*this)[2];

        return v;
      }

    ostream &
    svector::toStream(ostream & flux) const
      {
        flux << setprecision(constant::math::precision);
        flux << " " << (*this)[X] << " " << (*this)[Y] << " " << (*this)[Z] << endl;
        return flux;    
      }

    istream &
    svector::fromStream(istream & flux)
      {
        flux >> setprecision(constant::math::precision);
        flux >> (*this)[X] >> (*this)[Y] >> (*this)[Z];
        return flux;
      }
} //namespace hkl

ostream &
operator <<(ostream & flux, hkl::svector const & v)
{
    flux << "<" << v[X] << ", " << v[Y] << ", " << v[Z] << ">";
    return flux;
}
