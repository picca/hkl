#ifndef _SVECTOR_H
#define _SVECTOR_H


#include <iostream>
using namespace std;

#include <math.h>
#include <cstdlib>
#include <iomanip>
#include "HKLException.h"
#include "constant.h"

namespace hkl {


//forward declaration
class smatrix;

class svector {
  friend class smatrix;
  protected:
    double _x;

    double _y;

    double _z;


  public:
    svector();

    svector(double x, double y, double z);

    svector(const svector & source);

    double & x();

    double & y();

    double & z();

    double const & x() const;

    double const & y() const;

    double const & z() const;

    bool operator==(const svector & v) const;

    svector & operator*=(const svector & v);

    svector & operator*=(const hkl::smatrix & M);

    svector & operator*=(const double & d);

    svector & operator/=(const double & d);

    svector & operator-=(const svector & v);

    double sum() const;

    void set(double x, double y, double z);

    double scalar(const svector & v) const;

    svector vectorialProduct(const svector & v) const;

    double angle(const svector & v) const;

    hkl::smatrix axisSystem(const svector & v) const;

    double norm2() const;

    svector normalize() const;

    bool isColinear(const svector & v) const;

    void randomize();

    svector & randomize(const svector & v);

    svector & randomize(const svector & v1, const svector & v2);

    /**
     * \brief rotate a vector around another one with an angle.
     * \param axe The svector corresponding to the rotation axe.
     * \param angle the angle of rotation.
     * \return The new vector.
     */
    svector rotatedAroundVector(const svector & axe, double angle) const;

    ostream & printToStream(ostream & flux) const;

    ostream & toStream(ostream & flux) const;

    /*!
     * \brief Restore a svector from a stream.
     * \param flux The stream containing the svector to restore.
     */
    istream & fromStream(istream & flux);

    double norminf() const;

};
class smatrix {
  friend class svector;
  protected:
    double _m11;

    double _m12;

    double _m13;

    double _m21;

    double _m22;

    double _m23;

    double _m31;

    double _m32;

    double _m33;


  public:
    smatrix();

    smatrix(double m11, double m12, double m13, double m21, double m22, double m23, double m31, double m32, double m33);

    smatrix(double euler_x, double euler_y, double euler_z);

    smatrix(const smatrix & source);

    bool operator==(const smatrix & M) const;

    smatrix & operator*=(const smatrix & M);

    void set(double m11, double m12, double m13, double m21, double m22, double m23, double m31, double m32, double m33);

    void set(double euler_x, double euler_y, double euler_z);

    double get(unsigned int i, unsigned int j) const throw(hkl::HKLException);

    ostream & printToStream(ostream & flux) const;

    ostream & toStream(ostream & flux) const;

    /*!
     * \brief Restore a smatrix from a stream.
     * \param flux The stream containing the smatrix to restore.
     */
    istream & fromStream(istream & flux);

    smatrix operator*(const smatrix & M) const;

    hkl::svector operator*(const hkl::svector & v) const;

    hkl::svector asEulerian() const;

    smatrix transpose();

};

} // namespace hkl

/**
 * \brief Surcharge de l'operateur << pour la class svector
 * @param flux 
 * @param m 
 * @return 
 */
std::ostream & operator << (std::ostream & flux, hkl::svector const & v);

/**
 * \brief Surcharge de l'operateur << pour la class smatrix
 * @param flux 
 * @param m 
 * @return 
 */
std::ostream & operator << (std::ostream & flux, hkl::smatrix const & m);

#endif
