/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _SVECTOR_H
#define _SVECTOR_H


#include <ostream>
#include <istream>

#include <math.h>
#include <cstdlib>
#include <iomanip>
#include "HKLException.h"
#include "constant.h"

namespace hkl
  {


//forward declaration
  class smatrix;

  class svector
    {
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

      std::ostream & printToStream(std::ostream & flux) const;

      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a svector from a stream.
       * \param flux The stream containing the svector to restore.
       */
      std::istream & fromStream(std::istream & flux);

      double norminf() const;

    };
  class smatrix
    {
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

      smatrix operator*(const smatrix & M) const;

      hkl::svector operator*(const hkl::svector & v) const;

      void set(double m11, double m12, double m13, double m21, double m22, double m23, double m31, double m32, double m33);

      void set(double euler_x, double euler_y, double euler_z);

      double get(unsigned int i, unsigned int j) const throw(hkl::HKLException);

      hkl::svector asEulerian() const;

      smatrix transpose();

      std::ostream & printToStream(std::ostream & flux) const;

      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a smatrix from a stream.
       * \param flux The stream containing the smatrix to restore.
       */
      std::istream & fromStream(std::istream & flux);

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
