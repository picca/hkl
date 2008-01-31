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
#ifndef _LATTICE_H
#define _LATTICE_H


#include "svector.h"
#include "HKLException.h"
#include <ostream>
#include <istream>

#include "fitparameter.h"
#include "value.h"
namespace hkl
  {
  class FitParameter;
}
namespace hkl
  {
  class Value;
}

namespace hkl
  {

  class Lattice
    {
    protected:
      hkl::FitParameter * _a;

      hkl::FitParameter * _b;

      hkl::FitParameter * _c;

      hkl::FitParameter * _alpha;

      hkl::FitParameter * _beta;

      hkl::FitParameter * _gamma;

      mutable hkl::smatrix _B;


    private:
      mutable double _old_a;

      mutable double _old_b;

      mutable double _old_c;

      mutable double _old_alpha;

      mutable double _old_beta;

      mutable double _old_gamma;


    public:
      /**
       * @brief The default constructor.
       */
      Lattice();

      /**
       * @brief Another constructor.
       * @param a the a parameter of the Lattice
       * @param b the b parameter of the Lattice
       * @param c the c parameter of the Lattice
       * @param alpha the alpha parameter of the Lattice
       * @param beta the beta parameter of the Lattice
       * @param gamma the gamma parameter of the Lattice
       */
      Lattice(const hkl::Value & a, const hkl::Value & b, const hkl::Value & c, const hkl::Value & alpha, const hkl::Value & beta, const hkl::Value & gamma);

      /**
       * @brief The copy constructor.
       * @param source The Lattice to copy.
       */
      Lattice(const Lattice & source);

      /**
       * @brief The default destructor.
       */
      virtual ~Lattice();

      /**
       * @brief Get the a FitParameter of the Lattice.
       * @return A reference on the a FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      hkl::FitParameter & a();

      /**
       * @brief Get the b FitParameter of the Lattice.
       * @return A reference on the b FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      hkl::FitParameter & b();

      /**
       * @brief Get the c FitParameter of the Lattice.
       * @return A reference on the c FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      hkl::FitParameter & c();

      /**
       * @brief Get the alpha FitParameter of the Lattice.
       * @return A reference on the alpha FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      hkl::FitParameter & alpha();

      /**
       * @brief Get the beta FitParameter of the Lattice.
       * @return A reference on the beta FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      hkl::FitParameter & beta();

      /**
       * @brief Get the gamma FitParameter of the Lattice.
       * @return A reference on the gamma FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      hkl::FitParameter & gamma();

      /**
       * @brief Get the a FitParameter of the Lattice.
       * @return A reference on the a FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      const hkl::FitParameter & a() const;

      /**
       * @brief Get the b FitParameter of the Lattice.
       * @return A reference on the b FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      const hkl::FitParameter & b() const;

      /**
       * @brief Get the c FitParameter of the Lattice.
       * @return A reference on the c FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      const hkl::FitParameter & c() const;

      /**
       * @brief Get the alpha FitParameter of the Lattice.
       * @return A reference on the alpha FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      const hkl::FitParameter & alpha() const;

      /**
       * @brief Get the beta FitParameter of the Lattice.
       * @return A reference on the beta FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      const hkl::FitParameter & beta() const;

      /**
       * @brief Get the gamma FitParameter of the Lattice.
       * @return A reference on the gamma FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      const hkl::FitParameter & gamma() const;

      const hkl::smatrix & get_B() const throw(hkl::HKLException);

      const hkl::smatrix & get_B(bool & status) const;

      /**
       * @brief Compute the reciprocal Lattice.
       * @return The reciprocal Lattice.
       * @throw HKLException if the reciprocal Lattice can not be compute.
       * @todo See for the consign assignation.
       */
      Lattice reciprocal() const throw(hkl::HKLException);

      /**
       * @brief Randomize the Lattice.
       */
      void randomize();

      /**
       * \brief Are two Lattice equals ?
       * \param lattice the Lattice to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const Lattice & lattice) const;

      /**
       * @brief print the Lattice into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the Lattice
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the Lattice from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);


    protected:
      /**
       * @brief compute the B matrix from the fitParameters.
       * @return true if the calculus is valid.
       */
      bool _computeB() const;

      /**
       * @brief compute the reciprocal parameters of the Lattice.
       * @param[out] a_star the a_star value.
       * @param[out] b_star the b_star value.
       * @param[out] c_star the c_star value.
       * @param[out] alpha_star the alpha_star value.
       * @param[out] beta_star the beta_star value.
       * @param[out] gamma_star the gamma_star value.
       * @throw HKLException if the reciprocal calculus is not possible.
       */
      void _compute_reciprocal(double & a_star, double & b_star, double & c_star, double & alpha_star, double & beta_star, double & gamma_star) const throw(hkl::HKLException);

    };

} // namespace hkl
/**
 * @brief Surcharge de l'operateur << pour la class Lattice
 * @param flux The ostream to print into.
 * @param lattice The Lattice to print
 * @return
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::Lattice const & lattice)
{
  return lattice.printToStream(flux);
}
#endif
