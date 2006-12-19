#ifndef _LATTICE_H_
#define _LATTICE_H_

#include "svecmat.h"
#include "fitparameter.h"

using namespace std;

namespace hkl
  {

  class Lattice
    {

    public:

      /**
       * @brief The default constructor.
       */
      Lattice(void);

      /**
       * @brief Another constructor.
       * @param a the a parameter of the Lattice
       * @param b the b parameter of the Lattice
       * @param c the c parameter of the Lattice
       * @param alpha the alpha parameter of the Lattice
       * @param beta the beta parameter of the Lattice
       * @param gamma the gamma parameter of the Lattice
       */
      Lattice(Value const & a, Value const & b, Value const & c,
              Value const & alpha, Value const & beta, Value const & gamma);
      /**
       * @brief The copy constructor.
       * @param lattice The Lattice to copy.
       */
      Lattice(Lattice const & lattice);

      /**
       * @brief The default destructor.
       */
      virtual ~Lattice(void);

      /**
       * @brief Get the a FitParameter of the Lattice.
       * @return A reference on the a FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter & a(void)
      {
        return *_a;
      }

      /**
       * @brief Get the b FitParameter of the Lattice.
       * @return A reference on the b FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter & b(void)
      {
        return *_b;
      }

      /**
       * @brief Get the c FitParameter of the Lattice.
       * @return A reference on the c FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter & c(void)
      {
        return *_c;
      }

      /**
       * @brief Get the alpha FitParameter of the Lattice.
       * @return A reference on the alpha FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter & alpha(void)
      {
        return *_alpha;
      }

      /**
       * @brief Get the beta FitParameter of the Lattice.
       * @return A reference on the beta FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter & beta(void)
      {
        return *_beta;
      }

      /**
       * @brief Get the gamma FitParameter of the Lattice.
       * @return A reference on the gamma FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter & gamma(void)
      {
        return *_gamma;
      }

      /**
       * @brief Get the a FitParameter of the Lattice.
       * @return A constant reference on the a FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter const & a(void) const
        {
          return *_a;
        }

      /**
       * @brief Get the b FitParameter of the Lattice.
       * @return A constant reference on the b FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter const & b(void) const
        {
          return *_b;
        }

      /**
       * @brief Get the c FitParameter of the Lattice.
       * @return A constant reference on the c FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter const & c(void) const
        {
          return *_c;
        }

      /**
       * @brief Get the alpha FitParameter of the Lattice.
       * @return A constant reference on the alpha FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter const & alpha(void) const
        {
          return *_alpha;
        }

      /**
       * @brief Get the beta FitParameter of the Lattice.
       * @return A constant reference on the beta FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter const & beta(void) const
        {
          return *_beta;
        }

      /**
       * @brief Get the gamma FitParameter of the Lattice.
       * @return A constant reference on the gamma FitParameter.
       * @todo return fitparameter * instead of fitParameter &.
       */
      FitParameter const & gamma(void) const
        {
          return *_gamma;
        }

      smatrix & get_B(void) const throw (HKLException); //!< get the m_B smatrix

      smatrix & get_B(bool & status) const; //!< get the m_B smatrix

      /**
       * @brief Compute the reciprocal Lattice.
       * @return The reciprocal Lattice.
       * @throw HKLException if the reciprocal Lattice can not be compute.
       */
      Lattice const reciprocal(void) const throw (HKLException);

      /**
       * @brief Randomize the Lattice.
       */
      void randomize(void);

      /**
       * @brief Compare two Lattices.
       * @return true if both are equals.
       */
      bool operator == (Lattice const & lattice) const;

      /**
       * @brief print the Lattice into a flux
       * @param flux The stream to print into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Save the Lattice into a stream.
       * @param flux the stream to save the Lattice into.
       * @return The stream with the Lattice.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore a Lattice from a stream.
       * @param flux The stream containing the Lattice.
       * @return The modified stream.
       */
      istream & fromStream(istream & flux);

    protected:
      FitParameter * _a; //!< The a FitParameter.
      FitParameter * _b; //!< The b FitParameter.
      FitParameter * _c; //!< The c FitParameter.
      FitParameter * _alpha; //!< The alpha FitParameter.
      FitParameter * _beta; //!< The beta FitParameter.
      FitParameter * _gamma; //!< The gamma FitParameter.

      mutable smatrix _B; //!< The B matrix of the Lattice.

      /**
       * @brief compute the B matrix from the fitParameters.
       * @return true if the calculus is valid.
       */
      bool _computeB(void) const;

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
      void _compute_reciprocal(double & a_star, double & b_star, double & c_star,
                               double & alpha_star, double & beta_star, double & gamma_star) const throw (HKLException);

    private:
      mutable double _old_a; //!< The old value of a to recompute B if necessary.
      mutable double _old_b; //!< The old value of b to recompute B if necessary.
      mutable double _old_c; //!< The old value of c to recompute B if necessary.
      mutable double _old_alpha; //!< The old value of alpha to recompute B if necessary.
      mutable double _old_beta; //!< The old value of beta to recompute B if necessary.
      mutable double _old_gamma; //!< The old value of gamma to recompute B if necessary.
    };

} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class Lattice
 * @param flux The ostream to print into.
 * @param lattice The Lattice to print 
 * @return 
 */
inline ostream &
operator << (ostream & flux, hkl::Lattice const & lattice)
{
  return lattice.printToStream(flux);
}

#endif // _LATTICE_H_
