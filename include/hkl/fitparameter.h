#ifndef _FITPARAMETER_H
#define _FITPARAMETER_H

#include <iostream>

#include "parameter.h"

using namespace std;

namespace hkl
  {

  /**
   * @brief A class design to describe a fitted parameter
   */
  class FitParameter : public Parameter
    {
    public:

      /*!
       * \brief Constructor
       * \param name of the FitParameter.
       * \param value of the FitParameter.
       * \param min the minimum of the FitParameter.
       * \param max the maximum of the FitParameter.
       * \param flagFit is a fit parameter or not
       * \param precision to fullfill for the fit.
       */
      FitParameter(MyString const & name, MyString const & description,
                   Value const & min, Value const & current, Value const & max,
                   bool flagFit, double precision) throw (HKLException);

      /*!
       * \brief Get the flag of the FitParameter
       * \return the flag
       */
      bool get_flagFit(void) const
        {
          return _flagFit;
        }

      /*!
       * \brief Get the precision of the FitParameter.
       * \return The precision.
       */
      Value const &  get_precision(void) const
        {
          return _precision;
        }

      /*!
       * @brief Get the \f$\chi^2\f$ after a fit.
       * @return the \f$\chi^2\f$ of the affinement.
       */
      Value const & get_chi2(void) const
        {
          return _chi2;
        }

      /*!
       * \brief Set the flag of the FitParameter.
       * \param flagFit to set. 
       */
      void set_flagFit(bool flagFit)
      {
        _flagFit = flagFit;
      }

      /*!
       * \brief Set the FitParameter precision.
       * \param precision The precision to achieve during an fit.
       */
      void set_precision(Value const & precision)
      {
        _precision = precision;
      }

      /*!
       * \brief Set the FitParameter \f$\chi^2\f$.
       * \param chi2 The \f$\chi^2\f$ value to set.
       */
      void set_chi2(Value const & chi2)
      {
        _chi2 = chi2;
      }

      /**
       * \brief Are two FitParameter equals ?
       * \param fitParameter the FitParameter to compare with
       * \return The comparison of the two FitParameter.
       */
      bool operator==(FitParameter const & fitParameter) const;

      /*!
       * @brief shuffle the parameter
       */
      void randomize(void);

      /*!
       * \brief print the FitParameter into a flux
       * \param flux The stream to print into.
       * \return The flux modified.
       */
      ostream & printToStream(ostream & flux) const;

      /*!
       * \brief Save the FitParameter into a stream.
       * \param flux the stream to save the FitParameter into.
       * \return The stream with the FitParameter.
       */
      ostream & toStream(ostream & flux) const;

      /*!
       * \brief Restore a FitParameter from a stream.
       * \param flux The stream containing the FitParameter.
       */
      istream & fromStream(istream & flux);

    private:
      bool _flagFit; //!< the flag of the FitParameter.
      Value _precision; //!< the precision for the fit.
      Value _chi2; //!< the \f$\chi^2\f$ obtained during the calculation.
    };

} // namespace hkl

/*!
 * \brief Overload of the << operator for the Axe class
 * \param flux
 * \param fitParameter
 * \return The flux modified.
 */
inline ostream &
operator<<(ostream & flux, hkl::FitParameter const & fitParameter)
{
  return fitParameter.printToStream(flux);
}

#endif // _AXE_H
