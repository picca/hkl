
#include "fitparameter.h"

namespace hkl
  {

  /**
   * @brief Constructor
   * @param name of the FitParameter.
   * @param description The description of the FitParameter.
   * @param min the minimum of the FitParameter.
   * @param current the current value of the FitParameter.
   * @param max the maximum of the FitParameter.
   * @param toFit is a fit parameter or not
   * @param precision to fullfill for the fit.
   */
  FitParameter::FitParameter(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max, bool toFit, double precision)  :
      Parameter(name, description, min, current, max),
      _flagFit(toFit),
      _precision(precision),
      _chi2(0)
  {
  }

  /**
   * @brief Get the flag of the FitParameter class.
   * @return The _flagFit bool member.
   */
  bool FitParameter::get_flagFit() const
    {
      return _flagFit;
    }

  /**
   * @brief Get the precision of the FitParameter class.
   * @return A constant Value ref on the _precision member.
   */
  const hkl::Value & FitParameter::get_precision() const
    {
      return _precision;
    }

  /**
   * @brief Get the _chi2 of the FitParameter class.
   * @return A constant Value ref on the _chi2 member.
   */
  const hkl::Value & FitParameter::get_chi2() const
    {
      return _chi2;
    }

  /**
   * @brief Set the _flagFit member of the FitParameter class.
   * @param flagFit The bool to set.
   */
  void FitParameter::set_flagFit(bool flagFit)
  {
    _flagFit = flagFit;
  }

  /**
   * @brief Set the _precision member of the FitParameter class.
   * @param precision The hkl::Value to set.
   */
  void FitParameter::set_precision(hkl::Value precision)
  {
    _precision = precision;
  }

  /**
   * @brief Set the _chi2 member of the FitParameter class.
   * @param chi2 The hkl::Value to set.
   */
  void FitParameter::set_chi2(hkl::Value chi2)
  {
    _chi2 = chi2;
  }

  /**
   * \brief Are two FitParameter equals ?
   * \param fitParameter the FitParameter to compare with.
   * \return The comparison of the two FitParameter.
   */
  bool FitParameter::operator==(const hkl::FitParameter & fitParameter) const
    {
      return Parameter::operator==(fitParameter)
             && _flagFit == fitParameter._flagFit
             && _precision == fitParameter._precision
             && _chi2 == fitParameter._chi2;
    }

  /**
   * @brief shuffle the FitParameter.
   */
  void FitParameter::randomize()
  {
    if (_flagFit)
      {
        Value d = get_min() + (get_max()-get_min()) * ((double)rand()/(RAND_MAX+1.0));
        set_current(d);
      }
  }

  /*!
   * \brief print the FitParameter into a flux
   * \param flux The stream to print into.
   */
  std::ostream & FitParameter::printToStream(std::ostream & flux) const
    {
      flux << std::showpoint;
      Parameter::printToStream(flux);
      flux  << " Ŧ " << _precision.get_value() << ", "
      << "chi2: " << _chi2.get_value() << ", "
      << "To fit: " << _flagFit;
      flux << std::noshowpoint << std::noshowpos << std::dec;

      return flux;
    }

} // namespace hkl
