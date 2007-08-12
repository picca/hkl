
#include "fitparameter.h"

namespace hkl {

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
  // Bouml preserved body begin 00025702
  // Bouml preserved body end 00025702
}

/**
 * @brief Get the flag of the FitParameter class.
 * @return The _flagFit bool member.
 */
bool FitParameter::get_flagFit() const 
{
  // Bouml preserved body begin 00025802
      return _flagFit;
  // Bouml preserved body end 00025802
}

/**
 * @brief Get the precision of the FitParameter class.
 * @return A constant Value ref on the _precision member.
 */
const hkl::Value & FitParameter::get_precision() const 
{
  // Bouml preserved body begin 00025882
      return _precision;
  // Bouml preserved body end 00025882
}

/**
 * @brief Get the _chi2 of the FitParameter class.
 * @return A constant Value ref on the _chi2 member.
 */
const hkl::Value & FitParameter::get_chi2() const 
{
  // Bouml preserved body begin 00025902
      return _chi2;
  // Bouml preserved body end 00025902
}

/**
 * @brief Set the _flagFit member of the FitParameter class.
 * @param flagFit The bool to set. 
 */
void FitParameter::set_flagFit(bool flagFit) 
{
  // Bouml preserved body begin 00025982
      _flagFit = flagFit;
  // Bouml preserved body end 00025982
}

/**
 * @brief Set the _precision member of the FitParameter class.
 * @param precision The hkl::Value to set. 
 */
void FitParameter::set_precision(hkl::Value precision) 
{
  // Bouml preserved body begin 00025A02
      _precision = precision;
  // Bouml preserved body end 00025A02
}

/**
 * @brief Set the _chi2 member of the FitParameter class.
 * @param chi2 The hkl::Value to set. 
 */
void FitParameter::set_chi2(hkl::Value chi2) 
{
  // Bouml preserved body begin 00025C82
      _chi2 = chi2;
  // Bouml preserved body end 00025C82
}

/**
 * \brief Are two FitParameter equals ?
 * \param fitParameter the FitParameter to compare with.
 * \return The comparison of the two FitParameter.
 */
bool FitParameter::operator==(const hkl::FitParameter & fitParameter) const 
{
  // Bouml preserved body begin 00025D02
      return Parameter::operator==(fitParameter)
        && _flagFit == fitParameter._flagFit
        && _precision == fitParameter._precision
        && _chi2 == fitParameter._chi2;
  // Bouml preserved body end 00025D02
}

/**
 * @brief shuffle the FitParameter.
 */
void FitParameter::randomize() 
{
  // Bouml preserved body begin 00025D82
      if (_flagFit)
        {
          Value d = get_min() + (get_max()-get_min()) * ((double)rand()/(RAND_MAX+1.0));
          set_current(d);
        }
  // Bouml preserved body end 00025D82
}

/*!
 * \brief print the FitParameter into a flux
 * \param flux The stream to print into.
 */
ostream & FitParameter::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00025E02
      flux << showpoint << showpos;
      flux  << "FitParameter: \"" << get_name() << "\"\t"
            << "Minimum: " << get_min().get_value() << ", "
            << "current: " << get_current().get_value() << ", "
            << "Maximum: " << get_max().get_value() << ", "
            << "Precision: " << _precision.get_value() << ", "
            << "chi2: " << _chi2.get_value() << ", "
            << "To fit: " << _flagFit << endl;
      flux << noshowpoint << noshowpos << dec;
      
      return flux;
  // Bouml preserved body end 00025E02
}

/*!
 * \brief Save the FitParameter into a stream.
 * \param flux the stream to save the FitParameter into.
 * \return The stream with the FitParameter.
 */
ostream & FitParameter::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00025E82
      Parameter::toStream(flux);
      _precision.toStream(flux);
      _chi2.toStream(flux);
      flux << " " << _flagFit << endl;
      
      return flux;
  // Bouml preserved body end 00025E82
}

/*!
 * \brief Restore a FitParameter from a stream.
 * \param flux The stream containing the FitParameter to restore.
 * @todo call update_observers or not ?
 */
istream & FitParameter::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00025F02
      Parameter::fromStream(flux);
      _precision.fromStream(flux);
      _chi2.fromStream(flux);
      flux >> _flagFit;
      
      return flux;
  // Bouml preserved body end 00025F02
}


} // namespace hkl
