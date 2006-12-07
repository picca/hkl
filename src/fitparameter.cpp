#include <iomanip>

#include "fitparameter.h"

namespace hkl
  {

  FitParameter::FitParameter(MyString const & name, MyString const & description,
                             Value const & min, Value const & current, Value const & max,
                             bool flagFit, double precision) throw (HKLException) :
      Parameter(name, description, min, current, max),
      _flagFit(flagFit),
      _precision(precision),
      _chi2(0)
  {}

  bool
  FitParameter::operator==(FitParameter const & fitParameter) const
    {
      return Parameter::operator==(fitParameter)
             && _flagFit == fitParameter._flagFit
             && _precision == fitParameter._precision
             && _chi2 == fitParameter._chi2;
    }

  ostream &
  FitParameter::printToStream(ostream & flux) const
    {
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
    }

  void
  FitParameter::randomize(void)
  {
    if (_flagFit)
      {
        Value d = get_min() + (get_max()-get_min()) * ((double)rand()/(RAND_MAX+1.0));
        set_current(d);
      }
  }

  ostream &
  FitParameter::toStream(ostream & flux) const
    {
      Parameter::toStream(flux);
      _precision.toStream(flux);
      _chi2.toStream(flux);
      flux << " " << _flagFit << endl;

      return flux;
    }

  istream &
  FitParameter::fromStream(istream & flux)
  {
    Parameter::fromStream(flux);
    _precision.fromStream(flux);
    _chi2.fromStream(flux);
    flux >> _flagFit;

    return flux;
  }

} // namespace hkl
