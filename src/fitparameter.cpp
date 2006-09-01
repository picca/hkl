#include <iomanip>

#include "fitparameter.h"

namespace hkl {

    FitParameter::FitParameter(MyString const & name, MyString const & description,
                               double min, double value, double max,
                               bool flagFit, double precision) throw (HKLException) :
      Parameter(name, description, min, value, max),
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
        << "Minimum: " << get_min() << ", "
        << "current: " << get_current() << ", "
        << "Maximum: " << get_max() << ", "
        << "Precision: " << _precision << ", "
        << "chi2: " << _chi2 << ", "
        << "To fit: " << _flagFit << endl;
        flux << noshowpoint << noshowpos << dec;

        return flux;
      }

    void
    FitParameter::randomize(void)
      {
        if (_flagFit)
            set_current(get_min() + (get_max()-get_min()) * rand()/(RAND_MAX+1.0));
      }

    ostream &
    FitParameter::toStream(ostream & flux) const
      {
        Parameter::toStream(flux);
        _precision.toStream(flux);
        _chi2.toStream(flux);
        flux << setprecision(constant::math::precision) << " " << _flagFit << endl;

        return flux;    
      }

    istream &
    FitParameter::fromStream(istream & flux)
      {
        Parameter::fromStream(flux);
        _precision.fromStream(flux);
        _chi2.fromStream(flux);
        flux >> setprecision(constant::math::precision) >> _flagFit;

        return flux;
      }

} // namespace hkl
