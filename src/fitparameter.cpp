#include "fitparameter.h"

namespace hkl {

  FitParameter::FitParameter() :
    Range()
  {}

  FitParameter::FitParameter(std::string const & name, double value, double min, double max, bool flagFit, double precision)
    : Range(name, value, min, max)
    {
      set_flagFit(flagFit);
      set_precision(precision);
      set_chi2(0.);
    }

  FitParameter::FitParameter(FitParameter const & fitParameter)
    : Range(fitParameter.get_name(),
        fitParameter.get_value(),
        fitParameter.get_min(),
        fitParameter.get_max())
    {
      set_flagFit(fitParameter.get_flagFit());
      set_precision(fitParameter.get_precision());
      set_chi2(fitParameter.get_chi2());
    }

  FitParameter::~FitParameter()
  {}

  bool
    FitParameter::operator ==(FitParameter const & fitParameter) const
    {
      return Range::operator==(fitParameter)
        && get_flagFit() == fitParameter.get_flagFit()
        && get_precision() == fitParameter.get_precision()
        && get_chi2() == fitParameter.get_chi2();
    }

  std::ostream & 
    FitParameter::printToStream(std::ostream & flux) const
    {
      flux << std::showpoint << std::showpos;
      flux  << "FitParameter: \"" << get_name() << "\"\t"
        << "Minimum: " << get_min() << ", "
        << "Value: " << get_value() << ", "
        << "Maximum: " << get_max() << ", "
        << "Precision: " << get_precision() << ", "
        << "chi2: " << get_chi2() << ", "
        << "To fit: " << get_flagFit() << std::endl;
      flux << std::noshowpoint << std::noshowpos << std::dec;

      return flux;
    }

  void
    FitParameter::randomize(void)
    {
      if (get_flagFit()){
        //srand(static_cast<unsigned int>(time(0)));
#ifdef VCPP6
        set_value(get_min() + (get_max()-get_min()) * rand()/(RAND_MAX+1.0));
#else
        set_value(get_min() + (get_max()-get_min()) * std::rand()/(RAND_MAX+1.0));
#endif  
      }
    }
} // namespace hkl

  std::ostream &
operator <<(std::ostream & flux, hkl::FitParameter const & fitParameter)
{
  return fitParameter.printToStream(flux);
}
