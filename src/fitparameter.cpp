#include "fitparameter.h"

namespace hkl {

  FitParameter::FitParameter(void) :
    Range()
  {}

  FitParameter::FitParameter(string const & name, double value, double min, double max, bool flagFit, double precision)
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

  FitParameter::~FitParameter(void)
  {}

  bool
  FitParameter::operator ==(FitParameter const & fitParameter) const
  {
    return Range::operator==(fitParameter)
      && get_flagFit() == fitParameter.get_flagFit()
      && get_precision() == fitParameter.get_precision()
      && get_chi2() == fitParameter.get_chi2();
  }

  ostream & 
  FitParameter::printToStream(ostream & flux) const
  {
    flux << showpoint << showpos;
    flux  << "FitParameter: \"" << get_name() << "\"\t"
      << "Minimum: " << get_min() << ", "
      << "Value: " << get_value() << ", "
      << "Maximum: " << get_max() << ", "
      << "Precision: " << get_precision() << ", "
      << "chi2: " << get_chi2() << ", "
      << "To fit: " << get_flagFit() << endl;
    flux << noshowpoint << noshowpos << dec;

    return flux;
  }

  void
  FitParameter::randomize(void)
  {
    if (get_flagFit())
      set_value(get_min() + (get_max()-get_min()) * rand()/(RAND_MAX+1.0));
  }

  ostream &
  FitParameter::toStream(ostream & flux) const
  {
    Range::toStream(flux);
    flux << setprecision(constant::math::precision)
      << m_flagFit << " "
      << m_precision << " "
      << m_chi2 << endl;
    
    return flux;    
  }

  istream &
  FitParameter::fromStream(istream & flux)
  {
    Range::fromStream(flux);
    flux >> setprecision(constant::math::precision)
      >> m_flagFit >> m_precision >> m_chi2;
    
    return flux;
  }
      
} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::FitParameter const & fitParameter)
{
  return fitParameter.printToStream(flux);
}
