#include "diffractometer.h"
#include "portability.h"

namespace hkl
  {

  Diffractometer::Diffractometer(MyString const & name, MyString const & description) :
      HKLObject(name, description)
  {}

  Diffractometer::~Diffractometer(void)
  {}

  bool
  Diffractometer::operator ==(Diffractometer const & diffractometer) const
    {
      return HKLObject::operator==(diffractometer)
             && *_geometry == *(diffractometer._geometry)
             && *_samples == *(diffractometer._samples)
             && _modes == diffractometer._modes
             && _pseudoAxeEngines == diffractometer._pseudoAxeEngines;
    }

  ostream &
  Diffractometer::printToStream(ostream & flux) const
    {
      flux << endl;
      flux << "Diffractometer: \"" << get_name() << "\"" << endl;
      HKLObject::printToStream(flux);
      _geometry->printToStream(flux);
      _samples->printToStream(flux);
      _modes.printToStream(flux);
      _pseudoAxeEngines.printToStream(flux);

      return flux;
    }

  ostream &
  Diffractometer::toStream(ostream & flux) const
    {
      flux << " " << HKL_VERSION;
      HKLObject::toStream(flux);
      _geometry->toStream(flux);
      _samples->toStream(flux);
      _modes.toStream(flux);
      _pseudoAxeEngines.toStream(flux);

      return flux;
    }

  istream &
  Diffractometer::fromStream(istream & flux)
  {
    unsigned int version;

    flux >> version;
    if (version == HKL_VERSION)
      {
        HKLObject::fromStream(flux);
        _geometry->fromStream(flux);
        _samples->fromStream(flux);
        _modes.fromStream(flux);
        _pseudoAxeEngines.fromStream(flux);
      }
    return flux;
  }

} // namespace hkl
