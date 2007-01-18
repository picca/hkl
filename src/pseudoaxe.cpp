#include "pseudoaxeengine.h"

namespace hkl
  {

  PseudoAxe::PseudoAxe(MyString const & name, MyString const & description, Range const & read, Range & write, PseudoAxeEngine * engine) :
      ObjectReadOnly(name, description),
      _read(read),
      _write(write),
      _engine(engine)
  {}

  PseudoAxe::~PseudoAxe(void)
  {}

  void
  PseudoAxe::initialize(void) throw (HKLException)
  {
    _engine->initialize();
  }

  void
  PseudoAxe::uninitialize(void)
  {
    _engine->uninitialize();
  }

  bool
  PseudoAxe::get_initialized(void) const
    {
      return _engine->get_initialized();
    }

  bool
  PseudoAxe::get_readable(void) const
    {
      return _engine->get_readable();
    }

  bool
  PseudoAxe::get_writable(void) const
    {
      return _engine->get_writable();
    }

  Value const &
  PseudoAxe::get_min(void) const throw (HKLException)
  {
    if (_engine->get_readable())
      return _read.get_min();
    else
      {
        ostringstream reason;
        reason << "The pseudoAxe named : " << get_name() << " is not valid";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
  }

  Value const &
  PseudoAxe::get_max(void) const throw (HKLException)
  {
    if (_engine->get_readable())
      return _read.get_max();
    else
      HKLEXCEPTION("The pseudoAxe is not valid", "initialize it");
  }

  Value const &
  PseudoAxe::get_current(void) const throw (HKLException)
  {
    if (_engine->get_readable())
      return _read.get_current();
    else
      HKLEXCEPTION("The pseudoAxe is not valid", "initialize it");
  }

  void
  PseudoAxe::set_current(Value const & value) throw (HKLException)
  {
    _write.set_current(value.get_value());
    _engine->set();
  }

  void
  PseudoAxe::set_engine(PseudoAxeEngine * engine)
  {
    _engine = engine;
  }

  bool
  PseudoAxe::operator==(PseudoAxe const & pseudoAxe) const
    {
      return ObjectReadOnly::operator==(pseudoAxe)
             && _engine == pseudoAxe._engine;
    }

  ostream &
  PseudoAxe::printToStream(ostream & flux) const
    {
      ObjectReadOnly::printToStream(flux);
      return flux;
    }

  ostream &
  PseudoAxe::toStream(ostream & flux) const
    {
      ObjectReadOnly::toStream(flux);
      return flux;
    }

  istream &
  PseudoAxe::fromStream(istream & flux)
  {
    ObjectReadOnly::fromStream(flux);
    return flux;
  }

} // namespace hkl
