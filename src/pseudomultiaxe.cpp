#include "pseudoaxeengine.h"

namespace hkl
  {

  PseudoMultiAxe::PseudoMultiAxe(MyString const & name, MyString const & description, Range & range, PseudoAxeEngine * engine) :
      ObjectReadOnly(name, description),
      _range(range),
      _engine(engine)
  {}

  PseudoMultiAxe::~PseudoMultiAxe(void)
  {}

  void
  PseudoMultiAxe::initialize(void) throw (HKLException)
  {
    _engine->initialize();
  }

  void
  PseudoMultiAxe::uninitialize(void)
  {
    _engine->uninitialize();
  }

  bool
  PseudoMultiAxe::get_initialized(void) const
    {
      return _engine->get_initialized();
    }

  bool
  PseudoMultiAxe::get_readable(void) const
    {
      return _engine->get_readable();
    }

  bool
  PseudoMultiAxe::get_writable(void) const
    {
      return _engine->get_writable();
    }

  Value const &
  PseudoMultiAxe::get_min(void) const throw (HKLException)
  {
    if (_engine->get_readable())
      return _range.get_min();
    else
      {
        ostringstream reason;
        reason << "The pseudoMultiAxe named : " << get_name() << " is not valid";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
  }

  Value const &
  PseudoMultiAxe::get_max(void) const throw (HKLException)
  {
    if (_engine->get_readable())
      return _range.get_max();
    else
      HKLEXCEPTION("The pseudoMultiAxe is not valid", "initialize it");
  }

  Value const &
  PseudoMultiAxe::get_current(void) const throw (HKLException)
  {
    if (_engine->get_readable())
      return _range.get_current();
    else
      HKLEXCEPTION("The pseudoMultiAxe is not valid", "initialize it");
  }

  void
  PseudoMultiAxe::set_current(Value const & value) throw (HKLException)
  {
    _range.set_current(value);
    _engine->set();
  }

  bool
  PseudoMultiAxe::operator==(PseudoMultiAxe const & pseudoMultiAxe) const
    {
      return ObjectReadOnly::operator==(pseudoMultiAxe)
             && _engine == pseudoMultiAxe._engine;
    }

  ostream &
  PseudoMultiAxe::printToStream(ostream & flux) const
    {
      ObjectReadOnly::printToStream(flux);
      return flux;
    }

  ostream &
  PseudoMultiAxe::toStream(ostream & flux) const
    {
      ObjectReadOnly::toStream(flux);
      return flux;
    }

  istream &
  PseudoMultiAxe::fromStream(istream & flux)
  {
    ObjectReadOnly::fromStream(flux);
    return flux;
  }

} // namespace hkl
