#include "pseudoaxe.h"

namespace hkl
  {

  PseudoAxe::PseudoAxe(MyString const & name, MyString const & description) :
      HKLObject(name, description),
      Observer(),
      _writable(false),
      _readable(false),
      _initialized(false)
  {}

  PseudoAxe::PseudoAxe(PseudoAxe const & pseudoAxe) :
      HKLObject(pseudoAxe),
      Observer(),
      _writable(pseudoAxe._writable),
      _readable(pseudoAxe._readable),
      _initialized(pseudoAxe._initialized)
  {}

  PseudoAxe::~PseudoAxe(void)
  {}

  void
  PseudoAxe::uninitialize(void)
  {
    _initialized = false;
    _writable = false;
  }

  Value const &
  PseudoAxe::get_min(void)
  {
    if (_readable)
      return _range.get_min();
    else
      HKLEXCEPTION("The pseudoAxe is not valid", "initialize it");
  }

  Value const &
  PseudoAxe::get_max(void)
  {
    if (_readable)
      return _range.get_max();
    else
      HKLEXCEPTION("The pseudoAxe is not valid", "initialize it");
  }

  Value const &
  PseudoAxe::get_current(void)
  {
    if (_readable)
      return _range.get_current();
    else
      HKLEXCEPTION("The pseudoAxe is not valid", "initialize it");
  }

  bool
  PseudoAxe::operator==(PseudoAxe const & pseudoAxe) const
    {
      return HKLObject::operator==(pseudoAxe)
             && _initialized == pseudoAxe._initialized
             && _readable == pseudoAxe._readable
             && _writable == pseudoAxe._writable;
    }

  ostream &
  PseudoAxe::printToStream(ostream & flux) const
    {
      HKLObject::printToStream(flux);
      return flux;
    }

  ostream &
  PseudoAxe::toStream(ostream & flux) const
    {
      HKLObject::toStream(flux);
      flux << " " << _initialized
      << " " << _writable
      << " " << _readable << endl;
      return flux;
    }

  istream &
  PseudoAxe::fromStream(istream & flux)
  {
    HKLObject::fromStream(flux);
    flux >> _initialized >> _writable >> _readable;
    return flux;
  }

} // namespace hkl
