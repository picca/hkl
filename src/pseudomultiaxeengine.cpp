#include "pseudomultiaxeengine.h"

namespace hkl
  {

  PseudoMultiAxeEngine::PseudoMultiAxeEngine(void) :
      HKLObject("engine","engine"),
      Observer(),
      _writable(false),
      _readable(false),
      _initialized(false)
  {}

  PseudoMultiAxeEngine::PseudoMultiAxeEngine(PseudoMultiAxeEngine const & pseudoMultiAxeEngine) :
      HKLObject(pseudoMultiAxeEngine),
      Observer(),
      _writable(pseudoMultiAxeEngine._writable),
      _readable(pseudoMultiAxeEngine._readable),
      _initialized(pseudoMultiAxeEngine._initialized)
  {}

  PseudoMultiAxeEngine::~PseudoMultiAxeEngine(void)
  {}

  void
  PseudoMultiAxeEngine::uninitialize(void)
  {
    _initialized = false;
    _writable = false;
  }

  bool
  PseudoMultiAxeEngine::operator==(PseudoMultiAxeEngine const & pseudoMultiAxeEngine) const
    {
      return _initialized == pseudoMultiAxeEngine._initialized
             && _readable == pseudoMultiAxeEngine._readable
             && _writable == pseudoMultiAxeEngine._writable;
    }

  ostream &
  PseudoMultiAxeEngine::printToStream(ostream & flux) const
    {
      return flux;
    }

  ostream &
  PseudoMultiAxeEngine::toStream(ostream & flux) const
    {
      flux << " " << _initialized
      << " " << _writable
      << " " << _readable << endl;
      return flux;
    }

  istream &
  PseudoMultiAxeEngine::fromStream(istream & flux)
  {
    flux >> _initialized >> _writable >> _readable;
    return flux;
  }

} // namespace hkl
