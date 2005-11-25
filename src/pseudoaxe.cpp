#include "pseudoaxe.h"

namespace hkl {

  PseudoAxe::PseudoAxe(void)
    : ObjectWithParameters()
  {}

  PseudoAxe::~PseudoAxe(void)
  {}

  bool
  PseudoAxe::operator==(PseudoAxe const & pseudoAxe) const
  {
    return ObjectWithParameters::operator==(pseudoAxe);
  }

  ostream &
  PseudoAxe::printToStream(ostream & flux) const
  {
    return ObjectWithParameters::printToStream(flux);
  }

} // namespace hkl

ostream & operator<<(ostream & flux, hkl::PseudoAxe const & pseudoAxe)
{
  return pseudoAxe.printToStream(flux);
}
