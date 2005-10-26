#include "pseudoaxe.h"

namespace hkl {

  PseudoAxe::PseudoAxe(void) :
    ObjectWithParameters()
  {}

  PseudoAxe::PseudoAxe(PseudoAxe const & pseudoAxe) :
    ObjectWithParameters(pseudoAxe)
  {}

  PseudoAxe::~PseudoAxe(void)
  {}

  bool PseudoAxe::operator ==(PseudoAxe const & pseudoAxe) const
  {
    return ObjectWithParameters::operator==(pseudoAxe);
  }

  std::ostream & PseudoAxe::printToStream(std::ostream & flux) const
  {
    return ObjectWithParameters::printToStream(flux);
  }

} // namespace hkl

std::ostream & operator<<(std::ostream & flux, hkl::PseudoAxe const & pseudoAxe)
{
  return pseudoAxe.printToStream(flux);
}
