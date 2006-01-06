#include "pseudoaxe.h"

namespace hkl {

  PseudoAxe::PseudoAxe(void)
    : ObjectWithParameters()
  {}

  PseudoAxe::~PseudoAxe(void)
  {}

} // namespace hkl

ostream & operator<<(ostream & flux, hkl::PseudoAxe const & pseudoAxe)
{
  return pseudoAxe.printToStream(flux);
}
