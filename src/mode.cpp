#include "mode.h"

namespace hkl {

    Mode::Mode(void) {}

    Mode::~Mode(void) {}
    
    ostream &
    Mode::printToStream(ostream & flux) const
      { 
        flux << "Mode: " << get_name() << std::endl;
        return flux;
      }

} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::Mode const & mode)
{ 
    return mode.printToStream(flux);
}
